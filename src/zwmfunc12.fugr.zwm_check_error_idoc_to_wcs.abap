FUNCTION zwm_check_error_idoc_to_wcs .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM) TYPE  EDIDC-DOCNUM
*"     REFERENCE(I_MESTYP) TYPE  EDI_MESTYP
*"  CHANGING
*"     REFERENCE(C_RETURNCODE) TYPE  SY-SUBRC
*"     REFERENCE(C_ROLLBACK) TYPE  TBPRI OPTIONAL
*"----------------------------------------------------------------------
  DATA: lv_tanum     TYPE tanum.
  DATA: lv_lznum     TYPE lznum.
  DATA: lv_linha     TYPE fevor.

  DATA: ls_ltak      TYPE ltak.
  DATA: ls_lagp      TYPE lagp.
  DATA: ls_zwm077    TYPE zwm077.
  DATA: ls_zwm028    TYPE zwm028.
  DATA: ls_zwm080    TYPE zwm080.
  DATA: ls_e1ltcoh   TYPE e1ltcoh.
  DATA: ls_e1ltcah   TYPE e1ltcah.
  DATA: ls_e1ltcai   TYPE e1ltcai.
  DATA: ls_e1ltorh   TYPE e1ltorh.
  DATA: ls_e1ltori   TYPE e1ltori.
  DATA: ls_ztcaie    TYPE ztcaie.

  DATA: BEGIN OF i_ltorh OCCURS 10,
          lgnum LIKE ltak-lgnum,
          tanum LIKE ltak-tanum,
          bwlvs LIKE ltak-bwlvs,
          tbpri LIKE ltak-tbpri,
          trart LIKE ltak-trart,
          refnr LIKE ltak-refnr,
          betyp LIKE ltak-betyp,
          benum LIKE ltak-benum,
          kzpla LIKE ltak-kzpla,
          lznum LIKE ltak-lznum,
          bname LIKE ltak-bname,
          drukz LIKE ltak-drukz,
          nidru LIKE rl03a-nidru,
          pernr LIKE ltak-pernr,
          solwm LIKE ltak-solwm,
          solex LIKE ltak-solex,
          istwm LIKE ltak-istwm,
          nospl LIKE rl03a-nospl,
          zeiei LIKE ltak-zeiei,
          ausfb LIKE ltak-ausfb.
  DATA: END OF i_ltorh.

  DATA: lt_ltap      TYPE ltap       OCCURS 0 WITH HEADER LINE.
  DATA: lt_idoc_data TYPE edidd      OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltak_vb   LIKE ltak_vb    OCCURS 0 WITH HEADER LINE.
  DATA: lt_return    LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm078    TYPE zwm078     OCCURS 0 WITH HEADER LINE.

  FIELD-SYMBOLS: <fs_i_ltorh> LIKE i_ltorh.
  FIELD-SYMBOLS: <fs_ltak_vb> TYPE any.
  FIELD-SYMBOLS: <ft_ltak_vb> TYPE STANDARD TABLE.

** Dados de IDOC
**********************************************************************
  CALL FUNCTION 'IDOC_READ_COMPLETELY'
    EXPORTING
      document_number         = i_docnum
    TABLES
      int_edidd               = lt_idoc_data
    EXCEPTIONS
      document_not_exist      = 1
      document_number_invalid = 2
      OTHERS                  = 3.

** IDOC - Criação
**********************************************************************
  IF i_mestyp = 'WMTORD'.

    READ TABLE lt_idoc_data WITH KEY segnam = 'E1LTORH'.
    IF sy-subrc = 0.
      MOVE lt_idoc_data-sdata TO ls_e1ltorh.

      ASSIGN ('(SAPLLMDE)LT_LTAK_VB[]') TO <ft_ltak_vb>.
      IF NOT <ft_ltak_vb> IS INITIAL.

        lt_ltak_vb[] = <ft_ltak_vb>.

        READ TABLE lt_ltak_vb INDEX 1.
        IF sy-subrc = 0.
          ls_ltak-lgnum = lt_ltak_vb-lgnum.
          ls_ltak-tanum = lt_ltak_vb-tanum.
          ls_ltak-lznum = lt_ltak_vb-lznum.
        ENDIF.
      ELSE.
        ls_ltak-lgnum = ls_e1ltorh-lgnum.
        ls_ltak-tanum = ls_e1ltorh-tanum.
        ls_ltak-lznum = ls_e1ltorh-lznum.
      ENDIF.

      " OT de Entrada criada por WCS
      IF ls_ltak-tanum IS INITIAL.
        ASSIGN ('(SAPLLMDE)I_LTORH') TO <fs_i_ltorh>.
        IF sy-subrc = 0.
          ls_ltak-lgnum = <fs_i_ltorh>-lgnum.
          ls_ltak-tanum = <fs_i_ltorh>-tanum.
          ls_ltak-lznum = <fs_i_ltorh>-lznum.
        ENDIF.
      ENDIF.
    ENDIF.

    CHECK ls_ltak-lgnum = '100'.

    READ TABLE lt_idoc_data WITH KEY segnam = 'E1LTORI'.
    IF sy-subrc = 0.
      MOVE lt_idoc_data-sdata TO ls_e1ltori.
    ENDIF.

** Confirmação de Reserva de WCS
    IF ls_e1ltori-vltyp = 'CON' AND  ls_e1ltori-vlpla = 'CONFIRMA'.
      UPDATE zwm078 SET reserva = abap_true
                        rdatu  = sy-datum
                        rzeit  = sy-uzeit
                        rname  = sy-uname
      WHERE  lgnum = ls_e1ltorh-lgnum
      AND    tanum = ls_e1ltorh-tanum.
      IF sy-subrc = 0.
        COMMIT WORK.

        c_returncode = 0.
        CLEAR: c_rollback.
        EXIT.
      ENDIF.
    ENDIF.

** Guardar para Processamento de Lock/Unlock em JOB
    IF ls_e1ltori-vltyp = 'CHD' AND ( ls_e1ltori-vlpla = 'LOCK' OR ls_e1ltori-vlpla = 'UNLOCK' ).
      CLEAR ls_zwm080.

      " WMTORD - OT de Entrada de KORBER
      IF ls_ltak-lznum = '@CHD_BLOQ@'.
        c_returncode = 0.
        EXIT.
      ENDIF.

      GET TIME STAMP FIELD ls_zwm080-timestamp.

      ls_zwm080-lgnum  = ls_ltak-lgnum.
      ls_zwm080-exidv  = ls_ltak-lznum.
      ls_zwm080-tanum  = ls_ltak-tanum.
      ls_zwm080-docnum = i_docnum.

      ls_zwm080-lgpla  = ls_e1ltori-vlpla.
      ls_zwm080-datum  = sy-datum.
      ls_zwm080-uzeit  = sy-uzeit.
      ls_zwm080-uname  = sy-uname.

      MODIFY zwm080 FROM ls_zwm080.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.

** IDOC - Estorno
**********************************************************************
  ELSEIF i_mestyp = 'WMCATO'.

    READ TABLE lt_idoc_data WITH KEY segnam = 'E1LTCAH'.
    IF sy-subrc = 0.
      MOVE lt_idoc_data-sdata TO ls_e1ltcah.

      ls_ltak-lgnum = ls_e1ltcah-lgnum.
      ls_ltak-tanum = ls_e1ltcah-tanum.
    ENDIF.

    CHECK ls_ltak-lgnum = '100'.

    READ TABLE lt_idoc_data WITH KEY segnam = 'E1LTCAI'.
    IF sy-subrc = 0.
      MOVE lt_idoc_data-sdata TO ls_e1ltcai.
    ENDIF.

    READ TABLE lt_idoc_data WITH KEY segnam = 'ZTCAIE'.
    IF sy-subrc = 0.
      MOVE lt_idoc_data-sdata TO ls_ztcaie.
    ENDIF.

    SELECT SINGLE *
      FROM ltak INTO ls_ltak
      WHERE lgnum = ls_ltak-lgnum AND
            tanum = ls_ltak-tanum.

    SELECT *
       FROM ltap INTO TABLE lt_ltap
       WHERE lgnum = ls_ltak-lgnum
       AND   tanum = ls_ltak-tanum.

** Palete de Picking/Pal. Especial do Automático com Reserva
    READ TABLE lt_ltap INDEX 1.
    IF lt_ltap-vlenr IS NOT INITIAL AND lt_ltap-vlpla = 'AUT' AND ls_ltak-refnr IS NOT INITIAL.
      c_returncode = 0.
      CLEAR: c_rollback.
      EXIT.
    ENDIF.

** Palete de expedição - Carregar na Gravitica
    IF lt_ltap[] IS NOT INITIAL.
      SELECT *
        FROM zwm078 INTO TABLE lt_zwm078
        FOR ALL ENTRIES IN lt_ltap
        WHERE lgnum = lt_ltap-lgnum
        AND   tanum = lt_ltap-tanum.
    ENDIF.

    READ TABLE lt_zwm078 INDEX 1.
    IF sy-subrc = 0.
      UPDATE zwm078 SET rej   = abap_true
                        rlpla = ls_ztcaie-znlpla
      WHERE  lgnum = lt_zwm078-lgnum
      AND    tanum = lt_zwm078-tanum.

      IF sy-subrc = 0.
        COMMIT WORK.
        c_returncode = 0.
        CLEAR: c_rollback.
        EXIT.
      ENDIF.
    ENDIF.

    CLEAR lt_ltap.
    READ TABLE lt_ltap INDEX 1.

** Estorno de OT de Aviso de Entrada de Produção
    IF lt_ltap-vltyp = 'CHD' AND lt_ltap-vorga = 'ST' OR lt_ltap-vorga = 'SL'.

      lv_linha = ls_ltak-lznum.
      lv_lznum = lt_ltap-ablad.

      IF lv_linha <> 'GMA'. " Linha de Copacking
        CALL FUNCTION 'ZWM_CREATE_TO_CHD_WCS_IN'
          EXPORTING
            i_lgnum  = lt_ltap-lgnum
            i_lznum  = lv_lznum
            i_linha  = lv_linha
          TABLES
            t_return = lt_return
          EXCEPTIONS
            error    = 1
            OTHERS   = 2.
      ENDIF.
    ENDIF.

** IDOC - Confirmação
**********************************************************************
  ELSEIF i_mestyp = 'WMTOCO'.

    READ TABLE lt_idoc_data WITH KEY segnam = 'E1LTCOH'.
    IF sy-subrc = 0.
      MOVE lt_idoc_data-sdata TO ls_e1ltcoh.

      ls_ltak-lgnum = ls_e1ltcoh-lgnum.
      ls_ltak-tanum = ls_e1ltcoh-tanum.
    ENDIF.

    CHECK ls_ltak-lgnum = '100'.

** Paletes de saída nas mesas de Convencional
    SELECT SINGLE *
      FROM zwm077 INTO ls_zwm077
      WHERE lgnum  = ls_ltak-lgnum AND
            tanum  = ls_ltak-tanum.

    IF sy-subrc = 0.
      c_returncode = 0.
      EXIT.
    ENDIF.

** Expedição na gravítica de paletes do convencional
    SELECT SINGLE *
      FROM ltak INTO ls_ltak
      WHERE lgnum = ls_ltak-lgnum AND
            tanum = ls_ltak-tanum.

    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = ls_ltak-lgnum
      AND   tanum = ls_ltak-tanum.

    READ TABLE lt_ltap INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'ZWM_CHECK_TO_EXP'
        EXPORTING
          i_lgnum   = ls_ltak-lgnum
          i_refnr   = ls_ltak-refnr
          i_vbeln   = ls_ltak-benum
        IMPORTING
          cs_zwm028 = ls_zwm028.

      IF lt_ltap-vltyp <> 'AUT' AND ls_zwm028-st_pul = 'PUA'.
        c_returncode = 0.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
