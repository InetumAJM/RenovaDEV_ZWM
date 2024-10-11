FUNCTION z_wmfr_idoc_free_work.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LGNUM
*"     VALUE(I_REFNR) TYPE  LVS_REFNR
*"     VALUE(I_VBELN) TYPE  VBELN OPTIONAL
*"     VALUE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"     VALUE(I_STEP) TYPE  NUMC1 OPTIONAL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF lty_ltak.
          INCLUDE STRUCTURE ltak.
  TYPES:    source TYPE n,
         END OF lty_ltak.

  DATA: lt_ltak         TYPE TABLE OF lty_ltak,
        lt_ltak2        TYPE TABLE OF lty_ltak,
        lt_ltap         TYPE TABLE OF ltap,
        lt_ltapc        TYPE TABLE OF ltap_vb,
        lt_ltapc_app    TYPE TABLE OF ltap_vb,
        lt_ltakb        TYPE TABLE OF ltak,
        lt_zwm028       TYPE TABLE OF zwm028,
        lt_zwm020       TYPE TABLE OF zwm020,
        lt_zwmfrt004    TYPE TABLE OF zwmfrt004,
        lt_zwm001_zlock TYPE TABLE OF zwm001.

  DATA: ls_ltak      TYPE lty_ltak,
        ls_ltakb     TYPE ltak,
        ls_ltap      TYPE ltap,
        ls_ltapc     TYPE ltap_vb,
        ls_zwm028    TYPE zwm028,
        ls_zwm020    TYPE zwm020,
        ls_zwmfrt004 TYPE zwmfrt004.

  DATA: lv_queue1        TYPE lrf_queue,
        lv_queue2        TYPE lrf_queue,
        lv_zsyst         TYPE recvsystem,
        lv_lgpla_tap_rep TYPE lgpla,
        lv_activated     TYPE flag,
        lv_refnr         TYPE lvs_refnr,
        lv_i_refnr       TYPE lvs_refnr.

  FIELD-SYMBOLS: <ls_ltak> TYPE lty_ltak.

  lv_i_refnr = i_refnr.

  CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK_SAVE'
    EXPORTING
      i_lgnum  = i_lgnum
      i_refnr  = lv_i_refnr
      i_vbeln  = i_vbeln
      i_commit = abap_true.


** valida se Deve Lançar IDOC
***********************************************************************
  SELECT SINGLE valor FROM zwm001
                      INTO lv_activated
                      WHERE armazem   = i_lgnum AND
                            processo  = 'LIBERACAO_VIA_IDOC' AND
                            parametro = 'ACTIVAR'.

  CHECK lv_activated EQ abap_true.

** Retorna Parametros
***********************************************************************
  SELECT SINGLE valor FROM zwm001
                      INTO lv_zsyst
                      WHERE armazem   = i_lgnum AND
                            processo  = 'ENVIO_IDOC_DEST' AND
                            parametro = 'EDIDC_N_PARCEIRO'.

  SELECT SINGLE valor FROM zwm001
                      INTO lv_queue1
                      WHERE armazem   = i_lgnum AND
                            processo  = 'GESTAO_FILAS' AND
                            parametro = 'FILA_AUT_REP'.

  SELECT SINGLE valor FROM zwm001
                      INTO lv_queue2
                      WHERE armazem   = i_lgnum AND
                            processo  = 'GESTAO_FILAS' AND
                            parametro = 'FILA_AUT_SD'.

  SELECT SINGLE valor FROM zwm001
                      INTO lv_lgpla_tap_rep
                      WHERE armazem   = i_lgnum AND
                            processo  = 'TAPETE' AND
                            parametro = 'REP'.

  SELECT * FROM zwm001
           INTO TABLE lt_zwm001_zlock
                WHERE armazem   = i_lgnum AND
                      processo  = 'LIBERACAO_VIA_IDOC' AND
                      parametro = 'ZLOCK_OK'.

  SORT lt_zwm001_zlock BY valor.

** Retora Primeiras OT's
***********************************************************************
  DO 1 TIMES.
    IF NOT i_step IS INITIAL.
      CHECK i_step EQ 1.
    ENDIF.

    SELECT * FROM ltak
             INTO TABLE lt_ltak
             WHERE lgnum = i_lgnum AND
                   benum = lv_i_refnr AND
                   betyp = 'Z' AND
                   kquit = abap_false AND
                   queue = lv_queue1.
    CHECK sy-subrc EQ 0.


    SELECT * FROM ltap
             INTO TABLE lt_ltap
             FOR ALL ENTRIES IN lt_ltak
             WHERE lgnum = lt_ltak-lgnum AND
                   tanum = lt_ltak-tanum.

    CHECK sy-subrc EQ 0.

    LOOP AT lt_ltak ASSIGNING <ls_ltak>.
      <ls_ltak>-source = 1.
    ENDLOOP.
  ENDDO.

** Retora Segundas OT's
***********************************************************************
  DO 1 TIMES.
    IF NOT i_step IS INITIAL.
      CHECK i_step EQ 2.
    ENDIF.

    CHECK NOT i_vbeln IS INITIAL.

    SELECT * FROM ltak
             INTO TABLE lt_ltak2
             WHERE lgnum = i_lgnum AND
                   refnr = lv_i_refnr AND
                   kquit = abap_false AND
                   queue = lv_queue2.
    CHECK sy-subrc EQ 0.

    DELETE lt_ltak2 WHERE vbeln <> i_vbeln AND vbeln IS NOT INITIAL.
    CHECK NOT lt_ltak2 IS INITIAL.

    SELECT * FROM ltap
             APPENDING TABLE lt_ltap
             FOR ALL ENTRIES IN lt_ltak2
             WHERE lgnum = lt_ltak2-lgnum AND
                   tanum = lt_ltak2-tanum.

    CHECK sy-subrc EQ 0.

    LOOP AT lt_ltak2 ASSIGNING <ls_ltak>.
      <ls_ltak>-source = 2.
    ENDLOOP.

    APPEND LINES OF lt_ltak2 TO lt_ltak.
  ENDDO.

  CHECK NOT lt_ltap IS INITIAL.

** Remontadas
***********************************************************************
  SELECT * FROM zwm020
           INTO TABLE lt_zwm020
           FOR ALL ENTRIES IN lt_ltap
           WHERE armazem = lt_ltap-lgnum AND
                ( p1 = lt_ltap-nlenr OR p2 = lt_ltap-nlenr  OR
                  p1 = lt_ltap-vlenr OR p2 = lt_ltap-vlenr ).

  SORT lt_zwm020 BY p2.
  DELETE ADJACENT DUPLICATES FROM lt_zwm020 COMPARING p2.

** Remove OT's Enviadas
***********************************************************************
  DELETE lt_ltap WHERE kzsub = abap_true.
  CHECK NOT lt_ltap IS INITIAL.

** Dados Extras
***********************************************************************
  SELECT *  FROM zwmfrt004
            INTO TABLE lt_zwmfrt004
            FOR ALL ENTRIES IN lt_ltak
            WHERE lgnum = lt_ltak-lgnum AND
                  refnr = lt_ltak-refnr AND
                  original = abap_true.

  IF i_step <> 1.
    DELETE  lt_zwmfrt004 WHERE nlpla = lv_lgpla_tap_rep.
    CHECK NOT lt_zwmfrt004 IS INITIAL.
  ENDIF.

  SORT lt_zwmfrt004 BY refnr.

  SELECT * FROM zwm028
           INTO TABLE lt_zwm028
           FOR ALL ENTRIES IN lt_ltak
           WHERE lgnum = lt_ltak-lgnum AND
                 refnr = lt_ltak-refnr AND
                 remessa = ''.

  SORT lt_zwm028 BY refnr.


** Processa OT's
***********************************************************************
  LOOP AT lt_ltak INTO ls_ltak.
    IF ls_ltak-refnr IS INITIAL AND
       ls_ltak-betyp EQ 'Z'.
      lv_refnr = ls_ltak-benum.
    ELSE.
      lv_refnr = ls_ltak-refnr.
    ENDIF.


    IF ls_ltak-source EQ 1.
      ls_ltak-tapri = 98.
    ELSE.
      CLEAR: ls_zwm028.
      READ TABLE lt_zwm028
            INTO ls_zwm028
            WITH KEY refnr = lv_refnr
            BINARY SEARCH.

      CHECK sy-subrc EQ 0.

      IF i_step <> 1.
        READ TABLE lt_zwm001_zlock
          WITH KEY valor = ls_zwm028-zlock
          BINARY SEARCH
          TRANSPORTING NO FIELDS.

        CHECK sy-subrc EQ 0.
      ENDIF.
      ls_ltak-tapri = ls_zwm028-prioridade.
    ENDIF.

    LOOP AT lt_ltap INTO ls_ltap WHERE tanum = ls_ltak-tanum.
      CLEAR: ls_zwm020.
      READ TABLE lt_zwm020
            INTO ls_zwm020
            WITH KEY p2 = ls_ltap-vlenr
            BINARY SEARCH.
      CHECK sy-subrc <> 0.

      IF ls_ltak-source EQ 1.
        ls_ltap-nlpla = lv_lgpla_tap_rep.

        CLEAR: ls_zwmfrt004.
        ls_zwmfrt004-lgnum    = i_lgnum.
        ls_zwmfrt004-nlpla    = lv_lgpla_tap_rep.
        ls_zwmfrt004-refnr    = lv_i_refnr.
        ls_zwmfrt004-original = abap_true.
        MODIFY zwmfrt004 FROM ls_zwmfrt004.
      ELSEIF ls_ltak-source EQ 2.
        CLEAR: ls_zwmfrt004.
        LOOP AT lt_zwmfrt004 INTO ls_zwmfrt004 WHERE refnr = ls_ltak-refnr AND
                                                     nlpla <> lv_lgpla_tap_rep AND
                                                     original EQ abap_true.
          ls_ltap-nlpla = ls_zwmfrt004-nlpla.
          EXIT.
        ENDLOOP.
      ELSE.
        CONTINUE.
      ENDIF.


      ls_ltapc = ls_ltap.
      APPEND ls_ltapc TO lt_ltapc.
    ENDLOOP.

    ls_ltakb = ls_ltak.
    APPEND ls_ltakb TO lt_ltakb.
  ENDLOOP.

** Envio de IDOCS
***********************************************************************
  SORT lt_ltapc BY vlpla DESCENDING.
  SORT lt_ltakb BY tanum.

  LOOP AT lt_ltapc INTO ls_ltapc.
    CLEAR: lt_ltapc_app.

    CLEAR: ls_ltakb.
    READ TABLE lt_ltakb
          INTO ls_ltakb
          WITH KEY tanum = ls_ltapc-tanum
          BINARY SEARCH.

    CHECK sy-subrc EQ 0.

    APPEND ls_ltapc TO lt_ltapc_app.

    CALL FUNCTION 'L_IDOC_CREATE_WMTOID02'
      EXPORTING
        i_zsyst = lv_zsyst
        i_ltak  = ls_ltakb
        i_varia = ''
      TABLES
        t_ltap  = lt_ltapc_app.

    UPDATE ltap SET kzsub = abap_true
                WHERE lgnum = ls_ltapc-lgnum AND
                      tanum = ls_ltapc-tanum AND
                      tapos = ls_ltapc-tapos.
  ENDLOOP.

  COMMIT WORK.

** Aguardo por commit de IDOC
***********************************************************************
  DO 20 TIMES.
    IF lt_ltapc IS INITIAL.
      EXIT.
    ENDIF.

    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    SELECT * FROM ltap
             INTO TABLE lt_ltap
             BYPASSING BUFFER
             FOR ALL ENTRIES IN lt_ltapc
             WHERE lgnum = lt_ltapc-lgnum AND
                   tanum = lt_ltapc-tanum AND
                   tapos = lt_ltapc-tapos.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    DELETE lt_ltap WHERE kzsub = abap_true.
    IF lt_ltap IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.

** Final
***********************************************************************
  CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK_SAVE'
    EXPORTING
      i_lgnum  = i_lgnum
      i_refnr  = lv_i_refnr
      i_vbeln  = i_vbeln
      i_final  = abap_true
      i_commit = abap_true.
ENDFUNCTION.
