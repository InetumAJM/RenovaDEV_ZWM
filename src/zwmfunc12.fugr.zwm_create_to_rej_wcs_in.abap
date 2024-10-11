FUNCTION zwm_create_to_rej_wcs_in .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_LENUM) TYPE  LENUM
*"     REFERENCE(I_LGPLA_O) TYPE  LGPLA
*"     REFERENCE(I_LGPLA_D) TYPE  LGPLA
*"  EXPORTING
*"     REFERENCE(E_TANUM) TYPE  TANUM
*"  TABLES
*"      T_RETURN STRUCTURE  BDCMSGCOLL OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lv_lznum      TYPE lznum.
  DATA: lv_ablad      TYPE ablad.
  DATA: lv_refnr      TYPE lvs_refnr.
  DATA: lv_etiq       TYPE flag.
  DATA: lv_enfa       TYPE flag.
  DATA: lv_ret_code   TYPE flag.
  DATA: lv_zeugn      TYPE lvs_zeugn.
  DATA: lv_fevor      TYPE fevor.
  DATA: lv_bwlvs      TYPE bwlvs.

  DATA: ls_zwm013     TYPE zwm013.
  DATA: ls_lein       TYPE lein.
  DATA: ls_lagp       TYPE lagp.
  DATA: ls_msg        TYPE bdcmsgcoll.

  DATA: lt_lqua       TYPE TABLE OF lqua       WITH HEADER LINE.
  DATA: lt_ltak       TYPE TABLE OF ltak       WITH HEADER LINE.
  DATA: lt_ltap       TYPE TABLE OF ltap       WITH HEADER LINE.
  DATA: lt_zwm077     TYPE TABLE OF zwm077     WITH HEADER LINE.
  DATA: lt_zwm078     TYPE TABLE OF zwm078     WITH HEADER LINE.
  DATA: lt_ltap_creat TYPE TABLE OF ltap_creat WITH HEADER LINE.
  DATA: lt_ltap_conf  TYPE TABLE OF ltap_conf  WITH HEADER LINE.

** Validar Mesa de Entrada
**********************************************************************
  IF i_lgpla_d = 'MAN_ENT1'.
    CLEAR lv_ret_code.

    " Deseja ETIQUETAR a palete no WCS ?
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_type   = 'W'
        message_number = '374'
        message_lang   = sy-langu
      IMPORTING
        ret_code       = lv_ret_code.

    IF lv_ret_code = 'O'.
      lv_etiq = 'X'.
    ENDIF.

    CLEAR lv_ret_code.

    " Deseja ENVOLVER a palete no WCS ?
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_type   = 'W'
        message_number = '375'
        message_lang   = sy-langu
      IMPORTING
        ret_code       = lv_ret_code.

    IF lv_ret_code = 'O'.
      lv_enfa = 'X'.
    ENDIF.

    lv_zeugn(1) = '@'.

    IF lv_etiq = 'X'.
      lv_zeugn+1(1) = '1'.
    ELSE.
      lv_zeugn+1(1) = '0'.
    ENDIF.

    IF lv_enfa = 'X'.
      lv_zeugn+2(1) = '1'.
    ELSE.
      lv_zeugn+2(1) = '0'.
    ENDIF.
  ENDIF.

** Validar Palete de Expedição
**********************************************************************
  SELECT *
    FROM ltap INTO TABLE lt_ltap
    WHERE lgnum = i_lgnum
    AND   vlenr = i_lenum.

  IF lt_ltap[] IS NOT INITIAL.
    SELECT *
     FROM zwm078 INTO TABLE lt_zwm078
     FOR ALL ENTRIES IN lt_ltap
     WHERE lgnum = lt_ltap-lgnum
     AND   tanum = lt_ltap-tanum.
  ENDIF.

  READ TABLE lt_zwm078 INDEX 1.
  IF sy-subrc = 0.
    lv_ablad = i_lgpla_d.

    " Gerar novo idoc com os SSCCs das paletes
    CALL FUNCTION 'ZWM_IDOC_FREE_WORK_WCS'
      EXPORTING
        i_lgnum  = lt_zwm078-lgnum
        i_tanum  = lt_zwm078-tanum
        i_vlenr  = i_lenum
        i_refnr  = lt_zwm078-refnr
        i_vbeln  = lt_zwm078-vbeln
        i_resend = 'X'
        i_ablad  = lv_ablad.

    LOOP AT lt_zwm078.

      CLEAR lt_zwm078-rlpla.

      UPDATE zwm078 SET rej   = abap_false
                        rlpla = lt_zwm078-rlpla
      WHERE lgnum = lt_zwm078-lgnum
      AND   tanum = lt_zwm078-tanum
      AND   tapos = lt_zwm078-tapos.

      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDLOOP.

    e_tanum = lt_zwm078-tanum.

    EXIT.
  ENDIF.

** Validar Palete de Copacking
**********************************************************************
  SELECT SINGLE *
    FROM lein INTO ls_lein
    WHERE lenum = i_lenum.

  IF sy-subrc <> 0.

    SELECT SINGLE *
      FROM zwm013 INTO ls_zwm013
      WHERE armazem = i_lgnum
      AND   sscc    = i_lenum.

    IF sy-subrc = 0 AND ls_zwm013-pos_rej IS NOT INITIAL.

      " Aviso Entrada
      CALL FUNCTION 'ZWM_GET_PARAMETER'
        EXPORTING
          i_lgnum     = i_lgnum
          i_processo  = 'OT_DUMMY'
          i_parametro = 'MOV_WM_WCS_ENT'
        IMPORTING
          e_valor     = lv_bwlvs
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      " Linha
      SELECT *
        FROM ltap INTO TABLE lt_ltap
        WHERE lgnum = i_lgnum
        AND   ablad = i_lenum.

      IF lt_ltap[] IS NOT INITIAL.
        SELECT *
          FROM ltak INTO TABLE lt_ltak
          FOR ALL ENTRIES IN lt_ltap
          WHERE lgnum = lt_ltap-lgnum
          AND   tanum = lt_ltap-tanum.

        DELETE lt_ltak WHERE bwlvs <> lv_bwlvs.

        SORT lt_ltak BY bdatu DESCENDING bzeit DESCENDING.
      ENDIF.

      CLEAR lt_ltak.
      READ TABLE lt_ltak INDEX 1.
      IF sy-subrc = 0.
        lv_fevor = lt_ltak-lznum.
      ENDIF.

      " Palete
      lv_lznum = i_lenum.

      CALL FUNCTION 'ZWM_CREATE_TO_CHD_WCS_IN'
        EXPORTING
          i_lgnum    = i_lgnum
          i_lznum    = lv_lznum
          i_linha    = lv_fevor
          i_zeugn    = lv_zeugn
        IMPORTING
          e_to_dummy = e_tanum
        TABLES
          t_return   = t_return
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

      IF sy-subrc <> 0 OR e_tanum IS INITIAL.
        RAISE error.
      ELSE.
        CLEAR ls_zwm013-pos_rej.

        MODIFY zwm013 FROM ls_zwm013.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.

        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

** Validar Palete Armazém
**********************************************************************
  SELECT SINGLE *
    FROM lagp INTO ls_lagp
    WHERE lgnum = i_lgnum
    AND   lgtyp = 'REJ'
    AND   lgpla = i_lgpla_o.

  SELECT *
    FROM lqua INTO TABLE lt_lqua
    WHERE lgnum = ls_lagp-lgnum
    AND   lgtyp = ls_lagp-lgtyp
    AND   lgpla = ls_lagp-lgpla
    AND   lenum = i_lenum.

  IF sy-subrc <> 0.
    RAISE error.
  ENDIF.

** Validar Grupo
  REFRESH: lt_ltak, lt_ltap.

  SELECT *
    FROM ltap INTO TABLE lt_ltap
    WHERE lgnum = i_lgnum
    AND   vlenr = i_lenum.

  DELETE lt_ltap WHERE vltyp <> 'EAU'.

  IF lt_ltap[] IS NOT INITIAL.
    SELECT *
      FROM ltak INTO TABLE lt_ltak
      FOR ALL ENTRIES IN lt_ltap
      WHERE lgnum = lt_ltap-lgnum
      AND   tanum = lt_ltap-tanum.

    DELETE lt_ltak WHERE refnr IS INITIAL.

    " Obter Grupo
    READ TABLE lt_ltak INDEX 1.
    IF sy-subrc = 0.
      lv_refnr = lt_ltak-refnr.
    ENDIF.
  ENDIF.

  LOOP AT lt_lqua.
    CLEAR lt_ltap_creat.
    lt_ltap_creat-werks = lt_lqua-werks.
    lt_ltap_creat-lgort = lt_lqua-lgort.
    lt_ltap_creat-matnr = lt_lqua-matnr.
    lt_ltap_creat-charg = lt_lqua-charg.
    lt_ltap_creat-anfme = lt_lqua-verme.
    lt_ltap_creat-altme = lt_lqua-meins.
    lt_ltap_creat-vltyp = lt_lqua-lgtyp.
    lt_ltap_creat-vlpla = lt_lqua-lgpla.
    lt_ltap_creat-zeugn = lv_zeugn.
    lt_ltap_creat-nltyp = 'AUT'.
    APPEND lt_ltap_creat.
  ENDLOOP.

  CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
    EXPORTING
      i_lgnum       = i_lgnum
      i_bwlvs       = '999'
      i_refnr       = lv_refnr
    IMPORTING
      e_tanum       = e_tanum
    TABLES
      t_ltap_creat  = lt_ltap_creat
    EXCEPTIONS
      error_message = 99.

  IF sy-subrc <> 0 OR e_tanum IS INITIAL.
    CLEAR ls_msg.
    ls_msg-msgid  = sy-msgid.
    ls_msg-msgtyp = sy-msgty.
    ls_msg-msgnr  = sy-msgno.
    ls_msg-msgv1  = sy-msgv1.
    ls_msg-msgv2  = sy-msgv2.
    ls_msg-msgv3  = sy-msgv3.
    ls_msg-msgv4  = sy-msgv4.
    APPEND ls_msg TO t_return.

    RAISE error.
  ENDIF.

** Guardar Mesa de Entrada
  CALL FUNCTION 'ZWM_UPDATE_POS_ENT_WCS'
    EXPORTING
      i_lgnum = i_lgnum
      i_tanum = e_tanum
      i_su    = i_lenum
      i_lgpla = i_lgpla_d
      i_etiq  = lv_etiq
      i_enfa  = lv_enfa.

  REFRESH: lt_ltap, lt_ltap_conf.

  SELECT *
    FROM ltap INTO TABLE lt_ltap
    WHERE lgnum = i_lgnum
    AND   tanum = e_tanum.

  LOOP AT lt_ltap.
    CLEAR lt_ltap_conf.
    lt_ltap_conf-tanum = lt_ltap-tanum.
    lt_ltap_conf-tapos = lt_ltap-tapos.
    lt_ltap_conf-altme = lt_ltap-meins.
    lt_ltap_conf-nista = lt_ltap-vsolm.
    APPEND lt_ltap_conf.
  ENDLOOP.

  CALL FUNCTION 'L_TO_CONFIRM'
    EXPORTING
      i_lgnum       = i_lgnum
      i_tanum       = e_tanum
    TABLES
      t_ltap_conf   = lt_ltap_conf
    EXCEPTIONS
      error_message = 99.

  IF sy-subrc <> 0.
    CLEAR ls_msg.
    ls_msg-msgid  = sy-msgid.
    ls_msg-msgtyp = sy-msgty.
    ls_msg-msgnr  = sy-msgno.
    ls_msg-msgv1  = sy-msgv1.
    ls_msg-msgv2  = sy-msgv2.
    ls_msg-msgv3  = sy-msgv3.
    ls_msg-msgv4  = sy-msgv4.
    APPEND ls_msg TO t_return.

    RAISE error.
  ENDIF.


ENDFUNCTION.
