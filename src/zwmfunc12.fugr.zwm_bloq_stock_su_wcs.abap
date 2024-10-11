FUNCTION zwm_bloq_stock_su_wcs.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TANUM) TYPE  TANUM
*"     REFERENCE(I_EXIDV) TYPE  EXIDV
*"     REFERENCE(I_LGPLA) TYPE  LGPLA
*"  EXPORTING
*"     REFERENCE(E_MBLNR) TYPE  MBLNR
*"     REFERENCE(E_MJAHR) TYPE  MJAHR
*"     REFERENCE(E_TANUM_PC) TYPE  TANUM
*"  TABLES
*"      T_RETURN_MSG STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
  CONSTANTS: lc_object    TYPE balhdr-object    VALUE 'ZWCS'.
  CONSTANTS: lc_subobject TYPE balhdr-subobject VALUE 'ZWCS_11'.

  DATA: lv_extnumber  TYPE balnrext.
  DATA: lv_ubnum      TYPE ubnum.
  DATA: lv_code       TYPE bapi2017_gm_code.
  DATA: lv_mov        TYPE bwlvs.
  DATA: lv_tanum      TYPE tanum.
  DATA: lv_werks      TYPE werks_d.
  DATA: lv_lgort      TYPE lgort_d.
  DATA: lv_mat_doc    TYPE mblnr.
  DATA: lv_mat_year   TYPE mjahr.
  DATA: lv_flag_bloq  TYPE flag.
  DATA: lv_flag_error TYPE flag.
  DATA: lv_spgru      TYPE lvs_spgru.

  DATA: ls_lein       TYPE lein.
  DATA: ls_leinv      TYPE leinv.
  DATA: ls_ltak       TYPE ltak.

  DATA: lt_lqua       TYPE lqua       OCCURS 0 WITH HEADER LINE.
  DATA: lt_lubqu      TYPE lubqu      OCCURS 0 WITH HEADER LINE.
  DATA: lt_return_msg TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: lt_return     TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: lt_items      TYPE zwm018     OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap       TYPE ltap       OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap_conf  TYPE ltap_conf  OCCURS 0 WITH HEADER LINE.

** Validar Se OT foi criada
**********************************************************************
  IF i_tanum IS NOT INITIAL.
    DO 10 TIMES.

      SELECT SINGLE *
        FROM ltak INTO ls_ltak
        WHERE lgnum = i_lgnum
        AND   tanum = i_tanum.

      IF sy-subrc = 0.
        EXIT.
      ENDIF.

      WAIT UP TO 1 SECONDS.
    ENDDO.
  ENDIF.

  SELECT *
    FROM ltap INTO TABLE lt_ltap
    WHERE lgnum = i_lgnum
    AND   tanum = i_tanum.

** Validar Stock
**********************************************************************
  lv_extnumber = i_exidv.

  SELECT SINGLE *
    FROM lein INTO ls_lein
    WHERE lenum = i_exidv.

  IF sy-subrc <> 0.
    " SSCC & não existe no sistema!
    CLEAR lt_return_msg.
    lt_return_msg-msgtyp = 'E'.
    lt_return_msg-msgid  = 'ZWM001'.
    lt_return_msg-msgnr  = '138'.
    lt_return_msg-msgv1  = i_exidv.
    APPEND lt_return_msg.
  ENDIF.

  SELECT *
    FROM lqua INTO TABLE lt_lqua
    WHERE lgnum = i_lgnum
    AND   lenum = i_exidv.

  IF sy-subrc <> 0.
    " ERRO: SSCC & não existe no armazém &!
    CLEAR lt_return_msg.
    lt_return_msg-msgtyp = 'E'.
    lt_return_msg-msgid  = 'ZWM001'.
    lt_return_msg-msgnr  = '139'.
    lt_return_msg-msgv1  = i_exidv.
    lt_return_msg-msgv2  = i_lgnum.
    APPEND lt_return_msg.
  ENDIF.

  IF lt_return_msg[] IS NOT INITIAL.

    t_return_msg[] = lt_return_msg[].

    CALL FUNCTION 'ZWM_MSG_LOG_WCS'
      EXPORTING
        i_object    = lc_object
        i_subobject = lc_subobject
        i_extnumber = lv_extnumber
        i_state     = 'A'
      TABLES
        t_log2      = lt_return_msg
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    EXIT.
  ENDIF.

  SORT lt_lqua.
  READ TABLE lt_lqua INDEX 1.

  DO 1 TIMES.

** Stock Livre -> Bloqueado
    IF i_lgpla = 'LOCK'.

      IF ls_lein-spgru = '7' OR
         ls_lein-spgru = '8'.

        " SSCC & já foi bloqueado pelo WCS com o tipo de motivo & !
        CLEAR lt_return_msg.
        lt_return_msg-msgtyp = 'W'.
        lt_return_msg-msgid  = 'ZWM001'.
        lt_return_msg-msgnr  = '137'.
        lt_return_msg-msgv1  = i_exidv.
        lt_return_msg-msgv2  = ls_lein-spgru.
        APPEND lt_return_msg.
        EXIT.
      ENDIF.

      IF lt_lqua-bestq IS INITIAL.
        lv_code       = '04'.
        lv_mov        = '344'.
        ls_lein-skzua = 'X'.
        ls_lein-skzue = 'X'.
        ls_lein-spgru = '7'.
      ELSE.
        ls_lein-skzua = 'X'.
        ls_lein-skzue = 'X'.
        ls_lein-spgru = '8'.
      ENDIF.

** Stock Bloqueado -> Livre
    ELSEIF i_lgpla = 'UNLOCK'.

      IF ls_lein-spgru <> '7' AND
         ls_lein-spgru <> '8'.

        " SSCC & não pode ser desbloqueado pelo WCS. Tipo de motivo & de SAP.
        CLEAR lt_return_msg.
        lt_return_msg-msgtyp = 'W'.
        lt_return_msg-msgid  = 'ZWM001'.
        lt_return_msg-msgnr  = '136'.
        lt_return_msg-msgv1  = i_exidv.
        lt_return_msg-msgv2  = ls_lein-spgru.
        APPEND lt_return_msg.
        EXIT.
      ENDIF.

      IF ls_lein-spgru = '7'.
        lv_code = '04'.
        lv_mov  = '343'.
      ENDIF.

      lv_spgru = ls_lein-spgru.

      CLEAR ls_lein-skzua.
      CLEAR ls_lein-skzue.
      CLEAR ls_lein-spgru.

      lv_flag_bloq = 'X'.
    ENDIF.

  ENDDO.

  IF lt_return_msg[] IS NOT INITIAL.

    t_return_msg[] = lt_return_msg[].

    CALL FUNCTION 'ZWM_MSG_LOG_WCS'
      EXPORTING
        i_object    = lc_object
        i_subobject = lc_subobject
        i_extnumber = lv_extnumber
        i_state     = 'A'
      TABLES
        t_log2      = lt_return_msg
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    EXIT.
  ENDIF.

** Desbloquear SU
  IF lv_flag_bloq IS NOT INITIAL.

    MOVE-CORRESPONDING ls_lein TO ls_leinv.

    CALL FUNCTION 'L_LEIN_VERAENDERN'
      EXPORTING
        xleinv = ls_leinv.

    COMMIT WORK AND WAIT.
  ENDIF.

** Movimento MM
**********************************************************************
  CLEAR lv_flag_error.

  DO 1 TIMES.

    CHECK lv_code IS NOT INITIAL.

    LOOP AT lt_lqua.

      CLEAR:   lt_items, lt_return_msg, lt_lubqu.
      REFRESH: lt_items, lt_return_msg, lt_lubqu.

      CLEAR lt_items.
      lt_items-armazem    = i_lgnum.
      lt_items-material   = lt_lqua-matnr.
      lt_items-lote       = lt_lqua-charg.
      lt_items-quantidade = lt_lqua-gesme.
      lt_items-uni        = lt_lqua-meins.
      lt_items-spec_mvmt  = 'W'.
      COLLECT lt_items.

      CLEAR lt_lubqu.
      lt_lubqu-lqnum = lt_lqua-lqnum.
      lt_lubqu-menge = lt_lqua-gesme.
      lt_lubqu-kzuap = 'X'.
      APPEND lt_lubqu.

      lv_werks = lt_lqua-werks.
      lv_lgort = lt_lqua-lgort.
    ENDLOOP.

    DO 10 TIMES.
      CLEAR: lv_mat_doc, lv_mat_year.

      REFRESH: lt_return_msg.

      CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
        EXPORTING
          lgnum            = i_lgnum
          code             = lv_code
          mov_mm           = lv_mov
          testrun          = ' '
          plant_o          = lv_werks
          sloc_o           = lv_lgort
        IMPORTING
          materialdocument = lv_mat_doc
          matdocumentyear  = lv_mat_year
        TABLES
          return_msg       = lt_return_msg
          items            = lt_items
        EXCEPTIONS
          error            = 1
          OTHERS           = 2.

      IF lv_mat_doc IS NOT INITIAL.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.

    ENDDO.

    IF lv_mat_doc IS INITIAL.
      CLEAR lt_return_msg.
      lt_return_msg-msgtyp = 'E'.
      lt_return_msg-msgid  = 'ZWM001'.
      lt_return_msg-msgnr  = '000'.
      lt_return_msg-msgv1  = 'Erro na criação do documento material'.
      APPEND lt_return_msg.

      lv_flag_error = 'X'.
      EXIT.
    ENDIF.

    " Wait for update
    CHECK lv_flag_error IS INITIAL.

    CLEAR lv_ubnum.

    DO 10 TIMES.
      SELECT SINGLE ubnum FROM mseg INTO lv_ubnum
             BYPASSING BUFFER
             WHERE mblnr EQ lv_mat_doc
               AND mjahr EQ lv_mat_year
               AND zeile EQ 1.
      IF sy-subrc = 0.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDDO.

    IF lv_ubnum IS INITIAL.

      CLEAR lt_return_msg.
      lt_return_msg-msgtyp = 'E'.
      lt_return_msg-msgid  = 'ZWM001'.
      lt_return_msg-msgnr  = '000'.
      lt_return_msg-msgv1  = 'Erro leitura documento (MSEG)'.
      APPEND lt_return_msg.

      lv_flag_error = 'X'.
      EXIT.
    ELSE.

      " Documento de Material & criado!
      CLEAR lt_return_msg.
      lt_return_msg-msgtyp = 'S'.
      lt_return_msg-msgid  = 'ZWM001'.
      lt_return_msg-msgnr  = '135'.
      lt_return_msg-msgv1  = lv_mat_doc.
      APPEND lt_return_msg.
    ENDIF.

** Movimento WM
**********************************************************************
    CHECK lv_flag_error IS INITIAL.

    CLEAR lv_tanum.

    CALL FUNCTION 'L_TO_CREATE_POSTING_CHANGE'
      EXPORTING
        i_lgnum       = i_lgnum
        i_ubnum       = lv_ubnum
        i_squit       = 'X'
        i_commit_work = 'X'
      IMPORTING
        e_tanum       = lv_tanum
      TABLES
        t_lubqu       = lt_lubqu
      EXCEPTIONS
        error_message = 99.

    IF sy-subrc <> 0 OR lv_tanum IS INITIAL.

      ROLLBACK WORK.

      REFRESH: lt_return_msg.

      CLEAR lt_return_msg.
      lt_return_msg-msgtyp = sy-msgty.
      lt_return_msg-msgid  = sy-msgid.
      lt_return_msg-msgnr  = sy-msgno.
      lt_return_msg-msgv1  = sy-msgv1.
      lt_return_msg-msgv2  = sy-msgv2.
      lt_return_msg-msgv3  = sy-msgv3.
      lt_return_msg-msgv4  = sy-msgv4.
      APPEND lt_return_msg.

      " Estorna o Doc de material
      CALL FUNCTION 'ZWM_ESTORNA_DOC_MATERIAL'
        EXPORTING
          mblnr       = lv_mat_doc
          mjahr       = lv_mat_year
        TABLES
          return      = lt_return
        EXCEPTIONS
          erro_commit = 1
          OTHERS      = 2.

      lv_flag_error = 'X'.
      EXIT.

    ELSE.
      COMMIT WORK.

      " OT & posting change criada!
      CLEAR lt_return_msg.
      lt_return_msg-msgtyp = 'S'.
      lt_return_msg-msgid  = 'ZWM001'.
      lt_return_msg-msgnr  = '134'.
      lt_return_msg-msgv1  = lv_tanum.
      APPEND lt_return_msg.
    ENDIF.

  ENDDO.

** Bloqueia SU
**********************************************************************
  IF lv_flag_error IS NOT INITIAL.

    IF lv_flag_bloq IS NOT INITIAL.

      " Erro -> Bloquear novamente SU
      ls_lein-skzua = 'X'.
      ls_lein-skzue = 'X'.
      ls_lein-spgru = lv_spgru.

      MOVE-CORRESPONDING ls_lein TO ls_leinv.

      CALL FUNCTION 'L_LEIN_VERAENDERN'
        EXPORTING
          xleinv = ls_leinv.

      COMMIT WORK AND WAIT.
    ENDIF.

  ELSE.

** Bloquer SU
    IF lv_flag_bloq IS INITIAL.

      MOVE-CORRESPONDING ls_lein TO ls_leinv.

      CALL FUNCTION 'L_LEIN_VERAENDERN'
        EXPORTING
          xleinv = ls_leinv.

      COMMIT WORK AND WAIT.
    ENDIF.

    " SCCC & & com sucesso pelo WCS!
    CLEAR lt_return_msg.
    lt_return_msg-msgtyp = 'S'.
    lt_return_msg-msgid  = 'ZWM001'.
    lt_return_msg-msgnr  = '132'.
    lt_return_msg-msgv1  = i_exidv.

    IF i_lgpla = 'LOCK'.
      lt_return_msg-msgv2  = 'Bloqueado'.
    ELSE.
      lt_return_msg-msgv2  = 'Desbloqueado'.
    ENDIF.
    APPEND lt_return_msg.

    IF i_lgpla = 'LOCK'.
      " Tipo de motivo & de bloqueio!
      CLEAR lt_return_msg.
      lt_return_msg-msgtyp = 'S'.
      lt_return_msg-msgid  = 'ZWM001'.
      lt_return_msg-msgnr  = '133'.
      lt_return_msg-msgv1  = ls_lein-spgru.
      APPEND lt_return_msg.
    ENDIF.
  ENDIF.

** Confirmar OT de Chamada
**********************************************************************
  IF lv_flag_error IS INITIAL.

    e_mblnr    = lv_mat_doc.
    e_mjahr    = lv_mat_year.
    e_tanum_pc = lv_tanum.

    REFRESH lt_ltap_conf.

    LOOP AT lt_ltap.
      CLEAR lt_ltap_conf.
      lt_ltap_conf-tanum = lt_ltap-tanum.
      lt_ltap_conf-tapos = lt_ltap-tapos.
      lt_ltap_conf-nista = lt_ltap-vsolm.
      lt_ltap_conf-altme = lt_ltap-meins.
      APPEND lt_ltap_conf.
    ENDLOOP.

    IF i_tanum IS NOT INITIAL.
      CALL FUNCTION 'L_TO_CONFIRM'
        EXPORTING
          i_lgnum       = ls_ltak-lgnum
          i_tanum       = ls_ltak-tanum
        TABLES
          t_ltap_conf   = lt_ltap_conf
        EXCEPTIONS
          error_message = 99.
    ENDIF.
  ENDIF.

** Log
**********************************************************************
  IF lt_return_msg[] IS NOT INITIAL.

    t_return_msg[] = lt_return_msg[].

    CALL FUNCTION 'ZWM_MSG_LOG_WCS'
      EXPORTING
        i_object    = lc_object
        i_subobject = lc_subobject
        i_extnumber = lv_extnumber
        i_state     = 'A'
      TABLES
        t_log2      = lt_return_msg
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
  ENDIF.

ENDFUNCTION.
