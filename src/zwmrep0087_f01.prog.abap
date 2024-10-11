*&---------------------------------------------------------------------*
*&  Include           ZWMREP0087_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_whs.

  CLEAR whs.
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = l_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*   ERRO: Utilizador não tem armazem atribuído!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '001'.

    LEAVE TO SCREEN 0.

  ELSE.
    READ TABLE l_user WITH KEY statu = 'X'. "Util. Atrib. Arm.
    IF sy-subrc <> 0.
      WRITE l_user-lgnum TO text1 LEFT-JUSTIFIED.
*     ERRO: Utilizador não está atribuído ao armazém &!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMPMSG'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '002'
          message_var1   = text1.

      LEAVE TO SCREEN 0.

    ELSE.
      whs = l_user-lgnum.

      IF l_user-devty(5) = '16X20'.
        setscreen1 = '0001'.
      ELSE.
        setscreen1 = '0100'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_WHS
*&---------------------------------------------------------------------*
*&      Form  GET_PARAMETERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_parameters.
  DATA: lt_zwm001   TYPE zwm001 OCCURS 0 WITH HEADER LINE.
  DATA: lt_messages TYPE tab_bdcmsgcoll.

  MOVE whs TO text2.

** Centro Origem
**  CALL FUNCTION 'ZWM_GET_PARAMETER'
**    EXPORTING
**      i_lgnum     = whs
**      i_processo  = 'GERAL'
**      i_parametro = 'PLANT'
**    IMPORTING
**      e_valor     = valor
**      et_messages = lt_messages
**    EXCEPTIONS
**      error       = 1
**      OTHERS      = 2.
**
**  IF sy-subrc <> 0.
**    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
**      EXPORTING
**        it_messages = lt_messages.
**    LEAVE TO SCREEN 0.
**
**  ELSE.
**    gv_werks_o = valor.
**  ENDIF.


** Centro Origem
**  CALL FUNCTION 'ZWM_GET_PARAMETER'
**    EXPORTING
**      i_lgnum     = whs
**      i_processo  = 'GERAL'
**      i_parametro = 'LGORTBA'
**    IMPORTING
**      e_valor     = valor
**      et_messages = lt_messages
**    EXCEPTIONS
**      error       = 1
**      OTHERS      = 2.
**
**  IF sy-subrc <> 0.
**    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
**      EXPORTING
**        it_messages = lt_messages.
**    LEAVE TO SCREEN 0.
**
**  ELSE.
**    gv_lgort_o = valor.
**  ENDIF.

** Impressora
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = whs
      i_processo  = 'GERAL'
      i_parametro = 'PRINTER_PDF'
    IMPORTING
      e_valor     = valor
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    LEAVE TO SCREEN 0.
  ELSE.
    gv_printer = valor.
  ENDIF.

** Centro
  CLEAR valor.
  REFRESH: lt_zwm001.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = whs
      i_processo  = 'TRF_DEP_CENTRO'
      i_parametro = 'DEP_CENTRO'
    IMPORTING
      e_valor     = valor
      et_messages = lt_messages
    TABLES
      t_zwm001    = lt_zwm001
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  LOOP AT lt_zwm001.
    CLEAR gr_werks_d.
    gr_werks_d-sign   = 'I'.
    gr_werks_d-option = 'EQ'.
    gr_werks_d-low    = lt_zwm001-valor.
    APPEND gr_werks_d.
  ENDLOOP.

  SORT gr_werks_d BY low.

** Depósito
  CLEAR valor.
  REFRESH: lt_zwm001.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = whs
      i_processo  = 'TRF_DEP_CENTRO'
      i_parametro = 'DEP_DEST'
    IMPORTING
      e_valor     = valor
      et_messages = lt_messages
    TABLES
      t_zwm001    = lt_zwm001
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  LOOP AT lt_zwm001.
    CLEAR gr_lgort_d.
    gr_lgort_d-sign   = 'I'.
    gr_lgort_d-option = 'EQ'.
    gr_lgort_d-low    = lt_zwm001-valor.
    APPEND gr_lgort_d.
  ENDLOOP.

  SORT gr_lgort_d BY low.

*** Código da bapi
  CLEAR valor.
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = whs
      i_processo  = 'TRF_DEP_CENTRO'
      i_parametro = 'GM_CODE'
    IMPORTING
      e_valor     = valor
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    LEAVE TO SCREEN 0.

  ELSE.
    gv_gm_code  = valor.
  ENDIF.
*
*** Movimento MM
  CLEAR valor.
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = whs
      i_processo  = 'TRF_DEP_CENTRO'
      i_parametro = 'MOV_MM'
    IMPORTING
      e_valor     = valor
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    LEAVE TO SCREEN 0.

  ELSE.
    gv_mov_mm = valor.
  ENDIF.

ENDFORM.                    " GET_PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM status.
  SET PF-STATUS 'GUI_0001'.

  IF flag_page = 'X'.

    PERFORM get_su_details.

    LOOP AT SCREEN.
      IF screen-group1 = 'G1'.
        screen-input = 0.
        MODIFY SCREEN.

      ELSEIF screen-group1 = 'G2'.
        screen-input     = 0.
        screen-active    = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    IF scr_n = 1.
      LOOP AT SCREEN.
        IF screen-name = 'BT_F6'.
          screen-input     = 0.
          screen-active    = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ELSE.

*   PERFORM get_index.
    PERFORM get_su_details.

    LOOP AT SCREEN.
*     Depósito
      IF scr_lgort IS INITIAL.
        IF screen-name = 'SCR_WERKS' OR
           screen-name = 'SCR_SU'.
          screen-input = 0.
        ENDIF.

        IF screen-name = 'BT_F8_2'.
          screen-input     = 0.
          screen-active    = 0.
          screen-invisible = 1.
        ENDIF.

      ELSE.
        IF screen-name = 'SCR_LGORT'.
          screen-input = 0.
        ENDIF.

        IF screen-name = 'BT_F8_1'.
          screen-input     = 0.
          screen-active    = 0.
          screen-invisible = 1.
        ENDIF.
      ENDIF.

*     Centro
      IF scr_werks IS INITIAL.
        IF screen-name = 'SCR_SU'.
          screen-input = 0.
        ENDIF.

      ELSE.
        IF screen-name = 'SCR_WERKS'.
          screen-input = 0.
        ENDIF.

        IF screen-name = 'BT_F8_2'.
          screen-input     = 0.
          screen-active    = 0.
          screen-invisible = 1.
        ENDIF.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.

    LOOP AT SCREEN.
      IF scr_n = scr_total.
        IF screen-name = 'BT_F7'.
*          screen-input     = 0.
          screen-invisible = 1.
          screen-active    = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

      IF scr_n = 1.
        IF screen-name = 'BT_F6'.
*          screen-input     = 0.
          screen-invisible = 1.
          screen-active    = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

** SET Cursorfield
**********************************************************************
  IF scr_lgort IS INITIAL.
    SET CURSOR FIELD 'SCR_LGORT'.
  ELSEIF scr_werks IS INITIAL.
    SET CURSOR FIELD 'SCR_WERKS'.
  ELSE.
    SET CURSOR FIELD 'SCR_SU'.
  ENDIF.
ENDFORM.                    " STATUS
*&---------------------------------------------------------------------*
*&      Form  EXIT_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_command.

  CASE sy-ucomm.
    WHEN 'BACK'.
      PERFORM clear_fields_0001.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR sy-ucomm.

ENDFORM.                    " EXIT_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CHECK_LGORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_lgort.
  DATA: lv_lines TYPE i.
  DATA: lt_t001l TYPE t001l OCCURS 0 WITH HEADER LINE.

** Validar depósito
**********************************************************************
  CHECK scr_lgort IS NOT INITIAL.

  SELECT *
    FROM t001l INTO TABLE lt_t001l
    WHERE lgort = scr_lgort.

  IF sy-subrc <> 0.
** Depósito & não é válido para o centro &
    MOVE gv_werks  TO text2.
    MOVE scr_lgort TO text1.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '080'
        message_var1   = text1
        message_var2   = text2.

    CLEAR scr_lgort.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

  DESCRIBE TABLE lt_t001l LINES lv_lines.

  IF lv_lines = 1.
    READ TABLE lt_t001l INDEX 1.
    IF sy-subrc = 0.
      scr_werks = lt_t001l-werks.
    ENDIF.

  ELSE.
    REFRESH gr_werks_d.

    LOOP AT lt_t001l.
      CLEAR gr_werks_d.
      gr_werks_d-sign   = 'I'.
      gr_werks_d-option = 'EQ'.
      gr_werks_d-low    = lt_t001l-werks.
      APPEND gr_werks_d.
    ENDLOOP.

    SORT gr_werks_d BY low.
  ENDIF.

ENDFORM.                    " CHECK_LGORT
*&---------------------------------------------------------------------*
*&      Form  CHECK_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_werks.

  DATA: ls_t001w TYPE t001w.

** Validar centrp
**********************************************************************
  CHECK scr_werks IS NOT INITIAL.

  SELECT SINGLE *
    FROM t001w INTO ls_t001w
    WHERE werks = scr_werks.

  IF sy-subrc <> 0.
** Centro & inválido!
    MOVE ls_t001w-werks TO text1.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '128'
        message_var1   = text1.

    CLEAR scr_werks.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

ENDFORM.                    " CHECK_WERKS
*&---------------------------------------------------------------------*
*&      Form  CHECK_SCR_SU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_scr_su.
  DATA: lv_lenum   LIKE lein-lenum.
  DATA: lv_subrc   LIKE sy-subrc.
  DATA: lv_user    LIKE sy-uname.
  DATA: ls_lein    TYPE lein.
  DATA: ls_ltak    TYPE ltak.
  DATA: ls_lqua    TYPE lqua.
  DATA: ls_ltap    TYPE ltap.
  DATA: ls_zwm058  TYPE zwm058.

** Validar SSCC
**********************************************************************
  CHECK scr_su IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = scr_su
    IMPORTING
      output = scr_su.

** Verificar se SSCC existe em armazém.
  MOVE scr_su TO lv_lenum.

  SELECT SINGLE *
    FROM lein INTO ls_lein
    WHERE lenum = lv_lenum.

  IF sy-subrc = 0.
    MOVE ls_lein-lenum TO text1.
    MOVE ls_lein-lgnum TO text2.

*   Unidade depósito & já existe no armazém &!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '105'
        message_var1   = text1
        message_var2   = text2.

    CLEAR scr_su.
*    PERFORM clear_fields.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Verifica se foi criada OT
  SELECT SINGLE *
    FROM ltap INTO ls_ltap
    WHERE lgnum = whs      AND
         ( vlenr = lv_lenum OR
           nlenr = lv_lenum ).

  IF sy-subrc = 0.
*   Erro! Já existem Ot's Criadas!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '237'.

    CLEAR scr_su.
*    PERFORM clear_fields.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Valida se já foi contabilizada
  LOOP AT gt_items WHERE exidv = lv_lenum.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    MOVE lv_lenum TO text1.

*   Unidade depósito & já foi contabilizada!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '104'
        message_var1   = text1.

    CLEAR scr_su.
*    PERFORM clear_fields.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Valida se palete já foi transferida
  SELECT SINGLE *
    FROM zwm058 INTO ls_zwm058
    WHERE exidv = lv_lenum.

  IF sy-subrc = 0.
*   SSCC & já foi transferido para o centro & depósito &!
    MOVE ls_zwm058-exidv TO text1.
    MOVE ls_zwm058-werks TO text2.
    MOVE ls_zwm058-lgort TO text3.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '129'
        message_var1   = text1
        message_var2   = text2
        message_var3   = text3.

    CLEAR scr_su.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Centro e Deposito
***********************************************************************
  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
   EXPORTING
     i_user              = sy-uname
     i_exidv             = lv_lenum
     i_recall            = 'X'
     i_usewm             = 'X'
     i_userf             = 'X'
     i_usemm             = 'X'
     i_useaut            = 'X'
     i_get_lgnum         = 'X'
     i_get_werks         = 'X'
     i_get_lgort         = 'X'
* IMPORTING
*   ET_MESSAGES         =
   CHANGING
     c_lgnum             =  whs
     c_werks             = gv_werks_o
     c_lgort             = gv_lgort_o
   EXCEPTIONS
     error               = 1
     user_back           = 2
     OTHERS              = 3.


** Adicionar SU
**********************************************************************
  PERFORM add_su.
  PERFORM get_index.
  CLEAR scr_su.
ENDFORM.                    " CHECK_SCR_SU
*&---------------------------------------------------------------------*
*&      Form  CLEAR_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_fields.

  CLEAR: scr_su, scr_matnr, scr_desc1, scr_desc2, scr_charg, scr_qtd,
         scr_uni.

ENDFORM.                    " CLEAR_FIELDS
*&---------------------------------------------------------------------*
*&      Form  GET_INDEX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_index.

  DESCRIBE TABLE gt_items LINES scr_n.
*  scr_total = scr_n + 1.
  scr_total = scr_n.
ENDFORM.                    " GET_INDEX
*&---------------------------------------------------------------------*
*&      Form  GET_SU_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_su_details.
  DATA: lv_maktx TYPE maktx.

** Obter dados SU
**********************************************************************
  CLEAR: scr_su,  scr_su1, scr_matnr, scr_desc1, scr_desc2, scr_charg,
         scr_qtd, scr_uni.

  READ TABLE gt_items INDEX scr_n.
  IF sy-subrc = 0.

    CLEAR lv_maktx.
    SELECT SINGLE maktx
      FROM makt INTO lv_maktx
      WHERE matnr = gt_items-matnr AND
            spras = sy-langu.

*    MOVE gt_items-lenum   TO scr_su.
    MOVE gt_items-exidv  TO scr_su1.
    MOVE gt_items-matnr  TO scr_matnr.
    MOVE lv_maktx(20)    TO scr_desc1.
    MOVE lv_maktx+20(20) TO scr_desc2.
    MOVE gt_items-charg  TO scr_charg.
    MOVE gt_items-meins  TO scr_uni.
    MOVE gt_items-verme  TO scr_qtd.
  ENDIF.

ENDFORM.                    " GET_SU_DETAILS
*&---------------------------------------------------------------------*
*&      Form  ADD_SU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_su.

  DATA: lv_times  TYPE i.
  DATA: lv_exidv  TYPE exidv.
  DATA: ls_vekp   TYPE vekp.
  DATA: ls_zwm020 TYPE zwm020.
  DATA: lt_vepo   TYPE vepo OCCURS 0 WITH HEADER LINE.

** Adicionar SU
**********************************************************************
  CHECK scr_su IS NOT INITIAL.

  lv_times = 1.

** Validar palete remotandas
  CLEAR ls_zwm020.

  SELECT SINGLE *
   FROM zwm020 INTO ls_zwm020
   WHERE armazem = whs AND
        ( p1 = scr_su OR p2 = scr_su ).

  IF sy-subrc = 0.
    IF ls_zwm020-p1 IS NOT INITIAL AND
       ls_zwm020-p2 IS NOT INITIAL.
      lv_times = 2.

    ELSE.
      IF ls_zwm020-p1 IS INITIAL.
        ls_zwm020-p1 = ls_zwm020-p2.
      ENDIF.
    ENDIF.

  ELSE.
    ls_zwm020-p1 = scr_su.
  ENDIF.

  DO lv_times TIMES.

    IF sy-index = 1.
      lv_exidv = ls_zwm020-p1.
    ELSE.
      lv_exidv = ls_zwm020-p2.
    ENDIF.

    SELECT SINGLE *
      FROM vekp INTO ls_vekp
       WHERE exidv EQ lv_exidv.

    IF sy-subrc <> 0.
**    Erro! SSCC & invalido!
      WRITE lv_exidv TO text1 LEFT-JUSTIFIED.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '113'
          message_var1   = text1.

      CLEAR scr_su.
      LEAVE TO SCREEN setscreen1.
    ENDIF.

    REFRESH lt_vepo.

    SELECT *
      FROM vepo INTO TABLE lt_vepo
      WHERE venum = ls_vekp-venum.

    IF sy-subrc <> 0.
**    Erro! SSCC & sem conteudo!
      WRITE lv_exidv TO text1 LEFT-JUSTIFIED.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '241'
          message_var1   = text1.

      CLEAR scr_su.
      LEAVE TO SCREEN setscreen1.
    ENDIF.

    LOOP AT lt_vepo.
      CLEAR gt_items.
      gt_items-exidv = ls_vekp-exidv.
      gt_items-werks = lt_vepo-werks.
      gt_items-lgort = lt_vepo-lgort.
      gt_items-matnr = lt_vepo-matnr.
      gt_items-charg = lt_vepo-charg.
      gt_items-verme = lt_vepo-vemng.
      gt_items-meins = lt_vepo-vemeh.
      COLLECT gt_items.
    ENDLOOP.
  ENDDO.

ENDFORM.                    " ADD_SU
*&---------------------------------------------------------------------*
*&      Form  PGDN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pgdn.

  CHECK scr_n > 1.

  IF scr_n = scr_total.
    flag_page = 'X'.
  ENDIF.

  scr_n = scr_n - 1.

ENDFORM.                    " PGDN
*&---------------------------------------------------------------------*
*&      Form  PGUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pgup.

  IF scr_n < scr_total.
    scr_n = scr_n + 1.

    IF scr_n = scr_total.
      CLEAR flag_page.
    ENDIF.
  ENDIF.

ENDFORM.                    " PGUP
*&---------------------------------------------------------------------*
*&      Form  TRF_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trf_out.
  DATA: lv_goodsmvt_code   LIKE bapi2017_gm_code.
  DATA: lv_matdocument     TYPE bapi2017_gm_head_ret-mat_doc.
  DATA: lv_matdocyear      TYPE bapi2017_gm_head_ret-doc_year.
  DATA: lv_kunwe           TYPE kunwe.
  DATA: lv_vbeln           TYPE vbeln_vl.
  DATA: ls_t001w           TYPE t001w.
  DATA: ls_msg             TYPE bdcmsgcoll.
  DATA: ls_goodsmvt_header LIKE bapi2017_gm_head_01.
  DATA: lt_goodsmvt_item   LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE.
  DATA: lt_return          LIKE bapiret2                OCCURS 0 WITH HEADER LINE.
  DATA: lt_return_msg      LIKE bdcmsgcoll              OCCURS 0 WITH HEADER LINE.
  DATA: lt_dlv_items       TYPE lips                    OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm058          TYPE zwm058                  OCCURS 0 WITH HEADER LINE.

** Movimento de MM
**********************************************************************
  CHECK scr_werks IS NOT INITIAL AND scr_lgort  IS NOT INITIAL AND
        scr_su    IS INITIAL     AND gt_items[] IS NOT INITIAL.

  CLEAR: lv_goodsmvt_code, ls_goodsmvt_header, lt_goodsmvt_item.
  REFRESH: lt_goodsmvt_item.

** Dados Cabeçalho
  lv_goodsmvt_code              = gv_gm_code. "Código da Bapi
  ls_goodsmvt_header-doc_date   = sy-datum.   "Data de lançamento
  ls_goodsmvt_header-pstng_date = sy-datum.   "Data de documento
*  ls_goodsmvt_header-ref_doc_no = scr_vbeln.

** Items
  LOOP AT gt_items.
    CLEAR lt_goodsmvt_item.

    lt_goodsmvt_item-move_type     = gv_mov_mm.
    lt_goodsmvt_item-plant         = gv_werks_o.
    lt_goodsmvt_item-stge_loc      = gv_lgort_o.
    lt_goodsmvt_item-move_plant    = scr_werks.
    lt_goodsmvt_item-move_stloc    = scr_lgort.
    lt_goodsmvt_item-material      = gt_items-matnr.
    lt_goodsmvt_item-batch         = gt_items-charg.
    lt_goodsmvt_item-entry_qnt     = gt_items-verme.
    lt_goodsmvt_item-entry_uom     = gt_items-meins.
    lt_goodsmvt_item-entry_uom_iso = gt_items-meins.
    COLLECT lt_goodsmvt_item.
  ENDLOOP.

** Movimento MM
  CLEAR: lv_matdocument, lv_matdocyear.
  CLEAR: lt_return.
  REFRESH lt_return.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = ls_goodsmvt_header
      goodsmvt_code    = lv_goodsmvt_code
    IMPORTING
      materialdocument = lv_matdocument
      matdocumentyear  = lv_matdocyear
    TABLES
      goodsmvt_item    = lt_goodsmvt_item
      return           = lt_return.

  READ TABLE lt_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    CLEAR ls_msg.
    ls_msg-msgid = lt_return-id.
    ls_msg-msgnr = lt_return-number.
    ls_msg-msgv1 = lt_return-message_v1.
    ls_msg-msgv2 = lt_return-message_v2.
    ls_msg-msgv3 = lt_return-message_v3.
    ls_msg-msgv4 = lt_return-message_v4.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = ls_msg-msgid
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = ls_msg-msgnr
        message_var1   = ls_msg-msgv1
        message_var2   = ls_msg-msgv2
        message_var3   = ls_msg-msgv3
        message_var4   = ls_msg-msgv4.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    PERFORM clear_fields.
    EXIT.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

** Criar Remessa de transferencia
**********************************************************************

** Obter recebedor mercadoria
  READ TABLE gt_items INDEX 1.
  IF sy-subrc = 0.
    SELECT SINGLE *
      FROM t001w INTO ls_t001w
      WHERE werks = scr_werks.

    IF sy-subrc = 0.
      lv_kunwe = ls_t001w-kunnr.
    ENDIF.
  ENDIF.

** Obter Items
  REFRESH: lt_dlv_items.

  LOOP AT gt_items.
    CLEAR lt_dlv_items.
    lt_dlv_items-werks = gv_werks_o.
    lt_dlv_items-lgort = gv_lgort_o.
    lt_dlv_items-matnr = gt_items-matnr.
    lt_dlv_items-charg = gt_items-charg.
    lt_dlv_items-lfimg = gt_items-verme.
    lt_dlv_items-vrkme = gt_items-meins.
    lt_dlv_items-kdmat = gt_items-exidv.
    APPEND lt_dlv_items.
  ENDLOOP.

  CLEAR lv_vbeln.
  CALL FUNCTION 'ZWMMP_CREATE_DELIVERY_TRF'
    EXPORTING
      i_kunwe  = lv_kunwe
      i_lfart  = 'ZRTR'
      i_pstyv  = 'ZTRF'
    IMPORTING
      e_vbeln  = lv_vbeln
    TABLES
      t_return = lt_return_msg
      t_items  = lt_dlv_items
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  IF sy-subrc <> 0 OR lv_vbeln IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

*   Cancelar Documento
    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        materialdocument = lv_matdocument
        matdocumentyear  = lv_matdocyear
      TABLES
        return           = lt_return.

    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.

    READ TABLE lt_return_msg INDEX 1 INTO ls_msg.
    IF sy-subrc = 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_msg-msgid
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = ls_msg-msgnr
          message_var1   = ls_msg-msgv1
          message_var2   = ls_msg-msgv2
          message_var3   = ls_msg-msgv3
          message_var4   = ls_msg-msgv4.
    ENDIF.

    PERFORM clear_fields.
    EXIT.
  ENDIF.

** Imprime guia de remessa
**********************************************************************
  CALL FUNCTION 'Z_PRINT_PDF_REMESSA'
    EXPORTING
      i_lgnum           = whs
      i_vbeln           = lv_vbeln
      i_print           = gv_printer
*   I_COPIES          =
   EXCEPTIONS
     error_vbeln       = 1
     error_print       = 2
     error_dados       = 3
     OTHERS            = 4.

** Saida Efectuada com sucesso
**********************************************************************
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  MOVE lv_matdocument TO text1.
  MOVE lv_vbeln       TO text2.

  IF lv_vbeln IS NOT INITIAL.
* Documento & de transferência e remessa & criados!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '108'
        message_var1   = text1
        message_var2   = text2.
  ELSE.

**  Documento de transferência & criado!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '126'
        message_var1   = text1.
  ENDIF.

  LOOP AT gt_items.
    CLEAR lt_zwm058.
    lt_zwm058-exidv = gt_items-exidv.
    lt_zwm058-werks = scr_werks.
    lt_zwm058-lgort = scr_lgort.
    COLLECT lt_zwm058.
  ENDLOOP.

  IF sy-subrc = 0.
    MODIFY zwm058 FROM TABLE lt_zwm058.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  PERFORM clear_fields_0001.

ENDFORM.                    " TRF_OUT
*&---------------------------------------------------------------------*
*&      Form  CLEAR_FIELDS_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_fields_0001 .

  PERFORM clear_fields.

  CLEAR: scr_lgort, scr_werks, scr_n, scr_total.

  CLEAR:   gt_items.
  REFRESH: gt_items.

ENDFORM.                    " CLEAR_FIELDS_0001
*&---------------------------------------------------------------------*
*&      Form  LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list.

  DATA: lv_field   TYPE char20.
  DATA: lv_wide	   TYPE	char1.
  DATA: lv_lgort   TYPE lgort_d.
  DATA: lv_werks   TYPE werks_d.
  DATA: lt_rflista LIKE zwmrfs003 OCCURS 0 WITH HEADER LINE.

** Listagem Depósito/Centro
**********************************************************************
  GET CURSOR FIELD lv_field.

  IF setscreen1 = '0100'.
    lv_wide = 'X'.
  ENDIF.

** Depósito
  IF lv_field = 'SCR_LGORT'.
    LOOP AT gr_lgort_d.
      CLEAR: lt_rflista.
      lt_rflista-rflabel = gr_lgort_d-low.
      lt_rflista-rfvalor = gr_lgort_d-low.
      APPEND lt_rflista.
    ENDLOOP.

    CALL FUNCTION 'ZWMMP_SCREEN_LIST'
      EXPORTING
        titulo              = 'Sel. Depósito'(001)
        no_sel              = ''
        wide_screen         = lv_wide
      IMPORTING
        valor_out           = lv_lgort
      TABLES
        it_rflista          = lt_rflista
      EXCEPTIONS
        sem_entradas        = 1
        index_errado        = 2
        demasiadas_entradas = 3
        OTHERS              = 4.

    scr_lgort = lv_lgort.
    PERFORM check_lgort.

** Centro
  ELSEIF lv_field = 'SCR_WERKS'.

    LOOP AT gr_werks_d.
      CLEAR: lt_rflista.
      lt_rflista-rflabel = gr_werks_d-low.
      lt_rflista-rfvalor = gr_werks_d-low.
      APPEND lt_rflista.
    ENDLOOP.

    CALL FUNCTION 'ZWMMP_SCREEN_LIST'
      EXPORTING
        titulo              = 'Sel. Centro'(002)
        no_sel              = ''
        wide_screen         = lv_wide
      IMPORTING
        valor_out           = lv_werks
      TABLES
        it_rflista          = lt_rflista
      EXCEPTIONS
        sem_entradas        = 1
        index_errado        = 2
        demasiadas_entradas = 3
        OTHERS              = 4.

    scr_werks = lv_werks.
    PERFORM check_werks.
  ENDIF.

ENDFORM.                    " LIST
