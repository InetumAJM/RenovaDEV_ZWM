*&---------------------------------------------------------------------*
*&  Include           ZWMREP0133_F01
*&---------------------------------------------------------------------*


FORM get_whs.

  DATA : BEGIN OF l_user OCCURS 0.
          INCLUDE STRUCTURE lrf_wkqu.
  DATA : END OF l_user.

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
    MESSAGE e001(zwmmpmsg).

  ELSE.
    READ TABLE l_user WITH KEY statu = 'X'.
    IF sy-subrc <> 0.
*     ERRO: Utilizador não está atribuído ao armazém &!
      MESSAGE e002(zwmmpmsg) WITH sy-uname.
    ELSE.

      gv_lgnum = l_user-lgnum.

    ENDIF.
  ENDIF.

ENDFORM.                    " GET_WHS

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
  DATA: lt_messages TYPE tab_bdcmsgcoll.

  PERFORM get_whs.

** Parametros
**********************************************************************
  DO 1 TIMES.
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'MOV_CODE_IN'
      IMPORTING
        e_valor     = gv_bwart_in
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'MOV_CODE_OUT'
      IMPORTING
        e_valor     = gv_bwart_out
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'STG_LOC_FINAL'
      IMPORTING
        e_valor     = gv_lgort_f_in
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'COST_CENTER'
      IMPORTING
        e_valor     = gv_kostl
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'STG_LOC_COMPONENTS'
      IMPORTING
        e_valor     = gv_lgort_comp
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'COMPANY'
      IMPORTING
        e_valor     = gv_bukrs
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'DOC_TYPE'
      IMPORTING
        e_valor     = gv_blart
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'CONTA_RASAO_C'
      IMPORTING
        e_valor     = gv_hkont_c
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'CONTA_RASAO_D'
      IMPORTING
        e_valor     = gv_hkont_d
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'TAX_CODE'
      IMPORTING
        e_valor     = gv_mwskz
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.
  ENDDO.

  IF sy-subrc <> 0.
    PERFORM show_message_t USING lt_messages.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_screen_0001.
  PERFORM reset_0001.
  PERFORM new_page_0001.
  PERFORM call_screen USING '0001'.
ENDFORM.                    " CALL_SCREEN_0001
*&---------------------------------------------------------------------*
*&      Form  RESET_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001.
  CLEAR: scr0001.
ENDFORM.                    " RESET_0001
*&---------------------------------------------------------------------*
*&      Form  CONFIGURE_0001_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM configure_0001_fields.
  DATA: lv_subrc TYPE sysubrc.

** Screens
***********************************************************************
  LOOP AT SCREEN.

    CASE screen-name.
      WHEN ''.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " CONFIGURE_0001_FIELDS
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0001.
  CASE scr0001-okcode.
    WHEN 'SAVE'.
      PERFORM save_0001.
    WHEN 'CLR'.
      PERFORM undo_0001.
    WHEN 'UP'.
      PERFORM move_0001 USING 1.
    WHEN 'DN'.
      PERFORM move_0001 USING -1.
    WHEN 'NEXT'.
      PERFORM next_0001.
  ENDCASE.

  CLEAR: scr0001-okcode.
ENDFORM.                    " USER_COMMAND_0001
*&---------------------------------------------------------------------*
*&      Form  UNDO_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM undo_0001.
  DATA: lv_subrc TYPE sysubrc.

  IF NOT scr0001-lenum IS INITIAL.
    PERFORM reset_0001_lenum.
  ELSEIF NOT scr0001-exidv IS INITIAL.
    PERFORM check_exit_0001 CHANGING lv_subrc.
    CHECK lv_subrc EQ 0.
    PERFORM reset_all.
  ELSE.
    PERFORM reset_all.
  ENDIF.
ENDFORM.                    " UNDO_0001
*&---------------------------------------------------------------------*
*&      Form  NEXT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM next_0001.
  DATA: lv_field TYPE c LENGTH 50,
        lv_subrc TYPE sysubrc.

  GET CURSOR FIELD lv_field.

  CASE lv_field.
    WHEN ''.

    WHEN OTHERS.
      EXIT.
  ENDCASE.
ENDFORM.                    " NEXT_0001
*&---------------------------------------------------------------------*
*&      Form  call_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_USER_SCR01  text
*----------------------------------------------------------------------*
FORM call_screen  USING uv_dynnr TYPE sydynnr.
  CHECK sy-dynnr <> uv_dynnr.
  IF sy-dynnr EQ '1000'.
    CALL SCREEN uv_dynnr.
  ELSE.
    LEAVE TO SCREEN uv_dynnr.
  ENDIF.
ENDFORM.                    " call_screen
*&---------------------------------------------------------------------*
*&      Form  RELOAD_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reload_0001.
  DATA: ls_scr0001 TYPE gty_scr0001.

  ls_scr0001 = scr0001.
  PERFORM reset_0001.


ENDFORM.                    " RELOAD_0001
*&---------------------------------------------------------------------*
*&      Form  EXIT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_0001.
  DATA: lv_ret_code TYPE flag,
        lv_subrc    TYPE sysubrc.

  PERFORM check_exit_0001 CHANGING lv_subrc.
  CHECK lv_subrc EQ 0.

  PERFORM reset_all.
  LEAVE TO SCREEN 0.
ENDFORM.                    " EXIT_0001
*&---------------------------------------------------------------------*
*&      Form  SAVE_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_0001.
  DATA: lt_messages       TYPE tab_bdcmsgcoll,
        lt_msegc          TYPE TABLE OF mseg,
        lt_mseg           TYPE TABLE OF mseg,
        lt_items          TYPE tab_bapi_goodsmvt_item,
        lt_trite          TYPE l03b_trite_t,
        lt_accountgl      TYPE TABLE OF bapiacgl09,
        lt_currencyamount	TYPE TABLE OF bapiaccr09,
        lt_return	        TYPE TABLE OF bapiret2,
        lt_ltak_vb        TYPE pdt_t_ltak_vb.

  DATA: ls_message        TYPE bdcmsgcoll,
        ls_return	        TYPE bapiret2,
        ls_scr0001_reads  TYPE gty_scr0001,
        ls_scr0001_nwm    TYPE gty_scr0001,
        ls_mseg           TYPE mseg,
        ls_item           TYPE bapi2017_gm_item_create,
        ls_trite          TYPE l03b_trite,
        ls_zwm013         TYPE zwm013,
        ls_documentheader	TYPE bapiache09,
        ls_accountgl      TYPE bapiacgl09,
        ls_currencyamount	TYPE bapiaccr09,
        ls_t001           TYPE t001.

  DATA: lv_subrc    TYPE sysubrc,
        lv_ret_code TYPE flag,
        lv_itemno   TYPE posnr_acc.

  FIELD-SYMBOLS: <ls_trite> TYPE l03b_trite.

  PERFORM check_save_0001 CHANGING lv_subrc.
  CHECK lv_subrc EQ 0.

** Salvar e consolidar?
  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWM001'
      message_lang   = sy-langu
      message_type   = 'W'
      message_number = '050'
    IMPORTING
      ret_code       = lv_ret_code.

  CHECK lv_ret_code EQ 'O'.

** Entrada de Produto Final
**********************************************************************
  CLEAR: ls_scr0001_reads.
  READ TABLE gt_scr0001_reads
        INTO ls_scr0001_reads
        INDEX 1.

  CLEAR: ls_item, lt_items.
  ls_item-move_type  = gv_bwart_in.
  ls_item-plant      = ls_scr0001_reads-s_vepo-werks.
  ls_item-stge_loc   = gv_lgort_f_in.
  ls_item-material   = ls_scr0001_reads-s_vepo-matnr.
  ls_item-batch      = ls_scr0001_reads-s_vepo-charg.
  ls_item-entry_qnt  = ls_scr0001_reads-s_vepo-vemng.
  ls_item-entry_uom  = ls_scr0001_reads-s_vepo-vemeh.
  ls_item-item_text  = ls_scr0001_reads-s_lqua-lenum.
  ls_item-costcenter = gv_kostl.
  APPEND ls_item TO lt_items.

  CALL FUNCTION 'ZWM_GOODSMVT_CREATE'
    EXPORTING
      i_code       = '05'
      i_header_txt = scr0001-exidv
      i_bwart      = gv_bwart_in
      it_items     = lt_items
    IMPORTING
      et_mseg      = lt_mseg
      et_messages  = lt_messages
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    PERFORM goodsmvt_cancel USING lt_msegc.
    PERFORM show_message_t USING lt_messages.
    EXIT.
  ENDIF.

  APPEND LINES OF lt_mseg TO lt_msegc.

** Consumo de Componentes
**********************************************************************
  CLEAR: lt_items.
  LOOP AT gt_scr0001_reads INTO ls_scr0001_reads.
    CLEAR ls_item.
    ls_item-move_type  = gv_bwart_out.
    ls_item-plant      = ls_scr0001_reads-s_lqua-werks.
    ls_item-stge_loc   = ls_scr0001_reads-s_lqua-lgort.
    ls_item-material   = ls_scr0001_reads-s_lqua-matnr.
    ls_item-batch      = ls_scr0001_reads-s_lqua-charg.
    ls_item-entry_qnt  = ls_scr0001_reads-menge.
    ls_item-entry_uom  = ls_scr0001_reads-s_lqua-meins.
    ls_item-item_text  = ls_scr0001_reads-s_lqua-lenum.
    ls_item-costcenter = gv_kostl.
    COLLECT ls_item INTO lt_items.
  ENDLOOP.

  LOOP AT gt_scr0001_nwm_reads INTO ls_scr0001_nwm.
    CLEAR ls_item.
    ls_item-move_type  = gv_bwart_out.
    ls_item-plant      = ls_scr0001_nwm-s_mast-werks.
    ls_item-stge_loc   = gv_lgort_comp.
    ls_item-material   = ls_scr0001_nwm-s_stpo-idnrk.
    ls_item-batch      = ls_scr0001_nwm-s_mchb-charg.
    ls_item-entry_qnt  = ls_scr0001_nwm-menge.
    ls_item-entry_uom  = ls_scr0001_nwm-s_stpo-meins.
    ls_item-costcenter = gv_kostl.
    COLLECT ls_item INTO lt_items.
  ENDLOOP.

  CALL FUNCTION 'ZWM_GOODSMVT_CREATE'
    EXPORTING
      i_code       = '03'
      i_header_txt = scr0001-exidv
      i_bwart      = gv_bwart_out
      it_items     = lt_items
    IMPORTING
      et_mseg      = lt_mseg
      et_messages  = lt_messages
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    PERFORM goodsmvt_cancel USING lt_msegc.
    PERFORM show_message_t USING lt_messages.
    EXIT.
  ENDIF.

  APPEND LINES OF lt_mseg TO lt_msegc.

** Criação TO para Necessidade
***********************************************************************
  LOOP AT lt_mseg INTO ls_mseg WHERE NOT tbnum IS INITIAL.
    EXIT.
  ENDLOOP.

  CLEAR: lt_trite.
  LOOP AT gt_scr0001_reads INTO ls_scr0001_reads.
    CLEAR: ls_mseg.
    READ TABLE lt_mseg
          INTO ls_mseg
          WITH KEY sgtxt = ls_scr0001_reads-lenum
                   matnr = ls_scr0001_reads-s_lqua-matnr
                   charg = ls_scr0001_reads-s_lqua-charg.

    CLEAR: ls_trite.
    ls_trite-tbpos = ls_mseg-tbpos.
    ls_trite-anfme = ls_scr0001_reads-menge.
    ls_trite-altme = ls_scr0001_reads-s_lqua-meins.
    ls_trite-vlenr = ls_scr0001_reads-s_lqua-lenum.
    COLLECT ls_trite INTO lt_trite.
  ENDLOOP.

  CALL FUNCTION 'ZWM_TO_CREATE_TR'
    EXPORTING
      i_lgnum     = gv_lgnum
      i_tbnum     = ls_mseg-tbnum
      it_trite    = lt_trite
      i_squit     = abap_true
      i_commit    = abap_false
    IMPORTING
      et_ltak     = lt_ltak_vb
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    PERFORM goodsmvt_cancel USING lt_msegc.
    PERFORM show_message_t USING lt_messages.
    EXIT.
  ENDIF.

** Lançamento Contablistico
**********************************************************************
  SELECT SINGLE * FROM t001
                  INTO ls_t001
                  WHERE bukrs = gv_bukrs.

  ls_documentheader-username   = sy-uname.
  ls_documentheader-comp_code  = gv_bukrs.
  ls_documentheader-doc_date   = sy-datum.
  ls_documentheader-pstng_date = sy-datum.
  ls_documentheader-fisc_year  = sy-datum(4).
  ls_documentheader-fis_period = sy-datum+4(2).
  ls_documentheader-doc_type   = gv_blart.
  ls_documentheader-ref_doc_no = ls_mseg-mblnr.

  LOOP AT gt_scr0001_nwm INTO ls_scr0001_nwm WHERE s_stpo-postp <> 'L'.
    CLEAR: ls_accountgl.
    lv_itemno = lv_itemno + 1.
    ls_accountgl-itemno_acc = lv_itemno.
    ls_accountgl-gl_account = gv_hkont_c.
    ls_accountgl-doc_type   = gv_blart.
    ls_accountgl-pstng_date = sy-datum.
    ls_accountgl-tax_code   = gv_mwskz.
    ls_accountgl-costcenter = gv_kostl.
    ls_accountgl-material   = ls_scr0001_nwm-s_vepo-matnr.
    APPEND ls_accountgl TO lt_accountgl.

    CLEAR: ls_currencyamount.
    ls_currencyamount-itemno_acc = lv_itemno.
    ls_currencyamount-currency   = ls_t001-waers.
    ls_currencyamount-amt_doccur = ls_scr0001_nwm-s_stpo-preis.
    APPEND ls_currencyamount TO lt_currencyamount.

    CLEAR: ls_accountgl.
    lv_itemno = lv_itemno + 1.
    ls_accountgl-itemno_acc = lv_itemno.
    ls_accountgl-gl_account = gv_hkont_d.
    ls_accountgl-doc_type   = gv_blart.
    ls_accountgl-pstng_date = sy-datum.
    ls_accountgl-tax_code   = gv_mwskz.
    APPEND ls_accountgl TO lt_accountgl.

    CLEAR: ls_currencyamount.
    ls_currencyamount-itemno_acc = lv_itemno.
    ls_currencyamount-currency   = ls_t001-waers.
    ls_currencyamount-amt_doccur = ls_scr0001_nwm-s_stpo-preis * -1.
    APPEND ls_currencyamount TO lt_currencyamount.
  ENDLOOP.


  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = ls_documentheader
    TABLES
      accountgl      = lt_accountgl
      currencyamount = lt_currencyamount
      return         = lt_return.

  LOOP AT lt_return INTO ls_return WHERE type = 'E'
                                      OR type = 'A'.
    MOVE ls_return-type       TO ls_message-msgtyp.
    MOVE ls_return-id         TO ls_message-msgid.
    MOVE ls_return-number     TO ls_message-msgnr.
    MOVE ls_return-message_v1 TO ls_message-msgv1.
    MOVE ls_return-message_v2 TO ls_message-msgv2.
    MOVE ls_return-message_v3 TO ls_message-msgv3.
    MOVE ls_return-message_v4 TO ls_message-msgv4.
    APPEND ls_message TO lt_messages.
    CLEAR  ls_message.
  ENDLOOP.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    PERFORM goodsmvt_cancel USING lt_msegc.
    PERFORM show_message_t  USING lt_messages.
    EXIT.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.


  PERFORM update_zwm013.


  PERFORM reset_all.
ENDFORM.                    " SAVE_0001
*&---------------------------------------------------------------------*
*&      Form  CHECK_SAVE_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM check_save_0001 CHANGING cv_subrc TYPE sysubrc.
  CLEAR: cv_subrc.

  IF gt_scr0001_reads IS INITIAL.
    cv_subrc = 4.
    EXIT.
  ENDIF.

  PERFORM update_0001_quantity USING gt_scr0001_reads
                               CHANGING gt_scr0001
                                        scr0001.

  LOOP AT gt_scr0001 TRANSPORTING NO FIELDS WHERE menge_f > 0.
    cv_subrc = 4.
    EXIT.
  ENDLOOP.
ENDFORM.                    " CHECK_SAVE_0001
*
*&---------------------------------------------------------------------*
*&      Form  GOODSMVT_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MSEG  text
*----------------------------------------------------------------------*
FORM goodsmvt_cancel USING ut_mseg TYPE gty_t_mseg.
  DATA: lt_messages TYPE tab_bdcmsgcoll,
        lt_mseg     TYPE gty_t_mseg.

  DATA: ls_mseg TYPE mseg.

  lt_mseg = ut_mseg.
  SORT lt_mseg BY mblnr mjahr.
  DELETE ADJACENT DUPLICATES FROM lt_mseg COMPARING mblnr mjahr.

  LOOP AT lt_mseg INTO ls_mseg.
    CALL FUNCTION 'ZWM_GOODSMVT_CANCEL'
      EXPORTING
        i_mblnr     = ls_mseg-mblnr
        i_mjahr     = ls_mseg-mjahr
      IMPORTING
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      CALL FUNCTION 'Z_WM_RF_MESSAGE'
        EXPORTING
          it_messages = lt_messages.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GOODSMVT_CANCEL
*&---------------------------------------------------------------------*
*&      Form  MOVE_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0      text
*----------------------------------------------------------------------*
FORM move_0001 USING uv_goto TYPE i.
  DATA: lv_index TYPE sytabix.

  PERFORM set_total_pages_0001.

  lv_index = scr0001-page_a + uv_goto.

  PERFORM goto_0001 USING lv_index.
ENDFORM.                    " MOVE_0001

*&---------------------------------------------------------------------*
*&      Form  SET_TOTAL_PAGES_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_total_pages_0001.
  DESCRIBE TABLE gt_scr0001 LINES scr0001-page_t.
ENDFORM.                    " SET_TOTAL_PAGES_0001

*&---------------------------------------------------------------------*
*&      Form  APPEND_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_0001.
  DATA: ls_scr0001 TYPE gty_scr0001.

  DATA: lv_lines TYPE sytabix.

  ls_scr0001 = scr0001.
  ls_scr0001-appended = abap_true.
  APPEND ls_scr0001 TO gt_scr0001.
  PERFORM new_page_0001.

  DESCRIBE TABLE gt_scr0001 LINES lv_lines.
ENDFORM.                    "append_0001
*&---------------------------------------------------------------------*
*&      Form  GOTO_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_INDEX  text
*----------------------------------------------------------------------*
FORM goto_0001 USING uv_index TYPE sytabix.
  DATA: lt_messages TYPE tab_bdcmsgcoll.

  DATA: ls_scr0001 TYPE gty_scr0001.

  DATA: lv_index   TYPE sytabix.

  PERFORM set_total_pages_0001.
  PERFORM update_0001_quantity USING gt_scr0001_reads
                               CHANGING gt_scr0001
                                        scr0001.

  lv_index = uv_index.

  CHECK NOT gt_scr0001 IS INITIAL.

  IF uv_index <= 0.
    lv_index = 1.
  ELSEIF lv_index > scr0001-page_t.
    lv_index = scr0001-page_t.
*    PERFORM new_page_0001.
    EXIT.
  ENDIF.

  READ TABLE gt_scr0001
        INTO ls_scr0001
        INDEX lv_index.
  CHECK sy-subrc EQ 0.

** Passagem para tela
**********************************************************************
  scr0001 = ls_scr0001.
  scr0001-page_a = lv_index.
  PERFORM set_total_pages_0001.
ENDFORM.                    "goto_0001
*&---------------------------------------------------------------------*
*&      Form  NEW_PAGE_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM new_page_0001.
  PERFORM set_total_pages_0001.

  CLEAR: scr0001-appended.
  scr0001-page_a = scr0001-page_t + 1.
ENDFORM.                    "new_page_0001
*&---------------------------------------------------------------------*
*&      Form  DELETE_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_0001.
  DATA: lv_ret_code     TYPE flag,
        lv_message_var1	TYPE bdc_vtext1.

**  Apagar entrada &?

  lv_message_var1 = scr0001-page_a.
  CONDENSE lv_message_var1 NO-GAPS.

  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWM001'
      message_lang   = sy-langu
      message_type   = 'W'
      message_number = '048'
      message_var1   = lv_message_var1
    IMPORTING
      ret_code       = lv_ret_code.

  CHECK lv_ret_code EQ 'O'.

  DELETE gt_scr0001 INDEX scr0001-page_a.
  PERFORM new_page_0001.
ENDFORM.                    "delete_0001

*&---------------------------------------------------------------------*
*&      Form  RESET_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_all.
  PERFORM reset_0001.
  CLEAR: gt_scr0001, gt_scr0001_reads, gt_scr0001_nwm, gt_scr0001_nwm_reads.
  PERFORM call_screen_0001.
ENDFORM.                    "reset_all

*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_of_selection.
  PERFORM call_screen_0001.
ENDFORM.                    "start_of_selection
*&---------------------------------------------------------------------*
*&      Form  CHECK_0001_EXIDV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_0001_exidv.
  DATA: lt_vepo     TYPE TABLE OF vepo,
        lt_makt     TYPE TABLE OF makt,
        lt_mlgn     TYPE TABLE OF mlgn,
        lt_mast     TYPE TABLE OF mast,
        lt_stko     TYPE TABLE OF stko,
        lt_stpo     TYPE TABLE OF stpo,
        lt_mchb     TYPE TABLE OF mchb,
        lt_stpo_nwm TYPE TABLE OF stpo.

  DATA: ls_makt1       TYPE makt,
        ls_makt2       TYPE makt,
        ls_vekp        TYPE vekp,
        ls_vepo        TYPE vepo,
        ls_mlgn        TYPE mlgn,
        ls_mast        TYPE mast,
        ls_stko        TYPE stko,
        ls_stpo        TYPE stpo,
        ls_zwm013      TYPE zwm013,
        ls_scr0001     TYPE gty_scr0001,
        ls_scr0001_nwm TYPE gty_scr0001.

  DATA: lv_exidv TYPE exidv,
        lv_menge TYPE menge_d,
        lv_preis TYPE cprei.


  FIELD-SYMBOLS: <ls_mchb>        TYPE mchb,
                 <ls_scr0001_nwm> TYPE gty_scr0001.

  lv_exidv = scr0001-exidv.
  PERFORM reset_0001_exidv.
  scr0001-exidv = lv_exidv.

** Valida HU
**********************************************************************
  DO 1 TIMES.
    SELECT SINGLE * FROM vekp
                    INTO ls_vekp
                    WHERE exidv = scr0001-exidv.

    CHECK sy-subrc EQ 0.

    SELECT * FROM vepo
             INTO TABLE lt_vepo
             WHERE venum = ls_vekp-venum.

    CHECK sy-subrc EQ 0.
  ENDDO.

  IF sy-subrc <> 0.
**  Etiqueta & inválida
    PERFORM show_message USING 'E' 'ZWM001' '054' scr0001-exidv '' '' ''.
    PERFORM reset_0001_exidv.
    EXIT.
  ENDIF.

  SELECT * FROM makt
           INTO TABLE lt_makt
           FOR ALL ENTRIES IN lt_vepo
           WHERE matnr = lt_vepo-matnr AND
                 spras = sy-langu.

  CLEAR: ls_vepo.
  READ TABLE lt_vepo
        INTO ls_vepo
        INDEX 1.

  scr0001-s_vepo = ls_vepo.

** Valida se já processado
**********************************************************************
  SELECT SINGLE * FROM zwm013
                  INTO ls_zwm013
                  WHERE armazem = gv_lgnum AND
                        sscc    = scr0001-exidv.

  IF sy-subrc EQ 0.
**  Etiqueta & inválida
    PERFORM show_message USING 'E' 'ZWM001' '055' scr0001-exidv '' '' ''.
    PERFORM reset_0001_exidv.
    EXIT.
  ENDIF.

** Retorna WM no Destino
**********************************************************************
  SELECT SINGLE * FROM mlgn
                  INTO scr0001-s_mlgn_f
                  WHERE matnr = scr0001-s_vepo-matnr AND
                        lgnum = gv_lgnum_in.

  IF sy-subrc <> 0.
**  Material & da Palete & não está associado ao Armazem &
    PERFORM show_message USING 'E' 'ZWM001' '062' scr0001-s_vepo-matnr scr0001-exidv gv_lgnum_in ''.
    PERFORM reset_0001_exidv.
    EXIT.
  ENDIF.

** Retorna Lista Tecnica
**********************************************************************
  DO 1 TIMES.
    SELECT * FROM mast
             INTO TABLE lt_mast
             WHERE matnr = scr0001-s_vepo-matnr AND
                   werks = scr0001-s_vepo-werks AND
                   stlan = '5'.
    CHECK sy-subrc EQ 0.

    SELECT * FROM stko
             INTO TABLE lt_stko
             FOR ALL ENTRIES IN lt_mast
             WHERE stlnr = lt_mast-stlnr AND
                   datuv <= sy-datum.

    CHECK sy-subrc EQ 0.

    SELECT * FROM stpo
             INTO TABLE lt_stpo
             FOR ALL ENTRIES IN lt_stko
             WHERE stlty = lt_stko-stlty AND
                   stlnr = lt_stko-stlnr AND
                   datuv <= sy-datum.

    CHECK sy-subrc EQ 0.

    SELECT * FROM mlgn
             INTO TABLE lt_mlgn
             FOR ALL ENTRIES IN lt_stpo
             WHERE matnr = lt_stpo-idnrk AND
                   lgnum = gv_lgnum.
    SORT lt_mlgn BY matnr.

    CHECK sy-subrc EQ 0.

    SELECT * FROM makt
             APPENDING TABLE lt_makt
             FOR ALL ENTRIES IN lt_mlgn
             WHERE matnr = lt_mlgn-matnr AND
                   spras = sy-langu.

    SORT lt_makt BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_makt.

    SORT lt_mast BY andat DESCENDING.

    LOOP AT lt_mast INTO ls_mast.
      LOOP AT lt_stko INTO ls_stko WHERE stlnr = ls_mast-stlnr.
        CHECK ls_stko-bmeng > 0.

        CLEAR: ls_makt1.
        READ TABLE lt_makt
              INTO ls_makt1
              WITH KEY matnr = ls_mast-matnr
              BINARY SEARCH.

        LOOP AT lt_stpo INTO ls_stpo WHERE stlnr = ls_mast-stlnr.
          CLEAR: ls_makt2.
          READ TABLE lt_makt
                INTO ls_makt2
                WITH KEY matnr = ls_stpo-idnrk
                BINARY SEARCH.

          "Conversões
          CLEAR: lv_menge.
          lv_menge = ( scr0001-s_vepo-vemng * ls_stpo-menge ) / ls_stko-bmeng.
          ls_stpo-menge = lv_menge.

          CLEAR: lv_preis.
          lv_preis = ( scr0001-s_vepo-vemng * ls_stpo-preis ) / ls_stko-bmeng.
          ls_stpo-preis = lv_preis.

          CLEAR: ls_mlgn.
          READ TABLE lt_mlgn
                INTO ls_mlgn
                WITH KEY matnr = ls_stpo-idnrk
                BINARY SEARCH.
          IF sy-subrc <> 0.
            CLEAR: ls_scr0001_nwm.

            "Dados
            ls_scr0001_nwm-maktx1 = ls_makt1-maktx.
            ls_scr0001_nwm-maktx2 = ls_makt2-maktx.
            ls_scr0001_nwm-s_mast = ls_mast.
            ls_scr0001_nwm-s_stpo = ls_stpo.
            ls_scr0001_nwm-s_vepo = ls_vepo.
            APPEND ls_scr0001_nwm TO gt_scr0001_nwm ASSIGNING <ls_scr0001_nwm>.
            <ls_scr0001_nwm>-page_a = sy-tabix.
            CONTINUE.
          ENDIF.

          "Dados
          scr0001-maktx1 = ls_makt1-maktx.
          scr0001-maktx2 = ls_makt2-maktx.
          scr0001-s_mast = ls_mast.
          scr0001-s_stpo = ls_stpo.
          PERFORM append_0001.
        ENDLOOP.
        IF NOT gt_scr0001 IS INITIAL.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF NOT gt_scr0001 IS INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDDO.

  IF gt_scr0001 IS INITIAL.
**  Nenhuma Lista Técnica válida para Material &
    PERFORM show_message USING 'E' 'ZWM001' '056' scr0001-s_vepo-matnr '' '' ''.
    PERFORM reset_0001_exidv.
    EXIT.
  ENDIF.


** Determina Lotes de Stock Não WM
**********************************************************************
  DO 1 TIMES.
    CHECK NOT gt_scr0001_nwm IS INITIAL.

    SELECT * FROM mchb
             INTO TABLE lt_mchb
             FOR ALL ENTRIES IN gt_scr0001_nwm
             WHERE matnr = gt_scr0001_nwm-s_stpo-idnrk AND
                   werks = gt_scr0001_nwm-s_mast-werks AND
                   lgort = gv_lgort_comp.
    CHECK sy-subrc EQ 0.
    DELETE lt_mchb WHERE clabs <= 0.
    SORT lt_mchb BY ersda ASCENDING.

    LOOP AT gt_scr0001_nwm INTO ls_scr0001_nwm WHERE s_stpo-postp = 'L'.

      PERFORM update_0001_quantity USING gt_scr0001_nwm_reads
                                   CHANGING gt_scr0001_nwm
                                            ls_scr0001_nwm.
      CHECK ls_scr0001_nwm-menge_f > 0.

      LOOP AT lt_mchb ASSIGNING <ls_mchb> WHERE matnr = ls_scr0001_nwm-s_stpo-idnrk AND
                                                werks = ls_scr0001_nwm-s_mast-werks AND
                                                clabs > 0.

        IF ls_scr0001_nwm-menge_f <= 0.
          EXIT.
        ENDIF.

        IF ls_scr0001_nwm-menge_f >= <ls_mchb>-clabs.
          ls_scr0001_nwm-menge = <ls_mchb>-clabs.
        ELSE.
          ls_scr0001_nwm-menge = ls_scr0001_nwm-menge_f.
        ENDIF.

        <ls_mchb>-clabs = <ls_mchb>-clabs - ls_scr0001_nwm-menge.
        ls_scr0001_nwm-s_mchb = <ls_mchb>.


        APPEND ls_scr0001_nwm TO gt_scr0001_nwm_reads.
        PERFORM update_0001_quantity USING gt_scr0001_nwm_reads
                                     CHANGING gt_scr0001_nwm
                                              ls_scr0001_nwm.

      ENDLOOP.
    ENDLOOP.

    PERFORM update_0001_quantity USING gt_scr0001_nwm_reads
                             CHANGING gt_scr0001_nwm
                                      ls_scr0001_nwm.

    LOOP AT gt_scr0001_nwm INTO ls_scr0001_nwm  WHERE s_stpo-postp = 'L' AND
                                                      menge_f > 0.
      EXIT.
    ENDLOOP.
    IF sy-subrc EQ 0.
**    Não é possivel consumir o Componente &, stock insuficiente
      PERFORM show_message USING 'E'
                                 'ZWM001'
                                 '061'
                                 ls_scr0001_nwm-s_stpo-idnrk
                                 '' '' ''.

      PERFORM reset_0001_exidv.
      EXIT.
    ENDIF.

  ENDDO.



  PERFORM goto_0001 USING 1.
ENDFORM.                    " CHECK_0001_EXIDV
*&---------------------------------------------------------------------*
*&      Form  RESET_0001_EXIDV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001_exidv.
  PERFORM reset_all.

ENDFORM.                    " RESET_0001_EXIDV
*&---------------------------------------------------------------------*
*&      Form  STATUS_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM status_0001.
  DATA: lv_subrc TYPE sysubrc.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'RLMOB-PSAVE'.
        PERFORM check_save_0001 CHANGING lv_subrc.
        IF lv_subrc <> 0.
          screen-input = 0.
          screen-invisible = 1.
        ENDIF.
      WHEN 'SCR0001-EXIDV'.
        IF NOT scr0001-exidv IS INITIAL.
          screen-input = 0.
        ENDIF.
      WHEN 'SCR0001-LENUM'.
        IF scr0001-exidv IS INITIAL OR
           NOT scr0001-lenum IS INITIAL.
          screen-input = 0.
        ENDIF.

        IF scr0001-s_stpo-menge <= scr0001-menge_l OR
           scr0001-menge_f <= 0.
          screen-input = 0.
        ENDIF.
      WHEN 'SCR0001-MENGE'.
        IF scr0001-lenum IS INITIAL.
          screen-input = 0.
        ENDIF.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " STATUS_0001
*&---------------------------------------------------------------------*
*&      Form  CHECK_0001_LENUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_0001_lenum.
  DATA: lt_lqua TYPE TABLE OF lqua.

  DATA: ls_lqua          TYPE lqua,
        ls_scr0001       TYPE gty_scr0001,
        ls_scr0001_reads TYPE gty_scr0001.

  DATA: lv_lenum     TYPE lenum,
        lv_page      TYPE sytabix,
        lv_last_page TYPE sytabix.

  lv_lenum = scr0001-lenum.
  PERFORM reset_0001_lenum.
  scr0001-lenum = lv_lenum.

  SELECT * FROM lqua
           INTO TABLE lt_lqua
           WHERE lgnum = gv_lgnum AND
                 lenum = scr0001-lenum.

  DELETE lt_lqua WHERE verme <= 0.

  IF lt_lqua IS INITIAL.
**  Palete & inválida
    PERFORM show_message USING 'E' 'ZWM001' '057' scr0001-lenum '' '' ''.
    PERFORM reset_0001_lenum.
    EXIT.
  ENDIF.

** Retorna Entrada
**********************************************************************
  CLEAR: ls_lqua.
  READ TABLE lt_lqua
        INTO ls_lqua
        INDEX 1.

** Remove Quantidade Lida da Palete
**********************************************************************
  LOOP AT gt_scr0001_reads INTO ls_scr0001_reads WHERE lenum = ls_lqua-lenum AND
                                                       s_stpo-idnrk = ls_lqua-matnr.

    ls_lqua-verme = ls_lqua-verme - ls_scr0001_reads-menge.
    IF ls_lqua-verme < 0.
      ls_lqua-verme = 0.
    ENDIF.
  ENDLOOP.

  IF ls_lqua-verme <= 0.
**  Palete & sem stock disponivel
    PERFORM show_message USING 'E' 'ZWM001' '058' scr0001-lenum '' '' ''.
    PERFORM reset_0001_lenum.
    EXIT.
  ENDIF.

** Valida se Material está na Lista
**********************************************************************
  CLEAR: ls_scr0001.
  READ TABLE gt_scr0001
        INTO ls_scr0001
        WITH KEY s_stpo-idnrk = ls_lqua-matnr.
  lv_page = sy-tabix.

  IF sy-subrc <> 0.
**  Material & da Palete & não está associado à Lista Técnica
    PERFORM show_message USING 'E' 'ZWM001' '059' ls_lqua-matnr ls_lqua-lenum '' ''.
    PERFORM reset_0001_lenum.
    EXIT.
  ENDIF.

** Load de Pagina
**********************************************************************
  lv_last_page = scr0001-page_a.
  PERFORM goto_0001 USING lv_page.
  scr0001-lenum  = ls_lqua-lenum.
  scr0001-s_lqua = ls_lqua.

** Check de Quantidades
**********************************************************************
  IF scr0001-menge_f <= 0.
**  Material & totalmente satisfeito
    PERFORM show_message USING 'E' 'ZWM001' '060' ls_lqua-matnr '' '' ''.
    PERFORM reset_0001_lenum.
    PERFORM goto_0001 USING lv_last_page.
    EXIT.
  ENDIF.

  scr0001-menge = scr0001-menge_f.
  IF scr0001-menge > scr0001-s_lqua-verme.
    scr0001-menge = scr0001-s_lqua-verme.
  ENDIF.
ENDFORM.                    " CHECK_0001_LENUM
*&---------------------------------------------------------------------*
*&      Form  RESET_0001_LENUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001_lenum.
  CLEAR: scr0001-lenum, scr0001-s_lqua.
  PERFORM reset_0001_menge.
ENDFORM.                    " RESET_0001_LENUM
*&---------------------------------------------------------------------*
*&      Form  RESET_0001_MENGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001_menge.
  CLEAR: scr0001-menge.
ENDFORM.                    " RESET_0001_MENGE

*&---------------------------------------------------------------------*
*&      Form  UPDATE_0001_QUANTITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_0001_quantity USING ut_scr0001_reads TYPE gty_t_scr0001 "Tabela das Leituras
                          CHANGING ct_scr0001    TYPE gty_t_scr0001 "Tabela de Header
                                   cs_scr0001    TYPE gty_scr0001.  "Struc de Tela
  DATA: ls_scr0001_reads TYPE gty_scr0001,
        ls_scr0001       TYPE gty_scr0001.

  FIELD-SYMBOLS: <ls_scr0001> TYPE gty_scr0001.

  CLEAR: cs_scr0001-menge_l, cs_scr0001-menge_f.

  LOOP AT ct_scr0001 ASSIGNING <ls_scr0001>.
    CLEAR: <ls_scr0001>-menge_l, <ls_scr0001>-menge_f.

    LOOP AT ut_scr0001_reads INTO ls_scr0001_reads WHERE s_stpo-idnrk = <ls_scr0001>-s_stpo-idnrk.
      <ls_scr0001>-menge_l = <ls_scr0001>-menge_l + ls_scr0001_reads-menge.
    ENDLOOP.

    <ls_scr0001>-menge_f = <ls_scr0001>-s_stpo-menge - <ls_scr0001>-menge_l.
    IF <ls_scr0001>-menge_f < 0.
      <ls_scr0001>-menge_f = 0.
    ENDIF.
  ENDLOOP.

  CHECK cs_scr0001-page_a > 0.

  CLEAR: ls_scr0001.
  READ TABLE ct_scr0001
        INTO ls_scr0001
        INDEX cs_scr0001-page_a.

  CHECK sy-subrc EQ 0.

  cs_scr0001-menge_l = ls_scr0001-menge_l.
  cs_scr0001-menge_f = ls_scr0001-menge_f.
ENDFORM.                    " UPDATE_0001_QUANTITY
*&---------------------------------------------------------------------*
*&      Form  APPEND_0001_READS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_0001_reads .
  DATA: ls_scr0001 TYPE gty_scr0001.

  DATA: lv_lines TYPE sytabix.

  ls_scr0001 = scr0001.
  ls_scr0001-appended = abap_true.
  APPEND ls_scr0001 TO gt_scr0001_reads.
ENDFORM.                    " APPEND_0001_READS
*&---------------------------------------------------------------------*
*&      Form  CHECK_0001_MENGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_0001_menge .
  DATA: lv_menge TYPE menge_d.

  lv_menge = scr0001-menge.
  PERFORM reset_0001_menge.
  scr0001-menge = lv_menge.

  IF scr0001-menge > scr0001-s_lqua-verme OR
     scr0001-menge > scr0001-menge_f.
    PERFORM reset_0001_menge.
    scr0001-menge = scr0001-menge_f.
    EXIT.
  ENDIF.

  PERFORM append_0001_reads.
  PERFORM reset_0001_next_read.
ENDFORM.                    " CHECK_0001_MENGE
*&---------------------------------------------------------------------*
*&      Form  RESET_0001_NEXT_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001_next_read.
  DATA: ls_scr0001 TYPE gty_scr0001.

  DATA: lv_ok TYPE flag.

  PERFORM reset_0001_lenum.
  PERFORM update_0001_quantity USING gt_scr0001_reads
                               CHANGING gt_scr0001
                                        scr0001.
  IF scr0001-menge_f <= 0.
    LOOP AT gt_scr0001 INTO ls_scr0001.
      PERFORM goto_0001 USING ls_scr0001-page_a.
      IF scr0001-menge_f > 0.
        lv_ok = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_ok <> abap_true. "Last Read - Auto Submit
      PERFORM save_0001.
    ENDIF.
  ENDIF.
ENDFORM.                    " RESET_0001_NEXT_READ
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXIT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM check_exit_0001 CHANGING cv_subrc TYPE sysubrc.
  DATA: lv_ret_code TYPE flag.

  cv_subrc = 4.

  IF NOT gt_scr0001_reads IS INITIAL.
**  Sair e perder todos os dados lidos?
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWM001'
        message_lang   = sy-langu
        message_type   = 'W'
        message_number = '049'
      IMPORTING
        ret_code       = lv_ret_code.

    CHECK lv_ret_code EQ 'O'.
  ENDIF.

  CLEAR: cv_subrc.

ENDFORM.                    " CHECK_EXIT_0001
*&---------------------------------------------------------------------*
*&      Form  show_message_t
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MESSAGES  text
*----------------------------------------------------------------------*
FORM show_message_t USING ut_messages TYPE tab_bdcmsgcoll.
  DATA: ls_message TYPE bdcmsgcoll.

  CLEAR: ls_message.
  READ TABLE ut_messages
        INTO ls_message
        INDEX 1.


  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = ls_message-msgid
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = ls_message-msgnr
      message_var1   = ls_message-msgv1
      message_var2   = ls_message-msgv2
      message_var3   = ls_message-msgv3
      message_var4   = ls_message-msgv4.
ENDFORM.                    " show_message_t
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZWM013
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_zwm013.
  DATA: ls_zwm013 TYPE zwm013.

  ls_zwm013-armazem     = gv_lgnum_in.
  ls_zwm013-sscc        = scr0001-exidv.
  ls_zwm013-bloqueado   = abap_true.
  ls_zwm013-tipo_palete = scr0001-s_mlgn_f-lety1.

  MODIFY zwm013 FROM ls_zwm013.
  COMMIT WORK AND WAIT.
ENDFORM.                    " UPDATE_ZWM013
*&---------------------------------------------------------------------*
*&      Form  SHOW_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1094   text
*      -->P_1095   text
*      -->P_SCR0001_EXIDV  text
*      -->P_1097   text
*      -->P_1098   text
*      -->P_1099   text
*----------------------------------------------------------------------*
FORM show_message  USING u_msgtyp
                         u_msgid
                         u_msgnr
                         u_msgv1
                         u_msgv2
                         u_msgv3
                         u_msgv4.

  DATA: lt_messages TYPE tab_bdcmsgcoll.

  DATA: ls_message TYPE bdcmsgcoll.

  CLEAR: ls_message.
  ls_message-msgtyp = u_msgtyp.
  ls_message-msgid  = u_msgid.
  ls_message-msgnr  = u_msgnr.
  ls_message-msgv1  = u_msgv1.
  ls_message-msgv2  = u_msgv2.
  ls_message-msgv3  = u_msgv3.
  ls_message-msgv4  = u_msgv4.
  APPEND ls_message TO lt_messages.

  PERFORM show_message_t USING lt_messages.
ENDFORM.                    " SHOW_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  CANCEL_TO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_LTAK  text
*----------------------------------------------------------------------*
FORM cancel_to  USING ut_ltak TYPE pdt_t_ltak_vb.
  DATA: ls_ltak TYPE ltak_vb.

  LOOP AT ut_ltak INTO ls_ltak.

    CALL FUNCTION 'ZWM_TO_CANCEL'
      EXPORTING
        i_lgnum = gv_lgnum
        i_tanum = ls_ltak-tanum
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.

  ENDLOOP.

ENDFORM.                    " CANCEL_TO
