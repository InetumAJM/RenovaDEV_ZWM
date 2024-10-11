*&---------------------------------------------------------------------*
*&  Include           ZWMREP0148_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
  DATA: lv_icon_name       TYPE iconname,
        lv_button_text(20) TYPE c,
        lv_quickinfo       LIKE smp_dyntxt-quickinfo,
        lv_icon_str(255)   TYPE c.

*  WRITE icon_tools TO sscrfields-functxt_01 AS ICON.

** Add Conf Button
**********************************************************************
*  lv_icon_name = 'ICON_TOOLS'.
*  lv_button_text = ''.
*
*  lv_quickinfo = 'Configurações'.
*
*  CALL FUNCTION 'ICON_CREATE'
*    EXPORTING
*      name       = lv_icon_name
*      text       = lv_button_text
*      info       = lv_quickinfo
*      add_stdinf = 'X'
*    IMPORTING
*      result     = lv_icon_str
*    EXCEPTIONS
*      OTHERS     = 0.
*
*  WRITE lv_icon_str TO sscrfields-functxt_01 AS ICON.
*MOVE text-123 TO sscrfields-functxt_01+5.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_1000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_1000.

  CASE sy-ucomm.

    WHEN 'FC01'.
*      PERFORM configuration.
  ENDCASE.

ENDFORM.                    " USER_COMMAND_1000
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  DATA: lv_tabix      LIKE sy-tabix.
  DATA: lt_zwm080     TYPE zwm080 OCCURS 0 WITH HEADER LINE.

  DATA: lr_process    TYPE RANGE OF zwm080-process WITH HEADER LINE.

** Selecção de dados
**********************************************************************
  IF p_error IS NOT INITIAL.
    CLEAR lr_process.
    lr_process-sign   = 'I'.
    lr_process-option = 'EQ'.
    lr_process-low    = 'E'.
    APPEND lr_process.

  ELSEIF p_proc IS NOT INITIAL.

    CLEAR lr_process.
    lr_process-sign   = 'I'.
    lr_process-option = 'EQ'.
    lr_process-low    = 'S'.
    APPEND lr_process.

    CLEAR lr_process.
    lr_process-sign   = 'I'.
    lr_process-option = 'EQ'.
    lr_process-low    = 'W'.
    APPEND lr_process.
  ENDIF.

** Obter dados
  IF p_job IS NOT INITIAL.
    SELECT *
      FROM zwm080 INTO TABLE lt_zwm080
      WHERE ( process = '' OR process = 'E' OR process = 'W' ).
  ELSE.
    SELECT *
      FROM zwm080 INTO TABLE lt_zwm080
      WHERE exidv   IN s_exidv
      AND   datum   IN s_datum
      AND   process IN lr_process.
  ENDIF.

** Dados para ALV
**********************************************************************
  LOOP AT lt_zwm080.

    MOVE-CORRESPONDING lt_zwm080 TO gs_alv_table.

    IF gs_alv_table-process = 'E'.
      gs_alv_table-icon = icon_led_red.
    ELSEIF gs_alv_table-process = 'W'.
      gs_alv_table-icon = icon_led_yellow.
    ELSEIF gs_alv_table-process = 'S'.
      gs_alv_table-icon = icon_led_green.
    ELSE.
      gs_alv_table-icon = ''.
    ENDIF.

    APPEND gs_alv_table TO gt_alv_table.

  ENDLOOP.

  SORT gt_alv_table BY datum uzeit exidv.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv.

  DATA: ls_header    TYPE treev_hhdr,
        ls_sort      TYPE lvc_s_sort,
        ls_variant   TYPE disvariant,
        ls_list_comm TYPE slis_listheader,
        ls_layout    TYPE lvc_s_layo.

  DATA: lt_exclude   TYPE ui_functions,
        lt_sort      TYPE lvc_t_sort,
        lt_list_comm TYPE slis_t_listheader,
        lt_fieldcat  TYPE lvc_t_fcat.

  DATA: lcl_event_receiver TYPE REF TO lcl_event_receiver.

** Create ALV
**********************************************************************
  CHECK gcl_alv_grid IS INITIAL.

  IF gcl_container IS INITIAL.
    CREATE OBJECT gcl_container
      EXPORTING
        container_name = 'CONTAINER_ALV'.
  ENDIF.

  CREATE OBJECT gcl_alv_grid
    EXPORTING
      i_parent = gcl_container.

** Cabeçalho
  ls_layout-zebra      = abap_true.
  ls_layout-sel_mode   = 'D'.
  ls_layout-cwidth_opt = abap_true. "Optimize

** Variante
*  ls_variant          = us_variant.
  ls_variant-report   = sy-repid.
  ls_variant-username = sy-uname.

** Field catalog
  PERFORM get_fldcat  USING 'GT_ALV_TABLE' 'ZWM_014' gt_alv_table
                      CHANGING lt_fieldcat.

  PERFORM change_fldcat CHANGING lt_fieldcat.

** Display
  CALL METHOD gcl_alv_grid->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = lt_exclude
      i_save               = 'A'
      is_layout            = ls_layout
      is_variant           = ls_variant
    CHANGING
      it_fieldcatalog      = lt_fieldcat
      it_outtab            = gt_alv_table. " Data

** Eventos
***********************************************************************
  CREATE OBJECT lcl_event_receiver.

  SET HANDLER lcl_event_receiver->handle_hotspot_click
          FOR gcl_alv_grid.

  SET HANDLER lcl_event_receiver->handle_user_command
           FOR gcl_alv_grid.

  SET HANDLER lcl_event_receiver->handle_menu_button
           FOR gcl_alv_grid.

  SET HANDLER lcl_event_receiver->handle_toolbar
          FOR gcl_alv_grid.

  CALL METHOD gcl_alv_grid->set_toolbar_interactive.

ENDFORM.                    " INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  GET_FLDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM get_fldcat    USING pu_table_nm  TYPE slis_tabname
                         pu_structure TYPE typename
                         pt_table     TYPE STANDARD TABLE
                CHANGING pt_fieldcat  TYPE lvc_t_fcat.

  DATA: lt_fieldcat_i TYPE slis_t_fieldcat_alv.

** Obtem Field Catalog
***********************************************************************
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = pu_table_nm
      i_structure_name       = pu_structure
      i_inclname             = sy-repid
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_fieldcat_i
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

** Converte o Field Catalog
  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv = lt_fieldcat_i
*     IT_SORT_ALV     =
*     IT_FILTER_ALV   =
*     IS_LAYOUT_ALV   =
    IMPORTING
      et_fieldcat_lvc = pt_fieldcat
*     ET_SORT_LVC     =
*     ET_FILTER_LVC   =
*     ES_LAYOUT_LVC   =
    TABLES
      it_data         = pt_table
    EXCEPTIONS
      it_data_missing = 1
      OTHERS          = 2.

ENDFORM.                    " GET_FLDCAT
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FLDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM change_fldcat  CHANGING pt_fieldcat TYPE lvc_t_fcat.

  FIELD-SYMBOLS: <ls_fcat> TYPE lvc_s_fcat.

** Ajusta o Fieldcatalog
***********************************************************************
  LOOP AT pt_fieldcat ASSIGNING <ls_fcat>.

    CASE <ls_fcat>-fieldname.

      WHEN 'MANDT'.
        <ls_fcat>-no_out = 'X'.

      WHEN 'TIMESTAMP'.
        <ls_fcat>-no_out = 'X'.

      WHEN 'PROCESS'.
        <ls_fcat>-no_out = 'X'.

      WHEN 'ICON'.
        <ls_fcat>-coltext   = 'Status'.
*        <ls_fcat>-hotspot  = 'X'.
        <ls_fcat>-icon      = 'X'.
        <ls_fcat>-outputlen = '4'.

      WHEN 'ERRORMSG'.
        <ls_fcat>-coltext   = 'Mensagem'.
        <ls_fcat>-scrtext_s = 'Mensagem'.
        <ls_fcat>-scrtext_m = 'Mensagem'.
        <ls_fcat>-scrtext_l = 'Mensagem'.

    ENDCASE.

  ENDLOOP.

ENDFORM.                    " CHANGE_FLDCAT
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.

  DATA: lv_tabix TYPE sy-tabix.

** ALV
**********************************************************************
  IF p_job IS INITIAL.
    CALL SCREEN '0001'.

** JOB
**********************************************************************
  ELSE.

    CHECK gv_mod IS NOT INITIAL.

    IF gt_alv_table[] IS INITIAL.
      MESSAGE s000 WITH 'Sem dados para enviar!' DISPLAY LIKE 'W'.
      EXIT.
    ENDIF.

    LOOP AT gt_alv_table INTO gs_alv_table.

      lv_tabix = sy-tabix.

      PERFORM process_su_lock_unlock CHANGING gs_alv_table.


      MODIFY gt_alv_table FROM gs_alv_table INDEX lv_tabix.
    ENDLOOP.

    PERFORM send_email_with_errors.

  ENDIF.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_selection.

  DATA: lv_tabix         LIKE sy-tabix.
  DATA: ls_rows          TYPE lvc_s_row.
  DATA: ls_stable        TYPE lvc_s_stbl.
  DATA: lt_rows          TYPE lvc_t_row.

** Get orders
**********************************************************************
  CALL METHOD gcl_alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  LOOP AT lt_rows INTO ls_rows.

    READ TABLE gt_alv_table INTO gs_alv_table INDEX ls_rows-index.
    CHECK sy-subrc = 0.

    CHECK gs_alv_table-process <> 'S'.

    lv_tabix = sy-tabix.

    PERFORM process_su_lock_unlock CHANGING gs_alv_table.

    IF gs_alv_table-process = 'E'.
      gs_alv_table-icon = icon_led_red.
    ELSEIF gs_alv_table-process = 'W'.
      gs_alv_table-icon = icon_led_yellow.
    ELSEIF gs_alv_table-process = 'S'.
      gs_alv_table-icon = icon_led_green.
    ELSE.
      gs_alv_table-icon = ''.
    ENDIF.

    MODIFY gt_alv_table FROM gs_alv_table INDEX lv_tabix.
  ENDLOOP.

  IF sy-subrc = 0.
    ls_stable-row = 'X'.

    CALL METHOD gcl_alv_grid->refresh_table_display
      EXPORTING
        is_stable = ls_stable.

    MESSAGE s000(0k) WITH 'Registos processados'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXIT_COMMAND_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_command_0101.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL_WITH_ERRORS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email_with_errors.

  DATA: util_emissor  LIKE t001s-usnam.
  DATA: send_request  TYPE REF TO cl_bcs.
  DATA: document      TYPE REF TO cl_document_bcs.
  DATA: sender        TYPE REF TO cl_sapuser_bcs.
  DATA: recipient     TYPE REF TO if_recipient_bcs.
  DATA: bcs_exception TYPE REF TO cx_bcs.
  DATA: t_hex         TYPE solix_tab.
  DATA: html_string   TYPE string.
  DATA: xhtml_string  TYPE xstring.
  DATA: corpo_html    TYPE string.
  DATA: status_mail   TYPE bcs_stml.
  DATA: lv_subject    TYPE so_obj_des.
  DATA: lv_email      TYPE adr6-smtp_addr.
  DATA: lv_parvw      TYPE  parvw.
  DATA: lv_tabix      LIKE sy-tabix.
  DATA: lv_text       TYPE char100.
  DATA: ls_adr6       TYPE adr6.
  DATA: ls_return     TYPE bdcmsgcoll.
  DATA: lt_alv_table  TYPE TABLE OF st_alv_table WITH HEADER LINE.

** Obter dados
**********************************************************************
  LOOP AT gt_alv_table INTO gs_alv_table .
    APPEND gs_alv_table TO lt_alv_table.
  ENDLOOP.

  CHECK lt_alv_table[] IS NOT INITIAL.

** Emissor
**********************************************************************
  TRY.
      send_request = cl_bcs=>create_persistent( ).

      util_emissor = sy-uname.

      sender = cl_sapuser_bcs=>create( util_emissor ).

*     Adiciona emissor
      CALL METHOD send_request->set_sender
        EXPORTING
          i_sender = sender.

** Assunto e Corpo do email
**********************************************************************

** Assunto
      CONCATENATE sy-datum+6(2) '-' sy-datum+4(2) '-' sy-datum(4) '/' sy-uzeit(2) ':' sy-uzeit+2(2) INTO lv_subject.
      CONCATENATE lv_subject '-' 'AA: Erros Bloqueio/Desbloq. SU' INTO lv_subject SEPARATED BY space.

** Corpo email
      CONCATENATE corpo_html '<html>'
      INTO corpo_html SEPARATED BY space.

      CONCATENATE corpo_html 'ERROS:' '<br>' INTO corpo_html SEPARATED BY space.

      LOOP AT lt_alv_table.
        lv_tabix = sy-tabix.

        CONCATENATE corpo_html lt_alv_table-exidv '-' lt_alv_table-tanum '-'
        lt_alv_table-lgpla '-' lt_alv_table-errormsg  '<br>'
        INTO corpo_html SEPARATED BY space.
      ENDLOOP.

      CONCATENATE corpo_html '</html>'
      INTO corpo_html SEPARATED BY space.

      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = corpo_html
        IMPORTING
          buffer = xhtml_string
        EXCEPTIONS
          failed = 1
          OTHERS = 2.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = xhtml_string
        TABLES
          binary_tab = t_hex.

      document = cl_document_bcs=>create_document(
      i_type    = 'HTM'
      i_hex     = t_hex
      i_subject = lv_subject ).

      CALL METHOD send_request->set_document( document ).

** Destinatário
**********************************************************************
*      IF i_email IS NOT INITIAL.
*
*        recipient = cl_cam_address_bcs=>create_internet_address(
*            i_address_string = i_email ).
*
*        CALL METHOD send_request->add_recipient
*          EXPORTING
*            i_recipient = recipient
*            i_express   = 'X'.
*
*      ELSE.
      recipient = cl_distributionlist_bcs=>getu_persistent( i_dliname = gc_dliname
                                                            i_private = space ).
      CALL METHOD send_request->add_recipient
        EXPORTING
          i_recipient = recipient
          i_express   = 'X'.
*      ENDIF.

** Enviar email
**********************************************************************
      status_mail = 'N'.

      CALL METHOD send_request->set_status_attributes
        EXPORTING
          i_requested_status = status_mail
          i_status_mail      = status_mail.

      " Envio imediato
      send_request->set_send_immediately( 'X' ).

      " Envia mail
      CALL METHOD send_request->send( ).

    CATCH cx_bcs INTO bcs_exception.
      CLEAR ls_return.
      ls_return-msgid  = bcs_exception->msgid.
      ls_return-msgnr  = bcs_exception->msgno.
      ls_return-msgtyp = bcs_exception->msgty.
      ls_return-msgv1  = bcs_exception->msgv1.
      ls_return-msgv2  = bcs_exception->msgv2.
      ls_return-msgv3  = bcs_exception->msgv3.
      ls_return-msgv4  = bcs_exception->msgv4.

      MESSAGE ID ls_return-msgid TYPE 'I' NUMBER ls_return-msgnr INTO lv_text
      WITH ls_return-msgv1 ls_return-msgv2 ls_return-msgv3 ls_return-msgv4.

      WRITE : / lv_text.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DELETE_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_selection.

  DATA: lv_rc     TYPE c.
  DATA: lv_tabix  LIKE sy-tabix.
  DATA: ls_rows   TYPE lvc_s_row.
  DATA: ls_stable TYPE lvc_s_stbl.
  DATA: lt_rows   TYPE lvc_t_row.

** Get orders
**********************************************************************
  CALL METHOD gcl_alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  IF lt_rows[] IS NOT INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        defaultoption  = 'N'
        textline1      = 'Deseja eliminar registos?'
        titel          = 'Eliminar'
        cancel_display = 'N'
      IMPORTING
        answer         = lv_rc.

    CHECK lv_rc EQ 'J'.
  ENDIF.

  LOOP AT lt_rows INTO ls_rows.
    READ TABLE gt_alv_table INTO gs_alv_table INDEX ls_rows-index.
    CHECK sy-subrc = 0.

    CHECK gs_alv_table-process <> 'S'.

    DELETE gt_alv_table INDEX sy-tabix.

    DELETE FROM zwm080 WHERE timestamp = gs_alv_table-timestamp
                       AND   lgnum     = gs_alv_table-lgnum
                       AND   exidv     = gs_alv_table-exidv.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDLOOP.

  IF sy-subrc = 0.
    ls_stable-row = 'X'.

    CALL METHOD gcl_alv_grid->refresh_table_display
      EXPORTING
        is_stable = ls_stable.

    MESSAGE s000(0k) WITH 'Registos Eliminados'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ENQUEUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enqueue.

  DATA: lv_key      TYPE keyword-keyword.
  DATA: lv_uname    TYPE uname.

** Criar bloqueio
**********************************************************************
  MOVE 'LOCK_UNLOCK_ZWMREP0148' TO lv_key.

  CLEAR gv_mod.

  DO 10 TIMES.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        keyword_       = lv_key
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      gv_mod = 'X'.
      EXIT.
    ENDIF.

    WAIT UP TO 1 SECONDS.
  ENDDO.

  IF gv_mod IS INITIAL.
    MESSAGE i110 WITH sy-msgv1 sy-repid DISPLAY LIKE 'W'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DEQUEUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dequeue.

  DATA: lv_key TYPE keyword-keyword.

  MOVE 'LOCK_UNLOCK_ZWMREP0148' TO lv_key.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      keyword_ = lv_key.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SU_LOCK_UNLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_ALV_TABLE  text
*----------------------------------------------------------------------*
FORM process_su_lock_unlock  CHANGING ps_alv_table TYPE st_alv_table.

  DATA: lv_text   TYPE char100.
  DATA: ls_zwm080 TYPE zwm080.
  DATA: lt_return TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

** Processar
**********************************************************************
  CHECK ps_alv_table-mblnr IS INITIAL.

  REFRESH lt_return.

  CLEAR ps_alv_table-errormsg.

  CALL FUNCTION 'ZWM_BLOQ_STOCK_SU_WCS'
    EXPORTING
      i_lgnum      = ps_alv_table-lgnum
      i_tanum      = ps_alv_table-tanum
      i_exidv      = ps_alv_table-exidv
      i_lgpla      = ps_alv_table-lgpla
    IMPORTING
      e_mblnr      = ps_alv_table-mblnr
      e_mjahr      = ps_alv_table-mjahr
      e_tanum_pc   = ps_alv_table-tanum_pc
    TABLES
      t_return_msg = lt_return.

  READ TABLE lt_return INDEX 1.
  IF sy-subrc = 0.
    MESSAGE ID lt_return-msgid TYPE 'I' NUMBER lt_return-msgnr INTO lv_text
    WITH lt_return-msgv1 lt_return-msgv2 lt_return-msgv3 lt_return-msgv4.
  ENDIF.

  WRITE : / lv_text.

  IF ps_alv_table-mblnr IS NOT INITIAL.
    ps_alv_table-process = 'S'.
    CLEAR lv_text.

  ELSE.
    IF lt_return-msgtyp = 'W'.
      ps_alv_table-process = 'W'.

    ELSEIF lt_return-msgtyp = 'S'.
      ps_alv_table-process = 'S'.

    ELSEIF lt_return-msgtyp = 'E'.
      ps_alv_table-process = 'E'.
    ENDIF.

    ps_alv_table-errormsg = lv_text.
  ENDIF.

  MOVE-CORRESPONDING ps_alv_table TO ls_zwm080.

  MODIFY zwm080 FROM ls_zwm080.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_row.

  DATA: lv_return_code  TYPE char1.
  DATA: lv_exidv        TYPE exidv.
  DATA: ls_lein         TYPE lein.
  DATA: ls_stable       TYPE lvc_s_stbl.
  DATA: ls_zwm080       TYPE zwm080.
  DATA: lt_fields       LIKE sval OCCURS 0 WITH HEADER LINE.

** Adicionar Registo
**********************************************************************
*  lv_exidv = i_ltak_vb-lznum.
  CLEAR: lt_fields.
  lt_fields-tabname    = 'ZWM080'.
  lt_fields-fieldname  = 'EXIDV'.
  lt_fields-field_obl = 'X'.
  APPEND lt_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      no_value_check  = ' '
      popup_title     = 'Adicionar SSCC'
      start_column    = '15'
      start_row       = '15'
    IMPORTING
      returncode      = lv_return_code
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  CHECK lv_return_code <> 'A'.

  READ TABLE lt_fields WITH KEY tabname   = 'ZWM080'
                                fieldname = 'EXIDV'.
  IF sy-subrc = 0.
    lv_exidv = lt_fields-value.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_exidv
    IMPORTING
      output = lv_exidv.

  SELECT SINGLE *
    FROM lein INTO ls_lein
    WHERE lenum = lv_exidv.

  IF sy-subrc <> 0.
    " SSCC & inválido!
    MESSAGE s113(zwmmsg001) WITH lv_exidv DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF ls_lein-spgru <> '1' AND
     ls_lein-spgru <> '2'.

    " SSCC & não se encontra bloqueado.
    MESSAGE s363(zwmmsg001) WITH lv_exidv DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zwm080 INTO ls_zwm080
    WHERE exidv   = lv_exidv
    AND   process <> 'S'.

  IF sy-subrc = 0.
    MESSAGE s000(0k) WITH 'SSCC tem um registo pendente por processar' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  " Guardar para Processamento de Unlock
  CLEAR ls_zwm080.
  GET TIME STAMP FIELD ls_zwm080-timestamp.

  ls_zwm080-lgnum = ls_lein-lgnum.
  ls_zwm080-exidv = lv_exidv.
  ls_zwm080-lgpla = 'UNLOCK'.
  ls_zwm080-datum = sy-datum.
  ls_zwm080-uzeit = sy-uzeit.
  ls_zwm080-uname = sy-uname.

  MODIFY zwm080 FROM ls_zwm080.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
  ENDIF.

  CLEAR gs_alv_table.
  MOVE-CORRESPONDING ls_zwm080 TO gs_alv_table.
  APPEND gs_alv_table TO gt_alv_table.

  SORT gt_alv_table BY datum uzeit exidv.

  IF sy-subrc = 0.
    ls_stable-row = 'X'.

    CALL METHOD gcl_alv_grid->refresh_table_display
      EXPORTING
        is_stable = ls_stable.

    MESSAGE s000(0k) WITH 'Registo Adicionado'.
  ENDIF.

ENDFORM.
