*&---------------------------------------------------------------------*
*&  Include           ZWMREP0147_F01
*&---------------------------------------------------------------------*


FORM initialization.

  DATA: lt_messages TYPE tab_bdcmsgcoll.

  DATA: lv_lgnum TYPE lgnum.

  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user        = sy-uname
      i_recall      = abap_true
      i_usewm       = abap_true
      i_userf       = abap_true
      i_usemm       = abap_true
      i_useaut      = abap_true
      i_get_lgnum   = abap_true
      i_get_werks   = abap_true
      i_get_lgort   = abap_false
      i_first_werks = abap_true
      i_first_lgort = abap_true
    IMPORTING
      et_messages   = lt_messages
    CHANGING
      c_lgnum       = lv_lgnum
**      c_werks       = gv_werks
**      c_lgort       = gv_lgort
    EXCEPTIONS
      error         = 1
      user_back     = 2
      OTHERS        = 3.

  IF sy-subrc <> 0.
    PERFORM show_message USING lt_messages.
  ENDIF.

  IF NOT lv_lgnum IS INITIAL AND
     p_lgnum IS INITIAL.
    p_lgnum = lv_lgnum.
  ENDIF.


** Valida LGNUM
***********************************************************************
  PERFORM check_lgnum.


ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  CHECK_LGNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_lgnum.
  DATA: ls_t300t TYPE t300t.

  CHECK NOT p_lgnum IS INITIAL.

  SELECT SINGLE * FROM t300t
                  INTO ls_t300t
                  WHERE spras = sy-langu AND
                        lgnum = p_lgnum.

  IF sy-subrc <> 0.
**  & is not a valid Warehouse Number
    MESSAGE i092 DISPLAY LIKE 'E' WITH p_lgnum.
    CLEAR p_lgnum.
    CLEAR sc_lgnum.
    SET PARAMETER ID 'LGN' FIELD ''.
    EXIT.
  ENDIF.

  sc_lgnum = ls_t300t-lnumt.

ENDFORM.                    " CHECK_LGNUM
*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_of_selection.
  PERFORM reset_0001.
  PERFORM call_screen_0001.
ENDFORM.                    " START_OF_SELECTION
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_screen_0001.

  PERFORM get_data_0001.
  IF gt_alv_display_0001 IS INITIAL.
**  Sem dados para parâmetros informados
    MESSAGE s093 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM display_alv_0001.
  PERFORM call_screen USING '0001'.
ENDFORM.                    " CALL_SCREEN_0001
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_USER_SCR01  text
*----------------------------------------------------------------------*
FORM call_screen USING uv_dynnr TYPE sydynnr.

  CHECK sy-dynnr <> uv_dynnr.
  CALL SCREEN uv_dynnr.
ENDFORM.                    " CALL_SCREEN
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_0001.
  DATA: lt_ltbk       TYPE TABLE OF ltbk,
        lt_mara       TYPE HASHED TABLE OF mara WITH UNIQUE KEY matnr,
        lt_makt       TYPE HASHED TABLE OF makt WITH UNIQUE KEY matnr,
        lt_components	TYPE z_wm_cl_management=>t_matnr_prd_components.

  DATA: ls_alv_display_0001 TYPE gty_alv_display_0001,
        ls_ltbk             TYPE ltbk,
        ls_mara             TYPE mara,
        ls_makt             TYPE makt,
        ls_ltbp             TYPE ltbp,
        ls_component        TYPE z_wm_cl_management=>matnr_prd_components.

  DATA: lv_tabix     TYPE sytabix,
        lv_quant     TYPE menge_d,
        lv_quant_new TYPE menge_d.

  FIELD-SYMBOLS: <ls_alv_display_0001> TYPE gty_alv_display_0001.

  PERFORM reset_0001.

** Retorna NT
***********************************************************************
  SELECT * FROM ltbk
           INTO TABLE lt_ltbk
           WHERE lgnum = p_lgnum AND
                 statu IN ( '', 'T' ) AND
                 betyp = 'M'.

  DELETE lt_ltbk WHERE bwlvs <> '934'. " TODO: REVER MOVIMENTO
  CHECK NOT lt_ltbk IS INITIAL.

  SELECT * FROM ltbp
           INTO TABLE gt_ltbp
           FOR ALL ENTRIES IN lt_ltbk
           WHERE lgnum = lt_ltbk-lgnum AND
                 tbnum = lt_ltbk-tbnum.


  LOOP AT lt_ltbk INTO ls_ltbk.
    CLEAR: ls_alv_display_0001.

    CHECK NOT ls_ltbk-tbktx is INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_ltbk-benum
      IMPORTING
        output = ls_alv_display_0001-matnr.

    ls_alv_display_0001-tbnum = ls_ltbk-tbnum.

    TRY.
        ls_alv_display_0001-menge = ls_ltbk-tbktx.
      CATCH  cx_sy_conversion_no_number.
        CONTINUE.
    ENDTRY.


    APPEND ls_alv_display_0001 TO gt_alv_display_0001.
  ENDLOOP.
  CHECK NOT gt_alv_display_0001 IS INITIAL.

** Dados Por Material
**********************************************************************
  SELECT * FROM mara
           INTO TABLE lt_mara
           FOR ALL ENTRIES IN gt_alv_display_0001
           WHERE matnr = gt_alv_display_0001-matnr.

  CHECK NOT lt_mara IS INITIAL.


  SELECT * FROM makt
           INTO TABLE lt_makt
           FOR ALL ENTRIES IN lt_mara
           WHERE matnr = lt_mara-matnr AND
                 spras = sy-langu.

  LOOP AT gt_alv_display_0001 ASSIGNING <ls_alv_display_0001>.
    lv_tabix = sy-tabix.

    CLEAR: ls_mara.
    READ TABLE lt_mara
          INTO ls_mara
          WITH TABLE KEY matnr = <ls_alv_display_0001>-matnr.

    IF sy-subrc <> 0.
      DELETE gt_alv_display_0001 INDEX lv_tabix.
      CONTINUE.
    ENDIF.

    CLEAR: ls_makt.
    READ TABLE lt_makt
          INTO ls_makt
          WITH TABLE KEY matnr = <ls_alv_display_0001>-matnr.

    <ls_alv_display_0001>-maktx = ls_makt-maktx.
    <ls_alv_display_0001>-meins = ls_mara-meins.


** Calcula quantidade em falta
***********************************************************************
    " Determina a quantidade por componente, e baseado na quantidade fornecida, calcula quanto sotock já
    " está fornecido.

    CALL METHOD z_wm_cl_management=>get_componets
      EXPORTING
        i_lgnum       = p_lgnum
        i_werks       = gv_werks
        i_matnr       = <ls_alv_display_0001>-matnr
        i_stlnr       = <ls_alv_display_0001>-stlnr
        i_stlal       = <ls_alv_display_0001>-stlal
        i_menge_prod  = 1
      IMPORTING
        e_stlnr       = <ls_alv_display_0001>-stlnr
        e_stlal       = <ls_alv_display_0001>-stlal
        et_components = lt_components
      EXCEPTIONS
        error         = 1
        OTHERS        = 2.

    CLEAR: lv_quant_new.

    SORT lt_components BY matnr.
    LOOP AT gt_ltbp INTO ls_ltbp WHERE tbnum = <ls_alv_display_0001>-tbnum.

      CLEAR: ls_component.
      READ TABLE lt_components
            INTO ls_component
            WITH KEY matnr = ls_ltbp-matnr
            BINARY SEARCH.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CHECK ls_component-menge > 0.

      lv_quant = floor( ( ls_ltbp-menge - ls_ltbp-tamen ) / ls_component-menge ).
      IF lv_quant > lv_quant_new.
        lv_quant_new = lv_quant.
      ENDIF.
    ENDLOOP.

    IF lv_quant_new < 0.
      lv_quant_new = 0.
    ENDIF.

    IF lv_quant_new < <ls_alv_display_0001>-menge.
      <ls_alv_display_0001>-menge = lv_quant_new.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_DATA_0001
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_for_variant CHANGING c_layout TYPE slis_vari.
  DATA: ls_layout TYPE salv_s_layout_info,
        ls_key    TYPE salv_s_layout_key.

***********************************************************************
  ls_key-report = sy-repid.

  ls_layout = cl_salv_layout_service=>f4_layouts(
                         s_key    = ls_key
                         restrict = if_salv_c_layout=>restrict_none ).

  c_layout = ls_layout-layout.

ENDFORM.                    " F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_0001 .
  IF gref_alv_container_0001 IS BOUND.
    gref_alv_display_0001->refresh( ).
    EXIT.
  ENDIF.

** Container
***********************************************************************
  IF cl_salv_table=>is_offline( ) EQ if_salv_c_bool_sap=>false.
    CREATE OBJECT gref_alv_container_0001
      EXPORTING
        container_name = 'SCR0001-CONTAINER01'.
  ENDIF.

** Cria Output
***********************************************************************
  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          r_container    = gref_alv_container_0001
          container_name = 'SCR0001-CONTAINER01'
        IMPORTING
          r_salv_table   = gref_alv_display_0001
        CHANGING
          t_table        = gt_alv_display_0001.

    CATCH cx_salv_msg.
      EXIT.
  ENDTRY.

*  gref_alv_display->set_screen_status(
*    pfstatus      =  'SALV_STANDARD'
*    report        =  sy-repid
*    set_functions = gref_alv_display->c_functions_all ).

** Configuração
***********************************************************************
  PERFORM set_output_config_0001.

** Funções
***********************************************************************
  PERFORM set_functions_0001.

** Eventos
***********************************************************************
  PERFORM register_events_0001.

** Output
***********************************************************************
  gref_alv_display_0001->display( ).

ENDFORM.                    " DISPLAY_ALV_0001
*&---------------------------------------------------------------------*
*&      Form  RESET_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001 .
  CLEAR: scr0001, gt_ltbp.
  CLEAR: gt_alv_display_0001.
ENDFORM.                    " RESET_0001
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT_CONFIG_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output_config_0001 .
  DATA: lref_functions  TYPE REF TO cl_salv_functions,
        lref_columns    TYPE REF TO cl_salv_columns_table,
        lref_column     TYPE REF TO cl_salv_column_table,
        lref_layout     TYPE REF TO cl_salv_layout,
        lref_selections TYPE REF TO cl_salv_selections,
        lref_display    TYPE REF TO cl_salv_display_settings.

  DATA: ls_key TYPE salv_s_layout_key.

  DATA: lv_stext TYPE scrtext_s,
        lv_mtext TYPE scrtext_m,
        lv_ltext TYPE scrtext_l.

  lref_columns = gref_alv_display_0001->get_columns( ).
  lref_columns->set_optimize( abap_true ).

** Zebra
***********************************************************************
  lref_display = gref_alv_display_0001->get_display_settings( ). "General Display Settings  "get them all first
  lref_display->set_striped_pattern( cl_salv_display_settings=>true ). "zebra stripes

** Cor
***********************************************************************
**  lref_columns->set_color_column( 'T_COLOR' ).

** Layout (Variantes)
***********************************************************************
  lref_layout = gref_alv_display_0001->get_layout( ).

  ls_key-report = sy-repid.
  lref_layout->set_key( ls_key ).
  lref_layout->set_default( abap_true ). " Permitir Gravação
  lref_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

*  IF NOT p_layout IS INITIAL.
*    lref_layout->set_initial_layout( p_layout ).
*  ENDIF.

** Selecções
***********************************************************************
  lref_selections = gref_alv_display_0001->get_selections( ).
  lref_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

** Cell Type
***********************************************************************
*  lref_columns->set_cell_type_column( 'T_CELLTYPE' ).

** Campos Tecnicos
***********************************************************************
*  PERFORM set_technical USING: lref_columns ''.

  TRY.
**      lref_column ?= lref_columns->get_column( 'LGNUM' ).
**      lref_column->set_key( abap_false ).
**
**      lref_column ?= lref_columns->get_column( 'WERKS' ).
**      lref_column->set_key( abap_false ).
**
**      lref_column ?= lref_columns->get_column( 'PALLETIZER' ).
**      lref_column->set_key( abap_false ).
**
**      lref_column ?= lref_columns->get_column( 'EXIDV_O' ).
**      MOVE TEXT-003 TO lv_stext.
**      MOVE TEXT-003 TO lv_mtext.
**      MOVE TEXT-003 TO lv_ltext.
**      lref_column->set_short_text( lv_stext ).
**      lref_column->set_medium_text( lv_mtext ).
**      lref_column->set_long_text( lv_ltext ).
**      lref_column->set_key( abap_false ).


      lref_column ?= lref_columns->get_column( 'TBNUM' ).
      lref_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

      lref_column ?= lref_columns->get_column( 'MENGE' ).
      lref_column->set_quantity_column( 'MEINS' ).
  ENDTRY.

ENDFORM.                    " SET_OUTPUT_CONFIG_0001
*&---------------------------------------------------------------------*
*&      Form  SET_FUNCTIONS_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_functions_0001 .
  DATA: lref_functions TYPE REF TO cl_salv_functions,
        lv_text        TYPE string,
        lv_icon        TYPE string.

  CHECK gref_alv_display_0001 IS BOUND.

** Funções
***********************************************************************
  lref_functions = gref_alv_display_0001->get_functions( ).
  lref_functions->set_all( if_salv_c_bool_sap=>true ).
ENDFORM.                    " SET_FUNCTIONS_0001
*&---------------------------------------------------------------------*
*&      Form  REGISTER_EVENTS_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_events_0001 .
  DATA: lref_events TYPE REF TO cl_salv_events_table.

  lref_events = gref_alv_display_0001->get_event( ).

  CREATE OBJECT gref_alv_events_0001.

  SET HANDLER gref_alv_events_0001->on_link_click FOR lref_events.
ENDFORM.                    " REGISTER_EVENTS_0001

*&---------------------------------------------------------------------*
*&      Form  set_technical
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UREF_COLUMNS  text
*      -->UV_FNAME      text
*----------------------------------------------------------------------*
FORM set_technical USING uref_columns TYPE REF TO cl_salv_columns_table
                         uv_fname     TYPE lvc_fname.

  DATA: lref_column TYPE REF TO cl_salv_column_table.


  TRY.
      lref_column ?= uref_columns->get_column( uv_fname ).
      lref_column->set_technical( abap_true ).
    CATCH: cx_salv_not_found, cx_salv_data_error.
  ENDTRY.
ENDFORM.                    " SET_TECHNICAL

*&---------------------------------------------------------------------*
*&      Form  show_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UT_MESSAGES  text
*----------------------------------------------------------------------*
FORM show_message  USING ut_messages TYPE tab_bdcmsgcoll.
  READ TABLE ut_messages
        INTO DATA(ls_message)
        INDEX 1.


  MESSAGE ID ls_message-msgid TYPE 'I'
                              NUMBER ls_message-msgnr
                              DISPLAY LIKE ls_message-msgtyp
                              WITH ls_message-msgv1 ls_message-msgv2
                                   ls_message-msgv3 ls_message-msgv4.
ENDFORM.                    " SHOW_MESSAGE


*&---------------------------------------------------------------------*
*&      Form  ON_LINK_CLICK_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROW  text
*      -->P_COLUMN  text
*----------------------------------------------------------------------*
FORM on_link_click_0001 USING uv_row    TYPE i
                              uv_column TYPE lvc_fname.

  FIELD-SYMBOLS: <ls_alv_display_0001> TYPE gty_alv_display_0001.

  READ TABLE gt_alv_display_0001
   ASSIGNING <ls_alv_display_0001>
   INDEX uv_row.
  CHECK sy-subrc EQ 0.

  CASE uv_column.
    WHEN 'TBNUM'.
      CALL FUNCTION 'L_TR_DISPLAY'
        EXPORTING
          i_lgnum   = p_lgnum
          i_tbnum   = <ls_alv_display_0001>-tbnum
        EXCEPTIONS
          not_found = 1
          no_data   = 2
          OTHERS    = 3.
  ENDCASE.

ENDFORM.                    " ON_LINK_CLICK_0001
*&---------------------------------------------------------------------*
*&      Form  EXIT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_0001.
  LEAVE TO SCREEN 0.
ENDFORM.                    " EXIT_0001

*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_LINE_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_ALV_DISPLAY_0001  text
*      <--P_LV_ROW  text
*----------------------------------------------------------------------*
FORM get_selected_line_0001 CHANGING ct_alv_display_0001 TYPE gty_t_alv_display_0001.
  DATA: lt_rows TYPE salv_t_row.

  DATA: ls_alv_display_0001 TYPE gty_alv_display_0001.

  DATA: lv_index TYPE sytabix.

  DATA: lref_selections TYPE REF TO cl_salv_selections.

  CLEAR: ct_alv_display_0001.

  gref_alv_display_0001->get_metadata( ).

  lref_selections = gref_alv_display_0001->get_selections( ).

  lt_rows = lref_selections->get_selected_rows( ).

  READ TABLE lt_rows
        INTO lv_index
        INDEX 1.
  CHECK sy-subrc EQ 0.

  LOOP AT lt_rows INTO lv_index.

    READ TABLE gt_alv_display_0001
          INTO ls_alv_display_0001
          INDEX lv_index.
    CHECK sy-subrc EQ 0.

    APPEND ls_alv_display_0001 TO ct_alv_display_0001.

  ENDLOOP.

ENDFORM.                    " GET_SELECTED_LINE_0001
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0001.
  DATA: lv_subrc TYPE sysubrc.


  CASE scr0001-okcode.
    WHEN 'CRIARTO'.
      PERFORM create_to.
    WHEN 'RELOAD'.
      PERFORM reload_0001.
  ENDCASE.

  CLEAR: scr0001-okcode.

ENDFORM.                    " USER_COMMAND_0001
*&---------------------------------------------------------------------*
*&      Form  RELOAD_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reload_0001.
  PERFORM reset_0001.
  PERFORM get_data_0001.

  IF gt_alv_display_0001 IS INITIAL.
    PERFORM exit_0001.
  ELSE.
    gref_alv_display_0001->refresh( refresh_mode = if_salv_c_refresh=>full ).
  ENDIF.
ENDFORM.                    " RELOAD_0001
*&---------------------------------------------------------------------*
*&      Form  CREATE_TO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_to .
  DATA: lt_alv_display_0001 TYPE gty_t_alv_display_0001,
        lt_fields	          TYPE TABLE OF sval,
        lt_messages         TYPE tab_bdcmsgcoll,
        lt_components       TYPE z_wm_cl_management=>t_matnr_prd_components,
        lt_trite            TYPE l03b_trite_t.

  DATA: ls_alv_display_0001 TYPE gty_alv_display_0001,
        ls_field            TYPE sval,
        ls_trite            TYPE l03b_trite,
        ls_ltbp             TYPE ltbp,
        ls_component        TYPE z_wm_cl_management=>matnr_prd_components.

  DATA: lv_lines      TYPE sytabix,
        lv_menge      TYPE menge_d,
        lv_returncode TYPE c,
        lv_tanum      TYPE tanum.

  PERFORM get_selected_line_0001 CHANGING lt_alv_display_0001.
  CHECK NOT lt_alv_display_0001 IS INITIAL.

  DESCRIBE TABLE lt_alv_display_0001 LINES lv_lines.
  IF lv_lines > 1.
    MESSAGE s090 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE lt_alv_display_0001
        INTO ls_alv_display_0001
        INDEX 1.

  ls_field-fieldtext = ls_alv_display_0001-maktx.
  ls_field-tabname   = 'LTBP'.
  ls_field-fieldname = 'MENGE'.
  ls_field-field_obl = abap_true.
  APPEND ls_field TO lt_fields.

  CLEAR: ls_field.
  ls_field-value     = ls_alv_display_0001-meins.
  ls_field-tabname   = 'LTBP'.
  ls_field-fieldname = 'MEINS'.
  ls_field-field_attr = '02'.
  APPEND ls_field TO lt_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      no_value_check  = ' '
      popup_title     = text-001
      start_column    = '5'
      start_row       = '5'
    IMPORTING
      returncode      = lv_returncode
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.


  READ TABLE lt_fields
        INTO ls_field
        INDEX 1.

  lv_menge = ls_field-value.
  CHECK lv_menge >= 1.

  CALL FUNCTION 'ROUND'
    EXPORTING
      input         = lv_menge
    IMPORTING
      output        = lv_menge
    EXCEPTIONS
      input_invalid = 1
      overflow      = 2
      type_invalid  = 3
      OTHERS        = 4.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  IF lv_menge > ls_alv_display_0001-menge.
    MESSAGE s096.
    EXIT.
  ENDIF.

  CALL METHOD z_wm_cl_management=>get_componets
    EXPORTING
      i_lgnum       = p_lgnum
      i_werks       = gv_werks
      i_matnr       = ls_alv_display_0001-matnr
      i_stlnr       = ls_alv_display_0001-stlnr
      i_stlal       = ls_alv_display_0001-stlal
      i_menge_prod  = lv_menge
    IMPORTING
      et_messages   = lt_messages
      et_components = lt_components
    EXCEPTIONS
      error         = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
    PERFORM show_message USING lt_messages.
    EXIT.
  ENDIF.

  READ TABLE gt_ltbp
      TRANSPORTING NO FIELDS
      WITH KEY tbnum = ls_alv_display_0001-tbnum
      BINARY SEARCH.

  LOOP AT gt_ltbp INTO ls_ltbp FROM sy-tabix.
    IF ls_ltbp-tbnum <> ls_alv_display_0001-tbnum.
      EXIT.
    ENDIF.

    CHECK ls_ltbp-tamen < ls_ltbp-menge.

    LOOP AT lt_components INTO ls_component WHERE matnr = ls_ltbp-matnr.
      ls_trite-tbpos = ls_ltbp-tbpos.
      ls_trite-anfme = ls_component-pal_menge.
      ls_trite-altme = ls_component-meins.
      APPEND ls_trite TO lt_trite.
    ENDLOOP.
  ENDLOOP.
  CHECK NOT lt_trite IS INITIAL.

  CALL FUNCTION 'ZWM_TO_CREATE_TR'
    EXPORTING
      i_lgnum     = p_lgnum
      i_tbnum     = ls_alv_display_0001-tbnum
      it_trite    = lt_trite
    IMPORTING
      e_tanum     = lv_tanum
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    PERFORM show_message USING lt_messages.
    EXIT.
  ENDIF.

  PERFORM reload_0001.
ENDFORM.
