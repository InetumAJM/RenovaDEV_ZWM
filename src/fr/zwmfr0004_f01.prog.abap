*&---------------------------------------------------------------------*
*&  Include           ZWMFR0004_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN_PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection_screen_pbo.
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'P_AUFNR'.
        screen-input = 1.
      WHEN 'P_MENGE'.
        screen-input = 1.
      WHEN 'P_VHILM'.
        screen-input = 1.
      WHEN 'P_PRINT'.
        screen-input = 1.
      WHEN 'P_COPIES'.
        screen-input = 1.
      WHEN OTHERS.
        screen-input = 0.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

  IF p_aufnr IS INITIAL.
    SET CURSOR FIELD 'P_AUFNR'.
  ELSEIF p_menge IS INITIAL.
    SET CURSOR FIELD 'P_MENGE'.
  ELSEIF p_vhilm IS INITIAL.
    SET CURSOR FIELD 'P_VHILM'.
  ENDIF.
ENDFORM.                    " SELECTION_SCREEN_PBO
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN_PAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection_screen_pai .
  DATA: ls_afko TYPE afko,
        ls_aufk TYPE aufk,
        ls_mlgn TYPE mlgn,
        ls_mara TYPE mara.

** Dados de Centro e Depósito
***********************************************************************
  IF NOT p_werks IS INITIAL.
    SELECT SINGLE name1 FROM t001w
                        INTO sc_werks
                        WHERE werks = p_werks.
  ELSE.
    CLEAR sc_werks.
  ENDIF.

  IF NOT p_lgnum IS INITIAL.
    SELECT SINGLE lnumt FROM t300t
                        INTO sc_lgnum
                        WHERE spras = sy-langu AND
                              lgnum = p_lgnum.
  ELSE.
    CLEAR sc_lgnum.
  ENDIF.

** Dados Ordem
***********************************************************************
  DO 1 TIMES.
    CHECK NOT p_aufnr IS INITIAL.

    SELECT SINGLE * FROM aufk
                    INTO ls_aufk
                    WHERE aufnr = p_aufnr AND
                          werks = p_werks.
    CHECK sy-subrc EQ 0.

    SELECT SINGLE * FROM afko
                    INTO ls_afko
                    WHERE aufnr = p_aufnr.
    CHECK sy-subrc EQ 0.

    p_fevor = ls_afko-fevor.
    p_matnr = ls_afko-plnbez.
    p_gmein = ls_afko-gmein.
  ENDDO.

  IF sy-subrc <> 0.
    CLEAR: p_aufnr,p_fevor, p_matnr, p_gmein,
           p_ean11,p_menge,p_vhilm.
  ENDIF.

** EAN
***********************************************************************
  DO 1 TIMES.
    CHECK NOT p_matnr IS INITIAL AND
          NOT p_gmein IS INITIAL.

    SELECT SINGLE ean11 FROM mean
                        INTO p_ean11
                        WHERE matnr = p_matnr AND
                              meinh = p_gmein AND
                              lfnum IS NOT NULL.
  ENDDO.

** Quantidade
***********************************************************************
  DO 1 TIMES.
    CHECK p_menge IS INITIAL.

    CHECK NOT p_matnr IS INITIAL AND
          NOT p_lgnum IS INITIAL.

    SELECT SINGLE * FROM mlgn
                    INTO ls_mlgn
                    WHERE matnr = p_matnr AND
                          lgnum = p_lgnum.
    CHECK sy-subrc EQ 0.

    p_menge = ls_mlgn-lhmg1.
  ENDDO.

** Material Palete
***********************************************************************
  DO 1 TIMES.
    CHECK NOT p_vhilm IS INITIAL.

    SELECT SINGLE * FROM mara
                    INTO ls_mara
                    WHERE matnr = p_vhilm.
    IF sy-subrc <> 0.

      CLEAR: p_vhilm.
      CONTINUE.
    ENDIF.

    IF ls_mara-mtart <> 'PALT'.
      CLEAR: p_vhilm.
      CONTINUE.
    ENDIF.
  ENDDO.

  IF NOT p_matnr IS INITIAL.
    SELECT SINGLE maktx FROM makt
                        INTO sc_matnr
                        WHERE matnr = p_matnr AND
                              spras = sy-langu.
  ELSE.
    CLEAR: sc_matnr.
  ENDIF.

  IF NOT p_vhilm IS INITIAL.
    SELECT SINGLE maktx FROM makt
                        INTO sc_vhilm
                        WHERE matnr = p_vhilm AND
                              spras = sy-langu.
  ELSE.
    CLEAR sc_vhilm.
  ENDIF.
ENDFORM.                    " SELECTION_SCREEN_PAI
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
  p_werks = 'RFRA'.
  SET PARAMETER ID 'WRK' FIELD p_werks.

  p_lgnum = '150'.
  PERFORM selection_screen_pai.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_of_selection.
  DATA: lv_answer     TYPE c,
        lv_ean128_bc1 TYPE zwm_aux-out_cb_tx,
        lv_ean128_bc2 TYPE zwm_aux-out_cb_tx,
        lv_ean128_bc3 TYPE zwm_aux-out_cb_tx,
        lv_ean128_bc4 TYPE zwm_aux-out_cb_tx,
        lv_ean128_tx1 TYPE zwm_aux-out_cb_tx,
        lv_ean128_tx2 TYPE zwm_aux-out_cb_tx,
        lv_ean128_tx3 TYPE zwm_aux-out_cb_tx,
        lv_ean128_tx4 TYPE zwm_aux-out_cb_tx,
        lv_return     TYPE zwm_aux-retorno,
        lv_return_msg TYPE bapi_msg,
        lv_outstring  TYPE zwm_aux-out_string,
        lv_exidv1     TYPE exidv,
        lv_exidv2     TYPE exidv.

  DATA: lr_exidv TYPE RANGE OF exidv.

  DATA: lr_s_exidv LIKE LINE OF lr_exidv.

  DATA: ls_alv_display_0001 TYPE gty_alv_display_0001.

  IF p_ean11 IS INITIAL OR
     p_fevor IS INITIAL OR
     p_vhilm IS INITIAL OR
     p_aufnr IS INITIAL OR
     p_menge IS INITIAL.

    MESSAGE s065 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = 'Lançar entrada de Mercadoria?'(003)
      text_button_1         = 'Sim'(001)
      text_button_2         = 'Não'(002)
      display_cancel_button = abap_false
    IMPORTING
      answer                = lv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK lv_answer EQ 1.

  CALL FUNCTION 'Z_WMFR_RFC_PRODUCTION_ENTRY_V2'
    EXPORTING
      i_ean11      = p_ean11
      i_line       = p_fevor
      i_vhilm      = p_vhilm
      i_aufnr      = p_aufnr
      i_quantity   = p_menge
      i_simul      = abap_false
      i_noprint    = abap_false
      i_getpdf     = abap_false
      i_getzpl     = abap_false
    IMPORTING
      e_outstring  = lv_outstring
      e_ean128_bc1 = lv_ean128_bc1
      e_ean128_bc2 = lv_ean128_bc2
      e_ean128_bc3 = lv_ean128_bc3
      e_ean128_bc4 = lv_ean128_bc4
      e_ean128_tx1 = lv_ean128_tx1
      e_ean128_tx2 = lv_ean128_tx2
      e_ean128_tx3 = lv_ean128_tx3
      e_ean128_tx4 = lv_ean128_tx4
      e_return     = lv_return
      r_return_msg = lv_return_msg.


** Impressão
***********************************************************************
  DO 1 TIMES.
    CHECK NOT p_print IS INITIAL AND
          NOT p_copies IS INITIAL.

    lv_exidv1 = lv_outstring(20).
    lv_exidv2 = lv_outstring+20(20).

    IF NOT lv_exidv1 IS INITIAL.
      lr_s_exidv-low = lv_exidv1.
      lr_s_exidv-option = 'EQ'.
      lr_s_exidv-sign = 'I'.
      APPEND lr_s_exidv TO lr_exidv.
    ENDIF.

    IF NOT lv_exidv2 IS INITIAL.
      lr_s_exidv-low = lv_exidv2.
      lr_s_exidv-option = 'EQ'.
      lr_s_exidv-sign = 'I'.
      APPEND lr_s_exidv TO lr_exidv.
    ENDIF.

    CHECK NOT lr_exidv IS INITIAL.

    SUBMIT zwmrep0041 WITH p_print = p_print
                      WITH p_copies = p_copies
                      WITH s_sscc IN lr_exidv
                      AND RETURN.
  ENDDO.

** ALV
***********************************************************************
  ls_alv_display_0001-message = '>>>>>>> BC >>>>>>'.
  APPEND ls_alv_display_0001 TO gt_alv_display_0001.
  ls_alv_display_0001-message = lv_ean128_bc1.
  APPEND ls_alv_display_0001 TO gt_alv_display_0001.
  ls_alv_display_0001-message = lv_ean128_bc2.
  APPEND ls_alv_display_0001 TO gt_alv_display_0001.
  ls_alv_display_0001-message = lv_ean128_bc3.
  APPEND ls_alv_display_0001 TO gt_alv_display_0001.
  ls_alv_display_0001-message = lv_ean128_bc4.
  APPEND ls_alv_display_0001 TO gt_alv_display_0001.

  ls_alv_display_0001-message = '>>>>>>> BC TX >>>>>>'.
  APPEND ls_alv_display_0001 TO gt_alv_display_0001.
  ls_alv_display_0001-message = lv_ean128_tx1.
  APPEND ls_alv_display_0001 TO gt_alv_display_0001.
  ls_alv_display_0001-message = lv_ean128_tx2.
  APPEND ls_alv_display_0001 TO gt_alv_display_0001.
  ls_alv_display_0001-message = lv_ean128_tx3.
  APPEND ls_alv_display_0001 TO gt_alv_display_0001.
  ls_alv_display_0001-message = lv_ean128_tx4.
  APPEND ls_alv_display_0001 TO gt_alv_display_0001.

  ls_alv_display_0001-message = '>>>>>>> Mensagens >>>>>>'.
  APPEND ls_alv_display_0001 TO gt_alv_display_0001.
  ls_alv_display_0001-message = lv_return.
  APPEND ls_alv_display_0001 TO gt_alv_display_0001.
  ls_alv_display_0001-message = lv_return_msg.
  APPEND ls_alv_display_0001 TO gt_alv_display_0001.

  PERFORM call_screen_0001.
ENDFORM.                    " START_OF_SELECTION


*&---------------------------------------------------------------------*
*&      Form  call_screen_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_screen_0001.
  PERFORM display_alv_0001.
  PERFORM call_screen USING '0001'.
ENDFORM.                    " CALL_SCREEN_0001

*&---------------------------------------------------------------------*
*&      Form  call_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UV_DYNNR   text
*----------------------------------------------------------------------*
FORM call_screen USING uv_dynnr TYPE sydynnr.

  CHECK sy-dynnr <> uv_dynnr.
  CALL SCREEN uv_dynnr.
ENDFORM.                    " CALL_SCREEN

*&---------------------------------------------------------------------*
*&      Form  display_alv_0001
*&---------------------------------------------------------------------*
*       text
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

** Configuração
***********************************************************************
  PERFORM set_output_config_0001.

** Output
***********************************************************************
  gref_alv_display_0001->display( ).

ENDFORM.                    " DISPLAY_ALV_0001

*&---------------------------------------------------------------------*
*&      Form  reset_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM reset_0001 .
  CLEAR: scr0001.
  CLEAR: gt_alv_display_0001.
ENDFORM.                    " RESET_0001

*&---------------------------------------------------------------------*
*&      Form  set_output_config_0001
*&---------------------------------------------------------------------*
*       text
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

** Selecções
***********************************************************************
  lref_selections = gref_alv_display_0001->get_selections( ).
  lref_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

** Cell Type
***********************************************************************
*  lref_columns->set_cell_type_column( 'T_CELLTYPE' ).

** Campos Tecnicos
***********************************************************************
**  PERFORM set_technical USING: lref_columns 'WERKS',
**                               lref_columns 'LGORT',
**                               lref_columns 'DATUM',
**                               lref_columns 'UZEIT',
**                               lref_columns 'TYPE',
**                               lref_columns 'CWM'.

  TRY.
**      lref_column ?= lref_columns->get_column( 'WARN' ).
**      MOVE text-001 TO lv_stext.
**      MOVE text-001 TO lv_mtext.
**      MOVE text-001 TO lv_ltext.
**      lref_column->set_short_text( lv_stext ).
**      lref_column->set_medium_text( lv_mtext ).
**      lref_column->set_long_text( lv_ltext ).
**      lref_column->set_alignment( if_salv_c_alignment=>centered ).
    CATCH: cx_salv_not_found, cx_salv_data_error.
  ENDTRY.

ENDFORM.                    " SET_OUTPUT_CONFIG_0001

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
*&      Form  exit_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM exit_0001.
  LEAVE TO SCREEN 0.
ENDFORM.                    " EXIT_0001

*&---------------------------------------------------------------------*
*&      Form  status_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM status_0001.
  SET PF-STATUS 'Z001'.
  SET TITLEBAR 'Z001'.
ENDFORM.                    " STATUS_0001
*&---------------------------------------------------------------------*
*&      Form  F4_VHILM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_VHILM  text
*----------------------------------------------------------------------*
FORM f4_vhilm CHANGING cv_vhilm TYPE vhilm.
  DATA: lt_return_tab	TYPE TABLE OF ddshretval.

  DATA: ls_return_tab	TYPE ddshretval.

  DATA: lv_mtart TYPE mtart.

  lv_mtart = 'PALT'.

  SET PARAMETER ID 'MTA' FIELD lv_mtart.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'MARA'
      fieldname  = 'MATNR'
      searchhelp = 'Z_MATPAL_SE'
      shlpparam  = 'MATNR'
    TABLES
      return_tab = lt_return_tab
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CLEAR: lv_mtart.
  SET PARAMETER ID 'MTA' FIELD lv_mtart.

  CLEAR: ls_return_tab.
  READ TABLE lt_return_tab
        INTO ls_return_tab
        INDEX 1.

  CHECK sy-subrc EQ 0.

  cv_vhilm = ls_return_tab-fieldval.
ENDFORM.                    " F4_VHILM
