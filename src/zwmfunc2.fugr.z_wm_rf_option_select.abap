FUNCTION z_wm_rf_option_select.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TEXT) OPTIONAL
*"     REFERENCE(IT_ITEMS) TYPE  ZRF01_T_OPTION_SELECT_ITEMS
*"     REFERENCE(I_RIGHT_ALIGN) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_REQUIRED) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_RESET) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_TEXT)
*"     REFERENCE(E_KEY)
*"     REFERENCE(E_LINE) TYPE  SYTABIX
*"     REFERENCE(ES_ITEM) TYPE  ZRF01_OPTION_SELECT_ITEMS
*"----------------------------------------------------------------------
  DATA: lv_line_test TYPE char20,
        lv_index_c   TYPE char3,
        lv_name      TYPE string,
        lv_text      TYPE string,
        lv_scr       TYPE char20.


  DATA: lt_texts TYPE TABLE OF string.

  FIELD-SYMBOLS: <lv_text1>     TYPE char20,
                 <ls_0005_item> TYPE ZRF01_OPTION_SELECT_ITEMS.


  CLEAR: e_text, e_key, e_line, es_item, gt_0005_items,
         gv_0005_right_align.

  IF i_reset EQ abap_true.
    CLEAR gv_0005_reseted.
  ENDIF.

** Assignação de Parametros de Exportação
***********************************************************************
  ASSIGN e_key TO <gv_0005_selected_key>.
  CHECK <gv_0005_selected_key> IS ASSIGNED.

  ASSIGN e_text TO <gv_0005_selected_text>.
  CHECK <gv_0005_selected_text> IS ASSIGNED.

  ASSIGN e_line  TO <gv_0005_selected_line>.
  CHECK <gv_0005_selected_line> IS ASSIGNED.

  ASSIGN es_item TO <gs_0005_item>.
  CHECK <gs_0005_item> IS ASSIGNED.


** Obtem Armazém e Dimensão de Tela
***********************************************************************
  PERFORM user_own_data.

  IF lrf_wkqu-devty(5) = '16X20'.
    lv_scr = '0005'.
  ELSE.
    lv_scr = '0500'.
  ENDIF.

** Separa Texto
************************************************************************
  CALL FUNCTION 'RKD_WORD_WRAP'
    EXPORTING
      textline            = i_text
      delimiter           = space
      outputlen           = 20
    IMPORTING
      out_line1           = gv_0005_text1
      out_line2           = gv_0005_text2
      out_line3           = lv_line_test
    EXCEPTIONS
      outputlen_too_large = 1
      OTHERS              = 2.

  IF NOT lv_line_test IS INITIAL.
    gv_0005_text1 = i_text(20).
    gv_0005_text2 = i_text+20.
  ENDIF.

** Guarda dados
************************************************************************
  gt_0005_items = it_items.
  gv_0005_right_align = i_right_align.
  gv_0005_required = i_required.

** Keys
***********************************************************************
  LOOP AT gt_0005_items ASSIGNING <ls_0005_item>.
    CONDENSE <ls_0005_item>-op_key.
  ENDLOOP.

** Chama Ecran
************************************************************************

  gv_total_items = gc_0005_total_items.
  gv_0005_text_length = 18.
  CALL SCREEN lv_scr.
ENDFUNCTION.
