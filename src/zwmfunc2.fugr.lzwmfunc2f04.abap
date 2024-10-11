*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC2F04 .
*----------------------------------------------------------------------*

*{   INSERT         DEVK907424                                        1
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0005 .
  DATA: lv_read_field    TYPE c LENGTH 2,
        lv_index         TYPE syindex,
        lv_nome(20)      TYPE c,
        lv_subrc         TYPE sysubrc.

  FIELD-SYMBOLS: <lv_rb_tela_0005_botao> LIKE scr0005-te11.

  CASE scr0005-okcode.
    WHEN 'DN'.
      PERFORM check_btn_down_0005 USING lv_subrc.
      CHECK lv_subrc = 0.
      PERFORM status_0005 USING -1.
    WHEN 'UP'.
      PERFORM check_btn_up_0005 USING lv_subrc.
      CHECK lv_subrc = 0.
      PERFORM status_0005 USING 1.
    WHEN 'BACK'.
      CHECK gv_0005_required EQ abap_false.
      <gv_0005_selected_line> = 0.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      IF scr0005-okcode(2) = 'BT'.
        lv_read_field = scr0005-okcode+2.

        lv_index = lv_read_field.

        lv_index = lv_index + ( gv_total_items * ( scr0005-page_a - 1 ) ).

        PERFORM check_opc USING lv_index.
      ENDIF.
  ENDCASE.
ENDFORM.                    " USER_COMMAND_0005

*}   INSERT
*{   INSERT         DEVK907424                                        2
*&---------------------------------------------------------------------*
*&      Form  CHECK_OPC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_INDEX  text
*----------------------------------------------------------------------*
FORM check_opc USING uv_index TYPE sytabix.
  DATA: ls_0005_item  TYPE zrf01_option_select_items.

  DATA: lv_pag_act    TYPE i,
        lv_index_read TYPE sytabix.

  CHECK uv_index > 0.

  READ TABLE gt_0005_items INTO ls_0005_item INDEX uv_index.

  CHECK sy-subrc = 0.
  CHECK NOT ls_0005_item IS INITIAL.

  <gv_0005_selected_key> = ls_0005_item-op_key.
  <gv_0005_selected_text> = ls_0005_item-op_text.
  <gv_0005_selected_line> = uv_index.
  <gs_0005_item> = ls_0005_item.

  LEAVE TO SCREEN 0.
ENDFORM.                    " CHECK_OPC

*}   INSERT
