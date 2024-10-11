*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC2F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  msg_erro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM msg_erro .
  message_length  = strlen( line_out ).

* split message text into lines........................................
  .
  CLEAR: msg1,msg2,msg3,msg4,msg5.

  line_offset = i_line_size - 1.

  WHILE line_count <= i_lines AND mess_pos < message_length.

    line_count_c = line_count.
    CONCATENATE 'msg' line_count_c INTO field_name.

    ASSIGN (field_name) TO <mess_field>.
    line_pos = 0.

    WHILE line_pos < i_line_size AND mess_pos < message_length.
      <mess_field>+line_pos(1) = line_out+mess_pos(1).
      line_pos = line_pos + 1.
      mess_pos = mess_pos + 1.
    ENDWHILE.

    IF  mess_pos < message_length.
      test_char = con_x.
      WHILE ( NOT ( test_char IS INITIAL ) ) AND  ( line_pos > 0 ).

        line_pos = line_pos - 1.
        mess_pos = mess_pos - 1.
        test_char = <mess_field>+line_pos(1).

      ENDWHILE.

      IF line_pos > 0 AND line_pos < line_offset.

        CLEAR <mess_field>.
        from_pos = mess_pos - line_pos.
        length   = line_pos + 1.
        <mess_field> =  line_out+from_pos(length).

      ELSEIF line_pos = 0.
        mess_pos = mess_pos + line_offset.
      ENDIF.
      mess_pos = mess_pos + 1.

      IF NOT ( <mess_field> IS INITIAL ).
        line_count = line_count + 1.
      ENDIF.

    ENDIF.
  ENDWHILE.
ENDFORM.                    " msg_erro

*{   INSERT         DEVK907424                                        1
*&---------------------------------------------------------------------*
*&      Form  CHECK_0005_OPC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_0005_opc.
  DATA: ls_0005_op_key TYPE zrf01_option_select_items.

  DATA: lv_index TYPE sytabix,
        lv_opkey LIKE ls_0005_op_key-op_key.

  READ TABLE gt_0005_items
    WITH KEY op_key = scr0005-opc
    TRANSPORTING NO FIELDS.

  CHECK sy-subrc EQ 0.

  lv_index = sy-tabix.

  CHECK lv_index > 0.

  PERFORM check_opc USING lv_index.
ENDFORM.                    " CHECK_0005_OPC
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BDC_TAB  text
*      -->P_0008   text
*      -->P_0009   text
*      -->P_0010   text
*----------------------------------------------------------------------*
FORM dynpro TABLES bdc_tab STRUCTURE bdcdata
            USING VALUE(dynbegin) VALUE(name) VALUE(value).

  CLEAR bdc_tab.
  IF dynbegin = 'X'.
    bdc_tab-program  = name.
    bdc_tab-dynpro   = value.
    bdc_tab-dynbegin = 'X'.
  ELSE .
    bdc_tab-fnam = name.
    bdc_tab-fval = value.
  ENDIF.
  APPEND bdc_tab.
ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  GET_PARAMETER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_LGNUM  text
*      -->P_0027   text
*      -->P_0028   text
*      -->P_MATNR_PAL  text
*----------------------------------------------------------------------*
FORM get_parameter  USING    whs
                             module
                             param
                             valor.

  IF ti_zwm001[] IS INITIAL.
    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
      EXPORTING
        whs       = whs
      TABLES
        ti_zwm001 = ti_zwm001.
  ENDIF.

  CLEAR zwm001.
  READ TABLE ti_zwm001 WITH KEY      armazem   = whs
                                     processo  = module
                                     parametro = param
                                     BINARY SEARCH.
  IF sy-subrc = 0.
    MOVE ti_zwm001 TO zwm001.
  ENDIF.
  MOVE zwm001-valor TO valor.

ENDFORM.
