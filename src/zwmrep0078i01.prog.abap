*&---------------------------------------------------------------------*
*&  Include           ZWMREP0078I01                                    *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  check_sscc  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc INPUT.

  DATA: exidv_aux LIKE vekp-exidv.

  CHECK NOT scr_sscc IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = scr_sscc
    IMPORTING
      output = scr_sscc.

  SELECT SINGLE exidv FROM vekp INTO exidv_aux
          WHERE lgnum EQ lgnum
            AND exidv EQ scr_sscc.

  IF sy-subrc NE 0.

    CLEAR text1.
    WRITE scr_sscc TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '113'
        message_var1   = text1.

    CLEAR: scr_sscc.

  ENDIF.

ENDMODULE.                 " check_sscc  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code_0001.
    WHEN 'NEXT'.
      PERFORM imprime_itens_picking.

    WHEN 'CLR'.
      CLEAR: scr_sscc, scr_printer.

  ENDCASE.

  CLEAR: ok_code_0001.

ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  exit_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_0001 INPUT.

  CASE ok_code_0001.
    WHEN 'BACK'.
      CLEAR: scr_sscc, scr_printer.

      SET SCREEN '0000'.
      LEAVE SCREEN.

  ENDCASE.

ENDMODULE.                 " exit_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_printer  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_printer INPUT.

  DATA: l_lname LIKE tsp03l-lname .

  CHECK NOT scr_printer IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = scr_printer
    IMPORTING
      output = scr_printer.

  SELECT lname UP TO 1 ROWS FROM tsp03l INTO l_lname
          WHERE padest EQ scr_printer.
  ENDSELECT.

  IF sy-subrc NE 0.
    text1 = scr_printer.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '171'
        message_var1   = text1.
    CLEAR: scr_printer.
  ENDIF.

ENDMODULE.                 " check_printer  INPUT
