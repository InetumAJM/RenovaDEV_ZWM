*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I05 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0011  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0011 INPUT.
  CHECK NOT MATERIAL_OUTPUT IS INITIAL.

  READ TABLE TAB WITH KEY MATNR = MATERIAL_OUTPUT.
  IF SY-SUBRC <> 0.
    MOVE MATERIAL_OUTPUT TO TEXT1.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        MESSAGE_ID     = 'ZWMMSG001'
        MESSAGE_LANG   = SY-LANGU
        MESSAGE_TYPE   = 'E'
        MESSAGE_NUMBER = '070'
        MESSAGE_VAR1   = TEXT1.
    CLEAR MATERIAL_OUTPUT.
  ELSE.
    SET SCREEN '0000'.
    LEAVE SCREEN.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_0011  INPUT
