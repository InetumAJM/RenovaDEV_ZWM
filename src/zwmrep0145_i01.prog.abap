*&---------------------------------------------------------------------*
*&  Include           ZWMREP0145_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      CALL METHOD tree->free.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'REFRESH'.
      PERFORM refresh.

  ENDCASE.
  CLEAR SY-UCOMM.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
