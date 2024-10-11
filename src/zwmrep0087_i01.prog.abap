*&---------------------------------------------------------------------*
*&  Include           ZWMREP0087_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0001 INPUT.
  PERFORM exit_command.
ENDMODULE.                 " EXIT_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_LGORT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_lgort INPUT.
  PERFORM check_lgort.
ENDMODULE.                 " CHECK_LGORT  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_werks INPUT.
  PERFORM check_werks.
ENDMODULE.                 " CHECK_WERKS  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_SCR_SU  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_scr_su INPUT.
  PERFORM check_scr_su.
ENDMODULE.                 " CHECK_SCR_SU  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE sy-ucomm.
    WHEN 'CLR'.
      PERFORM clear_fields_0001.
    WHEN 'SAVE'.
      PERFORM trf_out.
    WHEN 'LIST'.
      PERFORM list.
    WHEN 'PGDN'.
      PERFORM pgdn.
    WHEN 'PGUP'.
      PERFORM pgup.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " USER_COMMAND_0001  INPUT
