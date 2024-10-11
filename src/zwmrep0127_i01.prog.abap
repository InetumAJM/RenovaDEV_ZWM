*&---------------------------------------------------------------------*
*&  Include           ZWMMPRF013_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0001 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      PERFORM clear_fields_0001.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR sy-ucomm.

ENDMODULE.                 " EXIT_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_SCR_SU  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_scr_su INPUT.
  PERFORM check_scr_su.

ENDMODULE.                 " CHECK_SCR_SU  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_SCR_POS_CONF  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_scr_pos_conf INPUT.
  PERFORM check_scr_pos_conf.

ENDMODULE.                 " CHECK_SCR_POS_CONF  INPUT
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
      PERFORM save_ot_do_mm USING scr_su
                                  abap_true.

    WHEN 'NEXT'.
  ENDCASE.
  CLEAR sy-ucomm.

ENDMODULE.                 " USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_LGORT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_LGORT INPUT.
  perform check_lgort.
ENDMODULE.                 " CHECK_LGORT  INPUT
