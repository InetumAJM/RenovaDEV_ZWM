*&---------------------------------------------------------------------*
*&  Include           ZWMREP0072_PAI                                   *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code_0001.
    WHEN 'CLR'.
      CLEAR : posicao, cursorfield, ok_code_0001.

    WHEN OTHERS. "'SAVE' or 'PESQ'.
      CHECK NOT posicao IS INITIAL.

      SET PARAMETER ID 'LGT' FIELD l_lgtyp.
      SET PARAMETER ID 'LGP' FIELD l_lgpla.
      CALL TRANSACTION 'ZWM_LM51'.
*      SUBMIT zrlmob003 AND RETURN.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  exit_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_0001 INPUT.

  CASE ok_code_0001.
    WHEN 'BACK'.
      CLEAR : posicao, cursorfield, ok_code_0001.
      SET SCREEN '0000'.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                 " exit_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_posicao  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_posicao INPUT.

  CHECK NOT posicao IS INITIAL.

** Valida posição
  PERFORM check_posicao.

ENDMODULE.                 " check_posicao  INPUT
