*&---------------------------------------------------------------------*
*&  Include           ZWMREP0071   _PAI                                *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code_0001.
    WHEN 'CLR'.
      CLEAR : posicao, qtd, uni, qtd_existente, cursorfield,
              ok_code_0001.

    WHEN 'SAVE'.
      CHECK NOT posicao IS INITIAL AND
            NOT qtd IS INITIAL.

** Confirmação da contagem de uma posição
      PERFORM regista_contagem_monitor.

    WHEN 'POS_VAZIA'.
      CHECK NOT posicao IS INITIAL.

** Confirmação da contagem de uma posição a zero
      PERFORM regista_contagem_monitor.

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
      CLEAR : posicao, qtd, quantidade, uni, cursorfield,
              ok_code_0001.
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

  CLEAR: return_msg, l_pos.
  REFRESH: return_msg.

  CHECK NOT posicao IS INITIAL.

  l_pos = posicao.

** Valida posição
  PERFORM check_posicao USING 'X'.

** Descobrir qual a unidade q se encontra associada à
** posição
  PERFORM find_uni CHANGING uni.
  MOVE 'QTD' TO cursorfield.

ENDMODULE.                 " check_posicao  INPUT
