*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I15 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT23  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit23 INPUT.
  CLEAR: ok_code_0023.
  CLEAR: porta2, pulmao2, num_palete, to_ret, item_ret.

*apaga entrada do dicionario de dados para o user
  DELETE FROM zwm011
  WHERE user_name = sy-uname AND
        status <> 'P'.

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  f3_activo = 'X'.

  SET SCREEN '0001'. LEAVE SCREEN.

ENDMODULE.                 " EXIT23  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0023  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0023 INPUT.

  CHECK NOT pulmao2 IS INITIAL.

  REFRESH return_msg.
  CLEAR return_msg.
  CALL FUNCTION 'ZWM_CONFIRM_TO'
    EXPORTING
      armazem              = armazem_ret
      confirm_type         = 'P'
*     SU                   =
    TABLES
      return_msg           = return_msg
    CHANGING
      to                   = to_ret
      to_item              = item_ret
    EXCEPTIONS
      confirm_type_error   = 1
      to_not_found         = 2
      to_already_confirmed = 3
      to_already_picked    = 4
      to_not_picked        = 5
      wrong_su             = 6
      missing_su           = 7
      error                = 8
      OTHERS               = 9.

  IF sy-subrc <> 0.
    READ TABLE return_msg INDEX 1.
    IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = return_msg-msgid
          message_lang   = sy-langu
          message_type   = return_msg-msgtyp
          message_number = return_msg-msgnr
          message_var1   = return_msg-msgv1
          message_var2   = return_msg-msgv2
          message_var3   = return_msg-msgv3.

      SET SCREEN '0000'.LEAVE SCREEN.
    ENDIF.
  ELSE.
*PROCESSAMENTO OK
*actualizar tabela ZWM011 com STATUS P
    CALL FUNCTION 'ZWM_MODIFY_ZWM011'
      EXPORTING
        armazem            = xuser-lgnum
        to_number          = to_ret
        to_item            = item_ret
        status             = 'P'
      EXCEPTIONS
        error_update_table = 1
        OTHERS             = 2.

*    break roffd.

    IF lrf_wkqu-devty(5) = '16X20'.
      SET SCREEN '0025'.LEAVE SCREEN.
    ELSE.
      SET SCREEN '0026'.LEAVE SCREEN.
    ENDIF.

  ENDIF.

ENDMODULE.                 " USER_COMMAND_0023  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PULMAO_2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pulmao_2 INPUT.
  CLEAR : text1, text2.

  CHECK NOT pulmao2 IS INITIAL.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = xuser-lgnum
      iv_bin_code = pulmao2
    IMPORTING
      ev_bin      = pulmao2
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    READ TABLE lt_messages INTO ls_messages INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_messages-msgid
          message_lang   = sy-langu
          message_type   = ls_messages-msgtyp
          message_number = ls_messages-msgnr
          message_var1   = ls_messages-msgv1
          message_var2   = ls_messages-msgv2
          message_var3   = ls_messages-msgv3
          message_var4   = ls_messages-msgv4.
      CLEAR pulmao2.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.

  IF pulmao1 <> pulmao2.

** 04.03.2005 - ROFFD ** DEL
** Registo da incidência
*    CALL FUNCTION 'ZWM_INSERT_ERROR'
*      EXPORTING
*        armazem    = xuser-lgnum
*        incidencia = '1'
*        posicao    = pulmao2
*      EXCEPTIONS
*        no_commit  = 1
*        OTHERS     = 2.
** 04.03.2005 - ROFFD ** DEL

    WRITE pulmao1 TO text1 LEFT-JUSTIFIED.
    WRITE pulmao2 TO text2 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '036'
        message_var1   = text1
        message_var2   = text2.
    CLEAR pulmao2.
  ENDIF.
ENDMODULE.                 " CHECK_PULMAO_2  INPUT
