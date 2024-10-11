*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I19 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  user_command_0029  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0029 INPUT.

  CHECK NOT sscc2 IS INITIAL AND
        NOT bin_dest2 IS INITIAL.

  CASE ok_code_0029.
    WHEN 'NEXT'.

      CALL FUNCTION 'ZWM_CONFIRM_TO'
        EXPORTING
          armazem              = xuser-lgnum
          confirm_type         = 'T'
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

          CLEAR : cursorfield, bin_dest2.
          MOVE 'BIN_DEST2' TO cursorfield.
          SET SCREEN '0029'.LEAVE SCREEN.


        ENDIF.
      ELSE.

        break roffd.
        CALL FUNCTION 'ZWM_MODIFY_ZWM011'
          EXPORTING
            armazem            = xuser-lgnum
            to_number          = to_ret
            to_item            = item_ret
            status             = 'P'
          EXCEPTIONS
            error_update_table = 1
            OTHERS             = 2.

        CLEAR: ok_code_0029, l_ltap_inv.
        CLEAR: sscc2, desc, qtd, uni,
               bin_dest1, bin_dest2, cursorfield.
        CLEAR l_ltap_inv.
        REFRESH l_ltap_inv.
        SELECT * INTO TABLE l_ltap_inv
            FROM ltap
                WHERE lgnum = xuser-lgnum AND
                      tanum = to_ret AND
                      pquit <> 'X' AND
                      pvqui = ' '.

        IF NOT l_ltap_inv[] IS INITIAL.
          MOVE 'SSCC2' TO cursorfield.
        ELSE.

          CALL FUNCTION 'ZWM_MODIFY_ZWM011'
            EXPORTING
              armazem            = xuser-lgnum
              to_number          = to_ret
              to_item            = item_ret
              status             = 'T'
            EXCEPTIONS
              error_update_table = 1
              OTHERS             = 2.
** Não tem mais tarefas
          CLEAR: to_ret, item_ret.
          SELECT * FROM zwm010 INTO TABLE tab_zwm010
           WHERE armazem = xuser-lgnum AND
                 equipamento = equipamento_.

          CALL FUNCTION 'ZWM_GET_TO_RET'
            EXPORTING
              armazem           = xuser-lgnum
              tamanho           = lrf_wkqu-devty(5)
            IMPORTING
              nova_to           = tab_zwm011
              tipo_queue        = tipo
            TABLES
              l_zwm010          = tab_zwm010
              return_msg        = return_msg
            EXCEPTIONS
              no_equipment      = 1
              no_work_available = 2
              OTHERS            = 3.
          IF sy-subrc <> 0.

            READ TABLE return_msg INDEX 1.
            IF sy-subrc = 0.
              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
                EXPORTING
                  message_id     = return_msg-msgid
                  message_lang   = sy-langu
                  message_type   = return_msg-msgtyp
                  message_number = return_msg-msgnr
                  message_var1   = return_msg-msgv1.
              CLEAR equipamento_.
** Para saltar para o ecrã correcto
              f3_activo = 'X'.
              IF lrf_wkqu-devty(5) = '16X20'.
                SET SCREEN '0001'.LEAVE SCREEN.
              ELSE.
                SET SCREEN '0002'.LEAVE SCREEN.
              ENDIF.
            ENDIF.

          ELSE.
** Atribuição de nova tarefa
            CALL FUNCTION 'ZWM_CALL_TASK_RET'
              EXPORTING
                armazem     = xuser-lgnum
                ecran       = ecran
                tab_zwm011  = tab_zwm011
                tipo_queue  = tipo
                equipamento = equipamento_.
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " user_command_0029  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_sscc2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc2 INPUT.

  CHECK NOT sscc2 IS INITIAL.

  IF sscc1 <> sscc2.
    WRITE sscc2 TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '113'
        message_var1   = text1.

    CLEAR: ok_code_0029, cursorfield, sscc2.
    MOVE 'SSCC2' TO cursorfield.
    EXIT.
  ENDIF.

  break roffd.
  CLEAR l_ltap_inv.
  REFRESH l_ltap_inv.
  SELECT * INTO TABLE l_ltap_inv
      FROM ltap
          WHERE lgnum = xuser-lgnum AND
                tanum = to_ret AND
                pquit <> 'X' AND
                pvqui = ' '.

  IF NOT l_ltap_inv[] IS INITIAL.
    LOOP AT l_ltap_inv.
      SELECT SINGLE *
          FROM lagp
              WHERE lgnum = l_ltap_inv-lgnum AND
                    lgtyp = l_ltap_inv-nltyp AND
                    lgpla = l_ltap_inv-nlpla.
      IF sy-subrc = 0.
        l_ltap_inv-sorlp = lagp-sorlp.
        MODIFY l_ltap_inv INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    SORT l_ltap_inv BY sorlp.

    READ TABLE l_ltap_inv INDEX 1.

    CALL FUNCTION 'ZWM_MODIFY_ZWM011'
      EXPORTING
        armazem            = xuser-lgnum
        to_number          = l_ltap_inv-tanum
        to_item            = l_ltap_inv-tapos
        status             = 'C'
      EXCEPTIONS
        error_update_table = 1
        OTHERS             = 2.

    MOVE l_ltap_inv-maktx TO desc.
    MOVE l_ltap_inv-vsolm TO qtd.
    MOVE l_ltap_inv-meins TO uni.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = l_ltap_inv-nltyp
        lgpla = l_ltap_inv-nlpla
      IMPORTING
        bin   = bin_dest1.

    CALL FUNCTION 'ZWM_CONFIRM_TO'
      EXPORTING
        armazem              = l_ltap_inv-lgnum
        confirm_type         = 'P'
      TABLES
        return_msg           = return_msg
      CHANGING
        to                   = l_ltap_inv-tanum
        to_item              = l_ltap_inv-tapos
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

        CLEAR : cursorfield.
        SET SCREEN '0029'.LEAVE SCREEN.
      ENDIF.
    ELSE.

      break roffd.
      to_ret = l_ltap_inv-tanum.
      item_ret = l_ltap_inv-tapos.
      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
        EXPORTING
          armazem            = xuser-lgnum
          to_number          = l_ltap_inv-tanum
          to_item            = l_ltap_inv-tapos
          status             = 'P'
        EXCEPTIONS
          error_update_table = 1
          OTHERS             = 2.


*      CLEAR: ok_code_0029, l_ltap_inv.
*      CLEAR: desc, qtd, uni,
*             bin_dest1, bin_dest2, cursorfield.
*      REFRESH l_ltap_inv.
      MOVE 'BIN_DEST2' TO cursorfield.
    ENDIF.
  ENDIF.
  MOVE 'BIN_DEST2' TO cursorfield.

ENDMODULE.                 " check_sscc2  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_bin_destino2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_bin_destino2 INPUT.

  CHECK NOT bin_dest2 IS INITIAL.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = xuser-lgnum
      iv_bin_code = bin_dest2
    IMPORTING
      ev_bin      = bin_dest2
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
      CLEAR: ok_code_0029, cursorfield, bin_dest2.
      MOVE 'BIN_DEST2' TO cursorfield.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.

  IF bin_dest1 <> bin_dest2.
    WRITE bin_dest2 TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '197'
        message_var1   = text1.

    CLEAR: ok_code_0029, cursorfield, bin_dest2.
    MOVE 'BIN_DEST2' TO cursorfield.
    EXIT.
  ENDIF.

ENDMODULE.                 " check_bin_destino2  INPUT
