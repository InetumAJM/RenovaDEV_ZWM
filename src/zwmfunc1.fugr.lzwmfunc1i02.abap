*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT5  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit5 INPUT.

  CLEAR: ok_code_0005.
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

ENDMODULE.                 " EXIT5  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PORTA2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_porta2 INPUT.
  CLEAR : text1, text2.

  CHECK NOT porta2 IS INITIAL.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = xuser-lgnum
      iv_bin_code = porta2
    IMPORTING
      ev_bin      = porta2
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
      CLEAR porta2.
      REFRESH lt_messages.
      MOVE 'PORTA2' TO cursorfield.
      RETURN.
    ENDIF.
  ENDIF.

  IF porta1 = porta2.

    MOVE 'PULMAO2' TO cursorfield.

  ELSE.

** 04.03.2005 - ROFFD ** DEL
** registo da incidência
*    CALL FUNCTION 'ZWM_INSERT_ERROR'
*      EXPORTING
*        armazem    = xuser-lgnum
*        incidencia = '1'
*        posicao    = porta2
*      EXCEPTIONS
*        no_commit  = 1
*        OTHERS     = 2.
** 04.03.2005 - ROFFD ** DEL

    WRITE porta1 TO text1 LEFT-JUSTIFIED.
    WRITE porta2 TO text2 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '039'
        message_var1   = text1
        message_var2   = text2.
    CLEAR porta2.
    MOVE 'PORTA2' TO cursorfield.

  ENDIF.

ENDMODULE.                 " CHECK_PORTA2  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PULMAO2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pulmao2 INPUT.
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

  ELSE.
    MOVE 'NUM_PALETE' TO cursorfield.
  ENDIF.

ENDMODULE.                 " CHECK_PULMAO2  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_NUM_PALETE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_num_palete INPUT.

  IF num_palete IS INITIAL.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '040'.
    CLEAR num_palete.

  ELSEIF num_palete = 0.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '040'.
    CLEAR num_palete.

  ENDIF.


ENDMODULE.                 " CHECK_NUM_PALETE  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0005 INPUT.

** Tabela auxiliar para actualização da tabela 16
  DATA : BEGIN OF l_zwm016 OCCURS 0.
           INCLUDE STRUCTURE zwm016.
         DATA : END OF l_zwm016.

*  DATA: ECRAN(4).

  REFRESH : return_msg.

  CASE ok_code_0005.
    WHEN 'NEXT'.

** Se todos os campos estiverem preenchidos
      IF NOT porta2 IS INITIAL AND
         NOT pulmao2 IS INITIAL AND
         NOT num_palete IS INITIAL.

** Confirmamos a TO parcialmente

        CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
          EXPORTING
            armazem              = xuser-lgnum
            confirm_type         = 'P'
*           SU                   =
          TABLES
            return_msg           = return_msg
          CHANGING
            to                   = to_ret
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
** Erro
          READ TABLE return_msg INDEX 1.

          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = return_msg-msgtyp
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = return_msg-msgnr.

        ELSE.

** Colocamos a entrada da TO associada ao utilizador a 'PICKED'
          CALL FUNCTION 'ZWM_MODIFY_ZWM011'
            EXPORTING
              armazem            = xuser-lgnum
              to_number          = to_ret
              to_item            = item_ret
              status             = 'P'
            EXCEPTIONS
              error_update_table = 1
              OTHERS             = 2.
          IF sy-subrc <> 0.
** Erro
          ELSE.

** Actualizamos a tabela 16 com a quantidade de paletes descarregadas
            l_zwm016-armazem = xuser-lgnum.
            l_zwm016-user_name = sy-uname.
            l_zwm016-porta = porta2.
            l_zwm016-pulmao = pulmao2.
            l_zwm016-num_paletes = num_palete.
            l_zwm016-to_number = to_ret.
            APPEND l_zwm016.


            CALL FUNCTION 'ZWM_MODIFY_ZWM016'
              EXPORTING
                armazem            = xuser-lgnum
              TABLES
                l_zwm016           = l_zwm016
              EXCEPTIONS
                error_update_table = 1
                OTHERS             = 2.
            IF sy-subrc <> 0.
** Erro
            ELSE.
              CLEAR : l_zwm016, to_ret, item_ret, porta2, porta1,
                      pulmao1, pulmao2, num_palete.
              REFRESH : l_zwm016.
            ENDIF.

          ENDIF.

        ENDIF.



*        BREAK-POINT.
** Caso tenha corrido tudo bem chama de novo a função para atribuição de
** uma nova TO
        CLEAR : tab_zwm011, return_msg.
        REFRESH : return_msg.
** Não sei se será preciso fazer isto de NOVO ????
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

** ERRO
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

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0005  INPUT
