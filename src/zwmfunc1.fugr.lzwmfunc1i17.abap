*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I17 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  status_0027  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0027 OUTPUT.

  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

ENDMODULE.                 " status_0027  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  exit27  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit27 INPUT.

  CLEAR: bin_output_o, su, su1, bin_output_d,
         bin_input, bin_output_r, bin_input_r, quant_in, quant_out,
         bin_output_kober_d.

  CLEAR tab_zwm010.
  REFRESH tab_zwm010.
** Apaga entrada do dicionario de dados para o user
** QUE PROCESSA TO PARA MENSULA OU DE MENSULA
  DELETE FROM zwm011
  WHERE armazem   = xuser-lgnum AND
        user_name = sy-uname AND
        status <> 'P'.

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  f3_activo = 'X'.
  SET SCREEN '0001'.
  LEAVE SCREEN.

ENDMODULE.                 " exit27  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_su_su1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_su_su1 INPUT.

  CHECK NOT su IS INITIAL.

  CLEAR: text1.

  IF su NE su1.
    text1 = su.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '033'
        message_var1   = text1.
    CLEAR su.
    cursorfield = 'SU'.
  ELSE.
    cursorfield = 'BIN_INPUT'.
  ENDIF.

ENDMODULE.                 " check_su_su1  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_BIN_INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_bin_input INPUT.

  CHECK NOT bin_input IS INITIAL.

  CLEAR: text1.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = xuser-lgnum
      iv_bin_code = bin_input
    IMPORTING
      ev_bin      = bin_input
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
      CLEAR bin_input.
      MOVE 'BIN_INPUT' TO cursorfield.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.

  IF bin_input NE bin_output_d.
    text1 = bin_input.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '034'
        message_var1   = text1.
    CLEAR bin_input.
    cursorfield = 'BIN_INPUT'.
  ELSE.

*** Confirmar a TO
    CALL FUNCTION 'ZWM_CONFIRM_TO'
      EXPORTING
        armazem              = xuser-lgnum
        confirm_type         = 'P'
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
        CLEAR: bin_input.
        cursorfield = 'BIN_INPUT'.
      ENDIF.
    ELSE.
** PROCESSAMENTO OK
** Actualizar tabela ZWM011 com STATUS P pq é TO
      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
        EXPORTING
          armazem            = xuser-lgnum
          to_number          = to_ret
          to_item            = item_ret
          status             = 'P'
        EXCEPTIONS
          error_update_table = 1
          OTHERS             = 2.
    ENDIF.

    cursorfield = 'QUANT_IN'.
  ENDIF.

ENDMODULE.                 " check_BIN_INPUT  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_bin_input_r  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_bin_input_r INPUT.

  CHECK NOT bin_input_r IS INITIAL.

  CLEAR: text1.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = xuser-lgnum
      iv_bin_code = bin_input_r
    IMPORTING
      ev_bin      = bin_input_r
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
      CLEAR bin_input_r.
      cursorfield = 'BIN_INPUT_R'.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.

  IF bin_input_r NE bin_output_r.
    text1 = bin_input_r.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '034'
        message_var1   = text1.
    CLEAR bin_input_r.
    cursorfield = 'BIN_INPUT_R'.
  ENDIF.

ENDMODULE.                 " check_bin_input_r  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_00027  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_00027 INPUT.

  CHECK NOT su           IS INITIAL AND
        NOT bin_input    IS INITIAL AND
        NOT quant_in     IS INITIAL.

  IF quant_in NE lqua-gesme.
    CHECK NOT bin_input_r IS INITIAL.
  ENDIF.

  CASE ok_code_0027.
    WHEN 'NEXT'.

** Confirmar a TO
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

          CLEAR : cursorfield.
        ENDIF.
      ELSE.
** Acerto Int
        CLEAR ltap.
        SELECT SINGLE *
            FROM ltap
                WHERE lgnum = xuser-lgnum AND
                      tanum = to_ret AND
                      tapos = item_ret.
        IF sy-subrc = 0.
          IF ltap-nltyp = 'INT'.
            CLEAR to.
            to = ltap-tanum.
            PERFORM acerto_int.
          ENDIF.
        ENDIF.

** PROCESSAMENTO OK
** actualizar tabela ZWM011 com STATUS T pq é TO
        CALL FUNCTION 'ZWM_MODIFY_ZWM011'
          EXPORTING
            armazem            = xuser-lgnum
            to_number          = to_ret
            to_item            = item_ret
            status             = 'T'
          EXCEPTIONS
            error_update_table = 1
            OTHERS             = 2.

        IF sy-subrc = 0.
          FREE: tab_zwm010, return_msg.
          CLEAR: bin_output_o, su, su1, bin_output_d, to_ret,
                 bin_input, bin_output_r, bin_input_r, quant_in,
                 quant_out, cursorfield, ok_code_0027, tab_zwm011,
                 tab_zwm010, return_msg, queue, bin_output_kober_d.

*--------------------------------------------------------------------*
* Início de Alteração - ROFF SDF Carlos Fernandes - 25.02.2016
*--------------------------------------------------------------------*
          IF gv_only_one EQ abap_true.
            LEAVE TO SCREEN 0.
          ENDIF.
*--------------------------------------------------------------------*
* Fim de Alteração - ROFF SDF Carlos Fernandes - 25.02.2016
*--------------------------------------------------------------------*

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

*          CLEAR: zwm010.
*          SELECT * FROM zwm010
*          INTO CORRESPONDING FIELDS OF TABLE tab_zwm010
*          WHERE armazem EQ xuser-lgnum
*            AND equipamento EQ equipamento_.
*
*          break roffd.
*
*          CALL FUNCTION 'ZWM_GET_TO_RET'
*            EXPORTING
*              armazem           = xuser-lgnum
*              tamanho           = lrf_wkqu-devty(5)
*            IMPORTING
*              nova_to           = tab_zwm011
*              tipo_queue        = tipo
*            TABLES
*              l_zwm010          = tab_zwm010
*              return_msg        = return_msg
*            EXCEPTIONS
*              no_equipment      = 1
*              no_work_available = 2
*              OTHERS            = 3.
*
*          IF sy-subrc <> 0.
*            READ TABLE return_msg INDEX 1.
*            IF sy-subrc = 0.
*              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*                EXPORTING
*                  message_id     = return_msg-msgid
*                  message_lang   = sy-langu
*                  message_type   = return_msg-msgtyp
*                  message_number = return_msg-msgnr
*                  message_var1   = return_msg-msgv1.
*
*              CLEAR equipamento_.
*
*              break roffd.
**                SET SCREEN '0000'.LEAVE SCREEN.
*
*              f3_activo = 'X'.
*
*              IF lrf_wkqu-devty(5) = '16X20'.
*                SET SCREEN '0001'.LEAVE SCREEN.
*              ELSE.
*                SET SCREEN '0002'.LEAVE SCREEN.
*              ENDIF.
*            ENDIF.
*          ELSE.
*
*            CALL FUNCTION 'ZWM_CALL_TASK_RET'
*              EXPORTING
*                armazem     = xuser-lgnum
*                ecran       = ecran
*                tab_zwm011  = tab_zwm011
*                tipo_queue  = tipo
*                equipamento = equipamento_.
*          ENDIF.


        ENDIF.
      ENDIF.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " user_command_00027  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_quant_in  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_quant_in INPUT.

  CHECK NOT quant_in IS INITIAL.

  CLEAR: text1.

  IF quant_in NE quant_out.
    text1 = quant_in.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '165'
        message_var1   = text1.
    CLEAR quant_in.
    cursorfield = 'QUANT_IN'.
  ELSE.

    CLEAR: lqua.
    SELECT SINGLE * FROM lqua
    WHERE lgnum EQ xuser-lgnum
      AND lenum EQ su.

    IF sy-subrc EQ 0.
      IF quant_in NE lqua-gesme.
        cursorfield = 'BIN_INPUT_R'.
      ENDIF.
    ELSE.
      lqua-gesme = quant_in.
      cursorfield = 'BIN_INPUT_R'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_quant_in  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_00032  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_00032 INPUT.

  CHECK   NOT bin_input    IS INITIAL AND
          NOT quant_in     IS INITIAL.


  CASE ok_code_0032.
    WHEN 'NEXT'.

** Confirmar a TO
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

          CLEAR : cursorfield.
        ENDIF.
      ELSE.

** PROCESSAMENTO OK
** actualizar tabela ZWM011 com STATUS T pq é TO
        CALL FUNCTION 'ZWM_MODIFY_ZWM011'
          EXPORTING
            armazem            = xuser-lgnum
            to_number          = to_ret
            to_item            = item_ret
            status             = 'T'
          EXCEPTIONS
            error_update_table = 1
            OTHERS             = 2.

        IF sy-subrc = 0.
          FREE: tab_zwm010, return_msg.


          CLEAR:   bin_output_o, bin_output_d, bin_input, quant_in,
                   quant_out, cursorfield, ok_code_0032,tab_zwm011,
                   tab_zwm010, return_msg, queue, bin_output.


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

    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_00032  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_QUANT_IN_32  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_quant_in_32 INPUT.

  CHECK NOT quant_in IS INITIAL.

  CLEAR: text1.

  IF quant_in NE quant_out.
    text1 = quant_in.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '165'
        message_var1   = text1.
    CLEAR quant_in.
    cursorfield = 'QUANT_IN'.
  ELSE.

*** Confirmar a TO
    CALL FUNCTION 'ZWM_CONFIRM_TO'
      EXPORTING
        armazem              = xuser-lgnum
        confirm_type         = 'P'
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
      ENDIF.
      CLEAR quant_in.
      cursorfield = 'QUANT_IN'.
    ELSE.
** PROCESSAMENTO OK
** Actualizar tabela ZWM011 com STATUS P pq é TO
      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
        EXPORTING
          armazem            = xuser-lgnum
          to_number          = to_ret
          to_item            = item_ret
          status             = 'P'
        EXCEPTIONS
          error_update_table = 1
          OTHERS             = 2.
      CLEAR: bin_input.
      cursorfield = 'BIN_INPUT'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_QUANT_IN_32  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_BIN_INPUT_32  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_bin_input_32 INPUT.

  CHECK NOT bin_input IS INITIAL.

  CLEAR: text1.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = xuser-lgnum
      iv_bin_code = bin_input
    IMPORTING
      ev_bin      = bin_input
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
      CLEAR bin_input.
      cursorfield = 'BIN_INPUT'.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.

  IF bin_input NE bin_output_d.
    text1 = bin_input.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '034'
        message_var1   = text1.
    CLEAR bin_input.
    cursorfield = 'BIN_INPUT'.
  ENDIF.

ENDMODULE.                 " CHECK_BIN_INPUT_32  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT32  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit32 INPUT.

  CLEAR:   bin_output_o, bin_output_d,
           bin_input   , quant_in    ,
           quant_out.

  CLEAR tab_zwm010.
  REFRESH tab_zwm010.

** Apaga entrada do dicionario de dados para o user
** QUE PROCESSA TO PARA MENSULA OU DE MENSULA
  DELETE FROM zwm011
  WHERE armazem   = xuser-lgnum AND
        user_name = sy-uname AND
        status <> 'P'.

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  f3_activo = 'X'.
  SET SCREEN '0001'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT32  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0030  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0030 INPUT.

*  DATA: lv_material TYPE matnr.

*  CLEAR: text1, lv_material.

  CLEAR: text1.
  CASE ok_code_0030.

    WHEN 'NEXT'.

** Criação da HU
      CHECK NOT material IS INITIAL AND
            NOT ean11 IS INITIAL AND
            NOT quantidade IS INITIAL AND
            NOT unidade IS INITIAL AND
            NOT lote IS INITIAL AND
            NOT pack_material IS INITIAL AND
            NOT n_etiq IS INITIAL.



*** Executar o primeiro passo da transferencia entre armazens
*      IF ecra_chamador = '0017'.
*
*        CLEAR items.
*        REFRESH items.
*
*        items-material = material.
*        items-quantidade = quantidade * n_etiq.
*        items-uni = unidade.
*        items-lote = lote.
*        APPEND items.
*
*        CLEAR valor.
*        PERFORM get_parameter USING xuser-lgnum
*                                'ENTRADA_FABRICA1'
*                                'CODE'
*                                valor.
*        MOVE valor TO code.
*        CLEAR valor.
*        PERFORM get_parameter USING xuser-lgnum
*                                'ENTRADA_FABRICA1'
*                                'MOV_S'
*                                valor.
*
*        MOVE valor TO mov_mm.
*        CLEAR valor.
*        PERFORM get_parameter USING xuser-lgnum
*                                'ENTRADA_FABRICA1'
*                                'PLANT'
*                                valor.
*
*        MOVE valor TO plant_o.
*        CLEAR valor.
*
*** Seleccionar o deposito de origem da transferencia
*        SELECT SINGLE deposito INTO sloc_o
*            FROM zwm032
*                WHERE armazem = xuser-lgnum AND
*                      ot = to_ret.
*
*        CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
*          EXPORTING
*            lgnum            = xuser-lgnum
*            code             = code
*            mov_mm           = mov_mm
*            testrun          = ' '
*            plant_o          = plant_o
*            sloc_o           = sloc_o
*          IMPORTING
*            materialdocument = mblnr
*            matdocumentyear  = gjahr
*          TABLES
*            return_msg       = return_msg
*            items            = items
*          EXCEPTIONS
*            error            = 1
*            OTHERS           = 2.
*        IF sy-subrc <> 0.
*          LOOP AT return_msg WHERE msgtyp = 'E'.
*            MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
*                    NUMBER return_msg-msgnr
*                    WITH return_msg-msgv1 return_msg-msgv2.
*          ENDLOOP.
*        ENDIF.
*      ENDIF.

      CLEAR : return_msg.
      REFRESH : return_msg, items_hu.

** Criação do LOTE, so no caso das entradas de terceiros
      IF NOT ecra_chamador = '0017'.

*** Confirmar Lote fornecedor e Data de validade
*        IF lote_frn IS NOT INITIAL OR data_val IS NOT INITIAL.
***        Lote de Fornecedor ou data de validade não preenchidos.
***        Deseja continuar?
*          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*            EXPORTING
*              message_id     = 'ZWMMSG001'
*              message_lang   = sy-langu
*              message_type   = 'W'
*              message_number = '279'
*            IMPORTING
*              ret_code       = return.
*          .
*          IF return = 'C'.
*            EXIT.
*          ENDIF.
*        ENDIF.

        CALL FUNCTION 'ZWM_BATCH_CREATE'
          EXPORTING
            armazem           = xuser-lgnum
            material          = material
            lote              = lote
            lote_fornecedor   = lote_frn
            data_validade     = data_val
          TABLES
            return_msg        = return_msg
          EXCEPTIONS
            erro              = 1
            batch_not_created = 2
            OTHERS            = 3.
        IF sy-subrc <> 0.

          IF sy-subrc = 1.
            CLEAR text1.
            WRITE lote TO text1 LEFT-JUSTIFIED.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '083'
                message_var1   = text1.
          ELSEIF sy-subrc = 2.

            READ TABLE return_msg INDEX 1.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = return_msg-msgnr
                message_var1   = return_msg-msgv1.
          ENDIF.

        ENDIF.
      ENDIF.

**   INSERIR CERTIFICADO
*      IF scr_cert IS NOT INITIAL.
*
*        CLEAR lv_textos.
*        REFRESH lt_textos.
*
*        lv_textos-tdline = scr_cert.
*        APPEND lv_textos TO lt_textos.
*
*        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*          EXPORTING
*            input        = material
*          IMPORTING
*            output       = lv_material
*          EXCEPTIONS
*            length_error = 1
*            OTHERS       = 2.
*        IF sy-subrc = 0.
*          CONCATENATE lv_material plant lote
*               INTO lv_fname.
*
**        lv_fname = lt_items-charg.
*          CALL FUNCTION 'CREATE_TEXT'
*            EXPORTING
*              fid       = 'VERM'
*              flanguage = sy-langu
*              fname     = lv_fname
*              fobject   = 'CHARGE'
*            TABLES
*              flines    = lt_textos.
*        ENDIF.
*      ENDIF.

** Preenchimento da tabela de items da HU
      items_hu-material = material.
      items_hu-quantity = quantidade.
      items_hu-unit = unidade.
      items_hu-batch = lote.
      APPEND items_hu.
*      break roffd.
      SELECT SINGLE num_entrada INTO talao
          FROM zwm017
              WHERE armazem = xuser-lgnum AND
                    ebeln = pedido AND
                    ebelp = item AND
                    material = material.

      FREE: itab_sscc.
      CLEAR: itab_sscc.

      DO n_etiq TIMES.
        CALL FUNCTION 'ZWM_CREATE_HU'
          EXPORTING
            warehouse                  = xuser-lgnum
            plant                      = plant
            s_loc                      = lgort
            packing_material           = pack_material
            talao                      = talao
          IMPORTING
            hukey                      = hukey
          TABLES
            return_msg                 = return_msg
            items                      = items_hu
          EXCEPTIONS
            empty_table                = 1
            reference_document_differs = 2
            empty_delivery_item        = 3
            item_not_found             = 4
            OTHERS                     = 5.
        IF sy-subrc <> 0.

          READ TABLE return_msg INDEX 1.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = return_msg-msgid
              message_lang   = sy-langu
              message_type   = return_msg-msgtyp
              message_number = return_msg-msgnr
              message_var1   = return_msg-msgv1.
          EXIT.
        ELSE.

          CLEAR itab_sscc.
          itab_sscc-sscc = hukey.
          APPEND itab_sscc.

** Sucesso - Mostra no ecrã o número da SU
*          WRITE hukey TO text1 LEFT-JUSTIFIED.
*          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*            EXPORTING
*              message_id     = 'ZWMMSG001'
*              message_lang   = sy-langu
*              message_type   = 'E'
*              message_number = '078'
*              message_var1   = text1.

        ENDIF.
      ENDDO.

** Sucesso - Imprime SSCC
** Obter a impressora
      PERFORM get_parameter USING xuser-lgnum
                              'EAN128'
                              pulmao2
                              valor.

      MOVE valor TO l_printer.

      IF l_printer IS INITIAL.
        WRITE pulmao2 TO text1 LEFT-JUSTIFIED.
        WRITE 'ZCD1' TO text2 LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '173'
            message_var1   = text1
            message_var2   = text2.

        l_printer = 'ZCD1'.
      ENDIF.

** Imprimir EAN128
      CALL FUNCTION 'ZWM_IMPRIME_EAN128'
        EXPORTING
          printer                  = l_printer
*         printer                  = 'ZCD1'
        TABLES
          sscc                     = itab_sscc
        EXCEPTIONS
          impressora_nao_existe    = 1
          sscc_nao_existe          = 2
          sscc_com_impressao_grupo = 3
          OTHERS                   = 4.
      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF ecra_chamador = '0017'.
        CLEAR : ean11, material, quantidade, unidade, pack_material,
          n_etiq, cursorfield,lote.

      ELSE.
** Apagar todos os campos do ecrã
        CLEAR : ean11, material, quantidade, unidade, pack_material,
                n_etiq, cursorfield.
      ENDIF.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0030  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT30  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit30 INPUT.
  CLEAR : lote_frn, data_val.
  IF lrf_wkqu-devty(5) = '16X20'.
    SET SCREEN '0015'. LEAVE SCREEN.
  ELSE.
    SET SCREEN '0016'.LEAVE SCREEN.
  ENDIF.
ENDMODULE.                 " EXIT30  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_BIN_OUTPUT_32  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_bin_output_32 INPUT.
  CHECK NOT bin_output IS INITIAL.

  CLEAR: text1.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = xuser-lgnum
      iv_bin_code = bin_output
    IMPORTING
      ev_bin      = bin_output
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
      CLEAR bin_output.
      cursorfield = 'BIN_OUTPUT'.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.

  IF bin_output NE bin_output_o.
    text1 = bin_output.
**  Erro! Posição & invalida!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '034'
        message_var1   = text1.
    CLEAR bin_output.
    cursorfield = 'BIN_OUTPUT'.
  ELSE.
    cursorfield = 'QUANT_IN'.
  ENDIF.

ENDMODULE.                 " CHECK_BIN_OUTPUT_32  INPUT
