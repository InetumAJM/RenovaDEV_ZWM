*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I09 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0015  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0015 INPUT.
  DATA talao(5).
  CLEAR text1.

  CASE ok_code_0015.

    WHEN 'NEXT'.
** Criação da HU
      CHECK NOT material IS INITIAL AND
            NOT ean11 IS INITIAL AND
            NOT quantidade IS INITIAL AND
            NOT unidade IS INITIAL AND
            NOT lote IS INITIAL AND
            NOT pack_material IS INITIAL AND
            NOT n_etiq IS INITIAL.

** Executar o primeiro passo da transferencia entre armazens
      IF ecra_chamador = '0017'.

        CLEAR items.
        REFRESH items.

        items-material = material.
        items-quantidade = quantidade * n_etiq.
        items-uni = unidade.
        items-lote = lote.
        APPEND items.

        CLEAR valor.
        PERFORM get_parameter USING xuser-lgnum
                                'ENTRADA_FABRICA1'
                                'CODE'
                                valor.
        MOVE valor TO code.
        CLEAR valor.
        PERFORM get_parameter USING xuser-lgnum
                                'ENTRADA_FABRICA1'
                                'MOV_S'
                                valor.

        MOVE valor TO mov_mm.
        CLEAR valor.
        PERFORM get_parameter USING xuser-lgnum
                                'ENTRADA_FABRICA1'
                                'PLANT'
                                valor.

        MOVE valor TO plant_o.
        CLEAR valor.

** Seleccionar o deposito de origem da transferencia
        SELECT SINGLE deposito INTO sloc_o
            FROM zwm032
                WHERE armazem = xuser-lgnum AND
                      ot = to_ret.

        CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
          EXPORTING
            lgnum            = xuser-lgnum
            code             = code
            mov_mm           = mov_mm
            testrun          = ' '
            plant_o          = plant_o
            sloc_o           = sloc_o
          IMPORTING
            materialdocument = mblnr
            matdocumentyear  = gjahr
          TABLES
            return_msg       = return_msg
            items            = items
          EXCEPTIONS
            error            = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          LOOP AT return_msg WHERE msgtyp = 'E'.
            MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
                    NUMBER return_msg-msgnr
                    WITH return_msg-msgv1 return_msg-msgv2.
          ENDLOOP.
        ENDIF.

      ENDIF.

      CLEAR : return_msg.
      REFRESH : return_msg, items_hu.

** Criação do LOTE, so no caso das entradas de terceiros

      IF NOT ecra_chamador = '0017'.
        CALL FUNCTION 'ZWM_BATCH_CREATE'
          EXPORTING
            armazem           = xuser-lgnum
            material          = material
            lote              = lote
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
*           printer                  = 'ZCD1'
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

ENDMODULE.                 " USER_COMMAND_0015  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT15  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit15 INPUT.

  CLEAR : ean11, material, quantidade, unidade, pack_material,
          n_etiq, cursorfield, su, posicao_pulmao,kober.

** Só no caso em que vem de um ecrã da entrada de terceiros
  IF ecra_chamador = '0015'.
    IF NOT su IS INITIAL.
      MOVE 'TIPO_PALETE' TO cursorfield.
    ELSE.
      MOVE 'SU' TO cursorfield.
    ENDIF.
    CLEAR ecra_chamador.

  ENDIF.

  SET SCREEN '0000'.
  LEAVE SCREEN.


ENDMODULE.                 " EXIT15  INPUT
