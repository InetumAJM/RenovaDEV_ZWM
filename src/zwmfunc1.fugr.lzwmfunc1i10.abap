*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I10 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT17  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit17 INPUT.

  CASE ok_code_0017.

    WHEN 'BACK'.
      CLEAR: ped_compra, pedido, item, pack_material,
             quantidade, unidade, material, lote, plant,
             lgort, tipo_palete,su,incidencia_terceiros,
             posicao_pulmao,kober.

      SET SCREEN '0000'.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.                 " EXIT17  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0017  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0017 INPUT.


  REFRESH: items, mat_struct, x_zwm013, return_msg.
  CLEAR: items, mat_struct, x_zwm013, return_msg.
  CLEAR: valor, mblnr, gjahr, code, mov_mm, mov_wm,
         e_to, s_type, certificado, parametro, pulmao,kober.

  CASE ok_code_0017.

    WHEN 'PRINT'.
*   FIM VERIFICAR SE CONFERIU TODAS AS PALETES
      SELECT SINGLE *
          FROM zwm016
              WHERE armazem = xuser-lgnum
                AND user_name = sy-uname
                AND porta = porta1.
*                AND pulmao = pulmao2.

      IF zwm016-num_paletes = zwm016-palete_conf.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '079'.

        CLEAR: pack_material, quantidade, unidade, material,
               plant, lgort, tipo_palete, lote, su, posicao_pulmao,kober.

      ELSE.
        CLEAR: pack_material, quantidade, unidade, material,
               plant, lgort, lote, tipo_palete,posicao_pulmao,kober.

        ecra_chamador = '0017'.
        IF lrf_wkqu-devty(5) = '16X20'.
          CALL SCREEN '0015'.
        ELSE.
          CALL SCREEN '0016'.
        ENDIF.
      ENDIF.

    WHEN 'INCID'.
*   registar incidencias
*      CHECK NOT su IS INITIAL.

** ecra que chama o ecra de incidencias
      ecra_chamador = '0017'.
      IF lrf_wkqu-devty(5) = '16X20'.
        CALL SCREEN '0010'.
      ELSE.
        CALL SCREEN '0009'.
      ENDIF.

    WHEN 'CONF'.
*   entrada de MM
      CHECK NOT su IS INITIAL AND NOT posicao_pulmao IS INITIAL.
*   verificar se esta a conferir paletes a mais do que o
*   que foi registado na descarga fisica
      SELECT SINGLE *
          FROM zwm016
              WHERE armazem = xuser-lgnum
                AND user_name = sy-uname
                AND porta = porta1.
*                    pulmao = pulmao2.

      IF zwm016-num_paletes = zwm016-palete_conf.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '079'.

        CLEAR: pack_material, quantidade, unidade, material,
               lote, plant, lgort, tipo_palete, su, posicao_pulmao,kober.
      ELSE.

** Criação do LOTE

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

** Criação da transferencia de material
** fabrica 1(a) para o armazem(cd)

        items-material = material.
        items-quantidade = quantidade.
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
                                'MOV_E'
                                valor.

        MOVE valor TO mov_mm.
        CLEAR valor.
        PERFORM get_parameter USING xuser-lgnum
                                'GERAL'
                                'PLANT'
                                valor.

        MOVE valor TO plant_o.
        CLEAR valor.
*        PERFORM get_parameter USING xuser-lgnum
*                                'GERAL'
*                                'LGORT'
*                                valor.
*
*        MOVE valor TO sloc_o.
*        CLEAR valor.

** Seleccionar o deposito de origem da transferencia
        SELECT SINGLE deposito INTO sloc_o
            FROM zwm032
                WHERE armazem = xuser-lgnum AND
                      ot = to_ret.

        DATA recebedor TYPE wempf.
        MOVE pulmao2+4(10) TO recebedor.
*        BREAK-POINT.
        CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
          EXPORTING
            lgnum            = xuser-lgnum
            code             = code
            recebedor        = recebedor
            mov_mm           = mov_mm
            testrun          = ' '
            plant_o          = ' '
            sloc_o           = ' '
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
        IF z_wm_cl_management=>is_remontada( i_lgnum = xuser-lgnum i_letyp = tipo_palete ) eq abap_true.
          SELECT SINGLE *
              FROM zwm013
                  WHERE armazem = xuser-lgnum AND
                  destino = pulmao2 AND
                  posicao_pulmao = posicao_pulmao.
          IF sy-subrc = 0.
            CLEAR x_zwm020.
            REFRESH x_zwm020.
            x_zwm020-armazem = xuser-lgnum.
            x_zwm020-p1 = zwm013-sscc.
            x_zwm020-p2 = su.
            APPEND x_zwm020.
            INSERT zwm020 FROM x_zwm020.
            COMMIT WORK.
          ENDIF.
        ENDIF.

*        inserir na tabela para desbloquear no fim
        x_zwm013-armazem = xuser-lgnum.
        x_zwm013-sscc = su.
*            X_ZWM013-TO_NUMBER = E_TO.
        x_zwm013-destino = pulmao2.
        x_zwm013-bloqueado = 'X'.
        x_zwm013-tipo_palete = tipo_palete.
        x_zwm013-incidencia = incidencia_terceiros.
        x_zwm013-posicao_pulmao = posicao_pulmao.
        x_zwm013-fabrica_1 = 'X'.
        x_zwm013-trans_terc = 'X'.
        APPEND x_zwm013.
        INSERT zwm013 FROM x_zwm013.
        COMMIT WORK.

        UPDATE zwm016 SET palete_conf = palete_conf + 1
            WHERE armazem = xuser-lgnum AND
                  user_name = sy-uname AND
                  porta = porta1.
        COMMIT WORK.

        CLEAR: pack_material, quantidade, unidade, material,
               plant, lgort, tipo_palete,su,incidencia_terceiros,
               posicao_pulmao ,kober, plant_o, sloc_o,lote.

        MOVE 'SU' TO cursorfield.

      ENDIF.

    WHEN 'SAVE'.
*   fim verificar se conferiu todas as paletes
      SELECT SINGLE *
          FROM zwm016
              WHERE armazem = xuser-lgnum
                AND user_name = sy-uname
                AND porta = porta1.

      IF zwm016-palete_conf IS INITIAL.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '093'.

      ELSEIF zwm016-num_paletes < zwm016-palete_conf.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'W'
            message_number = '080'
          IMPORTING
            ret_code       = resposta.
** Avançar
        IF resposta = 'O'.

** 04.03.2005 - ROFFD ** DEL
** Registar incidência e prosseguir
*          CALL FUNCTION 'ZWM_INSERT_ERROR'
*            EXPORTING
*              armazem    = xuser-lgnum
*              incidencia = '7'
*            EXCEPTIONS
*              no_commit  = 1
*              OTHERS     = 2.
** 04.03.2005 - ROFFD ** DEL

          PERFORM cria_doc_material_paletes_tr.
          PERFORM save.

        ELSEIF resposta = 'C'.
          UPDATE zwm016
            SET finalizada = ' '
              WHERE armazem = xuser-lgnum AND
                    user_name = sy-uname AND
                    porta = porta1.
*                    pulmao = pulmao2.
          COMMIT WORK.

          CLEAR: pack_material, quantidade, unidade, material,
                 plant, lgort, tipo_palete, su, posicao_pulmao,kober.
        ENDIF.

      ELSEIF zwm016-num_paletes > zwm016-palete_conf.

        paletes_dif = zwm016-num_paletes - zwm016-palete_conf.
        MOVE paletes_dif TO text1.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'W'
            message_number = '073'
            message_var1   = text1
          IMPORTING
            ret_code       = resposta.
** Avançar
        IF resposta = 'O'.

** 04.03.2005 - ROFFD ** DEL
** Registar incidência e prosseguir
*          CALL FUNCTION 'ZWM_INSERT_ERROR'
*            EXPORTING
*              armazem    = xuser-lgnum
*              incidencia = '7'
*            EXCEPTIONS
*              no_commit  = 1
*              OTHERS     = 2.
** 04.03.2005 - ROFFD ** DEL

          PERFORM cria_doc_material_paletes_tr.
          PERFORM save.

** Cancelar
        ELSEIF resposta = 'C'.
          UPDATE zwm016
            SET finalizada = ' '
              WHERE armazem = xuser-lgnum
                AND user_name = sy-uname
                AND porta = porta1.
*                    pulmao = pulmao2.
          CLEAR: pack_material, quantidade, unidade, material,
                 plant, lgort, tipo_palete, su, paletes_dif,
                 posicao_pulmao,kober.
        ENDIF.

      ELSE.

        PERFORM cria_doc_material_paletes_tr.
        PERFORM save.

      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0017  INPUT
