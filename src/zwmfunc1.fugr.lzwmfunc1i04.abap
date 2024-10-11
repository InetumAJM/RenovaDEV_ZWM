*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0009  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0009 INPUT.

  FREE: itab_lqua.
  CLEAR: st_type_o, st_type_d, st_type, mensula, bin_o,
         to, l_tanum, l_tapos, sscc, l_remontada, itab_lqua,
         l_vltyp, l_vlpla, l_letyp, l_linhas, l_nltyp, l_vlenr.

  REFRESH: sscc.

  CASE ok_code_0009.

    WHEN 'CLEAR'.

      CLEAR : cursorfield,
              incidencia,
              desc_incidencia,
              sscc_errado.

    WHEN 'NEXT'.

      IF NOT incidencia IS INITIAL. " AND sscc_errado IS INITIAL.

** verificar se a incidência é válida
        SELECT SINGLE * FROM zwm022
                        WHERE armazem = xuser-lgnum AND
                              incidencia = incidencia.
        IF sy-subrc <> 0.

          CLEAR text.
          WRITE incidencia TO text LEFT-JUSTIFIED.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '068'
              message_var1   = text.

          CLEAR : incidencia, desc_incidencia.
          MOVE 'INCIDENCIA' TO cursorfield.

        ELSE.

          PERFORM carrega_info_parametrizacao.

          MOVE zwm022-descricao TO desc_incidencia.
          MOVE 'SSCC_ERRADO' TO cursorfield.
        ENDIF.

      ENDIF.

    WHEN 'SAVE'.
** Regista Incidência
*      break roffd.

      IF NOT to_tri IS INITIAL.
        l_tanum = to_tri.
        l_tapos = item_tri.
      ELSE.
        l_tanum = to_ret.
        l_tapos = item_ret.
      ENDIF.

** Efectua registo de incidências
      PERFORM regista_incidencia.

      IF incidencia EQ '99'.
** Obter a posição de origem
        PERFORM get_posicao_origem USING l_tanum
                                         l_tapos
                                   CHANGING l_vltyp  "Posição Origem
                                            l_vlpla  "Coordenadas Origem
                                            l_letyp  "Tipo de Palete
                                            l_nltyp  "Posição destino
                                            l_vlenr. "SSCC Origem
        IF z_wm_cl_management=>is_remontada( i_lgnum = xuser-lgnum i_letyp = l_letyp ) eq abap_true.
          l_remontada = 'X'.
        ELSE.
          CLEAR: l_remontada.
        ENDIF.

        CASE ltap-vltyp.
          WHEN 'TRI'.
****************** Saídas Trilateral ************************
*            break roffd.

** Estorna a OT
            PERFORM estorna_to USING l_tanum
                                     l_tapos
                                     ' '.

** Actualiza tabela das mensulas
            PERFORM actualiza_zwm014.

** Cria nova OT com o novo destino
            PERFORM cria_nova_ot USING l_vlenr
                                       l_tanum
                                       l_tapos
                                       l_vltyp
                                       l_vlpla.

            IF l_remontada = 'X'.
** Paletes remontadas, vai obter a OT da de cima
              PERFORM get_to_remontada USING l_vlenr
                                             l_vltyp
                                             l_vlpla
                                             l_nltyp
                                             l_tanum
                                             l_tapos.
            ENDIF.

** Limpar registos da tabela ZWM011
            PERFORM actualiza_zwm011 USING l_tanum
                                           l_tapos.

** Obter OT Seguinte para o tri
            PERFORM get_next_to_tri.

          WHEN 'DRI' OR 'BLK'.
****************** Saídas Drive-In ************************
*            break roffd.

** Obter as quantidades para as OT
            PERFORM get_lqua USING l_vltyp
                                   l_vlpla
                             CHANGING l_linhas.

            IF l_remontada IS INITIAL AND
               l_linhas    EQ 1.
** Estorna a OT
              PERFORM estorna_to USING l_tanum
                                       l_tapos
                                       ' '.

** Cria nova OT com o novo destino
              READ TABLE itab_lqua INDEX 1.
              PERFORM cria_nova_ot USING itab_lqua-lenum
                                         l_tanum
                                         l_tapos
                                         l_vltyp
                                         l_vlpla.

** Limpar registos da tabela ZWM011
              PERFORM actualiza_zwm011 USING l_tanum
                                             l_tapos.

              CLEAR: incidencia, desc_incidencia, sscc_errado.

** Obter OT Seguinte para o dri
              PERFORM get_next_to_dri.

            ELSEIF l_remontada EQ 'X' AND "Remontadas
                   l_linhas    EQ 2.

** Estorna a OT de Origem
              PERFORM estorna_to USING l_tanum
                                       l_tapos
                                       'X'.

** Cria 1 OT
              PERFORM cria_nova_to_dri USING l_vltyp
                                             l_vlpla.

** Limpar registos da tabela ZWM011
              PERFORM actualiza_zwm011 USING l_tanum
                                             l_tapos.
              CLEAR: incidencia, desc_incidencia, sscc_errado.

** Obter OT Seguinte para o dri
              PERFORM get_next_to_dri.

            ELSE.
              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
                EXPORTING
                  message_id     = 'ZWMMSG001'
                  message_lang   = sy-langu
                  message_type   = 'E'
                  message_number = '184'.

              CLEAR: incidencia, desc_incidencia, sscc_errado.

              SET SCREEN '0000'.
              LEAVE SCREEN.

            ENDIF.

          WHEN OTHERS.
            CLEAR: incidencia, desc_incidencia, sscc_errado.

            SET SCREEN '0000'.
            LEAVE SCREEN.
        ENDCASE.

        CLEAR: incidencia, desc_incidencia, sscc_errado.
      ENDIF.

*** Tratamento de incidências das saídas
*      IF NOT to_tri IS INITIAL.
*        PERFORM trata_incidencia_saida_tri.
*      ENDIF.
*
*      IF NOT to_ret IS INITIAL.
*        IF bin_output_o(3) = st_type_dri.
*          PERFORM trata_incidencia_saida_dri.
*        ENDIF.
*      ENDIF.
*** Tratamento de incidências das saídas
*
*
*** Entrada de Terceiros -registo das incidencias na tabela
*** zwm013 e voltar ao ecra da conferencia
*      IF ecra_chamador = '0013' OR
*         ecra_chamador = '0017'.
*        incidencia_terceiros = incidencia.
*
*** Actualizar incidências
*        CALL FUNCTION 'ZWM_INSERT_ERROR'
*          EXPORTING
*            armazem    = xuser-lgnum
*            incidencia = incidencia_terceiros
*            posicao    = bin_output_d
*            sscc       = sscc_errado
*          EXCEPTIONS
*            no_commit  = 1
*            OTHERS     = 2.
*
*        SET SCREEN '0000'. LEAVE SCREEN.
*
*      ENDIF.
*
*** Registo de incidências na BD
*** Anulação da TO que estava associada ao user ... anulação da entrada
*** na tabela zwm011 e criação da nova TO tendo em conta uma nova
*mensula
*** de destino e bloqueio da mensula
*      IF bin_output_d(3) = st_type_men.
*** 1 - Bloqueio da mensula
*        SPLIT bin_output_d AT ' ' INTO st_type mensula.
*
*        DO.
*          CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
*            EXPORTING
*              mode_keyword   = 'X'
*              keyword_       = 'ZWM014'
*            EXCEPTIONS
*              foreign_lock   = 1
*              system_failure = 2
*              OTHERS         = 3.
*          IF sy-subrc = 0.
*            EXIT.
*          ELSE.
*            WAIT UP TO 1 SECONDS.
*          ENDIF.
*        ENDDO.
*** Mensula ocupada
*        SELECT SINGLE * FROM zwm014
*                        WHERE armazem = xuser-lgnum AND
*                              mensula = mensula.
*        IF sy-subrc = 0.
*          IF incidencia = 5.
*            zwm014-etiqueta = 'O'.
*          ELSE.
*            CLEAR : zwm014-su,
*                    zwm014-to_number,
*                    zwm014-to_item,
*                    zwm014-bin,
*                    zwm014-etiqueta,
*                    zwm014-su_transito.
*
*            IF incidencia = '3'.
*              zwm014-estado = 'O'.
*** Mensula em mal estado
*            ELSEIF incidencia = '4'.
*              zwm014-estado = 'D'.
*            ENDIF.
*          ENDIF.
*
*          MODIFY zwm014.
*          COMMIT WORK.
*
*          CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*            EXPORTING
*              mode_keyword   = 'X'
*              keyword_       = 'ZWM014'
*            EXCEPTIONS
*              foreign_lock   = 1
*              system_failure = 2
*              OTHERS         = 3.
*
*** 2 - Registo da incidência
*** Actualizar incidências
*          CALL FUNCTION 'ZWM_INSERT_ERROR'
*            EXPORTING
*              armazem    = xuser-lgnum
*              incidencia = incidencia
*              posicao    = bin_output_d
*              sscc       = sscc_errado
*            EXCEPTIONS
*              no_commit  = 1
*              OTHERS     = 2.
*
*** Verificar se a palete é remontada ... se sim apagar bin antigo da
*** tabela zwm020
*          SELECT SINGLE * FROM zwm020
*                          WHERE armazem = xuser-lgnum AND
*                                ( p1 = su OR
*                                  p2 = su ).
*          IF sy-subrc = 0.
*            CLEAR zwm020-bin.
*            MODIFY zwm020.
*            COMMIT WORK.
*          ENDIF.
*
*** Só executa estas operações para incidencias em que não se pode
*** utilizar a mensula
*          PERFORM descobre_nova_to.
*        ENDIF.
*
*      ELSE.           " Se não for mensula
*
*
*** 2 - Registo da incidência
*** Actualizar incidências
*        CALL FUNCTION 'ZWM_INSERT_ERROR'
*          EXPORTING
*            armazem    = xuser-lgnum
*            incidencia = incidencia
*            posicao    = bin_output_d
*            sscc       = sscc_errado
*          EXCEPTIONS
*            no_commit  = 1
*            OTHERS     = 2.
*
*
*        CLEAR mensula.
*        PERFORM descobre_nova_to.
*
*
*      ENDIF.

** DESCOBRIR QUAL É A NOVA TAREFA E CHAMAR O ECRÃ CORRESPONDENTE
*      CLEAR : to, f3_activo,sscc_errado,incidencia,
*              desc_incidencia,sscc.
*      REFRESH : sscc.
*
*      MOVE 'BIN_INPUT' TO cursorfield.
*      SET SCREEN '0000'.
*      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0009  INPUT

*&---------------------------------------------------------------------*
*&      Form  carrega_info_parametrizacao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_info_parametrizacao .

  DATA : valor_param LIKE zwm001-valor.

  IF st_type_tri IS INITIAL.
    CLEAR : valor_param.
** Storage type dos Trilaterais
    PERFORM get_parameter USING xuser-lgnum
                             'ENTRADA_ARMAZEM'
                             'ST_TRI'
                             valor_param.
    WRITE valor_param TO st_type_tri LEFT-JUSTIFIED.
  ENDIF.

  IF st_type_men IS INITIAL.
    CLEAR : valor_param.
** Storage type das Mensulas
    PERFORM get_parameter USING xuser-lgnum
                             'ENTRADA_ARMAZEM'
                             'ST_MEN'
                             valor_param.
    WRITE valor_param TO st_type_men LEFT-JUSTIFIED.
  ENDIF.

  IF st_type_dri IS INITIAL.
    CLEAR : valor_param.
** Storage type das Mensulas
    PERFORM get_parameter USING xuser-lgnum
                             'ENTRADA_ARMAZEM'
                             'ST_DRI'
                             valor_param.
    WRITE valor_param TO st_type_dri LEFT-JUSTIFIED.
  ENDIF.

ENDFORM.                    " carrega_info_parametrizacao
