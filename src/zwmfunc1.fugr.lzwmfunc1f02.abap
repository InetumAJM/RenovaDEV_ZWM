*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1F02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DESCOBRE_NOVA_TO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM descobre_nova_to .

  DATA : status,
         movimento_aux LIKE zwm001-valor,
         movimento LIKE ltak-bwlvs.

  CLEAR : status.

** Incidência de Palete estragada
  IF incidencia = '6'.
** Só faz para TOs que não estejam dentro da zona
** de Trilaterais
    IF bin_output_d(3) <> st_type_tri.
      PERFORM nova_to_incidencia.
    ENDIF.

  ENDIF.


** Incidência de posição estragada / danificada
  IF incidencia = '3' OR incidencia = '4'.

    IF bin_output_d(3) = st_type_men.
** Anulação da TO associada ao USER - estorno da TO no SAP

      CLEAR ltap_cancel.
      REFRESH ltap_cancel.

*** Se a origem for Mensula ... é TO_RET
      SELECT * FROM ltap INTO CORRESPONDING FIELDS OF TABLE
                                            ltap_cancel
               WHERE lgnum = xuser-lgnum AND
                     tanum = to_ret.

** Estorno da TO
      CALL FUNCTION 'ZWM_CANCEL_TO'
        EXPORTING
          armazem      = xuser-lgnum
        TABLES
          t_ltap_cancl = ltap_cancel
        EXCEPTIONS
          error        = 1
          OTHERS       = 2.
      IF sy-subrc = 0.

** Anular entrada da tabela de assignação de utilizador a TO e criar
** nova entrada para o user
        DELETE FROM zwm011
               WHERE armazem = xuser-lgnum AND
                     to_number = to_ret AND
                     user_name = sy-uname.
        COMMIT WORK.


** Criação de uma nova TO para a nova mensula
        READ TABLE ltap_cancel INDEX 1.

        SELECT SINGLE lgnum bwlvs FROM ltak INTO
                      (ltak-lgnum,ltak-bwlvs)
                      WHERE lgnum = xuser-lgnum AND
                            tanum = ltap_cancel-tanum.

        SELECT * FROM ltap
                 WHERE lgnum = xuser-lgnum AND
                       tanum = ltak-tanum.
          IF sy-subrc = 0.

            st_type_o = ltap-vltyp.
            st_type_d = ltap-nltyp.
            bin_o = ltap-vlpla.
            plant = ltap-werks.
            lgort = ltap-lgort.

            SELECT SINGLE lzone INTO certificado
                FROM lagp WHERE lgnum = xuser-lgnum AND
                                lgtyp = ltap-nltyp AND
                                lgpla = ltap-nlpla.


            sscc-sscc = ltap-nlenr.
            sscc-tipo_su = ltap-letyp.
            sscc-material = ltap-matnr.
            CLEAR sscc-variante.
            sscc-quantidade = ltap-vsola.
            sscc-uni = ltap-altme.
            CLEAR sscc-altura.
            sscc-lote_producao = ltap-charg.
            APPEND sscc.

          ENDIF.
        ENDSELECT.

        CLEAR return_msg.
        REFRESH return_msg.

        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
          EXPORTING
            warehouse   = ltak-lgnum
            mov_type    = ltak-bwlvs
            st_type_o   = st_type_o
            bin_origem  = bin_o
            st_type_d   = st_type_d
            plant       = plant
            s_loc       = lgort
            certificado = certificado
            req_number  = bin_o
            req_type    = 'E'
          IMPORTING
            to          = to
          TABLES
            return_msg  = return_msg
            sscc        = sscc
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.
        IF sy-subrc <> 0.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = return_msg-msgid
              message_lang   = sy-langu
              message_type   = return_msg-msgtyp
              message_number = return_msg-msgnr
              message_var1   = return_msg-msgv1
              message_var2   = return_msg-msgv2
              message_var3   = return_msg-msgv3.

          CLEAR : st_type_o, st_type_d, bin_o, plant, lgort,
                  ltap_cancel, sscc, return_msg, status.
          REFRESH : ltap_cancel, sscc, return_msg.
          SET SCREEN '0000'.LEAVE SCREEN.

        ELSE.

** Se não passar por mensula
          IF mensula IS INITIAL.
            status = 'P'.
          ELSEIF NOT mensula IS INITIAL.
            status = 'C'.
          ENDIF.

** Para tratamento de incidencias - próxima operação do
** operador
          zwm011-armazem = xuser-lgnum.
          zwm011-to_number = to.
          zwm011-to_item = '0001'.
          zwm011-status = status.
          zwm011-user_name = sy-uname.
          zwm011-equipamento = equipamento_.

          SELECT SINGLE queue FROM ltak INTO zwm011-queue
                              WHERE lgnum = xuser-lgnum AND
                                    tanum = to.

          MODIFY zwm011.
          COMMIT WORK.

** DESCOBRIR QUAL É A NOVA TAREFA E CHAMAR O ECRÃ CORRESPONDENTE
          CLEAR : to_ret, to, f3_activo,sscc_errado,incidencia,
                  desc_incidencia,sscc,status.
          REFRESH : sscc.

          IF lrf_wkqu-devty = '16X20'.
            LEAVE TO SCREEN '0001'.
          ELSEIF lrf_wkqu-devty = '8X40'.
            LEAVE TO SCREEN '0002'.
          ENDIF.

        ENDIF.
      ENDIF.



    ELSE.     " não passa por mensula

** 1 - ORIGEM PRODUCAO PARA PULMAO REPOSIÇÃO - to_ret
      CLEAR : st_type_o,
              st_type_d,
              bin_o,
              bin_d,
              plant,
              lgort.

      IF bin_output_d(3) = st_type_tri.
** Só na primeira vez é que é actualizada
        IF NOT to_tri IS INITIAL.
          to_ret = to_tri.
          CLEAR : to_tri.
        ENDIF.
      ENDIF.

** 1.1 - Confirmar TO do utilizador
      CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
        EXPORTING
          armazem              = xuser-lgnum
          confirm_type         = 'T'
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
      ENDIF.

** Actualizar tabela zwm011
** Anular entrada da tabela de assignação de utilizador a TO e criar
** nova entrada para o user
      DELETE FROM zwm011
             WHERE armazem = xuser-lgnum AND
                   to_number = to_ret AND
                   user_name = sy-uname.
      COMMIT WORK.

** 1.2 - Criar Nova TO com origem na posição anteriormente
**       confirmada
      SELECT SINGLE lgnum bwlvs FROM ltak INTO
                           (ltak-lgnum,ltak-bwlvs)
                           WHERE lgnum = xuser-lgnum AND
                                 tanum = to_ret.

      SELECT * FROM ltap
               WHERE lgnum = xuser-lgnum AND
                     tanum = to_ret.
        IF sy-subrc = 0.

          st_type_d = ltap-nltyp.
          bin_d = ltap-nlpla.
          plant = ltap-werks.
          lgort = ltap-lgort.

          SELECT SINGLE lzone INTO certificado
              FROM lagp WHERE lgnum = xuser-lgnum AND
                              lgtyp = ltap-nltyp AND
                              lgpla = ltap-nlpla.

          sscc-sscc = ltap-nlenr.
          sscc-tipo_su = ltap-letyp.
          sscc-material = ltap-matnr.
          CLEAR sscc-variante.
          sscc-quantidade = ltap-vsola.
          sscc-uni = ltap-altme.
          CLEAR sscc-altura.
          sscc-lote_producao = ltap-charg.
          APPEND sscc.

        ENDIF.

      ENDSELECT.

** Estruturas de auxilio à alteração do BIN ( bloqueio )
      DATA : BEGIN OF lbinh OCCURS 0.
              INCLUDE STRUCTURE e1lbinh.
      DATA : END OF lbinh.

      DATA : BEGIN OF lbini OCCURS 0.
              INCLUDE STRUCTURE e1lbini.
      DATA : END OF lbini.

      lbinh-lgnum = xuser-lgnum.
      lbinh-lgtyp = st_type_d.
      lbinh-block = 'X'.
      APPEND lbinh.

      lbini-lgpla = bin_d.
      lbini-skzue = 'X'.
      APPEND lbini.

      CALL FUNCTION 'ZWM_UPDATE_BIN'
        EXPORTING
          armazem        = xuser-lgnum
          posicao        = bin_d
          tipo_deposito  = st_type_d
          entrada        = 'X'
          lbinh          = lbinh
        TABLES
          lbini          = lbini
        EXCEPTIONS
          invalid_option = 1
          invalid_bin    = 2
          error_block    = 3
          OTHERS         = 4.

      CLEAR : lbinh, lbini.
      REFRESH : lbinh, lbini.

** Movimento correspondente ao tipo de depósito
      CLEAR movimento_aux.
      PERFORM get_parameter USING xuser-lgnum
                                 'INCIDENCIA'
                                 st_type_d
                                  movimento_aux.
      movimento = movimento_aux.

      CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
        EXPORTING
          warehouse   = ltak-lgnum
          mov_type    = movimento
          plant       = plant
          s_loc       = lgort
          certificado = certificado
          origem      = 'X'
        IMPORTING
          to          = to
        TABLES
          return_msg  = return_msg
          sscc        = sscc
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.
      IF sy-subrc <> 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = return_msg-msgid
            message_lang   = sy-langu
            message_type   = return_msg-msgtyp
            message_number = return_msg-msgnr
            message_var1   = return_msg-msgv1
            message_var2   = return_msg-msgv2
            message_var3   = return_msg-msgv3.

        CLEAR : st_type_o, st_type_d, bin_o, plant, lgort,
                ltap_cancel, sscc, return_msg, status.
        REFRESH : ltap_cancel, sscc, return_msg.
      ENDIF.


** Bloqueio para inventário - só para incidência de posição
** ocupada
      IF incidencia = '3'.

        lbinh-lgnum = xuser-lgnum.
        lbinh-lgtyp = st_type_d.
        lbinh-block = 'X'.
        APPEND lbinh.

        lbini-lgpla = bin_d.
        lbini-skzue = 'X'.
** Bloqueio para inventário
        lbini-skzsi = 'X'.
        lbini-spgru = '3'.
        APPEND lbini.

        CALL FUNCTION 'ZWM_UPDATE_BIN'
          EXPORTING
            armazem        = xuser-lgnum
            posicao        = bin_d
            tipo_deposito  = st_type_d
            entrada        = 'X'
            lbinh          = lbinh
          TABLES
            lbini          = lbini
          EXCEPTIONS
            invalid_option = 1
            invalid_bin    = 2
            error_block    = 3
            OTHERS         = 4.

        CLEAR : lbinh, lbini.
        REFRESH : lbinh, lbini.

      ENDIF.

** 1.3. - Confirmar parcialmente a nova TO
      CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
        EXPORTING
          armazem              = xuser-lgnum
          confirm_type         = 'P'
        TABLES
          return_msg           = return_msg
        CHANGING
          to                   = to
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
      ENDIF.

** Próximo tarefa do operador
      zwm011-armazem = xuser-lgnum.
      zwm011-to_number = to.
      zwm011-to_item = '0001'.
      zwm011-status = 'P'.
      zwm011-user_name = sy-uname.
      zwm011-equipamento = equipamento_.

      SELECT SINGLE queue FROM ltak INTO zwm011-queue
                          WHERE lgnum = xuser-lgnum AND
                                tanum = to.

      MODIFY zwm011.
      COMMIT WORK.

** DESCOBRIR QUAL É A NOVA TAREFA E CHAMAR O ECRÃ CORRESPONDENTE
** Para a actualização no ecrã 1 e 2 ... e a SU já aparecer no ecrã
      CLEAR to_ret.
      to_ret = to.

      CLEAR : to, f3_activo,sscc_errado,incidencia,
              desc_incidencia,sscc,status.
      REFRESH : sscc.

** Actualizar os campos dos ecrãs
      DATA : bin_aux LIKE lagp-lgpla.
      SELECT SINGLE nlpla FROM ltap INTO bin_aux
                          WHERE lgnum = xuser-lgnum AND
                                tanum = to_ret.

** Forma de mostrar a nova posição para o ecrã
      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = st_type_d
          lgpla = bin_aux
        IMPORTING
          bin   = bin_output_d.


      IF bin_output_d(3) = st_type_tri.
** Quando é nos Trilaterais temos de actualizar a tabela
** das mensulas
* Fazer lock antes de actualizar a tabela das mensulas
        DO.
          CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
            EXPORTING
              mode_keyword   = 'X'
              keyword_       = 'ZWM014'
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDDO.

        UPDATE zwm014 SET estado = ' ' su = su_aux prioridade = ' '
                WHERE armazem     = xuser-lgnum AND
                      su_transito = su.
        COMMIT WORK.

        CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'ZWM014'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        to_tri = to_ret.
        IF lrf_wkqu-devty = '16X20'.
          MOVE 'BIN_INPUT' TO cursorfield.
          LEAVE TO SCREEN '0003'.
        ELSEIF lrf_wkqu-devty = '8X40'.
          MOVE 'BIN_INPUT' TO cursorfield.
          LEAVE TO SCREEN '0004'.
        ENDIF.

      ELSE.   " se não for Trilaterais

        IF lrf_wkqu-devty = '16X20'.
          MOVE 'BIN_INPUT' TO cursorfield.
          LEAVE TO SCREEN '0001'.
        ELSEIF lrf_wkqu-devty = '8X40'.
          MOVE 'BIN_INPUT' TO cursorfield.
          LEAVE TO SCREEN '0002'.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDIF.


ENDFORM.                    " DESCOBRE_NOVA_TO
*&---------------------------------------------------------------------*
*&      Form  nova_to_incidencia
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nova_to_incidencia .

  DATA : status,
         mov_aux LIKE zwm001-valor,
         mov LIKE ltak-bwlvs.

  CLEAR : st_type_o,
          st_type_d,
          bin_o,
          bin_d,
          plant,
          lgort.


** Para tratamento onde o destino é a Mensula
  IF bin_output_d(3) = st_type_men.

    CLEAR ltap_cancel.
    REFRESH ltap_cancel.

*** Se a origem for Mensula ... é TO_RET
    SELECT * FROM ltap INTO CORRESPONDING FIELDS OF TABLE
                                          ltap_cancel
             WHERE lgnum = xuser-lgnum AND
                   tanum = to_ret.

** Estorno da TO
    CALL FUNCTION 'ZWM_CANCEL_TO'
      EXPORTING
        armazem      = xuser-lgnum
      TABLES
        t_ltap_cancl = ltap_cancel
      EXCEPTIONS
        error        = 1
        OTHERS       = 2.
    IF sy-subrc = 0.

** Anular entrada da tabela de assignação de utilizador a TO e criar
** nova entrada para o user
      DELETE FROM zwm011
             WHERE armazem = xuser-lgnum AND
                   to_number = to_ret AND
                   user_name = sy-uname.
      COMMIT WORK.


** Criação de uma nova TO para a nova mensula/posição
      READ TABLE ltap_cancel INDEX 1.
** dados de cabeçalho
      SELECT SINGLE lgnum bwlvs FROM ltak INTO
                    (ltak-lgnum,ltak-bwlvs)
                    WHERE lgnum = xuser-lgnum AND
                          tanum = ltap_cancel-tanum.

** dados de item
      SELECT * FROM ltap
               WHERE lgnum = xuser-lgnum AND
                     tanum = ltak-tanum.
        IF sy-subrc = 0.

          st_type_o = ltap-vltyp.
          st_type_d = ltap-nltyp.
          bin_o = ltap-vlpla.
          plant = ltap-werks.
          lgort = ltap-lgort.

          SELECT SINGLE lzone INTO certificado
              FROM lagp WHERE lgnum = xuser-lgnum AND
                              lgtyp = ltap-nltyp AND
                              lgpla = ltap-nlpla.


          sscc-sscc = ltap-nlenr.
          sscc-tipo_su = ltap-letyp.
          sscc-material = ltap-matnr.
          CLEAR sscc-variante.
          sscc-quantidade = ltap-vsola.
          sscc-uni = ltap-altme.
          CLEAR sscc-altura.
          sscc-lote_producao = ltap-charg.
          APPEND sscc.

        ENDIF.
      ENDSELECT.

      CLEAR return_msg.
      REFRESH return_msg.
*      BREAK-POINT.

** Movimento para zona de incidências
      CLEAR mov_aux.
      PERFORM get_parameter USING xuser-lgnum
                                 'INCIDENCIA'
                                 'INC'
                                  mov_aux.
      mov = mov_aux.

      CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
        EXPORTING
          warehouse   = ltak-lgnum
          mov_type    = mov
          st_type_o   = st_type_o
          bin_origem  = bin_o
          plant       = plant
          s_loc       = lgort
          certificado = certificado
        IMPORTING
          to          = to
        TABLES
          return_msg  = return_msg
          sscc        = sscc
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.
      IF sy-subrc <> 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = return_msg-msgid
            message_lang   = sy-langu
            message_type   = return_msg-msgtyp
            message_number = return_msg-msgnr
            message_var1   = return_msg-msgv1
            message_var2   = return_msg-msgv2
            message_var3   = return_msg-msgv3.

        CLEAR : st_type_o, st_type_d, bin_o, plant, lgort,
                ltap_cancel, sscc, return_msg, status.
        REFRESH : ltap_cancel, sscc, return_msg.
        SET SCREEN '0000'.LEAVE SCREEN.

      ELSE.

** 1.3. - Confirmar parcialmente a nova TO
        CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
          EXPORTING
            armazem              = xuser-lgnum
            confirm_type         = 'P'
          TABLES
            return_msg           = return_msg
          CHANGING
            to                   = to
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
        ENDIF.

** Inserção na tabela zwm011 da tarefa a executar pelo
** operador
        zwm011-armazem = xuser-lgnum.
        zwm011-to_number = to.
        zwm011-to_item = '0001'.
        zwm011-status = 'P'.
        zwm011-user_name = sy-uname.
        zwm011-equipamento = equipamento_.

        SELECT SINGLE queue FROM ltak INTO zwm011-queue
                            WHERE lgnum = xuser-lgnum AND
                                  tanum = to.
        MODIFY zwm011.
        COMMIT WORK.

        CLEAR to_ret.
        to_ret = to.

** DESCOBRIR QUAL É A NOVA TAREFA E CHAMAR O ECRÃ CORRESPONDENTE
        CLEAR : to, f3_activo,sscc_errado,incidencia,
                desc_incidencia,sscc,status.
        REFRESH : sscc.

** Forma de mostrar a nova posição para o ecrã
        DATA : bin_aux LIKE lagp-lgpla.
        SELECT SINGLE nlpla FROM ltap INTO bin_aux
                            WHERE lgnum = xuser-lgnum AND
                                  tanum = to_ret.

        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = 'INC'
            lgpla = bin_aux
          IMPORTING
            bin   = bin_output_d.

        IF lrf_wkqu-devty = '16X20'.
          MOVE 'BIN_INPUT' TO cursorfield.
          LEAVE TO SCREEN '0001'.
        ELSEIF lrf_wkqu-devty = '8X40'.
          MOVE 'BIN_INPUT' TO cursorfield.
          LEAVE TO SCREEN '0002'.
        ENDIF.

      ENDIF.
    ENDIF.


  ELSE.       " se não forem mensulas

** Só pode realizar estas operações para contrapesado
** e retráctil

** 1.1 - Confirmar TO do utilizador
    CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
      EXPORTING
        armazem              = xuser-lgnum
        confirm_type         = 'T'
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
    ENDIF.

** Actualizar tabela zwm011
** Anular entrada da tabela de assignação de utilizador a TO e criar
** nova entrada para o user
    DELETE FROM zwm011
           WHERE armazem = xuser-lgnum AND
                 to_number = to_ret AND
                 user_name = sy-uname.
    COMMIT WORK.

** 1.2 - Criar Nova TO com origem na posição anteriormente
**       confirmada
    SELECT SINGLE lgnum bwlvs FROM ltak INTO
                         (ltak-lgnum,ltak-bwlvs)
                         WHERE lgnum = xuser-lgnum AND
                               tanum = to_ret.
** dados de item
    SELECT * FROM ltap
             WHERE lgnum = xuser-lgnum AND
                   tanum = to_ret.
      IF sy-subrc = 0.

        st_type_d = ltap-nltyp.
        bin_d = ltap-nlpla.
        plant = ltap-werks.
        lgort = ltap-lgort.

        SELECT SINGLE lzone INTO certificado
            FROM lagp WHERE lgnum = xuser-lgnum AND
                            lgtyp = ltap-nltyp AND
                            lgpla = ltap-nlpla.


        sscc-sscc = ltap-nlenr.
        sscc-tipo_su = ltap-letyp.
        sscc-material = ltap-matnr.
        CLEAR sscc-variante.
        sscc-quantidade = ltap-vsola.
        sscc-uni = ltap-altme.
        CLEAR sscc-altura.
        sscc-lote_producao = ltap-charg.
        APPEND sscc.

      ENDIF.

    ENDSELECT.

** Movimento correspondente ao tipo de depósito
    CLEAR mov_aux.
    PERFORM get_parameter USING xuser-lgnum
                               'INCIDENCIA'
                               'INC'
                                mov_aux.
    mov = mov_aux.

    CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse   = ltak-lgnum
        mov_type    = mov
        plant       = plant
        s_loc       = lgort
        certificado = certificado
        origem      = 'X'
      IMPORTING
        to          = to
      TABLES
        return_msg  = return_msg
        sscc        = sscc
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = return_msg-msgid
          message_lang   = sy-langu
          message_type   = return_msg-msgtyp
          message_number = return_msg-msgnr
          message_var1   = return_msg-msgv1
          message_var2   = return_msg-msgv2
          message_var3   = return_msg-msgv3.

      CLEAR : st_type_o, st_type_d, bin_o, plant, lgort,
              ltap_cancel, sscc, return_msg, status.
      REFRESH : ltap_cancel, sscc, return_msg.
    ENDIF.

** 1.3. - Confirmar parcialmente a nova TO
    CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
      EXPORTING
        armazem              = xuser-lgnum
        confirm_type         = 'P'
      TABLES
        return_msg           = return_msg
      CHANGING
        to                   = to
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
    ENDIF.


** Próxima tarefa a ser executada pelo operador
    zwm011-armazem = xuser-lgnum.
    zwm011-to_number = to.
    zwm011-to_item = '0001'.
    zwm011-status = 'P'.
    zwm011-user_name = sy-uname.
    zwm011-equipamento = equipamento_.

    SELECT SINGLE queue FROM ltak INTO zwm011-queue
                        WHERE lgnum = xuser-lgnum AND
                              tanum = to.
    MODIFY zwm011.
    COMMIT WORK.

** DESCOBRIR QUAL É A NOVA TAREFA E CHAMAR O ECRÃ CORRESPONDENTE
** Para a actualização no ecrã 1 e 2 ... e a SU já aparecer no ecrã
    CLEAR to_ret.
    to_ret = to.

    CLEAR : to, f3_activo,sscc_errado,incidencia,
            desc_incidencia,sscc,status.
    REFRESH : sscc.

** Actualizar os campos dos ecrãs com a nova posição
    SELECT SINGLE nlpla FROM ltap INTO bin_aux
                        WHERE lgnum = xuser-lgnum AND
                              tanum = to_ret.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = 'INC'
        lgpla = bin_aux
      IMPORTING
        bin   = bin_output_d.

    IF lrf_wkqu-devty = '16X20'.
      MOVE 'BIN_INPUT' TO cursorfield.
      LEAVE TO SCREEN '0001'.
    ELSEIF lrf_wkqu-devty = '8X40'.
      MOVE 'BIN_INPUT' TO cursorfield.
      LEAVE TO SCREEN '0002'.
    ENDIF.

  ENDIF.

ENDFORM.                    " nova_to_incidencia

*&---------------------------------------------------------------------*
*&      Form  trata_incidencia_saida_tri
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trata_incidencia_saida_tri .

  DATA: corredor(4),n_mensulas_ocupadas TYPE i.
  CLEAR : corredor, n_mensulas_ocupadas.

** Incidências de posição em mau estado e
** posição ocupada
  IF incidencia = '3' OR incidencia = '4'.

** Verificar se a to é de saída ... pela queue
    SELECT SINGLE * FROM ltak
                    WHERE lgnum = xuser-lgnum AND
                          tanum = to_tri.
    IF sy-subrc = 0.
      SELECT SINGLE tipo FROM zwm010 INTO zwm010-tipo
                         WHERE armazem = ltak-lgnum AND
                               equipamento = equipamento_ AND
                               queue = ltak-queue.
** Fila de saída ... podemos executar o processamento
      IF zwm010-tipo = 'S'.
        DO.
          CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
            EXPORTING
              mode_keyword   = 'X'
              keyword_       = 'ZWM014'
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDDO.

** Actualização da MENSULA
        SELECT SINGLE * FROM zwm014
                        WHERE armazem = xuser-lgnum AND
                              su_transito = su1 AND
                              estado = 'X'.
        IF sy-subrc = 0.
          CLEAR : corredor.
          CONCATENATE zwm014-mensula(3) '%' INTO corredor.

          IF incidencia = 3.
            zwm014-estado = 'O'.
          ELSEIF incidencia = 4.
            zwm014-estado = 'D'.
          ENDIF.
          CLEAR : zwm014-su,zwm014-bin,zwm014-etiqueta,
                  zwm014-su_transito,zwm014-prioridade.
          MODIFY zwm014.
          COMMIT WORK.

*** Registo da INCIDÊNCIA
*          CALL FUNCTION 'ZWM_INSERT_ERROR'
*            EXPORTING
*              armazem    = xuser-lgnum
*              incidencia = incidencia
*              posicao    = bin_output_d
*              sscc       = sscc_errado
*            EXCEPTIONS
*              no_commit  = 1
*              OTHERS     = 2.

** Descobrir nova mensula ... e por no ecra
          SELECT SINGLE *
              FROM zwm014
                  WHERE armazem = xuser-lgnum AND
                        estado = ' ' AND
                        mensula LIKE corredor.
*      Não existe mensula para fazer a saída a to nao pode ser dada
          IF sy-subrc = 0.
            zwm014-armazem = xuser-lgnum.
            zwm014-su = su1.
            zwm014-su_transito = su1.
            zwm014-estado = 'X'.
            zwm014-bin = bin_output_o+4(10).
            MODIFY zwm014.

** verificar se neste corredor se está a ocupar
** a terceira mensula do corredor ...se sim colocar
** nas TOs que pertencem ao corredor prioridade máxima
            SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
                            WHERE armazem = xuser-lgnum AND
                                  mensula LIKE corredor AND
                                  estado = 'X'.

            IF n_mensulas_ocupadas >= 3.
              UPDATE zwm014 SET prioridade = 9
                            WHERE armazem = xuser-lgnum AND
                                  mensula LIKE corredor AND
                                  estado = 'X'.
            ENDIF.
            COMMIT WORK.

            CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
              EXPORTING
                mode_keyword   = 'X'
                keyword_       = 'ZWM014'
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.
** Nova mensula de destino
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = 'MEN'
                lgpla = zwm014-mensula
              IMPORTING
                bin   = bin_output_d.
          ENDIF.

          CLEAR : sscc_errado,incidencia,desc_incidencia.
          IF lrf_wkqu-devty = '16X20'.
            MOVE 'BIN_INPUT' TO cursorfield.
            LEAVE TO SCREEN '0003'.
          ELSEIF lrf_wkqu-devty = '8X40'.
            MOVE 'BIN_INPUT' TO cursorfield.
            LEAVE TO SCREEN '0004'.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " trata_incidencia_saida_tri
*&---------------------------------------------------------------------*
*&      Form  trata_incidencia_saida_dri
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trata_incidencia_saida_dri .

  DATA : mov_aux LIKE zwm001-valor.

** Para as saídas do drive in é obrigatório o
** SSCC estar preenchido
  CHECK NOT sscc_errado IS INITIAL.

** Palete danificada
  IF incidencia = '6'.

** Actualizar incidências
    CALL FUNCTION 'ZWM_INSERT_ERROR'
      EXPORTING
        armazem    = xuser-lgnum
        incidencia = '6'
        posicao    = bin_output_o
        sscc       = su
      EXCEPTIONS
        no_commit  = 1
        OTHERS     = 2.

** Criação de uma TO para a área de paletes estragadas
** e assignação da mesma ao operador

** Anular entrada da tabela de assignação de utilizador a TO e criar
** nova entrada para o user
    DELETE FROM zwm011
           WHERE armazem = xuser-lgnum AND
                 to_number = to_ret AND
                 user_name = sy-uname.
    COMMIT WORK.

** Dados de quanto da SU
    SELECT SINGLE * FROM lqua
                    WHERE lgnum = xuser-lgnum AND
                          lenum = sscc_errado.
    IF sy-subrc = 0.

      plant = lqua-werks.
      lgort = lqua-lgort.

      SELECT SINGLE lzone INTO certificado
          FROM lagp WHERE lgnum = xuser-lgnum AND
                          lgtyp = lqua-lgtyp AND
                          lgpla = lqua-lgpla.

      sscc-sscc = lqua-lenum.
      sscc-tipo_su = lqua-letyp.
      sscc-material = lqua-matnr.
      CLEAR sscc-variante.
      sscc-quantidade = lqua-verme.
      sscc-uni = lqua-meins.
      CLEAR sscc-altura.
      sscc-lote_producao = lqua-charg.
      APPEND sscc.

    ENDIF.

    CLEAR return_msg.
    REFRESH return_msg.
** Movimento para zona de incidências
    CLEAR mov_aux.
    PERFORM get_parameter USING xuser-lgnum
                               'INCIDENCIA'
                               'INC'
                                mov_aux.
    mov = mov_aux.

    CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse   = ltak-lgnum
        mov_type    = mov
        plant       = plant
        s_loc       = lgort
        certificado = certificado
        origem      = 'X'
      IMPORTING
        to          = to
      TABLES
        return_msg  = return_msg
        sscc        = sscc
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = return_msg-msgid
          message_lang   = sy-langu
          message_type   = return_msg-msgtyp
          message_number = return_msg-msgnr
          message_var1   = return_msg-msgv1
          message_var2   = return_msg-msgv2
          message_var3   = return_msg-msgv3.

      CLEAR : plant, lgort,sscc, return_msg.
      REFRESH : sscc, return_msg.
      SET SCREEN '0000'.LEAVE SCREEN.
    ELSE.
      CLEAR : return_msg.
      REFRESH : return_msg.

** 1.3. - Confirmar parcialmente a nova TO
      CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
        EXPORTING
          armazem              = xuser-lgnum
          confirm_type         = 'P'
        TABLES
          return_msg           = return_msg
        CHANGING
          to                   = to
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
      ENDIF.

** Inserção na tabela zwm011 da tarefa a executar pelo
** operador
      zwm011-armazem = xuser-lgnum.
      zwm011-to_number = to.
      zwm011-to_item = '0001'.
      zwm011-status = 'P'.
      zwm011-user_name = sy-uname.
      zwm011-equipamento = equipamento_.
      SELECT SINGLE queue FROM ltak INTO zwm011-queue
                          WHERE lgnum = xuser-lgnum AND
                                tanum = to.
      MODIFY zwm011.
      COMMIT WORK.

      CLEAR to_ret.
      to_ret = to.

** DESCOBRIR QUAL É A NOVA TAREFA E CHAMAR O ECRÃ CORRESPONDENTE
      CLEAR : to, f3_activo,incidencia,desc_incidencia,sscc.
      REFRESH : sscc.
** Forma de mostrar a nova posição para o ecrã
      DATA : bin_aux LIKE lagp-lgpla.
      SELECT SINGLE nlpla FROM ltap INTO bin_aux
                          WHERE lgnum = xuser-lgnum AND
                                tanum = to_ret.

      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = 'INC'
          lgpla = bin_aux
        IMPORTING
          bin   = bin_output_d.
      MOVE sscc_errado TO su.
      CLEAR : sscc_errado.

      IF lrf_wkqu-devty = '16X20'.
        MOVE 'BIN_INPUT' TO cursorfield.
        LEAVE TO SCREEN '0001'.
      ELSEIF lrf_wkqu-devty = '8X40'.
        MOVE 'BIN_INPUT' TO cursorfield.
        LEAVE TO SCREEN '0002'.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    " trata_incidencia_saida_dri
*&---------------------------------------------------------------------*
*&      Form  regista_incidencia
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM regista_incidencia .

  FREE: itab_return.
  CLEAR: itab_return.

** Registo da INCIDÊNCIA
  CALL FUNCTION 'ZWM_INSERT_ERROR'
    EXPORTING
      armazem    = xuser-lgnum
      incidencia = incidencia
      posicao    = bin_output_d
      sscc       = sscc_errado
      tanum      = l_tanum
      tapos      = l_tapos
    EXCEPTIONS
      no_commit  = 1
      OTHERS     = 2.

ENDFORM.                    " regista_incidencia
*&---------------------------------------------------------------------*
*&      Form  actualiza_zwm014
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_zwm014 .

  DATA: wa_zwm014 LIKE zwm014.

  CHECK NOT sscc_errado IS INITIAL.

  DO.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'ZWM014'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

  SELECT * FROM zwm014
           WHERE armazem = xuser-lgnum
             AND su = sscc_errado.
    wa_zwm014 = zwm014.
    EXIT.
  ENDSELECT.

  IF sy-subrc = 0.

    IF incidencia = 3.
      zwm014-estado = 'O'.
    ELSEIF incidencia = 4.
      zwm014-estado = 'D'.
    ENDIF.
    CLEAR: wa_zwm014-su, wa_zwm014-estado.

    MODIFY zwm014 FROM wa_zwm014.
    COMMIT WORK.
  ENDIF.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = 'ZWM014'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
** Nova mensula de destino
  CALL FUNCTION 'ZWM_CONCATENATE_BIN'
    EXPORTING
      lgtyp = 'MEN'
      lgpla = zwm014-mensula
    IMPORTING
      bin   = bin_output_d.

ENDFORM.                    " actualiza_zwm014
*&---------------------------------------------------------------------*
*&      Form  cria_nova_ot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_nova_ot USING p_sscc
                        p_tanum
                        p_tapos
                        p_vltyp
                        p_vlpla.

  CLEAR: i_sscc, return_msg.
  FREE: i_sscc, return_msg.

  CLEAR: mov, plant, s_loc, valor, ltap.

** Dados da OT
  SELECT SINGLE * FROM ltap
  WHERE lgnum = xuser-lgnum
    AND tanum = p_tanum
    AND tapos = p_tapos
    AND vlenr = p_sscc.

  IF sy-subrc = 0.
    i_sscc-material = ltap-matnr.
    i_sscc-quantidade = ltap-vsola.
    i_sscc-uni = ltap-altme.
    i_sscc-lote_producao = ltap-charg.
    APPEND i_sscc.
    CLEAR i_sscc.
  ENDIF.

** Movimento
  SELECT SINGLE valor INTO valor
      FROM zwm001
          WHERE armazem = xuser-lgnum AND
                processo = 'INCIDENCIA' AND
                parametro = 'INC'.

  MOVE valor TO mov.
  CLEAR valor.

** Centro
  SELECT SINGLE valor INTO valor
     FROM zwm001
         WHERE armazem = xuser-lgnum AND
               processo = 'GERAL' AND
               parametro = 'PLANT'.
  MOVE valor TO plant.
  CLEAR valor.

** Depósito
  SELECT SINGLE valor INTO valor
   FROM zwm001
       WHERE armazem = xuser-lgnum AND
             processo = 'GERAL' AND
             parametro = 'LGORT'.
  MOVE valor TO s_loc.
  CLEAR valor.

** Criar a TO
  CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
    EXPORTING
      warehouse  = xuser-lgnum
      mov_type   = mov
      plant      = plant
      s_loc      = s_loc
      st_type_o  = p_vltyp
      bin_origem = p_vlpla
    TABLES
      return_msg = return_msg
      sscc       = i_sscc
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

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

      CLEAR: su, cursorfield.
    ENDIF.

  ENDIF.

ENDFORM.                    " cria_nova_ot
*&---------------------------------------------------------------------*
*&      Form  confirma_to_incidencia
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirma_to_incidencia USING p_sscc
                                  p_tanum
                                  p_tapos.

  FREE: itab_return.
  CLEAR: itab_return.

** Picar o item da TO
  CALL FUNCTION 'ZWM_CONFIRM_TO'
    EXPORTING
      armazem              = xuser-lgnum
      confirm_type         = 'B'
      su                   = p_sscc
    TABLES
      return_msg           = itab_return
    CHANGING
      to                   = p_tanum
      to_item              = p_tapos
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
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " confirma_to_incidencia
*&---------------------------------------------------------------------*
*&      Form  get_to_remontada
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_to_remontada USING p_sscc
                            p_vltyp
                            p_vlpla
                            p_nltyp
                            p_tanum
                            p_tapos.

  DATA: x_tanum LIKE ltap-tanum,
        x_tapos LIKE ltap-tapos.

*  CLEAR: l_tanum, l_tapos.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_sscc
    IMPORTING
      output = p_sscc.

** Obter a palete de cima, visto ser uma remontada
  CLEAR: zwm020.
  SELECT SINGLE p2 FROM zwm020
  INTO l_sscc
  WHERE armazem = xuser-lgnum
    AND p1 = p_sscc.
  IF sy-subrc NE 0.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '181'.
  ELSE.

    IF p_nltyp = '916'.
** 2 OT's
      CLEAR: ltap.
      SELECT * FROM ltap
      WHERE lgnum EQ xuser-lgnum
        AND vlenr EQ l_sscc
        AND pquit NE 'X'.
        x_tanum = ltap-tanum.
        x_tapos = ltap-tapos.
        EXIT.
      ENDSELECT.

      IF sy-subrc EQ 0.
** Estorna a OT
        PERFORM estorna_to USING x_tanum
                                 x_tapos
                                 ' '.

** Cria nova OT com o novo destino
        PERFORM cria_nova_ot USING l_sscc
                                   x_tanum
                                   x_tapos
                                   p_vltyp
                                   p_vlpla.
      ELSE.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '181'.
      ENDIF.

    ELSE.
** 1 OT com 2 Item's
      CLEAR: ltap.
      SELECT * FROM ltap
      WHERE lgnum EQ xuser-lgnum
        AND vlenr EQ l_sscc
        AND tanum EQ p_tanum
        AND tapos NE p_tapos.
        x_tanum = ltap-tanum.
        x_tapos = ltap-tapos.
        EXIT.
      ENDSELECT.

      IF sy-subrc EQ 0.
** Estorna a OT
        PERFORM estorna_to USING x_tanum
                                 x_tapos
                                 ' '.

** Cria nova OT com o novo destino
        PERFORM cria_nova_ot USING l_sscc
                                   x_tanum
                                   x_tapos
                                   p_vltyp
                                   p_vlpla.
      ELSE.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '181'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_to_remontada
*&---------------------------------------------------------------------*
*&      Form  get_next_to_tri
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_next_to_tri.

  DATA: ecran(4).

*  break roffd.

  FREE return_msg.
  CLEAR: return_msg.

  SELECT * FROM zwm010 INTO TABLE tab_zwm010
  WHERE armazem = xuser-lgnum AND
        equipamento = equipamento_.

  ecran = lrf_wkqu-devty(5).

  CALL FUNCTION 'ZWM_GET_TO_TRI'
    EXPORTING
      armazem           = xuser-lgnum
      tamanho           = lrf_wkqu-devty(5)
    IMPORTING
      nova_to           = tab_zwm011
      tipo_queue        = tipo_tri
    TABLES
      l_zwm010          = tab_zwm010
      return_msg        = return_msg
    EXCEPTIONS
      no_equipment      = 1
      no_work_available = 2
      OTHERS            = 3.

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
          message_var2   = return_msg-msgv2.

      CLEAR: to, f3_activo, sscc_errado, incidencia,
              desc_incidencia, sscc, cursorfield.
      FREE: sscc.

      f3_activo = 'X'.
      SET SCREEN '0000'.
      LEAVE SCREEN.
    ENDIF.
  ELSE.
    CALL FUNCTION 'ZWM_CALL_TASK_TRI'
      EXPORTING
        armazem     = xuser-lgnum
        ecran       = ecran
        tab_zwm011  = tab_zwm011
        tipo_queue  = tipo_tri
        equipamento = equipamento_.

** Quando um operario quer sair da pistola
    IF NOT f3_activo IS INITIAL.
      CLEAR: to, sscc_errado, incidencia,
              desc_incidencia, sscc, cursorfield.
      FREE: sscc.

      IF lrf_wkqu-devty(5) = '16X20'.
        SET SCREEN '0001'.
      ELSE.
        SET SCREEN '0002'.
      ENDIF.

      LEAVE SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_next_to_tri
*&---------------------------------------------------------------------*
*&      Form  estorna_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TANUM  text
*      -->P_TAPOS  text
*----------------------------------------------------------------------*
FORM estorna_to  USING    p_tanum
                          p_tapos
                          p_dri.

  DATA t_ltap_cancl LIKE ltap_cancl OCCURS 0 WITH HEADER LINE.

  IF p_dri EQ 'X'.
    CLEAR: ltap.
    SELECT * FROM ltap
    WHERE tanum EQ p_tanum
      AND pquit NE 'X'.
      t_ltap_cancl-tanum = p_tanum.
      t_ltap_cancl-tapos = ltap-tapos.
      APPEND t_ltap_cancl.
      CLEAR t_ltap_cancl.
    ENDSELECT.
  ELSE.
    t_ltap_cancl-tanum = p_tanum.
    t_ltap_cancl-tapos = p_tapos.
    APPEND t_ltap_cancl.
    CLEAR t_ltap_cancl.
  ENDIF.

  CALL FUNCTION 'ZWM_CANCEL_TO'
    EXPORTING
      armazem      = xuser-lgnum
    TABLES
      t_ltap_cancl = t_ltap_cancl
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " estorna_to
*&---------------------------------------------------------------------*
*&      Form  get_posicao_origem
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TANUM  text
*      -->P_TAPOS  text
*----------------------------------------------------------------------*
FORM get_posicao_origem  USING    p_tanum
                                  p_tapos
                         CHANGING p_vltyp
                                  p_vlpla
                                  p_letyp
                                  p_nltyp
                                  p_vlenr.

** Dados da OT
  CLEAR: p_vltyp, p_vlpla, p_letyp, p_nltyp, p_vlenr.
  SELECT SINGLE vltyp vlpla letyp nltyp vlenr FROM ltap
  INTO (p_vltyp, p_vlpla, p_letyp, p_nltyp, p_vlenr)
  WHERE lgnum = xuser-lgnum
    AND tanum = p_tanum
    AND tapos = p_tapos.

ENDFORM.                    " get_posicao_origem
*&---------------------------------------------------------------------*
*&      Form  get_lqua
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VLTYP  text
*      -->P_VLPLA  text
*----------------------------------------------------------------------*
FORM get_lqua  USING    p_vltyp
                        p_vlpla
               CHANGING p_linhas.

  CLEAR: p_linhas.

  SELECT * FROM lqua
  INTO CORRESPONDING FIELDS OF TABLE itab_lqua
  WHERE lgnum EQ xuser-lgnum
    AND lgtyp EQ p_vltyp
    AND lgpla EQ p_vlpla.

  DESCRIBE TABLE itab_lqua LINES p_linhas.

ENDFORM.                    " get_lqua
*&---------------------------------------------------------------------*
*&      Form  actualiza_zwm011
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TANUM  text
*      -->P_TAPOS  text
*----------------------------------------------------------------------*
FORM actualiza_zwm011  USING    p_tanum
                                p_tapos.

  CALL FUNCTION 'ZWM_MODIFY_ZWM011'
    EXPORTING
      armazem            = xuser-lgnum
      to_number          = p_tanum
      to_item            = p_tapos
      status             = 'T'
    EXCEPTIONS
      error_update_table = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " actualiza_zwm011
*&---------------------------------------------------------------------*
*&      Form  cria_nova_to_dri
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_nova_to_dri USING p_vltyp
                            p_vlpla.

  CLEAR: i_sscc, return_msg.
  FREE: i_sscc, return_msg.

  CLEAR: mov, plant, s_loc, valor.

** Movimento
  SELECT SINGLE valor INTO valor
      FROM zwm001
          WHERE armazem = xuser-lgnum AND
                processo = 'INCIDENCIA' AND
                parametro = 'INC'.

  MOVE valor TO mov.
  CLEAR valor.

** Centro
  SELECT SINGLE valor INTO valor
     FROM zwm001
         WHERE armazem = xuser-lgnum AND
               processo = 'GERAL' AND
               parametro = 'PLANT'.
  MOVE valor TO plant.
  CLEAR valor.

** Depósito
  SELECT SINGLE valor INTO valor
   FROM zwm001
       WHERE armazem = xuser-lgnum AND
             processo = 'GERAL' AND
             parametro = 'LGORT'.
  MOVE valor TO s_loc.
  CLEAR valor.

** Criar a TO
** Dados da OT
  LOOP AT itab_lqua.
    i_sscc-sscc          = itab_lqua-lenum.
    i_sscc-material      = itab_lqua-matnr.
    i_sscc-quantidade    = itab_lqua-gesme.
    i_sscc-uni           = itab_lqua-meins.
    i_sscc-lote_producao = itab_lqua-charg.
    APPEND i_sscc.
    CLEAR i_sscc.
  ENDLOOP.

  CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
    EXPORTING
      warehouse  = xuser-lgnum
      mov_type   = mov
      plant      = plant
      s_loc      = s_loc
      st_type_o  = p_vltyp
      bin_origem = p_vlpla
      origem     = 'X'
    TABLES
      return_msg = return_msg
      sscc       = i_sscc
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

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

      CLEAR: su, cursorfield.
    ENDIF.

  ENDIF.

ENDFORM.                    " cria_nova_to_dri
*&---------------------------------------------------------------------*
*&      Form  get_next_to_dri
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_next_to_dri .

  DATA: ecran(4),
        tipo_dri.

  FREE return_msg.
  CLEAR: return_msg.

  SELECT * FROM zwm010 INTO TABLE tab_zwm010
  WHERE armazem = xuser-lgnum AND
        equipamento = equipamento_.

*  break roffd.

  ecran = lrf_wkqu-devty(5).

  CALL FUNCTION 'ZWM_GET_TO_RET'
    EXPORTING
      armazem           = xuser-lgnum
      tamanho           = lrf_wkqu-devty(5)
    IMPORTING
      nova_to           = tab_zwm011
      tipo_queue        = tipo_dri
    TABLES
      l_zwm010          = tab_zwm010
      return_msg        = return_msg
    EXCEPTIONS
      no_equipment      = 1
      no_work_available = 2
      OTHERS            = 3.

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
          message_var2   = return_msg-msgv2.

      CLEAR: to, f3_activo, sscc_errado, incidencia,
              desc_incidencia, sscc, cursorfield, queue.
      FREE: sscc.

*      break roffd.

      f3_activo = 'X'.
      SET SCREEN '0000'.
      LEAVE SCREEN.
    ENDIF.
  ELSE.
    CALL FUNCTION 'ZWM_CALL_TASK_RET'
      EXPORTING
        armazem     = xuser-lgnum
        ecran       = ecran
        tab_zwm011  = tab_zwm011
        tipo_queue  = tipo_dri
        equipamento = equipamento_.

** Quando um operario quer sair da pistola
    IF NOT f3_activo IS INITIAL.
      CLEAR: to, sscc_errado, incidencia,
              desc_incidencia, sscc, cursorfield.
      FREE: sscc.

      IF lrf_wkqu-devty(5) = '16X20'.
        SET SCREEN '0001'.
      ELSE.
        SET SCREEN '0002'.
      ENDIF.

      LEAVE SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_next_to_dri
*&---------------------------------------------------------------------*
*&      Form  GET_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_LGNUM  text
*----------------------------------------------------------------------*
FORM get_whs CHANGING cv_lgnum TYPE lgnum.

  DATA: lt_user TYPE TABLE OF lrf_wkqu.

  DATA: ls_user TYPE lrf_wkqu.

  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = lt_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  CHECK sy-subrc EQ 0.

  READ TABLE lt_user
        INTO ls_user
        WITH KEY statu = 'X'. "Util. Atrib. Arm.
  CHECK sy-subrc EQ 0.

  cv_lgnum = ls_user-lgnum.

ENDFORM.                    " GET_WHS
