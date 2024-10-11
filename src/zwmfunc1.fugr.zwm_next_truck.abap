FUNCTION zwm_next_truck.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"  TABLES
*"      RETURN_TRUCK STRUCTURE  ZWM_RETURN_TRUCK
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      NO_DOOR_UNLOCKED
*"      EXCEPTION_MANAGE_PARKING
*"      NO_QUEUE_EXISTS
*"      NO_DOOR_TRUCK
*"      ERROR_IN_GET_DOOR
*"      ERROR_IN_MANAGE_DOORS
*"----------------------------------------------------------------------

  DATA : tipo_camiao TYPE ztipo_camiao,
         porta LIKE zwm002-porta,
         index LIKE sy-tabix.

** Fila de espera
  DATA : BEGIN OF l_zwm003 OCCURS 0.
          INCLUDE STRUCTURE zwm003.
  DATA : atribuida.
  DATA : END OF l_zwm003.
** Estado das portas
  DATA : BEGIN OF l_zwm002 OCCURS 0.
          INCLUDE STRUCTURE zwm002.
  DATA : END OF l_zwm002.

** Contem as portas de DESCARGA válidas caso existam
  DATA : BEGIN OF l_portas_descarga OCCURS 0.
          INCLUDE STRUCTURE zwm002.
  DATA : tipo LIKE zwm007-tipo,
         pulmao1 LIKE zwm007-pulmao1,
         pulmao2 LIKE zwm007-pulmao2,
         pulmao3 LIKE zwm007-pulmao3,
         pulmao4 LIKE zwm007-pulmao4,
         pulmao5 LIKE zwm007-pulmao5,
         pulmao6 LIKE zwm007-pulmao6,
         pulmao7 LIKE zwm007-pulmao7,
         pulmao8 LIKE zwm007-pulmao8,
         pulmao9 LIKE zwm007-pulmao9,
         pulmao10 LIKE zwm007-pulmao10,
         pulmao11 LIKE zwm007-pulmao11,
         pulmao12 LIKE zwm007-pulmao12,
         pulmao13 LIKE zwm007-pulmao13,
         pulmao14 LIKE zwm007-pulmao14,
         pulmao15 LIKE zwm007-pulmao15,
         pulmao16 LIKE zwm007-pulmao16,
         pulmao17 LIKE zwm007-pulmao17,
         pulmao18 LIKE zwm007-pulmao18,
         pulmao19 LIKE zwm007-pulmao19,
         pulmao20 LIKE zwm007-pulmao20,
         pulmao21 LIKE zwm007-pulmao21,
         pulmao22 LIKE zwm007-pulmao22,
         pulmao23 LIKE zwm007-pulmao23,
         pulmao24 LIKE zwm007-pulmao24,
         pulmao25 LIKE zwm007-pulmao25,
         pulmao26 LIKE zwm007-pulmao26,
         pulmao27 LIKE zwm007-pulmao27,
         pulmao28 LIKE zwm007-pulmao28,
         atribuida,
         peso.
  DATA : END OF l_portas_descarga.


** Contem as portas de CARGA DIRECTA válidas caso existam
  DATA : BEGIN OF l_portas_cargad OCCURS 0.
          INCLUDE STRUCTURE zwm002.
  DATA : tipo LIKE zwm007-tipo,
         pulmao1 LIKE zwm007-pulmao1,
         pulmao2 LIKE zwm007-pulmao2,
         pulmao3 LIKE zwm007-pulmao3,
         pulmao4 LIKE zwm007-pulmao4,
         pulmao5 LIKE zwm007-pulmao5,
         pulmao6 LIKE zwm007-pulmao6,
         pulmao7 LIKE zwm007-pulmao7,
         pulmao8 LIKE zwm007-pulmao8,
         pulmao9 LIKE zwm007-pulmao9,
         pulmao10 LIKE zwm007-pulmao10,
         pulmao11 LIKE zwm007-pulmao11,
         pulmao12 LIKE zwm007-pulmao12,
         pulmao13 LIKE zwm007-pulmao13,
         pulmao14 LIKE zwm007-pulmao14,
         pulmao15 LIKE zwm007-pulmao15,
         pulmao16 LIKE zwm007-pulmao16,
         pulmao17 LIKE zwm007-pulmao17,
         pulmao18 LIKE zwm007-pulmao18,
         pulmao19 LIKE zwm007-pulmao19,
         pulmao20 LIKE zwm007-pulmao20,
         pulmao21 LIKE zwm007-pulmao21,
         pulmao22 LIKE zwm007-pulmao22,
         pulmao23 LIKE zwm007-pulmao23,
         pulmao24 LIKE zwm007-pulmao24,
         pulmao25 LIKE zwm007-pulmao25,
         pulmao26 LIKE zwm007-pulmao26,
         pulmao27 LIKE zwm007-pulmao27,
         pulmao28 LIKE zwm007-pulmao28,
         atribuida,
         peso.
  DATA : END OF l_portas_cargad.


** Contem as portas de CARGA PULMÃO válidas caso existam
  DATA : BEGIN OF l_portas_cargap OCCURS 0.
          INCLUDE STRUCTURE zwm002.
  DATA : tipo LIKE zwm007-tipo,
         pulmao1 LIKE zwm007-pulmao1,
         pulmao2 LIKE zwm007-pulmao2,
         pulmao3 LIKE zwm007-pulmao3,
         pulmao4 LIKE zwm007-pulmao4,
         pulmao5 LIKE zwm007-pulmao5,
         pulmao6 LIKE zwm007-pulmao6,
         pulmao7 LIKE zwm007-pulmao7,
         pulmao8 LIKE zwm007-pulmao8,
         pulmao9 LIKE zwm007-pulmao9,
         pulmao10 LIKE zwm007-pulmao10,
         pulmao11 LIKE zwm007-pulmao11,
         pulmao12 LIKE zwm007-pulmao12,
         pulmao13 LIKE zwm007-pulmao13,
         pulmao14 LIKE zwm007-pulmao14,
         pulmao15 LIKE zwm007-pulmao15,
         pulmao16 LIKE zwm007-pulmao16,
         pulmao17 LIKE zwm007-pulmao17,
         pulmao18 LIKE zwm007-pulmao18,
         pulmao19 LIKE zwm007-pulmao19,
         pulmao20 LIKE zwm007-pulmao20,
         pulmao21 LIKE zwm007-pulmao21,
         pulmao22 LIKE zwm007-pulmao22,
         pulmao23 LIKE zwm007-pulmao23,
         pulmao24 LIKE zwm007-pulmao24,
         pulmao25 LIKE zwm007-pulmao25,
         pulmao26 LIKE zwm007-pulmao26,
         pulmao27 LIKE zwm007-pulmao27,
         pulmao28 LIKE zwm007-pulmao28,
         atribuida,
         peso.
  DATA : END OF l_portas_cargap.

** Contem as remessas associadas ao transporte
  DATA : BEGIN OF remessas OCCURS 0,
         vbeln LIKE likp-vbeln.
  DATA : END OF remessas.

** Items das remessas
  DATA : l_lips LIKE lips OCCURS 0 WITH HEADER LINE.

  DATA t_lagp LIKE lagp OCCURS 0 WITH HEADER LINE.

  DATA : pulmao LIKE zwm007-pulmao1,
         num_quantos TYPE i,
         indice LIKE sy-tabix,
         incompleta,
         encontrou_porta,
         pulmao_aux LIKE lagp-lgber,
         encontrou_pulmao,
         num_pulmao TYPE i.

  CLEAR : return_msg, l_zwm003, pulmao, num_quantos, tipo_camiao, porta,
          incompleta, encontrou_porta, t_lagp, num_pulmao.
  REFRESH: return_msg, l_zwm003, l_lips, t_lagp.


** Dados de tabelas de parametrização

** Tipo de depósito dos pulmões
  DATA : st_pul LIKE lagp-lgtyp.
  CLEAR : st_pul.
  PERFORM get_parameter USING armazem
                          'ENTRADA_ARMAZEM'
                          'ST_PUL'
                           st_pul.


** Carregamento do estado GLOBAL do estado das portas
  CLEAR : l_zwm002.
  REFRESH: l_zwm002.

  CALL FUNCTION 'ZWM_MANAGE_DOORS'
    EXPORTING
      operacao                = '1'
      armazem                 = armazem
    TABLES
      l_zwm002                = l_zwm002
      return_msg              = return_msg
    EXCEPTIONS
      no_warehouse            = 1
      tab_zwm002_not_filled   = 2
      tab_l_zwm002_filled     = 3
      tab_l_zwm002_not_filled = 4
      invalid_parameter       = 5
      OTHERS                  = 6.
  IF sy-subrc <> 0.
    RAISE error_in_manage_doors.
  ENDIF.



** Primeiro - Ordenação da fila de espera para escolha do primeiro
**            camião a ser atribuido a uma porta

  CALL FUNCTION 'ZWM_MANAGE_PARKING'
    EXPORTING
      operacao                = '1'
      armazem                 = armazem
    TABLES
      l_zwm003                = l_zwm003
      return_msg              = return_msg
    EXCEPTIONS
      tab_zwm003_not_filled   = 1
      invalid_parameter       = 2
      tab_l_zwm003_filled     = 3
      tab_l_zwm003_not_filled = 4
      no_warehouse            = 5
      OTHERS                  = 6.
  IF sy-subrc <> 0.
    RAISE exception_manage_parking.
  ELSE.
** Verificar se existem camiões em fila de espera
** Se sim - continuação do algoritmo
** Se não - erro a indicar que nao existem camiões em fila de espera
    IF l_zwm003[] IS INITIAL.

      return_msg-msgid = 'ZWMMSG001'.
      return_msg-msgnr = '013'.
      return_msg-msgtyp = 'I'.
      return_msg-msgspra = sy-langu.
      APPEND return_msg.

      RAISE no_queue_exists.
    ELSEIF NOT l_zwm003[] IS INITIAL.

** Apagar as entradas que já estejam atribuídas a uma porta
      DELETE l_zwm003 WHERE estado = 'P'.

** Ordenação da fila de espera - INCOMPLETA !!!!!!! - FALTA ITENERÁRIO
*      SORT L_ZWM003 BY DATA ASCENDING HORA_ENTRADA ASCENDING.
** Ordenação da fila de espera - INCOMPLETA !!!!!!!
      CALL FUNCTION 'ZWM_SORT_QUEUE'
        EXPORTING
          armazem    = armazem
        TABLES
          return_msg = return_msg
          l_zwm003   = l_zwm003.

      DATA s_tabix LIKE sy-tabix.
      CLEAR s_tabix.
** Cálculo do algoritmo de selecção do próximo camião
      LOOP AT l_zwm003 WHERE atribuida IS INITIAL.

        s_tabix = sy-tabix.
** Tipo de camião ??
        IF l_zwm003-operacao = 'DESCARGA'.
          tipo_camiao = 'R'.
        ELSEIF l_zwm003-operacao = 'CARGA'.
** Seleccionar o transporte
          SELECT SINGLE n_transporte FROM zwm006 INTO
                                          zwm006-n_transporte
                          WHERE armazem = armazem AND
                                num_entrada = l_zwm003-num_entrada.
          IF sy-subrc = 0.

** Verificar se o transporte tem carga assignada

* Remessa
            SELECT SINGLE vbeln FROM vttp INTO vttp-vbeln
                                WHERE tknum = zwm006-n_transporte.
            IF sy-subrc = 0.
              SELECT * FROM vbss WHERE vbeln = vttp-vbeln.

                SELECT SINGLE *
                    FROM vbsk
                        WHERE sammg = vbss-sammg AND
                              smart = 'W'.
                IF sy-subrc = 0.
                  EXIT.
                ELSE.
                  CLEAR vbss.
                ENDIF.
              ENDSELECT.
              SELECT SINGLE *
                  FROM zwm028
                      WHERE lgnum = armazem AND
                            refnr = vbss-sammg.

              IF zwm028-st_pul = 'PUL' OR zwm028-st_dck = 'PUL'.
                tipo_camiao = 'P'.
              ELSEIF zwm028-st_dck = 'DCK'.
                tipo_camiao = 'E'.
              ELSE.
                tipo_camiao = 'E'.
              ENDIF.
            ELSE.
              tipo_camiao = 'E'.
            ENDIF.
          ELSE.
            tipo_camiao = 'E'.
          ENDIF.

** Verificar se o transporte tem carga assignada
        ELSE.
          tipo_camiao = 'E'.
        ENDIF.


        CLEAR : incompleta, lips-lgtyp,lagp, vttp-vbeln.

** Algoritmo de selecção do próximo camião

        IF tipo_camiao = 'R'.
** Verificar se existe pelo menos um PULMÃO LIVRE entre os q se
** encontram associados às portas de descarga

          IF l_portas_descarga[] IS INITIAL.

            SELECT * INTO CORRESPONDING FIELDS OF TABLE
                                        l_portas_descarga
                     FROM zwm002 AS w INNER JOIN zwm007 AS m
                     ON w~armazem = m~armazem AND
                        w~porta = m~porta
                     WHERE w~armazem = armazem AND
                           w~bloqueio = ' ' AND
                           w~estado = 'D' AND
                           m~tipo <> 'C'.
          ENDIF.

          IF sy-subrc = 0.

            SORT l_portas_descarga BY porta ASCENDING.

*            READ TABLE L_PORTAS_DESCARGA WITH KEY ATRIBUIDA = ' '.
            LOOP AT l_portas_descarga WHERE atribuida = ' '.
              DO 12 TIMES VARYING pulmao FROM
                                 l_portas_descarga-pulmao1 NEXT
                                 l_portas_descarga-pulmao2.

** Lê os três caracteres do meio q correspondem à área e identifica
** sempre os pulmões dois a dois
                CLEAR : pulmao_aux.
                pulmao_aux = pulmao+4(3).
*                break roffd.
** Possivelmente falta a área '001' para verificar se um conjunto de
** DOIS CANAIS estão LIVRES
                SELECT * INTO CORRESPONDING FIELDS OF TABLE t_lagp
                    FROM lagp
                        WHERE lgnum = armazem AND
                              lgtyp = st_pul AND
                              lgber = pulmao_aux AND
                              brand = ' '.

                IF t_lagp[] IS INITIAL.
                  CLEAR encontrou_pulmao.
                  CONTINUE.
                ENDIF.

                DESCRIBE TABLE t_lagp LINES num_pulmao.
                IF num_pulmao = 1.
                  CLEAR encontrou_pulmao.
                  CONTINUE.
                ENDIF.

                READ TABLE t_lagp INDEX 1.
                CLEAR: pulmao1, pulmao2.

                CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                  EXPORTING
                    lgtyp = t_lagp-lgtyp
                    lgpla = t_lagp-lgpla
                  IMPORTING
                    bin   = pulmao1.

                READ TABLE t_lagp INDEX 2.
                CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                  EXPORTING
                    lgtyp = t_lagp-lgtyp
                    lgpla = t_lagp-lgpla
                  IMPORTING
                    bin   = pulmao2.

                SELECT COUNT(*) INTO num_quantos
                        FROM zwm013
                            WHERE armazem = armazem AND
                                  ( destino = pulmao1 OR
                                    destino = pulmao2 ).
                IF sy-subrc = 0.
                  IF num_quantos <> 0.
                    CLEAR encontrou_pulmao.
                    CONTINUE.
                  ENDIF.
                ENDIF.

                CLEAR num_quantos.
                encontrou_pulmao = 'X'.
                EXIT.

              ENDDO.
** É pq a porta que encontrou não é válida
              IF encontrou_pulmao IS INITIAL.
                CLEAR : porta, encontrou_pulmao, num_quantos.
              ELSEIF NOT encontrou_pulmao IS INITIAL.
                CLEAR : encontrou_pulmao, num_quantos, sy-subrc.
                EXIT.
              ENDIF.

            ENDLOOP.

** Existe pelo menos um pulmão LIVRE de descarga pode prosseguir para o
** ALGORITMO de atribuição de PORTA
            IF num_quantos = 0 AND sy-subrc = 0.

              CALL FUNCTION 'ZWM_GET_DOOR'
                EXPORTING
                  armazem                  = armazem
                  tipo_camiao              = tipo_camiao
                  matricula                = l_zwm003-matricula
                  num_entrada              = l_zwm003-num_entrada
                IMPORTING
                  porta                    = porta
                TABLES
                  return_msg               = return_msg
                  l_zwm002                 = l_zwm002
                EXCEPTIONS
                  no_warehouse             = 1
                  wrong_tipo_camiao        = 2
                  error_in_manage_doors    = 3
                  error_in_doors           = 4
                  no_door_to_direct_charge = 5
                  OTHERS                   = 6.
              IF sy-subrc <> 0.

              ELSE.
                IF NOT porta IS INITIAL.
**TABELA AUXILIAR
                  DATA : BEGIN OF lagp_aux OCCURS 2,
                          lgpla LIKE lagp-lgpla.
                  DATA : END OF lagp_aux.

                  CLEAR lagp_aux.
                  REFRESH lagp_aux.
** Seleccionar os pulmões atribuídos à porta
                  SELECT * FROM lagp
                            WHERE lgnum = armazem AND
                                  lgtyp = st_pul AND
                                  lgber = pulmao_aux AND
                                  brand = ' '.
                    IF sy-subrc = 0.
                      lagp_aux-lgpla = lagp-lgpla.
                      APPEND lagp_aux.
                    ENDIF.
                  ENDSELECT.
** Seleccionar os pulmões atribuídos à porta
*                  CLEAR return_truck.
*                  REFRESH return_truck.
** Actualização da tabela de retorno com camião e porta atribuida
                  return_truck-porta = porta.
                  return_truck-matricula = l_zwm003-matricula.
                  return_truck-observacoes = l_zwm003-observacoes.
                  return_truck-num_entrada = l_zwm003-num_entrada.
** Transportador
                  SELECT SINGLE transportador FROM zwm005 INTO
                                         return_truck-transportador
                           WHERE num_entrada = l_zwm003-num_entrada.
** Nome do transportador
                  SELECT SINGLE name1 FROM lfa1 INTO
                                return_truck-desc_transportador
                           WHERE lifnr = return_truck-transportador.

** Seleccionar os pulmões atribuídos à porta
                  READ TABLE lagp_aux INDEX 1.
                  IF sy-subrc = 0.
                    return_truck-pulmao_1 = lagp_aux-lgpla.
                  ENDIF.

                  READ TABLE lagp_aux INDEX 2.
                  IF sy-subrc = 0.
                    return_truck-pulmao_2 = lagp_aux-lgpla.
                  ENDIF.
** Seleccionar os pulmões atribuídos à porta

                  APPEND return_truck.

** Actualizar fila de espera a indicar q o camião foi atribuido
                  l_zwm003-atribuida = 'X'.
                  MODIFY l_zwm003 INDEX s_tabix.

** Actualizar estado da porta a indicar q a porta está atribuida
                  READ TABLE l_portas_descarga WITH KEY porta = porta.
                  IF sy-subrc = 0.
                    index = sy-tabix.
                    l_portas_descarga-atribuida = 'X'.
                    MODIFY l_portas_descarga INDEX index.
                  ENDIF.

** Actualizar tabela com o estado GLOBAL
                  CLEAR index.
                  READ TABLE l_zwm002 WITH KEY porta = porta.
                  IF sy-subrc = 0.
                    index = sy-tabix.
                    l_zwm002-bloqueio = 'X'.

** Seleccionar os pulmões atribuídos à porta
                    READ TABLE lagp_aux INDEX 1.
                    IF sy-subrc = 0.
                      l_zwm002-pulmao_1 = lagp_aux-lgpla.
                    ENDIF.

                    READ TABLE lagp_aux INDEX 2.
                    IF sy-subrc = 0.
                      l_zwm002-pulmao_2 = lagp_aux-lgpla.
                    ENDIF.
** Seleccionar os pulmões atribuídos à porta

                    MODIFY l_zwm002 INDEX index.
                  ENDIF.

** Actualizar tabela de posições de depósito (lagp) com o indicador de
** OCUPADO para os dois pulmões de descarga
                  LOOP AT lagp_aux.
                    UPDATE lagp SET brand = 'X'
                                WHERE lgnum = armazem AND
                                      lgtyp = st_pul AND
                                      lgpla = lagp_aux-lgpla.
                    COMMIT WORK.
                  ENDLOOP.

                  CLEAR : return_truck, l_zwm003, zwm007, index ,
                          l_zwm002, lagp_aux, pulmao_aux.
                  CLEAR : num_quantos, pulmao, porta, tipo_camiao.

                  REFRESH : lagp_aux.
                ENDIF.
              ENDIF.

            ENDIF.   " num_quantos = 0

          ENDIF.


        ELSEIF tipo_camiao = 'E'.

** Camião de carga directa ... segue para o ALGORITMO de atribuição de
** porta

** Descobrir quais as portas associadas ao pulmão e que estejam
** desbloqueadas
          IF l_portas_cargad[] IS INITIAL.

            SELECT * INTO CORRESPONDING FIELDS OF TABLE
                                        l_portas_cargad
                     FROM zwm002 AS w INNER JOIN zwm007 AS m
                     ON w~armazem = m~armazem AND
                        w~porta = m~porta
                     WHERE w~armazem = armazem AND
                           w~bloqueio = ' ' AND
                           w~estado = 'D' AND
                           m~tipo <> 'D'.
          ENDIF.

** Só executa o algoritmo de atribuição de porta caso exista um
** transporte SAP associado ao camião
          IF NOT zwm006-n_transporte IS INITIAL.

            CALL FUNCTION 'ZWM_GET_DOOR'
              EXPORTING
                armazem                  = armazem
                tipo_camiao              = tipo_camiao
                matricula                = l_zwm003-matricula
                num_entrada              = l_zwm003-num_entrada
              IMPORTING
                porta                    = porta
              TABLES
                return_msg               = return_msg
                l_zwm002                 = l_zwm002
              EXCEPTIONS
                no_warehouse             = 1
                wrong_tipo_camiao        = 2
                error_in_manage_doors    = 3
                error_in_doors           = 4
                no_door_to_direct_charge = 5
                OTHERS                   = 6.
            IF sy-subrc <> 0.

*            RETURN_MSG-MSGID = 'ZWMMSG001'.
*            RETURN_MSG-MSGNR = '022'.
*            RETURN_MSG-MSGTYP = 'I'.
*            RETURN_MSG-MSGSPRA = SY-LANGU.
*            APPEND RETURN_MSG.
*
*            RAISE ERROR_IN_GET_DOOR.
            ELSE.
              IF NOT porta IS INITIAL.
** Actualização da tabela de retorno com camião e porta atribuida
                return_truck-porta = porta.
                return_truck-matricula = l_zwm003-matricula.
                return_truck-observacoes = l_zwm003-observacoes.
                return_truck-num_entrada = l_zwm003-num_entrada.
** Transportador
                SELECT SINGLE transportador FROM zwm005 INTO
                                        return_truck-transportador
                           WHERE num_entrada = l_zwm003-num_entrada.
** Nome do transportador
                SELECT SINGLE name1 FROM lfa1 INTO
                              return_truck-desc_transportador
                           WHERE lifnr = return_truck-transportador.

                CLEAR return_truck-pulmao_1.
                CLEAR return_truck-pulmao_2.
                APPEND return_truck.

** Actualizar estado da porta a indicar q a porta está atribuida
                READ TABLE l_portas_cargad WITH KEY porta = porta.
                IF sy-subrc = 0.
                  index = sy-tabix.
                  l_portas_cargad-atribuida = 'X'.
                  MODIFY l_portas_cargad INDEX index.
                ENDIF.

                l_zwm003-atribuida = 'X'.
                MODIFY l_zwm003 INDEX sy-tabix.


** Actualizar tabela com o estado GLOBAL
                CLEAR index.
                READ TABLE l_zwm002 WITH KEY porta = porta.
                IF sy-subrc = 0.
                  index = sy-tabix.
                  l_zwm002-bloqueio = 'X'.
                  MODIFY l_zwm002 INDEX index.
                ENDIF.

                CLEAR : return_truck, l_zwm003, zwm007.
                CLEAR : num_quantos, pulmao, porta, tipo_camiao.

              ENDIF.
            ENDIF.

          ENDIF.

        ELSEIF tipo_camiao = 'P'.


          SELECT SINGLE * FROM vttp WHERE tknum = zwm006-n_transporte.

          IF sy-subrc = 0.

            SELECT * FROM vbss WHERE vbeln = vttp-vbeln.

              SELECT SINGLE *
                  FROM vbsk
                      WHERE sammg = vbss-sammg AND
                            smart = 'W'.
              IF sy-subrc = 0.
                EXIT.
              ELSE.
                CLEAR vbss.
              ENDIF.

            ENDSELECT.

            SELECT SINGLE *
              FROM zwm028
                  WHERE lgnum = armazem AND
                        refnr = vbss-sammg.

            IF zwm028-total_paletes <> zwm028-paletes_pulmao.
              incompleta = 'X'.
            ENDIF.

          ENDIF.



** Se a carga já está completa ... segue-se ... senão passa-se para o
** próximo camião
          IF incompleta IS INITIAL.

** Descobrir quais as portas associadas ao pulmão e que estejam
** desbloqueadas
            IF l_portas_cargap[] IS INITIAL.

*                  LOOP AT l_lips.

              SELECT * INTO CORRESPONDING FIELDS OF TABLE
                                          l_portas_cargap
                      FROM zwm002 AS w INNER JOIN zwm007 AS m
                       ON w~armazem = m~armazem AND
                          w~porta = m~porta
                       WHERE w~armazem = armazem AND
                             w~bloqueio = ' ' AND
                             w~estado = 'D' AND
                             m~tipo <> 'D' AND
                             ( m~pulmao1 = zwm028-pulmao1 OR
                               m~pulmao2 = zwm028-pulmao1 OR
                               m~pulmao3 = zwm028-pulmao1 OR
                               m~pulmao4 = zwm028-pulmao1 OR
                               m~pulmao5 = zwm028-pulmao1 OR
                               m~pulmao6 = zwm028-pulmao1 OR
                               m~pulmao7 = zwm028-pulmao1 OR
                               m~pulmao8 = zwm028-pulmao1 OR
                               m~pulmao9 = zwm028-pulmao1 OR
                               m~pulmao10 = zwm028-pulmao1 OR
                               m~pulmao11 = zwm028-pulmao1 OR
                               m~pulmao12 = zwm028-pulmao1 OR
                               m~pulmao13 = zwm028-pulmao1 OR
                               m~pulmao14 = zwm028-pulmao1 OR
                               m~pulmao15 = zwm028-pulmao1 OR
                               m~pulmao16 = zwm028-pulmao1 OR
                               m~pulmao17 = zwm028-pulmao1 OR
                               m~pulmao18 = zwm028-pulmao1 OR
                               m~pulmao19 = zwm028-pulmao1 OR
                               m~pulmao20 = zwm028-pulmao1 OR
                               m~pulmao21 = zwm028-pulmao1 OR
                               m~pulmao22 = zwm028-pulmao1 OR
                               m~pulmao23 = zwm028-pulmao1 OR
                               m~pulmao24 = zwm028-pulmao1 OR
                               m~pulmao25 = zwm028-pulmao1 OR
                               m~pulmao26 = zwm028-pulmao1 OR
                               m~pulmao27 = zwm028-pulmao1 OR
                               m~pulmao28 = zwm028-pulmao1 ).
              IF sy-subrc = 0.

                encontrou_porta = 'X'.

              ENDIF.
            ENDIF.

            IF sy-subrc = 0 AND NOT encontrou_porta IS INITIAL.
** Achou portas desbloqueadas ... algoritmo de escolha de porta

              CALL FUNCTION 'ZWM_GET_DOOR'
                EXPORTING
                  armazem                  = armazem
                  tipo_camiao              = tipo_camiao
                  matricula                = l_zwm003-matricula
                  num_entrada              = l_zwm003-num_entrada
                IMPORTING
                  porta                    = porta
                TABLES
                  return_msg               = return_msg
                  l_zwm002                 = l_zwm002
                EXCEPTIONS
                  no_warehouse             = 1
                  wrong_tipo_camiao        = 2
                  error_in_manage_doors    = 3
                  error_in_doors           = 4
                  no_door_to_direct_charge = 5
                  OTHERS                   = 6.
              IF sy-subrc <> 0.
** ERRO
              ELSE.

                IF NOT porta IS INITIAL.
** Actualização da tabela de retorno com camião e porta atribuida
                  return_truck-porta = porta.
                  return_truck-matricula = l_zwm003-matricula.
                  return_truck-observacoes = l_zwm003-observacoes.
                  return_truck-num_entrada = l_zwm003-num_entrada.
** Transportador
                  SELECT SINGLE transportador FROM zwm005 INTO
                                     return_truck-transportador
                       WHERE num_entrada = l_zwm003-num_entrada.
** Nome do transportador
                  SELECT SINGLE name1 FROM lfa1 INTO
                                return_truck-desc_transportador
                       WHERE lifnr = return_truck-transportador.


                  IF zwm028-st_pul = 'PUL'.
                    return_truck-pulmao_1 = zwm028-pulmao1.
                    return_truck-pulmao_2 = zwm028-pulmao2.
                  ELSE.
                    CLEAR: return_truck-pulmao_1,
                           return_truck-pulmao_2.
                  ENDIF.
                  APPEND return_truck.

** Actualizar estado da porta a indicar q a porta está atribuida
                  READ TABLE l_portas_cargap WITH KEY porta = porta.
                  IF sy-subrc = 0.
                    index = sy-tabix.
                    l_portas_cargap-atribuida = 'X'.
                    MODIFY l_portas_cargap INDEX index.
                  ENDIF.

                  l_zwm003-atribuida = 'X'.
                  MODIFY l_zwm003 INDEX sy-tabix.

** Actualizar tabela com o estado GLOBAL
                  CLEAR index.
                  READ TABLE l_zwm002 WITH KEY porta = porta.
                  IF sy-subrc = 0.
                    index = sy-tabix.
                    l_zwm002-bloqueio = 'X'.
                    MODIFY l_zwm002 INDEX index.
                  ENDIF.

                  CLEAR : return_truck, l_zwm003, zwm007, remessas,
                          l_zwm002, l_lips.
                  CLEAR : num_quantos, pulmao, porta, tipo_camiao,
                          encontrou_porta.

                  REFRESH : remessas, l_lips.

                ENDIF.
              ENDIF.             "Get_door
            ENDIF.
          ENDIF.          " incompleta
        ENDIF.            "tipo_camiao = ??
      ENDLOOP.

    ENDIF.

  ENDIF.                "ZWM_MANAGE_PARKING


  CLEAR : l_portas_descarga, l_portas_cargap, l_portas_cargad, l_zwm002.
  REFRESH : l_portas_descarga, l_portas_cargap, l_portas_cargad,
            l_zwm002.


  IF return_truck[] IS INITIAL.

    return_msg-msgid = 'ZWMMSG001'.
    return_msg-msgnr = '014'.
    return_msg-msgtyp = 'I'.
    return_msg-msgspra = sy-langu.
    APPEND return_msg.

    RAISE no_door_truck.

  ENDIF.

ENDFUNCTION.
