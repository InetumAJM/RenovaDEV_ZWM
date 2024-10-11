FUNCTION zwm_get_door.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"     REFERENCE(TIPO_CAMIAO) TYPE  ZTIPO_CAMIAO
*"     REFERENCE(MATRICULA) LIKE  ZWM003-MATRICULA
*"     REFERENCE(NUM_ENTRADA) LIKE  ZWM003-NUM_ENTRADA
*"  EXPORTING
*"     REFERENCE(PORTA) LIKE  ZWM002-PORTA
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"      L_ZWM002 STRUCTURE  ZWM002 OPTIONAL
*"  EXCEPTIONS
*"      NO_WAREHOUSE
*"      WRONG_TIPO_CAMIAO
*"      ERROR_IN_MANAGE_DOORS
*"      ERROR_IN_DOORS
*"      NO_DOOR_TO_DIRECT_CHARGE
*"      NO_DOOR_EXISTS
*"      NO_DELIVERY_EXISTS
*"----------------------------------------------------------------------
  DATA : ti_zwm003 LIKE zwm003 OCCURS 0 WITH HEADER LINE,
         ti_zwm002 LIKE zwm002 OCCURS 0 WITH HEADER LINE.

  DATA : porta_d LIKE zwm002-porta,
         pulmao LIKE zwm007-pulmao1,
         pulmao1(14),
         pulmao2(14),
         pulmao_aux LIKE lagp-lgber,
         num_quantos TYPE i,
         encontrou_pulmao.

  DATA : BEGIN OF dados_transporte OCCURS 0.
  DATA: picking LIKE vbfa-vbeln.
          INCLUDE STRUCTURE vttp.
  DATA : END OF dados_transporte.

** Contem as remessas associadas ao transporte
  DATA : BEGIN OF remessas OCCURS 0,
         vbeln LIKE likp-vbeln.
  DATA : END OF remessas.

** Items das remessas
  DATA : l_lips LIKE lips OCCURS 0 WITH HEADER LINE.

  DATA t_lagp LIKE lagp OCCURS 0 WITH HEADER LINE.
** Materiais das remessas e repectivas posições
  DATA : BEGIN OF material_posicao OCCURS 0,
          zona  LIKE lagp-lzone,                 "zona do armazém
          n_to TYPE i.
*          anzqu LIKE lagp-anzqu,                  "numero de quantos
*          matnr LIKE lips-vbeln,                  "material
*          lgtyp LIKE lqua-lgtyp,                  "tipo de deposito
*          lgpla LIKE lqua-lgpla.                  "posicao
  DATA : END OF material_posicao.

  DATA num_pulmao TYPE i.

  CLEAR : ti_zwm002,
          ti_zwm003,
          remessas,
          material_posicao,
          porta_d,
          encontrou_pulmao,
          num_pulmao.

  REFRESH : ti_zwm002,
            ti_zwm003,
            remessas,
            material_posicao.


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


** Verificar se o armazém introduzido é válido
  SELECT SINGLE * FROM t300
                  WHERE lgnum = armazem.
  IF sy-subrc <> 0.
    RAISE no_warehouse.
  ENDIF.


** Dados de tabelas de parametrização

** Tipo de depósito dos pulmões
  DATA : st_pul LIKE lagp-lgtyp.
  CLEAR : st_pul.
  PERFORM get_parameter USING armazem
                          'ENTRADA_ARMAZEM'
                          'ST_PUL'
                           st_pul.

  break roffd.

** Camião para descarga
  IF tipo_camiao = 'R'.

    IF l_zwm002[] IS INITIAL.

      CALL FUNCTION 'ZWM_MANAGE_DOORS'
        EXPORTING
          operacao                = '1'
          armazem                 = armazem
        TABLES
          l_zwm002                = ti_zwm002
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
      ELSE.
** Verificações ao estado das portas
        IF ti_zwm002[] IS INITIAL.
          RAISE error_in_doors.
        ELSEIF NOT ti_zwm002[] IS INITIAL.
** Descobrir a porta

          SORT ti_zwm002 BY porta.

          LOOP AT ti_zwm002 WHERE bloqueio = ' '.

            SELECT SINGLE * FROM zwm007
            WHERE armazem = armazem   AND
                    porta = ti_zwm002-porta AND
                    tipo <> 'C'.
            IF sy-subrc = 0.
              porta = ti_zwm002-porta.
** verificar se para esta porta existe algum pulmão LIVRE

** 1 - Carregar pulmões para a porta
              IF sy-subrc = 0.
                DO 24 TIMES VARYING pulmao FROM
                                   zwm007-pulmao1 NEXT
                                   zwm007-pulmao2.

** Lê os três caracteres do meio q correspondem à área e identifica
** sempre os pulmões dois a dois
                  CLEAR : pulmao_aux.
                  pulmao_aux = pulmao+4(3).

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
              ENDIF.
** É pq a porta que encontrou não é válida
              IF encontrou_pulmao IS INITIAL.
                CLEAR : porta, encontrou_pulmao.
              ELSEIF NOT encontrou_pulmao IS INITIAL.
                CLEAR : encontrou_pulmao.
                EXIT.
              ENDIF.
*              EXIT.
            ENDIF.
          ENDLOOP.

        ENDIF.
      ENDIF.

** Para o algoritmo que se suporta no PRÓXIMO CAMIÃO
    ELSEIF NOT l_zwm002[] IS INITIAL.

      SORT l_zwm002 BY porta.

      LOOP AT l_zwm002 WHERE bloqueio = ' '.

        SELECT SINGLE * FROM zwm007
        WHERE armazem = armazem   AND
                porta = l_zwm002-porta AND
                tipo <> 'C'.
        IF sy-subrc = 0.
          porta = l_zwm002-porta.

** 1 - Carregar pulmões para a porta
          IF sy-subrc = 0.
            DO 24 TIMES VARYING pulmao FROM
                               zwm007-pulmao1 NEXT
                               zwm007-pulmao2.

** SG
** Lê os três caracteres do meio q correspondem à área e identifica
** sempre os pulmões dois a dois
              CLEAR : pulmao_aux.
              pulmao_aux = pulmao+4(3).

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
          ENDIF.

** É pq a porta que encontrou não é válida
          IF encontrou_pulmao IS INITIAL.
            CLEAR : porta, encontrou_pulmao.
          ELSEIF NOT encontrou_pulmao IS INITIAL.
            CLEAR : encontrou_pulmao.
            EXIT.
          ENDIF.
*              EXIT.

        ENDIF.
      ENDLOOP.

    ENDIF.

** Camião para carga directa
  ELSEIF tipo_camiao = 'E'.

** Verificar se existe transporte
    SELECT SINGLE * FROM zwm006
                    WHERE armazem = armazem AND
                          num_entrada = num_entrada.
    IF sy-subrc = 0.

      SELECT * FROM vttp
               WHERE tknum = zwm006-n_transporte.
** Carregar a(s) vária(s) remessas
        remessas-vbeln = vttp-vbeln.
        APPEND remessas.
      ENDSELECT.

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

      CLEAR zwm028.
      SELECT SINGLE *
          FROM zwm028
              WHERE lgnum = armazem AND
                    refnr = vbss-sammg.

      IF zwm028-st_pul IS INITIAL AND zwm028-st_dck IS INITIAL.
        CLEAR: porta, remessas, material_posicao.
        REFRESH: remessas, material_posicao.
        RAISE no_door_exists.
      ENDIF.

** Carregar os materiais válidos para cada uma das remessas
      IF NOT remessas[] IS INITIAL.
        LOOP AT remessas.
          SELECT * FROM lips
                   WHERE vbeln = remessas-vbeln AND
                         ( pstyv <> 'TANN' OR
                           pstyv <> 'TATX' OR
                           pstyv <> 'ZOF' ).
            IF sy-subrc = 0.

              SELECT *
                  FROM ltak
                      WHERE lgnum = armazem AND
                            vbeln = lips-vbeln.

                SELECT SINGLE *
                    FROM ltap
                        WHERE lgnum = ltak-lgnum AND
                              tanum = ltak-tanum.

                SELECT SINGLE *
                    FROM lagp
                        WHERE lgnum = ltap-lgnum AND
                              lgtyp = ltap-vltyp AND
                              lgpla = ltap-vlpla.
                IF sy-subrc = 0.
                  READ TABLE material_posicao WITH KEY zona = lagp-lzone.
                  IF sy-subrc = 0.
                    material_posicao-n_to = material_posicao-n_to + 1.
                    MODIFY material_posicao INDEX sy-tabix.
                  ELSE.
                    material_posicao-zona = lagp-lzone.
                    material_posicao-n_to = 1.
                    APPEND material_posicao.
                    CLEAR material_posicao.
                  ENDIF.
                ENDIF.
              ENDSELECT.
            ENDIF.
          ENDSELECT.
        ENDLOOP.
      ELSE.
*        CLEAR PORTA.
*        RAISE NO_DELIVERY_EXISTS.
      ENDIF.

** Ordenar a tabela pela posição q tem mais quantos e achar a porta
** associada à mesma
      SORT material_posicao BY n_to DESCENDING.
      READ TABLE material_posicao INDEX 1.
      IF sy-subrc = 0.
** percorrer as portas assignadas à ZONA para descobrir a primeira vaga
*        BREAK-POINT.
        SELECT SINGLE * FROM zwm008
                        WHERE zona = material_posicao-zona.
        IF sy-subrc = 0.
          DO 32 TIMES VARYING porta_d FROM
                    zwm008-porta1 NEXT zwm008-porta2.
** verificar se a porta está desbloqueada
            SELECT SINGLE * FROM zwm002
                            WHERE armazem = armazem AND
                                  porta = porta_d AND
                                  bloqueio = ' ' AND
                                  estado = 'D'.
            IF sy-subrc = 0.
              SELECT SINGLE *
                  FROM zwm007
                      WHERE armazem = armazem AND
                            porta = porta_d AND
                            tipo <> 'D'.
              IF sy-subrc = 0.
                porta = porta_d.
                EXIT.
              ELSE.
                CLEAR porta.
                CONTINUE.
              ENDIF.
            ELSE.
** Não existe porta desbloqueada
              CLEAR porta.
            ENDIF.
          ENDDO.
          IF porta IS INITIAL.
            RAISE no_door_exists.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR : material_posicao.
      REFRESH: material_posicao.

    ELSE.
      CLEAR porta.
      RAISE no_door_to_direct_charge.
    ENDIF.

** Camião para carga através de pulmão
  ELSEIF tipo_camiao = 'P'.

** A carga já está completa ... descobrir qual o pulmão pertencente à
** CARGA

** Descobrir remessas associadas ao transporte

    SELECT SINGLE * FROM zwm006
                    WHERE armazem = armazem AND
                          num_entrada = num_entrada.
    IF sy-subrc = 0.
** Número do transporte SAP
*      BREAK-POINT.
      SELECT SINGLE * FROM vttp
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
*        Procurar o pulmao para o grupo que está no transporte
        CLEAR zwm028.
        SELECT SINGLE *
            FROM zwm028
                WHERE lgnum = armazem AND
                      refnr = vbss-sammg.
        break roffd.
*         Achar uma porta de carga livre associáda ao pulmão
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

          LOOP AT l_portas_cargap.

            IF zwm028-pulmao1 = l_portas_cargap-pulmao1.

              l_portas_cargap-peso = 1.
              MODIFY l_portas_cargap INDEX sy-tabix.

            ELSEIF zwm028-pulmao1 = l_portas_cargap-pulmao2.

              l_portas_cargap-peso = 2.
              MODIFY l_portas_cargap INDEX sy-tabix.

            ELSEIF zwm028-pulmao1 = l_portas_cargap-pulmao3.

              l_portas_cargap-peso = 3.
              MODIFY l_portas_cargap INDEX sy-tabix.

            ELSEIF zwm028-pulmao1 = l_portas_cargap-pulmao4.

              l_portas_cargap-peso = 4.
              MODIFY l_portas_cargap INDEX sy-tabix.

            ELSEIF zwm028-pulmao1 = l_portas_cargap-pulmao5.

              l_portas_cargap-peso = 5.
              MODIFY l_portas_cargap INDEX sy-tabix.

            ELSEIF zwm028-pulmao1 = l_portas_cargap-pulmao6.

              l_portas_cargap-peso = 6.
              MODIFY l_portas_cargap INDEX sy-tabix.

            ELSEIF zwm028-pulmao1 = l_portas_cargap-pulmao7.

              l_portas_cargap-peso = 7.
              MODIFY l_portas_cargap INDEX sy-tabix.

            ELSEIF zwm028-pulmao1 = l_portas_cargap-pulmao8.

              l_portas_cargap-peso = 8.
              MODIFY l_portas_cargap INDEX sy-tabix.

            ENDIF.
          ENDLOOP.

          SORT l_portas_cargap BY peso.
          READ TABLE l_portas_cargap INDEX 1.
          IF sy-subrc = 0.
            porta = l_portas_cargap-porta.
            EXIT.
          ENDIF.
        ENDIF.


      ENDIF.
    ENDIF.
  ELSE.
    RAISE wrong_tipo_camiao.
  ENDIF.


ENDFUNCTION.
