FUNCTION zwm_get_to_tri .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"     REFERENCE(TAMANHO) TYPE  CHAR5 OPTIONAL
*"     REFERENCE(SENTIDO) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(TANUM) TYPE  TANUM OPTIONAL
*"  EXPORTING
*"     REFERENCE(NOVA_TO) TYPE  ZWM011
*"     REFERENCE(ECRA) TYPE  CHAR4
*"     REFERENCE(TIPO_QUEUE) LIKE  ZWM010-TIPO
*"  TABLES
*"      L_ZWM010 STRUCTURE  ZWM010
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      NO_EQUIPMENT
*"      NO_WORK_AVAILABLE
*"----------------------------------------------------------------------

** Variáveis globais à função
  DATA : wait_time(20), lock_key(50), queue LIKE ltak-queue, is_equal,
  ultimo_bin LIKE zwm011-ultimo_bin, save_index LIKE sy-tabix,
  queue_entrada_tri LIKE ltak-queue,
  queue_saida_tri LIKE ltak-queue,
  queue_pick_tri LIKE ltak-queue,
  production_queue(20),
  dev_inc_queue(20),
  tri_out_queue(20),
  tri_out_prm_queue(20),
  tri_out_cpk_queue(20),
  ultimo_tipo_dep LIKE zwm011-ultimo_tipo_dep,
*         corredor(3),
*         profundidade(3),
*         nivel(2),
  equipamento_actual LIKE zwm010-equipamento,
  sem_remessa LIKE zwm026-remessa.

** RL -> INS 08.05.2005
  DATA: l_to_11   LIKE ltap-tanum,
        l_item_11 LIKE ltap-tapos.
** RL <- INS 08.05.2005

  DATA: corredor     TYPE i,
        profundidade TYPE i,
        nivel        TYPE i.

  DATA: w_uinfo LIKE uinfo OCCURS 0 WITH HEADER LINE,
        l_uinfo LIKE uinfo OCCURS 0 WITH HEADER LINE.

** Tabelas auxiliares
  DATA : BEGIN OF l_zwm011 OCCURS 0.
           INCLUDE STRUCTURE zwm011.
         DATA : END OF l_zwm011.

  DATA : BEGIN OF l_zwm011_aux OCCURS 0.
           INCLUDE STRUCTURE zwm011.
         DATA : END OF l_zwm011_aux.

** Bloqueios de TOs
  DATA : BEGIN OF l_zwm013 OCCURS 0.
           INCLUDE STRUCTURE zwm013.
         DATA : END OF l_zwm013.

** Associação de corredores a operadores de TRILATERAIS
  DATA : BEGIN OF users_row OCCURS 0.
           INCLUDE STRUCTURE zwm_row_users.
         DATA : END OF users_row.

** Tabela com queues para passar à função
  DATA : BEGIN OF queues OCCURS 0,
           queue LIKE ltak-queue.
  DATA : END OF queues.

  STATICS: static_sentido(1).

  RANGES: equipamento_queue FOR ltak-queue.

  DATA: lt_ltak TYPE SORTED TABLE OF ltak WITH UNIQUE KEY tanum,
        ls_ltak TYPE ltak.

  DATA: lv_2spart TYPE flag.


** Sentido Prioritário dos Corredores
  IF NOT sentido IS INITIAL.
    static_sentido = sentido.
  ENDIF.
  IF static_sentido IS INITIAL.
    static_sentido = 'D'.
  ENDIF.



  IF l_zwm010[] IS INITIAL.
    RAISE no_equipment.
  ELSEIF NOT l_zwm010[] IS INITIAL.


** Carregar para memória as tabelas de parametrização

    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
      EXPORTING
        whs = armazem.


** get/clean user sessions/assignments
** get user sessions
    DATA: w_uinfos LIKE uinfos OCCURS 0 WITH HEADER LINE.

    CALL FUNCTION 'TH_SYSTEMWIDE_USER_LIST'
      TABLES
        list = w_uinfos.

    DELETE w_uinfos WHERE bname IS INITIAL.


** Tempo de espera no lock
    PERFORM get_parameter
            USING armazem
                  'ATRIBUICAO_TO'
                  'WAIT_TIME'
                  wait_time.


** A palavra de lock da função passa a ser o equipamento porque os locks
** vão passar a ser feitos por equipamento
    CLEAR equipamento_actual.
    READ TABLE l_zwm010 INDEX 1.
    WRITE l_zwm010-equipamento TO lock_key LEFT-JUSTIFIED.
    equipamento_actual = l_zwm010-equipamento.

    DO.
      CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lock_key
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc = 0.
        EXIT.
      ELSE.
        WAIT UP TO wait_time SECONDS.
      ENDIF.
    ENDDO.

    CLEAR : ultimo_bin, ultimo_tipo_dep,corredor,
            profundidade,nivel,sem_remessa.

** Guardar o ultimo BIN onde esteve o utilizador ... para a atribuição
** da tarefa ao utilizador
    SELECT SINGLE ultimo_bin ultimo_tipo_dep FROM zwm011
                                       INTO (ultimo_bin,ultimo_tipo_dep)
                             WHERE armazem = armazem AND
                                   user_name = sy-uname AND
                                   status = 'T'.

** RL -> MOD 08.05.2005
*** Corredor / profundidade / nivel actual do trilateral
*    corredor = ultimo_bin(3).
*    profundidade = ultimo_bin+4(3).
*    nivel = ultimo_bin+8(2).
*
*** Verificar se existem TOs na tabela dos utilizadores que já estejam
*** completamente processadas (status = T) ... se existirem apaga-se por
*** equipamento e utilizador
*    DELETE FROM zwm011 WHERE armazem = armazem AND
*                             equipamento = l_zwm010-equipamento AND
*                             user_name = sy-uname AND
*                             status = 'T'.
*    COMMIT WORK.

    IF sy-subrc NE 0.

      CLEAR: l_to_11, l_item_11.

** Guardar o ultimo BIN onde esteve o utilizador ... para a atribuição
** da tarefa ao utilizador
      SELECT SINGLE to_number to_item ultimo_bin ultimo_tipo_dep
      FROM zwm011
      INTO (l_to_11, l_item_11, ultimo_bin, ultimo_tipo_dep)
      WHERE armazem     = armazem
        AND user_name   = sy-uname
        AND equipamento = equipamento_actual.

      CLEAR ltap.
      SELECT SINGLE * FROM ltap
      WHERE lgnum = armazem
        AND tanum = l_to_11
        AND tapos = l_item_11
        AND pquit = 'X'.
      IF sy-subrc = 0.
** Corredor / profundidade / nivel actual do trilateral
        corredor = ultimo_bin(3).
        profundidade = ultimo_bin+4(3).
        nivel = ultimo_bin+8(2).

** Verificar se existem TOs na tabela dos utilizadores que já estejam
** completamente processadas (status = T) ... se existirem apaga-se por
** equipamento e utilizador
        DELETE FROM zwm011 WHERE armazem = armazem AND
                                 equipamento = l_zwm010-equipamento AND
                                 user_name = sy-uname.
        COMMIT WORK AND WAIT.

      ENDIF.
    ELSE.
** Corredor / profundidade / nivel actual do trilateral
      corredor = ultimo_bin(3).
      profundidade = ultimo_bin+4(3).
      nivel = ultimo_bin+8(2).

** Verificar se existem TOs na tabela dos utilizadores que já estejam
** completamente processadas (status = T) ... se existirem apaga-se por
** equipamento e utilizador
      DELETE FROM zwm011 WHERE armazem = armazem AND
                               equipamento = l_zwm010-equipamento AND
                               user_name = sy-uname AND
                               status = 'T'.
      COMMIT WORK AND WAIT.
    ENDIF.
** RL <- MOD 08.05.2005

** Para seguir para aqui não existiu nenhum crash ...
** Actualização das possiveis queues para este equipamento

** Verificar se isto funciona correctamente - não apaga a tabela e só a
** preenche se ela for inicial
*    CLEAR QUEUES.
*    REFRESH: QUEUES.

    IF queue IS INITIAL.
      LOOP AT l_zwm010.
** Range de QUEUES para este equipamento
        IF NOT l_zwm010-queue IS INITIAL.
          equipamento_queue-sign = 'I'.
          equipamento_queue-option = 'EQ'.
          equipamento_queue-low = l_zwm010-queue.
          APPEND equipamento_queue.

** Para descobrir em que corredor estão os operários
          queues-queue = l_zwm010-queue.
          APPEND queues.

        ENDIF.
      ENDLOOP.
    ENDIF.

** Verificar se existem users com TOs associadas q estejam off-line
    SELECT * FROM zwm011 INTO TABLE l_zwm011
             WHERE armazem = armazem.

    IF sy-subrc = 0.
** Apagar as entradas dos utilizadores q estejam off-line
      LOOP AT l_zwm011 WHERE status <> 'P'.
        save_index = sy-tabix.

        READ TABLE w_uinfos WITH KEY bname = l_zwm011-user_name.
        IF sy-subrc <> 0.
          DELETE l_zwm011 INDEX save_index.
        ENDIF.
      ENDLOOP.
    ENDIF.

** Actualização da BD
    MODIFY zwm011 FROM TABLE l_zwm011.
    COMMIT WORK.


    CLEAR is_equal.
*** Verificar se existiu um crash ... assignar de novo a TO - SÓ PARA
*** USER DO TRILATERAL QUE ESTÁ A ARRUMAR
*    SELECT SINGLE * FROM zwm011
*                    WHERE armazem = armazem AND
*                          user_name = sy-uname AND
*                          status = 'C'.
*    IF sy-subrc = 0.
*      MOVE zwm011 TO nova_to.
*
*      SELECT SINGLE tipo FROM zwm010 INTO tipo_queue
*                 WHERE armazem = armazem AND
*                       equipamento = l_zwm010-equipamento AND
*                       queue = zwm011-queue.
*
*      is_equal = 'X'.
*    ENDIF.

    SELECT SINGLE * FROM zwm011
        WHERE armazem = armazem AND
              user_name = sy-uname AND
              status = 'C'.

    IF sy-subrc = 0.
      CLEAR ltap.
      SELECT SINGLE *
          FROM ltap
              WHERE lgnum = zwm011-armazem AND
                    tanum = zwm011-to_number AND
                    tapos = zwm011-to_item AND
                    pquit = 'X'.
      IF sy-subrc = 0.
        DELETE FROM zwm011
            WHERE armazem = zwm011-armazem AND
                  to_number = zwm011-to_number AND
                  to_item = zwm011-to_item.
        COMMIT WORK.
      ELSE.
        MOVE zwm011 TO nova_to.
        SELECT SINGLE tipo FROM zwm010 INTO tipo_queue
                 WHERE armazem = armazem AND
                       equipamento = l_zwm010-equipamento AND
                       queue = zwm011-queue.

        is_equal = 'X'.
      ENDIF.
    ENDIF.

    IF NOT is_equal IS INITIAL.
      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lock_key
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      CLEAR : lock_key, is_equal,equipamento_actual.
      EXIT.
    ENDIF.

    SELECT SINGLE * FROM zwm011
                         WHERE armazem = armazem AND
                               user_name = sy-uname AND
                               status = 'P' AND
                               ( queue = 'QUEUEST' OR
                                 queue = 'QUEUEPT').
    IF sy-subrc = 0.
      CLEAR ltap.
      SELECT SINGLE *
          FROM ltap
              WHERE lgnum = zwm011-armazem AND
                    tanum = zwm011-to_number AND
                    tapos = zwm011-to_item AND
                    pvqui = 'X'.
      IF sy-subrc = 0.
        DELETE FROM zwm011
            WHERE armazem = zwm011-armazem AND
                  to_number = zwm011-to_number AND
                  to_item = zwm011-to_item.
        COMMIT WORK.
      ELSE.
        MOVE zwm011 TO nova_to.
        SELECT SINGLE tipo FROM zwm010 INTO tipo_queue
                 WHERE armazem = armazem AND
                       equipamento = l_zwm010-equipamento AND
                       queue = zwm011-queue.

        is_equal = 'X'.
      ENDIF.
    ENDIF.

    IF NOT is_equal IS INITIAL.
      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lock_key
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      CLEAR : lock_key, is_equal,equipamento_actual.
      EXIT.
    ENDIF.


    SELECT SINGLE * FROM zwm011
                         WHERE armazem = armazem AND
                               user_name = sy-uname AND
                               status = 'P' AND
                               ( queue = 'QUEUE1' OR
                                 queue = 'QUEUEDIC').
    IF sy-subrc = 0.
      CLEAR ltap.
      SELECT SINGLE *
          FROM ltap
              WHERE lgnum = zwm011-armazem AND
                    tanum = zwm011-to_number AND
                    tapos = zwm011-to_item AND
                    pquit = 'X'.
      IF sy-subrc = 0.
        DELETE FROM zwm011
            WHERE armazem = zwm011-armazem AND
                  to_number = zwm011-to_number AND
                  to_item = zwm011-to_item.
        COMMIT WORK.
      ELSE.
        MOVE zwm011 TO nova_to.
        SELECT SINGLE tipo FROM zwm010 INTO tipo_queue
         WHERE armazem = armazem AND
               equipamento = l_zwm010-equipamento AND
               queue = zwm011-queue.
        is_equal = 'X'.
      ENDIF.
    ENDIF.

    IF NOT is_equal IS INITIAL.
      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lock_key
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      CLEAR : lock_key, is_equal,equipamento_actual.
      EXIT.
    ENDIF.

** Carregar as TO para a(s) queue(s) pertencentes a este equipamento que
** não estejam confirmadas a nivel de cabeçalho
** Fila de saida da zona de TRILATERIAIS
    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_S_TRI_PAL_PRM'
                  tri_out_prm_queue.

    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_S_TRI_CPK'
                  tri_out_cpk_queue.


    SELECT * INTO CORRESPONDING FIELDS OF TABLE l_ltap BYPASSING BUFFER
                    FROM ltak AS k INNER JOIN ltap AS p
                    ON  k~lgnum = p~lgnum AND
                        k~tanum = p~tanum
                    WHERE k~lgnum = armazem AND
                          k~kquit = ' ' AND
                          k~queue IN equipamento_queue AND
                          p~pquit = ' '.

*      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_ltap BYPASSING BUFFER
*                    FROM ltak AS k INNER JOIN ltap AS p
*                    ON  k~lgnum = p~lgnum AND
*                        k~tanum = p~tanum
*                    WHERE k~lgnum = armazem AND
*                          k~kquit = ' ' AND
*                          k~queue = 'QUEUETPRM' AND
*                          p~pquit = ' '.

**********************************************************************
** TESTE DE ALTERAÇÃO - Diogo
***********************************************************************
    IF NOT tanum IS INITIAL.
      DELETE l_ltap WHERE tanum <> tanum.
    ENDIF.
**********************************************************************

** Verificar quais as TO interessantes para este equipamento ...
    IF sy-subrc = 0.
      IF NOT l_ltap[] IS INITIAL.
        SELECT * FROM ltak
           INTO TABLE lt_ltak
           FOR ALL ENTRIES IN l_ltap
           WHERE lgnum = l_ltap-lgnum AND
                 tanum = l_ltap-tanum.
      ENDIF.


      CLEAR save_index.
      LOOP AT l_ltap.
        save_index = sy-tabix.

        CLEAR: ls_ltak.
        READ TABLE lt_ltak
              INTO ls_ltak
              WITH TABLE KEY tanum = l_ltap-tanum.

        CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
          EXPORTING
            i_lgnum  = l_ltap-lgnum
            i_refnr  = l_ltap-refnr
          IMPORTING
            e_2spart = lv_2spart
          EXCEPTIONS
            error    = 1
            OTHERS   = 2.


        IF lv_2spart EQ abap_true AND
           l_ltap-vbeln IS INITIAL AND
           ls_ltak-betyp EQ 'L' AND
           NOT ls_ltak-benum IS INITIAL.
          l_ltap-vbeln = ls_ltak-benum.
        ENDIF.


        "Assignção a user
        SELECT SINGLE userass FROM zwm028
                              INTO l_ltap-userass
                              WHERE lgnum = l_ltap-lgnum AND
                                    refnr = l_ltap-refnr AND
                                    remessa = sem_remessa.

        IF l_ltap-userass <> '' AND l_ltap-userass <> sy-uname.
          DELETE l_ltap INDEX save_index.
          CONTINUE.
        ENDIF.


        READ TABLE l_zwm010 WITH KEY queue = l_ltap-queue.
        IF sy-subrc = 0.
** Entrada
          IF l_zwm010-tipo = 'E'.
            IF l_ltap-pvqui IS INITIAL.
              DELETE l_ltap INDEX save_index.
            ELSE.
** Actualizar prioridade associada a QUEUE
              MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
              MOVE l_zwm010-tipo TO l_ltap-tipo.
              MODIFY l_ltap INDEX save_index.
            ENDIF.
** Saida
          ELSEIF l_zwm010-tipo = 'S'.

            IF NOT l_ltap-pvqui IS INITIAL.
              DELETE l_ltap INDEX save_index.
            ELSE.

              IF l_ltap-queue = tri_out_prm_queue OR
                 l_ltap-queue = tri_out_cpk_queue.

                MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
                MOVE l_zwm010-tipo       TO l_ltap-tipo.

                l_ltap-prioridade = zwm028-prioridade.
                MODIFY l_ltap INDEX save_index.

              ELSEIF l_ltap-queue <> 'QUEUEPT'.
** Para as TOs de saída temos de ver o status de liberação
** do grupo ... só assim poderemos dar as TOs
                SELECT SINGLE * FROM zwm028
                           WHERE lgnum = l_ltap-lgnum AND
                                 refnr = l_ltap-refnr AND
                                 remessa = sem_remessa.
                IF sy-subrc = 0.
** 3 - LOCK PALETES COMPLETAS
** 4 - LOCK AMBAS
                  IF zwm028-tipo_lock = 'G' OR
                     zwm028-tipo_lock IS INITIAL.

                    IF zwm028-zlock <> '3' AND zwm028-zlock <> '4'.
                      DELETE l_ltap INDEX save_index.
                    ELSE.
                      MOVE l_zwm010-prioridade
                                         TO l_ltap-prioridade_queue.
                      MOVE l_zwm010-tipo TO l_ltap-tipo.
                      l_ltap-prioridade = zwm028-prioridade.
                      MODIFY l_ltap INDEX save_index.
                    ENDIF.
                  ELSEIF zwm028-tipo_lock = 'R'.
                    CLEAR zwm028.
                    SELECT SINGLE * FROM zwm028
                        WHERE lgnum = l_ltap-lgnum AND
                              refnr = l_ltap-refnr AND
                              remessa = l_ltap-vbeln.
                    IF sy-subrc <> 0.
                      CLEAR zwm040.
                      SELECT SINGLE * FROM zwm040
                          WHERE lgnum = l_ltap-lgnum
                            AND refnr = l_ltap-refnr
                            AND remessa = l_ltap-vbeln.
                      IF sy-subrc = 0.
                        CLEAR zwm028.
                        SELECT SINGLE * FROM zwm028
                            WHERE lgnum = l_ltap-lgnum AND
                                  refnr = l_ltap-refnr AND
                                  remessa = zwm040-id_servisan.
                      ENDIF.
                    ENDIF.
                    IF zwm028-zlock <> '3' AND zwm028-zlock <> '4'.
                      DELETE l_ltap INDEX save_index.
                    ELSE.
                      CLEAR zwm028.
                      SELECT SINGLE * FROM zwm028
                          WHERE lgnum = l_ltap-lgnum AND
                                refnr = l_ltap-refnr AND
                                remessa = sem_remessa.
                      MOVE l_zwm010-prioridade
                                         TO l_ltap-prioridade_queue.
                      MOVE l_zwm010-tipo TO l_ltap-tipo.
                      l_ltap-prioridade = zwm028-prioridade.
                      MODIFY l_ltap INDEX save_index.
                    ENDIF.
                  ENDIF.
                ELSEIF sy-subrc <> 0.
                  DELETE l_ltap INDEX save_index.
                ENDIF.

              ELSEIF l_ltap-queue = 'QUEUEPT'.

                CLEAR: l_ltap-prioridade.
                SELECT SINGLE prioridade FROM zwm028
                INTO l_ltap-prioridade
                WHERE lgnum = l_ltap-lgnum AND
                      refnr = l_ltap-benum AND
                      remessa = sem_remessa.
                MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
                MOVE l_zwm010-tipo TO l_ltap-tipo.
                MODIFY l_ltap INDEX save_index.

              ELSE.

                MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
                MOVE l_zwm010-tipo TO l_ltap-tipo.
                MODIFY l_ltap INDEX save_index.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

** Eliminar da selecção as TO que estão bloqueadas

    IF l_ltap[] IS INITIAL.

** unlock warehouse
      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lock_key
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      return_msg-msgtyp = 'E'.
      return_msg-msgid = 'ZWMMSG001'.
      return_msg-msgnr = '032'.
      APPEND return_msg.

      CLEAR equipamento_actual.
      RAISE no_work_available.

    ELSEIF NOT l_ltap[] IS INITIAL.

** Verificar quais as TO que estão bloqueadas e apaga-las
      LOOP AT l_ltap.
** Verificar se a TO que se atribui não está associada a outro user
        READ TABLE l_zwm011 WITH KEY to_number = l_ltap-tanum
                                     to_item = l_ltap-tapos.
        IF sy-subrc = 0.
          DELETE l_ltap WHERE tanum = l_ltap-tanum.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF l_ltap[] IS INITIAL.
** unlock warehouse
      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lock_key
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      return_msg-msgtyp = 'E'.
      return_msg-msgid = 'ZWMMSG001'.
      return_msg-msgnr = '032'.
      APPEND return_msg.

      CLEAR equipamento_actual.
      RAISE no_work_available.

    ELSEIF NOT l_ltap[] IS INITIAL.

** Verificar se a(s) TO(s) que vamos processar estão num corredor
** diferente daquele onde se encontram os outros user(s)
      CLEAR users_row.
      REFRESH users_row.

      CALL FUNCTION 'ZWM_GET_ROW_USERS_TRI'
        EXPORTING
          armazem         = armazem
        TABLES
          t_zwm011        = l_zwm011
          s_zwm_row_users = users_row
          s_zwm_queue     = queues.

** Fila de entrada de produção
      PERFORM get_parameter
              USING armazem
                    'GESTAO_FILAS'
                    'FILA_PRODUCAO'
                    production_queue.

** Fila de saida da zona de TRILATERIAIS
      PERFORM get_parameter
              USING armazem
                    'GESTAO_FILAS'
                    'FILA_SAIDA_TRI'
                    tri_out_queue.


** Fila de saida da zona de TRILATERIAIS PARA O REP
      PERFORM get_parameter
              USING armazem
                    'GESTAO_FILAS'
                    'FILA_PICK_TRI'
                    queue_pick_tri.

** Fila Entrada Devolucao + Incidencia
      PERFORM get_parameter
              USING armazem
                    'GESTAO_FILAS'
                    'FILA_DEV_INC'
                    dev_inc_queue.

** Eliminar o q não interessa
      LOOP AT l_ltap.

** Para queues de entrada ve-se na posição de destino
        IF l_ltap-queue = production_queue OR
           l_ltap-queue = dev_inc_queue.

** verificar se na posição de destino da TO está englobado o q vem dos
** outros utilizadores ( APENAS PELO CORREDOR )
          READ TABLE users_row WITH KEY corredor = l_ltap-nlpla(3).
          IF sy-subrc = 0.
            DELETE l_ltap WHERE nlpla(3) = users_row-corredor.
          ENDIF.

** Para queues de saida ve-se na posicao de origem
        ELSEIF l_ltap-queue = tri_out_queue     OR
               l_ltap-queue = tri_out_prm_queue OR
               l_ltap-queue = tri_out_cpk_queue OR
               l_ltap-queue = queue_pick_tri.

          READ TABLE users_row WITH KEY corredor = l_ltap-vlpla(3).
          IF sy-subrc = 0.
            DELETE l_ltap WHERE vlpla(3) = users_row-corredor.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDIF.

    IF l_ltap[] IS INITIAL.
** unlock warehouse
      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lock_key
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      return_msg-msgtyp = 'E'.
      return_msg-msgid = 'ZWMMSG001'.
      return_msg-msgnr = '032'.
      APPEND return_msg.

      CLEAR : equipamento_actual.
      RAISE no_work_available.

    ELSEIF NOT l_ltap[] IS INITIAL.


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
** Verificar qual a melhor TO para o operário do TRILATERAL
      PERFORM get_best_to_tri TABLES l_ltap
                          USING corredor
                                profundidade
                                nivel
                                ultimo_tipo_dep
                                armazem
                                equipamento_actual
                                production_queue
                                tri_out_queue
                                queue_pick_tri
                                dev_inc_queue
                                static_sentido
                          CHANGING nova_to
                                   tipo_queue.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'ZWM014'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
    ENDIF.


    IF l_ltap[] IS INITIAL.
** unlock warehouse
      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lock_key
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      return_msg-msgtyp = 'E'.
      return_msg-msgid = 'ZWMMSG001'.
      return_msg-msgnr = '032'.
      APPEND return_msg.

      CLEAR : equipamento_actual.
      RAISE no_work_available.

    ELSEIF NOT l_ltap[] IS INITIAL.

*** Retorno do ecra a ser chamado
*      SELECT SINGLE ecra FROM zwm012 INTO zwm012-ecra
*                         WHERE armazem = armazem AND
*                               queue = l_ltap-queue AND
*                               tamanho = tamanho.
*      IF sy-subrc = 0.
*        ecra = zwm012-ecra.
*      ENDIF.

** Actualização da BD - verificação da existência de uma entrada para o
** utilizador

      MOVE-CORRESPONDING nova_to TO l_zwm011_aux.
      APPEND l_zwm011_aux.
      MODIFY zwm011 FROM TABLE l_zwm011_aux.
      IF sy-subrc EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.


** Para queues de ENTRADA vamos actualizar as mensulas colocando a su
** como su em transito ... mas não desbloqueando a mensula
      IF tipo_queue = 'E'.
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

        UPDATE zwm014 SET su_transito = l_ltap-nlenr
                      WHERE armazem = armazem AND
                            su = l_ltap-nlenr.
        IF sy-subrc = 0.
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

      ELSEIF tipo_queue = 'S'.

        DATA aux_su LIKE ltap-nlenr.

        CLEAR: su1, aux_su, lqua, zwm020.

        SELECT SINGLE lenum INTO su1
            FROM lqua
                WHERE lgnum = l_ltap-lgnum AND
                      lgtyp = l_ltap-vltyp AND
                      lgpla = l_ltap-vlpla.

*        aux_su = l_ltap-vlenr.

        SELECT SINGLE *
            FROM zwm020
                WHERE armazem = l_ltap-lgnum AND
                      ( p1 = su1 OR p2 = su1 ).
        IF sy-subrc = 0.

          IF zwm020-p1 <> su1.
            su1 = zwm020-p1.
          ENDIF.

          l_ltap-vlenr = su1.

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

          UPDATE zwm014 SET su_transito = l_ltap-vlenr
                              WHERE armazem = armazem AND
                                    su = l_ltap-vlenr.
          IF sy-subrc = 0.
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

        ENDIF.

      ENDIF.

      CLEAR : l_zwm011_aux, l_zwm011, l_zwm013, l_ltap, w_uinfos,
              equipamento_actual.
      REFRESH : l_zwm011_aux, l_zwm011, l_zwm013, l_ltap, w_uinfos.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lock_key
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

    ENDIF.


  ENDIF.

ENDFUNCTION.
