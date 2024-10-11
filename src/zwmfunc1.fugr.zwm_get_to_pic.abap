FUNCTION zwm_get_to_pic.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"     REFERENCE(TAMANHO) TYPE  CHAR5
*"     REFERENCE(ADD_NEW_TO) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     REFERENCE(NOVA_TO) TYPE  ZWM011
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
*         queue_entrada_tri LIKE ltak-queue,
*         queue_saida_tri LIKE ltak-queue,
*         queue_descarga LIKE zwm001-valor,
*         queue_rep_pick LIKE zwm001-valor,
         n_insercoes TYPE i,
         ultimo_tipo_dep LIKE zwm011-ultimo_tipo_dep,
         corredor(3),
         profundidade(3),
         nivel(2),
         equipamento_actual LIKE zwm010-equipamento.

  DATA: w_uinfo LIKE uinfo OCCURS 0 WITH HEADER LINE,
        l_uinfo LIKE uinfo OCCURS 0 WITH HEADER LINE.

** Tabelas auxiliares
  DATA : BEGIN OF l_zwm011 OCCURS 0.
          INCLUDE STRUCTURE zwm011.
  DATA : END OF l_zwm011.

  DATA : BEGIN OF l_zwm011_aux OCCURS 0.
          INCLUDE STRUCTURE zwm011.
  DATA : END OF l_zwm011_aux.

  DATA sem_remessa LIKE zwm026-remessa.


  RANGES: equipamento_queue FOR ltak-queue.

*  CLEAR : queue_entrada_tri, queue_saida_tri.
*  CLEAR queue_rep_pick.
* BREAK ROFFD.

**
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


    CLEAR : ultimo_bin, ultimo_tipo_dep,corredor,profundidade,nivel.
** Guardar o ultimo BIN onde esteve o utilizador ... para a atribuição
** da tarefa ao utilizador
    SELECT SINGLE ultimo_bin ultimo_tipo_dep FROM zwm011
                                       INTO (ultimo_bin,ultimo_tipo_dep)
                             WHERE armazem = armazem AND
                                   user_name = sy-uname AND
                                   status = 'T'.

** Corredor / profundidade / nivel actual do retractil
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
    COMMIT WORK.

** Para seguir para aqui não existiu nenhum crash ...
** Actualização das possiveis queues para este equipamento
    LOOP AT l_zwm010.
** Range de QUEUES para este equipamento
      IF NOT l_zwm010-queue IS INITIAL.
        equipamento_queue-sign = 'I'.
        equipamento_queue-option = 'EQ'.
        equipamento_queue-low = l_zwm010-queue.
        APPEND equipamento_queue.
      ENDIF.
    ENDLOOP.

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
    SELECT SINGLE * FROM zwm011
                     WHERE armazem = armazem AND
                           user_name = sy-uname AND
                           status = 'P'.
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
        is_equal = 'X'.
      ENDIF.

*      MOVE zwm011 TO nova_to.
*
*** FALTA CHAMAR ECRA CORRECTO
*      is_equal = 'X'.
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
      CLEAR : lock_key, is_equal.
      EXIT.
    ENDIF.


    CLEAR is_equal.
** Verificar se existiu um crash ... assignar de novo a TO - SÓ PARA
** USER DO RECTRACTIL QUE ESTÁ A ARRUMAR
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

*      MOVE zwm011 TO nova_to.
*
*      SELECT SINGLE tipo FROM zwm010 INTO tipo_queue
*                         WHERE armazem = armazem AND
*                               equipamento = l_zwm010-equipamento AND
*                               queue = zwm011-queue.
*      is_equal = 'X'.
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
      CLEAR : lock_key, is_equal.
      EXIT.
    ENDIF.



** Carregar as TO para a(s) queue(s) pertencentes a este equipamento que
** não estejam confirmadas a nivel de cabeçalho
    SELECT * INTO CORRESPONDING FIELDS OF TABLE l_ltap
                    FROM ltak AS k INNER JOIN ltap AS p
                    ON  k~lgnum = p~lgnum AND
                        k~tanum = p~tanum
                    WHERE k~lgnum = armazem AND
                          k~kquit = ' ' AND
                          k~queue IN equipamento_queue AND
                          p~pquit = ' '.
*                          P~PVQUI = ' '.


** Verificar quais as TO interessantes para este equipamento ...
    IF sy-subrc = 0.
      CLEAR save_index.
      LOOP AT l_ltap.
        save_index = sy-tabix.


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
            IF NOT l_ltap-pvqui IS INITIAL.
              DELETE l_ltap INDEX save_index.
            ELSE.
** Actualizar prioridade associada a QUEUE
              MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
              MOVE l_zwm010-tipo TO l_ltap-tipo.
              MODIFY l_ltap INDEX save_index.
            ENDIF.
** Saida
          ELSEIF l_zwm010-tipo = 'S'.
            IF l_ltap-pvqui IS INITIAL.
              DELETE l_ltap INDEX save_index.
            ELSE.
** RL -> INS 27.04.2005
** Prioridades
              IF l_ltap-queue = 'QUEUELD' OR
                 l_ltap-queue = 'QUEUEPV' OR
                 l_ltap-queue = 'QUEUERI' OR
                 l_ltap-queue = 'QUEUERP' OR
                 l_ltap-queue = 'QUEUEPL' OR
                 l_ltap-queue = 'QUEUEPLV'.

                CLEAR: l_ltap-prioridade.
                SELECT SINGLE prioridade FROM zwm028
                INTO l_ltap-prioridade
                WHERE lgnum   = l_ltap-lgnum AND
                      refnr   = l_ltap-benum AND
                      remessa = ' '.
              ENDIF.
** RL <- INS 27.04.2005
** Actualizar prioridade associada a QUEUE
              MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
              MOVE l_zwm010-tipo TO l_ltap-tipo.
              MODIFY l_ltap INDEX save_index.
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

      RAISE no_work_available.

    ELSEIF NOT l_ltap[] IS INITIAL.
*      BREAK-POINT.

      LOOP AT l_ltap.
** Verificar se a TO que se atribui não está associada a outro user
        READ TABLE l_zwm011 WITH KEY to_number = l_ltap-tanum
                                     to_item = l_ltap-tapos
                                     user_name = sy-uname.
        IF sy-subrc <> 0.
          READ TABLE l_zwm011 WITH KEY to_number = l_ltap-tanum
                                       to_item = l_ltap-tapos.
          IF sy-subrc = 0.
            DELETE l_ltap WHERE tanum = l_ltap-tanum.
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

      RAISE no_work_available.

    ELSEIF NOT l_ltap[] IS INITIAL.


** Verificar qual a melhor TO para o operário do TRILATERAL
      PERFORM get_best_to_pck TABLES l_ltap
                          USING corredor
                                profundidade
                                nivel
                                ultimo_tipo_dep
                                armazem
                                equipamento_actual
*                              PRODUCTION_QUEUE
*                              TRI_OUT_QUEUE
                          CHANGING nova_to
                                tipo_queue.

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

      CLEAR : l_zwm011_aux, l_zwm011, l_ltap, w_uinfos.
      REFRESH : l_zwm011_aux, l_zwm011, l_ltap, w_uinfos.

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
