FUNCTION zwm_get_to_ret.
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
  queue_saida_tri   LIKE zwm001-valor,
  queue_descarga    LIKE zwm001-valor,
  queue_carga       LIKE zwm001-valor,
  queue_fabrica1    LIKE zwm001-valor,
  queue_saida_dri   LIKE zwm001-valor,
  queue_saida_pkf   LIKE zwm001-valor,
  queue_saida_spf   LIKE zwm001-valor,
  queue_saida_bpk   LIKE zwm001-valor,
  queue_saida_prm   LIKE zwm001-valor,
  queue_pick_dri    LIKE zwm001-valor,
  queue_pick_tri    LIKE zwm001-valor,
  queue_rep_int     LIKE zwm001-valor,
  queue_chd_elv     LIKE zwm001-valor,
  queue_saida_aprm  LIKE zwm001-valor,
  queue_reab_bpe    LIKE zwm001-valor,
  queue_pilha_vazia LIKE zwm001-valor,
  num_user          LIKE zwm001-valor,
  n_user            TYPE i,
  n_insercoes TYPE i,
  ultimo_tipo_dep LIKE zwm011-ultimo_tipo_dep,
  corredor(3),
  profundidade(3),
  nivel(2),
  equipamento_actual LIKE zwm010-equipamento,
  sem_remessa LIKE zwm026-remessa.

  DATA: lt_ltak   TYPE SORTED TABLE OF ltak WITH UNIQUE KEY tanum,
        lt_zwm077 TYPE TABLE OF zwm077 WITH HEADER LINE,
        ls_ltak   TYPE ltak.

  DATA: lv_2spart    TYPE flag.


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

  RANGES: equipamento_queue FOR ltak-queue.

*  BREAK-POINT.
  CLEAR : queue_saida_tri, queue_descarga,
          queue_saida_dri,queue_saida_pkf,
          queue_carga, queue_saida_prm,
          queue_pick_dri, queue_pick_tri,
          sem_remessa, queue_pilha_vazia.

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

** Fila de Descargas
    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_DESCARGA'
                  queue_descarga.

** Fila de Cargas
    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_CARGA'
                  queue_carga.

** Fila da fábrica 1
    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_FABRICA1'
                  queue_fabrica1.

** Fila Saida Drive in
    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_SAIDA_DRI'
                  queue_saida_dri.

** Fila Saida trilateral
    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_SAIDA_TRI'
                  queue_saida_tri.

** Fila Saida Picking Acabado
    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_SAIDA_PKF'
                  queue_saida_pkf.

** Fila saida picking
    PERFORM get_parameter
        USING armazem
              'GESTAO_FILAS'
              'FILA_SAIDA_SPF'
              queue_saida_spf.

** Fila saida BPK
    PERFORM get_parameter
        USING armazem
              'GESTAO_FILAS'
              'FILA_SAIDA_BPK'
              queue_saida_bpk.

** Fila Saida PRM
    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_S_PAL_REMONTADA'
                  queue_saida_prm.

** Fila do DRI para o REP
    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_PICK_DRI'
                  queue_pick_dri.

** Fila Pilha Paletes Vazias
    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_PILHA_VAZIA'
                  queue_pilha_vazia.


** Fila do TRI para o REP
    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_PICK_TRI'
                  queue_pick_tri.

** Fila do REP para o INT
    PERFORM get_parameter
            USING armazem
                  'GESTAO_FILAS'
                  'FILA_REP_INT'
                  queue_rep_int.

** Fila Chamada
    PERFORM get_parameter
           USING armazem
                 'GESTAO_FILAS'
                 'FILA_CHAMADA_ELV'
                 queue_chd_elv.

** Fila reabastecimento Buffer Paletização Especial
    PERFORM get_parameter
           USING armazem
                 'GESTAO_FILAS'
                 'FILA_ABAST_BPE'
                 queue_reab_bpe.

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
** Este caso acontece quando se acabou de realizar uma descarga fisica e
** se executou o pick da TO - atribui-se ao utilizador que fez a
** descarga

    SELECT SINGLE * FROM zwm011
                     WHERE armazem = armazem AND
                           user_name = sy-uname AND
                           status = 'P' AND
                           queue = queue_descarga.
    IF sy-subrc = 0.
      MOVE zwm011 TO nova_to.

** FALTA CHAMAR ECRA CORRECTO
      is_equal = 'X'.
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
      CLEAR : lock_key, is_equal,queue_descarga.
      EXIT.
    ENDIF.


    SELECT SINGLE * FROM zwm011
                        WHERE armazem = armazem AND
                              user_name = sy-uname AND
                              status = 'P' AND
                              queue = 'QUEUERM'.
    IF sy-subrc = 0.
      MOVE zwm011 TO nova_to.
      is_equal = 'X'.
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
      CLEAR : lock_key, is_equal,queue_descarga.
      EXIT.
    ENDIF.

    CLEAR is_equal.
** Este caso acontece quando se acabou de realizar uma descarga fisica
** da fabrica e se executou o pick da TO - atribui-se ao utilizador que
** fez a descarga
    SELECT SINGLE * FROM zwm011
                     WHERE armazem = armazem AND
                           user_name = sy-uname AND
                           status = 'P' AND
                           queue = queue_fabrica1.
    IF sy-subrc = 0.
      MOVE zwm011 TO nova_to.
      is_equal = 'X'.
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
      CLEAR : lock_key, is_equal,queue_descarga.
      EXIT.
    ENDIF.
***************************************************************


    CLEAR is_equal.
** Este caso acontece quando se acabou de realizar uma descarga fisica
** da fabrica e se executou o pick da TO - atribui-se ao utilizador que
** fez a descarga
    SELECT SINGLE * FROM zwm011
                     WHERE armazem = armazem AND
                           user_name = sy-uname AND
                           status = 'P' AND
                           queue = queue_saida_dri.
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
      CLEAR : lock_key, is_equal,queue_descarga.
      EXIT.
    ENDIF.

**************************************************************
** Este caso acontece quando se acabou de realizar um pick à
** TO de saida (segunda) e existe um crash - assignar de novo

    CLEAR is_equal.

    SELECT SINGLE * FROM zwm011
                     WHERE armazem   = armazem  AND
                           user_name = sy-uname AND
                           status    = 'P'      AND
                         ( queue = queue_saida_pkf OR
                           queue = queue_saida_spf OR
                           queue = queue_saida_bpk ).
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
      CLEAR : lock_key, is_equal,queue_descarga.
      EXIT.
    ENDIF.

    CLEAR is_equal.
** Este caso acontece quando se acabou de realizar um pick à
** TO de saida  e existe um crash - assignar de novo
    SELECT SINGLE * FROM zwm011
                     WHERE armazem = armazem AND
                           user_name = sy-uname AND
                           status = 'P' AND
                           queue = queue_pick_tri.
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
      CLEAR : lock_key, is_equal,queue_descarga.
      EXIT.
    ENDIF.



    CLEAR is_equal.

******* MUDAR PARA UM READ TABLEEEEEEEEEEEEE **********************

** Verificar se existiu um crash ... assignar de novo a TO - SÓ PARA
** USER DO RECTRACTIL QUE ESTÁ A ARRUMAR

    SELECT SINGLE * FROM zwm011
                    WHERE armazem = armazem AND
                          user_name = sy-uname AND
                          status = 'C'.
*                          status = 'P'.

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


** Verificar se existiu um crash ... assignar de novo a TO - SÓ PARA
** USER DO CONVENCIONAL QUE ESTÁ A ARRUMAR
*    READ TABLE l_zwm011 WITH KEY armazem = armazem
*                                 user_name = sy-uname
*                                 status = 'P'.
*    IF sy-subrc = 0.
*      MOVE l_zwm011 TO nova_to.
*
*      SELECT SINGLE tipo FROM zwm010 INTO tipo_queue
*                         WHERE armazem = armazem AND
*                               equipamento = l_zwm010-equipamento AND
*                               queue = l_zwm011-queue.
*      is_equal = 'X'.
*    ENDIF.

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
      CLEAR : lock_key, is_equal.
      EXIT.
    ENDIF.


** Carregar as TO para a(s) queue(s) pertencentes a este equipamento que
** não estejam confirmadas a nivel de cabeçalho
**********************************************************************
    REFRESH: l_ltap.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE l_ltap BYPASSING BUFFER
                    FROM ltak AS k INNER JOIN ltap AS p
                    ON  k~lgnum = p~lgnum AND
                        k~tanum = p~tanum
                    WHERE k~lgnum = armazem AND
                          k~kquit = ' ' AND
                          k~queue IN equipamento_queue AND
                          p~pquit = ' '.
    IF sy-subrc = 0.
      IF NOT l_ltap[] IS INITIAL.
        SELECT * FROM ltak
           INTO TABLE lt_ltak
           FOR ALL ENTRIES IN l_ltap
           WHERE lgnum = l_ltap-lgnum AND
                 tanum = l_ltap-tanum.
      ENDIF.

** WCS - Armazém Automático de Torres Novas
      SELECT *
        FROM zwm077 INTO TABLE lt_zwm077
        FOR ALL ENTRIES IN lt_ltak
        WHERE lgnum = lt_ltak-lgnum
        AND   tanum = lt_ltak-tanum.

      " Validar OTs de chamadas de Elevador de mesas de saida
      PERFORM check_wcs_che_tos USING armazem.

** Validar OTs
**********************************************************************
      SORT l_ltap BY tanum tapos.

      CLEAR save_index.

      LOOP AT l_ltap.
        save_index = sy-tabix.

        " WCS - Filtrar OTs de paletes com chamadas de elevadores
        READ TABLE lt_zwm077 WITH KEY tanum = l_ltap-tanum.
        IF sy-subrc = 0.

          IF lt_zwm077-pvqui IS INITIAL.
            DELETE l_ltap INDEX save_index.
            CONTINUE.
          ENDIF.

        ELSE.

          " Paletes do Automático
          IF l_ltap-vltyp = 'AUT' AND
             l_ltap-vlpla = 'AUT'.
            DELETE l_ltap INDEX save_index.
            CONTINUE.

            " Paletes Picking saída do EAU
          ELSEIF l_ltap-vltyp = 'EAU' AND l_ltap-queue = queue_saida_spf.
            DELETE l_ltap INDEX save_index.
            CONTINUE.
          ENDIF.
        ENDIF.

        " OTs Reabastecimento Buffer Paletização Especial (limpar prioridade da OT)
        IF l_ltap-queue = queue_reab_bpe.
          CLEAR l_ltap-tapri.

          MODIFY l_ltap INDEX save_index TRANSPORTING tapri.
        ENDIF.

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
** Só apaga se a QUEUE não FOR de Descarga
            IF NOT l_ltap-pvqui IS INITIAL AND
                   l_ltap-queue <> queue_descarga.
              DELETE l_ltap INDEX save_index.
              CONTINUE.
            ELSE.
** Actualizar prioridade associada a QUEUE
              MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
              MOVE l_zwm010-tipo TO l_ltap-tipo.
              MODIFY l_ltap INDEX save_index.
            ENDIF.
** Saida
          ELSEIF l_zwm010-tipo = 'S'.

*            IF l_ltap-pvqui IS INITIAL AND
            IF l_ltap-queue = queue_saida_tri.
              DELETE l_ltap INDEX save_index.
              CONTINUE.

            ELSEIF l_ltap-queue = queue_pick_dri.
              IF NOT l_ltap-pvqui IS INITIAL.
                DELETE l_ltap INDEX save_index.
                CONTINUE.
              ELSE.
** RL -> INS 27.04.2005
** Prioridades
                CLEAR: l_ltap-prioridade.
                SELECT SINGLE prioridade FROM zwm028
                INTO l_ltap-prioridade
                WHERE lgnum = l_ltap-lgnum AND
                      refnr = l_ltap-benum AND
                      remessa = sem_remessa.
** RL <- INS 27.04.2005
                MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
                MOVE l_zwm010-tipo TO l_ltap-tipo.
                MODIFY l_ltap INDEX save_index.
              ENDIF.

            ELSEIF l_ltap-queue = queue_pick_tri.
              IF l_ltap-pvqui IS INITIAL.
                DELETE l_ltap INDEX save_index.
                CONTINUE.
              ELSE.
** RL -> INS 27.04.2005
** Prioridades
                CLEAR: l_ltap-prioridade.
                SELECT SINGLE prioridade FROM zwm028
                INTO l_ltap-prioridade
                WHERE lgnum = l_ltap-lgnum AND
                      refnr = l_ltap-benum AND
                      remessa = sem_remessa.
** RL <- INS 27.04.2005
                MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
                MOVE l_zwm010-tipo TO l_ltap-tipo.
                MODIFY l_ltap INDEX save_index.
              ENDIF.

            ELSEIF l_ltap-queue = queue_saida_dri OR
                   l_ltap-queue = queue_saida_prm.

** Para a saida do drive in ... qd ela esta picked não se atibui
** utilizador
              IF NOT l_ltap-pvqui IS INITIAL.
                DELETE l_ltap INDEX save_index.
                CONTINUE.
              ENDIF.

              IF NOT l_ltap-pvqui IS INITIAL.
                MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
                MOVE l_zwm010-tipo TO l_ltap-tipo.
                MODIFY l_ltap INDEX save_index.

              ELSEIF l_ltap-pvqui IS INITIAL.

** Para as TOs de saída temos de ver o status de liberação
** do grupo ... só assim poderemos dar as TOs
                CLEAR zwm028.
                SELECT SINGLE * FROM zwm028
                                  WHERE lgnum = l_ltap-lgnum AND
                                        refnr = l_ltap-refnr AND
                                        remessa = sem_remessa.
                IF sy-subrc = 0.
** 3 - LOCK PALETES COMPLETAS
** 4 - LOCK AMBAS
                  IF zwm028-tipo_lock = 'G' OR
                     zwm028-tipo_lock IS INITIAL.

                    IF zwm028-zlock <> '3' AND
                       zwm028-zlock <> '4' AND
                       zwm028-zlock <> '5'.

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
                    IF zwm028-zlock <> '3' AND
                       zwm028-zlock <> '4' AND
                       zwm028-zlock <> '5'.

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
                ELSE.
                  DELETE l_ltap INDEX save_index.
                  CONTINUE.
                ENDIF.
              ENDIF.

            ELSEIF ( l_ltap-queue = queue_saida_pkf OR
                     l_ltap-queue = queue_saida_spf OR
                     l_ltap-queue = queue_saida_bpk ).

              IF l_ltap-queue = queue_saida_bpk.
                IF NOT l_ltap-pvqui IS INITIAL.
                  DELETE l_ltap INDEX save_index.
                  CONTINUE.
                ENDIF.
              ENDIF.

              CLEAR: zwm026, l_refnr, l_vbeln.
              SELECT * FROM zwm026
              WHERE armazem EQ l_ltap-lgnum
                AND sscc    EQ l_ltap-lznum.
                l_refnr = zwm026-grupo.
                l_vbeln = zwm026-remessa.
                EXIT.
              ENDSELECT.

              CLEAR: l_ltap-prioridade.

              SELECT SINGLE prioridade tipo_lock st_pul FROM zwm028
              INTO (l_ltap-prioridade, zwm028-tipo_lock, zwm028-st_pul)
              WHERE lgnum = l_ltap-lgnum AND
                    refnr = l_refnr      AND
                    remessa = sem_remessa.

              IF zwm028-tipo_lock = 'G' OR
                 zwm028-tipo_lock IS INITIAL.

                MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
                MOVE l_zwm010-tipo TO l_ltap-tipo.
                MODIFY l_ltap INDEX save_index.

                IF l_ltap-queue = queue_saida_bpk.
                  IF zwm028-st_pul = 'AUT' OR
                     zwm028-st_pul = 'BPK'.
                    DELETE l_ltap INDEX save_index.
                  ENDIF.
                ENDIF.

              ELSEIF zwm028-tipo_lock = 'R'.

                CLEAR zwm028.

                SELECT SINGLE * FROM zwm028
                    WHERE lgnum = l_ltap-lgnum AND
                          refnr = l_refnr AND
                          remessa = l_vbeln.
                IF sy-subrc <> 0.
                  CLEAR zwm040.
                  SELECT SINGLE * FROM zwm040
                      WHERE lgnum = l_ltap-lgnum
                        AND refnr = l_refnr
                        AND remessa = l_vbeln.
                  IF sy-subrc = 0.
                    CLEAR zwm028.
                    SELECT SINGLE * FROM zwm028
                        WHERE lgnum = l_ltap-lgnum AND
                              refnr = l_refnr AND
                              remessa = zwm040-id_servisan.
                  ENDIF.
                ENDIF.
                IF zwm028-zlock <> '3' AND
                   zwm028-zlock <> '4' AND
                   zwm028-zlock <> '5'.

                  IF l_ltap-queue = queue_saida_spf.

                    IF zwm028-zlock <> '2' AND l_ltap-nltyp <> 'BPK'.
                      DELETE l_ltap INDEX save_index.
                    ELSE.
                      MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
                      MOVE l_zwm010-tipo TO l_ltap-tipo.
*                    l_ltap-prioridade = zwm028-prioridade.
                      MODIFY l_ltap INDEX save_index.
                    ENDIF.

                  ELSEIF l_ltap-queue = queue_saida_bpk.

                    IF zwm028-st_pul = 'AUT' OR zwm028-st_pul = 'BPK'.
                      DELETE l_ltap INDEX save_index.
                    ELSEIF zwm028-zlock <> '2'.
                      DELETE l_ltap INDEX save_index.
                    ELSE.
                      MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
                      MOVE l_zwm010-tipo TO l_ltap-tipo.
*                    l_ltap-prioridade = zwm028-prioridade.
                      MODIFY l_ltap INDEX save_index.
                    ENDIF.

                  ELSE.
                    IF zwm028-st_ppk IS INITIAL.
                      DELETE l_ltap INDEX save_index.

                    ELSE.
                      MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
                      MOVE l_zwm010-tipo TO l_ltap-tipo.
*                    l_ltap-prioridade = zwm028-prioridade.
                      MODIFY l_ltap INDEX save_index.
                    ENDIF.
                  ENDIF.

                ELSE.
                  CLEAR zwm028.
                  SELECT SINGLE * FROM zwm028
                      WHERE lgnum = l_ltap-lgnum AND
                            refnr = l_refnr AND
                            remessa = sem_remessa.

                  MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
                  MOVE l_zwm010-tipo TO l_ltap-tipo.
                  l_ltap-prioridade = zwm028-prioridade.
                  MODIFY l_ltap INDEX save_index.
                ENDIF.

              ENDIF.
            ELSEIF l_ltap-queue = 'QUEUEST_M'.

              CLEAR: ltap, ltak, l_refnr.
              SELECT h~refnr INTO l_refnr
              FROM ltak AS h INNER JOIN ltap AS i
                        ON h~lgnum = i~lgnum AND
                           h~tanum = i~tanum
              WHERE h~lgnum EQ l_ltap-lgnum
                AND h~kquit NE 'X'
                AND i~vlenr EQ l_ltap-lznum
                AND i~vorga NE 'ST'.
                EXIT.
              ENDSELECT.

              CLEAR: l_ltap-prioridade.
              SELECT SINGLE prioridade FROM zwm028
              INTO l_ltap-prioridade
              WHERE lgnum = l_ltap-lgnum
                AND refnr = l_refnr
                AND remessa = sem_remessa.

              MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
              MOVE l_zwm010-tipo TO l_ltap-tipo.
              MODIFY l_ltap INDEX save_index.

            ELSE.
** Actualizar prioridade associada a QUEUE
              MOVE l_zwm010-prioridade TO l_ltap-prioridade_queue.
              MOVE l_zwm010-tipo TO l_ltap-tipo.
              MODIFY l_ltap INDEX save_index.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CLEAR : queue_descarga,l_ltap.

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

*** Verificar se alguma das TO está atribuida a outro user
      LOOP AT l_ltap.
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

** Verificar se estão mais que n users no mesmo drive in
      break roffd.
      DATA: lt_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.

      DATA : BEGIN OF sto_dri OCCURS 0,
               vlpla LIKE ltap-vlpla,
               count TYPE i,
             END OF sto_dri.

      DATA to_dri LIKE sto_dri OCCURS 0 WITH HEADER LINE.

      CLEAR: lt_ltap, sto_dri.
      REFRESH: lt_ltap, sto_dri.

      PERFORM get_parameter USING armazem
            'GESTAO_PRIORIDADES'
            'USERS_DRI'
            num_user.

      MOVE num_user TO n_user.
      IF NOT num_user IS INITIAL.

        IF NOT l_zwm011[] IS INITIAL.
          SELECT * INTO TABLE lt_ltap
            FROM ltap
                FOR ALL ENTRIES IN l_zwm011
                    WHERE lgnum = armazem
                      AND tanum = l_zwm011-to_number
                      AND pvqui = ' '.

          DELETE lt_ltap WHERE vltyp <> 'DRI'.
          IF NOT lt_ltap[] IS INITIAL.
            LOOP AT lt_ltap.
              MOVE lt_ltap-vlpla TO to_dri-vlpla.
              to_dri-count = 1.
              COLLECT to_dri.
              CLEAR to_dri.
            ENDLOOP.

            LOOP AT to_dri WHERE count >= n_user.
              DELETE l_ltap WHERE vltyp = 'DRI'
                              AND vlpla = to_dri-vlpla.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

** Verificar se estão mais que n users no mesmo BLK
    break roffd.

    DATA : BEGIN OF sto_blk OCCURS 0,
             vlpla LIKE ltap-vlpla,
             matnr LIKE ltap-matnr,
             vbeln LIKE ltap-vbeln,
             count TYPE i,
           END OF sto_blk.

    DATA to_blk LIKE sto_blk OCCURS 0 WITH HEADER LINE.

    CLEAR: lt_ltak, lt_ltap, sto_blk, num_user.
    REFRESH: lt_ltak, lt_ltap, sto_blk.

    PERFORM get_parameter USING armazem
          'GESTAO_PRIORIDADES'
          'USERS_BLK'
          num_user.

    MOVE num_user TO n_user.
    IF NOT num_user IS INITIAL.

      IF NOT l_zwm011[] IS INITIAL.
        SELECT * INTO TABLE lt_ltap
          FROM ltap
              FOR ALL ENTRIES IN l_zwm011
                  WHERE lgnum = armazem
                    AND tanum = l_zwm011-to_number
                    AND pvqui = ' '.

        DELETE lt_ltap WHERE vltyp <> 'BLK'.
        IF NOT lt_ltap[] IS INITIAL.
          LOOP AT lt_ltap.
            MOVE lt_ltap-vlpla TO to_blk-vlpla.
            MOVE lt_ltap-matnr TO to_blk-matnr.
            MOVE lt_ltap-vbeln TO to_blk-vbeln.
            to_blk-count = 1.
            COLLECT to_blk.
            CLEAR to_blk.
          ENDLOOP.

          LOOP AT to_blk WHERE count >= n_user.
            DELETE l_ltap WHERE vltyp = 'BLK'
                            AND vlpla = to_blk-vlpla
                            AND matnr = to_blk-matnr
                            AND vbeln = to_blk-vbeln.
          ENDLOOP.
        ENDIF.
      ENDIF.
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

** Verificar qual a melhor TO para o operário do contrapesado
*      break roffd.
      PERFORM get_best_to_cpes  TABLES l_ltap
                          USING corredor
                                profundidade
                                nivel
                                ultimo_tipo_dep
                                ultimo_bin
                                armazem
                                equipamento_actual
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

      CLEAR : l_zwm011_aux, l_zwm011, l_zwm013, l_ltap, w_uinfos.
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
