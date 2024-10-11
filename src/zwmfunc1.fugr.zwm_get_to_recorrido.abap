FUNCTION zwm_get_to_recorrido.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"  EXPORTING
*"     REFERENCE(N_PALETES) TYPE  I
*"     REFERENCE(USER_ASSIGNADO) TYPE  CHAR1
*"  TABLES
*"      L_ZWM010 STRUCTURE  ZWM010
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"      L_ZWM026 STRUCTURE  ZWM_S026
*"  EXCEPTIONS
*"      NO_WORK_AVAILABLE
*"      NO_EQUIPMENT
*"----------------------------------------------------------------------

** RL -> LOG ZWM026
*  tables: zwm026_log.
*  DATA: wa_log LIKE zwm026_log.
** RL <- LOG ZWM026

** RL -> INS 27.04.2006
  DATA: itab_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE.

  FREE: itab_zwm026.
  CLEAR: itab_zwm026.
** RL <- INS 27.04.2006

** Variáveis globais à função
  DATA : wait_time(20), lock_key(50),
         sem_user                LIKE sy-uname,
         sem_to                  LIKE ltak-tanum,
         sem_remessa             LIKE zwm026-remessa,
         sem_recorrido           LIKE zwm026-num_recorrido,
         n_linhas                TYPE i,
         num_recorrido_escolhido LIKE zwm026-num_recorrido,
         save_index              LIKE sy-tabix,
         lt_ltap                 TYPE TABLE OF ltap WITH HEADER LINE.

  CLEAR : sem_user,
          sem_to,
          sem_recorrido,
          n_linhas,
          num_recorrido_escolhido,
          sem_remessa,
          save_index.

  IF l_zwm010[] IS INITIAL.
    RAISE no_equipment.
  ELSEIF NOT l_zwm010[] IS INITIAL.

** Carregar para memória as tabelas de parametrização
    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
      EXPORTING
        whs = armazem.


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

  ENDIF.

** Verificar se já existe alguma entrada associada
** ao utilizador
** RL -> MOD 27.04.2006
** Prioridades
*  SELECT * FROM zwm026 INTO CORRESPONDING FIELDS OF TABLE l_zwm026
*           WHERE armazem = armazem AND
*                 user_name = sy-uname AND
*                 to_number <> sem_to AND
*                 estado <> 'T'
*           ORDER BY num_recorrido ASCENDING.
*  IF sy-subrc = 0.

  SELECT * FROM zwm026 INTO CORRESPONDING FIELDS OF TABLE l_zwm026
           WHERE armazem = armazem AND
                 user_name = sy-uname AND
                 estado <> 'T'.

  DELETE l_zwm026 WHERE to_number = sem_to.

  SORT l_zwm026 BY num_recorrido ASCENDING.

  IF NOT l_zwm026[] IS INITIAL.
** Já existe uma entrada para o user ... assignar a mesma
    DESCRIBE TABLE l_zwm026 LINES n_linhas.
    IF n_linhas = 1.
      n_paletes = 1.
    ELSE.
      SORT l_zwm026 BY n_pal_picking.
      CLEAR : n_paletes.
      LOOP AT l_zwm026.
        AT NEW n_pal_picking.
          n_paletes = n_paletes + 1.
        ENDAT.
      ENDLOOP.
    ENDIF.


    CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = lock_key
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    CLEAR user_assignado.
    SORT l_zwm026 BY estado.

    READ TABLE l_zwm026 WITH KEY estado = 'C'.
    IF sy-subrc <> 0.
      CLEAR user_assignado.
    ELSE.
      IF l_zwm026-sscc IS INITIAL.
        CLEAR user_assignado.
      ELSE.
        user_assignado = 'X'.
      ENDIF.
    ENDIF.
    EXIT.
  ENDIF.


** Descobrir qual o primeiro válido para o operário o
** poder executar
** Prioridades

*  SELECT * FROM zwm026 INTO CORRESPONDING FIELDS OF TABLE l_zwm026
*           WHERE armazem = armazem AND
*                 user_name = sem_user AND
*                 estado = ' ' AND
*                 num_recorrido <> sem_recorrido AND
*                 to_number <> sem_to
*           ORDER BY num_recorrido ASCENDING.
*  IF sy-subrc <> 0.

  SELECT * FROM zwm026 INTO CORRESPONDING FIELDS OF TABLE l_zwm026
           WHERE armazem = armazem AND
                 user_name = sem_user AND
                 estado = ' '.

** Apagar da tabela de picking - Paletes com OTs estornadas
  IF l_zwm026[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      FOR ALL ENTRIES IN l_zwm026
      WHERE lgnum = armazem
      AND   tanum = l_zwm026-to_number.

    DELETE lt_ltap WHERE vorga = 'ST' OR  vorga = 'SL'.
  ENDIF.

  LOOP AT l_zwm026 WHERE to_number IS NOT INITIAL.

    save_index = sy-tabix.

    READ TABLE lt_ltap WITH KEY tanum = l_zwm026-to_number.
    CHECK sy-subrc <> 0.

    DELETE FROM zwm026 WHERE armazem       = l_zwm026-armazem
                       AND   n_pal_picking = l_zwm026-n_pal_picking
                       AND   i_pal_picking = l_zwm026-i_pal_picking
                       AND   grupo         = l_zwm026-grupo
                       AND   remessa       = l_zwm026-remessa
                       AND   posnr         = l_zwm026-posnr
                       AND   sub_item      = l_zwm026-sub_item.

    CHECK sy-subrc = 0.

    COMMIT WORK.

    DELETE l_zwm026 INDEX save_index.

  ENDLOOP.

  DELETE l_zwm026 WHERE num_recorrido = sem_recorrido
                     OR to_number = sem_to.

  SORT l_zwm026 BY num_recorrido.

  IF l_zwm026[] IS INITIAL.
** Erro ... não existem recorridos para processar
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
    return_msg-msgnr = '103'.
    APPEND return_msg.

    RAISE no_work_available.
  ELSE.
** Verificar bloqueios a nível de grupos
    LOOP AT l_zwm026.
      save_index = sy-tabix.
      SELECT SINGLE * FROM zwm028
                      WHERE lgnum = l_zwm026-armazem AND
                            refnr = l_zwm026-grupo AND
                            remessa = sem_remessa.
      IF sy-subrc = 0.
** LOCK 2 - lock de picking
** LOCK 4 - lock de ambos
        IF zwm028-zlock <> '2' AND zwm028-zlock <> '4'.
          DELETE l_zwm026 INDEX save_index.
** RL -> INS 27.04.2006
** Prioridades
        ELSE.
          l_zwm026-prioridade = zwm028-prioridade.
          MODIFY l_zwm026 INDEX save_index.
** RL <- INS 27.04.2005
        ENDIF.
      ELSE.
        DELETE l_zwm026 INDEX save_index.
      ENDIF.
    ENDLOOP.
    CLEAR : l_zwm026.
** Verificar bloqueios a nível de grupos

** Seleccionar apenas um recorrido e trabalhar com esse actualizando a
** BD
    IF l_zwm026[] IS INITIAL.
** erro ... não existem recorridos para processar
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
      return_msg-msgnr = '103'.
      APPEND return_msg.

      RAISE no_work_available.
    ELSE.

** RL -> MOD 27.04.2006
** Prioridades
*      SORT l_zwm026 BY num_recorrido sorlp.
      SORT l_zwm026 BY prioridade DESCENDING
                       num_recorrido sorlp.
** RL <- MOD 27.04.2006

      READ TABLE l_zwm026 INDEX 1.
      IF sy-subrc = 0.
        num_recorrido_escolhido = l_zwm026-num_recorrido.
        DELETE l_zwm026 WHERE num_recorrido <> num_recorrido_escolhido.
        CLEAR : l_zwm026.
      ENDIF.

** RL -> MOD 27.04.2006
** Prioridades
** Actualização da BD com o user que vai processar o recorrido
*      LOOP AT l_zwm026.
*        l_zwm026-user_name = sy-uname.
*        l_zwm026-estado = 'C'.
*        MODIFY l_zwm026.
*      ENDLOOP.
*
*      IF NOT l_zwm026[] IS INITIAL.
*        MODIFY zwm026 FROM TABLE l_zwm026.
*        COMMIT WORK.
*      ENDIF.

** Actualização da BD com o user que vai processar o recorrido
      IF NOT l_zwm026[] IS INITIAL.
        LOOP AT l_zwm026.
          l_zwm026-user_name = sy-uname.
          l_zwm026-estado = 'C'.
          MODIFY l_zwm026.

          MOVE-CORRESPONDING l_zwm026 TO itab_zwm026.
          APPEND itab_zwm026.
          UPDATE zwm026 FROM itab_zwm026.
          COMMIT WORK.
          CLEAR: itab_zwm026.
        ENDLOOP.

*** RL -> LOG ZWM026
*        CLEAR: zwm026_log, wa_log.
*        GET TIME.
*        LOOP AT l_zwm026.
*          MOVE-CORRESPONDING l_zwm026 TO wa_log.
*          wa_log-data = sy-datum.
*          wa_log-hora = sy-uzeit.
*          wa_log-user_tarefa = sy-uname.
*          wa_log-programa = sy-repid.
*          MODIFY zwm026_log FROM wa_log.
*        ENDLOOP.
*** RL <- LOG ZWM026

*        UPDATE zwm026 FROM TABLE itab_zwm026.
*        COMMIT WORK.
      ENDIF.
** RL <- MOD 27.04.2005

**Actualização do número de paletes que tem de colocar
** no empilhador
      DESCRIBE TABLE l_zwm026 LINES n_linhas.
      IF n_linhas = 1.
        n_paletes = 1.
        IF l_zwm026-pal_destino = 'PICKING 02'.
          CLEAR itab_zwm026.
          REFRESH itab_zwm026.
          LOOP AT l_zwm026.
            l_zwm026-pal_destino = 'PICKING 01'.
            MODIFY l_zwm026.

            MOVE-CORRESPONDING l_zwm026 TO itab_zwm026.
            UPDATE zwm026 FROM itab_zwm026.
            COMMIT WORK.
            APPEND itab_zwm026.
            CLEAR: itab_zwm026.
          ENDLOOP.

*          UPDATE zwm026 FROM TABLE itab_zwm026.
*          COMMIT WORK.
        ENDIF.
      ELSE.
        SORT l_zwm026 BY n_pal_picking.
        CLEAR : n_paletes.
        LOOP AT l_zwm026.
          AT NEW n_pal_picking.
            n_paletes = n_paletes + 1.
          ENDAT.
        ENDLOOP.
      ENDIF.
    ENDIF.

    CLEAR user_assignado.
    CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = lock_key
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

  ENDIF.

ENDFUNCTION.
