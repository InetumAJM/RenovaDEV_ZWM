FUNCTION zwm_manage_parking_v1.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(OPERACAO) TYPE  CHAR1
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"  TABLES
*"      L_ZWM003 STRUCTURE  ZWM003_AUX OPTIONAL
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      TAB_ZWM003_NOT_FILLED
*"      INVALID_PARAMETER
*"      TAB_L_ZWM003_FILLED
*"      TAB_L_ZWM003_NOT_FILLED
*"      NO_WAREHOUSE
*"----------------------------------------------------------------------

  CLEAR : return_msg.
  REFRESH : return_msg.

** Verificar se o armazém introduzido é válido
  SELECT SINGLE * FROM t300
                  WHERE lgnum = armazem.
  IF sy-subrc <> 0.
    RAISE no_warehouse.
  ENDIF.

** POSSIVEIS OPERAÇÔES
**
** 1 - Visualizar o que está na fila de espera
** 2 - Modificar/Acrescentar na fila de espera
** 3 - Retirar o que está na fila de espera e acrescentar entrada ao
**     histórico

  IF operacao = '1'.

    IF NOT l_zwm003[] IS INITIAL.
** ERRO tabela tem de vir preenchida
      RAISE tab_l_zwm003_filled.
    ELSE.
      SELECT * FROM zwm003_aux INTO
                           CORRESPONDING FIELDS OF TABLE l_zwm003
               WHERE armazem = armazem.

      IF sy-subrc = 0.
** Por enquanto ordenar por múmero sequencial de entrada
*        SORT L_ZWM003 BY NUM_ENTRADA ASCENDING.

        CALL FUNCTION 'ZWM_SORT_QUEUE_V1'
          EXPORTING
            armazem    = armazem
          TABLES
            return_msg = return_msg
            l_zwm003   = l_zwm003.

** Actualização das posições da fila de espera
        LOOP AT l_zwm003.
          l_zwm003-posicao = sy-tabix.
          MODIFY l_zwm003.
        ENDLOOP.

** Actualização da BD - POR ENQUANTO NAO ACTUALIZO
*        IF NOT L_ZWM003[] IS INITIAL.
*          MODIFY ZWM003 FROM TABLE L_ZWM003.
*          IF SY-SUBRC = 0.
*            COMMIT WORK.
*          ELSE.
*            ROLLBACK WORK.
*          ENDIF.
*        ENDIF.

      ELSE.
        return_msg-msgid = 'ZWMMSG001'.
        return_msg-msgnr = '001'.
        return_msg-msgtyp = 'I'.
        return_msg-msgspra = sy-langu.
        APPEND return_msg.

*        RAISE TAB_ZWM003_NOT_FILLED.

      ENDIF.

    ENDIF.


  ELSEIF operacao = '2'.

    IF NOT l_zwm003[] IS INITIAL.

      READ TABLE l_zwm003 INDEX 1.
      "INI-Inetum-AJM-03/07/24-Nao Atualizar carga após atribuiçao de porta
      DATA lv_subrc TYPE sy-subrc.
      SELECT SINGLE *
        FROM zwm003_aux
        INTO @DATA(ls_zwm003_aux)
        WHERE armazem     EQ @l_zwm003-armazem
          AND num_entrada EQ @l_zwm003-num_entrada.
      IF sy-subrc IS NOT INITIAL.
        SELECT SINGLE *
          FROM zwm006_aux
          INTO @DATA(ls_zwm006_aux)
          WHERE armazem      EQ @l_zwm003-armazem
            AND num_entrada  EQ @l_zwm003-num_entrada
            AND n_transporte EQ @l_zwm003-n_transporte.
        IF sy-subrc IS INITIAL AND
           ls_zwm006_aux-porta IS NOT INITIAL.
          lv_subrc = 4.
        ELSE.
          CLEAR lv_subrc.
        ENDIF.
      ELSE.
*        CLEAR sy-subrc.
        CLEAR lv_subrc.
      ENDIF.
      IF lv_subrc IS INITIAL.
        "fim-inetum-ajm-03/07/24-nao atualizar carga após atribuiçao de porta

        MOVE-CORRESPONDING l_zwm003 TO zwm003_aux.
        MODIFY zwm003_aux.

        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
    ELSE.
** ERRO tabela tem de vir preenchida
      RAISE tab_l_zwm003_not_filled.
    ENDIF.

    CLEAR l_zwm003.
    REFRESH l_zwm003.

  ELSEIF operacao = '3'.

    IF NOT l_zwm003[] IS INITIAL.

** Actualiza tabela de histórico
      READ TABLE l_zwm003 INDEX 1.
      MOVE-CORRESPONDING l_zwm003 TO zwm004.
      MODIFY zwm004.

      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

** Apaga entrada da tabela onde está a fila de espera
      DELETE zwm003_aux FROM TABLE l_zwm003.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

    ELSE.
** ERRO tabela tem de vir preenchida
      RAISE tab_l_zwm003_not_filled.
    ENDIF.

    CLEAR l_zwm003.
    REFRESH l_zwm003.

  ELSE.

** ERRO e EXCEPÇÃO
    RAISE invalid_parameter.
  ENDIF.



ENDFUNCTION.
