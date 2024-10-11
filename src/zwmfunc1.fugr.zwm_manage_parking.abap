FUNCTION ZWM_MANAGE_PARKING.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(OPERACAO) TYPE  CHAR1
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"  TABLES
*"      L_ZWM003 STRUCTURE  ZWM003 OPTIONAL
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      TAB_ZWM003_NOT_FILLED
*"      INVALID_PARAMETER
*"      TAB_L_ZWM003_FILLED
*"      TAB_L_ZWM003_NOT_FILLED
*"      NO_WAREHOUSE
*"----------------------------------------------------------------------

  CLEAR : RETURN_MSG.
  REFRESH : RETURN_MSG.

** Verificar se o armazém introduzido é válido
  SELECT SINGLE * FROM T300
                  WHERE LGNUM = ARMAZEM.
  IF SY-SUBRC <> 0.
    RAISE NO_WAREHOUSE.
  ENDIF.

** POSSIVEIS OPERAÇÔES
**
** 1 - Visualizar o que está na fila de espera
** 2 - Modificar/Acrescentar na fila de espera
** 3 - Retirar o que está na fila de espera e acrescentar entrada ao
**     histórico

  IF OPERACAO = '1'.

    IF NOT L_ZWM003[] IS INITIAL.
** ERRO tabela tem de vir preenchida
      RAISE TAB_L_ZWM003_FILLED.
    ELSE.
      SELECT * FROM ZWM003 INTO CORRESPONDING FIELDS OF TABLE L_ZWM003
               WHERE ARMAZEM = ARMAZEM.

      IF SY-SUBRC = 0.
** Por enquanto ordenar por múmero sequencial de entrada
*        SORT L_ZWM003 BY NUM_ENTRADA ASCENDING.

        CALL FUNCTION 'ZWM_SORT_QUEUE'
          EXPORTING
            ARMAZEM    = ARMAZEM
          TABLES
            RETURN_MSG = RETURN_MSG
            L_ZWM003   = L_ZWM003.

** Actualização das posições da fila de espera
        LOOP AT L_ZWM003.
          L_ZWM003-POSICAO = SY-TABIX.
          MODIFY L_ZWM003.
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
        RETURN_MSG-MSGID = 'ZWMMSG001'.
        RETURN_MSG-MSGNR = '001'.
        RETURN_MSG-MSGTYP = 'I'.
        RETURN_MSG-MSGSPRA = SY-LANGU.
        APPEND RETURN_MSG.

*        RAISE TAB_ZWM003_NOT_FILLED.

      ENDIF.

    ENDIF.


  ELSEIF OPERACAO = '2'.

    IF NOT L_ZWM003[] IS INITIAL.

      READ TABLE L_ZWM003 INDEX 1.
      MOVE-CORRESPONDING L_ZWM003 TO ZWM003.
      MODIFY ZWM003.

      IF SY-SUBRC = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ELSE.
** ERRO tabela tem de vir preenchida
      RAISE TAB_L_ZWM003_NOT_FILLED.
    ENDIF.

    CLEAR L_ZWM003.
    REFRESH L_ZWM003.

  ELSEIF OPERACAO = '3'.

    IF NOT L_ZWM003[] IS INITIAL.

** Actualiza tabela de histórico
      READ TABLE L_ZWM003 INDEX 1.
      MOVE-CORRESPONDING L_ZWM003 TO ZWM004.
      MODIFY ZWM004.

      IF SY-SUBRC = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

** Apaga entrada da tabela onde está a fila de espera
      DELETE ZWM003 FROM TABLE L_ZWM003.
      IF SY-SUBRC = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

    ELSE.
** ERRO tabela tem de vir preenchida
      RAISE TAB_L_ZWM003_NOT_FILLED.
    ENDIF.

    CLEAR L_ZWM003.
    REFRESH L_ZWM003.

  ELSE.

** ERRO e EXCEPÇÃO
    RAISE INVALID_PARAMETER.
  ENDIF.



ENDFUNCTION.
