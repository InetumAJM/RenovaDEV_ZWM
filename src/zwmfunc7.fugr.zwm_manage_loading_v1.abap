FUNCTION ZWM_MANAGE_LOADING_V1.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(OPERACAO) TYPE  CHAR1
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"  TABLES
*"      L_ZWM006 STRUCTURE  ZWM006_AUX
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      NO_WAREHOUSE
*"      TAB_L_ZWM006_FILLED
*"      TAB_ZWM006_NOT_FILLED
*"      TAB_L_ZWM006_NOT_FILLED
*"      INVALID_PARAMETER
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
** 1 - Visualizar o que está na fila de descarga
** 2 - Modificar/Acrescentar na fila de descarga


  IF OPERACAO = '1'.

    IF NOT L_ZWM006[] IS INITIAL.
** ERRO tabela nao pode vir preenchida
      RAISE TAB_L_ZWM006_FILLED.
    ELSE.
      SELECT * FROM ZWM006_AUX INTO CORRESPONDING FIELDS OF
                                    TABLE L_ZWM006
               WHERE ARMAZEM = ARMAZEM.

      IF SY-SUBRC = 0.
** Por enquanto ordenar por múmero sequencial de entrada
*        SORT L_ZWM006 BY NUM_ENTRADA ASCENDING.
      ELSE.
        RETURN_MSG-MSGID = 'ZWMMSG001'.
        RETURN_MSG-MSGNR = '004'.
        RETURN_MSG-MSGTYP = 'I'.
        RETURN_MSG-MSGSPRA = SY-LANGU.
        APPEND RETURN_MSG.

        RAISE TAB_ZWM006_NOT_FILLED.

      ENDIF.

    ENDIF.


  ELSEIF OPERACAO = '2'.

    IF NOT L_ZWM006[] IS INITIAL.

      READ TABLE L_ZWM006 INDEX 1.
      MOVE-CORRESPONDING L_ZWM006 TO ZWM006_AUX.
      MODIFY ZWM006_AUX.

      IF SY-SUBRC = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ELSE.
** ERRO tabela tem de vir preenchida
      RAISE TAB_L_ZWM006_NOT_FILLED.
    ENDIF.

    CLEAR L_ZWM006.
    REFRESH L_ZWM006.

  ELSE.

** ERRO e EXCEPÇÃO
    RAISE INVALID_PARAMETER.
  ENDIF.



ENDFUNCTION.
