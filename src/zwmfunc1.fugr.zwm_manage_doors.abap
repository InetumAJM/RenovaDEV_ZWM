FUNCTION ZWM_MANAGE_DOORS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(OPERACAO) TYPE  CHAR1
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"  TABLES
*"      L_ZWM002 STRUCTURE  ZWM002
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      NO_WAREHOUSE
*"      TAB_ZWM002_NOT_FILLED
*"      TAB_L_ZWM002_FILLED
*"      TAB_L_ZWM002_NOT_FILLED
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
** 1 - Visualizar o estado das portas
** 2 - Modificar/Acrescentar estado das portas


  IF OPERACAO = '1'.

    IF NOT L_ZWM002[] IS INITIAL.
** ERRO tabela tem de vir preenchida
      RAISE TAB_L_ZWM002_FILLED.
    ELSE.
      SELECT * FROM ZWM002 INTO CORRESPONDING FIELDS OF TABLE L_ZWM002
               WHERE ARMAZEM = ARMAZEM.

      IF SY-SUBRC = 0.
** Por enquanto ordenar por múmero sequencial de entrada
        SORT L_ZWM002 BY PORTA.
      ELSE.
        RETURN_MSG-MSGID = 'ZWMMSG001'.
        RETURN_MSG-MSGNR = '002'.
        RETURN_MSG-MSGTYP = 'I'.
        RETURN_MSG-MSGSPRA = SY-LANGU.
        APPEND RETURN_MSG.

        RAISE TAB_ZWM002_NOT_FILLED.

      ENDIF.

    ENDIF.


  ELSEIF OPERACAO = '2'.

    IF NOT L_ZWM002[] IS INITIAL.

      READ TABLE L_ZWM002 INDEX 1.
      MOVE-CORRESPONDING L_ZWM002 TO ZWM002.
      MODIFY ZWM002.

      IF SY-SUBRC = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
    ELSE.
** ERRO tabela tem de vir preenchida
      RAISE TAB_L_ZWM002_NOT_FILLED.
    ENDIF.

    CLEAR L_ZWM002.
    REFRESH L_ZWM002.

  ELSE.

** ERRO e EXCEPÇÃO
    RAISE INVALID_PARAMETER.

  ENDIF.

ENDFUNCTION.
