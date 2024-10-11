FUNCTION ZWM_MANAGE_UNLOADING.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(OPERACAO) TYPE  CHAR1
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"  TABLES
*"      L_ZWM005 STRUCTURE  ZWM005
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      NO_WAREHOUSE
*"      TAB_L_ZWM005_FILLED
*"      TAB_ZWM005_NOT_FILLED
*"      TAB_L_ZWM005_NOT_FILLED
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

    IF NOT L_ZWM005[] IS INITIAL.
** ERRO tabela nao pode vir preenchida
      RAISE TAB_L_ZWM005_FILLED.
    ELSE.
      SELECT * FROM ZWM005 INTO CORRESPONDING FIELDS OF TABLE L_ZWM005
               WHERE ARMAZEM = ARMAZEM.

      IF SY-SUBRC = 0.
** Por enquanto ordenar por múmero sequencial de entrada
        SORT L_ZWM005 BY NUM_ENTRADA ASCENDING.
      ELSE.
        RETURN_MSG-MSGID = 'ZWMMSG001'.
        RETURN_MSG-MSGNR = '005'.
        RETURN_MSG-MSGTYP = 'I'.
        RETURN_MSG-MSGSPRA = SY-LANGU.
        APPEND RETURN_MSG.

        RAISE TAB_ZWM005_NOT_FILLED.

      ENDIF.

    ENDIF.


  ELSEIF OPERACAO = '2'.

    IF NOT L_ZWM005[] IS INITIAL.

*      READ TABLE L_ZWM005 INDEX 1.
      LOOP AT L_ZWM005.

        MOVE-CORRESPONDING L_ZWM005 TO ZWM005.
** Descrição do transportador
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = L_ZWM005-TRANSPORTADOR
          IMPORTING
            OUTPUT = L_ZWM005-TRANSPORTADOR.


        SELECT SINGLE NAME1 FROM LFA1 INTO ZWM005-DESC_TRANSP
                            WHERE LIFNR = L_ZWM005-TRANSPORTADOR.


        MODIFY ZWM005.

        IF SY-SUBRC = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDLOOP.

    ELSE.
** ERRO tabela tem de vir preenchida
      RAISE TAB_L_ZWM005_NOT_FILLED.
    ENDIF.

    CLEAR L_ZWM005.
    REFRESH L_ZWM005.

  ELSE.

** ERRO e EXCEPÇÃO
    RAISE INVALID_PARAMETER.
  ENDIF.



ENDFUNCTION.
