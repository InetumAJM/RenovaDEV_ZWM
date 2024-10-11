FUNCTION ZWM_SORT_QUEUE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"      L_ZWM003 STRUCTURE  ZWM003
*"----------------------------------------------------------------------

** Fila de espera
  DATA : BEGIN OF TI_ZWM003 OCCURS 0.
          INCLUDE STRUCTURE ZWM003.
  DATA : DPLBG LIKE VTTK-DPLBG,             "dia carga planeada
         UPLBG LIKE VTTK-UPLBG,             "hora carga planeada
         VKORG LIKE LIKP-VKORG,             "organização de vendas
         P_ORV,                             "peso organização vendas
         N_CLI TYPE I,                      "numero de clientes transpo.
         LDDAT LIKE LIKP-LDDAT,             "dia de descarga
         LDUHR LIKE LIKP-LDUHR.             "hora de descarga
  DATA : END OF TI_ZWM003.

  DATA : VALOR LIKE ZWM001-VALOR,
         INDEX LIKE SY-TABIX.


  CLEAR : TI_ZWM003, INDEX.
  REFRESH : TI_ZWM003.

  TI_ZWM003[] = L_ZWM003[].

**
** Ordenação da fila de espera - CRITÉRIOS
**
** 1 - Dia e Hora de carga planeada
** 2 - Organização de vendas
**  2.1 - França
**  2.2 - Espanha
**  2.3 - Servisan
**  2.4 - Portugal
** 3 - Zona de transporte - dentro deste critério em caso de igualdade
**     dá-se prioridade às entregas que tenham mais clientes-FALTA ESTAA
** 4 - Dia e hora de descarga no cliente
** 5 - Hora de chegada à portaria

  LOOP AT TI_ZWM003.

    INDEX = SY-TABIX.

** Número do transporte associado à carga
    SELECT SINGLE N_TRANSPORTE FROM ZWM006 INTO ZWM006-N_TRANSPORTE
                               WHERE ARMAZEM = ARMAZEM AND
                                    NUM_ENTRADA = TI_ZWM003-NUM_ENTRADA.
    IF SY-SUBRC = 0.

      SELECT SINGLE * FROM VTTK
                      WHERE TKNUM = ZWM006-N_TRANSPORTE.
      IF SY-SUBRC = 0.
** Dia e Hora de carga planeada
        TI_ZWM003-DPLBG = VTTK-DPLBG.
        TI_ZWM003-UPLBG = VTTK-UPLBG.
      ELSE.
        TI_ZWM003-DPLBG = '99999999'.
        TI_ZWM003-UPLBG = '999999'.
      ENDIF.

** Número de clientes associados ao transporte
      SELECT COUNT( DISTINCT VBELN ) FROM VTTP INTO TI_ZWM003-N_CLI
                      WHERE TKNUM = ZWM006-N_TRANSPORTE.

** Organização de vendas associada à remessa
** Remessa
      SELECT SINGLE VBELN FROM VTTP INTO VTTP-VBELN
                          WHERE TKNUM = ZWM006-N_TRANSPORTE.
      CLEAR LIKP.
** Organização de vendas, dia de descarga e hora de descarga
      SELECT SINGLE VKORG LDDAT LDUHR FROM LIKP
                    INTO (TI_ZWM003-VKORG,TI_ZWM003-LDDAT,
                          TI_ZWM003-LDUHR)
                          WHERE VBELN = VTTP-VBELN.
      IF SY-SUBRC = 0.
** Carregar o peso da organização de vendas.
        PERFORM GET_PARAMETER USING ARMAZEM
                                    'GESTAO_PARQUE'
                                    TI_ZWM003-VKORG(2)
                                    VALOR.
        IF SY-SUBRC = 0.
          WRITE VALOR TO TI_ZWM003-P_ORV LEFT-JUSTIFIED.
        ENDIF.
      ELSE.
        TI_ZWM003-LDDAT = '99999999'.
        TI_ZWM003-LDUHR = '999999'.
      ENDIF.

** É uma DESCARGA
    ELSE.

      TI_ZWM003-DPLBG = '99999999'.
      TI_ZWM003-UPLBG = '999999'.
      TI_ZWM003-N_CLI = 0.
      TI_ZWM003-LDDAT = '99999999'.
      TI_ZWM003-LDUHR = '999999'.
      CLEAR TI_ZWM003-P_ORV.

    ENDIF.

    MODIFY TI_ZWM003 INDEX INDEX.
    CLEAR : ZWM006-N_TRANSPORTE, VTTK, TI_ZWM003, VALOR, VTTP-VBELN.

  ENDLOOP.

** Actualização dos campos de data e hora para a ordenação
  CLEAR INDEX.
  LOOP AT TI_ZWM003.

    INDEX = SY-TABIX.

    IF TI_ZWM003-DPLBG = '00000000'.
      TI_ZWM003-DPLBG = '99999999'.
      TI_ZWM003-UPLBG = '999999'.
    ENDIF.

    IF TI_ZWM003-LDDAT = '00000000'.
      TI_ZWM003-LDDAT = '99999999'.
      TI_ZWM003-LDUHR = '999999'.
    ENDIF.

    MODIFY TI_ZWM003 INDEX INDEX.
  ENDLOOP.


*  BREAK-POINT.
  SORT TI_ZWM003 BY DPLBG ASCENDING UPLBG ASCENDING P_ORV DESCENDING
                    N_CLI DESCENDING LDDAT ASCENDING LDUHR ASCENDING
                    DATA ASCENDING HORA_ENTRADA ASCENDING.


  CLEAR L_ZWM003.
  REFRESH L_ZWM003.

** Actualização da tabela final
  LOOP AT TI_ZWM003.
    MOVE-CORRESPONDING TI_ZWM003 TO L_ZWM003.
    APPEND L_ZWM003.
  ENDLOOP.

ENDFUNCTION.
