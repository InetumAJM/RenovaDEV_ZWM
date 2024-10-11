FUNCTION zwm_entry_loading_v1.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"     REFERENCE(MATRICULA) TYPE  CHAR20
*"     REFERENCE(N_TRANSPORTE) LIKE  ZWM006_AUX-N_TRANSPORTE OPTIONAL
*"     REFERENCE(TRANSPORTADOR) LIKE  VTTK-TDLNR OPTIONAL
*"     REFERENCE(TIPO_CAMIAO) LIKE  VTTK-SDABW OPTIONAL
*"     REFERENCE(OBSERVACOES) LIKE  ZWM006_AUX-OBSERVACOES OPTIONAL
*"     REFERENCE(MODO) TYPE  CHAR1 DEFAULT 'C'
*"     REFERENCE(OBSERVACOES2) TYPE  ZWM006_AUX-OBSERVACOES OPTIONAL
*"     REFERENCE(IS_ZWM_018) TYPE  ZWM_018 OPTIONAL
*"  EXPORTING
*"     REFERENCE(NUM_ENTRADA) LIKE  ZWM003-NUM_ENTRADA
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      NO_WAREHOUSE
*"      DUPLICATE_ENTRY
*"----------------------------------------------------------------------

  CLEAR : return_msg.
  REFRESH : return_msg.

** Verificar se o armazém introduzido é válido
  SELECT SINGLE * FROM t300
                  WHERE lgnum = armazem.
  IF sy-subrc <> 0.
    RAISE no_warehouse.
  ENDIF.

** Verificar se já existe alguma entrada na tabela para o mesmo camião
** que ainda não tenha sido finalizada
  CLEAR zwm006_aux.
  SELECT SINGLE * FROM zwm006_aux
                  WHERE armazem = armazem
                  AND   n_transporte = n_transporte
                  AND   finalizada = ' '.

*  IF sy-subrc = 0.
  IF modo EQ 'M'.
    IF zwm006_aux IS INITIAL.
      EXIT.
    ENDIF.
    num_entrada = zwm006_aux-num_entrada.
*    zwm006_aux-armazem        = armazem.
    zwm006_aux-matricula      = matricula.
*    zwm006_aux-num_entrada    = num_entrada.
*------------------------------------------------Nº do Transporte
*      zwm006_aux-n_transporte = n_transporte.
*------------------------------------------------Data e Hora de Entrada
*    zwm006_aux-hora_reg   = sy-timlo.
*    zwm006_aux-data_reg   = sy-datlo.

*-------------------------------------------------Observações
    zwm006_aux-observacoes  = observacoes.
    zwm006_aux-observacoes2 = observacoes2.
    zwm006_aux = CORRESPONDING #( BASE ( zwm006_aux ) is_zwm_018 ).

    MODIFY zwm006_aux.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ELSE.
    IF sy-subrc = 0.

      return_msg-msgid = 'ZWMMSG001'.
      return_msg-msgtyp = 'I'.
      return_msg-msgnr = '008'.
      APPEND return_msg.

      RAISE duplicate_entry.

    ENDIF.
*----------------------------------Geração do número de entrada - Talão
    CLEAR : num_entrada.
    DATA l_number LIKE inrdp-tonumber.
*  break roffd.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '05'
        object                  = 'LVS_LENUM'
        quantity                = '1'
      IMPORTING
        number                  = l_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    MOVE l_number+15(5) TO num_entrada.

* Actualização da BD
    CLEAR zwm006_aux.
    zwm006_aux-armazem        = armazem.
    zwm006_aux-matricula      = matricula.
    zwm006_aux-num_entrada    = num_entrada.
*------------------------------------------------Nº do Transporte
    IF NOT n_transporte IS INITIAL.
      zwm006_aux-n_transporte = n_transporte.
    ELSE.
      CLEAR zwm006_aux-n_transporte.
    ENDIF.
*------------------------------------------------Data e Hora de Entrada
*  zwm006_aux-hora_reg   = sy-uzeit.
*  zwm006_aux-data_reg   = sy-datum.

    zwm006_aux-hora_reg   = sy-timlo.
    zwm006_aux-data_reg   = sy-datlo.

*-------------------------------------------------Observações
    IF NOT observacoes IS INITIAL.
      zwm006_aux-observacoes = observacoes.
    ELSE.
      CLEAR zwm006_aux-observacoes.
    ENDIF.
    zwm006_aux-observacoes2 = observacoes2.
    zwm006_aux = CORRESPONDING #( BASE ( zwm006_aux ) is_zwm_018 ).

    MODIFY zwm006_aux.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.


** IMPRESSÃO DO TALÃO ....  fazer ou não ???

ENDFUNCTION.
