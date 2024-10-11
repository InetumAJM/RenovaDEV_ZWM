FUNCTION zwm_entry_loading.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"     REFERENCE(MATRICULA) TYPE  CHAR20
*"     REFERENCE(N_TRANSPORTE) LIKE  ZWM006-N_TRANSPORTE OPTIONAL
*"     REFERENCE(TRANSPORTADOR) LIKE  ZWM006-TRANSPORTADOR OPTIONAL
*"     REFERENCE(TIPO_CAMIAO) LIKE  ZWM006-TIPO_CAMIAO OPTIONAL
*"     REFERENCE(OBSERVACOES) LIKE  ZWM006-OBSERVACOES OPTIONAL
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
  SELECT SINGLE * FROM zwm006
                  WHERE armazem = armazem AND
                        matricula = matricula AND
                        finalizada = ' '.
  IF sy-subrc = 0.

    return_msg-msgid = 'ZWMMSG001'.
    return_msg-msgtyp = 'I'.
    return_msg-msgnr = '008'.
    APPEND return_msg.

    RAISE duplicate_entry.

  ENDIF.


* Geração do número de entrada

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

  CLEAR zwm006.
  zwm006-armazem = armazem.
  zwm006-matricula = matricula.
  zwm006-num_entrada = num_entrada.
  IF NOT n_transporte IS INITIAL.
    zwm006-n_transporte = n_transporte.
  ELSE.
    CLEAR zwm006-n_transporte.
  ENDIF.
  zwm006-hora_entrada = sy-uzeit.
  zwm006-data = sy-datum.
  IF NOT transportador IS INITIAL.
    zwm006-transportador = transportador.
  ELSE.
    CLEAR zwm006-transportador.
  ENDIF.
  IF NOT tipo_camiao IS INITIAL.
    zwm006-tipo_camiao = tipo_camiao.
  ELSE.
    CLEAR zwm006-tipo_camiao.
  ENDIF.
  IF NOT observacoes IS INITIAL.
    zwm006-observacoes = observacoes.
  ELSE.
    CLEAR zwm006-observacoes.
  ENDIF.

  MODIFY zwm006.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.


** IMPRESSÃO DO TALÃO ....  fazer ou não ???

ENDFUNCTION.
