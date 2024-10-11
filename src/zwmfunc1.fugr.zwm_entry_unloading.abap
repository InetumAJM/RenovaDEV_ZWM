FUNCTION zwm_entry_unloading.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"     REFERENCE(ORD_COMPRA) LIKE  EKKO-EBELN OPTIONAL
*"     REFERENCE(MATRICULA) TYPE  CHAR20
*"     REFERENCE(TIPO_CAMIAO) LIKE  ZWM005-TIPO_CAMIAO OPTIONAL
*"     REFERENCE(TRANSPORTADOR) LIKE  ZWM005-TRANSPORTADOR OPTIONAL
*"     REFERENCE(OBSERVACOES) LIKE  ZWM005-OBSERVACOES OPTIONAL
*"     REFERENCE(OBSERVACOES2) LIKE  ZWM005-OBSERVACOES OPTIONAL
*"  EXPORTING
*"     REFERENCE(NUM_ENTRADA) LIKE  ZWM003-NUM_ENTRADA
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"      ORDENS STRUCTURE  ZORDENS
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
  SELECT SINGLE * FROM zwm005
                  WHERE armazem = armazem AND
                        matricula = matricula AND
                        finalizada = ' '.
  IF sy-subrc = 0.

    return_msg-msgid = 'ZWMMSG001'.
    return_msg-msgtyp = 'I'.
    return_msg-msgnr = '006'.
    APPEND return_msg.

    RAISE duplicate_entry.

  ENDIF.


** Geração do número de entrada -
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

** Actualização da BD

  CLEAR zwm005.
  zwm005-armazem = armazem.
  zwm005-matricula = matricula.
  zwm005-hora_entrada = sy-uzeit.
  zwm005-data = sy-datum.
  IF NOT ord_compra IS INITIAL.
    zwm005-ord_compra = ord_compra.
  ELSE.
    CLEAR zwm005-ord_compra.
  ENDIF.
  IF NOT tipo_camiao IS INITIAL.
    zwm005-tipo_camiao = tipo_camiao.
  ELSE.
    CLEAR zwm005-tipo_camiao.
  ENDIF.
  IF NOT observacoes IS INITIAL.
    zwm005-observacoes = observacoes.
  ELSE.
    CLEAR zwm005-observacoes.
  ENDIF.
  zwm005-observacoes2 = observacoes2.

  IF NOT transportador IS INITIAL.
    zwm005-transportador = transportador.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = zwm005-transportador
      IMPORTING
        output = zwm005-transportador.


    SELECT SINGLE name1 FROM lfa1 INTO zwm005-desc_transp
                        WHERE lifnr = zwm005-transportador.

  ELSE.
    CLEAR zwm005-transportador.
  ENDIF.
  zwm005-num_entrada = num_entrada.
  MODIFY zwm005.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

** Caso tenha mais que uma ordem de compra para dar entrada
  IF NOT ordens[] IS INITIAL.

    LOOP AT ordens.

      CLEAR zwm005.
      zwm005-armazem = armazem.
      zwm005-matricula = matricula.
      zwm005-hora_entrada = sy-uzeit.
      zwm005-data = sy-datum.
      IF NOT ord_compra IS INITIAL.
        zwm005-ord_compra = ordens-ebeln.
      ELSE.
        CLEAR zwm005-ord_compra.
      ENDIF.
      IF NOT tipo_camiao IS INITIAL.
        zwm005-tipo_camiao = tipo_camiao.
      ELSE.
        CLEAR zwm005-tipo_camiao.
      ENDIF.
      IF NOT observacoes IS INITIAL.
        zwm005-observacoes = observacoes.
      ELSE.
        CLEAR zwm005-observacoes.
      ENDIF.

      IF NOT transportador IS INITIAL.
        zwm005-transportador = transportador.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = zwm005-transportador
          IMPORTING
            output = zwm005-transportador.


        SELECT SINGLE name1 FROM lfa1 INTO zwm005-desc_transp
                            WHERE lifnr = zwm005-transportador.

      ELSE.
        CLEAR zwm005-transportador.
      ENDIF.
      zwm005-num_entrada = num_entrada.
      MODIFY zwm005.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.


    ENDLOOP.
  ENDIF.


** IMPRESSÃO DO TALÃO ....  fazer ou não ???

ENDFUNCTION.
