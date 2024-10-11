FUNCTION zwm_entry_parking_v1.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"     REFERENCE(TIPO_CAMIAO) TYPE  ZTIPO_CAMIAO
*"     REFERENCE(MATRICULA) LIKE  ZWM003-MATRICULA
*"     REFERENCE(NUM_ENTRADA) LIKE  ZWM003-NUM_ENTRADA
*"     REFERENCE(OBSERVACOES) LIKE  ZWM003-OBSERVACOES OPTIONAL
*"     REFERENCE(OBSERVACOES2) TYPE  ZWM003-OBSERVACOES OPTIONAL
*"  EXPORTING
*"     REFERENCE(PORTA) LIKE  ZWM002-PORTA
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      NO_WAREHOUSE
*"      WRONG_TIPO_CAMIAO
*"      ERROR_IN_MANAGE_PARKING
*"      ERROR_IN_MANAGE_DOORS
*"      ERROR_IN_GET_DOOR
*"----------------------------------------------------------------------

  CLEAR: zwm028, vbsk, vbss, vttp.

  CLEAR   : return_msg,ti_zwm002, ti_zwm003.
  REFRESH : return_msg,ti_zwm002, ti_zwm003.


** Carregar tabelas de parametrização
  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs       = armazem
    TABLES
      ti_zwm001 = ti_zwm001.



** Verificar se o armazém introduzido é válido
  SELECT SINGLE * FROM t300
                  WHERE lgnum = armazem.
  IF sy-subrc <> 0.
    RAISE no_warehouse.
  ENDIF.


  IF tipo_camiao = 'O'.

    PERFORM actualiza_lista_espera USING armazem matricula num_entrada
                                         observacoes observacoes2
                                         'CARGA' sy-timlo
                                         sy-datlo    'E'.


** Camião para descarga
  ELSEIF tipo_camiao = 'R'.

    PERFORM actualiza_lista_espera USING armazem matricula num_entrada
                                         observacoes observacoes2
                                         'DESCARGA' sy-timlo
                                         sy-datlo    'E'.
**Carga directa
  ELSEIF tipo_camiao = 'E'.

    PERFORM actualiza_lista_espera USING armazem     matricula
                                         num_entrada observacoes observacoes2
                                         'CARGA'     sy-timlo
                                         sy-datlo    'E'.

** Camião para carga através de pulmão
  ELSEIF tipo_camiao = 'P'.

    PERFORM actualiza_lista_espera USING armazem matricula num_entrada
                                         observacoes observacoes2
                                         'CARGA' sy-timlo
                                         sy-datlo     'F'.

  ELSE.
    RAISE wrong_tipo_camiao.
  ENDIF.


ENDFUNCTION.
