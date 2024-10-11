FUNCTION zwm_next_truck_s.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"  TABLES
*"      RETURN_TRUCK STRUCTURE  ZWM_RETURN_TRUCK
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      NO_DOOR_UNLOCKED
*"      EXCEPTION_MANAGE_PARKING
*"      NO_QUEUE_EXISTS
*"      NO_DOOR_TRUCK
*"      ERROR_IN_GET_DOOR
*"      ERROR_IN_MANAGE_DOORS
*"----------------------------------------------------------------------



  CLEAR : return_msg, l_zwm003, pulmao, num_quantos, tipo_camiao, porta,
          incompleta, encontrou_porta, t_lagp, num_pulmao, t_portas.
  REFRESH: return_msg, l_zwm003, l_lips, t_lagp, t_portas.

  CLEAR : return_truck, return_truck[].

** Dados de tabelas de parametrização

** Tipo de depósito dos pulmões
  DATA : st_pul LIKE lagp-lgtyp.
  CLEAR : st_pul.
  PERFORM get_parameter USING armazem
                          'ENTRADA_ARMAZEM'
                          'ST_PUL'
                           st_pul.


** Carregamento do estado GLOBAL do estado das portas
  CLEAR : l_zwm002.
  REFRESH: l_zwm002.


  CALL FUNCTION 'ZWM_MANAGE_DOORS'
    EXPORTING
      operacao                = '1'
      armazem                 = armazem
    TABLES
      l_zwm002                = l_zwm002
      return_msg              = return_msg
    EXCEPTIONS
      no_warehouse            = 1
      tab_zwm002_not_filled   = 2
      tab_l_zwm002_filled     = 3
      tab_l_zwm002_not_filled = 4
      invalid_parameter       = 5
      OTHERS                  = 6.
  IF sy-subrc <> 0.
    RAISE error_in_manage_doors.
  ENDIF.


** Primeiro - Ordenação da fila de espera para escolha do primeiro
**            camião a ser atribuido a uma porta

  CALL FUNCTION 'ZWM_MANAGE_PARKING_V1'
    EXPORTING
      operacao                = '1'
      armazem                 = armazem
    TABLES
      l_zwm003                = l_zwm003
      return_msg              = return_msg
    EXCEPTIONS
      tab_zwm003_not_filled   = 1
      invalid_parameter       = 2
      tab_l_zwm003_filled     = 3
      tab_l_zwm003_not_filled = 4
      no_warehouse            = 5
      OTHERS                  = 6.
  IF sy-subrc <> 0.
    RAISE exception_manage_parking.
  ELSE.
** Verificar se existem camiões em fila de espera
** Se sim - continuação do algoritmo
** Se não - erro a indicar que nao existem camiões em fila de espera
    IF l_zwm003[] IS INITIAL.

      return_msg-msgid = 'ZWMMSG001'.
      return_msg-msgnr = '013'.
      return_msg-msgtyp = 'I'.
      return_msg-msgspra = sy-langu.
      APPEND return_msg.

      RAISE no_queue_exists.

    ELSEIF NOT l_zwm003[] IS INITIAL.

** Apagar as entradas que já estejam atribuídas a uma porta
      DELETE l_zwm003 WHERE estado = 'P'.

** Ordenação da fila de espera - INCOMPLETA !!!!!!! - FALTA ITENERÁRIO
*      SORT L_ZWM003 BY DATA ASCENDING HORA_ENTRADA ASCENDING.
** Ordenação da fila de espera - INCOMPLETA !!!!!!!
      CALL FUNCTION 'ZWM_SORT_QUEUE_V1'
        EXPORTING
          armazem    = armazem
        TABLES
          return_msg = return_msg
          l_zwm003   = l_zwm003.

      CLEAR s_tabix.
*======================================================================
** Cálculo do algoritmo de selecção do próximo camião
      LOOP AT l_zwm003 WHERE atribuida IS INITIAL.

        s_tabix = sy-tabix.
** Tipo de camião ??

        PERFORM tipo_camiao USING armazem.

        CLEAR : incompleta, lips-lgtyp,lagp, vttp-vbeln.

        IF tipo_camiao = 'R'.

*----existe um PULMÃO LIVRE entre os associados às portas de descarga ?

          PERFORM get_porta_descarga TABLES return_truck
                                     USING armazem st_pul.

        ELSEIF tipo_camiao = 'E'.

*----------------------------------------------Camião de carga directa
*-------------seleccionar as portas associadas ao pulmão desbloqueadas

          PERFORM get_portas_carga_directa TABLES return_truck
                                           USING armazem.

        ELSEIF tipo_camiao = 'P'.

*---------------------------------------------Camião de carga no pulmão
*--------------seleccionar as portas associadas ao pulmão desbloqueadas

          PERFORM get_porta_pulmao TABLES return_truck
                                   USING armazem.

        ENDIF.            "tipo_camiao = ??

      ENDLOOP.

    ENDIF.

  ENDIF.                "ZWM_MANAGE_PARKING


  CLEAR : l_portas_descarga, l_portas_cargap, l_portas_cargad, l_zwm002.
  REFRESH : l_portas_descarga, l_portas_cargap, l_portas_cargad,
            l_zwm002.


  IF return_truck[] IS INITIAL AND NOT ok_code_0005 IS INITIAL.

    return_msg-msgid = 'ZWMMSG001'.
    return_msg-msgnr = '014'.
    return_msg-msgtyp = 'I'.
    return_msg-msgspra = sy-langu.
    APPEND return_msg.

    RAISE no_door_truck.

  ENDIF.

ENDFUNCTION.
