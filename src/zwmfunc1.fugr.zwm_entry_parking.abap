FUNCTION zwm_entry_parking.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"     REFERENCE(TIPO_CAMIAO) TYPE  ZTIPO_CAMIAO
*"     REFERENCE(MATRICULA) LIKE  ZWM003-MATRICULA
*"     REFERENCE(NUM_ENTRADA) LIKE  ZWM003-NUM_ENTRADA
*"     REFERENCE(OBSERVACOES) LIKE  ZWM003-OBSERVACOES OPTIONAL
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

  DATA : ti_zwm003 LIKE zwm003 OCCURS 0 WITH HEADER LINE,
         ti_zwm002 LIKE zwm002 OCCURS 0 WITH HEADER LINE.

** Contem as remessas associadas ao transporte
  DATA : BEGIN OF remessas OCCURS 0,
         vbeln LIKE likp-vbeln.
  DATA : END OF remessas.


  DATA : existe_porta,
         incompleta.

  CLEAR : porta, existe_porta, incompleta,
          zwm028, vbsk, vbss, vttp.

  CLEAR : return_msg,ti_zwm002,ti_zwm003.
  REFRESH : return_msg,ti_zwm002,ti_zwm003.


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

    CLEAR ti_zwm003.
    REFRESH ti_zwm003.

**Prenchimento da tabela para dar entrada na fila de espera
    ti_zwm003-armazem = armazem.
    ti_zwm003-matricula = matricula.
    ti_zwm003-num_entrada = num_entrada.
    IF NOT observacoes IS INITIAL.
      ti_zwm003-observacoes = observacoes.
    ELSE.
      CLEAR ti_zwm003-observacoes.
    ENDIF.
    ti_zwm003-operacao = 'CARGA'.
    ti_zwm003-hora_entrada = sy-uzeit.
    ti_zwm003-data = sy-datum.
    ti_zwm003-estado = 'E'.
    APPEND ti_zwm003.

    CALL FUNCTION 'ZWM_MANAGE_PARKING'
      EXPORTING
        operacao                = '2'
        armazem                 = armazem
      TABLES
        l_zwm003                = ti_zwm003
        return_msg              = return_msg
      EXCEPTIONS
        tab_zwm003_not_filled   = 1
        invalid_parameter       = 2
        tab_l_zwm003_filled     = 3
        tab_l_zwm003_not_filled = 4
        no_warehouse            = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
      RAISE error_in_manage_parking.
    ENDIF.

** Camião para descarga
  ELSEIF tipo_camiao = 'R'.

** Verificar se existem camiões em PARKING
    CALL FUNCTION 'ZWM_MANAGE_PARKING'
      EXPORTING
        operacao                = '1'
        armazem                 = armazem
      TABLES
        l_zwm003                = ti_zwm003
        return_msg              = return_msg
      EXCEPTIONS
        tab_zwm003_not_filled   = 1
        invalid_parameter       = 2
        tab_l_zwm003_filled     = 3
        tab_l_zwm003_not_filled = 4
        no_warehouse            = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
      RAISE error_in_manage_parking.
    ELSE.

** Apagar as entradas em que o camião já está na porta
      DELETE ti_zwm003 WHERE estado = 'P'.


** Se existir fila de espera
      IF NOT ti_zwm003[] IS INITIAL.
        CLEAR ti_zwm003.
        REFRESH ti_zwm003.

**Prenchimento da tabela para dar entrada na fila de espera
        ti_zwm003-armazem = armazem.
        ti_zwm003-matricula = matricula.
        ti_zwm003-num_entrada = num_entrada.
        IF NOT observacoes IS INITIAL.
          ti_zwm003-observacoes = observacoes.
        ELSE.
          CLEAR ti_zwm003-observacoes.
        ENDIF.
        ti_zwm003-operacao = 'DESCARGA'.
        ti_zwm003-hora_entrada = sy-uzeit.
        ti_zwm003-data = sy-datum.
        ti_zwm003-estado = 'E'.
        APPEND ti_zwm003.

        CALL FUNCTION 'ZWM_MANAGE_PARKING'
          EXPORTING
            operacao                = '2'
            armazem                 = armazem
          TABLES
            l_zwm003                = ti_zwm003
            return_msg              = return_msg
          EXCEPTIONS
            tab_zwm003_not_filled   = 1
            invalid_parameter       = 2
            tab_l_zwm003_filled     = 3
            tab_l_zwm003_not_filled = 4
            no_warehouse            = 5
            OTHERS                  = 6.
        IF sy-subrc <> 0.
          RAISE error_in_manage_parking.
        ENDIF.

** Não existe fila de espera - verificar se existem portas de descarga
** livres
      ELSEIF ti_zwm003[] IS INITIAL.
*        BREAK-POINT.
        CALL FUNCTION 'ZWM_MANAGE_DOORS'
          EXPORTING
            operacao                = '1'
            armazem                 = armazem
          TABLES
            l_zwm002                = ti_zwm002
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
        ELSE.
          CLEAR : existe_porta.
** Verificar se existe pelo menos uma porta livre para DESCARGA
          LOOP AT ti_zwm002 WHERE bloqueio = ' '.

            SELECT SINGLE * FROM zwm007
            WHERE armazem = armazem   AND
                    porta = ti_zwm002-porta AND
                    tipo <> 'C'.
            IF sy-subrc = 0.
              existe_porta = 'X'.
              EXIT.
            ENDIF.
          ENDLOOP.

** Se existir porta - algoritmo de escolha de porta
** Se não existir porta - inserção na fila de espera

          IF existe_porta IS INITIAL.
**Prenchimento da tabela para dar entrada na fila de espera
            ti_zwm003-armazem = armazem.
            ti_zwm003-matricula = matricula.
            ti_zwm003-num_entrada = num_entrada.
            IF NOT observacoes IS INITIAL.
              ti_zwm003-observacoes = observacoes.
            ELSE.
              CLEAR ti_zwm003-observacoes.
            ENDIF.
            CLEAR ti_zwm003-porta.
            ti_zwm003-operacao = 'CARGA'.
            ti_zwm003-hora_entrada = sy-uzeit.
            ti_zwm003-data = sy-datum.
            ti_zwm003-estado = 'E'.
            APPEND ti_zwm003.

** Registo da entrada na fila de espera na BD
            CALL FUNCTION 'ZWM_MANAGE_PARKING'
              EXPORTING
                operacao                = '2'
                armazem                 = armazem
              TABLES
                l_zwm003                = ti_zwm003
                return_msg              = return_msg
              EXCEPTIONS
                tab_zwm003_not_filled   = 1
                invalid_parameter       = 2
                tab_l_zwm003_filled     = 3
                tab_l_zwm003_not_filled = 4
                no_warehouse            = 5
                OTHERS                  = 6.
            IF sy-subrc <> 0.
              RAISE error_in_manage_parking.
            ENDIF.

          ELSEIF NOT existe_porta IS INITIAL.

** Retorna a porta atribuida pelo algoritmo
            CALL FUNCTION 'ZWM_GET_DOOR'
              EXPORTING
                armazem               = armazem
                tipo_camiao           = tipo_camiao
                matricula             = matricula
                num_entrada           = num_entrada
              IMPORTING
                porta                 = porta
              TABLES
                return_msg            = return_msg
              EXCEPTIONS
                no_warehouse          = 1
                wrong_tipo_camiao     = 2
                error_in_manage_doors = 3
                error_in_doors        = 4
                OTHERS                = 5.
            IF sy-subrc <> 0.
              RAISE error_in_get_door.
            ELSE.
              IF porta IS INITIAL.
**Prenchimento da tabela para dar entrada na fila de espera
                ti_zwm003-armazem = armazem.
                ti_zwm003-matricula = matricula.
                ti_zwm003-num_entrada = num_entrada.
                IF NOT observacoes IS INITIAL.
                  ti_zwm003-observacoes = observacoes.
                ELSE.
                  CLEAR ti_zwm003-observacoes.
                ENDIF.
                CLEAR ti_zwm003-porta.
                ti_zwm003-operacao = 'DESCARGA'.
                ti_zwm003-hora_entrada = sy-uzeit.
                ti_zwm003-data = sy-datum.
                ti_zwm003-estado = 'E'.
                APPEND ti_zwm003.

** Registo da entrada na fila de espera na BD
                CALL FUNCTION 'ZWM_MANAGE_PARKING'
                  EXPORTING
                    operacao                = '2'
                    armazem                 = armazem
                  TABLES
                    l_zwm003                = ti_zwm003
                    return_msg              = return_msg
                  EXCEPTIONS
                    tab_zwm003_not_filled   = 1
                    invalid_parameter       = 2
                    tab_l_zwm003_filled     = 3
                    tab_l_zwm003_not_filled = 4
                    no_warehouse            = 5
                    OTHERS                  = 6.
                IF sy-subrc <> 0.
                  RAISE error_in_manage_parking.
                ENDIF.

              ENDIF.

            ENDIF.


          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.


** Camião para carga directa
*    BREAK ROFFD.
  ELSEIF tipo_camiao = 'E'.


** Verificar se existem camiões em PARKING
    CALL FUNCTION 'ZWM_MANAGE_PARKING'
      EXPORTING
        operacao                = '1'
        armazem                 = armazem
      TABLES
        l_zwm003                = ti_zwm003
        return_msg              = return_msg
      EXCEPTIONS
        tab_zwm003_not_filled   = 1
        invalid_parameter       = 2
        tab_l_zwm003_filled     = 3
        tab_l_zwm003_not_filled = 4
        no_warehouse            = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
      RAISE error_in_manage_parking.
    ELSE.

** Apagar as entradas em que o camião já está na porta
      DELETE ti_zwm003 WHERE estado = 'P'.

** Se existir fila de espera - acrescenta-se mais este camião
      IF NOT ti_zwm003[] IS INITIAL.
        CLEAR ti_zwm003.
        REFRESH ti_zwm003.

**Prenchimento da tabela para dar entrada na fila de espera
        ti_zwm003-armazem = armazem.
        ti_zwm003-matricula = matricula.
        ti_zwm003-num_entrada = num_entrada.
        IF NOT observacoes IS INITIAL.
          ti_zwm003-observacoes = observacoes.
        ELSE.
          CLEAR ti_zwm003-observacoes.
        ENDIF.
        CLEAR ti_zwm003-porta.
        ti_zwm003-operacao = 'CARGA'.
        ti_zwm003-hora_entrada = sy-uzeit.
        ti_zwm003-data = sy-datum.
        ti_zwm003-estado = 'E'.
        APPEND ti_zwm003.

** Realizar entrada na BD
        CALL FUNCTION 'ZWM_MANAGE_PARKING'
          EXPORTING
            operacao                = '2'
            armazem                 = armazem
          TABLES
            l_zwm003                = ti_zwm003
            return_msg              = return_msg
          EXCEPTIONS
            tab_zwm003_not_filled   = 1
            invalid_parameter       = 2
            tab_l_zwm003_filled     = 3
            tab_l_zwm003_not_filled = 4
            no_warehouse            = 5
            OTHERS                  = 6.
        IF sy-subrc <> 0.
          RAISE error_in_manage_parking.
        ENDIF.

** Não existe fila de espera - verificar se existem portas de carga
** livres
      ELSEIF ti_zwm003[] IS INITIAL.


        CALL FUNCTION 'ZWM_MANAGE_DOORS'
          EXPORTING
            operacao                = '1'
            armazem                 = armazem
          TABLES
            l_zwm002                = ti_zwm002
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
        ELSE.
          CLEAR : existe_porta.
** Verificar se existe pelo menos uma porta livre para CARGA
          LOOP AT ti_zwm002 WHERE bloqueio = ' '.

            SELECT SINGLE * FROM zwm007
            WHERE armazem = armazem   AND
                    porta = ti_zwm002-porta AND
                    tipo <> 'D'.
            IF sy-subrc = 0.
              existe_porta = 'X'.
              EXIT.
            ENDIF.
          ENDLOOP.

** Se existir porta - algoritmo de escolha de porta
** Se não existir porta - inserção na fila de espera

          IF existe_porta IS INITIAL.

**Prenchimento da tabela para dar entrada na fila de espera
            ti_zwm003-armazem = armazem.
            ti_zwm003-matricula = matricula.
            ti_zwm003-num_entrada = num_entrada.
            IF NOT observacoes IS INITIAL.
              ti_zwm003-observacoes = observacoes.
            ELSE.
              CLEAR ti_zwm003-observacoes.
            ENDIF.
            CLEAR ti_zwm003-porta.
            ti_zwm003-operacao = 'CARGA'.
            ti_zwm003-hora_entrada = sy-uzeit.
            ti_zwm003-data = sy-datum.
            ti_zwm003-estado = 'E'.
            APPEND ti_zwm003.

** Registo da entrada na fila de espera na BD
            CALL FUNCTION 'ZWM_MANAGE_PARKING'
              EXPORTING
                operacao                = '2'
                armazem                 = armazem
              TABLES
                l_zwm003                = ti_zwm003
                return_msg              = return_msg
              EXCEPTIONS
                tab_zwm003_not_filled   = 1
                invalid_parameter       = 2
                tab_l_zwm003_filled     = 3
                tab_l_zwm003_not_filled = 4
                no_warehouse            = 5
                OTHERS                  = 6.
            IF sy-subrc <> 0.
              RAISE error_in_manage_parking.
            ENDIF.

          ELSEIF NOT existe_porta IS INITIAL.

** Retorna a porta atribuida pelo algoritmo
            CALL FUNCTION 'ZWM_GET_DOOR'
              EXPORTING
                armazem               = armazem
                tipo_camiao           = tipo_camiao
                matricula             = matricula
                num_entrada           = num_entrada
              IMPORTING
                porta                 = porta
              TABLES
                return_msg            = return_msg
              EXCEPTIONS
                no_warehouse          = 1
                wrong_tipo_camiao     = 2
                error_in_manage_doors = 3
                error_in_doors        = 4
                OTHERS                = 5.
            IF sy-subrc <> 0.
              RAISE error_in_get_door.
            ELSEIF porta IS INITIAL.
**Prenchimento da tabela para dar entrada na fila de espera
              ti_zwm003-armazem = armazem.
              ti_zwm003-matricula = matricula.
              ti_zwm003-num_entrada = num_entrada.
              IF NOT observacoes IS INITIAL.
                ti_zwm003-observacoes = observacoes.
              ELSE.
                CLEAR ti_zwm003-observacoes.
              ENDIF.
              CLEAR ti_zwm003-porta.
              ti_zwm003-operacao = 'CARGA'.
              ti_zwm003-hora_entrada = sy-uzeit.
              ti_zwm003-data = sy-datum.
              ti_zwm003-estado = 'E'.
              APPEND ti_zwm003.

** Registo da entrada na fila de espera na BD
              CALL FUNCTION 'ZWM_MANAGE_PARKING'
                EXPORTING
                  operacao                = '2'
                  armazem                 = armazem
                TABLES
                  l_zwm003                = ti_zwm003
                  return_msg              = return_msg
                EXCEPTIONS
                  tab_zwm003_not_filled   = 1
                  invalid_parameter       = 2
                  tab_l_zwm003_filled     = 3
                  tab_l_zwm003_not_filled = 4
                  no_warehouse            = 5
                  OTHERS                  = 6.
              IF sy-subrc <> 0.
                RAISE error_in_manage_parking.
              ENDIF.
            ENDIF.


          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.



** Camião para carga através de pulmão
  ELSEIF tipo_camiao = 'P'.

** Verificar se a carga para este camião já está efectuada
** Se sim - assignar porta
** Se não - fila de espera

** Descobrir transporte associado ao camião
    SELECT SINGLE * FROM zwm006
                    WHERE armazem = armazem AND
                          num_entrada = num_entrada.

    IF sy-subrc = 0.
** Se não existir transporte ... fila de espera
      IF zwm006-n_transporte IS INITIAL.

        ti_zwm003-armazem = armazem.
        ti_zwm003-matricula = matricula.
        ti_zwm003-num_entrada = num_entrada.
        IF NOT observacoes IS INITIAL.
          ti_zwm003-observacoes = observacoes.
        ELSE.
          CLEAR ti_zwm003-observacoes.
        ENDIF.
        CLEAR ti_zwm003-porta.
        ti_zwm003-operacao = 'CARGA'.
        ti_zwm003-hora_entrada = sy-uzeit.
        ti_zwm003-data = sy-datum.
        ti_zwm003-estado = 'F'.
        APPEND ti_zwm003.

** Registo da entrada na fila de espera na BD
        CALL FUNCTION 'ZWM_MANAGE_PARKING'
          EXPORTING
            operacao                = '2'
            armazem                 = armazem
          TABLES
            l_zwm003                = ti_zwm003
            return_msg              = return_msg
          EXCEPTIONS
            tab_zwm003_not_filled   = 1
            invalid_parameter       = 2
            tab_l_zwm003_filled     = 3
            tab_l_zwm003_not_filled = 4
            no_warehouse            = 5
            OTHERS                  = 6.
        IF sy-subrc <> 0.
          RAISE error_in_manage_parking.
        ENDIF.

      ELSEIF NOT zwm006-n_transporte IS INITIAL.
** Verificar se a carga está finalizada


** Descobrir remessas associadas ao transporte

        SELECT SINGLE * FROM vttp WHERE tknum = zwm006-n_transporte.

** Verificar se a carga está completa
        IF sy-subrc = 0.

          SELECT * FROM vbss WHERE vbeln = vttp-vbeln.

            SELECT SINGLE *
                FROM vbsk
                    WHERE sammg = vbss-sammg AND
                          smart = 'W'.
            IF sy-subrc = 0.
              EXIT.
            ELSE.
              CLEAR vbss.
            ENDIF.

          ENDSELECT.
          CLEAR zwm028.
          SELECT SINGLE *
            FROM zwm028
                WHERE lgnum = armazem AND
                      refnr = vbss-sammg.

          IF zwm028-total_paletes <> zwm028-paletes_pulmao.
            incompleta = 'X'.
          ENDIF.

        ENDIF.

** Quer dizer q está incompleta ... FILA de ESPERA
        IF NOT incompleta IS INITIAL.


          ti_zwm003-armazem = armazem.
          ti_zwm003-matricula = matricula.
          ti_zwm003-num_entrada = num_entrada.
          IF NOT observacoes IS INITIAL.
            ti_zwm003-observacoes = observacoes.
          ELSE.
            CLEAR ti_zwm003-observacoes.
          ENDIF.
          CLEAR ti_zwm003-porta.
          ti_zwm003-operacao = 'CARGA'.
          ti_zwm003-hora_entrada = sy-uzeit.
          ti_zwm003-data = sy-datum.
          ti_zwm003-estado = 'F'.
          APPEND ti_zwm003.

** Registo da entrada na fila de espera na BD
          CALL FUNCTION 'ZWM_MANAGE_PARKING'
            EXPORTING
              operacao                = '2'
              armazem                 = armazem
            TABLES
              l_zwm003                = ti_zwm003
              return_msg              = return_msg
            EXCEPTIONS
              tab_zwm003_not_filled   = 1
              invalid_parameter       = 2
              tab_l_zwm003_filled     = 3
              tab_l_zwm003_not_filled = 4
              no_warehouse            = 5
              OTHERS                  = 6.
          IF sy-subrc <> 0.
            RAISE error_in_manage_parking.
          ENDIF.

** Carga Completa ... atribuição de porta
        ELSE.

          CALL FUNCTION 'ZWM_GET_DOOR'
            EXPORTING
              armazem                  = armazem
              tipo_camiao              = tipo_camiao
              matricula                = matricula
              num_entrada              = num_entrada
            IMPORTING
              porta                    = porta
            TABLES
              return_msg               = return_msg
*              L_ZWM002                 = L_ZWM002
            EXCEPTIONS
              no_warehouse             = 1
              wrong_tipo_camiao        = 2
              error_in_manage_doors    = 3
              error_in_doors           = 4
              no_door_to_direct_charge = 5
              OTHERS                   = 6.
          IF sy-subrc <> 0.
** ERRO
            RAISE error_in_get_door.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ELSE.
    RAISE wrong_tipo_camiao.
  ENDIF.


ENDFUNCTION.
