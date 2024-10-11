FUNCTION zwm_find_bin.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ST_TYPE) LIKE  LQUA-LGTYP
*"     REFERENCE(NUM_PALETES) LIKE  ZWM028-PALETES_PULMAO
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"     REFERENCE(REFNR) TYPE  LVS_REFNR
*"     REFERENCE(VIRTUAL) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(POSICAO1) LIKE  LQUA-LGPLA
*"     REFERENCE(POSICAO2) LIKE  LQUA-LGPLA
*"     REFERENCE(E_KOBER) TYPE  KOBER
*"  EXCEPTIONS
*"      NUM_PALETES_INITIAL
*"----------------------------------------------------------------------
  DATA: lt_pulmoes TYPE TABLE OF lagp-lgpla.

  DATA : tipo_dep_pul LIKE zwm001-valor,
         tipo_dep_dck LIKE zwm001-valor,
         l_zwm007     LIKE zwm007 OCCURS 0 WITH HEADER LINE,
         pulmao       LIKE lagp-lgpla,
         pulmao_aux   LIKE lagp-lgber,
         num_quantos  TYPE i.

  DATA: lv_num_pul TYPE i.

  DATA: lr_lgpla_v TYPE RANGE OF lgpla.

  DATA: ls_r_lgpla_v LIKE LINE OF lr_lgpla_v.

  DATA : BEGIN OF lagp_aux OCCURS 2,
           lgpla LIKE lagp-lgpla.
  DATA : END OF lagp_aux.

** Contem as portas de CARGA DIRECTA válidas caso existam
  DATA : BEGIN OF l_portas_carga OCCURS 0.
           INCLUDE STRUCTURE zwm002.
           DATA : tipo LIKE zwm007-tipo.
  DATA : END OF l_portas_carga.

  DATA t_lagp LIKE lagp OCCURS 0 WITH HEADER LINE.

  DATA num_pulmao TYPE i.

  DATA: lv_sort_asc TYPE flag.

  CLEAR : tipo_dep_pul,
          tipo_dep_dck,
          l_zwm007,
          pulmao,
          num_quantos,
          lagp_aux,
          l_portas_carga,
          posicao1,
          posicao2.

  REFRESH : l_zwm007,
            lagp_aux,
            l_portas_carga.

  IF num_paletes IS INITIAL.
    RAISE num_paletes_initial.
  ENDIF.

  PERFORM get_parameter USING armazem
                             'TROCA_ATRIBUICAO_PUL'
                             'ACTIVAR'
                              lv_sort_asc.


  lv_num_pul = 1. " Numero de Pulmoes  a Retornar

  ls_r_lgpla_v-low = '999-*'.
  ls_r_lgpla_v-option = 'CP'.
  ls_r_lgpla_v-sign = 'I'.
  APPEND ls_r_lgpla_v TO lr_lgpla_v.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 31.05.2012 12:20:35
*  Motivo: Determinação Para Meio Pulmão
*--------------------------------------------------------------------*
*  CALL FUNCTION 'ZWM_MPUL_FIND_BIN'
*    EXPORTING
*      i_lgnum       = armazem
*      i_lgtyp       = st_type
*      i_num_paletes = num_paletes
*      i_refnr       = refnr
*    IMPORTING
*      e_posicao     = posicao1
*      e_kober       = e_kober.
*
*  CHECK posicao1 IS INITIAL.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


** Parametrizações
  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs       = armazem
    TABLES
      ti_zwm001 = ti_zwm001.
** Tipo de depósito PUL
  PERFORM get_parameter USING armazem
                             'ENTRADA_ARMAZEM'
                             'ST_PUL'
                              tipo_dep_pul.
** Tipo de depósito DCK
  PERFORM get_parameter USING armazem
                             'ENTRADA_ARMAZEM'
                             'ST_DCK'
                              tipo_dep_dck.

  IF st_type = 'PUA'.

    SELECT * INTO TABLE t_lagp
        FROM lagp
            WHERE lgnum = armazem AND
                  lgtyp = st_type AND
                  brand = abap_false AND
                  anzqu = 0.

    DELETE t_lagp WHERE lgpla = 'EXP4_ENTRJ'.

    PERFORM filter_select_pul USING armazem st_type CHANGING t_lagp[].


    READ TABLE t_lagp
         INDEX 1.

    posicao1 = t_lagp-lgpla.
    e_kober  = t_lagp-kober.

    EXIT.
  ELSEIF tipo_dep_pul = st_type.
** Portas e pulmões associados ( carga ou ambos )
    SELECT * FROM zwm007 INTO TABLE l_zwm007
             WHERE armazem = armazem AND
                   tipo <> 'D'.

    IF armazem = '150'.

      CLEAR t_lagp.
      REFRESH t_lagp.

      IF virtual EQ abap_true.

        SELECT * INTO TABLE t_lagp
            FROM lagp
                WHERE lgnum = armazem AND
                      lgtyp = tipo_dep_pul AND
                      lgpla IN lr_lgpla_v AND
                      brand = ' '.
      ELSE.

        SELECT * INTO TABLE t_lagp
            FROM lagp
                WHERE lgnum = armazem AND
                      lgtyp = tipo_dep_pul AND
                      lgpla NOT IN lr_lgpla_v AND
                      brand = ' '.

      ENDIF.

      IF num_paletes <= 17.
        SORT t_lagp BY kober DESCENDING lgpla ASCENDING.
      ELSEIF num_paletes >= 18.
        SORT t_lagp BY kober ASCENDING lgpla ASCENDING.
      ENDIF.

      LOOP AT t_lagp.
        CLEAR: pulmao1.

        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = 'PUL'
            lgpla = t_lagp-lgpla
          IMPORTING
            bin   = pulmao1.

        CLEAR num_quantos.
        SELECT COUNT(*) INTO num_quantos
            FROM zwm013
                WHERE armazem = armazem AND
                      destino = pulmao1.
        IF sy-subrc = 0.
          DELETE t_lagp WHERE lgpla = t_lagp-lgpla.
        ENDIF.
      ENDLOOP.

      IF t_lagp[] IS INITIAL.
        EXIT.
      ENDIF.

      IF num_paletes <= 17.

        READ TABLE t_lagp INDEX 1.
        posicao1 = t_lagp-lgpla.
        lagp_aux-lgpla = t_lagp-lgpla.
        APPEND lagp_aux.
        EXIT.

      ELSEIF num_paletes >= 18.

        LOOP AT t_lagp.

          CLEAR : pulmao_aux.
          pulmao_aux = t_lagp-lgpla+4(3).

          LOOP AT t_lagp WHERE lgber = pulmao_aux.
            CLEAR lagp_aux.
            lagp_aux-lgpla = t_lagp-lgpla.
            APPEND lagp_aux.
          ENDLOOP.

          DESCRIBE TABLE lagp_aux.
          IF sy-tfill <> 2.
            CLEAR lagp_aux.
            REFRESH lagp_aux.
            DELETE t_lagp WHERE lgber = pulmao_aux.
          ELSE.
            EXIT.
          ENDIF.

        ENDLOOP.

        IF lagp_aux[] IS INITIAL.
          EXIT.
        ENDIF.

        READ TABLE lagp_aux INDEX 1.
        posicao1 = lagp_aux-lgpla.

        READ TABLE lagp_aux INDEX 2.
        posicao2 = lagp_aux-lgpla.

        EXIT.
      ENDIF.

    ENDIF.

    IF lv_sort_asc EQ abap_false.
      SORT l_zwm007 BY porta DESCENDING.
    ELSE.
      SORT l_zwm007 BY porta ASCENDING.
    ENDIF.

    IF num_paletes <= 17.
      LOOP AT l_zwm007.
        DO 24 TIMES VARYING pulmao FROM
                           l_zwm007-pulmao1 NEXT
                           l_zwm007-pulmao2.
          IF NOT pulmao IS INITIAL.
            SELECT SINGLE * FROM lagp
                     WHERE lgnum = armazem AND
                           lgtyp = tipo_dep_pul AND
                           lgpla = pulmao AND
                           brand = ' '.
            IF sy-subrc = 0.
              CLEAR: pulmao1, pulmao2.

              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = 'PUL'
                  lgpla = pulmao
                IMPORTING
                  bin   = pulmao1.

              CLEAR num_quantos.
              SELECT COUNT(*) INTO num_quantos
                  FROM zwm013
                      WHERE armazem = armazem AND
                            destino = pulmao1.
              IF sy-subrc <> 0.
                CLEAR num_quantos.
              ENDIF.
            ELSE.
              num_quantos = num_quantos + 1.
            ENDIF.

            IF NOT num_quantos IS INITIAL.
              CONTINUE.
            ELSE.
              lagp_aux-lgpla = lagp-lgpla.
              APPEND lagp_aux.
              EXIT.
            ENDIF.

*            ENDIF.
          ENDIF.
        ENDDO.
      ENDLOOP.
    ELSEIF num_paletes >= 18.
      lv_num_pul = 2.
      LOOP AT l_zwm007.
        DO 24 TIMES VARYING pulmao FROM
                           l_zwm007-pulmao1 NEXT
                           l_zwm007-pulmao2.
          IF NOT pulmao IS INITIAL.

            SELECT SINGLE * FROM lagp
                WHERE lgnum = armazem AND
                      lgtyp = tipo_dep_pul AND
                      lgpla = pulmao.
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.

            CLEAR : pulmao_aux.
            pulmao_aux = pulmao+4(3).

            CLEAR t_lagp.
            REFRESH t_lagp.

            SELECT * INTO CORRESPONDING FIELDS OF TABLE t_lagp
                FROM lagp
                    WHERE lgnum = armazem AND
                          lgtyp = tipo_dep_pul AND
                          lgber = pulmao_aux AND
                          brand = ' '.

            IF t_lagp[] IS INITIAL.
              CONTINUE.
            ENDIF.

            DESCRIBE TABLE t_lagp LINES num_pulmao.
            IF num_pulmao = 1.
              CONTINUE.
            ENDIF.

            READ TABLE t_lagp INDEX 1.
            CLEAR: pulmao1, pulmao2.

            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_lagp-lgtyp
                lgpla = t_lagp-lgpla
              IMPORTING
                bin   = pulmao1.

            READ TABLE t_lagp INDEX 2.
            CALL FUNCTION 'ZWM_CONCATENATE_BIN'
              EXPORTING
                lgtyp = t_lagp-lgtyp
                lgpla = t_lagp-lgpla
              IMPORTING
                bin   = pulmao2.

            SELECT COUNT(*) INTO num_quantos
                    FROM zwm013
                        WHERE armazem = armazem AND
                              ( destino = pulmao1 OR
                                destino = pulmao2 ).
            IF sy-subrc = 0.
              IF num_quantos <> 0.
                CONTINUE.
              ENDIF.
            ENDIF.

            READ TABLE t_lagp INDEX 1.
            posicao1 = t_lagp-lgpla.
            lagp_aux-lgpla = t_lagp-lgpla.
            APPEND lagp_aux.

            READ TABLE t_lagp INDEX 2.
            posicao2 = t_lagp-lgpla.
            lagp_aux-lgpla = t_lagp-lgpla.
            APPEND lagp_aux.
          ENDIF.
        ENDDO.
      ENDLOOP.
    ENDIF.
  ELSEIF st_type = 'PLT'.

* Portas e plataformas associados ( carga ou ambos )
    SELECT * FROM zwm007 INTO TABLE l_zwm007
        WHERE armazem = armazem AND
              tipo <> 'D'.
    SORT l_zwm007 BY porta DESCENDING.
    IF num_paletes <= 34.
      LOOP AT l_zwm007.
        DO 24 TIMES VARYING pulmao FROM
                                  l_zwm007-pulmao1 NEXT
                                  l_zwm007-pulmao2.
          IF NOT pulmao IS INITIAL.
            SELECT SINGLE * FROM lagp
                WHERE lgnum = armazem AND
                      lgtyp = 'PLT' AND
                      lgpla = pulmao AND
                      brand = ' '.
            IF sy-subrc = 0.
              CLEAR: pulmao1, pulmao2.

              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = 'PLT'
                  lgpla = pulmao
                IMPORTING
                  bin   = pulmao1.

              CLEAR num_quantos.
              SELECT COUNT(*) INTO num_quantos
                  FROM zwm013
                      WHERE armazem = armazem AND
                            destino = pulmao1.
              IF sy-subrc <> 0.
                CLEAR num_quantos.
              ENDIF.
            ELSE.
              num_quantos = num_quantos + 1.
            ENDIF.

            IF NOT num_quantos IS INITIAL.
              CONTINUE.
            ELSE.
              lagp_aux-lgpla = lagp-lgpla.
              APPEND lagp_aux.
              EXIT.
            ENDIF.

          ENDIF.
        ENDDO.
      ENDLOOP.
    ELSEIF num_paletes >= 35.
      lv_num_pul = 2. "Preciso de 2 Pulmoes

      LOOP AT l_zwm007.
        IF posicao1 IS INITIAL.
          DO 24 TIMES VARYING pulmao FROM
                                     l_zwm007-pulmao1 NEXT
                                     l_zwm007-pulmao2.
            IF NOT pulmao IS INITIAL.

              SELECT SINGLE * FROM lagp
                  WHERE lgnum = armazem AND
                        lgtyp = 'PLT' AND
                        lgpla = pulmao.
              IF sy-subrc <> 0.
                CONTINUE.
              ENDIF.

              CLEAR : pulmao_aux.
*              pulmao_aux = pulmao+4(3).
              pulmao_aux = pulmao(3).
              CLEAR t_lagp.
              REFRESH t_lagp.

              SELECT * INTO CORRESPONDING FIELDS OF TABLE t_lagp
                  FROM lagp
                      WHERE lgnum = armazem AND
                            lgtyp = 'PLT' AND
                            lgber = pulmao_aux AND
                            brand = ' '.

              IF t_lagp[] IS INITIAL.
                CONTINUE.
              ENDIF.

              DESCRIBE TABLE t_lagp LINES num_pulmao.
              IF num_pulmao = 1.
                CONTINUE.
              ENDIF.

              READ TABLE t_lagp INDEX 1.
              CLEAR: pulmao1, pulmao2.

              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_lagp-lgtyp
                  lgpla = t_lagp-lgpla
                IMPORTING
                  bin   = pulmao1.

              READ TABLE t_lagp INDEX 2.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_lagp-lgtyp
                  lgpla = t_lagp-lgpla
                IMPORTING
                  bin   = pulmao2.

              SELECT COUNT(*) INTO num_quantos
                  FROM zwm013
                      WHERE armazem = armazem AND
                            ( destino = pulmao1 OR
                              destino = pulmao2 ).
              IF sy-subrc = 0.
                IF num_quantos <> 0.
                  CONTINUE.
                ENDIF.
              ENDIF.

              READ TABLE t_lagp INDEX 1.
              posicao1 = t_lagp-lgpla.
              lagp_aux-lgpla = t_lagp-lgpla.
              APPEND lagp_aux.

              READ TABLE t_lagp INDEX 2.
              posicao2 = t_lagp-lgpla.
              lagp_aux-lgpla = t_lagp-lgpla.
              APPEND lagp_aux.
            ENDIF.
          ENDDO.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ELSEIF st_type = 'PLM'.

* Portas e pulmões associados ( carga ou ambos )
    SELECT * FROM zwm007 INTO TABLE l_zwm007
        WHERE armazem = armazem AND
              tipo <> 'D'.
    SORT l_zwm007 BY porta DESCENDING.
    IF num_paletes <= 2.
      LOOP AT l_zwm007.
        DO 24 TIMES VARYING pulmao FROM
                                    l_zwm007-pulmao1 NEXT
                                    l_zwm007-pulmao2.
          IF NOT pulmao IS INITIAL.
            SELECT SINGLE * FROM lagp
                WHERE lgnum = armazem AND
                      lgtyp = 'PLM' AND
                      lgpla = pulmao AND
                      brand = ' '.
            IF sy-subrc = 0.
              CLEAR: pulmao1, pulmao2.

              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = 'PLM'
                  lgpla = pulmao
                IMPORTING
                  bin   = pulmao1.

              CLEAR num_quantos.
              SELECT COUNT(*) INTO num_quantos
                  FROM zwm013
                      WHERE armazem = armazem AND
                            destino = pulmao1.
              IF sy-subrc <> 0.
                CLEAR num_quantos.
              ENDIF.
            ELSE.
              num_quantos = num_quantos + 1.
            ENDIF.

            IF NOT num_quantos IS INITIAL.
              CONTINUE.
            ELSE.
              lagp_aux-lgpla = lagp-lgpla.
              APPEND lagp_aux.
              EXIT.
            ENDIF.

*            ENDIF.
          ENDIF.
        ENDDO.
      ENDLOOP.
    ELSEIF num_paletes >= 3.
      lv_num_pul = 2. "Preciso de 2 Pulmoes

      LOOP AT l_zwm007.
        IF posicao1 IS INITIAL.
          DO 24 TIMES VARYING pulmao FROM
                                      l_zwm007-pulmao1 NEXT
                                      l_zwm007-pulmao2.
            IF NOT pulmao IS INITIAL.

              SELECT SINGLE * FROM lagp
                  WHERE lgnum = armazem AND
                        lgtyp = 'PLM' AND
                        lgpla = pulmao.
              IF sy-subrc <> 0.
                CONTINUE.
              ENDIF.

              CLEAR : pulmao_aux.
              pulmao_aux = pulmao+4(3).

              CLEAR t_lagp.
              REFRESH t_lagp.

              SELECT * INTO CORRESPONDING FIELDS OF TABLE t_lagp
                  FROM lagp
                      WHERE lgnum = armazem AND
                            lgtyp = 'PLM' AND
                            lgber = pulmao_aux AND
                            brand = ' '.

              IF t_lagp[] IS INITIAL.
                CONTINUE.
              ENDIF.

              DESCRIBE TABLE t_lagp LINES num_pulmao.
              IF num_pulmao = 1.
                CONTINUE.
              ENDIF.

              READ TABLE t_lagp INDEX 1.
              CLEAR: pulmao1, pulmao2.

              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_lagp-lgtyp
                  lgpla = t_lagp-lgpla
                IMPORTING
                  bin   = pulmao1.

              READ TABLE t_lagp INDEX 2.
              CALL FUNCTION 'ZWM_CONCATENATE_BIN'
                EXPORTING
                  lgtyp = t_lagp-lgtyp
                  lgpla = t_lagp-lgpla
                IMPORTING
                  bin   = pulmao2.

              SELECT COUNT(*) INTO num_quantos
                  FROM zwm013
                      WHERE armazem = armazem AND
                           ( destino = pulmao1 OR
                             destino = pulmao2 ).
              IF sy-subrc = 0.
                IF num_quantos <> 0.
                  CONTINUE.
                ENDIF.
              ENDIF.

              READ TABLE t_lagp INDEX 1.
              posicao1 = t_lagp-lgpla.
              lagp_aux-lgpla = t_lagp-lgpla.
              APPEND lagp_aux.

              READ TABLE t_lagp INDEX 2.
              posicao2 = t_lagp-lgpla.
              lagp_aux-lgpla = t_lagp-lgpla.
              APPEND lagp_aux.
            ENDIF.
          ENDDO.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ELSEIF tipo_dep_dck = st_type.
** Descobrir uma porta livre de carga - ordenação é decrescente

    SELECT * INTO CORRESPONDING FIELDS OF TABLE
                                 l_portas_carga
             FROM zwm002 AS w INNER JOIN zwm007 AS m
             ON w~armazem = m~armazem AND
                w~porta = m~porta
             WHERE w~armazem = armazem AND
                   w~bloqueio = ' ' AND
                   w~estado = 'D' AND
                   m~tipo <> 'D'.
    IF sy-subrc = 0.
      SORT l_portas_carga BY porta DESCENDING.
      READ TABLE l_portas_carga INDEX 1.
      IF sy-subrc = 0.
        CONCATENATE '000-000-' l_portas_carga-porta+1(2)
        INTO lagp_aux-lgpla.
        APPEND lagp_aux.
      ENDIF.
    ELSE.
      CLEAR : lagp_aux.
      REFRESH : lagp_aux.
    ENDIF.

  ENDIF.

  CLEAR : posicao1,
          posicao2.

  PERFORM filter_select_pul USING armazem st_type CHANGING lagp_aux[].

** Actualização da BD - ocupar posições
  LOOP AT lagp_aux.
    IF sy-tabix = 1.
      posicao1 = lagp_aux-lgpla.
    ELSEIF sy-tabix = 2 AND lv_num_pul EQ 2.
      posicao2 = lagp_aux-lgpla.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
