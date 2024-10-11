FUNCTION zwm_cross_docking.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LGNUM) TYPE  LGNUM
*"     REFERENCE(MATERIAL) TYPE  MATNR
*"     REFERENCE(LOTE) TYPE  CHARG_D
*"     REFERENCE(QUANTIDADE) TYPE  LTAP_VSOLA
*"     REFERENCE(UNI) TYPE  MEINS
*"     REFERENCE(TIPO_PALETE) TYPE  LVS_LETYP
*"     REFERENCE(SU) TYPE  LENUM
*"     REFERENCE(SU2) TYPE  LENUM OPTIONAL
*"     REFERENCE(ST_ORIGEM) TYPE  LGTYP
*"     REFERENCE(BIN_ORIGEM) TYPE  LGPLA
*"     REFERENCE(EQUIPAMENTO) TYPE  CHAR20 OPTIONAL
*"  EXPORTING
*"     REFERENCE(TO) TYPE  TANUM
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA : BEGIN OF l_ltap OCCURS 0.
          INCLUDE STRUCTURE ltap.
  DATA : END OF l_ltap.

  DATA : BEGIN OF c_ltap OCCURS 0.
          INCLUDE STRUCTURE ltap.
  DATA : END OF c_ltap.

  DATA : BEGIN OF l_zwm011 OCCURS 0.
          INCLUDE STRUCTURE zwm011.
  DATA : END OF l_zwm011.

  DATA: l_ltap_cancl LIKE ltap_cancl OCCURS 0 WITH HEADER LINE,
        return_msg   LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
        sscc         LIKE zwm_sscc   OCCURS 0 WITH HEADER LINE.

  DATA: wait_time(20),
        lock_key(50),
        queue_saida_tri LIKE zwm001-valor,
        queue_descarga  LIKE zwm001-valor,
        queue_fabrica1  LIKE zwm001-valor,
        queue_saida_dri LIKE zwm001-valor,
        unidade         LIKE mara-meins,
        lock(1),
        qtd_pal         LIKE ltap-vsolm,
        save_index      LIKE sy-tabix.

  DATA lv_deactivate TYPE flag.

  RANGES: equipamento_queue FOR ltak-queue.
**   Verificar se existe alguma To para algum fornecimento pendente de
**   preparação

  DATA: lv_2step  TYPE flag,
        ls_t311   TYPE t311,
        lt_zwm028 TYPE TABLE OF zwm028.

  CLEAR : queue_saida_tri, queue_descarga,
          queue_saida_dri, queue_fabrica1.

  CLEAR l_ltap.
  REFRESH l_ltap.

** valida se dave fazer cross dock
***********************************************************************
  CLEAR: lv_deactivate.

  SELECT SINGLE valor FROM zwm001
                      INTO lv_deactivate
                      WHERE armazem   = lgnum AND
                            processo  = 'ZWM_CROSS_DOCKING' AND
                            parametro = 'DESACTIVAR'.

  CHECK lv_deactivate <> abap_true.

** Carregar para memória as tabelas de parametrização

  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs = lgnum.

* Tempo de espera no lock
  PERFORM get_parameter
          USING lgnum
                'ATRIBUICAO_TO'
                'WAIT_TIME'
                wait_time.

*** Fila de Descargas
*  PERFORM get_parameter
*          USING lgnum
*                'GESTAO_FILAS'
*                'FILA_DESCARGA'
*                queue_descarga.
*
*** Fila da fábrica 1
*  PERFORM get_parameter
*          USING lgnum
*                'GESTAO_FILAS'
*                'FILA_FABRICA1'
*                queue_fabrica1.

  CLEAR equipamento_queue.
** Fila Saida Drive in
  PERFORM get_parameter
          USING lgnum
                'GESTAO_FILAS'
                'FILA_SAIDA_DRI'
                queue_saida_dri.

  equipamento_queue-sign = 'I'.
  equipamento_queue-option = 'EQ'.
  equipamento_queue-low = queue_saida_dri.
  APPEND equipamento_queue.
  CLEAR equipamento_queue.

** Fila Saida trilateral
  PERFORM get_parameter
          USING lgnum
                'GESTAO_FILAS'
                'FILA_SAIDA_TRI'
                queue_saida_tri.

  equipamento_queue-sign = 'I'.
  equipamento_queue-option = 'EQ'.
  equipamento_queue-low = queue_saida_tri.
  APPEND equipamento_queue.
  CLEAR equipamento_queue.

*****************REMONTADAS*************************

  CLEAR lock.

  DO.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'CONVENCIONAL'
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      lock = 'X'.
      EXIT.
    ELSE.
      WAIT UP TO wait_time SECONDS.
    ENDIF.
  ENDDO.

  CHECK NOT lock IS INITIAL.

  CLEAR lock.
  DO.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'TRILATERAL'
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      lock = 'X'.
      EXIT.
    ELSE.
      WAIT UP TO wait_time SECONDS.
    ENDIF.
  ENDDO.

  CHECK NOT lock IS INITIAL.

  CLEAR lock.
  DO.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'TRILATERALE'
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      lock = 'X'.
      EXIT.
    ELSE.
      WAIT UP TO wait_time SECONDS.
    ENDIF.
  ENDDO.

  CHECK NOT lock IS INITIAL.

  SELECT * FROM zwm011 INTO TABLE l_zwm011
              WHERE armazem = lgnum.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE l_ltap BYPASSING BUFFER
                  FROM ( ltak AS k INNER JOIN ltap AS p
                  ON  k~lgnum = p~lgnum AND
                      k~tanum = p~tanum )
                      INNER JOIN zwm028 AS z
                      ON k~lgnum = z~lgnum AND
                         k~refnr = z~refnr
                  WHERE k~lgnum = lgnum AND
                        k~kquit = ' ' AND
                        k~queue IN equipamento_queue AND
                        ( k~bwlvs = '601' OR k~bwlvs = '850' ) AND
                        p~matnr = material AND
*                        p~vsolm = quantidade AND
*                        p~meins = uni AND
                        p~letyp = tipo_palete AND
                        p~vorga <> 'ST' AND
                        p~pquit = ' ' AND
                        p~pvqui = ' ' AND
                        z~zlock > '2' AND
                        z~remessa = ' '.

**   Se existir uma To, estornar a mesma e criar uma nova com origem
**   ST_ORIGEM/BIN_ORIGEM e movimento 601

  DELETE l_ltap WHERE NOT vista IS INITIAL.

  IF l_ltap[] IS INITIAL.

    CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'TRILATERALE'
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'TRILATERAL'
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'CONVENCIONAL'
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    CLEAR to.
  ELSE.
*** Verificar se alguma das TO está atribuida a outro user
    SORT l_ltap BY tanum tapos.
    SORT l_zwm011 BY to_number to_item.

    LOOP AT l_ltap.
      READ TABLE l_zwm011 WITH KEY to_number = l_ltap-tanum
                                   to_item = l_ltap-tapos
                                   user_name = sy-uname.
      IF sy-subrc <> 0.
        READ TABLE l_zwm011 WITH KEY to_number = l_ltap-tanum
                                     to_item = l_ltap-tapos.
        IF sy-subrc = 0.
          DELETE l_ltap WHERE tanum = l_ltap-tanum.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF l_ltap[] IS INITIAL.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'TRILATERALE'
          _scope         = '1'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'TRILATERAL'
          _scope         = '1'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'CONVENCIONAL'
          _scope         = '1'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      CLEAR to.
    ELSE.

**  Eliminar das to´s possiveis que pertencem a remessas bloqueadas
**  e que a carga é preparada para o PLT.

      CLEAR save_index.
      LOOP AT l_ltap.
        save_index = sy-tabix.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 27.06.2012 16:13:04
*  Motivo: Valida Picking em 2 Passos
*--------------------------------------------------------------------*
        CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
          EXPORTING
            i_lgnum = l_ltap-lgnum
            i_tanum = l_ltap-tanum
          IMPORTING
            e_2step = lv_2step
            es_t311 = ls_t311
          EXCEPTIONS
            error   = 1
            OTHERS  = 2.

        CLEAR zwm028.

        IF lv_2step EQ abap_false.
          SELECT SINGLE *
              FROM zwm028
                  WHERE lgnum = lgnum
                    AND remessa = l_ltap-vbeln.
        ELSE.
          SELECT SINGLE *
              FROM zwm028
                  WHERE lgnum = lgnum
                    AND refnr = ls_t311-refnr AND
                        remessa <> ''.
        ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

        IF NOT zwm028 IS INITIAL.
          IF ( zwm028-st_pul = 'PLT' OR zwm028-st_dck = 'DCK' ) AND
            zwm028-zlock <= 2.
            DELETE l_ltap INDEX save_index.
            CONTINUE.
          ENDIF.
        ELSE.
          CLEAR zwm040.
          SELECT SINGLE *
              FROM zwm040
                  WHERE lgnum = lgnum
                    AND remessa = l_ltap-vbeln.
          IF sy-subrc = 0.
            SELECT SINGLE *
                FROM zwm028
                    WHERE lgnum = lgnum
                      AND remessa = zwm040-id_servisan.
            IF ( zwm028-st_pul = 'PLT' OR zwm028-st_dck = 'DCK' ) AND
                zwm028-zlock <= 2.
              DELETE l_ltap INDEX save_index.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

      IF l_ltap[] IS INITIAL.

        CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'TRILATERALE'
            _scope         = '1'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'TRILATERAL'
            _scope         = '1'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'CONVENCIONAL'
            _scope         = '1'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        CLEAR to.
        EXIT.
      ENDIF.

      SORT l_ltap BY tanum.
      LOOP AT l_ltap.
        save_index = sy-tabix.

        IF l_ltap-vltyp = 'TRI'.
          IF l_ltap-vsolm <> quantidade.
            DELETE l_ltap INDEX save_index.
            CONTINUE.
          ENDIF.
        ENDIF.

        IF l_ltap-vltyp = 'DRI' OR l_ltap-vltyp = 'BLK'.
          CLEAR:  qtd_pal, marm.
          SELECT SINGLE *
              FROM marm
                  WHERE matnr = l_ltap-matnr
                    AND meinh = 'PAL'.
          qtd_pal = marm-umrez / marm-umren.
          IF quantidade <> qtd_pal.
            DELETE l_ltap INDEX save_index.
            CONTINUE.
          ENDIF.

          IF z_wm_cl_management=>is_remontada( i_lgnum = lgnum i_letyp = tipo_palete ) eq abap_true.
            qtd_pal = qtd_pal * 2.
          ENDIF.

          IF l_ltap-vsolm <> qtd_pal.
            DELETE l_ltap INDEX save_index.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF l_ltap[] IS INITIAL.

        CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'TRILATERALE'
            _scope         = '1'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'TRILATERAL'
            _scope         = '1'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'CONVENCIONAL'
            _scope         = '1'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        CLEAR to.
        EXIT.
      ENDIF.

      READ TABLE l_ltap INDEX 1.
      IF l_ltap-vltyp = 'DRI' OR l_ltap-vltyp = 'BLK'.
        MOVE-CORRESPONDING l_ltap TO c_ltap.
        APPEND c_ltap.
      ELSE.
        IF z_wm_cl_management=>is_remontada( i_lgnum = lgnum i_letyp = tipo_palete ) eq abap_true.

          CLEAR zwm020.
          SELECT SINGLE *
              FROM zwm020
                  WHERE armazem = lgnum AND
                        ( p1 = l_ltap-vlenr OR
                          p2 = l_ltap-vlenr ).

          SORT l_ltap BY vlenr.
          READ TABLE l_ltap WITH KEY vlenr = zwm020-p1.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING l_ltap TO c_ltap.
            APPEND c_ltap.
          ENDIF.
          READ TABLE l_ltap WITH KEY vlenr = zwm020-p2.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING l_ltap TO c_ltap.
            APPEND c_ltap.
          ENDIF.

          DATA linhas TYPE i.
          CLEAR linhas.

          DESCRIBE TABLE c_ltap LINES linhas.

          IF z_wm_cl_management=>is_remontada( i_lgnum = lgnum i_letyp = tipo_palete ) eq abap_true.
            IF linhas <> 2.
              CLEAR to.
              EXIT.
            ENDIF.
          ENDIF.

        ELSE.
          MOVE-CORRESPONDING l_ltap TO c_ltap.
          APPEND c_ltap.
        ENDIF.
      ENDIF.

      DATA flag_cancel(1).

      CLEAR flag_cancel.

      LOOP AT c_ltap.
        DO 30 TIMES.
          CLEAR flag_cancel.
          CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
            EXPORTING
              warehouse     = c_ltap-lgnum
              tanum         = c_ltap-tanum
              tapos         = c_ltap-tapos
            TABLES
              return_msg    = return_msg
            EXCEPTIONS
              error_message = 1
              OTHERS        = 2.
          IF sy-subrc <> 0.
            flag_cancel = 'X'.
            CLEAR to.
            WAIT UP TO 1 SECONDS.
          ELSE.
            CLEAR flag_cancel.
            EXIT.
          ENDIF.
        ENDDO.
        IF NOT flag_cancel IS INITIAL.
          CLEAR to.
          EXIT.
        ENDIF.
      ENDLOOP.

      CHECK flag_cancel IS INITIAL.
************************
* Criar to   PRO -> CRD

      LOOP AT c_ltap.

*       Caso o lote venha preenchido
        c_ltap-charg = lote.

        IF c_ltap-vltyp = 'TRI'.
          IF sscc[] IS INITIAL.
            sscc-sscc = su.
          ELSE.
            sscc-sscc = su2.
          ENDIF.

          sscc-tipo_su       = c_ltap-letyp.
          sscc-material      = c_ltap-matnr.
          sscc-lote_producao = c_ltap-charg.
          sscc-quantidade    = c_ltap-vsolm.
          sscc-uni           = c_ltap-meins.
          APPEND sscc.
          CLEAR sscc.

        ELSEIF c_ltap-vltyp = 'DRI' OR c_ltap-vltyp = 'BLK'.

          IF z_wm_cl_management=>is_remontada( i_lgnum = lgnum i_letyp = tipo_palete ) eq abap_true.
            sscc-sscc          = su.
            sscc-tipo_su       = c_ltap-letyp.
            sscc-material      = c_ltap-matnr.
            sscc-lote_producao = c_ltap-charg.
            sscc-quantidade    = c_ltap-vsolm / 2.
            sscc-uni           = c_ltap-meins.
            APPEND sscc.
            CLEAR sscc.

            sscc-sscc          = su2.
            sscc-tipo_su       = c_ltap-letyp.
            sscc-material      = c_ltap-matnr.
            sscc-lote_producao = c_ltap-charg.
            sscc-quantidade    = c_ltap-vsolm / 2.
            sscc-uni           = c_ltap-meins.
            APPEND sscc.
            CLEAR sscc.

          ELSE.
            sscc-sscc          = su.
            sscc-tipo_su       = c_ltap-letyp.
            sscc-material      = c_ltap-matnr.
            sscc-lote_producao = c_ltap-charg.
            sscc-quantidade    = c_ltap-vsolm.
            sscc-uni           = c_ltap-meins.
            APPEND sscc.
            CLEAR sscc.
          ENDIF.

        ENDIF.
      ENDLOOP.

      DO 30 TIMES.
        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
          EXPORTING
            warehouse   = lgnum
            mov_type    = '905'
            st_type_o   = st_origem
            bin_origem  = bin_origem
            st_type_d   = 'CRD'
            bin_destino = bin_origem
            plant       = 'RENV'
            s_loc       = 'CD'
          IMPORTING
            to          = to
          TABLES
            return_msg  = return_msg
            sscc        = sscc
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.
        IF sy-subrc <> 0.
          CLEAR to.
          WAIT UP TO 1 SECONDS.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

      SELECT SINGLE * FROM ltak
          WHERE lgnum = l_ltap-lgnum AND
                tanum = l_ltap-tanum.

      READ TABLE c_ltap INDEX 1.

      DATA: qtd LIKE ltap-vsola,
            st_aux LIKE lagp-lgtyp.

      CLEAR: qtd, st_aux.

      IF c_ltap-vltyp = 'DRI' OR c_ltap-vltyp = 'BLK'.
        IF z_wm_cl_management=>is_remontada( i_lgnum = lgnum i_letyp = tipo_palete ) eq abap_true.
          qtd = c_ltap-vsola / 2.
        ELSE.
          qtd = c_ltap-vsola.
        ENDIF.
      ELSE.
        qtd = c_ltap-vsola.
      ENDIF.

      IF st_origem = 'PRO'.
        st_aux = 'CRD'.
      ELSE.
        st_aux = 'PUL'.
      ENDIF.

*     Caso o lote venha preenchido
      c_ltap-charg = lote.

      DO 100 TIMES.
        CALL FUNCTION 'ZWM_TO_CREATE_OUT'
          EXPORTING
            warehouse     = lgnum
            refnr         = ltak-refnr
            vbeln         = ltak-vbeln
            posnr         = c_ltap-posnr
            vsola         = qtd
            meins         = uni
            vltyp         = st_aux
            vlpla         = bin_origem
            su            = su
            su2           = su2
            werks         = c_ltap-werks
            lgort         = c_ltap-lgort
            matnr         = c_ltap-matnr
            charg         = c_ltap-charg
            benum         = ltak-benum
          IMPORTING
            to            = to
          TABLES
            return_msg    = return_msg
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.

*        CALL FUNCTION 'ZWM_CREATE_TO_DELIVERY'
*          EXPORTING
*            warehouse     = lgnum
*            refnr         = ltak-refnr
*            vbeln         = ltak-vbeln
*            posnr         = c_ltap-posnr
*            vsola         = qtd
*            vltyp         = st_aux
*            vlpla         = bin_origem
*            su            = su
*            su2           = su2
*          IMPORTING
*            to            = to
*          TABLES
*            return_msg    = return_msg
*          EXCEPTIONS
*            error_message = 1
*            OTHERS        = 2.

        IF sy-subrc <> 0.
          CLEAR to.
          WAIT UP TO 1 SECONDS.
        ELSE.
          EXIT.
        ENDIF.

      ENDDO.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'TRILATERALE'
          _scope         = '1'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'TRILATERAL'
          _scope         = '1'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'CONVENCIONAL'
          _scope         = '1'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

    ENDIF.
  ENDIF.

ENDFUNCTION.
