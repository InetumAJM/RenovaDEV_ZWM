*----------------------------------------------------------------------*
***INCLUDE LZWMFR_FGF02 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  f_createto_move_su
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IO_BALLOG  text
*      <--CS_DBLOG   text
*      <--CS_LTAK    text
*      <--CT_LTAP    text
*      <--C_SUBRC    text
*----------------------------------------------------------------------*
FORM f_createto_move_su USING io_ballog TYPE REF TO zcl_ca_bal_log
                        CHANGING cs_dblog TYPE zwmfrt002
                                 cs_ltak  TYPE ltak_vb
                                 ct_ltap  TYPE tt_ltap_vb
                                 c_subrc  TYPE sysubrc.
  DATA lv_message TYPE bapiret2-message.                    "#EC NEEDED
  DATA lv_subrc   TYPE sysubrc.

  DATA lt_ltak TYPE STANDARD TABLE OF ltak_vb.

  DATA ls_ltap    TYPE ltap.
  DATA ls_ltap_vb TYPE ltap_vb.
  DATA ls_zwm020  TYPE zwm020.

  FIELD-SYMBOLS <fs_zwmmpt001>  LIKE LINE OF gt_zwmmpt001[].

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
    WITH TABLE KEY processo   = c_process_interface_abs
                   parametro  = c_param_mov_wm_interf
                   item       = 1.
  IF sy-subrc EQ 0.
    cs_dblog-bwlvs  = <fs_zwmmpt001>-valor.
  ELSE.
    MESSAGE e047 INTO lv_message.
    io_ballog->log_add_message( ).

    c_subrc = c_return_no_wm_movemnt.
    RETURN.
  ENDIF.

  CALL FUNCTION 'L_TO_CREATE_MOVE_SU'
    EXPORTING
      i_lenum               = cs_dblog-lenum
      i_bwlvs               = cs_dblog-bwlvs
      i_commit_work         = abap_false
    TABLES
      t_ltak                = lt_ltak[]
      t_ltap_vb             = ct_ltap[]
    EXCEPTIONS
      not_confirmed_to      = 1
      foreign_lock          = 2
      bwlvs_wrong           = 3
      betyp_wrong           = 4
      nltyp_wrong           = 5
      nlpla_wrong           = 6
      nltyp_missing         = 7
      nlpla_missing         = 8
      squit_forbidden       = 9
      lgber_wrong           = 10
      xfeld_wrong           = 11
      drukz_wrong           = 12
      ldest_wrong           = 13
      no_stock_on_su        = 14
      su_not_found          = 15
      update_without_commit = 16
      no_authority          = 17
      benum_required        = 18
      ltap_move_su_wrong    = 19
      lenum_wrong           = 20
      error_message         = 99
      OTHERS                = 21.
  IF sy-subrc NE 0.
    io_ballog->log_add_message( ).

    MESSAGE e030 WITH cs_dblog-lenum INTO lv_message.
    io_ballog->log_add_message( ).

    c_subrc = c_return_no_to_created.
    RETURN.
  ELSE.
    READ TABLE lt_ltak[] INTO cs_ltak INDEX 1.

    cs_dblog-tanum = cs_ltak-tanum.
    cs_dblog-pquit = abap_true.
  ENDIF.

** Valida Remontada
***********************************************************************
  DO 1 TIMES.
    SELECT SINGLE * FROM zwm020
                    INTO ls_zwm020
                    WHERE armazem = cs_dblog-lgnum AND
                          p1      = cs_dblog-lenum.

    CHECK NOT ls_zwm020-p2 IS INITIAL.

    CALL FUNCTION 'L_TO_CREATE_MOVE_SU'
      EXPORTING
        i_lenum               = ls_zwm020-p2
        i_bwlvs               = cs_dblog-bwlvs
        i_commit_work         = abap_false
      EXCEPTIONS
        not_confirmed_to      = 1
        foreign_lock          = 2
        bwlvs_wrong           = 3
        betyp_wrong           = 4
        nltyp_wrong           = 5
        nlpla_wrong           = 6
        nltyp_missing         = 7
        nlpla_missing         = 8
        squit_forbidden       = 9
        lgber_wrong           = 10
        xfeld_wrong           = 11
        drukz_wrong           = 12
        ldest_wrong           = 13
        no_stock_on_su        = 14
        su_not_found          = 15
        update_without_commit = 16
        no_authority          = 17
        benum_required        = 18
        ltap_move_su_wrong    = 19
        lenum_wrong           = 20
        error_message         = 99
        OTHERS                = 21.

    IF sy-subrc NE 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      io_ballog->log_add_message( ).

      MESSAGE e030 WITH ls_zwm020-p2 INTO lv_message.
      io_ballog->log_add_message( ).

      c_subrc = c_return_no_to_created.
      CLEAR: ct_ltap, cs_ltak.
      RETURN.
    ENDIF.
  ENDDO.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.

** Testa OT
***********************************************************************
  LOOP AT ct_ltap INTO ls_ltap_vb.
    ls_ltap = ls_ltap_vb.

    PERFORM check_aut_front_to USING ls_ltap CHANGING lv_subrc.
    IF lv_subrc <> 0.
**    Existe uma OT em aberto para uma Posição de profundidade superior
      MESSAGE e067 WITH ls_ltap-tanum INTO lv_message.
      io_ballog->log_add_message( ).

      c_subrc = c_return_invalid_to.
      RETURN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " F_CREATETO_MOVE_SU
*&---------------------------------------------------------------------*
*&      Form  f_check_user_lgnum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_LGNUM    text
*      -->IO_BALLOG  text
*      <--C_SUBRC    text
*----------------------------------------------------------------------*
FORM f_check_user_lgnum USING i_lgnum   TYPE lgnum
                              io_ballog TYPE REF TO zcl_ca_bal_log
                        CHANGING c_subrc TYPE sysubrc.
  DATA lv_message TYPE bapiret2-message.                    "#EC NEEDED

  DATA lt_xuser TYPE STANDARD TABLE OF lrf_wkqu WITH DEFAULT KEY.

  IF c_subrc NE 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
      i_lgnum        = i_lgnum
    TABLES
      t_xuser        = lt_xuser[]
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc NE 0.
    MESSAGE e034 WITH sy-uname i_lgnum INTO lv_message.
    io_ballog->log_add_message( ).

    c_subrc = c_return_no_lgnum.
    RETURN.
  ENDIF.

  SORT lt_xuser[] BY statu.
  DELETE lt_xuser[] WHERE statu NE abap_true.

  IF lt_xuser[] IS INITIAL.
    MESSAGE e034 WITH sy-uname i_lgnum INTO lv_message.
    io_ballog->log_add_message( ).

    c_subrc = c_return_no_lgnum.
    RETURN.
  ENDIF.
ENDFORM.                    "f_check_user_lgnum
*&---------------------------------------------------------------------*
*&      Form  F_GET_HARDCODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_LGNUM    text
*      -->IO_BALLOG  text
*      <--C_SUBRC    text
*----------------------------------------------------------------------*
FORM f_get_hardcodes USING i_lgnum   TYPE lgnum
                           io_ballog TYPE REF TO zcl_ca_bal_log
                     CHANGING c_subrc TYPE sysubrc.
  DATA lv_message TYPE bapiret2-message.                    "#EC NEEDED

  FREE gt_zwmmpt001[].

  IF c_subrc NE 0.
    RETURN.
  ENDIF.

  SELECT processo parametro item valor
    FROM zwm001 INTO TABLE gt_zwmmpt001[]
    WHERE armazem EQ i_lgnum.
  IF sy-subrc NE 0.
    MESSAGE e044 WITH i_lgnum INTO lv_message.
    io_ballog->log_add_message( ).

    c_subrc = c_return_no_hardcodes.
  ENDIF.
ENDFORM.                    " F_GET_HARDCODES
*&---------------------------------------------------------------------*
*&      Form  f_check_pending_su
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_LGNUM    text
*      -->I_LENUM    text
*      -->IO_BALLOG  text
*      <--C_SUBRC    text
*      <--CS_LTAK    text
*      <--CT_LTAP    text
*----------------------------------------------------------------------*
FORM f_check_pending_su USING i_lgnum     TYPE lgnum
                              i_lenum     TYPE lenum
                              io_ballog   TYPE REF TO zcl_ca_bal_log
                        CHANGING c_subrc TYPE sysubrc
                                 cs_ltak TYPE ltak_vb
                                 ct_ltap TYPE tt_ltap_vb.
  DATA lv_message TYPE bapiret2-message.                    "#EC NEEDED
  DATA lv_lqnum   TYPE lqua-lqnum.                          "#EC NEEDED
  DATA lv_subrc   TYPE sysubrc.

  DATA ls_ltak  TYPE ltak.

  DATA lt_ltap TYPE STANDARD TABLE OF ltap WITH KEY lgnum tanum tapos.

  DATA lr_queues  TYPE RANGE OF ltak-queue.

  FIELD-SYMBOLS <fs_ltap>       LIKE LINE OF lt_ltap[].
  FIELD-SYMBOLS <fs_ltap_vb>    LIKE LINE OF ct_ltap[].
  FIELD-SYMBOLS <fs_zwmmpt001>  LIKE LINE OF gt_zwmmpt001[].
  FIELD-SYMBOLS <fs_queue>      LIKE LINE OF lr_queues[].

  IF c_subrc NE 0.
    RETURN.
  ENDIF.

  LOOP AT gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001> WHERE processo EQ c_process_queues_maint
                                                    AND parametro EQ c_param_mov_aut_queues.
    APPEND INITIAL LINE TO lr_queues[] ASSIGNING <fs_queue>.
    <fs_queue>-sign     = c_rsig_i.
    <fs_queue>-option   = c_ropt_eq.
    <fs_queue>-low      = <fs_zwmmpt001>-valor.
  ENDLOOP.

  SELECT lqnum UP TO 1 ROWS
    FROM lqua INTO lv_lqnum
    WHERE lgnum EQ i_lgnum
      AND lenum EQ i_lenum. " Index LQUA~ZSU
  ENDSELECT.
  IF sy-subrc NE 0.
    MESSAGE e045 INTO lv_message.
    io_ballog->log_add_message( ).

    c_subrc = c_return_no_su_to_move.
    RETURN.
  ENDIF.

  SELECT *
    FROM ltap INTO TABLE lt_ltap[]
    WHERE lgnum EQ i_lgnum
      AND nlenr EQ i_lenum
      AND pquit EQ abap_false.  " Partial index LTAP~ZWM

  " Delete all entries where stock status is not initial
  SORT lt_ltap[] BY bestq.
  DELETE lt_ltap[] WHERE bestq IS NOT INITIAL.
  " Delete all entries where SU is not confirmed
  SORT lt_ltap[] BY pquit.
  DELETE lt_ltap[] WHERE pquit NE abap_false.

  IF lt_ltap[] IS NOT INITIAL. " there is already a pending TO for this SU
    MESSAGE i046  INTO lv_message.
    io_ballog->log_add_message( ).

    c_subrc = c_return_pending_to.
  ELSE.
    RETURN.
  ENDIF.

  READ TABLE lt_ltap[] ASSIGNING <fs_ltap> INDEX 1.
  IF sy-subrc EQ 0.
    SELECT SINGLE *
      FROM ltak INTO ls_ltak
      WHERE lgnum EQ <fs_ltap>-lgnum
        AND tanum EQ <fs_ltap>-tanum.

    IF ls_ltak-queue NOT IN lr_queues[].
      MESSAGE e061 WITH <fs_ltap>-tanum INTO lv_message.
      io_ballog->log_add_message( ).

      c_subrc = c_return_invalid_to.
      RETURN.
    ENDIF.

    PERFORM check_aut_front_to USING <fs_ltap> CHANGING lv_subrc.
    IF lv_subrc <> 0.
      MESSAGE e067 WITH <fs_ltap>-tanum INTO lv_message.
      io_ballog->log_add_message( ).

      c_subrc = c_return_invalid_to.
      RETURN.
    ENDIF.


    MOVE-CORRESPONDING ls_ltak TO cs_ltak.

    LOOP AT lt_ltap[] ASSIGNING <fs_ltap>.
      CHECK <fs_ltap>-lgnum EQ ls_ltak-lgnum.
      CHECK <fs_ltap>-tanum EQ ls_ltak-tanum.

      APPEND INITIAL LINE TO ct_ltap[] ASSIGNING <fs_ltap_vb>.
      MOVE-CORRESPONDING <fs_ltap> TO <fs_ltap_vb>.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_CHECK_PENDING_SU
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUT_FRONT_TO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_LTAP>  text
*      <--P_C_SUBRC  text
*----------------------------------------------------------------------*
FORM check_aut_front_to  USING    us_ltap TYPE ltap
                         CHANGING cv_subrc.
  DATA: lt_ltak       TYPE SORTED TABLE OF ltak WITH UNIQUE KEY tanum,
        lt_ltap       TYPE TABLE OF ltap,
        lt_queue      TYPE TABLE OF lrf_queue,
        lt_queue_in   TYPE TABLE OF lrf_queue,
        lt_queue_out  TYPE TABLE OF lrf_queue,
        lt_zwm001     TYPE TABLE OF zwm001,
        lt_ltap_rem   TYPE TABLE OF ltap.

  DATA: ls_ltak   TYPE ltak,
        ls_ltap   TYPE ltap,
        ls_zwm001 TYPE zwm001,
        ls_zwm020 TYPE zwm020.

  DATA: lr_lgpla TYPE RANGE OF lgpla.

  DATA: ls_r_lgpla LIKE LINE OF lr_lgpla.

  DATA: lv_pos   TYPE c,
        lv_pos2  TYPE c,
        lv_queue TYPE lrf_queue.

** Queues de Entrada Em AUT
***********************************************************************
  SELECT * FROM zwm001
           INTO TABLE lt_zwm001
           WHERE armazem = us_ltap-lgnum AND
                 processo = 'GESTAO_FILAS' AND
                 parametro = 'FILA_ENT_AUT'.

  LOOP AT lt_zwm001 INTO ls_zwm001.
    MOVE ls_zwm001-valor TO lv_queue.
    APPEND lv_queue TO lt_queue_in.
    APPEND lv_queue TO lt_queue.
  ENDLOOP.

  SELECT * FROM zwm001
           INTO TABLE lt_zwm001
           WHERE armazem = us_ltap-lgnum AND
                 processo = 'GESTAO_FILAS' AND
                 parametro = 'FILA_SAIDA_AUT'.

  LOOP AT lt_zwm001 INTO ls_zwm001.
    MOVE ls_zwm001-valor TO lv_queue.
    APPEND lv_queue TO lt_queue_out.
    APPEND lv_queue TO lt_queue.
  ENDLOOP.

  CHECK NOT lt_queue IS INITIAL.

** LTAK
***********************************************************************
  SELECT SINGLE * FROM ltak
                  INTO ls_ltak
                  WHERE lgnum = us_ltap-lgnum AND
                        tanum = us_ltap-tanum.
  CHECK sy-subrc EQ 0.

** Reorna Ultima OT para Posição da Frente
**********************************************************************
  SELECT * FROM ltak
           INTO TABLE lt_ltak
           FOR ALL ENTRIES IN lt_queue
           WHERE lgnum = us_ltap-lgnum AND
                 kquit = abap_false AND
                 queue = lt_queue-table_line.
  CHECK sy-subrc EQ 0.

  SELECT * FROM ltap
           INTO TABLE lt_ltap
           FOR ALL ENTRIES IN lt_ltak
           WHERE lgnum = lt_ltak-lgnum AND
                 tanum = lt_ltak-tanum.
  CHECK sy-subrc EQ 0.





  "Apaga OT's que não quer considerar
  LOOP AT lt_ltap INTO ls_ltap.
    CLEAR: ls_ltak.
    READ TABLE lt_ltak
          INTO ls_ltak
          WITH TABLE KEY tanum = ls_ltap-tanum.
    CHECK sy-subrc EQ 0.

    "testa se OT de Entrada
    READ TABLE lt_queue_in
      WITH KEY table_line = ls_ltak-queue
      TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      "apaga se não for para o mesmo tipo de deposito da ot testada
      DELETE lt_ltap WHERE tanum = ls_ltap-tanum  AND
                           tapos = ls_ltap-tapos  AND
                           nltyp <> us_ltap-nltyp.

      "apaga se ot já tiver sido comunicada
      DELETE lt_ltap WHERE tanum = ls_ltap-tanum AND
                           tapos = ls_ltap-tapos AND
                           kzsub = abap_true.

      "apaga se a posição de destino for diferente da posição de destino da ot testada
      CLEAR: ls_r_lgpla, lr_lgpla.
      CONCATENATE us_ltap-nlpla(8) '*' INTO ls_r_lgpla-low.
      ls_r_lgpla-sign = 'I'.
      ls_r_lgpla-option = 'CP'.
      APPEND ls_r_lgpla TO lr_lgpla.
      DELETE lt_ltap WHERE tanum = ls_ltap-tanum  AND
                           tapos = ls_ltap-tapos  AND
                           NOT nlpla IN lr_lgpla.

      CONTINUE.
    ENDIF.

    "testa se OT de saida
    READ TABLE lt_queue_out
      WITH KEY table_line = ls_ltak-queue
      TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      "apaga por ot Picked
      DELETE lt_ltap WHERE tanum = ls_ltap-tanum  AND
                           tapos = ls_ltap-tapos  AND
                           pvqui EQ abap_true.

      "apaga se não vier do mesmo tipo de deposito da ot testada
      DELETE lt_ltap WHERE tanum = ls_ltap-tanum  AND
                           tapos = ls_ltap-tapos  AND
                           vltyp <> us_ltap-nltyp.

      "apaga se a posição de origem for diferente da posição de destino da ot testada
      CLEAR: ls_r_lgpla, lr_lgpla.
      CONCATENATE us_ltap-nlpla(8) '*' INTO ls_r_lgpla-low.
      ls_r_lgpla-sign = 'I'.
      ls_r_lgpla-option = 'CP'.
      APPEND ls_r_lgpla TO lr_lgpla.
      DELETE lt_ltap WHERE tanum = ls_ltap-tanum  AND
                           tapos = ls_ltap-tapos  AND
                           NOT vlpla IN lr_lgpla.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  DELETE lt_ltap WHERE tanum = us_ltap-tanum AND
                       tapos = us_ltap-tapos.

** Valida se tem uma remontada e apaga remontada
***********************************************************************
  DO 1 TIMES.
    SELECT SINGLE * FROM zwm020
                    INTO ls_zwm020
                    WHERE armazem = us_ltap-lgnum AND
                          p1      = us_ltap-nlenr.
    CHECK NOT ls_zwm020-p2 IS INITIAL.

    SELECT * FROM ltap
             INTO TABLE lt_ltap_rem
             WHERE lgnum = ls_zwm020-armazem AND
                   nlenr = ls_zwm020-p2 AND
                   pquit = abap_false.
    CHECK sy-subrc EQ 0.

    LOOP AT lt_ltap_rem INTO ls_ltap.
      DELETE lt_ltap WHERE tanum = ls_ltap-tanum AND
                           tapos = ls_ltap-tapos.
    ENDLOOP.
  ENDDO.



  CHECK NOT lt_ltap IS INITIAL.

  LOOP AT lt_ltap INTO ls_ltap.
    CLEAR: ls_ltak.
    READ TABLE lt_ltak
              INTO ls_ltak
              WITH TABLE KEY tanum = ls_ltap-tanum.
    CHECK sy-subrc EQ 0.

    READ TABLE lt_queue_in
      WITH KEY table_line = ls_ltak-queue
      TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      lv_pos  = ls_ltap-nlpla+8(1).
      lv_pos2 = us_ltap-nlpla+8(1).
      CHECK lv_pos > lv_pos2.
      cv_subrc = 4.
      RETURN.
    ENDIF.

    READ TABLE lt_queue_out
      WITH KEY table_line = ls_ltak-queue
      TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      cv_subrc = 4.
      RETURN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_AUT_FRONT_TO
*&---------------------------------------------------------------------*
*&      Form  EXIT_ARMAUT_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_armaut_lock USING i_lgnum TYPE lgnum
                            i_lgtyp TYPE lgtyp.

  CONCATENATE sy-mandt i_lgnum i_lgtyp 'LOCK_EXIT' INTO gv_key_exit_lock SEPARATED BY '_'.


  DO 30 TIMES.
    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    DO 1000 TIMES.
      "Tenta Fazer Vários Enqueues em Menos de um segundo.
      CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
        EXPORTING
          keyword_       = gv_key_exit_lock
          _scope         = '1'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc EQ 0.
        RETURN.
      ENDIF.
    ENDDO.
  ENDDO.
ENDFORM.                    " EXIT_ARMAUT_LOCK
*&---------------------------------------------------------------------*
*&      Form  EXIT_ARMAUT_UNLOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_armaut_unlock .
  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      keyword_ = gv_key_exit_lock.

  CLEAR: gv_key_exit_lock.
ENDFORM.                    " EXIT_ARMAUT_UNLOCK
