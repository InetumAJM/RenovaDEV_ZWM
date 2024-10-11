*----------------------------------------------------------------------*
***INCLUDE LZWMFR_FGF05 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_DATA_INIT_GET_HARDCODES
*&---------------------------------------------------------------------*
FORM f_data_init_get_hardcodes TABLES et_zwm001 STRUCTURE zwm001
                               USING i_lgnum TYPE lgnum.
  DATA lt_zwm001 TYPE TABLE OF zwm001.

  SELECT *
    FROM zwm001 INTO TABLE lt_zwm001
    WHERE armazem EQ i_lgnum.

  et_zwm001[] = lt_zwm001[].
ENDFORM.                    " F_DATA_INIT_GET_HARDCODES
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0010_CHECK_LENUM
*&---------------------------------------------------------------------*
FORM f_data_scr0010_check_lenum USING is_ltak     TYPE ltak
                                      i_equipment TYPE char20
                                      i_lenum     TYPE lenum
                                      i_process   TYPE zwmmp_parametro
                                CHANGING cs_ltap   TYPE ltap
                                         cs_zwm011 TYPE zwm011
                                         c_lenum   TYPE lenum
                                         c_okcode  TYPE syucomm.
  DATA lt_ltap       TYPE TABLE OF ltap.
  DATA lt_zwm011_upd TYPE TABLE OF zwm011.

  DATA ls_bdcmsgcoll TYPE bdcmsgcoll.
  DATA ls_ltak       TYPE ltak.
  DATA ls_ltap       TYPE ltap.
  DATA ls_zwm020     TYPE zwm020.

  DATA lv_exist   TYPE exist.
  DATA lv_keyword TYPE keywords.
  DATA lv_lenum   TYPE lenum.
  DATA lv_lock    TYPE abap_bool.
  DATA lv_msg     TYPE string.
  DATA lv_subrc   TYPE sysubrc.

  IF c_lenum IS INITIAL.
    CLEAR c_okcode.
    EXIT.
  ENDIF.

  " for PRM SU we don't need to check this validation
  CASE i_process.
    WHEN c_param_queue_aut_prm.
      lv_lenum = c_lenum.
    WHEN OTHERS.
      lv_lenum = i_lenum.
  ENDCASE.

  IF c_lenum NE lv_lenum.
    MESSAGE e062(zwmfr001) WITH c_lenum lv_lenum INTO lv_msg.

    ls_bdcmsgcoll-msgid = sy-msgid.
    ls_bdcmsgcoll-msgtyp = sy-msgty.
    ls_bdcmsgcoll-msgnr = sy-msgno.
    ls_bdcmsgcoll-msgv1 = sy-msgv1.
    ls_bdcmsgcoll-msgv2 = sy-msgv2.

    lv_subrc = 4.
  ELSE.
    IF cs_zwm011-status EQ 'C'.
      CASE i_process.
        WHEN c_param_queue_aut_rep.
          CONCATENATE sy-mandt is_ltak-betyp is_ltak-benum is_ltak-queue INTO lv_keyword.
        WHEN OTHERS.
          CONCATENATE sy-mandt is_ltak-refnr INTO lv_keyword.
      ENDCASE.

      PERFORM f_enqueue_keyword
        USING lv_keyword
              3
              lv_lock.
      IF lv_lock EQ abap_true.
        lv_subrc = 0.

        CASE i_process.
          WHEN c_param_queue_aut_sd_pul.

            PERFORM f_get_lung_bin
              USING is_ltak
                    i_equipment
              CHANGING cs_zwm011-ultimo_bin.
          WHEN OTHERS.
            " Do nothing
        ENDCASE.

        cs_zwm011-status = 'P'.

        APPEND cs_zwm011 TO lt_zwm011_upd .

        CALL FUNCTION 'Z_WMFR_SAVE_ZWM011' IN UPDATE TASK
          TABLES
            it_zwm011_upd = lt_zwm011_upd.

        PERFORM f_commit_work USING 'X' 0.
        PERFORM f_dequeue_keyword USING lv_keyword.
      ELSE.
        lv_subrc = 4.

        "Acção bloqueada pelo sistema. Tentar novamente
        MESSAGE e064(zwmfr001) INTO lv_msg.

        ls_bdcmsgcoll-msgid = sy-msgid.
        ls_bdcmsgcoll-msgtyp = sy-msgty.
        ls_bdcmsgcoll-msgnr = sy-msgno.
      ENDIF.
    ENDIF.

    IF lv_subrc EQ 0.
      IF cs_ltap-pvqui EQ abap_false.
        APPEND cs_ltap TO lt_ltap.
      ENDIF.

      " F_GET_ZWM020_REMOUNT_DATA
      PERFORM f_get_remount_su_data
        USING cs_ltap
        CHANGING ls_ltak
                 ls_ltap
                 ls_zwm020
                 lv_exist.
      IF lv_exist EQ abap_true.
        IF ls_ltap-pvqui EQ abap_false.
          APPEND ls_ltap TO lt_ltap.
        ENDIF.
      ENDIF.

      " confirm first step
      PERFORM f_data_l_to_confirm
        TABLES lt_ltap
        USING '1'
        CHANGING lv_subrc
                 ls_bdcmsgcoll.

      IF lv_subrc EQ 0.
        cs_ltap-pvqui = abap_true.

        PERFORM f_commit_work USING 'X' 0.
      ELSE.
        PERFORM f_rollback.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lv_subrc NE 0.
    PERFORM f_show_message USING ls_bdcmsgcoll.

    CLEAR: c_okcode, c_lenum.
  ENDIF.
ENDFORM.                    "F_DATA_SCR0010_CHECK_LENUM
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0010_CHECK_LGPLA_C
*&---------------------------------------------------------------------*
FORM f_data_scr0010_check_lgpla_c USING is_ltak   TYPE ltak
                                        i_lgpla   TYPE char14
                                        i_process TYPE zwmmp_parametro
                                  CHANGING c_lgpla  TYPE char14
                                           c_pul    TYPE numc2
                                           c_okcode TYPE syucomm.
  DATA lv_msg   TYPE string.
  DATA lv_subrc TYPE sysubrc.

  DATA ls_bdcmsgcoll TYPE bdcmsgcoll.

  IF c_lgpla IS INITIAL.
    CLEAR c_okcode.
    EXIT.
  ENDIF.

  IF c_lgpla NE i_lgpla.
    MESSAGE e063(zwmfr001) INTO lv_msg.

    ls_bdcmsgcoll-msgid = sy-msgid.
    ls_bdcmsgcoll-msgtyp = sy-msgty.
    ls_bdcmsgcoll-msgnr = sy-msgno.

    PERFORM f_show_message USING ls_bdcmsgcoll.

    CLEAR: c_lgpla, c_okcode.
  ELSE.
    " if process is PUL then we need to get bin position
    CASE i_process.
      WHEN c_param_queue_aut_sd_pul.
        PERFORM f_get_lung_free_position
          USING is_ltak
          CHANGING c_pul.

      WHEN OTHERS.
    ENDCASE.
  ENDIF.

ENDFORM.                    "F_DATA_SCR0010_CHECK_LGPLA_C
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0010_CHECK_PUL
*&---------------------------------------------------------------------*
FORM f_data_scr0010_check_pul USING i_lgpla   TYPE char14
                                    i_lgnum   TYPE lgnum
                                    i_process TYPE zwmmp_parametro
                                    i_pul     TYPE numc2
                              CHANGING c_pul    TYPE numc2
                                       c_okcode TYPE syucomm.

  DATA ls_bdcmsgcoll TYPE bdcmsgcoll.

  DATA lv_msg TYPE string.

  IF i_process NE c_param_queue_aut_sd_pul.
    EXIT.
  ENDIF.

  IF c_pul EQ '00' OR c_pul IS INITIAL.
    CLEAR c_okcode.
    EXIT.
  ENDIF.

  IF c_pul NE i_pul.
    MESSAGE e060(zwmfr001) INTO lv_msg.

    ls_bdcmsgcoll-msgid = sy-msgid.
    ls_bdcmsgcoll-msgtyp = sy-msgty.
    ls_bdcmsgcoll-msgnr = sy-msgno.

    PERFORM f_show_message USING ls_bdcmsgcoll.

    CLEAR: c_okcode, c_pul.
  ELSE.
    " this should never happen

    SELECT mandt UP TO 1 ROWS
      FROM zwm013 INTO sy-mandt
      WHERE armazem EQ i_lgnum
        AND destino EQ i_lgpla
        AND posicao_pulmao EQ c_pul.
    ENDSELECT.
    IF sy-subrc EQ 0.
      MESSAGE e175(zwmmsg001) INTO lv_msg.

      ls_bdcmsgcoll-msgid = sy-msgid.
      ls_bdcmsgcoll-msgtyp = sy-msgty.
      ls_bdcmsgcoll-msgnr = sy-msgno.

      PERFORM f_show_message USING ls_bdcmsgcoll.

      CLEAR: c_okcode, c_pul.
    ENDIF.
  ENDIF.

ENDFORM.                    "F_DATA_SCR0010_CHECK_PUL
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0010_UCOMM_NEXT
*&---------------------------------------------------------------------*
FORM f_data_scr0010_ucomm_next.
  DATA lv_subrc TYPE sysubrc.

  CASE gv_process.
    WHEN c_param_queue_aut_rep.
      PERFORM f_data_scr0010_ucomm_next_rep
        USING gs_ltak
              gs_ltap
              gs_zwm011
              gs_zwmfrt005
              gs_xuser-bname
              gv_equipment
        CHANGING lv_subrc.

    WHEN c_param_queue_aut_sd_pul.
      PERFORM f_data_scr0010_ucomm_next_pul
        USING gs_ltak
              gs_ltap
              gs_zwm011
              gs_zwmfrt005
              gs_xuser-bname
              gv_equipment
              gs_scr001-lgpla_conf
              gs_scr001-lgpla_pul2
        CHANGING lv_subrc.

    WHEN c_param_queue_aut_sd_dck.
      PERFORM f_data_scr0010_ucomm_next_dck
        USING gs_ltak
              gs_ltap
              gs_zwm011
              gs_zwmfrt005
              gs_xuser-bname
              gv_equipment
        CHANGING lv_subrc.

    WHEN c_param_queue_aut_prm.
      PERFORM f_data_scr0010_ucomm_next_prm
        USING gs_ltak
              gs_ltap
              gs_zwm011
              gs_scr001-lenum
        CHANGING lv_subrc.

    WHEN OTHERS.
  ENDCASE.

  IF lv_subrc EQ 0.
    " get new work
    CLEAR gs_scr001.

    PERFORM f_get_aut_to
      USING gs_xuser
            gv_equipment
      CHANGING lv_subrc.

    IF lv_subrc NE 0.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_DATA_SCR0010_UCOMM_NEXT
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0010_UCOMM_NEXT_REP
*&---------------------------------------------------------------------*
FORM f_data_scr0010_ucomm_next_rep USING is_ltak      TYPE ltak
                                         is_ltap      TYPE ltap
                                         is_zwm011    TYPE zwm011
                                         is_zwmfrt005 TYPE zwmfrt005
                                         i_bname      TYPE xubname
                                         i_equipment TYPE char20
                                   CHANGING c_subrc TYPE sysubrc.

  CONSTANTS c_lgpla_dummy TYPE char14 VALUE '              '.
  CONSTANTS c_pul_dummy   TYPE numc2  VALUE '00'.

  DATA lt_ltak             TYPE TABLE OF ltak.
  DATA lt_ltap             TYPE TABLE OF ltap.
  DATA lt_zwm011_ins       TYPE TABLE OF zwm011.
  DATA lt_zwm011_del       TYPE TABLE OF zwm011.
  DATA lt_zwm013_ins_dummy TYPE TABLE OF zwm013.
  DATA lt_zwm013_upd_dummy TYPE TABLE OF zwm013.
  DATA lt_zwm020_del       TYPE TABLE OF zwm020.
  DATA lt_zwm028_upd       TYPE TABLE OF zwm028.
  DATA lt_zwmfrt004_del    TYPE TABLE OF zwmfrt004.
  DATA lt_zwmfrt005_del    TYPE TABLE OF zwmfrt005.

  DATA ls_bdcmsgcoll TYPE bdcmsgcoll.

  DATA lv_keyword  TYPE keywords.
  DATA lv_lock     TYPE abap_bool.
  DATA lv_msg      TYPE string.
  DATA lv_unlock_r TYPE abap_bool.

  CONCATENATE sy-mandt is_ltak-betyp is_ltak-benum is_ltak-queue INTO lv_keyword.

  PERFORM f_enqueue_keyword
    USING lv_keyword
          3
          lv_lock.
  IF lv_lock EQ abap_true.
    APPEND is_ltak TO lt_ltak.
    APPEND is_ltap TO lt_ltap.

    " call subprocess aut 3
    PERFORM f_data_scr0010_ucomm_aut_rmt
      TABLES lt_ltak
             lt_ltap
             lt_zwm011_ins
             lt_zwm013_ins_dummy
             lt_zwm013_upd_dummy
             lt_zwm020_del
             lt_zwm028_upd
      USING is_ltak
            is_ltap
            is_zwm011
            i_bname
            i_equipment
            c_lgpla_dummy
            c_param_queue_aut_rep
            c_pul_dummy
      CHANGING ls_bdcmsgcoll
               c_subrc
               lv_unlock_r.

    IF c_subrc EQ 0.
      PERFORM f_data_l_to_confirm
        TABLES lt_ltap
        USING '2'
        CHANGING c_subrc
                 ls_bdcmsgcoll.
    ENDIF.

    IF c_subrc EQ 0.
      " check for update is ZWMFRT004
      PERFORM f_update_zwmfrt004
        TABLES lt_zwmfrt004_del
               lt_ltak
               lt_zwm028_upd
               lt_zwm013_ins_dummy
               lt_zwm013_upd_dummy
        USING is_ltap
              is_zwmfrt005
              c_param_queue_aut_rep.

      APPEND is_zwm011 TO lt_zwm011_del.

      APPEND is_zwmfrt005 TO lt_zwmfrt005_del.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWM011' IN UPDATE TASK
        TABLES
          it_zwm011_del = lt_zwm011_del
          it_zwm011_ins = lt_zwm011_ins.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWM020' IN UPDATE TASK
        TABLES
          it_zwm020_del = lt_zwm020_del.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWM028' IN UPDATE TASK
        TABLES
          it_zwm028_upd = lt_zwm028_upd.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWMFRT004' IN UPDATE TASK
        TABLES
          it_zwmfrt004_del = lt_zwmfrt004_del.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWMFRT005' IN UPDATE TASK
        TABLES
          it_zwmfrt005_del = lt_zwmfrt005_del.

      PERFORM f_commit_work USING 'X' 2.
      PERFORM f_dequeue_keyword USING lv_keyword.
      IF is_ltap-nltyp <> 'PKB'.
        PERFORM f_create_picking_to
          USING is_ltak
                i_bname.
      ENDIF.
    ELSE.
      PERFORM f_rollback.
      PERFORM f_dequeue_keyword USING lv_keyword.
    ENDIF.
  ELSE.
    c_subrc = 4.

    "Acção bloqueada pelo sistema. Tentar novamente
    MESSAGE e064(zwmfr001) INTO lv_msg.

    ls_bdcmsgcoll-msgid = sy-msgid.
    ls_bdcmsgcoll-msgtyp = sy-msgty.
    ls_bdcmsgcoll-msgnr = sy-msgno.
  ENDIF.

  IF c_subrc = 4.
    PERFORM f_show_message USING ls_bdcmsgcoll.
  ENDIF.
ENDFORM.                    " F_DATA_SCR0010_UCOMM_NEXT_REP
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0010_UCOMM_NEXT_DCK
*&---------------------------------------------------------------------*
FORM f_data_scr0010_ucomm_next_dck USING is_ltak      TYPE ltak
                                         is_ltap      TYPE ltap
                                         is_zwm011    TYPE zwm011
                                         is_zwmfrt005 TYPE zwmfrt005
                                         i_bname      TYPE xubname
                                         i_equipment  TYPE char20
                                   CHANGING c_subrc TYPE sysubrc.

  CONSTANTS c_lgpla_dummy TYPE char14 VALUE '            '.
  CONSTANTS c_pul_dummy   TYPE numc2  VALUE '00'.

  DATA lt_ltak             TYPE TABLE OF ltak.
  DATA lt_ltap             TYPE TABLE OF ltap.
  DATA lt_zwm011_ins       TYPE TABLE OF zwm011.
  DATA lt_zwm011_del       TYPE TABLE OF zwm011.
  DATA lt_zwm013_ins_dummy TYPE TABLE OF zwm013.
  DATA lt_zwm013_upd_dummy TYPE TABLE OF zwm013.
  DATA lt_zwm020_del       TYPE TABLE OF zwm020.
  DATA lt_zwm028_upd       TYPE TABLE OF zwm028.
  DATA lt_zwmfrt004_del    TYPE TABLE OF zwmfrt004.
  DATA lt_zwmfrt005_del    TYPE TABLE OF zwmfrt005.

  DATA ls_bdcmsgcoll TYPE bdcmsgcoll.

  DATA lv_keyword  TYPE keywords.
  DATA lv_lock     TYPE abap_bool.
  DATA lv_msg      TYPE string.
  DATA lv_unlock_r TYPE abap_bool.
  DATA lv_unlock_vb TYPE vbeln.

  CONCATENATE sy-mandt is_ltak-refnr INTO lv_keyword.

  PERFORM f_enqueue_keyword
    USING lv_keyword
          3
          lv_lock.
  IF lv_lock EQ abap_true.
    APPEND is_ltak TO lt_ltak.
    APPEND is_ltap TO lt_ltap.

    PERFORM f_data_update_dck
      TABLES lt_zwm028_upd
      USING is_ltak
            is_zwm011
      CHANGING lv_unlock_r
               lv_unlock_vb.

    " call subprocess aut 3
    PERFORM f_data_scr0010_ucomm_aut_rmt
      TABLES lt_ltak
             lt_ltap
             lt_zwm011_ins
             lt_zwm013_ins_dummy
             lt_zwm013_upd_dummy
             lt_zwm020_del
             lt_zwm028_upd
      USING is_ltak
            is_ltap
            is_zwm011
            i_bname
            i_equipment
            c_lgpla_dummy
            c_param_queue_aut_sd_dck
            c_pul_dummy
      CHANGING ls_bdcmsgcoll
               c_subrc
               lv_unlock_r.

    IF c_subrc EQ 0.
      PERFORM f_data_l_to_confirm
        TABLES lt_ltap
        USING '2'
        CHANGING c_subrc
                 ls_bdcmsgcoll.
    ENDIF.

    IF c_subrc EQ 0.
      APPEND is_zwm011 TO lt_zwm011_del.

      APPEND is_zwmfrt005 TO lt_zwmfrt005_del.

      PERFORM f_update_zwmfrt004
        TABLES lt_zwmfrt004_del
               lt_ltak
               lt_zwm028_upd
               lt_zwm013_ins_dummy
               lt_zwm013_upd_dummy
        USING is_ltap
              is_zwmfrt005
              c_param_queue_aut_sd_dck.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWM011' IN UPDATE TASK
        TABLES
          it_zwm011_del = lt_zwm011_del
          it_zwm011_ins = lt_zwm011_ins.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWM020' IN UPDATE TASK
        TABLES
          it_zwm020_del = lt_zwm020_del.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWM028' IN UPDATE TASK
        TABLES
          it_zwm028_upd = lt_zwm028_upd.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWMFRT004' IN UPDATE TASK
        TABLES
          it_zwmfrt004_del = lt_zwmfrt004_del.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWMFRT005' IN UPDATE TASK
        TABLES
          it_zwmfrt005_del = lt_zwmfrt005_del.

      IF lv_unlock_r EQ abap_true.
        CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK_SAVE' IN UPDATE TASK
          EXPORTING
            i_lgnum  = is_ltak-lgnum
            i_refnr  = is_ltak-refnr
            i_vbeln  = lv_unlock_vb
            i_commit = ' '.
      ENDIF.

      PERFORM f_commit_work USING 'X' 2.
      PERFORM f_dequeue_keyword USING lv_keyword.

      IF lv_unlock_r EQ abap_true.
        CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
          EXPORTING
            i_lgnum  = is_ltak-lgnum
            i_refnr  = is_ltak-refnr
            i_vbeln  = lv_unlock_vb
            i_commit = 'X'.
      ENDIF.
    ELSE.
      PERFORM f_rollback.
      PERFORM f_dequeue_keyword USING lv_keyword.
      PERFORM f_show_message USING ls_bdcmsgcoll.
    ENDIF.
  ELSE.
    c_subrc = 4.

    "Acção bloqueada pelo sistema. Tentar novamente
    MESSAGE e064(zwmfr001) INTO lv_msg.

    ls_bdcmsgcoll-msgid = sy-msgid.
    ls_bdcmsgcoll-msgtyp = sy-msgty.
    ls_bdcmsgcoll-msgnr = sy-msgno.

    PERFORM f_show_message USING ls_bdcmsgcoll.
  ENDIF.

ENDFORM.                    " F_DATA_SCR0010_UCOMM_NEXT_DCK
*&---------------------------------------------------------------------*
*&      Form  F_DATA_UPDATE_DCK
*&---------------------------------------------------------------------*
FORM f_data_update_dck TABLES ct_zwm028 STRUCTURE zwm028
                       USING is_ltak   TYPE ltak
                             is_zwm011 TYPE zwm011
                       CHANGING c_unlock_r TYPE abap_bool
                                c_unlock_vb TYPE vbeln.

  DATA lt_zwm028 TYPE TABLE OF zwm028.

  DATA ls_zwm028 TYPE zwm028.

  DATA: lv_zlock TYPE zlock,
        lv_vbeln TYPE vbeln.


  FIELD-SYMBOLS <zwm028> TYPE zwm028.

  PERFORM get_to_vbeln CHANGING is_ltak.

  READ TABLE ct_zwm028 ASSIGNING <zwm028>
    WITH KEY lgnum = is_ltak-lgnum
             refnr = is_ltak-refnr
             remessa = is_ltak-vbeln.
  IF sy-subrc NE 0.
    SELECT SINGLE *
      FROM zwm028 INTO ls_zwm028
      WHERE lgnum EQ is_ltak-lgnum
        AND refnr EQ is_ltak-refnr
        AND remessa EQ is_ltak-vbeln.
    IF sy-subrc EQ 0.
      APPEND ls_zwm028 TO ct_zwm028 ASSIGNING <zwm028>.
      <zwm028>-paletes_carro = <zwm028>-paletes_carro + 1.
    ENDIF.
  ELSE.
    <zwm028>-paletes_carro = <zwm028>-paletes_carro + 1.
  ENDIF.

  " check type lock 'R'
  IF <zwm028>-tipo_lock EQ 'R'.
    IF <zwm028>-paletes_carro EQ <zwm028>-total_paletes.
      c_unlock_r = abap_true.
    ENDIF.
  ENDIF.

  READ TABLE ct_zwm028 ASSIGNING <zwm028>
    WITH KEY lgnum = is_ltak-lgnum
             refnr = is_ltak-refnr
             remessa = space.
  IF sy-subrc NE 0.
    SELECT SINGLE *
      FROM zwm028 INTO ls_zwm028
      WHERE lgnum EQ is_ltak-lgnum
        AND refnr EQ is_ltak-refnr
        AND remessa EQ space.
    IF sy-subrc EQ 0.
      APPEND ls_zwm028 TO ct_zwm028 ASSIGNING <zwm028>.
      <zwm028>-paletes_carro = <zwm028>-paletes_carro + 1.
    ENDIF.
  ELSE.
    <zwm028>-paletes_carro = <zwm028>-paletes_carro + 1.
  ENDIF.

  IF c_unlock_r EQ abap_true.
    SELECT *
      FROM zwm028
      INTO TABLE lt_zwm028
      WHERE lgnum EQ <zwm028>-lgnum
        AND refnr EQ <zwm028>-refnr
        AND zlock EQ '1'.

    lv_zlock = <zwm028>-zlock.

    SELECT mandt UP TO 1 ROWS
      FROM zwm001 INTO sy-mandt
      WHERE armazem EQ is_zwm011-armazem
        AND processo EQ c_process_change_order
        AND parametro EQ c_param_bin_type
        AND valor EQ is_zwm011-ultimo_tipo_dep.
    ENDSELECT.
    IF sy-subrc EQ 0.
      SORT lt_zwm028 BY ordem DESCENDING.
    ELSE.
      SORT lt_zwm028 BY ordem ASCENDING.
    ENDIF.

    READ TABLE lt_zwm028 ASSIGNING <zwm028> INDEX 1.
    IF sy-subrc EQ 0.
      <zwm028>-zlock = lv_zlock.
      c_unlock_vb = <zwm028>-remessa.
      APPEND <zwm028> TO ct_zwm028.
    ENDIF.
  ENDIF.

ENDFORM.                    "F_DATA_UPDATE_DCK
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0010_UCOMM_NEXT_PUL
*&---------------------------------------------------------------------*
FORM f_data_scr0010_ucomm_next_pul USING is_ltak      TYPE ltak
                                         is_ltap      TYPE ltap
                                         is_zwm011    TYPE zwm011
                                         is_zwmfrt005 TYPE zwmfrt005
                                         i_bname      TYPE xubname
                                         i_equipment  TYPE char20
                                         i_lgpla      TYPE char14
                                         i_pul        TYPE numc2
                                   CHANGING c_subrc TYPE sysubrc.

  DATA lt_ltak          TYPE TABLE OF ltak.
  DATA lt_ltap          TYPE TABLE OF ltap.
  DATA lt_zwm011_ins    TYPE TABLE OF zwm011.
  DATA lt_zwm011_del    TYPE TABLE OF zwm011.
  DATA lt_zwm013_ins    TYPE TABLE OF zwm013.
  DATA lt_zwm013_upd    TYPE TABLE OF zwm013.
  DATA lt_zwm020_del    TYPE TABLE OF zwm020.
  DATA lt_zwm028_upd    TYPE TABLE OF zwm028.
  DATA lt_zwmfrt004_del TYPE TABLE OF zwmfrt004.
  DATA lt_zwmfrt005_del TYPE TABLE OF zwmfrt005.

  DATA ls_bdcmsgcoll TYPE bdcmsgcoll.
  DATA ls_zwm013     TYPE zwm013.

  DATA lv_keyword   TYPE keywords.
  DATA lv_lock      TYPE abap_bool.
  DATA lv_msg       TYPE string.
  DATA lv_unlock_r  TYPE abap_bool.
  DATA lv_unlock_vb TYPE vbeln.

  FIELD-SYMBOLS <zwm013> TYPE zwm013.

  CONCATENATE sy-mandt is_ltak-refnr INTO lv_keyword.

  PERFORM f_enqueue_keyword
    USING lv_keyword
          3
    CHANGING lv_lock.
  IF lv_lock EQ abap_true.
    APPEND is_ltak TO lt_ltak.

    PERFORM f_data_update_pul
      TABLES lt_zwm028_upd
      USING is_ltak
            is_zwm011
      CHANGING lv_unlock_r
               lv_unlock_vb.

    SELECT SINGLE *
      FROM zwm013
      INTO ls_zwm013
      WHERE armazem EQ is_ltap-lgnum
        AND sscc EQ is_ltap-vlenr.
    IF sy-subrc EQ 0.
      APPEND ls_zwm013 TO lt_zwm013_upd ASSIGNING <zwm013>.
      <zwm013>-destino = i_lgpla.
      <zwm013>-bloqueado = 'X'.
      <zwm013>-tipo_palete = is_ltap-letyp.
      <zwm013>-posicao_pulmao = i_pul.
    ELSE.
      APPEND INITIAL LINE TO lt_zwm013_ins ASSIGNING <zwm013>.
      <zwm013>-armazem = is_ltap-lgnum.
      <zwm013>-sscc = is_ltap-vlenr.
      <zwm013>-destino = i_lgpla.
      <zwm013>-bloqueado = 'X'.
      <zwm013>-tipo_palete = is_ltap-letyp.
      <zwm013>-posicao_pulmao = i_pul.
    ENDIF.

    " call subprocess 3.
    PERFORM f_data_scr0010_ucomm_aut_rmt
      TABLES lt_ltak
             lt_ltap
             lt_zwm011_ins
             lt_zwm013_ins
             lt_zwm013_upd
             lt_zwm020_del
             lt_zwm028_upd
      USING is_ltak
            is_ltap
            is_zwm011
            i_bname
            i_equipment
            i_lgpla
            c_param_queue_aut_sd_pul
            i_pul
      CHANGING ls_bdcmsgcoll
               c_subrc
               lv_unlock_r.
    IF c_subrc EQ 0.
      APPEND is_zwm011 TO lt_zwm011_del.

      APPEND is_zwmfrt005 TO lt_zwmfrt005_del.

      PERFORM f_update_zwmfrt004
        TABLES lt_zwmfrt004_del
               lt_ltak
               lt_zwm028_upd
               lt_zwm013_ins
               lt_zwm013_upd
        USING is_ltap
              is_zwmfrt005
              c_param_queue_aut_sd_pul.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWM011' IN UPDATE TASK
        TABLES
          it_zwm011_del = lt_zwm011_del
          it_zwm011_ins = lt_zwm011_ins.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWM013' IN UPDATE TASK
        TABLES
          it_zwm013_ins = lt_zwm013_ins
          it_zwm013_upd = lt_zwm013_upd.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWM020' IN UPDATE TASK
        TABLES
          it_zwm020_del = lt_zwm020_del.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWM028' IN UPDATE TASK
        TABLES
          it_zwm028_upd = lt_zwm028_upd.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWMFRT004' IN UPDATE TASK
        TABLES
          it_zwmfrt004_del = lt_zwmfrt004_del.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWMFRT005' IN UPDATE TASK
        TABLES
          it_zwmfrt005_del = lt_zwmfrt005_del.

      IF lv_unlock_r EQ abap_true.
        CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK_SAVE' IN UPDATE TASK
          EXPORTING
            i_lgnum  = is_ltak-lgnum
            i_refnr  = is_ltak-refnr
            i_vbeln  = lv_unlock_vb
            i_commit = ' '.
      ENDIF.

      PERFORM f_commit_work USING 'X' 2.
      PERFORM f_dequeue_keyword USING lv_keyword.

      IF lv_unlock_r EQ abap_true.
        CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
          EXPORTING
            i_lgnum  = is_ltak-lgnum
            i_refnr  = is_ltak-refnr
            i_vbeln  = lv_unlock_vb
            i_commit = 'X'.
      ENDIF.
    ELSE.
      c_subrc = 4.

      PERFORM f_rollback.
      PERFORM f_dequeue_keyword USING lv_keyword.
    ENDIF.
  ELSE.
    c_subrc = 4.

    "Acção bloqueada pelo sistema. Tentar novamente
    MESSAGE e064(zwmfr001) INTO lv_msg.

    ls_bdcmsgcoll-msgid = sy-msgid.
    ls_bdcmsgcoll-msgtyp = sy-msgty.
    ls_bdcmsgcoll-msgnr = sy-msgno.
  ENDIF.

  IF c_subrc = 4.
    PERFORM f_show_message USING ls_bdcmsgcoll.
  ENDIF.
ENDFORM.                    "F_DATA_SCR0010_NEXT_SAVE_PUL
*&---------------------------------------------------------------------*
*&      Form  F_DATA_UPDATE_PUL
*&---------------------------------------------------------------------*
FORM f_data_update_pul TABLES ct_zwm028 STRUCTURE zwm028
                       USING is_ltak   TYPE ltak
                             is_zwm011 TYPE zwm011
                       CHANGING c_unlock_r  TYPE abap_bool
                                c_unlock_vb TYPE vbeln.

  DATA lt_zwm028 TYPE TABLE OF zwm028.

  DATA ls_zwm028 TYPE zwm028.

  DATA lv_zlock TYPE zlock.

  FIELD-SYMBOLS <zwm028> TYPE zwm028.

  PERFORM get_to_vbeln CHANGING is_ltak.

  READ TABLE ct_zwm028 ASSIGNING <zwm028>
    WITH KEY lgnum = is_ltak-lgnum
             refnr = is_ltak-refnr
             remessa = is_ltak-vbeln.
  IF sy-subrc NE 0.
    SELECT SINGLE *
      FROM zwm028 INTO ls_zwm028
      WHERE lgnum EQ is_ltak-lgnum
        AND refnr EQ is_ltak-refnr
        AND remessa EQ is_ltak-vbeln.
    IF sy-subrc EQ 0.
      APPEND ls_zwm028 TO ct_zwm028 ASSIGNING <zwm028>.
      <zwm028>-paletes_pulmao = <zwm028>-paletes_pulmao + 1.
    ENDIF.
  ELSE.
    <zwm028>-paletes_pulmao = <zwm028>-paletes_pulmao + 1.
  ENDIF.

  " check type lock 'R'
  IF <zwm028>-tipo_lock EQ 'R'.
    IF <zwm028>-paletes_pulmao EQ <zwm028>-total_paletes.
      c_unlock_r = abap_true.
    ENDIF.
  ENDIF.

  READ TABLE ct_zwm028 ASSIGNING <zwm028>
    WITH KEY lgnum = is_ltak-lgnum
             refnr = is_ltak-refnr
             remessa = space.
  IF sy-subrc NE 0.
    SELECT SINGLE *
      FROM zwm028 INTO ls_zwm028
      WHERE lgnum EQ is_ltak-lgnum
        AND refnr EQ is_ltak-refnr
        AND remessa EQ space.
    IF sy-subrc EQ 0.
      APPEND ls_zwm028 TO ct_zwm028 ASSIGNING <zwm028>.
      <zwm028>-paletes_pulmao = <zwm028>-paletes_pulmao + 1.
    ENDIF.
  ELSE.
    <zwm028>-paletes_pulmao = <zwm028>-paletes_pulmao + 1.
  ENDIF.

  IF c_unlock_r EQ abap_true.
    SELECT *
      FROM zwm028
      INTO TABLE lt_zwm028
      WHERE lgnum EQ <zwm028>-lgnum
        AND refnr EQ <zwm028>-refnr
        AND zlock EQ '1'.

    lv_zlock = <zwm028>-zlock.

    SELECT mandt UP TO 1 ROWS
      FROM zwm001 INTO sy-mandt
      WHERE armazem EQ is_zwm011-armazem
        AND processo EQ c_process_change_order
        AND parametro EQ c_param_bin_type
        AND valor EQ is_zwm011-ultimo_tipo_dep.
    ENDSELECT.
    IF sy-subrc EQ 0.
      SORT lt_zwm028 BY ordem DESCENDING.
    ELSE.
      SORT lt_zwm028 BY ordem ASCENDING.
    ENDIF.

    READ TABLE lt_zwm028 ASSIGNING <zwm028> INDEX 1.
    IF sy-subrc EQ 0.
      <zwm028>-zlock = lv_zlock.
      c_unlock_vb = <zwm028>-remessa.
      APPEND <zwm028> TO ct_zwm028.
    ENDIF.
  ENDIF.
ENDFORM.                    "F_DATA_UPDATE_PUL
*&---------------------------------------------------------------------*
*&      Form  F_DATA_L_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM f_data_l_to_confirm TABLES it_ltap STRUCTURE ltap
                         USING i_qunkz TYPE rl03t-quknz
                         CHANGING c_subrc       TYPE sysubrc
                                  cs_bdcmsgcoll TYPE bdcmsgcoll.

  DATA lt_ltap_conf TYPE STANDARD TABLE OF ltap_conf.

  DATA: lv_subrc TYPE sysubrc,
        lv_subst TYPE rl03tsubst.

  FIELD-SYMBOLS <ltap>      TYPE ltap.
  FIELD-SYMBOLS <ltap_conf> LIKE LINE OF lt_ltap_conf[].

  SORT it_ltap BY tanum tapos.

  CLEAR: lv_subst.
  LOOP AT it_ltap ASSIGNING <ltap>.
    AT NEW tanum.
      FREE lt_ltap_conf.
    ENDAT.

    APPEND INITIAL LINE TO lt_ltap_conf[] ASSIGNING <ltap_conf>.
    <ltap_conf>-tanum  = <ltap>-tanum.
    <ltap_conf>-tapos  = <ltap>-tapos.
    <ltap_conf>-altme  = <ltap>-altme.
    <ltap_conf>-nista  = <ltap>-vsola.

    IF <ltap>-kzsub EQ abap_true.
      lv_subst = abap_true.
    ENDIF.

    AT END OF tanum.
      CALL FUNCTION 'L_TO_CONFIRM'
        EXPORTING
          i_lgnum                        = <ltap>-lgnum
          i_tanum                        = <ltap>-tanum
          i_quknz                        = i_qunkz
          i_subst                        = lv_subst
          i_update_task                  = ' '
          i_commit_work                  = ' '
        TABLES
          t_ltap_conf                    = lt_ltap_conf[]
        EXCEPTIONS
          to_confirmed                   = 1
          to_doesnt_exist                = 2
          item_confirmed                 = 3
          item_subsystem                 = 4
          item_doesnt_exist              = 5
          item_without_zero_stock_check  = 6
          item_with_zero_stock_check     = 7
          one_item_with_zero_stock_check = 8
          item_su_bulk_storage           = 9
          item_no_su_bulk_storage        = 10
          one_item_su_bulk_storage       = 11
          foreign_lock                   = 12
          squit_or_quantities            = 13
          vquit_or_quantities            = 14
          bquit_or_quantities            = 15
          quantity_wrong                 = 16
          double_lines                   = 17
          kzdif_wrong                    = 18
          no_difference                  = 19
          no_negative_quantities         = 20
          wrong_zero_stock_check         = 21
          su_not_found                   = 22
          no_stock_on_su                 = 23
          su_wrong                       = 24
          too_many_su                    = 25
          nothing_to_do                  = 26
          no_unit_of_measure             = 27
          xfeld_wrong                    = 28
          update_without_commit          = 29
          no_authority                   = 30
          lqnum_missing                  = 31
          charg_missing                  = 32
          no_sobkz                       = 33
          no_charg                       = 34
          nlpla_wrong                    = 35
          two_step_confirmation_required = 36
          two_step_conf_not_allowed      = 37
          pick_confirmation_missing      = 38
          quknz_wrong                    = 39
          hu_data_wrong                  = 40
          no_hu_data_required            = 41
          hu_data_missing                = 42
          hu_not_found                   = 43
          picking_of_hu_not_possible     = 44
          not_enough_stock_in_hu         = 45
          serial_number_data_wrong       = 46
          serial_numbers_not_required    = 47
          no_differences_allowed         = 48
          serial_number_not_available    = 49
          serial_number_data_missing     = 50
          to_item_split_not_allowed      = 51
          input_wrong                    = 52
          error_message                  = 99
          OTHERS                         = 53.
      IF sy-subrc NE 0.
        lv_subrc = 4.

        cs_bdcmsgcoll-msgid  = sy-msgid.
        cs_bdcmsgcoll-msgtyp  = sy-msgty.
        cs_bdcmsgcoll-msgnr  = sy-msgno.
        cs_bdcmsgcoll-msgv1  = sy-msgv1.
        cs_bdcmsgcoll-msgv2  = sy-msgv2.
        cs_bdcmsgcoll-msgv3  = sy-msgv3.
        cs_bdcmsgcoll-msgv4  = sy-msgv4.

        EXIT.
      ENDIF.

    ENDAT.
  ENDLOOP.

  c_subrc = lv_subrc.

ENDFORM.                    " F_DATA_L_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0010_UCOMM_AUT_RMT
*&---------------------------------------------------------------------*
FORM f_data_scr0010_ucomm_aut_rmt TABLES ct_ltak       STRUCTURE ltak
                                         ct_ltap       STRUCTURE ltap
                                         ct_zwm011_ins STRUCTURE zwm011
                                         ct_zwm013_ins STRUCTURE zwm013
                                         ct_zwm013_upd STRUCTURE zwm013
                                         ct_zwm020_del STRUCTURE zwm020
                                         ct_zwm028_upd STRUCTURE zwm028
                                  USING is_ltak     TYPE ltak
                                        is_ltap     TYPE ltap
                                        is_zwm011   TYPE zwm011
                                        i_bname     TYPE xubname
                                        i_equipment TYPE char20
                                        i_lgpla     TYPE char14
                                        i_process   TYPE zwmmp_parametro
                                        i_pul       TYPE numc2
                                  CHANGING cs_bdcmsgcoll TYPE bdcmsgcoll
                                           c_subrc       TYPE sysubrc
                                           c_unlock_r    TYPE abap_bool.

  DATA lt_ltap TYPE STANDARD TABLE OF ltap.

  DATA ls_ltak   TYPE ltak.
  DATA ls_ltap   TYPE ltap.
  DATA ls_zwm013 TYPE zwm013.
  DATA ls_zwm020 TYPE zwm020.

  DATA lv_exist TYPE abap_bool.
  DATA lv_queue TYPE zwm_valor.
  DATA lv_valid TYPE abap_bool.

  FIELD-SYMBOLS <zwm011> TYPE zwm011.
  FIELD-SYMBOLS <zwm013> TYPE zwm013.

  PERFORM f_get_remount_su_data
    USING is_ltap
    CHANGING ls_ltak
             ls_ltap
             ls_zwm020
             lv_exist.

  IF lv_exist EQ abap_true.
    PERFORM f_is_auto_remount_valid
     USING is_ltak
           ls_ltak
     CHANGING lv_valid.

    IF lv_valid EQ abap_true.
      APPEND ls_ltak TO ct_ltak.

      CASE i_process.
        WHEN c_param_queue_aut_sd_pul.
*          PERFORM f_data_update_pul
*            TABLES ct_zwm028_upd
*            USING ls_ltak
*                  is_zwm011
*            CHANGING c_unlock_r.

          SELECT SINGLE *
            FROM zwm013
            INTO ls_zwm013
            WHERE armazem EQ ls_ltap-lgnum
              AND sscc EQ ls_ltap-vlenr.
          IF sy-subrc EQ 0.
            APPEND ls_zwm013 TO ct_zwm013_upd ASSIGNING <zwm013>.
            <zwm013>-destino = i_lgpla.
            <zwm013>-bloqueado = 'X'.
            <zwm013>-tipo_palete = is_ltap-letyp.
            <zwm013>-posicao_pulmao = i_pul.
          ELSE.
            APPEND INITIAL LINE TO ct_zwm013_ins ASSIGNING <zwm013>.
            <zwm013>-armazem = ls_ltap-lgnum.
            <zwm013>-sscc = ls_ltap-vlenr.
            <zwm013>-destino = i_lgpla.
            <zwm013>-bloqueado = 'X'.
            <zwm013>-tipo_palete = is_ltap-letyp.
            <zwm013>-posicao_pulmao = i_pul.
          ENDIF.

*          APPEND ls_zwm020 TO ct_zwm020_del.

        WHEN c_param_queue_aut_sd_dck.
          APPEND ls_ltap TO ct_ltap.

*          PERFORM f_data_update_dck
*            TABLES ct_zwm028_upd
*            USING ls_ltak
*                  is_zwm011
*            CHANGING c_unlock_r.

*          APPEND ls_zwm020 TO ct_zwm020_del.
        WHEN OTHERS.
          APPEND ls_ltap TO ct_ltap.

*          APPEND ls_zwm020 TO ct_zwm020_del.
      ENDCASE.
    ELSE.
      c_subrc = 0.

      APPEND INITIAL LINE TO ct_zwm011_ins ASSIGNING <zwm011>.
      <zwm011>-armazem = ls_ltap-lgnum.
      <zwm011>-to_number = ls_ltap-tanum.
      <zwm011>-to_item = ls_ltap-tapos.
      <zwm011>-user_name = i_bname.
      <zwm011>-equipamento = i_equipment.
      <zwm011>-status = 'P'.
      <zwm011>-queue = ls_ltak-queue.
      <zwm011>-ultimo_tipo_dep = ls_ltap-nltyp.
      <zwm011>-ultimo_bin = ls_ltap-nlpla.

      APPEND ls_zwm020 TO ct_zwm020_del.
    ENDIF.
  ELSE.
    c_subrc = 0.

    " erroneous entry must be deleted
    IF ls_zwm020 IS NOT INITIAL.
      APPEND ls_zwm020 TO ct_zwm020_del.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_DATA_SCR0010_UCOMM_AUT_RMT
*&---------------------------------------------------------------------*
*&      Form  F_GET_ACTIVE_USERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_UINFOS  text
*----------------------------------------------------------------------*
FORM f_get_active_users  TABLES et_uinfos STRUCTURE uinfos.
  DATA lt_uinfos TYPE TABLE OF uinfos.

** get/clean user sessions/assignments
** get user sessions

  CALL FUNCTION 'TH_SYSTEMWIDE_USER_LIST'
    TABLES
      list = lt_uinfos.

  DELETE lt_uinfos WHERE bname IS INITIAL.
  SORT lt_uinfos BY bname.
  DELETE ADJACENT DUPLICATES FROM lt_uinfos COMPARING bname.

  et_uinfos[] = lt_uinfos[].
ENDFORM.                    " F_GET_ACTIVE_USERS
*&---------------------------------------------------------------------*
*&      Form  F_GET_ASSIGN_TOS_FOR_WAREHOUSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZWM011  text
*      -->P_GS_XUSER_LGNUM  text
*----------------------------------------------------------------------*
FORM f_get_assign_tos_for_warehouse TABLES et_zwm011 STRUCTURE zwm011
                                    USING i_lgnum     TYPE lgnum
                                          i_equipment TYPE char20.
  DATA lt_zwm011 TYPE TABLE OF zwm011.

  SELECT *
    FROM zwm011
    INTO TABLE lt_zwm011
    WHERE armazem EQ i_lgnum.

* we are only interested in entries assign to our equipment
  IF i_equipment IS NOT INITIAL.
    DELETE lt_zwm011 WHERE equipamento NE i_equipment.
  ENDIF.

  et_zwm011[] = lt_zwm011[].
ENDFORM.                    " F_GET_ASSIGN_TOS_FOR_WAREHOUSE
*&---------------------------------------------------------------------*
*&      Form  F_REMOVE_LTAP_ASSIGN_TOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZWM011  text
*      -->P_LT_LTAP  text
*      -->P_LT_LTAP_ASSIGN  text
*----------------------------------------------------------------------*
FORM f_remove_ltap_assign_tos TABLES it_zwm011      STRUCTURE zwm011
                                     it_ltap        STRUCTURE ltap
                                     et_ltap_assign STRUCTURE ltap.

  DATA lt_tabix TYPE TABLE OF sytabix.

  DATA lv_tabix TYPE sytabix.

  FIELD-SYMBOLS <ltap>   TYPE ltap.
  FIELD-SYMBOLS <zwm011> TYPE zwm011.

  SORT it_ltap BY tanum ASCENDING tapos ASCENDING.

  LOOP AT it_zwm011 ASSIGNING <zwm011>.
    READ TABLE it_ltap ASSIGNING <ltap>
      WITH KEY tanum = <zwm011>-to_number
               tapos = <zwm011>-to_item
      BINARY SEARCH.
    IF sy-subrc EQ 0.
      APPEND sy-tabix TO lt_tabix.

      APPEND <ltap> TO et_ltap_assign.
    ENDIF.
  ENDLOOP.

  IF lt_tabix IS NOT INITIAL .
    SORT lt_tabix DESCENDING.

    LOOP AT lt_tabix INTO lv_tabix.
      DELETE it_ltap INDEX lv_tabix.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " F_REMOVE_LTAP_ASSIGN_TOS
*&---------------------------------------------------------------------*
*&      Form  F_REMOVE_ZWMFRT005_ASSIGN_TOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_ZWMFRT005  text
*      -->IT_LTAP_ASSIGN  text
*      -->ET_ZWMRFT005_DEL  text
*----------------------------------------------------------------------*
FORM f_remove_zwmfrt005_assign_tos TABLES it_ltap          STRUCTURE ltap
                                          it_ltap_assign   STRUCTURE ltap
                                          et_zwmfrt005_del STRUCTURE zwmfrt005
                                   USING it_zwmfrt005      TYPE ty_st_zwmfrt005.


  DATA lt_tabix TYPE TABLE OF sytabix.

  DATA lv_tabix TYPE sytabix.

  FIELD-SYMBOLS <zwmfrt005> TYPE zwmfrt005.

  SORT it_ltap BY vlenr ASCENDING tanum ASCENDING tapos ASCENDING.
  SORT it_ltap_assign BY vlenr ASCENDING tanum ASCENDING tapos ASCENDING.

  LOOP AT it_zwmfrt005 ASSIGNING <zwmfrt005>.
    lv_tabix = sy-tabix.

    READ TABLE it_ltap TRANSPORTING NO FIELDS
      WITH KEY vlenr = <zwmfrt005>-lenum
      BINARY SEARCH.
    IF sy-subrc NE 0.
      READ TABLE it_ltap_assign TRANSPORTING NO FIELDS
        WITH KEY vlenr = <zwmfrt005>-lenum
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF <zwmfrt005>-picked EQ abap_true.
          APPEND lv_tabix TO lt_tabix.
        ENDIF.
      ELSE.
        " Inconsistency in the DB
        APPEND <zwmfrt005> TO et_zwmfrt005_del.

        APPEND lv_tabix TO lt_tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lt_tabix IS NOT INITIAL.
    SORT lt_tabix DESCENDING.

    LOOP AT lt_tabix INTO lv_tabix.
      DELETE it_zwmfrt005 INDEX lv_tabix.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " F_REMOVE_ZWMFRT005_ASSIGN_TOS
*&---------------------------------------------------------------------*
*&      Form  F_SET_FIELDS_0010
*&---------------------------------------------------------------------*
FORM f_set_fields_0010.
  CLEAR gs_texts.

  CASE gs_zwm011-ultimo_tipo_dep.
    WHEN 'REP' OR 'PKB'.
      " Screen Texts
      gs_texts-scr001_title      = text-013.
      gs_texts-scr001_nlpla      = text-032.
      gs_texts-scr001_npall_conf = text-033.
      gs_texts-scr001_lgpla      = text-015.
      gs_texts-scr001_npall_load = space.
      gs_texts-scr001_lgpla_conf = text-016.

      " Screen Variables
      CONCATENATE 'AUT' gs_zwmfrt005-nlpla
        INTO gs_scr001-nlpla SEPARATED BY space.
      gs_scr001-vlenr = gs_ltap-vlenr.
      gs_scr001-matnr = gs_ltap-matnr.
      gs_scr001-maktx_1 = gs_makt-maktx(20).
      gs_scr001-maktx_2 = gs_makt-maktx+20(20).
*      gs_scr001-lenum = gs_zwmfrt005-lenum.

      IF gs_zwm011-status EQ 'P'.
        gs_scr001-lenum = gs_zwmfrt005-lenum.
        CONCATENATE gs_zwm011-ultimo_tipo_dep gs_zwm011-ultimo_bin
          INTO gs_scr001-lgpla SEPARATED BY space.
      ENDIF.
    WHEN 'PUL'.
      " Screen Texts
      gs_texts-scr001_title      = text-013.
      gs_texts-scr001_nlpla      = text-032.
      gs_texts-scr001_npall_conf = text-033.
      gs_texts-scr001_lgpla      = text-015.
      gs_texts-scr001_npall_load = text-017.
      gs_texts-scr001_lgpla_conf = text-016.

      " Screen Variables
      CONCATENATE 'AUT' gs_zwmfrt005-nlpla
        INTO gs_scr001-nlpla SEPARATED BY space.
      gs_scr001-vlenr = gs_ltap-vlenr.
      gs_scr001-matnr = gs_ltap-matnr.
      gs_scr001-maktx_1 = gs_makt-maktx(20).
      gs_scr001-maktx_2 = gs_makt-maktx+20(20).

      IF gs_zwm011-status EQ 'P'.
        gs_scr001-lenum = gs_zwmfrt005-lenum.
        CONCATENATE gs_zwm011-ultimo_tipo_dep gs_zwm011-ultimo_bin
          INTO gs_scr001-lgpla SEPARATED BY space.

        gs_scr001-kober = gs_zwm028-kober.
        gs_scr001-npall = gs_zwm028-total_paletes.
      ENDIF.
    WHEN 'DCK'.
      " Screen Texts
      gs_texts-scr001_title      = text-013.
      gs_texts-scr001_nlpla      = text-032.
      gs_texts-scr001_npall_conf = text-033.
      gs_texts-scr001_lgpla      = text-015.
      gs_texts-scr001_npall_load = text-017.
      gs_texts-scr001_lgpla_conf = text-016.

      " Screen Variables
      CONCATENATE 'AUT' gs_zwmfrt005-nlpla
        INTO gs_scr001-nlpla SEPARATED BY space.
      gs_scr001-vlenr = gs_ltap-vlenr.
      gs_scr001-matnr = gs_ltap-matnr.
      gs_scr001-maktx_1 = gs_makt-maktx(20).
      gs_scr001-maktx_2 = gs_makt-maktx+20(20).

      IF gs_zwm011-status EQ 'P'.
        gs_scr001-lenum = gs_zwmfrt005-lenum.
        CONCATENATE gs_zwm011-ultimo_tipo_dep gs_zwm011-ultimo_bin
          INTO gs_scr001-lgpla SEPARATED BY space.

        gs_scr001-npall = gs_zwm028-total_paletes.
      ENDIF.
    WHEN 'PRM'.
      " Screen Texts
      gs_texts-scr001_title      = text-013.
      gs_texts-scr001_nlpla      = text-032.
      gs_texts-scr001_npall_conf = text-033.
      gs_texts-scr001_lgpla      = text-015.
      gs_texts-scr001_npall_load = space.
      gs_texts-scr001_lgpla_conf = text-016.

      " Screen Variables
      CONCATENATE 'AUT' gs_zwmfrt005-nlpla
        INTO gs_scr001-nlpla SEPARATED BY space.
      gs_scr001-matnr = gs_ltap-matnr.
      gs_scr001-maktx_1 = gs_makt-maktx(20).
      gs_scr001-maktx_2 = gs_makt-maktx+20(20).

      IF gs_zwm011-status EQ 'P'.
        gs_scr001-lenum = gs_ltap-vlenr.
        CONCATENATE gs_ltap-nltyp gs_ltap-nlpla
          INTO gs_scr001-lgpla SEPARATED BY space.
      ENDIF.
    WHEN OTHERS.
      "do nothing
  ENDCASE.
ENDFORM.                    " F_SET_FIELDS_0010
*&---------------------------------------------------------------------*
*&      Form  F_GET_AUT_TO
*&---------------------------------------------------------------------*
FORM f_get_aut_to USING is_xuser TYPE lrf_wkqu
                        i_equipment TYPE char20
                  CHANGING c_subrc TYPE sysubrc.
  DATA lt_ltap          TYPE STANDARD TABLE OF ltap.
  DATA lt_ltap_assign   TYPE STANDARD TABLE OF ltap.
  DATA lt_tabix         TYPE TABLE OF sytabix.
  DATA lt_uinfos        TYPE TABLE OF uinfos.
  DATA lt_zwm001        TYPE TABLE OF zwm001.
  DATA lt_zwm011        TYPE TABLE OF zwm011.
  DATA lt_zwm011_del    TYPE TABLE OF zwm011.
  DATA lt_zwm011_ins    TYPE TABLE OF zwm011.
  DATA lt_zwm028        TYPE STANDARD TABLE OF zwm028.
  DATA lt_zwm028_aux    TYPE STANDARD TABLE OF zwm028.
  DATA lt_zwm028_pri    TYPE STANDARD TABLE OF zwm028.
  DATA lt_zwmfrt004     TYPE SORTED TABLE OF zwmfrt004 WITH UNIQUE KEY refnr nlpla.
  DATA lt_zwmfrt004_aux TYPE STANDARD TABLE OF zwmfrt004.
  DATA lt_zwmfrt005     TYPE ty_st_zwmfrt005.
  DATA lt_zwmfrt005_aux TYPE STANDARD TABLE OF zwmfrt005.
  DATA lt_zwmfrt005_del TYPE STANDARD TABLE OF zwmfrt005.
  DATA lt_zwmfrt006     TYPE SORTED TABLE OF zwmfrt006 WITH UNIQUE KEY tapete.
  DATA lt_zwmfrt006_aux TYPE STANDARD TABLE OF zwmfrt006.

  DATA ls_bdcmsgcoll TYPE bdcmsgcoll.
  DATA ls_ltak       TYPE ltak.
  DATA ls_ltap       TYPE ltap.
  DATA ls_zwm028     TYPE zwm028.
  DATA ls_zwmfrt005  TYPE zwmfrt005.

  DATA lv_index       TYPE syindex.
  DATA lv_keyword     TYPE keywords.
  DATA lv_lock        TYPE abap_bool.
  DATA lv_msg         TYPE string.
  DATA lv_tabix       TYPE sytabix.
  DATA lv_to_assigned TYPE abap_bool.
  DATA lv_value       TYPE zwm_valor.

  FIELD-SYMBOLS <ltap>       TYPE ltap.
  FIELD-SYMBOLS <zwm001>     TYPE zwm001.
  FIELD-SYMBOLS <zwm011>     TYPE zwm011.
  FIELD-SYMBOLS <zwm028>     TYPE zwm028.
  FIELD-SYMBOLS <zwm028_pri> TYPE zwm028.
  FIELD-SYMBOLS <zwmfrt004>  TYPE zwmfrt004.
  FIELD-SYMBOLS <zwmfrt005>  TYPE zwmfrt005.
  FIELD-SYMBOLS <zwmfrt006>  TYPE zwmfrt006.

  CONCATENATE sy-mandt i_equipment INTO lv_keyword.

  " Get TO for user
  FREE: gs_ltak, gs_ltap, gs_makt, gs_zwm011, gs_zwm028, gs_zwmfrt005.
  FREE: gv_process.

  PERFORM f_enqueue_keyword
    USING lv_keyword
          10
          lv_lock.
  IF lv_lock EQ abap_true.
    " get hardcodes for program
    PERFORM f_data_init_get_hardcodes
      TABLES lt_zwm001
      USING is_xuser-lgnum.

    PERFORM f_get_active_users
      TABLES lt_uinfos.

    PERFORM f_get_assign_tos_for_warehouse
      TABLES lt_zwm011
      USING is_xuser-lgnum
            i_equipment.

** Get all TOs in table ZWM011 that are completely processed or
** that have an off-line user assigned to it
    LOOP AT lt_zwm011 ASSIGNING <zwm011>.
      lv_tabix = sy-tabix.

      CASE <zwm011>-status.
        WHEN 'T'. " Transfered - Completely processed
          APPEND <zwm011> TO lt_zwm011_del.
          APPEND lv_tabix TO lt_tabix.

        WHEN 'C'. " Created
          " check if user assign to ZWM011 is off-line
          READ TABLE lt_uinfos TRANSPORTING NO FIELDS
            WITH KEY bname = <zwm011>-user_name
            BINARY SEARCH.
          IF sy-subrc NE 0.
            APPEND <zwm011> TO lt_zwm011_del.
            APPEND lv_tabix TO lt_tabix.
          ENDIF.

        WHEN OTHERS. " Picked
          " do nothig
      ENDCASE.
    ENDLOOP.

    " after this point internal table LT_ZWM011 has all the TO's that are assign to online users
    IF lt_tabix IS NOT INITIAL.
      SORT lt_tabix DESCENDING.
      LOOP AT lt_tabix INTO lv_tabix.
        DELETE lt_zwm011 INDEX lv_tabix.
      ENDLOOP.
    ENDIF.

    READ TABLE lt_zwm011 ASSIGNING <zwm011>
      WITH KEY user_name = is_xuser-bname.
    IF sy-subrc EQ 0.
      lv_tabix = sy-tabix.

      SELECT SINGLE *
        FROM ltap
        INTO ls_ltap
        WHERE lgnum EQ <zwm011>-armazem
          AND tanum EQ <zwm011>-to_number
          AND tapos EQ <zwm011>-to_item
          AND pquit EQ space.
      IF sy-subrc EQ 0.
        lv_to_assigned = abap_true.

        SELECT SINGLE *
          FROM ltak
          INTO ls_ltak
          WHERE lgnum EQ ls_ltap-lgnum
            AND tanum EQ ls_ltap-tanum.

        SELECT * UP TO 1 ROWS
          FROM zwmfrt005
          INTO ls_zwmfrt005
          WHERE lgnum EQ ls_ltap-lgnum
            AND lenum EQ ls_ltap-vlenr.
        ENDSELECT.

        SELECT SINGLE *
          FROM zwm028
          INTO ls_zwm028
          WHERE lgnum EQ ls_zwmfrt005-lgnum
            AND refnr EQ ls_zwmfrt005-refnr
            AND remessa EQ space.

        gs_ltak = ls_ltak.
        gs_ltap = ls_ltap.

        gs_zwm011 = <zwm011>.
        gs_zwm028 = ls_zwm028.
        gs_zwmfrt005 = ls_zwmfrt005.
      ELSE.
        " delete entry in ZWM011 table
        APPEND <zwm011> TO lt_zwm011_del.
        DELETE lt_zwm011 INDEX lv_tabix.
      ENDIF.
    ENDIF.

    IF lv_to_assigned EQ abap_false.
      " determine TO
      SELECT *
        FROM zwmfrt006
        INTO TABLE lt_zwmfrt006
        WHERE lgnum EQ is_xuser-lgnum.
      IF sy-subrc EQ 0.
        SELECT *
          FROM zwmfrt005
          INTO TABLE lt_zwmfrt005
          FOR ALL ENTRIES IN lt_zwmfrt006
          WHERE lgnum EQ lt_zwmfrt006-lgnum
            AND nlpla EQ lt_zwmfrt006-tapete.
        IF sy-subrc EQ 0.
          SELECT *
            FROM ltap
            INTO TABLE lt_ltap
            FOR ALL ENTRIES IN lt_zwmfrt005
            WHERE lgnum EQ lt_zwmfrt005-lgnum
              AND vlenr EQ lt_zwmfrt005-lenum
              AND pquit EQ space.
        ENDIF.
      ENDIF.

      " remove assign TOs from LTAP
      PERFORM f_remove_ltap_assign_tos
        TABLES lt_zwm011
               lt_ltap
               lt_ltap_assign.

      " remove assign TOs of ZWMFRT005
      PERFORM f_remove_zwmfrt005_assign_tos
        TABLES lt_ltap
               lt_ltap_assign
               lt_zwmfrt005_del
        USING lt_zwmfrt005.

      IF lt_ltap IS NOT INITIAL.
        SELECT *
          FROM zwmfrt004
          INTO TABLE lt_zwmfrt004
          WHERE lgnum EQ is_xuser-lgnum.
        IF sy-subrc EQ 0.
          SELECT *
            FROM zwm028
            INTO TABLE lt_zwm028
            FOR ALL ENTRIES IN lt_zwmfrt004
            WHERE lgnum EQ lt_zwmfrt004-lgnum
              AND refnr EQ lt_zwmfrt004-refnr
              AND remessa EQ space.
        ENDIF.

        SORT lt_ltap BY vlenr tanum tapos.

        lt_zwm028_pri = lt_zwm028.
        SORT lt_zwm028_pri BY prioridade ASCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_zwm028_pri COMPARING prioridade.

        SORT lt_zwm028 BY prioridade DESCENDING.
        SORT lt_zwm028_pri BY prioridade DESCENDING.

        lv_index = 0.
        LOOP AT lt_zwm028_pri ASSIGNING <zwm028_pri>.
          FREE: lt_zwm028_aux, lt_zwmfrt004_aux, lt_zwmfrt005_aux, lt_zwmfrt006_aux.

          LOOP AT lt_zwm028 ASSIGNING <zwm028> FROM lv_index.
            IF <zwm028_pri>-prioridade GT <zwm028>-prioridade.
              EXIT.
            ENDIF.

            lv_index = lv_index + 1.

            IF <zwm028_pri>-prioridade EQ <zwm028>-prioridade.
              APPEND <zwm028> TO lt_zwm028_aux.
            ENDIF.
          ENDLOOP.

          " Get group treadmills
          SORT lt_zwm028_aux BY refnr ASCENDING .

          LOOP AT lt_zwm028_aux ASSIGNING <zwm028>.
            LOOP AT lt_zwmfrt004 ASSIGNING <zwmfrt004>
              WHERE refnr EQ <zwm028>-refnr.
              APPEND <zwmfrt004> TO lt_zwmfrt004_aux.
            ENDLOOP.
          ENDLOOP.

          LOOP AT lt_zwmfrt004_aux ASSIGNING <zwmfrt004>.
            READ TABLE lt_zwmfrt006 ASSIGNING <zwmfrt006>
              WITH TABLE KEY tapete = <zwmfrt004>-nlpla.
            IF sy-subrc EQ 0.
              APPEND <zwmfrt006> TO lt_zwmfrt006_aux.
            ENDIF.
          ENDLOOP.

          SORT lt_zwmfrt006_aux BY tapete ASCENDING.
          DELETE ADJACENT DUPLICATES FROM lt_zwmfrt006_aux COMPARING tapete.

          SORT lt_zwmfrt006_aux
            BY prioridade DESCENDING
               tapete ASCENDING.

          LOOP AT lt_zwmfrt006_aux ASSIGNING <zwmfrt006>.
            FREE lt_zwmfrt005_aux.

            LOOP AT lt_zwmfrt005 ASSIGNING <zwmfrt005>
              WHERE nlpla EQ <zwmfrt006>-tapete.
              APPEND <zwmfrt005> TO lt_zwmfrt005_aux.
            ENDLOOP.

            SORT lt_zwmfrt005_aux BY posnr ASCENDING.

            " read only the first entry on the treadmill - mouth
            READ TABLE lt_zwmfrt005_aux ASSIGNING <zwmfrt005> INDEX 1.
            IF sy-subrc EQ 0.
              READ TABLE lt_zwm028_aux ASSIGNING <zwm028>
                WITH KEY refnr = <zwmfrt005>-refnr
                BINARY SEARCH.
              IF sy-subrc EQ 0.
                READ TABLE lt_ltap INTO ls_ltap
                  WITH KEY vlenr = <zwmfrt005>-lenum
                  BINARY SEARCH.
                IF sy-subrc EQ 0.
                  SELECT SINGLE *
                    FROM ltak
                    INTO ls_ltak
                    WHERE lgnum EQ ls_ltap-lgnum
                      AND tanum EQ ls_ltap-tanum.

                  READ TABLE lt_zwm001 ASSIGNING <zwm001>
                    WITH KEY processo = c_process_queues_maint
                             valor = ls_ltak-queue.
                  IF sy-subrc EQ 0.
                    CASE <zwm001>-parametro.
                      WHEN c_param_queue_aut_rep.
                        lv_to_assigned = abap_true.

                        gs_ltak = ls_ltak.
                        gs_ltap = ls_ltap.

                        gs_zwm011-armazem = gs_ltap-lgnum.
                        gs_zwm011-to_number = gs_ltap-tanum.
                        gs_zwm011-to_item = gs_ltap-tapos.
                        gs_zwm011-user_name = gs_xuser-bname.
                        gs_zwm011-equipamento = i_equipment.
                        gs_zwm011-status = 'C'.
                        gs_zwm011-queue = gs_ltak-queue.
                        gs_zwm011-ultimo_tipo_dep = gs_ltap-nltyp.
                        gs_zwm011-ultimo_bin = gs_ltap-nlpla.

                        APPEND gs_zwm011 TO lt_zwm011_ins.

                        gs_zwm028 = <zwm028>.
                        gs_zwmfrt005 = <zwmfrt005>.

                      WHEN c_param_queue_aut_sd.
                        IF <zwm028>-st_pul IS NOT INITIAL.
                          lv_value = <zwm028>-st_pul.

                          READ TABLE lt_zwm001 ASSIGNING <zwm001>
                            WITH KEY processo = c_process_warehouse_ent
                                     parametro = c_param_queue_aut_sd_pul
                                     valor = lv_value.
                          IF sy-subrc EQ 0.
                            READ TABLE lt_zwm011 TRANSPORTING NO FIELDS
                              WITH KEY ultimo_bin = <zwm028>-pulmao1.
                            IF sy-subrc NE 0.
                              IF <zwm028>-pulmao2 IS NOT INITIAL.
                                READ TABLE lt_zwm011 TRANSPORTING NO FIELDS
                                  WITH KEY ultimo_bin = <zwm028>-pulmao2.
                                IF sy-subrc NE 0.
                                  lv_to_assigned = abap_true.
                                ENDIF.
                              ELSE.
                                lv_to_assigned = abap_true.
                              ENDIF.
                            ENDIF.

                            IF lv_to_assigned = abap_true.
                              gs_ltak = ls_ltak.
                              gs_ltap = ls_ltap.

                              gs_zwm011-armazem = gs_ltap-lgnum.
                              gs_zwm011-to_number = gs_ltap-tanum.
                              gs_zwm011-to_item = gs_ltap-tapos.
                              gs_zwm011-user_name = gs_xuser-bname.
                              gs_zwm011-equipamento = i_equipment.
                              gs_zwm011-status = 'C'.
                              gs_zwm011-queue = gs_ltak-queue.
                              gs_zwm011-ultimo_tipo_dep = lv_value.
                              gs_zwm011-ultimo_bin = <zwm028>-pulmao1.

                              APPEND gs_zwm011 TO lt_zwm011_ins.

                              gs_zwm028 = <zwm028>.
                              gs_zwmfrt005 = <zwmfrt005>.
                            ENDIF.
                          ENDIF.

                        ELSE.
                          IF <zwm028>-st_dck IS NOT INITIAL.
                            lv_value = <zwm028>-st_dck.

                            READ TABLE lt_zwm001 ASSIGNING <zwm001>
                              WITH KEY processo = c_process_warehouse_ent
                                       parametro = c_param_queue_aut_sd_dck
                                       valor = lv_value.
                            IF sy-subrc EQ 0.
                              lv_to_assigned = abap_true.

                              gs_ltak = ls_ltak.
                              gs_ltap = ls_ltap.

                              gs_zwm011-armazem = gs_ltap-lgnum.
                              gs_zwm011-to_number = gs_ltap-tanum.
                              gs_zwm011-to_item = gs_ltap-tapos.
                              gs_zwm011-user_name = gs_xuser-bname.
                              gs_zwm011-equipamento = i_equipment.
                              gs_zwm011-status = 'C'.
                              gs_zwm011-queue = gs_ltak-queue.
                              gs_zwm011-ultimo_tipo_dep = lv_value.
                              gs_zwm011-ultimo_bin = <zwm028>-porta.

                              APPEND gs_zwm011 TO lt_zwm011_ins.

                              gs_zwm028 = <zwm028>.
                              gs_zwmfrt005 = <zwmfrt005>.
                            ENDIF.
                          ENDIF.
                        ENDIF.

                      WHEN c_param_queue_aut_prm.
                        lv_to_assigned = abap_true.

                        gs_ltak = ls_ltak.
                        gs_ltap = ls_ltap.

                        gs_zwm011-armazem = gs_ltap-lgnum.
                        gs_zwm011-to_number = gs_ltap-tanum.
                        gs_zwm011-to_item = gs_ltap-tapos.
                        gs_zwm011-user_name = gs_xuser-bname.
                        gs_zwm011-equipamento = i_equipment.
                        gs_zwm011-status = 'C'.
                        gs_zwm011-queue = gs_ltak-queue.
                        gs_zwm011-ultimo_tipo_dep = gs_ltap-nltyp.
                        gs_zwm011-ultimo_bin = gs_ltap-nlpla.

                        APPEND gs_zwm011 TO lt_zwm011_ins.

                        gs_zwm028 = <zwm028>.
                        gs_zwmfrt005 = <zwmfrt005>.

                      WHEN OTHERS.
                        " do nothing
                    ENDCASE.
                  ENDIF.

                ELSE.
                  " Inconsistency in table ZWMFRT005
                  APPEND <zwmfrt005> TO lt_zwmfrt005_del.
                ENDIF.
              ENDIF.
            ENDIF.

            IF lv_to_assigned EQ abap_true.
              EXIT.
            ENDIF.
          ENDLOOP.

          IF lv_to_assigned EQ abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    " update table ZWM011
    IF lt_zwm011_ins IS NOT INITIAL OR
       lt_zwm011_del IS NOT INITIAL.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWM011' IN UPDATE TASK
        TABLES
          it_zwm011_del = lt_zwm011_del
          it_zwm011_ins = lt_zwm011_ins.

      PERFORM f_commit_work USING 'X' 0.
    ENDIF.

    PERFORM f_dequeue_keyword
      USING lv_keyword.

    IF lv_to_assigned = abap_false.
      c_subrc = 4.

      FREE: gs_ltak, gs_ltap, gs_makt, gs_zwm011, gs_zwm028, gs_zwmfrt005.
      FREE: gv_process.

      MESSAGE e032(zwmmsg001) INTO lv_msg.

      ls_bdcmsgcoll-msgid = sy-msgid.
      ls_bdcmsgcoll-msgtyp = sy-msgty.
      ls_bdcmsgcoll-msgnr = sy-msgno.

      PERFORM f_show_message USING ls_bdcmsgcoll.
    ELSE.
      c_subrc = 0.

      SELECT SINGLE *
        FROM makt
        INTO gs_makt
        WHERE matnr EQ gs_ltap-matnr
          AND spras EQ sy-langu.

      CASE gs_zwm011-ultimo_tipo_dep.
        WHEN 'REP' OR 'PKB'.
          gv_process = c_param_queue_aut_rep.
        WHEN 'PUL'.
          gv_process = c_param_queue_aut_sd_pul.
        WHEN 'DCK'.
          gv_process = c_param_queue_aut_sd_dck.
        WHEN 'PRM'.
          gv_process = c_param_queue_aut_prm.
        WHEN OTHERS.
          " process not specified
      ENDCASE.
    ENDIF.

  ELSE.
    c_subrc = 4.

    FREE: gs_ltak, gs_ltap, gs_makt, gs_zwm011, gs_zwm028, gs_zwmfrt005.
    FREE: gv_process.

    "Acção bloqueada pelo sistema. Tentar novamente
    MESSAGE e064(zwmfr001) INTO lv_msg.

    ls_bdcmsgcoll-msgid = sy-msgid.
    ls_bdcmsgcoll-msgtyp = sy-msgty.
    ls_bdcmsgcoll-msgnr = sy-msgno.

    PERFORM f_show_message USING ls_bdcmsgcoll.
  ENDIF.
ENDFORM.                    " F_GET_AUT_TO
*&---------------------------------------------------------------------*
*&      Form  F_ENQUEUE_KEYWORD
*&---------------------------------------------------------------------*
FORM f_enqueue_keyword USING i_keyword TYPE keywords
                             i_times   TYPE i
                       CHANGING c_lock TYPE abap_bool.
  DATA lv_lock  TYPE abap_bool.
  DATA lv_count TYPE i.

  " Enqueue keyword equipment
  DO.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = i_keyword
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      lv_lock = abap_true.
      EXIT.
    ELSE.
      lv_count = lv_count + 1.

      IF lv_count LE i_times .
        WAIT UP TO 1 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDDO.

  c_lock = lv_lock.
ENDFORM.                    " F_ENQUEUE_KEYWORD
*&---------------------------------------------------------------------*
*&      Form  F_DEQUEUE_KEYWORD
*&---------------------------------------------------------------------*
FORM f_dequeue_keyword USING i_keyword TYPE keywords.
  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = i_keyword
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
ENDFORM.                    " F_DEQUEUE_KEYWORD
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_MESSAGE
*&---------------------------------------------------------------------*
FORM f_show_message USING is_bdcmsgcoll TYPE bdcmsgcoll.

  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = is_bdcmsgcoll-msgid
      message_lang   = sy-langu
      message_type   = is_bdcmsgcoll-msgtyp
      message_number = is_bdcmsgcoll-msgnr
      message_var1   = is_bdcmsgcoll-msgv1
      message_var2   = is_bdcmsgcoll-msgv2
      message_var3   = is_bdcmsgcoll-msgv3
      message_var4   = is_bdcmsgcoll-msgv4.

ENDFORM.                    "F_SHOW_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  F_COMMIT_WORK
*&---------------------------------------------------------------------*
FORM f_commit_work USING i_wait TYPE bapiwait
                         i_time TYPE i.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = i_wait.

  IF i_time NE 0.
    WAIT UP TO i_time SECONDS.
  ENDIF.
ENDFORM.                    "F_COMMIT_WORK

*&---------------------------------------------------------------------*
*&      Form  F_ROLLBACK
*&---------------------------------------------------------------------*
FORM f_rollback.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ENDFORM.                    "F_ROLLBACK
*&---------------------------------------------------------------------*
*&      Form  F_GET_LUNG_BIN
*&---------------------------------------------------------------------*
FORM f_get_lung_bin  USING is_ltak     TYPE ltak
                           i_equipment TYPE char20
                     CHANGING c_lgpla TYPE lgpla.

  DATA ls_zwm028 TYPE zwm028.

  DATA lv_lung_pal TYPE numc2.
  DATA lv_max_pal  TYPE numc2.
  DATA lv_value    TYPE zwm_valor.

  SELECT SINGLE *
    FROM zwm028
    INTO ls_zwm028
    WHERE lgnum EQ is_ltak-lgnum
      AND refnr EQ is_ltak-refnr
      AND remessa EQ space.
  CHECK sy-subrc EQ 0.

  lv_lung_pal = ls_zwm028-paletes_pulmao.

  PERFORM f_get_parameter
    USING is_ltak-lgnum
          c_process_lung
          c_param_max_pal
    CHANGING lv_value.

  IF lv_value IS NOT INITIAL .
    IF lv_value(2) CO '123456789'.
      lv_max_pal = lv_value.
    ELSE.
      lv_max_pal = 0.
    ENDIF.
  ELSE.
    lv_max_pal = 0.
  ENDIF.

  IF lv_lung_pal < lv_max_pal.
    c_lgpla = ls_zwm028-pulmao1.
  ELSE.
    c_lgpla = ls_zwm028-pulmao2.
  ENDIF.
ENDFORM.                    " F_GET_LUNG_BIN
*&---------------------------------------------------------------------*
*&      Form  F_GET_LUNG_FREE_POSITION
*&---------------------------------------------------------------------*
FORM f_get_lung_free_position  USING is_ltak TYPE ltak
                               CHANGING c_pul TYPE numc2.

  DATA lv_max_pal TYPE numc2.
  DATA lv_pos     TYPE numc2.
  DATA lv_pul_pal TYPE numc2.
  DATA lv_pul_pos TYPE numc2.
  DATA lv_value   TYPE zwm_valor.

  PERFORM f_get_parameter
    USING is_ltak-lgnum
          c_process_lung
          c_param_max_pal
    CHANGING lv_value.

  IF lv_value IS NOT INITIAL.
    IF lv_value(2) CO '1234567890'.
      lv_max_pal = lv_value.

      SELECT SINGLE paletes_pulmao
        FROM zwm028
        INTO lv_pul_pal
        WHERE lgnum EQ is_ltak-lgnum
          AND refnr EQ is_ltak-refnr
          AND remessa EQ space.

      WHILE lv_pul_pal GE lv_max_pal.
        lv_pul_pal = lv_pul_pal - lv_max_pal.
      ENDWHILE.

      lv_pul_pos = lv_max_pal - lv_pul_pal.

    ENDIF.
  ELSE.
    lv_max_pal = 0.
  ENDIF.

  c_pul = lv_pul_pos.
ENDFORM.                    " F_GET_LUNG_FREE_POSITION
*&---------------------------------------------------------------------*
*&      Form  F_DATA_SCR0010_UCOMM_NEXT_PRM
*&---------------------------------------------------------------------*
FORM f_data_scr0010_ucomm_next_prm USING is_ltak   TYPE ltak
                                         is_ltap   TYPE ltap
                                         is_zwm011 TYPE zwm011
                                         i_lenum   TYPE lenum
                                   CHANGING c_subrc TYPE sysubrc.
  DATA lt_ltap       TYPE TABLE OF ltap.
  DATA lt_zwm011_del TYPE TABLE OF zwm011.
  DATA lt_zwm020_del TYPE TABLE OF zwm020.

  DATA ls_bdcmsgcoll TYPE bdcmsgcoll.
  DATA ls_zwm020     TYPE zwm020.

  DATA lv_keyword TYPE keywords.
  DATA lv_lock    TYPE abap_bool.
  DATA lv_msg     TYPE string.

  CONCATENATE sy-mandt is_ltak-refnr INTO lv_keyword.

  PERFORM f_enqueue_keyword
    USING lv_keyword
          3
          lv_lock.
  IF lv_lock EQ abap_true.
    APPEND is_ltap TO lt_ltap.

    PERFORM f_data_l_to_confirm
      TABLES lt_ltap
      USING '2'
      CHANGING c_subrc
              ls_bdcmsgcoll.

    IF c_subrc EQ 0.
      APPEND is_zwm011 TO lt_zwm011_del.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWM011' IN UPDATE TASK
        EXPORTING
          it_zwm011_del = lt_zwm011_del.

      " treat erroneous cases and remove zwm020 entry
      SELECT * UP TO 1 ROWS
        FROM zwm020
        INTO ls_zwm020
        WHERE armazem EQ is_ltap-lgnum
          AND p2 EQ i_lenum.
      ENDSELECT.
      IF sy-subrc EQ 0 .
        APPEND ls_zwm020 TO lt_zwm020_del.
      ENDIF.

      CALL FUNCTION 'Z_WMFR_SAVE_ZWM020' IN UPDATE TASK
        EXPORTING
          it_zwm020_del = lt_zwm020_del.

      PERFORM f_commit_work USING 'X' 2.
      PERFORM f_dequeue_keyword USING lv_keyword.
    ELSE.
      c_subrc = 4.

      PERFORM f_rollback.
      PERFORM f_dequeue_keyword USING lv_keyword.
    ENDIF.
  ELSE.
    c_subrc = 4.

    "Acção bloqueada pelo sistema. Tentar novamente
    MESSAGE e064(zwmfr001) INTO lv_msg.

    ls_bdcmsgcoll-msgid = sy-msgid.
    ls_bdcmsgcoll-msgtyp = sy-msgty.
    ls_bdcmsgcoll-msgnr = sy-msgno.
  ENDIF.

  IF c_subrc = 4.
    PERFORM f_show_message USING ls_bdcmsgcoll.
  ENDIF.
ENDFORM.                    " F_DATA_SCR0010_UCOMM_NEXT_PRM
*&---------------------------------------------------------------------*
*&      Form  F_GET_PARAMETER
*&---------------------------------------------------------------------*
FORM f_get_parameter USING i_lgnum     TYPE lgnum
                           i_process   TYPE zwm_processo
                           i_parameter TYPE zwm_parametro
                     CHANGING c_value TYPE zwm_valor.

  DATA lt_zwm001 TYPE TABLE OF zwm001.

  FIELD-SYMBOLS <zwm001> TYPE zwm001.

  IF gt_zwm001[] IS INITIAL.
    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
      EXPORTING
        whs       = i_lgnum
      TABLES
        ti_zwm001 = lt_zwm001.

    LOOP AT lt_zwm001 ASSIGNING <zwm001>.
      INSERT <zwm001> INTO TABLE gt_zwm001.
    ENDLOOP.
  ENDIF.

  READ TABLE gt_zwm001 ASSIGNING <zwm001>
    WITH KEY processo  = i_process
             parametro = i_parameter
    BINARY SEARCH.
  IF sy-subrc = 0.
    c_value = <zwm001>-valor.
  ENDIF.
ENDFORM.                    " F_GET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_PICKING_TO
*&---------------------------------------------------------------------*
FORM f_create_picking_to USING is_ltak TYPE ltak
                               i_bname TYPE xubname.

  DATA lt_ltap       TYPE TABLE OF ltap.
  DATA lt_zwm011_ins TYPE TABLE OF zwm011.

  DATA ls_ltak TYPE ltak.

  DATA lv_subrc TYPE sysubrc.
  DATA lv_tanum TYPE tanum.

  FIELD-SYMBOLS <ltap>   TYPE ltap.
  FIELD-SYMBOLS <zwm011> TYPE zwm011.

  SET UPDATE TASK LOCAL.

  CALL FUNCTION 'Z_WMFR_CREATE_PICKING_TO'
    EXPORTING
      i_lgnum = is_ltak-lgnum
      i_tanum = is_ltak-tanum
    IMPORTING
      e_subrc = lv_subrc
      e_tanum = lv_tanum.

  IF lv_subrc EQ 0.
    SELECT SINGLE *
      FROM ltak
      INTO ls_ltak
      WHERE lgnum EQ is_ltak-lgnum
        AND tanum EQ lv_tanum.

    SELECT *
      FROM ltap
      INTO TABLE lt_ltap
      WHERE lgnum EQ is_ltak-lgnum
        AND tanum EQ lv_tanum.

    LOOP AT lt_ltap ASSIGNING <ltap>.
      APPEND INITIAL LINE TO lt_zwm011_ins ASSIGNING <zwm011>.
      <zwm011>-armazem = <ltap>-lgnum.
      <zwm011>-to_number = <ltap>-tanum.
      <zwm011>-to_item = <ltap>-tapos.
      <zwm011>-user_name = i_bname.
      <zwm011>-equipamento = 'RETRACTIL'.
      <zwm011>-status = 'C'.
      <zwm011>-queue = ls_ltak-queue.
      <zwm011>-ultimo_tipo_dep = <ltap>-nltyp.
      <zwm011>-ultimo_bin = <ltap>-nlpla.
    ENDLOOP.

    CALL FUNCTION 'Z_WMFR_SAVE_ZWM011' IN UPDATE TASK
      TABLES
        it_zwm011_ins = lt_zwm011_ins.

    PERFORM f_commit_work USING 'X' 0.

    READ TABLE lt_zwm011_ins ASSIGNING <zwm011> INDEX 1.

    CALL FUNCTION 'ZWM_CALL_TASK_RET'
      EXPORTING
        armazem     = is_ltak-lgnum
        tab_zwm011  = <zwm011>
        equipamento = <zwm011>-equipamento
        primeira    = 'X'
        onlyone     = 'X'.
  ENDIF.

ENDFORM.                    " F_CREATE_PICKING_TO
*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_ZWMFRT004
*&---------------------------------------------------------------------*
FORM f_update_zwmfrt004 TABLES et_zwmfrt004_del STRUCTURE zwmfrt004
                               it_ltak          STRUCTURE ltak
                               it_zwm028        STRUCTURE zwm028
                               it_zwm013_ins    STRUCTURE zwm013
                               it_zwm013_upd    STRUCTURE zwm013
                        USING is_ltap      TYPE ltap
                              is_zwmfrt005 TYPE zwmfrt005
                              i_process    TYPE zwmmp_parametro.

  DATA lt_ltak             TYPE TABLE OF ltak.
  DATA lt_ltap             TYPE TABLE OF ltap.
  DATA lt_ltap_ori         TYPE SORTED TABLE OF ltap WITH UNIQUE KEY vlenr.
  DATA ls_ltap             TYPE ltap.
  DATA lt_zwmfrt004        TYPE TABLE OF zwmfrt004.
  DATA lt_zwmfrt004_del    TYPE TABLE OF zwmfrt004.
  DATA lt_zwmfrt005        TYPE TABLE OF zwmfrt005.
  DATA lt_zwm020           TYPE TABLE OF zwm020.
  DATA lt_zwm013           TYPE TABLE OF zwm013.
  DATA ls_zwm013           TYPE zwm013.
  DATA lv_aut_queue        TYPE lrf_queue.
  DATA lv_valor            TYPE zwm_valor.
  DATA lv_total            TYPE sytabix.
  DATA lv_tabix            TYPE sytabix.
  DATA lv_total_remont     TYPE sytabix.
  DATA lv_total_remont_ori TYPE sytabix.
  DATA lv_paletes_carro    TYPE sytabix.
  DATA lv_paletes_pulmao   TYPE sytabix.
  DATA lv_pul1             TYPE char14.
  DATA lv_pul2             TYPE char14.
  DATA lv_total_all        TYPE sytabix.

  FIELD-SYMBOLS <ltak>      TYPE ltak.
  FIELD-SYMBOLS <zwm001>    TYPE zwm001.
  FIELD-SYMBOLS <zwm028>    TYPE zwm028.
  FIELD-SYMBOLS <zwmfrt004> TYPE zwmfrt004.

  "select AUT Queue
  SELECT SINGLE valor
           FROM zwm001
           INTO lv_valor
           WHERE  armazem   = is_zwmfrt005-lgnum AND
                  processo  = c_process_queues_maint AND
                  parametro = c_param_queue_aut_sd.

  MOVE lv_valor TO lv_aut_queue.

  DO 1 TIMES.
    SELECT * FROM ltak
             INTO TABLE lt_ltak
             WHERE lgnum = is_zwmfrt005-lgnum AND
                   refnr = is_zwmfrt005-refnr AND
                   queue = lv_aut_queue.
    CHECK sy-subrc EQ 0.

    SELECT * FROM ltap
             INTO TABLE lt_ltap
             FOR ALL ENTRIES IN lt_ltak
             WHERE lgnum = lt_ltak-lgnum AND
                   tanum = lt_ltak-tanum.
    CHECK sy-subrc EQ 0.

    DELETE lt_ltap WHERE vorga EQ 'ST'.

    SORT lt_ltap BY vlenr.
    DELETE ADJACENT DUPLICATES FROM lt_ltap COMPARING vlenr.
    lt_ltap_ori = lt_ltap.

    CHECK NOT lt_ltap IS INITIAL.

    DESCRIBE TABLE lt_ltap LINES lv_total.

    SELECT * FROM zwm020
             INTO TABLE lt_zwm020
             FOR ALL ENTRIES IN lt_ltap
             WHERE armazem = is_zwmfrt005-lgnum AND
                   ( p1 = lt_ltap-vlenr OR p2 = lt_ltap-vlenr ).

    LOOP AT lt_ltap INTO ls_ltap.
      lv_tabix = sy-tabix.

      IF ls_ltap-pquit EQ abap_true.
        "so valido no DCK
        lv_paletes_carro = lv_paletes_carro + 1.
      ENDIF.

      READ TABLE lt_zwm020
        WITH KEY p1 = ls_ltap-vlenr
        TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_zwm020
        WITH KEY p2 = ls_ltap-vlenr
        TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        IF ls_ltap-pquit EQ abap_true.
          "so valido no DCK
          lv_paletes_carro = lv_paletes_carro - 1.
        ENDIF.
        CONTINUE.
      ENDIF.

      DELETE lt_ltap INDEX lv_tabix.
    ENDLOOP.

    DESCRIBE TABLE lt_ltap LINES lv_total_remont_ori.
    lv_total_remont = lv_total_remont_ori / 2.
    lv_total_remont = ceil( lv_total_remont ).

    lv_total_all = lv_total.
    lv_total = lv_total - ( lv_total_remont_ori - lv_total_remont ).

    READ TABLE it_zwm028 ASSIGNING <zwm028>
      WITH KEY lgnum = is_zwmfrt005-lgnum
               refnr = is_zwmfrt005-refnr
               remessa = space.

    CHECK <zwm028> IS ASSIGNED.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = <zwm028>-st_pul
        lgpla = <zwm028>-pulmao1
      IMPORTING
        bin   = lv_pul1.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = <zwm028>-st_pul
        lgpla = <zwm028>-pulmao2
      IMPORTING
        bin   = lv_pul2.

    SELECT * FROM zwm013
             INTO TABLE lt_zwm013
             WHERE armazem = <zwm028>-lgnum AND
                   ( destino = lv_pul1 OR destino = lv_pul2 ).

    APPEND LINES OF it_zwm013_ins TO lt_zwm013.
    APPEND LINES OF it_zwm013_upd TO lt_zwm013.

    LOOP AT  lt_zwm013 INTO ls_zwm013.
      READ TABLE lt_ltap_ori
            WITH TABLE KEY vlenr = ls_zwm013-sscc
            TRANSPORTING NO FIELDS.
      CHECK sy-subrc EQ 0.
      "so valido para PUL
      lv_paletes_pulmao = lv_paletes_pulmao + 1.
    ENDLOOP.
  ENDDO.




  " get queues for group
  SELECT *
    FROM zwmfrt004
    INTO TABLE lt_zwmfrt004
    WHERE lgnum EQ is_zwmfrt005-lgnum
      AND refnr EQ is_zwmfrt005-refnr.

  CASE i_process.
    WHEN c_param_queue_aut_rep.
      " get all tos
      IF it_ltak[] IS NOT INITIAL.
        SELECT *
          FROM ltak
          INTO TABLE lt_ltak
          FOR ALL ENTRIES IN it_ltak
          WHERE lgnum EQ it_ltak-lgnum
            AND benum EQ it_ltak-benum
            AND kquit EQ space
            AND queue EQ it_ltak-queue
            AND betyp EQ 'Z'.

        " delete tos that are to be confirmed
        LOOP AT it_ltak ASSIGNING <ltak>.
          DELETE lt_ltak WHERE tanum EQ <ltak>-tanum.
        ENDLOOP.

        IF lt_ltak IS INITIAL.
          LOOP AT gt_zwm001 ASSIGNING <zwm001>
            WHERE processo EQ c_process_treadmill
              AND parametro EQ c_param_queue_rep.

            READ TABLE lt_zwmfrt004 ASSIGNING <zwmfrt004>
              WITH KEY nlpla = <zwm001>-valor.
            IF sy-subrc EQ 0.
              APPEND <zwmfrt004> TO lt_zwmfrt004_del.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    WHEN c_param_queue_aut_sd_pul.
      READ TABLE it_zwm028 ASSIGNING <zwm028>
        WITH KEY lgnum = is_zwmfrt005-lgnum
                 refnr = is_zwmfrt005-refnr
                 remessa = space.
      IF sy-subrc EQ 0.
        IF lv_paletes_pulmao EQ lv_total_all.
          LOOP AT lt_zwmfrt004 ASSIGNING <zwmfrt004>.
            IF <zwmfrt004>-original EQ abap_true.
              " Check if is not a REP queue
              READ TABLE gt_zwm001 TRANSPORTING NO FIELDS
                WITH KEY processo = c_process_treadmill
                         parametro = c_param_queue_rep
                         valor = <zwmfrt004>-nlpla.
              IF sy-subrc NE 0.
                APPEND <zwmfrt004> TO lt_zwmfrt004_del.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    WHEN c_param_queue_aut_sd_dck.
      READ TABLE it_zwm028 ASSIGNING <zwm028>
        WITH KEY lgnum = is_zwmfrt005-lgnum
                 refnr = is_zwmfrt005-refnr
                 remessa = space.
      IF sy-subrc EQ 0.
        IF lv_paletes_carro EQ lv_total.
          LOOP AT lt_zwmfrt004 ASSIGNING <zwmfrt004>.
            IF <zwmfrt004>-original EQ abap_true.
              " Check if is not a REP queue
              READ TABLE gt_zwm001 TRANSPORTING NO FIELDS
                WITH KEY processo = c_process_treadmill
                         parametro = c_param_queue_rep
                         valor = <zwmfrt004>-nlpla.
              IF sy-subrc NE 0.
                APPEND <zwmfrt004> TO lt_zwmfrt004_del.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
      " do nothig
  ENDCASE.

  " get TOs for same group in the same treadmil
  " but only if the treadmill is not the original one
  READ TABLE lt_zwmfrt004 ASSIGNING <zwmfrt004>
    WITH KEY lgnum = is_zwmfrt005-lgnum
             nlpla = is_zwmfrt005-nlpla
             refnr = is_zwmfrt005-refnr.
  IF sy-subrc EQ 0.
    IF <zwmfrt004>-original EQ abap_false.
      SELECT *
        FROM zwmfrt005
        INTO TABLE lt_zwmfrt005
        WHERE lgnum EQ is_zwmfrt005-lgnum
          AND nlpla EQ is_zwmfrt005-nlpla
          AND refnr EQ is_zwmfrt005-refnr.

      " delete SU in treadmill an validate if there are still more
      DELETE lt_zwmfrt005 WHERE lenum EQ is_ltap-vlenr.

      IF lt_zwmfrt005 IS INITIAL.
        APPEND <zwmfrt004> TO lt_zwmfrt004_del.
      ENDIF.
    ENDIF.
  ENDIF.

  et_zwmfrt004_del[] = lt_zwmfrt004_del[].
ENDFORM.                    " F_UPDATE_ZWMFRT004
*&---------------------------------------------------------------------*
*&      Form  F_GET_REMOUNT_SU_DATA
*&---------------------------------------------------------------------*
FORM f_get_remount_su_data USING is_ltap TYPE ltap
                           CHANGING cs_ltak   TYPE ltak
                                    cs_ltap   TYPE ltap
                                    cs_zwm020 TYPE zwm020
                                    c_exist   TYPE abap_bool.

  DATA ls_ltak   TYPE ltak.
  DATA ls_ltap   TYPE ltap.
  DATA ls_zwm020 TYPE zwm020.

  DATA lv_exist TYPE abap_bool.

  SELECT * UP TO 1 ROWS
    FROM zwm020
    INTO ls_zwm020
    WHERE armazem EQ is_ltap-lgnum
      AND p1 EQ is_ltap-vlenr.
  ENDSELECT.
  IF sy-subrc EQ 0.
    SELECT * UP TO 1 ROWS
      FROM ltap
      INTO ls_ltap
      WHERE lgnum EQ ls_zwm020-armazem
        AND vlenr EQ ls_zwm020-p2
        AND pquit EQ abap_false.
    ENDSELECT.
    IF sy-subrc EQ 0.
      lv_exist = abap_true.

      SELECT SINGLE *
        FROM ltak
        INTO ls_ltak
        WHERE lgnum EQ ls_ltap-lgnum
          AND tanum EQ ls_ltap-tanum.

    ENDIF.
  ENDIF.

  cs_ltak   = ls_ltak.
  cs_ltap   = ls_ltap.
  cs_zwm020 = ls_zwm020.
  c_exist = lv_exist.
ENDFORM.                    " F_GET_ZWM020_REMOUNT_SU
*&---------------------------------------------------------------------*
*&      Form  F_IS_AUTO_REMOUNT_VALID
*&---------------------------------------------------------------------*
FORM f_is_auto_remount_valid USING is_ltak_org TYPE ltak
                                   is_ltak_rmt TYPE ltak
                             CHANGING c_valid TYPE abap_bool.

  DATA lt_zwm001 TYPE TABLE OF zwm001.

  DATA lr_queue TYPE RANGE OF lrf_queue.

  DATA lv_valid TYPE abap_bool.

  FIELD-SYMBOLS <r_queue> LIKE LINE OF lr_queue.
  FIELD-SYMBOLS <zwm011>  TYPE zwm001.

  IF gt_zwm001 IS INITIAL .
    PERFORM f_data_init_get_hardcodes
      TABLES lt_zwm001
      USING is_ltak_org-lgnum.
    LOOP AT lt_zwm001 ASSIGNING <zwm011>.
      INSERT <zwm011> INTO TABLE gt_zwm001.
    ENDLOOP.
  ENDIF.

  LOOP AT gt_zwm001 ASSIGNING <zwm011>
    WHERE processo EQ c_process_queues_maint
      AND parametro EQ c_param_queue_aut_prm.

    APPEND INITIAL LINE TO lr_queue ASSIGNING <r_queue>.
    <r_queue>-sign = 'I'.
    <r_queue>-option = 'EQ'.
    <r_queue>-low = <zwm011>-valor.
  ENDLOOP.

  SORT lr_queue BY sign option low high.
  DELETE ADJACENT DUPLICATES FROM lr_queue COMPARING ALL FIELDS.

  IF lr_queue IS NOT INITIAL.
    IF is_ltak_rmt-queue NOT IN lr_queue.
      IF is_ltak_rmt-queue EQ is_ltak_org-queue.
        lv_valid = abap_true.
      ENDIF.
    ENDIF.
  ENDIF.

  c_valid = lv_valid.
ENDFORM.                    " F_IS_AUTO_REMOUNT_VALID

*&---------------------------------------------------------------------*
*&      Form  f_posicao_pulmao_skip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_LGNUM    text
*      -->I_POS      text
*      <--C_POS      text
*----------------------------------------------------------------------*
FORM f_posicao_pulmao_skip USING    i_lgnum TYPE lgnum
                                    i_pos   TYPE numc2
                           CHANGING c_pos TYPE numc2.

  CHECK i_lgnum EQ '150'.

  c_pos = i_pos.
ENDFORM.                    "f_posicao_pulmao_skip
*&---------------------------------------------------------------------*
*&      Form  GET_TO_VBELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IS_LTAK  text
*      <--P_LV_VBELN  text
*----------------------------------------------------------------------*
FORM get_to_vbeln  CHANGING cs_ltak  TYPE ltak.
  DATA: lv_vbeln TYPE vbeln.

  CHECK cs_ltak-vbeln IS INITIAL AND cs_ltak-betyp EQ 'L'.

  lv_vbeln = cs_ltak-benum.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_vbeln
    IMPORTING
      output = lv_vbeln.

  cs_ltak-vbeln = lv_vbeln.
ENDFORM.
