FUNCTION zwm_exit_to_create_wcs.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_LTAK_VB) TYPE  LTAK_VB
*"     VALUE(IT_LTAP_VB) TYPE  TT_LTAP_VB
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lv_lgtyp_aut TYPE lgtyp VALUE 'AUT'.

  DATA: lt_ltap_vb   TYPE tt_ltap_vb,
        lt_ltap      TYPE TABLE OF ltap,
        lt_lgpla_new TYPE TABLE OF lgpla,
        lt_lqua      TYPE TABLE OF lqua,
        lt_zwm078    TYPE TABLE OF zwm078,
        lt_ltap_conf TYPE TABLE OF ltap_conf.

  DATA: ls_ltap_vb   TYPE ltap_vb,
        ls_ltak      TYPE ltak,
        ls_ltap      TYPE ltap,
        ls_zwm078    TYPE zwm078,
        ls_ltap_conf TYPE ltap_conf.

  DATA: lv_tabix     TYPE sytabix,
        lv_lgpla_new TYPE lgpla,
        lv_bwlvs     TYPE bwlvs,
        lv_subst     TYPE rl03t-subst,
        lv_lenum     TYPE lenum,
        lv_value     TYPE zwm_valor.

** Validar Palete de Expedição Rejeitada no EXP4_ENTRJ
***********************************************************************
  CHECK is_ltak_vb-lgnum = '100'.

  READ TABLE it_ltap_vb INTO ls_ltap_vb INDEX 1.

  IF ls_ltap_vb-vltyp = 'CHE' AND ls_ltap_vb-vlpla = 'EXP4_ENTRJ'.

    lv_lenum = ls_ltap_vb-ablad.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_lenum
      IMPORTING
        output = lv_lenum.

    IF lv_lenum IS NOT INITIAL.
      SELECT *
        FROM ltap INTO TABLE lt_ltap
        WHERE lgnum = is_ltak_vb-lgnum
        AND   vlenr = lv_lenum.

      DELETE lt_ltap WHERE pquit = 'X'.

      IF lt_ltap[] IS NOT INITIAL.
        SELECT *
          FROM zwm078 INTO TABLE lt_zwm078
          FOR ALL ENTRIES IN lt_ltap
          WHERE lgnum = lt_ltap-lgnum
          AND   tanum = lt_ltap-tanum
          AND   tapos = lt_ltap-tapos.
      ENDIF.

      READ TABLE lt_zwm078 INTO ls_zwm078 INDEX 1.
      IF sy-subrc = 0.
        UPDATE zwm078 SET rej   = 'X'
                          rlpla = 'EXP4_REJ'
        WHERE lgnum = ls_zwm078-lgnum
        AND   tanum = ls_zwm078-tanum.

        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDIF.

** Confirmar Logo OT
    DO 10 TIMES.
      SELECT SINGLE *
        FROM ltak INTO ls_ltak
        WHERE lgnum = is_ltak_vb-lgnum
        AND   tanum = is_ltak_vb-tanum.

      IF sy-subrc = 0.
        EXIT.
      ENDIF.

      WAIT UP TO 1 SECONDS.
    ENDDO.

    IF ls_ltap_vb-kzsub = 'X'.
      lv_subst = 'X'.
    ENDIF.

    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = is_ltak_vb-lgnum
      AND   tanum = is_ltak_vb-tanum.

    LOOP AT lt_ltap INTO ls_ltap.
      CLEAR ls_ltap_conf.
      ls_ltap_conf-tanum = ls_ltap-tanum.
      ls_ltap_conf-tapos = ls_ltap-tapos.
      ls_ltap_conf-altme = ls_ltap-meins.
      ls_ltap_conf-nista = ls_ltap-vsolm.
      APPEND ls_ltap_conf TO lt_ltap_conf.
    ENDLOOP.

    CALL FUNCTION 'L_TO_CONFIRM'
      EXPORTING
        i_lgnum       = is_ltak_vb-lgnum
        i_tanum       = is_ltak_vb-tanum
        i_quknz       = '4'
        i_subst       = lv_subst
      TABLES
        t_ltap_conf   = lt_ltap_conf
      EXCEPTIONS
        error_message = 99.

  ENDIF.

ENDFUNCTION.
