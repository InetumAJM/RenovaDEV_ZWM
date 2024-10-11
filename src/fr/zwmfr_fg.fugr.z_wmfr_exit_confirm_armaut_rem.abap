FUNCTION z_wmfr_exit_confirm_armaut_rem.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_LTAK_VB) TYPE  LTAK_VB
*"     REFERENCE(IT_LTAP_VB) TYPE  TT_LTAP_VB
*"----------------------------------------------------------------------
  DATA: lt_zwm001    TYPE TABLE OF zwm001,
        lt_zwm020    TYPE TABLE OF zwm020,
        lt_lgtyp     TYPE TABLE OF lgtyp,
        lt_bwlvs     TYPE TABLE OF bwlvs,
        lt_ltap_vb   TYPE  tt_ltap_vb,
        lt_ltap      TYPE TABLE OF ltap,
        lt_ltap_conf TYPE TABLE OF ltap_conf.

  DATA: ls_zwm001  TYPE zwm001,
        ls_ltap_vb TYPE	ltap_vb,
        ls_zwm020  TYPE TABLE OF zwm020,
        ls_ltap    TYPE ltap.

  DATA: lv_bwlvs TYPE bwlvs,
        lv_lgtyp TYPE lgtyp,
        lv_tabix TYPE sytabix.

  lt_ltap_vb = it_ltap_vb.

  DELETE lt_ltap_vb WHERE pquit <> abap_true OR
                          vorga EQ 'ST' OR
                          vorga EQ 'SL' OR
                          nlenr IS INITIAL.

  CHECK NOT lt_ltap_vb IS INITIAL.


** Valida se Deve Executar
***********************************************************************
  SELECT * FROM zwm001
           INTO TABLE lt_zwm001
           WHERE armazem = is_ltak_vb-lgnum AND
                 processo = 'ARRUMA_REMONTADA_AUT' AND
                 parametro = 'MOV_WM'.

  LOOP AT lt_zwm001 INTO ls_zwm001.
    lv_bwlvs = ls_zwm001-valor.
    APPEND lv_bwlvs TO lt_bwlvs.
  ENDLOOP.
  SORT lt_bwlvs.
  DELETE ADJACENT DUPLICATES FROM lt_bwlvs.

  READ TABLE lt_bwlvs
    WITH KEY table_line = is_ltak_vb-bwlvs
    BINARY SEARCH
    TRANSPORTING NO FIELDS.
  CHECK sy-subrc EQ 0.



  SELECT * FROM zwm001
           INTO TABLE lt_zwm001
           WHERE armazem = is_ltak_vb-lgnum AND
                 processo = 'ARRUMA_REMONTADA_AUT' AND
                 parametro = 'NLTYP_OK'.

  LOOP AT lt_zwm001 INTO ls_zwm001.
    lv_lgtyp = ls_zwm001-valor.
    APPEND lv_lgtyp TO lt_lgtyp.
  ENDLOOP.
  SORT lt_lgtyp.
  DELETE ADJACENT DUPLICATES FROM lt_lgtyp.

  LOOP AT lt_ltap_vb INTO ls_ltap_vb.
    lv_tabix = sy-tabix.

    READ TABLE lt_lgtyp
      WITH KEY table_line = ls_ltap_vb-nltyp
      BINARY SEARCH
      TRANSPORTING NO FIELDS.
    CHECK sy-subrc <> 0.
    DELETE lt_ltap_vb INDEX lv_tabix.
  ENDLOOP.

  CHECK NOT lt_ltap_vb IS INITIAL.

** retorna Remontadas
***********************************************************************
  SELECT * FROM zwm020
           INTO TABLE lt_zwm020
           FOR ALL ENTRIES IN lt_ltap_vb
           WHERE armazem = lt_ltap_vb-lgnum AND
                 p1 = lt_ltap_vb-nlenr.

  CHECK sy-subrc EQ 0.

** Retorna OT's para SU remontadas
***********************************************************************
  SELECT * FROM ltap
           INTO TABLE lt_ltap
           FOR ALL ENTRIES IN lt_zwm020
           WHERE lgnum = lt_zwm020-armazem AND
                 nlenr = lt_zwm020-p2 AND
                 pquit = abap_false.

  CHECK sy-subrc EQ 0.

** Confirma OT's
***********************************************************************
  SORT lt_ltap BY nlenr.
  DELETE ADJACENT DUPLICATES FROM lt_ltap COMPARING nlenr.
  LOOP AT lt_ltap INTO ls_ltap.

    IF ls_ltap-kzsub IS NOT INITIAL.
      CALL FUNCTION 'L_TO_CONFIRM_SU'
        EXPORTING
          i_lenum       = ls_ltap-nlenr
          i_squit       = abap_true
          i_quknz       = '1'
          i_subst       = ls_ltap-kzsub
          i_commit_work = abap_false
        TABLES
          t_ltap_conf   = lt_ltap_conf
        EXCEPTIONS
          OTHERS        = 1.

      WAIT UP TO 1 SECONDS.
      CALL FUNCTION 'L_TO_CONFIRM_SU'
        EXPORTING
          i_lenum       = ls_ltap-nlenr
          i_squit       = abap_true
          i_quknz       = '2'
          i_subst       = ls_ltap-kzsub
          i_commit_work = abap_false
        TABLES
          t_ltap_conf   = lt_ltap_conf
        EXCEPTIONS
          OTHERS        = 1.

    ELSE.

      CALL FUNCTION 'L_TO_CONFIRM_SU'
        EXPORTING
          i_lenum       = ls_ltap-nlenr
          i_squit       = abap_true
          i_commit_work = abap_false
        TABLES
          t_ltap_conf   = lt_ltap_conf
        EXCEPTIONS
          OTHERS        = 1.
    ENDIF.
  ENDLOOP.




ENDFUNCTION.
