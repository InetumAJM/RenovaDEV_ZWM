FUNCTION z_wmfr_exit_to_create_armauto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_LTAK_VB) TYPE  LTAK_VB
*"     REFERENCE(IT_LTAP_VB) TYPE  TT_LTAP_VB
*"  EXCEPTIONS
*"      ERROR
*"      NOT_EXIT
*"----------------------------------------------------------------------
  DATA: lv_lgtyp_aut TYPE lgtyp VALUE 'AUT'.


  DATA: lt_ltap_vb   TYPE tt_ltap_vb,
        lt_ltap      TYPE TABLE OF ltap,
        lt_lgpla_new TYPE TABLE OF lgpla,
        lt_lqua      TYPE TABLE OF lqua.

  DATA: ls_ltap_vb TYPE ltap_vb,
        ls_ltap    TYPE ltap,
        ls_lqua    TYPE lqua.

  DATA: lv_tabix     TYPE sytabix,
        lv_lgpla_new TYPE lgpla,
        lv_bwlvs     TYPE bwlvs,
        lv_value     TYPE zwm_valor.

  IF is_ltak_vb-lgnum <> '150'.
    RAISE not_exit.
  ENDIF.

** Parametros
***********************************************************************
  SELECT SINGLE valor FROM zwm001
                      INTO lv_value
                      WHERE armazem   = is_ltak_vb-lgnum AND
                            processo  = 'TRANSFERENCIA_AUT' AND
                            parametro = 'MOV_WM'.

  lv_bwlvs = lv_value.

  lt_ltap_vb = it_ltap_vb.

** Valida Armazem AUT
***********************************************************************
  DELETE lt_ltap_vb WHERE vltyp <> lv_lgtyp_aut.
  CHECK NOT lt_ltap_vb IS INITIAL.

** Valida Profundidade
***********************************************************************
  LOOP AT lt_ltap_vb INTO ls_ltap_vb.
    IF ls_ltap_vb-vlpla+8(1) <> 1.
      DELETE lt_ltap_vb INDEX sy-tabix.
      CONTINUE.
    ENDIF.

    CONCATENATE ls_ltap_vb-vlpla(8) '2' INTO lv_lgpla_new.
    CONDENSE lv_lgpla_new NO-GAPS.
    APPEND lv_lgpla_new TO lt_lgpla_new.
  ENDLOOP.
  CHECK NOT lt_ltap_vb IS INITIAL.

  SORT lt_lgpla_new.
  DELETE ADJACENT DUPLICATES FROM lt_lgpla_new.
  CHECK NOT lt_lgpla_new IS INITIAL.


** Lock
***********************************************************************
  PERFORM exit_armaut_lock USING is_ltak_vb-lgnum lv_lgtyp_aut.

** Retorna OT's para Novas Posições
***********************************************************************
  SELECT * FROM ltap
           INTO TABLE lt_ltap
           FOR ALL ENTRIES IN lt_lgpla_new
           WHERE lgnum = is_ltak_vb-lgnum AND
                 pquit = abap_false AND
                 vltyp = lv_lgtyp_aut AND
                 vlpla = lt_lgpla_new-table_line.

  SORT lt_ltap BY vlpla.

  LOOP AT lt_lgpla_new INTO lv_lgpla_new.
    lv_tabix = sy-tabix.

    CLEAR: ls_ltap.
    READ TABLE lt_ltap
          INTO ls_ltap
          WITH KEY vlpla = lv_lgpla_new
          BINARY SEARCH.

    IF sy-subrc EQ 0.
      DELETE lt_lgpla_new INDEX lv_tabix.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  IF lt_lgpla_new IS INITIAL.
    PERFORM exit_armaut_unlock.
    EXIT.
  ENDIF.

** Retorna Paletes Pares
***********************************************************************
  SELECT * FROM lqua
           INTO TABLE lt_lqua
           FOR ALL ENTRIES IN lt_lgpla_new
           WHERE lgnum = is_ltak_vb-lgnum AND
                 lgtyp = lv_lgtyp_aut AND
                 lgpla = lt_lgpla_new-table_line.

  SORT lt_lqua BY lenum.
  DELETE ADJACENT DUPLICATES FROM lt_lqua COMPARING lenum.

  PERFORM exit_armaut_unlock.

  LOOP AT lt_lqua INTO ls_lqua.
    DO 50 TIMES.
      CALL FUNCTION 'L_TO_CREATE_MOVE_SU'
        EXPORTING
          i_lenum       = ls_lqua-lenum
          i_bwlvs       = lv_bwlvs
          i_commit_work = abap_false
          i_bname       = sy-uname
          i_tapri       = 99
        EXCEPTIONS
          error_message = 99.
      IF sy-subrc <> 0.
        WAIT UP TO 1 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ENDLOOP.
ENDFUNCTION.
