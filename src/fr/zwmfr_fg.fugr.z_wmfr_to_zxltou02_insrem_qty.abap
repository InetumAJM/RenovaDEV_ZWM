FUNCTION z_wmfr_to_zxltou02_insrem_qty.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_LTAK) TYPE  LTAK_VB
*"     VALUE(IT_LTAP) TYPE  TT_LTAP_VB OPTIONAL
*"----------------------------------------------------------------------
  DATA lv_fevor       TYPE fevor.                           "#EC NEEDED
  DATA lv_aufnr       TYPE aufnr.
  DATA lv_kostl       TYPE kostl.
  DATA lv_totype      TYPE char2.
  DATA lv_aufnr_kostl TYPE char12.
  DATA lv_nltyp_cc    TYPE ltap-nltyp.
  DATA lv_nltyp_aufnr TYPE ltap-nltyp.

  DATA ls_zwmmpt004 TYPE zwmmpt004.

  DATA lr_nltyp TYPE RANGE OF ltap-nltyp.

  DATA lt_ltap    TYPE tt_ltap_vb.
  DATA lt_mara    TYPE ty_ht_mara.
  DATA lt_kostl   TYPE STANDARD TABLE OF ty_kostl.
  DATA lt_hkostl  TYPE ty_ht_kostl.

  FIELD-SYMBOLS <fs_zwm001> LIKE LINE OF gt_zwmmpt001[].
  FIELD-SYMBOLS <fs_ltap>   LIKE LINE OF it_ltap[].
  FIELD-SYMBOLS <fs_nltyp>  LIKE LINE OF lr_nltyp[].
  FIELD-SYMBOLS <fs_kostl>  LIKE LINE OF lt_hkostl[].
  FIELD-SYMBOLS <fs_mara>   LIKE LINE OF lt_mara[].
*"----------------------------------------------------------------------

  IF is_ltak-kquit NE abap_true.
    RETURN.
  ENDIF.

  LOOP AT it_ltap[] ASSIGNING <fs_ltap>.
    DELETE FROM zwmmpt012 WHERE lgnum EQ <fs_ltap>-lgnum
                            AND tanum EQ <fs_ltap>-tanum
                            AND tapos EQ <fs_ltap>-tapos.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.

  lt_ltap[] = it_ltap[].
  SORT lt_ltap[] BY vlenr.
  DELETE lt_ltap[] WHERE vlenr IS INITIAL.
  SORT lt_ltap[] BY lgnum tanum tapos.

  GET TIME.

  IF is_ltak-lznum IS NOT INITIAL.
    SPLIT is_ltak-lznum AT '#' INTO lv_fevor lv_totype lv_aufnr_kostl.
  ENDIF.

  FREE gt_zwmmpt001[].
  SELECT processo parametro item valor
    FROM zwmmpt001 INTO TABLE gt_zwmmpt001[]
    WHERE armazem EQ is_ltak-lgnum.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_nltyp_cc
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_nltyp_cc = <fs_zwm001>-valor.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_nltyp_aufnr
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_nltyp_aufnr = <fs_zwm001>-valor.
  ENDIF.

  APPEND INITIAL LINE TO lr_nltyp[] ASSIGNING <fs_nltyp>.
  <fs_nltyp>-sign   = c_rsig_i.
  <fs_nltyp>-option = c_ropt_eq.
  <fs_nltyp>-low    = lv_nltyp_cc.
  APPEND INITIAL LINE TO lr_nltyp[] ASSIGNING <fs_nltyp>.
  <fs_nltyp>-sign   = c_rsig_i.
  <fs_nltyp>-option = c_ropt_eq.
  <fs_nltyp>-low    = lv_nltyp_aufnr.

  IF lt_ltap[] IS NOT INITIAL.
    SELECT matnr extwg
      FROM mara INTO TABLE lt_mara[]
      FOR ALL ENTRIES IN lt_ltap[]
      WHERE matnr EQ lt_ltap-matnr.

    IF lt_mara[] IS NOT INITIAL.
      SELECT extwg kostl fevor
        FROM zwmmpt008 INTO TABLE lt_kostl[]
        FOR ALL ENTRIES IN lt_mara[]
        WHERE extwg EQ lt_mara-extwg.

      SORT lt_kostl[] BY extwg fevor.
      DELETE ADJACENT DUPLICATES FROM lt_kostl[] COMPARING extwg fevor.

      LOOP AT lt_kostl[] ASSIGNING <fs_kostl>.
        INSERT <fs_kostl> INTO TABLE lt_hkostl[].
      ENDLOOP.
    ENDIF.
  ENDIF.

  LOOP AT lt_ltap[] ASSIGNING <fs_ltap>.
    CHECK <fs_ltap>-nltyp IN lr_nltyp[].

    CHECK <fs_ltap>-vorga NE c_vorga_st AND <fs_ltap>-vorga NE c_vorga_sl.

    CLEAR ls_zwmmpt004.

    CASE <fs_ltap>-nltyp.
      WHEN lv_nltyp_aufnr.
        lv_aufnr  = lv_aufnr_kostl.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_aufnr
          IMPORTING
            output = ls_zwmmpt004-aufnr.
      WHEN lv_nltyp_cc.
        READ TABLE lt_mara[] ASSIGNING <fs_mara>
          WITH TABLE KEY matnr = <fs_ltap>-matnr.
        IF sy-subrc EQ 0.
          READ TABLE lt_hkostl[] ASSIGNING <fs_kostl>
            WITH TABLE KEY extwg = <fs_mara>-extwg
                           fevor = lv_fevor.
          IF sy-subrc EQ 0.
            lv_kostl = <fs_kostl>-kostl.
          ENDIF.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_kostl
          IMPORTING
            output = ls_zwmmpt004-kostl.
    ENDCASE.

    ls_zwmmpt004-fevor        = lv_fevor.
    ls_zwmmpt004-lgnum        = <fs_ltap>-lgnum.
    ls_zwmmpt004-lenum        = <fs_ltap>-vlenr.
    ls_zwmmpt004-matnr        = <fs_ltap>-matnr.
    ls_zwmmpt004-maktx        = <fs_ltap>-maktx.
    ls_zwmmpt004-charg        = <fs_ltap>-charg.
    ls_zwmmpt004-tanum        = <fs_ltap>-tanum.
    ls_zwmmpt004-qtd_consumo  = <fs_ltap>-vsola.
    ls_zwmmpt004-meins        = <fs_ltap>-meins.
    ls_zwmmpt004-reg_user     = sy-uname.
    ls_zwmmpt004-reg_time     = sy-uzeit.
    ls_zwmmpt004-reg_date     = sy-datum.

    INSERT zwmmpt004 FROM ls_zwmmpt004.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ENDIF.

    CALL FUNCTION 'Z_WMFR_TO_CONSUMPTION'
      EXPORTING
        i_lgnum = <fs_ltap>-lgnum
        i_lenum = <fs_ltap>-vlenr.
  ENDLOOP.
ENDFUNCTION.
