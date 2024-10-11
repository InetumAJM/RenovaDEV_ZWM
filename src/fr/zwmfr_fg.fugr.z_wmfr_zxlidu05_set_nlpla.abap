FUNCTION z_wmfr_zxlidu05_set_nlpla.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TANUM) TYPE  TANUM
*"     REFERENCE(I_NLPLA) TYPE  LTAP_NLPLA
*"     REFERENCE(I_LENUM) TYPE  LENUM
*"  EXPORTING
*"     REFERENCE(E_NLPLA) TYPE  LTAP_NLPLA
*"  CHANGING
*"     REFERENCE(CT_LTAP) TYPE  PDT_T_LTAP_CONF
*"----------------------------------------------------------------------
  DATA lv_group TYPE ltak-refnr.
  DATA lv_dbmax TYPE i.
  DATA lv_nlpla TYPE ltap-nlpla.

  DATA ls_ltak      TYPE ltak.
  DATA ls_zwmfrt004 TYPE zwmfrt004.
  DATA ls_zwmfrt005 TYPE zwmfrt005.

  DATA lt_zwm001  TYPE STANDARD TABLE OF zwm001.
  DATA lt_ltap    TYPE STANDARD TABLE OF ltap.

  DATA lr_nltyp TYPE RANGE OF ltap-nltyp.
  DATA lr_vltyp TYPE RANGE OF ltap-vltyp.
  DATA lr_param TYPE RANGE OF zwm001-parametro.

  FIELD-SYMBOLS <fs_ltap_c>   LIKE LINE OF ct_ltap[].
  FIELD-SYMBOLS <fs_ltap>     LIKE LINE OF lt_ltap[].
  FIELD-SYMBOLS <fs_zwm001>   LIKE LINE OF lt_zwm001[].
  FIELD-SYMBOLS <fs_nltyp>    LIKE LINE OF lr_nltyp[].
  FIELD-SYMBOLS <fs_vltyp>    LIKE LINE OF lr_vltyp[].
  FIELD-SYMBOLS <fs_param>    LIKE LINE OF lr_param[].
*"----------------------------------------------------------------------

  FREE gt_zwmfrt004[].
  FREE gt_zwmfrt005[].

  APPEND INITIAL LINE TO lr_param[] ASSIGNING <fs_param>.
  <fs_param>-sign   = c_rsig_i.
  <fs_param>-option = c_ropt_eq.
  <fs_param>-low    = c_param_nltyp_ok.
  APPEND INITIAL LINE TO lr_param[] ASSIGNING <fs_param>.
  <fs_param>-sign   = c_rsig_i.
  <fs_param>-option = c_ropt_eq.
  <fs_param>-low    = c_param_vltyp_ok.

  IF ct_ltap[] IS INITIAL.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM ltak INTO ls_ltak
    WHERE lgnum EQ i_lgnum
      AND tanum EQ i_tanum.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  SELECT *
    FROM ltap INTO TABLE lt_ltap[]
    FOR ALL ENTRIES IN ct_ltap[]
    WHERE lgnum EQ i_lgnum
      AND tanum EQ ct_ltap-tanum
      AND tapos EQ ct_ltap-tapos.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  SELECT *
    FROM zwm001 INTO TABLE lt_zwm001[]
    WHERE armazem EQ i_lgnum
      AND processo EQ c_process_exit_idoc
      AND parametro IN lr_param[].
  LOOP AT lt_zwm001[] ASSIGNING <fs_zwm001>.
    CASE <fs_zwm001>-parametro.
      WHEN c_param_nltyp_ok.
        APPEND INITIAL LINE TO lr_nltyp[] ASSIGNING <fs_nltyp>.
        <fs_nltyp>-sign   = c_rsig_i.
        <fs_nltyp>-option = c_ropt_eq.
        <fs_nltyp>-low    = <fs_zwm001>-valor.
      WHEN c_param_vltyp_ok.
        APPEND INITIAL LINE TO lr_vltyp[] ASSIGNING <fs_vltyp>.
        <fs_vltyp>-sign   = c_rsig_i.
        <fs_vltyp>-option = c_ropt_eq.
        <fs_vltyp>-low    = <fs_zwm001>-valor.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
  ENDLOOP.

  LOOP AT lt_ltap[] ASSIGNING <fs_ltap>.
    IF <fs_ltap>-vltyp NOT IN lr_vltyp[].
      RETURN.
    ENDIF.

    IF <fs_ltap>-nltyp NOT IN lr_nltyp[].
      RETURN.
    ENDIF.
  ENDLOOP.

  IF ls_ltak-refnr IS NOT INITIAL.
    lv_group  = ls_ltak-refnr.
  ELSEIF ls_ltak-betyp EQ 'Z' AND ls_ltak-benum IS NOT INITIAL.
    lv_group  = ls_ltak-benum.
  ELSE.
    RETURN.
  ENDIF.

  READ TABLE ct_ltap[] ASSIGNING <fs_ltap_c> INDEX 1.
  IF sy-subrc EQ 0.
    lv_nlpla  = <fs_ltap_c>-nlpla.
  ENDIF.

  CLEAR e_nlpla.

  LOOP AT ct_ltap[] ASSIGNING <fs_ltap_c>.
    CLEAR <fs_ltap_c>-nlpla.
  ENDLOOP.

  SELECT SINGLE *
    FROM zwmfrt005 INTO ls_zwmfrt005
    WHERE lgnum EQ i_lgnum
      AND nlpla EQ lv_nlpla
      AND refnr EQ lv_group
      AND lenum EQ i_lenum.
  IF sy-subrc EQ 0.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM zwmfrt004 INTO ls_zwmfrt004
    WHERE lgnum EQ i_lgnum
      AND nlpla EQ lv_nlpla
      AND refnr EQ lv_group.
  IF sy-subrc NE 0.
    ls_zwmfrt004-mandt  = sy-mandt.
    ls_zwmfrt004-lgnum  = i_lgnum.
    ls_zwmfrt004-nlpla  = lv_nlpla.
    ls_zwmfrt004-refnr  = lv_group.

*    INSERT zwmfrt004 FROM ls_zwmfrt004.

    INSERT ls_zwmfrt004 INTO TABLE gt_zwmfrt004[].
  ENDIF.

  lv_dbmax  = 0.

  SELECT MAX( posnr )
    FROM zwmfrt005 INTO lv_dbmax
    WHERE lgnum EQ i_lgnum
      AND nlpla EQ lv_nlpla
      AND refnr EQ lv_group.

  ls_zwmfrt005-mandt  = sy-mandt.
  ls_zwmfrt005-lgnum  = i_lgnum.
  ls_zwmfrt005-nlpla  = lv_nlpla.
  ls_zwmfrt005-refnr  = lv_group.
  ls_zwmfrt005-lenum  = i_lenum.
  ls_zwmfrt005-posnr  = lv_dbmax + 1.

*  INSERT zwmfrt005 FROM ls_zwmfrt005.

  INSERT ls_zwmfrt005 INTO TABLE gt_zwmfrt005[].
ENDFUNCTION.
