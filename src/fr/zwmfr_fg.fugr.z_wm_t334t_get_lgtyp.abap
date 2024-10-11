FUNCTION z_wm_t334t_get_lgtyp.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_KZEAR) TYPE  T334TKZEAR DEFAULT 'A'
*"     REFERENCE(I_LGTKZ) TYPE  LVS_LGTKZ OPTIONAL
*"     REFERENCE(I_BESTQ) TYPE  BESTQ OPTIONAL
*"     REFERENCE(I_SOBKZ) TYPE  SOBKZ OPTIONAL
*"     REFERENCE(I_LAGKL) TYPE  LVS_LAGKL OPTIONAL
*"     REFERENCE(I_WGFKL) TYPE  MGEF_WGFKL OPTIONAL
*"     REFERENCE(I_BWREF) TYPE  BWLVS
*"     REFERENCE(I_LGREF) TYPE  LVS_LGREF OPTIONAL
*"     REFERENCE(I_MATNR) TYPE  MATNR OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_LGTYP) TYPE  MAWM_INT_TT_LGTYP
*"----------------------------------------------------------------------
  DATA lv_ltkza     TYPE t334t-lgtkz.
  DATA lv_bestq     TYPE t334t-bestq.
  DATA lv_sobkz     TYPE t334t-sobkz.
  DATA lv_lagkl     TYPE t334t-lagkl.
  DATA lv_wgfkl     TYPE t334t-wgfkl.
  DATA lv_lgref     TYPE t334t-lgref.
  DATA lv_counter   TYPE i.
  DATA lv_counter_c TYPE char2.
  DATA lv_lgtyp_col TYPE fieldname.

  DATA ls_t334t TYPE t334t.

  FIELD-SYMBOLS <fs_lgtyp>  TYPE lgtyp.
*"----------------------------------------------------------------------
  IF i_matnr IS NOT SUPPLIED AND i_lgtkz IS NOT SUPPLIED.
    RETURN.
  ENDIF.

  IF i_matnr IS SUPPLIED.
    SELECT SINGLE ltkza
      FROM mlgn INTO lv_ltkza
      WHERE matnr EQ i_matnr
        AND lgnum EQ i_lgnum.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
  ENDIF.

  IF i_lgtkz IS SUPPLIED.
    lv_ltkza  = i_lgtkz.
  ENDIF.

  IF i_bestq IS SUPPLIED.
    lv_bestq  = i_bestq.
  ENDIF.

  IF i_sobkz IS SUPPLIED.
    lv_sobkz  = i_sobkz.
  ENDIF.

  IF i_lagkl IS SUPPLIED.
    lv_lagkl  = i_lagkl.
  ENDIF.

  IF i_wgfkl IS SUPPLIED.
    lv_wgfkl  = i_wgfkl.
  ENDIF.

  IF i_lgref IS SUPPLIED.
    lv_lgref  = i_lgref.
  ENDIF.

  SELECT SINGLE *
    FROM t334t INTO ls_t334t
    WHERE lgnum EQ i_lgnum
      AND kzear EQ i_kzear
      AND lgtkz EQ lv_ltkza
      AND bestq EQ lv_bestq
      AND sobkz EQ lv_sobkz
      AND lagkl EQ lv_lagkl
      AND wgfkl EQ lv_wgfkl
      AND bwref EQ i_bwref
      AND lgref EQ lv_lgref.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  lv_counter  = 0.
  DO 29 TIMES.
    lv_counter_c  = lv_counter.
    lv_counter    = lv_counter + 1.
    CONDENSE lv_counter_c.

    CLEAR lv_lgtyp_col.
    IF lv_counter LE 9.
      CONCATENATE 'LGTY' lv_counter_c INTO lv_lgtyp_col.
    ELSE.
      CONCATENATE 'LGT' lv_counter_c INTO lv_lgtyp_col.
    ENDIF.
    CONDENSE lv_lgtyp_col.

    ASSIGN COMPONENT lv_lgtyp_col OF STRUCTURE ls_t334t TO <fs_lgtyp>.
    CHECK sy-subrc EQ 0.
    CHECK <fs_lgtyp> IS NOT INITIAL.

    APPEND <fs_lgtyp> TO et_lgtyp[].
  ENDDO.
ENDFUNCTION.
