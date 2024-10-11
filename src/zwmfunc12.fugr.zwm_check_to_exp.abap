FUNCTION zwm_check_to_exp .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"     REFERENCE(I_VBELN) TYPE  VBELN_VL
*"  EXPORTING
*"     REFERENCE(CS_ZWM028) TYPE  ZWM028
*"----------------------------------------------------------------------
  DATA: ls_zwm040 TYPE zwm040.
  DATA: ls_zwm028 TYPE zwm028.

  SELECT SINGLE *
    FROM zwm040 INTO ls_zwm040
    WHERE lgnum   = i_lgnum
    AND   refnr   = i_refnr
    AND   remessa = i_vbeln.

  IF sy-subrc = 0.
    SELECT SINGLE *
      FROM zwm028 INTO ls_zwm028
      WHERE lgnum   = ls_zwm040-lgnum
      AND   refnr   = ls_zwm040-refnr
      AND   remessa = ls_zwm040-id_servisan.
  ELSE.
    SELECT SINGLE *
      FROM zwm028 INTO ls_zwm028
      WHERE lgnum   = i_lgnum
      AND   refnr   = i_refnr
      AND   remessa = i_vbeln.
  ENDIF.

  cs_zwm028 = ls_zwm028.

ENDFUNCTION.
