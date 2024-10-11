FUNCTION zwm_get_tud_wcs.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_LETYP) TYPE  LVS_LETYP
*"  EXPORTING
*"     REFERENCE(E_WCS_LETYP) TYPE  ZWM_WCS_LETYP
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: ls_zwm079 TYPE zwm079.

  SELECT SINGLE *
    FROM zwm079 INTO ls_zwm079
    WHERE lgnum = i_lgnum
    AND   letyp = i_letyp.
  IF sy-subrc <> 0.
    RAISE error.
  ELSE.
    e_wcs_letyp = ls_zwm079-wcs_letyp.
  ENDIF.


ENDFUNCTION.
