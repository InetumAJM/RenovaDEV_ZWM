FUNCTION zwm_get_mesa_wcs.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_LGTYP) TYPE  LGTYP
*"     REFERENCE(I_LGPLA) TYPE  LGPLA OPTIONAL
*"     REFERENCE(I_TYPE) TYPE  ZWM_WCS_TYPE
*"  EXPORTING
*"     REFERENCE(E_MESA) TYPE  ZWM_WCS_POINT
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: ls_zwm075 TYPE zwm075.

  SELECT SINGLE *
    FROM zwm075 INTO ls_zwm075
    WHERE lgnum    = i_lgnum
    AND   lgtyp    = i_lgtyp
    AND   lgpla    = i_lgpla
    AND   wcs_type = i_type.

  IF sy-subrc <> 0.
    RAISE error.
  ELSE.
    e_mesa = ls_zwm075-wcs_point.
  ENDIF.


ENDFUNCTION.
