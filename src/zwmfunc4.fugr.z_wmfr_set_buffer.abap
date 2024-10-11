FUNCTION z_wmfr_set_buffer.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"----------------------------------------------------------------------
  FREE itab_zwm001[].

  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs       = i_lgnum
    TABLES
      ti_zwm001 = itab_zwm001[].
ENDFUNCTION.
