FUNCTION ZWM_SPLIT_BIN.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(BIN) TYPE  CHAR14
*"  EXPORTING
*"     REFERENCE(LGTYP) TYPE  LGTYP
*"     REFERENCE(LGPLA) TYPE  LGPLA
*"----------------------------------------------------------------------

  MOVE BIN(3) TO LGTYP.
  MOVE BIN+4(10) TO LGPLA.

ENDFUNCTION.
