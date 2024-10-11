FUNCTION ZWM_CONCATENATE_BIN.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LGTYP) TYPE  LGTYP
*"     REFERENCE(LGPLA) TYPE  LGPLA
*"  EXPORTING
*"     REFERENCE(BIN) TYPE  CHAR14
*"----------------------------------------------------------------------

  CONCATENATE LGTYP LGPLA INTO BIN SEPARATED BY SPACE.

ENDFUNCTION.
