FUNCTION ZWM_LM_TRANSFER_ORDER_DISPLAY.
*"--------------------------------------------------------------------
*"*"Interface local:
*"       IMPORTING
*"             VALUE(IF_TANUM) LIKE  LTAK-TANUM
*"             VALUE(IF_LGNUM) LIKE  LTAK-LGNUM
*"       EXCEPTIONS
*"              NO_PERMISSION
*"--------------------------------------------------------------------

  AUTHORITY-CHECK OBJECT 'L_TCODE'
           ID 'TCD' FIELD 'LT21'.
  IF SY-SUBRC NE 0.
    RAISE NO_PERMISSION.
  ENDIF.

  SET PARAMETER ID 'TAN' FIELD IF_TANUM.
  SET PARAMETER ID 'LGN' FIELD IF_LGNUM.

  CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN.

ENDFUNCTION.
