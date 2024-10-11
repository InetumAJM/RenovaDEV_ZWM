FUNCTION ZWM_LM_VENDOR_DISPLAY.
*"--------------------------------------------------------------------
*"*"Interface local:
*"       TABLES
*"              CT_WORKTAB STRUCTURE  ZWMOV
*"       EXCEPTIONS
*"              NO_PERMISSION
*"--------------------------------------------------------------------
  DATA: BEGIN OF LT_SELECT OCCURS 10,
          LIFNR LIKE ZWMOV-LIFNR,
        END OF LT_SELECT,
        LF_ANSWER TYPE C,
        LF_TRANSACTION LIKE SY-TCODE,
        LF_TEXTLINE1(50) TYPE C.

* Check permission: If no permission for MK02 check permission for MK03
  AUTHORITY-CHECK OBJECT 'F_LFA1_APP'
           ID 'ACTVT' FIELD '02'
           ID 'APPKZ' FIELD 'M'.
  IF SY-SUBRC NE 0.
    AUTHORITY-CHECK OBJECT 'F_LFA1_APP'
             ID 'ACTVT' FIELD '03'
             ID 'APPKZ' FIELD 'M'.
    IF SY-SUBRC NE 0.
      RAISE NO_PERMISSION.
    ELSE.
      LF_TRANSACTION = 'MK03'.
    ENDIF.
  ELSE.
    LF_TRANSACTION = 'MK02'.
  ENDIF.

  LOOP AT CT_WORKTAB.
    LT_SELECT-LIFNR = CT_WORKTAB-LIFNR.
    COLLECT LT_SELECT.
  ENDLOOP.

  LOOP AT LT_SELECT.
    IF SY-TABIX GT 1.
      LF_TEXTLINE1 = 'Nächsten Lieferanten &1 anzeigen?'(034).
      REPLACE '&1' WITH LT_SELECT-LIFNR INTO LF_TEXTLINE1.
      CONDENSE LF_TEXTLINE1.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
           EXPORTING
*               DEFAULTOPTION  = 'Y'
                TEXTLINE1      = LF_TEXTLINE1
                TITEL          = 'Lieferantenstamm anzeigen'(035)
*               START_COLUMN   = 25
*               START_ROW      = 6
*               CANCEL_DISPLAY = 'X'
           IMPORTING
                ANSWER         = LF_ANSWER.
      CASE LF_ANSWER.
        WHEN 'N'.
          CONTINUE.
        WHEN 'A'.
          EXIT.
      ENDCASE.
    ENDIF.
    SET PARAMETER ID 'LIF' FIELD LT_SELECT-LIFNR.
    CALL TRANSACTION LF_TRANSACTION.
  ENDLOOP.

ENDFUNCTION.
