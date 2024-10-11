FUNCTION ZWM_LM_CUSTOMER_DISPLAY.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      CT_WORKTAB STRUCTURE  ZWMOV
*"  EXCEPTIONS
*"      NO_PERMISSION
*"----------------------------------------------------------------------

  DATA: BEGIN OF LT_SELECT OCCURS 10,
          KUNNR LIKE ZWMOV-KUNNR,
          VKORG LIKE ZWMOV-VKORG,
          VTWEG LIKE ZWMOV-VTWEG,
        END OF LT_SELECT,
        LF_ANSWER TYPE C,
        LF_TRANSACTION LIKE SY-TCODE,
        LF_FLAG_ITEMS TYPE C,
        LF_TEXTLINE1(50) TYPE C,
        LF_TEXTLINE2(50) TYPE C.

* Check permission: If no permission for VD02 check permission for VD03
  AUTHORITY-CHECK OBJECT 'F_KNA1_APP'
           ID 'ACTVT' FIELD '02'
           ID 'APPKZ' FIELD 'V'.
  IF SY-SUBRC NE 0.
    AUTHORITY-CHECK OBJECT 'F_KNA1_APP'
             ID 'ACTVT' FIELD '03'
             ID 'APPKZ' FIELD 'V'.
    IF SY-SUBRC NE 0.
      RAISE NO_PERMISSION.
    ELSE.
      LF_TRANSACTION = 'VD03'.
    ENDIF.
  ELSE.
    LF_TRANSACTION = 'VD02'.
  ENDIF.

  PERFORM ITEM_AVAILABLE_CHECK TABLES   CT_WORKTAB
                               CHANGING LF_FLAG_ITEMS.

  LOOP AT CT_WORKTAB.
    CLEAR LT_SELECT.
    LT_SELECT-KUNNR = CT_WORKTAB-KUNNR.
    LT_SELECT-VKORG = CT_WORKTAB-VKORG.
    LT_SELECT-VTWEG = CT_WORKTAB-VTWEG.
    COLLECT LT_SELECT.
  ENDLOOP.

  LOOP AT LT_SELECT.
    IF SY-TABIX GT 1.
      LF_TEXTLINE1 = 'Nächsten Kunden &1 anzeigen?'(010).
      REPLACE '&1' WITH LT_SELECT-KUNNR INTO LF_TEXTLINE1.
      IF LF_FLAG_ITEMS NE SPACE.
        LF_TEXTLINE2 = 'Verkaufsorganisation &1 Vertriebsweg &2'(009).
        REPLACE '&1' WITH LT_SELECT-VKORG INTO LF_TEXTLINE2.
        REPLACE '&2' WITH LT_SELECT-VTWEG INTO LF_TEXTLINE2.
      ELSE.
        LF_TEXTLINE2 = 'Verkaufsorganisation &1'(001).
        REPLACE '&1' WITH LT_SELECT-VKORG INTO LF_TEXTLINE2.
      ENDIF.
      CONDENSE LF_TEXTLINE1.
      CONDENSE LF_TEXTLINE2.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
           EXPORTING
*               DEFAULTOPTION  = 'Y'
                TEXTLINE1      = LF_TEXTLINE1
                TEXTLINE2      = LF_TEXTLINE2
                TITEL          = 'Kundenstamm anzeigen'(011)
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
    SET PARAMETER ID 'KUN' FIELD LT_SELECT-KUNNR.
    SET PARAMETER ID 'VKO' FIELD LT_SELECT-VKORG.
    IF LF_FLAG_ITEMS NE SPACE.
      SET PARAMETER ID 'VTW' FIELD LT_SELECT-VTWEG.
    ENDIF.
    CALL TRANSACTION LF_TRANSACTION.
  ENDLOOP.

ENDFUNCTION.
