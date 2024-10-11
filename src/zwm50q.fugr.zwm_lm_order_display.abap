FUNCTION ZWM_LM_ORDER_DISPLAY.
*"--------------------------------------------------------------------
*"*"Interface local:
*"       IMPORTING
*"             VALUE(IF_FLAG_INBOUND) TYPE  CHAR1 DEFAULT SPACE
*"       TABLES
*"              CT_WORKTAB STRUCTURE  ZWMOV
*"              ET_TVST STRUCTURE  TVST OPTIONAL
*"       EXCEPTIONS
*"              NO_PERMISSION
*"--------------------------------------------------------------------

  DATA: LT_SELECT LIKE GS_SELECT OCCURS 0 WITH HEADER LINE,
        LF_TVST_LINES TYPE I,
        LF_TEXTLINE(40) TYPE C,
        LF_TEXT_VBELN(12) TYPE C,
        LF_ANSWER TYPE C.

* Extract document numbers of the selected deliveries
  LOOP AT CT_WORKTAB.
    LT_SELECT-VBELN = CT_WORKTAB-VBELN.
    COLLECT LT_SELECT.
    ET_TVST-VSTEL = CT_WORKTAB-VSTEL.
    COLLECT ET_TVST.
  ENDLOOP.

* Check display authority for selected shipping points
*  LOOP AT ET_TVST.
*    AUTHORITY-CHECK OBJECT 'V_LIKP_VST'
*             ID 'VSTEL' FIELD ET_TVST-VSTEL
*             ID 'ACTVT' FIELD '03'.
*    IF SY-SUBRC EQ 0.
*      DELETE ET_TVST.
*    ENDIF.
*  ENDLOOP.
*  DESCRIBE TABLE ET_TVST LINES LF_TVST_LINES.
*  IF LF_TVST_LINES NE 0.
*    RAISE NO_PERMISSION.
*  ENDIF.

* Call display transaction for the selected documents
  LOOP AT LT_SELECT.
    IF SY-TABIX GT 1.
      LF_TEXTLINE = 'Nächsten Beleg &1 bearbeiten?'(006).
      WRITE LT_SELECT-VBELN TO LF_TEXT_VBELN NO-ZERO.
      REPLACE '&1' WITH LF_TEXT_VBELN INTO LF_TEXTLINE.
      CONDENSE LF_TEXTLINE.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
           EXPORTING
*               DEFAULTOPTION  = 'Y'
                TEXTLINE1      = LF_TEXTLINE
*               TEXTLINE2      = ' '
                TITEL          = 'Markierte Lieferungen anzeigen'(002)
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
*    IF IF_FLAG_INBOUND EQ SPACE.
      SET PARAMETER ID 'AUN' FIELD LT_SELECT-VBELN.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
*    ELSE.
*      SET PARAMETER ID 'VLM' FIELD LT_SELECT-VBELN. "Lieferavis
*      SET PARAMETER ID 'VLG' FIELD LT_SELECT-VBELN. "GrobWE
*      CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
*    ENDIF.
    PERFORM CHANGE_COLOR TABLES CT_WORKTAB
                         USING  LT_SELECT-VBELN
                                POSNR_INITIAL
                                COLOR_VISIT.
  ENDLOOP.

ENDFUNCTION.
