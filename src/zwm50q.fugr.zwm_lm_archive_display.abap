FUNCTION ZWM_LM_ARCHIVE_DISPLAY.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      CT_WORKTAB STRUCTURE  ZWMOV
*"----------------------------------------------------------------------
  DATA: LT_SELECT LIKE GS_SELECT OCCURS 10 WITH HEADER LINE,
        LF_ANSWER TYPE C,
        LF_TEXTLINE(40) TYPE C,
        LF_TEXT_VBELN(12) TYPE C.

* Extract document numbers from worktable
  LOOP AT CT_WORKTAB.
    LT_SELECT-VBELN = CT_WORKTAB-VBELN.
    COLLECT LT_SELECT.
  ENDLOOP.

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
                TITEL          = 'Verknüpfte Originale anzeigen'(005)
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
    CALL FUNCTION 'RV_DISPLAY_ARCHIV'
         EXPORTING
              VBELN               = LT_SELECT-VBELN
              VBTYP               = 'J'
         EXCEPTIONS
              ARCHIVIERTER_BELEG  = 1
              KEIN_T681Z_EINTRAG  = 2
              KEIN_VERTRIEBSBELEG = 3
              OTHERS              = 4.
    IF SY-SUBRC NE 0.                  "only exception 2 may appear
      MESSAGE S016 WITH LT_SELECT-VBELN.
    ENDIF.
    PERFORM CHANGE_COLOR TABLES CT_WORKTAB
                         USING  LT_SELECT-VBELN
                                POSNR_INITIAL
                                COLOR_VISIT.
  ENDLOOP.


ENDFUNCTION.
