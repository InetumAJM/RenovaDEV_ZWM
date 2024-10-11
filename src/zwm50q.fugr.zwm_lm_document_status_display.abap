FUNCTION ZWM_LM_DOCUMENT_STATUS_DISPLAY.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      CT_WORKTAB STRUCTURE  ZWMOV
*"----------------------------------------------------------------------
  DATA: LF_FLAG_ITEMS TYPE C,
        LF_EXIT_FLAG TYPE C,
        LS_VBUP LIKE VBUP,
        LS_VBUK LIKE VBUK,
        LF_FCODE LIKE SY-UCOMM,
        LF_WORKTAB_LINES TYPE I,
        LF_WORKTAB_INDEX LIKE SY-TABIX.

  PERFORM ITEM_AVAILABLE_CHECK TABLES   CT_WORKTAB
                               CHANGING LF_FLAG_ITEMS.

  CLEAR LF_EXIT_FLAG.
  LF_WORKTAB_INDEX = 1.
  DESCRIBE TABLE CT_WORKTAB LINES LF_WORKTAB_LINES.

  WHILE LF_EXIT_FLAG EQ SPACE.
    READ TABLE CT_WORKTAB INDEX LF_WORKTAB_INDEX.
    IF LF_FLAG_ITEMS NE SPACE.
      CLEAR LS_VBUP.
      SELECT SINGLE * FROM VBUK
                      INTO LS_VBUK
                      WHERE VBELN EQ CT_WORKTAB-VBELN.
      IF SY-SUBRC EQ 0.
        LS_VBUK-VBOBJ = 'L'.
        SELECT SINGLE * FROM VBUP
                        INTO LS_VBUP
                        WHERE VBELN EQ CT_WORKTAB-VBELN
                        AND   POSNR EQ CT_WORKTAB-POSNR.
        IF SY-SUBRC EQ 0.
          LS_VBUK-VBOBJ = 'L'.
          CALL FUNCTION 'RV_DOCUMENT_POS_STATUS_TEXTS'
               EXPORTING
                    VBUP_IN       = LS_VBUP
                    VBUK_IN       = LS_VBUK
                    WINDOW_SENDEN = 'X'
                    VBUP_GELESEN  = 'X'
                    VBTYP         = LS_VBUK-VBTYP
               IMPORTING
                    FCODE         = LF_FCODE.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR LS_VBUK.
      SELECT SINGLE * FROM VBUK
                      INTO LS_VBUK
                      WHERE VBELN EQ CT_WORKTAB-VBELN.
      IF SY-SUBRC EQ 0.
        LS_VBUK-VBOBJ = 'L'.
        CALL FUNCTION 'RV_DOCUMENT_HEAD_STATUS_TEXTS'
             EXPORTING
                  VBUK_IN       = LS_VBUK
                  WINDOW_SENDEN = 'X'
                  VBUK_GELESEN  = 'X'
                  VBTYP         = LS_VBUK-VBTYP
             IMPORTING
                  FCODE         = LF_FCODE.
      ENDIF.
    ENDIF.
    IF LF_FLAG_ITEMS NE SPACE.
      PERFORM CHANGE_COLOR TABLES CT_WORKTAB
                           USING  CT_WORKTAB-VBELN
                                  CT_WORKTAB-POSNR
                                  COLOR_VISIT.
    ELSE.
      PERFORM CHANGE_COLOR TABLES CT_WORKTAB
                           USING  CT_WORKTAB-VBELN
                                  POSNR_INITIAL
                                  COLOR_VISIT.
    ENDIF.
    CASE LF_FCODE.
      WHEN 'OBJ-'.
        IF LF_WORKTAB_INDEX GT 1.
          SUBTRACT 1 FROM LF_WORKTAB_INDEX.
        ELSE.
          MESSAGE S015.                "no more marked documents
        ENDIF.
      WHEN 'OBJ+'.
        IF LF_WORKTAB_INDEX LT LF_WORKTAB_LINES.
          ADD 1 TO LF_WORKTAB_INDEX.
        ELSE.
          MESSAGE S015.                "no more marked documents
        ENDIF.
      WHEN OTHERS.
        LF_EXIT_FLAG = CHARX.
    ENDCASE.

  ENDWHILE.

ENDFUNCTION.
