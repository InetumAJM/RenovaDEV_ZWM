FUNCTION ZWM_LM_DOCUMENT_FLOW_DISPLAY.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      CT_WORKTAB STRUCTURE  ZWMOV
*"----------------------------------------------------------------------

  DATA: LF_FLAG_ITEMS TYPE C,
        LF_ANSWER TYPE C,
        LS_VBCO6 LIKE VBCO6,
        LF_TEXTLINE(50) TYPE C,
        LF_TEXT_VBELN(12) TYPE C,
        LF_TEXT_POSNR(8) TYPE C.

  PERFORM ITEM_AVAILABLE_CHECK TABLES   CT_WORKTAB
                               CHANGING LF_FLAG_ITEMS.

  LOOP AT CT_WORKTAB.
    IF SY-TABIX GT 1.
      IF LF_FLAG_ITEMS NE SPACE.
        LF_TEXTLINE = 'Nächste Position &1/&2 bearbeiten?'(012).
        WRITE CT_WORKTAB-VBELN TO LF_TEXT_VBELN NO-ZERO.
        WRITE CT_WORKTAB-POSNR TO LF_TEXT_POSNR NO-ZERO.
        REPLACE '&1' WITH LF_TEXT_VBELN INTO LF_TEXTLINE.
        REPLACE '&2' WITH LF_TEXT_POSNR INTO LF_TEXTLINE.
      ELSE.
        LF_TEXTLINE = 'Nächsten Beleg &1 bearbeiten?'(006).
        WRITE CT_WORKTAB-VBELN TO LF_TEXT_VBELN NO-ZERO.
        REPLACE '&1' WITH LF_TEXT_VBELN INTO LF_TEXTLINE.
      ENDIF.
      CONDENSE LF_TEXTLINE.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
           EXPORTING
*               DEFAULTOPTION  = 'Y'
                TEXTLINE1      = LF_TEXTLINE
*               TEXTLINE2      = ' '
                TITEL          = 'Belegfluß anzeigen'(003)
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
    CLEAR LS_VBCO6.
    LS_VBCO6-MANDT = SY-MANDT.
    LS_VBCO6-VBELN = CT_WORKTAB-VBELN.
    IF LF_FLAG_ITEMS NE SPACE.
      LS_VBCO6-POSNR = CT_WORKTAB-POSNR.
    ENDIF.
    CALL DIALOG   'RV_DOCUMENT_FLOW'
         EXPORTING
              VBCO6         FROM LS_VBCO6
              MAKT-MAKTX    FROM CT_WORKTAB-ARKTX
              KNA1-KUNNR    FROM CT_WORKTAB-KUNNR
              KNA1-NAME1    FROM CT_WORKTAB-NAME_WE
              MAKT-MATNR    FROM CT_WORKTAB-MATNR
              TVAKT-BEZEI   FROM SPACE
              TVAPT-VTEXT   FROM SPACE
*           ERDAT         FROM SPACE
              IVKORG        FROM CT_WORKTAB-VKORG
              IVTWEG        FROM CT_WORKTAB-VTWEG.
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
  ENDLOOP.

ENDFUNCTION.
