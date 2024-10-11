FUNCTION ZWM_LM_OUTPUT_SELECTION.
*"--------------------------------------------------------------------
*"*"Interface local:
*"       IMPORTING
*"             VALUE(IF_FLAG_INBOUND) TYPE  C DEFAULT SPACE
*"       TABLES
*"              CT_OUTPUT_V2 STRUCTURE  LIPOV_OUTPUT
*"              CT_OUTPUT_V4 STRUCTURE  LIPOV_OUTPUT
*"              CT_OUTPUT_V5 STRUCTURE  LIPOV_OUTPUT
*"              CT_OUTPUT_E1 STRUCTURE  LIPOV_OUTPUT OPTIONAL
*"--------------------------------------------------------------------
  DATA: LT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
        LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
        LS_LAYOUT TYPE SLIS_LAYOUT_ALV,
        LS_KEYINFO TYPE SLIS_KEYINFO_ALV,
        LT_OUTPUT_ALL LIKE LIPOV_OUTPUT OCCURS 0 WITH HEADER LINE,
        LT_OUTPUT_HEADER LIKE LIPOV_OUTPUT_HEADER OCCURS 0
                               WITH HEADER LINE,
        LS_SORT TYPE SLIS_SORTINFO_ALV,
        LT_SORT TYPE SLIS_T_SORTINFO_ALV,
        LF_ANSWER TYPE C.

* Copy output selection to display table
  IF IF_FLAG_INBOUND EQ SPACE.
    LOOP AT CT_OUTPUT_V2.
      MOVE-CORRESPONDING CT_OUTPUT_V2 TO LT_OUTPUT_ALL.
      LT_OUTPUT_ALL-KAPPL = 'V2'.
      APPEND LT_OUTPUT_ALL.
    ENDLOOP.
    LOOP AT CT_OUTPUT_V4.
      MOVE-CORRESPONDING CT_OUTPUT_V4 TO LT_OUTPUT_ALL.
      LT_OUTPUT_ALL-KAPPL = 'V4'.
      APPEND LT_OUTPUT_ALL.
    ENDLOOP.
    LOOP AT CT_OUTPUT_V5.
      MOVE-CORRESPONDING CT_OUTPUT_V5 TO LT_OUTPUT_ALL.
      LT_OUTPUT_ALL-KAPPL = 'V5'.
      APPEND LT_OUTPUT_ALL.
    ENDLOOP.
  ELSE.
    LOOP AT CT_OUTPUT_E1.
      MOVE-CORRESPONDING CT_OUTPUT_E1 TO LT_OUTPUT_ALL.
      LT_OUTPUT_ALL-KAPPL = 'E1'.
      APPEND LT_OUTPUT_ALL.
    ENDLOOP.
  ENDIF.
* Fill header table
  IF IF_FLAG_INBOUND EQ SPACE.
    LT_OUTPUT_HEADER-KAPPL = 'V2'.
    LT_OUTPUT_HEADER-VTEXT = 'Lieferungsnachrichten'(013).
    APPEND LT_OUTPUT_HEADER.
    LT_OUTPUT_HEADER-KAPPL = 'V4'.
    LT_OUTPUT_HEADER-VTEXT = 'Kommissioniernachrichten'(015).
    APPEND LT_OUTPUT_HEADER.
    LT_OUTPUT_HEADER-KAPPL = 'V5'.
    LT_OUTPUT_HEADER-VTEXT = 'Gruppennachrichten'(014).
    APPEND LT_OUTPUT_HEADER.
  ELSE.
    LT_OUTPUT_HEADER-KAPPL = 'E1'.
    LT_OUTPUT_HEADER-VTEXT = 'Anlieferungsnachrichten'(033).
    APPEND LT_OUTPUT_HEADER.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME         = 'SAPLZWM50Q'
            I_INTERNAL_TABNAME     = 'LT_OUTPUT_HEADER'
            I_STRUCTURE_NAME       = 'LIPOV_OUTPUT_HEADER'
            I_CLIENT_NEVER_DISPLAY = 'X'
       CHANGING
            CT_FIELDCAT            = LT_FIELDCAT
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE E500 WITH 'Error: REUSE_ALV_FIELDCATALOG_MERGE'.
                                       "#EC NOTEXT
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME         = 'SAPLZWM50Q'
            I_INTERNAL_TABNAME     = 'LT_OUTPUT_ALL'
            I_STRUCTURE_NAME       = 'LIPOV_OUTPUT'
            I_CLIENT_NEVER_DISPLAY = 'X'
       CHANGING
            CT_FIELDCAT            = LT_FIELDCAT
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.
  IF SY-SUBRC <> 0.
    MESSAGE E500 WITH 'Error: REUSE_ALV_FIELDCATALOG_MERGE'. "#EC NOTEXT
  ENDIF.

* Modify fieldcatalog, layout and sorting
  LS_FIELDCAT-TECH = CHARX.
  MODIFY LT_FIELDCAT FROM LS_FIELDCAT TRANSPORTING TECH
                     WHERE FIELDNAME EQ 'SELKZ'
                     OR    FIELDNAME EQ 'KAPPL'
                     OR    FIELDNAME EQ 'EXPAND'.

  LS_LAYOUT-COLWIDTH_OPTIMIZE = CHARX.
  LS_LAYOUT-ZEBRA = CHARX.
  LS_LAYOUT-BOX_FIELDNAME = 'SELKZ'.
  LS_LAYOUT-NO_VLINE = SPACE.
  LS_LAYOUT-EXPAND_FIELDNAME = 'EXPAND'.
  LS_LAYOUT-BOX_TABNAME = 'LT_OUTPUT_ALL'.

  LS_KEYINFO-HEADER01 = 'KAPPL'.
  LS_KEYINFO-ITEM01 = 'KAPPL'.

  REFRESH LT_SORT.
  LS_SORT-SPOS = 1.
  LS_SORT-FIELDNAME = 'KAPPL'.
  LS_SORT-TABNAME = 'LT_OUTPUT_ALL'.
  LS_SORT-UP = CHARX.
  APPEND LS_SORT TO LT_SORT.
  LS_SORT-SPOS = 2.
  LS_SORT-FIELDNAME = 'KSCHL'.
  LS_SORT-TABNAME = 'LT_OUTPUT_ALL'.
  LS_SORT-UP = CHARX.
  APPEND LS_SORT TO LT_SORT.

  DO.
    CLEAR LF_ANSWER.
* Display list: Maintaining variants is not supported here
    CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
         EXPORTING
              I_INTERFACE_CHECK        = ' '
              I_CALLBACK_PROGRAM       = 'SAPLZWM50Q'
              I_CALLBACK_PF_STATUS_SET = 'SET_STATUS_OUTPUT'
              IS_LAYOUT                = LS_LAYOUT
              IT_FIELDCAT              = LT_FIELDCAT
              IT_SORT                  = LT_SORT
              I_TABNAME_HEADER         = 'LT_OUTPUT_HEADER'
              I_TABNAME_ITEM           = 'LT_OUTPUT_ALL'
              I_STRUCTURE_NAME_HEADER  = 'LIPOV_OUTPUT_HEADER'
              I_STRUCTURE_NAME_ITEM    = 'LIPOV_OUTPUT'
              IS_KEYINFO               = LS_KEYINFO
         TABLES
              T_OUTTAB_HEADER          = LT_OUTPUT_HEADER
              T_OUTTAB_ITEM            = LT_OUTPUT_ALL
         EXCEPTIONS
              PROGRAM_ERROR            = 1
              OTHERS                   = 2.
    IF SY-SUBRC <> 0.
      MESSAGE E500 WITH 'Error: REUSE_ALV_HIERSEQ_LIST_DISPLAY'.
                                       "#EC NOTEXT
    ENDIF.

    IF SY-UCOMM EQ '&F12'.
      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
           EXPORTING
                TEXTLINE1    =
                 'Wollen Sie die Nachrichtenauswahl abbrechen?'(016)
                TITEL        = 'Abbrechen der Nachrichtenauswahl'(017)
           IMPORTING
                ANSWER       = LF_ANSWER.
    ENDIF.
    IF LF_ANSWER NE 'N'.
      CLEAR SY-UCOMM.
      EXIT.
    ENDIF.
  ENDDO.
* Move selection to output tables (if not F12 was pressed)
  IF LF_ANSWER NE 'J'.
    REFRESH: CT_OUTPUT_V2, CT_OUTPUT_V4, CT_OUTPUT_V5.
    LOOP AT LT_OUTPUT_ALL.
      CASE LT_OUTPUT_ALL-KAPPL.
        WHEN 'V2'.
          CT_OUTPUT_V2 = LT_OUTPUT_ALL.
          APPEND CT_OUTPUT_V2.
        WHEN 'V4'.
          CT_OUTPUT_V4 = LT_OUTPUT_ALL.
          APPEND CT_OUTPUT_V4.
        WHEN 'V5'.
          CT_OUTPUT_V5 = LT_OUTPUT_ALL.
          APPEND CT_OUTPUT_V5.
        WHEN 'E1'.
          CT_OUTPUT_E1 = LT_OUTPUT_ALL.
          APPEND CT_OUTPUT_E1.
      ENDCASE.
    ENDLOOP.
    MESSAGE S041.
  ENDIF.
ENDFUNCTION.
