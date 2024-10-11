FUNCTION ZWM_LM_CALCULATE_DATE_RANGE.
*"--------------------------------------------------------------------
*"*"Interface local:
*"       IMPORTING
*"             VALUE(IF_TODAY) LIKE  SY-DATLO DEFAULT SY-DATLO
*"             VALUE(IF_DATERANGE) TYPE  LE_SHP_DATERANGE
*"             VALUE(IF_CALENDAR) LIKE  SCAL-FCALID OPTIONAL
*"       TABLES
*"              IT_VSTEL STRUCTURE  RANGE_C4 OPTIONAL
*"              ET_DATE STRUCTURE  RANGE_DATE
*"       EXCEPTIONS
*"              MISSING_PARAMETER
*"              DATE_RANGE_NOT_DETERMINED
*"--------------------------------------------------------------------

  DATA: lf_newdate  LIKE sy-datlo,
        lf_lastdate LIKE sy-datlo,
        lf_enddate  LIKE sy-datlo,
        lf_week LIKE scal-week,
        lt_vstel LIKE tvst OCCURS 0 WITH HEADER LINE,
        BEGIN OF lt_factory OCCURS 0,
          fabkl LIKE tvst-fabkl,
        END OF lt_factory.

  IF NOT it_vstel IS REQUESTED AND
     if_calendar IS INITIAL.
    RAISE missing_parameter.
  ENDIF.

  IF NOT if_calendar IS INITIAL.
    lt_factory = lt_factory-fabkl.
    APPEND lt_factory.
  ELSE.
    SELECT * FROM tvst
             INTO TABLE lt_vstel
             WHERE vstel IN it_vstel.
    LOOP AT lt_vstel.
      lt_factory-fabkl = lt_vstel-fabkl.
      COLLECT lt_factory.
    ENDLOOP.
  ENDIF.

  CASE if_daterange.
    WHEN 'A'.                          "today
      et_date-sign   = 'I'.
      et_date-option = 'EQ'.
      et_date-low = if_today.
      CLEAR et_date-high.
      APPEND et_date.
    WHEN 'B'.                          "today and tomorrow
      lf_enddate = if_today + 1.
      CLEAR lf_lastdate.
      LOOP AT lt_factory.
        CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
             EXPORTING
                  correct_option               = '+'
                  date                         = lf_enddate
                  factory_calendar_id          = lt_factory-fabkl
             IMPORTING
                  date                         = lf_newdate
             EXCEPTIONS
                  calendar_buffer_not_loadable = 1
                  correct_option_invalid       = 2
                  date_after_range             = 3
                  date_before_range            = 4
                  date_invalid                 = 5
                  factory_calendar_not_found   = 6
                  OTHERS                       = 7.
        IF sy-subrc <> 0.
          RAISE date_range_not_determined.
        ENDIF.
        IF lf_newdate GT lf_lastdate.
          lf_lastdate = lf_newdate.
        ENDIF.
      ENDLOOP.
      et_date-sign   = 'I'.
      et_date-option = 'EQ'.
      et_date-low = if_today.
      et_date-high = lf_lastdate.
      APPEND et_date.
    WHEN 'C'. "today and the next two working days
      lf_enddate = if_today.
      DO 2 TIMES.
        ADD 1 TO lf_enddate.
        CLEAR lf_lastdate.
        LOOP AT lt_factory.
          CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
               EXPORTING
                    correct_option               = '+'
                    date                         = lf_enddate
                    factory_calendar_id          = lt_factory-fabkl
               IMPORTING
                    date                         = lf_newdate
               EXCEPTIONS
                    calendar_buffer_not_loadable = 1
                    correct_option_invalid       = 2
                    date_after_range             = 3
                    date_before_range            = 4
                    date_invalid                 = 5
                    factory_calendar_not_found   = 6
                    OTHERS                       = 7.
          IF sy-subrc <> 0.
            RAISE date_range_not_determined.
          ENDIF.
          IF lf_newdate GT lf_lastdate.
            lf_lastdate = lf_newdate.
          ENDIF.
          lf_enddate = lf_lastdate.
        ENDLOOP.
      ENDDO.
      et_date-sign   = 'I'.
      et_date-option = 'EQ'.
      et_date-low = if_today.
      et_date-high = lf_lastdate.
      APPEND et_date.
    WHEN 'D'.                          "until the end of this week
      CALL FUNCTION 'DATE_GET_WEEK'
           EXPORTING
                date         = if_today                  "#EC DOM_EQUAL
           IMPORTING
                week         = lf_week
           EXCEPTIONS
                date_invalid = 1
                OTHERS       = 2.
      IF sy-subrc <> 0.
        RAISE date_range_not_determined.
      ENDIF.
      CALL FUNCTION 'WEEK_GET_FIRST_DAY'
           EXPORTING
                week         = lf_week
           IMPORTING
                date         = lf_newdate
           EXCEPTIONS
                week_invalid = 1
                OTHERS       = 2.
      IF sy-subrc <> 0.
        RAISE date_range_not_determined.
      ENDIF.
      lf_lastdate = lf_newdate + 6.
      et_date-sign   = 'I'.
      et_date-option = 'EQ'.
      et_date-low = if_today.
      et_date-high = lf_lastdate.
      APPEND et_date.
    WHEN 'E'.
      CALL FUNCTION 'DATE_GET_WEEK'
           EXPORTING
                date         = if_today                  "#EC DOM_EQUAL
           IMPORTING
                week         = lf_week
           EXCEPTIONS
                date_invalid = 1
                OTHERS       = 2.
      IF sy-subrc NE 0.
        RAISE date_range_not_determined.
      ENDIF.
      CALL FUNCTION 'WEEK_GET_FIRST_DAY'
           EXPORTING
                week         = lf_week
           IMPORTING
                date         = lf_newdate
           EXCEPTIONS
                week_invalid = 1
                OTHERS       = 2.
      IF sy-subrc <> 0.
        RAISE date_range_not_determined.
      ENDIF.
      lf_lastdate = lf_newdate + 13.
      et_date-sign   = 'I'.
      et_date-option = 'EQ'.
      et_date-low = if_today.
      et_date-high = lf_lastdate.
      APPEND et_date.
  ENDCASE.

ENDFUNCTION.
