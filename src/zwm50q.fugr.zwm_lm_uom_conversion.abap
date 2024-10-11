FUNCTION ZWM_LM_UOM_CONVERSION.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IF_REF_GEWEI) LIKE  LIPOV-GEWEI DEFAULT SPACE
*"     VALUE(IF_REF_VOLEH) LIKE  LIPOV-VOLEH DEFAULT SPACE
*"  TABLES
*"      CT_POSTAB STRUCTURE  ZWMOV
*"----------------------------------------------------------------------
  DATA: LT_GEWEI LIKE GS_UNIT OCCURS 10 WITH HEADER LINE,
        LT_VOLEH LIKE GS_UNIT OCCURS 10 WITH HEADER LINE,
        LS_GEWEI_MAX       LIKE GS_UNIT,
        LS_GEWEI_MIN       LIKE GS_UNIT,
        LS_GEWEI_COUNT     LIKE GS_UNIT,
        LS_GEWEI_REFERENCE LIKE GS_UNIT,
        LS_VOLEH_MAX       LIKE GS_UNIT,
        LS_VOLEH_MIN       LIKE GS_UNIT,
        LS_VOLEH_COUNT     LIKE GS_UNIT,
        LS_VOLEH_REFERENCE LIKE GS_UNIT,
        LF_FAKT_GEWEI TYPE F,
        LF_FAKT_VOLEH TYPE F.

  FIELD-SYMBOLS: <LS_POSTAB> TYPE ZWMOV.

  REFRESH: LT_GEWEI, LT_VOLEH.
  CLEAR  : LT_GEWEI, LT_VOLEH.

* Check whether the given reference units have the correct dimension
  PERFORM UNIT_DIMENSION_CHECK USING 'MASS'
                               CHANGING IF_REF_GEWEI.
  PERFORM UNIT_DIMENSION_CHECK USING 'VOLUME'
                               CHANGING IF_REF_VOLEH.

* Determine reference units of measure if not given
  IF IF_REF_GEWEI IS INITIAL OR IF_REF_VOLEH IS INITIAL.
* 1. Collect all weight and volume units on the list
    LOOP AT CT_POSTAB ASSIGNING <LS_POSTAB>
                      WHERE NOT GEWEI IS INITIAL
                      OR    NOT VOLEH IS INITIAL.
      IF NOT <LS_POSTAB>-GEWEI IS INITIAL.
        PERFORM UNIT_COLLECT TABLES LT_GEWEI
                             USING  <LS_POSTAB>-GEWEI.
      ENDIF.
      IF NOT <LS_POSTAB>-VOLEH IS INITIAL.
        PERFORM UNIT_COLLECT TABLES LT_VOLEH
                             USING  <LS_POSTAB>-VOLEH.
      ENDIF.
    ENDLOOP.

* 2. Determine greatest unit of measure
    SORT LT_GEWEI BY MAXFAKT ASCENDING.
    READ TABLE LT_GEWEI INDEX 1.
    LS_GEWEI_MAX = LT_GEWEI.
    SORT LT_VOLEH BY MAXFAKT ASCENDING.
    READ TABLE LT_VOLEH INDEX 1.
    LS_VOLEH_MAX = LT_VOLEH.

* 3. Determine smallest unit of measure
    SORT LT_GEWEI BY MAXFAKT DESCENDING.
    READ TABLE LT_GEWEI INDEX 1.
    LS_GEWEI_MIN = LT_GEWEI.
    SORT LT_VOLEH BY MAXFAKT DESCENDING.
    READ TABLE LT_VOLEH INDEX 1.
    LS_VOLEH_MIN = LT_VOLEH.

* 4. Determine the most common unit of measure
    SORT LT_GEWEI BY COUNTER DESCENDING.
    READ TABLE LT_GEWEI INDEX 1.
    LS_GEWEI_COUNT = LT_GEWEI.
    SORT LT_VOLEH BY COUNTER DESCENDING.
    READ TABLE LT_VOLEH INDEX 1.
    LS_VOLEH_COUNT = LT_VOLEH.

* 5. Determine difference between the smallest and the greatest unit
*    and determine reference unit
    LF_FAKT_GEWEI = LS_GEWEI_MIN-MAXFAKT / LS_GEWEI_MAX-MAXFAKT.
    IF LF_FAKT_GEWEI LE 1000.
      LS_GEWEI_REFERENCE = LS_GEWEI_COUNT.
    ELSE.
      LOOP AT LT_GEWEI WHERE MAXFAKT GT LS_GEWEI_MAX-MAXFAKT
                       AND   MAXFAKT LT LS_GEWEI_MIN-MAXFAKT.
        LS_GEWEI_REFERENCE = LT_GEWEI.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        LS_GEWEI_REFERENCE = LS_GEWEI_MAX.
      ENDIF.
    ENDIF.

    LF_FAKT_VOLEH = LS_VOLEH_MIN-MAXFAKT / LS_VOLEH_MAX-MAXFAKT.
    IF LF_FAKT_VOLEH LE 1000.
      LS_VOLEH_REFERENCE = LS_VOLEH_COUNT.
    ELSE.
      LOOP AT LT_VOLEH WHERE MAXFAKT GT LS_VOLEH_MAX-MAXFAKT
                       AND   MAXFAKT LT LS_VOLEH_MIN-MAXFAKT.
        LS_VOLEH_REFERENCE = LT_VOLEH.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        LS_VOLEH_REFERENCE = LS_VOLEH_MAX.
      ENDIF.
    ENDIF.
  ENDIF.
* Take into account given UoM instead of calculated ones
  IF NOT IF_REF_GEWEI IS INITIAL.
    LS_GEWEI_REFERENCE-MSEHI = IF_REF_GEWEI.
  ENDIF.
  IF NOT IF_REF_VOLEH IS INITIAL.
    LS_VOLEH_REFERENCE-MSEHI = IF_REF_VOLEH.
  ENDIF.

* 6. Convert all weights and volumes to reference units
  LOOP AT CT_POSTAB ASSIGNING <LS_POSTAB>
                    WHERE NOT GEWEI IS INITIAL
                    OR    NOT VOLEH IS INITIAL.

    IF <LS_POSTAB>-GEWEI NE LS_GEWEI_REFERENCE-MSEHI.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
           EXPORTING
                INPUT                = <LS_POSTAB>-BRGEW
                NO_TYPE_CHECK        = 'X'
                ROUND_SIGN           = 'X'
                UNIT_IN              = <LS_POSTAB>-GEWEI
                UNIT_OUT             = LS_GEWEI_REFERENCE-MSEHI
           IMPORTING
                OUTPUT               = <LS_POSTAB>-BRGEW
           EXCEPTIONS
                CONVERSION_NOT_FOUND = 1
                DIVISION_BY_ZERO     = 2
                INPUT_INVALID        = 3
                OUTPUT_INVALID       = 4
                OVERFLOW             = 5
                TYPE_INVALID         = 6
                UNITS_MISSING        = 7
                UNIT_IN_NOT_FOUND    = 8
                UNIT_OUT_NOT_FOUND   = 9
                OTHERS               = 10.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
           EXPORTING
                INPUT                = <LS_POSTAB>-NTGEW
                NO_TYPE_CHECK        = 'X'
                ROUND_SIGN           = 'X'
                UNIT_IN              = <LS_POSTAB>-GEWEI
                UNIT_OUT             = LS_GEWEI_REFERENCE-MSEHI
           IMPORTING
                OUTPUT               = <LS_POSTAB>-NTGEW
           EXCEPTIONS
                CONVERSION_NOT_FOUND = 1
                DIVISION_BY_ZERO     = 2
                INPUT_INVALID        = 3
                OUTPUT_INVALID       = 4
                OVERFLOW             = 5
                TYPE_INVALID         = 6
                UNITS_MISSING        = 7
                UNIT_IN_NOT_FOUND    = 8
                UNIT_OUT_NOT_FOUND   = 9
                OTHERS               = 10.
      IF SY-SUBRC EQ 0.
        <LS_POSTAB>-GEWEI = LS_GEWEI_REFERENCE-MSEHI.
      ENDIF.
    ENDIF.
    IF <LS_POSTAB>-VOLEH NE LS_VOLEH_REFERENCE-MSEHI.
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
           EXPORTING
                INPUT                = <LS_POSTAB>-VOLUM
                NO_TYPE_CHECK        = 'X'
                ROUND_SIGN           = 'X'
                UNIT_IN              = <LS_POSTAB>-VOLEH
                UNIT_OUT             = LS_VOLEH_REFERENCE-MSEHI
           IMPORTING
                OUTPUT               = <LS_POSTAB>-VOLUM
           EXCEPTIONS
                CONVERSION_NOT_FOUND = 1
                DIVISION_BY_ZERO     = 2
                INPUT_INVALID        = 3
                OUTPUT_INVALID       = 4
                OVERFLOW             = 5
                TYPE_INVALID         = 6
                UNITS_MISSING        = 7
                UNIT_IN_NOT_FOUND    = 8
                UNIT_OUT_NOT_FOUND   = 9
                OTHERS               = 10.
      IF SY-SUBRC EQ 0.
        <LS_POSTAB>-VOLEH = LS_VOLEH_REFERENCE-MSEHI.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
