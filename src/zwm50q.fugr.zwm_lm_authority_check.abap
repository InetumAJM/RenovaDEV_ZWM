FUNCTION ZWM_LM_AUTHORITY_CHECK.
*"--------------------------------------------------------------------
*"*"Interface local:
*"       TABLES
*"              IT_VSTEL STRUCTURE  RANGE_C4
*"              ET_TVST STRUCTURE  TVST OPTIONAL
*"       EXCEPTIONS
*"              NO_PERMISSION
*"--------------------------------------------------------------------

  DATA: BEGIN OF LT_VSTEL OCCURS 10,
          VSTEL LIKE TVST-VSTEL,
        END OF LT_VSTEL,
        LF_TVST_LINES TYPE I.

  REFRESH ET_TVST.
  CLEAR ET_TVST.

  SELECT VSTEL FROM TVST
               APPENDING CORRESPONDING FIELDS OF TABLE LT_VSTEL
               WHERE VSTEL IN IT_VSTEL.

  LOOP AT LT_VSTEL.
    AUTHORITY-CHECK OBJECT 'V_LIKP_VST'
             ID 'VSTEL' FIELD LT_VSTEL-VSTEL
             ID 'ACTVT' FIELD '03'.
    IF SY-SUBRC NE 0.
      ET_TVST-VSTEL = LT_VSTEL-VSTEL.
      APPEND ET_TVST.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE ET_TVST LINES LF_TVST_LINES.
  IF LF_TVST_LINES NE 0.
    RAISE NO_PERMISSION.
  ENDIF.

ENDFUNCTION.
