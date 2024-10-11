FUNCTION ZWM_LM_DESELECT_REFNUM.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_LGNUM STRUCTURE  RANGE_C3
*"      CT_POSTAB STRUCTURE  ZWMOV
*"----------------------------------------------------------------------
  DATA: BEGIN OF LT_SELECT OCCURS 10,
          VBELN LIKE LIPS-VBELN,
        END OF LT_SELECT,
        BEGIN OF LT_LGNUM OCCURS 0,
          LGNUM LIKE T300-LGNUM,
        END OF LT_LGNUM,
        LT_RBNUM LIKE T311A OCCURS 10 WITH HEADER LINE,
        LT_RBNUM_FINAL LIKE T311A OCCURS 10 WITH HEADER LINE.

  FIELD-SYMBOLS: <LS_POSTAB> TYPE ZWMOV.

  REFRESH: LT_RBNUM, LT_RBNUM_FINAL, LT_SELECT.
  CLEAR  : LT_RBNUM, LT_RBNUM_FINAL, LT_SELECT.

* Search for selected warehouse numbers
  SELECT LGNUM FROM T300
               APPENDING CORRESPONDING FIELDS OF TABLE LT_LGNUM
               WHERE LGNUM IN IT_LGNUM.
  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

* Extract document numbers from document table
  LOOP AT CT_POSTAB ASSIGNING <LS_POSTAB>.
    LT_SELECT-VBELN = <LS_POSTAB>-VBELN.
    COLLECT LT_SELECT.
  ENDLOOP.

* Filter documents already assigned to a reference number
  LOOP AT LT_LGNUM.
    LT_RBNUM-LGNUM = LT_LGNUM-LGNUM.
    LOOP AT LT_SELECT.
      LT_RBNUM-RBNUM = LT_SELECT-VBELN.
      APPEND LT_RBNUM.
    ENDLOOP.
    CALL FUNCTION 'L_REF_DESELECT_DOC'
         EXPORTING
              I_LGNUM = LT_LGNUM-LGNUM
              I_RBTYP = 'L'
         TABLES
              T_RBNUM = LT_RBNUM.
*   Transfer document numbers to be deleted to the result table
    APPEND LINES OF LT_RBNUM TO LT_RBNUM_FINAL.
    REFRESH LT_RBNUM.
    CLEAR LT_RBNUM.
  ENDLOOP.

* Delete deselected documents from the document table
  LOOP AT CT_POSTAB WHERE NOT VBELN IS INITIAL.
    READ TABLE LT_RBNUM_FINAL WITH KEY RBNUM = CT_POSTAB-VBELN.
    IF SY-SUBRC EQ 0.
      DELETE CT_POSTAB.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
