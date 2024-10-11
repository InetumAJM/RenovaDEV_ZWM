FUNCTION ZWM_GET_MATER_BY_EAN.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WAREHOUSE) TYPE  LGNUM
*"     REFERENCE(EAN) TYPE  EAN11
*"     REFERENCE(UNIT) TYPE  MEINS
*"  EXPORTING
*"     REFERENCE(MATNR) TYPE  MATNR
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      NOT_FOUND
*"      MORE_THAN_ONE
*"----------------------------------------------------------------------

  DATA: LIN  TYPE I,
        MAT1 TYPE MARA-MATNR,
        MAT2 TYPE MARA-MATNR,
        UN LIKE MEAN-MEINH.

  PERFORM USER_OWN_DATA.

  UN = UNIT.
  CLEAR: MATNR, MATERIAL_OUTPUT.
  CLEAR TAB.
  REFRESH TAB.

  SELECT M~EAN11 M~MATNR
         INTO TABLE TAB
         FROM MEAN AS M INNER JOIN MLGN AS G
         ON M~MATNR = G~MATNR
         WHERE M~EAN11 = EAN AND
               G~LGNUM = WAREHOUSE AND
               M~MEINH = UN.

  DESCRIBE TABLE TAB LINES LIN.
  IF LIN = 1. " OK existe 1 material para ean
    READ TABLE TAB INDEX LIN.
    MOVE TAB-MATNR TO MATNR.
  ELSEIF LIN = 0.
    CLEAR RETURN_MSG.
    MOVE 'E' TO RETURN_MSG-MSGTYP.
    MOVE 'ZWMMSG001' TO RETURN_MSG-MSGID.
    MOVE '069' TO RETURN_MSG-MSGNR.
    WRITE EAN TO RETURN_MSG-MSGV1 LEFT-JUSTIFIED.
    APPEND RETURN_MSG.
    RAISE NOT_FOUND.
  ELSEIF LIN > 1.

    IF LRF_WKQU-DEVTY(5) = '16X20'.
      CALL SCREEN '0011'.
    ELSE.
      CALL SCREEN '0012'.
    ENDIF.

    MATNR = MATERIAL_OUTPUT.

  ENDIF.


ENDFUNCTION.
