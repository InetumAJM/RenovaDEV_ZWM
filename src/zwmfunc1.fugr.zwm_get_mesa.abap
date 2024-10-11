FUNCTION zwm_get_mesa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MATNR) TYPE  MATNR
*"     REFERENCE(QUANTIDADE) TYPE  VEMNG
*"  EXPORTING
*"     REFERENCE(MESA) TYPE  CHAR14
*"----------------------------------------------------------------------

  CLEAR: marc, marm, mesa.

  SELECT SINGLE maabc FROM marc INTO marc-maabc
          WHERE matnr = matnr
            AND werks = 'RENV'.

  CHECK sy-subrc EQ 0.

  IF marc-maabc IS INITIAL.
    SELECT SINGLE mesa INTO mesa FROM zwm009 WHERE abc = 'X'.

  ELSE.
    SELECT SINGLE umrez FROM marm INTO marm-umrez
            WHERE matnr EQ matnr
              AND meinh EQ 'PAL'.

    IF quantidade < marm-umrez.
      SELECT SINGLE mesa INTO mesa FROM zwm009 WHERE abc = 'Z'.
    ELSE.
      SELECT SINGLE mesa INTO mesa FROM zwm009 WHERE abc = marc-maabc.
    ENDIF.

  ENDIF.

ENDFUNCTION.
