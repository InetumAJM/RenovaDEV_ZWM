*----------------------------------------------------------------------*
***INCLUDE LZWM082O01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  GET_WEEKDAY_NAME  OUTPUT
*&---------------------------------------------------------------------*
MODULE get_weekday_name OUTPUT.

  IF zwm082-weekday IS NOT INITIAL.
    SELECT SINGLE kurzt
      FROM t246
      INTO gs_9500-weekday_name
      WHERE sprsl EQ sy-langu
        AND wotnr EQ zwm082-weekday.
  ENDIF.

ENDMODULE.
