*&---------------------------------------------------------------------*
*&  Include           ZWMREP0071_PBO                                   *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  IF posicao IS INITIAL.
    cursorfield = 'POSICAO'.
  ELSE.
    cursorfield = 'QTD'.
  ENDIF.

  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

** dados para o user
  PERFORM find_whs.

** dados globais de parametrização
  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs       = whs
    TABLES
      ti_zwm001 = itab_zwm001.

ENDMODULE.                 " STATUS_0001  OUTPUT
