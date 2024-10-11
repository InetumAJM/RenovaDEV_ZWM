*&---------------------------------------------------------------------*
*&  Include           ZWMREP0072_PBO                                   *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  CLEAR : posicao, cursorfield, ok_code_0001.
  cursorfield = 'POSICAO'.

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
