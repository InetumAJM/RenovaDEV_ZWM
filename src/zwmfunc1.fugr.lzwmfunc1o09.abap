*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1O09 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0023  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0023 OUTPUT.
  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

* Quando um operario quer sair da pistola
  IF NOT f3_activo IS INITIAL.
    SET SCREEN '0000'.
*    CLEAR F3_ACTIVO.
    LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " STATUS_0023  OUTPUT
