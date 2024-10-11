*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1O11 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  status_0029  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0029 OUTPUT.
  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

* Quando um operario quer sair da pistola
  IF NOT f3_activo IS INITIAL.
    SET SCREEN '0000'.
    LEAVE SCREEN.
  ENDIF.
ENDMODULE.                 " status_0029  OUTPUT
