*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1O06 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0017  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0017 OUTPUT.

  SET PF-STATUS 'ZRF'.

  SET CURSOR FIELD cursorfield.

** Quando um operario quer sair da pistola
  IF NOT f3_activo IS INITIAL.
    SET SCREEN '0000'.
    LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " STATUS_0017  OUTPUT
