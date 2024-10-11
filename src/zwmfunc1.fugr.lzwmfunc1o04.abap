*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1O04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0013  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0013 OUTPUT.
  SET PF-STATUS 'ZRF'.

*  MOVE 'SU' TO CURSORFIELD.
  SET CURSOR FIELD CURSORFIELD.

** Quando um operario quer sair da pistola
  IF NOT F3_ACTIVO IS INITIAL.
    SET SCREEN '0000'.
*    CLEAR F3_ACTIVO.
    LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " STATUS_0013  OUTPUT
