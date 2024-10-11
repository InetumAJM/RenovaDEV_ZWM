*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1O02 .
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
*  MODULE STATUS_0005 OUTPUT
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
MODULE STATUS_0005 OUTPUT.

  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD CURSORFIELD.
*  CLEAR: PORTA2, PULMAO2, NUM_PALETE.

* Quando um operario quer sair da pistola
  IF NOT F3_ACTIVO IS INITIAL.
    SET SCREEN '0000'.
*    CLEAR F3_ACTIVO.
    LEAVE SCREEN.
  ENDIF.


ENDMODULE.                 " STATUS_0005  OUTPUT
