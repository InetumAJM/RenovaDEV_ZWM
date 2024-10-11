*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1O03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0009  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0009 OUTPUT.

  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

  IF xuser-lgnum IS INITIAL.
    PERFORM user_own_data.
  ENDIF.


** Quando um operario quer sair da pistola
  IF NOT f3_activo IS INITIAL.
    SET SCREEN '0000'.
*    CLEAR F3_ACTIVO.
    LEAVE SCREEN.
  ENDIF.

** Limpar o indicador das incidencias da entrada de
** terceiros - para poder colocar várias dentro da
** conferência
  CLEAR : incidencia_terceiros.

ENDMODULE.                 " STATUS_0009  OUTPUT
