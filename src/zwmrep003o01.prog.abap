*&---------------------------------------------------------------------*
*&      Module  STATUS_0007  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0007 OUTPUT.
  SET PF-STATUS 'ZWMDESCARGA1'.
  SET TITLEBAR 'TRANSPORTE'.

ENDMODULE.                 " STATUS_0007  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0008  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0008 OUTPUT.
  SET PF-STATUS 'ZWMENTRADA'.
  SET TITLEBAR 'TERCEIROS'.

ENDMODULE.                 " STATUS_0008  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0009  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0009 OUTPUT.
  SET PF-STATUS 'ZWMDESCARGA1'.
  SET TITLEBAR 'Inserção de Ordens de Compra'.

  SET CURSOR FIELD cursorfield.

ENDMODULE.                 " STATUS_0009  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0011  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0011 OUTPUT.

  SET PF-STATUS 'ZWMENTRADA'.
  SET TITLEBAR 'DEVOLUCAO'.

  SET CURSOR FIELD cursorfield.

  IF cliente IS INITIAL.
    LOOP AT SCREEN.
      CHECK screen-group1 EQ 'EXI'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDLOOP.

  ELSEIF doc_renova IS INITIAL.
    LOOP AT SCREEN.
      CHECK screen-group2 EQ 'EXI'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDLOOP.

  ELSEIF material IS INITIAL.
    LOOP AT SCREEN.
      CHECK screen-group3 EQ 'EXI'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDLOOP.

  ELSEIF lote IS INITIAL.
    LOOP AT SCREEN.
      CHECK screen-group4 EQ 'EXI'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  CHECK cod_dev IS INITIAL.
  LOOP AT SCREEN.
    CHECK screen-name EQ 'COD_MOT'.
    screen-input = '0'.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " STATUS_0011  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0012  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0012 OUTPUT.

  SET PF-STATUS 'ZWMENTRADA'.
  SET TITLEBAR 'TERCEIROS'.

ENDMODULE.                 " STATUS_0012  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0013  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0013 OUTPUT.
  SET PF-STATUS 'ZWMENTRADA'.
  SET TITLEBAR 'FABRICA_1'.

  PERFORM user_own_data.
ENDMODULE.                 " STATUS_0013  OUTPUT
