*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1O05 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0015  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0015 OUTPUT.

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

** Quando o campo lote está preenchido é porque este
** ecrã é chamado por uma entrada de terceiros. Caso
** contrário é uma entrada de transferência e o campo
** tem de ficar em aberto para colocar o lote
  IF NOT lote IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'LOTE'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF lote IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'LOTE'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF NOT material IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'MATERIAL'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF material IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'MATERIAL'.
        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_0015  OUTPUT
