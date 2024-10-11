*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1O07 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0019  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0019 output.
 SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

  IF xuser-lgnum IS INITIAL.
    PERFORM user_own_data.
  ENDIF.

* Quando um operario quer sair da pistola
  IF NOT f3_activo IS INITIAL.
    SET SCREEN '0000'.
    LEAVE SCREEN.
  ENDIF.


** Depois de inserido o pulmao fecha-se o campo da confirmação do pulmão
  IF NOT pulmao2 IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'PULMAO2'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.


endmodule.                 " STATUS_0019  OUTPUT
