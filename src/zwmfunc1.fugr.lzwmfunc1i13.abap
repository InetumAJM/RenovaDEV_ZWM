*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I13 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0019  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0019 INPUT.
  CASE ok_code_0019.

    WHEN 'NEXT'.

      IF NOT pulmao2 IS INITIAL.

        PERFORM confirma_to.

        CLEAR : cursorfield.
** Ecrã para conferência das entradas da fabrica 1
        IF lrf_wkqu-devty(5) = '16X20'.
          CALL SCREEN '0017'.
        ELSE.
          CALL SCREEN '0018'.
        ENDIF.
      ENDIF.

    WHEN OTHERS.

  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0019  INPUT
