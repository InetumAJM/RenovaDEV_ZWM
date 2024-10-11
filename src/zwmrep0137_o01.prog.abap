*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0137_O01                                            *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  status_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'GUI_0001'.

  PERFORM get_data_screen_0001.

** Screen
**********************************************************************
  LOOP AT SCREEN.

    " Grupo
    IF scr1-refnr IS INITIAL.
      IF screen-name = 'SCR1-REFNR'.
        screen-input = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

** Cursor Field
**********************************************************************
  IF scr1-refnr IS INITIAL.
    SET CURSOR FIELD 'SCR1-REFNR'.
  ENDIF.

ENDMODULE.                 " status_0001  OUTPUT
