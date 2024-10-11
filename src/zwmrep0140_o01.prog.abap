*&---------------------------------------------------------------------*
*&  Include           ZWMREP0140_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'GUI_0001'.
*  SET TITLEBAR 'xxx'.

  LOOP AT SCREEN.

    IF scr0001-lgtyp IS INITIAL.
      IF screen-name = 'SCR0001-LGTYP'.
        screen-input = 1.
      ENDIF.

    ELSEIF scr0001-matnr IS INITIAL.
      IF screen-name = 'SCR0001-EAN11'.
        screen-input = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

  IF scr0001-lgtyp IS INITIAL.
    SET CURSOR FIELD 'SCR0001-LGTYP'.
  ELSEIF scr0001-matnr IS INITIAL.
    SET CURSOR FIELD 'SCR0001-EAN11'.
  ENDIF.

ENDMODULE.
