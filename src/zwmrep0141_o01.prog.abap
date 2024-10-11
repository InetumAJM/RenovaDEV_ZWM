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

    IF scr0001-pos IS INITIAL.
      IF screen-name = 'SCR0001-POS'.
        screen-input = 1.
      ENDIF.

    ELSEIF scr0001-lenum IS INITIAL AND scr0001-lenvw = 'X' AND scr0001-npos > 1.
      IF screen-name = 'SCR0001-LENUM'.
        screen-input = 1.
      ENDIF.

    ELSEIF scr0001-dest IS INITIAL AND scr0001-npos > 1.
      IF screen-name = 'SCR0001-DEST'.
        screen-input = 1.
      ENDIF.
    ENDIF.

    IF scr0001-lenvw IS INITIAL.
      IF screen-group1 = 'G1'.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
    ENDIF.

    IF scr0001-npos <= 1.
      IF screen-group1 = 'G1'.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.

      IF screen-group1 = 'G2'.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

  IF scr0001-lgtyp IS INITIAL.
    SET CURSOR FIELD 'SCR0001-POS'.
  ELSEIF scr0001-lenum IS INITIAL AND scr0001-lenvw = 'X' AND scr0001-npos > 1.
    SET CURSOR FIELD 'SCR0001-LENUM'.
  ELSEIF scr0001-dest IS INITIAL AND scr0001-npos > 1.
    SET CURSOR FIELD 'SCR0001-DEST'.
  ENDIF.

ENDMODULE.
