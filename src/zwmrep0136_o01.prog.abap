*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0136_O01                                            *
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

    " Paginação
    IF scr1-idx < scr1-tot.

      IF screen-name = 'SCR1-REFNR'  OR
         screen-name = 'SCR1-VBELN'  OR
         screen-name = 'SCR1-SSCC_IN'.
        screen-input = 0.
      ENDIF.

      IF screen-name = 'RLMOB-PSAVE'.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.

*      IF screen-name = 'BT_F4'.
*        screen-input     = 1.
*        screen-invisible = 0.
*      ENDIF.

      " Input
    ELSE.

      " Grupo
      IF scr1-refnr IS INITIAL.
        IF screen-name = 'SCR1-REFNR'.
          screen-input = 1.
        ENDIF.

        " Remessa
      ELSEIF scr1-vbeln IS INITIAL.
        IF screen-name = 'SCR1-VBELN'.
          screen-input = 1.
        ENDIF.

        " Palete
      ELSEIF scr1-sscc_in IS INITIAL.
        IF screen-name = 'SCR1-SSCC_IN'.
          screen-input = 1.
        ENDIF.
      ENDIF.
*
*      IF screen-name = 'BT_F4'.
*        screen-input     = 0.
*        screen-invisible = 1.
*      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

** Cursor Field
**********************************************************************
  IF scr1-refnr IS INITIAL.
    SET CURSOR FIELD 'SCR1-REFNR'.
  ELSEIF scr1-vbeln IS INITIAL.
    SET CURSOR FIELD 'SCR1-VBELN'.
  ELSEIF scr1-sscc_in IS INITIAL.
    SET CURSOR FIELD 'SCR1-SSCC_IN'.
  ENDIF.

ENDMODULE.                 " status_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0002 OUTPUT.

  SET PF-STATUS 'GUI_0001'.

** Screen
**********************************************************************
  LOOP AT SCREEN.

    " Palete
    IF scr2-sscc_in IS INITIAL.
      IF screen-name = 'SCR2-SSCC_IN'.
        screen-input = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

** Cursor Field
**********************************************************************
  IF scr2-sscc_in IS INITIAL.
    SET CURSOR FIELD 'SCR2-SSCC_IN'.
  ENDIF.

ENDMODULE.                 " STATUS_0002  OUTPUT
