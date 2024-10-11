*&---------------------------------------------------------------------*
*&  Include           ZWMREP0129_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'GUI_0001'.
  SET TITLEBAR 'TIT_0001'.

  IF tree IS INITIAL.
    PERFORM init_tree.
  ENDIF.

  IF gc_alv_grid IS INITIAL.
    PERFORM init_alv.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.

  LOOP AT SCREEN.
    IF scr1-refnr IS INITIAL.
      IF screen-name = 'SCR1-VBELN'.
        screen-input = 0.
      ENDIF.
    ELSE.
      IF screen-name = 'SCR1-REFNR'.
        screen-input = 0.
      ENDIF.
    ENDIF.

    IF scr1-vbeln IS NOT INITIAL OR gv_2step = 'X'.
      IF screen-name = 'SCR1-VBELN'.
        screen-input = 0.
      ENDIF.
    ENDIF.

    IF scr1-emb IS INITIAL.
      IF screen-name = 'SCR1-EMB'.
        screen-invisible = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

  IF scr1-refnr IS INITIAL.
    SET CURSOR FIELD 'SCR1-REFNR'.
  ELSEIF gv_2step IS INITIAL.
    SET CURSOR FIELD 'SCR1-VBELN'.
  ENDIF.

ENDMODULE.                 " STATUS_0001  OUTPUT
