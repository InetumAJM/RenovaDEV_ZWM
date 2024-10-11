*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0121_O01                                              *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  status_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'STATUS_GUI'.


  LOOP AT SCREEN.
    IF screen-name = 'B_RESTOS'.
      IF aufnr_in IS INITIAL.
        screen-invisible = '1'.
        screen-active = '0'.
      ELSE.
        screen-invisible = '0'.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


  SET CURSOR FIELD cursorfield.

ENDMODULE.                 " status_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0002 OUTPUT.

  SET PF-STATUS 'STATUS_GUI2'.

  LOOP AT SCREEN.
    IF screen-name = 'MATNR2_IN'.
      IF flg_matnr_input = 'X'.
        screen-input = '1'.
      ELSE.
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF screen-name = 'RLMOB-PPGDN'.
      IF current_item IS INITIAL OR current_item = 1.
        screen-invisible = '1'.
        screen-active = '0'.
      ELSE.
        screen-invisible = '0'.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF screen-name = 'RLMOB-PPGUP'.
      IF current_item = total_items.
        screen-invisible = '1'.
        screen-active = '0'.
      ELSE.
        screen-invisible = '0'.
        screen-active = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  perform carrega_item.

  SET CURSOR FIELD cursorfield2.

ENDMODULE.                 " STATUS_0002  OUTPUT
