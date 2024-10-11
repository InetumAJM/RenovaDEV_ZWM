*&---------------------------------------------------------------------*
*&  Include           ZWMMPRF013_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'GUI_0001'.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'SCR_SU'.
        IF scr_su IS INITIAL.
          SET CURSOR FIELD 'SCR_SU'.
          screen-input = '1'.

        ELSEIF gv_bktxt IS INITIAL.
          SET CURSOR FIELD 'GV_BKTXT'.
          screen-input = '0'.

        ELSEIF gv_lgort_dest IS INITIAL.
          SET CURSOR FIELD 'GV_LGORT_DEST'.
          screen-input = '0'.
        ELSE.
          screen-input = '0'.
        ENDIF.

      WHEN 'GV_BKTXT'.
        IF scr_su IS INITIAL.
          screen-input = '0'.
        ELSE.
          screen-input = '1'.
        ENDIF.

      WHEN 'GV_LGORT_DEST'.
        IF scr_su IS INITIAL.
          screen-input = '0'.
        ELSE.
          screen-input = '1'.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.


ENDMODULE.                 " STATUS_0001  OUTPUT
