*&---------------------------------------------------------------------*
*&  Include           ZWMREP0047O01                                    *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  PERFORM initialization.


ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0002 OUTPUT.

  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

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

  IF gv_get_batch <> 'X'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'LT'.
        screen-input = 0.
        screen-invisible = 1.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " status_0002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0003 OUTPUT.

  SET PF-STATUS 'ZRF1'.
  SET CURSOR FIELD cursorfield.

ENDMODULE.                 " status_0003  OUTPUT

*{   INSERT         DEVK939042                                        1
*&---------------------------------------------------------------------*
*&      Module  STATUS_0004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0004 OUTPUT.
  SET PF-STATUS 'ZRF1'.
  PERFORM status_0004.
  PERFORM initialization.
ENDMODULE.                 " STATUS_0004  OUTPUT

*}   INSERT
