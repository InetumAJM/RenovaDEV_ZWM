*&---------------------------------------------------------------------*
*&  Include           ZWMREP0078O01                                    *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  status_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  IF scr_printer IS INITIAL.
    MOVE 'SCR_PRINTER' TO cursorfield.
  ELSE.
    MOVE 'SCR_SSCC' TO cursorfield.
  ENDIF.

  SET PF-STATUS 'STATUS_GUI'.
  SET CURSOR FIELD cursorfield.

ENDMODULE.                 " status_0001  OUTPUT
