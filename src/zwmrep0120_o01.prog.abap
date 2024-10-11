*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0120_O01                                             *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  status_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'STATUS_GUI'.
*  SET CURSOR FIELD 'PDT_ORIGEM'.
  if MATNR_OUT is not initial.
    set cursor field 'SCR_DATE'.
  endif.
  if scr_date is initial.
    move sy-datum to scr_date.
  endif.

ENDMODULE.                 " status_0001  OUTPUT
