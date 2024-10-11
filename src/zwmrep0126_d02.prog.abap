*&---------------------------------------------------------------------*
*&  Include           ZWMREP0126_D02
*&---------------------------------------------------------------------*

DATA: BEGIN OF gs_select_screen_help,
        refnr TYPE lvs_refnr,
        datum TYPE datum,
      END OF gs_select_screen_help.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_lgnum TYPE lgnum OBLIGATORY MEMORY ID lgn.
SELECT-OPTIONS: s_refnr FOR gs_select_screen_help-refnr.
SELECT-OPTIONS: s_datum FOR gs_select_screen_help-datum.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
PARAMETERS: rb_real  RADIOBUTTON GROUP rad1 DEFAULT 'X'.
PARAMETERS: rb_back RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK b2.
