*&---------------------------------------------------------------------*
*&  Include           ZWMREP0125_D02
*&---------------------------------------------------------------------*

DATA: BEGIN OF gs_select_screen_help,
        lgnum TYPE lgnum,
        lgtyp TYPE lqua-lgtyp,
        lgpla TYPE lqua-lgpla,
        lenum TYPE lqua-lenum,
        vhilm TYPE vekp-vhilm,
        matnr TYPE vepo-matnr,
      END OF gs_select_screen_help.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_lgnum TYPE ltak-lgnum OBLIGATORY MEMORY ID lgn.
SELECT-OPTIONS: s_lgtyp FOR gs_select_screen_help-lgtyp.
SELECT-OPTIONS: s_lgpla FOR gs_select_screen_help-lgpla.
SELECT-OPTIONS: s_lenum FOR gs_select_screen_help-lenum.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
SELECT-OPTIONS: s_vhilm FOR gs_select_screen_help-vhilm.
SELECT-OPTIONS: s_matnr FOR gs_select_screen_help-matnr.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-b03.
PARAMETERS: p_layout TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b3.
