*&---------------------------------------------------------------------*
*&  Include           ZWMREP0143_D02
*&---------------------------------------------------------------------*

DATA: BEGIN OF gs_search_help,
         refnr TYPE t311a-refnr,
         vbeln TYPE likp-vbeln,
      END OF gs_search_help.

PARAMETERS: p_lgnum TYPE lgnum.
SELECT-OPTIONS: s_refnr FOR gs_search_help-refnr.
SELECT-OPTIONS: s_vbeln FOR gs_search_help-vbeln.
