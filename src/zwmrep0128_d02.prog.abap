*&---------------------------------------------------------------------*
*&  Include           ZWMREP0128_D02
*&---------------------------------------------------------------------*


**********************************************************************
** ESTRUTURAS GLOBAIS
**********************************************************************
DATA: BEGIN OF gs_serch_help_struct,
        vbeln LIKE likp-vbeln,
      END OF   gs_serch_help_struct.


SELECTION-SCREEN BEGIN OF BLOCK fselect1 WITH FRAME TITLE text-b01.
PARAMETERS: p_doc_o TYPE tknum OBLIGATORY.
PARAMETERS: p_doc_d TYPE tknum OBLIGATORY.
SELECT-OPTIONS : s_vbeln FOR  gs_serch_help_struct-vbeln OBLIGATORY.
SELECTION-SCREEN END OF BLOCK fselect1.
