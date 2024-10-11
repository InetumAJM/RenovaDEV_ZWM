*&---------------------------------------------------------------------*
*&  Include           ZWMREP0083TOP
*&---------------------------------------------------------------------*

TABLES: t300, t311a, t001w, vbrk, mara, vttk, likp, adrc.

DATA: gt_dados           LIKE zwm_001 OCCURS 0 WITH HEADER LINE,
      gt_dados_genericos LIKE zwm_002 OCCURS 0 WITH HEADER LINE.

** Variáveis Globais
************************************************************************
DATA:
      gv_error(1) TYPE c.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_lgnum  LIKE t311a-lgnum OBLIGATORY MEMORY ID lgn.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
SELECT-OPTIONS: s_refnr FOR t311a-refnr.
SELECT-OPTIONS: s_vbeln FOR likp-vbeln.

SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS: s_vfeln FOR vbrk-vbeln.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-b03.
PARAMETERS: p_imp TYPE rspolname MATCHCODE OBJECT prin.
SELECTION-SCREEN END OF BLOCK b3.
