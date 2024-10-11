*&---------------------------------------------------------------------*
*&  Include           ZWMREP0149_TOP
*&---------------------------------------------------------------------*
TABLES: zwm_015.

TYPES: BEGIN OF st_alv_table.
         INCLUDE STRUCTURE zwm_016.
       TYPES: END OF st_alv_table.

** Variaveis ALV
**********************************************************************
DATA: gcl_container TYPE REF TO cl_gui_custom_container,
      gcl_alv_grid  TYPE REF TO cl_gui_alv_grid.

DATA: gs_alv TYPE st_alv_table.
DATA: gt_alv TYPE TABLE OF st_alv_table.

** Opções de Selecção
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_lgnum LIKE lqua-lgnum OBLIGATORY MEMORY ID lgn.

SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: s_matnr  FOR zwm_015-matnr.
SELECT-OPTIONS: s_charg  FOR zwm_015-charg.
SELECT-OPTIONS: s_bestq  FOR zwm_015-bestq.
SELECT-OPTIONS: s_vfdat  FOR zwm_015-vfdat.
SELECT-OPTIONS: s_lenum  FOR zwm_015-lenum.
SELECT-OPTIONS: s_refnr  FOR zwm_015-refnr.
SELECT-OPTIONS: s_nltyp  FOR zwm_015-nltyp.
SELECT-OPTIONS: s_nlpla  FOR zwm_015-nlpla.
SELECTION-SCREEN SKIP 1.

PARAMETERS: p_all RADIOBUTTON GROUP g1 DEFAULT 'X',
            p_pck RADIOBUTTON GROUP g1,
            p_ppe RADIOBUTTON GROUP g1.

*SELECTION-SCREEN SKIP 1.
*PARAMETERS: p_wcs RADIOBUTTON  GROUP g2 DEFAULT 'X',
*            p_sap RADIOBUTTON  GROUP g2.

SELECTION-SCREEN END OF BLOCK b1.
