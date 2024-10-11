*&---------------------------------------------------------------------*
*&  Include           ZWMREP0139_TOP
*&---------------------------------------------------------------------*

TABLES: sscrfields, zwm013, lagp.

TYPES: BEGIN OF st_alv_table.
        INCLUDE STRUCTURE zwm_008.
TYPES: END OF st_alv_table.

** Variaveis ALV
**********************************************************************
DATA: gcl_container TYPE REF TO cl_gui_custom_container,
      gcl_alv_grid  TYPE REF TO cl_gui_alv_grid.

DATA: gs_alv_table TYPE st_alv_table.
DATA: gt_alv_table TYPE TABLE OF st_alv_table.

** Variaveis globais
**********************************************************************
DATA: gv_resend     TYPE flag.
DATA: gv_flag_error TYPE flag.
DATA: gs_kna1       TYPE kna1.

DATA: gt_aufk TYPE aufk OCCURS 0 WITH HEADER LINE.

** Telas
**********************************************************************
DATA: BEGIN OF scr101,
       kunnr    LIKE kna1-kunnr,
       name     TYPE char70,
       ort01    LIKE kna1-ort01,
       stras    LIKE kna1-stras,
       pstlz    LIKE kna1-pstlz,
*       loja     TYPE char40,
     END OF scr101.

** Selection Screen
**********************************************************************
PARAMETERS: p_lgnum  TYPE lagp-lgnum OBLIGATORY.
PARAMETERS: p_lgtyp  TYPE lagp-lgtyp OBLIGATORY.

SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS: s_lgpla FOR lagp-lgpla.

SELECTION-SCREEN SKIP 1.

*PARAMETERS: p_wm AS CHECKBOX no-display.

SELECTION-SCREEN FUNCTION KEY 1.
