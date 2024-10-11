*&---------------------------------------------------------------------*
*&  Include           ZWMREP0148_TOP
*&---------------------------------------------------------------------*
INCLUDE <icon>.

CONSTANTS gc_dliname TYPE so_obj_nam VALUE 'ZWCS_SU_BLOQ'.

TABLES: zwm080, sscrfields.

TYPES: BEGIN OF st_alv_table.
         INCLUDE STRUCTURE zwm_014.
       TYPES: END OF st_alv_table.

** Variaveis ALV
**********************************************************************
DATA: gcl_container TYPE REF TO cl_gui_custom_container,
      gcl_alv_grid  TYPE REF TO cl_gui_alv_grid.

DATA: gs_alv_table TYPE st_alv_table.
DATA: gt_alv_table TYPE TABLE OF st_alv_table.

** Variáveis Globais
**********************************************************************
DATA: gv_mod  TYPE c.

** Selection Screen
**********************************************************************
PARAMETERS p_lgnum TYPE lgnum OBLIGATORY.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS: s_datum FOR zwm080-datum.
SELECT-OPTIONS: s_exidv FOR zwm080-exidv.

SELECTION-SCREEN SKIP 1.

PARAMETERS: p_job AS CHECKBOX.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_all   RADIOBUTTON  GROUP g2 DEFAULT 'X',
            p_proc  RADIOBUTTON  GROUP g2,
            p_error RADIOBUTTON  GROUP g2.
SELECTION-SCREEN END OF BLOCK b1.
