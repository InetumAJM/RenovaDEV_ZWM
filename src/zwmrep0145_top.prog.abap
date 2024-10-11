*&---------------------------------------------------------------------*
*&  Include           ZWMREP0145_TOP
*&---------------------------------------------------------------------*
INCLUDE <icon>.

CLASS cl_gui_cfw         DEFINITION LOAD.

TYPES: BEGIN OF st_log,
         ordem TYPE char10,
         matnr TYPE matnr,
         msg   TYPE char255,
       END OF st_log.

DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      tree               TYPE REF TO cl_gui_alv_tree,
      g_toolbar          TYPE REF TO cl_gui_toolbar.

*DATA: gv_tree_evt_rec    TYPE REF TO lcl_tree_event_receiver,
*      gv_toolbar_evt_rec TYPE REF TO lcl_toolbar_event_receiver.

DATA: gt_fieldcatalog TYPE lvc_t_fcat,
      gt_sort         TYPE lvc_t_sort.

DATA: flag_tree,
      flag_alv_grid,
      not_first_time.

DATA: g_top_node     TYPE lvc_nkey,
      g_top_node_key TYPE lvc_nkey.

DATA: BEGIN OF gt_expand_nodes OCCURS 0,
        node TYPE lvc_nkey,
      END OF gt_expand_nodes.

DATA: gt_log TYPE st_log OCCURS 0 WITH HEADER LINE.

** Variáveis Para ALV em Grid
**********************************************************************
DATA: gc_alv_container TYPE REF TO cl_gui_custom_container.
DATA: gcl_alv_grid     TYPE REF TO cl_gui_alv_grid.
DATA: gt_fcat          TYPE lvc_t_fcat.
DATA: gs_fcat          TYPE lvc_s_fcat.

DATA: gcl_splitter   TYPE REF TO cl_gui_splitter_container.
*DATA: gcl_splitter2  TYPE REF TO cl_gui_splitter_container.
DATA: gcl_container  TYPE REF TO cl_gui_custom_container.
DATA: gcl_container1 TYPE REF TO cl_gui_container.
DATA: gcl_container2 TYPE REF TO cl_gui_container.

** Tabelas DD
************************************************************************
TABLES: t311, afko.

** Constantes
************************************************************************
CONSTANTS: gc_icon_batch    TYPE char4 VALUE '@EJ@'.
CONSTANTS: gc_icon_matnr    TYPE char4 VALUE '@A6@'.
CONSTANTS: gc_icon_ordem    TYPE char4 VALUE '@9Z@'.
CONSTANTS: gc_icon_lgnum    TYPE char4 VALUE '@WV@'.
CONSTANTS: gc_icon_locked   TYPE char4 VALUE '@06@'.
CONSTANTS: gc_icon_unlocked TYPE char4 VALUE '@07@'.
*CONSTANTS: gc_icon_ok       TYPE char4 VALUE '@5B@'.

CONSTANTS: c_icon_ok(4) TYPE c VALUE '@01@',
           c_icon_no(4) TYPE c VALUE '@02@'.

** Variáveis
************************************************************************
DATA: gv_werks      TYPE werks_d.
DATA: gv_lgort      TYPE lgort_d.
DATA: gv_queue      TYPE ltak-queue.
DATA: gv_bwlvs      TYPE bwlvs.
DATA: gv_max_cap    TYPE i.
DATA: gv_lgtyp_desc TYPE lgtyp.
DATA: gv_lgtyp_prd  TYPE lgtyp.
DATA: gr_lgtyp_frio TYPE RANGE OF lgtyp WITH HEADER LINE.

DATA: gt_zwm001 LIKE zwm001 OCCURS 0 WITH HEADER LINE.

DATA: text1 LIKE bdcmsgcoll-msgv1,
      text2 LIKE bdcmsgcoll-msgv2,
      text3 LIKE bdcmsgcoll-msgv3,
      text4 LIKE bdcmsgcoll-msgv4.

** Tabelas Internas
************************************************************************
TYPES: BEGIN OF st_alv_table.
         INCLUDE STRUCTURE zwm_011.
       TYPES: END OF st_alv_table.

TYPES: BEGIN OF st_alv.
         INCLUDE TYPE zwm_010.
       TYPES: END OF st_alv.

** Tree
DATA: gt_alv TYPE STANDARD TABLE OF st_alv WITH HEADER LINE.
DATA: gt_alv1 LIKE LINE OF gt_alv OCCURS 0.

** Grid
DATA: gt_alv_table   TYPE TABLE OF st_alv_table.

** Campos da tela 0101
DATA: BEGIN OF scr1,
        npal  TYPE i,
        tpal  TYPE i,
        maktx TYPE maktx,
      END OF scr1.

** Opções de Selecção
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_lgnum LIKE lqua-lgnum DEFAULT '100' OBLIGATORY MEMORY ID lgn.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
SELECT-OPTIONS: s_refnr FOR t311-refnr, "OBLIGATORY,
                s_refnt FOR t311-refnt,
                s_datum FOR t311-datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.
