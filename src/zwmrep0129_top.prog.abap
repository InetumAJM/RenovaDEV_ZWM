*&---------------------------------------------------------------------*
*&  Include           ZWMREP0129_TOP
*&---------------------------------------------------------------------*
TABLES: mlgn.

** TYPES
**********************************************************************
TYPES: BEGIN OF st_alv_tree,
         lgnum   TYPE lgnum,
         lnumt   TYPE lvs_lnumt,
         exidv   TYPE exidv,
         upd_del TYPE char1,
       END OF st_alv_tree.

TYPES: BEGIN OF st_alv_grid.
        INCLUDE STRUCTURE vepo.
TYPES: END OF st_alv_grid.

** Variáveis ALV Tree
**********************************************************************
DATA: gt_alv_tree  TYPE TABLE OF st_alv_tree WITH HEADER LINE.
DATA: gt_alv_tree1 TYPE TABLE OF st_alv_tree.

DATA: tree            TYPE REF TO cl_gui_alv_tree,
      g_toolbar       TYPE REF TO cl_gui_toolbar,
      g_toolbar_grid  TYPE REF TO cl_gui_toolbar.

DATA: gt_fieldcatalog     TYPE lvc_t_fcat,
      gt_sort             TYPE lvc_t_sort.

DATA: g_top_node_key     TYPE lvc_nkey,
      g_tree_container TYPE REF TO cl_gui_custom_container.

** Variáveis ALV Grid
**********************************************************************
DATA: gt_alv TYPE TABLE OF st_alv_grid.
DATA: gs_alv TYPE st_alv_grid.

DATA: gc_alv_container TYPE REF TO cl_gui_custom_container.
DATA: gc_alv_grid      TYPE REF TO cl_gui_alv_grid.
DATA: gt_fcat          TYPE lvc_t_fcat.
DATA: gs_fcat          TYPE lvc_s_fcat.

** Variáveis globais
**********************************************************************
DATA: gv_2step TYPE flag.
DATA: gr_lgtyp TYPE RANGE OF lgtyp WITH HEADER LINE.
DATA: flag_alv_tree.
DATA: flag_alv_grid.

** Variáveis Tela 0001
**********************************************************************
DATA: BEGIN OF scr1,
        lgnum        LIKE mlgn-lgnum,
        lnumt        TYPE lvs_lnumt,
        refnr        TYPE lvs_refnr,
        vbeln        TYPE vbeln,
        exidv        TYPE exidv,
        emb          TYPE xfeld,
*        maktx1       TYPE maktx,
*        maktx2       TYPE maktx,
*        lety1        LIKE mlgn-lety1,
*        lety2        LIKE mlgn-lety2,
      END OF scr1.
