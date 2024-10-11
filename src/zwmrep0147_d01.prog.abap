*&---------------------------------------------------------------------*
*&  Include           ZWMREP0147_D01
*&---------------------------------------------------------------------*

************************************************************************
** TYPE POOLS
************************************************************************
TYPE-POOLS: zrf01, abap.

************************************************************************
** TIPOS GLOBAIS
************************************************************************
TYPES: BEGIN OF gty_scr0001,
         okcode TYPE syucomm,
       END OF gty_scr0001.

TYPES: BEGIN OF gty_alv_display_0001,
         matnr TYPE matnr,
         tbnum TYPE tbnum,
         maktx TYPE maktx,
         menge TYPE menge_d,
         meins TYPE meins,
         stlnr TYPE stnum,
         stlal TYPE stlal,
       END OF gty_alv_display_0001.

TYPES: gty_t_alv_display_0001 TYPE TABLE OF gty_alv_display_0001.

************************************************************************
** TELAS
************************************************************************
DATA: scr0001         TYPE gty_scr0001.

**********************************************************************
** CLASSES
**********************************************************************
CLASS gcl_handle_events DEFINITION DEFERRED.

************************************************************************
** TABELAS GLOBAIS
************************************************************************
DATA: gt_alv_display_0001 TYPE TABLE OF gty_alv_display_0001,
      gt_ltbp             TYPE SORTED TABLE OF ltbp WITH NON-UNIQUE KEY tbnum.

************************************************************************
** REFERENCIAS GLOBAIS
************************************************************************
DATA: gref_alv_display_0001   TYPE REF TO cl_salv_table,
      gref_alv_events_0001    TYPE REF TO gcl_handle_events,
      gref_alv_container_0001 TYPE REF TO cl_gui_custom_container.

************************************************************************
** VARIAVEIS GLOBAIS
************************************************************************
DATA: gv_werks TYPE werks_d,
      gv_lgort TYPE lgort_d.
