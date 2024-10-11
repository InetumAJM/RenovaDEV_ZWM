*&---------------------------------------------------------------------*
*&  Include           ZWMFR0004_D01
*&---------------------------------------------------------------------*

TYPE-POOLS: abap.


************************************************************************
** TIPOS GLOBAIS
************************************************************************
TYPES: BEGIN OF gty_scr0001,
         okcode TYPE syucomm,
       END OF gty_scr0001.

TYPES: BEGIN OF gty_alv_display_0001,
         message TYPE c LENGTH 200,
       END OF gty_alv_display_0001.

TYPES: gty_t_alv_display_0001 TYPE TABLE OF gty_alv_display_0001.
DATA gv_old_aufnr TYPE aufk-aufnr.

************************************************************************
** TELAS
************************************************************************
DATA: scr0001         TYPE gty_scr0001.

************************************************************************
** TABELAS GLOBAIS
************************************************************************
DATA: gt_alv_display_0001 TYPE TABLE OF gty_alv_display_0001.

************************************************************************
** REFERENCIAS GLOBAIS
************************************************************************
DATA: gref_alv_display_0001   TYPE REF TO cl_salv_table,
      gref_alv_container_0001 TYPE REF TO cl_gui_custom_container.
