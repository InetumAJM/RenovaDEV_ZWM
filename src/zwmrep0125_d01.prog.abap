*&---------------------------------------------------------------------*
*&  Include           ZWMREP0125_D01
*&---------------------------------------------------------------------*

***********************************************************************
** TYPE-POOLS
***********************************************************************
TYPE-POOLS: abap.

***********************************************************************
** TYPES
***********************************************************************
TYPES: BEGIN OF gty_alv_display,
        lgtyp    TYPE lgtyp,
        lgpla    TYPE lgpla,
        exidv    TYPE exidv,
        matnr    TYPE matnr,
        maktx    TYPE maktx,
        vhilm    TYPE vhilm,
        quant    TYPE i,
        vhilm_m  TYPE vhilm,
        quant_m  TYPE i,
        vhilm_q  TYPE vhilm,
        quant_q  TYPE i,
       END OF gty_alv_display.

***********************************************************************
** OBJECTOS GLOBAIS
***********************************************************************
DATA: gref_alv_display TYPE REF TO cl_salv_table,
      gref_events      TYPE REF TO gcl_handle_events.

***********************************************************************
** TABELAS GLOBAIS
***********************************************************************
DATA: gt_alv_display TYPE TABLE OF gty_alv_display.
