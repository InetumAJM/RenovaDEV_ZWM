*&---------------------------------------------------------------------*
*&  Include           ZWMREP0088_D01
*&---------------------------------------------------------------------*

***********************************************************************
** TYPE POOLS
***********************************************************************
TYPE-POOLS: abap.

***********************************************************************
** TIPOS
***********************************************************************
TYPES: BEGIN OF gty_scr0001,
         okcode     TYPE syucomm,
         exidv      TYPE exidv,
         exidv2     TYPE exidv,
         ean11      TYPE ean11,
         matnr      TYPE matnr,
         meinh      TYPE meinh,
         maktx_a    TYPE maktx,
         maktx_b    TYPE maktx,
         vhilm      TYPE vhilm,
         menge      TYPE menge_d,
         menge_conf TYPE flag,
         menge_pal  TYPE menge_d,
         nlpla      TYPE ltap_nlpla,
         nlpla_conf TYPE ltap_nlpla,
         remontada  TYPE flag,
         saved      TYPE flag,
       END OF gty_scr0001.

***********************************************************************
** SCREENS
***********************************************************************
DATA: scr0001 TYPE gty_scr0001.

***********************************************************************
** GLOBAL VARIABLES
***********************************************************************
DATA: gv_lgnum TYPE lgnum.
