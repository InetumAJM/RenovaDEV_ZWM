*&---------------------------------------------------------------------*
*&  Include           ZWMREP0133_D01
*&---------------------------------------------------------------------*


***********************************************************************
** TYPE POOLS
***********************************************************************
TYPE-POOLS: abap.

***********************************************************************
** TIPOS
***********************************************************************
TYPES: BEGIN OF gty_scr0001,
         appended TYPE flag,
         okcode   TYPE syucomm,
         exidv    TYPE exidv,
         maktx1   TYPE c LENGTH 20,
         maktx2   TYPE c LENGTH 20,
         menge_l  TYPE menge_d, "Quantidade Lida
         lenum    TYPE lenum,
         menge    TYPE menge_d, "Quantidade a Ler
         menge_f  TYPE menge_d, "Quantidade em falta
         s_mlgn_f TYPE mlgn,
         s_vepo   TYPE vepo,
         s_mast   TYPE mast,
         s_stpo   TYPE stpo,
         s_lqua   TYPE lqua,
         s_mchb   TYPE mchb,
         page_a   TYPE sytabix,
         page_t   TYPE sytabix,
       END OF gty_scr0001.

TYPES: gty_t_scr0001 TYPE TABLE OF gty_scr0001.

TYPES: gty_t_mseg TYPE TABLE OF mseg.

***********************************************************************
** SCREENS
***********************************************************************
DATA: scr0001 TYPE gty_scr0001.

***********************************************************************
** VARIAVEIS
***********************************************************************
DATA: gt_scr0001           TYPE TABLE OF gty_scr0001,
      gt_scr0001_reads     TYPE TABLE OF gty_scr0001,
      gt_scr0001_nwm       TYPE TABLE OF gty_scr0001,
      gt_scr0001_nwm_reads TYPE TABLE OF gty_scr0001.

DATA: gv_lgnum TYPE lgnum.

DATA: gv_lgnum_in   TYPE lgnum VALUE '100',
      gv_bwart_in   TYPE bwart,
      gv_bwart_out  TYPE bwart,
      gv_lgort_f_in TYPE lgort_d,

      gv_kostl      type kostl,
      gv_lgort_comp type lgort_d,

      gv_bukrs      type bukrs,
      gv_blart      type blart,

      gv_hkont_c    type hkont,
      gv_hkont_d    type hkont,
      gv_mwskz      type mwskz.
