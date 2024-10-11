*&---------------------------------------------------------------------*
*&  Include           ZWMREP0124_D01
*&---------------------------------------------------------------------*

** Type Polls
***********************************************************************
TYPE-POOLS: abap.

** Tipos
***********************************************************************
TYPES: gtyp_menge_c TYPE c LENGTH 18,
       gtyp_page    TYPE c LENGTH 2.

TYPES: gty_t_tanum TYPE TABLE OF tanum.

TYPES: BEGIN OF gty_mm_mov,
         mblnr TYPE mblnr,
         mjahr TYPE mjahr,
       END OF gty_mm_mov.

TYPES: gty_t_mm_mov TYPE TABLE OF gty_mm_mov.

TYPES: gty_t_zwm029 TYPE TABLE OF zwm029.

TYPES: BEGIN OF gty_scr0001,
         okcode     TYPE syucomm,
         lgpla_bc   TYPE barcode,
         lgtyp      TYPE lgtyp,
         lgpla      TYPE lgpla,
         matnr      TYPE matnr,
         xchpf      TYPE xchpf,
         xchpf_real TYPE xchpf,
         maktx      TYPE maktx,
         maktxa     TYPE char20,
         maktxb     TYPE char20,
         charg      TYPE charg_d,
         werks      TYPE werks_d,
         lgort      TYPE lgort_d,
         menge_disp TYPE menge_d,
         meins_disp TYPE meins,
         menge_c    TYPE gtyp_menge_c,
         menge      TYPE menge_d,
         meins      TYPE meins,
         pag        TYPE gtyp_page,
         pag_t      TYPE gtyp_page,
         new        TYPE flag,
         appended   TYPE flag,
       END OF gty_scr0001.

TYPES: BEGIN OF gty_scr0002,
         okcode TYPE syucomm,
         ean    TYPE ean11,
       END OF gty_scr0002.

** Screens
***********************************************************************
DATA: scr0001     TYPE gty_scr0001,
      scr0002     TYPE gty_scr0002,
      scr0001_new TYPE gty_scr0001.

** Tabelas Globais
***********************************************************************
DATA: gt_lqua    TYPE TABLE OF lqua,
      gt_scr0001 TYPE TABLE OF gty_scr0001.

** Variaveis Globais
***********************************************************************
DATA: gv_lgnum          TYPE lgnum,
      gv_bsskz_mov      TYPE lvs_bsskz,
      gv_mm_code_ent    TYPE gm_code,
      gv_mm_code_sai    TYPE gm_code,
      gv_mm_code_transf TYPE gm_code,
      gv_bwlvs_ent      TYPE bwlvs,
      gv_bwlvs_sai      TYPE bwlvs,
      gv_lgort_dif      TYPE lgort_d,
      gv_bwart_ent      TYPE bwart,
      gv_bwart_sai      TYPE bwart,
      gv_bwart_transf   TYPE bwart,
      gv_kostl_ent      TYPE kostl,
      gv_kostl_sai      TYPE kostl,
      gv_scr0001        TYPE c LENGTH 4,
      gv_scr0002        TYPE c LENGTH 4.
