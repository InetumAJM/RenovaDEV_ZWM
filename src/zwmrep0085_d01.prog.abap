*&---------------------------------------------------------------------*
*&  Include           ZWMREP0085_D01
*&---------------------------------------------------------------------*
***********************************************************************
** Pool de Tipos
***********************************************************************
TYPE-POOLS: abap.

**********************************************************************
** Tipos
**********************************************************************
TYPES: BEGIN OF gty_scr0001,
        ok_code     LIKE sy-ucomm,
        exidv       TYPE exidv,
        exidv1      TYPE exidv,
        matnr       TYPE matnr,
        maktx_a(20) TYPE c,
        maktx_b(20) TYPE c,
        ean11       TYPE marm-ean11,
        charg       TYPE charg_d,
        vemng       LIKE vepo-vemng,
        altme       LIKE vepo-altme,
        vemeh       LIKE vepo-vemeh,
        origem(14)  TYPE c,
        actual      TYPE i,
        total       TYPE i,
      END OF gty_scr0001.

TYPES: BEGIN OF gty_mat,
        exidv       LIKE vekp-exidv,
        matnr       LIKE vepo-matnr,
        charg       LIKE vepo-charg,
        vemng       LIKE vepo-vemng,
        vemeh       LIKE vepo-vemeh,
        altme       LIKE vepo-altme,
        maktx_a(20) TYPE c,
        maktx_b(20) TYPE c,
        pos(14)     TYPE c,
        werks       TYPE werks_d,
        lgort       TYPE lgort_d,
        vkorg       TYPE vkorg,
        vtweg       TYPE vekp-vtweg,
  END OF gty_mat.

*DATA: BEGIN OF gt_mat OCCURS 0,
*        exidv       like vekp-exidv,
*        matnr       LIKE vepo-matnr,
*        charg       LIKE vepo-charg,
*        vemng       LIKE vepo-vemng,
*        altme       LIKE vepo-altme,
*        maktx_a(20) TYPE c,
*        maktx_b(20) TYPE c,
*        pos(14)     TYPE c,
*      END OF gt_mat.



DATA: BEGIN OF gs_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA: END OF gs_user.

**********************************************************************
** Estrutura Globais
**********************************************************************
DATA: scr0001 TYPE gty_scr0001.

**********************************************************************
** Tabelas Globais
**********************************************************************
DATA: gt_sscc TYPE TABLE OF gty_scr0001.
DATA: gt_mat  TYPE TABLE OF gty_mat WITH HEADER LINE.
**********************************************************************
** Variaveis Globais
**********************************************************************
DATA: gv_lgnum    TYPE lqua-lgnum,
      gv_text1    LIKE bdcmsgcoll-msgv1,
      gv_text2    LIKE bdcmsgcoll-msgv2,
      gv_text3    LIKE bdcmsgcoll-msgv2,
      gv_text4    LIKE bdcmsgcoll-msgv2,
      setscreen1  TYPE char4,
      gv_printer  TYPE fpmedium.
