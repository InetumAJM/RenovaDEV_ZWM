*&---------------------------------------------------------------------*
*&  Include           ZWMREP0087_TOP
*&---------------------------------------------------------------------*
TYPES: BEGIN OF st_items,
        exidv TYPE lenum,
        werks TYPE werks_d,
        lgort TYPE lgort_d,
        matnr TYPE matnr,
        charg TYPE charg_d,
        verme TYPE lqua_verme,
        meins TYPE meins,
      END OF st_items.

**********************************************************************
** Variáveis Globais
**********************************************************************
DATA: resposta,
      whs         LIKE lqua-lgnum,
      text1       LIKE bdcmsgcoll-msgv1,
      text2       LIKE bdcmsgcoll-msgv2,
      text3       LIKE bdcmsgcoll-msgv2,
      text4       LIKE bdcmsgcoll-msgv2,
      setscreen1  TYPE char4,
      valor       LIKE zwm001-valor.

DATA: flag_page.

DATA: gv_printer  TYPE fpmedium.
DATA: gv_werks    TYPE werks.
DATA: gv_werks_o  TYPE werks.
DATA: gv_lgort_o  TYPE lgort_d.
DATA: gv_gm_code  TYPE gm_code.
DATA: gv_mov_mm   TYPE bwart.

**********************************************************************
** Estruturas globais
**********************************************************************
DATA: BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA: END OF l_user.

DATA: gr_lgort_d  TYPE RANGE OF lgort_d WITH HEADER LINE.
DATA: gr_werks_d  TYPE RANGE OF werks_d WITH HEADER LINE.

DATA: gt_items    TYPE st_items OCCURS 0 WITH HEADER LINE.
DATA: gt_items_h  TYPE st_items OCCURS 0 WITH HEADER LINE.

*DATA: gt_lqua TYPE lqua OCCURS 0 WITH HEADER LINE.

**********************************************************************
** Variáveis da tela
**********************************************************************
DATA: scr_su        TYPE lenum.
DATA: scr_su1       TYPE lenum.
DATA: scr_su_rec    TYPE lenum.
DATA: scr_matnr     TYPE matnr.
DATA: scr_desc1     TYPE char20.
DATA: scr_desc2     TYPE char20.
DATA: scr_charg     TYPE charg_d.
DATA: scr_qtd       TYPE ltap-vsolm.
DATA: scr_uni       TYPE ltap-meins.
DATA: scr_n         TYPE i.
DATA: scr_total     TYPE i.
DATA: scr_lgort     TYPE lgort_d.
DATA: scr_werks     TYPE werks_d.
