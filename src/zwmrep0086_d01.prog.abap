*&---------------------------------------------------------------------*
*&  Include           ZWMREP0086_D01
*&---------------------------------------------------------------------*

***********************************************************************
** Pool de Tipos
***********************************************************************
TYPE-POOLS: abap.

**********************************************************************
** Variáveis Globais
**********************************************************************
DATA: gv_whs         LIKE lqua-lgnum,
      gv_text1       LIKE bdcmsgcoll-msgv1,
      gv_text2       LIKE bdcmsgcoll-msgv2,
      gv_text3       LIKE bdcmsgcoll-msgv2,
      gv_text4       LIKE bdcmsgcoll-msgv2,
      gv_setscreen1  TYPE char4,
      cursorfield(20).

DATA:  gv_mm_mov   LIKE ltak-bwlvs,
       gv_wm_mov   LIKE ltak-bwlvs,
       gv_tp_dep_dest  TYPE lqua-lgtyp,
       gv_pos_dep_dest TYPE lqua-lgpla.

**********************************************************************
** Estruturas globais
**********************************************************************
DATA: BEGIN OF gs_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA: END OF gs_user.


DATA: BEGIN OF scr0001,
      ok_code      TYPE sy-ucomm,
      lenum        TYPE lqua-lenum,
      matnr        TYPE lqua-matnr,
      maktx        TYPE makt-maktx,
      maktx_a(20)  TYPE c,
      maktx_b(20)  TYPE c,
      charg        TYPE lqua-charg,
      p_ori(14)    TYPE c,
      p_dest(14)   TYPE c,
      gesme        TYPE lqua-gesme,
      qtd          TYPE lqua-gesme,
      gesme_max    TYPE lqua-gesme,
      vemeh        TYPE meins,
      meins        TYPE meins,
      lgort_d      TYPE t001l-lgort,
      lgort_o      TYPE lgort_d,
      ean11        TYPE marm-ean11,
      lgtyp        TYPE lgtyp,
      lgpla        TYPE lgpla,
      werks        TYPE werks_d,
      lgort        like t001l-lgort,
      bktxt        TYPE mkpf-bktxt,
      umrez        TYPE marm-umrez,
      umren        TYPE marm-umren,
    END OF scr0001.

**********************************************************************
** Ranges globais
**********************************************************************

TYPES: BEGIN OF gty_td_su,
          lgtyp TYPE t331-lgtyp,
          lenvw TYPE t331-lenvw,
       END OF gty_td_su.


TYPES: BEGIN OF gty_td,
   lgtyp TYPE t331-lgtyp,
END OF gty_td.
DATA: gt_td_su TYPE TABLE OF  gty_td_su,
*      gt_td    TYPE TABLE OF  t331-lgtyp.
      gt_td    TYPE TABLE OF gty_td.
