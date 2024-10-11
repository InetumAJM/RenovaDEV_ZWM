*&---------------------------------------------------------------------*
*&  Include           ZWMMPRF013_TOP
*&---------------------------------------------------------------------*

***********************************************************************
** Pool de Tipos
***********************************************************************
TYPE-POOLS: abap.

**********************************************************************
** Variáveis Globais
**********************************************************************
DATA: resposta,
      whs        LIKE lqua-lgnum,
      text1      LIKE bdcmsgcoll-msgv1,
      text2      LIKE bdcmsgcoll-msgv2,
      text3      LIKE bdcmsgcoll-msgv2,
      text4      LIKE bdcmsgcoll-msgv2,
      setscreen1 TYPE char4,
      valor      LIKE zwm001-valor.


DATA: gv_werks      TYPE werks_d.
DATA: gv_lgort      TYPE lgort_d.
DATA: gv_lgort_dest TYPE lgort_d.
DATA: gv_lgort_dest_cons TYPE lgort_d.
DATA: gv_queue      LIKE ltak-queue.
DATA: gv_to_mov     TYPE bwlvs.
DATA: gv_gm_code    TYPE gm_code.
DATA: gv_gm_mov     TYPE bwart.
DATA: gv_gm_mov_unblock   TYPE bwart.
DATA: gv_gm_mov_block     TYPE bwart.
DATA: gv_bktxt      TYPE mkpf-bktxt.
DATA: gv_su_block.
DATA: gv_email_target TYPE so_recname.
**********************************************************************
** Estruturas globais
**********************************************************************
DATA: BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
      DATA: END OF l_user.

DATA: gt_ltap TYPE ltap OCCURS 0 WITH HEADER LINE.

**********************************************************************
** Variáveis da tela
**********************************************************************
DATA: scr_su        TYPE lenum.
DATA: scr_matnr     TYPE matnr.
DATA: scr_desc1     TYPE char20.
DATA: scr_desc2     TYPE char20.
DATA: scr_charg     TYPE charg_d.
DATA: scr_qtd       TYPE ltap-vsolm.
DATA: scr_uni       TYPE ltap-meins.
DATA: scr_pos_dest  TYPE lagp-lgpla.
DATA: scr_pos_dest_lgtyp  TYPE lagp-lgtyp.
DATA: scr_pos_conf  TYPE lagp-lgpla.
DATA: scr_pos_ori_typ  TYPE lagp-lgtyp.
DATA: gv_interf_typ TYPE lgtyp.

RANGES: r_arm_typ FOR lagp-lgtyp.
