*&---------------------------------------------------------------------*
*& Include ZWMREP0137_TOP                                             *
*&                                                                     *
*&---------------------------------------------------------------------*

** Tabelas DD
************************************************
TABLES: afko, resb, vekp, vepo, makt, mara, mlgn, lqua, zwm001.

** Constantes
*************************************************
CONSTANTS: gc_true  TYPE c VALUE 'X',
           gc_false TYPE c VALUE ' '.

** Variáveis
*************************************************
DATA: resposta,
      whs         LIKE lqua-lgnum,
      text1       LIKE bdcmsgcoll-msgv1,
      text2       LIKE bdcmsgcoll-msgv2,
      text3       LIKE bdcmsgcoll-msgv3,
      text4       LIKE bdcmsgcoll-msgv4,
      valor       LIKE zwm001-valor.

DATA:
*      gv_matnr_ordem  LIKE resb-matnr,
      plant           LIKE mseg-werks,
      lgort           LIKE mseg-lgort,
      code            LIKE bapi2017_gm_code,
      mov             LIKE mseg-bwart.

** Telas
DATA: setscreen1(4),
      setscreen2(4),
      cursorfield(20),
      cursorfield2(20).

** TELA 0001
DATA: BEGIN OF scr1,
        refnr   TYPE lvs_refnr,
        vbeln_1 TYPE vbeln_vl,
        npal_1  TYPE numc2,
        pk_1    TYPE xfeld,
        pa_1    TYPE xfeld,
        sm_1    TYPE xfeld,
        vbeln_2 TYPE vbeln_vl,
        npal_2  TYPE numc2,
        pk_2    TYPE xfeld,
        pa_2    TYPE xfeld,
        sm_2    TYPE xfeld,
        vbeln_3 TYPE vbeln_vl,
        npal_3  TYPE numc2,
        pk_3    TYPE xfeld,
        pa_3    TYPE xfeld,
        sm_3    TYPE xfeld,
        idx     TYPE i,
        tot     TYPE i,
      END OF scr1.

** Global Data
DATA: gv_key     TYPE keyword-keyword.

DATA: gt_zwm069  TYPE zwm069  OCCURS 0 WITH HEADER LINE.
DATA: gt_bdcdata TYPE bdcdata OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA: END OF l_user.
