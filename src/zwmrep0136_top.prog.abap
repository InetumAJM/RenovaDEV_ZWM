*&---------------------------------------------------------------------*
*& Include ZWMREP0136_TOP                                             *
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
        vbeln   TYPE vbeln_vl,
        sscc_in TYPE lenum,
        sscc    TYPE lenum,
        matnr   TYPE matnr,
        maktx1  TYPE char20,
        maktx2  TYPE char20,
        charg   TYPE charg_d,
        menge   TYPE menge_d,
        meins   TYPE meins,
        idx     TYPE i,
        tot     TYPE i,
      END OF scr1.

DATA: BEGIN OF scr2,
        sscc_in TYPE lenum,
        sscc    TYPE lenum,
        matnr   TYPE matnr,
        maktx1  TYPE char20,
        maktx2  TYPE char20,
        charg   TYPE charg_d,
        menge   TYPE menge_d,
        meins   TYPE meins,
      END OF scr2.

** Global Data
DATA: gv_key    TYPE keyword-keyword.

DATA: gt_zwm069 TYPE zwm069 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA: END OF l_user.
