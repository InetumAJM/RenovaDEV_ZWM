*&---------------------------------------------------------------------*
*& Include ZWMREP0121_TOP                                             *
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
DATA: ok_code_0001 LIKE sy-ucomm,
      aufnr_in     LIKE afko-aufnr,
      sscc_in      LIKE vekp-exidv,
      data_in      LIKE mkpf-budat,
      matnr_out    LIKE lqua-matnr,
      maktx_out1   LIKE makt-maktx,
      maktx_out2   LIKE makt-maktx,
      meins_out    LIKE lqua-meins,
      charg_out    LIKE lqua-charg,
      menge_out    LIKE vepo-vemng.

** TELA 0002
DATA: ok_code_0002 LIKE sy-ucomm,
      ean11_in     LIKE marm-ean11,
      sscc2_out    LIKE vekp-exidv,
      matnr2_in    LIKE lqua-matnr,
      maktx2_out1  LIKE makt-maktx,
      maktx2_out2  LIKE makt-maktx,
      meins2_out   LIKE lqua-meins,
      charg2_out   LIKE lqua-charg,
      menge2_out   LIKE vepo-vemng,
      current_item TYPE i,
      total_items  TYPE i.


** Global Data
DATA: ti_zwm001 LIKE zwm001 OCCURS 0 WITH HEADER LINE,
      gt_marm   LIKE marm   OCCURS 0 WITH HEADER LINE,
      gt_lqua   LIKE lqua   OCCURS 0 WITH HEADER LINE.

DATA: gt_resb LIKE resb OCCURS 0 WITH HEADER LINE.

DATA: flg_matnr_input(1).

DATA: BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA: END OF l_user.
