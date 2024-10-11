*&---------------------------------------------------------------------*
*& Include ZWMREP0122_TOP                                              *
*&                                                                     *
*&---------------------------------------------------------------------*

** Tabelas DD
************************************************
TABLES: vekp, vepo, makt, zwm001, mlgn, mara, afko.

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

DATA: plant   LIKE mseg-werks,
      lgort   LIKE mseg-lgort,
      code    LIKE bapi2017_gm_code,
      mov     LIKE mseg-bwart.

** Telas
DATA: setscreen1(4),
      cursorfield(20).

** TELA 0001
DATA: ok_code_0001 LIKE sy-ucomm,
      aufnr_in     LIKE afko-aufnr,
      sscc_out     LIKE vekp-exidv,
      matnr_in     LIKE lqua-matnr,
      ean_in       LIKE marm-ean11,
      menge_in     LIKE mseg-menge,
      data_in      LIKE mkpf-budat,
      maktx_out1   LIKE makt-maktx,
      maktx_out2   LIKE makt-maktx,
      meins_out    LIKE lqua-meins,
      charg_out    LIKE lqua-charg,
      menge_out    LIKE vepo-vemng.

DATA: ti_zwm001 LIKE zwm001 OCCURS 0 WITH HEADER LINE,
      t_marm    LIKE marm   OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA: END OF l_user.
