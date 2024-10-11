*&---------------------------------------------------------------------*
*& Include ZWMREP0123TOP                                               *
*&                                                                     *
*&---------------------------------------------------------------------*

** Tabelas DD
************************************************
TABLES: vekp, vepo, makt, zwm001, mlgn, mara, mchb.

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
      valor       LIKE zwm001-valor,
      plant       LIKE mard-werks,
      lgort       LIKE mard-lgort,
      l_printer   LIKE nast-ldest,
      n_copias    like itcpo-tdcopies.
DATA: setscreen1(4),
      cursorfield(20).

DATA: ok_code_0001 LIKE sy-ucomm,
      matnr_in     LIKE lqua-matnr,
      ean_in       LIKE marm-ean11,
      menge_in     LIKE mseg-menge,
      maktx_out1   LIKE makt-maktx,
      maktx_out2   LIKE makt-maktx,
      meins_out    LIKE lqua-meins,
      charg_in     LIKE lqua-charg,
      vhilm_in     LIKE vekp-vhilm,
      n_print      TYPE i,
      print_in    TYPE itcpo-tddest.

DATA: ti_zwm001  LIKE zwm001     OCCURS 0 WITH HEADER LINE,
      t_marm     LIKE marm       OCCURS 0 WITH HEADER LINE,
      return_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
      itab_sscc  LIKE zwm_ean128 OCCURS 0 WITH HEADER LINE,
      items_hu   LIKE zwm_items_hu OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA: END OF l_user.

DATA: gv_tcode TYPE char1.
DATA: gv_tud   TYPE letyp.
