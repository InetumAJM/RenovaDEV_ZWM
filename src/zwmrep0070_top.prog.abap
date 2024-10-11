*&---------------------------------------------------------------------*
*& Include ZWMREP0070_TOP                                              *
*&                                                                     *
*&---------------------------------------------------------------------*

** Tabelas DD
************************************************
TABLES: lagp.

** Constantes
*************************************************
CONSTANTS: gc_true  TYPE c VALUE 'X',
           gc_false TYPE c VALUE ' '.

** Variáveis
*************************************************
DATA: resposta,
*     lgnum     LIKE lqua-lgnum,
      currpage  TYPE i,
      totalpage TYPE i,
      auxindex  TYPE i,
      linhas    TYPE i,
      qtd       LIKE ekpo-menge,
      text1     LIKE bdcmsgcoll-msgv1,
      text2     LIKE bdcmsgcoll-msgv2.

** Telas
DATA: setscreen1(4),
      cursorfield(20).

** TELA 0001
DATA: ok_code_0001 LIKE sy-ucomm,
      pdt_sscc     LIKE vekp-exidv,
      pdt_pag(3),
      pdt_total(3),

      pdt_matnr1   LIKE vepo-matnr,
      pdt_maktx1a(20),
      pdt_maktx1b(20),
      pdt_vemng1   LIKE vepo-vemng,
      pdt_altme1   LIKE vepo-altme,
      pdt_charg1   LIKE lqua-charg,

      pdt_matnr2   LIKE vepo-matnr,
      pdt_maktx2a(20),
      pdt_maktx2b(20),
      pdt_vemng2   LIKE vepo-vemng,
      pdt_altme2   LIKE vepo-altme,
      pdt_charg2   LIKE lqua-charg.

** Tabelas Internas
*************************************************
DATA: BEGIN OF it_vekp OCCURS 0,
         venum LIKE vekp-venum,
         erdat LIKE vekp-erdat,
      END OF it_vekp.

DATA: BEGIN OF it_vepo OCCURS 0,
        matnr LIKE vepo-matnr,
        charg LIKE vepo-charg,
        vemng LIKE vepo-vemng,
        vemeh LIKE vepo-vemeh,
        altme LIKE vepo-altme,
      END OF it_vepo.

DATA: BEGIN OF it_makt OCCURS 0,
         matnr LIKE makt-matnr,
         maktx LIKE makt-maktx,
      END OF it_makt.

DATA: BEGIN OF it_mat OCCURS 0,
        matnr  LIKE vepo-matnr,
        charg  LIKE vepo-charg,
        vemng  LIKE vepo-vemng,
        altme  LIKE vepo-altme,
        maktxa LIKE pdt_maktx1a,
        maktxb LIKE pdt_maktx1b,
      END OF it_mat.

*DATA: BEGIN OF l_user OCCURS 0.
*        INCLUDE STRUCTURE lrf_wkqu.
*DATA: END OF l_user.
