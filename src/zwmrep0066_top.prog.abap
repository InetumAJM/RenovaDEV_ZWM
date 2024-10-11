*&---------------------------------------------------------------------*
*& Include ZWMREP0066_TOP                                              *
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
DATA: aux_lgtyp   LIKE lagp-lgtyp,
      aux_lgpla   LIKE lagp-lgpla,
      gs_lqua     TYPE lqua,
      gs_marm     TYPE marm,
      resposta,
      whs         LIKE lqua-lgnum,
      lincount    TYPE i,
      currpage    TYPE i,
      totalpage   TYPE i,
      text1       LIKE bdcmsgcoll-msgv1,
      text2       LIKE bdcmsgcoll-msgv2,
      matnr       LIKE mara-matnr,
      pdt_pag(3),
      pdt_total(3).

** Telas
DATA: setscreen1(4),
      cursorfield(20).

** TELA 0001
DATA: ok_code_0001 LIKE sy-ucomm,

      pdt_origem(14) ,
      pdt_matnr LIKE lqua-matnr,
      pdt_ean11   TYPE marm-ean11,
      pdt_maktx_a LIKE makt-maktx,
      pdt_maktx_b LIKE makt-maktx,
      pdt_meins_1 LIKE lqua-meins,
      pdt_charg_1 LIKE lqua-charg,
      pdt_gesme_1(6) TYPE n," LIKE lqua-gesme,
      pdt_verme_1(6) TYPE n," LIKE lqua-verme,
      pdt_einme_1(6) TYPE n," LIKE lqua-einme,
      pdt_ausme_1(6) TYPE n." LIKE lqua-ausme.


**** Types
*************************************************
TYPES: BEGIN OF emat,
        matnr   LIKE lqua-matnr,
        maktx   LIKE makt-maktx,
        gesme   LIKE lqua-gesme,
        verme   LIKE lqua-verme,
        einme   LIKE lqua-einme,
        ausme   LIKE lqua-ausme,
        meins   LIKE lqua-meins,
        charg   LIKE lqua-charg,
     END OF emat.

TYPES: BEGIN OF elqua,
        matnr   LIKE lqua-matnr,
        gesme   LIKE lqua-gesme,
        verme   LIKE lqua-verme,
        einme   LIKE lqua-einme,
        ausme   LIKE lqua-ausme,
        meins   LIKE lqua-meins,
        charg   LIKE lqua-charg,
     END OF elqua.

** Tabelas Internas
*************************************************
DATA: it_mat  TYPE emat   OCCURS 0 WITH HEADER LINE,
      it_lqua TYPE elqua  OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA: END OF l_user.

DATA: BEGIN OF it_makt OCCURS 0,
        matnr LIKE makt-matnr,
        maktx LIKE makt-maktx,
      END OF it_makt.
