*&---------------------------------------------------------------------*
*& Include ZWMREP0132_TOP                                             *
*&                                                                     *
*&---------------------------------------------------------------------*

** Tabelas DD
************************************************
TABLES: afko, csks, mara, mchb, makt, pa0001, t320,
        pa0105. "(+)INETUM - NR - 27.07.2021 - RENPRJ00019
** Variáveis
*************************************************
DATA: resposta,
      whs      LIKE lqua-lgnum,
      text1    LIKE bdcmsgcoll-msgv1,
      text2    LIKE bdcmsgcoll-msgv2,
      valor    LIKE zwm001-valor.

DATA: gv_ordem  TYPE c,
      gv_ccusto TYPE c,
      gv_mnv    TYPE c.

DATA: plant   LIKE mseg-werks,
      lgort_o LIKE mseg-lgort,
      lgort_d LIKE mseg-lgort,
      code    LIKE bapi2017_gm_code,
      mov     LIKE mseg-bwart.

** Telas
DATA: setscreen1(4),
      cursorfield(20).

** TELA 0001

DATA: BEGIN OF scr,
        cc_ordem(10),
        pernr        LIKE pa0030-pernr,
* INETUM - NR - 28.07.2021 - RENPRJ00019 - Inicio
        requisitante TYPE zrequisitante,
* INETUM - NR - 28.07.2021 - RENPRJ00019 - Fim
        matnr        LIKE mara-matnr,
        maktx1       LIKE makt-maktx,
        maktx2       LIKE makt-maktx,
        charg        LIKE mchb-charg,
        menge        LIKE mseg-menge,
        meins        LIKE mara-meins,
      END OF scr.

DATA: ok_code_0001 LIKE sy-ucomm.

DATA ti_zwm001 LIKE zwm001 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
      DATA: END OF l_user.

DATA ls_mchb LIKE mchb.

* INETUM - NR - 26.07.2021 - RENPRJ00019 - Inicio
TYPES: BEGIN OF ty_hardcode,
         bwart_cc_1     TYPE zhardcode_table-ue_low,
         bwart_cc_2     TYPE zhardcode_table-ue_low,
         bwart_ord_cc_1 TYPE zhardcode_table-ue_low,
         bwart_ord_cc_2 TYPE zhardcode_table-ue_low,
       END OF ty_hardcode.

DATA: gs_hardcode TYPE ty_hardcode.
* INETUM - NR - 26.07.2021 - RENPRJ00019 - Fim
