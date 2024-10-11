*&---------------------------------------------------------------------*
*&  Include           ZWMREP0068_TOP                                   *
*&---------------------------------------------------------------------*

** Tabelas DD
************************************************************************
TABLES: lein, lqua, mlgt.

CONSTANTS: gc_lgtyp_rej TYPE lgtyp VALUE 'REJ'.
CONSTANTS: gc_lgpla_rej TYPE lgpla VALUE 'MAN_REJ'.
CONSTANTS: gc_lgtyp_wcs TYPE lgtyp VALUE 'EAU'.
CONSTANTS: gc_lgpla_wcs TYPE lgpla VALUE 'MAN_ENT1'.

** Variáveis
************************************************************************
DATA : ok_code_0001 LIKE sy-ucomm.

*screen routing
DATA: setscreen1(4).

DATA: lgpla           LIKE lqua-lgpla,
      lgtyp           LIKE lqua-lgtyp,
      bestq           LIKE lqua-bestq,
      lgnum           LIKE ltap-lgnum,
      werks           TYPE werks_d,
      lgort           TYPE lgort_d,
      bwlvs           TYPE bwlvs,
      lgtyp_pck       LIKE lqua-lgtyp,
      lgtyp_dri       LIKE lqua-lgtyp,
      pdt_sscc        LIKE lein-lenum,
      pdt_sscc2       LIKE lein-lenum,
      pdt_lgpla(14),
      pdt_matnr       LIKE mara-matnr,
      maktx           LIKE makt-maktx,
      pdt_maktx1(20),
      pdt_maktx2(20),
      pdt_gesme       LIKE lqua-gesme,
      pdt_meins       LIKE mara-meins,
      pdt_charg       LIKE mchb-charg,
      dest_dri(14),
      anzqu           LIKE lagp-anzqu,
      skzue           LIKE lagp-skzue,
      skzua           LIKE lagp-skzua,
      cursorfield(30),
      indice          LIKE sy-tabix,
      ret_code,
      text1(100),
      text2(100),
      pdt_dest(14),
      pdt_refnr       TYPE lvs_refnr,
      pdt_pck(14).

** Tabelas Internas
************************************************************************
DATA: it_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

DATA: it_lgtyp LIKE zwm001 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
      DATA: END OF l_user.

DATA: BEGIN OF return_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
      DATA: END OF return_msg.
