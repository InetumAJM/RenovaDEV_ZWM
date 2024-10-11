*&---------------------------------------------------------------------*
*&  Include           ZWMREP0069_TOP                                   *
*&---------------------------------------------------------------------*

** Tabelas DD
************************************************************************
TABLES: lagp.

** Variáveis
************************************************************************
DATA: ok_code_0001 LIKE sy-ucomm.

** Screen routing
DATA: setscreen1(4).

** Tela
DATA: pdt_lgpla(14),
      pdt_matnr LIKE mara-matnr,
      pdt_maktx1(20),
      pdt_maktx2(20),
      cursorfield(30).

DATA: lgpla     LIKE lqua-lgpla,
      lgtyp     LIKE lqua-lgtyp,
      lgnum     LIKE ltap-lgnum,
      lgtyp_pck LIKE lqua-lgtyp,
      maktx     LIKE makt-maktx,
      linhas    TYPE i,
      ret_code,
      text1(100),
      text2(100).

** Tabelas Internas
************************************************************************
DATA: BEGIN OF it_mlgt OCCURS 0,
        matnr LIKE mlgt-matnr,
      END OF it_mlgt.

DATA : BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA : END OF l_user.

DATA : BEGIN OF return_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF return_msg.
