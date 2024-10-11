*&---------------------------------------------------------------------*
*&  Include           ZWMREP0072_TOP                                *
*&---------------------------------------------------------------------*
PROGRAM  ZWMREP0072 MESSAGE-ID zwmmsg001.

INCLUDE: rlmobinc.

TABLES: link, linp.

DATA : ok_code_0001 LIKE sy-ucomm,
       posicao(14),
       cursorfield(30),
       text1(100),
       text2(100),
       ret_code,
       whs TYPE lgnum,
       l_bin(14) TYPE c,
       l_lgtyp TYPE lgtyp,
       l_lgpla TYPE lgpla.

DATA: itab_zwm001 LIKE zwm001 OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA : END OF l_user.
