*&---------------------------------------------------------------------*
*&  Include           ZXEAN128U02                                      *
*&---------------------------------------------------------------------*


*TABLES: vekp, vepo.
*
*DATA: t_vepo LIKE vepo OCCURS 0 WITH HEADER LINE,
*      linha TYPE i.
*
*CLEAR: t_vepo, linha.
*REFRESH t_vepo.
*
*SELECT SINGLE * FROM vekp WHERE exidv = IS_EAN128_DATA-EXIDV.
*
*SELECT * INTO TABLE t_vepo
*        FROM vepo
*            WHERE venum = vekp-venum.
*
*DESCRIBE TABLE t_vepo LINES linha.
*
*IF linha = 1.
*  READ TABLE t_vepo INDEX 1.
**  cs_ean128_data-charg = t_vepo-charg.
**  cs_ean128_data-vfdat = t_vepo-vfdat.
*ENDIF.
*
*data exit .
*while exit is initial .
*endwhile .
