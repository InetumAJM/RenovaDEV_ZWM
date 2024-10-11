*&---------------------------------------------------------------------*
*&  Include           ZXEAN128U01                                      *
*&---------------------------------------------------------------------*

*BREAK-POINT.

*TABLES: vekp, vepo.

*DATA: t_vepo LIKE vepo OCCURS 0 WITH HEADER LINE,
*      linha TYPE i.

*CLEAR: t_vepo, linha.
*REFRESH t_vepo.

*SELECT SINGLE * FROM vekp WHERE exidv = if_hu_extern.
*
*SELECT * INTO TABLE t_vepo
*        FROM vepo
*            WHERE venum = vepo-venum.
*
*DESCRIBE TABLE t_vepo LINES linha.
*
*IF linha = 1.
*  READ TABLE t_vepo INDEX 1.
*  cs_ean128_data-charg = t_vepo-charg.
*  cs_ean128_data-vfdat = t_vepo-vfdat.
*ENDIF.
*
*data exit .
*while exit is initial .
*endwhile .
