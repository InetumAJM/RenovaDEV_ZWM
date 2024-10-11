*&---------------------------------------------------------------------*
*&  Include           ZWMFR0004_D02
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_werks  TYPE werks_d.
SELECTION-SCREEN COMMENT 40(20) sc_werks.
PARAMETERS: p_lgnum  TYPE lgnum.
SELECTION-SCREEN COMMENT 40(20) sc_lgnum.
PARAMETERS: p_fevor  TYPE afko-fevor.
PARAMETERS: p_aufnr  TYPE aufk-aufnr. " MATCHCODE OBJECT zwm_prod_order.
PARAMETERS: p_matnr  TYPE matnr.
SELECTION-SCREEN COMMENT 55(20) sc_matnr.
PARAMETERS: p_ean11  TYPE mean-ean11.
PARAMETERS: p_gmein  TYPE afko-gmein.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(31) meinhtx FOR FIELD p_meinh.
*PARAMETERS: p_meinh TYPE marm-meinh.
*SELECTION-SCREEN COMMENT 38(6) hoehetx FOR FIELD p_hoehe.
*PARAMETERS: p_hoehe TYPE marm-hoehe.
*PARAMETERS: p_meabm TYPE marm-meabm.
*SELECTION-SCREEN END OF LINE.

PARAMETERS: p_menge  TYPE menge_d.
PARAMETERS: p_vhilm  TYPE matnr.
SELECTION-SCREEN COMMENT 55(20) sc_vhilm.
PARAMETERS: p_nrpal  TYPE n LENGTH 2 DEFAULT 1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
PARAMETERS: p_print  LIKE pri_params-pdest,
            p_copies LIKE itcpo-tdcopies DEFAULT 3.
SELECTION-SCREEN END OF BLOCK b2.
