*&---------------------------------------------------------------------*
*&  Include           ZWMFR0004_D02
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_werks  TYPE werks_d.
SELECTION-SCREEN COMMENT 40(20) sc_werks.
PARAMETERS: p_lgnum  TYPE lgnum.
SELECTION-SCREEN COMMENT 40(20) sc_lgnum.
PARAMETERS: p_aufnr  TYPE aufk-aufnr.
PARAMETERS: p_fevor  TYPE afko-fevor.
PARAMETERS: p_matnr  TYPE matnr.
SELECTION-SCREEN COMMENT 55(20) sc_matnr.
PARAMETERS: p_ean11  TYPE mean-ean11.
PARAMETERS: p_gmein  TYPE afko-gmein.
PARAMETERS: p_menge  TYPE menge_d.
PARAMETERS: p_vhilm  TYPE matnr.
SELECTION-SCREEN COMMENT 55(20) sc_vhilm.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
PARAMETERS: p_print   LIKE pri_params-pdest,
            p_copies  LIKE itcpo-tdcopies DEFAULT 2.
SELECTION-SCREEN END OF BLOCK b2.
