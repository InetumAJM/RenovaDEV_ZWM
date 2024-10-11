*&---------------------------------------------------------------------*
*&  Include           ZWMREP0135_D02
*&---------------------------------------------------------------------*


************************************************************************
** Parâmetros de entrada
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-000.
PARAMETERS: p_werks LIKE t001w-werks DEFAULT 'RENV' OBLIGATORY.
SELECT-OPTIONS: s_matnr FOR mara-matnr.
SELECT-OPTIONS: s_data  FOR sy-datum.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN: END OF BLOCK blk1.

PARAMETERS p_cwidth DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_hrzgln DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_vrtgln DEFAULT 'X' NO-DISPLAY.
