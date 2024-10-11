*&---------------------------------------------------------------------*
*&  Include           ZWMREP0142_SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.

PARAMETERS p_lgnum TYPE lgnum DEFAULT '100' OBLIGATORY.
PARAMETERS p_lgtypo TYPE lgtyp DEFAULT 'PAL' OBLIGATORY.
PARAMETERS p_lgtypd TYPE lgtyp DEFAULT '916' OBLIGATORY.
SELECT-OPTIONS s_matnr FOR mara-matnr.

SELECTION-SCREEN END OF BLOCK b01.
