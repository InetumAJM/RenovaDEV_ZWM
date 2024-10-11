*&---------------------------------------------------------------------*
*&  Include           ZECRAM0046                                       *
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
PARAMETERS:     P_SAMMG LIKE zwmlog02-SAMMG obligatory.
SELECT-OPTIONS: s_vbeln FOR zwmlog02-vbeln.

SELECTION-SCREEN END OF BLOCK a1.
