*&---------------------------------------------------------------------*
*&  Include           ZECRAM                                           *
*&---------------------------------------------------------------------*
********************* SELECTION SCREEN *********************

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.

PARAMETERS lgnum LIKE mlgn-lgnum DEFAULT '100' OBLIGATORY MEMORY ID lgn.

SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.

SELECT-OPTIONS: refnr FOR t311-refnr OBLIGATORY,
                refnt FOR t311-refnt,
                vkorg FOR likp-vkorg,
                datum FOR t311-datum OBLIGATORY.

SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) text-p01.
*                TEXT-P01 'Release Indicator'
PARAMETERS rel RADIOBUTTON GROUP rel   DEFAULT 'X'.
SELECTION-SCREEN COMMENT (13)  text-p02.
*                TEXT-P02 'released'
PARAMETERS notrel RADIOBUTTON GROUP rel.
SELECTION-SCREEN COMMENT (18)  text-p03.
*                TEXT-P02 'Not released'
PARAMETERS bothrel RADIOBUTTON GROUP rel.
SELECTION-SCREEN COMMENT (6)  text-p04.
*                TEXT-P02 'Both'

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) text-l00.
PARAMETERS: p_curso RADIOBUTTON GROUP lib.
SELECTION-SCREEN COMMENT (13)  text-l01.
PARAMETERS: p_concl RADIOBUTTON GROUP lib.
SELECTION-SCREEN COMMENT (18)  text-l02.
PARAMETERS: p_ambos RADIOBUTTON GROUP lib.
SELECTION-SCREEN COMMENT (6)  text-l03.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
**
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-b03.
PARAMETERS: p_vis RADIOBUTTON GROUP list DEFAULT 'X'.
PARAMETERS: p_mod RADIOBUTTON GROUP list.
SELECTION-SCREEN END OF BLOCK b3.
********************* END OF SELECTION SCREEN *********************
