*----------------------------------------------------------------------*
* Include: ZWMFR0001_SCR
*----------------------------------------------------------------------*
* Description: Monitor de planeamento do abastecimento à produção
*----------------------------------------------------------------------*
* Author........: [Tiago Pateiro] [ROFFD] [ROFF(SDF)]
* Creation date: 2015-10-23
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS s_werks FOR aufk-werks NO INTERVALS NO-EXTENSION OBLIGATORY.
SELECT-OPTIONS s_gstrp FOR afko-gstrp.
SELECT-OPTIONS s_fevor FOR afko-fevor.
SELECT-OPTIONS s_aufnr FOR afko-aufnr.
SELECT-OPTIONS s_matnr FOR resb-matnr.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS p_layo TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b2.
