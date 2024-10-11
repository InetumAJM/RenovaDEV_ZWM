*&---------------------------------------------------------------------*
*&  Include      ZWMREP0052_ECRA                                       *
*&---------------------------------------------------------------------*
************************************************************************
** Parâmetros de entrada
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-000.
PARAMETERS: p_lgnum LIKE ltak-lgnum DEFAULT '100' OBLIGATORY.
SELECT-OPTIONS: s_ename FOR usr01-bname MATCHCODE OBJECT user_comp,
                s_queue FOR ltak-queue,
                s_data  FOR sy-datum OBLIGATORY,
                s_hora  FOR sy-uzeit OBLIGATORY.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: s_bwlvs FOR ltak-bwlvs.

SELECTION-SCREEN: END OF BLOCK blk1.
