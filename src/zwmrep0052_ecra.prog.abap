*&---------------------------------------------------------------------*
*&  Include      ZWMREP0052_ECRA                                       *
*&---------------------------------------------------------------------*
************************************************************************
** Parâmetros de entrada
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-000.
PARAMETERS: p_lgnum LIKE ltak-lgnum DEFAULT '100' OBLIGATORY.
SELECT-OPTIONS: s_queue FOR ltak-queue,
                s_data  FOR sy-datum OBLIGATORY,
* Acrescentar hora de selecção: Ferreira 17.10.2006
                s_hora  FOR sy-uzeit OBLIGATORY,
* Fim hora de selecção
                s_ename FOR usr01-bname MATCHCODE OBJECT user_comp.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: s_bwlvs FOR ltak-bwlvs.
**
SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) text-p01.
PARAMETERS p_all RADIOBUTTON GROUP rel DEFAULT 'X'.
SELECTION-SCREEN COMMENT (13)  text-p05.
PARAMETERS p_cri RADIOBUTTON GROUP rel.
SELECTION-SCREEN COMMENT (13)  text-p02.
PARAMETERS p_pic RADIOBUTTON GROUP rel.
SELECTION-SCREEN COMMENT (13)  text-p03.
PARAMETERS p_fim RADIOBUTTON GROUP rel.
SELECTION-SCREEN COMMENT (13)  text-p04.

SELECTION-SCREEN END OF LINE.
**
SELECTION-SCREEN: END OF BLOCK blk1.
