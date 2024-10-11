*&---------------------------------------------------------------------*
*& Report  ZWMREP0083
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zwmrep0083 MESSAGE-ID zwmmsg001.

INCLUDE: zwmrep0083top,
         zwmrep0083f01.



** Initialization
************************************************************************
INITIALIZATION.
  PERFORM inicializacao.


** AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN ON p_lgnum.
  PERFORM check_p_lgnum.

AT SELECTION-SCREEN on BLOCK b2.
  PERFORM check_bloco.

** START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  CHECK gv_error IS INITIAL.

  PERFORM get_data.

************************************************************************
END-OF-SELECTION.

  CHECK gv_error IS INITIAL.


  PERFORM imprime_lista.
