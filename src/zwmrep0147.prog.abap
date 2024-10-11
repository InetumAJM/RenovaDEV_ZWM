*&---------------------------------------------------------------------*
*& Report  ZWMREP0147
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zwmrep0147 MESSAGE-ID zwm001.

INCLUDE zwmrep0147_d01.
INCLUDE zwmrep0147_d02.
INCLUDE zwmrep0147_c01.
INCLUDE zwmrep0147_f01.
INCLUDE zwmrep0147_o01.
INCLUDE zwmrep0147_i01.

INITIALIZATION.
  PERFORM initialization.

** Validações
************************************************************************
AT SELECTION-SCREEN ON p_lgnum.
  PERFORM check_lgnum.


START-OF-SELECTION.
  PERFORM start_of_selection.
