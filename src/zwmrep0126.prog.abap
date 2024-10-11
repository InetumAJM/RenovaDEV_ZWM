*&---------------------------------------------------------------------*
*& Report  ZWMREP0126
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zwmrep0126 MESSAGE-ID zwm001.

INCLUDE zwmrep0126_d01.
INCLUDE zwmrep0126_d02.
INCLUDE zwmrep0126_f01.

INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
  PERFORM start_of_selection.
