*&---------------------------------------------------------------------*
*& Report  ZWMREP0135
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zwmrep0135 MESSAGE-ID zmsg.

INCLUDE zwmrep0135_d01.
INCLUDE zwmrep0135_d02.
INCLUDE zwmrep0135_f01.

INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
  PERFORM start_of_selection.
