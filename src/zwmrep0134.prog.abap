*&---------------------------------------------------------------------*
*& Report  ZWMREP0134
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zwmrep0134 MESSAGE-ID zwm001.

INCLUDE zwmrep0134_d01.

INCLUDE zwmrep0134_d02 .
INCLUDE zwmrep0134_o02 .
INCLUDE zwmrep0134_i02 .
INCLUDE zwmrep0134_f02 .


INCLUDE zwmrep0134_f01.
INCLUDE zwmrep0134_o01.
INCLUDE zwmrep0134_i01.

INITIALIZATION.
  PERFORM initialization.


START-OF-SELECTION.
  PERFORM start_of_selection.
