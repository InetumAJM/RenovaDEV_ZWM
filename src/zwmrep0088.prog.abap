*&---------------------------------------------------------------------*
*& Report  ZWMREP0088
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zwmrep0088.

** INCLUDES
***********************************************************************
INCLUDE zwmrep0088_d01. " Declaração de Dados Globais
INCLUDE zwmrep0088_i01. " Modulos PAI
INCLUDE zwmrep0088_o01. " Modulos PBO
INCLUDE zwmrep0088_f01. " Rotinas

** INITIALIZATION
***********************************************************************
INITIALIZATION.
  PERFORM initialization.

** START OF SELECTION
***********************************************************************
START-OF-SELECTION.
  PERFORM call_screen_0001.
