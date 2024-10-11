*&---------------------------------------------------------------------*
*& Report  Z1OMMP05_V4B
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z1ommp05_v4b.


INCLUDE z1ommp05_v4b_d01.
INCLUDE z1ommp05_v4b_f01.
INCLUDE z1ommp05_v4b_i01.
INCLUDE z1ommp05_v4b_o01.

INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
  PERFORM call_screen_0100.
