*&---------------------------------------------------------------------*
*& Report  ZWM_LISTA_STOCK_ROLOS_BOB
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zwm_lista_stock_rolos_bob.

INCLUDE zwm_lista_stock_rolos_bob_top.
INCLUDE zwm_lista_stock_rolos_bob_scr.
INCLUDE zwm_lista_stock_rolos_bob_f01.


START-OF-SELECTION.
  PERFORM get_data.
  PERFORM treat_data.

END-OF-SELECTION.
  PERFORM display_data.
