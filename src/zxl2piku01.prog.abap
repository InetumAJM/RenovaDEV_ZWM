*&---------------------------------------------------------------------*
*&  Include           ZXL2PIKU01
*&---------------------------------------------------------------------*

CALL FUNCTION 'ZWM_EXIT_SAPLL2PIK_001'
  EXPORTING
    i_t311    = i_t311
  TABLES
    t_tritem  = t_tritem
    t_delitem = t_delitem.
