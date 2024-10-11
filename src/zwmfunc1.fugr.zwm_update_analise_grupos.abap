FUNCTION zwm_update_analise_grupos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LTAK-LGNUM
*"     REFERENCE(I_REFNR) TYPE  LTAK-REFNR
*"----------------------------------------------------------------------

  DATA: lt_rspar TYPE TABLE OF rsparams.

  DATA: ls_rspar   TYPE rsparams,
        ls_t311    TYPE t311.


  SELECT SINGLE * FROM t311
                   INTO ls_t311
                   WHERE lgnum = i_lgnum AND
                         refnr = i_refnr.


  ls_rspar-selname = 'LGNUM'.
  ls_rspar-kind = 'P'.
  ls_rspar-sign = 'I'.
  ls_rspar-option = 'EQ'.
  ls_rspar-low = i_lgnum.
  APPEND ls_rspar TO lt_rspar.
  CLEAR ls_rspar.

  ls_rspar-selname = 'REFNR'.
  ls_rspar-kind = 'S'.
  ls_rspar-sign = 'I'.
  ls_rspar-option = 'EQ'.
  ls_rspar-low = i_refnr.
  APPEND ls_rspar TO lt_rspar.
  CLEAR ls_rspar.

  ls_rspar-selname = 'DATUM'.
  ls_rspar-kind = 'S'.
  ls_rspar-sign = 'I'.
  ls_rspar-option = 'EQ'.
  ls_rspar-low = ls_t311-datum.
  APPEND ls_rspar TO lt_rspar.
  CLEAR ls_rspar.

  ls_rspar-selname = 'ANZEIGE'.
  ls_rspar-kind = 'P'.
  ls_rspar-sign = 'I'.
  ls_rspar-option = 'EQ'.
  ls_rspar-low = ' '.
  APPEND ls_rspar TO lt_rspar.
  CLEAR ls_rspar.

  SUBMIT rl2stk00 WITH SELECTION-TABLE lt_rspar AND RETURN.
ENDFUNCTION.
