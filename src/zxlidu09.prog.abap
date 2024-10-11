*&---------------------------------------------------------------------*
*&  Include           ZXLIDU09
*&---------------------------------------------------------------------*

** WCS - Armazém Automático de Torres Novas
**********************************************************************
CALL FUNCTION 'ZWM_CHECK_ERROR_IDOC_TO_WCS'
  EXPORTING
    i_docnum     = i_docnum
    i_mestyp     = i_mestyp
  CHANGING
    c_returncode = x_returncode
    c_rollback   = x_flg_error_rollback.
