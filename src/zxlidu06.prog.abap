*&---------------------------------------------------------------------*
*&  Include           ZXLIDU06
*&---------------------------------------------------------------------*

** Estorno de OT de Armazém de Torres Novas (WCS)
**********************************************************************
IF i_lgnum = '100'.
  CALL FUNCTION 'ZWM_GET_IDOC_WMCATO_FROM_WCS'
    EXPORTING
      i_lgnum        = i_lgnum
      i_tanum        = i_tanum
    CHANGING
      ct_ltap_cancel = t_ltap_cancl
      ct_idoc_data   = t_idoc_data
    EXCEPTIONS
      error          = 1
      OTHERS         = 2.
ENDIF.
