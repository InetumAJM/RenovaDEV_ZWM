*&---------------------------------------------------------------------*
*&  Include           ZXLIDU12
*&---------------------------------------------------------------------*

** Armazém Automático WCS
**********************************************************************
IF i_lgnum = '100'.

  CALL FUNCTION 'ZWM_GET_IDOC_WMTORD_FROM_WCS'
    EXPORTING
      i_lgnum         = i_lgnum
    CHANGING
      c_lznum         = x_lznum
      cs_idoc_control = i_idoc_control
      cs_ltap_cr      = x_ltap_cr
      ct_idoc_data    = t_idoc_data[]
    EXCEPTIONS
      error           = 1
      OTHERS          = 2.

ENDIF.
