*&---------------------------------------------------------------------*
*&  Include           ZXLIDU19
*&---------------------------------------------------------------------*

IF x_lgnum = '100'.
  CALL FUNCTION 'ZWM_GET_IDOC_WMTORD_M_FROM_WCS'
    EXPORTING
      i_lgnum        = x_lgnum
      i_idoc_control = i_idoc_control
    CHANGING
      ct_idoc_data   = t_idoc_data[]
      ct_ltori       = t_ltori[]
      c_lznum        = x_lznum
    EXCEPTIONS
      error          = 1
      OTHERS         = 2.
ENDIF.
