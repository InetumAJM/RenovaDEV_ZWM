*&---------------------------------------------------------------------*
*&  Include           ZXLIDU03
*&---------------------------------------------------------------------*

** Armazém Automático Torres Novas (WCS)
**********************************************************************
IF i_lgnum = '100'.
  CALL FUNCTION 'ZWM_CREATE_IDOC_LIB_GROUP'
    EXPORTING
      i_lgnum         = i_lgnum
      i_refnr         = i_refnr
      i_addin         = i_addin
    CHANGING
      cs_idoc_control = x_idoc_control
      ct_ltap_vb      = t_ltap[]
      ct_idoc_data    = t_idoc_data[]
    EXCEPTIONS
      error           = 1
      OTHERS          = 2.

ENDIF.
