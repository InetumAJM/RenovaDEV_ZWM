*&---------------------------------------------------------------------*
*& Report  ZWMHU
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zwmhu MESSAGE-ID zwm001.

DATA: lv_lgnum TYPE lgnum,
      lv_exidv TYPE exidv,
      lv_rc    TYPE i,
      lt_data  TYPE TABLE OF char20,
      lv_werks TYPE werks_d,
      lv_lgort TYPE lgort_d.


CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
  EXPORTING
    i_userf     = 'X'
    i_usemm     = 'X'
    i_useaut    = 'X'
    i_get_lgnum = 'X'
    i_get_werks = ''
    i_get_lgort = ''
  CHANGING
    c_lgnum     = lv_lgnum
    c_werks     = lv_werks
  EXCEPTIONS
    error       = 1
    user_back   = 2
    OTHERS      = 3.

CHECK sy-subrc EQ 0.


IF lv_werks IS INITIAL.
  lv_werks = 'RENV'.
ENDIF.

** Gera Nº de SU
***********************************************************************
CALL FUNCTION 'LE_SSCC_GENERATE'
  EXPORTING
    if_werk               = lv_werks
    if_lgnum              = lv_lgnum
    if_huart              = '3'
  IMPORTING
    ef_sscc               = lv_exidv
  EXCEPTIONS
    iln_not_found         = 1
    invalid_call          = 2
    invalid_customizing   = 3
    internal_error        = 4
    error_on_number_range = 5
    OTHERS                = 6.

IF sy-subrc <> 0.
  EXIT.
ENDIF.


CHECK sy-subrc EQ 0.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    input  = lv_exidv
  IMPORTING
    output = lv_exidv.



** Exporta Nº para Memória
***********************************************************************
APPEND lv_exidv TO lt_data.

CALL METHOD cl_gui_frontend_services=>clipboard_export
  IMPORTING
    data                 = lt_data
  CHANGING
    rc                   = lv_rc
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    not_supported_by_gui = 3
    OTHERS               = 4.


** Retorno
***********************************************************************
MESSAGE s000 WITH lv_exidv.
