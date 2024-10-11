FUNCTION zwm_sscc_generate.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_LGORT) TYPE  LGORT_D OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_EXIDV) TYPE  EXIDV
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------


  DATA: ls_message TYPE bdcmsgcoll.

  DATA: lv_lgnum TYPE lgnum,
        lv_exidv TYPE exidv.


  CLEAR: e_exidv, et_messages.

  lv_lgnum = i_lgnum.


  CALL FUNCTION 'LE_SSCC_GENERATE'
    EXPORTING
      if_werk               = i_werks
      if_lgort              = i_lgort
      if_lgnum              = lv_lgnum
      if_huart              = '3'
    IMPORTING
      ef_sscc               = e_exidv
    EXCEPTIONS
      iln_not_found         = 1
      invalid_call          = 2
      invalid_customizing   = 3
      internal_error        = 4
      error_on_number_range = 5
      OTHERS                = 6.

  IF sy-subrc <> 0.
    ls_message-msgid  = sy-msgid.
    ls_message-msgtyp = sy-msgty.
    ls_message-msgnr  = sy-msgno.
    ls_message-msgv1  = sy-msgv1.
    ls_message-msgv2  = sy-msgv2.
    ls_message-msgv3  = sy-msgv3.
    ls_message-msgv4  = sy-msgv4.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = e_exidv
    IMPORTING
      output = e_exidv.

** Valida se Numeração Já Existe
***********************************************************************
  SELECT SINGLE exidv FROM vekp
                      INTO lv_exidv
                      WHERE exidv = e_exidv.

  IF sy-subrc EQ 0.
**  Choque na numeração se SSCC, & já existe
    ls_message-msgid  = 'ZRF001'.
    ls_message-msgtyp = 'E'.
    ls_message-msgnr  = '332'.
    ls_message-msgv1  = e_exidv.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.


ENDFUNCTION.
