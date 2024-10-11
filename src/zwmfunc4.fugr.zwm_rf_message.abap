FUNCTION zwm_rf_message.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MESSAGE_ID)
*"     REFERENCE(I_MESSAGE_TYPE)
*"     REFERENCE(I_MESSAGE_NUMBER)
*"     REFERENCE(I_MESSAGE_VAR1) OPTIONAL
*"     REFERENCE(I_MESSAGE_VAR2) OPTIONAL
*"     REFERENCE(I_MESSAGE_VAR3) OPTIONAL
*"     REFERENCE(I_MESSAGE_VAR4) OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_RET_CODE) TYPE  FLAG
*"----------------------------------------------------------------------
  DATA: lv_message_id     TYPE  bdc_mid,
        lv_message_lang   TYPE  sylangu,
        lv_message_type   TYPE  bdc_mart,
        lv_message_number TYPE  bdc_mnr,
        lv_message_var1   TYPE  bdc_vtext1,
        lv_message_var2   TYPE  bdc_vtext1,
        lv_message_var3   TYPE  bdc_vtext1,
        lv_message_var4   TYPE  bdc_vtext1.

  CLEAR e_ret_code.

  lv_message_id     = i_message_id.
  lv_message_lang   = sy-langu.
  lv_message_type   = i_message_type.
  lv_message_number = i_message_number.
  lv_message_var1   = i_message_var1.
  lv_message_var2   = i_message_var2.
  lv_message_var3   = i_message_var3.
  lv_message_var4   = i_message_var4.


  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = lv_message_id
      message_lang   = lv_message_lang
      message_type   = lv_message_type
      message_number = lv_message_number
      message_var1   = lv_message_var1
      message_var2   = lv_message_var2
      message_var3   = lv_message_var3
      message_var4   = lv_message_var4
    IMPORTING
      ret_code       = e_ret_code.


ENDFUNCTION.
