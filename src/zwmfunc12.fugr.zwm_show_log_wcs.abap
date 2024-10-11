FUNCTION zwm_show_log_wcs.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_OBJECT) TYPE  BALHDR-OBJECT
*"     REFERENCE(I_SUBOBJECT) TYPE  BALHDR-SUBOBJECT
*"     REFERENCE(I_EXTNUMBER) TYPE  BALNREXT
*"     REFERENCE(I_DATE_I) TYPE  DATUM
*"     REFERENCE(I_TIME_I) TYPE  UZEIT
*"     REFERENCE(I_DATE_E) TYPE  DATUM OPTIONAL
*"     REFERENCE(I_TIME_E) TYPE  UZEIT OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_NUM_LOGS) TYPE  SY-DBCNT
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lv_count LIKE sy-dbcnt.

** Show LOG
**********************************************************************
  CALL FUNCTION 'APPL_LOG_DISPLAY'
    EXPORTING
      object                    = i_object
      subobject                 = i_subobject
      external_number           = i_extnumber
*     OBJECT_ATTRIBUTE          = 0
*     SUBOBJECT_ATTRIBUTE       = 0
*     EXTERNAL_NUMBER_ATTRIBUTE = 0
      date_from                 = i_date_i
      time_from                 = i_time_i
      date_to                   = i_date_e
      time_to                   = i_time_e
*     TITLE_SELECTION_SCREEN    = ' '
      title_list_screen         = 'Log Mensagens WCS'
*     COLUMN_SELECTION          = '11112221122   '
      suppress_selection_dialog = 'X'
*     COLUMN_SELECTION_MSG_JUMP = '1'
*     EXTERNAL_NUMBER_DISPLAY_LENGTH       = 20
*     I_S_DISPLAY_PROFILE       =
*     I_VARIANT_REPORT          = ' '
    IMPORTING
      number_of_protocols       = e_num_logs
    EXCEPTIONS
      no_authority              = 1
      OTHERS                    = 2.



ENDFUNCTION.
