FUNCTION ZWM_TR_CREATE .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_UPDATE_TASK) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_COMMIT_WORK) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_SINGLE_ITEM) TYPE  FLAG OPTIONAL
*"     REFERENCE(IT_LTBA) TYPE  ZWM_TT_LTBA
*"  EXPORTING
*"     REFERENCE(E_TBNUM) TYPE  TBNUM
*"     REFERENCE(ET_LTBA) TYPE  ZWM_TT_LTBA
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lt_ltba TYPE zwm_tt_ltba.

  DATA: ls_ltba TYPE ltba.

  DATA: ls_message TYPE bdcmsgcoll.

  lt_ltba = it_ltba.

  CLEAR et_ltba.

  CALL FUNCTION 'L_TR_CREATE'
    EXPORTING
      i_single_item         = i_single_item
      i_save_only_all       = abap_true
      i_update_task         = i_update_task
      i_commit_work         = i_commit_work
    TABLES
      t_ltba                = lt_ltba
    EXCEPTIONS
      item_error            = 1
      no_entry_in_int_table = 2
      item_without_number   = 3
      no_update_item_error  = 4
      OTHERS                = 5.

  IF sy-subrc <> 0.
    CLEAR: ls_message.
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = sy-msgid.
    ls_message-msgnr  = sy-msgno.
    ls_message-msgv1  = sy-msgv1.
    ls_message-msgv2  = sy-msgv2.
    ls_message-msgv3  = sy-msgv3.
    ls_message-msgv4  = sy-msgv4.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  et_ltba = lt_ltba.

  CLEAR: ls_ltba.
  READ TABLE lt_ltba
        INTO ls_ltba
        INDEX 1.

  e_tbnum = ls_ltba-tbnum.
ENDFUNCTION.
