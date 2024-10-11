FUNCTION zwm_log_message.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MASTER) OPTIONAL
*"     REFERENCE(I_OBJECT) TYPE  BALOBJ_D
*"     REFERENCE(I_SUBOBJECT) TYPE  BALSUBOBJ
*"     REFERENCE(I_EXTNUMBER) TYPE  ANY OPTIONAL
*"     REFERENCE(I_EXPIRE_DAYS) TYPE  INT4 OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(IT_MESSAGES) TYPE  TAB_BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  DATA: lt_messages TYPE TABLE OF bdcmsgcoll,
        lt_handle   TYPE bal_t_logh.

  DATA: ls_log     TYPE bal_s_log,
        ls_bal_msg TYPE bal_s_msg,
        ls_message TYPE bdcmsgcoll.

  DATA: lv_handle TYPE balloghndl,
        lv_log_on TYPE flag.

***********************************************************************
  IF NOT it_messages IS INITIAL.
    lt_messages = it_messages.
  ELSE.
**  Invocação sem Mensagens
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = 'ZWM001'.
    ls_message-msgnr  = '015'.
    APPEND ls_message TO lt_messages.
  ENDIF.

** Inicialização
***********************************************************************
  GET TIME.
  ls_log-object    = i_object.
  ls_log-subobject = i_subobject.

** Identificação Externa
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = i_extnumber
    IMPORTING
      output = ls_log-extnumber.

  IF NOT i_master IS INITIAL.
    CONCATENATE i_master ls_log-extnumber INTO ls_log-extnumber.
  ENDIF.

  CONDENSE ls_log-extnumber NO-GAPS.

  IF NOT i_expire_days IS INITIAL.
    ls_log-aldate_del = sy-datum + i_expire_days.
    ls_log-del_before = 'X'.
  ENDIF.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = ls_log
    IMPORTING
      e_log_handle            = lv_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.

  CHECK sy-subrc EQ 0 AND NOT lv_handle IS INITIAL.


** Adiciona as Mensagens
***********************************************************************
  LOOP AT lt_messages INTO ls_message.

**  Alinha inicialmente à esquerda (caso contrário corta)
    SHIFT:  ls_message-msgv1 LEFT DELETING LEADING space,
            ls_message-msgv2 LEFT DELETING LEADING space,
            ls_message-msgv3 LEFT DELETING LEADING space,
            ls_message-msgv4 LEFT DELETING LEADING space.

    WRITE: ls_message-msgv1 TO ls_bal_msg-msgv1 LEFT-JUSTIFIED,
           ls_message-msgv2 TO ls_bal_msg-msgv2 LEFT-JUSTIFIED,
           ls_message-msgv3 TO ls_bal_msg-msgv3 LEFT-JUSTIFIED,
           ls_message-msgv4 TO ls_bal_msg-msgv4 LEFT-JUSTIFIED.

    ls_bal_msg-msgty = ls_message-msgtyp.
    ls_bal_msg-msgid = ls_message-msgid.
    ls_bal_msg-msgno = ls_message-msgnr.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = lv_handle
        i_s_msg          = ls_bal_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    CLEAR: ls_bal_msg.
  ENDLOOP.


** Grava DD
***********************************************************************
  APPEND lv_handle TO lt_handle.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle   = lt_handle
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.

** Reinicializa
  CALL FUNCTION 'BAL_LOG_REFRESH'
    EXPORTING
      i_log_handle = lv_handle.


** Commit
***********************************************************************
  IF NOT i_commit IS INITIAL.
    COMMIT WORK.
  ENDIF.


ENDFUNCTION.
