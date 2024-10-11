FUNCTION ywm_rf_message_screen.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"----------------------------------------------------------------------
  DATA: ls_message TYPE bdcmsgcoll.

  LOOP AT it_messages INTO ls_message.
    IF ls_message-msgspra IS INITIAL.
      ls_message-msgspra = sy-langu.
    ENDIF.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = ls_message-msgid
        message_lang   = ls_message-msgspra
        message_type   = ls_message-msgtyp
        message_number = ls_message-msgnr
        message_var1   = ls_message-msgv1
        message_var2   = ls_message-msgv2
        message_var3   = ls_message-msgv3
        message_var4   = ls_message-msgv4.
  ENDLOOP.
ENDFUNCTION.
