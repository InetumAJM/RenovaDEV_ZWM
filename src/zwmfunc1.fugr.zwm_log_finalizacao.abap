FUNCTION zwm_log_finalizacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(OBJECT) LIKE  BALHDR-OBJECT DEFAULT SPACE
*"     REFERENCE(SUBOBJECT) LIKE  BALHDR-SUBOBJECT DEFAULT SPACE
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: lt_balnri LIKE balnri OCCURS 0 WITH HEADER LINE.

  REFRESH: return_msg, lt_balnri.
  CLEAR:   return_msg, lt_balnri.

** Escreve Mensagens
  CALL FUNCTION 'APPL_LOG_WRITE_MESSAGES'
    EXPORTING
      object              = object
      subobject           = subobject
      update_or_insert    = 'I'
    TABLES
      messages            = it_msgs
    EXCEPTIONS
      object_not_found    = 1
      subobject_not_found = 2
      OTHERS              = 3.

  IF sy-subrc <> 0.
    return_msg-msgtyp = sy-msgty.
    return_msg-msgid  = sy-msgid.
    return_msg-msgnr  = sy-msgno.
    return_msg-msgv1  = sy-msgv1.
    return_msg-msgv2  = sy-msgv2.
    return_msg-msgv3  = sy-msgv3.
    return_msg-msgv4  = sy-msgv4.
    APPEND return_msg.
    CLEAR  return_msg.
    RAISE error.
  ENDIF.

  REFRESH: it_msgs.
  CLEAR:   it_msgs.

** Grava BD
  CALL FUNCTION 'APPL_LOG_WRITE_DB'
    EXPORTING
      object                = object
      subobject             = subobject
*     log_handle            = ' '
*     update_task           = ' '
    TABLES
      object_with_lognumber = lt_balnri
    EXCEPTIONS
      object_not_found      = 1
      subobject_not_found   = 2
      internal_error        = 3
      OTHERS                = 4.

  IF sy-subrc <> 0.
    return_msg-msgtyp = sy-msgty.
    return_msg-msgid  = sy-msgid.
    return_msg-msgnr  = sy-msgno.
    return_msg-msgv1  = sy-msgv1.
    return_msg-msgv2  = sy-msgv2.
    return_msg-msgv3  = sy-msgv3.
    return_msg-msgv4  = sy-msgv4.
    APPEND return_msg.
    CLEAR  return_msg.
    RAISE error.
  ENDIF.

ENDFUNCTION.
