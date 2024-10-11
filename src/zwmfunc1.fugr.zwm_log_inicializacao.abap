FUNCTION zwm_log_inicializacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(OBJECT) LIKE  BALHDR-OBJECT DEFAULT SPACE
*"     REFERENCE(SUBOBJECT) LIKE  BALHDR-SUBOBJECT DEFAULT SPACE
*"     VALUE(INICIALIZACAO) TYPE  CHAR1 DEFAULT 'X'
*"     VALUE(CABECALHO) TYPE  CHAR1 DEFAULT 'X'
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: l_header LIKE balhdri.

  REFRESH: return_msg.
  CLEAR:   return_msg, l_header.

** Inicialização
  IF inicializacao EQ 'X'.

    REFRESH: it_msgs.
    CLEAR:   it_msgs.

    CALL FUNCTION 'APPL_LOG_INIT'
      EXPORTING
        object              = object
        subobject           = subobject
*       log_handle          = ' '
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

    CALL FUNCTION 'APPL_LOG_SET_OBJECT'
      EXPORTING
        object                    = object
        subobject                 = subobject
*       LOG_HANDLE                = ' '
*       NO_LOG                    = ' '
      EXCEPTIONS
        object_not_found          = 1
        subobject_not_found       = 2
        OTHERS                    = 3.

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

  ENDIF.

** Cabeçalho
  IF cabecalho EQ 'X'.

    l_header-object    = object.
    l_header-subobject = subobject.
    l_header-extnumber = sy-datum.
    l_header-aldate    = sy-datlo.
    l_header-altime    = sy-uzeit.
    l_header-aluser    = sy-uname.
    l_header-altcode   = sy-tcode.
    l_header-alprog    = sy-cprog.

    CALL FUNCTION 'APPL_LOG_WRITE_HEADER'
      EXPORTING
        header                    = l_header
*       LOG_HANDLE                =
*     IMPORTING
*       update_or_insert          =
*       e_log_handle              =
      EXCEPTIONS
        object_not_found          = 1
        subobject_not_found       = 2
        error                     = 3
        OTHERS                    = 4.

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

  ENDIF.

ENDFUNCTION.
