FUNCTION zwm_msg_log_wcs .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_OBJECT) TYPE  BALHDR-OBJECT
*"     REFERENCE(I_SUBOBJECT) TYPE  BALHDR-SUBOBJECT
*"     REFERENCE(I_EXTNUMBER) TYPE  BALNREXT OPTIONAL
*"     REFERENCE(I_STATE) TYPE  CHAR1
*"     REFERENCE(I_HEADER) TYPE  XFELD DEFAULT 'X'
*"     REFERENCE(I_LOG) TYPE  BALMI OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_MSG) TYPE  CHAR255
*"  TABLES
*"      T_LOG1 STRUCTURE  BAPIRET2 OPTIONAL
*"      T_LOG2 STRUCTURE  BDCMSGCOLL OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  CONSTANTS: lc_logcl1 LIKE balmi-probclass VALUE '1',
             lc_logcl2 LIKE balmi-probclass VALUE '2',
             lc_logcl3 LIKE balmi-probclass VALUE '3',
             lc_logcl4 LIKE balmi-probclass VALUE '4'.

** Tipos de mensagem
  CONSTANTS: lc_msgty_head LIKE syst-msgty  VALUE 'H',
             lc_msgty_succ LIKE syst-msgty  VALUE 'S',
             lc_msgty_info LIKE syst-msgty  VALUE 'I',
             lc_msgty_warn LIKE syst-msgty  VALUE 'W',
             lc_msgty_err  LIKE syst-msgty  VALUE 'E',
             lc_msgty_aben LIKE syst-msgty  VALUE 'A'.

  DATA: lv_msgtyp TYPE symsgty.
  DATA: lv_msgid  TYPE symsgid.
  DATA: lv_msgno  TYPE symsgno.
  DATA: lv_msgv1  TYPE symsgv.
  DATA: lv_msgv2  TYPE symsgv.
  DATA: lv_msgv3  TYPE symsgv.
  DATA: lv_msgv4  TYPE symsgv.
  DATA: lv_tabix  TYPE sy-tabix.

  DATA: ls_header LIKE balhdri.

  DATA: lt_msgs   TYPE balmi  OCCURS 0 WITH HEADER LINE.
  DATA: lt_balnri TYPE balnri OCCURS 0 WITH HEADER LINE.


** Inicialização
**********************************************************************
  IF i_state EQ 'I' OR i_state EQ 'A'.

    CALL FUNCTION 'APPL_LOG_INIT'
      EXPORTING
        object              = i_object
        subobject           = i_subobject
*       log_handle          = ' '
      EXCEPTIONS
        object_not_found    = 1
        subobject_not_found = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      RAISE error.
    ENDIF.

    CALL FUNCTION 'APPL_LOG_SET_OBJECT'
      EXPORTING
        object              = i_object
        subobject           = i_subobject
*       LOG_HANDLE          = ' '
*       NO_LOG              = ' '
      EXCEPTIONS
        object_not_found    = 1
        subobject_not_found = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      RAISE error.
    ENDIF.

** Cabeçalho
    IF i_header = 'X'.

      CLEAR ls_header.

      IF i_extnumber IS NOT INITIAL.
        ls_header-extnumber = i_extnumber.
      ELSE.
        ls_header-extnumber = sy-datum.
      ENDIF.

      ls_header-object      = i_object.
      ls_header-subobject   = i_subobject.

      ls_header-aldate      = sy-datlo.
      ls_header-altime      = sy-uzeit.
      ls_header-aluser      = sy-uname.
      ls_header-altcode     = sy-tcode.
      ls_header-alprog      = sy-cprog.
      ls_header-aldate_del  = sy-datum + 90. " Data de expiração
      ls_header-del_before  = 'X'.

      CALL FUNCTION 'APPL_LOG_WRITE_HEADER'
        EXPORTING
          header              = ls_header
        EXCEPTIONS
          object_not_found    = 1
          subobject_not_found = 2
          error               = 3
          OTHERS              = 4.

      IF sy-subrc <> 0.
        RAISE error.
      ENDIF.
    ENDIF.
  ENDIF.

** Mensagem
**********************************************************************
  IF i_state EQ 'M' OR
     i_state EQ 'T' OR
     i_state EQ 'A'.

** Log
    REFRESH: lt_msgs.

    IF i_log IS NOT INITIAL.
      CLEAR lt_msgs.
      lt_msgs-msgty = i_log-msgty.
      lt_msgs-msgid = i_log-msgid.
      lt_msgs-msgno = i_log-msgno.
      lt_msgs-msgv1 = i_log-msgv1.
      lt_msgs-msgv2 = i_log-msgv2.
      lt_msgs-msgv3 = i_log-msgv3.
      lt_msgs-msgv4 = i_log-msgv4.
      APPEND lt_msgs.

      " Escreve a mensagem de Log
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = lt_msgs-msgid
          msgnr               = lt_msgs-msgno
          msgv1               = lt_msgs-msgv1
          msgv2               = lt_msgs-msgv2
          msgv3               = lt_msgs-msgv3
          msgv4               = lt_msgs-msgv4
        IMPORTING
          message_text_output = e_msg.
    ENDIF.

    LOOP AT t_log1.
      CLEAR lt_msgs.
      lt_msgs-msgty = t_log1-type.
      lt_msgs-msgid = t_log1-id.
      lt_msgs-msgno = t_log1-number.
      lt_msgs-msgv1 = t_log1-message_v1.
      lt_msgs-msgv2 = t_log1-message_v2.
      lt_msgs-msgv3 = t_log1-message_v3.
      lt_msgs-msgv4 = t_log1-message_v4.
      APPEND lt_msgs.
    ENDLOOP.

    LOOP AT t_log2.
      CLEAR lt_msgs.
      lt_msgs-msgty = t_log2-msgtyp.
      lt_msgs-msgid = t_log2-msgid.
      lt_msgs-msgno = t_log2-msgnr.
      lt_msgs-msgv1 = t_log2-msgv1.
      lt_msgs-msgv2 = t_log2-msgv2.
      lt_msgs-msgv3 = t_log2-msgv3.
      lt_msgs-msgv4 = t_log2-msgv4.
      APPEND lt_msgs.
    ENDLOOP.

** Mensagens
    LOOP AT lt_msgs.
      lv_tabix = sy-tabix.

      CONDENSE lt_msgs-msgv1.
      CONDENSE lt_msgs-msgv2.
      CONDENSE lt_msgs-msgv3.
      CONDENSE lt_msgs-msgv4.

      CASE lv_msgtyp.
        WHEN lc_msgty_head.
          lt_msgs-probclass = lc_logcl2.
        WHEN lc_msgty_succ.
          lt_msgs-probclass = lc_logcl4.
        WHEN lc_msgty_info.
          lt_msgs-probclass = lc_logcl3.
        WHEN lc_msgty_warn.
          lt_msgs-probclass = lc_logcl2.
        WHEN lc_msgty_err.
          lt_msgs-probclass = lc_logcl1.
        WHEN lc_msgty_aben.
          lt_msgs-probclass = lc_logcl1.
      ENDCASE.

      lt_msgs-alsort   = 'GEN'.
      lt_msgs-detlevel = '1'.

      MODIFY lt_msgs INDEX lv_tabix.
    ENDLOOP.

    "Escreve Mensagem
    CALL FUNCTION 'APPL_LOG_WRITE_MESSAGES'
      EXPORTING
        object              = i_object
        subobject           = i_subobject
        update_or_insert    = 'I'
      TABLES
        messages            = lt_msgs
      EXCEPTIONS
        object_not_found    = 1
        subobject_not_found = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      RAISE error.
    ENDIF.
  ENDIF.

** Finalização
**********************************************************************
  IF i_state EQ 'F' OR
     i_state EQ 'T' OR
     i_state EQ 'A'.

    CALL FUNCTION 'APPL_LOG_WRITE_DB'
      EXPORTING
        object                = i_object
        subobject             = i_subobject
*       log_handle            = ' '
*       update_task           = ' '
      TABLES
        object_with_lognumber = lt_balnri
      EXCEPTIONS
        object_not_found      = 1
        subobject_not_found   = 2
        internal_error        = 3
        OTHERS                = 4.

    IF sy-subrc <> 0.
      RAISE error.
    ENDIF.
  ENDIF.

ENDFUNCTION.
