FUNCTION zwm_log_mensagem.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(OBJECT) LIKE  BALHDR-OBJECT DEFAULT SPACE
*"     REFERENCE(SUBOBJECT) LIKE  BALHDR-SUBOBJECT DEFAULT SPACE
*"     VALUE(MSGTYP) TYPE  SYMSGTY OPTIONAL
*"     VALUE(MSGID) TYPE  SYMSGID OPTIONAL
*"     VALUE(MSGNO) TYPE  SYMSGNO OPTIONAL
*"     VALUE(MSGV1) TYPE  SYMSGV OPTIONAL
*"     VALUE(MSGV2) TYPE  SYMSGV OPTIONAL
*"     VALUE(MSGV3) TYPE  SYMSGV OPTIONAL
*"     VALUE(MSGV4) TYPE  SYMSGV OPTIONAL
*"     VALUE(MSGDT) TYPE  BALLEVEL DEFAULT 1
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  CONSTANTS: k_logcl1 LIKE balmi-probclass VALUE '1',
             k_logcl2 LIKE balmi-probclass VALUE '2',
             k_logcl3 LIKE balmi-probclass VALUE '3',
             k_logcl4 LIKE balmi-probclass VALUE '4'.

  IF msgid IS INITIAL.
    msgid = 'ZWMMSG001'.
    msgno = '000'.
  ENDIF.

  it_msgs-msgty     = msgtyp.
  it_msgs-msgid     = msgid.
  it_msgs-msgno     = msgno.
  it_msgs-msgv1     = msgv1.
  CONDENSE it_msgs-msgv1.
  it_msgs-msgv2     = msgv2.
  CONDENSE it_msgs-msgv2.
  it_msgs-msgv3     = msgv3.
  CONDENSE it_msgs-msgv3.
  it_msgs-msgv4     = msgv4.
  CONDENSE it_msgs-msgv4.

  CASE msgtyp.
    WHEN k_msgty_head.
      it_msgs-probclass = k_logcl2.
    WHEN k_msgty_succ.
      it_msgs-probclass = k_logcl4.
    WHEN k_msgty_info.
      it_msgs-probclass = k_logcl3.
    WHEN k_msgty_warn.
      it_msgs-probclass = k_logcl2.
    WHEN k_msgty_err.
      it_msgs-probclass = k_logcl1.
    WHEN k_msgty_aben.
      it_msgs-probclass = k_logcl1.
  ENDCASE.

  it_msgs-alsort   = 'GEN'.
  it_msgs-detlevel = msgdt.
  APPEND it_msgs.
  CLEAR  it_msgs.

ENDFUNCTION.
