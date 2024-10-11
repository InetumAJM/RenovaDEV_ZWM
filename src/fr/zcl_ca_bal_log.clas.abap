class ZCL_CA_BAL_LOG definition
  public
  final
  create public .

*"* public components of class ZCL_CA_BAL_LOG
*"* do not include other source files here!!!
public section.
  type-pools ABAP .

  constants C_MSGTY_W type SYMSGTY value 'W'. "#EC NOTEXT
  data MT_LOGMSG type BAL_T_MSG read-only .
  data MV_LOG_EXTNO type BALNREXT read-only .
  data MV_LOG_HNDL type BALLOGHNDL read-only .
  data MV_LOG_NO type BALOGNR read-only .

  methods CONSTRUCTOR
    importing
      !I_LOG_OBJ type BALOBJ_D
      !I_LOG_SOBJ type BALSUBOBJ
      !I_LOGNO type BALOGNR optional
      !I_EXTNO type BALNREXT optional .
  methods LOG_ADD_MESSAGE
    importing
      !I_MSGTY type SYMSGTY default SY-MSGTY
      !I_MSGID type SYMSGID default SY-MSGID
      !I_MSGNO type SYMSGNO default SY-MSGNO
      !I_MSGV1 type SYMSGV default SY-MSGV1
      !I_MSGV2 type SYMSGV default SY-MSGV2
      !I_MSGV3 type SYMSGV default SY-MSGV3
      !I_MSGV4 type SYMSGV default SY-MSGV4 .
  methods LOG_SAVE
    returning
      value(R_LOG_NO) type BALOGNR .
  methods LOG_DISPLAY
    importing
      !IT_HANDLE type BAL_T_LOGH optional
      !IT_LOGNUM type BAL_T_LOGN optional .
protected section.
*"* protected components of class ZCL_CA_BAL_LOG
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_CA_BAL_LOG
*"* do not include other source files here!!!

  data MV_LOG_OBJ type BALOBJ_D .
  data MV_LOG_SOBJ type BALSUBOBJ .
ENDCLASS.



CLASS ZCL_CA_BAL_LOG IMPLEMENTATION.


METHOD constructor.
  DATA ls_log TYPE bal_s_log.

  DATA lt_lognumber   TYPE bal_t_logn.
  DATA lt_loghandle   TYPE bal_t_logh.

  FIELD-SYMBOLS <fs_handle> LIKE LINE OF lt_loghandle[].

  CLEAR me->mv_log_obj.
  CLEAR me->mv_log_sobj.
  CLEAR me->mv_log_hndl.
  CLEAR me->mv_log_extno.

  me->mv_log_obj      = i_log_obj.
  me->mv_log_sobj     = i_log_sobj.

  IF i_extno IS SUPPLIED.
    me->mv_log_extno  = i_extno.
  ENDIF.

  IF i_logno IS SUPPLIED.
    APPEND i_logno TO lt_lognumber[].

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_lognumber      = lt_lognumber[]
      IMPORTING
        e_t_log_handle     = lt_loghandle[]
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    READ TABLE lt_loghandle[] ASSIGNING <fs_handle> INDEX 1.
    IF sy-subrc EQ 0.
      me->mv_log_hndl = <fs_handle>.
    ENDIF.
  ELSE.
    GET TIME.
    ls_log-object     = me->mv_log_obj.
    ls_log-subobject  = me->mv_log_sobj.
    ls_log-extnumber  = me->mv_log_extno.
    ls_log-aldate     = sy-datum.
    ls_log-altime     = sy-uzeit.
    ls_log-aluser     = sy-uname.
    ls_log-altcode    = sy-tcode.
    ls_log-almode     = 'D'.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = ls_log
      IMPORTING
        e_log_handle = me->mv_log_hndl.
  ENDIF.
ENDMETHOD.


METHOD log_add_message.
  DATA ls_msg TYPE bal_s_msg.

  ls_msg-msgty = i_msgty.
  ls_msg-msgid = i_msgid.
  ls_msg-msgno = i_msgno.
  ls_msg-msgv1 = i_msgv1.
  ls_msg-msgv2 = i_msgv2.
  ls_msg-msgv3 = i_msgv3.
  ls_msg-msgv4 = i_msgv4.

  IF i_msgty EQ c_msgty_w.
    ls_msg-probclass  = 4.
  ENDIF.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = me->mv_log_hndl
      i_s_msg      = ls_msg.

  INSERT ls_msg INTO TABLE me->mt_logmsg[].
ENDMETHOD.


METHOD log_display.
  DATA ls_profile TYPE bal_s_prof.

  DATA lt_handle  TYPE bal_t_logh.

  FIELD-SYMBOLS <fs_handle> LIKE LINE OF lt_handle[].

  ls_profile-show_all   = abap_true.
  ls_profile-use_grid   = abap_true.
  ls_profile-langu      = sy-langu.
  ls_profile-cwidth_opt = abap_true.

  IF it_handle[] IS NOT SUPPLIED AND it_lognum[] IS NOT SUPPLIED.
    RETURN.
  ELSEIF it_handle[] IS NOT SUPPLIED AND it_lognum[] IS SUPPLIED.
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_lognumber      = it_lognum[]
      IMPORTING
        e_t_log_handle     = lt_handle[]
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
  ELSE.
    lt_handle[] = it_handle[].
  ENDIF.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile  = ls_profile
      i_t_log_handle       = lt_handle[]
    EXCEPTIONS
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4
      OTHERS               = 5.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  LOOP AT lt_handle[] ASSIGNING <fs_handle>.
    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING
        i_log_handle  = <fs_handle>
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
  ENDLOOP.
ENDMETHOD.


METHOD log_save.
  DATA lt_log TYPE STANDARD TABLE OF balnri.

  FIELD-SYMBOLS <fs_log> LIKE LINE OF lt_log[].

  CALL FUNCTION 'APPL_LOG_WRITE_DB'
    EXPORTING
      object                = me->mv_log_obj
      subobject             = me->mv_log_sobj
      log_handle            = me->mv_log_hndl
    TABLES
      object_with_lognumber = lt_log[]
    EXCEPTIONS
      object_not_found      = 1
      subobject_not_found   = 2
      internal_error        = 3
      OTHERS                = 4.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.
  READ TABLE lt_log[] ASSIGNING <fs_log> INDEX 1.
  IF sy-subrc EQ 0.
    me->mv_log_no = <fs_log>-lognumber.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.

  r_log_no  = me->mv_log_no.
ENDMETHOD.
ENDCLASS.
