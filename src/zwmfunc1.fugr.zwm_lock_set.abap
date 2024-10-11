FUNCTION zwm_lock_set.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_KEYWORD) TYPE  KEYWORDS
*"     REFERENCE(I_WAIT) TYPE  INT4 DEFAULT 1
*"     REFERENCE(I_SCOPE) TYPE  C DEFAULT '1'
*"  EXPORTING
*"     REFERENCE(ES_KEYWORD) TYPE  KEYWORD
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lv_keyword TYPE keywords,
        lv_locked  TYPE flag,
        lv_times   TYPE i,
        lv_name    TYPE char50,
        ls_message TYPE bdcmsgcoll.

***********************************************************************
  CLEAR: et_messages, es_keyword.


** Chave de Bloqueio
***********************************************************************
  es_keyword-keyword = i_keyword.

** Verifica se já Existe Bloqueio e Bloqueia se Livre
***********************************************************************
  lv_times = i_wait + 1.

  DO lv_times TIMES.
    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = i_keyword
        x_datum        = 'X'
        x_zeit         = 'X'
        x_pos          = 'X'
        _scope         = i_scope
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc EQ 0.
      lv_locked = 'X'.
      EXIT.
    ENDIF.
  ENDDO.


  IF lv_locked IS INITIAL.
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = sy-msgid.
    ls_message-msgnr = sy-msgno.
    ls_message-msgv1 = sy-msgv1.
    ls_message-msgv2 = sy-msgv2.
    ls_message-msgv3 = sy-msgv3.
    ls_message-msgv4 = sy-msgv4.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.


ENDFUNCTION.
