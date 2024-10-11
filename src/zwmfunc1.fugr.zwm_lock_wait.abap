FUNCTION zwm_lock_wait.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_KEYWORD) TYPE  KEYWORD OPTIONAL
*"     REFERENCE(I_GARG) TYPE  EQEGRAARG OPTIONAL
*"     REFERENCE(I_MANDT) TYPE  MANDT DEFAULT SY-MANDT
*"     REFERENCE(I_ELEMENT1) OPTIONAL
*"     REFERENCE(I_ELEMENT2) OPTIONAL
*"     REFERENCE(I_ELEMENT3) OPTIONAL
*"     REFERENCE(I_GNAME) TYPE  EQEGRANAME DEFAULT 'KEYWORD'
*"     REFERENCE(I_WAIT) TYPE  INT4 DEFAULT 10
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"     REFERENCE(E_UNAME) TYPE  UNAME
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: lt_enq     TYPE TABLE OF seqg3,
        ls_enq     TYPE seqg3,
        lv_guname  LIKE sy-uname,
        lv_garg    TYPE eqegraarg,
        lv_times   TYPE i VALUE 1,
        ls_message TYPE bdcmsgcoll.

***********************************************************************
  CLEAR: et_messages, e_uname.


** Chave de Bloqueio
***********************************************************************
  IF NOT is_keyword IS INITIAL.
**  Chave KEYWORD
    lv_garg = is_keyword.

  ELSEIF NOT i_garg IS INITIAL.
**  Chave Standard Indicada Directamente
    lv_garg = i_garg.

  ELSE.
**  Constroi Chave de Bloqueio
    CONCATENATE i_mandt lv_garg i_element1 i_element2 i_element3
           INTO lv_garg.

    CONDENSE lv_garg NO-GAPS.
  ENDIF.

  CHECK NOT lv_garg IS INITIAL.


** Aguarda Libertar do Bloqueio
***********************************************************************
  IF NOT i_wait IS INITIAL.
    lv_times = i_wait + 1.
  ENDIF.

  DO lv_times TIMES.
    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CALL FUNCTION 'ENQUE_READ'
      EXPORTING
        gname  = i_gname
        garg   = lv_garg
        guname = lv_guname
      TABLES
        enq    = lt_enq.

    CHECK lt_enq IS INITIAL.
    EXIT.
  ENDDO.


** Retorno
***********************************************************************
  CHECK NOT lt_enq IS INITIAL.

  READ TABLE lt_enq INTO ls_enq INDEX 1.

  e_uname = ls_enq-guname.

** Processo Bloqueado por &
  ls_message-msgtyp  = 'E'.
  ls_message-msgid   = 'ZWMSG001'.
  ls_message-msgnr   = '004'.
  ls_message-msgspra = sy-langu.
  ls_message-msgv1   = ls_enq-guname.
  APPEND ls_message TO et_messages.
  RAISE error.
ENDFUNCTION.
