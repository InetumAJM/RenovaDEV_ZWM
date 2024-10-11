FUNCTION Z_WM_DEQUEUE_WAIT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_GARG) TYPE  EQEGRAARG
*"     REFERENCE(I_MANDT) TYPE  MANDT DEFAULT SY-MANDT
*"     REFERENCE(I_ELEMENT1) OPTIONAL
*"     REFERENCE(I_ELEMENT2) OPTIONAL
*"     REFERENCE(I_ELEMENT3) OPTIONAL
*"     REFERENCE(I_GNAME) TYPE  EQEGRANAME OPTIONAL
*"     REFERENCE(I_SECONDS) TYPE  INT4 DEFAULT 20
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lt_enq    TYPE TABLE OF seqg3,
        lv_guname LIKE sy-uname,
        lv_garg   TYPE eqegraarg.

  DATA: ls_message TYPE bdcmsgcoll,
        ls_enq     TYPE seqg3.

  CLEAR et_messages.

** Chave de Bloqueio
***********************************************************************
  IF NOT i_garg IS INITIAL.
**  Indicada Directamente
    lv_garg = i_garg.

  ELSE.
**  Constroi Chave de Bloqueio
    IF NOT i_mandt IS INITIAL.
      lv_garg = i_mandt.
    ENDIF.
    CONCATENATE lv_garg i_element1 i_element2 i_element3
           INTO lv_garg.
    CONDENSE lv_garg NO-GAPS.

  ENDIF.

** Aguarda Libertar do Bloqueio
***********************************************************************
  DO i_seconds TIMES.
    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CALL FUNCTION 'ENQUE_READ'
      EXPORTING
        gclient       = sy-mandt
*       gname         =
        garg          = lv_garg
        guname        = lv_guname
      TABLES
        enq           = lt_enq.

    IF NOT i_gname IS INITIAL.
      DELETE lt_enq WHERE gname NE i_gname.
    ENDIF.
    CHECK lt_enq IS INITIAL.
    EXIT.
  ENDDO.

  IF NOT lt_enq IS INITIAL.
**  Processo Bloqueado por &
    READ TABLE lt_enq INTO ls_enq INDEX 1.

    ls_message-msgtyp = 'E'.
    ls_message-msgid = 'ZWM001'.
    ls_message-msgnr = '034'.
    ls_message-msgv1 = ls_enq-guname.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.


ENDFUNCTION.
