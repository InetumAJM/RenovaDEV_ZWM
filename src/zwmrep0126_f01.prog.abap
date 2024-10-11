*&---------------------------------------------------------------------*
*&  Include           ZWMREP0126_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_of_selection.
  DATA: lt_messages	 TYPE	tab_bdcmsgcoll.

  CALL FUNCTION 'ZWM_TO_DELEVERY_ABAST'
    EXPORTING
      i_lgnum     = p_lgnum
      ir_refnr    = s_refnr[]
      ir_datum    = s_datum[]
      i_commit    = abap_true
    IMPORTING
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    PERFORM show_error USING lt_messages.
  ENDIF.
ENDFORM.                    " START_OF_SELECTION
*&---------------------------------------------------------------------*
*&      Form  SHOW_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MESSAGES  text
*----------------------------------------------------------------------*
FORM show_error USING ut_messages	TYPE tab_bdcmsgcoll.
  DATA: ls_message TYPE bdcmsgcoll.

  READ TABLE ut_messages
        INTO ls_message
        INDEX 1.

  IF rb_real EQ abap_true.
    MESSAGE ID ls_message-msgid
          TYPE 'I' NUMBER ls_message-msgnr
          DISPLAY LIKE ls_message-msgtyp
          WITH ls_message-msgv1
               ls_message-msgv2
               ls_message-msgv3
               ls_message-msgv4.
  ENDIF.
ENDFORM.                    " SHOW_ERROR
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
  IF sy-batch EQ abap_true.
    rb_real = abap_false.
    rb_back = abap_true.
  ELSE.
    rb_real = abap_true.
    rb_back = abap_false.
  ENDIF.

  SET CURSOR FIELD 'S_REFNR-LOW'.

** Data
***********************************************************************
  s_datum-sign = 'I'.
  s_datum-option = 'GE'.
  s_datum-low  = sy-datum - 10.
  APPEND s_datum.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  MAKE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I  text
*----------------------------------------------------------------------*
FORM make_message USING uv_msgnr uv_msgtyp
                        uv_msgv1 uv_msgv2
                        uv_msgv3 uv_msgv4.

  DATA: lt_messages	TYPE tab_bdcmsgcoll.

  DATA: ls_message TYPE bdcmsgcoll.

  ls_message-msgid  = 'ZWM001'.
  ls_message-msgtyp = uv_msgtyp.
  ls_message-msgnr  = uv_msgnr.
  ls_message-msgv1  = uv_msgv1.
  ls_message-msgv2  = uv_msgv2.
  ls_message-msgv3  = uv_msgv3.
  ls_message-msgv4  = uv_msgv4.

  APPEND ls_message TO lt_messages.

  PERFORM show_error USING lt_messages.
ENDFORM.                    " MAKE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_LX39
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_T311  text
*----------------------------------------------------------------------*
FORM update_lx39 USING ut_t311 TYPE gty_t_t311.
  DATA: lt_rspar TYPE TABLE OF rsparams.

  DATA: ls_rspar TYPE rsparams,
        ls_t311  TYPE t311.

  CHECK NOT ut_t311 IS INITIAL.

  ls_rspar-selname = 'LGNUM'.
  ls_rspar-kind = 'P'.
  ls_rspar-sign = 'I'.
  ls_rspar-option = 'EQ'.
  ls_rspar-low = p_lgnum.
  APPEND ls_rspar TO lt_rspar.
  CLEAR ls_rspar.

  LOOP AT ut_t311 INTO ls_t311.
    ls_rspar-selname = 'REFNR'.
    ls_rspar-kind = 'S'.
    ls_rspar-sign = 'I'.
    ls_rspar-option = 'EQ'.
    ls_rspar-low = ls_t311-refnr.
    APPEND ls_rspar TO lt_rspar.
    CLEAR ls_rspar.
  ENDLOOP.

  LOOP AT s_datum.
    ls_rspar-selname = 'DATUM'.
    ls_rspar-kind = 'S'.
    MOVE-CORRESPONDING s_datum TO ls_rspar.
    APPEND ls_rspar TO lt_rspar.
    CLEAR ls_rspar.
  ENDLOOP.

  ls_rspar-selname = 'ANZEIGE'.
  ls_rspar-kind = 'P'.
  ls_rspar-sign = 'I'.
  ls_rspar-option = 'EQ'.
  ls_rspar-low = ' '.
  APPEND ls_rspar TO lt_rspar.
  CLEAR ls_rspar.

  SUBMIT rl2stk00 WITH SELECTION-TABLE lt_rspar AND RETURN.
ENDFORM.                    " UPDATE_LX39
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM validate_selection_screen CHANGING cv_subrc TYPE sysubrc.
  CLEAR cv_subrc.

  IF s_refnr[] IS INITIAL AND
     s_datum[] IS INITIAL.

** 	Deve indicar Grupos a processar, ou datas de criação de Grupos
    MESSAGE s014 DISPLAY LIKE 'E'.

    cv_subrc = 4.
    EXIT.
  ENDIF.

ENDFORM.                    " VALIDATE_SELECTION_SCREEN
