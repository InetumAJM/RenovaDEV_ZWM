FUNCTION ZWM_TO_CANCEL .
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TANUM) TYPE  TANUM
*"     REFERENCE(I_TAPOS) TYPE  TAPOS OPTIONAL
*"     REFERENCE(IT_LTAP) TYPE  TT_TAPOS OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  RL03ACOMIT DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"--------------------------------------------------------------------


  DATA: ls_message TYPE bdcmsgcoll,
        lt_ltap_c  TYPE TABLE OF ltap_cancl.

  CLEAR: et_messages.

** Items
***********************************************************************
  IF NOT it_ltap IS INITIAL.
    lt_ltap_c = it_ltap.

    DELETE lt_ltap_c WHERE tanum NE i_tanum.

  ELSE.
    SELECT tanum tapos FROM ltap INTO TABLE lt_ltap_c
     WHERE lgnum EQ i_lgnum
       AND tanum EQ i_tanum.

    IF NOT i_tapos IS INITIAL.
      DELETE lt_ltap_c WHERE tapos NE i_tapos.
    ENDIF.

  ENDIF.

  CHECK NOT lt_ltap_c IS INITIAL.


** Cancela Tarefa
***********************************************************************
  CALL FUNCTION 'L_TO_CANCEL'
    EXPORTING
      i_lgnum                      = i_lgnum
      i_tanum                      = i_tanum
*     I_SOLEX                      = 0
*     I_CANCL                      = ' '
*     I_SUBST                      = ' '
*     I_QNAME                      = SY-UNAME
*     I_UPDATE_TASK                = ' '
      i_commit_work                = i_commit
    TABLES
      t_ltap_cancl                 = lt_ltap_c
    EXCEPTIONS
      to_confirmed                 = 1
      to_doesnt_exist              = 2
      item_confirmed               = 3
      item_doesnt_exist            = 4
      foreign_lock                 = 5
      double_lines                 = 6
      nothing_to_do                = 7
      xfeld_wrong                  = 8
      su_movement_partly_confirmed = 9
      update_without_commit        = 10
      no_authority                 = 11
      OTHERS                       = 12.


** Processamento de Erros
***********************************************************************
  IF sy-subrc <> 0.
    ls_message-msgid  = sy-msgid.
    ls_message-msgtyp = sy-msgty.
    ls_message-msgnr  = sy-msgno.
    ls_message-msgv1  = sy-msgv1.
    ls_message-msgv2  = sy-msgv2.
    ls_message-msgv3  = sy-msgv3.
    ls_message-msgv4  = sy-msgv4.
    APPEND ls_message TO et_messages.
    ROLLBACK WORK.
    RAISE error.
  ENDIF.


ENDFUNCTION.
