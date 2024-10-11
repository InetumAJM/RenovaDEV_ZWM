*&---------------------------------------------------------------------*
*&  Include           Z10MM050_V1                                      *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: l_gtarg   LIKE seqg3-gtarg,
        l_user    LIKE sy-uname,
        bloqueado.

  SET PF-STATUS 'ZRF1'.

*& Begin of Modification by Tiago Pateiro - ROFF @ 06.01.2016 15:25:12
  PERFORM f_user_get_lgnum.
*& End of Modification by Tiago Pateiro - ROFF @ 06.01.2016 15:25:15

  IF flag_bloqueio IS INITIAL.

** Verifica se já existe bloqueio e bloqueia se livre

    CONCATENATE 'Z10MMP03_' gv_lgnum INTO l_gtarg.

*    l_gtarg = 'Z10MMP03'.

    CLEAR bloqueado.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = l_gtarg
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      l_user = sy-msgv1.
      CONCATENATE 'O Prog. já está a ser processado pelo usuário'(001)
                   l_user
             INTO text SEPARATED BY space.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '000'
          message_var1   = text.

      LEAVE TO SCREEN 0.
    ELSE.
      flag_bloqueio = 'X'.
    ENDIF.

  ENDIF.

  IF NOT flag_erro IS INITIAL.

** Erro não apanhado
    it_msg-msgid  = sy-msgid.
    it_msg-msgtyp = sy-msgty.
    it_msg-msgnr  = sy-msgno.
    it_msg-msgv1  = sy-msgv1.
    it_msg-msgv2  = sy-msgv2.
    it_msg-msgv3  = sy-msgv3.
    it_msg-msgv4  = sy-msgv4.
    APPEND it_msg.

    CONCATENATE 'HU_1' l_idx INTO l_zztpdoc.
    CONDENSE l_zztpdoc NO-GAPS.
    PERFORM log_processo
       USING it_msg-msgtyp l_zztpdoc it_msg-msgv1 ' '.
  ENDIF.

  CLEAR vbss-sammg.
  SET PARAMETER ID 'GRN' FIELD vbss-sammg.

ENDMODULE.                             " STATUS_0100  OUTPUT
