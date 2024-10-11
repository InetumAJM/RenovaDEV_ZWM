FUNCTION zwm_tapri_change.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TANUM) TYPE  TANUM
*"     REFERENCE(I_TAPRI) TYPE  TAPRI
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: ls_message TYPE bdcmsgcoll.

  CLEAR: et_messages.

  CALL FUNCTION 'L_RFMON_TAPRI_CHANGE'
    EXPORTING
      iv_lgnum  = i_lgnum
      iv_tanum  = i_tanum
      iv_tapri  = i_tapri
    EXCEPTIONS
      not_found = 1
      confirmed = 2
      no_change = 3
      failed    = 4
      OTHERS    = 5.

  IF sy-subrc <> 0.
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = sy-msgid.

    CASE sy-subrc.
      WHEN 1.
**      Ordem de Transporte & Inválida
        ls_message-msgnr = '023'.
        ls_message-msgv1 = i_tanum.
      WHEN 2.
**      Ordem de Transporte & Confirmada
        ls_message-msgnr = '024'.
        ls_message-msgv1 = i_tanum.
      WHEN OTHERS.
**      Erro a alterar Ordem de Transporte &
        ls_message-msgnr = '025'.
        ls_message-msgv1 = i_tanum.
    ENDCASE.

    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  CHECK i_commit EQ abap_true.

  COMMIT WORK AND WAIT.
ENDFUNCTION.
