FUNCTION ZWM_TBPRI_CHANGE .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TBNUM) TYPE  TBNUM
*"     REFERENCE(I_TBPRI) TYPE  TBPRI
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: ls_message TYPE bdcmsgcoll.

  CLEAR: et_messages.

*  IF i_tbpri IS NOT INITIAL.

    UPDATE ltbk SET tbpri = i_tbpri
    WHERE lgnum = i_lgnum
    AND   tbnum = i_tbnum.

    IF sy-subrc <> 0.
      ls_message-msgtyp = 'E'.
      ls_message-msgspra = sy-langu.
      ls_message-msgid = sy-msgid.

      CASE sy-subrc.
        WHEN 1.
**      Ordem de Transporte & Inválida
          ls_message-msgnr = '023'.
          ls_message-msgv1 = i_tbnum.
        WHEN 2.
**      Ordem de Transporte & Confirmada
          ls_message-msgnr = '024'.
          ls_message-msgv1 = i_tbnum.
        WHEN OTHERS.
**      Erro a alterar Ordem de Transporte &
          ls_message-msgnr = '025'.
          ls_message-msgv1 = i_tbnum.
      ENDCASE.

      APPEND ls_message TO et_messages.
      RAISE error.
    ENDIF.

    CHECK i_commit EQ abap_true.

    COMMIT WORK AND WAIT.
*  ENDIF.

ENDFUNCTION.
