FUNCTION ywm_message_screen.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MESSAGE_ID) TYPE  BDCMSGCOLL-MSGID
*"     REFERENCE(MESSAGE_LANG) TYPE  SY-LANGU
*"     REFERENCE(MESSAGE_TYPE) TYPE  BDCMSGCOLL-MSGTYP
*"     REFERENCE(MESSAGE_NUMBER) TYPE  BDCMSGCOLL-MSGNR
*"     REFERENCE(MESSAGE_VAR1) TYPE  BDCMSGCOLL-MSGV1 OPTIONAL
*"     REFERENCE(MESSAGE_VAR2) TYPE  BDCMSGCOLL-MSGV2 OPTIONAL
*"     REFERENCE(MESSAGE_VAR3) TYPE  BDCMSGCOLL-MSGV3 OPTIONAL
*"     REFERENCE(MESSAGE_VAR4) TYPE  BDCMSGCOLL-MSGV4 OPTIONAL
*"  EXPORTING
*"     REFERENCE(RET_CODE) TYPE  FLAG
*"----------------------------------------------------------------------
  DATA: lv_message_lang TYPE sylangu.


  PERFORM user_own_data.

  IF lrf_wkqu-devty(5) = '16X20'.
    i_line_size = 18.
    i_lines  = 13.
  ELSE.
    i_line_size = 38.
    i_lines  = 6.
  ENDIF.

  CLEAR : mess_pos, line_offset, line_out,
         msg6, msg5, msg4, msg3, msg2, msg1.
  line_count = 1.

  lv_message_lang = message_lang.
  IF lv_message_lang IS INITIAL.
    lv_message_lang = sy-langu.
  ENDIF.

  SELECT SINGLE * FROM t100 WHERE sprsl = lv_message_lang
                              AND arbgb = message_id
                              AND msgnr = message_number.

  IF sy-subrc = 0.
    MOVE t100-text TO line_out.
    REPLACE '&' WITH message_var1 INTO line_out.
    CONDENSE line_out.
    REPLACE '&' WITH message_var2 INTO line_out.
    CONDENSE line_out.
    REPLACE '&' WITH message_var3 INTO line_out.
    CONDENSE line_out.
    REPLACE '&' WITH message_var4 INTO line_out.
    CONDENSE line_out.

    PERFORM msg_erro.

    IF lrf_wkqu-devty(5) = '16X20'.

      IF message_type = 'W'.
        CALL SCREEN '0002'.
        MOVE retcode TO ret_code.
      ELSE.
        CALL SCREEN '0001'.
        MOVE retcode TO ret_code.
      ENDIF.

    ELSE.

      IF message_type = 'W'.
        CALL SCREEN '0004'.
        MOVE retcode TO ret_code.
      ELSE.
        CALL SCREEN '0003'.
        MOVE retcode TO ret_code.
      ENDIF.

    ENDIF.

  ELSE.
* a mensagem não foi encontrada
    MOVE 'Erro na chamada de Ecrã.' TO line_out.
    PERFORM msg_erro.
  ENDIF.



ENDFUNCTION.
