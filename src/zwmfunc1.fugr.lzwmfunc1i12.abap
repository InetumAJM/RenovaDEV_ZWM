*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I12 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_PULMAO_fab1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pulmao_fab1 INPUT.
  CLEAR : text1,
            text2.

  IF NOT pulmao2 IS INITIAL.

** Descodifica Bin
    CALL FUNCTION 'ZWM_DECODE_BIN'
      EXPORTING
        iv_lgnum    = xuser-lgnum
        iv_bin_code = pulmao2
      IMPORTING
        ev_bin      = pulmao2
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      READ TABLE lt_messages INTO ls_messages INDEX 1.
      IF sy-subrc = 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = ls_messages-msgid
            message_lang   = sy-langu
            message_type   = ls_messages-msgtyp
            message_number = ls_messages-msgnr
            message_var1   = ls_messages-msgv1
            message_var2   = ls_messages-msgv2
            message_var3   = ls_messages-msgv3
            message_var4   = ls_messages-msgv4.
        CLEAR : pulmao2.
        REFRESH lt_messages.
        MOVE 'PULMAO2' TO cursorfield.
        RETURN.
      ENDIF.
    ENDIF.

    IF pulmao2 <> pulmao1.
** ERRO

      WRITE pulmao2 TO text1 LEFT-JUSTIFIED.
      WRITE pulmao1 TO text2 LEFT-JUSTIFIED.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '036'
          message_var1   = text1
          message_var2   = text2.

** 04.03.2005 - ROFFD ** DEL
** Registo da incidencia
*      CALL FUNCTION 'ZWM_INSERT_ERROR'
*        EXPORTING
*          armazem    = xuser-lgnum
*          incidencia = '1'
*          posicao    = pulmao2
*        EXCEPTIONS
*          no_commit  = 1
*          OTHERS     = 2.
*      IF sy-subrc = 0.
      CLEAR : pulmao2.
      MOVE 'PULMAO2' TO cursorfield.
*      ENDIF.
** 04.03.2005 - ROFFD ** DEL

    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_PULMAO_fab1  INPUT
