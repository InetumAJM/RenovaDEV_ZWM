*&---------------------------------------------------------------------*
*&  Include           ZWMREP0069_I01                                   *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  exit_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_0001 INPUT.

  CASE ok_code_0001.

    WHEN 'BACK'.
      PERFORM clear_fields.
      LEAVE TO SCREEN '0000'.

  ENDCASE.

ENDMODULE.                 " exit_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_pdt_lgpla  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pdt_lgpla INPUT.

  DATA: p_lgpla LIKE lqua-lgpla,
        p_lgtyp LIKE lqua-lgtyp.

  DATA: lt_messages TYPE tab_bdcmsgcoll.

  CHECK NOT pdt_lgpla IS INITIAL.

  CLEAR: pdt_matnr, maktx, pdt_maktx1, pdt_maktx2.

  p_lgtyp = pdt_lgpla(3).
  p_lgpla = pdt_lgpla+4(10).

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = l_user-lgnum
      iv_bin_code = pdt_lgpla
    IMPORTING
      ev_bin      = pdt_lgpla
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    READ TABLE lt_messages INTO DATA(ls_messages) INDEX 1.
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
      PERFORM clear_fields.
      REFRESH lt_messages.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

  IF p_lgtyp NE lgtyp_pck.
** Erro! Posição & não pertence ao tipo de depósito & !
    WRITE pdt_lgpla TO text1 LEFT-JUSTIFIED.
    WRITE lgtyp_pck TO text2 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_type   = 'E'
        message_number = '239'
        message_lang   = sy-langu
        message_var1   = text1
        message_var2   = text2
      IMPORTING
        ret_code       = ret_code.
    IF ret_code = 'O'.
      PERFORM clear_fields.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

** Valida a existência da posição
  CLEAR lagp.
  SELECT SINGLE lgpla
           FROM lagp
           INTO lagp-lgpla
          WHERE lgnum EQ lgnum
            AND lgtyp EQ p_lgtyp
            AND lgpla EQ p_lgpla.

  IF sy-subrc NE 0.
** Erro! Posição & inexistente!
    WRITE pdt_lgpla TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_type   = 'E'
        message_number = '240'
        message_lang   = sy-langu
        message_var1   = text1
      IMPORTING
        ret_code       = ret_code.
    IF ret_code = 'O'.
      PERFORM clear_fields.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

** Obtem o material associado à posição
  REFRESH: it_mlgt.
  CLEAR:   it_mlgt, linhas.
  SELECT matnr FROM mlgt
         INTO CORRESPONDING FIELDS OF TABLE it_mlgt
          WHERE lgnum EQ lgnum
            AND lgtyp EQ p_lgtyp
            AND lgpla EQ p_lgpla.

  DESCRIBE TABLE it_mlgt LINES linhas.

  IF linhas EQ 0.
** Erro! Posição & não tem material atribuido!
    WRITE pdt_lgpla TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_type   = 'E'
        message_number = '223'
        message_lang   = sy-langu
        message_var1   = text1
      IMPORTING
        ret_code       = ret_code.
    IF ret_code = 'O'.
      PERFORM clear_fields.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ELSEIF linhas > 1.
** Erro! Posição & tem mais que um material associado!
    WRITE pdt_lgpla TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_type   = 'E'
        message_number = '238'
        message_lang   = sy-langu
        message_var1   = text1
      IMPORTING
        ret_code       = ret_code.
    IF ret_code = 'O'.
      PERFORM clear_fields.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

  READ TABLE it_mlgt INDEX 1.

** Material
  pdt_matnr = it_mlgt-matnr.

** Descrição Material
  CLEAR maktx.
  SELECT SINGLE maktx FROM makt INTO maktx
          WHERE matnr = pdt_matnr
            AND spras = sy-langu.

  pdt_maktx1 = maktx(20).
  pdt_maktx2 = maktx+20(20).

ENDMODULE.                 " check_pdt_lgpla  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code_0001.

    WHEN 'CLR'.
      PERFORM clear_fields.

    WHEN 'NEXT'.

    WHEN 'SAVE'.

  ENDCASE.

  CLEAR ok_code_0001.

ENDMODULE.                 " user_command_0001  INPUT
