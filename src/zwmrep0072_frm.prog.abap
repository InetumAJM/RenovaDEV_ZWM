*&---------------------------------------------------------------------*
*&  Include           ZWMREP0072_FRM                                   *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  find_whs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_whs .

  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = l_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*    raise no_warehouse_found.
  ELSE.
    READ TABLE l_user WITH KEY statu = con_x.
    IF sy-subrc <> 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '003'
        IMPORTING
          ret_code       = ret_code.
      IF ret_code = 'O'.
        LEAVE TO SCREEN '0001'.
      ENDIF.
    ELSE.
      whs = l_user-lgnum.
    ENDIF.
  ENDIF.

ENDFORM.                    " find_whs

*&---------------------------------------------------------------------*
*&      Form  regista_contagem_monitor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM regista_contagem_monitor .

*  zwm038-lgnum = whs.
*  zwm038-lgtyp = posicao(3).
*  zwm038-lgpla = posicao+4(10).
*  zwm038-qtd_contada = qtd.
*  zwm038-uni = uni.
*
*  GET TIME.
*  zwm038-data_contagem = sy-datum.
*  zwm038-qtd_existente = qtd_existente.
*
*  MODIFY zwm038.
*  COMMIT WORK.

  CLEAR: cursorfield, posicao, ok_code_0001.

  LEAVE TO SCREEN '0001'.

ENDFORM.                    " regista_contagem_monitor

*&---------------------------------------------------------------------*
*&      Form  check_posicao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_posicao.

  DATA: itab_inv LIKE link OCCURS 0 WITH HEADER LINE.

  DATA: lt_messages TYPE TAB_BDCMSGCOLL.

  text1 = posicao.

  l_bin = posicao(14).

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = whs
      iv_bin_code = l_bin
    IMPORTING
      ev_bin      = l_bin
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
      CLEAR: posicao, ok_code_0001.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ZWM_SPLIT_BIN'
    EXPORTING
      bin   = l_bin
    IMPORTING
      lgtyp = l_lgtyp
      lgpla = l_lgpla.

  CLEAR: lagp.
  SELECT SINGLE * FROM lagp
  WHERE lgnum EQ whs
    AND lgtyp EQ l_lgtyp
    AND lgpla EQ l_lgpla.

  IF sy-subrc NE 0.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '191'
        message_var1   = text1.
    CLEAR: posicao, ok_code_0001.
  ELSE.
** Verifica se já existe doc de inventário para a posição

    CLEAR: link, linp.
    SELECT *
    FROM link AS k INNER JOIN linp AS p ON p~lgnum = k~lgnum
                                       AND p~ivnum = k~ivnum
           INTO CORRESPONDING FIELDS OF TABLE itab_inv
           WHERE k~lgnum    EQ whs
           AND   k~lgtyp    EQ l_lgtyp
           AND   p~lgpla    EQ l_lgpla.

*    SELECT * FROM link
*    WHERE lgnum EQ whs
*      AND lgtyp EQ l_lgtyp.
*      SELECT * FROM linp
*      WHERE lgnum EQ whs
*        AND ivnum EQ link-ivnum
*        AND lgpla EQ l_lgpla.
*        EXIT.
*      ENDSELECT.
*      IF sy-subrc EQ 0.
*        EXIT.
*      ENDIF.
*    ENDSELECT.
    IF sy-subrc NE 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '193'
          message_var1   = text1.
      CLEAR: posicao, ok_code_0001.
    ENDIF.
  ENDIF.

*message id 'ZWMMSG001' type 'E' number '193'.

ENDFORM.                    " check_posicao
