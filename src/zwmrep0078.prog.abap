*&---------------------------------------------------------------------*
*& Report  ZWMREP0078                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zwmrep0078.

INCLUDE zwmrep0078top.
INCLUDE zwmrep0078o01.
INCLUDE zwmrep0078i01.
INCLUDE zwmrep0078f01.

START-OF-SELECTION.

** Obtenção do Armazem
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = l_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    EXIT.
  ELSE.

    READ TABLE l_user WITH KEY statu = 'X'.

    IF sy-subrc <> 0.
      WRITE sy-uname TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG000'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '003'
          message_msg1   = text1
        IMPORTING
          ret_code       = resposta.

    ELSE.
      lgnum = l_user-lgnum.
      IF l_user-devty(5) = '16X20'.
        screen1 = '0001'.
      ELSE.
        screen1 = '0002'.
      ENDIF.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

  CHECK NOT lgnum IS INITIAL.

  CALL SCREEN screen1.
