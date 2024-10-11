*&---------------------------------------------------------------------*
*&  Include           ZWMREP0123O01                                   *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = l_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*      raise no_warehouse_found.
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
      IF resposta = 'O'.
        LEAVE TO SCREEN '0001'.
      ENDIF.

    ELSE.
      whs = l_user-lgnum.
    ENDIF.
  ENDIF.

  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

  IF NOT matnr_in IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'MATNR_IN'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF matnr_in IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'MATNR_IN'.
        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_0001  OUTPUT
