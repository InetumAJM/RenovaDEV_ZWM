*&---------------------------------------------------------------------*
*&  Include           ZWMREP0021O01                                    *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'ZRF'.

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
      lgnum = l_user-lgnum.
    ENDIF.
  ENDIF.

  " Paletização no Automático (WCS)
  IF sy-tcode = 'ZWM028A'.
    gv_aut_wcs = 'X'.
  ELSE.
    CLEAR gv_aut_wcs.
  ENDIF.


** Armazém Automático (WCS)
  IF gv_aut_wcs IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 = 'G1'.
        screen-input     = '0'.
        screen-invisible = '1'.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF sscc IS INITIAL.
    SET CURSOR FIELD 'SSCC'.
  ELSEIF lgpla_in IS INITIAL AND gv_aut_wcs = 'X'.
    SET CURSOR FIELD 'LGPLA_IN'.
  ENDIF.

ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0002 OUTPUT.

  SET PF-STATUS 'ZRF1'.

** Empilhador

  tit = 'SapConsole-Envio'.

** Armazém Automático (WCS)
  IF gv_aut_wcs IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 = 'G1'.
        screen-input     = '0'.
        screen-invisible = '1'.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF sscc IS INITIAL.
    SET CURSOR FIELD 'SSCC'.
  ELSEIF lgpla_in IS INITIAL AND gv_aut_wcs = 'X'.
    SET CURSOR FIELD 'LGPLA_IN'.
  ENDIF.

ENDMODULE.                 " status_0002  OUTPUT
