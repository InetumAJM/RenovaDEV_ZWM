*&---------------------------------------------------------------------*
*&  Include           ZWMREP0069_F01                                   *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  clear_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_fields .

  CLEAR : pdt_lgpla,
          pdt_matnr,
          pdt_maktx1,
          pdt_maktx2,
          p_lgtyp,
          p_lgpla,
          cursorfield.

ENDFORM.                    " clear_fields
*&---------------------------------------------------------------------*
*&      Form  find_whs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_whs .

** Wharehouse
  CLEAR lgnum.
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = l_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
**   raise no_warehouse_found.
  ELSE.
    READ TABLE l_user WITH KEY statu = 'X'.
    IF sy-subrc <> 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '003'
          message_var1   = text1
        IMPORTING
          ret_code       = ret_code.
      IF ret_code = 'O'.
        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ELSE.
      lgnum = l_user-lgnum.
      IF l_user-devty = '8X40'.
        setscreen1 = '0002'.
      ELSE.
        setscreen1 = '0001'.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    " find_whs

*&---------------------------------------------------------------------*
*&      Form  get_parametrizacao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_parametrizacao .

  DATA: p_valor LIKE zwm001-valor.

** Parametrização

** Tipo de depósito de picking
  CLEAR p_valor.
  SELECT SINGLE valor
           FROM zwm001
           INTO p_valor
          WHERE armazem   = lgnum
            AND processo  = 'REABASTECIMENTO'
            AND parametro = 'ST_PCK'.

  IF p_valor IS INITIAL.
    MESSAGE e000 WITH 'Falha parametrização: REABASTECIMENTO - ST_PCK'.
  ELSE.
    lgtyp_pck = p_valor.
  ENDIF.

ENDFORM.                    " get_parametrizacao
