*&---------------------------------------------------------------------*
*&  Include           ZWMREP0123F01                                    *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  FIND_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_whs.

  CLEAR whs.
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
    READ TABLE l_user WITH KEY statu = gc_true.  " con_x.
    IF sy-subrc <> 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '003'
        IMPORTING
          ret_code       = resposta.
      IF resposta = 'O'.
        LEAVE TO SCREEN '0001'.
      ENDIF.
    ELSE.
      whs = l_user-lgnum.
      setscreen1 = '0001'.
    ENDIF.
  ENDIF.

ENDFORM.                    " FIND_WHS

*&---------------------------------------------------------------------*
*&      Form  GET_CUSTOMIZING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_customizing .

  CLEAR: plant, lgort, l_printer.

  PERFORM get_parameter USING whs
                             'GERAL'
                             'PLANT'
                              valor.

  WRITE valor TO plant LEFT-JUSTIFIED.

  PERFORM get_parameter USING whs
                             'GERAL'
                             'LGORT'
                              valor.

  WRITE valor TO lgort LEFT-JUSTIFIED.


  IF gv_tcode = 'B'.
    PERFORM get_parameter USING whs
                               'GERAL'
                               'PRINTER_A'
                                valor.
  ELSE.
    PERFORM get_parameter USING whs
                             'GERAL'
                             'PRINTER'
                              valor.
  ENDIF.


  WRITE valor TO l_printer LEFT-JUSTIFIED.
  print_in = l_printer.
ENDFORM.                    " GET_CUSTOMIZING
*&---------------------------------------------------------------------*
*&      Form  get_parameter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->WHS        text
*      -->MODULE     text
*      -->PARAM      text
*      -->VALOR      text
*----------------------------------------------------------------------*
FORM get_parameter  USING    whs
                             module
                             param
                             valor.

  IF ti_zwm001[] IS INITIAL.
    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
      EXPORTING
        whs       = whs
      TABLES
        ti_zwm001 = ti_zwm001.
  ENDIF.

  CLEAR zwm001.
  READ TABLE ti_zwm001 WITH KEY      armazem   = whs
                                     processo  = module
                                     parametro = param
                                     BINARY SEARCH.
  IF sy-subrc = 0.
    MOVE ti_zwm001 TO zwm001.
  ENDIF.
  MOVE zwm001-valor TO valor.

ENDFORM.                    " GET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  CHECK_PRINT_IN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_print_in .
  DATA: lt_tsp031 TYPE TABLE OF tsp03l.
  DATA: l_count   TYPE i.

  CHECK NOT print_in IS INITIAL.

  CLEAR: l_count.

  l_count = STRLEN( print_in ).
  IF l_count GT 4.
    CONCATENATE 'Nome da Impressora só pode ter 4 Caracteres'
                '-' print_in '-' 'Erro'
           INTO text1 SEPARATED BY space.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text1.

    MOVE 'PRINT_IN' TO cursorfield.
    CLEAR: print_in.
    EXIT.

  ENDIF.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = print_in
    IMPORTING
      output = l_printer.

  SELECT * FROM tsp03l INTO TABLE lt_tsp031
  WHERE padest EQ l_printer.

  IF sy-subrc NE 0.
    text1 = print_in.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '068'
        message_var1   = text1.

    MOVE 'PRINT_IN' TO cursorfield.
    CLEAR print_in.
    EXIT.
  ENDIF.
ENDFORM.                    " CHECK_PRINT_IN
