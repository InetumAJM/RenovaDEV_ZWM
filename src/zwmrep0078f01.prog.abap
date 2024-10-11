*&---------------------------------------------------------------------*
*&  Include           ZWMREP0078F01                                    *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  imprime_itens_picking
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_itens_picking .

  DATA: ft_sscc LIKE zwm_ean128 OCCURS 0 WITH HEADER LINE.

  CHECK NOT scr_sscc    IS INITIAL AND
        NOT scr_printer IS INITIAL.

  REFRESH: ft_sscc.
  CLEAR:   ft_sscc.

  ft_sscc-sscc = scr_sscc.
  APPEND ft_sscc.

  CALL FUNCTION 'ZWM_IMPRIME_CONTEUDO_PICKING'
    EXPORTING
      printer                  = scr_printer
    TABLES
      sscc                     = ft_sscc
    EXCEPTIONS
      impressora_nao_existe    = 1
      sscc_nao_existe          = 2
      sscc_com_impressao_grupo = 3
      OTHERS                   = 4.

  IF sy-subrc <> 0.
    text1 = 'Erro na impressão SSCC'(001).
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text1.
  ELSE.
    text1 = 'Impressao com sucesso'(002).
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text1.
  ENDIF.

  CLEAR: scr_sscc.

ENDFORM.                    " imprime_itens_picking
