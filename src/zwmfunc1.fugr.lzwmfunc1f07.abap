*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1F07 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  valida_qtd_lote
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_qtd_lote .

  DATA qtd_total LIKE mchb-clabs.

  REFRESH t_mchb.
  CLEAR: t_mchb, qtd_total.

  qtd_total = quantidade * n_etiq.

  SELECT SINGLE *
      FROM zwm032
          WHERE armazem = xuser-lgnum AND
                ot = to_ret.

  SELECT * INTO TABLE t_mchb
      FROM mchb
          WHERE matnr = material AND
                werks = 'RENV' AND
                lgort = zwm032-deposito AND
                clabs > 0.

  SORT t_mchb BY ersda.
  CLEAR lote.

  LOOP AT t_mchb.

    IF t_mchb-clabs >= qtd_total.
      lote = t_mchb-charg.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF lote IS INITIAL.
**  Não existe lote para a transferencia.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '158'.

    CLEAR n_etiq.
    MOVE 'N_ETIQ' TO cursorfield.
  ENDIF.

ENDFORM.                    " valida_qtd_lote
