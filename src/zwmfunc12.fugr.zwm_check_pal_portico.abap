FUNCTION zwm_check_pal_portico .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_PORTA) TYPE  LGTOR
*"     REFERENCE(I_SSCC) TYPE  LENUM OPTIONAL
*"  TABLES
*"      T_RETURN STRUCTURE  BDCMSGCOLL OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: ls_zwm002 TYPE zwm002.
  DATA: ls_zwm081 TYPE zwm081.

** Validar Leitura SSCC na Porta com Pórtico
**********************************************************************
  SELECT SINGLE *
    FROM zwm002 INTO ls_zwm002
    WHERE armazem = i_lgnum
    AND   porta   = i_porta.

  CHECK sy-subrc = 0 AND ls_zwm002-portico = 'X'.

  IF ls_zwm002-bloqueio_sscc IS NOT INITIAL.

    " Porta & - Erro no pórtico - Bloqueio SSCC &
    CLEAR t_return.
    MOVE 'E'            TO t_return-msgtyp.
    MOVE 'ZWM001'       TO t_return-msgid.
    MOVE '141'          TO t_return-msgnr.
    MOVE sy-langu       TO t_return-msgspra.
    MOVE ls_zwm002-sscc TO t_return-msgv1.
    APPEND t_return.

    RAISE error.
  ENDIF.

  DO 20 TIMES.
    SELECT SINGLE *
      FROM zwm081 INTO ls_zwm081
      WHERE armazem = i_lgnum
      AND   sscc    = i_sscc.

    IF sy-subrc = 0 AND ls_zwm081-porta = i_porta.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

  IF ls_zwm081 IS INITIAL.
    " Porta & - Erro no pórtico - Sem validação de SSCC &
    CLEAR t_return.
    MOVE 'E'      TO t_return-msgtyp.
    MOVE 'ZWM001' TO t_return-msgid.
    MOVE '142'    TO t_return-msgnr.
    MOVE sy-langu TO t_return-msgspra.
    MOVE i_sscc   TO t_return-msgv1.
    APPEND t_return.

    RAISE error.
  ENDIF.

ENDFUNCTION.
