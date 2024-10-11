FUNCTION zwm_rfc_get_linhas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(COD_LINHA) LIKE  Z02RPEDLINHAS-COD_LINHA OPTIONAL
*"  EXPORTING
*"     VALUE(RETORNO) LIKE  ZWM_AUX-RETORNO
*"     VALUE(P_OUT) LIKE  ZWM_AUX-OUT_PRODUCAO
*"  TABLES
*"      LINHAS STRUCTURE  Z02RPEDLINHAS OPTIONAL
*"  EXCEPTIONS
*"      EXCEPTION
*"----------------------------------------------------------------------

  RANGES: r_cod FOR z02rpedlinhas-cod_linha.

  FREE: linhas, r_cod.
  CLEAR: linhas, retorno, r_cod, p_out.

  IF NOT cod_linha IS INITIAL.
    r_cod-low = cod_linha.
    r_cod-sign = 'I'.
    r_cod-option = 'EQ'.
    APPEND r_cod.
  ENDIF.

** Lista somente linhas que estejam activas
  SELECT * FROM z02rplinhas
  INTO CORRESPONDING FIELDS OF TABLE linhas
  WHERE cod_linha IN r_cod
    AND activo NE 'X'.

  IF sy-subrc NE 0.
    retorno = '10'. "Tabela sem registos
    EXIT.
  ELSE.
    LOOP AT linhas.
      CONCATENATE linhas-cod_linha '|' linhas-fevor '|'
                  linhas-descricao INTO p_out.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
