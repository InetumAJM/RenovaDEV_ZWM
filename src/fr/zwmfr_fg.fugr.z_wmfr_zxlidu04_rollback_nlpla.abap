FUNCTION z_wmfr_zxlidu04_rollback_nlpla.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_SUBRC) TYPE  SYSUBRC
*"     VALUE(IT_ZWMFRT004) TYPE  ZWMFR_TTZWMFRT004
*"     VALUE(IT_ZWMFRT005) TYPE  ZWMFR_TTZWMFRT005
*"----------------------------------------------------------------------
  IF i_subrc NE 0.
    RETURN.
  ENDIF.

  INSERT zwmfrt004 FROM TABLE it_zwmfrt004[].
  COMMIT WORK AND WAIT.

  INSERT zwmfrt005 FROM TABLE it_zwmfrt005[].
  COMMIT WORK AND WAIT.
ENDFUNCTION.
