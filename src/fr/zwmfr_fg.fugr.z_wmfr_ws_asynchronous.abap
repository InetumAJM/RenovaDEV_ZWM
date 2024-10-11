FUNCTION z_wmfr_ws_asynchronous.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_EDIDC) TYPE  EDI_DC40_TT
*"     VALUE(IT_EDIDD) TYPE  EDI_DD40_TT
*"----------------------------------------------------------------------
  DATA lt_edidc TYPE edi_dc40_tt.
  DATA lt_edidd TYPE edi_dd40_tt.
*"----------------------------------------------------------------------

  lt_edidc[] = it_edidc[].
  lt_edidd[] = it_edidd[].

  CALL FUNCTION 'IDOC_INBOUND_ASYNCHRONOUS'
    TABLES
      idoc_control_rec_40 = lt_edidc[]
      idoc_data_rec_40    = lt_edidd[].

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.
ENDFUNCTION.
