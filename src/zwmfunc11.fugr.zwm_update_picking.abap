FUNCTION zwm_update_picking.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LFIMG) TYPE  LFIMG
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"     REFERENCE(I_POSNR) TYPE  POSNR
*"----------------------------------------------------------------------
  DATA: bdc_tab LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
        messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
        aux_quant(13).

  DATA: opt     LIKE ctu_params.

  REFRESH : bdc_tab.

  WRITE i_lfimg TO aux_quant LEFT-JUSTIFIED.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A' '4004'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LIKP-VBELN' i_vbeln.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A' '1000'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=POPO_T'.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A' '0111'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'RV50A-POSNR' i_posnr.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=WEIT'.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A' '1000'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LIPSD-PIKMG(01)' aux_quant.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A' '1000'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=SICH_T'.

  opt-defsize = 'X'.
  opt-dismode = 'P'.   "No Display
  opt-updmode = 'S'.   "Update Sincrono

  CALL TRANSACTION 'VL02N' USING bdc_tab
                                  OPTIONS FROM opt
                                  MESSAGES INTO messtab.

ENDFUNCTION.
