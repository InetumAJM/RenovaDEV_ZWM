FUNCTION zwm_batch_input_deliv.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LFIMG) TYPE  LFIMG
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"     REFERENCE(I_ITEM_PAI) TYPE  POSNR OPTIONAL
*"     REFERENCE(I_ITEM_FILHO) TYPE  POSNR OPTIONAL
*"     REFERENCE(I_PSTYV) TYPE  PSTYV OPTIONAL
*"----------------------------------------------------------------------
  DATA: bdc_tab LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
        messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
       aux_quant(13).

  DATA: opt     LIKE ctu_params.

  REFRESH : bdc_tab.

  WRITE i_lfimg TO aux_quant LEFT-JUSTIFIED.

** Abre tela VL02n / insere Remessa / enter
  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A' '4004'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LIKP-VBELN' i_vbeln.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

** se item pai com filhos colocar 0
*  IF i_pstyv  ='TAN' AND i_lfimg IS INITIAL.
  IF i_item_pai IS NOT INITIAL AND i_lfimg IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '1000'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=POPO_T'.

** Insere Item de remessa Pai
    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '0111'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'RV50A-POSNR' i_item_pai.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE'  '=WEIT'.

    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '1000'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=CHPL_T01'.

    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '1000'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LIPSD-G_LFIMG(01)'
                                    aux_quant.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '1000'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=SICH_T'.



*    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '1000'.
*    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=CHPL_T01'.
*
*    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '1000'.
*    PERFORM dynpro TABLES bdc_tab USING ' ' 'LIPSD-G_LFIMG(01)'
*                                    aux_quant.
*    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.
*
*    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '1000'.
*    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=SICH_T'.
  ELSE.

** Posiciona no item
    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '1000'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=POPO_T'.

** Insere Item de remessa Pai
    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '0111'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'RV50A-POSNR' i_item_pai.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE'  '=WEIT'.

** se item pai sem filhos colocar valor
*    IF i_pstyv  ='TAN'.
    IF i_item_filho IS INITIAL.
     PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'          '1000'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'LIPSD-G_LFIMG(01)'
                                          aux_quant.
*    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE'        '/00'.

*      PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '1000'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=SICH_T'.

** se item filho colocar valor
    ELSE.
**
      PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '1000'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=CHSP_T'.

** Posiciona no Item
      PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '3000'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=POPO_T'.

** Insere item Filho
      PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '0111'.
     PERFORM dynpro TABLES bdc_tab USING ' ' 'RV50A-POSNR' i_item_filho.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE'  '=WEIT'.

** Altera Valor do item
      PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'       '3000'.
     PERFORM dynpro TABLES bdc_tab USING ' ' 'LIPS-LFIMG(01)' aux_quant.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE'     '/00'.

      PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A'   '3000'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=SICH_T'.

    ENDIF.
  ENDIF.
  opt-defsize = 'X'.
  opt-dismode = 'P'.   "No Display
*opt-dismode = 'A'.
  opt-updmode = 'S'.   "Update Sincrono

  CALL TRANSACTION 'VL02N' USING bdc_tab
                                  OPTIONS FROM opt
                                  MESSAGES INTO messtab.

ENDFUNCTION.
