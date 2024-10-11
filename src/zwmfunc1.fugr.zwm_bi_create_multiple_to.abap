FUNCTION zwm_bi_create_multiple_to.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WAREHOUSE) TYPE  LGNUM
*"     REFERENCE(MOV_TYPE) TYPE  BWLVS
*"     REFERENCE(ST_TYPE_O) TYPE  LGTYP OPTIONAL
*"     REFERENCE(BIN_ORIGEM) TYPE  LGPLA OPTIONAL
*"     REFERENCE(ST_TYPE_D) TYPE  LGTYP OPTIONAL
*"     REFERENCE(BIN_DESTINO) TYPE  LGPLA OPTIONAL
*"     REFERENCE(STOCK_CAT) TYPE  BESTQ OPTIONAL
*"     REFERENCE(PLANT) TYPE  WERKS_D
*"     REFERENCE(S_LOC) TYPE  LGORT_D
*"     REFERENCE(CERTIFICADO) TYPE  LTAP-ZEUGN OPTIONAL
*"     REFERENCE(ORIGEM) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(REQ_NUMBER) TYPE  LVS_BENUM OPTIONAL
*"     REFERENCE(REQ_TYPE) TYPE  LVS_BETYP OPTIONAL
*"     REFERENCE(SSCC_ADICIONAL) TYPE  LVS_LZNUM OPTIONAL
*"     REFERENCE(GRUPO) TYPE  LVS_REFNR OPTIONAL
*"  EXPORTING
*"     REFERENCE(TO) TYPE  TANUM
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"      SSCC STRUCTURE  ZWM_SSCC
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: bdc_tab LIKE bdcdata OCCURS 0 WITH HEADER LINE.
  DATA messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: opt LIKE ctu_params.

  DATA: aux_quant(20).

  READ TABLE sscc INDEX 1.
  CHECK sy-subrc = 0.

  WRITE sscc-quantidade TO aux_quant LEFT-JUSTIFIED.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0101'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-LGNUM'  warehouse.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-BWLVS'  mov_type.
  IF NOT req_number IS INITIAL.
    PERFORM dynpro TABLES bdc_tab
                          USING ' ' 'LTAK-BENUM' req_number.
  ENDIF.
  IF NOT req_type IS INITIAL.
    PERFORM dynpro TABLES bdc_tab
                          USING ' ' 'LTAK-BETYP' req_type.
  ENDIF.
  IF NOT grupo IS INITIAL.
    PERFORM dynpro TABLES bdc_tab
                          USING ' ' 'LTAK-REFNR' grupo.
  ENDIF.

  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-MATNR'  sscc-material.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-ANFME' aux_quant.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-ALTME'  sscc-uni.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-WERKS'  plant.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-LGORT'  s_loc.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-DUNKL'  'D'.
  IF NOT sscc-lote_producao IS INITIAL.
    PERFORM dynpro TABLES bdc_tab
                          USING ' ' 'LTAP-CHARG' sscc-lote_producao.
  ENDIF.
  IF NOT certificado IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-ZEUGN' certificado.
  ENDIF.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.
*
*  perform dynpro tables bdc_tab using 'X' 'SAPML03T' '0102'.
*  if not source_sty is initial.
*    perform dynpro tables bdc_tab using ' ' 'LTAP-VLTYP' source_sty.
*  endif.
*  if not source_sec is initial.
*    perform dynpro tables bdc_tab using ' ' 'LTAP-VLBER' source_sec.
*  endif.
*  if not source_bin is initial.
*    perform dynpro tables bdc_tab using ' ' 'LTAP-VLPLA' source_bin.
*  endif.
*  if not source_numb is initial.
*    perform dynpro tables bdc_tab using ' ' 'LTAP-VLQNR' source_numb.
*  endif.
*  if not dest_sty is initial.
*    perform dynpro tables bdc_tab using ' ' 'LTAP-NLTYP' dest_sty.
*  endif.
*  if not dest_sec is initial.
*    perform dynpro tables bdc_tab using ' ' 'LTAP-NLBER' dest_sec.
*  endif.
*  if not dest_bin is initial.
*    perform dynpro tables bdc_tab using ' ' 'LTAP-NLPLA' dest_bin.
*  endif.
*  if not dest_numb is initial.
*    perform dynpro tables bdc_tab using ' ' 'LTAP-NLQNR' dest_numb.
*  endif.
*  perform dynpro tables bdc_tab using ' ' 'BDC_OKCODE' '/00'.

  opt-defsize = 'X'.
  opt-dismode = 'N'.   "No Display
  opt-updmode = 'S'.   "Update Sincrono

  CALL TRANSACTION 'LT01' USING bdc_tab
                                  OPTIONS FROM opt
                                  MESSAGES INTO messtab.
  IF sy-subrc = 0.
    READ TABLE messtab WITH KEY msgtyp = 'S'.
*                                msgid  = 'L3'
*                                msgnr  = '016'.
    IF sy-subrc = 0.
      MOVE messtab-msgv1 TO to.
      APPEND messtab TO return_msg.
    ELSE.

      return_msg[] = messtab[].
      RAISE error.
      EXIT.
    ENDIF.
  ELSE.
    return_msg[] = messtab[].
    RAISE error.
    EXIT.
  ENDIF.

ENDFUNCTION.
