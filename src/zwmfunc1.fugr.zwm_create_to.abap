FUNCTION zwm_create_to .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WAREHOUSE) TYPE  LGNUM
*"     REFERENCE(MOV_TYPE) TYPE  BWLVS
*"     REFERENCE(MATERIAL) TYPE  MATNR
*"     REFERENCE(QUANTITY) TYPE  RL03TANFME
*"     REFERENCE(UNIT) TYPE  LRMEI
*"     REFERENCE(PLANT) TYPE  WERKS_D
*"     REFERENCE(S_LOC) TYPE  LGORT_D
*"     REFERENCE(LOTE) TYPE  CHARG_D
*"     REFERENCE(SOURCE_STY) TYPE  LTAP_VLTYP
*"     REFERENCE(SOURCE_BIN) TYPE  LTAP_VLPLA
*"     REFERENCE(DEST_STY) TYPE  LTAP_NLTYP OPTIONAL
*"     REFERENCE(DEST_BIN) TYPE  LTAP_VLPLA OPTIONAL
*"     REFERENCE(REQ_TYPE) TYPE  LVS_BETYP OPTIONAL
*"     REFERENCE(REQ_NUMBER) TYPE  LVS_BENUM OPTIONAL
*"     REFERENCE(SU) TYPE  LTAP-VLENR OPTIONAL
*"  EXPORTING
*"     REFERENCE(TO) TYPE  TANUM
*"     REFERENCE(TO_ITEM) TYPE  TAPOS
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: bdc_tab LIKE bdcdata OCCURS 0 WITH HEADER LINE.
  DATA messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: opt LIKE ctu_params.
  data: lv_unit TYPE meins.

  DATA: aux_quant(20).
  WRITE quantity TO aux_quant LEFT-JUSTIFIED.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0101'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-LGNUM' warehouse.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-BWLVS' mov_type.

  IF NOT req_number IS INITIAL.
    PERFORM dynpro TABLES bdc_tab
                          USING ' ' 'LTAK-BENUM' req_number.
  ENDIF.
  IF NOT req_type IS INITIAL.
    PERFORM dynpro TABLES bdc_tab
                          USING ' ' 'LTAK-BETYP' req_type.
  ENDIF.

  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-MATNR' material.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-ANFME' aux_quant.


  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = unit
      language       = sy-langu
    IMPORTING
      output         = lv_unit
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    lv_unit = unit.
  ENDIF.


  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-ALTME' lv_unit.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-WERKS' plant.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-LGORT' s_loc.
  IF NOT lote IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-CHARG' lote.
  ENDIF.

  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0102'.
  IF NOT source_sty IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-VLTYP' source_sty.
  ENDIF.
  IF NOT source_bin IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-VLPLA' source_bin.
  ENDIF.
  IF NOT su IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-VLENR' su.
  ENDIF.
  IF NOT dest_sty IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-NLTYP' dest_sty.
  ENDIF.
  IF NOT dest_bin IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-NLPLA' dest_bin.
  ENDIF.


  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

  opt-defsize = 'X'.
  opt-dismode = 'N'.   "No Display
  opt-updmode = 'S'.   "Update Sincrono

  CALL TRANSACTION 'LT01' USING bdc_tab
                                  OPTIONS FROM opt
                                  MESSAGES INTO messtab.
  IF sy-subrc = 0.
    READ TABLE messtab WITH KEY msgtyp = 'S'
                                msgid  = 'L3'
                                msgnr  = '016'.
    IF sy-subrc = 0.
      MOVE messtab-msgv1 TO to.
      to_item = 1.
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
