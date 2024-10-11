FUNCTION zwm_embalar_hu.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WAREHOUSE) TYPE  LGNUM OPTIONAL
*"     REFERENCE(HU) TYPE  EXIDV OPTIONAL
*"     REFERENCE(VBELN) TYPE  VBELN OPTIONAL
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL OPTIONAL
*"  EXCEPTIONS
*"      EMPTY_TABLE
*"      REFERENCE_DOCUMENT_DIFFERS
*"      EMPTY_DELIVERY_ITEM
*"      ITEM_NOT_FOUND
*"      OTHERS
*"----------------------------------------------------------------------

**  Associa a hu á remessa
  DATA: bdc_tab LIKE bdcdata OCCURS 0 WITH HEADER LINE.
  DATA messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: opt LIKE ctu_params.


  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A' '4004'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LIKP-VBELN' vbeln.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=VERP_T'.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPLV51G' '6000'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=UE6VDIR'.


  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPLV51G' '6000'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'VEKP-EXIDV' hu.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=ENTR'.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPLV51G' '6000'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=SICH'.

  opt-defsize = 'X'.
  opt-dismode = 'N'.   "No Display
  opt-updmode = 'S'.

  CALL TRANSACTION 'VL02N' USING bdc_tab
                                  OPTIONS FROM opt
                                  MESSAGES INTO messtab.
  IF sy-subrc <> 0.
    LOOP AT messtab.
      APPEND messtab TO return_msg.
    ENDLOOP.
    RAISE others.
  ELSE.

    COMMIT WORK.

  ENDIF.
ENDFUNCTION.
