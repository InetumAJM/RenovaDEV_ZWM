FUNCTION zwm_embalar_hu_multi.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WAREHOUSE) TYPE  LGNUM OPTIONAL
*"     REFERENCE(HU) TYPE  EXIDV OPTIONAL
*"     REFERENCE(VBELN) TYPE  VBELN OPTIONAL
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL OPTIONAL
*"      IT_LTAP STRUCTURE  ZWM_LTAP OPTIONAL
*"  EXCEPTIONS
*"      EMPTY_TABLE
*"      REFERENCE_DOCUMENT_DIFFERS
*"      EMPTY_DELIVERY_ITEM
*"      ITEM_NOT_FOUND
*"      ERROR
*"----------------------------------------------------------------------

**  Associa a hu á remessa
  DATA: bdc_tab LIKE bdcdata OCCURS 0 WITH HEADER LINE.
  DATA: opt LIKE ctu_params.

  CHECK NOT it_ltap[] IS INITIAL.

  LOOP AT it_ltap.

    AT FIRST.

      PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPMV50A' '4004'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'LIKP-VBELN' vbeln.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=VERP_T'.

      PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPLV51G' '6000'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=UE6VDIR'.

    ENDAT.

    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPLV51G' '6000'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'VEKP-EXIDV' it_ltap-sscc.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=ENTR'.

    AT LAST.
      PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPLV51G' '6000'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=SICH'.
    ENDAT.

  ENDLOOP.

  opt-defsize = 'X'.
  opt-dismode = 'N'.   "No Display
  opt-updmode = 'S'.

  CLEAR:   return_msg.
  REFRESH: return_msg.

  CALL TRANSACTION 'VL02N' USING bdc_tab
                                  OPTIONS FROM opt
                                  MESSAGES INTO return_msg.

  IF sy-subrc <> 0.
    RAISE error.
  ELSE.

    LOOP AT return_msg WHERE msgtyp EQ 'A'.
      RAISE error.
    ENDLOOP.

    COMMIT WORK.

    LOOP AT return_msg WHERE msgnr EQ '157'.
      return_msg-msgtyp = 'E'.
      MODIFY return_msg.
    ENDLOOP.

    IF sy-subrc EQ 0.
      RAISE error.
    ENDIF.

  ENDIF.
ENDFUNCTION.
