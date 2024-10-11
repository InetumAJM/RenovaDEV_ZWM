FUNCTION zwm_create_storage_unit .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WAREHOUSE) TYPE  LGNUM
*"     REFERENCE(MOV_TYPE) TYPE  BWLVS
*"     REFERENCE(ST_TYPE) TYPE  LGTYP OPTIONAL
*"     REFERENCE(REQ_TYPE) TYPE  LVS_BETYP OPTIONAL
*"     REFERENCE(REQ_NUMBER) TYPE  LVS_BENUM OPTIONAL
*"     REFERENCE(STOCK_CAT) TYPE  BESTQ OPTIONAL
*"     REFERENCE(PLANT) TYPE  WERKS_D
*"     REFERENCE(S_LOC) TYPE  LGORT_D
*"     REFERENCE(SU_TYPE) TYPE  LVS_LETYP
*"     REFERENCE(DEST_BIN) TYPE  LTAP-NLPLA OPTIONAL
*"     REFERENCE(CERTIFICADO) TYPE  LTAP-ZEUGN OPTIONAL
*"     REFERENCE(MAT_STRUCT) TYPE  ZWM_MATERIAL
*"  EXPORTING
*"     REFERENCE(TO) TYPE  TANUM
*"  TABLES
*"      RESULT_MSG STRUCTURE  BDCMSGCOLL
*"  CHANGING
*"     REFERENCE(SU_NUMBER) TYPE  LENUM
*"  EXCEPTIONS
*"      OTHERS
*"----------------------------------------------------------------------
  DATA: transorderkey LIKE bapitransorderkey OCCURS 0 WITH HEADER LINE,
        toheaderdata  LIKE  bapitohead OCCURS 0 WITH HEADER LINE,
        toitemdata  LIKE  bapitoitem OCCURS 0 WITH HEADER LINE,
        extensionout  LIKE  bapiparex OCCURS 0 WITH HEADER LINE,
        return LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        bdc_tab LIKE bdcdata OCCURS 0 WITH HEADER LINE,
        messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: opt LIKE ctu_params,
        aux_quant(17),
        aux_lenum(20),
        output_meins LIKE mara-meins.

  REFRESH :messtab, bdc_tab.

  WRITE mat_struct-menge TO aux_quant LEFT-JUSTIFIED.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0170'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-LGNUM' warehouse.
  IF NOT su_number IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_LENUM_OUTPUT'
      EXPORTING
        input           = su_number
      IMPORTING
        output          = aux_lenum
      EXCEPTIONS
        t344_get_failed = 1
        OTHERS          = 2.

*    UNPACK SU_NUMBER TO AUX_LENUM.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LEIN-LENUM' aux_lenum.
  ENDIF.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-BWLVS' mov_type.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-WERKS' plant.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-LGORT' s_loc.
  IF NOT req_type IS INITIAL AND NOT req_number IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-BETYP' req_type.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-BENUM' req_number.
  ENDIF.
  IF NOT stock_cat IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-BESTQ' stock_cat.
  ENDIF.

  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0171'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LEIN-LETYP' su_type.
  IF NOT dest_bin IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' '*LTAP-NLPLA' dest_bin.
  ENDIF.
*& Begin of Modification by Tiago Pateiro - ROFF @ 14.12.2015 15:44:32
  IF st_type IS SUPPLIED.
    PERFORM dynpro TABLES bdc_tab USING ' ' '*LTAP-NLTYP' st_type.
  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 14.12.2015 15:44:32
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-MATNR(01)'
                                           mat_struct-material.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-ANFME(01)' aux_quant.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input                = mat_struct-meins
      language             = sy-langu
    IMPORTING
*     LONG_TEXT            =
      output               = output_meins
*     SHORT_TEXT           =
   EXCEPTIONS
     unit_not_found       = 1
     OTHERS               = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-ALTME(01)'
                                           output_meins.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-CHARG(01)'
                                           mat_struct-charg.
  IF NOT certificado IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-ZEUGN(01)'
                                             certificado.
  ENDIF.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=BU'.

  opt-defsize = 'X'.
  opt-dismode = 'P'.   "No Display
*opt-dismode = 'A'.
  opt-updmode = 'S'.   "Update Sincrono

  CALL TRANSACTION 'LT07' USING bdc_tab
                                  OPTIONS FROM opt
                                  MESSAGES INTO messtab.
  IF sy-subrc <> 0.
    LOOP AT messtab.
      APPEND messtab TO result_msg.
    ENDLOOP.
    RAISE others.
  ELSE.
* fill messages
    LOOP AT messtab.
      APPEND messtab TO result_msg.
    ENDLOOP.

* fill to number
    READ TABLE messtab WITH KEY msgtyp = 'S'
                                msgid = 'L3'
                                msgnr = '016'.
    IF sy-subrc = 0.
      MOVE messtab-msgv1 TO to.

* fill SU number
      MOVE to TO transorderkey-trans_ord.
      transorderkey-to_item = 1.

      REFRESH: return, toheaderdata, toitemdata, extensionout.
      CLEAR: return, toheaderdata, toitemdata, extensionout.
*      CALL FUNCTION 'BAPI_WHSE_TO_GET_DETAIL'
*        EXPORTING
*          WHSENUMBER        = WAREHOUSE
*          TRANSFERORDERNO   = TRANSORDERKEY-TRANS_ORD
*          TRANSFERORDERITEM = TRANSORDERKEY-TO_ITEM
*        TABLES
*          TOHEADERDATA      = TOHEADERDATA
*          TOITEMDATA        = TOITEMDATA
*          EXTENSIONOUT      = EXTENSIONOUT
*          RETURN            = RETURN.
*      READ TABLE TOITEMDATA INDEX 1.
*      IF SY-SUBRC = 0.
*        SU_NUMBER = TOITEMDATA-DEST_SU.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.
