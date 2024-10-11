FUNCTION ZWM_CANCEL_TO_ITEM_DELIVERY.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WAREHOUSE) TYPE  LGNUM
*"     REFERENCE(TANUM) TYPE  TANUM
*"     REFERENCE(TAPOS) TYPE  TAPOS
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR_MESSAGE
*"----------------------------------------------------------------------
  DATA: bdc_tab LIKE bdcdata OCCURS 0 WITH HEADER LINE.
  DATA messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: opt LIKE ctu_params.

** 1 ecrã
  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0118'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-TANUM' tanum.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-TAPOS' tapos.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-LGNUM' warehouse.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-RHELL' ' '.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-RDNKL' 'X'.


  opt-defsize = 'X'.
  opt-dismode = 'N'.   "No Display
  opt-updmode = 'S'.   "Update Sincrono

  CALL TRANSACTION 'LT15' USING bdc_tab
                                  OPTIONS FROM opt
                                  MESSAGES INTO messtab.
  IF sy-subrc <> 0.
    LOOP AT messtab.
      APPEND messtab TO return_msg.
    ENDLOOP.
    RAISE error_message.
  ELSE.
* fill messages
    LOOP AT messtab.
      APPEND messtab TO return_msg.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
