FUNCTION zwm_confirm_all_items_to.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) TYPE  LGNUM
*"     REFERENCE(CONFIRM_TYPE) TYPE  CHAR1
*"     REFERENCE(SU) TYPE  LENUM OPTIONAL
*"     REFERENCE(BIN) TYPE  LGPLA OPTIONAL
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  CHANGING
*"     REFERENCE(TO) TYPE  TANUM
*"  EXCEPTIONS
*"      CONFIRM_TYPE_ERROR
*"      TO_NOT_FOUND
*"      TO_ALREADY_CONFIRMED
*"      TO_ALREADY_PICKED
*"      TO_NOT_PICKED
*"      WRONG_SU
*"      MISSING_SU
*"      ERROR
*"----------------------------------------------------------------------
  DATA: toheaderdata LIKE  bapitohead OCCURS 0 WITH HEADER LINE,
        toitemdata   LIKE  bapitoitem OCCURS 0 WITH HEADER LINE,
        extensionout LIKE  bapiparex OCCURS 0 WITH HEADER LINE,
        return       LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  DATA: bdc_tab LIKE bdcdata OCCURS 0 WITH HEADER LINE.
  DATA messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: opt LIKE ctu_params.
  DATA: aux_su(20), aux_bin(10).

  DATA: lv_subst  TYPE rlistsubst.
  DATA: ls_ltap   TYPE ltap.

* check CONFIRM_TYPE
  IF confirm_type <> 'P'
     AND confirm_type <> 'T'
     AND confirm_type <> 'B'.
    RAISE confirm_type_error.
    EXIT.
  ENDIF.


** Valida se bloqueio à remessa
  IF NOT to IS INITIAL AND confirm_type EQ 'T'.
    PERFORM valida_bloq USING armazem
                              to.

  ENDIF.

* confirm TO with SU
  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0111'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-LGNUM' armazem.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-TANUM' to.
*  PERFORM DYNPRO TABLES BDC_TAB USING ' ' 'RL03T-DUNKL' 'H'.
  IF NOT bin IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-DUNKL' 'H'.
  ELSE.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-DUNKL' 'D'.
  ENDIF.

  IF confirm_type = 'P'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-QENTN' 'X'.
  ELSEIF confirm_type = 'T'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-QTRAN' 'X'.
  ELSEIF confirm_type = 'B'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-QENTR' 'X'.
  ENDIF.

  IF lv_subst IS NOT INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'RLIST-SUBST' 'X'.
  ENDIF.

  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

  IF NOT bin IS INITIAL.
    WRITE bin TO aux_bin.
    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0114'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAP-NLPLA(02)' aux_bin.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=BU'.
  ELSEIF su IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0113'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=BU'.
  ELSE.
    WRITE su TO aux_su.
    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0180'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LQBL-LENUM(01)' aux_su.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0180'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=QUD'.
  ENDIF.


  opt-defsize = 'X'.
  opt-dismode = 'N'.   "No Display
  opt-updmode = 'S'.   "Update Sincrono

  IF confirm_type = 'P'.
    CALL TRANSACTION 'LT12' USING bdc_tab
                                    OPTIONS FROM opt
                                    MESSAGES INTO messtab.
  ELSEIF confirm_type = 'T'.
    CALL TRANSACTION 'LT12' USING bdc_tab
                                    OPTIONS FROM opt
                                    MESSAGES INTO messtab.
  ELSEIF confirm_type = 'B'.
    CALL TRANSACTION 'LT12' USING bdc_tab
                                    OPTIONS FROM opt
                                    MESSAGES INTO messtab.
  ENDIF.

  IF sy-subrc = 0.
    LOOP AT messtab.
      APPEND messtab TO return_msg.
    ENDLOOP.
  ELSE.
    LOOP AT messtab WHERE msgtyp = 'E' .
      CLEAR su1.
      APPEND messtab TO return_msg.
      RAISE error.
      EXIT.
    ENDLOOP.

  ENDIF.


ENDFUNCTION.
