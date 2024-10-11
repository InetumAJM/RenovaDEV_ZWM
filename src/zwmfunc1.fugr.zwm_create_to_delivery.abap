FUNCTION zwm_create_to_delivery.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WAREHOUSE) TYPE  LGNUM
*"     REFERENCE(REFNR) TYPE  LVS_REFNR
*"     REFERENCE(VBELN) TYPE  VBELN
*"     REFERENCE(POSNR) TYPE  POSNR
*"     REFERENCE(VSOLA) TYPE  LTAP-VSOLA OPTIONAL
*"     REFERENCE(SU) TYPE  LENUM OPTIONAL
*"     REFERENCE(SU2) TYPE  LENUM OPTIONAL
*"     REFERENCE(VLTYP) TYPE  LGTYP OPTIONAL
*"     REFERENCE(VLPLA) TYPE  LGPLA OPTIONAL
*"     REFERENCE(BACKGROUND) TYPE  LVS_DUNKL OPTIONAL
*"     REFERENCE(CHARG) TYPE  CHARG_D OPTIONAL
*"  EXPORTING
*"     REFERENCE(TO) TYPE  TANUM
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR_MESSAGE
*"----------------------------------------------------------------------
  DATA: bdc_tab LIKE bdcdata OCCURS 0 WITH HEADER LINE.
  DATA messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: opt LIKE ctu_params.
  DATA quant(16).
  DATA lv_lgnum TYPE lgnum.
  DATA lv_werks TYPE werks_d. " << INS ROFF(SDF):TMGP:04.01.2016 17:08:22
  DATA lv_lgort TYPE lgort_d.

  WRITE vsola TO quant.

*& Begin of Modification by Tiago Pateiro - ROFF @ 04.01.2016 17:08:26

  lv_lgnum = warehouse.
  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user      = sy-uname
      i_refnr     = refnr
      i_vbeln     = vbeln
      i_usemm     = 'X'
      i_useaut    = 'X'
      i_get_lgnum = 'X'
      i_get_werks = 'X'
      i_get_lgort = 'X'
    CHANGING
      c_lgnum     = lv_lgnum
      c_werks     = lv_werks
      c_lgort     = lv_lgort
    EXCEPTIONS
      error       = 1
      user_back   = 2
      OTHERS      = 3.


**  SELECT werks UP TO 1 ROWS
**    FROM t320 INTO lv_werks
**    WHERE lgort EQ 'CD'
**      AND lgnum EQ warehouse.
**  ENDSELECT.
**  IF sy-subrc NE 0.
**    lv_werks = 'RENV'.
**  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 04.01.2016 17:08:26

** 1 ecrã - LT03
  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0151'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-LGNUM' lv_lgnum.
*  PERFORM dynpro TABLES bdc_tab USING ' ' 'VBLKP-WERKS' 'RENV'. " << DEL ROFF(SDF):TMGP:04.01.2016 17:10:00
  PERFORM dynpro TABLES bdc_tab USING ' ' 'VBLKP-WERKS' lv_werks. " << INS ROFF(SDF):TMGP:04.01.2016 17:10:02
  PERFORM dynpro TABLES bdc_tab USING ' ' 'VBLKK-VBELN' vbeln.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-REFNR' refnr.
  PERFORM dynpro TABLES bdc_tab USING ' ' '*VBLKP-POSNR' posnr.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-ALAKT' 'X'.

  IF background = 'D'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-DUNKL' 'D'.
  ENDIF.

  PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-KOMIM' ' '.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-EINLM' ' '.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-EINTA' ' '.

  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0105'.
  IF NOT su2 IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_CURSOR'
                                            'LTAPA-ANFME(02)'.
  ELSE.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_CURSOR'
                                            'LTAPA-VLENR(01)'.
  ENDIF.

  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAPA-ANFME(01)' quant.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAPA-CHARG(01)' charg.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAPA-VLENR(01)' su.

  IF NOT su2 IS INITIAL.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAPA-ANFME(02)' quant.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAPA-CHARG(02)' charg.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAPA-VLENR(02)' su2.
  ENDIF.

  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

  PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0105'.
  PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=BU'.

  opt-defsize = 'X'.
  opt-dismode = 'N'.   "No Display
  opt-updmode = 'S'.   "Update Sincrono

  CALL TRANSACTION 'LT03' USING bdc_tab
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

* fill to number
  READ TABLE messtab WITH KEY msgtyp = 'S'
                              msgid = 'L3'
                              msgnr = '016'.
  IF sy-subrc = 0.
    MOVE messtab-msgv1 TO to.
  ENDIF.

ENDFUNCTION.
