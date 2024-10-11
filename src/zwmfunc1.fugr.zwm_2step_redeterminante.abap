FUNCTION zwm_2step_redeterminante.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     VALUE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"     VALUE(IR_REFNR) TYPE  ZWM_R_REFNR OPTIONAL
*"     VALUE(IT_T311) TYPE  ZWM_T_T311 OPTIONAL
*"  EXPORTING
*"     VALUE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"----------------------------------------------------------------------
  DATA: lt_bdc_tab  TYPE TABLE OF bdcdata,
        lt_messages TYPE tab_bdcmsgcoll,
        lt_t311     TYPE TABLE OF t311.

  DATA: lr_refnr TYPE zwm_r_refnr.

  DATA: ls_opt     TYPE ctu_params,
        ls_t311    TYPE t311,
        ls_r_refnr LIKE LINE OF ir_refnr.

  DATA: lv_2step TYPE flag.

  CLEAR: et_messages.

  lr_refnr = ir_refnr.

  IF NOT i_refnr IS INITIAL.
    ls_r_refnr-low = i_refnr.
    ls_r_refnr-sign   = 'I'.
    ls_r_refnr-option = 'EQ'.
    APPEND ls_r_refnr TO lr_refnr.
  ENDIF.

** Retorna Grupos
***********************************************************************
  IF NOT lr_refnr IS INITIAL.
    SELECT * FROM t311
             INTO TABLE lt_t311
             WHERE lgnum = i_lgnum AND
                   refnr IN lr_refnr.
  ENDIF.

  IF NOT it_t311 IS INITIAL.
    APPEND LINES OF it_t311 TO lt_t311.
  ENDIF.

  SORT lt_t311 BY lgnum refnr.
  DELETE ADJACENT DUPLICATES FROM lt_t311 COMPARING lgnum refnr.

** batch Input
***********************************************************************
  LOOP AT lt_t311 INTO ls_t311.
    PERFORM dynpro TABLES lt_bdc_tab USING 'X' 'SAPLL2PIK' '0101'.
    PERFORM dynpro TABLES lt_bdc_tab USING ' ' 'RL05S-LGNUM' ls_t311-lgnum.
    PERFORM dynpro TABLES lt_bdc_tab USING ' ' 'RL05S-REFNR_2ST' ls_t311-refnr.
    PERFORM dynpro TABLES lt_bdc_tab USING ' ' 'RL05S-HELL' 'X'.
    PERFORM dynpro TABLES lt_bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

    PERFORM dynpro TABLES lt_bdc_tab USING 'X' 'SAPMSSY0' '0120'.
    PERFORM dynpro TABLES lt_bdc_tab USING ' ' 'BDC_OKCODE' '=2SBE'.
    PERFORM dynpro TABLES lt_bdc_tab USING 'X' 'SAPMSSY0' '0120'.
    PERFORM dynpro TABLES lt_bdc_tab USING ' ' 'BDC_OKCODE' '=BU'.


    ls_opt-defsize = 'X'.
    ls_opt-dismode = 'N'.   "No Display
    ls_opt-updmode = 'S'.   "Update Sincrono

    CALL TRANSACTION 'LT72' USING lt_bdc_tab
                            OPTIONS FROM ls_opt
                            MESSAGES INTO lt_messages.

    DELETE lt_messages WHERE msgtyp <> 'E' AND
                             msgtyp <> 'A'.

    APPEND LINES OF lt_messages TO et_messages.
  ENDLOOP.
ENDFUNCTION.
