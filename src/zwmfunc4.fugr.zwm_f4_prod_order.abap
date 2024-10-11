FUNCTION zwm_f4_prod_order.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------

  DATA rc.


* RETURN
  IF callcontrol-step = 'RETURN'.
*   PERFORM AUTHORITY_CHECK TABLES RECORD_TAB SHLP_TAB
*                           CHANGING SHLP CALLCONTROL.
    EXIT.
  ENDIF.

* EXIT immediately, if you do not want to handle this step
  IF callcontrol-step <> 'SELONE' AND
     callcontrol-step <> 'SELECT' AND
     callcontrol-step <> 'DISP'.
    EXIT.
  ENDIF.


*"----------------------------------------------------------------------
* STEP SELONE  (Select one of the elementary searchhelps)
*"----------------------------------------------------------------------
* This step is only called for collective searchhelps. It may be used
* to reduce the amount of elementary searchhelps given in SHLP_TAB.
* The compound searchhelp is given in SHLP.
* If you do not change CALLCONTROL-STEP, the next step is the
* dialog, to select one of the elementary searchhelps.
* If you want to skip this dialog, you have to return the selected
* elementary searchhelp in SHLP and to change CALLCONTROL-STEP to
* either to 'PRESEL' or to 'SELECT'.
  IF callcontrol-step = 'SELONE'.
*   PERFORM SELONE .........
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP PRESEL  (Enter selection conditions)
*"----------------------------------------------------------------------
* This step allows you, to influence the selection conditions either
* before they are displayed or in order to skip the dialog completely.
* If you want to skip the dialog, you should change CALLCONTROL-STEP
* to 'SELECT'.
* Normaly only SHLP-SELOPT should be changed in this step.
  IF callcontrol-step = 'PRESEL'.
*   PERFORM PRESEL ..........
    EXIT.
  ENDIF.


*"----------------------------------------------------------------------
* STEP SELECT    (Select values)
*"----------------------------------------------------------------------
* This step may be used to overtake the data selection completely.
* To skip the standard seletion, you should return 'DISP' as following
* step in CALLCONTROL-STEP.
* Normally RECORD_TAB should be filled after this step.
* Standard function module F4UT_RESULTS_MAP may be very helpfull in this
* step.
  IF callcontrol-step = 'SELECT'.

    TYPES: BEGIN OF lty_order_c,
             aufnr TYPE c LENGTH 12,
             linha TYPE c LENGTH 3,
             matnr TYPE c LENGTH 18,
             ean11 TYPE c LENGTH 18,
             maktx TYPE c LENGTH 40,
             prodp TYPE c LENGTH 10,
             meins TYPE c LENGTH 3,
             data  TYPE c LENGTH 8,
           END OF lty_order_c.
    DATA ls_order_c TYPE lty_order_c.
    DATA lt_ztporders TYPE ztporders.
    DATA: lv_linha TYPE fevor.

    LOOP AT shlp_tab-interface INTO DATA(ls_sh) WHERE value IS NOT INITIAL.
      IF ls_sh-shlpfield EQ 'LINHA'.
        lv_linha = ls_sh-value.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'ZWS_GET_PROD_ORDER'
      EXPORTING
        linha  = lv_linha "'H09'
*       DATA   =
      IMPORTING
        orders = lt_ztporders.
*    record_tab[] = lt_ztporders[].
    IF lt_ztporders IS INITIAL.
      rc = 4.
    ELSE.
      LOOP AT lt_ztporders INTO DATA(ls_order).
        ls_order_c = CORRESPONDING #( ls_order ).
        APPEND ls_order_c TO record_tab.
      ENDLOOP.
    ENDIF.


    IF rc = 0.
      callcontrol-step = 'DISP'.
    ELSE.
      callcontrol-step = 'EXIT'.
    ENDIF.
*   EXIT. "Don't process STEP DISP additionally in this call.
  ENDIF.



*"----------------------------------------------------------------------
* STEP DISP     (Display values)
*"----------------------------------------------------------------------
* This step is called, before the selected data is displayed.
* You can e.g. modify or reduce the data in RECORD_TAB
* according to the users authority.
* If you want to get the standard display dialog afterwards, you
* should not change CALLCONTROL-STEP.
* If you want to overtake the dialog on you own, you must return
* the following values in CALLCONTROL-STEP:
* - "RETURN" if one line was selected. The selected line must be
*   the only record left in RECORD_TAB. The corresponding fields of
*   this line are entered into the screen.
* - "EXIT" if the values request should be aborted
* - "PRESEL" if you want to return to the selection dialog
* Standard function modules F4UT_PARAMETER_VALUE_GET and
* F4UT_PARAMETER_RESULTS_PUT may be very helpfull in this step.
  IF callcontrol-step = 'DISP'.
*   PERFORM AUTHORITY_CHECK TABLES RECORD_TAB SHLP_TAB
*                           CHANGING SHLP CALLCONTROL.
    EXIT.
  ENDIF.




ENDFUNCTION.
