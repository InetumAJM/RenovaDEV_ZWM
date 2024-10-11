FUNCTION z_wm_goods_inbound.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_EBELN) TYPE  EBELN OPTIONAL
*"     REFERENCE(I_VBELN) TYPE  VBELN OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"     REFERENCE(E_MBLNR) TYPE  MBLNR
*"     REFERENCE(E_MJAHR) TYPE  MJAHR
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lt_bdc_tab TYPE TABLE OF bdcdata,
        lt_messtab TYPE TABLE OF bdcmsgcoll,
        lt_vbfa    TYPE TABLE OF vbfa,
        lt_vbeln   TYPE TABLE OF vbeln.

  DATA: opt     LIKE ctu_params.

  DATA: ls_message TYPE bdcmsgcoll.

  DATA: lv_vbeln  TYPE vbeln,
        lv_vbeln2 TYPE vbeln.

  CLEAR: et_messages, e_mblnr, e_mjahr.


  IF i_vbeln IS INITIAL.
    DO 1 TIMES.

      SELECT vbeln FROM lips
                   INTO TABLE lt_vbeln
                   WHERE vgbel = i_ebeln.

      CHECK sy-subrc EQ 0.


      SELECT * FROM vbfa
              INTO TABLE lt_vbfa
              FOR ALL ENTRIES IN lt_vbeln
              WHERE vbelv = lt_vbeln-table_line AND
                    vbtyp_n = 'i'.


      LOOP AT lt_vbeln INTO lv_vbeln2.
        READ TABLE lt_vbfa
           TRANSPORTING NO FIELDS
           WITH KEY vbelv = lv_vbeln2.

        CHECK sy-subrc = 0.

        lv_vbeln = lv_vbeln2.
        EXIT.
      ENDLOOP.


    ENDDO.



  ELSE.
    lv_vbeln = i_vbeln.
  ENDIF.



  PERFORM dynpro TABLES lt_bdc_tab USING 'X' 'SAPMM07M'      '0201'.
  PERFORM dynpro TABLES lt_bdc_tab USING ''  'RM07M-BWARTWE' '101'.
  PERFORM dynpro TABLES lt_bdc_tab USING ''  'RM07M-WVERS3'  'X'.
  PERFORM dynpro TABLES lt_bdc_tab USING ''  'XFULL'         'X'.
  PERFORM dynpro TABLES lt_bdc_tab USING ''  'RM07M-VBELN'   lv_vbeln.
  PERFORM dynpro TABLES lt_bdc_tab USING ''  'BDC_OKCODE'    '/00'.

  PERFORM dynpro TABLES lt_bdc_tab USING 'X' 'SAPMM07M'      '0221'.
  PERFORM dynpro TABLES lt_bdc_tab USING ''  'BDC_OKCODE'    '=SELE'.

  PERFORM dynpro TABLES lt_bdc_tab USING 'X' 'SAPMM07M'      '0221'.
  PERFORM dynpro TABLES lt_bdc_tab USING ''  'BDC_OKCODE'    '=BU'.


  opt-defsize = 'X'.
  opt-dismode = 'P'.   "No Display
  opt-updmode = 'S'.   "Update Sincrono

  CALL TRANSACTION 'MB0A' USING lt_bdc_tab
                                  OPTIONS FROM opt
                                  MESSAGES INTO lt_messtab.

  LOOP AT lt_messtab INTO ls_message WHERE msgtyp = 'E' OR
                                           msgtyp = 'A'.

    APPEND ls_message TO et_messages.
  ENDLOOP.
  IF sy-subrc EQ 0.
    RAISE error.
  ENDIF.


  LOOP AT lt_messtab INTO ls_message WHERE msgtyp = 'S' AND
                                           msgid = 'M7' AND
                                           msgnr = '060'.

    e_mblnr = ls_message-msgv1.
    e_mjahr = sy-datum(4).
  ENDLOOP.


ENDFUNCTION.
