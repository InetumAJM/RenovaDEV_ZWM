FUNCTION z_wmfr_ws_asynchronous_lite.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     VALUE(I_MESTYP) TYPE  EDI4MESTYP
*"     VALUE(I_IDOCTY) TYPE  EDI4IDOCTP
*"     VALUE(IT_EDIDD) TYPE  ZWMFR_TTEDIDD
*"  EXPORTING
*"     VALUE(E_SUBRC) TYPE  SYSUBRC
*"----------------------------------------------------------------------
  DATA lv_extno   TYPE balnrext.
  DATA lv_lgnum   TYPE lgnum.
  DATA lv_return  TYPE zwm_aux-retorno.
  DATA lv_subrc   TYPE sysubrc.
  DATA lv_ediport TYPE edi4sndpor.
  DATA lv_edienvi TYPE edi4sndpor.
  DATA lv_message TYPE bapiret2-message.                    "#EC NEEDED
  DATA lv_segnum  TYPE edi_dd40-segnum.

  DATA ls_xuser   TYPE lrf_wkqu.

  DATA lt_edidc TYPE edi_dc40_tt.
  DATA lt_edidd TYPE edi_dd40_tt.

  DATA lo_ballog  TYPE REF TO zcl_ca_bal_log.

  FIELD-SYMBOLS <fs_edidc>      LIKE LINE OF lt_edidc[].
  FIELD-SYMBOLS <fs_edidd_i>    LIKE LINE OF it_edidd[].
  FIELD-SYMBOLS <fs_edidd>      LIKE LINE OF lt_edidd[].
  FIELD-SYMBOLS <fs_zwmmpt001>  LIKE LINE OF gt_zwmmpt001[].
*"----------------------------------------------------------------------

  IF i_lgnum IS NOT SUPPLIED.
    PERFORM f_data_init_get_user_data CHANGING ls_xuser lv_return. " get user RF data for WM
    IF lv_return IS INITIAL.
      lv_lgnum  = ls_xuser-lgnum.
    ENDIF.
  ELSEIF i_lgnum IS INITIAL.
    PERFORM f_data_init_get_user_data CHANGING ls_xuser lv_return. " get user RF data for WM
    IF lv_return IS INITIAL.
      lv_lgnum  = ls_xuser-lgnum.
    ENDIF.
  ELSE.
    lv_lgnum  = i_lgnum.
  ENDIF.

  CONCATENATE lv_lgnum i_mestyp INTO lv_extno.
  CONDENSE lv_extno.

  CREATE OBJECT lo_ballog
    EXPORTING
      i_log_obj  = c_balobj_zwm_fr
      i_log_sobj = c_balsubobj_interface
      i_extno    = lv_extno.

  PERFORM f_get_hardcodes USING lv_lgnum lo_ballog CHANGING lv_subrc.
  IF lv_subrc NE 0.
    lo_ballog->log_save( ).

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    e_subrc = 8.
    RETURN.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
    WITH TABLE KEY processo   = c_process_edi_wm_auto
                   parametro  = c_param_edi_wm_sndpor
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_ediport  = <fs_zwmmpt001>-valor.
  ELSE.
    MESSAGE e058 INTO lv_message.
    lo_ballog->log_add_message( ).
    lo_ballog->log_save( ).

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    e_subrc = 8.
    RETURN.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwmmpt001>
    WITH TABLE KEY processo   = c_process_edi_wm_auto
                   parametro  = c_param_edi_wm_rcvpor
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_edienvi  = <fs_zwmmpt001>-valor.
  ELSE.
    MESSAGE e059 INTO lv_message.
    lo_ballog->log_add_message( ).
    lo_ballog->log_save( ).

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    e_subrc = 8.
    RETURN.
  ENDIF.

  APPEND INITIAL LINE TO lt_edidc[] ASSIGNING <fs_edidc>.
  <fs_edidc>-idoctyp  = i_idocty.
  <fs_edidc>-mestyp   = i_mestyp.
  <fs_edidc>-sndpor   = lv_ediport.
  <fs_edidc>-sndprt   = c_edi_partyp_ls.
  <fs_edidc>-sndprn   = lv_ediport.
  <fs_edidc>-rcvpor   = lv_edienvi.
  <fs_edidc>-rcvprt   = c_edi_partyp_ls.
  <fs_edidc>-rcvprn   = lv_ediport.

  lv_segnum = 1.

  LOOP AT it_edidd[] ASSIGNING <fs_edidd_i>.
    APPEND INITIAL LINE TO lt_edidd[] ASSIGNING <fs_edidd>.
    <fs_edidd>-segnam = <fs_edidd_i>-segnam.
    <fs_edidd>-sdata  = <fs_edidd_i>-sdata.
    <fs_edidd>-segnum = lv_segnum.

    lv_segnum = lv_segnum + 1.
  ENDLOOP.

  CALL FUNCTION 'IDOC_INBOUND_ASYNCHRONOUS'
    TABLES
      idoc_control_rec_40 = lt_edidc[]
      idoc_data_rec_40    = lt_edidd[].

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.

  e_subrc = 0.
ENDFUNCTION.
