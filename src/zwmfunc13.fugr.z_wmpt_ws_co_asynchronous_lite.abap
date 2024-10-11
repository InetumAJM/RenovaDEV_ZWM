FUNCTION z_wmpt_ws_co_asynchronous_lite.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(I_MESTYP) TYPE  EDI4MESTYP
*"     REFERENCE(I_IDOCTY) TYPE  EDI4IDOCTP
*"     REFERENCE(I_LOGPOR) TYPE  PRX_LOGICAL_PORT_NAME OPTIONAL
*"     REFERENCE(IT_EDIDD) TYPE  EDIDD_TT
*"  EXPORTING
*"     REFERENCE(E_SUBRC) TYPE  SYSUBRC
*"----------------------------------------------------------------------
  DATA ls_input   TYPE zwmpt_ws_asynchronous_lite_so1.
  DATA ls_output  TYPE zwmpt_ws_asynchronous_lite_soa.

  DATA lt_items   TYPE zwmpt_sedidd_tab.

  DATA lo_proxy TYPE REF TO zwm_pt_wcs_co_host_rec_soap.

  DATA: lv_logpor TYPE prx_logical_port_name.

  FIELD-SYMBOLS <fs_edidd>  LIKE LINE OF it_edidd[].
  FIELD-SYMBOLS <fs_item>   LIKE LINE OF lt_items[].
*"----------------------------------------------------------------------

  IF i_logpor IS INITIAL.
    SELECT SINGLE valor FROM zwm001
                        INTO lv_logpor
                        WHERE armazem   = i_lgnum AND
                              processo  = 'LIBERACAO_VIA_IDOC' AND
                              parametro = 'DESTINO_PRD'.
  ELSE.
    lv_logpor = i_logpor.
  ENDIF.


**********************************************************************

  TRY.
      IF NOT lv_logpor IS INITIAL.
        CREATE OBJECT lo_proxy
          EXPORTING
            logical_port_name = lv_logpor.
      ELSE.
        CREATE OBJECT lo_proxy.
      ENDIF.
    CATCH cx_ai_system_fault.
      e_subrc = 8.
      RETURN.
  ENDTRY.

  LOOP AT it_edidd[] ASSIGNING <fs_edidd>.
    APPEND INITIAL LINE TO lt_items[] ASSIGNING <fs_item>.
    <fs_item>-segnam  = <fs_edidd>-segnam.
    <fs_item>-sdata   = <fs_edidd>-sdata.
  ENDLOOP.

  ls_input-z_wmpt_ws_asynchronous_lite1-iidocty         = i_idocty.
  ls_input-z_wmpt_ws_asynchronous_lite1-ilgnum          = i_lgnum.
  ls_input-z_wmpt_ws_asynchronous_lite1-imestyp         = i_mestyp.
  ls_input-z_wmpt_ws_asynchronous_lite1-it_edidd-zwmpt_sedidd[] = lt_items[].

  TRY.
      lo_proxy->zwmpt_ws_asynchronous_lite( EXPORTING input   = ls_input
                                            IMPORTING output  = ls_output ).
    CATCH cx_ai_system_fault cx_ai_application_fault.
      e_subrc = 8.
      RETURN.
  ENDTRY.

  e_subrc = 0.
ENDFUNCTION.
