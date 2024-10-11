FUNCTION zwm_get_stock_wcs.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(IS_STOCK) TYPE  ZWMPT_WS_STOCK_PICTURE
*"  TABLES
*"      T_STOCK TYPE  ZWMPT_WS_STOCK_PICTURE_RES_TAB
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA lo_proxy TYPE REF TO zwm_pt_wcs_co_host_rec_soap.

  DATA: lv_logpor     TYPE prx_logical_port_name.
  DATA: ls_input      TYPE zwmpt_ws_stock_picture_soap_in.
  DATA: ls_output     TYPE zwmpt_ws_stock_picture_soap_ou.
  DATA: ls_stock_pic  TYPE zwmpt_ws_stock_picture.

** Validar Serviço
**********************************************************************
  SELECT SINGLE valor FROM zwm001
                      INTO lv_logpor
                      WHERE armazem   = i_lgnum AND
                            processo  = 'LIBERACAO_VIA_IDOC' AND
                            parametro = 'DESTINO_PRD'.

  TRY.
      CREATE OBJECT lo_proxy
        EXPORTING
          logical_port_name = lv_logpor.

    CATCH cx_ai_system_fault.
      RAISE error.
  ENDTRY.

** Obter Stock
**********************************************************************
  ls_input-z_wmpt_ws_stock_picture1 = ls_stock_pic.

  TRY.
      lo_proxy->zwmpt_ws_stock_picture( EXPORTING input   = ls_input
                                        IMPORTING output  = ls_output ).
    CATCH cx_ai_system_fault
          cx_ai_application_fault.

      RAISE error.
  ENDTRY.

  t_stock[] = ls_output-zwmpt_ws_stock_picture_result-items-zwmpt_ws_stock_picture_respons[].

ENDFUNCTION.
