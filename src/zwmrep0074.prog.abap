************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0074                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Monitor de Encomendas                                    *
* Criado por: Luís Rocha                                               *
* Criado em.: 14/10/2004                                               *
* Tipo PRG..: Report                                                   *
************************************************************************

report ZWMREP0074 message-id vla.

*&---------------------------------------------------------------------*
*& Sales order monitor (selection screen and list)                     *
*&---------------------------------------------------------------------*

* general data declaration
include zwm_rv50qtop.
* selection screen declaration for free and picking selection
include zwm_rv50qasel_v2.
* form routine pool
include zwm_rv50qaf01.

**********************************************************************
*                          Main program
**********************************************************************
initialization.
  perform determine_default_variant using    sy-repid       "HP_367865
                                    changing gf_default_variant.
  perform set_status_and_title.
  perform select_options_restrict.                          "HUM
* PERFORM check_standard_or_retail.  "route plan now in standard system
* PERFORM get_report_parameter.

at selection-screen output.
  perform set_default_variant using    sy-repid             "HP_367865
                              changing gf_default_variant.
  perform prepare_output.

*at selection-screen on if_refgw.
*  if not if_refgw is initial.
*    select single * from t006 where msehi = if_refgw and
*                                    dimid = 'MASS'.
*    if sy-subrc <> 0.
*      message e042(vla) with if_refgw.
*    endif.
*  endif.

*at selection-screen on if_refvo.
*  if not if_refvo is initial.
*    select single * from t006 where msehi = if_refvo and
*                                    dimid = 'VOLUME'.
*    if sy-subrc <> 0.
*      message e042(vla) with if_refvo.
*    endif.
*  endif.

*at selection-screen on block addpartner.
** check input for additional partner
*  perform analyse_add_partner.

at selection-screen.
  perform process_input.

start-of-selection.
* start order selection and display list
  perform selection_order_display.
