class ZCL_ZWM_GESTAO_CAIS_DE_DPC_EXT definition
  public
  inheriting from ZCL_ZWM_GESTAO_CAIS_DE_DPC
  create public .

public section.

  methods ACT_DESBLOQUEIA_PORTA
    importing
      !IT_PARAMETER type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_FUNC_IMPORT optional
    exporting
      !ER_DATA type ref to DATA
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
protected section.

  methods DETALHE_PORTASET_GET_ENTITY
    redefinition .
  methods DETALHE_PORTASET_GET_ENTITYSET
    redefinition .
private section.

  types:
    BEGIN OF ys_filters ,
           armazem TYPE zwm002-armazem,
           porta   TYPE zwm002-porta,
           tknum   TYPE tknum,
         END OF ys_filters .

  data GT_DET_PORTA type ZCL_ZWM_GESTAO_CAIS_DE_MPC=>TT_DETALHE_PORTA .
  data GS_DET_PORTA type ZCL_ZWM_GESTAO_CAIS_DE_MPC=>TS_DETALHE_PORTA .

  methods GET_FILTERS
    importing
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION optional
    returning
      value(RS_FILTROS) type YS_FILTERS .
ENDCLASS.



CLASS ZCL_ZWM_GESTAO_CAIS_DE_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
**  EXPORTING
**    iv_action_name          =
**    it_parameter            =
**    io_tech_request_context =
**  IMPORTING
**    er_data                 =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  TRY.
      CASE iv_action_name.
        WHEN 'DesbloqueiaPorta'.
          act_desbloqueia_porta( EXPORTING it_parameter            = it_parameter
                                           io_tech_request_context = io_tech_request_context
                                     IMPORTING er_data = er_data ).
        WHEN OTHERS.
          CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~execute_action
            EXPORTING
              iv_action_name          = iv_action_name
              it_parameter            = it_parameter
              io_tech_request_context = io_tech_request_context
            IMPORTING
              er_data                 = er_data.
      ENDCASE.
    CATCH /iwbep/cx_mgw_busi_exception .
    CATCH /iwbep/cx_mgw_tech_exception .
  ENDTRY.


ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entity.

  DATA(ls_filters) = get_filters( it_key_tab = it_key_tab ).

  TRY.
      CASE iv_entity_name.
        WHEN 'TZwm028'.
        WHEN 'detalhe_porta'.
          gs_det_porta = zcl_wm_gestao_cais_de_carga=>get_detalhe_portas( porta = ls_filters-porta ).
          IF gs_det_porta-bloqueio_sscc EQ zcl_wm_gestao_cais_de_carga=>cs_bloqueio_sscc-desbloquear.
            CLEAR gs_det_porta-bloqueio_sscc.
          ENDIF.

***DATA(lv_techname) = 'T_ZWM028'.
***APPEND lv_techname TO et_expanded_tech_clauses.

          copy_data_to_ref(
           EXPORTING
            is_data = gs_det_porta
           CHANGING
            cr_data = er_entity ).

        WHEN OTHERS.

          CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~get_expanded_entity
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_key_tab               = it_key_tab
              it_navigation_path       = it_navigation_path
              io_expand                = io_expand
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              er_entity                = er_entity
              es_response_context      = es_response_context
              et_expanded_clauses      = et_expanded_clauses
              et_expanded_tech_clauses = et_expanded_tech_clauses.
      ENDCASE.
    CATCH /iwbep/cx_mgw_busi_exception .
    CATCH /iwbep/cx_mgw_tech_exception .
  ENDTRY.

ENDMETHOD.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset.

  DATA(ls_filters) = get_filters( it_key_tab               = it_key_tab
                                  it_filter_select_options = it_filter_select_options ).

  TRY.
      CASE iv_entity_name.
        WHEN 'TZwm028'.

        WHEN 'detalhe_porta'.
*          DATA gt_det_porta TYPE zcl_zwm_gestao_cais_de_mpc=>tt_detalhe_porta.
          zcl_wm_gestao_cais_de_carga=>get_detalhe_portas(
            EXPORTING
              porta             = ls_filters-porta
            IMPORTING
              et_detalhe_portas = gt_det_porta ).

          LOOP AT gt_det_porta ASSIGNING FIELD-SYMBOL(<lfs_det_porta>).
            IF <lfs_det_porta>-bloqueio_sscc EQ zcl_wm_gestao_cais_de_carga=>cs_bloqueio_sscc-desbloquear.
              CLEAR <lfs_det_porta>-bloqueio_sscc.
            ENDIF.
          ENDLOOP.
***DATA(lv_techname) = 'T_ZWM028'.
***APPEND lv_techname TO et_expanded_tech_clauses.

          copy_data_to_ref(
           EXPORTING
            is_data = gt_det_porta
           CHANGING
            cr_data = er_entityset ).

        WHEN OTHERS.
          CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset
            EXPORTING
              iv_entity_name           = iv_entity_name
              iv_entity_set_name       = iv_entity_set_name
              iv_source_name           = iv_source_name
              it_filter_select_options = it_filter_select_options
              it_order                 = it_order
              is_paging                = is_paging
              it_navigation_path       = it_navigation_path
              it_key_tab               = it_key_tab
              iv_filter_string         = iv_filter_string
              iv_search_string         = iv_search_string
              io_expand                = io_expand
              io_tech_request_context  = io_tech_request_context
            IMPORTING
              er_entityset             = er_entityset
              et_expanded_clauses      = et_expanded_clauses
              et_expanded_tech_clauses = et_expanded_tech_clauses
              es_response_context      = es_response_context.
      ENDCASE.
    CATCH /iwbep/cx_mgw_busi_exception .
    CATCH /iwbep/cx_mgw_tech_exception .
  ENDTRY.
ENDMETHOD.


METHOD act_desbloqueia_porta.

  DATA: ls_data TYPE zcl_zwm_gestao_cais_de_mpc=>ts_detalhe_porta.

  CHECK line_exists( it_parameter[ name = 'Porta' ] ).

  DATA(lv_porta) = VALUE tkonn( it_parameter[ name = 'Porta' ]-value ).

  CHECK lv_porta IS NOT INITIAL.

  CONSTANTS lc_desbloquear VALUE '-'."abap_false.

  zcl_wm_gestao_cais_de_carga=>set_bloqueio_porta_sscc(
    EXPORTING
      porta    = CONV #( lv_porta )
      bloqueio = lc_desbloquear
    IMPORTING
      es_detalhe_porta = ls_data "DATA(ls_zwm_s002)
  ).
  IF ls_data-bloqueio_sscc EQ zcl_wm_gestao_cais_de_carga=>cs_bloqueio_sscc-desbloquear.
    CLEAR ls_data-bloqueio_sscc.
  ENDIF.

*  ls_data = CORRESPONDING #( ls_zwm002 ).

  copy_data_to_ref( EXPORTING is_data = ls_data
                     CHANGING cr_data = er_data ).

ENDMETHOD.


METHOD detalhe_portaset_get_entity.

  DATA(ls_filters) = get_filters( it_key_tab               = it_key_tab ).

  er_entity = zcl_wm_gestao_cais_de_carga=>get_detalhe_portas(
                porta = ls_filters-porta
                inc_t_zwm028 = abap_false ).

ENDMETHOD.


METHOD detalhe_portaset_get_entityset.
**TRY.
*CALL METHOD SUPER->DETALHE_PORTASET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

  DATA(ls_filters) = get_filters( it_key_tab               = it_key_tab
                                  it_filter_select_options = it_filter_select_options ).


  zcl_wm_gestao_cais_de_carga=>get_detalhe_portas(
    EXPORTING
      porta        = ls_filters-porta
      inc_t_zwm028 = abap_false
    IMPORTING
      et_detalhe_portas = et_entityset ).

ENDMETHOD.


METHOD get_filters.

  DATA lv_val TYPE string.

  CLEAR rs_filtros.

  LOOP AT it_key_tab INTO DATA(ls_key_tab).
    lv_val = ls_key_tab-value.
    CONDENSE lv_val NO-GAPS.
    IF lv_val IS INITIAL.
      CONTINUE.
    ENDIF.
*    TRANSLATE ls_key_tab-name TO UPPER CASE.

    ASSIGN COMPONENT ls_key_tab-name
      OF STRUCTURE rs_filtros
        TO FIELD-SYMBOL(<lfv_val>).
    IF <lfv_val> IS ASSIGNED.
      <lfv_val> = lv_val.
    ENDIF.
  ENDLOOP.

  LOOP AT it_filter_select_options INTO DATA(ls_filter).
    READ TABLE ls_filter-select_options INTO DATA(ls_selopt) INDEX 1.
    IF ls_selopt-low IS INITIAL.
      CONTINUE.
    ENDIF.
    ASSIGN COMPONENT ls_filter-property
      OF STRUCTURE rs_filtros
        TO <lfv_val>.
    IF <lfv_val> IS ASSIGNED.
      <lfv_val> = ls_selopt-low.
    ENDIF.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.
