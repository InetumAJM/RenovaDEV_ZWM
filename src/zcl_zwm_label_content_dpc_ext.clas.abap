class ZCL_ZWM_LABEL_CONTENT_DPC_EXT definition
  public
  inheriting from ZCL_ZWM_LABEL_CONTENT_DPC
  create public .

public section.

  types:
    BEGIN OF ty_filter,
           sscc TYPE exidv,
         END OF ty_filter .

  methods GET_FILTERS
    importing
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION optional
    exporting
      value(ES_FILTROS) type ANY .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
protected section.

  methods LABELCONTENTSET_GET_ENTITY
    redefinition .
  methods LABELCONTENTSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZWM_LABEL_CONTENT_DPC_EXT IMPLEMENTATION.


METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entity.

  DATA ls_filters TYPE ty_filter.

  get_filters( EXPORTING it_key_tab = it_key_tab
               IMPORTING es_filtros = ls_filters ).

  TRY.
      CASE iv_entity_name.
        WHEN 'Linhas'.
        WHEN 'LabelContent'.
          DATA(ls_label_content) =
          zcl_wm_label_content=>get_prepare_label_content(
            EXPORTING
              iv_sscc          = ls_filters-sscc ).

          copy_data_to_ref(
           EXPORTING
            is_data = ls_label_content
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

  DATA ls_filters TYPE ty_filter.

  get_filters( EXPORTING it_key_tab = it_key_tab
                         it_filter_select_options = it_filter_select_options
               IMPORTING es_filtros = ls_filters ).

  TRY.
      CASE iv_entity_name.
        WHEN 'Linhas'.
        WHEN 'LabelContent'.

          DATA lt_label_content TYPE STANDARD TABLE OF zwm_s008.

          DATA(ls_label_content) =
          zcl_wm_label_content=>get_prepare_label_content(
            EXPORTING
              iv_sscc          = ls_filters-sscc ).

          IF ls_label_content IS NOT INITIAL.
            APPEND ls_label_content TO lt_label_content.
          ENDIF.

          copy_data_to_ref(
           EXPORTING
            is_data = lt_label_content
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


METHOD get_filters.

  DATA lv_val TYPE string.

  CLEAR es_filtros.

  LOOP AT it_key_tab INTO DATA(ls_key_tab).
    lv_val = ls_key_tab-value.
    CONDENSE lv_val NO-GAPS.
    IF lv_val IS INITIAL.
      CONTINUE.
    ENDIF.
*    TRANSLATE ls_key_tab-name TO UPPER CASE.

    ASSIGN COMPONENT ls_key_tab-name
      OF STRUCTURE es_filtros
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
      OF STRUCTURE es_filtros
        TO <lfv_val>.
    IF <lfv_val> IS ASSIGNED.
      <lfv_val> = ls_selopt-low.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD labelcontentset_get_entity.

  DATA ls_filters TYPE ty_filter.

  get_filters( EXPORTING it_key_tab = it_key_tab
*                         it_filter_select_options = it_filter_select_options
               IMPORTING es_filtros = ls_filters ).

  DATA(ls_label_content) =
  zcl_wm_label_content=>get_prepare_label_content(
    EXPORTING
      iv_sscc          = ls_filters-sscc
      get_conteudo        = abap_false
      get_entrega_picking = abap_true ).

  MOVE-CORRESPONDING ls_label_content TO er_entity.

ENDMETHOD.


METHOD labelcontentset_get_entityset.

  DATA ls_filters TYPE ty_filter.

  get_filters( EXPORTING it_key_tab = it_key_tab
                         it_filter_select_options = it_filter_select_options
               IMPORTING es_filtros = ls_filters ).

  DATA(ls_label_content) =
  zcl_wm_label_content=>get_prepare_label_content(
    EXPORTING
      iv_sscc                = ls_filters-sscc
      get_conteudo        = abap_false
      get_entrega_picking = abap_true  ).

  APPEND CORRESPONDING #( ls_label_content ) TO et_entityset.

ENDMETHOD.
ENDCLASS.
