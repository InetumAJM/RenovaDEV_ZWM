class ZCL_UI2_CL_JSON definition
  public
  inheriting from /UI2/CL_JSON
  create public .

public section.

  class-methods ZSERIALIZE
    importing
      !DATA type DATA
      !COMPRESS type BOOL default C_BOOL-FALSE
      !NAME type STRING optional
      !PRETTY_NAME type PRETTY_NAME_MODE default PRETTY_MODE-NONE
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR optional
      !ASSOC_ARRAYS type BOOL default C_BOOL-FALSE
      !TS_AS_ISO8601 type BOOL default C_BOOL-FALSE
      !EXPAND_INCLUDES type BOOL default C_BOOL-TRUE
      !ASSOC_ARRAYS_OPT type BOOL default C_BOOL-FALSE
      !NUMC_AS_STRING type BOOL default C_BOOL-FALSE
      !NAME_MAPPINGS type NAME_MAPPINGS optional
      !CONVERSION_EXITS type BOOL default C_BOOL-FALSE
      !FORMAT_OUTPUT type BOOL default C_BOOL-FALSE
      !HEX_AS_BASE64 type BOOL default C_BOOL-TRUE
    returning
      value(R_JSON) type JSON .

  methods SERIALIZE_INT
    redefinition .
protected section.

  methods ZPRETTY_NAME
    importing
      !IN type CSEQUENCE
    returning
      value(OUT) type STRING .
  methods ZDUMP_INT
    importing
      !DATA type DATA
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR optional
      !CONVEXIT type STRING optional
      !LEVEL type I default 0
    returning
      value(R_JSON) type JSON .
  methods ZGET_SYMBOLS_STRUCT
  final
    importing
      !TYPE_DESCR type ref to CL_ABAP_STRUCTDESCR
      !INCLUDE_ALIASES type ABAP_BOOL default ABAP_FALSE
      !DATA type ref to DATA optional
      !LEVEL type I default 0
    returning
      value(RESULT) type T_S_STRUCT_CACHE_RES .
private section.

  data MV_EXTENDED type BOOL .
  class-data MC_ME_TYPE type STRING .
  class-data MC_COV_ERROR type C .
ENDCLASS.



CLASS ZCL_UI2_CL_JSON IMPLEMENTATION.


METHOD serialize_int.
*CALL METHOD SUPER->SERIALIZE_INT
*  EXPORTING
*    DATA       =
**    name       =
**    type_descr =
*  RECEIVING
*    R_JSON     =
*    .

  " **********************************************************************
  " Usage examples and documentation can be found on SCN:
  " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
  " **********************************************************************  "

  DATA: lo_descr      TYPE REF TO cl_abap_typedescr,
        lo_elem_descr TYPE REF TO cl_abap_elemdescr,
        lv_convexit   TYPE string.

  IF type_descr IS INITIAL.
    lo_descr = cl_abap_typedescr=>describe_by_data( data ).
  ELSE.
    lo_descr = type_descr.
  ENDIF.

  IF mv_conversion_exits EQ abap_true AND lo_descr->kind EQ cl_abap_typedescr=>kind_elem.
    lo_elem_descr ?= lo_descr.
    lv_convexit = get_convexit_func( elem_descr = lo_elem_descr input = abap_false ).
  ENDIF.

*  r_json = dump_int( data = data type_descr = lo_descr convexit = lv_convexit ).
  r_json = zdump_int( data = data type_descr = lo_descr convexit = lv_convexit ).

  IF name IS NOT INITIAL AND ( mv_compress IS INITIAL OR r_json IS NOT INITIAL ).
    CONCATENATE `"` name `":` r_json INTO r_json.
  ENDIF.

ENDMETHOD.


METHOD ZDUMP_INT.

  DATA: lo_typedesc   TYPE REF TO cl_abap_typedescr,
        lo_elem_descr TYPE REF TO cl_abap_elemdescr,
        lo_classdesc  TYPE REF TO cl_abap_classdescr,
        lo_structdesc TYPE REF TO cl_abap_structdescr,
        lo_tabledescr TYPE REF TO cl_abap_tabledescr,
          ls_struct_sym TYPE t_s_struct_cache_res,
        lt_symbols    TYPE t_t_symbol,
          lt_keys       TYPE STANDARD TABLE OF REF TO data WITH DEFAULT KEY,
        lt_properties TYPE STANDARD TABLE OF string,
        lo_obj_ref    TYPE REF TO object,
        lo_data_ref   TYPE REF TO data,
        ls_skip_key   TYPE LINE OF abap_keydescr_tab,
        lv_array_opt  TYPE abap_bool,
          indent        TYPE string,
          lv_indent     LIKE indent,
          lv_level      LIKE level,
          lv_lb         TYPE string,
        lv_prop_name  TYPE string,
        lv_keyval     TYPE string,
        lv_itemval    TYPE string.

  FIELD-SYMBOLS: <line>   TYPE any,
                 <value>  TYPE any,
                 <data>   TYPE data,
                 <key>    TYPE LINE OF abap_keydescr_tab,
                   <symbol> TYPE t_s_symbol,
                 <table>  TYPE ANY TABLE.

    " increase hierarchy level
    lv_level = level + 1.

  " we need here macro instead of method calls because of the performance reasons.
  " based on SAT measurements.

  CASE type_descr->kind.
    WHEN cl_abap_typedescr=>kind_ref.

      IF data IS INITIAL.
        r_json = `null`.                                    "#EC NOTEXT
      ELSEIF type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
        lo_data_ref ?= data.
        lo_typedesc = cl_abap_typedescr=>describe_by_data_ref( lo_data_ref ).
        ASSIGN lo_data_ref->* TO <data>.
          r_json = dump_int( data = <data> type_descr = lo_typedesc level = level ).
      ELSE.
        lo_obj_ref ?= data.
        lo_classdesc ?= cl_abap_typedescr=>describe_by_object_ref( lo_obj_ref ).
          lt_symbols = get_symbols_class( type_descr = lo_classdesc object = lo_obj_ref ).
          r_json = dump_symbols( it_symbols = lt_symbols level = level ).
      ENDIF.

    WHEN cl_abap_typedescr=>kind_elem.
      lo_elem_descr ?= type_descr.
      dump_type data lo_elem_descr r_json convexit.

    WHEN cl_abap_typedescr=>kind_struct.

      lo_structdesc ?= type_descr.

*        ls_struct_sym = get_symbols_struct( type_descr = lo_structdesc level = level ).
        ls_struct_sym = zget_symbols_struct( type_descr = lo_structdesc level = level ).
        ASSIGN ls_struct_sym-data->* TO <data>.
        <data> = data.

        r_json = dump_symbols( it_symbols = ls_struct_sym-symbols level = level ).

    WHEN cl_abap_typedescr=>kind_table.

      lo_tabledescr ?= type_descr.
      lo_typedesc = lo_tabledescr->get_table_line_type( ).

      ASSIGN data TO <table>.

        IF mv_format_output EQ abap_true.
          indent = get_indent( level ).
          lv_indent = get_indent( lv_level ).
        ENDIF.

      " optimization for structured tables
      IF lo_typedesc->kind EQ cl_abap_typedescr=>kind_struct.
        lo_structdesc ?= lo_typedesc.

*          ls_struct_sym = get_symbols_struct( type_descr = lo_structdesc level = level ).
          ls_struct_sym = Zget_symbols_struct( type_descr = lo_structdesc level = level ).
          ASSIGN ls_struct_sym-data->* TO <line>.

        " here we have differentiation of output of simple table to JSON array
        " and sorted or hashed table with unique key into JSON associative array
        IF lo_tabledescr->has_unique_key IS NOT INITIAL AND mv_assoc_arrays IS NOT INITIAL.

          IF lo_tabledescr->key_defkind EQ lo_tabledescr->keydefkind_user.
            LOOP AT lo_tabledescr->key ASSIGNING <key>.
                READ TABLE ls_struct_sym-symbols WITH KEY name = <key>-name ASSIGNING <symbol>.
                APPEND <symbol>-value TO lt_keys.
            ENDLOOP.
          ENDIF.

          IF lines( lo_tabledescr->key ) EQ 1.
            READ TABLE lo_tabledescr->key INDEX 1 INTO ls_skip_key.
              DELETE ls_struct_sym-symbols WHERE name EQ ls_skip_key-name.
            " remove object wrapping for simple name-value tables
              IF mv_assoc_arrays_opt EQ abap_true AND lines( ls_struct_sym-symbols ) EQ 1.
              lv_array_opt = abap_true.
            ENDIF.
          ENDIF.

          LOOP AT <table> INTO <line>.
              CLEAR: lv_prop_name.
              " construct key attribute name
            IF lo_tabledescr->key_defkind EQ lo_tabledescr->keydefkind_user.
                LOOP AT lt_keys INTO lo_data_ref.
                  ASSIGN lo_data_ref->* TO <value>.
                lv_keyval = <value>.
                CONDENSE lv_keyval.
                IF lv_prop_name IS NOT INITIAL.
                  CONCATENATE lv_prop_name mc_key_separator lv_keyval INTO lv_prop_name.
                ELSE.
                  lv_prop_name = lv_keyval.
                ENDIF.
              ENDLOOP.
            ELSE.
                LOOP AT ls_struct_sym-symbols ASSIGNING <symbol>.
                ASSIGN <symbol>-value->* TO <value>.
                lv_keyval = <value>.
                CONDENSE lv_keyval.
                IF lv_prop_name IS NOT INITIAL.
                  CONCATENATE lv_prop_name mc_key_separator lv_keyval INTO lv_prop_name.
                ELSE.
                  lv_prop_name = lv_keyval.
                ENDIF.
              ENDLOOP.
            ENDIF.

              lv_itemval = dump_symbols( it_symbols = ls_struct_sym-symbols opt_array = lv_array_opt format_scope = abap_false level = lv_level ).
              IF lv_array_opt EQ abap_true.
                IF mv_format_output EQ abap_true AND lv_itemval IS NOT INITIAL.
                  CONCATENATE `"` lv_prop_name `": ` lv_itemval INTO lv_itemval.
                ELSE.
                  CONCATENATE `"` lv_prop_name `":` lv_itemval INTO lv_itemval.
                ENDIF.
            ELSE.
                IF mv_format_output EQ abap_true AND lv_itemval IS NOT INITIAL.
                  CONCATENATE `"` lv_prop_name `": {` lv_itemval lv_indent `}` INTO lv_itemval.
                ELSE.
                  CONCATENATE `"` lv_prop_name `":{` lv_itemval `}` INTO lv_itemval.
                ENDIF.
            ENDIF.
            APPEND lv_itemval TO lt_properties.

          ENDLOOP.

            format_list_output `{` lt_properties `}` r_json.

        ELSE.
          LOOP AT <table> INTO <line>.
              lv_itemval = dump_symbols( it_symbols = ls_struct_sym-symbols level = lv_level ).
            APPEND lv_itemval TO lt_properties.
          ENDLOOP.

            format_list_output `[` lt_properties `]` r_json.

        ENDIF.
      ELSE.
        LOOP AT <table> ASSIGNING <value>.
            lv_itemval = dump_int( data = <value> type_descr = lo_typedesc level = lv_level ).
          APPEND lv_itemval TO lt_properties.
        ENDLOOP.

          format_list_output `[` lt_properties `]` r_json.

      ENDIF.

  ENDCASE.

ENDMETHOD.                    "dump


  METHOD zget_symbols_struct.

    DATA: comp_tab     TYPE cl_abap_structdescr=>component_table,
          sym_cache    LIKE result,
          symbol       TYPE t_s_symbol,
          struct_descr TYPE REF TO cl_abap_structdescr,
          struct_cache LIKE LINE OF mt_struct_cache.

    FIELD-SYMBOLS: <comp>   LIKE LINE OF comp_tab,
                   <symbol> LIKE symbol,
                   <cache>  LIKE LINE OF mt_name_mappings,
                   <struct> LIKE LINE OF mt_struct_cache,
                   <data>   TYPE data,
                   <field>  TYPE any.

    READ TABLE mt_struct_cache WITH TABLE KEY type_descr = type_descr include_aliases = include_aliases level = level
    ASSIGNING <struct>.
    IF sy-subrc IS NOT INITIAL.
      struct_cache-type_descr       = type_descr.
      struct_cache-include_aliases  = include_aliases.
      struct_cache-level            = level.

      CREATE DATA struct_cache-result-data TYPE HANDLE type_descr.
      INSERT struct_cache INTO TABLE mt_struct_cache ASSIGNING <struct>.
      ASSIGN <struct>-result-data->* TO <data>.

      comp_tab = type_descr->get_components( ).

      LOOP AT comp_tab ASSIGNING <comp>.
        IF <comp>-name IS NOT INITIAL AND
          ( <comp>-as_include EQ abap_false OR include_aliases EQ abap_true OR mv_expand_includes EQ abap_false ).
          symbol-name = <comp>-name.
          symbol-type = <comp>-type.
          IF symbol-type->kind EQ cl_abap_typedescr=>kind_elem.
            symbol-elem_type ?= symbol-type.
          ELSE.
            CLEAR symbol-elem_type.
          ENDIF.
          IF mv_conversion_exits EQ abap_true AND symbol-elem_type IS NOT INITIAL.
            symbol-convexit_in = get_convexit_func( elem_descr = symbol-elem_type input = abap_true ).
            symbol-convexit_out = get_convexit_func( elem_descr = symbol-elem_type input = abap_false ).
          ENDIF.
          is_compressable symbol-type symbol-name symbol-compressable.
          ASSIGN COMPONENT symbol-name OF STRUCTURE <data> TO <field>.
          GET REFERENCE OF <field> INTO symbol-value.
          format_name symbol-name mv_pretty_name symbol-header.
          CONCATENATE `"` symbol-header `":` INTO symbol-header.
          IF mv_format_output EQ abap_true.
            CONCATENATE symbol-header ` ` INTO symbol-header.
          ENDIF.
          APPEND symbol TO <struct>-result-symbols.
        ENDIF.
        IF <comp>-as_include EQ abap_true AND mv_expand_includes EQ abap_true.
          struct_descr ?= <comp>-type.
          sym_cache = get_symbols_struct( type_descr = struct_descr include_aliases = include_aliases ).
          LOOP AT sym_cache-symbols INTO symbol.
            CONCATENATE symbol-name <comp>-suffix INTO symbol-name.
            IF symbol-type->kind EQ cl_abap_typedescr=>kind_elem.
              symbol-elem_type ?= symbol-type.
            ELSE.
              CLEAR symbol-elem_type.
            ENDIF.
            IF mv_conversion_exits EQ abap_true AND symbol-elem_type IS NOT INITIAL.
              symbol-convexit_in = get_convexit_func( elem_descr = symbol-elem_type input = abap_true ).
              symbol-convexit_out = get_convexit_func( elem_descr = symbol-elem_type input = abap_false ).
            ENDIF.
            is_compressable symbol-type symbol-name symbol-compressable.
            ASSIGN COMPONENT symbol-name OF STRUCTURE <data> TO <field>.
            GET REFERENCE OF <field> INTO symbol-value.
            format_name symbol-name mv_pretty_name symbol-header.
            CONCATENATE `"` symbol-header `":` INTO symbol-header.
            IF mv_format_output EQ abap_true.
              CONCATENATE symbol-header ` ` INTO symbol-header.
            ENDIF.
            APPEND symbol TO <struct>-result-symbols.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.

    result = <struct>-result.

    IF data IS BOUND AND data NE <struct>-result-data.
      result-data = data.
      ASSIGN data->* TO <data>.
      LOOP AT result-symbols ASSIGNING <symbol>.
        ASSIGN COMPONENT <symbol>-name OF STRUCTURE <data> TO <field>.
        GET REFERENCE OF <field> INTO <symbol>-value.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "GET_SYMBOLS_STRUCT


METHOD ZPRETTY_NAME.

  DATA: tokens TYPE TABLE OF char128,
        cache  LIKE LINE OF mt_name_mappings.

  FIELD-SYMBOLS: <token> LIKE LINE OF tokens,
                 <cache> LIKE LINE OF mt_name_mappings.

  READ TABLE mt_name_mappings WITH TABLE KEY abap = in ASSIGNING <cache>.
  IF sy-subrc IS INITIAL.
    out = <cache>-json.
  ELSE.
    out = in.

    REPLACE ALL OCCURRENCES OF `__` IN out WITH `*`.

    TRANSLATE out TO LOWER CASE.
    TRANSLATE out USING `/_:_~_`.
    SPLIT out AT `_` INTO TABLE tokens.
    LOOP AT tokens ASSIGNING <token>. " FROM 2.
      TRANSLATE <token>(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF tokens INTO out.
    REPLACE ALL OCCURRENCES OF `*` IN out WITH `_`.

    cache-abap  = in.
    cache-json = out.
    INSERT cache INTO TABLE mt_name_mappings.
    INSERT cache INTO TABLE mt_name_mappings_ex.
  ENDIF.

ENDMETHOD.                    "pretty_name


METHOD zserialize.

  " **********************************************************************
  " Usage examples and documentation can be found on SCN:
  " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
  " **********************************************************************  "

  CONSTANTS: lc_method TYPE string VALUE `SERIALIZE_INT`.
*  CONSTANTS: lc_method TYPE string VALUE `ZSERIALIZE_INT`.

  DATA: lo_json  TYPE REF TO object.

*    CREATE OBJECT lo_json TYPE (mc_me_type)
  CREATE OBJECT lo_json TYPE zcl_ui2_cl_json
    EXPORTING
      compress         = compress
      pretty_name      = pretty_name
      name_mappings    = name_mappings
      assoc_arrays     = assoc_arrays
      assoc_arrays_opt = assoc_arrays_opt
      expand_includes  = expand_includes
      numc_as_string   = numc_as_string
      conversion_exits = conversion_exits
      format_output    = format_output
      hex_as_base64    = hex_as_base64
      ts_as_iso8601    = ts_as_iso8601.

  CALL METHOD lo_json->(lc_method)
    EXPORTING
      name       = name
      data       = data
      type_descr = type_descr
    RECEIVING
      r_json     = r_json.

ENDMETHOD.                    "serialize
ENDCLASS.
