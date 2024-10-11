*&---------------------------------------------------------------------*
*&  Include           ZWMREP0090_CLS
*&---------------------------------------------------------------------*

CLASS lcl_alv DEFINITION
  FINAL
  CREATE PUBLIC .
*  CLASS me
*  MESSAGE ID zwmmsg001.

  PUBLIC SECTION.
    CLASS-METHODS:
      inicialization,
      set_lock CHANGING cv_keyword TYPE keyword-keyword OPTIONAL,
      remove_lock CHANGING cv_keyword TYPE keyword-keyword OPTIONAL,
      show_progress IMPORTING iv_perc TYPE any
                              iv_text TYPE any,
      get_variante IMPORTING iv_varia          TYPE disvariant-variant OPTIONAL
                   RETURNING VALUE(rv_variant) TYPE disvariant-variant,

      get_dados,
      init_tree,
      init_icons,
      icon_create IMPORTING iv_icon        TYPE any
                            iv_info        TYPE any OPTIONAL
                  RETURNING VALUE(rv_icon) TYPE salv_de_tree_image,
      ucomm_refresh,
      ucomm_status_carga,
      ucomm_alv IMPORTING iv_tipo TYPE c.

    TYPES: ty_dados TYPE zwm_s001.



    TYPES: BEGIN OF ty_vbfa,
             vbelv     TYPE vbfa-vbelv,
             posnv     TYPE vbfa-posnv,
             rfmng_sum TYPE vbfa-rfmng,
             meins     TYPE vbfa-meins,
           END OF ty_vbfa.

    TYPES: BEGIN OF ty_ekbe,
             ebeln     TYPE ekbe-ebeln,
             ebelp     TYPE ekbe-ebelp,
             menge_sum TYPE ekbe-menge,
             meins     TYPE ekpo-meins,
           END OF ty_ekbe.

    TYPES: BEGIN OF ty_vbpa_adrc,
             vbeln      TYPE vbpa-vbeln,
             posnr      TYPE vbpa-posnr,
             parvw      TYPE vbpa-parvw,
             kunnr      TYPE vbpa-kunnr,
             lifnr      TYPE vbpa-lifnr,
             adrnr      TYPE vbpa-adrnr,
             addrnumber TYPE adrc-addrnumber,
             date_from  TYPE adrc-date_from,
             nation     TYPE adrc-nation,
             date_to    TYPE adrc-date_to,
             title      TYPE adrc-title,
             name1      TYPE adrc-name1,
             name2      TYPE adrc-name2,
             name3      TYPE adrc-name3,
             name4      TYPE adrc-name4,
             name_text  TYPE adrc-name_text,
             name_co    TYPE adrc-name_co,
             city1      TYPE adrc-city1,
             city2      TYPE adrc-city2,
             city_code  TYPE adrc-city_code,
             cityp_code TYPE adrc-cityp_code,
             home_city  TYPE adrc-home_city,
             cityh_code TYPE adrc-cityh_code,
             chckstatus TYPE adrc-chckstatus,
             regiogroup TYPE adrc-regiogroup,
             post_code1 TYPE adrc-post_code1,
             country    TYPE adrc-country,
           END OF ty_vbpa_adrc.

    TYPES: BEGIN OF ty_tvfptz,
             tplst  TYPE tvfptz-tplst,
             tdlnr  TYPE tvfptz-tdlnr,
             vsart  TYPE tvfptz-vsart,
             land1  TYPE tvfptz-land1,
             fpstlz TYPE tvfptz-fpstlz,
             tpstlz TYPE tvfptz-tpstlz,
             trfzn  TYPE tvfptz-trfzn,
             bezei  TYPE tvftzt-bezei,
           END OF ty_tvfptz.

    TYPES: BEGIN OF ty_zroutyn_t01_i,
             ntrans  TYPE zroutyn_t01_i-ntrans,
             itrans  TYPE zroutyn_t01_i-itrans,
             ltrans  TYPE zroutyn_t01_i-ltrans,
             doc     TYPE zroutyn_t01_i-doc,
             item    TYPE zroutyn_t01_i-item,
             doctype TYPE zroutyn_t01_i-doctype,
             vbeln   TYPE zroutyn_t01_i-vbeln,
             posnr   TYPE zroutyn_t01_i-posnr,
             refnr   TYPE zroutyn_t01_i-refnr,
             ernam   TYPE zroutyn_t01_i-ernam,
             erdat   TYPE zroutyn_t01_i-erdat,
             erzet   TYPE zroutyn_t01_i-erzet,
             seq     TYPE zroutyn_t01_i-seq,
           END OF ty_zroutyn_t01_i.

    TYPES: BEGIN OF ty_ztransp_h,
             idt         TYPE ztransp_h-idt,
             ntrans      TYPE ztransp_h-ntrans,
             tknum       TYPE ztransp_h-tknum,
             desbloquear TYPE ztransp_h-desbloquear,
             ddtext      TYPE ztransp_h-ddtext,
             zonadist    TYPE ztransp_h-zonadist,
             tndrst      TYPE ztransp_h-tndrst,
             netwr       TYPE zcust_trsp-netwr,
           END OF ty_ztransp_h.
    TYPES: BEGIN OF lty_grupo_paletes,
             refnr   TYPE lvs_refnr,
             paletes TYPE zpalete_picking_tt,
           END OF lty_grupo_paletes,
           lty_grupo_paletes_table TYPE TABLE OF lty_grupo_paletes.
    TYPES: BEGIN OF y_pal_totais,
*             vkorg TYPE zwm_s001-vkorg,
*             tknum TYPE zwm_s001-tknum,
*             refnr TYPE zwm_s001-refnr,
             palcp   TYPE zwm_s001-palcp,
             palpk   TYPE zwm_s001-palpk,
             palto   TYPE zwm_s001-palto,
             palpk_s TYPE zwm_s001-palpk_s,
             palto_s TYPE zwm_s001-palto_s,
           END OF y_pal_totais,
*           yt_pal_totais TYPE TABLE OF y_pal_totais WITH DEFAULT KEY.

           BEGIN OF y_vkorg_tot,
             vkorg  TYPE zwm_s001-vkorg,
*             INCLUDE TYPE y_pal_tot.
             totais TYPE y_pal_totais,
           END OF y_vkorg_tot,
           yt_vkorg_tot TYPE TABLE OF y_vkorg_tot WITH DEFAULT KEY,

           BEGIN OF y_tknum_tot,
             tknum  TYPE zwm_s001-tknum,
             totais TYPE y_pal_totais,
           END OF y_tknum_tot,
           yt_tknum_tot TYPE TABLE OF y_tknum_tot WITH DEFAULT KEY,

           BEGIN OF y_refnr_tot,
             refnr  TYPE zwm_s001-refnr,
             totais TYPE y_pal_totais,
           END OF y_refnr_tot,
           yt_refnr_tot TYPE TABLE OF y_refnr_tot WITH DEFAULT KEY,

           BEGIN OF y_pal_sum,
             head  TYPE y_pal_totais,
             vkorg TYPE yt_vkorg_tot,
             tknum TYPE yt_tknum_tot,
             refnr TYPE yt_refnr_tot,
           END OF y_pal_sum.

    CLASS-DATA: gs_variant TYPE disvariant,
*                gt_dados   TYPE STANDARD TABLE OF ty_dados.
                gt_alv     TYPE STANDARD TABLE OF ty_dados,
                gt_dados   TYPE STANDARD TABLE OF zwm_s001,
                BEGIN OF gs_icon,
                  falta_comp   TYPE salv_de_tree_image,
                  finalizada   TYPE salv_de_tree_image,
                  falta_parc   TYPE salv_de_tree_image,
                  lock_one     TYPE salv_de_tree_image,
                  lock_two     TYPE salv_de_tree_image,
                  lock_three   TYPE salv_de_tree_image,
                  lock_four    TYPE salv_de_tree_image,
                  lock_five    TYPE salv_de_tree_image,
                  transp_atu   TYPE salv_de_tree_image,
                  select_all   TYPE salv_de_tree_image,
                  deselect_all TYPE salv_de_tree_image,
*                  BEGIN OF tndrst,
*                    empty TYPE salv_de_tree_image,
*                    nw    TYPE salv_de_tree_image,
*                    ac    TYPE salv_de_tree_image,
*                    aw    TYPE salv_de_tree_image,
*                    rj    TYPE salv_de_tree_image,
*                    cf    TYPE salv_de_tree_image,
*                    ex    TYPE salv_de_tree_image,
*                    cn    TYPE salv_de_tree_image,
*                  END OF tndrst,
                END OF gs_icon,
*                gt_vbfa           TYPE TABLE OF ty_vbfa,
*                gt_ekbe           TYPE TABLE OF ty_ekbe,
                gt_vbpa_adrc      TYPE TABLE OF ty_vbpa_adrc,
                gt_tvfptz         TYPE TABLE OF ty_tvfptz,
                gt_ztransp_h      TYPE TABLE OF ty_ztransp_h,
                gr_abgru          TYPE RANGE OF vbap-abgru,
                gt_grupo_palestes TYPE lty_grupo_paletes_table,
                gt_zroutyn_t01_i  TYPE STANDARD TABLE OF ty_zroutyn_t01_i,
                gt_zwm006_aux     TYPE STANDARD TABLE OF zwm006_aux.
    CLASS-DATA: gr_container TYPE REF TO cl_gui_custom_container,
                gr_tree      TYPE REF TO cl_salv_tree.
*    CLASS-DATA: gr_tree  type ref to cl_gui_alv_tree.
    CLASS-DATA: gs_pal_sum  TYPE y_pal_sum,
                gv_tipo_alv.
    CLASS-DATA: gr_table_2     TYPE REF TO cl_salv_table.


  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: gt_fieldcatalog TYPE lvc_t_fcat, "Fieldcatalog.
                gv_columnname   TYPE lvc_fname,
                go_columns      TYPE REF TO cl_salv_columns_tree,
                go_cols         TYPE REF TO cl_salv_columns_table,
                go_aggregations TYPE REF TO cl_salv_aggregations.
    CLASS-METHODS:
      build_fieldcatalog,
      create_tree,
      register_events,
*      build_header RETURNING VALUE(rs_header) TYPE treev_hhdr,
      build_header,
      supply_data,
      set_layout,
      set_total_carga_vkorg IMPORTING iv_key  TYPE salv_de_node_key
                                      iv_cont TYPE i,
      add_first_line IMPORTING is_data       TYPE ty_dados
                     RETURNING VALUE(rv_key) TYPE salv_de_node_key ,
      add_vkorg_line IMPORTING is_data       TYPE ty_dados
                               iv_key        TYPE salv_de_node_key
                     RETURNING VALUE(rv_key) TYPE salv_de_node_key ,
      add_new_line IMPORTING is_data       TYPE ty_dados
                             iv_key        TYPE salv_de_node_key
                             iv_text_field TYPE fieldname
                   RETURNING VALUE(rv_key) TYPE salv_de_node_key,
      set_columnname IMPORTING iv_columnname TYPE lvc_fname,
      set_col_position IMPORTING iv_position TYPE i,
      set_title_text IMPORTING iv_text TYPE any,
      valida_add_doc_itm IMPORTING iv_doc TYPE lips-vgbel
                                   iv_itm TYPE any "lips-vgpos
                                   iv_qtd TYPE any "kwmeng
                                   is_str TYPE any,

      valida_complete_data CHANGING cs_data       TYPE ty_dados,

      add_soma_pal IMPORTING iv_palcomp TYPE any
                             iv_palpick TYPE any
                   CHANGING  cs_pal_sum TYPE y_pal_totais.

ENDCLASS.


*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §4.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING ir_tree TYPE REF TO cl_salv_tree OPTIONAL,
*                            io_alv  TYPE ref to lcl_alv,

      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function sender,

      on_link_click FOR EVENT link_click OF cl_salv_events_tree
        IMPORTING columnname  "Type LVC_FNAME
                  node_key,	  "Type	SALV_DE_NODE_KEY

      on_link_click_2 FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column sender,

      on_double_click FOR EVENT double_click OF cl_salv_events_tree
        IMPORTING node_key   "Type  SALV_DE_NODE_KEY
                  columnname, "Type	LVC_FNAME
      on_checkbox_change FOR EVENT checkbox_change OF cl_salv_events_tree
        IMPORTING columnname
                  node_key
                  checked,

      on_before_user_command FOR EVENT before_salv_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_after_user_command FOR EVENT after_salv_function OF cl_salv_events
        IMPORTING e_salv_function,

      get_node_level IMPORTING node_key            TYPE salv_de_node_key
                     RETURNING VALUE(rv_hierlevel) TYPE i,

      on_click IMPORTING columnname   TYPE lvc_fname
                         node_key     TYPE  salv_de_node_key
                         double_click TYPE bkk_yesno OPTIONAL,
      desbloquear IMPORTING ir_node TYPE REF TO cl_salv_node,
      ucomm_fornecer,
      ucomm_desbloq,
      ucomm_select_all IMPORTING iv_check TYPE abap_bool,
      select_all       IMPORTING iv_check TYPE abap_bool
                                 ir_node  TYPE REF TO cl_salv_node.

*    DATA: gr_tree TYPE REF TO cl_salv_tree.
*          go_alv  TYPE REF TO lcl_alv.

ENDCLASS.                    "lcl_handle_events DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* §4.2 implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD constructor.
*    gr_tree = ir_tree.
  ENDMETHOD.

  METHOD on_checkbox_change.

    TRY.
        DATA(lr_node) = lcl_alv=>gr_tree->get_nodes( )->get_node( node_key ).
        DATA(lv_check) = lr_node->get_hierarchy_item( )->is_checked( ).
        CASE get_node_level( node_key ).
          WHEN 4.
            LOOP AT lr_node->get_children( )
                INTO DATA(lr_matnr_lvl).
              lr_matnr_lvl-node->get_hierarchy_item( )->set_checked( lv_check ).
            ENDLOOP.
          WHEN 5.
            IF lv_check EQ abap_false.
              lr_node->get_parent( )->get_hierarchy_item( )->set_checked( lv_check ).
            ELSE.
              DATA(lv_all) = abap_true.
              LOOP AT lr_node->get_parent( )->get_children( ) INTO DATA(ls_matnr_lvl).
                IF ls_matnr_lvl-node->get_hierarchy_item( )->is_checked( ) EQ abap_false.
                  lv_all = abap_false.
                  EXIT.
                ENDIF.
              ENDLOOP.
              IF lv_all EQ abap_true.
                lr_node->get_parent( )->get_hierarchy_item( )->set_checked( abap_true ).
              ENDIF.
            ENDIF.
        ENDCASE.
      CATCH cx_salv_msg.    "
    ENDTRY.

  ENDMETHOD.

  METHOD desbloquear.

    FIELD-SYMBOLS <lfs_alv> TYPE zwm_s001.

    "referencia ZTPRP2_B_F01 ln 263 FORM user_command3 + p_selfield-fieldname = 'ICON'.
    DATA(lv_icon) = ir_node->get_item( 'ICON_DESB' )->get_icon( ).
    IF lv_icon IS INITIAL OR
       lv_icon EQ icon_green_light.
      EXIT.
    ENDIF.

    DATA(ls_data_row) = ir_node->get_data_row( ).
    ASSIGN ls_data_row->* TO <lfs_alv>.

    SELECT SINGLE *
      INTO @DATA(aux_header)
      FROM ztransp_h
      WHERE "idt   eq <lfs_alv>-idt and
            tknum EQ @<lfs_alv>-tknum.
    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ENDIF.

    IF <lfs_alv>-desbloquear = ' '.
      IF <lfs_alv>-zonadist NE '**********' AND
         <lfs_alv>-zonadist NE '++++++++++'.
        <lfs_alv>-desbloquear = 'X'.
        lv_icon   = icon_green_light.
      ENDIF.
    ENDIF.

    IF lv_icon NE icon_green_light.
      EXIT.
    ENDIF.

    aux_header-desbloquear = <lfs_alv>-desbloquear.
    CONCATENATE sy-datum sy-uzeit INTO aux_header-changedon.
    MODIFY ztransp_h FROM aux_header.
*          lr_node->set_data_row( <lfs_alv> ).

    ir_node->get_item( 'ICON_DESB' )->set_icon( lv_icon ).

  ENDMETHOD.

  METHOD on_click.

*    data: ls_alv type zwm_s001.
    FIELD-SYMBOLS <lfs_alv> TYPE zwm_s001.
    TRY.
        DATA(lr_node) = lcl_alv=>gr_tree->get_nodes( )->get_node( node_key ).
      CATCH cx_root.
        EXIT.
    ENDTRY.

    DATA(ls_data_row) = lr_node->get_data_row( ).
    ASSIGN ls_data_row->* TO <lfs_alv>.
    IF <lfs_alv> IS ASSIGNED.
      CASE columnname.
        WHEN '&Hierarchy' OR ''.
*        BREAK-POINT.
          TRY.
              CASE get_node_level( node_key ).
                WHEN 1. "Organização
                  IF lr_node->get_hierarchy_item( )->get_icon( ) EQ lcl_alv=>gs_icon-select_all.
                    DATA(lv_checked) = abap_true.
                  ELSE.
                    lv_checked = abap_false.
                  ENDIF.

                  select_all( iv_check = lv_checked
                              ir_node  = lr_node ).

                WHEN 2. "Transporte
                  SET PARAMETER ID 'TNR' FIELD <lfs_alv>-tknum.
****                  CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
****                  CALL TRANSACTION 'VI04' AND SKIP FIRST SCREEN.
***                  DATA lr_tknum TYPE RANGE OF vttk-tknum.
***                  lr_tknum = VALUE #( sign = 'I' option = 'EQ' ( low = <lfs_alv>-tknum ) ).
***                  SUBMIT rv56trsl
***                    WITH k_tknum IN lr_tknum "<lfs_alv>-tknum
***                     AND RETURN.
                  IF double_click EQ abap_true.
                    CALL TRANSACTION 'VT02N' AND SKIP FIRST SCREEN.
                  ELSE.
                    CALL TRANSACTION 'VI01' AND SKIP FIRST SCREEN.
                  ENDIF.
                WHEN 3. "Grupo
                  SET PARAMETER ID 'GRN' FIELD <lfs_alv>-refnr.
                  CALL TRANSACTION 'VG02' AND SKIP FIRST SCREEN.
                WHEN 4. "Guias
                  SET PARAMETER ID 'VL' FIELD <lfs_alv>-rbnum.
                  CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
                WHEN 5. "Material
                  SET PARAMETER ID 'MAT' FIELD <lfs_alv>-matnr.
                  CALL TRANSACTION 'MMBE'.
                WHEN OTHERS.
              ENDCASE.
            CATCH cx_salv_msg.    " .
            CATCH cx_root.
          ENDTRY.
*gr_tree->get_.
        WHEN 'ICON_DESB'.
          desbloquear( lr_node ).
*          "referencia ZTPRP2_B_F01 ln 263 FORM user_command3 + p_selfield-fieldname = 'ICON'.
*          DATA(lv_icon) = lr_node->get_item( 'ICON_DESB' )->get_icon( ).
*          IF lv_icon EQ icon_green_light.
*            EXIT.
*          ENDIF.
*
*          IF <lfs_alv>-desbloquear = ' '.
*            IF <lfs_alv>-zonadist NE '**********' AND
*               <lfs_alv>-zonadist NE '++++++++++'.
*              <lfs_alv>-desbloquear = 'X'.
*              lv_icon   = icon_green_light.
*            ENDIF.
*          ENDIF.
*
*          IF lv_icon NE icon_green_light.
*            EXIT.
*          ENDIF.
*
*          SELECT SINGLE *
*            INTO @DATA(aux_header)
*            FROM ztransp_h
*            WHERE "idt   eq <lfs_alv>-idt and
*                  tknum EQ @<lfs_alv>-tknum.
*          aux_header-desbloquear = <lfs_alv>-desbloquear.
*          CONCATENATE sy-datum sy-uzeit INTO aux_header-changedon.
*          MODIFY ztransp_h FROM aux_header.
**          lr_node->set_data_row( <lfs_alv> ).
*
*          lr_node->get_item( 'ICON_DESB' )->set_icon( lv_icon ).

        WHEN 'TNDRRC_T'.
          IF <lfs_alv>-tknum IS NOT INITIAL.
            lcl_alv=>show_progress( iv_perc = 5 iv_text = text-u01 ). "A atualizar Status da proposta
            CALL FUNCTION 'ZTP_PORT_ON_UPDATE_SHIPMENT'
              EXPORTING
                tknum      = <lfs_alv>-tknum
                force_updt = 'X'.
            WAIT UP TO 3 SECONDS.
            SELECT SINGLE tndrrc,
                          dd07t~ddtext
              FROM vttk
                       LEFT OUTER JOIN dd07t
                       ON  domname    EQ 'TNDRRC'
                       AND ddlanguage EQ 'P'
                       AND as4local   EQ 'A'
                       AND domvalue_l EQ vttk~tndrrc
              INTO @DATA(ls_stt_prop)
              WHERE tknum EQ @<lfs_alv>-tknum.
            IF ls_stt_prop-tndrrc NE <lfs_alv>-tndrrc.
              <lfs_alv>-tndrrc   = ls_stt_prop-tndrrc.
              <lfs_alv>-tndrrc_t = ls_stt_prop-ddtext.
              lr_node->set_data_row( <lfs_alv> ).
            ENDIF.
          ENDIF.

        WHEN OTHERS.
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD on_link_click_2.

*    IF lcl_alv=>gv_tipo_alv EQ 1.
*      READ TABLE lcl_alv=>gt_dados INTO DATA(ls_linha) INDEX row.
*    ELSE.
*      READ TABLE lcl_alv=>gt_alv INTO ls_linha INDEX row.
*    ENDIF.
    DATA ls_linha TYPE zwm_s001.
    READ TABLE <gft_alv2> INTO ls_linha INDEX row.

*    DATA(lo_metadata) = lcl_alv=>gr_table_2->get_metadata( ).

    CASE column.
      WHEN 'VBELN'.
        SELECT SINGLE vbeln
          FROM vbak
          INTO @DATA(lv_vbeln)
          WHERE vbeln EQ @ls_linha-vbeln.
        IF sy-subrc IS INITIAL.
          SET PARAMETER ID 'AUN' FIELD ls_linha-vbeln.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ELSE.
          SET PARAMETER ID 'VL' FIELD ls_linha-vbeln.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'VGBEL'.
        SET PARAMETER ID 'AUN' FIELD ls_linha-vgbel.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      WHEN 'TKNUM'. "Transporte
        SET PARAMETER ID 'TNR' FIELD ls_linha-tknum.
*        IF double_click EQ abap_true.
        CALL TRANSACTION 'VT02N' AND SKIP FIRST SCREEN.
*        ELSE.
*        CALL TRANSACTION 'VI01' AND SKIP FIRST SCREEN.
*        ENDIF.
      WHEN 'REFNR'. "Grupo
        SET PARAMETER ID 'GRN' FIELD ls_linha-refnr.
        CALL TRANSACTION 'VG02' AND SKIP FIRST SCREEN.
      WHEN 'RBNUM'. "Guias
        SET PARAMETER ID 'VL' FIELD ls_linha-rbnum.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
      WHEN 'MATNR'. "Material
        SET PARAMETER ID 'MAT' FIELD ls_linha-matnr.
        CALL TRANSACTION 'MMBE'.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD on_link_click.

    on_click(
      EXPORTING
        columnname   = columnname
        node_key     = node_key
        double_click = abap_false
*        sender     =
    ).

  ENDMETHOD.

  METHOD on_double_click.
***    BREAK-POINT.
*    on_link_click(
    on_click(
      EXPORTING
        columnname   = columnname
        node_key     = node_key
        double_click = abap_true
*        sender     =
    ).
  ENDMETHOD.

  METHOD get_node_level.
*    DATA(lv_parent1) = node_key.
*    DATA(lr_node) = gr_tree->get_nodes( )->get_node( node_key ).
    DATA(lr_node) = lcl_alv=>gr_tree->get_nodes( )->get_node( node_key ).

    DATA(lr_parent) = lr_node->get_parent( ).
    WHILE lr_parent->get_key( ) NE cl_alv_tree_base=>c_virtual_root_node.
      lr_node      = lr_parent.
      rv_hierlevel = rv_hierlevel + 1.
      lr_parent    = lr_node->get_parent( ).
    ENDWHILE.
  ENDMETHOD.

  METHOD on_user_command.
**    BREAK-POINT.
    CASE e_salv_function.
      WHEN 'FORNECER'.
        ucomm_fornecer( ).
      WHEN 'REFRESH'.
*        go_alv->ucomm_refresh( ).
        lcl_alv=>ucomm_refresh( ).
      WHEN 'STATUS_CARGA'.
        lcl_alv=>ucomm_status_carga( ).
      WHEN 'SELECT_ALL'.
        ucomm_select_all( abap_true ).
      WHEN 'DESELECT_ALL'.
        ucomm_select_all( abap_false ).
*      WHEN 'UCOMM_ALV'.
*        lcl_alv=>ucomm_alv( ).
      WHEN 'DESBLOQ'.
        ucomm_desbloq( ).
      WHEN OTHERS.
        IF e_salv_function(9) EQ 'UCOMM_ALV'.
          lcl_alv=>ucomm_alv( e_salv_function+9 ).
        ENDIF.
    ENDCASE.
*      lcl_alv=>gr_tree->get_selections( )->set_selected_columns( value = VALUE #( ) ).

*    perform show_function_info using e_salv_function text-i08.
  ENDMETHOD.                    "on_user_command

  METHOD ucomm_select_all.

*    BREAK-POINT.
    TRY.
*        DATA(nodes) = lcl_alv=>gr_tree->get_nodes( ).
*        DATA(tree) = nodes->get_all_nodes( ).
*        DATA(lr_top_node) = nodes->get_top_node( ).
*        DATA(lt_top_node_children) = lr_top_node->get_children( ).
*        LOOP AT lcl_alv=>gr_tree->get_nodes( )->get_top_node( )->get_children( ) INTO DATA(ls_tknum_lvl).
**          LOOP AT ls_top_child-node->get_children( ) INTO DATA(ls_tknum_lvl).
*            LOOP AT ls_tknum_lvl-node->get_first_child( )->get_children( ) INTO DATA(ls_rbnum_lvl).
*              ls_rbnum_lvl-node->get_hierarchy_item( )->set_checked( iv_check ).
*            ENDLOOP.
**          ENDLOOP.
*        ENDLOOP.

        DATA(lr_top_node) = lcl_alv=>gr_tree->get_nodes( )->get_top_node( ).
        LOOP AT lr_top_node->get_children( ) INTO DATA(ls_vkort_lvl).
          select_all( iv_check = iv_check
                      ir_node  = ls_vkort_lvl-node ). "lr_top_node ).
        ENDLOOP.
*        DO.
*          select_all( iv_check = iv_check
*                      ir_node  = lr_top_node ).
*          RETURN.
*          IF iv_check EQ abap_true.
*            lr_top_node->get_hierarchy_item( )->set_icon( lcl_alv=>gs_icon-deselect_all  ).
*          ELSE.
*            lr_top_node->get_hierarchy_item( )->set_icon( lcl_alv=>gs_icon-select_all  ).
*          ENDIF.
*          LOOP AT lr_top_node->get_children( ) INTO DATA(ls_tknum_lvl).
*            LOOP AT ls_tknum_lvl-node->get_first_child( )->get_children( ) INTO DATA(ls_rbnum_lvl).
*              ls_rbnum_lvl-node->get_hierarchy_item( )->set_checked( iv_check ).
*              LOOP AT ls_rbnum_lvl-node->get_children( ) INTO DATA(ls_mantr_lvl).
*                ls_mantr_lvl-node->get_hierarchy_item( )->set_checked( iv_check ).
*              ENDLOOP.
*            ENDLOOP.
*          ENDLOOP.
*          TRY.
*              lr_top_node = lr_top_node->get_next_sibling( ).
*            CATCH cx_salv_msg.  "
*              EXIT.
*          ENDTRY.
*        ENDDO.

      CATCH cx_salv_msg.    "
      CATCH cx_root.    "
    ENDTRY.

  ENDMETHOD.

  METHOD select_all.
    TRY.
*        LOOP AT ir_node->get_children( ) INTO DATA(ls_vkort_lvl).
        IF iv_check EQ abap_true.
          ir_node->get_hierarchy_item( )->set_icon( lcl_alv=>gs_icon-deselect_all  ).
        ELSE.
          ir_node->get_hierarchy_item( )->set_icon( lcl_alv=>gs_icon-select_all  ).
        ENDIF.
*          LOOP AT ls_vkort_lvl-node->get_children( ) INTO DATA(ls_tknum_lvl).
        LOOP AT ir_node->get_children( ) INTO DATA(ls_tknum_lvl).
          LOOP AT ls_tknum_lvl-node->get_children( ) INTO DATA(ls_refnr_lvl).
            LOOP AT ls_refnr_lvl-node->get_children( ) INTO DATA(ls_rbnum_lvl).
              ls_rbnum_lvl-node->get_hierarchy_item( )->set_checked( iv_check ).
              LOOP AT ls_rbnum_lvl-node->get_children( ) INTO DATA(ls_mantr_lvl).
                ls_mantr_lvl-node->get_hierarchy_item( )->set_checked( iv_check ).
              ENDLOOP.
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.
*        ENDLOOP.
      CATCH cx_salv_msg.    "
      CATCH cx_root.    "
    ENDTRY.

  ENDMETHOD.

  METHOD ucomm_desbloq.
    DATA(lt_sel) = lcl_alv=>gr_tree->get_selections( )->get_selected_nodes( ).
    LOOP AT lt_sel INTO DATA(ls_sel).
*      ls_sel-key
*      ls_sel-node
      desbloquear( ls_sel-node ).
    ENDLOOP.
    REFRESH lt_sel.
    lcl_alv=>gr_tree->get_selections( )->set_selected_nodes( lt_sel ).

  ENDMETHOD.

  METHOD ucomm_fornecer.
    FIELD-SYMBOLS <lfs_alv>       TYPE zwm_s001.
    FIELD-SYMBOLS <lfs_alv_child> TYPE zwm_s001.
*    DATA lt_doc_list TYPE tt_vbeln.

    TYPES lty_item_data_spl TYPE STANDARD TABLE OF /spe/bapiobdlvitemchg.
    DATA lt_item_data_spl TYPE lty_item_data_spl.

    TYPES: BEGIN OF lty_doc,
             rbnum         TYPE t311a-rbnum,
             vbeln         TYPE vbap-vbeln,
*             posnr         TYPE vbap-posnr,
*             abgru         TYPE vbap-abgru,
             ebeln         TYPE ekpo-ebeln,
*             ebelp         TYPE ekpo-ebelp,
*             loekz         TYPE ekpo-loekz,
*             elikz         TYPE ekpo-elikz,
             "BAPI_OUTB_DELIVERY_CHANGE
             item_data     TYPE bapiobdlvitemchg_t,
             item_control  TYPE bapiobdlvitemctrlchg_t,
             item_data_spl LIKE lt_item_data_spl,
             "BAPI_DELIVERYPROCESSING_EXEC
             request       TYPE /spe/request_t,
             createditems  TYPE /spe/bapidelicreateditems_tab,
             "bapis ret
             return        TYPE bapiret2_t,
           END OF lty_doc.
    DATA lt_doc_list TYPE STANDARD TABLE OF lty_doc.

* get all existing nodes
    TRY.
*        DATA(nodes) = gr_tree->get_nodes( ).
        DATA(nodes) = lcl_alv=>gr_tree->get_nodes( ).
        DATA(tree) = nodes->get_all_nodes( ).

      CATCH cx_salv_msg.
        EXIT.
    ENDTRY.

* get selected docs
    LOOP AT tree INTO DATA(leaf).
      DATA(item) = leaf-node->get_hierarchy_item( ).
      IF item->is_checked( ) EQ abap_true.
        DATA(ls_data_row) = leaf-node->get_data_row( ).
        ASSIGN ls_data_row->* TO <lfs_alv>.
        IF <lfs_alv>-lvl NE 4.
          CONTINUE.
        ENDIF.
*        "get list of children
*        DATA(lt_children) = leaf-node->get_children( ).
*        LOOP AT lt_children INTO DATA(lo_child).
*          ls_data_row = lo_child-node->get_data_row( ).
*          ASSIGN ls_data_row->* TO <lfs_alv_child>.

*        <lfs_alv_child> = <lfs_alv>.
        ASSIGN <lfs_alv> TO <lfs_alv_child>.
        DATA(lo_child)  = leaf.

        READ TABLE lt_doc_list
          ASSIGNING FIELD-SYMBOL(<lfs_doc>)
            WITH KEY rbnum = <lfs_alv_child>-rbnum.
        IF sy-subrc IS NOT INITIAL.
          APPEND INITIAL LINE TO lt_doc_list ASSIGNING <lfs_doc>.
        ENDIF.
        <lfs_doc> = CORRESPONDING #( BASE ( <lfs_doc> ) <lfs_alv_child> ).
*          IF <lfs_alv_child>-falta_parc EQ abap_true.
        DATA(lv_icon) = lo_child-node->get_item( 'STATUS_IC' )->get_icon( ).
        IF lv_icon EQ lcl_alv=>gs_icon-falta_parc.
          "BAPI_OUTB_DELIVERY_CHANGE
          APPEND VALUE #( deliv_numb       = <lfs_alv_child>-vbeln
                          deliv_item       = <lfs_alv_child>-posnr
*                          dlv_qty          = <lfs_alv_child>-kwmeng
                          dlv_qty          = <lfs_alv_child>-qtd_dif + <lfs_alv_child>-lfimg
                          base_uom         = <lfs_alv_child>-vrkme
                          fact_unit_nom    = 1
                          fact_unit_denom  = 1
                           ) TO <lfs_doc>-item_data.
          APPEND VALUE #( deliv_numb = <lfs_alv_child>-vbeln
                          deliv_item = <lfs_alv_child>-posnr
                          chg_delqty = abap_true
                          del_item   = abap_false
                           ) TO <lfs_doc>-item_control.
*        APPEND VALUE #( deliv_numb  = <lfs_alv>-vgbel "vbeln
*                        deliv_item  = <lfs_alv>-vgpos "posnr
*                        stge_loc    = <lfs_alv>-lgort "???
*                         ) TO <lfs_doc>-item_data_spl.
*          ELSEIF <lfs_alv>-falta_comp EQ abap_true.
        ELSEIF lv_icon EQ lcl_alv=>gs_icon-falta_comp.
          "BAPI_DELIVERYPROCESSING_EXEC
*L  Fornecimento
*B  Pedido
*A  Ordem do cliente
*1  Ordem do cliente em sistema externo
*2  Pedido em sistema externo
          APPEND VALUE #( plant                     = <lfs_alv_child>-werks
                          stge_loc                  = <lfs_alv_child>-lgort
                          document_numb             = <lfs_alv_child>-vbeln
                          document_item             = <lfs_alv_child>-posnr
                          document_type             = 'A' "Ordem do cliente "'B' "Pedido
                          document_type_predecessor = 'A' "Ordem do cliente "'B' "Pedido
                          delivery_date             = sy-datum
                          transp_plan_date          = sy-datum
                          loading_date              = sy-datum
                          goods_issue_date          = sy-datum
                          material                  = <lfs_alv_child>-matnr
                          quantity_base__uom        = <lfs_alv_child>-kwmeng
                          base_uom                  = <lfs_alv_child>-vrkme
                        ) TO <lfs_doc>-request.
        ENDIF.
*      ENDLOOP.
**********************************************************************
**... § remove the checked nodes
**            TRY.
**                leaf-node->delete( ).
**              CATCH cx_salv_msg.
**            ENDTRY.
      ENDIF.
    ENDLOOP.

    DATA: lt_bdcdata TYPE tab_bdcdata.
    DATA: lt_messtab TYPE tab_bdcmsgcoll.
    DATA lv_mode VALUE 'E'. "'N'.

* process selected docs
    DATA lt_collected_return TYPE bapiret2_t.
    LOOP AT lt_doc_list INTO DATA(ls_doc).
      ASSIGN ls_doc-return TO FIELD-SYMBOL(<lft_return>).
      REFRESH lt_bdcdata.
      REFRESH lt_messtab.
*********************
*      Falta Parcial
      IF ls_doc-item_data IS NOT INITIAL.
        APPEND VALUE #( type       = 'I'
                        id         = 'VL'
                        number     = '001'
                        message    = ''
                        message_v1 = |---> LOG Falta Parcial: { ls_doc-rbnum }|
                      ) TO lt_collected_return.
*        CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
*          EXPORTING
*            header_data    = VALUE bapiobdlvhdrchg( deliv_numb = ls_doc-rbnum )
*            header_control = VALUE bapiobdlvhdrctrlchg( deliv_numb = ls_doc-rbnum )
*            delivery       = ls_doc-vbeln
*          TABLES
*            item_data      = ls_doc-item_data
*            item_control   = ls_doc-item_control
*            return         = <lft_return>
*            item_data_spl  = ls_doc-item_data_spl
*          EXCEPTIONS
*            error_message  = 95
*            OTHERS         = 99.
*        READ TABLE <lft_return> TRANSPORTING NO FIELDS
*          WITH KEY type = 'E'.
*        IF sy-subrc IS NOT INITIAL.
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*            EXPORTING
*              wait = abap_true.
*          IF <lft_return> IS INITIAL.
*            APPEND VALUE #( type       = 'S'
*                            id         = 'VL'
*                            number     = '749'
*                            message_v1 = ls_doc-rbnum
*                          ) TO <lft_return>.
*          ENDIF.
*        ENDIF.
*        APPEND LINES OF <lft_return> TO lt_collected_return.

        lt_bdcdata = VALUE #(
        "tela inicial
          ( program = 'SAPMV50A' dynpro = '4004' dynbegin = abap_true )
          ( fnam  = 'BDC_OKCODE' fval  = '/00' )
          ( fnam  = 'LIKP-VBELN' fval  = ls_doc-rbnum ) ).
        LOOP AT ls_doc-item_data INTO DATA(ls_itm).
*          DATA(lv_qty) = |{ ls_itm-dlv_qty NUMBER = USER }|.
          APPEND VALUE:
            "Posicionar
            #( program = 'SAPMV50A' dynpro = '1000' dynbegin = abap_true ) TO lt_bdcdata,
            #( fnam  = 'BDC_OKCODE' fval  = '=POPO_T' ) TO lt_bdcdata,
            "popup posicionar item + enter
            #( program = 'SAPMV50A' dynpro = '0111' dynbegin = abap_true ) TO lt_bdcdata,
            #( fnam  = 'BDC_OKCODE' fval  = '=WEIT' ) TO lt_bdcdata,
            #( fnam  = 'RV50A-POSNR' fval  = ls_itm-deliv_item ) TO lt_bdcdata,
            "mudar valor linha
            #( program = 'SAPMV50A' dynpro = '1000' dynbegin = abap_true ) TO lt_bdcdata,
            #( fnam  = 'BDC_OKCODE' fval  = '/00' ) TO lt_bdcdata,
            #( fnam  = 'LIPSD-G_LFIMG(01)' fval  = |{ ls_itm-dlv_qty NUMBER = USER ALIGN = LEFT }| ) TO lt_bdcdata.
        ENDLOOP.
        APPEND VALUE:
          "Salvar
          #( program = 'SAPMV50A' dynpro = '1000' dynbegin = abap_true ) TO lt_bdcdata,
          #( fnam  = 'BDC_OKCODE' fval  = '=SICH_T' ) TO lt_bdcdata.
        CALL TRANSACTION 'VL02N'
          USING lt_bdcdata
            MODE lv_mode
                MESSAGES INTO lt_messtab.

        LOOP AT lt_messtab INTO DATA(ls_messtab).
          APPEND VALUE #( type       = ls_messtab-msgtyp
                          id         = ls_messtab-msgid
                          number     = ls_messtab-msgnr
                          message_v1 = ls_messtab-msgv1
                          message_v2 = ls_messtab-msgv2
                          message_v3 = ls_messtab-msgv3
                          message_v4 = ls_messtab-msgv4
                        ) TO <lft_return>.
        ENDLOOP.

        READ TABLE <lft_return> TRANSPORTING NO FIELDS
          WITH KEY type = 'E'.

        IF sy-subrc IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          IF <lft_return> IS INITIAL.
            APPEND VALUE #( type       = 'S'
                            id         = 'VL'
                            number     = '749'
                            message_v1 = ls_doc-rbnum
                          ) TO <lft_return>.
          ENDIF.
        ENDIF.
        APPEND LINES OF <lft_return> TO lt_collected_return.

      ENDIF.
*********************
*      Falta Total
      IF ls_doc-item_data IS NOT INITIAL.
        WAIT UP TO 3 SECONDS.
      ENDIF.

      IF ls_doc-request IS NOT INITIAL.
        APPEND VALUE #( type       = 'I'
                        id         = 'VL'
                        number     = '001'
                        message    = ''
                        message_v1 = |---> LOG Falta Total: { ls_doc-rbnum }|
                      ) TO lt_collected_return.
***        CALL FUNCTION 'BAPI_DELIVERYPROCESSING_EXEC'
****         EXPORTING
****           DELIVERY_EXTEND         =
****           TECHN_CONTROL           =
***          TABLES
***            request       = ls_doc-request
****           PARTNER       =
****           PARTNER_ADDR  =
****           TEXT_HEADER   =
****           TEXT_LINES    =
***            createditems  = ls_doc-createditems
***            return        = <lft_return>
****           EXTENSION1    =
****           EXTENSION2    =
****           CO_CHAR_VALUES          =
****           BATCH_ATTRIBUTES        =
****           BATCH_VALUES_CHAR       =
****           BATCH_VALUES_CURR       =
****           BATCH_VALUES_NUM        =
****           TOKENREFERENCE          =
****           EXTENSIONIN   =
****           ITEM_SERIAL_NO          =
***          EXCEPTIONS
***            error_message = 95
***            OTHERS        = 99.
***        IF <lft_return> IS INITIAL AND sy-msgid IS NOT INITIAL.
***          APPEND VALUE #( type     = sy-msgty
***                          id         = sy-msgid
***                          number     = sy-msgno
***                          message    = ''
***                          message_v1 = sy-msgv1
***                          message_v2 = sy-msgv2
***                          message_v3 = sy-msgv3
***                          message_v4 = sy-msgv4
***                         ) TO <lft_return>.
***        ENDIF.

        READ TABLE ls_doc-request INTO DATA(ls_req) INDEX 1.
        SELECT SINGLE vdatu
          FROM vbak
          INTO @DATA(lv_vdatu)
          WHERE vbeln EQ @ls_req-document_numb.
        DATA lv_vdatu_bdc TYPE bdc_fval.
        WRITE lv_vdatu TO lv_vdatu_bdc.
        lt_bdcdata = VALUE #(
        "tela inicial
          ( program = 'SAPMV50A' dynpro = '4004' dynbegin = abap_true )
          ( fnam  = 'BDC_OKCODE' fval  = '/00' )
          ( fnam  = 'LIKP-VBELN' fval  = ls_doc-rbnum )
          "fornecer ordem
          ( program = 'SAPMV50A' dynpro = '1000' dynbegin = abap_true )
          ( fnam  = 'BDC_OKCODE' fval  = '=RAUF_T' )
          "popup ordem
          ( program = 'SAPMV50A' dynpro = '0105' dynbegin = abap_true )
          ( fnam  = 'BDC_OKCODE' fval  = '=ENT1' )
*          ( fnam  = 'LV50C-DATBI' fval  = lv_vdatu )
          ( fnam  = 'LV50C-DATBI' fval  = lv_vdatu_bdc )
          ( fnam  = 'LV50C-VBELN' fval  = ls_req-document_numb )
          "Salvar
          ( program = 'SAPMV50A' dynpro = '1000' dynbegin = abap_true )
          ( fnam  = 'BDC_OKCODE' fval  = 'SICH_T' )
         ).
        CALL TRANSACTION 'VL02N'
          USING lt_bdcdata
            MODE lv_mode
                MESSAGES INTO lt_messtab.

        LOOP AT lt_messtab INTO ls_messtab.
          APPEND VALUE #( type       = ls_messtab-msgtyp
                          id         = ls_messtab-msgid
                          number     = ls_messtab-msgnr
                          message_v1 = ls_messtab-msgv1
                          message_v2 = ls_messtab-msgv2
                          message_v3 = ls_messtab-msgv3
                          message_v4 = ls_messtab-msgv4
                        ) TO <lft_return>.
        ENDLOOP.

        READ TABLE <lft_return> TRANSPORTING NO FIELDS
          WITH KEY type = 'E'.

        IF sy-subrc IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          IF <lft_return> IS INITIAL.
            APPEND VALUE #( type       = 'S'
                            id         = 'VL'
                            number     = '749'
                            message_v1 = ls_doc-rbnum
                          ) TO <lft_return>.
          ENDIF.
        ENDIF.
        APPEND LINES OF <lft_return> TO lt_collected_return.
      ENDIF.
    ENDLOOP.

    IF lt_collected_return IS NOT INITIAL.
      CALL FUNCTION 'C14ALD_BAPIRET2_SHOW'
        TABLES
          i_bapiret2_tab = lt_collected_return.

      READ TABLE lt_collected_return TRANSPORTING NO FIELDS
        WITH KEY type = 'S'.
      IF sy-subrc IS INITIAL.
        lcl_alv=>ucomm_refresh( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD on_before_user_command.
*    BREAK-POINT.
*      lcl_alv=>gr_tree->get_selections( )->set_selected_columns( value = VALUE #( ) ).
*    perform show_function_info using e_salv_function text-i09.
  ENDMETHOD.                    "on_before_user_command

  METHOD on_after_user_command.
*    BREAK-POINT.
*    lcl_alv=>gr_tree->get_selections( )->set_selected_columns( value = VALUE #( ) ).
*    perform show_function_info using e_salv_function text-i10.
  ENDMETHOD.                    "on_after_user_command

ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
