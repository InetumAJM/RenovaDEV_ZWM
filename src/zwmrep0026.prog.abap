************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0026                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Quadro Folha de Carga                                    *
* Criado por: Luís Rocha                                               *
* Criado em.: 17/11/2004                                               *
* Tipo PRG..: Report ALV Tree                                          *
************************************************************************

REPORT zwmrep0026 NO STANDARD PAGE HEADING.

CLASS cl_gui_column_tree DEFINITION LOAD.
CLASS cl_gui_cfw         DEFINITION LOAD.

DATA tree1      TYPE REF TO cl_gui_alv_tree.
DATA mr_toolbar TYPE REF TO cl_gui_toolbar.

INCLUDE <icon>.

TABLES: likp,
        kna1,
        ltap,
        ltak,
        t300.

TABLES: zwm013,
        zwm026,
        zwm040,
        zwm028.

***** variáveis e tabelas internas

DATA: t_ltak LIKE ltak OCCURS 0 WITH HEADER LINE,
      t_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.

DATA: gs_zwm028 LIKE zwm028.

DATA: BEGIN OF tab_ger OCCURS 0,
         refnr          LIKE zwm028-refnr,
         ordem          LIKE zwm028-ordem,
         remessa        LIKE zwm028-remessa,
      END OF tab_ger,
      reg_ger LIKE tab_ger,
      key_ger LIKE tab_ger.

DATA: BEGIN OF tab_pal OCCURS 0,
        remessa        LIKE ltap-vbeln,
        sscc           LIKE zwm013-sscc,
        destino        LIKE zwm013-destino,
        posicao_pulmao LIKE zwm013-posicao_pulmao,
      END OF tab_pal.

DATA: BEGIN OF tab_out OCCURS 0,
         refnr          LIKE zwm028-refnr,
         ordem          LIKE zwm028-ordem,
         remessa        LIKE zwm028-remessa,
         destino        LIKE zwm013-destino,
         posicao_pulmao LIKE zwm013-posicao_pulmao,
         sscc           LIKE zwm013-sscc,
         nome_cliente   LIKE kna1-name1,
      END OF tab_out.

TYPES: t_out LIKE tab_out.

DATA: itens TYPE lvc_t_indx.

DATA: linhas TYPE i,
      aux_node TYPE lvc_index.

DATA: percentagem TYPE p DECIMALS 2,
      indice_tab TYPE lvc_index.

*data : begin of gt_zdialog_rf_001_t occurs 0.
*        include structure zdialog_rf_001_t.
*data : end of gt_zdialog_rf_001_t.

DATA: grupo LIKE zwm028-refnr,
      lock(1),
      prioridade(3),
      ok_code_0002 LIKE sy-ucomm,
      rad_opt1(1),
      rad_opt2(1).

DATA: aux_s_date(10).

DATA: gt_fieldcatalog     TYPE lvc_t_fcat,
      gt_sort             TYPE lvc_t_sort,
      gt_out              TYPE t_out OCCURS 0,
      gs_out              TYPE t_out,
      gs_out_save         TYPE t_out.

DATA:   g_repid            LIKE sy-repid.
DATA:   ok_code            LIKE sy-ucomm,   "OK-Code
        g_user_command     TYPE slis_formname
                                       VALUE 'USER_COMMAND',
        g_status           TYPE slis_formname
                                       VALUE 'STANDARD_FULLSCREEN',
        grid               TYPE REF TO cl_gui_alv_grid,
        g_custom_container TYPE REF TO cl_gui_custom_container,
        selected(1)                    VALUE 'X',
        total_itens        TYPE i,
        total_completed    TYPE i.

DATA: g_top_node     TYPE lvc_nkey,
      g_top_node_key TYPE lvc_nkey.

DATA: aux_refnr LIKE t311-refnr.

DATA: g_flag_change,
      g_flag_lock.

DATA: g_selected_node TYPE	lvc_nkey,
      g_fieldname     TYPE	lvc_fname.

DATA: g_selected_nodes TYPE lvc_t_nkey .

DATA: not_first_time.

DATA: flag_tree.

*---------------------------------------------------------------------*
*       CLASS lcl_toolbar_event_receiver DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_toolbar_event_receiver DEFINITION.


  PUBLIC SECTION.
    METHODS: on_function_selected
               FOR EVENT function_selected OF cl_gui_toolbar
                 IMPORTING fcode.

*             on_toolbar_dropdown
*               for event dropdown_clicked of cl_gui_toolbar
*                 importing fcode
*                           posx
*                           posy.

ENDCLASS.                    "lcl_toolbar_event_receiver DEFINITION
*
*---------------------------------------------------------------------*
*       CLASS lcl_toolbar_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_toolbar_event_receiver IMPLEMENTATION.
*
  METHOD on_function_selected.
    CASE fcode.
      WHEN 'TOP'.
*       Primeira posição
        PERFORM altera_posicao USING 0.
      WHEN 'UP'.
*       Sobe uma posição
        PERFORM altera_posicao USING 1.
      WHEN 'DOWN'.
*       Baixa uma posição
        PERFORM altera_posicao USING 2.
      WHEN 'END'.
*       Última posição
        PERFORM altera_posicao USING 3.
    ENDCASE.
  ENDMETHOD.                    "on_function_selected
*
*  method on_toolbar_dropdown.
** create contextmenu
*    data: l_menu type ref to cl_ctmenu,
*          l_fc_handled type as4flag.
*
*    create object l_menu.
*    clear l_fc_handled.
*
*    case fcode.
*      when 'INSERT_LC'.
*        l_fc_handled = 'X'.
**       insert as last child
*        call method l_menu->add_function
*                exporting fcode   = 'INSERT_LC'
*                text    = 'Insert New Line as Last Child'.
**       insert as first child
*        call method l_menu->add_function
*                exporting fcode   = 'INSERT_FC'
*                text    = 'Insert New Line as First Child'.
**       insert as next sibling
*        call method l_menu->add_function
*                exporting fcode   = 'INSERT_NS'
*                text    = 'Insert New Line as Next Sibling'.
**       insert as last sibling
*        call method l_menu->add_function
*                exporting fcode   = 'INSERT_LS'
*                text    = 'Insert New Line as Last Sibling'.
**       insert as first sibling
*        call method l_menu->add_function
*                exporting fcode   = 'INSERT_FS'
*              text    = 'Insert New Line as First Sibling'.
*    endcase.
*
** show dropdownbox
*    if l_fc_handled = 'X'.
*      call method mr_toolbar->track_context_menu
*        exporting
*            context_menu = l_menu
*            posx         = posx
*            posy         = posy.
*    endif.
*
*  endmethod.                    "on_toolbar_dropdown
*
ENDCLASS.                    "lcl_toolbar_event_receiver IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_tree_event_receiver DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_tree_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS handle_item_double_click
      FOR EVENT item_double_click OF cl_gui_alv_tree
      IMPORTING node_key
                fieldname.

    METHODS handle_button_click
      FOR EVENT button_click OF cl_gui_alv_tree
      IMPORTING node_key
                fieldname.

    METHODS handle_link_click
      FOR EVENT link_click OF cl_gui_alv_tree
      IMPORTING node_key
                fieldname.

    METHODS handle_header_click
      FOR EVENT header_click OF cl_gui_alv_tree
      IMPORTING fieldname.

ENDCLASS.                    "lcl_tree_event_receiver DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_tree_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_tree_event_receiver IMPLEMENTATION.

  METHOD handle_item_double_click.
    PERFORM event_double_click.
  ENDMETHOD.                    "handle_item_double_click

  METHOD handle_button_click.
  ENDMETHOD.                    "handle_button_click

  METHOD handle_link_click.
  ENDMETHOD.                    "handle_link_click

  METHOD handle_header_click.
  ENDMETHOD.                    "handle_header_click

ENDCLASS.                    "lcl_tree_event_receiver IMPLEMENTATION

DATA: toolbar_event_receiver TYPE REF TO lcl_toolbar_event_receiver.


********************* SELECTION SCREEN *********************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS: p_lgnum LIKE mlgn-lgnum OBLIGATORY MEMORY ID lgn,
            p_refnr LIKE zwm028-refnr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

********************* END OF SELECTION SCREEN *********************

INITIALIZATION.
  DATA: xuser LIKE lrf_wkqu OCCURS 0 WITH HEADER LINE.
* Read the user data from table lrf_wkqu ( for all the warehouses)
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = xuser
    EXCEPTIONS
      no_entry_found = 01.
  IF sy-subrc = 0.
    READ TABLE xuser INDEX 1.
    MOVE xuser-lgnum TO p_lgnum.
  ENDIF.


START-OF-SELECTION.

  PERFORM valida_parametros_entrada.
  IF sy-subrc <> 0.
*   ERRO: Nº Depósito Inválido !
    MESSAGE s146(zwmmsg001).
    EXIT.
  ENDIF.

  PERFORM select_data.

  IF tab_out[] IS INITIAL.
*   Não existem dados para processar !
    MESSAGE s147(zwmmsg001).
    EXIT.
  ENDIF.

  CLEAR not_first_time.

END-OF-SELECTION.

  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
*       process before output
*----------------------------------------------------------------------*
MODULE pbo OUTPUT.

  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'GERAL'.
  IF tree1 IS INITIAL.
    PERFORM init_tree.
  ENDIF.
  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.                             " PBO  OUTPUT

*---------------------------------------------------------------------*
*  MODULE status_0050 OUTPUT
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
MODULE status_0050 OUTPUT.
  CLEAR tree1.
  LEAVE TO SCREEN 100.
ENDMODULE.                    "status_0200 OUTPUT

*---------------------------------------------------------------------*
*  MODULE status_0100 OUTPUT
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'GERAL'.
  IF tree1 IS INITIAL.
    PERFORM init_tree.
  ENDIF.
  IF flag_tree IS INITIAL.
    PERFORM cria_hierarquia.
    flag_tree = 'X'.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.                    "status_0100 OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       process after input
*----------------------------------------------------------------------*
MODULE pai INPUT.

  CASE ok_code.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      PERFORM exit_program.

    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.
  ENDCASE.
  CLEAR ok_code.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.                             " PAI  INPUT

*---------------------------------------------------------------------*
*  MODULE USER_COMMAND_0050 INPUT
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
MODULE user_command_0050 INPUT.
  CLEAR tree1.
  CALL SCREEN 100.
ENDMODULE.                    "USER_COMMAND_0050 INPUT


*---------------------------------------------------------------------*
*  MODULE user_command_0100 INPUT
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: selected_node TYPE lvc_nkey,
        item_name     TYPE lvc_fname.

  CASE ok_code.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      PERFORM exit_program.
    WHEN 'SAVE'.
      PERFORM actualiza_zwm028.
    WHEN 'PRN'.
      PERFORM imprime_folha_carga.
    WHEN 'REF'.
      CLEAR flag_tree.
      not_first_time = 'X'.

    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.

  ENDCASE.
  CLEAR ok_code.
*  call method cl_gui_cfw=>flush.

ENDMODULE.                    "user_command_0100 INPUT

*&---------------------------------------------------------------------*
*&      Form  exit_program
*&---------------------------------------------------------------------*
*       free object and leave program
*----------------------------------------------------------------------*
FORM exit_program.

  PERFORM verifica_alteracoes USING g_flag_change.
  IF NOT g_flag_change IS INITIAL.
    CLEAR g_flag_change.
    PERFORM popup USING text-m23 ' '  ' '
                        text-m29 ' '  text-m25
                        g_flag_change.
    CHECK g_flag_change = 'J'.
  ENDIF.

  CALL METHOD tree1->free.
  SET SCREEN 0. LEAVE SCREEN.

ENDFORM.                               " exit_program

*&---------------------------------------------------------------------*
*&      Form  init_tree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree.

  DATA: lt_expand_nodes TYPE lvc_t_nkey.

  DATA: l_toolbar_excluding TYPE ui_functions.

* create fieldcatalog
  PERFORM build_fieldcatalog.

*  perform ajusta_propriedades.

* create container for alv-tree
  DATA: l_tree_container_name(30) TYPE c,
        l_custom_container TYPE REF TO cl_gui_custom_container.

  l_tree_container_name = 'TREE1'.

  IF sy-batch IS INITIAL.
    CREATE OBJECT l_custom_container
      EXPORTING
            container_name = l_tree_container_name
      EXCEPTIONS
            cntl_error                  = 1
            cntl_system_error           = 2
            create_error                = 3
            lifetime_error              = 4
            lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc <> 0.
*     ERRO: Create Container
      MESSAGE e148(zwmmsg001).
    ENDIF.
  ENDIF.

* create tree control
  CREATE OBJECT tree1
    EXPORTING
        parent              = l_custom_container
       node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
        item_selection      = 'X'
        no_html_header      = ''
        no_toolbar          = ''
    EXCEPTIONS
        cntl_error                   = 1
        cntl_system_error            = 2
        create_error                 = 3
        lifetime_error               = 4
        illegal_node_selection_mode  = 5
        failed                       = 6
        illegal_column_name          = 7.
  IF sy-subrc <> 0.
*   ERRO: Create Object Tree
    MESSAGE e149(zwmmsg001).
  ENDIF.

* create Hierarchy-header
  DATA l_hierarchy_header TYPE treev_hhdr.
  PERFORM build_hierarchy_header CHANGING l_hierarchy_header.

* create info-table for html-header
  DATA: lt_list_commentary TYPE slis_t_listheader,
        l_logo             TYPE sdydo_value.

  PERFORM build_comment USING
                 lt_list_commentary
                 l_logo.

* repid for saving variants
  DATA: ls_variant TYPE disvariant.
  ls_variant-report = sy-repid.

  PERFORM define_toolbar_excluding CHANGING l_toolbar_excluding.

  l_logo = 'RENOVA_LOGO'.

* create empty tree-control
  CALL METHOD tree1->set_table_for_first_display
    EXPORTING
      is_hierarchy_header  = l_hierarchy_header
      it_list_commentary   = lt_list_commentary
      i_logo               = l_logo
      i_background_id      = 'ALV_BACKGROUND'
      i_save               = 'A'
      is_variant           = ls_variant
      it_toolbar_excluding = l_toolbar_excluding
    CHANGING
      it_outtab            = gt_out "table must be empty !!
      it_fieldcatalog      = gt_fieldcatalog.

* register events
  PERFORM register_events.

* add own functioncodes to the toolbar
  PERFORM change_toolbar.

ENDFORM.                    "init_tree

*&--------------------------------------------------------------------*
*&      Form  change_toolbar
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM change_toolbar.

* get toolbar control
  CALL METHOD tree1->get_toolbar_object
    IMPORTING
      er_toolbar = mr_toolbar.

  CHECK NOT mr_toolbar IS INITIAL.

* add separator to toolbar
  CALL METHOD mr_toolbar->add_button
    EXPORTING
      fcode     = ''
      icon      = ''
      butn_type = cntb_btype_sep
      text      = ''
      quickinfo = 'Separador'.

* add Standard Button to toolbar (for Prioridade 0)
  CALL METHOD mr_toolbar->add_button
    EXPORTING
      fcode     = 'TOP'
      icon      = icon_total_up
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = 'Primeira'.

* add Standard Button to toolbar (for Prioridade - 10)
  CALL METHOD mr_toolbar->add_button
    EXPORTING
      fcode     = 'UP'
      icon      = icon_next_value
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = 'Sobe 1'.

* add Standard Button to toolbar (for Prioridade - 10)
  CALL METHOD mr_toolbar->add_button
    EXPORTING
      fcode     = 'DOWN'
      icon      = icon_previous_value
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = 'Baixa 1'.

* add Standard Button to toolbar (for Prioridade + 1)
  CALL METHOD mr_toolbar->add_button
    EXPORTING
      fcode     = 'END'
      icon      = icon_total_down
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = 'Última'.

* set event-handler for toolbar-control
  CREATE OBJECT toolbar_event_receiver.
  SET HANDLER toolbar_event_receiver->on_function_selected
                                                      FOR mr_toolbar.
*  set handler toolbar_event_receiver->on_toolbar_dropdown
*                                                      for mr_toolbar.

ENDFORM.                               " change_toolbar

*&--------------------------------------------------------------------*
*&      Form  cria_hierarquia
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM cria_hierarquia.

  CALL METHOD tree1->delete_all_nodes.

* create hierarchy
  PERFORM create_hierarchy.

* this method must be called to send the data to the frontend
*  call method tree1->frontend_update.

* Expand first level
  CALL METHOD tree1->expand_node
    EXPORTING
      i_node_key       = g_top_node_key
      i_expand_subtree = 'X'.

* Determina top_node
  CALL METHOD tree1->get_top_node
    IMPORTING
      e_node_key = g_top_node.

* adjust column_width
  CALL METHOD tree1->column_optimize.

  CHECK NOT reg_ger IS INITIAL.

  CLEAR g_selected_node.
  LOOP AT gt_out INTO gs_out.
    IF gs_out-refnr   = reg_ger-refnr   AND
       gs_out-ordem   = reg_ger-ordem   AND
       gs_out-remessa = reg_ger-remessa AND
       gs_out-sscc IS INITIAL.
      WRITE sy-tabix TO g_selected_node.
      g_selected_node = g_top_node_key + g_selected_node - 1.
      EXIT.
    ENDIF.
  ENDLOOP.
  CHECK NOT g_selected_node IS INITIAL.

  IF NOT g_fieldname IS INITIAL.
    CALL METHOD tree1->set_selected_item
      EXPORTING
        i_node_key  = g_selected_node
        i_fieldname = g_fieldname.
  ELSE.
    CLEAR g_selected_nodes.
    APPEND g_selected_node TO g_selected_nodes.
    CALL METHOD tree1->set_selected_nodes
      EXPORTING
        it_selected_nodes = g_selected_nodes.
  ENDIF.

ENDFORM.                    " init_tree

*&---------------------------------------------------------------------*
*&      Form  build_fieldcatalog
*&---------------------------------------------------------------------*
*       build fieldcatalog for structure t001
*----------------------------------------------------------------------*
FORM build_fieldcatalog.

  DATA: pos TYPE i VALUE 1.
  DATA: aux_cat TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE.

  REFRESH aux_cat.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'REFNR'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
*  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Grupo'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'ORDEM'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-edit          = 'X'.
  aux_cat-coltext       = 'Ordem.'.
  aux_cat-tooltip       = 'Ordem'.
*  aux_cat-outputlen     =  2.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'REMESSA'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
*  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Fornecimento'.
  aux_cat-tooltip       = 'Fornecimento'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'SSCC'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-coltext       = 'SSCC'.
  aux_cat-tooltip       = 'SSCC'.
*  aux_cat-outputlen     =  20.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'DESTINO'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
*  aux_cat-no_zero       = 'X'.
  aux_cat-coltext       = 'Pulmão'.
  aux_cat-tooltip       = 'Pulmão'.
*  aux_cat-outputlen     =  20.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'POSICAO_PULMAO'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-just          = 'X'.
  aux_cat-coltext       = 'Posição'.
  aux_cat-tooltip       = 'Posição no Pulmão'.
*  aux_cat-outputlen     =  20.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'NOME_CLIENTE'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
*  aux_cat-no_zero       = 'X'.
  aux_cat-coltext       = 'Nome'.
  aux_cat-tooltip       = 'Nome do Cliente'.
*  aux_cat-outputlen     =  20.
  APPEND aux_cat.
  ADD 1 TO pos.

*  clear aux_cat.
*  aux_cat-col_pos       =  pos.
*  aux_cat-fieldname     = 'STATUS'.
**  aux_cat-no_out        = 'X'.
*  aux_cat-icon          = 'X'.
*  aux_cat-coltext       = 'Status'.
*  aux_cat-tooltip       = 'Status da Linha'.
*  append aux_cat.
*  add 1 to pos.

  gt_fieldcatalog[] = aux_cat[].

ENDFORM.                               " build_fieldcatalog

*&--------------------------------------------------------------------*
*&      Form  ajusta_propriedades
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM ajusta_propriedades. " TABLES sort     TYPE slis_t_sortinfo_alv.

  DATA: pos TYPE i VALUE 1.

  DATA ls_sort_wa TYPE lvc_s_sort.

* Ordenação
  REFRESH gt_sort.

  CLEAR pos.

* create sort-table
*  add 1 to pos.
*  ls_sort_wa-spos = pos.
*  ls_sort_wa-fieldname = 'REFNR'.
*  ls_sort_wa-up = 'X'.
*  ls_sort_wa-subtot = 'X'.
*  append ls_sort_wa to gt_sort.

  ADD 1 TO pos.
  ls_sort_wa-spos = pos.
  ls_sort_wa-fieldname = 'REMESSA'.
  ls_sort_wa-up = 'X'.
  ls_sort_wa-subtot = 'X'.
  APPEND ls_sort_wa TO gt_sort.


ENDFORM.                    " AJUSTA_PROPRIEDADES

*&---------------------------------------------------------------------*
*&      Form  build_hierarchy_header
*&---------------------------------------------------------------------*
*       build hierarchy-header-information
*----------------------------------------------------------------------*
*      -->P_L_HIERARCHY_HEADER  strucxture for hierarchy-header
*----------------------------------------------------------------------*
FORM build_hierarchy_header CHANGING
                               p_hierarchy_header TYPE treev_hhdr.

  p_hierarchy_header-heading = 'Grupo/Fornecimento/SSCC'.
  p_hierarchy_header-tooltip =
                         'Hierarquia: Grupo/Fornecimento/SSCC'.
  p_hierarchy_header-width = 50.
  p_hierarchy_header-width_pix = ' '.

ENDFORM.                               " build_hierarchy_header

*&---------------------------------------------------------------------*
*&      Form  create_hierarchy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_hierarchy.

  DATA: ls_out  TYPE t_out,
        lsx_out TYPE t_out.

  DATA: l_refnr_key       TYPE lvc_nkey,
        l_vbeln_key       TYPE lvc_nkey,
        l_last_key        TYPE lvc_nkey.

  IF not_first_time = 'X'.
    PERFORM select_data.
  ENDIF.

  REFRESH gt_out.

  CLEAR: tab_out, ls_out.
  LOOP AT tab_out INTO ls_out.
    AT NEW refnr.
*    on change of ls_out-all.
      PERFORM add_refnr_line USING ls_out
                                   ''
                          CHANGING l_refnr_key.
      g_top_node_key = l_refnr_key.
*    endon.
    ENDAT.
    lsx_out = ls_out.
    AT NEW remessa.
*    on change of ls_out-refnr.
      PERFORM add_vbeln_line USING lsx_out
                                   l_refnr_key
                          CHANGING l_vbeln_key.
*    endon.
    ENDAT.
    PERFORM add_complete_line USING ls_out
                                    l_vbeln_key
                           CHANGING l_last_key.
  ENDLOOP.

  not_first_time = 'X'.
ENDFORM.                    "create_hierarchy

*&--------------------------------------------------------------------*
*&      Form  add_refnr_line
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->PS_OUT     text
*      -->P_RELAT_KEYtext
*      -->P_NODE_KEY text
*---------------------------------------------------------------------*
FORM add_refnr_line USING     ps_out TYPE t_out
                               p_relat_key TYPE lvc_nkey
                     CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.
*  ls_item_layout-t_image = '@3P@'.
  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
*  ls_item_layout-style   =
*                        cl_gui_column_tree=>style_intensifd_critical.
  APPEND ls_item_layout TO lt_item_layout.

  MOVE ps_out-refnr TO ls_out-refnr.

* add node
  l_node_text =  ps_out-refnr.
  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    "add_all_line

*&--------------------------------------------------------------------*
*&      Form  add_vbeln_line
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->PS_OUT     text
*      -->P_RELAT_KEYtext
*      -->P_NODE_KEY text
*---------------------------------------------------------------------*
FORM add_vbeln_line USING  ps_out TYPE t_out
                           p_relat_key TYPE lvc_nkey
                 CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.
  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
*  if ps_out-kzdru is initial.
*    move '@06@' to ls_item_layout-t_image.
*    ls_item_layout-style   =
*                          cl_gui_column_tree=>style_intensifd_critical.
*  else.
*    move '@07@' to ls_item_layout-t_image.
*  endif.
  APPEND ls_item_layout TO lt_item_layout.

  MOVE ps_out-refnr        TO ls_out-refnr.
  MOVE ps_out-ordem        TO ls_out-ordem.
  MOVE ps_out-remessa      TO ls_out-remessa.
  MOVE ps_out-nome_cliente TO ls_out-nome_cliente.

*  move ps_out-kzdru      to ls_out-kzdru.
*  move ps_out-prioridade to ls_out-prioridade.
*  move ps_out-lock_grupo to ls_out-lock_grupo.
*  move ps_out-locked_out to ls_out-locked_out.

* add node
  CLEAR l_node_text.
*  write ps_out-refnr to aux_refnr.
*  concatenate  aux_refnr '-' ps_out-refnt into l_node_text.

  MOVE ls_out-remessa TO l_node_text.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    "add_vbeln_line

*&--------------------------------------------------------------------*
*&      Form  add_complete_line
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->PS_OUT     text
*      -->P_RELAT_KEYtext
*      -->P_NODE_KEY text
*---------------------------------------------------------------------*
FORM add_complete_line USING   ps_out TYPE t_out
                               p_relat_key TYPE lvc_nkey
                     CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
*  ls_item_layout-class   = cl_gui_column_tree=>item_class_checkbox.
*  ls_item_layout-editable = 'X'.
  APPEND ls_item_layout TO lt_item_layout.

  ls_out = ps_out.

  CLEAR ls_out-nome_cliente.

*  clear: ls_out-prioridade,
*         ls_out-lock_grupo,
*         ls_out-locked_out.

  l_node_text =  ps_out-sscc.
  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      is_outtab_line   = ls_out
      i_node_text      = l_node_text
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                               " add_complete_line

*&---------------------------------------------------------------------*
*&      Form  register_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_events.
* define the events which will be passed to the backend
  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.

* define the events which will be passed to the backend
  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  APPEND l_event TO lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_checkbox_change.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_header_context_men_req.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
*  append l_event to lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_header_click.
  APPEND l_event TO lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_item_keypress.
*  append l_event to lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  APPEND l_event TO lt_events.

  CALL METHOD tree1->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
*   ERRO: Register Events
    MESSAGE e150(zwmmsg001).
  ENDIF.

* set Handler
  DATA: l_event_receiver TYPE REF TO lcl_tree_event_receiver.
  CREATE OBJECT l_event_receiver.

  SET HANDLER l_event_receiver->handle_item_double_click FOR tree1.

ENDFORM.                               " register_events

*&--------------------------------------------------------------------*
*&      Form  define_toolbar_excluding
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->PT_TOOLBAR_textUDING
*---------------------------------------------------------------------*
FORM define_toolbar_excluding
  CHANGING pt_toolbar_excluding TYPE ui_functions.

  APPEND cl_alv_tree_base=>mc_fc_calculate
    TO pt_toolbar_excluding.

*    append cl_alv_tree_base=>mc_fc_calculate_avg
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_calculate_min
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_calculate_max
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_calculate_sum
*      to pt_toolbar_excluding.

*    append cl_alv_tree_base=>mc_fc_expand
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_collapse
*      to pt_toolbar_excluding.

*    append cl_alv_tree_base=>mc_fc_col_optimize
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_col_invisible
*      to pt_toolbar_excluding.

*    append cl_alv_tree_base=>mc_fc_find
*      to pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_help
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_current_variant
    TO pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_load_variant
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_save_variant
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_maintain_variant
*      to pt_toolbar_excluding.

*    append cl_alv_tree_base=>mc_fc_print_back
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_print_back_all
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_print_prev
*      to pt_toolbar_excluding.
*
*    append cl_alv_tree_base=>mc_fc_print_prev_all
*      to pt_toolbar_excluding.
ENDFORM.                               " define_toolbar_excluding

*&---------------------------------------------------------------------*
*&      Form  valida_parametros_entrada
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_parametros_entrada .

  SELECT SINGLE * FROM t300
                    WHERE
                      lgnum = p_lgnum.
ENDFORM.                    "valida_parametros_entrada

*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       Cabecalho da ALV
*----------------------------------------------------------------------*
*      -->P_LT_LIST_COMMENTARY  text
*      -->P_L_LOGO  text
*----------------------------------------------------------------------*
FORM build_comment USING
      pt_list_commentary TYPE slis_t_listheader
      p_logo             TYPE sdydo_value.

  DATA: ls_line TYPE slis_listheader,
        text(60),
        l_lines LIKE sy-tabix,
        l_tabix LIKE sy-tabix.

  CLEAR : text,
          aux_s_date.

* LIST HEADING LINE: TYPE H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = 'Folha de Carga'(t00).
  APPEND ls_line TO pt_list_commentary.

* STATUS LINE: TYPE S
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Armazém:'(t01).
  ls_line-info = p_lgnum.
  APPEND ls_line TO pt_list_commentary.

** ACTION LINE: TYPE A
  CLEAR ls_line.
  ls_line-typ  = 'A'.
  GET TIME.
  WRITE sy-datum TO aux_s_date.
* TEXT-T02 DATA DE EXECUÇÃO
  CONCATENATE text-t06 aux_s_date INTO text
                         SEPARATED BY space.
  ls_line-info = text.
  APPEND ls_line TO pt_list_commentary.

ENDFORM.                    "build_comment

*&--------------------------------------------------------------------*
*&      Form  fill_title
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_TIPO     text
*      -->F_LOW      text
*      -->F_HIGH     text
*      -->F_TABIX    text
*      -->F_INFO     text
*---------------------------------------------------------------------*
FORM fill_title USING f_tipo f_low f_high f_tabix f_info.

  DATA: l_text(60),
        l_text_low(60),
        l_text_high(60).

  DATA: l_len  TYPE i,
        l_offs TYPE i,
        l_ctrl TYPE i.

  CLEAR: l_text, l_text_low, l_text_high.
  IF f_low IS INITIAL.
    PERFORM trata_tipo USING f_tipo f_high
                    CHANGING l_text_high.
    CONCATENATE 'até' l_text_high INTO l_text SEPARATED BY space.
  ELSE.
    IF f_high IS INITIAL.
      PERFORM trata_tipo USING f_tipo f_low
                      CHANGING l_text_low.
      IF f_tabix = 1.
        MOVE l_text_low TO l_text.
      ELSE.
        CONCATENATE '/' l_text_low INTO l_text
                                    SEPARATED BY space.
      ENDIF.
    ELSE.
      PERFORM trata_tipo USING f_tipo f_low
                      CHANGING l_text_low.
      PERFORM trata_tipo USING f_tipo f_high
                      CHANGING l_text_high.
      IF f_tabix = 1.
        CONCATENATE 'de' l_text_low 'a' l_text_high INTO l_text
                                      SEPARATED BY space.
      ELSE.
        CONCATENATE '/ de' l_text_low 'a' l_text_high INTO l_text
                                      SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDIF.
  IF f_tabix = 1.
    MOVE l_text TO f_info.
  ELSE.
    l_offs = STRLEN( f_info ) + 1.
    l_len = STRLEN( l_text ).
    l_ctrl = l_offs + l_len.
    CHECK l_ctrl LE 60.
    MOVE l_text TO f_info+l_offs(l_len).
  ENDIF.
ENDFORM.                    "fill_title

*&--------------------------------------------------------------------*
*&      Form  trata_tipo
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_TYP      text
*      -->F_INPUT    text
*      -->F_OUTPUT   text
*---------------------------------------------------------------------*
FORM trata_tipo USING f_typ f_input f_output.
  CASE f_typ.
    WHEN 'D'.
      WRITE f_input TO f_output DD/MM/YYYY.
    WHEN 'C'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = f_input
        IMPORTING
          output = f_output.
    WHEN OTHERS.
      MOVE f_input TO f_output.
  ENDCASE.
ENDFORM.                    "trata_tipo

*&---------------------------------------------------------------------*
*&      Form  GET_PARAMETER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WAREHOUSE  text
*      -->P_0052   text
*      -->P_0053   text
*      -->P_HOST  text
*----------------------------------------------------------------------*
FORM get_parameter USING armazem
                         processo
                         parametro
                         valor.

*  if gt_zdialog_rf_001_t[] is initial.
*    select * from zdialog_rf_001_t
*                  into table gt_zdialog_rf_001_t
*             where armazem = armazem.
*    if sy-subrc <> 0.
*      raise error.
*    endif.
*  endif.
*
*  read table gt_zdialog_rf_001_t with key armazem = armazem
*                                          processo = processo
*                                          parametro = parametro
*                                          binary search.
*  if sy-subrc = 0.
*    valor = gt_zdialog_rf_001_t-valor.
*  endif.
*

ENDFORM.                    " GET_PARAMETER

*&--------------------------------------------------------------------*
*&      Form  event_double_click
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM event_double_click.
  DATA: l_selected_node TYPE lvc_nkey,
        l_item_name     TYPE lvc_fname.

  DATA: l_index         TYPE lvc_nkey.

  CALL METHOD tree1->get_selected_item
    IMPORTING
      e_selected_node = l_selected_node
      e_fieldname     = l_item_name.

  l_index = l_selected_node - g_top_node + 1.

  CASE l_item_name.
    WHEN '&Hierarchy'.
      READ TABLE gt_out INTO gs_out INDEX l_index.
      CHECK sy-subrc = 0.
      CHECK NOT gs_out-remessa IS INITIAL.
      SET PARAMETER ID 'VL' FIELD gs_out-remessa.
      CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
*    when 'LOCK_GRUPO' or 'LOCKED_OUT'.
*      read table gt_out into gs_out index l_index.
*      check sy-subrc = 0.
*      check not gs_out-lock_grupo is initial.
*      g_flag_lock = 'X'.
*      move gs_out-refnr      to ecran-grupo.
*      move gs_out-lock_grupo to ecran-lock_grupo.
*      move gs_out-prioridade to ecran-prioridade.
*      perform ler_texto_lock_grupo
*                       using ecran-lock_grupo
*                    changing dd07d-ddtext.
*      clear g_flag_change.
*      call screen '0200' starting at 10 5  ending at 60 12.
*      if not g_flag_change is initial.
*        call method tree1->change_node
*          exporting
*            i_node_key    = l_selected_node
*            i_outtab_line = gs_out.
**       this method must be called to send the data to the frontend
*        call method tree1->frontend_update.
*      endif.
*    when 'PRIORIDADE'.
*      read table gt_out into gs_out index l_index.
*      check sy-subrc = 0.
*      check not gs_out-prioridade is initial.
*      clear g_flag_lock.
*      move gs_out-refnr      to ecran-grupo.
*      move gs_out-lock_grupo to ecran-lock_grupo.
*      move gs_out-prioridade to ecran-prioridade.
*      perform ler_texto_lock_grupo
*                       using ecran-lock_grupo
*                    changing dd07d-ddtext.
*      clear g_flag_change.
*      call screen '0200' starting at 10 5  ending at 60 12.
*      if not g_flag_change is initial.
*        call method tree1->change_node
*          exporting
*            i_node_key    = l_selected_node
*            i_outtab_line = gs_out.
**       this method must be called to send the data to the frontend
*        call method tree1->frontend_update.
*      endif.
  ENDCASE.

ENDFORM.                    "event_double_click

*&--------------------------------------------------------------------*
*&      Form  actualiza_zwm028
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM actualiza_zwm028.
  PERFORM verifica_alteracoes USING g_flag_change.
  IF g_flag_change IS INITIAL.
*   Não existem alterações
    MESSAGE i152(zwmmsg001).
  ENDIF.
  CLEAR g_flag_change.
  PERFORM confirm_step USING text-m23
                             text-m28
                             text-m25
                             g_flag_change.
  CHECK g_flag_change = 'J'.
  LOOP AT tab_ger.
    SELECT SINGLE * FROM zwm028 INTO gs_zwm028
           WHERE lgnum   = p_lgnum
             AND refnr   = tab_ger-refnr
             AND remessa = tab_ger-remessa.
    CHECK sy-subrc = 0.
    CHECK gs_zwm028-ordem <> tab_ger-ordem.
    gs_zwm028-ordem = tab_ger-ordem.
    UPDATE zwm028 FROM gs_zwm028.
  ENDLOOP.
ENDFORM.                    "actualiza_zwm028

*&--------------------------------------------------------------------*
*&      Form  verifica_alteracoes
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_FLAG_CHANtext
*---------------------------------------------------------------------*
FORM verifica_alteracoes USING f_flag_change.
  CLEAR f_flag_change.
  LOOP AT tab_ger.
    SELECT SINGLE * FROM zwm028 INTO gs_zwm028
           WHERE lgnum   = p_lgnum
             AND refnr   = tab_ger-refnr
             AND remessa = tab_ger-remessa.
    CHECK sy-subrc = 0.
    CHECK gs_zwm028-ordem <> tab_ger-ordem.
    gs_zwm028-ordem = tab_ger-ordem.
    f_flag_change = 'X'.
  ENDLOOP.
ENDFORM.                    "verifica_alteracoes

*&--------------------------------------------------------------------*
*&      Form  imprime_folha_carga
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM imprime_folha_carga.
  PERFORM verifica_alteracoes USING g_flag_change.
  IF NOT g_flag_change IS INITIAL.
    CLEAR g_flag_change.
    PERFORM popup USING text-m23 ' '  ' '
                        text-m30 ' '  text-m25
                        g_flag_change.
    CHECK g_flag_change = 'J'.
  ENDIF.
  SUBMIT zwmrep0027
         WITH p_lgnum = p_lgnum
         WITH p_refnr = p_refnr
                     AND RETURN.
ENDFORM.                    "imprime_folha_carga

*&--------------------------------------------------------------------*
*&      Form  altera_posicao
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_FLAG     text
*---------------------------------------------------------------------*
FORM altera_posicao USING f_flag.
  DATA: l_selected_node TYPE lvc_nkey,
        l_item_name     TYPE lvc_fname.

  DATA: l_nkey  TYPE lvc_nkey.

  DATA: l_index LIKE sy-tabix,
        l_tabix LIKE sy-tabix,
        l_lines LIKE sy-tabix.

  DATA: l_ordem     LIKE zwm028-ordem,
        l_ordem_ant LIKE zwm028-ordem,
        l_ordem_seg LIKE zwm028-ordem.

  CALL METHOD tree1->get_selected_item
    IMPORTING
      e_selected_node = l_selected_node
      e_fieldname     = l_item_name.

*  check l_item_name = 'REMESSA'.
  g_fieldname = l_item_name.

  IF l_selected_node IS INITIAL.
    CLEAR g_selected_nodes.
    CALL METHOD tree1->get_selected_nodes
      CHANGING
        ct_selected_nodes = g_selected_nodes.
    READ TABLE g_selected_nodes INTO l_selected_node INDEX 1.
  ENDIF.

  l_nkey = l_selected_node - g_top_node + 1.

  READ TABLE gt_out INTO gs_out INDEX l_nkey.
  CHECK sy-subrc = 0.

  CHECK NOT gs_out-remessa IS INITIAL
        AND gs_out-sscc    IS INITIAL.

  CLEAR: key_ger, reg_ger, tab_ger.

  key_ger-refnr   = gs_out-refnr.
  key_ger-ordem   = gs_out-ordem.
  key_ger-remessa = gs_out-remessa.
  READ TABLE tab_ger WITH KEY key_ger BINARY SEARCH.
  CHECK sy-subrc = 0.
  l_tabix = sy-tabix.

  DESCRIBE TABLE tab_ger LINES l_lines.

  CASE f_flag.
    WHEN '0'.
      MOVE '01' TO l_ordem.
      tab_ger-ordem = l_ordem.
      MODIFY tab_ger INDEX l_tabix.
*     Guarda registo
      reg_ger = tab_ger.
      LOOP AT tab_ger.
        CHECK sy-tabix <> l_tabix.
        l_ordem = l_ordem + 1.
        tab_ger-ordem = l_ordem.
        MODIFY tab_ger.
      ENDLOOP.
    WHEN '1'.
      CHECK l_tabix > 1.
      l_index = l_tabix - 1.
      l_ordem_seg = tab_ger-ordem.
      LOOP AT tab_ger.
        IF sy-tabix = l_index.
          l_ordem_ant   = tab_ger-ordem.
          tab_ger-ordem = l_ordem_seg.
          MODIFY tab_ger.
        ELSEIF sy-tabix = l_tabix.
          tab_ger-ordem = l_ordem_ant.
          MODIFY tab_ger.
*         Guarda registo
          reg_ger = tab_ger.
        ENDIF.
      ENDLOOP.
    WHEN '2'.
      CHECK l_tabix < l_lines.
      l_index = l_tabix + 1.
      READ TABLE tab_ger INDEX l_index.
      CHECK sy-subrc = 0.
      l_ordem_seg = tab_ger-ordem.
      LOOP AT tab_ger.
        IF sy-tabix = l_tabix.
          l_ordem_ant   = tab_ger-ordem.
          tab_ger-ordem = l_ordem_seg.
          MODIFY tab_ger.
*         Guarda registo
          reg_ger = tab_ger.
        ELSEIF sy-tabix = l_index.
          tab_ger-ordem = l_ordem_ant.
          MODIFY tab_ger.
        ENDIF.
      ENDLOOP.
    WHEN '3'.
      MOVE '01' TO l_ordem.
      LOOP AT tab_ger.
        CHECK sy-tabix <> l_tabix.
        tab_ger-ordem = l_ordem.
        MODIFY tab_ger.
        l_ordem = l_ordem + 1.
      ENDLOOP.
      READ TABLE tab_ger INDEX l_tabix.
      tab_ger-ordem = l_ordem.
      MODIFY tab_ger INDEX l_tabix.
*     Guarda registo
      reg_ger = tab_ger.
  ENDCASE.

  SORT tab_ger BY refnr ordem remessa.

  PERFORM cria_tab_out.

*  call method tree1->change_node
*    exporting
*      i_node_key    = l_selected_node
*      i_outtab_line = gs_out.

* this method must be called to send the data to the frontend
*  call method tree1->frontend_update.

  CLEAR: flag_tree, not_first_time.

ENDFORM.                    "altera_posicao

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.

  DATA: l_destino LIKE zwm013-destino.

  RANGES: lr_destino FOR zwm013-destino.

  DATA: lt_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE,
        lt_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE,
** RL -> INS 07.06.2005 -------------------------------------
        lt_zwm040 LIKE zwm040 OCCURS 0 WITH HEADER LINE.
** RL <- INS 07.06.2005 -------------------------------------

  DATA: BEGIN OF lt_zwm013 OCCURS 0,
          sscc           LIKE zwm013-sscc,
          destino        LIKE zwm013-destino,
          posicao_pulmao LIKE zwm013-posicao_pulmao,
        END OF lt_zwm013.

  DATA: BEGIN OF lt_ltap OCCURS 0,
          vbeln LIKE ltap-vbeln,
          vlenr LIKE ltap-vlenr,
        END OF lt_ltap.

  REFRESH: lr_destino, tab_ger, tab_pal, lt_ltap.

  CLEAR: tab_pal, tab_ger, lt_ltap.

* ZWM028
  SELECT * FROM zwm028 INTO TABLE lt_zwm028
           WHERE lgnum = p_lgnum
             AND refnr = p_refnr.

* ZWM026
  SELECT * FROM zwm026 INTO TABLE lt_zwm026
           WHERE armazem = p_lgnum
             AND grupo   = p_refnr.
  DELETE lt_zwm026 WHERE remessa IS INITIAL.

* Cria TAB_OUT
  LOOP AT lt_zwm028.

** RL -> INS 07.06.2005 -------------------------------------
    FREE: lt_zwm040.
    CLEAR: lt_zwm040.
** RL <- INS 07.06.2005 -------------------------------------

    IF lt_zwm028-remessa IS INITIAL.

      CLEAR lr_destino.

      lr_destino-sign = 'I'.
      lr_destino-option = 'EQ'.

      IF NOT lt_zwm028-pulmao1 IS INITIAL.
        CLEAR l_destino.
        CONCATENATE lt_zwm028-st_pul lt_zwm028-pulmao1
                   INTO l_destino SEPARATED BY space.
        lr_destino-low = l_destino.
        APPEND lr_destino.
      ENDIF.

      IF NOT lt_zwm028-pulmao2 IS INITIAL.
        CLEAR l_destino.
        CONCATENATE lt_zwm028-st_pul lt_zwm028-pulmao2
                   INTO l_destino SEPARATED BY space.
        lr_destino-low = l_destino.
        APPEND lr_destino.
      ENDIF.

    ELSE.

** RL -> INS 07.06.2005 -------------------------------------
      IF lt_zwm028-servisan = 'X'.
        SELECT * FROM zwm040
        INTO CORRESPONDING FIELDS OF TABLE lt_zwm040
        WHERE id_servisan EQ lt_zwm028-remessa
          AND lgnum       EQ p_lgnum
          AND refnr       EQ p_refnr.

        LOOP AT lt_zwm040.
          MOVE-CORRESPONDING lt_zwm028 TO tab_ger.
          tab_ger-remessa = lt_zwm040-remessa.
          APPEND tab_ger.
          CLEAR tab_ger.
        ENDLOOP.
      ELSE.
** RL <- INS 07.06.2005 -------------------------------------
        CLEAR tab_ger.
        MOVE-CORRESPONDING lt_zwm028 TO tab_ger.
        APPEND tab_ger.
** RL -> INS 07.06.2005 -------------------------------------
      ENDIF.
** RL <- INS 07.06.2005 -------------------------------------
    ENDIF.
  ENDLOOP.

  IF NOT tab_ger[] IS INITIAL.
    SELECT vbeln vlenr
           FROM ltap INTO TABLE lt_ltap
                FOR ALL ENTRIES IN tab_ger
                    WHERE vbeln = tab_ger-remessa.
    DELETE lt_ltap WHERE vlenr IS INITIAL.
  ENDIF.

  IF NOT lr_destino[] IS INITIAL.
    REFRESH lt_zwm013.
    SELECT sscc destino posicao_pulmao
           FROM zwm013 INTO TABLE lt_zwm013
                WHERE armazem = p_lgnum
                  AND destino IN lr_destino.
  ENDIF.

  LOOP AT lt_zwm026.
    lt_ltap-vbeln = lt_zwm026-remessa.
    lt_ltap-vlenr = lt_zwm026-sscc.
    COLLECT lt_ltap.
  ENDLOOP.

  SORT lt_ltap   BY vbeln vlenr.
  SORT lt_zwm013 BY sscc.

  LOOP AT lt_ltap.
    CLEAR: lt_zwm013.
    READ TABLE lt_zwm013 WITH KEY lt_ltap-vlenr BINARY SEARCH.
    tab_pal-remessa        = lt_ltap-vbeln.
    tab_pal-sscc           = lt_zwm013-sscc.
    tab_pal-destino        = lt_zwm013-destino.
    tab_pal-posicao_pulmao = lt_zwm013-posicao_pulmao.
    APPEND tab_pal.
  ENDLOOP.

  SORT tab_ger BY refnr ordem remessa.
  SORT tab_pal BY remessa sscc destino.

  FREE: lt_zwm028, lt_zwm026, lt_zwm013, lt_ltap.

  PERFORM cria_tab_out.

ENDFORM.                    "select_data

*&--------------------------------------------------------------------*
*&      Form  cria_tab_out
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM cria_tab_out.

  DATA: l_index LIKE sy-tabix.

  REFRESH tab_out. CLEAR tab_out.

  LOOP AT tab_ger.

    CLEAR tab_out.
    tab_out-refnr   = tab_ger-refnr.
    tab_out-ordem   = tab_ger-ordem.
    tab_out-remessa = tab_ger-remessa.

    SELECT SINGLE * FROM likp WHERE vbeln = tab_ger-remessa.
    IF sy-subrc = 0.
      SELECT SINGLE * FROM kna1 WHERE kunnr = likp-kunnr.
      IF sy-subrc = 0.
        tab_out-nome_cliente = kna1-name1.
      ENDIF.
    ENDIF.

    CLEAR: tab_pal.
    READ TABLE tab_pal WITH KEY tab_ger-remessa BINARY SEARCH.
    WHILE sy-subrc = 0 AND tab_pal-remessa = tab_ger-remessa.
      l_index = sy-tabix + 1.
      tab_out-sscc           = tab_pal-sscc.
      tab_out-destino        = tab_pal-destino.
      tab_out-posicao_pulmao = tab_pal-posicao_pulmao.
      APPEND tab_out.
      READ TABLE tab_pal INDEX l_index.
    ENDWHILE.

    IF l_index IS INITIAL.
      APPEND tab_out.
    ENDIF.

  ENDLOOP.

  SORT tab_out BY refnr ordem remessa destino posicao_pulmao.

ENDFORM.                    "cria_tab_out

*---------------------------------------------------------------------*
*       FORM CONFIRM_STEP                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_LINE1                                                       *
*  -->  F_LINE2                                                       *
*  -->  F_TITLE                                                       *
*  -->  F_ANSWER                                                      *
*---------------------------------------------------------------------*
FORM confirm_step USING f_line1 f_line2 f_title f_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
*           defaultoption  = ' '
            textline1 = f_line1
            textline2 = f_line2
            titel     = f_title
       IMPORTING
            answer    = f_answer
       EXCEPTIONS
            OTHERS    = 1.
ENDFORM.                    "confirm_step

*---------------------------------------------------------------------*
*       FORM popup                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM popup USING f_diag1 f_diag2 f_diag3
                 f_line1 f_line2 f_title
                 f_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
    EXPORTING
      defaultoption  = ' '
      diagnosetext1  = f_diag1
      diagnosetext2  = f_diag2
      diagnosetext3  = f_diag3
      textline1      = f_line1
      textline2      = f_line2
      titel          = f_title
    IMPORTING
      answer         = f_answer
    EXCEPTIONS
      titel_too_long = 01.
ENDFORM.                    "popup
