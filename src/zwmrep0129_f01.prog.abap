*&---------------------------------------------------------------------*
*&  Include           ZWMREP0129_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_whs.

  DATA : BEGIN OF l_user OCCURS 0.
          INCLUDE STRUCTURE lrf_wkqu.
  DATA : END OF l_user.

  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = l_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
*   ERRO: Utilizador não tem armazem atribuído!
    MESSAGE e001(zwmmpmsg).

  ELSE.
    READ TABLE l_user WITH KEY statu = 'X'.
    IF sy-subrc <> 0.
*     ERRO: Utilizador não está atribuído ao armazém &!
      MESSAGE e002(zwmmpmsg) WITH sy-uname.
    ELSE.

      scr1-lgnum = l_user-lgnum.

**    Denominação do Armazém
      SELECT SINGLE lnumt FROM t300t INTO scr1-lnumt
              WHERE spras EQ sy-langu
                AND lgnum EQ scr1-lgnum.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_WHS
*&---------------------------------------------------------------------*
*&      Form  INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree.

  DATA: l_tree_container_name(30) TYPE c,
        l_custom_container  TYPE REF TO cl_gui_custom_container.

  DATA: lt_expand_nodes     TYPE lvc_t_nkey,
        l_toolbar_excluding TYPE ui_functions.

  DATA: l_hierarchy_header  TYPE treev_hhdr.

  DATA: lt_list_commentary  TYPE slis_t_listheader,
        l_logo              TYPE sdydo_value.
  DATA: ls_variant          TYPE disvariant.

**********************************************************************
** 1- Criar o container da tela para inserir o ALV-TREE
**********************************************************************
  l_tree_container_name = 'ALV_TREE_CONTAINER'.

  IF sy-batch IS INITIAL.
    CREATE OBJECT l_custom_container
      EXPORTING
        container_name              = l_tree_container_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc <> 0.
*      MESSAGE e072(zbc_rf_messages).
    ENDIF.
  ENDIF.

  REFRESH gt_alv_tree1.

**********************************************************************
** 2- Criar o ALV-TREE
**********************************************************************
  CREATE OBJECT tree
    EXPORTING
      parent                      = l_custom_container
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single "node_sel_mode_multiple
      item_selection              = 'X'
      no_html_header              = 'X'
*      no_toolbar                  = 'X' "Inserir/Remover Toolbar
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.

  IF sy-subrc <> 0.
*    MESSAGE e073(zbc_rf_messages).
  ENDIF.

** Criar Fieldcatalog
  PERFORM build_fieldcatalog.
** Ajustar propriedades
  PERFORM adjust_proprieties.
** Criar o cabeçalho da hierarquia
  PERFORM build_hierarchy_header CHANGING l_hierarchy_header.
* repid for saving variants
  ls_variant-report = sy-repid.
** Definir toolbar
  PERFORM define_toolbar_excluding CHANGING l_toolbar_excluding.

**********************************************************************
** 3- Configurar e apresentar ALV-TREE  "vazio"
**********************************************************************
  CALL METHOD tree->set_table_for_first_display
    EXPORTING
      is_hierarchy_header  = l_hierarchy_header
      it_list_commentary   = lt_list_commentary
*      i_logo               = l_logo
*      i_background_id      = 'ALV_BACKGROUND'
      i_save               = 'A'
      is_variant           = ls_variant
      it_toolbar_excluding = l_toolbar_excluding
    CHANGING
      it_outtab            = gt_alv_tree1 "table must be empty !!
      it_fieldcatalog      = gt_fieldcatalog.

** Alterar o toolbar / Inserir botões,separadores,etc. (customizado)
  PERFORM change_toolbar.

** Registar eventos para ALV-TREE/Toolbar (customizado)
  PERFORM register_events.

  CALL METHOD tree->frontend_update.

  PERFORM cria_hierarquia.

ENDFORM.                    " INIT_TREE
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .
  DATA: pos     TYPE i VALUE 1.
  DATA: aux_cat TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE.

  REFRESH aux_cat.

  CLEAR gt_fieldcatalog[].
  REFRESH gt_fieldcatalog[].

** Armazém
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'LGNUM'.
*  nao aparece a esquerda
  aux_cat-no_out        = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-coltext       = 'Armazém'.
  aux_cat-outputlen     = '3'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** Descrição Armazém
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'LNUMT'.
*  nao aparece a esquerda
  aux_cat-no_out        = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-coltext       = 'Descr. Armazém'.
  aux_cat-outputlen     = '35'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** HU
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'EXIDV'.
*  nao aparece a esquerda
  aux_cat-no_out        = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-coltext       = 'SSCC'.
  aux_cat-outputlen     = '20'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

  gt_fieldcatalog[] = aux_cat[].

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  ADJUST_PROPRIETIES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM adjust_proprieties.

  DATA: pos        TYPE i VALUE 1.
  DATA: ls_sort_wa TYPE lvc_s_sort.

* Ordenação
  REFRESH gt_sort.

*  CLEAR pos.
*
** create sort-table
*  ADD 1 TO pos.
*  ls_sort_wa-spos = pos.
*  ls_sort_wa-fieldname = 'VBELN'.
*  ls_sort_wa-up = 'X'.
*  ls_sort_wa-subtot = 'X'.
*  APPEND ls_sort_wa TO gt_sort.
*
*  ADD 1 TO pos.
*  ls_sort_wa-spos = pos.
*  ls_sort_wa-fieldname = 'HU_CAIXA'.
*  ls_sort_wa-up = 'X'.
*  ls_sort_wa-subtot = 'X'.
*  APPEND ls_sort_wa TO gt_sort.
*
*  ADD 1 TO pos.
*  ls_sort_wa-spos = pos.
*  ls_sort_wa-fieldname = 'POSTO'.
*  ls_sort_wa-up = 'X'.
*  ls_sort_wa-subtot = 'X'.
*  APPEND ls_sort_wa TO gt_sort.
*
*  ADD 1 TO pos.
*  ls_sort_wa-spos = pos.
*  ls_sort_wa-fieldname = 'MATERIAL'.
*  ls_sort_wa-up = 'X'.
*  ls_sort_wa-subtot = 'X'.
*  APPEND ls_sort_wa TO gt_sort.

ENDFORM.                    " ADJUST_PROPRIETIES
*&---------------------------------------------------------------------*
*&      Form  BUILD_HIERARCHY_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_HIERARCHY_HEADER  text
*----------------------------------------------------------------------*
FORM build_hierarchy_header  CHANGING
                             p_hierarchy_header TYPE treev_hhdr.

  p_hierarchy_header-heading = 'Lista SSCC'.
  p_hierarchy_header-tooltip = 'Lista SSCC'.
  p_hierarchy_header-width = 50.
  p_hierarchy_header-width_pix = 'X'.

ENDFORM.                    " BUILD_HIERARCHY_HEADER
*&---------------------------------------------------------------------*
*&      Form  DEFINE_TOOLBAR_EXCLUDING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_TOOLBAR_EXCLUDING  text
*----------------------------------------------------------------------*
FORM define_toolbar_excluding CHANGING
                              pt_toolbar_excluding TYPE ui_functions.

  APPEND cl_alv_tree_base=>mc_fc_calculate
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_calculate_avg
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_calculate_min
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_calculate_max
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_calculate_sum
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_expand
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_collapse
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_col_optimize
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_col_invisible
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_find
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_help
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_current_variant
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_load_variant
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_save_variant
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_maintain_variant
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_print_back
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_print_back_all
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_print_prev
    TO pt_toolbar_excluding.

  APPEND cl_alv_tree_base=>mc_fc_print_prev_all
    TO pt_toolbar_excluding.

ENDFORM.                    " DEFINE_TOOLBAR_EXCLUDING
*&---------------------------------------------------------------------*
*&      Form  CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_toolbar.

  CALL METHOD tree->get_toolbar_object
    IMPORTING
      er_toolbar = g_toolbar.

  CHECK NOT g_toolbar IS INITIAL. "could happen if you do not use the
  "standard toolbar

** Eliminar SSCC
  CALL METHOD g_toolbar->add_button
    EXPORTING
      fcode     = 'DEL'
      icon      = '@11@'
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = 'Eliminar SSCC'.    "Information

** Modificar SSCC
  CALL METHOD g_toolbar->add_button
    EXPORTING
      fcode     = 'EDIT'
      icon      = '@3I@'
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = 'Modificar SSCC'.    "Information

** Guardar
  CALL METHOD g_toolbar->add_button
    EXPORTING
      fcode     = 'SAVE'
      icon      = '@2L@'
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = 'Gravar SSCC'.    "Information

  CALL METHOD g_toolbar->set_button_state
    EXPORTING
      enabled          = ''
*    checked          = ' '
      fcode            = 'SAVE'
    EXCEPTIONS
      cntl_error       = 1
      cntb_error_fcode = 2
      OTHERS           = 3.

ENDFORM.                    " CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  REGISTER_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_events .

  DATA: lt_events TYPE cntl_simple_events,
        l_event TYPE cntl_simple_event,
        lv_tree_evt_rec TYPE REF TO lcl_tree_event_receiver,
        lv_toolbar_evt_rec TYPE REF TO lcl_toolbar_event_receiver.

* Get register Events
  CALL METHOD tree->get_registered_events
    IMPORTING
      events = lt_events.

* define the events which will be passed to the backend
*  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_checkbox_change.
*  append l_event to lt_events.
*  l_event-eventid =
*  cl_gui_column_tree=>eventid_header_context_men_req.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_header_click.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_item_keypress.
*  append l_event to lt_events.

  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  l_event-appl_event = 'X'." Eventos a ser tratados no PAI
  APPEND l_event TO lt_events.

* register events on frontend
  CALL METHOD tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.
  ENDIF.

  CREATE OBJECT lv_tree_evt_rec.
  SET HANDLER lv_tree_evt_rec->handle_item_double_click FOR tree.
*
  CREATE OBJECT lv_toolbar_evt_rec.
  SET HANDLER lv_toolbar_evt_rec->on_button_clicked FOR g_toolbar.


ENDFORM.                    " REGISTER_EVENTS
*&---------------------------------------------------------------------*
*&      Form  CRIA_HIERARQUIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_hierarquia.
  DATA: lv_child_node  TYPE lvc_nkey.

** Eliminar todos nós já criados
  CALL METHOD tree->delete_all_nodes.

* Criar árvore de nós
  PERFORM create_hierarchy.

* Expandir nó seleccionado
  CALL METHOD tree->get_first_child
    EXPORTING
      i_node_key       = g_top_node_key
    IMPORTING
      e_child_node_key = lv_child_node.

  IF lv_child_node IS NOT INITIAL.
    CALL METHOD tree->expand_node
      EXPORTING
        i_node_key = g_top_node_key.
  ENDIF.
*** optimize column-width
*  CALL METHOD tree->column_optimize
*    EXPORTING
*      i_include_heading = 'X'.

ENDFORM.                    " CRIA_HIERARQUIA
*&---------------------------------------------------------------------*
*&      Form  CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_hierarchy.

  DATA: ls_alv TYPE st_alv_tree.

  DATA: l_item1_key TYPE lvc_nkey,
        l_item2_key TYPE lvc_nkey,
        l_item3_key TYPE lvc_nkey,
        l_last_key  TYPE lvc_nkey.

** IMPORTANTE - Tabela com os nós do ALV-TREE
  CLEAR:   gt_alv_tree1.
  REFRESH: gt_alv_tree1.

**********************************************************************
** Nó escolhido
**********************************************************************
  PERFORM add_top_line USING    scr1-lgnum
                                ' '
                       CHANGING g_top_node_key.

  LOOP AT gt_alv_tree.
    MOVE gt_alv_tree TO ls_alv.

** Nó Material
**********************************************************************
    AT NEW exidv.
      PERFORM add_item1_line USING    ls_alv-exidv
                                      g_top_node_key
                             CHANGING l_last_key.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*&      Form  ADD_TOP_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LGNUM  text
*      -->P_0639   text
*      <--P_G_TOP_NODE_KEY  text
*----------------------------------------------------------------------*
FORM add_top_line  USING    p_lgnum     TYPE lgnum
                            p_node_key  TYPE lvc_nkey
                   CHANGING p_child_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      LIKE LINE OF gt_alv_tree1.

  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  DATA: ls_node_layout TYPE lvc_s_layn.

** Icon do nó
  ls_node_layout-exp_image = '@WV@'.
  ls_node_layout-n_image   = '@WV@'.
** Texto do nó
  WRITE p_lgnum TO l_node_text.

** Adicionar nó
  CALL METHOD tree->add_node
    EXPORTING
      i_relat_node_key = p_node_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
      is_node_layout   = ls_node_layout
    IMPORTING
      e_new_node_key   = p_child_key.
ENDFORM.                    " ADD_TOP_LINE
*&---------------------------------------------------------------------*
*&      Form  ADD_ITEM1_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUT_N1  text
*      -->P_G_TOP_NODE_KEY  text
*      <--P_L_ITEM1_KEY  text
*----------------------------------------------------------------------*
FORM add_item1_line  USING  p_exidv     TYPE exidv
                            p_node_key  TYPE lvc_nkey
                 CHANGING   p_child_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      LIKE LINE OF gt_alv_tree1.

  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  DATA: ls_node_layout TYPE lvc_s_layn.

  ls_out-exidv = p_exidv.

** Icon do nó
  ls_node_layout-exp_image = '@TV@'.
  ls_node_layout-n_image   = '@TV@'.

** Texto do nó
  WRITE p_exidv TO l_node_text LEFT-JUSTIFIED.

  CALL METHOD tree->add_node
    EXPORTING
      i_relat_node_key = p_node_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
      is_node_layout   = ls_node_layout
    IMPORTING
      e_new_node_key   = p_child_key.

ENDFORM.                    " ADD_ITEM1_LINE
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv.

  DATA: lv_event_receiver  TYPE REF TO lcl_grid_event_receiver.
  DATA: ls_layout          TYPE lvc_s_layo.
  DATA: lt_toolbar         TYPE ui_functions.

  CREATE OBJECT gc_alv_container
    EXPORTING
      container_name = 'ALV_GUI_CONTAINER'.

  CREATE OBJECT gc_alv_grid
    EXPORTING
      i_parent = gc_alv_container.

  PERFORM get_fieldcatalog.

  PERFORM define_toolbar_grid_excluding CHANGING lt_toolbar.

  ls_layout-sel_mode = 'A'. "Multiple lines

  CALL METHOD gc_alv_grid->set_table_for_first_display
    EXPORTING
      is_layout            = ls_layout
      it_toolbar_excluding = lt_toolbar
    CHANGING
      it_fieldcatalog      = gt_fcat
      it_outtab            = gt_alv.


  CREATE OBJECT lv_event_receiver.

  SET HANDLER lv_event_receiver->handle_user_command FOR gc_alv_grid.
  SET HANDLER lv_event_receiver->handle_toolbar      FOR gc_alv_grid.
  SET HANDLER lv_event_receiver->handle_data_changed FOR gc_alv_grid.

*  CALL METHOD gc_alv_grid->set_toolbar_interactive.

  CALL METHOD gc_alv_grid->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.

  CALL METHOD gc_alv_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.                    " INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fieldcatalog.

  DATA: pos          TYPE int4.
  DATA: ls_fcat      TYPE lvc_s_fcat.
  DATA: fieldcat_alv TYPE slis_t_fieldcat_alv.

** Field Catalog da Tabela de Output
**********************************************************************
  CLEAR: gs_fcat, gt_fcat.
  REFRESH: gt_fcat.

** Item
  ADD 1 TO pos.

  CLEAR gs_fcat.
  gs_fcat-col_pos       =  pos.
  gs_fcat-fieldname     = 'VEPOS'.
*  gs_fcat-no_out        = 'X'.
  gs_fcat-no_zero       = 'X'.
  gs_fcat-coltext       = 'Item'.
  gs_fcat-outputlen     = '6'.
  gs_fcat-fix_column    = 'X'.
  APPEND gs_fcat TO gt_fcat.

** Remessa
  ADD 1 TO pos.

  CLEAR gs_fcat.
  gs_fcat-col_pos       =  pos.
  gs_fcat-fieldname     = 'VBELN'.
*  gs_fcat-no_out        = 'X'.
  gs_fcat-no_zero       = 'X'.
  gs_fcat-coltext       = 'Remessa'.
  gs_fcat-outputlen     = '10'.
  gs_fcat-fix_column    = 'X'.
  APPEND gs_fcat TO gt_fcat.

** Item Remessa
  ADD 1 TO pos.

  CLEAR gs_fcat.
  gs_fcat-col_pos       =  pos.
  gs_fcat-fieldname     = 'POSNR'.
*  gs_fcat-no_out        = 'X'.
  gs_fcat-no_zero       = 'X'.
  gs_fcat-coltext       = 'It.Rem'.
  gs_fcat-outputlen     = '6'.
  gs_fcat-fix_column    = 'X'.
  APPEND gs_fcat TO gt_fcat.

** Material
  ADD 1 TO pos.

  CLEAR gs_fcat.
  gs_fcat-col_pos       =  pos.
  gs_fcat-fieldname     = 'MATNR'.
*  gs_fcat-no_out        = 'X'.
  gs_fcat-no_zero       = 'X'.
  gs_fcat-coltext       = 'Material'.
  gs_fcat-outputlen     = '15'.
  gs_fcat-fix_column    = 'X'.
  APPEND gs_fcat TO gt_fcat.

** Lote
  ADD 1 TO pos.

  CLEAR gs_fcat.
  gs_fcat-col_pos       =  pos.
  gs_fcat-fieldname     = 'CHARG'.
*  gs_fcat-no_out        = 'X'.
  gs_fcat-no_zero       = 'X'.
  gs_fcat-coltext       = 'Lote'.
  gs_fcat-outputlen     = '10'.
  gs_fcat-fix_column    = 'X'.
  APPEND gs_fcat TO gt_fcat.

** Qtd.
  ADD 1 TO pos.

  CLEAR gs_fcat.
  gs_fcat-col_pos       =  pos.
  gs_fcat-fieldname     = 'VEMNG'.
  gs_fcat-ref_table     = 'VEPO'.
  gs_fcat-ref_field     = 'VEMNG'.
*  gs_fcat-no_out        = 'X'.
  gs_fcat-no_zero       = 'X'.
  gs_fcat-edit          = 'X'.
  gs_fcat-coltext       = 'Quantidade'.
  gs_fcat-outputlen     = '17'.
  gs_fcat-fix_column    = 'X'.
  APPEND gs_fcat TO gt_fcat.

** Unidade
  ADD 1 TO pos.

  CLEAR gs_fcat.
  gs_fcat-col_pos       =  pos.
  gs_fcat-fieldname     = 'VEMEH'.
*  gs_fcat-no_out        = 'X'.
  gs_fcat-no_zero       = 'X'.
  gs_fcat-coltext       = 'Uni.'.
  gs_fcat-outputlen     = '4'.
  gs_fcat-fix_column    = 'X'.
  APPEND gs_fcat TO gt_fcat.

** Centro
  ADD 1 TO pos.

  CLEAR gs_fcat.
  gs_fcat-col_pos       =  pos.
  gs_fcat-fieldname     = 'WERKS'.
*  gs_fcat-no_out        = 'X'.
  gs_fcat-no_zero       = 'X'.
  gs_fcat-coltext       = 'Centro'.
  gs_fcat-outputlen     = '10'.
  gs_fcat-fix_column    = 'X'.
  APPEND gs_fcat TO gt_fcat.

** Depósito
  ADD 1 TO pos.

  CLEAR gs_fcat.
  gs_fcat-col_pos       =  pos.
  gs_fcat-fieldname     = 'LGORT'.
*  gs_fcat-no_out        = 'X'.
  gs_fcat-no_zero       = 'X'.
  gs_fcat-coltext       = 'Depósito'.
  gs_fcat-outputlen     = '10'.
  gs_fcat-fix_column    = 'X'.
  APPEND gs_fcat TO gt_fcat.

ENDFORM.                    " GET_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  DEFINE_TOOLBAR_GRID_EXCLUDING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_TOOLBAR  text
*----------------------------------------------------------------------*
FORM define_toolbar_grid_excluding  CHANGING pt_exclude TYPE ui_functions.

  DATA: ls_exclude TYPE ui_func.

* Local manipulation

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_mb_paste.
  APPEND ls_exclude TO pt_exclude.

* Row manipulation
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO pt_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
*  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO pt_exclude.

** Info
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_exclude TO pt_exclude.

ENDFORM.                    " DEFINE_TOOLBAR_GRID_EXCLUDING
*&---------------------------------------------------------------------*
*&      Form  CHECK_REFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_refnr.

  DATA: ls_t311  TYPE t311.

** Validar Grupo
**********************************************************************
  CHECK scr1-refnr IS NOT INITIAL.

  CLEAR gv_2step.

  SELECT SINGLE *
    FROM t311 INTO ls_t311
    WHERE lgnum = scr1-lgnum AND
          refnr = scr1-refnr.

  IF sy-subrc <> 0.
    CLEAR scr1-refnr.
    MESSAGE e000 WITH 'Grupo inválido'.
  ENDIF.

** Validar se grupo em 2 passos
  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum = scr1-lgnum
      i_refnr = scr1-refnr
    IMPORTING
      e_2step = gv_2step
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.

** Paletes definidas ao nivel do grupo
**********************************************************************
*  IF gv_2step = 'X'.
  PERFORM get_sscc USING gv_2step.
*  ENDIF.

ENDFORM.                    " CHECK_REFNR
*&---------------------------------------------------------------------*
*&      Form  CHECK_VBELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vbeln.

  DATA: ls_t311a TYPE t311a.
  DATA: lt_ltak  LIKE ltak OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap  LIKE ltap OCCURS 0 WITH HEADER LINE.

** Validar Remessa
**********************************************************************
  CHECK scr1-vbeln IS NOT INITIAL.

  SELECT SINGLE *
    FROM t311a INTO ls_t311a
    WHERE lgnum = scr1-lgnum AND
          refnr = scr1-refnr AND
          rbnum = scr1-vbeln.

  IF sy-subrc <> 0.
    CLEAR scr1-vbeln.
    MESSAGE e000 WITH 'Remessa inválida'.
  ENDIF.

** Paletes definidas ao nivel do remessa
**********************************************************************
  CLEAR gv_2step.

  PERFORM get_sscc USING gv_2step.

ENDFORM.                    " CHECK_VBELN
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command .

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'OTHER'.
      PERFORM clear_fields.
*      PERFORM change_material_letyp.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_KEY  text
*      -->P_FIELDNAME  text
*----------------------------------------------------------------------*
FORM event_double_click  USING  p_node_key p_fieldname.

  DATA: ls_node_alv TYPE st_alv_tree.

* Selecciona a linha da tabela gt_alv1 correspondente ao nó
  CALL METHOD tree->get_outtab_line
    EXPORTING
      i_node_key    = p_node_key
    IMPORTING
      e_outtab_line = ls_node_alv.

* Carregar dados para SSCC
  IF ls_node_alv-lgnum IS INITIAL AND
     ls_node_alv-exidv IS NOT INITIAL.

    PERFORM get_data_from_sscc USING ls_node_alv-exidv.
  ENDIF.

ENDFORM.                    " EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  GET_SSCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_2STEP  text
*----------------------------------------------------------------------*
FORM get_sscc USING pv_2step.

  DATA: lv_tabix  LIKE sy-tabix.
  DATA: lt_ltak   LIKE ltak   OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap   LIKE ltap   OCCURS 0 WITH HEADER LINE.
  DATA: lt_vekp   LIKE vekp   OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm044 LIKE zwm044 OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm056 LIKE zwm056 OCCURS 0 WITH HEADER LINE.

** Validar paletes
**********************************************************************

** Paletes completas
  SELECT *
    FROM ltak INTO TABLE lt_ltak
    WHERE lgnum = scr1-lgnum AND
          refnr = scr1-refnr.

*  IF pv_2step IS INITIAL.
  IF scr1-vbeln IS NOT INITIAL.
    DELETE lt_ltak WHERE vbeln <> scr1-vbeln.
  ENDIF.

  IF lt_ltak[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      FOR ALL ENTRIES IN lt_ltak
      WHERE lgnum = lt_ltak-lgnum AND
            tanum = lt_ltak-tanum.
  ENDIF.

  DELETE lt_ltap WHERE vorga <> 'LF' AND
                       vorga <> 'QB' AND
                       vorga <> space.

  DELETE lt_ltap WHERE vorga EQ space AND
                       nltyp <> '815'.

** Paletes Picking
*  IF pv_2step = 'X'.
  IF scr1-vbeln IS INITIAL.
    SELECT *
      FROM zwm026 INTO TABLE lt_zwm026
      WHERE armazem  = scr1-lgnum AND
            grupo    = scr1-refnr.
  ELSE.
    SELECT *
        FROM zwm026 INTO TABLE lt_zwm026
        WHERE armazem  = scr1-lgnum AND
              grupo    = scr1-refnr AND
              remessa  = scr1-vbeln.
  ENDIF.

  DELETE lt_zwm026 WHERE sscc IS INITIAL.

** Paletes especias
*  IF pv_2step IS INITIAL.
  IF scr1-vbeln IS NOT INITIAL.
    SELECT *
      FROM zwm044 INTO TABLE lt_zwm044
      WHERE lgnum = scr1-lgnum AND
            refnr = scr1-refnr AND
            vbeln = scr1-vbeln.
  ENDIF.

** Obter paletes
**********************************************************************

** Completas
  DELETE lt_ltap WHERE vlenr EQ space.

** Picking
  LOOP AT lt_zwm026.
    CLEAR lt_ltap.
    lt_ltap-vlenr = lt_zwm026-sscc.
    APPEND lt_ltap.
  ENDLOOP.

** Especiais
  IF lt_zwm044[] IS NOT INITIAL.
    LOOP AT lt_ltap.
      LOOP AT lt_zwm044 WHERE exidv  = lt_ltap-vlenr AND
                              status = 'I'.

        DELETE lt_ltap WHERE vlenr = lt_zwm044-exidv.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_zwm044 WHERE status = 'F'.
      CLEAR lt_ltap.
      lt_ltap-vlenr = lt_zwm044-exidv.
      APPEND lt_ltap.
    ENDLOOP.
  ENDIF.

  SORT lt_ltap BY vlenr.
  DELETE ADJACENT DUPLICATES FROM lt_ltap COMPARING vlenr.


** Validar se foram eliminadas
  IF lt_ltap[] IS NOT INITIAL.
    SELECT *
      FROM zwm056 INTO TABLE lt_zwm056
      FOR ALL ENTRIES IN lt_ltap
      WHERE exidv = lt_ltap-vlenr.

    LOOP AT lt_ltap.
      lv_tabix = sy-tabix.

      READ TABLE lt_zwm056 WITH KEY exidv = lt_ltap-vlenr.

      CHECK sy-subrc = 0.
      CHECK lt_zwm056-refnr = scr1-refnr.

      DELETE lt_ltap INDEX lv_tabix.
    ENDLOOP.
  ENDIF.

  IF lt_ltap[] IS NOT INITIAL.
    SELECT *
      FROM vekp INTO TABLE lt_vekp
      FOR ALL ENTRIES IN lt_ltap
      WHERE exidv = lt_ltap-vlenr.
  ENDIF.

** Lista SSCC
  SORT lt_vekp BY exidv.

  REFRESH: gt_alv_tree.
  REFRESH: gt_alv.

  LOOP AT lt_ltap.

    READ TABLE lt_vekp WITH KEY exidv = lt_ltap-vlenr.
    CHECK sy-subrc = 0.

    CLEAR: gt_alv_tree.
    gt_alv_tree-lgnum = scr1-lgnum.
    gt_alv_tree-lnumt = scr1-lnumt.
    gt_alv_tree-exidv = lt_ltap-vlenr.
    APPEND gt_alv_tree.
  ENDLOOP.

  PERFORM cria_hierarquia.

  CALL METHOD gc_alv_grid->refresh_table_display.

ENDFORM.                    " GET_SSCC
*&---------------------------------------------------------------------*
*&      Form  CLEAR_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_fields.

  CLEAR: scr1-refnr, scr1-vbeln, scr1-exidv, scr1-emb.
  CLEAR: gv_2step.

  REFRESH: gt_alv_tree.
  REFRESH: gt_alv.

  PERFORM cria_hierarquia.

  CALL METHOD gc_alv_grid->refresh_table_display.


ENDFORM.                    " CLEAR_FIELDS
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_SSCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_NODE_ALV_EXIDV  text
*----------------------------------------------------------------------*
FORM get_data_from_sscc USING p_exidv TYPE exidv.

  DATA: lv_tabix  LIKE sy-tabix.
  DATA: ls_vekp   TYPE vekp.
  DATA: ls_alv    LIKE LINE OF gt_alv.
  DATA: lt_vepo   TYPE vepo   OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap   TYPE ltap   OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm026 TYPE zwm026 OCCURS 0 WITH HEADER LINE.

** Obter dados SSCC
**********************************************************************
  CHECK p_exidv IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_exidv
    IMPORTING
      output = p_exidv.

  REFRESH: gt_alv.

  SELECT SINGLE *
    FROM vekp INTO ls_vekp
    WHERE exidv = p_exidv.

  IF sy-subrc = 0.
    SELECT *
      FROM vepo INTO TABLE gt_alv
      WHERE venum = ls_vekp-venum.
  ENDIF.

  scr1-exidv = p_exidv.

  IF ls_vekp-vpobj = '01' OR
     ls_vekp-vpobj = '04'.
    scr1-emb = 'X'.
  ELSE.
    CLEAR scr1-emb.
  ENDIF.

** Picking
  SELECT *
    FROM zwm026 INTO TABLE lt_zwm026
    WHERE armazem = scr1-lgnum AND
          grupo   = scr1-refnr.

  IF scr1-vbeln IS NOT INITIAL.
    DELETE lt_zwm026 WHERE remessa <> scr1-vbeln.
  ENDIF.

  DELETE lt_zwm026 WHERE sscc <> scr1-exidv.

** Completas
  IF lt_zwm026[] IS INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = scr1-lgnum AND
          ( vlenr = scr1-exidv OR
            nlenr = scr1-exidv ).

    DELETE lt_ltap WHERE vorga <> 'LF' AND
                         vorga <> 'QB' AND
                         vorga <> space.

    DELETE lt_ltap WHERE vorga EQ space AND
                         nltyp <> '815'.

    IF scr1-vbeln IS NOT INITIAL.
      DELETE lt_ltap WHERE vbeln <> scr1-vbeln.
    ENDIF.
  ENDIF.

*  CLEAR gs_alv.
*  gs_alv-vepos = '000010'.
*  gs_alv-matnr = '10001'.
*  gs_alv-charg = 'L1'.
*  gs_alv-vemng = 10.
*  gs_alv-vemeh = 'UN'.
*  gs_alv-lgort = '0001'.
*  gs_alv-werks = '1000'.
*  APPEND gs_alv TO gt_alv.
*
*  CLEAR gs_alv.
*  gs_alv-vepos = '000020'.
*  gs_alv-matnr = '50000'.
*  gs_alv-charg = 'L2'.
*  gs_alv-vemng = 25.
*  gs_alv-vemeh = 'UN'.
*  gs_alv-lgort = '0001'.
*  gs_alv-werks = '1000'.
*  APPEND gs_alv TO gt_alv.
*
*  IF p_exidv = '00356010459000500129'.
*    CLEAR gs_alv.
*    gs_alv-vepos = '000030'.
*    gs_alv-matnr = '60000'.
*    gs_alv-charg = 'L2'.
*    gs_alv-vemng = 50.
*    gs_alv-vemeh = 'UN'.
*    gs_alv-lgort = '0001'.
*    gs_alv-werks = '1000'.
*    APPEND gs_alv TO gt_alv.
*  ENDIF.

  SORT lt_ltap   BY matnr charg.
  SORT lt_zwm026 BY material lote.

  LOOP AT gt_alv INTO ls_alv.

    lv_tabix = sy-tabix.

    IF scr1-emb IS INITIAL.
      CLEAR: ls_alv-vbeln, ls_alv-posnr.
    ENDIF.

    CHECK ls_alv-vbeln IS INITIAL AND
          ls_alv-posnr IS INITIAL.

*   Picking
    READ TABLE lt_zwm026 WITH KEY material = ls_alv-matnr
                                  lote     = ls_alv-charg.
    IF sy-subrc = 0.
      ls_alv-vbeln = lt_zwm026-remessa.
      ls_alv-posnr = lt_zwm026-posnr.

*   Completas
    ELSE.
      READ TABLE lt_ltap WITH KEY matnr = ls_alv-matnr
                                  charg = ls_alv-charg.
      IF sy-subrc = 0.
        ls_alv-vbeln = lt_ltap-vbeln.
        ls_alv-posnr = lt_ltap-posnr.
      ENDIF.
    ENDIF.

    MODIFY gt_alv INDEX lv_tabix FROM ls_alv.
  ENDLOOP.

  CALL METHOD gc_alv_grid->refresh_table_display.

ENDFORM.                    " GET_DATA_FROM_SSCC
*&---------------------------------------------------------------------*
*&      Form  DELETE_SSCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_sscc.

  DATA: lv_selected_node TYPE lvc_nkey.
  DATA: lv_answer        TYPE c.
  DATA: lv_item_name     TYPE lvc_fname.
  DATA: ls_zwm056        TYPE zwm056.
  DATA: ls_alv           TYPE st_alv_tree.
  DATA: ls_vekp          TYPE vekp.
  DATA: lt_selected      TYPE lvc_t_nkey.
  DATA: lt_return	       LIKE	bapiret2 OCCURS 0 WITH HEADER LINE.

** Verificar selecção
**********************************************************************
  CALL METHOD tree->get_selected_item
    IMPORTING
      e_selected_node = lv_selected_node
      e_fieldname     = lv_item_name.

  IF lv_selected_node IS INITIAL.
    CALL METHOD tree->get_selected_nodes
      CHANGING
        ct_selected_nodes = lt_selected
      EXCEPTIONS
        cntl_system_error = 1
        dp_error          = 2
        failed            = 3
        OTHERS            = 4.

    LOOP AT lt_selected INTO lv_selected_node.

      CALL METHOD tree->get_outtab_line
        EXPORTING
          i_node_key    = lv_selected_node
        IMPORTING
          e_outtab_line = ls_alv.
      EXIT.
    ENDLOOP.

  ELSE.
    CALL METHOD tree->get_outtab_line
      EXPORTING
        i_node_key    = lv_selected_node
      IMPORTING
        e_outtab_line = ls_alv.
  ENDIF.

** Validar se SSCC já foi embalado
  CHECK ls_alv-exidv IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ls_alv-exidv
    IMPORTING
      output = ls_alv-exidv.

  CLEAR ls_vekp.
  SELECT SINGLE *
    FROM vekp INTO ls_vekp
    WHERE exidv = ls_alv-exidv.

  IF ls_vekp-vpobj = '01' OR
     ls_vekp-vpobj = '04'.
    MESSAGE s047 WITH ls_alv-exidv DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

** Questionar Utilizador
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = 'N'
      textline1      = 'Deseja eliminar SSCC'
      textline2      = ls_alv-exidv
      titel          = 'Eliminar SSCC'
      cancel_display = 'X'
    IMPORTING
      answer         = lv_answer.

  CHECK lv_answer = 'J'.

** Eliminar SSCC
**********************************************************************
  CLEAR ls_zwm056.
  ls_zwm056-exidv  = ls_alv-exidv.
  ls_zwm056-refnr  = scr1-refnr.
  ls_zwm056-vbeln  = scr1-vbeln.
  ls_zwm056-uname  = sy-uname.
  ls_zwm056-erdat  = sy-datum.
  ls_zwm056-erzeit = sy-uzeit.

  MODIFY zwm056 FROM ls_zwm056.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
  ENDIF.

*  CALL FUNCTION 'BAPI_HU_DELETE'
*    EXPORTING
*      hukey  = ls_alv-exidv
*    TABLES
*      return = lt_return.
*
*  READ TABLE lt_return WITH KEY type = 'E'.
*  IF sy-subrc = 0.
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
*    MESSAGE ID lt_return-id TYPE lt_return-type NUMBER lt_return-number
*    WITH lt_return-message_v1 lt_return-message_v2 lt_return-message_v3 lt_return-message_v4.
*    EXIT.
*
*  ELSE.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*  ENDIF.

** Actualizar ALV
  CALL METHOD tree->delete_subtree
    EXPORTING
      i_node_key                = lv_selected_node
      i_update_parents_expander = ''
      i_update_parents_folder   = 'X'
    EXCEPTIONS
      node_key_not_in_model     = 1
      OTHERS                    = 2.

  DELETE gt_alv_tree WHERE exidv = ls_alv-exidv.

  IF ls_alv-exidv = scr1-exidv OR gt_alv_tree[] IS INITIAL.
    CLEAR: scr1-exidv, scr1-emb.
    REFRESH: gt_alv.

    CALL METHOD gc_alv_grid->refresh_table_display.
  ENDIF.

  CALL METHOD tree->update_calculations.
  CALL METHOD tree->frontend_update.

ENDFORM.                    " DELETE_SSCC
*&---------------------------------------------------------------------*
*&      Form  EDIT_SSCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM edit_sscc.

  DATA: ls_alv  TYPE st_alv_grid.
  DATA: ls_vekp TYPE vekp.

  IF gc_alv_grid->is_ready_for_input( ) = 0.

    CHECK gt_alv[] IS NOT INITIAL.

    READ TABLE gt_alv INDEX 1 INTO ls_alv.

    CLEAR ls_vekp.
    SELECT SINGLE *
      FROM vekp INTO ls_vekp
      WHERE venum = ls_alv-venum.

    IF ls_vekp-vpobj = '01' OR
       ls_vekp-vpobj = '04'.
      MESSAGE s047 WITH scr1-exidv DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CALL METHOD gc_alv_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD g_toolbar->set_button_state
      EXPORTING
        enabled          = 'X'
        fcode            = 'SAVE'
      EXCEPTIONS
        cntl_error       = 1
        cntb_error_fcode = 2
        OTHERS           = 3.

  ELSE.

    CALL METHOD gc_alv_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.

    CALL METHOD g_toolbar->set_button_state
      EXPORTING
        enabled          = ''
        fcode            = 'SAVE'
      EXCEPTIONS
        cntl_error       = 1
        cntb_error_fcode = 2
        OTHERS           = 3.
  ENDIF.

ENDFORM.                    " EDIT_SSCC
*&---------------------------------------------------------------------*
*&      Form  SAVE_SSCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_sscc.
  DATA: lv_valid  TYPE char1.
  DATA: lv_tabix  LIKE sy-tabix.
  DATA: ls_vekp   TYPE vekp.
  DATA: ls_alv    TYPE st_alv_grid.
  DATA: lt_return TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF lt_vepo OCCURS 0.
          INCLUDE STRUCTURE vepo.
*  DATA: upd_del TYPE char1.
  DATA: END OF lt_vepo.

** Obter modificações
**********************************************************************
  CALL METHOD gc_alv_grid->check_changed_data
    IMPORTING
      e_valid = lv_valid.

  CHECK lv_valid = 'X'.

  LOOP AT gt_alv INTO ls_alv WHERE vemng IS INITIAL.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    MESSAGE s044 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

** Sem dados - eliminar HU
  IF gt_alv[] IS INITIAL.
    PERFORM delete_sscc.
  ELSE.

** Obter dados SSCC
**********************************************************************
    SELECT SINGLE *
      FROM vekp INTO ls_vekp
      WHERE exidv = scr1-exidv.

    IF sy-subrc = 0.
      SELECT *
        FROM vepo INTO TABLE lt_vepo
        WHERE venum = ls_vekp-venum.
    ENDIF.

** Validar Modificações
**********************************************************************
    LOOP AT lt_vepo.
      lv_tabix = sy-tabix.

      READ TABLE gt_alv WITH KEY venum = lt_vepo-venum
                                 vepos = lt_vepo-vepos INTO ls_alv.
      IF sy-subrc = 0.
        IF lt_vepo-vemng <> ls_alv-vemng.
          lt_vepo-vemng  = ls_alv-vemng.
        ELSE.
          DELETE lt_vepo INDEX lv_tabix.
          CONTINUE.
        ENDIF.
      ELSE.
        lt_vepo-vemng = 0.
      ENDIF.

      MODIFY lt_vepo INDEX lv_tabix TRANSPORTING vemng.
    ENDLOOP.

** Gravar modificações
**********************************************************************
    REFRESH lt_return.

    CALL FUNCTION 'ZWM_HU_PACK_AND_UNPACK'
      EXPORTING
        i_exidv  = scr1-exidv
      TABLES
        t_items  = lt_vepo
        t_return = lt_return
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF sy-subrc <> 0.
      READ TABLE lt_return INDEX 1.

      MESSAGE ID lt_return-msgid TYPE lt_return-msgtyp NUMBER lt_return-msgnr
      WITH lt_return-msgv1 lt_return-msgv2 lt_return-msgv3 lt_return-msgv4.
      EXIT.
    ENDIF.

** Actualizar ALV.
    PERFORM get_data_from_sscc USING scr1-exidv.

*   SSCC & modificado!
    MESSAGE s045 WITH scr1-exidv.
  ENDIF.

  PERFORM edit_sscc.

ENDFORM.                    " SAVE_SSCC
*&---------------------------------------------------------------------*
*&      Form  REMOVE_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM remove_item.

  DATA: lt_rows TYPE lvc_t_row.

** Obter linhas a remover
  CALL METHOD gc_alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

ENDFORM.                    " REMOVE_ITEM
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM data_changed  USING pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  DATA: ls_s_modi TYPE lvc_s_modi.
  DATA: ls_alv    TYPE st_alv_grid.

  LOOP AT pr_data_changed->mt_mod_cells INTO ls_s_modi.

    READ TABLE gt_alv INDEX ls_s_modi-row_id INTO ls_alv.
    CHECK sy-subrc = 0.

    CASE ls_s_modi-fieldname.
      WHEN 'VEMNG'.

        IF ls_s_modi-value IS INITIAL.
          CALL METHOD pr_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_s_modi-row_id
              i_tabix     = ls_s_modi-tabix
              i_fieldname = ls_s_modi-fieldname
              i_value     = ls_alv-vemng.

          MESSAGE s044 DISPLAY LIKE 'E'.
        ENDIF.

    ENDCASE.
  ENDLOOP.
*    READ TABLE gt_alv_init INDEX ls_s_modi-row_id.
*    CHECK sy-subrc = 0.
*
*MESSAGE s255(zbcmsg001) WITH gt_alv_init-matnr ls_s_modi-value DISPLAY LIKE 'E'.




ENDFORM.                    " DATA_CHANGED
