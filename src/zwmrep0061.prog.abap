************************************************************************
*                                                                      *
* Criação                                                              *
*  Autor: Ricardo Lopes - ROFF                                         *
*  Data:  25.05.2005                                                   *
*  Descrição: Lista de TO's por User.                                  *
*
* Modificações
*  Autor:
*  Data:
*  Descrição:
*  Pesquisar alterações por:
*
************************************************************************

REPORT zwmrep0061 MESSAGE-ID zwmmsg001 .

************************************************************************
** Includes
************************************************************************

INCLUDE zwmrep0061_dados.
INCLUDE zwmrep0061_ecra.

************************************************************************
** Initialization
************************************************************************
INITIALIZATION.

  PERFORM get_user_data.

************************************************************************
** Start-of-Selection
************************************************************************
START-OF-SELECTION.

*  IF sy-sysid = 'SS2'.
*    MESSAGE s000 WITH
*         'Executar este Relatório Só na Maquina de Qualidade'.
*    EXIT.
*  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = text-001.

  PERFORM select_data.

  IF itab[] IS INITIAL.
*   Não existem dados para processar !
    MESSAGE s147(zwmmsg001).
    EXIT.
  ENDIF.

  CLEAR not_first_time.

END-OF-SELECTION.

  CALL SCREEN 100.

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

  IF NOT itab[] IS INITIAL.
    IF flag_tree IS INITIAL.
      PERFORM cria_hierarquia.
      flag_tree = 'X'.
    ENDIF.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.                    "status_0100 OUTPUT

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
    WHEN 'REFR'.
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

  DATA: lt_expand_nodes           TYPE lvc_t_nkey,
        l_toolbar_excluding       TYPE ui_functions,
        lt_list_commentary        TYPE slis_t_listheader,
        l_logo                    TYPE sdydo_value,
        l_hierarchy_header        TYPE treev_hhdr,
        l_tree_container_name(30) TYPE c,
        l_custom_container        TYPE REF TO cl_gui_custom_container,
        ls_variant                TYPE disvariant.

  REFRESH gt_out.

* create fieldcatalog for structure t001
  PERFORM build_fieldcatalog.

* create container for alv-tree

  l_tree_container_name = 'TREE1'.

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

    ENDIF.
  ENDIF.

* create tree control
  CREATE OBJECT tree1
    EXPORTING
      parent                      = l_custom_container
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
      item_selection              = 'X'
      no_html_header              = ''
      no_toolbar                  = ''
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.
  IF sy-subrc <> 0.

  ENDIF.

* create Hierarchy-header
  PERFORM build_hierarchy_header CHANGING l_hierarchy_header.

* create info-table for html-header
  PERFORM build_comment USING
                 lt_list_commentary
                 l_logo.

* repid for saving variant
  ls_variant-report = sy-repid.

*  perform define_toolbar_excluding changing l_toolbar_excluding.

  l_logo = 'RENOVA_LOGO'.

** Opções da Toolbar a excluir
  PERFORM exluir_toolbar CHANGING l_toolbar_excluding.

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

** get toolbar control
  CALL METHOD tree1->get_toolbar_object
    IMPORTING
      er_toolbar = mr_toolbar.

* set event-handler for toolbar-control
  CREATE OBJECT toolbar_event_receiver.
  SET HANDLER toolbar_event_receiver->on_function_selected
                                                      FOR mr_toolbar.

ENDFORM.                               " change_toolbar

*&--------------------------------------------------------------------*
*&      Form  cria_hierarquia
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM cria_hierarquia.

  CALL METHOD tree1->delete_all_nodes.

** create hierarchy
  PERFORM create_hierarchy.

** this method must be called to send the data to the frontend
*  CALL METHOD tree1->frontend_update.

**** Expand first level
  CALL METHOD tree1->expand_node
    EXPORTING
      i_node_key = g_top_node_key.

*** Determina top_node
  CALL METHOD tree1->get_top_node
    IMPORTING
      e_node_key = g_top_node.

* adjust column_width
  CALL METHOD tree1->column_optimize.

ENDFORM.                    " init_tree

*&---------------------------------------------------------------------*
*&      Form  build_fieldcatalog
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM build_fieldcatalog.

  DATA: pos TYPE i VALUE 1.

  DATA: aux_cat TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE.

  REFRESH aux_cat.
  CLEAR itab.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'FILA_OUT'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Fila/Equipamento'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'TOTAL_TO'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Total de TO'.
  aux_cat-just          = 'C'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PERC_TO'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = '% Data/Equip.'.
  aux_cat-just          = 'C'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PERC_TO_EQUIP'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = '% Equip./Fila'.
  aux_cat-just          = 'C'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'TAPOS'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Item TO'.
  aux_cat-just          = 'C'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'VLTYP'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Origem'.
  aux_cat-just          = 'C'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'NLTYP'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Destino'.
  aux_cat-just          = 'C'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'MATNR'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Material'.
  aux_cat-just          = 'C'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'MAKTX'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Descrição'.
  aux_cat-just          = 'L'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'CHARG'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Lote'.
  aux_cat-just          = 'L'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'VSOLM'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Quant.'.
  aux_cat-just          = 'R'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'MEINS'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'UM'.
  aux_cat-just          = 'L'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'LETYP'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Tip.Pal.'.
  aux_cat-just          = 'L'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'EZEIT'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Hora conf.TO'.
  aux_cat-just          = 'L'.
  APPEND aux_cat.
  ADD 1 TO pos.

*---------------------------------------------------------
  gt_fieldcatalog[] = aux_cat[].

ENDFORM.                               " build_fieldcatalog

*&--------------------------------------------------------------------*
*&      Form  ajusta_propriedades
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM ajusta_propriedades. " TABLES sort     TYPE slis_t_sortinfo_alv.

  DATA ls_sort_wa TYPE lvc_s_sort.

* Ordenação
  REFRESH gt_sort.

* create sort-table
  ls_sort_wa-spos = 1.
  ls_sort_wa-fieldname = 'FILA_OUT'.
  ls_sort_wa-up = 'X'.
  ls_sort_wa-subtot = 'X'.
  APPEND ls_sort_wa TO gt_sort.

  ls_sort_wa-spos = 2.
  ls_sort_wa-fieldname = 'TANUM'.
  ls_sort_wa-up = 'X'.
  ls_sort_wa-subtot = 'X'.
  ls_sort_wa-no_out = 'X'.
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

  p_hierarchy_header-heading = 'User/Data Criação/Equipamento/Fila'.
  p_hierarchy_header-tooltip = 'User/Data Criação/Equipamento/Fila'.
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
*        l_index LIKE sy-tabix,
        lsx_out TYPE t_out.

  DATA: l_fila_key   TYPE lvc_nkey,
        l_status_key TYPE lvc_nkey,
        l_lgnum_key  TYPE lvc_nkey,
        l_user_key   TYPE lvc_nkey,
        l_data_key   TYPE lvc_nkey,
        l_equip_key  TYPE lvc_nkey,
        l_last_key   TYPE lvc_nkey.

  IF not_first_time = 'X'.
    PERFORM select_data.

    CHECK NOT itab[] IS INITIAL.

    IF itab[] IS INITIAL.
*     Não existem dados para processar !
      MESSAGE s196(zwmmsg001).
*     Coloca apenas 1o Nível
      ls_out-fila_out = 'Data'.
      PERFORM add_all_line USING ls_out
                                 ''
                        CHANGING l_fila_key.
      g_top_node = l_fila_key.
    ENDIF.
  ENDIF.

** Não lista se não existirem dados
  CHECK NOT itab[] IS INITIAL.

  REFRESH gt_out.

  CLEAR: itab, ls_out, total_dia.
  SORT itab BY lgnum ename bdatu equipamento fila status_to tanum tapos ezeit.

  LOOP AT itab.
    ls_out = itab.

    AT NEW lgnum.
      PERFORM add_all_line USING ls_out
                                 ''
                        CHANGING l_lgnum_key.
      g_top_node_key = l_lgnum_key.
    ENDAT.

    lsx_out = ls_out.

    AT NEW ename.
      PERFORM add_user_line USING lsx_out
                                 l_lgnum_key
                        CHANGING l_user_key.
    ENDAT.

    AT NEW data.
      PERFORM add_data_line USING lsx_out
                                 l_user_key
                        CHANGING l_data_key.
    ENDAT.

    AT NEW equipamento.
      PERFORM add_equip_line USING lsx_out
                                 l_data_key
                        CHANGING l_equip_key.
    ENDAT.

    AT NEW fila.
      PERFORM add_fila_line USING lsx_out
                                 l_equip_key
                        CHANGING l_fila_key.
    ENDAT.

    AT NEW status_to.
      PERFORM add_status_line USING lsx_out
                                   l_fila_key
                          CHANGING l_status_key.
    ENDAT.

    PERFORM add_complete_line USING ls_out
                                    l_status_key
                           CHANGING l_last_key.

    AT END OF data.
      CLEAR: total_dia.
    ENDAT.
  ENDLOOP.

  not_first_time = 'X'.

ENDFORM.                    "create_hierarchy

*&--------------------------------------------------------------------*
*&      Form  add_all_line
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->PS_OUT     text
*      -->P_RELAT_KEYtext
*      -->P_NODE_KEY text
*---------------------------------------------------------------------*
FORM add_all_line USING     ps_out TYPE t_out
                               p_relat_key TYPE lvc_nkey
                     CHANGING  p_node_key TYPE lvc_nkey.

  TABLES: t300t.

  DATA: l_node_text TYPE lvc_value,
        ls_out TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
  APPEND ls_item_layout TO lt_item_layout.

  CLEAR: t300t.
  SELECT SINGLE * FROM t300t
  WHERE spras EQ sy-langu
    AND lgnum EQ p_lgnum.

** add node
  CONCATENATE p_lgnum '-' t300t-lnumt INTO l_node_text
            SEPARATED BY space.

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
  APPEND ls_item_layout TO lt_item_layout.

  ls_out = ps_out.

  CLEAR: ls_out-total_to.

  l_node_text =  ps_out-tanum.

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
  l_event-eventid = cl_gui_column_tree=>eventid_header_click.
  APPEND l_event TO lt_events.
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

  ENDIF.

* set Handler
  DATA: l_event_receiver TYPE REF TO lcl_tree_event_receiver.
  CREATE OBJECT l_event_receiver.

  SET HANDLER l_event_receiver->handle_item_double_click FOR tree1.

ENDFORM.                               " register_events


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
  ls_line-info = 'Lista de Filas'(t00).
  APPEND ls_line TO pt_list_commentary.

** STATUS LINE: TYPE S
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Armazém:'(t01).
  ls_line-info = p_lgnum.
  APPEND ls_line TO pt_list_commentary.
*
** Filas
  CLEAR ls_line.
  DESCRIBE TABLE s_queue LINES l_lines.
  IF l_lines > 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Fila:'(t06).

    CASE l_lines.
      WHEN 1.
        l_tabix = 1.
        PERFORM fill_title USING 'D'
                                 s_queue-low
                                 s_queue-high
                                 l_tabix
                        CHANGING ls_line-info.
      WHEN OTHERS.
        LOOP AT s_queue.
          l_tabix = sy-tabix.
          PERFORM fill_title USING 'D'
                                   s_queue-low
                                   s_queue-high
                                   l_tabix
                          CHANGING ls_line-info.
        ENDLOOP.
    ENDCASE.
    APPEND ls_line TO pt_list_commentary.
  ENDIF.

** Data: Inicio
  CLEAR ls_line.
  DESCRIBE TABLE s_data LINES l_lines.
  IF l_lines > 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Data Criação da TO:'(t04).

    CASE l_lines.
      WHEN 1.
        l_tabix = 1.
        PERFORM fill_title USING 'D'
                                 s_data-low
                                 s_data-high
                                 l_tabix
                        CHANGING ls_line-info.
      WHEN OTHERS.
        LOOP AT s_data.
          l_tabix = sy-tabix.
          PERFORM fill_title USING 'D'
                                   s_data-low
                                   s_data-high
                                   l_tabix
                          CHANGING ls_line-info.
        ENDLOOP.
    ENDCASE.
    APPEND ls_line TO pt_list_commentary.
  ENDIF.
*
** Indicador Em carga: Inicio
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Indicador de TO:'(t05).
  ls_line-info = text-p05.
  APPEND ls_line TO pt_list_commentary.
*
** ACTION LINE: TYPE A
  CLEAR ls_line.
  ls_line-typ  = 'A'.
  GET TIME.
  WRITE sy-datum TO aux_s_date.
  CONCATENATE text-t07 aux_s_date INTO text
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
*  l_index = l_selected_node.                                " + 1.

  CASE l_item_name.
    WHEN '&Hierarchy'.
      READ TABLE gt_out INTO gs_out INDEX l_index.
      CHECK sy-subrc = 0.
      CHECK NOT gs_out-tanum IS INITIAL.
      SET PARAMETER ID 'LGN' FIELD p_lgnum.
      SET PARAMETER ID 'TAN' FIELD gs_out-tanum.
      SET PARAMETER ID 'TAP' FIELD gs_out-tapos.
      CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN.

    WHEN 'TANUM'.
      READ TABLE gt_out INTO gs_out INDEX l_index.
      CHECK sy-subrc = 0.
      CHECK NOT gs_out-tanum IS INITIAL.
      SET PARAMETER ID 'LGN' FIELD p_lgnum.
      SET PARAMETER ID 'TAN' FIELD gs_out-tanum.
      SET PARAMETER ID 'TAP' FIELD gs_out-tapos.
      SET PARAMETER ID 'LGN' FIELD p_lgnum.
      CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN.

  ENDCASE.

ENDFORM.                    "event_double_click

*&---------------------------------------------------------------------*
*&      Form  GET_USER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_user_data .

  CLEAR: usr01.
  SELECT SINGLE * FROM usr01
  WHERE bname = sy-uname.

ENDFORM.                    " GET_USER_DATA
*&---------------------------------------------------------------------*
*&      Form  exluir_toolbar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_EXCL_FUNC  text
*----------------------------------------------------------------------*
FORM exluir_toolbar  CHANGING ct_excl_func
                                     TYPE ui_functions.

  CLEAR ct_excl_func.
  APPEND '&CALC'                TO ct_excl_func.

ENDFORM.                    " exluir_toolbar

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.

  DATA: itab_zwm010 LIKE zwm010 OCCURS 0 WITH HEADER LINE,
         l_data(10),
         l_flag_952,
         l_count_pal(3) TYPE p DECIMALS 1,
         l_index     LIKE sy-tabix,
         l_idx       LIKE sy-tabix.

  RANGES: r_pquit FOR ltap-pquit,
          r_pvqui FOR ltap-pvqui,
          r_queue FOR ltak-queue.

  FREE: itab, itab_dados, itab_to, itab_data.
  CLEAR: itab, itab_dados, itab_to, itab_data.

  SELECT * FROM zwm010
  INTO CORRESPONDING FIELDS OF TABLE itab_zwm010
  WHERE armazem EQ p_lgnum
    AND queue   IN s_queue.

  IF s_queue[] IS INITIAL.
    itab_zwm010-equipamento = 'CONVENCIONAL'.
    itab_zwm010-queue = 'QUEUERM'.
    APPEND itab_zwm010.
  ELSE.
    IF 'QUEUERM' IN s_queue.
      itab_zwm010-equipamento = 'CONVENCIONAL'.
      itab_zwm010-queue = 'QUEUERM'.
      APPEND itab_zwm010.
    ENDIF.
  ENDIF.

  CHECK NOT itab_zwm010[] IS INITIAL.

  r_queue-sign = 'I'.
  r_queue-option = 'EQ'.

  LOOP AT itab_zwm010.
    IF itab_zwm010-queue <> 'QUEUEC'.
      r_queue-low = itab_zwm010-queue.
      COLLECT r_queue.
      SORT r_queue.
    ENDIF.
  ENDLOOP.

  SELECT *
  FROM ltak AS h INNER JOIN ltap AS i ON h~tanum = i~tanum AND
                                         h~lgnum = i~lgnum
         INTO CORRESPONDING FIELDS OF TABLE itab_dados
         WHERE h~lgnum EQ p_lgnum
           AND h~queue IN r_queue
           AND ( i~qdatu IN s_data OR i~edatu IN s_data )
           AND ( i~qzeit IN s_hora OR i~ezeit IN s_hora )
           AND h~bwlvs IN s_bwlvs
           AND ( i~ename IN s_ename OR i~qname IN s_ename )
           AND i~pquit IN r_pquit
           AND i~pvqui IN r_pvqui
           AND i~vorga NE 'ST'. "Estornos

  CHECK sy-subrc EQ 0.

  DELETE itab_dados WHERE queue = 'QUEUERM' AND ( nltyp <> 'PRM' AND nltyp <> 'INC' ).

  DELETE itab_dados WHERE vltyp = '815'.

  SORT itab_zwm010 BY queue.

  SORT itab_dados BY tanum tapos bdatu queue.

  CLEAR itab_dados_aux.
  REFRESH itab_dados_aux.

  LOOP AT itab_dados.

    IF itab_dados-queue = 'QUEUEPV' OR
       itab_dados-queue = 'QUEUERM'.
      itab_dados-bdatu = itab_dados-qdatu.
      itab_dados-ezeit = itab_dados-qzeit.
      itab_dados-ename = itab_dados-qname.
    ELSE.
      itab_dados-bdatu = itab_dados-edatu.
    ENDIF.
    MODIFY itab_dados INDEX sy-tabix.

    IF itab_dados-queue = 'QUEUEPT' OR
       itab_dados-queue = 'QUEUE1' OR
       itab_dados-queue = 'QUEUEDIC'.

      IF   itab_dados-ename IN s_ename AND
           itab_dados-edatu IN s_data  AND
           itab_dados-ezeit IN s_hora.

        IF itab_dados-qname IN s_ename AND
           itab_dados-qdatu IN s_data  AND
           itab_dados-qzeit IN s_hora .

          IF itab_dados-vltyp = 'TRI' OR itab_dados-nltyp = 'TRI'.
            itab_dados-ename = itab_dados-qname.
            itab_dados-bdatu = itab_dados-qdatu.
            itab_dados-ezeit = itab_dados-qzeit.
            MOVE-CORRESPONDING itab_dados TO itab_dados_aux.
            APPEND itab_dados_aux.
          ENDIF.
        ENDIF.
      ELSE.
        IF itab_dados-qname IN s_ename AND
           itab_dados-qdatu IN s_data  AND
           itab_dados-qzeit IN s_hora.

          itab_dados-ename = itab_dados-qname.
          itab_dados-bdatu = itab_dados-qdatu.
          itab_dados-ezeit = itab_dados-qzeit.
          MODIFY itab_dados INDEX sy-tabix.

        ENDIF.
      ENDIF.

    ENDIF.
  ENDLOOP.

  LOOP AT itab_dados_aux.
    MOVE-CORRESPONDING itab_dados_aux TO itab_dados.
    APPEND itab_dados.
  ENDLOOP.

  DELETE itab_dados WHERE ename IS INITIAL.
  DELETE itab_dados WHERE NOT ename IN s_ename.
  DELETE itab_dados WHERE NOT bdatu IN s_data.
  DELETE itab_dados WHERE NOT ezeit IN s_hora.

  CLEAR: l_flag_952.

  CLEAR t340.
  SELECT SINGLE * FROM t340 WHERE tcode = 'LT21'.

  IF t340-trtyp = 'A'.
    LOOP AT itab_dados  WHERE kgvnq = 'X'
                         AND posty = '1'
                         AND pquit = 'X'.
      DELETE itab_dados.
    ENDLOOP.
  ENDIF.

  LOOP AT itab_dados.
    l_idx = sy-tabix.

    MOVE-CORRESPONDING itab_dados TO itab.

    CLEAR: l_data, l_count_pal.

    CASE usr01-datfm.
      WHEN '1' OR '2' OR '3'.
        WRITE itab_dados-bdatu TO l_data USING EDIT MASK '__.__.____'.
      WHEN OTHERS.
        WRITE itab_dados-bdatu TO l_data USING EDIT MASK '____.__.__'.
    ENDCASE.

    itab-data = l_data.

    LOOP AT itab_zwm010 WHERE queue EQ itab_dados-queue.
      CONCATENATE itab-equipamento itab_zwm010-equipamento
             INTO itab-equipamento SEPARATED BY space.
    ENDLOOP.

    CLEAR: t346t.
    SELECT SINGLE * FROM t346t
    WHERE spras EQ sy-langu
      AND lgnum EQ p_lgnum
      AND queue EQ itab_dados-queue.

    itab-fila = itab_dados-queue.

    CONCATENATE itab_dados-queue '-' t346t-qutxt INTO itab-fila_out
      SEPARATED BY space.

** Obter o nº de TO's por status
    IF itab_dados-pquit EQ ' ' AND
       itab_dados-pvqui EQ ' '.
      itab_to-fila = itab-fila.
      itab_to-status_to = 'C'. "Criada
      itab_to-num_to = 1.
      itab-status_to = 'A'.
      itab-status_out = 'TO Criadas'.
    ELSEIF itab_dados-pquit EQ ' ' AND
           itab_dados-pvqui EQ 'X'.
      itab_to-fila = itab-fila.
      itab_to-status_to = 'P'. "Picada
      itab_to-num_to = 1.
      itab-status_to = 'B'.
      itab-status_out = 'TO Confirmadas'.
    ELSEIF itab_dados-pquit EQ 'X' AND
           itab_dados-pvqui EQ 'X'.
      itab_to-fila = itab-fila.
      itab_to-status_to = 'F'. "Finalizada
      itab_to-num_to = 1.
      itab-status_to = 'C'.
      itab-status_out = 'TO Finalizadas'.
    ELSEIF itab_dados-pquit EQ 'X' AND
           itab_dados-pvqui EQ ' '.
      itab_to-fila = itab-fila.
      itab_to-status_to = 'F'. "Finalizada
      itab_to-num_to = 1.
      itab-status_to = 'C'.
      itab-status_out = 'TO Finalizadas'.
    ENDIF.

** Obter a descrição do material
    CLEAR: makt.
    SELECT SINGLE maktx FROM makt
    INTO itab-maktx
    WHERE spras EQ sy-langu
      AND matnr EQ itab-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = itab-matnr
      IMPORTING
        output = itab-matnr.

** Tabelas auxiliares para totais

** Paletes Remontadas
    IF z_wm_cl_management=>is_remontada( is_data = itab_dados ) eq abap_true.
      l_count_pal = itab_to-num_to.
      CLEAR: itab_to-num_to.
      l_count_pal = l_count_pal / 2.
      itab_to-num_to = l_count_pal.
    ENDIF.

    itab_to-bdatu = itab-bdatu.
    itab_to-ename = itab-ename.
    itab_to-tanum = itab_dados-tanum.
    COLLECT itab_to.
    CLEAR: itab_to.
    SORT itab_to.

    MOVE-CORRESPONDING itab_dados TO itab_data.
    itab_data-equipamento = itab-equipamento.
    itab_data-num_to = 1.
    COLLECT itab_data.
    CLEAR: itab_data.
    SORT itab_data.

** Verificar os tipos de movimentos
    CASE itab-bwlvs.
      WHEN '950'.
        itab-vltyp = 'MEN'.
        itab-nltyp = 'CRG'.
      WHEN '952'.
        itab-vltyp = 'PKF'.
        itab-nltyp = 'CRG'.
        l_flag_952 = 'X'.
      WHEN '956'.
        itab-vltyp = 'PKF'.
        itab-nltyp = 'PPK'.
      WHEN '971'.
        itab-vltyp = 'PKR'.
        itab-nltyp = 'PCK'.
      WHEN '973'.
        itab-vltyp = 'REP'.
        itab-nltyp = 'PCK'.
      WHEN '976'.
        itab-vltyp = 'PRB'.
        itab-nltyp = 'PKB'.
      WHEN OTHERS.
        IF itab-nltyp = '916' OR
           itab-nltyp = '850'.
          itab-nltyp = 'CRG'.
        ENDIF.
    ENDCASE.

    APPEND itab.
    CLEAR: itab.

    AT END OF tanum.
      READ TABLE itab_dados INDEX l_idx.
      IF l_flag_952 = 'X'.
        LOOP AT itab_to WHERE tanum EQ itab_dados-tanum.
          itab_to-num_to = 1.
          MODIFY itab_to.
        ENDLOOP.
      ENDIF.

      CLEAR: l_flag_952.
    ENDAT.
  ENDLOOP.

  SORT itab BY lgnum ename data equipamento fila status_to tanum tapos ezeit.

ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  add_status_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LSX_OUT  text
*      -->P_L_FILA_KEY  text
*      <--P_L_STATUS_KEY  text
*----------------------------------------------------------------------*
FORM add_status_line  USING ps_out TYPE t_out
                           p_relat_key TYPE lvc_nkey
                 CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.

  CASE ps_out-status_to.
    WHEN 'A'.
      MOVE '@5C@' TO ls_item_layout-t_image.

** Nº de TO só criadas
      LOOP AT itab_to WHERE bdatu = ps_out-bdatu
                        AND fila  = ps_out-fila
                        AND ename = ps_out-ename
                        AND status_to = 'A'.
        ls_out-total_to = itab_to-num_to + ls_out-total_to.
      ENDLOOP.

    WHEN 'B'.
      MOVE '@5D@' TO ls_item_layout-t_image.

** Nº de TO só picadas
      LOOP AT itab_to WHERE bdatu = ps_out-bdatu
                        AND fila  = ps_out-fila
                        AND ename = ps_out-ename
                        AND status_to = 'P'.
        ls_out-total_to = itab_to-num_to + ls_out-total_to.
      ENDLOOP.

    WHEN 'C'.
      MOVE '@5B@' TO ls_item_layout-t_image.

** Nº de TO só finalizadas
      LOOP AT itab_to WHERE bdatu = ps_out-bdatu
                        AND fila  = ps_out-fila
                        AND ename = ps_out-ename
                        AND status_to = 'F'.
        ls_out-total_to = itab_to-num_to + ls_out-total_to.
      ENDLOOP.

  ENDCASE.

  APPEND ls_item_layout       TO lt_item_layout.

* add node
  CLEAR l_node_text.

  MOVE ps_out-status_out TO l_node_text.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    " add_status_line
*&---------------------------------------------------------------------*
*&      Form  add_fila_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUT  text
*      -->P_L_LGNUM_KEY  text
*      <--P_L_FILA_KEY  text
*----------------------------------------------------------------------*
FORM add_fila_line  USING ps_out TYPE t_out
                           p_relat_key TYPE lvc_nkey
                 CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out,
        total_to(5).

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.

  APPEND ls_item_layout       TO lt_item_layout.

** Nº de TO só criadas
  LOOP AT itab_to WHERE bdatu = ps_out-bdatu
                    AND fila  = ps_out-fila
                    AND ename = ps_out-ename
                    AND status_to = 'C'.
    ls_out-to_criada = itab_to-num_to + ls_out-to_criada.
  ENDLOOP.

** Nº de TO só picadas
  LOOP AT itab_to WHERE bdatu = ps_out-bdatu
                    AND fila  = ps_out-fila
                    AND ename = ps_out-ename
                    AND status_to = 'P'.
    ls_out-to_pende = itab_to-num_to + ls_out-to_pende.
  ENDLOOP.

** Nº de TO só finalizadas
  LOOP AT itab_to WHERE bdatu = ps_out-bdatu
                    AND fila  = ps_out-fila
                    AND ename = ps_out-ename
                    AND status_to = 'F'.
    ls_out-to_final = itab_to-num_to + ls_out-to_final.
  ENDLOOP.

  ls_out-total_to = ls_out-to_final + ls_out-to_pende +
                    ls_out-to_criada.

** Nº Total de TO
  CLEAR: ls_out-to_criada, ls_out-to_pende, ls_out-to_final.

  LOOP AT itab_data WHERE bdatu       EQ ps_out-bdatu AND
                          equipamento EQ ps_out-equipamento.

    CLEAR: ls_out-to_criada, ls_out-to_pende, ls_out-to_final.

** Nº de TO só criadas
    LOOP AT itab_to WHERE bdatu = itab_data-bdatu
                      AND fila  = itab_data-queue
                      AND ename = itab_data-ename
                      AND status_to = 'C'.
      ls_out-to_criada = itab_to-num_to + ls_out-to_criada.
    ENDLOOP.

** Nº de TO só picadas
    LOOP AT itab_to WHERE bdatu = itab_data-bdatu
                      AND fila  = itab_data-queue
                      AND ename = itab_data-ename
                      AND status_to = 'P'.
      ls_out-to_pende = itab_to-num_to + ls_out-to_pende.
    ENDLOOP.

** Nº de TO só finalizadas
    LOOP AT itab_to WHERE bdatu = itab_data-bdatu
                      AND fila  = itab_data-queue
                      AND ename = itab_data-ename
                      AND status_to = 'F'.
      ls_out-to_final = itab_to-num_to + ls_out-to_final.
    ENDLOOP.

    total_to = ls_out-to_criada + ls_out-to_pende +
                      ls_out-to_final  + total_to.
  ENDLOOP.

** % de TO's
  IF NOT total_fila IS INITIAL.
    ls_out-perc_to_equip = ls_out-total_to / total_fila * 100.
  ENDIF.

* add node
  CLEAR l_node_text.

  MOVE ps_out-fila_out TO l_node_text.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    " add_fila_line
*&---------------------------------------------------------------------*
*&      Form  add_data_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LSX_OUT  text
*      -->P_L_LGNUM_KEY  text
*      <--P_L_DATA_KEY  text
*----------------------------------------------------------------------*
FORM add_data_line  USING ps_out TYPE t_out
                           p_relat_key TYPE lvc_nkey
                 CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.

  APPEND ls_item_layout       TO lt_item_layout.

** Nº Total de TO
  LOOP AT itab_data WHERE bdatu EQ ps_out-bdatu AND
                          ename EQ ps_out-ename.

    LOOP AT itab_to WHERE bdatu = itab_data-bdatu
                      AND fila  = itab_data-queue
                      AND ename = itab_data-ename.
      ls_out-total_to = itab_to-num_to + ls_out-total_to.
    ENDLOOP.
  ENDLOOP.

  total_dia = ls_out-total_to.

* add node
  CLEAR l_node_text.

  MOVE ps_out-data TO l_node_text.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    " add_data_line
*&---------------------------------------------------------------------*
*&      Form  add_equip_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LSX_OUT  text
*      -->P_L_DATA_KEY  text
*      <--P_L_EQUIP_KEY  text
*----------------------------------------------------------------------*
FORM add_equip_line  USING ps_out TYPE t_out
                           p_relat_key TYPE lvc_nkey
                 CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.

  APPEND ls_item_layout       TO lt_item_layout.

** Nº Total de TO
  LOOP AT itab_data WHERE bdatu       EQ ps_out-bdatu       AND
                          equipamento EQ ps_out-equipamento AND
                          ename       EQ ps_out-ename.

    CLEAR: ls_out-to_criada, ls_out-to_pende, ls_out-to_final.

** Nº de TO só criadas
    LOOP AT itab_to WHERE bdatu = itab_data-bdatu
                      AND fila  = itab_data-queue
                      AND ename = itab_data-ename
                      AND status_to = 'C'.
      ls_out-to_criada = itab_to-num_to + ls_out-to_criada.
    ENDLOOP.

** Nº de TO só picadas
    LOOP AT itab_to WHERE bdatu = itab_data-bdatu
                      AND fila  = itab_data-queue
                      AND ename = itab_data-ename
                      AND status_to = 'P'.
      ls_out-to_pende = itab_to-num_to + ls_out-to_pende.
    ENDLOOP.

** Nº de TO só finalizadas
    LOOP AT itab_to WHERE bdatu = itab_data-bdatu
                      AND fila  = itab_data-queue
                      AND ename = itab_data-ename
                      AND status_to = 'F'.
      ls_out-to_final = itab_to-num_to + ls_out-to_final.
    ENDLOOP.

    ls_out-total_to = ls_out-to_criada + ls_out-to_pende +
                      ls_out-to_final  + ls_out-total_to.
  ENDLOOP.


  CLEAR: total_fila.
  total_fila = ls_out-total_to.

** % de TO's
  IF NOT total_dia IS INITIAL.
    ls_out-perc_to = ls_out-total_to / total_dia * 100.
  ENDIF.

* add node
  CLEAR l_node_text.

  MOVE ps_out-equipamento TO l_node_text.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    " add_equip_line

*&---------------------------------------------------------------------*
*&      Form  add_user_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LSX_OUT  text
*      -->P_L_LGNUM_KEY  text
*      <--P_L_USER_KEY  text
*----------------------------------------------------------------------*
FORM add_user_line  USING ps_out TYPE t_out
                           p_relat_key TYPE lvc_nkey
                 CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out.

** Set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.

  APPEND ls_item_layout       TO lt_item_layout.

** Nº Total de TO
  LOOP AT itab_to WHERE ename = ps_out-ename.
    ls_out-total_to = itab_to-num_to + ls_out-total_to.
  ENDLOOP.

* add node
  CLEAR l_node_text.

  MOVE ps_out-ename TO l_node_text.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    " add_user_line
*
