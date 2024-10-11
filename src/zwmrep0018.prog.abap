************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0018                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Criação de rotas de picking - WM                         *
* Criado por: Bruno Simões                                             *
* Criado em.: 01/03/2004                                               *
* Tipo PRG..: Report                                                   *
************************************************************************
REPORT  zwmrep0018 MESSAGE-ID zwmmsg001.

INCLUDE rlmobinc.
TYPE-POOLS: slis.

TABLES : t311,
         zwm026.

** Tabelas internas
DATA : BEGIN OF l_zwm026 OCCURS 0.
         INCLUDE STRUCTURE zwm026.
       DATA : END OF l_zwm026.

DATA : BEGIN OF pares_paletes OCCURS 0.
         INCLUDE STRUCTURE zwmpares_paletes.
       DATA : END OF pares_paletes.

DATA : BEGIN OF pares_paletes_alv OCCURS 0.
DATA : texto(20),
       recorrido(10).
        INCLUDE STRUCTURE zwmpares_paletes.
DATA : material   LIKE zwm026-material,
       maktx      LIKE makt-maktx,
       remessa    LIKE zwm026-remessa,
       grupo      LIKE zwm026-remessa,
       quantidade LIKE zwm026-quantidade,
       unidade    LIKE zwm026-unidade,
       to_number  LIKE zwm026-to_number,
       col        TYPE slis_t_specialcol_alv.
DATA : END OF pares_paletes_alv.

DATA: flag(1).
DATA: wa_026 LIKE zwm026.

******************* ALV data declare *********************
CLASS cl_gui_column_tree DEFINITION LOAD.
CLASS cl_gui_cfw DEFINITION LOAD.

DATA: tree           TYPE REF TO cl_gui_alv_tree_simple,
      aux_s_date(10),
      ok_code_0001   LIKE sy-ucomm,
      l_number       LIKE inrdp-tonumber,
      result         LIKE sy-subrc.

CONSTANTS: fcode_disp LIKE sy-ucomm VALUE 'DISP',
           picking(7) VALUE 'PICKING'.

DATA: gt_fieldcatalog TYPE lvc_t_fcat,
      gt_sort         TYPE lvc_t_sort,
      gt_tab          LIKE pares_paletes_alv OCCURS 0.

DATA: g_repid    LIKE sy-repid,
      indice_tab TYPE lvc_index.
DATA: ok_code            LIKE sy-ucomm,   "OK-Code
      g_user_command     TYPE slis_formname VALUE 'USER_COMMAND',
      g_status           TYPE slis_formname VALUE 'STANDARD_FULLSCREEN',
      grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container.


*---------------------------------------------------------------------*
*       CLASS CL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS cl_tree_event_receiver DEFINITION.

  PUBLIC SECTION.
    DATA: ucomm TYPE sy-ucomm.
    DATA: selfield TYPE slis_selfield.
*   double click
    METHODS handle_double_click
                FOR EVENT item_double_click OF cl_gui_alv_tree_simple
      IMPORTING fieldname index_outtab grouplevel.

    METHODS: on_add_hierarchy_node
                FOR EVENT on_add_hierarchy_node OF cl_gui_alv_tree_simple
      IMPORTING grouplevel
                index_outtab.
  PRIVATE SECTION.

ENDCLASS.                    "CL_TREE_EVENT_RECEIVER DEFINITION

*---------------------------------------------------------------------*
*       CLASS CL_EVENT_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS cl_tree_event_receiver IMPLEMENTATION.
* handle double_click
  METHOD handle_double_click.
    MOVE index_outtab TO indice_tab.
*    PERFORM funcao.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

  METHOD on_add_hierarchy_node.
    DATA ls_outtab_line TYPE tab.
*    ls_outtab_line-planetype = 'Note'. "#EC NOTEXT
    CALL METHOD tree->set_hierarchy_data
      EXPORTING
        is_outtab_line = ls_outtab_line.
  ENDMETHOD.                    "ON_ADD_HIERARCHY_NODE
ENDCLASS.                    "CL_TREE_EVENT_RECEIVER IMPLEMENTATION
******************* ALV data declare *********************



** Grupos de remessas
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_grupo FOR t311-refnr.
SELECTION-SCREEN SKIP.
PARAMETERS: p_back AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b0.


START-OF-SELECTION.

  PERFORM user_own_data.
*check se user esta ou nao associado ao armazem
  IF xuser-lgnum IS INITIAL.
    MESSAGE e003.
  ENDIF.


** Validações a nível do grupo de WM
  PERFORM valida_grupos.
** Seleccionar as paletes de picking criadas anteriormente
** para estes grupos e calcular as rotas
  IF result = 0.
    PERFORM carrega_paletes_picking.
    CLEAR flag.
    IF p_back IS INITIAL.
**    Apresenta ALV
      SET SCREEN '0001'.
    ELSE.
**    Executa directamente
      PERFORM confirma_porta.
    ENDIF.
  ENDIF.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  valida_grupos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_grupos .
  DATA: lt_t311 TYPE TABLE OF t311.

  DATA: ls_t311 TYPE t311.

  CLEAR : ok_code_0001,
          result.

  SELECT * FROM t311
           INTO TABLE lt_t311
           WHERE lgnum = xuser-lgnum AND
                 refnr IN s_grupo.

  IF sy-subrc <> 0.
**  Grupo não existente
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '095'.
    EXIT.
  ENDIF.

  IF xuser-lgnum EQ '100'.
    LOOP AT lt_t311 INTO ls_t311.
      IF z_wm_cl_management=>is_group_completed( is_data = ls_t311 ) EQ abap_false.
**      Atenção, ainda existem OT's por criar para o Grupo &
        MESSAGE i011(zwm001) WITH ls_t311-refnr.
        result = sy-subrc.
        EXIT.
      ENDIF.
    ENDLOOP.

  ELSE.
** Verificar - Grupo Liberado
    LOOP AT lt_t311 INTO ls_t311.
**      Grupo não liberado
      IF ls_t311-kzdru IS INITIAL.
        result = 4.
        MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '100' WITH ls_t311-refnr.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " valida_grupos


*&---------------------------------------------------------------------*
*&      Form  carrega_paletes_picking
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_paletes_picking .

  SELECT * FROM zwm026 INTO TABLE l_zwm026 BYPASSING BUFFER
           WHERE armazem = xuser-lgnum AND
                 grupo IN s_grupo.

  IF sy-subrc <> 0.
** Não existem paletes de picking para o(s) grupo(s)
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '101'.
    EXIT.
  ELSE.

    IF NOT l_zwm026[] IS INITIAL.
      CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

*      CALL FUNCTION 'ZWM_ROTA_PICKING_BACKUP'
      CALL FUNCTION 'ZWM_ROTA_PICKING'
        EXPORTING
          armazem           = xuser-lgnum
        TABLES
          l_zwm026          = l_zwm026
          pares_paletes_aux = pares_paletes.

    ENDIF.

  ENDIF.

ENDFORM.                    " carrega_paletes_picking


*&---------------------------------------------------------------------*
*&      Form  alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv .
  DATA : n_pal_picking LIKE zwm026-n_pal_picking.

  CLEAR : pares_paletes_alv.
  REFRESH : pares_paletes_alv.

** Actualização de campos para a listagem ALV
  LOOP AT pares_paletes.

    pares_paletes_alv-recorrido = pares_paletes_alv-recorrido + 1.
    pares_paletes_alv-texto = 'ROTAS DE PICKING'.
    pares_paletes_alv-pal1 = pares_paletes-pal1.
    CLEAR pares_paletes_alv-pal2.
** Carregar material
    SELECT * FROM zwm026
             WHERE n_pal_picking = pares_paletes-pal1.
      pares_paletes_alv-pal1 = pares_paletes-pal1.
      pares_paletes_alv-material = zwm026-material.
      pares_paletes_alv-remessa = zwm026-remessa.
      pares_paletes_alv-grupo = zwm026-grupo.
      pares_paletes_alv-quantidade = zwm026-quantidade.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = zwm026-unidade
          language       = sy-langu
        IMPORTING
          output         = pares_paletes_alv-unidade
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.

      pares_paletes_alv-maktx = zwm026-descricao.
      pares_paletes_alv-to_number = zwm026-to_number.
      CLEAR pares_paletes_alv-pal2.
      APPEND pares_paletes_alv.
    ENDSELECT.

    SELECT * FROM zwm026
               WHERE n_pal_picking = pares_paletes-pal2.
      pares_paletes_alv-pal1 = pares_paletes-pal2.
      pares_paletes_alv-material = zwm026-material.
      pares_paletes_alv-remessa = zwm026-remessa.
      pares_paletes_alv-grupo = zwm026-grupo.
      pares_paletes_alv-quantidade = zwm026-quantidade.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = zwm026-unidade
          language       = sy-langu
        IMPORTING
          output         = pares_paletes_alv-unidade
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.

      pares_paletes_alv-maktx = zwm026-descricao.
      pares_paletes_alv-to_number = zwm026-to_number.
      CLEAR pares_paletes_alv-pal2.
      APPEND pares_paletes_alv.
    ENDSELECT.

  ENDLOOP.

** Métodos para a impressão do ALV
  gt_tab[] = pares_paletes_alv[].

**
  IF tree IS INITIAL.
    PERFORM init_tree.
  ENDIF.

  CALL METHOD tree->refresh_table_display.
  CALL METHOD tree->expand_tree
    EXPORTING
      i_level = 1.

ENDFORM.                    " alv


*&---------------------------------------------------------------------*
*&      Form  init_tree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree .

  PERFORM fieldcat_init.

  PERFORM ajusta_propriedades.


  DATA: l_tree_container_name(30) TYPE c,
        l_custom_container        TYPE REF TO cl_gui_custom_container.

** Nome do customer control criado na tela
  l_tree_container_name = 'TREE'.

  CREATE OBJECT l_custom_container
    EXPORTING
      container_name              = l_tree_container_name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

* create tree control
  CREATE OBJECT tree
    EXPORTING
      i_parent                    = l_custom_container
      i_node_selection_mode       = cl_gui_column_tree=>node_sel_mode_multiple
      i_item_selection            = 'X'
      i_no_html_header            = ''
      i_no_toolbar                = ''
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.

* create info-table for html-header
  DATA: lt_list_commentary TYPE slis_t_listheader,
        l_logo             TYPE sdydo_value.

  PERFORM build_comment USING
                 lt_list_commentary
                 l_logo.

* repid for saving variants
  DATA: ls_variant TYPE disvariant.
  ls_variant-report = sy-repid.

* register events
  PERFORM register_events.

** Apagar registos vazios
  DELETE gt_tab WHERE material IS INITIAL.

* create hierarchy
  CALL METHOD tree->set_table_for_first_display
    EXPORTING
      it_list_commentary = lt_list_commentary
      i_logo             = l_logo
      i_background_id    = 'ALV_BACKGROUND'
      i_save             = 'A'
      is_variant         = ls_variant
    CHANGING
      it_sort            = gt_sort
      it_outtab          = gt_tab
      it_fieldcatalog    = gt_fieldcatalog.


  CALL METHOD tree->refresh_table_display.
* expand first level
  CALL METHOD tree->expand_tree
    EXPORTING
      i_level = 1.

* optimize column-width
  CALL METHOD tree->column_optimize
    EXPORTING
      i_start_column = tree->c_hierarchy_column_name
      i_end_column   = tree->c_hierarchy_column_name.


ENDFORM.                    " init_tree

*---------------------------------------------------------------------*
*       FORM fieldcat_init                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fieldcat_init.

  DATA: pos TYPE i VALUE 1.
  DATA: aux_cat TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE.
  REFRESH aux_cat.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'TEXTO'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-coltext       = 'Rotas'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'RECORRIDO'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-coltext       = 'Número de Rota'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PAL1'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-coltext       = 'Palete Picking'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'MATERIAL'.
  aux_cat-datatype       = 'CHAR'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-coltext       = 'Material'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'MAKTX'.
  aux_cat-datatype      = 'CHAR'.
  aux_cat-outputlen     = '45'.
  aux_cat-coltext       = 'Descrição'.
  APPEND aux_cat.
  ADD 1 TO pos.


  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'REMESSA'.
  aux_cat-datatype      = 'CHAR'.
  aux_cat-outputlen     = '15'.
  aux_cat-coltext       = 'Remessa'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'GRUPO'.
  aux_cat-datatype      = 'CHAR'.
  aux_cat-outputlen     = '15'.
  aux_cat-coltext       = 'Grupo'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'QUANTIDADE'.
  aux_cat-datatype      = 'QUAN'.
  aux_cat-do_sum        = 'X'.
  aux_cat-outputlen     = '25'.
  aux_cat-just          = 'R'.
  aux_cat-coltext       = 'Quantidade a retirar'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'UNIDADE'.
  aux_cat-datatype      = 'UNIT'.
  aux_cat-outputlen     = '10'.
  aux_cat-just          = 'L'.
  aux_cat-coltext       = 'Unidade'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'TO_NUMBER'.
  aux_cat-datatype      = 'CHAR'.
  aux_cat-outputlen     = '15'.
  aux_cat-coltext       = 'Número da OT'.
  APPEND aux_cat.
  ADD 1 TO pos.

  gt_fieldcatalog[] = aux_cat[].

ENDFORM.                    "FIELDCAT_INIT

*---------------------------------------------------------------------*
*       FORM ajusta_propriedades                                      *
*---------------------------------------------------------------------*
FORM ajusta_propriedades. " TABLES sort     TYPE slis_t_sortinfo_alv.

*  data: slis_layout_alv.

  DATA ls_sort_wa TYPE lvc_s_sort.
* Ordenação
  REFRESH gt_sort.

* create sort-table
  ls_sort_wa-spos = 1.
  ls_sort_wa-fieldname = 'TEXTO'.
  ls_sort_wa-up = 'X'.
  ls_sort_wa-level = ls_sort_wa-spos.
  ls_sort_wa-subtot = 'X'.
  APPEND ls_sort_wa TO gt_sort.

* create sort-table
  ls_sort_wa-spos = 2.
  ls_sort_wa-fieldname = 'RECORRIDO'.
  ls_sort_wa-up = 'X'.
  ls_sort_wa-subtot = 'X'.
  APPEND ls_sort_wa TO gt_sort.

** create sort-table
  ls_sort_wa-spos = 3.
  ls_sort_wa-fieldname = 'PAL1'.
  ls_sort_wa-up = 'X'.
  APPEND ls_sort_wa TO gt_sort.

  ls_sort_wa-spos = 4.
  ls_sort_wa-fieldname = 'MATERIAL'.
  ls_sort_wa-up = 'X'.
  APPEND ls_sort_wa TO gt_sort.

ENDFORM.                    " AJUSTA_PROPRIEDADES


*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_LIST_COMMENTARY  text
*      -->P_L_LOGO  text
*----------------------------------------------------------------------*
FORM build_comment USING
      pt_list_commentary TYPE slis_t_listheader
      p_logo             TYPE sdydo_value.

  DATA: ls_line  TYPE slis_listheader,
        text(60).

  CLEAR : text,
          aux_s_date.

* LIST HEADING LINE: TYPE H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = 'ROTAS DE PICKING'.
  APPEND ls_line TO pt_list_commentary.

  CLEAR ls_line.
  ls_line-typ  = 'A'.
  GET TIME.
  WRITE sy-datum TO aux_s_date.
  CONCATENATE 'DATA DE EXECUÇÂO: ' aux_s_date INTO text.
  ls_line-info = text.
  APPEND ls_line TO pt_list_commentary.

  CLEAR ls_line.
  ls_line-typ  = 'A'.
  ls_line-info = ' '.
  APPEND ls_line TO pt_list_commentary.

  p_logo = 'RENOVA_LOGO'.

ENDFORM.                    " build_comment


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
  CLEAR l_event.
  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_header_context_men_req.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_header_click.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_keypress.
  APPEND l_event TO lt_events.

  CALL METHOD tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

  DATA: l_event_receiver TYPE REF TO cl_tree_event_receiver.
  CREATE OBJECT l_event_receiver.
  SET HANDLER l_event_receiver->on_add_hierarchy_node FOR tree.
  SET HANDLER l_event_receiver->handle_double_click
                                                        FOR tree.
ENDFORM.                    " register_events


*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'ZWMGESTAO'.
  SET TITLEBAR 'GERAL'.

  IF flag IS INITIAL.
    PERFORM alv.
  ENDIF.
ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  acerta_zwm026
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RANGE_REFNR  text
*----------------------------------------------------------------------*
FORM acerta_zwm026.

  DATA: wa_zwm026 LIKE zwm026,
        to_ant    LIKE zwm026-to_number.

  DATA: it_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE.

** Acerta tabela ZWMRF026 no caso de entradas (TO's) repetidas
  CLEAR:   it_zwm026, to_ant.
  REFRESH: it_zwm026.

  SELECT * FROM zwm026 INTO TABLE it_zwm026
             WHERE armazem EQ '100'
               AND grupo   IN s_grupo.

  SORT it_zwm026 BY to_number n_pal_picking grupo.

  LOOP AT it_zwm026 INTO wa_zwm026.
    IF wa_zwm026-to_number EQ to_ant.
      DELETE zwm026 FROM wa_zwm026.
      COMMIT WORK.
    ELSE.
      to_ant = wa_zwm026-to_number.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " acerta_zwm026

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  DATA : n_pal_picking LIKE zwm026-n_pal_picking,
         contador(2)   VALUE '00'.

  CASE ok_code_0001.

** Confirmação das rotas de picking
    WHEN 'CONF_ROTA'.

      PERFORM confirma_porta.

  ENDCASE.
  IF ok_code_0001 <> 'CONF_ROTA'.
    flag = 'X'.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit1 INPUT.
  CALL METHOD tree->free.
  LEAVE PROGRAM.
ENDMODULE.                 " EXIT1  INPUT

*&---------------------------------------------------------------------*
*&      Form  confirma_porta
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirma_porta .

  CLEAR : l_number,
          n_pal_picking,
          l_zwm026,
          contador.

  REFRESH : l_zwm026.

  LOOP AT pares_paletes.


** Verificar se para o par de paletes em questão
** já existe um recorrido atribuído na BD
    CLEAR : zwm026.
    SELECT SINGLE * FROM zwm026
                    WHERE ( n_pal_picking = pares_paletes-pal1 OR
                            n_pal_picking = pares_paletes-pal2 ).

    IF zwm026-num_recorrido IS INITIAL.

** Criação de um número de recorrido por par de palete de picking
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '04'
          object                  = 'LVS_LENUM'
          quantity                = '1'
        IMPORTING
          number                  = l_number
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

** Actualização da BD para os pares de paletes
      CLEAR contador.
      DO 4 TIMES VARYING n_pal_picking FROM
                         pares_paletes-pal1 NEXT pares_paletes-pal2.

        IF NOT n_pal_picking IS INITIAL.
          contador = contador + 1.
          UNPACK contador TO contador.
          SELECT * FROM zwm026
                   WHERE armazem = xuser-lgnum AND
                         n_pal_picking = n_pal_picking.
            IF sy-subrc = 0.
              MOVE-CORRESPONDING zwm026 TO l_zwm026.
              l_zwm026-num_recorrido = l_number.
              CONCATENATE picking contador INTO
                    l_zwm026-pal_destino SEPARATED BY space.
              APPEND l_zwm026.
            ENDIF.
          ENDSELECT.
        ENDIF.

      ENDDO.

** Modificação da BD
*          UPDATE zwm026 FROM TABLE l_zwm026.
*          COMMIT WORK.

      LOOP AT l_zwm026 INTO wa_026.
        UPDATE zwm026 FROM wa_026.
        COMMIT WORK.
      ENDLOOP.

      CLEAR :   contador,
                l_zwm026,
                l_number.
      REFRESH : l_zwm026.

    ENDIF.
  ENDLOOP.

** Validação da tabela ZWM026
  PERFORM acerta_zwm026.

  LEAVE PROGRAM.

ENDFORM.                    " confirma_porta
