*&---------------------------------------------------------------------*
*&  Include           ZWMREP0003F01                                    *
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
 FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                          p_table_name
                          p_mark_name
                 CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA: l_ok     TYPE sy-ucomm,
         l_offset TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
   SEARCH p_ok FOR p_tc_name.
   IF sy-subrc <> 0.
     EXIT.
   ENDIF.
   l_offset = strlen( p_tc_name ) + 1.
   l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
   CASE l_ok.
     WHEN 'INSR'.                      "insert row
       PERFORM fcode_insert_row USING    p_tc_name
                                         p_table_name.
       CLEAR p_ok.

     WHEN 'DELE'.                      "delete row
       PERFORM fcode_delete_row USING    p_tc_name
                                         p_table_name
                                         p_mark_name.
       CLEAR p_ok.

     WHEN 'P--' OR                     "top of list
          'P-'  OR                     "previous page
          'P+'  OR                     "next page
          'P++'.                       "bottom of list
       PERFORM compute_scrolling_in_tc USING p_tc_name
                                             l_ok.
       CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
*     WHEN 'MARK1'.                      "mark one filled line
*       PERFORM fcode_tc_mark1_line USING p_tc_name
*                                         p_table_name
*                                         p_mark_name   .
*       CLEAR p_ok.

     WHEN 'MARK'.                      "mark all filled lines
       PERFORM fcode_tc_mark_lines USING p_tc_name
                                         p_table_name
                                         p_mark_name   .
       CLEAR p_ok.

     WHEN 'DMRK'.                      "demark all filled lines
       PERFORM fcode_tc_demark_lines USING p_tc_name
                                           p_table_name
                                           p_mark_name .
       CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

   ENDCASE.

 ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_insert_row
               USING    p_tc_name           TYPE dynfnam
                        p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA l_lines_name       LIKE feld-name.
   DATA l_selline          LIKE sy-stepl.
   DATA l_lastline         TYPE i.
   DATA l_line             TYPE i.
   DATA l_table_name       LIKE feld-name.
   FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
   FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
   FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE p_table_name '[]' INTO l_table_name. "table body
   ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
   ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
   GET CURSOR LINE l_selline.
   IF sy-subrc <> 0.                   " append line to table
     l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
     IF l_selline > <lines>.
       <tc>-top_line = l_selline - <lines> + 1 .
     ELSE.
       <tc>-top_line = 1.
     ENDIF.
   ELSE.                               " insert line into table
     l_selline = <tc>-top_line + l_selline - 1.
     l_lastline = <tc>-top_line + <lines> - 1.
   ENDIF.
*&SPWIZARD: set new cursor line                                        *
   l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
   INSERT INITIAL LINE INTO <table> INDEX l_selline.
   <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
   SET CURSOR LINE l_line.

 ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_delete_row
               USING    p_tc_name           TYPE dynfnam
                        p_table_name
                        p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA l_table_name       LIKE feld-name.

   FIELD-SYMBOLS <tc>         TYPE cxtab_control.
   FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <wa>.
   FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE p_table_name '[]' INTO l_table_name. "table body
   ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
   DESCRIBE TABLE <table> LINES <tc>-lines.

   LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

     IF <mark_field> = 'X'.
       DELETE <table> INDEX syst-tabix.
       IF sy-subrc = 0.
         <tc>-lines = <tc>-lines - 1.
       ENDIF.
     ENDIF.
   ENDLOOP.

 ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
 FORM compute_scrolling_in_tc USING    p_tc_name
                                       p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA l_tc_new_top_line     TYPE i.
   DATA l_tc_name             LIKE feld-name.
   DATA l_tc_lines_name       LIKE feld-name.
   DATA l_tc_field_name       LIKE feld-name.

   FIELD-SYMBOLS <tc>         TYPE cxtab_control.
   FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
   ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
   IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
     l_tc_new_top_line = 1.
   ELSE.
*&SPWIZARD: no, ...                                                    *
     CALL FUNCTION 'SCROLLING_IN_TABLE'
       EXPORTING
         entry_act      = <tc>-top_line
         entry_from     = 1
         entry_to       = <tc>-lines
         last_page_full = 'X'
         loops          = <lines>
         ok_code        = p_ok
         overlapping    = 'X'
       IMPORTING
         entry_new      = l_tc_new_top_line
       EXCEPTIONS
*        NO_ENTRY_OR_PAGE_ACT  = 01
*        NO_ENTRY_TO    = 02
*        NO_OK_CODE_OR_PAGE_GO = 03
         OTHERS         = 0.
   ENDIF.

*&SPWIZARD: get actual tc and column                                   *
   GET CURSOR FIELD l_tc_field_name
              AREA  l_tc_name.

   IF syst-subrc = 0.
     IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
       SET CURSOR FIELD l_tc_field_name LINE 1.
     ENDIF.
   ENDIF.

*&SPWIZARD: set the new top line                                       *
   <tc>-top_line = l_tc_new_top_line.


 ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
 FORM fcode_tc_mark_lines USING p_tc_name
                                p_table_name
                                p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
   DATA l_table_name       LIKE feld-name.

   FIELD-SYMBOLS <tc>         TYPE cxtab_control.
   FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <wa>.
   FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE p_table_name '[]' INTO l_table_name. "table body
   ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
   LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

     <mark_field> = 'X'.
   ENDLOOP.
 ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
 FORM fcode_tc_demark_lines USING p_tc_name
                                  p_table_name
                                  p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA l_table_name       LIKE feld-name.

   FIELD-SYMBOLS <tc>         TYPE cxtab_control.
   FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <wa>.
   FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE p_table_name '[]' INTO l_table_name. "table body
   ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
   LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

     <mark_field> = space.
   ENDLOOP.
 ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Form  check_carga
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM check_carga USING ret_code .
** Inicio
** No caso de se estar a atribuir uma porta de carga
** Verificar se a carga esta ja preparada no Pulmão.
*   BREAK ROFFD.
   IF fila_porta-operacao = 'CARGA'.

     SELECT SINGLE *
         FROM zwm006_aux
             WHERE num_entrada = fila_porta-talao.

     IF NOT zwm006_aux-n_transporte IS INITIAL.
       SELECT SINGLE *
           FROM vttp WHERE tknum = zwm006_aux-n_transporte.

       IF sy-subrc = 0.
         SELECT * FROM vbss WHERE vbeln = vttp-vbeln.

           SELECT SINGLE *
               FROM vbsk
                   WHERE sammg = vbss-sammg AND
                         smart = 'W'.
           IF sy-subrc = 0.
             EXIT.
           ELSE.
             CLEAR vbss.
           ENDIF.
         ENDSELECT.
       ENDIF.
       SELECT SINGLE *
           FROM zwm028
               WHERE lgnum = xuser-lgnum AND
                     refnr = vbss-sammg.


       IF zwm028-total_paletes > zwm028-paletes_pulmao.
** Carga ainda nao esta preparada no pulmao
         ret_code = 'X'.
         MESSAGE i128.
       ENDIF.
     ELSE.
** Nao tem transporte
       ret_code = 'X'.
       MESSAGE i127.

     ENDIF.
   ENDIF.
** Fim

 ENDFORM.                    " check_carga
*&---------------------------------------------------------------------*
*&      Form  inicializa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM inicializa .

   CLEAR : vttk, vttp, zwm006_aux, zwm003_aux, lfa1, tvrot, likp,
           kna1, vbfa.

   CLEAR: ok_code_0004, tipo_camiao, transportador, matricula,
          doc_carga, nome_transp, g_novo_trans.

   MOVE 'DOC_CARGA' TO cursorfield.

 ENDFORM.                    " inicializa
*&---------------------------------------------------------------------*
*&      Form  fcode_tc_mark1_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TC_NAME  text
*      -->P_P_TABLE_NAME  text
*      -->P_P_MARK_NAME  text
*----------------------------------------------------------------------*
 FORM fcode_tc_mark1_line USING p_tc_name
                                p_table_name
                                p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
   DATA l_table_name       LIKE feld-name.

   FIELD-SYMBOLS <tc>         TYPE cxtab_control.
   FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <wa>.
   FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE p_table_name '[]' INTO l_table_name. "table body
   ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
   LOOP AT <table> ASSIGNING <wa>.
     CHECK sy-tabix EQ indice_fila.
*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

     <mark_field> = 'X'.
   ENDLOOP.

 ENDFORM.                    " fcode_tc_mark1_line
*&---------------------------------------------------------------------*
*&      Form  tab_final_cargas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM tab_final_cargas .

   SORT carga1 BY porta ASCENDING.
   gt_tab_c = carga1[].

 ENDFORM.                    " tab_final_cargas
*&---------------------------------------------------------------------*
*&      Form  tab_final_descargas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM tab_final_descargas .

   SORT izwm005 BY porta ASCENDING.
   gt_tab_d = izwm005[].

 ENDFORM.                    " tab_final_descargas
*&---------------------------------------------------------------------*
*&      Form  init_tree_cargas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM init_tree_cargas .


   REFRESH : gt_tab_c. CLEAR gt_tab_c.
   PERFORM fieldcat_init_cargas.

*   PERFORM ajusta_propriedades_cargas.

* create Event Receiver
*  CREATE OBJECT treec_event_receiver.

* create container for alv-tree
   DATA: l_tree_container_namec(30) TYPE c,
         l_custom_containerc        TYPE REF TO cl_gui_custom_container.

   l_tree_container_namec = 'TREEC'.

   CREATE OBJECT l_custom_containerc
     EXPORTING
       container_name              = l_tree_container_namec
     EXCEPTIONS
       cntl_error                  = 1
       cntl_system_error           = 2
       create_error                = 3
       lifetime_error              = 4
       lifetime_dynpro_dynpro_link = 5.

*---------------------------------------------------------------Jun2005
   CREATE OBJECT splitter
     EXPORTING
       parent  = l_custom_containerc
       rows    = 2              "2
       columns = 1.            "1

   CALL METHOD splitter->get_container
     EXPORTING
       row       = 1                  "1
       column    = 1                  "1
     RECEIVING
       container = container_1.

   CALL METHOD splitter->get_container
     EXPORTING
       row       = 2                     "1
       column    = 1                     "2
     RECEIVING
       container = container_2.

*------------FIM-------------------------------------------------------

* create tree control
*----------------------------------------------------------------
   CREATE OBJECT treec
     EXPORTING
       parent                      = container_1
**         parent              = l_custom_containerc
       node_selection_mode         =
                                     cl_gui_column_tree=>node_sel_mode_multiple
*                              cl_gui_column_tree=>node_sel_mode_single
       item_selection              = 'X'
*      i_no_html_header            = ''
       no_toolbar                  = ''
       no_html_header              = 'X'
*      i_no_toolbar                = 'X'
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

*-----------------------------------------------------------Com Mai2005
*  PERFORM build_comment USING
*                 lt_list_commentary
*                 l_logo.
*----------------------------------------------------------------------
* create Hierarchy-header
   DATA: l_hierarchy_header TYPE treev_hhdr, i_nome LIKE dd02l-tabname.
   PERFORM build_hierarchy_header CHANGING l_hierarchy_header.

* repid for saving variants
   DATA: ls_variant TYPE disvariant.
   ls_variant-report = sy-repid.

* register events
   PERFORM register_events_cargas.

   CLEAR i_nome.
   i_nome = 'ZWM_AUX_CARGAS'.

* create hierarchy

   CALL METHOD treec->set_table_for_first_display
     EXPORTING
       i_structure_name    = i_nome
       is_hierarchy_header = l_hierarchy_header
*      i_logo              = l_logo
       i_background_id     = 'ALV_BACKGROUND'
       i_save              = 'A'
       is_variant          = ls_variant
     CHANGING
*      it_sort             = gt_sort_c
       it_outtab           = gt_tab_c
       it_fieldcatalog     = gt_fieldcatalog_c.


   CALL METHOD treec->frontend_update.
**   flush
*    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS EXPORTING CONTROL = GRID.
   CALL METHOD cl_gui_cfw=>flush.

* expand first level
*   CALL METHOD treec->expand_tree
*     EXPORTING
*       i_level = 1.

* optimize column-width
*   CALL METHOD treec->column_optimize
*     EXPORTING
*       i_start_column = treec->c_hierarchy_column_name
*       i_end_column   = treec->c_hierarchy_column_name.

 ENDFORM.                    " init_tree_cargas
*&---------------------------------------------------------------------*
*&      Form  init_tree_descargas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM init_tree_descargas .

   REFRESH : gt_tab_d. CLEAR gt_tab_d.

   PERFORM fieldcat_init_descargas.

***   PERFORM ajusta_propriedades_descargas.

* create Event Receiver
*  CREATE OBJECT tree_event_receiver.

* create container for alv-tree
   DATA: l_tree_container_named(30) TYPE c,
         l_custom_containerd        TYPE REF TO cl_gui_custom_container.

   l_tree_container_named = 'TREED'.

   CREATE OBJECT l_custom_containerd
     EXPORTING
       container_name              = l_tree_container_named
     EXCEPTIONS
       cntl_error                  = 1
       cntl_system_error           = 2
       create_error                = 3
       lifetime_error              = 4
       lifetime_dynpro_dynpro_link = 5.

* create tree control
   CREATE OBJECT treed
     EXPORTING
       parent                      = container_2
**         parent              = l_custom_containerd
       node_selection_mode         =
                                     cl_gui_column_tree=>node_sel_mode_multiple
       item_selection              = 'X'
*      i_no_html_header            = ''
       no_toolbar                  = ''
       no_html_header              = 'X'
*      i_no_toolbar                = 'X'
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

*-----------------------------------------------------------Com Mai2005
*  PERFORM build_comment USING
*                 lt_list_commentary
*                 l_logo.
*----------------------------------------------------------------------
* create Hierarchy-header
   DATA l_hierarchy_header_d TYPE treev_hhdr.
   PERFORM build_hierarchy_header_d CHANGING l_hierarchy_header_d.

* repid for saving variants
   DATA: ls_variant TYPE disvariant.
   ls_variant-report = sy-repid.

* register events
   PERFORM register_events_descargas.

* create hierarchy
   CALL METHOD treed->set_table_for_first_display
     EXPORTING
       is_hierarchy_header = l_hierarchy_header_d
*      it_list_commentary  = lt_list_commentary
*      i_logo              = l_logo
       i_background_id     = 'ALV_BACKGROUND'
       i_save              = 'A'
       is_variant          = ls_variant
     CHANGING
*      it_sort             = gt_sort_d
       it_outtab           = gt_tab_d
       it_fieldcatalog     = gt_fieldcatalog_d.

**   flush
*    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS EXPORTING CONTROL = GRID.
   CALL METHOD cl_gui_cfw=>flush.

* expand first level
*   CALL METHOD treed->expand_tree
*     EXPORTING
*       i_level = 1.

* optimize column-width
   CALL METHOD treed->column_optimize
     EXPORTING
       i_start_column = treed->c_hierarchy_column_name
       i_end_column   = treed->c_hierarchy_column_name.

 ENDFORM.                    " init_tree_descargas
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init_CARGAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM fieldcat_init_cargas .
   DATA: it_tabname TYPE slis_t_fieldcat_alv,
         li_tab     TYPE slis_fieldcat_alv.

**------------------------------------------------------------
   CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
     EXPORTING
       i_structure_name       = 'ZWM_AUX_CARGAS'
       i_bypassing_buffer     = 'X'
     CHANGING
       ct_fieldcat            = it_tabname
     EXCEPTIONS
       inconsistent_interface = 1
       program_error          = 2
       OTHERS                 = 3.
   IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.
*----------------------------------------------------------
   LOOP AT it_tabname INTO li_tab.

     MOVE-CORRESPONDING li_tab TO linha.
     CASE linha-fieldname.

       WHEN 'ALL'.
*         linha-scrtext_l = 'ALL'.
*         linha-scrtext_m = 'ALL'.
*         linha-scrtext_s = 'ALL'.
         linha-no_out = 'X'.


       WHEN 'NUM_ENTRADA'.
         linha-coltext = 'Talão'(006).
         linha-no_out  = 'X'.
         linha-no_zero = 'X'.

       WHEN 'REMESSA'.
         linha-coltext = 'Remessa'(019).
         linha-no_out  = 'X'.
         linha-no_zero = 'X'.

       WHEN 'DIA_REG'.
         linha-coltext = 'Dia Registo'(020).
         linha-no_zero = 'X'.

       WHEN 'HORA_REG'.
         linha-coltext = 'Hora Registo'(021).
         linha-no_zero = 'X'.

       WHEN 'PORTA'.
         linha-coltext = 'Porta'(022).
*         linha-no_zero = 'X'.

       WHEN 'MATRICULA'.
         linha-coltext = 'Matricula'(023).
*         linha-no_zero = 'X'.

       WHEN 'TRANSPORTE'.
         linha-coltext = 'Transporte'(024).
         linha-no_zero = 'X'.

       WHEN 'DOCUMENTO'.
         linha-coltext = 'Grupo'(025).
         linha-no_zero = 'X'.

       WHEN 'VTEXT'.
         linha-coltext = 'Desc. Grupo'(026).

       WHEN 'DESTINO'.
         linha-coltext = 'Destino'(027).
         linha-no_zero = 'X'.

       WHEN 'CLIENTE'.
         linha-coltext = 'Cliente'(028).
         linha-no_zero = 'X'.

       WHEN 'NOME'.
         linha-coltext = 'Nome'(029).

       WHEN 'TRANSPORTADOR'.
         linha-coltext = 'Transportador'(030).
         linha-no_zero = 'X'.

       WHEN 'OBSERVACOES'.
*         linha-coltext = 'Obs.'(031). "Ini-06/06/24-Inetum-Ajm
         linha-coltext = 'Telemóvel'(131). "Ini-06/06/24-Inetum-Ajm
         "Ini-06/06/24-Inetum-Ajm
       WHEN 'OBSERVACOES2'.
         PERFORM set_alv_text USING 'Obs.'(031) CHANGING linha.
       WHEN 'PESINI'.
         PERFORM set_alv_text USING 'Peso Inicial' CHANGING linha.
       WHEN 'PESFIM'.
         PERFORM set_alv_text USING 'Peso Final' CHANGING linha.
       WHEN 'PESTG'.
         PERFORM set_alv_text USING 'Peso Teórico Grupo' CHANGING linha.
       WHEN 'DIFKG'.
         PERFORM set_alv_text USING 'Diferença KG' CHANGING linha.
       WHEN 'DIFP'.
         PERFORM set_alv_text USING 'Diferença Percentual' CHANGING linha.
         "Fim-06/06/24-Inetum-Ajm
       WHEN 'LVSTK'.
         linha-coltext = 'St. WM'(032).

       WHEN 'ZLVTX'.
         linha-coltext = 'Desc.'(033).

       WHEN 'PKSTK'.
         linha-coltext = 'St. Embalamento'(034).

       WHEN 'ZPKTX'.
         linha-coltext = 'Desc.'(035).

       WHEN 'WBSTK'.
         linha-coltext = 'St. Saida Merc.'(036).

       WHEN 'ZWBTX'.
         linha-coltext = 'Desc.'(037).

       WHEN OTHERS.
         linha-no_out = 'X'.

     ENDCASE.

     APPEND linha. CLEAR linha.

   ENDLOOP.

   gt_fieldcatalog_c[] = linha[].

   CLEAR: linha, linha[].


 ENDFORM.                    " fieldcat_init_CARGAS
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init_DESCARGAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM fieldcat_init_descargas .

   DATA: it_tabname_d TYPE slis_t_fieldcat_alv,
         li_tab_d     TYPE slis_fieldcat_alv.

   CLEAR : it_tabname_d, li_tab_d.
**------------------------------------------------------------
   CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
     EXPORTING
       i_structure_name       = 'ZWM_AUX_DESCARGAS'
       i_bypassing_buffer     = 'X'
     CHANGING
       ct_fieldcat            = it_tabname_d
     EXCEPTIONS
       inconsistent_interface = 1
       program_error          = 2
       OTHERS                 = 3.
   IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.
*----------------------------------------------------------
   CLEAR: linha, linha[].

   LOOP AT it_tabname_d INTO li_tab_d.

     MOVE-CORRESPONDING li_tab_d TO linha.
     CASE linha-fieldname.

*       WHEN 'ALL'.
**         linha-scrtext_l = 'Talão'.
**         linha-scrtext_m = 'Talão'.
**         linha-scrtext_s = 'Talão'.
*         linha-no_out = 'X'.

       WHEN 'NUM_ENTRADA'.
         linha-coltext = 'Talão'(006).
         linha-no_out = 'X'.
         linha-no_zero = 'X'.

       WHEN 'OBSERVACOES'.
         linha-coltext   = 'Observações'(038).

       WHEN 'DATA'.
         linha-coltext = 'Dia Registo'(020).
         linha-no_zero = 'X'.

       WHEN 'HORA_ENTRADA'.
         linha-coltext = 'Hora Entrada'(039).
*         linha-no_zero = 'X'.

       WHEN 'PORTA'.
         linha-coltext = 'Porta'(040).
*         linha-no_zero = 'X'.

       WHEN 'ORD_COMPRA'.
         linha-coltext = 'Documento Compra'(041).
         linha-no_zero = 'X'.

       WHEN 'MATRICULA'.
         linha-coltext = 'Matricula'(042).
*         linha-no_zero = 'X'.

*       WHEN 'TIPO_CAMIAO'.
*         linha-scrtext_l = 'Tipo Camião'.
*         linha-scrtext_m = 'Tipo Camião'.
*         linha-scrtext_s = 'Tipo Camião'.
*Transportador: código e descrição

       WHEN 'DESC_TRANSP'.
         linha-coltext = 'Desc Transporte'(043).

       WHEN 'TRANSPORTADOR'.
         linha-coltext = 'Transportador'(044).
         linha-no_zero = 'X'.

       WHEN OTHERS.
         linha-no_out = 'X'.
         linha-no_zero = 'X'.

     ENDCASE.

     APPEND linha. CLEAR linha.

   ENDLOOP.

   gt_fieldcatalog_d[] = linha[].

 ENDFORM.                    " fieldcat_init_DESCARGAS
*&---------------------------------------------------------------------*
*&      Form  register_events_cargas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM register_events_cargas .
* define the events which will be passed to the backend
   DATA: lt_events_c TYPE cntl_simple_events,
         l_event_c   TYPE cntl_simple_event.

* define the events which will be passed to the backend
   CLEAR l_event_c.
   l_event_c-eventid = cl_gui_column_tree=>eventid_item_double_click.
   APPEND l_event_c TO lt_events_c.
* l_event_c-eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
* APPEND l_event_c TO lt_events_c.
*l_event_c-eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
*APPEND l_event_c TO lt_events_c.
*l_event_c-eventid = cl_gui_column_tree=>eventid_header_context_men_req.
*   APPEND l_event_c TO lt_events_c.
   l_event_c-eventid = cl_gui_column_tree=>eventid_expand_no_children.
   APPEND l_event_c TO lt_events_c.
*   l_event_c-eventid = cl_gui_column_tree=>eventid_header_click.
*   APPEND l_event_c TO lt_events_c.
   l_event_c-eventid = cl_gui_column_tree=>eventid_item_keypress.
   APPEND l_event_c TO lt_events_c.

   CALL METHOD treec->set_registered_events
     EXPORTING
       events                    = lt_events_c
     EXCEPTIONS
       cntl_error                = 1
       cntl_system_error         = 2
       illegal_event_combination = 3.


   DATA: l_event_receiver_c TYPE REF TO cl_tree_event_receiver.
   CREATE OBJECT l_event_receiver_c.
*   SET HANDLER l_event_receiver_c->on_add_hierarchy_node_c FOR treec.
   SET HANDLER l_event_receiver_c->handle_double_click_c FOR treec.

 ENDFORM.                    " register_events_cargas
*&---------------------------------------------------------------------*
*&      Form  ajusta_propriedades_DESCARGAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM ajusta_propriedades_descargas .

   DATA ls_sort_wa_d TYPE lvc_s_sort.
* Ordenação
   REFRESH gt_sort_d. CLEAR gt_sort_d.

* create sort-table
   ls_sort_wa_d-spos = 1.
   ls_sort_wa_d-fieldname = 'NUM_ENTRADA'.
   ls_sort_wa_d-up = 'X'.
   ls_sort_wa_d-subtot = 'X'.
   APPEND ls_sort_wa_d TO gt_sort_d.

   ls_sort_wa_d-spos = 2.
   ls_sort_wa_d-fieldname = 'MATRICULA'.
   ls_sort_wa_d-up = 'X'.
   ls_sort_wa_d-subtot = 'X'.
   APPEND ls_sort_wa_d TO gt_sort_d.

 ENDFORM.                    " ajusta_propriedades_DESCARGAS
*&---------------------------------------------------------------------*
*&      Form  register_events_DESCARGAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM register_events_descargas .
* define the events which will be passed to the backend
   DATA: lt_events_d TYPE cntl_simple_events,
         l_event_d   TYPE cntl_simple_event.

* define the events which will be passed to the backend
   CLEAR l_event_d.
   l_event_d-eventid = cl_gui_column_tree=>eventid_item_double_click.
   APPEND l_event_d TO lt_events_d.
*  l_event_d-eventid = cl_gui_column_tree=>eventid_node_context_menu_req
*   APPEND l_event_d TO lt_events_d.
*  l_event_d-eventid = cl_gui_column_tree=>eventid_item_context_menu_req
*   APPEND l_event_d TO lt_events_d.
* l_event_d-eventid = cl_gui_column_tree=>eventid_header_context_men_req
*   APPEND l_event_d TO lt_events_d.
   l_event_d-eventid = cl_gui_column_tree=>eventid_expand_no_children.
   APPEND l_event_d TO lt_events_d.
*   l_event_d-eventid = cl_gui_column_tree=>eventid_header_click.
*   APPEND l_event_d TO lt_events_d.
   l_event_d-eventid = cl_gui_column_tree=>eventid_item_keypress.
   APPEND l_event_d TO lt_events_d.

   CALL METHOD treed->set_registered_events
     EXPORTING
       events                    = lt_events_d
     EXCEPTIONS
       cntl_error                = 1
       cntl_system_error         = 2
       illegal_event_combination = 3.


   DATA: l_event_receiver_d TYPE REF TO cl_tree_event_receiver.
   CREATE OBJECT l_event_receiver_d.
*   SET HANDLER l_event_receiver_d->on_add_hierarchy_node_d FOR treed.
   SET HANDLER l_event_receiver_d->handle_double_click_d
                                                         FOR treed.

 ENDFORM.                    " register_events_DESCARGAS
*&---------------------------------------------------------------------*
*&      Form  funcao_cargas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM funcao_cargas USING field.  " group .

   READ TABLE carga1 INDEX indice_tab.

   IF field = '&Hierarchy'.

*     IF group IS INITIAL.

     SET PARAMETER ID 'VL' FIELD carga1-remessa.
     CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.

*     ELSEIF group = 'TRANSPORTE'.
*
*       SET PARAMETER ID 'TNR ' FIELD carga1-transporte.
*       CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
*
*     ENDIF.

   ELSEIF field NE '&Hierarchy'.

   ENDIF.

 ENDFORM.                    " funcao_cargas
*&---------------------------------------------------------------------*
*&      Form  funcao_descargas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM funcao_descargas .


   READ TABLE izwm005 INDEX indice_tab.


 ENDFORM.                    " funcao_descargas
*&---------------------------------------------------------------------*
*&      Form  GET_STATUS_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VTTP_VBELN  text
*----------------------------------------------------------------------*
 FORM get_status_doc  USING    p_fornecimento.

   DATA: wa_vbstt   LIKE vbstt, wa_vbuk_in LIKE  vbuk.

   CLEAR: wa_vbstt, wa_vbuk_in, carga1-lvstk, carga1-pkstk,
          carga1-wbstk, carga1-zlvtx, carga1-zpktx, carga1-zwbtx,
          wa_vbstt-lvsta_bez, wa_vbstt-pkstk_bez, wa_vbstt-wbsta_bez.

   wa_vbuk_in-vbeln = p_fornecimento.

   SELECT SINGLE lvstk
                 pkstk
                 wbstk INTO (carga1-lvstk, carga1-pkstk, carga1-wbstk)
                       FROM  vbuk
          WHERE  vbeln  = wa_vbuk_in-vbeln.


   CALL FUNCTION 'RV_DOCUMENT_HEAD_STATUS_TEXTS'
     EXPORTING
*      sprache       = sy-langu
       vbuk_in       = wa_vbuk_in
       window_senden = ' '
       vbtyp         = 'J'
     IMPORTING
       vbstt_wa      = wa_vbstt.

   carga1-zlvtx = wa_vbstt-lvsta_bez.
   carga1-zpktx = wa_vbstt-pkstk_bez.
   carga1-zwbtx = wa_vbstt-wbsta_bez.

 ENDFORM.                    " GET_STATUS_DOC
*&---------------------------------------------------------------------*
*&      Form  get_desc_grupo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VTTP_VBELN  text
*----------------------------------------------------------------------*
 FORM get_desc_grupo  USING p_vbeln p_rc.
   CLEAR : carga1-documento, carga1-vtext, p_rc.
   SELECT a~sammg
          b~vtext
     INTO (carga1-documento, carga1-vtext)
     FROM vbss AS a
     INNER JOIN vbsk AS b
     ON    a~sammg = b~sammg
     WHERE a~vbeln = p_vbeln
     AND   b~smart = 'W'.

   ENDSELECT.

   IF NOT sy-subrc IS INITIAL.
     p_rc = sy-subrc.
   ENDIF.

 ENDFORM.                    " get_desc_grupo
*&---------------------------------------------------------------------*
*&      Form  cria_hierarquia
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM cria_hierarquia_c .
   CALL METHOD treec->delete_all_nodes.

* create hierarchy
   PERFORM create_hierarchy_c.

* this method must be called to send the data to the frontend
*  call method tree1->frontend_update.

* Expand first level
   CALL METHOD treec->expand_node
     EXPORTING
       i_node_key = g_top_node_key.

* Determina top_node
   CALL METHOD treec->get_top_node
     IMPORTING
       e_node_key = g_top_node.

* adjust column_width
   CALL METHOD treec->column_optimize.

 ENDFORM.                    " cria_hierarquia
*&---------------------------------------------------------------------*
*&      Form  create_hierarchy_c
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM create_hierarchy_c .


   CLEAR: carga1, ls_out, lsx_out, l_all_key, l_talao_key,
          l_remessa_key, l_last_key.

   REFRESH : gt_tab_c. CLEAR gt_tab_c.

*-----------------------------------------------ins Mar2005
   SORT carga1 BY all num_entrada remessa.
*-----------------------------------------------end
   LOOP AT carga1 INTO ls_out.

     AT NEW all.

       PERFORM add_all_line USING ls_out
                                  ''
                         CHANGING l_all_key.
       g_top_node_key = l_all_key.

     ENDAT.

     lsx_out = ls_out.

     AT NEW num_entrada.

       PERFORM add_num_entrada_line USING lsx_out
                                    l_all_key
                           CHANGING l_talao_key.
     ENDAT.

     PERFORM add_complete_line USING lsx_out
                                     l_talao_key
                            CHANGING l_last_key.

   ENDLOOP.

 ENDFORM.                    " create_hierarchy_c
*&---------------------------------------------------------------------*
*&      Form  add_complete_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUT  text
*      -->P_L_REMESSA_KEY  text
*      <--P_L_LAST_KEY  text
*----------------------------------------------------------------------*
 FORM add_complete_line USING   ps_out LIKE carga1
                                p_relat_key TYPE lvc_nkey
                      CHANGING  p_node_key TYPE lvc_nkey.

   DATA: l_node_text TYPE lvc_value,
         ls_out      LIKE carga1.

* set item-layout
   DATA: lt_item_layout TYPE lvc_t_layi,
         ls_item_layout TYPE lvc_s_layi.

   CLEAR: lt_item_layout, lt_item_layout[], ls_item_layout.

   ls_item_layout-fieldname = treec->c_hierarchy_column_name.
   APPEND ls_item_layout TO lt_item_layout.

   MOVE ps_out TO ls_out.

*limpar campos não relevantes para linha de remessa
   CLEAR: ls_out-num_entrada,
*          ls_out-remessa,
          ls_out-documento,
          ls_out-vtext,
          ls_out-dia_reg,
          ls_out-hora_reg,
          ls_out-matricula,
          ls_out-transporte,
          ls_out-transportador,
          ls_out-observacoes,
          ls_out-porta.

   l_node_text =  ps_out-remessa.

   CALL METHOD treec->add_node
     EXPORTING
       i_relat_node_key = p_relat_key
       i_relationship   = cl_gui_column_tree=>relat_last_child
       is_outtab_line   = ls_out
       i_node_text      = l_node_text
       it_item_layout   = lt_item_layout
     IMPORTING
       e_new_node_key   = p_node_key.

 ENDFORM.                    " add_complete_line
*&---------------------------------------------------------------------*
*&      Form  add_num_entrada_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LSX_OUT  text
*      -->P_L_ALL_KEY  text
*      <--P_L_NUM_ENTRADA_OUT_KEY  text
*----------------------------------------------------------------------*
 FORM add_all_line USING   ps_out STRUCTURE carga1
                                p_relat_key TYPE lvc_nkey
                      CHANGING  p_node_key TYPE lvc_nkey.

   DATA: p_val.
   DATA: l_node_text TYPE lvc_value,
         ls_out      LIKE zwm_aux_cargas.

* set item-layout
   DATA: lt_item_layout TYPE lvc_t_layi,
         ls_item_layout TYPE lvc_s_layi.


   ls_item_layout-fieldname = treec->c_hierarchy_column_name.
   APPEND ls_item_layout TO lt_item_layout.

   MOVE ps_out-all TO ls_out-all.

   CLEAR l_node_text.

   MOVE ps_out-all TO l_node_text.

   IF l_node_text IS INITIAL.
     l_node_text =  'Talão'(006).
   ENDIF.


   CALL METHOD treec->add_node
     EXPORTING
       i_relat_node_key = p_relat_key
       i_relationship   = cl_gui_column_tree=>relat_last_child
       i_node_text      = l_node_text
       is_outtab_line   = ls_out
       it_item_layout   = lt_item_layout
     IMPORTING
       e_new_node_key   = p_node_key.

 ENDFORM.                    "add_all_line
*&---------------------------------------------------------------------*
*&      Form  add_num_entrada_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LSX_OUT  text
*      -->P_L_ALL_KEY  text
*      <--P_L_NUM_ENTRADA_OUT_KEY  text
*----------------------------------------------------------------------*
 FORM add_num_entrada_line USING   ps_out STRUCTURE carga1
                                p_relat_key TYPE lvc_nkey
                      CHANGING  p_node_key TYPE lvc_nkey.

   DATA: l_node_text TYPE lvc_value,
         ls_out      LIKE carga1,
         ls_layout   TYPE lvc_s_layn.

* set item-layout
   DATA: lt_item_layout TYPE lvc_t_layi,
         ls_item_layout TYPE lvc_s_layi.

   ls_item_layout-fieldname = treec->c_hierarchy_column_name.
   APPEND ls_item_layout TO lt_item_layout.


   APPEND ls_item_layout TO lt_item_layout.

   MOVE ps_out TO ls_out.

   MOVE ps_out-all TO ls_out-all.

   MOVE ps_out-num_entrada TO ls_out-num_entrada.

   CLEAR l_node_text.

   l_node_text = ps_out-num_entrada.

   CLEAR: ls_out-cliente,
          ls_out-nome,
          ls_out-destino,
          ls_out-lvstk,
          ls_out-zlvtx,
          ls_out-pkstk,
          ls_out-zpktx,
          ls_out-wbstk,
          ls_out-zwbtx.

   IF ls_out-carregada = 'X'.
     ls_layout-style = '6'.
   ENDIF.

   CALL METHOD treec->add_node
     EXPORTING
       i_relat_node_key = p_relat_key
       i_relationship   = cl_gui_column_tree=>relat_last_child
       i_node_text      = l_node_text
       is_outtab_line   = ls_out
       it_item_layout   = lt_item_layout
       is_node_layout   = ls_layout
     IMPORTING
       e_new_node_key   = p_node_key.

   MOVE ps_out TO ls_out.


 ENDFORM.                    " add_num_entrada_line
*&---------------------------------------------------------------------*
*&      Form  build_hierarchy_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_HIERARCHY_HEADER  text
*----------------------------------------------------------------------*
 FORM build_hierarchy_header CHANGING
                                p_hierarchy_header TYPE treev_hhdr.

   p_hierarchy_header-heading = 'Talão/Remessa'(045).
   p_hierarchy_header-tooltip = 'Talão/Remessa'(045).
   p_hierarchy_header-width = 15.
   p_hierarchy_header-width_pix = ' '.

 ENDFORM.                    " build_hierarchy_header
*&---------------------------------------------------------------------*
*&      Form  cria_hierarquia_d
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM cria_hierarquia_d .

   CALL METHOD treed->delete_all_nodes.

* create hierarchy
   PERFORM create_hierarchy_d.

* this method must be called to send the data to the frontend
*  call method tree1->frontend_update.

* Expand first level
   CALL METHOD treed->expand_node
     EXPORTING
       i_node_key = g_top_node_key_d.

* Determina top_node
   CALL METHOD treed->get_top_node
     IMPORTING
       e_node_key = g_top_node_d.

* adjust column_width
   CALL METHOD treed->column_optimize.

 ENDFORM.                    " cria_hierarquia_d
*&---------------------------------------------------------------------*
*&      Form  create_hierarchy_d
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM create_hierarchy_d .
   CLEAR: izwm005, ls_out_d, lsx_out_d, l_all_key, l_talao_key,
          l_pedido_key, l_last_key.

   REFRESH : gt_tab_d. CLEAR gt_tab_d.

*-----------------------------------------------ins Mar2005
   SORT izwm005 BY all num_entrada ord_compra.
*-----------------------------------------------end
   LOOP AT izwm005 INTO ls_out_d.

     AT NEW all.

       PERFORM add_all_line_d USING ls_out_d
                                  ''
                         CHANGING l_all_key.
       g_top_node_key_d = l_all_key.

     ENDAT.

     lsx_out_d = ls_out_d.

     AT NEW num_entrada.

       PERFORM add_num_entrada_line_d USING lsx_out_d
                                    l_all_key
                           CHANGING l_talao_key.
     ENDAT.


     IF NOT ls_out_d-ord_compra IS INITIAL.

       PERFORM add_complete_line_d USING lsx_out_d
                                       l_talao_key
                              CHANGING l_last_key.

     ENDIF.

   ENDLOOP.

 ENDFORM.                    " create_hierarchy_d
*&---------------------------------------------------------------------*
*&      Form  add_num_entrada_line_d
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LSX_OUT  text
*      -->P_L_ALL_KEY  text
*      <--P_L_TALAO_KEY  text
*----------------------------------------------------------------------*
 FORM add_num_entrada_line_d USING ps_out STRUCTURE izwm005
                                p_relat_key TYPE lvc_nkey
                      CHANGING  p_node_key TYPE lvc_nkey.

   DATA: l_node_text TYPE lvc_value,
         ls_out      LIKE izwm005.

* set item-layout
   DATA: lt_item_layout TYPE lvc_t_layi,
         ls_item_layout TYPE lvc_s_layi.

   ls_item_layout-fieldname = treed->c_hierarchy_column_name.
   APPEND ls_item_layout TO lt_item_layout.


   APPEND ls_item_layout TO lt_item_layout.

   MOVE ps_out TO ls_out.

   MOVE ps_out-all TO ls_out-all.

   MOVE ps_out-num_entrada TO ls_out-num_entrada.

   CLEAR l_node_text.

   l_node_text = ps_out-num_entrada.

   CLEAR: ls_out-ord_compra,
          ls_out-transportador, ls_out-desc_transp.

   CALL METHOD treed->add_node
     EXPORTING
       i_relat_node_key = p_relat_key
       i_relationship   = cl_gui_column_tree=>relat_last_child
       i_node_text      = l_node_text
       is_outtab_line   = ls_out
       it_item_layout   = lt_item_layout
     IMPORTING
       e_new_node_key   = p_node_key.

   MOVE ps_out TO ls_out.

 ENDFORM.                    " add_num_entrada_line_d
*&---------------------------------------------------------------------*
*&      Form  add_complete_line_d
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LSX_OUT  text
*      -->P_L_TALAO_KEY  text
*      <--P_L_LAST_KEY  text
*----------------------------------------------------------------------*
 FORM add_complete_line_d USING   ps_out LIKE izwm005
                                p_relat_key TYPE lvc_nkey
                      CHANGING  p_node_key TYPE lvc_nkey.

   DATA: l_node_text TYPE lvc_value,
         ls_out      LIKE izwm005.

* set item-layout
   DATA: lt_item_layout TYPE lvc_t_layi,
         ls_item_layout TYPE lvc_s_layi.

   CLEAR: lt_item_layout, lt_item_layout[], ls_item_layout.

   ls_item_layout-fieldname = treed->c_hierarchy_column_name.
   APPEND ls_item_layout TO lt_item_layout.

   MOVE ps_out TO ls_out.

*limpar campos não relevantes para linha de remessa
*   CLEAR: ls_out-num_entrada,
*          ls_out-remessa,
*          ls_out-documento,
*          ls_out-vtext,
*          ls_out-dia,
*          ls_out-hora,
*          ls_out-matricula,
*          ls_out-transporte,
*          ls_out-transportador,
*          ls_out-observacoes,
*          ls_out-porta.
*
*   l_node_text =  ps_out-remessa.

   CALL METHOD treed->add_node
     EXPORTING
       i_relat_node_key = p_relat_key
       i_relationship   = cl_gui_column_tree=>relat_last_child
       is_outtab_line   = ls_out
       i_node_text      = l_node_text
       it_item_layout   = lt_item_layout
     IMPORTING
       e_new_node_key   = p_node_key.

 ENDFORM.                    " add_complete_line_d
*&---------------------------------------------------------------------*
*&      Form  add_all_line_d
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUT_D  text
*      -->P_2174   text
*      <--P_L_ALL_KEY  text
*----------------------------------------------------------------------*
 FORM add_all_line_d USING   ps_out STRUCTURE izwm005
                                p_relat_key TYPE lvc_nkey
                      CHANGING  p_node_key TYPE lvc_nkey.

   DATA: l_node_text TYPE lvc_value,
         ls_out      LIKE izwm005.

* set item-layout
   DATA: lt_item_layout TYPE lvc_t_layi,
         ls_item_layout TYPE lvc_s_layi.

   ls_item_layout-fieldname = treed->c_hierarchy_column_name.
   APPEND ls_item_layout TO lt_item_layout.

   MOVE ps_out-all TO ls_out-all.

   CLEAR l_node_text.

   MOVE ps_out-all TO l_node_text.

   IF l_node_text IS INITIAL.
     l_node_text =  'Talão'.
   ENDIF.

   CALL METHOD treed->add_node
     EXPORTING
       i_relat_node_key = p_relat_key
       i_relationship   = cl_gui_column_tree=>relat_last_child
       i_node_text      = l_node_text
       is_outtab_line   = ls_out
       it_item_layout   = lt_item_layout
     IMPORTING
       e_new_node_key   = p_node_key.

 ENDFORM.                    " add_all_line_d
*&---------------------------------------------------------------------*
*&      Form  SELECT_CARGAS_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM select_cargas_1 .
   RANGES datas FOR sy-datum OCCURS 10.
   DATA: dat_aux     LIKE sy-datum,flag(1),
         aux_entrada TYPE zwm006-num_entrada,
         contador    TYPE i,
         dest        TYPE tvrot-bezei.

   DATA: izwm006 TYPE zwm006 OCCURS 0 WITH HEADER LINE.

   REFRESH: carga1,datas,izwm006.

   CLEAR flag.

   dat_aux = sy-datum.
*inicializa range
   datas-sign   = 'I'.
   datas-option = 'BT'.

*--------------------------------------------------------Ins Mai2005

***  datas-low    = dat_aux - 5.
   datas-low    = dat_aux - 30.

   datas-high   = dat_aux + 1.
   APPEND datas.


   DATA: t_vttk   LIKE vttk OCCURS 0 WITH HEADER LINE,
         t_zwm006 LIKE zwm006 OCCURS 0 WITH HEADER LINE,
         t_zwm003 LIKE zwm003 OCCURS 0 WITH HEADER LINE.

   CLEAR: t_vttk, t_zwm006, t_zwm003.
   REFRESH: t_vttk, t_zwm006, t_zwm003.



   CLEAR vttk.
   SELECT * FROM vttk
   WHERE dpreg IN datas.
     CLEAR carga1.
     CLEAR flag.
*data de fim de carregamento
     IF vttk-dalen IS INITIAL.
       CLEAR zwm006_aux.

       SELECT SINGLE * FROM zwm006_aux
       WHERE   armazem      = xuser-lgnum AND
               n_transporte = vttk-tknum AND
               finalizada   = ' '.

       IF sy-subrc = 0.
         carga1-num_entrada  = zwm006_aux-num_entrada.
         carga1-matricula    = zwm006_aux-matricula.
         carga1-porta        = zwm006_aux-porta.
         carga1-observacoes  = zwm006_aux-observacoes.
         carga1-hora_reg     = zwm006_aux-hora_reg.
         carga1-dia_reg      = zwm006_aux-data_reg.
         "Ini-06/06/24-Inetum-Ajm
         carga1-observacoes2  = zwm006_aux-observacoes2.
         "peso bascula
         carga1-pesini = zwm006_aux-pesini.
         carga1-pesfim = zwm006_aux-pesfim.
         carga1-pestg  = zwm006_aux-pestg .
         carga1-difkg  = zwm006_aux-difkg .
         carga1-difp   = zwm006_aux-difp  .
         carga1-meins  = zwm006_aux-meins.
         carga1-carregada = zwm006_aux-carregada.
         "Fim-06/06/24-Inetum-Ajm
       ELSE.
         CLEAR zwm003_aux.

         SELECT SINGLE * FROM zwm003_aux
         WHERE armazem   = xuser-lgnum
         AND   matricula = vttk-signi.

         IF sy-subrc = 0.
           carga1-num_entrada  = zwm003_aux-num_entrada.
           carga1-matricula    = zwm003_aux-matricula.
           carga1-porta        = zwm003_aux-porta.

*actualiza nºtransporte na tabela de cargas
           CLEAR zwm006_aux.

           SELECT SINGLE * FROM zwm006_aux
           WHERE armazem     = xuser-lgnum
           AND   num_entrada = zwm003_aux-num_entrada
           AND   finalizada  = ' '.

           IF sy-subrc = 0.
             MOVE-CORRESPONDING zwm006_aux TO izwm006.
             izwm006-n_transporte = vttk-tknum.
             APPEND izwm006.
           ENDIF.
         ELSE.
**se não existe na tabela zwm006 ou zwm003 passar ao proximo transporte
*
*continue.
           carga1-matricula    = vttk-signi.

         ENDIF.


       ENDIF.
       "Ini-25/06/24-Inetum-Ajm - peso Bascula
       carga1-meins = 'KG'.
       SELECT SUM( btgew )
         FROM likp
           INTO @carga1-pestg
         WHERE vbeln IN ( SELECT vbeln
                           FROM vttp
                             WHERE tknum EQ @vttk-tknum ).
       "Fim-25/06/24-Inetum-Ajm - peso Bascula

*transportador
       CLEAR lfa1.
       SELECT SINGLE * FROM lfa1
       WHERE lifnr = vttk-tdlnr.

       MOVE lfa1-name1 TO carga1-transportador.
*tipo camião
***      MOVE vttk-sdabw TO carga1-tipo.
***      MOVE vttk-dplbg TO carga1-dia_carga.
***      MOVE vttk-uplbg TO carga1-hora_carga.
       MOVE vttk-tknum TO carga1-transporte.
*saber se tem picking e o numero
*destino

*destino
       CLEAR tvrot.
       SELECT SINGLE * FROM tvrot
       WHERE spras = sy-langu AND
             route = vttk-route.

       IF sy-subrc = 0.
         MOVE tvrot-bezei TO dest.
         IF dest CS '>'.
           sy-fdpos = sy-fdpos + 2.
*          MOVE dest+sy-fdpos(20) TO carga1-destino.
           MOVE dest+sy-fdpos TO carga1-destino.
         ENDIF.
       ENDIF.

*busca nº de picking (caso haja)
       CLEAR vttp.
       SELECT * FROM vttp
       WHERE tknum = vttk-tknum.

         carga1-remessa = vttp-vbeln.
*nome do cliente

         CLEAR likp.
         SELECT SINGLE * FROM likp
         WHERE vbeln = vttp-vbeln.

         CLEAR kna1.
         SELECT SINGLE * FROM kna1
         WHERE kunnr = likp-kunnr.

         carga1-nome = kna1-name1.

         carga1-all = 'Talão'.

         CLEAR rc.
         PERFORM get_desc_grupo USING vttp-vbeln rc.

         IF NOT rc IS INITIAL.
           CLEAR carga1.
         ENDIF.

         CHECK rc IS INITIAL.
         PERFORM get_status_doc USING vttp-vbeln.

         APPEND carga1.  CLEAR carga1.
         flag = 'X'.
       ENDSELECT.

       IF flag IS INITIAL AND carga1-remessa NE space.

         carga1-all = 'Talão'.
         CLEAR rc.
         PERFORM get_desc_grupo USING vttp-vbeln rc.

         IF NOT rc IS INITIAL.
           CLEAR carga1.
         ENDIF.

         CHECK rc IS INITIAL.
         PERFORM get_status_doc USING vttp-vbeln.

         APPEND carga1. CLEAR carga1.
       ENDIF.
     ENDIF.
   ENDSELECT.

 ENDFORM.                    " SELECT_CARGAS_1
*&---------------------------------------------------------------------*
*&      Form  desbloqueio
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM desbloqueio .

   DATA: lv_gtarg TYPE eqegtarg.

   CONCATENATE 'G_PORTARIA_' xuser-lgnum INTO lv_gtarg.
   CONDENSE lv_gtarg NO-GAPS.

   CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
     EXPORTING
       mode_keyword   = 'X'
       keyword_       = lv_gtarg
     EXCEPTIONS
       foreign_lock   = 1
       system_failure = 2
       OTHERS         = 3.
*
*   CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*     EXPORTING
*       mode_keyword   = 'X'
*       keyword_       = 'G_CARGA'
*     EXCEPTIONS
*       foreign_lock   = 1
*       system_failure = 2
*       OTHERS         = 3.

   READ TABLE return_msg INDEX 1.
   IF sy-subrc = 0.
     MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
                NUMBER return_msg-msgnr WITH return_msg-msgv1
                return_msg-msgv2 return_msg-msgv3 return_msg-msgv4.

     CLEAR: doc_carga, matricula, return_msg.
     REFRESH return_msg.
   ENDIF.

 ENDFORM.                    " desbloqueio
*&---------------------------------------------------------------------*
*&      Form  build_hierarchy_header_d
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_HIERARCHY_HEADER_D  text
*----------------------------------------------------------------------*
 FORM build_hierarchy_header_d CHANGING
                                p_hierarchy_header_d TYPE treev_hhdr.

   p_hierarchy_header_d-heading = 'Talão/Pedido compra'(046).
   p_hierarchy_header_d-tooltip = 'Talão/Pedido compra'(046).
   p_hierarchy_header_d-width = 20.
   p_hierarchy_header_d-width_pix = ' '.

 ENDFORM.                    " build_hierarchy_header_d
*&---------------------------------------------------------------------*
*&      Form  dados_complemantares
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM dados_complemantares .

*transportador
   CLEAR lfa1.
   SELECT SINGLE * FROM lfa1
                   WHERE lifnr = vttk-tdlnr.

   MOVE lfa1-name1 TO carga1-transportador.
*tipo camião
***      MOVE vttk-sdabw TO carga1-tipo.
***      MOVE vttk-dplbg TO carga1-dia_carga.
***      MOVE vttk-uplbg TO carga1-hora_carga.
   MOVE vttk-tknum TO carga1-transporte.
*saber se tem picking e o numero
*destino

*destino
   CLEAR tvrot.
   SELECT SINGLE * FROM tvrot
                   WHERE spras = sy-langu
                   AND   route = vttk-route.

   IF sy-subrc = 0.
     MOVE tvrot-bezei TO dest.
     IF dest CS '>'.
       sy-fdpos = sy-fdpos + 2.
*          MOVE dest+sy-fdpos(20) TO carga1-destino.
       MOVE dest+sy-fdpos TO carga1-destino.
     ENDIF.
   ENDIF.

*busca nº de picking (caso haja)
   CLEAR vttp.
   SELECT * FROM vttp
            WHERE tknum = vttk-tknum.

     carga1-remessa = vttp-vbeln.
*nome do cliente

     CLEAR likp.
     SELECT SINGLE * FROM likp
                     WHERE vbeln = vttp-vbeln.

     CLEAR kna1.
     SELECT SINGLE * FROM  kna1
                     WHERE kunnr = likp-kunnr.

     carga1-nome = kna1-name1.

     carga1-all = 'Talão'(006).

     CLEAR rc.
     PERFORM get_desc_grupo USING vttp-vbeln CHANGING rc.

     IF NOT rc IS INITIAL.
       CLEAR carga1.
     ENDIF.

     CHECK rc IS INITIAL.
     PERFORM get_status_doc USING vttp-vbeln.

     carga1-all = 'Talão'(006).
     APPEND carga1.

     flag = 'X'.

   ENDSELECT.

   IF sy-subrc NE 0.
     carga1-all = 'Talão'(006).
     APPEND carga1.
   ENDIF.

 ENDFORM.                    " dados_complemantares
*&---------------------------------------------------------------------*
*&      Form  get_status_de_remessas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM get_status_de_remessas USING p_sammg g_rc.
   DATA: it_t311a LIKE t311a OCCURS 0 WITH HEADER LINE.


   SELECT * INTO TABLE it_t311a FROM  t311a
          WHERE  lgnum  = xuser-lgnum
          AND    refnr  = p_sammg.

   CLEAR g_rc.
   LOOP AT it_t311a.
*----------------------------verificar se foi feita saida de mercadoria
     SELECT SINGLE wbstk INTO vbuk-wbstk
                            FROM  vbuk
               WHERE  vbeln  = it_t311a-rbnum
               AND    wbstk = 'C'.

     IF sy-subrc NE 0.
       g_rc = sy-subrc.

       IF g_rc NE 0.
         EXIT.
       ENDIF.
     ENDIF.


   ENDLOOP.

 ENDFORM.                    " get_status_de_remessas
*&---------------------------------------------------------------------*
*&      Form  efectua_saida_camiao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM efectua_saida_camiao .
   CLEAR : pulmao, st_pul, rc.

   IF talao_saida IS INITIAL.
     SET SCREEN '0000'. LEAVE SCREEN.
     EXIT.
   ENDIF.

   PERFORM get_parameter USING xuser-lgnum
                           'ENTRADA_ARMAZEM'
                           'ST_PUL'
                            st_pul.

   GET TIME.

   PERFORM saida_de_cargas USING rc.


   IF NOT rc IS INITIAL.
     CLEAR rc.
     PERFORM saida_de_descargas USING rc.
   ENDIF.

   IF rc IS INITIAL.
     PERFORM actualiza_lista_de_espera USING rc.
   ENDIF.

   CLEAR : talao_saida, ok_code_0010.

   DATA: lv_gtarg TYPE eqegtarg.
   CONCATENATE 'G_PORTARIA_' xuser-lgnum INTO lv_gtarg.
   CONDENSE lv_gtarg NO-GAPS.

   CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
     EXPORTING
       mode_keyword   = 'X'
       keyword_       = lv_gtarg
     EXCEPTIONS
       foreign_lock   = 1
       system_failure = 2
       OTHERS         = 3.

*   PERFORM desbloqueio.

   IF NOT rc IS INITIAL.
     MESSAGE e000 WITH 'Não foi possivel efectuar saida de Transporte'(047).
   ENDIF.


 ENDFORM.                    " efectua_saida_camiao
*&---------------------------------------------------------------------*
*&      Form  saida_de_cargas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RC  text
*----------------------------------------------------------------------*
 FORM saida_de_cargas  USING  p_rc.

   DATA: data_aux LIKE zwm006_aux-data_reg,
         hora_aux LIKE zwm006_aux-hora_reg.

   IF talao_saida IS INITIAL.
     SET SCREEN '0000'. LEAVE SCREEN.
     EXIT.
   ENDIF.

** Verificar se é um camião de Carga
   CLEAR zwm006_aux.
   SELECT SINGLE * FROM zwm006_aux
                   WHERE armazem     = xuser-lgnum
                   AND   num_entrada = talao_saida
                   AND   finalizada  EQ space.

   data_aux = zwm006_aux-data_reg.
   hora_aux = zwm006_aux-hora_reg.

   IF sy-subrc <> 0.

     p_rc = sy-subrc.

     PERFORM desbloqueio.

** Erro a indicar que o talão não deu entrada no armazém
*     MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '060'.

   ELSE.

**  Se o Camião não tiver porta não é necessária nenhuma validação para
**  fazer saida
     IF zwm006_aux-porta IS INITIAL.
       PERFORM confirma_saida_sem_porta.

       PERFORM actualiza_tabelas USING 'zwm006_aux' talao_saida
                                       xuser-lgnum
                                       ''
                                       data_aux
                                       hora_aux.

       PERFORM actualiza_tabelas USING 'zwm003_aux' talao_saida
                                       xuser-lgnum ''
                                       ''
                                       ''.
       PERFORM update_doc_trasp_saida USING zwm006_aux-n_transporte.

       PERFORM desbloqueio.

       CLEAR: talao_saida, p_rc.
       EXIT.

     ENDIF.
** Só pode fazer a saída se a carga estiver completa.
     CLEAR vttp.
     SELECT SINGLE * FROM vttp
                     WHERE tknum = zwm006_aux-n_transporte.

     IF sy-subrc = 0.
       SELECT * FROM vbss WHERE vbeln = vttp-vbeln.

*----------------seleccionar todas as remessas e verificar se todas
*----------------tem status de saida de mercadoria

         PERFORM get_status_de_remessas USING vbss-sammg  rc.

         IF rc NE 0.
           EXIT.
         ENDIF.


       ENDSELECT.

     ENDIF.

     IF rc NE 0.

*-----------------------------sem saida de mercadoria efectuada
       PERFORM pop_confirma_saida_merc USING n_rc p_rc
                                             talao_saida
                                             vbss-sammg.
     ELSE.

*-----------------------------com saida de mercadoria efectuada
       PERFORM confirma_saida_sem_porta.

     ENDIF.


     PERFORM actualiza_pulmoes USING zwm006_aux-porta.

** Actualizar tabela de descarga com o pisco a indicar que está
** finalizada
     UPDATE zwm006_aux SET finalizada = 'X'
                           carregada  = 'X'
                       hora_saida = sy-timlo
                       data_saida = sy-datlo
                   WHERE armazem     = xuser-lgnum
                   AND   num_entrada = talao_saida
                   AND   data_reg    = data_aux
                   AND   hora_reg    = hora_aux.

     COMMIT WORK.

     PERFORM update_doc_trasp_saida USING zwm006_aux-n_transporte.

** FL -> 10/01/2006
** Display Portaria
     CALL FUNCTION 'ZWM_RFC_DISPLAY_PORTARIA'
       EXPORTING
         modo        = 'A'
         chamada_rfc = ' '
         porta       = zwm006_aux-porta
         i_lgnum     = zwm006_aux-armazem " << INS ROFF(SDF):TMGP:29.01.2016 12:43:21
*      TABLES
*        registos    =
       EXCEPTIONS
         erro        = 1
         erro_modo   = 2
         OTHERS      = 3.

     IF sy-subrc <> 0.
       MESSAGE i242.
     ENDIF.
** FL <- 10/01/2006

     PERFORM actualiza_tabelas USING 'zwm003_aux' talao_saida
                                     xuser-lgnum '' '' ''.

*actualiza o transporte.
*regista hora de saida
*     CLEAR vttk.
*     SELECT SINGLE * FROM vttk
*     WHERE tknum = zwm006_aux-n_transporte.
*
*     IF sy-subrc = 0.
*
*       CALL FUNCTION 'ZWM_CHANGE_SHIPMENT'
*         EXPORTING
*           matricula               = ' '
*           tipo_camiao             = ' '
*           n_transporte            = vttk-tknum
*           transportador           = ' '
*           dalen                   = sy-datum
*           ualen                   = sy-uzeit
*           datbg                   = sy-datum
*           uatbg                   = sy-uzeit
*         TABLES
*           return_msg              = return_msg
*         EXCEPTIONS
*           shipment_does_not_exist = 1
*           OTHERS                  = 2.
*
*
*       IF sy-subrc <> 0.
*
*         PERFORM desbloqueio.
*
*       ENDIF.
*
*     ENDIF.

   ENDIF.

 ENDFORM.                    " saida_de_cargas
*&---------------------------------------------------------------------*
*&      Form  saida_de_descargas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RC  text
*----------------------------------------------------------------------*
 FORM saida_de_descargas  USING    p_rc.

   DATA: data_aux LIKE zwm005-data,
         hora_aux LIKE zwm005-hora_entrada.

** Talão de Descarga

**  Se o Camião não tiver porta não é necessária nenhuma validação para
**  fazer saida
** Libertar Pulmão(oes) de Descarga
   CLEAR: zwm005, p_rc.
   SELECT SINGLE * FROM zwm005
                   WHERE armazem     = xuser-lgnum
                   AND   num_entrada = talao_saida.

   data_aux = zwm005-data.
   hora_aux = zwm005-hora_entrada.

   IF zwm005-porta IS INITIAL.

     PERFORM confirma_saida_sem_porta.

     PERFORM actualiza_tabelas USING 'zwm005' talao_saida
                                     xuser-lgnum ''
                                     data_aux
                                     hora_aux.

     PERFORM actualiza_tabelas USING 'zwm003_aux' talao_saida
                                     xuser-lgnum '' '' ''.

     PERFORM desbloqueio.

     CLEAR talao_saida.

     EXIT.
   ENDIF.

   DATA aux_porta LIKE zwm016-porta.
   CONCATENATE 'DCK 000-000-' zwm005-porta+1(2) INTO aux_porta.

** Verificar se a descarga ja foi finalizada.
   CLEAR zwm016.
   SELECT SINGLE * FROM zwm016
                   WHERE armazem = xuser-lgnum
                   AND   porta   = aux_porta.

   IF zwm016-finalizada IS INITIAL AND sy-subrc EQ 0.

     PERFORM desbloqueio.

     MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '057' WITH talao_saida.
     CLEAR : talao_saida.
     EXIT.
   ENDIF.

** Já tenho a porta e vou libertar pulmões - actualizar LAGP
   PERFORM actualiza_pulmoes USING zwm005-porta.

** Actualizar tabela de descarga com o pisco a indicar que está
** finalizada
   UPDATE zwm005 SET finalizada     = 'X'
                     hora_saida     = sy-timlo
                     data_saida     = sy-datlo
                 WHERE armazem      = xuser-lgnum
                 AND   num_entrada  = talao_saida
                 AND   data         = data_aux.
*                 AND   hora_entrada = hora_aux.

   IF sy-subrc NE 0.
     p_rc = 4.
   ELSE.
     COMMIT WORK.
   ENDIF.

** FL -> 10/01/2006
** Display Portaria
   CALL FUNCTION 'ZWM_RFC_DISPLAY_PORTARIA'
     EXPORTING
       modo        = 'A'
       chamada_rfc = ' '
       porta       = zwm005-porta
       i_lgnum     = zwm005-armazem " << INS ROFF(SDF):TMGP:29.01.2016 12:43:11
*    TABLES
*      registos    =
     EXCEPTIONS
       erro        = 1
       erro_modo   = 2
       OTHERS      = 3.

   IF sy-subrc <> 0.
     MESSAGE i242.
   ENDIF.
** FL <- 10/01/2006

   PERFORM actualiza_tabelas USING 'zwm003_aux' talao_saida
                                   xuser-lgnum '' '' ''.


*  limpar as tabelas zwm0017, zwm0018, zwm023

   DATA clearporta(14).
   DATA t_zwm017 LIKE zwm017 OCCURS 0 WITH HEADER LINE.

   CLEAR: clearporta, t_zwm017.
   REFRESH t_zwm017.

   CONCATENATE 'DCK 000-000-' zwm005-porta+1(2) INTO clearporta.

   SELECT * INTO TABLE t_zwm017
            FROM zwm017
            WHERE armazem     = xuser-lgnum
            AND   num_entrada = talao_saida.

   IF sy-subrc = 0.
     LOOP AT t_zwm017.
       DELETE FROM zwm023
              WHERE armazem = t_zwm017-armazem
              AND   ebeln   = t_zwm017-ebeln
              AND   ebelp   = t_zwm017-ebelp.
       COMMIT WORK.

       IF sy-subrc NE 0.
         p_rc = 4.
       ENDIF.

     ENDLOOP.

     PERFORM actualiza_tabelas USING  'zwm017' talao_saida
                                     xuser-lgnum '' '' ''.
     IF sy-subrc NE 0.
       p_rc = 4.
     ENDIF.

   ENDIF.

   PERFORM actualiza_tabelas USING 'zwm016' ''
                                   xuser-lgnum clearporta
                                   '' ''.
   IF sy-subrc NE 0.
     p_rc = 4.
   ENDIF.


 ENDFORM.                    " saida_de_descargas
*&---------------------------------------------------------------------*
*&      Form  actualiza_lista_de_espera
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RC  text
*----------------------------------------------------------------------*
 FORM actualiza_lista_de_espera  USING    p_rc.
** Retirar a entrada ( caso exista ) da fila de espera

   CLEAR : ti_zwm003, ti_zwm003[], p_rc.

   SELECT SINGLE * FROM zwm003_aux
                   WHERE armazem     = xuser-lgnum
                   AND   num_entrada = talao_saida.
   IF sy-subrc = 0.
** Existe entrada ... actualizar fila de espera e colocar entrada no
** histórico.
     REFRESH : return_msg.

     MOVE-CORRESPONDING zwm003_aux TO ti_zwm003.
     APPEND ti_zwm003.

     CALL FUNCTION 'ZWM_MANAGE_PARKING_V1'
       EXPORTING
         operacao                = '3'
         armazem                 = xuser-lgnum
       TABLES
         l_zwm003                = ti_zwm003
         return_msg              = return_msg
       EXCEPTIONS
         tab_zwm003_not_filled   = 1
         invalid_parameter       = 2
         tab_l_zwm003_filled     = 3
         tab_l_zwm003_not_filled = 4
         no_warehouse            = 5
         OTHERS                  = 6.
     IF sy-subrc <> 0.

       PERFORM desbloqueio.

       RAISE error.
     ENDIF.

     CLEAR   : ti_zwm003.
     REFRESH : ti_zwm003.

   ENDIF.


   PERFORM desbloqueio.

   CLEAR : talao_saida, ti_zwm002, zwm028, zwm002, zwm005,
           zwm003_aux, zwm016, zwm017, zwm006_aux, lagp.

   REFRESH : ti_zwm002.
   CLEAR   : ti_zwm002.

 ENDFORM.                    " actualiza_lista_de_espera
*&---------------------------------------------------------------------*
*&      Form  actualiza_tabelas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM actualiza_tabelas USING tab1 talao arma porta data hora.

   CASE tab1.

     WHEN 'zwm005'.
       UPDATE zwm005 SET finalizada     = 'X'
                         hora_saida     = sy-timlo
                         data_saida     = sy-datlo
                     WHERE armazem      = arma
                     AND   num_entrada  = talao
                     AND   data         = data.
*                     AND   hora_entrada = hora.


     WHEN 'zwm003_aux'.
       DELETE FROM zwm003_aux
           WHERE armazem     = arma
           AND   num_entrada = talao.

     WHEN 'zwm006_aux'.
       UPDATE zwm006_aux SET finalizada = 'X'
                         hora_saida     = sy-timlo
                         data_saida     = sy-datlo
                     WHERE armazem      = arma
                     AND   num_entrada  = talao
                     AND   data_reg     = data
                     AND   hora_reg     = hora.

     WHEN 'zwm016'.
       DELETE FROM zwm016
         WHERE armazem = arma
         AND   porta   = porta.

     WHEN 'zwm017'.
       DELETE FROM zwm017
           WHERE armazem     = arma
           AND   num_entrada = talao.

   ENDCASE.



   COMMIT WORK AND WAIT.
 ENDFORM.                    " actualiza_tabelas

*&---------------------------------------------------------------------*
*&      Form  VALIDA_CAMPOS_VAZIOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM valida_campos_vazios .

   IF doc_carga IS INITIAL AND NOT matricula IS INITIAL.

     MOVE 'DOC_CARGA' TO cursorfield.
     MESSAGE w000 WITH 'Preencher Nº transporte'(048).
     CLEAR: ok_code_0004, tipo_camiao, transportador, matricula,
            doc_carga, nome_transp.

   ELSEIF matricula IS INITIAL AND NOT doc_carga IS INITIAL.

     MOVE 'DOC_CARGA' TO cursorfield.
     MESSAGE w000 WITH 'Preencher  Matricula'(049).
     CLEAR: ok_code_0004, tipo_camiao, transportador, matricula,
            doc_carga, nome_transp.

   ELSEIF doc_carga IS INITIAL AND  matricula IS INITIAL.

     MOVE 'DOC_CARGA' TO cursorfield.
     MESSAGE w000 WITH 'Nº transporte e Matricula,'(050)
                       'deve preencher ambos'(051).

     CLEAR: ok_code_0004, tipo_camiao, transportador, matricula,
            doc_carga, nome_transp.
   ENDIF.

 ENDFORM.                    " VALIDA_CAMPOS_VAZIOS

*&---------------------------------------------------------------------*
*&      Form  impressao_talao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM impressao_talao USING num_entrada.

   IF NOT num_entrada IS INITIAL.
*       PERFORM desbloqueio.

     DATA: lv_gtarg TYPE eqegtarg.
     CONCATENATE 'G_CARGA_' xuser-lgnum INTO lv_gtarg.
     CONDENSE lv_gtarg NO-GAPS.

     CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
       EXPORTING
         mode_keyword   = 'X'
         keyword_       = lv_gtarg
       EXCEPTIONS
         foreign_lock   = 1
         system_failure = 2
         OTHERS         = 3.
     " 2018.01.31 - Paulo Sousa -  Tornar Impressão opcional
     DATA: resposta TYPE c.
     CLEAR resposta.

     CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
         titlebar              = 'Portaria'(065)
         text_question         = text-075 "Imprimir talão de registo de portaria?
         text_button_1         = 'Sim'(001)
         text_button_2         = 'Não'(002)
         default_button        = '2'
         display_cancel_button = ' '
       IMPORTING
         answer                = resposta
       EXCEPTIONS
         text_not_found        = 1
         OTHERS                = 2.

     IF resposta = '1'.
       SUBMIT zwmrep0004_pdf WITH p_talao = num_entrada AND RETURN. " Nuno Bairro 01/07/2024 Talão PDF

       MESSAGE i000 WITH 'Fila de espera documento em impressão'(052)
                          num_entrada.
     ELSE.
       MESSAGE i000 WITH 'Fila de espera SEM impressão de documento'(076)
                          num_entrada.
     ENDIF.
   ENDIF.
   PERFORM inicializa.
*   CLEAR: doc_compra, matricula.
   SET SCREEN '0000'. LEAVE SCREEN.

 ENDFORM.                    " impressao_talao

*&---------------------------------------------------------------------*
*&      Form  erro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1797   text
*      -->P_G_DEL_ALL  text
*      -->P_=  text
*      -->P_1800   text
*----------------------------------------------------------------------*
 FORM erro  USING    VALUE(p_msg).

   g_del_all = 'X'.

   PERFORM inicializa.

   MOVE 'DOC_CARGA' TO cursorfield.

   IF NOT p_msg IS INITIAL.
     MESSAGE e000 WITH p_msg.
   ENDIF.
 ENDFORM.                    " erro

*&---------------------------------------------------------------------*
*&      Form  actualiza_pulmoes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZWM006_AUX_PORTA  text
*----------------------------------------------------------------------*
 FORM actualiza_pulmoes  USING    porta.

   CLEAR : ti_zwm002, ti_zwm002[].

** Já tenho a porta e vou libertar pulmões - actualizar LAGP
   CLEAR zwm002.
   SELECT SINGLE * FROM zwm002
                   WHERE armazem = xuser-lgnum
                   AND   porta   = porta.

   CHECK sy-subrc = 0 AND zwm002-estado <> 'D'.

   CHECK zwm002-num_entrada = talao_saida.

** Tabela para actualização do estado das portas
   MOVE-CORRESPONDING zwm002 TO ti_zwm002.
   APPEND ti_zwm002.
   CLEAR : pulmao.
   DO 2 TIMES VARYING pulmao FROM
                                  zwm002-pulmao_1 NEXT
                                  zwm002-pulmao_2.

** FL -> 18/01/2006
**-------------------------------------------------------Ins Jun2005
**       IF zwm005-porta NE space.
*
*       CLEAR lagp.
*       SELECT SINGLE * FROM  lagp CLIENT SPECIFIED
*              WHERE  lgnum  = xuser-lgnum
*              AND    lgtyp  = st_pul
*              AND    lgpla  = pulmao.
*
*       PERFORM actualiza_log_lagp USING 'INI' 'SAIDA_CAM' talao_saida.
**       ENDIF.
**------------------------------------------------------------------
*       UPDATE lagp SET brand = ' '
*                   WHERE lgnum = xuser-lgnum
*                   AND   lgtyp = st_pul
*                   AND   lgpla = pulmao.
*
**-------------------------------------------------------Ins Jun2005
**       IF zwm005-porta NE space.
*       CLEAR lagp.
*       SELECT SINGLE * FROM  lagp CLIENT SPECIFIED
*              WHERE  lgnum  = xuser-lgnum
*              AND    lgtyp  = st_pul
*              AND    lgpla  = pulmao.
*
*       PERFORM actualiza_log_lagp USING 'FIM' 'SAIDA_CAM' talao_saida.
**       ENDIF.
**------------------------------------------------------------------
*       CLEAR st_pul.
     DATA stype LIKE lagp-lgtyp.

     CLEAR stype.
     SELECT SINGLE st_pul INTO stype
        FROM zwm028
            WHERE lgnum = xuser-lgnum
              AND refnr = vbss-sammg
              AND remessa = ' '.
     IF stype IS INITIAL.
       stype = st_pul.
     ENDIF.

     CLEAR wa_lagpv.
     SELECT SINGLE * FROM lagp
             WHERE lgnum = xuser-lgnum
              AND   lgtyp = stype
              AND   lgpla = pulmao.

     IF sy-subrc EQ 0.
       MOVE-CORRESPONDING lagp TO wa_lagpv.
       IF xuser-lgnum <> '150'.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 06.06.2012 14:31:32
*  Motivo: Desbloqueio de Pulmões e Mini Pulmões
*--------------------------------------------------------------------*

         CALL FUNCTION 'ZWM_MPUL_FREE_PULMAO'
           EXPORTING
             i_lgnum   = xuser-lgnum
             i_refnr   = vbss-sammg
             is_zwm028 = zwm028
           CHANGING
             cs_lagpv  = wa_lagpv.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
       ELSE.
         wa_lagpv-brand = ' '.
       ENDIF.

       CALL FUNCTION 'L_LAGP_VERAENDERN'
         EXPORTING
           xlagpv = wa_lagpv.

       COMMIT WORK AND WAIT.
     ENDIF.
** FL <- 18/01/2006

*** apagar as paletes que estão no pulmão
     DATA pulmao1(14).
     CALL FUNCTION 'ZWM_CONCATENATE_BIN'
       EXPORTING
         lgtyp = stype
         lgpla = pulmao
       IMPORTING
         bin   = pulmao1.
*------------------------------------------------------------Jun2005
*          DELETE FROM zwm013
*            WHERE armazem = xuser-lgnum AND destino = pulmao1.
*----------------------------------------------------------------End
     COMMIT WORK.
   ENDDO.
*   ENDIF.

   READ TABLE ti_zwm002 INDEX 1.

   ti_zwm002-estado = 'D'.

   CLEAR: ti_zwm002-bloqueio, ti_zwm002-pulmao_1, ti_zwm002-pulmao_2,
          ti_zwm002-num_entrada, ti_zwm002-user_name.

   MODIFY ti_zwm002 INDEX 1.

   MODIFY zwm002 FROM TABLE ti_zwm002.
   COMMIT WORK.

   CLEAR : ti_zwm002, ti_zwm002[].

 ENDFORM.                    " actualiza_pulmoes

*&---------------------------------------------------------------------*
*&      Form  no_de_topo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM no_de_topo USING p_val.

   CLEAR: ls_out, l_all_key.
   IF p_val = 'C'.

     CALL METHOD treec->delete_all_nodes.

     PERFORM add_all_line USING ls_out
                                ''
                       CHANGING l_all_key.

   ELSEIF p_val = 'D'.
     CALL METHOD treed->delete_all_nodes.

     PERFORM add_all_line_d USING ls_out
                                ''
                       CHANGING l_all_key.
   ENDIF.

   g_top_node_key = l_all_key.

 ENDFORM.                    " no_de_topo

*&---------------------------------------------------------------------*
*&      Form  valida_grupo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VTTK_TKNUM  text
*----------------------------------------------------------------------*
 FORM valida_grupo  USING  p_tknum.

   CLEAR vttp-vbeln.
   SELECT vbeln INTO vttp-vbeln FROM  vttp UP TO 1 ROWS
          WHERE  tknum  = p_tknum.

   ENDSELECT.

   CLEAR rc.

   PERFORM get_desc_grupo USING vttp-vbeln CHANGING rc.
   CLEAR vttp-vbeln.

   IF NOT rc IS INITIAL.
     PERFORM erro USING 'Transporte com Grupo inválido'(053).
     PERFORM inicializa.
   ENDIF.

 ENDFORM.                    " valida_grupo
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DOC_TRASP_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM update_doc_trasp_saida USING uv_tknum TYPE tknum.

   CALL FUNCTION 'Z_WM_SHIPMENT_SET_STATUS'
     EXPORTING
       i_tknum  = uv_tknum
       i_status = 4
       i_wait   = 'X'
     EXCEPTIONS
       error    = 1
       OTHERS   = 2.

   CALL FUNCTION 'Z_WM_SHIPMENT_SET_STATUS'
     EXPORTING
       i_tknum  = uv_tknum
       i_status = 5
       i_wait   = 'X'
     EXCEPTIONS
       error    = 1
       OTHERS   = 2.

 ENDFORM.                    " UPDATE_DOC_TRASP_SAIDA
