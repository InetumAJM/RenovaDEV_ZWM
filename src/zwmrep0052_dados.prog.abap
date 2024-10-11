*&---------------------------------------------------------------------*
*&  Include          ZWMREP0052_DADOS                                  *
*&---------------------------------------------------------------------*

CLASS cl_gui_column_tree DEFINITION LOAD.
CLASS cl_gui_cfw         DEFINITION LOAD.

DATA tree1      TYPE REF TO cl_gui_alv_tree.
DATA mr_toolbar TYPE REF TO cl_gui_toolbar.

INCLUDE <icon>.

************************************************************************
** Tabelas DD
************************************************************************
TABLES: ltap, ltak, usr01, zwm010, makt, t346t, t340.

************************************************************************
** Tabelas Internas
************************************************************************
DATA: BEGIN OF itab OCCURS 0,
       lgnum LIKE ltap-lgnum,
       data(12),
       equipamento(40),
       fila(50),
       fila_out(300),
       status_to,
       status_out(30),
       tanum LIKE ltak-tanum,
       tapos LIKE ltap-tapos,
       queue LIKE ltak-queue,
       bdatu LIKE ltak-bdatu,
       vltyp LIKE ltap-vltyp,
       nltyp LIKE ltap-nltyp,
       pquit LIKE ltap-pquit,
       pvqui LIKE ltap-pvqui,
       vsolm LIKE ltap-vsolm,
       meins LIKE ltap-meins,
       charg LIKE ltap-charg,
       total_to LIKE sy-tabix,
       perc_to(3) TYPE p DECIMALS 2,
       perc_to_equip(3) TYPE p DECIMALS 2,
       to_criada(5),
       to_pende(5),
       to_final(5),
       matnr  LIKE mara-matnr,
       maktx  LIKE makt-maktx,
       bwlvs  LIKE ltak-bwlvs,
       ename  LIKE ltap-ename,
       ezeit  LIKE ltap-ezeit,
* Alteração Ferreira 2007.04.11
       qname  LIKE ltap-qname,
       qzeit  LIKE ltap-qzeit,
* Fim alteração 2007.04.11
       letyp  LIKE ltap-letyp,
      END OF itab.

DATA: BEGIN OF itab_dados OCCURS 0,
       lgnum LIKE ltap-lgnum,
       tanum LIKE ltak-tanum,
       tapos LIKE ltap-tapos,
       ename LIKE ltap-ename,
       edatu LIKE ltap-edatu,
       qdatu LIKE ltap-qdatu,
       qname LIKE ltap-qname,
       queue LIKE ltak-queue,
       bdatu LIKE ltak-bdatu,
       matnr LIKE ltap-matnr,
       maktx LIKE ltap-maktx,
       vltyp LIKE ltap-vltyp,
       nltyp LIKE ltap-nltyp,
       pquit LIKE ltap-pquit,
       pvqui LIKE ltap-pvqui,
       vsolm LIKE ltap-vsolm,
       meins LIKE ltap-meins,
       charg LIKE ltap-charg,
       letyp LIKE ltap-letyp,
       bwlvs LIKE ltak-bwlvs,
       ezeit LIKE ltap-ezeit,
* Alteração Ferreira 2007.04.11
       qzeit  LIKE ltap-qzeit,
* Fim alteração 2007.04.11
       kgvnq LIKE ltak-kgvnq,
       posty LIKE ltap-posty,
      END OF itab_dados.

DATA: BEGIN OF itab_to OCCURS 0,
        bdatu  LIKE ltak-bdatu,
        fila(40),
        status_to,
        num_to(3) TYPE p DECIMALS 1,
        tanum  LIKE ltak-tanum,
      END OF itab_to.

DATA: BEGIN OF itab_data OCCURS 0,
       bdatu  LIKE ltak-bdatu,
       queue  LIKE ltak-queue,
       equipamento(40),
       num_to LIKE sy-tabix,
      END OF itab_data.

DATA: itab_dados_aux LIKE itab_dados OCCURS 0 WITH HEADER LINE.

TYPES: t_out LIKE itab.

******************* ALV data declare
CLASS cl_gui_column_tree DEFINITION LOAD.
CLASS cl_gui_cfw DEFINITION LOAD.

DATA: tree  TYPE REF TO cl_gui_alv_tree_simple.

DATA: gt_fieldcatalog     TYPE lvc_t_fcat,
      gt_sort             TYPE lvc_t_sort,
      gt_out              TYPE t_out OCCURS 0,
      gs_out              TYPE t_out,
      gs_out_save         TYPE t_out.

DATA:   g_repid            LIKE sy-repid,
        total_fila         LIKE sy-tabix,
        total_dia          LIKE sy-tabix.

DATA:   ok_code            LIKE sy-ucomm,   "OK-Code
        g_ok_code          LIKE sy-ucomm,
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

DATA: aux_s_date(10),
      g_flag_change,
      not_first_time,
      flag_tree.

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
*    CASE fcode.
*    ENDCASE.
  ENDMETHOD.                    "on_function_selected

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
