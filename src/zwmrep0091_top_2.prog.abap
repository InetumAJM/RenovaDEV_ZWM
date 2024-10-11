*&---------------------------------------------------------------------*
*&  Include           ZWMREP0091_TOP_2
*&---------------------------------------------------------------------*
TABLES: likp, t311, vbpa, adrc, mlgn, vttk, a570.

TYPE-POOLS: vrm.

DATA gt_dados_ant TYPE STANDARD TABLE OF zwm_s001 WITH HEADER LINE.
DATA: BEGIN OF gt_faltas OCCURS 0,
        ordem         TYPE vbeln_va,
        item          TYPE vgpos,
        matnr         TYPE matnr,
        arktx         TYPE arktx,
        lfimg         TYPE lfimg,
        werks         TYPE werks_d,
        finalizada(1),
      END OF gt_faltas.

DATA:     gt_faltas_aux     LIKE gt_faltas     OCCURS 0 WITH HEADER LINE.

DATA: gt_t311  LIKE t311  OCCURS 0 WITH HEADER LINE,
      gt_t311a LIKE t311a OCCURS 0 WITH HEADER LINE,
      gt_vttk  LIKE vttk  OCCURS 0 WITH HEADER LINE,
      gt_vttp  LIKE vttp  OCCURS 0 WITH HEADER LINE,
      gt_likp  LIKE likp  OCCURS 0 WITH HEADER LINE,
      gt_lips  LIKE lips  OCCURS 0 WITH HEADER LINE,
      gt_kna1  LIKE kna1  OCCURS 0 WITH HEADER LINE,
      gt_vbak  LIKE vbak  OCCURS 0 WITH HEADER LINE,
      gt_vbap  LIKE vbap  OCCURS 0 WITH HEADER LINE,
      gt_vbfa  LIKE vbfa  OCCURS 0 WITH HEADER LINE,
      gt_ekko  LIKE ekko  OCCURS 0 WITH HEADER LINE,
      gt_ekpo  LIKE ekpo  OCCURS 0 WITH HEADER LINE,
      gt_ekbe  LIKE ekbe  OCCURS 0 WITH HEADER LINE.


DATA: BEGIN OF gt_header OCCURS 0,
        vkorg     TYPE vkorg,
        tknum     TYPE tknum,
        numero    TYPE i,
        bezei     TYPE bezei40,
        trans     TYPE name1_gp,
        signi     TYPE signi,
        pal       TYPE i,
        clientes  TYPE vttk_add01,
        peso      TYPE gsgew,
        un_peso   TYPE gewei,
        volume    TYPE volum_15,
        un_volume TYPE voleh,
        tp_carro  TYPE bezei40,
        dtdis     TYPE dtdis,
        observ    TYPE exti1,
      END OF gt_header.

DATA: BEGIN OF gt_items OCCURS 0,
        vkorg     TYPE vkorg,
        tknum     TYPE tknum,
        refnr     TYPE lvs_refnr,
        refnt     TYPE lvs_refnt,
        vbeln     TYPE vbeln_vl,
        kunnr     TYPE kunnr,
        land1     TYPE land1_gp,
        name1_rm1 TYPE ad_name1,
        name1_rm2 TYPE ad_name1,
        palcp     TYPE zpalcomp,
        palpk     TYPE zwn_pal_picking,
        palto     TYPE zwn_pal_picking,
        palpk_s   TYPE zpalcomp,
        palto_s   TYPE zpalcomp,
        pfalt     TYPE zprodfal,
      END OF gt_items.

DATA: BEGIN OF tab_out OCCURS 0,
        all(20)       TYPE c,
        vkorg         TYPE vkorg,
        vkorg_out(51),
        tknum         TYPE tknum,
        refnr         TYPE lvs_refnr,
        refnr_out(51),
        trfzn         TYPE trfzn,
        bezei         TYPE bezei40,
        trans         TYPE name1_gp,
        vbeln         TYPE vbeln_vl,
        land1         TYPE land1_gp,
        name1_rm1     TYPE name1_gp,
        name1_rm2     TYPE name1_gp,
        palcp         TYPE zpalcomp,
        palpk         TYPE zwn_pal_picking,
        palto         TYPE zwn_pal_picking,
        palpk_s       TYPE zpalcomp,
        palto_s       TYPE zpalcomp,
        pfalt         TYPE zprodfal,
      END OF tab_out.

* INI - JFARIA - RENCFR00002 - 09.03.2012
TYPES: BEGIN OF ty_rank,
         tknum  TYPE tknum,
         trans  TYPE name1_gp,
         tdlnr  TYPE a570-tdlnr,
         name1  TYPE lfa1-name1,
         trfznz TYPE a570-trfznz,
         bezei  TYPE tvftzt-bezei,
         kbetr  TYPE konp-kbetr,
       END OF ty_rank.

DATA: wa_rank TYPE ty_rank,
      ti_rank TYPE TABLE OF ty_rank WITH HEADER LINE.
* FIM - JFARIA - RENCFR00002 - 09.03.2012

TYPES: t_out LIKE tab_out.

TYPES: ty_dados LIKE gt_dados_ant.

DATA : gt_data  TYPE TABLE OF ty_dados,
       gt_table TYPE REF TO cl_salv_table.

DATA: gv_exit(1) TYPE c.

DATA: is_variant   TYPE disvariant.

DATA: r_grid           TYPE REF TO cl_gui_alv_grid.
DATA: alv_keyinfo      TYPE slis_keyinfo_alv,
      alv_fieldcat     TYPE slis_t_fieldcat_alv,
      wa_alv_fieldcat  TYPE slis_fieldcat_alv,
      alv_fieldcat2    TYPE slis_t_fieldcat_alv,
      wa_alv_fieldcat2 TYPE slis_fieldcat_alv,
      alv_sort         TYPE slis_t_sortinfo_alv,
      wa_sort          TYPE slis_sortinfo_alv,
      alv_layout       TYPE slis_layout_alv,
      alv_layout2      TYPE slis_layout_alv,
      lv_repid         LIKE sy-repid.

DATA: lv_name  TYPE         vrm_id,
      lt_list  TYPE         vrm_values,
      wa_value LIKE LINE OF lt_list.

DATA: ls_dados_header LIKE LINE OF gt_dados_ant.

******************* ALV data declare
CLASS cl_gui_column_tree DEFINITION LOAD.
CLASS cl_gui_cfw DEFINITION LOAD.


DATA: tree  TYPE REF TO cl_gui_alv_tree_simple.


DATA: gt_fieldcatalog TYPE lvc_t_fcat,
      gt_sort         TYPE lvc_t_sort,
      gt_out          TYPE t_out OCCURS 0,
      wa_out          TYPE t_out,
      gs_out          TYPE t_out,
      gs_out_save     TYPE t_out.

DATA:   g_repid            LIKE sy-repid.
DATA: "ok_code            LIKE sy-ucomm,   "OK-Code
  ok_code_0200       LIKE sy-ucomm,   "OK-Code
  ok_code_0400       LIKE sy-ucomm,   "OK-Code
  g_desbloq          LIKE zwm028-tipo_lock,
  g_ok_code          LIKE sy-ucomm,
  "gv_cargas          TYPE i,
  g_user_command     TYPE slis_formname
                                 VALUE 'USER_COMMAND',
  g_status           TYPE slis_formname
                                 VALUE 'STANDARD_FULLSCREEN',
  grid               TYPE REF TO cl_gui_alv_grid,
  g_custom_container TYPE REF TO cl_gui_custom_container,
  selected(1)        VALUE 'X',
  total_itens        TYPE i,
  total_completed    TYPE i.

DATA: g_top_node     TYPE lvc_nkey,
      g_top_node_key TYPE lvc_nkey.

DATA: aux_refnr LIKE t311-refnr.

DATA: aux_s_date(10).

DATA: g_flag_change,
      g_flag_lock.

DATA: not_first_time.

DATA: l_user LIKE sy-uname.

DATA: flag_tree.
