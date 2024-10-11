*&---------------------------------------------------------------------*
*&  Include           ZWMREP0003TOP                                    *
*&---------------------------------------------------------------------*
PROGRAM zwmrep003 MESSAGE-ID zwmmsg001.

TYPE-POOLS: slis.
INCLUDE <icon>.

INCLUDE rlmobinc.
TABLES: zwm001,
        zwm002,
*        zwm003,
        t300t,
        tvrot,
        zwm004,
        ekko,
        ekpo,
        vbfa,
        zwm005,
*        zwm006,
        zwm007,
        vttp,
        kna1,
        zwm016,
        zwm017,
        zwm018,
        zwm019,
        vbrk,
        zwm025,
        vbsk,
        vbss,
        zwm028,
        t001l,
        mch1,
        zwm036,
        vbuk,
        zwm006_aux,
        zwm003_aux,
        tvsak.
*        zwm002_aux.

DATA: itab_dynp   LIKE dynpread OCCURS 0 WITH HEADER LINE,
      itab_return LIKE ddshretval OCCURS 0 WITH HEADER LINE.

TYPES: BEGIN OF lcarga,
         simbolo(4),
         num_entrada   LIKE zwm006-num_entrada,
         dia           TYPE sy-datum,
         hora          TYPE sy-uzeit,
         porta         TYPE zwm006-porta,
         matricula     LIKE zwm006-matricula,
         tipo          TYPE zwm006-tipo_camiao,
         transportador LIKE lfa1-name1,
         observacoes   TYPE zwm003-observacoes,
         destino(20),
         cliente       LIKE kna1-kunnr,
         nome(20),
         dia_carga     TYPE sy-datum,
         hora_carga    TYPE sy-uzeit,
         transporte    TYPE vttk-tknum,
         documento     TYPE ebeln,
         remessa       TYPE ebeln,
         dia_entrega   TYPE sy-datum,
         hora_entrega  TYPE sy-uzeit,
         flag          TYPE i,
       END OF lcarga.

TYPES: BEGIN OF ldescarga,
         simbolo(4),
         num_entrada  TYPE zwm005-num_entrada,
         data         TYPE zwm005-data,
         hora_entrada TYPE zwm005-hora_entrada,
         porta        TYPE zwm005-porta,
         matricula    TYPE zwm005-matricula,
         tipo_camiao  TYPE zwm005-tipo_camiao,
         desc_transp  TYPE zwm005-desc_transp,
         observacoes  TYPE zwm005-observacoes,
         ord_compra   TYPE zwm005-ord_compra,
       END OF ldescarga.


DATA : BEGIN OF ti_zwm001 OCCURS 0.
         INCLUDE STRUCTURE zwm001.
       DATA: END OF ti_zwm001.

*tabelas internas
***DATA: carga1 TYPE lcarga OCCURS 0 WITH HEADER LINE. "Cargas
DATA: carga1           LIKE zwm_aux_cargas OCCURS 0 WITH HEADER LINE.
DATA: izwm003          TYPE zwm003_aux OCCURS 0 WITH HEADER LINE.
DATA: izwm002          TYPE zwm002 OCCURS 0 WITH HEADER LINE. "Portas
***DATA: izwm005 TYPE ldescarga OCCURS 0 WITH HEADER LINE. "Descargas
DATA: izwm005          LIKE zwm_aux_descargas OCCURS 0 WITH HEADER LINE.
DATA: tab_zwm005       TYPE zwm005 OCCURS 0 WITH HEADER LINE.
DATA: izwm006          TYPE zwm006_aux OCCURS 0 WITH HEADER LINE.
DATA: zwm_return_truck TYPE zwm_return_truck OCCURS 0 WITH HEADER LINE.

*------------------------------tratamento de nós---------------------
DATA: flag_tree, flag_treec, flag_tree_d.
DATA: g_top_node       TYPE lvc_nkey, g_top_node_d TYPE lvc_nkey,
      g_top_node_key   TYPE lvc_nkey, g_top_node_key_d TYPE lvc_nkey.

DATA: ls_out    LIKE zwm_aux_cargas    OCCURS 0 WITH HEADER LINE,
      ls_out_d  LIKE zwm_aux_descargas OCCURS 0 WITH HEADER LINE,
      lsx_out   LIKE zwm_aux_cargas    OCCURS 0 WITH HEADER LINE,
      lsx_out_d LIKE zwm_aux_descargas OCCURS 0 WITH HEADER LINE.


DATA: l_all_key     TYPE lvc_nkey,
      l_talao_key   TYPE lvc_nkey,
      l_pedido_key  TYPE lvc_nkey,
      l_remessa_key TYPE lvc_nkey,
      l_last_key    TYPE lvc_nkey.

DATA  g_idx LIKE sy-tabix.

*dados gerais
DATA: ok_code_0001    LIKE sy-ucomm,
      ok_code_0002    LIKE sy-ucomm,
      ok_code_0003    LIKE sy-ucomm,
      ok_code_0004    LIKE sy-ucomm,
      ok_code_0005    LIKE sy-ucomm,
      ok_code_0006    LIKE sy-ucomm,
      ok_code_0007    LIKE sy-ucomm,
      ok_code_0008    LIKE sy-ucomm,
      ok_code_0009    LIKE sy-ucomm,
      ok_code_0010    LIKE sy-ucomm,
      ok_code_0011    LIKE sy-ucomm,
      ok_code_0012    LIKE sy-ucomm,
      ok_code_0013    LIKE sy-ucomm,
      ok_code_0014    LIKE sy-ucomm,
      cursorfield(20),
      num_entrada     LIKE zwm003-num_entrada,
      return_msg      TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
      g_del_all.

*dados ecran 2
DATA: matricula     LIKE zwm003-matricula,
      matricula_scr LIKE zwm003-matricula,
      galera        LIKE zwm003-matricula,
      t_armazem(38).

"Ini-Inetum-AJM-25/06/24 - Peso Bascula
DATA: gs_0004 TYPE zwm_018.
"Fim-Inetum-AJM-25/06/24 - Peso Bascula

*dados ecran 3
DATA: doc_compra    TYPE ekko-ebeln,
      tipo_camiao   LIKE vttk-sdabw,
      transportador LIKE vttk-tdlnr,
      observacao    LIKE vttk-tpbez,
      observacao2   LIKE vttk-tpbez,
      nome_transp   LIKE lfa1-name1.
DATA: gv_scr0004_mode.

DATA: porta LIKE zwm002-porta.

** Para colocar várias ordens de compra em simultâneo
DATA : BEGIN OF ordens OCCURS 0,
         ebeln LIKE ekko-ebeln.
DATA : END OF ordens.

*dados ecran 4
DATA: doc_carga     TYPE vttk-tknum.
RANGES datas        FOR sy-datum OCCURS 10.

DATA: dat_aux      LIKE sy-datum,
      flag(1),
      aux_entrada  TYPE zwm006_aux-num_entrada,
      contador     TYPE i,
      dest         TYPE tvrot-bezei,
      g_novo_trans.

*dados  ecran 5
DATA: indice_porta TYPE sy-tabix,
      indice_fila  TYPE sy-tabix.

TYPES: BEGIN OF portas1,
         index(1).
         INCLUDE STRUCTURE zwm_return_truck.
         TYPES:  flag(1).
TYPES  END OF portas1.

DATA: tab_portas1 TYPE portas1 OCCURS 0 WITH HEADER LINE. "Portas

DATA: BEGIN OF fila_porta OCCURS 0,
        index(1),
        talao       TYPE zwm003_aux-num_entrada,
        matricula   TYPE zwm003_aux-matricula,
        observacao  TYPE zwm003_aux-observacoes,
        operacao    TYPE zwm003_aux-operacao,
        chegada     TYPE zwm003_aux-hora_entrada,
        dia_reg     TYPE zwm003_aux-data,
        pulmoes(21),
        kober       TYPE kober,
        flag(1)     TYPE c,
      END OF fila_porta.

*dados ecran 6
DATA: matricula_d    TYPE zwm006_aux-matricula,
      matricula_o    TYPE zwm006_aux-matricula,
      porta_d        TYPE zwm006-porta,
      tipo_porta(14).

*dados ecran 7
DATA: numero_transporte TYPE vttk-tknum.

*ecran 1
DATA: aux_s_date(10), aux_s_time(8), aux_s_user(12),
      v1(4),
      v2(4),
      v3(4),
      v5(4),
      v4(4),
      v6(4),
      v7(4).

DATA: BEGIN OF tab_f OCCURS 0,
        armazem        TYPE zwm002-armazem,
        porta          TYPE zwm002-porta,
        estado(4),
        matricula      TYPE vttk-signi,
*      tipo(8),
        estado_interno TYPE zwm002-estado,
*---------------------------------------------------ins Mai2005
        num_entrada    LIKE zwm006-num_entrada,
*      col TYPE slis_t_specialcol_alv,
      END OF tab_f.

*ecran 7
DATA: BEGIN OF help OCCURS 0,
        matricula TYPE zwm005-matricula,
      END OF help.

* ecran 8
DATA: pedido      LIKE ekpo-ebeln,
      item        LIKE ekpo-ebelp,
      material    LIKE ekpo-matnr,
      descricao   LIKE makt-maktx,
      quantidade  LIKE ekpo-menge,
      uni         LIKE ekpo-meins,
      talao(5),
      lote        LIKE mch1-charg,
      certificado LIKE ltap-zeugn.

*ecran 9
DATA : doc_compra1  TYPE ekko-ebeln,
       doc_compra2  TYPE ekko-ebeln,
       doc_compra3  TYPE ekko-ebeln,
       doc_compra4  TYPE ekko-ebeln,
       doc_compra5  TYPE ekko-ebeln,
       doc_compra6  TYPE ekko-ebeln,
       doc_compra7  TYPE ekko-ebeln,
       doc_compra8  TYPE ekko-ebeln,
       doc_compra9  TYPE ekko-ebeln,
       doc_compra10 TYPE ekko-ebeln.

* ecran 10
DATA: talao_saida(5).

DATA: percentagem TYPE p DECIMALS 2,
      indice_tab  TYPE lvc_index.
* ecran 11
DATA: remessa               LIKE lips-vbeln,
      item_r                LIKE lips-posnr,
      cliente               LIKE kna1-kunnr,
      descricao_cliente(40),
      doc_renova            LIKE likp-vbeln,
      l_teste               LIKE likp-vbeln,
      cod_dev               LIKE zwm036-cod_dev,
      l_text                LIKE dd07v-ddtext,
      cod_mot               LIKE zwm036-cod_motivo,
      motivo                LIKE zwm036-motivo.

* ecran 12
DATA: material1 LIKE mara-matnr,
      material2 LIKE mara-matnr,
      material3 LIKE mara-matnr,
      des1      LIKE makt-maktx,
      des2      LIKE makt-maktx,
      des3      LIKE makt-maktx,
      uni1      LIKE mara-meins,
      uni2      LIKE mara-meins,
      uni3      LIKE mara-meins,
      qtd1(2),
      qtd2(2),
      qtd3(2).

* ecran 13

DATA: deposito LIKE mard-lgort.

CONSTANTS: fcode_disp   LIKE sy-ucomm VALUE 'DISP'.
CONSTANTS: BEGIN OF gc_scr0004,
             modificar VALUE 'M',
             criar     VALUE 'C',
           END OF gc_scr0004.

DATA: gt_fieldcatalog TYPE lvc_t_fcat,
      gt_sort         TYPE lvc_t_sort,
      gt_tab          LIKE tab_f OCCURS 0.
*--------------------------------------------novos controlos
*------------------lista de cargas----------------------------

DATA: gt_fieldcatalog_c TYPE lvc_t_fcat,
      linha             TYPE lvc_s_fcat  OCCURS 0 WITH HEADER LINE,
      gt_sort_c         TYPE lvc_t_sort,
      gt_tab_c          LIKE carga1 OCCURS 0.

*------------------lista de descargas-------------------------
DATA: gt_fieldcatalog_d TYPE lvc_t_fcat,
      gt_sort_d         TYPE lvc_t_sort,
      gt_tab_d          LIKE izwm005 OCCURS 0.

*-------------------------------------------------------------

DATA:   g_repid LIKE sy-repid.
DATA: ok_code            LIKE sy-ucomm,   "OK-Code
      g_user_command     TYPE slis_formname VALUE 'USER_COMMAND',
      g_status           TYPE slis_formname VALUE 'STANDARD_FULLSCREEN',
      grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      rc                 LIKE sy-subrc,
      n_rc(1).

*----------------------------------------------------------------------
DATA: new_doc_carga     LIKE doc_carga,
      new_matricula     LIKE matricula,
      new_observacao    LIKE observacao,
      new_tipo_camiao   LIKE tipo_camiao,
      new_transportador LIKE transportador,
      new_nome_transp   LIKE lfa1-name1.
*----------------------------------------------------------------------


DATA: pulmao    LIKE zwm002-pulmao_1,  g_rc LIKE sy-subrc,
      ti_zwm002 LIKE zwm002 OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF ti_zwm003 OCCURS 0.
         INCLUDE STRUCTURE zwm003_aux.
       DATA : END OF ti_zwm003.

** Dados de tabelas de parametrização

** Tipo de depósito dos pulmões
DATA : st_pul LIKE lagp-lgtyp.

DATA: l_tit(40).

DATA: l_user LIKE sy-uname.

DATA: mblnr       LIKE mkpf-mblnr,
      gjahr       LIKE mkpf-mjahr,
      code        LIKE bapi2017_gm_code,
      mov_mm      TYPE bwlvs,
      mov_wm      TYPE bwlvs,
      porta_desc  TYPE ablad,
      to          TYPE tanum,
      st_type_o   TYPE lgtyp,
      st_type_d   TYPE lgtyp,
      bin_destino LIKE ltap-nlpla,
      bin_origem  LIKE ltap-vlpla,
      range       LIKE inri-nrrangenr,
      plant       TYPE werks_d,
      s_loc       TYPE lgort_d,
      valor(20),
      text1(40).

DATA: items     LIKE zwm018 OCCURS 0 WITH HEADER LINE,
      wa_zwm017 LIKE zwm017 OCCURS 0 WITH HEADER LINE,
      i_zwm017  LIKE zwm017 OCCURS 0 WITH HEADER LINE,
      i_sscc    LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

DATA: wa_lagpv LIKE lagpv.

******************* ALV data declare
TYPES: BEGIN OF g_ty_s_param,
         select_amount     TYPE i,
         no_info_popup     TYPE char1,
         info_popup_once   TYPE char1,
         events_info_popup TYPE lvc_fname OCCURS 0,
         edit_type         TYPE i,
         edit_mode         TYPE i,
         edit_event        TYPE i,
         edit_fields       TYPE lvc_fname OCCURS 0,
         dragdrop_type     TYPE i,
         dragdrop_effect   TYPE i,
         dragdrop_fields   TYPE lvc_fname OCCURS 0,
         bypassing_buffer  TYPE char1,
         buffer_active     TYPE char1,
       END   OF g_ty_s_param.

DATA: gs_param TYPE g_ty_s_param.

CLASS cl_gui_column_tree DEFINITION LOAD.
CLASS cl_gui_cfw DEFINITION LOAD.

DATA: tree  TYPE REF TO cl_gui_alv_tree_simple.
*----------------------------------------------------------------------
*DATA: treec  TYPE REF TO cl_gui_alv_tree_simple.
*DATA: treed  TYPE REF TO cl_gui_alv_tree_simple.
DATA: treec  TYPE REF TO cl_gui_alv_tree.
DATA: treed  TYPE REF TO cl_gui_alv_tree.
*--------------------info splitter-------------------------------------
DATA container_1 TYPE REF TO cl_gui_container.
DATA container_2 TYPE REF TO cl_gui_container.
DATA splitter TYPE REF TO cl_gui_splitter_container.

*---------------------------------------------------------------------*
*       CLASS CL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS cl_tree_event_receiver DEFINITION.

  PUBLIC SECTION.
    DATA: ucomm TYPE sy-ucomm.
    DATA: selfield TYPE slis_selfield.

*     methods handle_double_click
*       for event item_double_click of cl_gui_alv_tree
*       importing fieldname index_outtab.              "  grouplevel.

*    METHODS handle_double_click
*      FOR EVENT item_double_click OF cl_gui_alv_tree_simple
*      IMPORTING fieldname index_outtab grouplevel.
*
*    METHODS: on_add_hierarchy_node
*              FOR EVENT on_add_hierarchy_node OF cl_gui_alv_tree_simple
*                    IMPORTING grouplevel
*                              index_outtab.

**--------------------------------------------------------------------
    METHODS handle_double_click_c
                FOR EVENT item_double_click OF cl_gui_alv_tree
      IMPORTING fieldname.   " index_outtab.              " grouplevel.
*
    METHODS handle_double_click_d
                FOR EVENT item_double_click OF cl_gui_alv_tree
      IMPORTING fieldname.  " index_outtab.              "  grouplevel.
*

**--------------------------------------------------------------------
*    METHODS: on_add_hierarchy_node
*              FOR EVENT on_add_hierarchy_node OF cl_gui_alv_tree
*                    IMPORTING grouplevel
*                              index_outtab.
*
**------------------------------------------------------
*    METHODS: on_add_hierarchy_node_c
*              FOR EVENT on_add_hierarchy_node OF cl_gui_alv_tree
*                    IMPORTING grouplevel
*                              index_outtab.
**------------------------------------------------------
*    METHODS: on_add_hierarchy_node_d
*              FOR EVENT on_add_hierarchy_node OF cl_gui_alv_tree
*                    IMPORTING grouplevel
*                              index_outtab.

  PRIVATE SECTION.

ENDCLASS.                    "CL_TREE_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
*       CLASS CL_EVENT_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS cl_tree_event_receiver IMPLEMENTATION.
*  handle double_click
*   method handle_double_click.
*  MOVE index_outtab TO indice_tab.
*  PERFORM funcao.
*ENDMETHOD.                    "HANDLE_DOUBLE_CLICK
*
**--------------------------------------------------------
  METHOD handle_double_click_c.

    DATA: lt_selected_node TYPE lvc_t_nkey.
    DATA: ls_outtab TYPE zwm_aux_cargas.

    DATA: l_selected_node TYPE lvc_nkey,
          l_item_name     TYPE lvc_fname.

    DATA: l_index         TYPE lvc_nkey.

    CLEAR: ls_outtab, l_selected_node, lt_selected_node.

    CALL METHOD treec->get_selected_item
      IMPORTING
        e_selected_node = l_selected_node
        e_fieldname     = l_item_name.

    IF l_selected_node IS INITIAL.

      CALL METHOD treec->get_selected_nodes
        CHANGING
          ct_selected_nodes = lt_selected_node
        EXCEPTIONS
          cntl_system_error = 1
          dp_error          = 2
          failed            = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CALL METHOD cl_gui_cfw=>flush.
      READ TABLE lt_selected_node INTO l_selected_node INDEX 1.

    ENDIF.

    l_index = l_selected_node - g_top_node + 1.

    CASE l_item_name.
      WHEN '&Hierarchy'.
        READ TABLE gt_tab_c INTO ls_outtab INDEX l_index.
        CHECK sy-subrc = 0.
        CHECK NOT ls_outtab-remessa IS INITIAL.
        SET PARAMETER ID 'VL' FIELD ls_outtab-remessa.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
        CLEAR ls_outtab.

      WHEN 'DOCUMENTO'.
        READ TABLE gt_tab_c INTO ls_outtab INDEX l_index.
        CHECK sy-subrc = 0.
        CHECK NOT ls_outtab-documento IS INITIAL.
        SET PARAMETER ID 'GRN' FIELD ls_outtab-documento.
        CALL TRANSACTION 'VG03' AND SKIP FIRST SCREEN.
        CLEAR ls_outtab.

      WHEN 'TRANSPORTE'.
        READ TABLE gt_tab_c INTO ls_outtab INDEX l_index.
        CHECK sy-subrc = 0.
        CHECK NOT ls_outtab-transporte IS INITIAL.
        SET PARAMETER ID 'TNR' FIELD ls_outtab-transporte.
        CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
        CLEAR ls_outtab.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

  METHOD handle_double_click_d.
    DATA: lt_selected_node TYPE lvc_t_nkey.
    DATA: ls_outtab TYPE zwm_aux_descargas.

    DATA: l_selected_node TYPE lvc_nkey,
          l_item_name     TYPE lvc_fname.

    DATA: l_index         TYPE lvc_nkey.

    CLEAR: ls_outtab, l_selected_node, lt_selected_node.

    CALL METHOD treed->get_selected_item
      IMPORTING
        e_selected_node = l_selected_node
        e_fieldname     = l_item_name.

    IF l_selected_node IS INITIAL.

      CALL METHOD treed->get_selected_nodes
        CHANGING
          ct_selected_nodes = lt_selected_node
        EXCEPTIONS
          cntl_system_error = 1
          dp_error          = 2
          failed            = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      CALL METHOD cl_gui_cfw=>flush.
      READ TABLE lt_selected_node INTO l_selected_node INDEX 1.

    ENDIF.

    l_index = l_selected_node - g_top_node + 1.

    CASE l_item_name.
      WHEN '&Hierarchy'.
*        READ TABLE gt_tab_D INTO ls_outtab INDEX l_index.
*        CHECK sy-subrc = 0.
*        CHECK NOT ls_outtab-remessa IS INITIAL.
*        SET PARAMETER ID 'VL' FIELD ls_outtab-remessa.
*        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
*        CLEAR ls_outtab.

      WHEN 'ORD_COMPRA'.
        READ TABLE gt_tab_d INTO ls_outtab INDEX l_index.
        CHECK sy-subrc = 0.
        CHECK NOT ls_outtab-ord_compra IS INITIAL.
        SET PARAMETER ID 'BES' FIELD ls_outtab-ord_compra.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        CLEAR ls_outtab.

*      WHEN 'TRANSPORTE'.
*        READ TABLE gt_tab_c INTO ls_outtab INDEX l_index.
*        CHECK sy-subrc = 0.
*        CHECK NOT ls_outtab-TRANSPORTE IS INITIAL.
*        SET PARAMETER ID 'TNR' FIELD ls_outtab-TRANSPORTE.
*        CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
*        CLEAR ls_outtab.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK
*
*--------------------------------------------------------


*  METHOD on_add_hierarchy_node.
*    DATA ls_outtab_line TYPE tab.
**    ls_outtab_line-planetype = 'Note'. "#EC NOTEXT
*    CALL METHOD tree->set_hierarchy_data
*      EXPORTING
*        is_outtab_line = ls_outtab_line.
*  ENDMETHOD.                    "ON_ADD_HIERARCHY_NODE

**-------------------------------------------------------
*  METHOD on_add_hierarchy_node_c.
*    DATA ls_outtab_line TYPE tab.
**    ls_outtab_line-planetype = 'Note'. "#EC NOTEXT
*    CALL METHOD treec->set_hierarchy_data
*      EXPORTING
*        is_outtab_line = ls_outtab_line.
*  ENDMETHOD.                    "ON_ADD_HIERARCHY_NODE
*
**------------------------------------------------------
*  METHOD on_add_hierarchy_node_d.
*    DATA ls_outtab_line TYPE tab.
**    ls_outtab_line-planetype = 'Note'. "#EC NOTEXT
*    CALL METHOD treed->set_hierarchy_data
*      EXPORTING
*        is_outtab_line = ls_outtab_line.
*  ENDMETHOD.                    "ON_ADD_HIERARCHY_NODE

ENDCLASS.                    "CL_TREE_EVENT_RECEIVER IMPLEMENTATION

*DATA: tree_event_receiver TYPE REF TO cl_event_receiver.


*************

*&spwizard: declaration of tablecontrol 'FILA' itself
CONTROLS: fila TYPE TABLEVIEW USING SCREEN 0002.

*&spwizard: lines of tablecontrol 'FILA'
DATA:     g_fila_lines  LIKE sy-loopc.


*&spwizard: declaration of tablecontrol 'CARGA' itself
CONTROLS: carga TYPE TABLEVIEW USING SCREEN 0001.

*&spwizard: declaration of tablecontrol 'PORT' itself
***CONTROLS: port TYPE TABLEVIEW USING SCREEN 0005.
CONTROLS: port TYPE TABLEVIEW USING SCREEN 9005.

*&spwizard: lines of tablecontrol 'PORT'
DATA:     g_port_lines  LIKE sy-loopc.

*&spwizard: declaration of tablecontrol 'TCFILA' itself
CONTROLS: tcfila TYPE TABLEVIEW USING SCREEN 0005.

*&spwizard: lines of tablecontrol 'TCFILA'
DATA:     g_tcfila_lines  LIKE sy-loopc.

*&spwizard: declaration of tablecontrol 'CARGAA' itself
CONTROLS: cargaa TYPE TABLEVIEW USING SCREEN 0001.

*&spwizard: lines of tablecontrol 'CARGAA'
DATA:     g_cargaa_lines  LIKE sy-loopc.

*&spwizard: declaration of tablecontrol 'DESCARGA' itself
CONTROLS: descarga TYPE TABLEVIEW USING SCREEN 0001.

*&spwizard: lines of tablecontrol 'DESCARGA'
DATA:     g_descarga_lines  LIKE sy-loopc.

DATA: l_matnr    LIKE mara-matnr,
      l_lote     LIKE mch1-charg,
      l_cliente  LIKE kna1-kunnr,
      l_material LIKE mara-matnr,
      l_vbeln    LIKE likp-vbeln,
      l_cod      LIKE dd07v-domvalue_l.

*&spwizard: declaration of tablecontrol 'TCFILA1' itself
CONTROLS: tcfila1 TYPE TABLEVIEW USING SCREEN 9005.
