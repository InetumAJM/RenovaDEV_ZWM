*&---------------------------------------------------------------------*
*&  Include           ZDADOS                                           *
*&---------------------------------------------------------------------*

CLASS cl_gui_column_tree DEFINITION LOAD.
CLASS cl_gui_cfw         DEFINITION LOAD.

DATA tree1      TYPE REF TO cl_gui_alv_tree.
DATA mr_toolbar TYPE REF TO cl_gui_toolbar.

CONSTANTS: gc_vkorg_multi     TYPE vkorg VALUE 'MULT',
           gc_multi_vbeln_txt TYPE c LENGTH 20 VALUE '<< MULTI REMESSA >>',
           gc_incomplete_tag  TYPE c LENGTH 3 VALUE 'XXX'.

INCLUDE zwm_constants.

INCLUDE <icon>.

TABLES: t320,
        t311,
        t340,
        lagp,
        vttk,
        vbpa,
        adrc,
        tvkot,
        makt,
        mara,
        marm,
        mlgn,
        mlgt,
        ltap,
        ltak,
        likp,
        lqua,
        zwm001,
        zwm002,
        zwm028,
        zwm026,
        zwm003_aux,
        zwm006_aux,
        zwm031,
        zwm040,
        zwm020,
        zwm049.

DATA: BEGIN OF all_tasks OCCURS 0,
        vkorg           LIKE likp-vkorg,
        lgnum           LIKE ltak-lgnum,
        tanum           LIKE ltak-tanum,
        vbeln           LIKE likp-vbeln, "ltak-vbeln,
        posnr           LIKE ltap-posnr,
        refnr           LIKE ltak-refnr,
        refnt           LIKE t311-refnt,
        kquit           LIKE ltak-kquit,
        tapos           LIKE ltap-tapos,          "item data
        vltyp           LIKE ltap-vltyp,
        vlpla           LIKE ltap-vlpla,
        matnr           LIKE ltap-matnr,
        vsolm           LIKE ltap-vsolm,
        nsolm           LIKE ltap-nsolm,
        vista           LIKE ltap-vista,
        meins           LIKE ltap-meins,
        pquit           LIKE ltap-pquit, "Confirmation
        pvqui           LIKE ltap-pvqui, "Pick
        ename           LIKE ltap-ename,
        vdifm           LIKE ltap-vdifm,
        kgvnq           LIKE ltak-kgvnq,
        posty           LIKE ltap-posty,
        vlenr           LIKE ltap-vlenr,
        letyp           LIKE ltap-letyp,
        maktx           LIKE ltap-maktx,
        st_pul          LIKE zwm028-st_pul,
        pulmao1         LIKE zwm028-pulmao1,
        pulmao2         LIKE zwm028-pulmao2,
        st_ppk          LIKE zwm028-st_ppk,
        pre_pick        LIKE zwm028-pre_pick,
        st_dck          LIKE zwm028-st_dck,
        porta           LIKE zwm028-porta,
        zlock           LIKE zwm028-zlock,
        num_recorrido   LIKE zwm026-num_recorrido,
        total_paletes   LIKE zwm028-total_paletes,
        paletes_pulmao  LIKE zwm028-paletes_pulmao,
        paletes_dri     LIKE zwm028-paletes_pulmao,
        paletes_tri     LIKE zwm028-paletes_pulmao,
        paletes_prm     LIKE zwm028-paletes_pulmao,
        paletes_picking LIKE zwm028-paletes_pulmao,
        paletes_blk     LIKE zwm028-paletes_pulmao,
        sscc            LIKE zwm026-sscc,
        prioridade,
        paletes_carro   LIKE zwm028-paletes_carro,
        pick_2step      TYPE flag,
        pick_2spart     TYPE flag,
        kober           TYPE kober,
        benum           TYPE ltak-benum,
        nltyp           TYPE ltap-nltyp,
        free_idoc       TYPE edi_docnum,
      END OF all_tasks.

DATA: itab_serv LIKE all_tasks OCCURS 0 WITH HEADER LINE.

DATA: gt_expand_node TYPE TABLE OF lvc_nkey.

DATA: BEGIN OF itab_serv_aux OCCURS 0,
        refnr    LIKE t311-refnr,
        kunnr    LIKE kna1-kunnr,
        servisan,
      END OF itab_serv_aux.

DATA: BEGIN OF tab_out OCCURS 0,
        all(20)            TYPE c,
        vkorg_out(51),
        vkorg              LIKE likp-vkorg,
        refnr_out(51),
        refnr              LIKE ltak-refnr,
        kunnr              LIKE kna1-kunnr,
        serv_out(51),
        vbeln(20),
*       vbeln LIKE likp-vbeln, "ltak-vbeln,
        posnr              LIKE ltap-posnr,
        servisan           LIKE sy-tabix,
        vbeln_out(51),
        tanum(10),
        tapos(4),
        vltyp              LIKE ltap-vltyp,
        vlpla              LIKE ltap-vlpla,
        matnr              LIKE ltap-matnr,
        maktx              LIKE makt-maktx,
        vsolm              LIKE ltap-vsolm,
        nsolm              LIKE ltap-nsolm,
        meins              LIKE ltap-meins,
        letyp              LIKE ltap-letyp,
        status(4),
        created            TYPE c,
        open               TYPE i,
        pick               TYPE i,
        user               LIKE ltap-ename,
        destino(35),
        pul_side           TYPE c LENGTH 2,
        zlock              LIKE zwm028-zlock,
        palete_especial(1),
        to_partida(1),
        to_partida_pkl(1),
        icon_lock(4),
        icon_created(4),
        desc_lock(20),
        icon_palete(4),
        icon_portaria(4),
        num_recorrido      LIKE zwm026-num_recorrido,
        transporte         LIKE vttk-tknum,
        index              LIKE sy-tabix, "Indice de controle de ordenação
        nova_ordenacao     LIKE sy-tabix, "Indice para ordenação de Carga
        total_paletes      LIKE zwm028-total_paletes,
        paletes_pulmao     LIKE zwm028-paletes_pulmao,
        porta              LIKE zwm028-porta,
        vlenr              LIKE ltap-vlenr,
        sscc               LIKE zwm026-sscc,
        idocnum            TYPE edi_docnum,
        idocnum_lib        TYPE edi_docnum,
        posicao_pulmao(2),
        prioridade,
        paletes_carro      LIKE zwm028-paletes_pulmao,
        paletes_dri(4),
        paletes_tri(4),
        paletes_prm(4),
        paletes_crd(4),
        paletes_aut(4),
        paletes_pck(4),
        paletes_blk(4),
        paletes_bpk(4),
*       paletes_dri    LIKE zwm028-paletes_pulmao,
*       paletes_tri    LIKE zwm028-paletes_pulmao,
*       paletes_prm    LIKE zwm028-paletes_pulmao,
*       paletes_crd    LIKE zwm028-paletes_pulmao,
*       paletes_picking LIKE zwm028-paletes_pulmao,
      END OF tab_out.

TYPES: t_out LIKE tab_out.

DATA : BEGIN OF l_portas_carga OCCURS 0.
         INCLUDE STRUCTURE zwm002.
         DATA : tipo LIKE zwm007-tipo.
DATA : END OF l_portas_carga.

DATA: BEGIN OF gt_group_rem OCCURS 0,
        refnr  TYPE lvs_refnr,
        vbeln  TYPE vbeln,
        2step  TYPE flag,
        2spart TYPE flag,
      END OF gt_group_rem.

DATA: itab_carga LIKE zwm003_aux OCCURS 0 WITH HEADER LINE.

DATA BEGIN OF itab_aux OCCURS 0.
        INCLUDE STRUCTURE zwm006_aux.
DATA: index LIKE sy-tabix,
      END OF itab_aux.

DATA ti_zwm001 LIKE zwm001 OCCURS 0 WITH HEADER LINE.
DATA: itens TYPE lvc_t_indx.

DATA lt_zwm003 LIKE zwm003_aux OCCURS 0 WITH HEADER LINE.
DATA: gt_zwm028 TYPE TABLE OF zwm028 WITH HEADER LINE.
DATA: gt_zwm040 TYPE TABLE OF zwm040 WITH HEADER LINE.

DATA: linhas   TYPE i,
      aux_node TYPE lvc_index.

DATA: zlock            LIKE zwm028-zlock,
      texto0400(40),
      caract_carga(40),
      st_destino       LIKE ltap-vltyp,
      st_destino2      LIKE ltap-vltyp,
      bin_destino1     LIKE ltap-vlpla,
      bin_destino2     LIKE ltap-vlpla,
      bin_kober1       TYPE kober.

DATA: g_lock LIKE zwm028-zlock.

DATA: g_grupo LIKE t311-refnr.

DATA i_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF i_grupo OCCURS 0.
DATA : grupo LIKE zwm026-grupo.
DATA : END OF i_grupo.

RANGES: grupo FOR t311-refnr,
        transporte FOR vttk-tknum,
        r_id FOR zwm040-id_servisan.

DATA: gr_refnr        TYPE RANGE OF lvs_refnr,
      gr_refnr_2step  TYPE RANGE OF lvs_refnr,
      gr_refnr_2spart TYPE RANGE OF lvs_refnr.

DATA: percentagem TYPE p DECIMALS 2,
      indice_tab  TYPE lvc_index,
      l_index     LIKE sy-tabix.

DATA: xuser LIKE lrf_wkqu OCCURS 0 WITH HEADER LINE.

DATA: picking_queue LIKE ltak-queue,
      tri_queue     LIKE ltak-queue,
      dri_queue     LIKE ltak-queue,
      queue_crd     LIKE ltak-queue,
      aut_queue_tri LIKE ltak-queue,
      aut_queue     LIKE ltak-queue,
      picking_prm   LIKE ltak-queue,
      num_dias      TYPE i.

DATA valor LIKE zwm001-valor.

DATA: wa_lagpv LIKE lagpv.

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
DATA: ok_code            LIKE sy-ucomm,   "OK-Code
      ok_code_0200       LIKE sy-ucomm,   "OK-Code
      ok_code_0400       LIKE sy-ucomm,   "OK-Code
      g_desbloq          LIKE zwm028-tipo_lock,
      g_ok_code          LIKE sy-ucomm,
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

DATA: BEGIN OF lt_aux OCCURS 0,
        vbeln           LIKE likp-vbeln,
        refnr           LIKE t311-refnr,
        palete_especial,
      END OF lt_aux.

DATA: BEGIN OF lt_to_part OCCURS 0,
        refnr          LIKE t311-refnr,
        vbeln          LIKE likp-vbeln,
        tanum          LIKE ltap-tanum,
        tapos          LIKE ltap-tapos,
        to_partida,
        to_partida_pkl,
      END OF lt_to_part.

DATA: BEGIN OF lt_transportes OCCURS 0,
        refnr LIKE t311-refnr,
        tknum LIKE vttk-tknum,
      END OF lt_transportes.


DATA: BEGIN OF lt_paletes OCCURS 0,
        refnr   LIKE t311-refnr,
        vbeln   LIKE likp-vbeln,
        kunnr   LIKE kna1-kunnr,
        pal_dri LIKE ltap-vsolm,
        pal_tri LIKE ltap-vsolm,
        pal_prm LIKE ltap-vsolm,
        pal_crd LIKE ltap-vsolm,
        pal_aut LIKE ltap-vsolm,
        pal_blk LIKE ltap-vsolm,
        pal_pck LIKE ltap-vsolm,
        pal_bpk LIKE ltap-vsolm,
*  pal_dri LIKE zwm028-paletes_pulmao,
*  pal_tri LIKE zwm028-paletes_pulmao,
*  pal_prm LIKE zwm028-paletes_pulmao,
      END OF lt_paletes.

DATA trans LIKE vttk-tknum.

DATA: ok_code_0300 LIKE sy-ucomm,
      l_prioridade LIKE zwm028-prioridade.

DATA qtd_pal LIKE ltap-vsolm.

DATA  gv_init_lgnum TYPE lgnum.
DATA  ok_code_0001(5).
DATA  pulmao LIKE lagp-lgtyp.
DATA  bin_pulmao LIKE lagp-lgpla.
DATA  porta_pul LIKE lagp-lgtyp.
DATA  bin_porta_pul LIKE lagp-lgpla.
DATA  pick_pre LIKE lagp-lgtyp.
DATA  bin_pick_pre LIKE lagp-lgpla.
DATA  posicoes LIKE zwm027 OCCURS 0 WITH HEADER LINE.
DATA index TYPE sy-tabix.
DATA num_prio     TYPE i.
DATA: paletes_dri LIKE ltap-vsolm,
      paletes_tri LIKE ltap-vsolm,
      paletes_prm LIKE ltap-vsolm,
      paletes_crd LIKE ltap-vsolm,
      paletes_aut LIKE ltap-vsolm,
      paletes_blk LIKE ltap-vsolm,
      paletes_pck LIKE ltap-vsolm,
      paletes_bpk LIKE ltap-vsolm.

DATA lt_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE.
DATA: t_ltak LIKE ltak OCCURS 0 WITH HEADER LINE,
      t_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.

DATA qtd_res LIKE ltap-vsolm.
DATA: itab_28   LIKE zwm028    OCCURS 0 WITH HEADER LINE,
      lt_t311   LIKE t311      OCCURS 0 WITH HEADER LINE,
      lt_t311a  LIKE t311a      OCCURS 0 WITH HEADER LINE,
      lt_zwm028 LIKE zwm028    OCCURS 0 WITH HEADER LINE.


DATA: gr_lgpla_v TYPE RANGE OF lgpla.
DATA: gs_r_lgpla_v LIKE LINE OF gr_lgpla_v.

DATA: pal_picking TYPE n LENGTH 3,
      pal_aut     TYPE n LENGTH 3,
      pal_manual  TYPE n LENGTH 3,
      pal_esp     TYPE n LENGTH 3.

gs_r_lgpla_v-low = '999-*'.
gs_r_lgpla_v-option = 'CP'.
gs_r_lgpla_v-sign = 'I'.
APPEND gs_r_lgpla_v TO gr_lgpla_v.

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
    CASE fcode.
      WHEN 'WAVE'.
        PERFORM criar_wave_picking.
      WHEN 'PRIO'.
        PERFORM altera_prioridade.
      WHEN 'USERADD'.
        PERFORM assign_user.
      WHEN 'TO_PAR'.
        PERFORM processa_to_partida.
      WHEN 'TO_PKL'.
        PERFORM procesa_to_pkl.
*      WHEN 'CARGA'.
*        PERFORM tipo_carga.
      WHEN 'LOCK1'.
*       Lock Total
        g_lock = 1.
*        perform altera_lock using g_lock.
      WHEN 'LOCK2'.
*       Unlock Picking
        g_lock = 2.
        PERFORM altera_lock USING g_lock.
      WHEN 'LOCK3'.
*       Unlock Paletes Completas
        g_lock = 3.
        PERFORM altera_lock USING g_lock.
      WHEN 'LOCK4'.
*       Unlock Picking e Paletes Completas
        g_lock = 4.
        PERFORM altera_lock USING g_lock.
      WHEN 'LOCK5'.
*       Unlock Carga
        g_lock = 5.
        PERFORM altera_lock USING g_lock.
      WHEN 'LOADCAR'.
*       Carrega Carro
        PERFORM load_car.
      WHEN 'FREE_WORK'.
        PERFORM free_work.
      WHEN 'RESEND_IDOC'.
        PERFORM resend_idoc.
      WHEN 'PRINT_PAL'.
        PERFORM print_pal_especial.
      WHEN 'MAPA_PLAN'.
        PERFORM mapa_planeamento.

    ENDCASE.
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
