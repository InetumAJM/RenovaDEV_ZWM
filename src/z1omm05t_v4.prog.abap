*&---------------------------------------------------------------------*
*& Include Z1OMM03T                          Pool de módulos  Z1OMMP03 *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  z1ommp03 NO STANDARD PAGE HEADING.

TYPE-POOLS: z1om, abap, zwm01.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 18.06.2012 16:35:14
*  Motivo: Constantes
*--------------------------------------------------------------------*
INCLUDE zwm_constants.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
TYPES: BEGIN OF t_linha,
         posnr LIKE lips-posnr,
         matnr LIKE lips-matnr,
         arktx LIKE lips-arktx,
         charg LIKE lips-charg,
         lfimg LIKE lips-lfimg,
         vrkme LIKE lips-vrkme,
         vgbel LIKE lips-vgbel,
         vgpos LIKE lips-vgpos,
         uepos LIKE lips-uepos,
       END OF t_linha.

TABLES: vbss,
        vbuk,
        vbup,
        likp,
        lips,
        vbap,
        kna1,
        vbfa,
        marc,
        tcurx,
        z1mom,
        z1com,
        z1com_s, *z1com_s,
        z1pom_log,
        vekp,
        vepo,
        ltap,
        ltak,
        zwmlog,
        zwmlipslog,
        mara,
        t311,
        vttk,
        vttp,
        zwmlog02,
        vbpa,
        zwm001,
        zwm026,
        zwm046,
        zwm039,
        zwm059,
        t311a,
        mlgn,
        tvko,
        lfa1,
        zwm065.


DATA : it_ltak LIKE ltak OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF tabi_guias OCCURS 0,
        vbeln LIKE lips-vbeln,
*       komau like lipov-komau,
        vbelv LIKE vbfa-vbelv,
        kunnr LIKE likp-kunnr,
        name1 LIKE kna1-name1,
        ort01 LIKE kna1-ort01,
        linha TYPE t_linha OCCURS 0,
      END OF tabi_guias.

DATA: BEGIN OF tabi_guias_por_item OCCURS 0,
        guia LIKE lips-vbeln,
*       komau like lipov-komau,
        vbelv LIKE vbfa-vbelv,
        kunnr LIKE likp-kunnr,
        name1 LIKE kna1-name1,
        ort01 LIKE kna1-ort01,
        oferta_processada,
        guia_bloqueada,
        linha LIKE z1com_s OCCURS 0,
        servis,
      END OF tabi_guias_por_item.

DATA: g_item LIKE vbap-posnr.

*-----------------------------------------------------------------
DATA : BEGIN OF wa_grp_item.
        INCLUDE STRUCTURE z1com_s.
DATA :  guia LIKE lips-vbeln,
END OF wa_grp_item.

DATA : it_grp_item LIKE wa_grp_item OCCURS 0 WITH HEADER LINE.
DATA : sub_item LIKE z1com_s OCCURS 0 WITH HEADER LINE.

DATA : g_tabix LIKE sy-tabix.

DATA tabi_bdcdata LIKE bdcdata OCCURS 0 WITH HEADER LINE.
*-----------------------------------------------------------------
DATA: bdcdata  LIKE bdcdata OCCURS 0 WITH HEADER LINE,
      it_msg   LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA: it_message TYPE tab_bdcmsgcoll.
DATA : wa_z1com_s LIKE z1com_s.

*-----------------------------------------------------------------
DATA tabi_status_guia LIKE vbup OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF tabi_creditos OCCURS 0,
        guia  LIKE likp-vbeln,
        vbeln LIKE vbap-vbeln,
        vgpos LIKE vbap-vgpos,
        kbetr(16),
        waerk LIKE z1com_s-waerk,
      END OF tabi_creditos.

DATA: it_creditos_log LIKE tabi_creditos OCCURS 0 WITH HEADER LINE.

DATA: g_dec TYPE  esecompavg.

DATA: loop_lines LIKE sy-index,
      tab_lines LIKE sy-index,
      page_begin TYPE i,
      read_index LIKE sy-index.

DATA: okcode LIKE sy-ucomm,
      save_okcode LIKE okcode.

DATA: quant_alterada.

DATA: tabix LIKE sy-tabix,
      num_guias TYPE i.

DATA icon_oferta(50).
*-------------------------------------Histórico de emb. do fornecimento
DATA : BEGIN OF st_hu,
         venum   TYPE  venum,
         vbeln   TYPE  vbeln_vl,
         posnr   TYPE  posnr_vl,
         vhilm   TYPE  vhilm,
         exidv   TYPE  exidv,
         charg   TYPE  charg_d,
*        werks     TYPE  werks_d,
*        lgort     TYPE  lgort_d,
         matnr     TYPE  matnr,
         vemng     TYPE  vemng,
         vemeh     TYPE  vemeh,
       END OF st_hu.

DATA : it_hu LIKE st_hu OCCURS 0 WITH HEADER LINE.

*-------------------------------------Fornecimento após recalculo
DATA: BEGIN OF st_lips,
        vbeln LIKE lips-vbeln,
        posnr LIKE lips-posnr,
        matnr LIKE lips-matnr,
        lfimg LIKE lips-lfimg,
        meins LIKE lips-meins,
        charg LIKE lips-charg,
        lgtyp LIKE lips-lgtyp,
        vgpos LIKE lips-vgpos,
        uepos LIKE lips-uepos,
        pstyv LIKE lips-pstyv,
      END OF st_lips.

DATA: it_lips     LIKE st_lips OCCURS 0 WITH HEADER LINE.
DATA: it_lips_old LIKE st_lips OCCURS 0 WITH HEADER LINE.
DATA: it_lips_aux LIKE lips  OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF st_ltap.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 20.06.2012 09:52:27
*  Motivo: Inclui toda a Estrutura
*--------------------------------------------------------------------*
        INCLUDE STRUCTURE zwm026.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
DATA:

*        REMESSA       like zwm026-REMESSA,
*        sscc          LIKE zwm026-sscc,
*        QUANTIDADE    like zwm026-QUANTIDADE,
*        LOTE          like zwm026-LOTE,
*        MATERIAL      like zwm026-MATERIAL,
*        PACK_MATERIAL like zwm026-PACK_MATERIAL,
         ord(1),
      END OF st_ltap .

DATA: BEGIN OF st_ltap1,
         guia LIKE lips-vbeln,
      END OF st_ltap1 .

DATA: it_ltap      LIKE st_ltap       OCCURS 0 WITH HEADER LINE.
DATA: it_ltap_all  LIKE st_ltap       OCCURS 0 WITH HEADER LINE.
DATA: it_sscc_pick LIKE zwm_ltap_pick OCCURS 0 WITH HEADER LINE.
DATA: it_ltap1     LIKE st_ltap1      OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF emb_serv,
         kunnr     LIKE likp-kunnr,
         vhilm     LIKE vekp-vhilm,
         exidv     LIKE vekp-exidv,
         vbeln_gen LIKE vekp-vbeln_gen,
         venum     LIKE vekp-venum,
       END OF emb_serv.

DATA it_serv LIKE emb_serv OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF lin_rem,
         vhilm     LIKE vekp-vhilm,
         qtd(17),
         vbeln     LIKE likp-vbeln,
      END OF lin_rem.

DATA it_linrem LIKE lin_rem OCCURS 0 WITH HEADER LINE.

*--------------------------------------embalagens a criar

DATA: BEGIN OF st_emb,
        venum      TYPE  venum,
        vhilm      TYPE  vhilm,
        ltak_vbeln TYPE  vbeln,
        ltap_posnr TYPE  posnr,
        material   TYPE  matnr,
        quantity   TYPE  erfmg,
        unit       TYPE  meins,
        batch      TYPE  charg_d,
      END OF st_emb.

DATA: it_emb LIKE st_emb OCCURS 0 WITH HEADER LINE.

*--------------------------------------------dados para criar ot
DATA : BEGIN OF bi_tab OCCURS 0,
         posnr  TYPE posnr,
         vbeln  TYPE vbeln,
         matnr  TYPE matnr,
         lgtyp  TYPE lgtyp,
         lfimg  TYPE lfimg,
         charg  TYPE charg_d,
       END OF bi_tab.

*-------------------------------------------items para criar OT
DATA: BEGIN OF it_ot OCCURS 0,
           posnr  TYPE posnr,
           vbeln  TYPE vbeln,
      END OF it_ot.

DATA: l_tabix LIKE sy-tabix.
DATA: v_modo VALUE 'A',
      it_lqua LIKE lqua OCCURS 0 WITH HEADER LINE,
      l_num TYPE i,
      l_pos(2) TYPE n,
      idx_lips LIKE sy-tabix,
      idx_lqua LIKE sy-tabix,
      g_so_credito,
      g_pai       TYPE posnr,
      g_item_of   TYPE posnr,
      g_doc       TYPE vbeln,
      g_idx_bonus LIKE sy-tabix,
      idx_tabi    LIKE sy-tabix,
      z_tabix     LIKE sy-tabix,
      g_item_del,
      l_idx(3),
      l_zztpdoc LIKE zwmlog-zztpdoc,
      g_erro, g_bloq, l_ultimo, l_primeiro,
      g_pal TYPE i,
      flag_erro,          "Erro
      flag_bloqueio VALUE ' ',
      gv_lgnum  TYPE lgnum. " << INS ROFF(SDF):TMGP:06.01.2016 15:33:15


*------------------------------------------Associar embalagens
DATA: it_items LIKE zwm_items_hu OCCURS 0 WITH HEADER LINE,

      BEGIN OF it_vepo_fut OCCURS 0,
        venum       LIKE vepo-venum,
        vhilm       LIKE vekp-vhilm, "pchep/prouge..
        ltak_vbeln  LIKE lips-vbeln,
        ltap_posnr  LIKE ltap-posnr,
        material    LIKE lips-matnr,
        lfimg       LIKE lips-lfimg,
        unit        LIKE lips-meins,
        charg       LIKE lips-charg,
      END OF it_vepo_fut,

      l_warehouse        TYPE  lgnum   VALUE '100',
      l_plant            TYPE  werks_d,
      l_s_loc            TYPE  lgort_d,
      l_packing_material TYPE  matnr   VALUE 'PROUGE',
      l_hu               TYPE  exidv,
      l_vbeln            TYPE  vbeln.

DATA: it_zwmlog LIKE zwmlog OCCURS 0 WITH HEADER LINE.
DATA: it_zwmlog02 LIKE zwmlog02 OCCURS 0 WITH HEADER LINE.


DATA: g_rc LIKE sy-subrc, g_rc2 LIKE sy-subrc.
DATA: text TYPE  bdcmsgcoll-msgv1,
      setcursor(20), g_dummy(220),
      old_group LIKE vbss-sammg.

DATA i_string(30).

DATA: gt_lips           TYPE TABLE OF lips,
      gt_vbeln_skip     TYPE TABLE OF vbeln,
      gt_vepo           TYPE TABLE OF zwm051,
      gt_pack_in_transp TYPE zwm01_t_pack_in_transp.
