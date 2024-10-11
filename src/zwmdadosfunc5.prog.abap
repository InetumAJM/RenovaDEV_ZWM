*&---------------------------------------------------------------------*
*&  Include           ZWMDADOSFUNC5                                    *
*&---------------------------------------------------------------------*
TYPE-POOLS z1om.

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
        VTTP,
        zwmlog02,
        vbpa,
        zwm039.


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
      END OF tabi_guias_por_item.

DATA tabi_bdcdata LIKE bdcdata OCCURS 0 WITH HEADER LINE.
*-----------------------------------------------------------------
DATA: bdcdata  LIKE bdcdata OCCURS 0 WITH HEADER LINE,
      it_msg   LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
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

data: it_creditos_log like tabi_creditos occurs 0 with header line.

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
*         werks     TYPE  werks_d,
*         lgort     TYPE  lgort_d,
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
data: it_lips_aux like lips  OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF st_ltap,
*        REMESSA       like zwm026-REMESSA,
        sscc          LIKE zwm026-sscc,
*        QUANTIDADE    like zwm026-QUANTIDADE,
*        LOTE          like zwm026-LOTE,
*        MATERIAL      like zwm026-MATERIAL,
*        PACK_MATERIAL like zwm026-PACK_MATERIAL,
      END OF st_ltap .

DATA it_ltap LIKE st_ltap OCCURS 0 WITH HEADER LINE.
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
      IDX_TABI    LIKE sy-tabix,
      z_tabix     LIKE sy-tabix,
      g_item_del,
      l_idx(2),
      l_zztpdoc LIKE zwmlog-zztpdoc,
      g_erro, g_bloq.

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
      l_plant            TYPE  werks_d VALUE 'RENV',
      l_s_loc            TYPE  lgort_d VALUE 'CD',
      l_packing_material TYPE  matnr   VALUE 'PROUGE',
      l_hu               TYPE  exidv,
      l_vbeln            TYPE  vbeln.

data: it_zwmlog like zwmlog occurs 0 with header line.
data: it_zwmlog02 like zwmlog02 occurs 0 with header line.

*----------------------------------------------------Tela 0150

DATA: BEGIN OF IT_VTTP OCCURS 0,
       TKNUM LIKE VTTP-TKNUM,
       VBELN LIKE VTTP-VBELN,
      END OF IT_VTTP.

data: g_rc like sy-subrc.
DATA: text TYPE  bdcmsgcoll-msgv1,
      setcursor(20).
