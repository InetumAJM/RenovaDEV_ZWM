*&---------------------------------------------------------------------*
*& Include ZWMREP0047TOP                                               *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  zwmrep0047 MESSAGE-ID zwmmsg001.

TYPE-POOLS: abap.

CONSTANTS: gc_mode_hu TYPE c VALUE 'B',
           gc_mode_normal TYPE c VALUE 'A'.


TABLES : lagp,
         zwm001,
         mara,
         vekp,
         vepo,
         lqua,
         mlgt,
         mlgn,
         marc,
         marm,
         mchb.

DATA: gv_mode TYPE c.

DATA: cursorfield(20),
      lgnum LIKE ltap-lgnum,
      ok_code_0001 LIKE sy-ucomm,
      ok_code_0002 LIKE sy-ucomm,
      ok_code_0003 LIKE sy-ucomm,
      ok_code_0004 LIKE sy-ucomm,
      text1  LIKE bdcmsgcoll-msgv1,
      text2  LIKE bdcmsgcoll-msgv2,
      mov TYPE bwlvs,
      retcode,
      resposta,
      plant TYPE werks_d,
      s_loc TYPE lgort_d,
      valor LIKE zwm001-valor,
      bin_origem(14),
      material LIKE ltap-matnr,
      tipo_palete LIKE ltap-letyp,
      unidade LIKE ltap-meins,
      pack_material LIKE mara-matnr,
      quantidade LIKE vepo-vemng,
      ean11 LIKE marm-ean11,
      lgtyp LIKE lagp-lgtyp,
      lgpla LIKE lagp-lgpla,
      qtd_total LIKE lqua-verme,
      lote LIKE lqua-charg,
      lote_in LIKE lqua-charg,
      linhas TYPE i,
      mblnr LIKE mkpf-mblnr,
      gjahr LIKE mkpf-mjahr,
** RL -> INS 02.05.2005
      documento LIKE bapi2017_gm_head_ret,
      g_mblnr LIKE mkpf-mblnr,
      g_gjahr LIKE mkpf-mjahr,
      l_mov_mm TYPE bwlvs,
      l_umrez LIKE marm-umrez,
      l_linha LIKE sy-tabix,
** RL <- INS 02.05.2005
      o_mblnr LIKE mkpf-mblnr,
      o_gjahr LIKE mkpf-mjahr,
      code LIKE bapi2017_gm_code,
      mov_mm TYPE bwlvs,
      hukey LIKE bapihukey-hu_exid,
      l_printer LIKE nast-ldest,
      l_copies  LIKE itcpo-tdcopies,
      mov_wm TYPE bwlvs,
      e_to TYPE tanum,
      certificado LIKE ltap-zeugn,
      parametro(20),
      s_type LIKE ltap-vltyp,
      adicional LIKE ltak-lznum,
      sscc_origem TYPE exidv,
      sscc_label  TYPE c LENGTH 20.

TYPES: BEGIN OF st,
      matnr LIKE lqua-matnr,
      charg LIKE lqua-charg,
      verme LIKE lqua-verme,
      meins LIKE lqua-meins,
      letyp LIKE mlgn-lety1,
      sscc  LIKE vekp-exidv,
END OF st.

DATA: t_lqua TYPE st OCCURS 0 WITH HEADER LINE,
*      c_lqua TYPE st OCCURS 0 WITH HEADER LINE,
      t_items TYPE st OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF l_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
DATA : END OF l_user.

DATA : BEGIN OF return_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF return_msg.

DATA: t_marm LIKE marm OCCURS 0 WITH HEADER LINE.

DATA: items LIKE zwm018 OCCURS 0 WITH HEADER LINE.

** RL -> INS 02.05.2005
DATA: l_meins LIKE mara-meins,
      l_index LIKE sy-tabix.

DATA: itab_aux LIKE zwm018 OCCURS 0 WITH HEADER LINE,
      itab_sum TYPE st     OCCURS 0 WITH HEADER LINE.
** RL <- INS 02.05.2005

** Para a criação da HU
DATA : BEGIN OF items_hu OCCURS 0.
        INCLUDE STRUCTURE zwm_items_hu.
DATA : END OF items_hu.

DATA: itab_sscc LIKE zwm_ean128 OCCURS 0 WITH HEADER LINE.

DATA x_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

DATA mat_struct LIKE zwm_material OCCURS 0 WITH HEADER LINE.

DATA qtd_out LIKE zwm_material-menge.

DATA: gv_werks   TYPE werks_d,
      gv_werks_t TYPE werks_d.

DATA: gv_lgort_o TYPE lgort_d.
DATA: gv_lgort_d TYPE lgort_d.
DATA: gv_bind    TYPE char14.
DATA: gv_get_batch TYPE flag.

DATA: gv_lgort_mode TYPE flag.
