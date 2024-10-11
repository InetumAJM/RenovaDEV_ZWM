*&---------------------------------------------------------------------*
*& Include ZWMREP0032TOP                                     PoolMóds. *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  zwmrep0032 MESSAGE-ID zmsg.

TABLES: mara, marm, afpo, afko, makt,t001w, mlgn, t300t, z02rparln,
        tj02t, jest, aufk, zwm030, z02rpconsprod, z02rpsessao, mean.

DATA: itab_dynp   LIKE dynpread    OCCURS 0 WITH HEADER LINE,
      itab_return LIKE ddshretval  OCCURS 0 WITH HEADER LINE,
      itab_arln   LIKE z02rparln   OCCURS 0 WITH HEADER LINE,
      itab_sessao LIKE z02rpsessao OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF itab_tj02t OCCURS 0,
        txt04 LIKE tj02t-txt04,
        istat LIKE tj02t-istat,
      END OF itab_tj02t.

DATA: ok_code    LIKE sy-ucomm,
      matnr      LIKE mara-matnr,
      maktx      LIKE makt-maktx,
      ean11      LIKE mean-ean11,
      matnr2      LIKE mara-matnr,
      maktx2      LIKE makt-maktx,
      ean112      LIKE mean-ean11,
      meins      LIKE mara-meins,
      lgnum      LIKE mlgn-lgnum,
      meins2      LIKE mara-meins,
      lgnum2      LIKE mlgn-lgnum,
      lnumt      LIKE t300t-lnumt,
      centro     LIKE z02rparln-werks VALUE 'RENV',
      desc1      LIKE t001w-name1,
      divisao    LIKE z02rparln-divisao,
      area1      LIKE z02rparln-dispo,
      linha      LIKE z02rparln-fevor,
      aufnr      LIKE afpo-aufnr,
      charg      LIKE afpo-bwtar,
      aufnr2      LIKE afpo-aufnr,
      charg2      LIKE afpo-bwtar,
      quant(1)   TYPE c,
      lhmg1      LIKE mlgn-lhmg1, "Quantidade Standard
      lhme1      LIKE mlgn-lhme1,
            lhmg2      LIKE mlgn-lhmg1, "Quantidade Standard
      lhme2      LIKE mlgn-lhme1,
      matnr_pal  LIKE mara-matnr VALUE 'PCHEP', "Tipo Palete
      maktx_pal  LIKE makt-maktx,
      printer    LIKE nast-ldest VALUE 'LOCL',
      num_pal(2) TYPE n VALUE '1',
      l_matnr    LIKE mara-matnr,
      l_lgnum    LIKE mlgn-lgnum,
      l_aufnr    LIKE afko-aufnr,
      l_lety1    LIKE mlgn-lety1,
      l_sscc1    LIKE bapihukey-hu_exid,
      l_sscc2    LIKE bapihukey-hu_exid,
      l_umrez    LIKE marm-umrez,
      l_msg(255),
      ctrl(1)    TYPE c,
      erro(1)    TYPE c,
      cursorfield(15),
      imprimir(1) TYPE c VALUE 'X',
      l_filler   LIKE sy-tabix,
      l_caracteres LIKE sy-tabix,
      l_acerto   LIKE sy-tabix,
      operador   TYPE char80,
      datum      LIKE sy-datum,
      l_lhmg1    LIKE mlgn-lhmg1.

RANGES: r_gstrs FOR afko-gstrs,
        r_data  FOR sy-datum,
        r_txt04 FOR tj02t-txt04,
        r_stat  FOR jest-stat.

DATA: BEGIN OF itab_sscc OCCURS 0,
        sscc1 LIKE bapihukey-hu_exid,
        sscc2 LIKE bapihukey-hu_exid,
        log(255),
      END OF itab_sscc.

CONSTANTS: c_x(1) VALUE '£'.

TYPE-POOLS: slis.

************************************************************************
*   Dados p/ALV
************************************************************************
DATA: fieldcat_linha TYPE slis_fieldcat_alv,
      fieldcat_tab TYPE slis_t_fieldcat_alv,
      wa_eventos TYPE slis_alv_event,
      eventos TYPE slis_t_event,
      layout TYPE slis_layout_alv,
      is_variant TYPE disvariant,
      reprepid TYPE slis_reprep_id,
      it_sort TYPE slis_t_sortinfo_alv,
      is_sort TYPE slis_sortinfo_alv,
      grid_title TYPE lvc_title.

DATA: p_cwidth(1) VALUE 'X',
      p_hrzgln(1) VALUE 'X',
      p_vrtgln(1) VALUE 'X'.
**
