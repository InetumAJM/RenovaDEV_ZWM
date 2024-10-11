*&---------------------------------------------------------------------*
*&  Include           ZWMREP0135_D01
*&---------------------------------------------------------------------*

************************************************************************
** Tabelas DD
************************************************************************
TABLES: mara, zwm030, makt, mean, usr01.

************************************************************************
** Types e Includes
************************************************************************
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


************************************************************************
** Variáveis
************************************************************************
DATA: idx       LIKE sy-tabix,
      programa  LIKE sy-repid,
      l_aut.

DATA: gv_lgnum TYPE lgnum,
      gv_lgort TYPE lgort_d.


************************************************************************
**
************************************************************************
TYPES: BEGIN OF gty_itab,
        werks TYPE werks_d,
        lgort TYPE lgort_d,
        matnr TYPE matnr,
        maktx LIKE makt-maktx,
        ean11 LIKE mean-ean11,
        charg TYPE charg_d,
        menge TYPE menge_d,
        meins TYPE meins,
        data  TYPE datum,
       END OF gty_itab.

DATA: gt_itab TYPE TABLE OF gty_itab.
