*&---------------------------------------------------------------------*
*&  Include           ZDADOS0046                                       *
*&---------------------------------------------------------------------*
TYPE-POOLS : slis.

INCLUDE <icon>.

TABLES: zwmlog, zwmlog02, usr01, zwmlipslog, likp.

DATA: it_zwmlog   LIKE zwmlog   OCCURS 0 WITH HEADER LINE,
      it_zwmlog02 LIKE zwmlog02 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF itab OCCURS 0.
        INCLUDE STRUCTURE zwmlog02.
DATA: END OF itab.



DATA: fieldcat_linha TYPE slis_fieldcat_alv,
      fieldcat_tab   TYPE slis_t_fieldcat_alv,
      wa_events      TYPE slis_alv_event,
      events         TYPE slis_t_event,
      layout         TYPE slis_layout_alv,
      sort           TYPE slis_t_sortinfo_alv,
      reprepid       TYPE slis_reprep_id,
      repid          LIKE sy-repid,
      idx            LIKE sy-tabix,
      g_bloq,
      it_fieldcat    TYPE  slis_t_fieldcat_alv.

*------------------------------------------------Reprocessa Remessa
DATA: delivery_items LIKE lips OCCURS 0 WITH HEADER LINE,
      prot LIKE prott OCCURS 0 WITH HEADER LINE,
      con_x                 TYPE c   VALUE 'X'.

*------------------------------------------------Reprocessa OT
DATA : BEGIN OF bi_tab OCCURS 0,
         posnr  TYPE posnr,
         vbeln  TYPE vbeln,
         matnr  TYPE matnr,
         lgtyp  TYPE lgtyp,
         lfimg  TYPE lfimg,
         charg  TYPE charg_d,
       END OF bi_tab.

DATA: bdcdata  LIKE bdcdata OCCURS 0 WITH HEADER LINE,
      it_msg   LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

*-------------------------------------------------Reprocessa HU

DATA: BEGIN OF it_hu OCCURS 0,
       lgnum LIKE zwmlog02-lgnum,
       hu    LIKE zwmlog02-hu,
       vbeln LIKE zwmlog02-vbeln,
      END OF it_hu.

DATA: zdummy(220).

DATA: l_zztpdoc(10),
      g_qtd(17),
      f_salesdocument     LIKE bapivbeln-vbeln,
      it_order_header_in  LIKE bapisdh1,
      it_order_header_inx LIKE bapisdh1x,
      it_return           LIKE bapiret2  occurs 0 with header line,
      it_conditions_in    LIKE bapicond  occurs 0 with header line,
      it_conditions_inx   LIKE bapicondx occurs 0 with header line.

*----------------------------------------------------Tela 0160

DATA: BEGIN OF IT_VTTP OCCURS 0,
       TKNUM LIKE VTTP-TKNUM,
       VBELN LIKE VTTP-VBELN,
      END OF IT_VTTP.

data: g_rc like sy-subrc.
DATA: text TYPE  bdcmsgcoll-msgv1,
      setcursor(20).
