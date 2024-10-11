*&---------------------------------------------------------------------*
*& Include ZWMREP0073_TOP
*&                                                                     *
*&---------------------------------------------------------------------*

TABLES : zwm038,
         t311,
         t340d,
         lqua,
         lagp.

** Estruturas auxiliares
DATA: BEGIN OF cont_inv OCCURS 0,
        lgnum LIKE zwm038-lgnum,
        lgtyp LIKE zwm038-lgtyp,
        lgpla LIKE zwm038-lgpla,
        qtd_existente LIKE zwm038-qtd_existente,
        qtd_contada LIKE zwm038-qtd_contada,
        uni LIKE zwm038-uni,
        data_contagem LIKE zwm038-data_contagem,
        ivnum LIKE zwm038-ivnum,
        status(4),
        istat LIKE link-istat,
        uname LIKE link-uname,
        descricao LIKE dd07v-ddtext,
        chk,
END OF cont_inv.


DATA: BEGIN OF xlink_aux OCCURS 0.
        INCLUDE STRUCTURE link.
DATA: END OF xlink_aux.

DATA: BEGIN OF linp_aux OCCURS 0.
        INCLUDE STRUCTURE linp_vb.
DATA: END OF linp_aux.

DATA: BEGIN OF linv_aux OCCURS 0.
        INCLUDE STRUCTURE linv_vb.
DATA: END OF linv_aux.

***********************************************************************
** Variáveis auxiliares para o ALV
DATA: ok_code      LIKE sy-ucomm,
      grid_control TYPE REF TO cl_gui_alv_grid,
      container    TYPE REF TO cl_gui_custom_container,
      control      TYPE REF TO i_oi_container_control,
      variant      LIKE disvariant,
      repid        LIKE sy-repid,
      wa_layout    TYPE lvc_s_layo,
      wa_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      layout       TYPE slis_layout_alv,
      it_fieldcat  TYPE slis_t_fieldcat_alv,
      sort         TYPE slis_t_sortinfo_alv,
      fieldcat     TYPE lvc_t_fcat,
      wa_fieldcat  TYPE lvc_s_fcat,
      itab_sort    TYPE lvc_t_sort,
      wa_sort      TYPE lvc_s_sort.

DATA: it_header     TYPE kkblo_t_listheader.
DATA:  v_linno           LIKE sy-linno.
DATA: header        TYPE kkblo_listheader.
***************************************************************

DATA : ivnum LIKE linv-ivnum,
       result LIKE sy-tabix,
       save_index LIKE sy-tabix,
       domvalue LIKE dd07v-domvalue_l.

** Parameters
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN SKIP.

PARAMETERS: p_lgnum LIKE t311-lgnum DEFAULT '100' OBLIGATORY.
SELECTION-SCREEN SKIP.

PARAMETERS: p_data LIKE zwm038-data_contagem OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
