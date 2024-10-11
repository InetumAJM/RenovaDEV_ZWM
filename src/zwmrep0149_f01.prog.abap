*&---------------------------------------------------------------------*
*&  Include           ZWMREP0149_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

*  IF p_wcs IS NOT INITIAL.
*    PERFORM get_data_wcs.
*
*  ELSEIF p_sap IS NOT INITIAL.
  PERFORM get_data_sap.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LIST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_data.

  IF gt_alv[] IS INITIAL.
    MESSAGE s000 WITH 'Sem dados a listar'.
    EXIT.
  ENDIF.

  CALL SCREEN '0001'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_1000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_1000.

  CASE sy-ucomm.

    WHEN 'FC01'.
*      PERFORM configuration.
  ENDCASE.

ENDFORM.                    " USER_COMMAND_1000
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv.

  DATA: ls_header    TYPE treev_hhdr,
        ls_sort      TYPE lvc_s_sort,
        ls_variant   TYPE disvariant,
        ls_list_comm TYPE slis_listheader,
        ls_layout    TYPE lvc_s_layo.

  DATA: lt_exclude   TYPE ui_functions,
        lt_sort      TYPE lvc_t_sort,
        lt_list_comm TYPE slis_t_listheader,
        lt_fieldcat  TYPE lvc_t_fcat.

  DATA: lcl_event_receiver TYPE REF TO lcl_event_receiver.

** Create ALV
**********************************************************************
  CHECK gcl_alv_grid IS INITIAL.

  IF gcl_container IS INITIAL.
    CREATE OBJECT gcl_container
      EXPORTING
        container_name = 'CONTAINER_ALV'.
  ENDIF.

  CREATE OBJECT gcl_alv_grid
    EXPORTING
      i_parent = gcl_container.

** Cabeçalho
  ls_layout-zebra      = abap_true.
  ls_layout-sel_mode   = 'D'.
  ls_layout-cwidth_opt = abap_true. "Optimize

** Variante
*  ls_variant          = us_variant.
  ls_variant-report   = sy-repid.
  ls_variant-username = sy-uname.

** Field catalog
  PERFORM get_fldcat  USING 'GT_ALV' 'ZWM_016' gt_alv
                      CHANGING lt_fieldcat.

  PERFORM change_fldcat CHANGING lt_fieldcat.

** Display
  CALL METHOD gcl_alv_grid->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = lt_exclude
      i_save               = 'A'
      is_layout            = ls_layout
      is_variant           = ls_variant
    CHANGING
      it_fieldcatalog      = lt_fieldcat
      it_outtab            = gt_alv. " Data

** Eventos
***********************************************************************
  CREATE OBJECT lcl_event_receiver.

  SET HANDLER lcl_event_receiver->handle_hotspot_click
          FOR gcl_alv_grid.

  SET HANDLER lcl_event_receiver->handle_user_command
           FOR gcl_alv_grid.

  SET HANDLER lcl_event_receiver->handle_menu_button
           FOR gcl_alv_grid.

  SET HANDLER lcl_event_receiver->handle_toolbar
          FOR gcl_alv_grid.

  CALL METHOD gcl_alv_grid->set_toolbar_interactive.

ENDFORM.                    " INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  GET_FLDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM get_fldcat    USING pu_table_nm  TYPE slis_tabname
                         pu_structure TYPE typename
                         pt_table     TYPE STANDARD TABLE
                CHANGING pt_fieldcat  TYPE lvc_t_fcat.

  DATA: lt_fieldcat_i TYPE slis_t_fieldcat_alv.

** Obtem Field Catalog
***********************************************************************
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = pu_table_nm
      i_structure_name       = pu_structure
      i_inclname             = sy-repid
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_fieldcat_i
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

** Converte o Field Catalog
  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv = lt_fieldcat_i
*     IT_SORT_ALV     =
*     IT_FILTER_ALV   =
*     IS_LAYOUT_ALV   =
    IMPORTING
      et_fieldcat_lvc = pt_fieldcat
*     ET_SORT_LVC     =
*     ET_FILTER_LVC   =
*     ES_LAYOUT_LVC   =
    TABLES
      it_data         = pt_table
    EXCEPTIONS
      it_data_missing = 1
      OTHERS          = 2.

ENDFORM.                    " GET_FLDCAT
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FLDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM change_fldcat  CHANGING pt_fieldcat TYPE lvc_t_fcat.

  FIELD-SYMBOLS: <ls_fcat> TYPE lvc_s_fcat.

** Ajusta o Fieldcatalog
***********************************************************************
  LOOP AT pt_fieldcat ASSIGNING <ls_fcat>.

    CASE <ls_fcat>-fieldname.

      WHEN 'MANDT'.
        <ls_fcat>-no_out = 'X'.

      WHEN 'TIMESTAMP'.
        <ls_fcat>-no_out = 'X'.

      WHEN 'PROCESS'.
        <ls_fcat>-no_out = 'X'.

      WHEN 'ICON'.
        <ls_fcat>-coltext   = 'Status'.
*        <ls_fcat>-hotspot  = 'X'.
        <ls_fcat>-icon      = 'X'.
        <ls_fcat>-outputlen = '4'.

      WHEN 'TANUM'.
        <ls_fcat>-hotspot  = 'X'.


      WHEN 'ICON_OT'.
        <ls_fcat>-coltext   = 'St. OT'.
*        <ls_fcat>-hotspot  = 'X'.
        <ls_fcat>-icon      = 'X'.
        <ls_fcat>-outputlen = '4'.

      WHEN 'LENUM_SAP'.
        <ls_fcat>-coltext   = 'SSCC SAP'.
        <ls_fcat>-scrtext_s = 'SSCC SAP'.
        <ls_fcat>-scrtext_m = 'SSCC SAP'.
        <ls_fcat>-scrtext_l = 'SSCC SAP'.

      WHEN 'LENUM_WCS'.
        <ls_fcat>-coltext   = 'SSCC WCS'.
        <ls_fcat>-scrtext_s = 'SSCC WCS'.
        <ls_fcat>-scrtext_m = 'SSCC WCS'.
        <ls_fcat>-scrtext_l = 'SSCC WCS'.

      WHEN 'ERRORMSG'.
        <ls_fcat>-coltext   = 'Mensagem'.
        <ls_fcat>-scrtext_s = 'Mensagem'.
        <ls_fcat>-scrtext_m = 'Mensagem'.
        <ls_fcat>-scrtext_l = 'Mensagem'.

    ENDCASE.

  ENDLOOP.

ENDFORM.                    " CHANGE_FLDCAT
*&---------------------------------------------------------------------*
*&      Form  GET_LAST_OT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_LTAP_ORG  text
*      -->P_LS_LTAP_DES  text
*      <--P_LT_LTAP  text
*----------------------------------------------------------------------*
FORM get_last_ot  USING    ps_ltap_org TYPE ltap
                           ps_ltap_des TYPE ltap
                  CHANGING ps_ltap     TYPE ltap.

  IF ps_ltap_org-qdatu > ps_ltap_des-qdatu.
    ps_ltap = ps_ltap_org.

  ELSEIF ps_ltap_org-qdatu = ps_ltap_des-qdatu.

    IF ps_ltap_org-qzeit > ps_ltap_des-qzeit.
      ps_ltap = ps_ltap_org.
    ELSE.
      ps_ltap = ps_ltap_des.
    ENDIF.

  ELSE.
    ps_ltap = ps_ltap_des.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_WCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_wcs.

*  DATA: ls_ltap_org    TYPE ltap.
*  DATA: ls_ltap_des    TYPE ltap.
*  DATA: ls_ltap_sla    TYPE ltap.
*  DATA: ls_vekp        TYPE vekp.
*
*  DATA: lt_items       TYPE zwmrf_stock_wamas OCCURS 0 WITH HEADER LINE.
*  DATA: lt_items_slave TYPE zwmrf_stock_wamas OCCURS 0 WITH HEADER LINE.
*  DATA: lt_items_desc  TYPE zwmrf_stock_wamas OCCURS 0 WITH HEADER LINE.
*  DATA: lt_items_split TYPE zwmrf_stock_wamas OCCURS 0 WITH HEADER LINE.
*  DATA: lt_zwmrft002   TYPE zwmrft002         OCCURS 0 WITH HEADER LINE.
*  DATA: lt_ltap        TYPE ltap              OCCURS 0 WITH HEADER LINE.
*  DATA: lt_ltap_org    TYPE ltap              OCCURS 0 WITH HEADER LINE.
*  DATA: lt_ltap_des    TYPE ltap              OCCURS 0 WITH HEADER LINE.
*  DATA: lt_ltap_sla    TYPE ltap              OCCURS 0 WITH HEADER LINE.
*  DATA: lt_lqua        TYPE lqua              OCCURS 0 WITH HEADER LINE.
*  DATA: lt_ltak        TYPE ltak              OCCURS 0 WITH HEADER LINE.
*  DATA: lv_hu_type     TYPE zwmrf_hu_type.
*
*** Obter Stock WAMAS
***********************************************************************
*  CALL FUNCTION 'ZWMRF_GET_STOCK_WAMAS'
*    EXPORTING
*      i_lgnum       = p_lgnum
*      i_all         = 'X'
*    TABLES
*      t_items       = lt_items
*      t_items_desc  = lt_items_desc
*      t_items_split = lt_items_split
*    EXCEPTIONS
*      error         = 1
*      OTHERS        = 2.
*
*  DELETE lt_items       WHERE base_qty <= 0.
*  DELETE lt_items_desc  WHERE base_qty <= 0.
*  DELETE lt_items_split WHERE base_qty <= 0.
*
*  IF p_desc IS NOT INITIAL.
*    lt_items[] = lt_items_desc[].
*  ELSEIF p_split IS NOT INITIAL.
*    lt_items[] = lt_items_split[].
*  ENDIF.
*
*** Filtros
***********************************************************************
*  IF s_matnr[] IS NOT INITIAL.
*    DELETE lt_items WHERE matnr NOT IN s_matnr.
*  ENDIF.
*
*  IF s_plant[] IS NOT INITIAL.
*    DELETE lt_items WHERE plant NOT IN s_plant.
*  ENDIF.
*
*  IF s_stloc[] IS NOT INITIAL.
*    DELETE lt_items WHERE stge_loc NOT IN s_stloc.
*  ENDIF.
*
*  IF s_sttype[] IS NOT INITIAL.
*    DELETE lt_items WHERE stock_type NOT IN s_sttype.
*  ENDIF.
*
*  IF s_spstck[] IS NOT INITIAL.
*    DELETE lt_items WHERE sp_stck_no NOT IN s_spstck.
*  ENDIF.
*
*  IF s_batch[] IS NOT INITIAL.
*    DELETE lt_items WHERE batch NOT IN s_batch.
*  ENDIF.
*
*  IF s_bbd[] IS NOT INITIAL.
*    DELETE lt_items WHERE bbd NOT IN s_bbd.
*  ENDIF.
*
*  IF s_sscc[] IS NOT INITIAL.
*    DELETE lt_items WHERE sscc NOT IN s_sscc.
*  ENDIF.
*
*  IF s_loadid[] IS NOT INITIAL.
*    DELETE lt_items WHERE load_aid NOT IN s_loadid.
*  ENDIF.
*
*  IF s_sscc_s[] IS NOT INITIAL.
*    DELETE lt_items WHERE sscc_slave NOT IN s_sscc_s.
*  ENDIF.
*
*  IF s_taxes[] IS NOT INITIAL.
*    DELETE lt_items WHERE taxes NOT IN s_taxes.
*  ENDIF.
*
*  IF s_stoloc[] IS NOT INITIAL.
*    DELETE lt_items WHERE stoloc NOT IN s_stoloc.
*  ENDIF.
*
*** Stock do Wamas
*  IF lt_items[] IS NOT INITIAL.
*    SELECT *
*      FROM lqua INTO TABLE lt_lqua
*      FOR ALL ENTRIES IN lt_items
*      WHERE lgnum = p_lgnum
*      AND   lenum = lt_items-sscc.
*
*    SELECT *
*      FROM lqua APPENDING TABLE lt_lqua
*      FOR ALL ENTRIES IN lt_items
*      WHERE lgnum = p_lgnum
*      AND   lenum = lt_items-sscc_slave.
*
*    SELECT *
*      FROM ltap INTO TABLE lt_ltap_org
*      FOR ALL ENTRIES IN lt_items
*      WHERE vlenr = lt_items-sscc
*      AND   lgnum = p_lgnum.
*
*    SELECT *
*      FROM ltap INTO TABLE lt_ltap_des
*      FOR ALL ENTRIES IN lt_items
*      WHERE nlenr = lt_items-sscc
*      AND   lgnum = p_lgnum.
*  ENDIF.
*
*  lt_items_slave[] = lt_items[].
*
*  DELETE lt_items_slave WHERE sscc_slave IS INITIAL.
*
*  IF lt_items_slave[] IS NOT INITIAL.
*    SELECT *
*     FROM ltap INTO TABLE lt_ltap_sla
*     FOR ALL ENTRIES IN lt_items_slave
*     WHERE vlenr = lt_items_slave-sscc_slave
*     AND   lgnum = p_lgnum.
*  ENDIF.
*
*  APPEND LINES OF lt_ltap_org TO lt_ltap.
*  APPEND LINES OF lt_ltap_des TO lt_ltap.
*  APPEND LINES OF lt_ltap_sla TO lt_ltap.
*
*  IF lt_ltap[] IS NOT INITIAL.
*    SELECT *
*      FROM ltak INTO TABLE lt_ltak
*      FOR ALL ENTRIES IN lt_ltap
*      WHERE lgnum = lt_ltap-lgnum
*      AND   tanum = lt_ltap-tanum.
*  ENDIF.
*
*  IF lt_ltak[] IS NOT INITIAL.
*    SELECT *
*      FROM zwmrft002 INTO TABLE lt_zwmrft002
*      FOR ALL ENTRIES IN lt_ltak
*      WHERE armazem = lt_ltak-lgnum
*      AND   grupo   = lt_ltak-refnr.
*  ENDIF.
*
*  SORT lt_ltap     BY vlenr nlenr pquit.
*  SORT lt_ltap_org BY vlenr qdatu DESCENDING qzeit DESCENDING.
*  SORT lt_ltap_des BY nlenr qdatu DESCENDING qzeit DESCENDING.
*  SORT lt_lqua     BY lenum.
*
*** Dados listagem ALV
***********************************************************************
*  LOOP AT lt_items.
*    CLEAR: gs_alv, ls_vekp.
*
*    IF lt_items-sscc_slave IS NOT INITIAL.
** Inetum SDF - Migração Rise 2022 - LD >
**      SELECT SINGLE *
**        FROM vekp INTO ls_vekp
**        WHERE exidv = lt_items-sscc_slave.
*      SELECT * UP TO 1 ROWS
*        FROM vekp INTO ls_vekp
*        WHERE exidv = lt_items-sscc_slave
*        ORDER BY PRIMARY KEY.
*      ENDSELECT.
** < Inetum SDF - Migração Rise 2022 - LD
*    ENDIF.
*
*    MOVE-CORRESPONDING lt_items TO gs_alv.
*
*
*
*** Dados da SU
*    CLEAR lt_lqua.
*
*    READ TABLE lt_lqua WITH KEY lenum = lt_items-sscc.
*    IF sy-subrc <> 0.
*      IF lt_items-sscc_slave IS NOT INITIAL.
*        READ TABLE lt_lqua WITH KEY lenum = lt_items-sscc_slave.
*        IF sy-subrc <> 0.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    gs_alv-lgtyp = lt_lqua-lgtyp.
*    gs_alv-lgpla = lt_lqua-lgpla.
*    gs_alv-stge_loc = lt_lqua-lgort.
*    gs_alv-stock_type = lt_lqua-bestq.
*
*** Dados da OT
*    CLEAR lt_ltap.
*
*    DO 1 TIMES.
*
*      " OT Pendente
*      READ TABLE lt_ltap WITH KEY vlenr = lt_items-sscc
*                                  pquit = ''.
*      CHECK sy-subrc <> 0.
*
*      READ TABLE lt_ltap WITH KEY nlenr = lt_items-sscc
*                                  pquit = ''.
*      CHECK sy-subrc <> 0.
*
*      IF ls_vekp-inhalt = 'SANDWICH'.
*        READ TABLE lt_ltap WITH KEY vlenr = lt_items-sscc_slave
*                                    pquit = ''.
*        CHECK sy-subrc <> 0.
*      ENDIF.
*
*      " Ultima OT confirmada
*      CLEAR ls_ltap_org.
*      READ TABLE lt_ltap_org INTO ls_ltap_org WITH KEY vlenr = lt_items-sscc.
*
*      CLEAR ls_ltap_des.
*      READ TABLE lt_ltap_des INTO ls_ltap_des WITH KEY nlenr = lt_items-sscc.
*
*      PERFORM get_last_ot USING ls_ltap_org ls_ltap_des CHANGING lt_ltap.
*
*      IF ls_vekp-inhalt = 'SANDWICH'.
*        CLEAR ls_ltap_des.
*        READ TABLE lt_ltap_sla INTO ls_ltap_sla WITH KEY vlenr = lt_items-sscc_slave.
*
*        PERFORM get_last_ot USING lt_ltap ls_ltap_sla CHANGING lt_ltap.
*      ENDIF.
*
*    ENDDO.
*
*    gs_alv-tanum = lt_ltap-tanum.
*    gs_alv-vltyp = lt_ltap-vltyp.
*    gs_alv-vlpla = lt_ltap-vlpla.
*    gs_alv-nltyp = lt_ltap-nltyp.
*    gs_alv-nlpla = lt_ltap-nlpla.
*
*    " Rack
*    IF gs_alv-stoloc = 'R' OR gs_alv-stoloc = 'S' OR
*       gs_alv-stoloc = 'E' OR gs_alv-stoloc = 'F'.
*
*      IF gs_alv-lgtyp =  '600' AND
*         gs_alv-lgpla = 'BLACK-BOX'.
*        gs_alv-icon = icon_led_green.
*      ELSE.
*        gs_alv-icon = icon_led_red.
*      ENDIF.
*
*      " Ramp
*    ELSE."IF gs_alv-stoloc = '3'.
*      IF gs_alv-lgpla IS INITIAL.
*        gs_alv-icon = icon_led_green.
*
*        READ TABLE lt_ltak WITH KEY tanum = gs_alv-tanum.
*        IF sy-subrc = 0.
*          READ TABLE lt_zwmrft002 WITH KEY grupo = lt_ltak-refnr.
*          IF sy-subrc = 0.
*            gs_alv-lgtyp = lt_zwmrft002-zona.
*            gs_alv-lgpla = lt_zwmrft002-porta.
*          ENDIF.
*        ENDIF.
*
*      ELSE.
*        gs_alv-icon = icon_led_red.
*      ENDIF.
*
*    ENDIF.
*
*    IF lt_ltap-pquit = ''.
*      gs_alv-icon_ot = icon_ws_start_whse_proc_backgr.
*      CLEAR gs_alv-icon.
*
*    ELSE.
*      gs_alv-icon_ot = icon_store_location.
*    ENDIF.
*
*    IF NOT p_type IS INITIAL.
*
*      CLEAR: lv_hu_type.
*      CALL FUNCTION 'ZWMRF_GET_INFO_HU'
*        EXPORTING
*          i_hu      = gs_alv-sscc
*        IMPORTING
*          e_hu_type = lv_hu_type
*        EXCEPTIONS
*          error     = 1
*          OTHERS    = 2.
*      IF sy-subrc EQ 0.
*        gs_alv-hu_type = lv_hu_type.
*      ENDIF.
*    ENDIF.
*
*
*    APPEND gs_alv TO gt_alv.
*  ENDLOOP.
*
*  IF s_lgtyp[] IS NOT INITIAL.
*    DELETE gt_alv WHERE lgtyp NOT IN s_lgtyp.
*  ENDIF.
*
*  IF s_lgpla[] IS NOT INITIAL.
*    DELETE gt_alv WHERE lgpla NOT IN s_lgpla.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_SAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_sap.

  DATA: ls_vekp      TYPE vekp.
  DATA: ls_stock     TYPE zwmpt_ws_stock_picture.
  DATA: ls_stock_res TYPE zwmpt_ws_stock_picture_respons.

  DATA: lt_zwm026    TYPE zwm026 OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm028    TYPE zwm028 OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap      TYPE ltap   OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap_epe  TYPE ltap   OCCURS 0 WITH HEADER LINE.
  DATA: lt_lqua      TYPE lqua   OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltak      TYPE ltak   OCCURS 0 WITH HEADER LINE.

  DATA: lt_stock     TYPE zwmpt_ws_stock_picture_res_tab.


** Obter Stock SAP e WCS
**********************************************************************
  CALL FUNCTION 'ZWM_GET_STOCK_WCS'
    EXPORTING
      i_lgnum  = p_lgnum
      is_stock = ls_stock
    TABLES
      t_stock  = lt_stock
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

** Stock de SAP
  SELECT *
    FROM lqua INTO TABLE lt_lqua
    WHERE lgnum = p_lgnum
    AND   lgtyp  = 'AUT'.

** Filtros
**********************************************************************
  IF s_matnr[] IS NOT INITIAL.
    DELETE lt_lqua  WHERE matnr NOT IN s_matnr.
    DELETE lt_stock WHERE matnr NOT IN s_matnr.
  ENDIF.

  IF s_charg[] IS NOT INITIAL.
    DELETE lt_lqua  WHERE charg NOT IN s_charg.
    DELETE lt_stock WHERE charg NOT IN s_charg.
  ENDIF.

  IF s_bestq[] IS NOT INITIAL.
    DELETE lt_lqua  WHERE bestq NOT IN s_bestq.
    DELETE lt_stock WHERE bestq NOT IN s_bestq.
  ENDIF.

  IF s_vfdat[] IS NOT INITIAL.
    DELETE lt_lqua  WHERE vfdat NOT IN s_vfdat.
    DELETE lt_stock WHERE vfdat NOT IN s_vfdat.
  ENDIF.

  IF s_lenum[] IS NOT INITIAL.
    DELETE lt_lqua  WHERE lenum NOT IN s_lenum.
    DELETE lt_stock WHERE lenum NOT IN s_lenum.
  ENDIF.

  IF lt_lqua[] IS NOT INITIAL.
    SELECT *
     FROM ltap INTO TABLE lt_ltap
     FOR ALL ENTRIES IN lt_lqua
     WHERE lgnum = lt_lqua-lgnum
     AND   tanum = lt_lqua-btanr.

    SELECT *
      FROM ltap INTO TABLE lt_ltap_epe
      FOR ALL ENTRIES IN lt_lqua
      WHERE lgnum = lt_lqua-lgnum
      AND   nlenr = lt_lqua-lenum.

    SELECT *
      FROM zwm026 INTO TABLE lt_zwm026
      FOR ALL ENTRIES IN lt_lqua
      WHERE armazem = lt_lqua-lgnum
      AND   sscc    = lt_lqua-lenum.
  ENDIF.

  IF lt_ltap[] IS NOT INITIAL.
    SELECT *
      FROM ltak INTO TABLE lt_ltak
      FOR ALL ENTRIES IN lt_ltap
      WHERE lgnum = lt_ltap-lgnum
      AND   tanum = lt_ltap-tanum.
  ENDIF.

  IF lt_ltak[] IS NOT INITIAL.
    SELECT *
      FROM zwm028 INTO TABLE lt_zwm028
      FOR ALL ENTRIES IN lt_ltak
      WHERE lgnum = lt_ltak-lgnum
      AND   refnr = lt_ltak-refnr.
  ENDIF.

  DELETE lt_ltap_epe WHERE vltyp <> 'EPE'.

  SORT lt_ltap_epe BY vltyp nlenr.
  SORT lt_zwm028   BY lgnum refnr.
  SORT lt_zwm026   BY sscc.
  SORT lt_ltap     BY lgnum tanum.
  SORT lt_stock    BY lenum.

** Dados Paletes SAP
**********************************************************************
  LOOP AT lt_lqua.
    CLEAR: gs_alv, ls_vekp.

    gs_alv-matnr     = lt_lqua-matnr.
    gs_alv-bestq     = lt_lqua-bestq.
    gs_alv-charg     = lt_lqua-charg.
    gs_alv-vfdat     = lt_lqua-vfdat.
    gs_alv-lenum_sap = lt_lqua-lenum.
    gs_alv-meins     = lt_lqua-meins.

    IF lt_lqua-verme IS NOT INITIAL.
      gs_alv-menge = lt_lqua-verme.
*    ELSEIF lt_lqua-einme IS NOT INITIAL.
*      gs_alv-menge = lt_lqua-einme.
    ELSEIF lt_lqua-ausme IS NOT INITIAL AND lt_lqua-einme IS INITIAL.
      gs_alv-menge = lt_lqua-ausme.
    ENDIF.

    CHECK gs_alv-menge IS NOT INITIAL.

    " Validar Palete
    READ TABLE lt_stock WITH KEY lenum = lt_lqua-lenum TRANSPORTING NO FIELDS BINARY SEARCH.
    IF sy-subrc = 0.
      gs_alv-icon      = icon_led_green.
      gs_alv-lenum_wcs = lt_lqua-lenum.

      DELETE lt_stock INDEX sy-tabix.
    ELSE.
      gs_alv-icon = icon_led_red.
    ENDIF.

    " Paletização Especial
    READ TABLE lt_ltap_epe WITH KEY vltyp = 'EPE'
                                    nlenr = lt_lqua-lenum BINARY SEARCH.
    IF sy-subrc = 0.
      gs_alv-pal_typ = 'E'.
    ENDIF.

    " Picking
    READ TABLE lt_zwm026 WITH KEY sscc = lt_lqua-lenum BINARY SEARCH.
    IF sy-subrc = 0.
      gs_alv-pal_typ = 'P'.
    ENDIF.

    " Dados da OT
    READ TABLE lt_ltap WITH KEY lgnum = lt_lqua-lgnum
                                tanum = lt_lqua-btanr BINARY SEARCH.
    IF sy-subrc = 0.
      IF lt_ltap-pquit = ''.
        gs_alv-icon_ot = icon_ws_start_whse_proc_backgr.
      ELSE.
        gs_alv-icon_ot = icon_store_location.
      ENDIF.
    ENDIF.

    gs_alv-tanum = lt_ltap-tanum.
    gs_alv-nltyp = lt_ltap-nltyp.
    gs_alv-nlpla = lt_ltap-nlpla.

    " Validar Pulmão
    READ TABLE lt_ltak WITH KEY tanum = gs_alv-tanum.
    IF sy-subrc = 0.
      gs_alv-refnr = lt_ltak-refnr.

      IF gs_alv-refnr IS NOT INITIAL.
        READ TABLE lt_zwm028 WITH KEY refnr = lt_ltak-refnr BINARY SEARCH.
        IF sy-subrc = 0.
          gs_alv-nltyp = lt_zwm028-st_pul.
          gs_alv-nlpla = lt_zwm028-pulmao1.
        ENDIF.
      ENDIF.

    ENDIF.

    APPEND gs_alv TO gt_alv.
  ENDLOOP.

** Dados Paletes WCS
**********************************************************************
  LOOP AT lt_stock INTO ls_stock_res.

    CLEAR: gs_alv.
    gs_alv-matnr     = ls_stock_res-matnr.
    gs_alv-bestq     = ls_stock_res-bestq.
    gs_alv-charg     = ls_stock_res-charg.
    gs_alv-vfdat     = ls_stock_res-vfdat.
    gs_alv-lenum_wcs = ls_stock_res-lenum.
    gs_alv-meins     = ls_stock_res-meins.
    gs_alv-menge     = ls_stock_res-vsolm.
    gs_alv-icon      = icon_led_red.

    APPEND gs_alv TO gt_alv.
  ENDLOOP.

** Filtros
  IF s_refnr[] IS NOT INITIAL.
    DELETE gt_alv WHERE refnr NOT IN s_refnr.
  ENDIF.

  IF s_nltyp[] IS NOT INITIAL.
    DELETE gt_alv WHERE nltyp NOT IN s_nltyp.
  ENDIF.

  IF s_nlpla[] IS NOT INITIAL.
    DELETE gt_alv WHERE nlpla NOT IN s_nlpla.
  ENDIF.

  IF p_pck IS NOT INITIAL.
    DELETE gt_alv WHERE pal_typ <> 'P'.
  ENDIF.

  IF p_ppe IS NOT INITIAL.
    DELETE gt_alv WHERE pal_typ <> 'E'.
  ENDIF.

  SORT gt_alv BY lenum_sap matnr charg.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TRANSFERE_PALETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfere_palete .
*  DATA: lt_create_to TYPE TABLE OF ltap_creat,
*        lt_hu_mat    TYPE TABLE OF vepo,
*        lt_hu_item   TYPE TABLE OF bapihuitem,
*
*        ls_create_to TYPE ltap_creat,
*        ls_hu_mat    TYPE vepo,
*        ls_hu_item   TYPE bapihuitem,
*
*        lv_hu_type   TYPE zwmrf_hu_type,
*        lv_master    TYPE exidv,
*        lv_tanum     TYPE tanum,
*        lv_lznum     TYPE ltak-lznum.
*
*  DATA: lt_rows TYPE lvc_t_row,
*        ls_rows TYPE lvc_s_row,
*        ls_alv  TYPE st_alv_table.
*
*
*  REFRESH: lt_create_to, lt_hu_mat, lt_hu_item, lt_rows.
*  CLEAR:   ls_create_to, ls_hu_mat, ls_hu_item, lv_hu_type,
*           lv_master, lv_tanum, ls_rows, ls_alv.
*
*
**  IF p_wamas IS INITIAL.
***   615  Só é possivel transferir palete a partir do Stock do WAMAS!
**    MESSAGE s615 DISPLAY LIKE 'E'.
**    EXIT.
**  ENDIF.
*
*  CALL METHOD gcl_alv_grid->get_selected_rows
*    IMPORTING
*      et_index_rows = lt_rows.
*
*  CHECK NOT lt_rows IS INITIAL.
*
*  IF lines( lt_rows ) GT 1.
**   614  Só pode selecionar uma palete de cada vez!
*    MESSAGE s614 DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*
*  READ TABLE lt_rows INTO ls_rows INDEX 1.
*  CHECK sy-subrc EQ 0.
*
*  READ TABLE gt_alv INTO ls_alv INDEX ls_rows-index.
*  CHECK sy-subrc EQ 0.
*
*  IF ls_alv-icon_ot NE '@AC@'.
**   617  Palete & com tarefa pendente!
*    MESSAGE s617 DISPLAY LIKE 'E' WITH ls_alv-sscc.
*    EXIT.
*  ENDIF.
*
** Diferentes Casos Possiveis
** 1. Palete Simples
**    LS_ALV-SSCC - Preenchido
**    LS_ALV-SSCC_SLAVE - Vazio
**
** 2. Palete Sandwich
**    LS_ALV-SSCC - Preenchido
**    LS_ALV-SSCC_SLAVE - Numero Palete master -
**    Transfere LS_ALV-SSCC_SLAVE para cada uma das LS_ALV-SSCC (conforme Qtd de paletes dentro)
**
** 3. Meias Paletes (são duas sem Escrava)
**    LS_ALV-SSCC - Preenchido
**    LS_ALV-SSCC_SLAVE - Preenchido
**    Transfere as duas LS_ALV-SSCC para a Master (LS_ALV-SSCC_SLAVE)
**
** ??. Meias Paletes (sao duas com escrava)
**    LS_ALV-SSCC - Preenchido
**    LS_ALV-SSCC_SLAVE - Preenchido
**    Transfere
*
*
**E_HU_TYPE = 1 - Palete Simples
*
*  CALL FUNCTION 'ZWMRF_GET_INFO_HU'
*    EXPORTING
*      i_hu        = ls_alv-sscc
**     I_PALETE    =
*    IMPORTING
*      e_hu_master = lv_master
**     E_PALETE    =
*      e_hu_type   = lv_hu_type
*    TABLES
*      t_hu_mat    = lt_hu_mat
*      t_huitem    = lt_hu_item
*    EXCEPTIONS
*      error       = 1
*      OTHERS      = 2.
*  IF sy-subrc <> 0.
*    EXIT.
*  ENDIF.
*
*  CASE lv_hu_type.
*    WHEN 1. "Palete Simples
*
**     Todos os materiais da HU
*      LOOP AT lt_hu_mat INTO ls_hu_mat.
*        ls_create_to-matnr = ls_hu_mat-matnr.
*        ls_create_to-werks = ls_alv-plant.
*        ls_create_to-lgort = ls_alv-stge_loc.
*        ls_create_to-charg = ls_hu_mat-charg.
*        ls_create_to-bestq = ls_alv-stock_type.
*        ls_create_to-anfme = ls_hu_mat-vemng.
*        ls_create_to-altme = ls_hu_mat-altme.
*        ls_create_to-vltyp = '600'.
*        ls_create_to-vlpla = 'BLACK-BOX'.
*        ls_create_to-vlenr = ls_alv-sscc.
*        ls_create_to-nltyp = '603'.
*        ls_create_to-nlpla = 'INSP'.
*        ls_create_to-nlenr = ls_alv-sscc.
*        APPEND ls_create_to TO lt_create_to.
*      ENDLOOP.
*
*    WHEN 2. " SANDWICH
*      LOOP AT lt_hu_item INTO ls_hu_item WHERE hu_item_type EQ 1.
*        ls_create_to-matnr = ls_hu_item-material.
*        ls_create_to-werks = ls_alv-plant.
*        ls_create_to-lgort = ls_alv-stge_loc.
*        ls_create_to-charg = ls_hu_item-batch.
*        ls_create_to-bestq = ls_alv-stock_type.
*        ls_create_to-anfme = ls_hu_item-pack_qty.
*        ls_create_to-altme = ls_hu_item-alt_unit_qty.
*        ls_create_to-vltyp = '600'.
*        ls_create_to-vlpla = 'BLACK-BOX'.
*        ls_create_to-vlenr = lv_master.
*        ls_create_to-nltyp = '603'.
*        ls_create_to-nlpla = 'INSP'.
*        ls_create_to-nlenr = ls_hu_item-hu_exid.
*        APPEND ls_create_to TO lt_create_to.
*      ENDLOOP.
*
*    WHEN 4. " Meias Paletes
*      LOOP AT lt_hu_item INTO ls_hu_item WHERE hu_item_type EQ 1.
*        ls_create_to-matnr = ls_hu_item-material.
*        ls_create_to-werks = ls_alv-plant.
*        ls_create_to-lgort = ls_alv-stge_loc.
*        ls_create_to-charg = ls_hu_item-batch.
*        ls_create_to-bestq = ls_alv-stock_type.
*        ls_create_to-anfme = ls_hu_item-pack_qty.
*        ls_create_to-altme = ls_hu_item-alt_unit_qty.
*        ls_create_to-vltyp = '600'.
*        ls_create_to-vlpla = 'BLACK-BOX'.
*        ls_create_to-vlenr = ls_hu_item-hu_exid.
*        ls_create_to-nltyp = '603'.
*        ls_create_to-nlpla = 'INSP'.
*        ls_create_to-nlenr = lv_master.
*        APPEND ls_create_to TO lt_create_to.
*      ENDLOOP.
*    WHEN OTHERS.
*  ENDCASE.
*
*  CHECK NOT lt_create_to[] IS INITIAL.
*
*  CLEAR lv_lznum.
*
*  IF ls_alv-sscc_slave(5) = '2DUSS'.
*    lv_lznum = ls_alv-sscc_slave.
*  ENDIF.
*
*  CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
*    EXPORTING
*      i_lgnum       = p_lgnum
*      i_bwlvs       = '912'
*      i_lznum       = lv_lznum
*    IMPORTING
*      e_tanum       = lv_tanum
*    TABLES
*      t_ltap_creat  = lt_create_to
*    EXCEPTIONS
*      error_message = 99.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    EXIT.
*  ELSE.
**   616  OT & criada com Sucesso!
*    MESSAGE i616 WITH lv_tanum.
*  ENDIF.

ENDFORM.
