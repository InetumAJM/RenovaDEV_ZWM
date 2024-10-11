*&---------------------------------------------------------------------*
*&  Include           ZWMREP0139_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.

  DATA: BEGIN OF lt_user OCCURS 0.
          INCLUDE STRUCTURE lrf_wkqu.
  DATA: END OF lt_user.

** Armazém
  CLEAR p_lgnum.

  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = lt_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  READ TABLE lt_user WITH KEY statu = 'X'.  " con_x.
  IF sy-subrc = 0.
    p_lgnum = lt_user-lgnum.
  ENDIF.

** Pulmão
*  p_lgtyp = 'PUL'.

*  CLEAR s_lgtyp.
*  s_lgtyp-sign   = 'I'.
*  s_lgtyp-option = 'EQ'.
*  s_lgtyp-low    = 'PUL'.
*  APPEND s_lgtyp.
*
*  CLEAR s_lgtyp.
*  s_lgtyp-sign   = 'I'.
*  s_lgtyp-option = 'EQ'.
*  s_lgtyp-low    = 'PLT'.
*  APPEND s_lgtyp.
*
*  CLEAR s_lgtyp.
*  s_lgtyp-sign   = 'I'.
*  s_lgtyp-option = 'EQ'.
*  s_lgtyp-low    = 'PLM'.
*  APPEND s_lgtyp.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  DATA: lv_destino TYPE zwm013-destino.

  DATA: ls_zwm013  TYPE zwm013.
  DATA: ls_lein    TYPE lein.

  DATA: lt_zwm013  TYPE zwm013 OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm026  TYPE zwm026 OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltak    TYPE ltak   OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap    TYPE ltap   OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap_r  TYPE ltap   OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap_g  TYPE ltap   OCCURS 0 WITH HEADER LINE.
  DATA: lt_vbuk    TYPE vbuk   OCCURS 0 WITH HEADER LINE.
  DATA: lt_t311a   TYPE t311a  OCCURS 0 WITH HEADER LINE.

** Dados ALV
**********************************************************************
  IF p_lgtyp <> 'PUL' AND p_lgtyp <> 'PLT' AND p_lgtyp <> 'PLM'.
    MESSAGE s000(0k) WITH 'Tipo depósito' p_lgtyp 'inválido' DISPLAY LIKE 'E'.
    gv_flag_error = 'X'.
    EXIT.
  ENDIF.

  CONCATENATE p_lgtyp '%' INTO lv_destino.

  SELECT *
    FROM zwm013 INTO TABLE lt_zwm013
    WHERE armazem = p_lgnum AND
          destino LIKE lv_destino.

  IF s_lgpla[] IS NOT INITIAL.
    DELETE lt_zwm013 WHERE destino+4(10) NOT IN s_lgpla.
  ENDIF.

  IF lt_zwm013[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      FOR ALL ENTRIES IN lt_zwm013
      WHERE lgnum = p_lgnum AND
            vlenr = lt_zwm013-sscc.

    SELECT *
      FROM zwm026 INTO TABLE lt_zwm026
      FOR ALL ENTRIES IN lt_zwm013
      WHERE armazem = p_lgnum
      AND   sscc    = lt_zwm013-sscc.
  ENDIF.

** OTs para Remessa
  lt_ltap_r[] = lt_ltap[].

  DELETE lt_ltap_r WHERE nltyp <> '916'.

  IF lt_ltap_r[] IS NOT INITIAL.
    SELECT *
      FROM vbuk INTO TABLE lt_vbuk
      FOR ALL ENTRIES IN lt_ltap_r
      WHERE vbeln = lt_ltap_r-nlpla.
  ENDIF.

** OTs para Grupo
  lt_ltap_g[] = lt_ltap[].

  DELETE lt_ltap_g WHERE nltyp <> '815'.

  IF lt_ltap_g[] IS NOT INITIAL.
    SELECT *
      FROM ltak INTO TABLE lt_ltak
      FOR ALL ENTRIES IN lt_ltap_g
      WHERE lgnum = lt_ltap_g-lgnum AND
            tanum = lt_ltap_g-tanum.

    DELETE lt_ltak WHERE refnr IS INITIAL.
  ENDIF.

  IF lt_ltak[] IS NOT INITIAL.
    SELECT *
      FROM t311a INTO TABLE lt_t311a
      FOR ALL ENTRIES IN lt_ltak
      WHERE lgnum = lt_ltak-lgnum
      AND   refnr = lt_ltak-refnr.
  ENDIF.

  IF lt_zwm026[] IS NOT INITIAL.
    SELECT *
      FROM t311a APPENDING TABLE lt_t311a
      FOR ALL ENTRIES IN lt_zwm026
      WHERE lgnum = lt_zwm026-armazem
      AND   refnr = lt_zwm026-grupo.
  ENDIF.

  IF lt_t311a[] IS NOT INITIAL.
    SELECT *
      FROM vbuk APPENDING TABLE lt_vbuk
      FOR ALL ENTRIES IN lt_t311a
      WHERE vbeln = lt_t311a-rbnum.
  ENDIF.

** Alv
**********************************************************************
  SORT lt_ltap_r BY vlenr.
  SORT lt_ltap_g BY vlenr.
  SORT lt_vbuk   BY vbeln.
  SORT lt_zwm026 BY sscc.
  SORT lt_t311a  BY refnr rbnum.

  REFRESH gt_alv_table.

  LOOP AT lt_zwm013.

    CLEAR gs_alv_table.

    MOVE-CORRESPONDING lt_zwm013 TO gs_alv_table.

    DO 1 TIMES.

** Paletes Completas
      SELECT SINGLE *
         FROM lein INTO ls_lein
         WHERE lenum = lt_zwm013-sscc.

      IF sy-subrc = 0.
        gs_alv_table-icon_wm = icon_warehouse.
        EXIT.
      ENDIF.

      " Remessa
      CLEAR lt_ltap_r.
      READ TABLE lt_ltap_r WITH KEY vlenr = lt_zwm013-sscc.
      IF sy-subrc = 0.

        CLEAR lt_vbuk.
        READ TABLE lt_vbuk WITH KEY vbeln = lt_ltap_r-nlpla.
        IF lt_vbuk-wbstk <> 'C'.
          gs_alv_table-icon_wm = icon_warehouse.
        ENDIF.

        IF lt_vbuk-vbeln IS NOT INITIAL.
          gs_alv_table-documento = lt_vbuk-vbeln.
        ENDIF.

        EXIT.
      ENDIF.

      " Grupo
      CLEAR lt_ltap_g.
      READ TABLE lt_ltap_g WITH KEY vlenr = lt_zwm013-sscc.
      IF sy-subrc = 0.

        CLEAR lt_ltak.
        READ TABLE lt_ltak WITH KEY tanum = lt_ltap_g-tanum.

        IF lt_ltak-refnr IS NOT INITIAL.
          gs_alv_table-documento = lt_ltak-refnr.
        ENDIF.

        CLEAR lt_vbuk.
        LOOP AT lt_t311a WHERE refnr = lt_ltak-refnr.

          CLEAR lt_vbuk.
          READ TABLE lt_vbuk WITH KEY vbeln = lt_t311a-rbnum.
          IF lt_vbuk-wbstk = 'C'.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF lt_vbuk-wbstk <> 'C'.
          gs_alv_table-icon_wm = icon_warehouse.
        ENDIF.

        EXIT.
      ENDIF.

** Paletes de Picking
      READ TABLE lt_zwm026 WITH KEY sscc = lt_zwm013-sscc.
      IF sy-subrc = 0.

        IF lt_zwm026-grupo IS NOT INITIAL.
          gs_alv_table-documento = lt_zwm026-grupo.
        ENDIF.

        CLEAR lt_vbuk.
        LOOP AT lt_t311a WHERE refnr = lt_zwm026-grupo.

          CLEAR lt_vbuk.
          READ TABLE lt_vbuk WITH KEY vbeln = lt_t311a-rbnum.
          IF lt_vbuk-wbstk = 'C'.
            EXIT.
          ENDIF.

        ENDLOOP.

        IF lt_vbuk-wbstk <> 'C'.
          gs_alv_table-icon_wm = icon_warehouse.
        ENDIF.

        EXIT.
      ENDIF.
    ENDDO.

*    IF p_wm IS NOT INITIAL.
*      CHECK gs_alv_table-icon_wm  IS INITIAL.
*    ENDIF.

    APPEND gs_alv_table TO gt_alv_table.
  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_SSCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_sscc.

  DATA: lv_tabix     TYPE sy-tabix.
  DATA: lv_count     TYPE i.

  DATA: ls_rows      TYPE lvc_s_row.
  DATA: ls_stable    TYPE lvc_s_stbl.
  DATA: ls_zwm013    TYPE zwm013.
  DATA: ls_lein      TYPE lein.
  DATA: ls_alv_table TYPE st_alv_table.

  DATA: lt_rows      TYPE lvc_t_row.
  DATA: lt_alv_table TYPE TABLE OF st_alv_table.

** Get Lines
**********************************************************************
  CALL METHOD gcl_alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  LOOP AT lt_rows INTO ls_rows.

    READ TABLE gt_alv_table INTO gs_alv_table INDEX ls_rows-index.
    CHECK sy-subrc = 0.

    APPEND gs_alv_table TO lt_alv_table.

  ENDLOOP.

  LOOP AT lt_alv_table INTO ls_alv_table.
    lv_tabix = sy-tabix.

    CHECK ls_alv_table-icon_wm IS INITIAL.

    MOVE-CORRESPONDING ls_alv_table TO ls_zwm013.

    SELECT SINGLE *
      FROM lein INTO ls_lein
      WHERE lenum = ls_zwm013-sscc.



    IF sy-subrc <> 0.
      DELETE FROM zwm013 WHERE armazem = ls_zwm013-armazem AND
                               sscc    = ls_zwm013-sscc.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.

        lv_count = lv_count + 1.
      ENDIF.

      DELETE gt_alv_table WHERE armazem = ls_zwm013-armazem AND
                                sscc    = ls_zwm013-sscc.
    ENDIF.
  ENDLOOP.

  IF lv_count IS NOT INITIAL.
    MESSAGE s330(zwmmsg001) WITH lv_count.
  ENDIF.

  ls_stable-row = 'X'.

  CALL METHOD gcl_alv_grid->refresh_table_display
    EXPORTING
      is_stable = ls_stable.


ENDFORM.                    " DELETE_SSCC
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.

  CHECK gv_flag_error IS INITIAL.

  CALL SCREEN '0001'.

ENDFORM.                    " PROCESS_DATA
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
  PERFORM get_fldcat  USING 'GT_ALV_TABLE' 'ZWM_008' gt_alv_table
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
      it_outtab            = gt_alv_table. " Data

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
      it_fieldcat_alv       = lt_fieldcat_i
*     IT_SORT_ALV           =
*     IT_FILTER_ALV         =
*     IS_LAYOUT_ALV         =
    IMPORTING
      et_fieldcat_lvc       = pt_fieldcat
*     ET_SORT_LVC           =
*     ET_FILTER_LVC         =
*     ES_LAYOUT_LVC         =
    TABLES
      it_data               = pt_table
    EXCEPTIONS
      it_data_missing       = 1
      OTHERS                = 2.

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

      WHEN 'ARMAZEM'.

      WHEN 'SSCC'.

      WHEN 'DESTINO'.
        <ls_fcat>-coltext   = 'Bin'.
        <ls_fcat>-outputlen = '16'.

      WHEN 'POSICAO_PULMAO'.
        <ls_fcat>-coltext   = 'Posição Pulmão'.
        <ls_fcat>-outputlen = '16'.

      WHEN 'DOCUMENTO'.
        <ls_fcat>-coltext   = 'Documento'.
        <ls_fcat>-outputlen = '10'.


      WHEN 'ICON_WM'.
        <ls_fcat>-coltext   = 'Armazém'.
*        <ls_fcat>-hotspot  = 'X'.
        <ls_fcat>-icon      = 'X'.
        <ls_fcat>-outputlen = '6'.

      WHEN OTHERS.
        <ls_fcat>-no_out = 'X'.

    ENDCASE.

  ENDLOOP.

ENDFORM.                    " CHANGE_FLDCAT
*&---------------------------------------------------------------------*
*&      Form  SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_output.

  LOOP AT SCREEN.
    IF screen-name = 'S_LGTYP-LOW' OR
       screen-name = 'S_LGTYP-HIGH'.
      screen-input = 0.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " SCREEN_OUTPUT
