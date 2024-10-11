*&---------------------------------------------------------------------*
*& Report  ZWM0001
*&---------------------------------------------------------------------*
************************************************************************
*&      *********************************************************
*&      *  INETUM - Consultoria em Tecnologia de Informação     *
*&      *                                                       *
*&      *                         SAP                           *
*&      *********************************************************
*&  Report    : ZWMREP0150
*&  Transação : ZWM168
*&  Nm.Cliente: Renova
*&  Descrição.: Monitor Modo Noite
*&  Criado Por: AndersonJM
*&  Criado em.: 04/06/2024
************************************************************************

REPORT zwmrep0150.

TABLES zwm083.

DATA gt_alv TYPE STANDARD TABLE OF zwm_017.
*DATA gt_zwm083 TYPE STANDARD TABLE OF zwm083.
*DATA gt_zwm030 TYPE STANDARD TABLE OF zwm030.

DATA: gr_table     TYPE REF TO cl_salv_table.
CLASS lcl_handle_events DEFINITION DEFERRED.
DATA event_handler TYPE REF TO lcl_handle_events.

CONSTANTS: gc_empty VALUE ''.
CONSTANTS: gc_renv TYPE zwm030-werks VALUE 'RENV'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_linha FOR zwm083-linha.
*PARAMETERS: p_semord AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

INCLUDE zwmrep0150_cl01.

START-OF-SELECTION.

  PERFORM start_of_selection.

  PERFORM alv.


*  CALL SCREEN 9500.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9500 OUTPUT.
  SET PF-STATUS 'STATUS_9500'.
  SET TITLEBAR 'TTB_9500'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9500  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9500 INPUT.

  CASE sy-ucomm.
    WHEN 'VOLTAR'.
      PERFORM uc_voltar.
    WHEN 'SALVAR'.
      PERFORM uc_salvar.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  UC_VOLTAR
*&---------------------------------------------------------------------*
FORM uc_voltar .

  SET SCREEN 0.
  LEAVE SCREEN.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  UC_SALVAR
*&---------------------------------------------------------------------*
FORM uc_salvar .

*  DATA lt_zwm083     TYPE STANDARD TABLE OF zwm083.
*  DATA lt_zwm083_del TYPE STANDARD TABLE OF zwm083.
**  lt_zwm083 = CORRESPONDING #( gt_alv ).
*
*  REFRESH lt_zwm083.
*  LOOP AT gt_alv INTO DATA(ls_alv)
*    WHERE "inativo EQ abap_false AND
*          alterado EQ abap_true.
*    IF ls_alv-inativo EQ abap_true.
*      APPEND CORRESPONDING #( ls_alv ) TO lt_zwm083.
*    ELSE.
*      APPEND CORRESPONDING #( ls_alv ) TO lt_zwm083_del.
*    ENDIF.
*  ENDLOOP.
*
*  DESCRIBE TABLE lt_zwm083     LINES DATA(lv_lines).
*  DESCRIBE TABLE lt_zwm083_del LINES DATA(lv_lines_del).
*
*  IF lt_zwm083 IS NOT INITIAL.
*    MODIFY zwm083 FROM TABLE lt_zwm083.
*  ENDIF.
*
*  IF lt_zwm083_del IS NOT INITIAL.
*    DELETE zwm083 FROM TABLE lt_zwm083_del.
*  ENDIF.
*
*  ADD lv_lines_del TO lv_lines.
*  IF lv_lines IS NOT INITIAL.
*    COMMIT WORK.
*  ENDIF.
*
*  MESSAGE |{ lv_lines } registro(s) salvo(s).| TYPE 'S'.

  PERFORM uc_voltar.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM start_of_selection .

  SELECT *
    FROM zwm083
    INTO TABLE @DATA(lt_zwm083)
    WHERE linha IN @s_linha.

  DATA lt_linhas TYPE zwm_t_z02rpedlinhas.
  CALL FUNCTION 'ZWM_RFC_GET_LINHAS'
    TABLES
      linhas        = lt_linhas
    EXCEPTIONS
      exception     = 1
      error_message = 99
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
*  ELSE.
*    LOOP AT lt_linhas INTO DATA(ls_linha)
*        WHERE fevor IN s_linha.
*      READ TABLE lt_zwm083 INTO DATA(ls_zwm083)
*        WITH KEY cod_linha = ls_linha-cod_linha
*                 linha     = ls_linha-fevor.
*      IF sy-subrc IS NOT INITIAL.
*        APPEND VALUE #(
*        cod_linha = ls_linha-cod_linha
*        linha     = ls_linha-fevor
*        aufnr     = gc_empty
*        matnr     = gc_empty
*        datum     = gc_empty
*        inativo   = gc_empty
*        ) TO lt_zwm083.
*      ENDIF.
*    ENDLOOP.
  ENDIF.

  IF lt_linhas IS NOT INITIAL.
    SORT lt_linhas BY fevor.
** Excluir as ordens de produção que estejam encerradas técnicamente
    DATA lr_istat TYPE RANGE OF tj02t-istat.
    SELECT 'I'  AS sign,
           'EQ' AS option,
           istat AS low
        FROM tj02t
        INTO TABLE @lr_istat
      WHERE "spras EQ @sy-langu and
            ( txt04 EQ 'ENTE' OR
              txt04 EQ 'ENCE' ).
*I0046
*I0045

    DATA(lr_lety) = zcl_wm_utils=>get_t334e_letyp_range( ).

    SELECT zwm030~aufnr,
           zwm030~linha,
           zwm030~matnr,
           zwm030~prodp,
           zwm030~meins_p,
           zwm030~werks,
           zwm030~charg,
           zwm030~lgort,
           zwm030~lgnum,
           zwm030~data,
           zwm030~data_in,
           zwm030~data_out,
           zwm030~data_bloq,
           zwm030~data_des,
            aufk~objnr,
              mlgn~lety1,
                makt~maktx
      FROM zwm030
      INNER JOIN aufk
        ON aufk~aufnr EQ zwm030~aufnr
        INNER JOIN makt
          ON  makt~matnr EQ zwm030~matnr
          AND makt~spras EQ @sy-langu
          LEFT OUTER JOIN mlgn
            ON  mlgn~matnr EQ zwm030~matnr
            AND mlgn~lgnum EQ zwm030~lgnum
      INTO TABLE @DATA(lt_zwm030)
      FOR ALL ENTRIES IN @lt_linhas
    WHERE zwm030~linha    EQ @lt_linhas-fevor
      AND zwm030~werks    EQ @gc_renv
      AND zwm030~bloqueio NE @abap_true
      AND mlgn~lety1      IN @lr_lety
      AND aufk~objnr      NOT IN ( SELECT objnr
                                    FROM jest
                                    WHERE objnr EQ aufk~objnr
                                      AND stat  IN @lr_istat
                                      AND inact EQ ' ' ).

    LOOP AT lt_zwm030 INTO DATA(ls_zwm030).
      READ TABLE lt_linhas INTO DATA(ls_linha)
        WITH KEY fevor = ls_zwm030-linha
        BINARY SEARCH.

      APPEND INITIAL LINE TO gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>).

***      READ TABLE lt_zwm083 ASSIGNING FIELD-SYMBOL(<lfs_zwm083>)
***        WITH KEY "cod_linha = ls_zwm030-cod_linha
***                 linha     = ls_zwm030-linha
****                 matnr     = ls_zwm030-matnr.
***                 aufnr     = ls_zwm030-aufnr.
***      IF sy-subrc IS NOT INITIAL.
***        READ TABLE lt_zwm083 ASSIGNING <lfs_zwm083>
***          WITH KEY linha     = ls_zwm030-linha
****                   matnr     = space.
***                   aufnr     = space.
***      ENDIF.
*      IF sy-subrc IS NOT INITIAL.
*        APPEND INITIAL LINE TO lt_zwm083 ASSIGNING <lfs_zwm083>.
*      ENDIF.

      <lfs_alv>-cod_linha = ls_linha-cod_linha.
      <lfs_alv>-linha     = ls_linha-fevor.
      <lfs_alv>-linhat    = ls_linha-descricao.
      <lfs_alv>-aufnr     = ls_zwm030-aufnr.
      <lfs_alv>-matnr     = ls_zwm030-matnr.
      <lfs_alv>-maktx     = ls_zwm030-maktx.
      <lfs_alv>-datum     = ls_zwm030-data.
      <lfs_alv>-inativo_ic   = icon_okay.
      READ TABLE lt_zwm083 INTO DATA(ls_83)
              WITH KEY linha     = ls_zwm030-linha
                       aufnr     = ls_zwm030-aufnr.
      IF sy-subrc IS INITIAL.
        <lfs_alv>-inativo_ic   = icon_cancel.
      ENDIF.
    ENDLOOP.
  ENDIF.

*  IF p_semord EQ abap_false.
*    DELETE lt_zwm083 WHERE aufnr IS INITIAL.
*  ENDIF.

  SORT lt_zwm083 BY linha aufnr matnr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV
*&---------------------------------------------------------------------*
FORM alv .

  CALL METHOD cl_salv_table=>factory
*    EXPORTING
*      list_display = if_salv_c_bool_sap=>true    " ALV Displayed in List Mode
    IMPORTING
      r_salv_table = gr_table
    CHANGING
      t_table      = gt_alv.

*  DATA(lr_functions) = gr_table->get_functions( ).
*  lr_functions->set_all( abap_true ).

  gr_table->set_screen_status(
  pfstatus      =  'STT_MONITOR_NOITE'
  report        =  sy-repid
  set_functions = gr_table->c_functions_all ).

  "método de seleção linhas
  DATA lo_selections TYPE REF TO cl_salv_selections.
  lo_selections = gr_table->get_selections( ).
  lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
  "deixa salvar layout
  DATA: gr_layout TYPE REF TO cl_salv_layout.
  DATA: key       TYPE salv_s_layout_key.
  key-report = sy-repid.
  gr_layout = gr_table->get_layout( ).
  gr_layout->set_key( key ).
  gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

  DATA(lr_cols) = gr_table->get_columns( ).
  lr_cols->set_optimize( ).

  LOOP AT lr_cols->get( ) ASSIGNING FIELD-SYMBOL(<ls_column>).
    <ls_column>-r_column->set_f1_rollname( value = '' ).
    CASE <ls_column>-columnname.
      WHEN 'MANDT' OR 'ALTERADO' OR 'INATIVO'.
        <ls_column>-r_column->set_technical( ).
      WHEN 'LINHA'.
        PERFORM set_coltext USING <ls_column>-r_column 'Linha'.
      WHEN 'INATIVO_IC'.
        PERFORM set_coltext USING <ls_column>-r_column 'Desativar'.
        <ls_column>-r_column->set_alignment( if_salv_c_alignment=>centered ).
*      <LFS_ALV>-COD_LINHA = LS_LINHA-COD_LINHA.
*      <LFS_ALV>-LINHA     = LS_LINHA-FEVOR.
*      <LFS_ALV>-LINHAT    = LS_LINHA-DESCRICAO.
*      <LFS_ALV>-AUFNR     = LS_ZWM030-AUFNR.
*      <LFS_ALV>-MATNR     = LS_ZWM030-MATNR.
*      <LFS_ALV>-MAKTX     = LS_ZWM030-MAKTX.
*      <LFS_ALV>-DATUM     = LS_ZWM030-DATA.
*      <LFS_ALV>-INATIVO   = ICON_OKAY.
    ENDCASE.
  ENDLOOP.
*  PERFORM trata_colunas USING gr_table.

  "sort
  DATA: o_sorts   TYPE REF TO cl_salv_sorts.
  o_sorts = gr_table->get_sorts( ).
  o_sorts->add_sort( 'LINHA' ).
  o_sorts->add_sort( 'AUFNR' ).

  "event
  DATA(gr_events) = gr_table->get_event( ).
  CREATE OBJECT event_handler.
  SET HANDLER event_handler->on_user_command FOR gr_events.

  CALL METHOD gr_table->display.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ON_USER_COMMAND
*&---------------------------------------------------------------------*
FORM on_user_command  USING    p_ucomm.

*  DATA(lt_rows) = gr_table->get_selections( )->get_selected_rows( ).

  CASE p_ucomm.
    WHEN 'ATIVAR'.
      PERFORM ativa_desativa USING icon_okay.
    WHEN 'DESATIVAR'.
      PERFORM ativa_desativa USING icon_cancel.
    WHEN 'SALVAR'.
      PERFORM uc_salvar.
    WHEN 'CONFIGURA'.
      PERFORM uc_configura.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATIVA_DESATIVA
*&---------------------------------------------------------------------*
FORM ativa_desativa  USING    p_icon.

  DATA(lt_rows) = gr_table->get_selections( )->get_selected_rows( ).
  CHECK lt_rows IS NOT INITIAL.

  DATA ls_zwm083 TYPE zwm083.

  LOOP AT lt_rows INTO DATA(lv_row).
    READ TABLE gt_alv ASSIGNING FIELD-SYMBOL(<lfs_alv>) INDEX lv_row.
    IF <lfs_alv>-inativo_ic EQ p_icon.
      CONTINUE.
    ENDIF.
    DATA(lv_upd) = abap_true.
    <lfs_alv>-alterado   = abap_true.
    ls_zwm083 = CORRESPONDING #( <lfs_alv> ).

    <lfs_alv>-inativo_ic = p_icon.
    IF p_icon EQ icon_okay.
      <lfs_alv>-inativo = abap_false.
      DELETE zwm083 FROM ls_zwm083.
    ELSE.
      <lfs_alv>-inativo = abap_true.
      ls_zwm083-inativo = abap_true.
      MODIFY zwm083 FROM ls_zwm083.
    ENDIF.
  ENDLOOP.
  IF lv_upd EQ abap_true.
    COMMIT WORK.
  ENDIF.

  gr_table->get_selections( )->set_selected_rows( VALUE salv_t_row( ) ).

  gr_table->refresh(
*  EXPORTING
*    s_stable     =
*    refresh_mode = IF_SALV_C_REFRESH=>SOFT
  ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_COLTEXT
*&---------------------------------------------------------------------*
FORM set_coltext  USING    p_col TYPE REF TO cl_salv_column
                           p_text.

  p_col->set_long_text( CONV #( p_text ) ).
  p_col->set_short_text( CONV #( p_text ) ).
  p_col->set_medium_text( CONV #( p_text ) ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  UC_CONFIGURA
*&---------------------------------------------------------------------*
FORM uc_configura .

  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      action                       = 'S'
*     CORR_NUMBER                  = '          '
*     GENERATE_MAINT_TOOL_IF_MISSING       = ' '
*     SHOW_SELECTION_POPUP         = ' '
      view_name                    = 'ZWM082'
*     NO_WARNING_FOR_CLIENTINDEP   = ' '
*     RFC_DESTINATION_FOR_UPGRADE  = ' '
*     CLIENT_FOR_UPGRADE           = ' '
*     VARIANT_FOR_SELECTION        = ' '
*     COMPLEX_SELCONDS_USED        = ' '
*     CHECK_DDIC_MAINFLAG          = ' '
*     SUPPRESS_WA_POPUP            = ' '
* TABLES
*     DBA_SELLIST                  =
*     EXCL_CUA_FUNCT               =
    EXCEPTIONS
      client_reference             = 1
      foreign_lock                 = 2
      invalid_action               = 3
      no_clientindependent_auth    = 4
      no_database_function         = 5
      no_editor_function           = 6
      no_show_auth                 = 7
      no_tvdir_entry               = 8
      no_upd_auth                  = 9
      only_show_allowed            = 10
      system_failure               = 11
      unknown_field_in_dba_sellist = 12
      view_not_found               = 13
      maintenance_prohibited       = 14
      error_message                = 99
      OTHERS                       = 15.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
