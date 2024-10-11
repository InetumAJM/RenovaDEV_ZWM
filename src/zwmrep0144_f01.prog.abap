*&---------------------------------------------------------------------*
*&  Include           ZWMREP0144_F01
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
  DATA: lv_icon_name       TYPE iconname,
        lv_button_text(20) TYPE c,
        lv_quickinfo       LIKE smp_dyntxt-quickinfo,
        lv_icon_str(255)   TYPE c.

*  WRITE icon_tools TO sscrfields-functxt_01 AS ICON.

** Add Conf Button
**********************************************************************
*  lv_icon_name = 'ICON_TOOLS'.
*  lv_button_text = ''.
*
*  lv_quickinfo = 'Configurações'.
*
*  CALL FUNCTION 'ICON_CREATE'
*    EXPORTING
*      name       = lv_icon_name
*      text       = lv_button_text
*      info       = lv_quickinfo
*      add_stdinf = 'X'
*    IMPORTING
*      result     = lv_icon_str
*    EXCEPTIONS
*      OTHERS     = 0.
*
*  WRITE lv_icon_str TO sscrfields-functxt_01 AS ICON.
*MOVE text-123 TO sscrfields-functxt_01+5.

ENDFORM.                    " INITIALIZATION
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
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  DATA: lv_bwlvs  TYPE bwlvs.
  DATA: lv_datum  TYPE datum.
  DATA: ls_vekp   TYPE vekp.
  DATA: ls_vepo   TYPE vepo.

  DATA: lt_zwm076 TYPE TABLE OF zwm076 WITH HEADER LINE.
  DATA: lt_zwm084 TYPE TABLE OF zwm084 WITH HEADER LINE.
  DATA: lt_ltak   TYPE TABLE OF ltak   WITH HEADER LINE.
  DATA: lt_ltap   TYPE TABLE OF ltap   WITH HEADER LINE.


** Selecção de dados
**********************************************************************
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = p_lgnum
      i_processo  = 'OT_DUMMY'
      i_parametro = 'MOV_WM_WCS'
    IMPORTING
      e_valor     = lv_bwlvs
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  lv_datum = sy-datum - 10. "dias.

** OTs Antigas
  IF p_del IS NOT INITIAL.

    SELECT *
      FROM ltak INTO TABLE lt_ltak
      WHERE lgnum = p_lgnum
      AND   bwlvs = lv_bwlvs
      AND   kquit = ''.

    SORT lt_ltak BY lgnum tanum.

*    DELETE lt_ltak WHERE bdatu > lv_datum.

    IF lt_ltak[] IS NOT INITIAL.
      SELECT *
        FROM ltap INTO TABLE lt_ltap
        FOR ALL ENTRIES IN lt_ltak
        WHERE lgnum = lt_ltak-lgnum
        AND   tanum = lt_ltak-tanum.

      SORT lt_ltap BY tanum tapos.
    ENDIF.

    LOOP AT lt_ltak.
      CLEAR lt_zwm076.
      lt_zwm076-lgnum = lt_ltak-lgnum.
      lt_zwm076-tanum = lt_ltak-tanum.
      lt_zwm076-datum = lt_ltak-bdatu.
      lt_zwm076-uzeit = lt_ltak-bzeit.

      READ TABLE lt_ltap WITH KEY tanum = lt_ltak-tanum.
      IF sy-subrc = 0.
        lt_zwm076-exidv = lt_ltap-ablad.
        lt_zwm076-lgpla = lt_ltap-nlpla.
      ENDIF.

      APPEND lt_zwm076.
    ENDLOOP.

** Reprocessamento erros de entrada de produção no AUT
  ELSEIF p_ent_e IS NOT INITIAL.

    SELECT *
      FROM zwm076 INTO TABLE lt_zwm076
      WHERE lgnum = p_lgnum.

** Reprocessamento erros de entrada no AUT
  ELSEIF  p_ent_a IS NOT INITIAL.

    SELECT *
      FROM zwm084 INTO TABLE lt_zwm084
      WHERE lgnum = p_lgnum.

    LOOP AT lt_zwm084.
      MOVE-CORRESPONDING lt_zwm084 TO lt_zwm076.
      APPEND lt_zwm076.
    ENDLOOP.
  ENDIF.

** Filtros
  IF s_datum[] IS NOT INITIAL.
    DELETE lt_zwm076 WHERE datum NOT IN s_datum.
  ENDIF.

  IF s_exidv[] IS NOT INITIAL.
    DELETE lt_zwm076 WHERE exidv NOT IN s_exidv.
  ENDIF.

*** Dados para ALV
***********************************************************************
  SORT lt_zwm076 BY tanum.

  IF lt_zwm076[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      FOR ALL ENTRIES IN lt_zwm076
       WHERE lgnum = lt_zwm076-lgnum
       AND   tanum = lt_zwm076-tanum.

    SORT lt_ltap BY tanum tapos.
  ENDIF.

  LOOP AT lt_zwm076.

    CLEAR gs_alv_table.
    gs_alv_table-icon   = icon_led_red.
    gs_alv_table-tanum  = lt_zwm076-tanum.
    gs_alv_table-datum  = lt_zwm076-datum.
    gs_alv_table-uzeit  = lt_zwm076-uzeit.

    LOOP AT lt_ltap WHERE tanum = lt_zwm076-tanum.

      " SSCC 1
      IF gs_alv_table-exidv1 IS INITIAL.
        gs_alv_table-exidv1 = lt_ltap-nlenr.

        IF gs_alv_table-exidv1 IS INITIAL.
          gs_alv_table-exidv1 = lt_ltap-ablad.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_alv_table-exidv1
          IMPORTING
            output = gs_alv_table-exidv1.

        " SSCC 2
      ELSE.
        gs_alv_table-exidv2 = lt_ltap-nlenr.

        IF gs_alv_table-exidv2 IS INITIAL.
          gs_alv_table-exidv2 = lt_ltap-ablad.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_alv_table-exidv2
          IMPORTING
            output = gs_alv_table-exidv2.

        EXIT.
      ENDIF.
    ENDLOOP.

    " Obter dados da HU
    SELECT SINGLE *
      FROM vekp INTO ls_vekp
      WHERE exidv = gs_alv_table-exidv1.

    IF sy-subrc = 0.
      SELECT SINGLE *
        FROM vepo INTO ls_vepo
        WHERE venum = ls_vekp-venum.
    ENDIF.

    gs_alv_table-matnr = ls_vepo-matnr.
    gs_alv_table-charg = ls_vepo-charg.

    SELECT SINGLE maktx
      FROM makt INTO gs_alv_table-maktx
      WHERE matnr = ls_vepo-matnr
      AND   spras = sy-langu.


    gs_alv_table-menge   = ls_vepo-vemng.
    gs_alv_table-meins   = ls_vepo-vemeh.
    gs_alv_table-lgpla   = lt_zwm076-lgpla.
    gs_alv_table-tanum_a = lt_zwm076-tanum_a.
    gs_alv_table-error   = lt_zwm076-errormsg.
    gs_alv_table-docnum  = lt_zwm076-docnum.

    APPEND gs_alv_table TO gt_alv_table.

  ENDLOOP.

ENDFORM.                    " GET_DATA
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
  PERFORM get_fldcat  USING 'GT_ALV_TABLE' 'ZWM_009' gt_alv_table
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

      WHEN 'DOCNUM'.
        <ls_fcat>-no_out = 'X'.

      WHEN 'ICON'.
        <ls_fcat>-coltext   = 'Status'.
*        <ls_fcat>-hotspot  = 'X'.
        <ls_fcat>-icon      = 'X'.
        <ls_fcat>-outputlen = '4'.

      WHEN 'EXIDV1'.
        <ls_fcat>-coltext   = 'SSCC1'.
        <ls_fcat>-scrtext_s = 'SSCC1'.
        <ls_fcat>-scrtext_m = 'SSCC1'.
        <ls_fcat>-scrtext_l = 'SSCC1'.

      WHEN 'EXIDV2'.
        <ls_fcat>-coltext   = 'SSCC2'.
        <ls_fcat>-scrtext_s = 'SSCC2'.
        <ls_fcat>-scrtext_m = 'SSCC2'.
        <ls_fcat>-scrtext_l = 'SSCC2'.

      WHEN 'TANUM_A'.
        <ls_fcat>-coltext   = 'OT. Arrum.'.
        <ls_fcat>-scrtext_s = 'OT. Arrum.'.
        <ls_fcat>-scrtext_m = 'OT. Arrumação'.
        <ls_fcat>-scrtext_l = 'OT. Arrumação'.
        <ls_fcat>-hotspot   = 'X'.

        IF p_del IS NOT INITIAL.
          <ls_fcat>-no_out = 'X'.
        ENDIF.

      WHEN 'ERROR'.
        <ls_fcat>-coltext   = 'Erro'.
        <ls_fcat>-scrtext_s = 'Erro'.
        <ls_fcat>-scrtext_m = 'Erro'.
        <ls_fcat>-scrtext_l = 'Erro'.

*        if p_del is NOT INITIAL.
*        <ls_fcat>-no_out = 'X'.
*          endif.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " CHANGE_FLDCAT
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.

  DATA: lv_tabix TYPE sy-tabix.
  DATA: lv_num   TYPE char5.

** ALV
**********************************************************************
  IF p_job IS INITIAL.
    CALL SCREEN '0001'.

** JOB
**********************************************************************
  ELSE.

    CHECK gv_mod IS NOT INITIAL.

    IF gt_alv_table[] IS INITIAL.
      MESSAGE s000 WITH 'Sem dados para processsar!' DISPLAY LIKE 'W'.
      EXIT.
    ENDIF.

    CLEAR gv_count.

    LOOP AT gt_alv_table INTO gs_alv_table.
      lv_tabix = sy-tabix.

      PERFORM process_entry USING lv_tabix CHANGING gs_alv_table.
    ENDLOOP.

    " Envio de email de reprocessamento de entrada produção
    IF p_ent_e IS NOT INITIAL OR p_ent_a IS NOT INITIAL.
      PERFORM send_email_with_errors.
    ENDIF.

    IF gv_count IS NOT INITIAL.
      MESSAGE s000 WITH gv_count 'Paletes reprocessadas com sucesso!'.
    ENDIF.
  ENDIF.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_selection.

  DATA: lv_tabix         LIKE sy-tabix.
  DATA: ls_rows          TYPE lvc_s_row.
  DATA: ls_stable        TYPE lvc_s_stbl.
  DATA: lt_rows          TYPE lvc_t_row.
  DATA: lt_aufk          TYPE aufk          OCCURS 0 WITH HEADER LINE.
*  DATA: lt_orders        TYPE zroutynff_t02 OCCURS 0 WITH HEADER LINE.
*  DATA: lt_zroutynff_t02 TYPE zroutynff_t02 OCCURS 0 WITH HEADER LINE.

** Get orders
**********************************************************************
  CALL METHOD gcl_alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  CLEAR gv_count.

  LOOP AT lt_rows INTO ls_rows.
    READ TABLE gt_alv_table INTO gs_alv_table INDEX ls_rows-index.
    CHECK sy-subrc = 0.

    lv_tabix = sy-tabix.

    PERFORM process_entry USING lv_tabix CHANGING gs_alv_table.
  ENDLOOP.

  IF sy-subrc = 0.
    ls_stable-row = 'X'.

    CALL METHOD gcl_alv_grid->refresh_table_display
      EXPORTING
        is_stable = ls_stable.

    IF gv_count IS NOT INITIAL.
      MESSAGE s000 WITH gv_count 'Paletes reprocessadas com sucesso!'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXIT_COMMAND_0101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_command_0101.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROC_REG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_ALV_TABLE  text
*----------------------------------------------------------------------*
FORM proc_reg CHANGING ps_alv_table TYPE st_alv_table.

  DATA: lv_retorno TYPE numc2,
        lv_to      TYPE tanum,
        lv_msg     TYPE zwm_msg,
        lv_mesa    TYPE char14.

  DATA: lv_bwlvs      TYPE bwlvs.
  DATA: lv_tanum      TYPE tanum.
  DATA: ls_ltak       TYPE ltak.
  DATA: ls_lqua       TYPE lqua.
  DATA: ls_zwm013     TYPE zwm013.
  DATA: ls_zwm077     TYPE zwm077.
  DATA: ls_ltak_vb    TYPE ltak_vb.

  DATA: lt_ltap       TYPE TABLE OF ltap       WITH HEADER LINE.
  DATA: lt_ltap_creat TYPE TABLE OF ltap_creat WITH HEADER LINE.
  DATA: lt_ltap_cancl TYPE TABLE OF ltap_cancl WITH HEADER LINE.
  DATA: lt_zwm077     TYPE TABLE OF zwm077     WITH HEADER LINE.
  DATA: lt_ltap_vb    TYPE TABLE OF ltap_vb    WITH HEADER LINE.

** Parametros
**********************************************************************
** Movimento - Não gera IDOC - "943"
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = p_lgnum
      i_processo  = 'WCS'
      i_parametro = 'MOV_WM_ENT_PROD'
    IMPORTING
      e_valor     = lv_bwlvs
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

** Estorno de OTs não processadas
**********************************************************************
  IF p_del = 'X'.

    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = p_lgnum
      AND   tanum = ps_alv_table-tanum.

    CHECK sy-subrc = 0.

    LOOP AT lt_ltap.
      CLEAR lt_ltap_cancl.
      MOVE lt_ltap-tanum TO lt_ltap_cancl-tanum.
      MOVE lt_ltap-tapos TO lt_ltap_cancl-tapos.
      APPEND lt_ltap_cancl.
    ENDLOOP.

    CALL FUNCTION 'L_TO_CANCEL'
      EXPORTING
        i_lgnum       = lt_ltap-lgnum
        i_tanum       = lt_ltap-tanum
        i_commit_work = 'X'
        i_subst       = 'X'
      TABLES
        t_ltap_cancl  = lt_ltap_cancl
      EXCEPTIONS
        error_message = 13
        OTHERS        = 14.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO ps_alv_table-error
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      CLEAR ps_alv_table.
    ENDIF.

** Reprocessamento OTs de entrada de produção
**********************************************************************
  ELSEIF p_ent_e = 'X'.

** Falhou OT final de entrada no AUT
    IF ps_alv_table-tanum_a IS NOT INITIAL.

      SELECT *
        FROM ltap INTO TABLE lt_ltap
        WHERE lgnum = p_lgnum
        AND   nlenr = ps_alv_table-exidv1.

      READ TABLE lt_ltap WITH KEY vlpla = 'EAU'
                                  nlpla = 'AUT'.
      IF sy-subrc = 0.
        CLEAR ps_alv_table.
        EXIT.
      ENDIF.

      SELECT *
        FROM ltap INTO TABLE lt_ltap
        WHERE lgnum = p_lgnum
        AND   tanum = ps_alv_table-tanum_a.

      " Criar ot com confirmação imediata (não gera idoc)
      LOOP AT lt_ltap.
        CLEAR lt_ltap_creat.
        lt_ltap_creat-werks = lt_ltap-werks.
        lt_ltap_creat-lgort = lt_ltap-lgort.
        lt_ltap_creat-matnr = lt_ltap-matnr.
        lt_ltap_creat-charg = lt_ltap-charg.
        lt_ltap_creat-anfme = lt_ltap-nsola.
        lt_ltap_creat-altme = lt_ltap-altme.
        lt_ltap_creat-vltyp = lt_ltap-nltyp.
        lt_ltap_creat-vlpla = lt_ltap-nlpla.
        lt_ltap_creat-vlenr = lt_ltap-nlenr.
        lt_ltap_creat-letyp = lt_ltap-letyp.
        APPEND lt_ltap_creat.
      ENDLOOP.

      CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
        EXPORTING
          i_lgnum       = p_lgnum
          i_bwlvs       = lv_bwlvs
        IMPORTING
          e_tanum       = lv_tanum
        TABLES
          t_ltap_creat  = lt_ltap_creat
        EXCEPTIONS
          error_message = 99.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO ps_alv_table-error
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        CLEAR ps_alv_table.
      ENDIF.

** Falhou OT arrumação + OT final de entrdada no AUT com confirmação
    ELSE.

      SELECT SINGLE *
        FROM zwm013 INTO ls_zwm013
        WHERE armazem = p_lgnum
        AND sscc      = ps_alv_table-exidv1.

      IF sy-subrc <>  0.
        CLEAR ls_zwm013.

        ls_zwm013-armazem     = p_lgnum.
        ls_zwm013-sscc        = ps_alv_table-exidv1.
        ls_zwm013-bloqueado   = 'X'.

        SELECT SINGLE lety1
          FROM mlgn INTO ls_zwm013-tipo_palete
          WHERE matnr = ps_alv_table-matnr
          AND   lgnum = p_lgnum.

        INSERT zwm013 FROM ls_zwm013.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.

      CONCATENATE 'PRO' ps_alv_table-lgpla INTO lv_mesa SEPARATED BY space.

*      lv_mesa = 'PRO AUT'.

      CLEAR lv_retorno.

      CALL FUNCTION 'ZWM_RFC_GET_MESA'
        EXPORTING
          i_sscc  = ps_alv_table-exidv1
          i_mesa  = lv_mesa
        IMPORTING
          retorno = lv_retorno.

      CALL FUNCTION 'ZWM_RFC_GET_TO'
        EXPORTING
          i_sscc  = ps_alv_table-exidv1
          mesa    = lv_mesa
        IMPORTING
          e_to    = lv_to
          retorno = lv_retorno
          msg     = lv_msg.

      IF lv_retorno <> '00'.
        ps_alv_table-error = lv_msg.

      ELSE.

        " Armazém Convencional
        IF ps_alv_table-lgpla <> 'AUT'.

          GET TIME.

          CLEAR ls_zwm077.
          ls_zwm077-lgnum  = p_lgnum.
          ls_zwm077-tanum  = lv_to.
          ls_zwm077-docnum = ps_alv_table-docnum.
          ls_zwm077-lgpla  = ps_alv_table-lgpla.
          ls_zwm077-rdatu  = sy-datum.
          ls_zwm077-rzeit  = sy-uzeit.
          ls_zwm077-rname  = sy-uname.
          ls_zwm077-exidv  = ps_alv_table-exidv1.

          APPEND ls_zwm077 TO lt_zwm077.

          IF lt_zwm077[] IS NOT INITIAL.
            MODIFY zwm077 FROM TABLE lt_zwm077.
            IF sy-subrc = 0.
              COMMIT WORK AND WAIT.
            ENDIF.
          ENDIF.

          CLEAR ps_alv_table.

          " Armazém Automático
        ELSE.

          DO 5 TIMES.

            " No Exit da confirmação da OT
            " Validar se criou e confirmou OT do EAU -> AUT
            SELECT SINGLE *
              FROM lqua INTO ls_lqua
              WHERE lgnum = p_lgnum
              AND   lgtyp = 'AUT'
              AND   lgpla = 'AUT'
              AND   lenum = ps_alv_table-exidv1.
            IF sy-subrc = 0.
              EXIT.
            ENDIF.
            WAIT UP TO 1 SECONDS.
          ENDDO.

          IF ls_lqua IS NOT INITIAL.
            CLEAR ps_alv_table.
          ELSE.
            ps_alv_table-tanum_a = lv_to.
            ps_alv_table-error = 'OT EAU -> AUT - Não foi criada e confirmada'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


** Reprocessamento OTs de entrada automático
**********************************************************************
  ELSEIF p_ent_a = 'X'.

    SELECT SINGLE *
      FROM ltak INTO ls_ltak
      WHERE lgnum = p_lgnum
      AND   tanum = ps_alv_table-tanum.

    MOVE-CORRESPONDING ls_ltak TO ls_ltak_vb.

    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = p_lgnum
      AND   tanum = ps_alv_table-tanum.

    SORT lt_ltap.

    LOOP AT lt_ltap.
      CLEAR lt_ltap_vb.
      MOVE-CORRESPONDING lt_ltap TO lt_ltap_vb.
      APPEND lt_ltap_vb.
    ENDLOOP.

    CLEAR lv_tanum.
    CALL FUNCTION 'ZWM_CHECK_TO_CONFIRM_WCS'
      EXPORTING
        i_ltak_vb = ls_ltak_vb
      IMPORTING
        e_tanum   = lv_tanum
      TABLES
        t_ltap_vb = lt_ltap_vb.

    SELECT SINGLE errormsg
         FROM zwm084 INTO ps_alv_table-error
         WHERE lgnum = p_lgnum
         AND   tanum = ps_alv_table-tanum.

    IF sy-subrc <> 0.
      CLEAR ps_alv_table.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL_WITH_ERRORS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email_with_errors.

  DATA: util_emissor  LIKE t001s-usnam.
  DATA: send_request  TYPE REF TO cl_bcs.
  DATA: document      TYPE REF TO cl_document_bcs.
  DATA: sender        TYPE REF TO cl_sapuser_bcs.
  DATA: lo_sender     TYPE REF TO if_sender_bcs VALUE IS INITIAL.
  DATA: recipient     TYPE REF TO if_recipient_bcs.
  DATA: bcs_exception TYPE REF TO cx_bcs.
  DATA: t_hex         TYPE solix_tab.
  DATA: html_string   TYPE string.
  DATA: xhtml_string  TYPE xstring.
  DATA: corpo_html    TYPE string.
  DATA: status_mail   TYPE bcs_stml.
  DATA: lv_subject    TYPE so_obj_des.
  DATA: lv_email      TYPE adr6-smtp_addr.
  DATA: lv_parvw      TYPE  parvw.
  DATA: lv_tabix      LIKE sy-tabix.
  DATA: lv_text       TYPE char100.
  DATA: ls_adr6       TYPE adr6.
  DATA: ls_zwm076     TYPE zwm076.
  DATA: ls_zwm084     TYPE zwm084.
  DATA: ls_return     TYPE bdcmsgcoll.
  DATA: lt_alv_table  TYPE TABLE OF st_alv_table WITH HEADER LINE.

** Obter dados
**********************************************************************
  LOOP AT gt_alv_table INTO gs_alv_table .

    IF p_ent_e = 'X'.
      SELECT SINGLE *
        FROM zwm076 INTO ls_zwm076
        WHERE lgnum = p_lgnum
        AND   exidv = gs_alv_table-exidv1.

      CHECK ls_zwm076-email IS INITIAL.

      UPDATE zwm076 SET email   = 'X'
                    WHERE lgnum = p_lgnum
                    AND   exidv = gs_alv_table-exidv1.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.

    ELSEIF p_ent_a = 'X'.

      SELECT SINGLE *
        FROM zwm084 INTO ls_zwm084
        WHERE lgnum = p_lgnum
        AND   exidv = gs_alv_table-tanum.

      CHECK ls_zwm084-email IS INITIAL.

      UPDATE zwm084 SET email   = 'X'
                    WHERE lgnum = p_lgnum
                    AND   tanum = gs_alv_table-tanum.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    APPEND gs_alv_table TO lt_alv_table.
  ENDLOOP.

  CHECK lt_alv_table[] IS NOT INITIAL.

** Emissor
**********************************************************************
  TRY.
      send_request = cl_bcs=>create_persistent( ).

*      util_emissor = sy-uname.
*
*      sender = cl_sapuser_bcs=>create( util_emissor ).
*
**     Adiciona emissor
*      CALL METHOD send_request->set_sender
*        EXPORTING
*          i_sender = sender.

      lo_sender = cl_cam_address_bcs=>create_internet_address( 'logistica@renova.pt' ).

      send_request->set_sender(
                  EXPORTING
                    i_sender = lo_sender ).

** Assunto e Corpo do email
**********************************************************************

** Assunto
*      CONCATENATE sy-datum+6(2) '-' sy-datum+4(2) '-' sy-datum(4) '/' sy-uzeit(2) ':' sy-uzeit+2(2) INTO lv_subject.
      IF p_ent_e IS NOT INITIAL.
        lv_subject = 'Erros Entrada de OTs de Produção no Automático'.
      ELSEIF p_ent_a IS NOT INITIAL.
        lv_subject = 'Erros Entrada de OTs no Automático'.
      ENDIF.

** Corpo email
      CONCATENATE corpo_html '<html>'
      INTO corpo_html SEPARATED BY space.

      CONCATENATE corpo_html 'ERROS:' '<br>' INTO corpo_html SEPARATED BY space.

      LOOP AT lt_alv_table.
        lv_tabix = sy-tabix.

        CONCATENATE corpo_html lt_alv_table-exidv1 '-' lt_alv_table-tanum '-'
         lt_alv_table-error '<br>'
        INTO corpo_html SEPARATED BY space.
      ENDLOOP.

      CONCATENATE corpo_html '</html>'
      INTO corpo_html SEPARATED BY space.

      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = corpo_html
        IMPORTING
          buffer = xhtml_string
        EXCEPTIONS
          failed = 1
          OTHERS = 2.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = xhtml_string
        TABLES
          binary_tab = t_hex.

      document = cl_document_bcs=>create_document(
      i_type    = 'HTM'
      i_hex     = t_hex
      i_subject = lv_subject ).

      CALL METHOD send_request->set_document( document ).

** Destinatário
**********************************************************************
*      IF i_email IS NOT INITIAL.
*
*        recipient = cl_cam_address_bcs=>create_internet_address(
*            i_address_string = i_email ).
*
*        CALL METHOD send_request->add_recipient
*          EXPORTING
*            i_recipient = recipient
*            i_express   = 'X'.
*
*      ELSE.
      recipient = cl_distributionlist_bcs=>getu_persistent( i_dliname = gc_dliname
                                                            i_private = space ).
      CALL METHOD send_request->add_recipient
        EXPORTING
          i_recipient = recipient
          i_express   = 'X'.
*      ENDIF.

** Enviar email
**********************************************************************
      status_mail = 'N'.

      CALL METHOD send_request->set_status_attributes
        EXPORTING
          i_requested_status = status_mail
          i_status_mail      = status_mail.

      " Envio imediato
      send_request->set_send_immediately( 'X' ).

      " Envia mail
      CALL METHOD send_request->send( ).

    CATCH cx_bcs INTO bcs_exception.
      CLEAR ls_return.
      ls_return-msgid  = bcs_exception->msgid.
      ls_return-msgnr  = bcs_exception->msgno.
      ls_return-msgtyp = bcs_exception->msgty.
      ls_return-msgv1  = bcs_exception->msgv1.
      ls_return-msgv2  = bcs_exception->msgv2.
      ls_return-msgv3  = bcs_exception->msgv3.
      ls_return-msgv4  = bcs_exception->msgv4.

      MESSAGE ID ls_return-msgid TYPE 'I' NUMBER ls_return-msgnr INTO lv_text
      WITH ls_return-msgv1 ls_return-msgv2 ls_return-msgv3 ls_return-msgv4.

      WRITE : / lv_text.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DELETE_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_selection.

  DATA: lv_rc     TYPE c.
  DATA: lv_tabix  LIKE sy-tabix.
  DATA: ls_rows   TYPE lvc_s_row.
  DATA: ls_stable TYPE lvc_s_stbl.
  DATA: lt_rows   TYPE lvc_t_row.

** Get orders
**********************************************************************
  CALL METHOD gcl_alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows.

  IF lt_rows[] IS NOT INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        defaultoption  = 'N'
        textline1      = 'Deseja eliminar registos?'
        titel          = 'Eliminar'
        cancel_display = 'N'
      IMPORTING
        answer         = lv_rc.

    CHECK lv_rc EQ 'J'.
  ENDIF.

  LOOP AT lt_rows INTO ls_rows.
    READ TABLE gt_alv_table INTO gs_alv_table INDEX ls_rows-index.
    CHECK sy-subrc = 0.

*    CHECK gs_alv_table-process <> 'S'.

    DELETE gt_alv_table INDEX sy-tabix.

*    DELETE FROM zwmrft096 WHERE timestamp = gs_alv_table-timestamp
*                          AND   lgnum     = gs_alv_table-lgnum
*                          AND   exidv     = gs_alv_table-exidv.
*    IF sy-subrc = 0.
*      COMMIT WORK AND WAIT.
*    ENDIF.

  ENDLOOP.

  IF sy-subrc = 0.
    ls_stable-row = 'X'.

    CALL METHOD gcl_alv_grid->refresh_table_display
      EXPORTING
        is_stable = ls_stable.

    MESSAGE s000(0k) WITH 'Registos Eliminados'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ENQUEUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enqueue.

  DATA: lv_key      TYPE keyword-keyword.
  DATA: lv_uname    TYPE uname.

** Criar bloqueio
**********************************************************************
  MOVE 'ENT_AUT_ZWMREP0144' TO lv_key.

  CLEAR gv_mod.

  DO 10 TIMES.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        keyword_       = lv_key
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      gv_mod = 'X'.
      EXIT.
    ENDIF.

    WAIT UP TO 1 SECONDS.
  ENDDO.

  IF gv_mod IS INITIAL.
    MESSAGE i366 WITH sy-msgv1 sy-repid DISPLAY LIKE 'W'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DEQUEUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dequeue.

  DATA: lv_key TYPE keyword-keyword.

  MOVE 'ENT_AUT_ZWMREP0144' TO lv_key.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      keyword_ = lv_key.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_TABIX  text
*      <--P_GS_ALV_TABLE  text
*----------------------------------------------------------------------*
FORM process_entry  USING    pv_tabix     TYPE syst_tabix
                    CHANGING ps_alv_table TYPE st_alv_table .

  DATA: lv_exidv TYPE exidv.
  DATA: lv_tanum TYPE tanum.

  lv_exidv = ps_alv_table-exidv1.
  lv_tanum = ps_alv_table-tanum.

  PERFORM proc_reg CHANGING ps_alv_table.

  IF ps_alv_table IS INITIAL.
    DELETE gt_alv_table INDEX pv_tabix.

    IF p_ent_e = 'X'.
      DELETE FROM zwm076 WHERE lgnum = p_lgnum
                         AND   exidv = lv_exidv.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ELSEIF p_ent_a = 'X'.
      DELETE FROM zwm084 WHERE lgnum = p_lgnum
                         AND   tanum = lv_tanum.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    gv_count = gv_count + 1.

  ELSE.
    MODIFY gt_alv_table FROM ps_alv_table INDEX pv_tabix.

    IF p_ent_e = 'X'.
      UPDATE zwm076 SET tanum_a  = ps_alv_table-tanum_a
                        errormsg = ps_alv_table-error
                    WHERE lgnum = p_lgnum
                    AND   exidv = lv_exidv.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
