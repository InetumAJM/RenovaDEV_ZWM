*&---------------------------------------------------------------------*
*&  Include           ZWMREP0145_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init .

*  DATA: lv_valor LIKE zwm001-valor.
*  DATA: lv_num_dias TYPE i.
*
*  CLEAR lv_valor.
*  PERFORM get_parameter
*          USING p_lgnum
*                'MONITOR_ENTREGAS'
*                'NUM_DIAS'
*                lv_valor.
*
*  MOVE lv_valor TO lv_num_dias.
*
*  CLEAR s_datum.
*  s_datum-sign = 'I'.
*  s_datum-option = 'GE'.
*  s_datum-low = sy-datum - lv_num_dias.
*  APPEND s_datum.


ENDFORM.                    " inicializacao
*&---------------------------------------------------------------------*
*&      Form  GET_PARAMETRIZACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_parameters.

  DATA: lv_valor LIKE zwm001-valor.

** Centro
  CLEAR lv_valor.
  PERFORM get_parameter
          USING p_lgnum
                'GERAL'
                'PLANT'
                gv_werks.

  IF gv_werks IS INITIAL.
    MESSAGE e000(zwmmsg001) WITH 'Falta parametro centro'.
  ENDIF.

** Depósito
  CLEAR lv_valor.
  PERFORM get_parameter
          USING p_lgnum
                'GERAL'
                'LGORT'
                gv_lgort.

  IF gv_lgort IS INITIAL.
    MESSAGE e000(zwmmsg001) WITH 'Falta parametro depósito'.
  ENDIF.

** Movimento NT
  CLEAR lv_valor.
  PERFORM get_parameter
          USING p_lgnum
                'PALETIZACAO_ESPECIAL'
                'MOV_WM_NT'
                gv_bwlvs.

  IF gv_bwlvs IS INITIAL.
    MESSAGE e000(zwmmsg001) WITH 'Falta parametro movimento NT'.
  ENDIF.

** Capacidade Máxima BPE
  CLEAR lv_valor.
  PERFORM get_parameter
          USING p_lgnum
                'PALETIZACAO_ESPECIAL'
                'CAP_MAX_PAL_BPE'
                gv_max_cap.

  IF gv_max_cap IS INITIAL.
    MESSAGE e000(zwmmsg001) WITH 'Falta parametro cap. máx. BPE'.
  ENDIF.


ENDFORM.                    " GET_PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  DATA: lv_tabix   LIKE sy-tabix.
  DATA: lv_subrc   LIKE sy-subrc.
  DATA: lv_datum   TYPE datum.
  DATA: lv_status  TYPE char4.
  DATA: lv_menge   TYPE menge_d.
  DATA: lv_2step   TYPE flag.
  DATA: lv_2spart  TYPE flag.

  DATA: ls_alv     TYPE st_alv.

  DATA: lt_t311     TYPE TABLE OF t311   WITH HEADER LINE.
  DATA: lt_kna1     TYPE TABLE OF kna1   WITH HEADER LINE.
  DATA: lt_t311a    TYPE TABLE OF t311a  WITH HEADER LINE.
  DATA: lt_likp     TYPE TABLE OF likp   WITH HEADER LINE.
  DATA: lt_lips     TYPE TABLE OF lips   WITH HEADER LINE.
  DATA: lt_ltbk     TYPE TABLE OF ltbk   WITH HEADER LINE.
  DATA: lt_ltbp     TYPE TABLE OF ltbp   WITH HEADER LINE.
  DATA: lt_ltak     TYPE TABLE OF ltak   WITH HEADER LINE.
  DATA: lt_ltak_exp TYPE TABLE OF ltak   WITH HEADER LINE.
  DATA: lt_ltap     TYPE TABLE OF ltap   WITH HEADER LINE.
  DATA: lt_zwm031   TYPE TABLE OF zwm031 WITH HEADER LINE.
  DATA: lt_makt     TYPE TABLE OF makt   WITH HEADER LINE.
  DATA: lt_alv      TYPE TABLE OF st_alv WITH HEADER LINE.
  DATA: lt_alv_aux  TYPE TABLE OF st_alv WITH HEADER LINE.
  DATA: lt_marm     TYPE TABLE OF marm   WITH HEADER LINE.

** Obter dados
**********************************************************************
  REFRESH gt_alv.
  REFRESH gt_alv_table.

  CLEAR flag_tree.

** Grupos
  READ TABLE s_datum INDEX 1.

  lv_datum = s_datum-low - 7.

  SELECT *
    FROM t311 INTO TABLE lt_t311
    WHERE lgnum = p_lgnum  AND
          refnr IN s_refnr AND
          refnt IN s_refnt AND
          datum >= lv_datum.

  LOOP AT lt_t311.
    lv_tabix = sy-tabix.

    lt_t311-drdat      = sy-datum.
    lt_t311-drdat+2(2) = lt_t311-refnt+6(2).
    lt_t311-drdat+4(2) = lt_t311-refnt+3(2).
    lt_t311-drdat+6(2) = lt_t311-refnt(2).

    MODIFY lt_t311 INDEX lv_tabix TRANSPORTING drdat.
  ENDLOOP.

  DELETE lt_t311 WHERE drdat NOT IN s_datum.

  CHECK lt_t311[] IS NOT INITIAL.

** Remessas
  IF NOT lt_t311[] IS INITIAL.
    SELECT *
      FROM t311a INTO TABLE lt_t311a
      FOR ALL ENTRIES IN lt_t311
      WHERE lgnum = lt_t311-lgnum
      AND   refnr = lt_t311-refnr.
  ENDIF.

  IF lt_t311a[] IS NOT INITIAL.
    SELECT *
      FROM likp INTO TABLE lt_likp
      FOR ALL ENTRIES IN lt_t311a
      WHERE vbeln = lt_t311a-rbnum.
  ENDIF.

  IF lt_likp[] IS NOT INITIAL.
    SELECT *
      FROM lips INTO TABLE lt_lips
      FOR ALL ENTRIES IN lt_likp
      WHERE vbeln = lt_likp-vbeln.

    DELETE lt_lips WHERE lfimg <= 0.
  ENDIF.

** Paletização Especial
  IF lt_likp[] IS NOT INITIAL.
    SELECT *
      FROM zwm031 INTO TABLE lt_zwm031
      FOR ALL ENTRIES IN lt_likp
      WHERE lgnum = p_lgnum
      AND   kunnr = lt_likp-kunnr.
  ENDIF.

** NTs
  IF lt_t311[] IS NOT INITIAL.
    SELECT *
      FROM ltbk INTO TABLE lt_ltbk
      FOR ALL ENTRIES IN lt_t311
      WHERE lgnum = p_lgnum
      AND   betyp = 'S'
      AND   benum = lt_t311-refnr.
  ENDIF.

  IF lt_ltbk[] IS NOT INITIAL.
    SELECT *
     FROM ltbp INTO TABLE lt_ltbp
      FOR ALL ENTRIES IN lt_ltbk
      WHERE lgnum = p_lgnum
      AND   tbnum = lt_ltbk-tbnum.
  ENDIF.

** OTs
  IF lt_ltbk[] IS NOT INITIAL.
    SELECT *
      FROM ltak INTO TABLE lt_ltak
      FOR ALL ENTRIES IN lt_ltbk
      WHERE lgnum = p_lgnum
      AND   tbnum = lt_ltbk-tbnum.
  ENDIF.

  IF lt_ltak[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      FOR ALL ENTRIES IN lt_ltak
      WHERE lgnum = p_lgnum
      AND   tanum = lt_ltak-tanum.

    DELETE lt_ltap WHERE vorga = 'ST' OR vorga = 'SL'. "Eliminar extornadas
  ENDIF.

  LOOP AT lt_ltak.
    lv_tabix = sy-tabix.

    READ TABLE lt_ltap WITH KEY tanum = lt_ltak-tanum.
    CHECK sy-subrc <> 0.

    DELETE lt_ltak INDEX lv_tabix.

  ENDLOOP.

** OTs Expedição
  IF lt_t311[] IS NOT INITIAL.
    SELECT *
      FROM ltak INTO TABLE lt_ltak_exp
      FOR ALL ENTRIES IN lt_t311
      WHERE lgnum = p_lgnum
      AND   refnr = lt_t311-refnr.
  ENDIF.


** Clientes
  IF lt_likp[] IS NOT INITIAL.
    SELECT *
      FROM kna1 INTO TABLE lt_kna1
      FOR ALL ENTRIES IN lt_likp
      WHERE kunnr = lt_likp-kunnr.
  ENDIF.

** Descrição Material
  IF lt_lips[] IS NOT INITIAL.
    SELECT *
      FROM makt INTO TABLE lt_makt
      FOR ALL ENTRIES IN lt_lips
      WHERE matnr = lt_lips-matnr
      AND   spras = sy-langu.
  ENDIF.

** Unidade PAL
  IF lt_makt[] IS NOT INITIAL.
    SELECT *
      FROM marm INTO TABLE lt_marm
      FOR ALL ENTRIES IN lt_makt
      WHERE matnr = lt_makt-matnr
      AND   meinh = 'PAL'.
  ENDIF.

** Listagem ALV
**********************************************************************
  SORT lt_t311   BY drdat.
  SORT lt_t311a  BY refnr rbnum.
  SORT lt_likp   BY vbeln.
  SORT lt_lips   BY vbeln matnr.
  SORT lt_ltbk   BY lznum.
  SORT lt_makt   BY matnr.
  SORT lt_zwm031 BY kunnr matnr.
  SORT lt_marm   BY matnr.

** Grupos
  LOOP AT lt_t311.

    " Só permite Criar NT se não tiver OTs de Expedição Criadas
    READ TABLE lt_ltbk WITH KEY benum = lt_t311-refnr.
    IF sy-subrc <> 0.
      READ TABLE lt_ltak_exp WITH KEY refnr = lt_t311-refnr.
      CHECK sy-subrc <> 0.
    ENDIF.

    CLEAR: lv_2step, lv_2spart.

    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = lt_t311-lgnum
        i_refnr  = lt_t311-refnr
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    LOOP AT lt_t311a WHERE refnr = lt_t311-refnr.

      " Cliente
      READ TABLE lt_likp WITH KEY vbeln = lt_t311a-rbnum.
      CHECK sy-subrc = 0.

      LOOP AT lt_lips WHERE vbeln = lt_likp-vbeln.

        " Paletização Especial
        READ TABLE lt_zwm031 WITH KEY kunnr = lt_likp-kunnr
                                      matnr = lt_lips-matnr.

        CHECK sy-subrc = 0.

        " Validar se não é palete Remontada ????
        CHECK lt_zwm031-remontada IS INITIAL.

        READ TABLE lt_kna1 WITH KEY kunnr = lt_likp-kunnr.

        READ TABLE lt_makt WITH KEY matnr = lt_lips-matnr.

*        READ TABLE lt_marm WITH KEY matnr = lt_lips-matnr.

        CLEAR ls_alv.
        ls_alv-refnr  = lt_t311-refnr.
        ls_alv-refnt  = lt_t311-refnt.
        ls_alv-drdat  = lt_t311-drdat.
        ls_alv-kunnr  = lt_zwm031-kunnr.

        " Picking à Remessa
        IF lv_2step IS INITIAL.

          CONCATENATE lt_lips-vbeln '#' lt_lips-posnr INTO ls_alv-lznum.

          " Picking à Remessa agrupado
        ELSEIF lv_2spart = 'X'.
          ls_alv-lznum = lt_likp-vbeln.

          " Picking ao Grupo
        ELSE.
          ls_alv-lznum = lt_zwm031-kunnr.
        ENDIF.

        ls_alv-name1  = lt_kna1-name1.
        ls_alv-matnr  = lt_lips-matnr.
        ls_alv-maktx  = lt_makt-maktx.
        ls_alv-menge  = lt_lips-lfimg.
        ls_alv-meins  = lt_lips-vrkme.

*        ls_alv-pal    = ls_alv-menge / lt_marm-umrez.
*        ls_alv-pal_pe = ls_alv-menge / lt_zwm031-unporpal.

        COLLECT ls_alv INTO lt_alv.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

** Validar quantidade de Paletização
  SORT lt_alv BY refnr kunnr matnr.

  LOOP AT lt_alv.

    lv_tabix = sy-tabix.

    READ TABLE lt_zwm031 WITH KEY kunnr = lt_alv-kunnr
                                  matnr = lt_alv-matnr.

    READ TABLE lt_marm WITH KEY matnr = lt_alv-matnr.
    IF sy-subrc <> 0.
      MESSAGE e000(zwmmsg001) WITH 'Material' lt_alv-matnr 'sem unidade PAL definido'.
      EXIT.
    ENDIF.

    " Paletes
    lv_menge = lt_alv-menge / lt_marm-umrez.
    lv_menge = floor( lv_menge ).

    IF lv_menge = 0.
      DELETE lt_alv INDEX lv_tabix.
      CONTINUE.
    ENDIF.

    lt_alv-menge = lv_menge * lt_marm-umrez.
    lt_alv-pal = lv_menge.

    " Paletes - Paletização Especial
    lv_menge = lt_alv-menge / lt_zwm031-unporpal.

    lv_menge = ceil( lv_menge ).
    lt_alv-pal_pe = lv_menge.

    MODIFY lt_alv INDEX lv_tabix TRANSPORTING menge pal pal_pe.
  ENDLOOP.

** NTs/OTs
  SORT lt_ltbk BY benum.

  LOOP AT lt_alv.

    CLEAR ls_alv.
    ls_alv = lt_alv.

    " Validar NTs
    LOOP AT lt_ltbk WHERE benum = lt_alv-refnr AND
                          lznum = lt_alv-lznum.

      LOOP AT lt_ltbp WHERE tbnum = lt_ltbk-tbnum
                      AND   matnr = lt_alv-matnr.

        " Validar OTs
        LOOP AT lt_ltak WHERE tbnum = lt_ltbp-tbnum.

          ls_alv-tbnum  = lt_ltbp-tbnum.
          ls_alv-tanum  = lt_ltak-tanum.

          IF lt_ltbk-statu = 'E'.
            ls_alv-st_nt = icon_green_light.
          ELSE.
            ls_alv-st_nt = icon_yellow_light.
          ENDIF.

          IF lt_ltak-kquit = 'X'.
            ls_alv-st_ot = icon_green_light.
          ELSEIF lt_ltak-kvqui = 'X'.
            ls_alv-st_ot = icon_yellow_light.
          ELSE.
            ls_alv-st_ot = icon_red_light.
          ENDIF.

          READ TABLE lt_ltap WITH KEY lgnum = lt_ltak-lgnum
                                      tanum = lt_ltak-tanum.
          IF sy-subrc = 0.
            ls_alv-vltyp = lt_ltap-vltyp.
            ls_alv-vlpla = lt_ltap-vlpla.
          ENDIF.

          APPEND ls_alv TO lt_alv_aux.
        ENDLOOP.

        " Sem OTs Criadas
        IF sy-subrc <> 0.
          ls_alv-st_nt = icon_red_light.
          ls_alv-tbnum = lt_ltbp-tbnum.
          APPEND ls_alv TO lt_alv_aux.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    " Sem NTs Criadas
    IF sy-subrc <> 0.
      APPEND ls_alv TO lt_alv_aux.
    ENDIF.

  ENDLOOP.

** Validar Status do Grupo
  SORT lt_alv BY refnr.
  DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING refnr.

  LOOP AT lt_alv.
    CLEAR lv_status.

    " Com NTs
    READ TABLE lt_ltbk WITH KEY benum = lt_alv-refnr.

    IF sy-subrc = 0.

      " Finalizado
      LOOP AT lt_ltbk WHERE benum = lt_alv-refnr AND statu <> 'E'.
        EXIT.
      ENDLOOP.

      IF sy-subrc <> 0.
        LOOP AT lt_alv_aux WHERE refnr = lt_alv-refnr AND tanum IS NOT INITIAL
                                                      AND st_ot <> icon_green_light.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          lv_status = icon_led_green.
        ENDIF.
      ENDIF.

      " Com OTs´
      IF lv_status IS INITIAL.

        LOOP AT lt_alv_aux WHERE refnr = lt_alv-refnr AND tanum IS NOT INITIAL.
          EXIT.
        ENDLOOP.

        IF sy-subrc = 0.
          lv_status = icon_ws_start_whse_proc_backgr.
        ENDIF.
      ENDIF.

      IF lv_status IS INITIAL.
        lv_status = icon_operation.
      ENDIF.
    ENDIF.

    CHECK lv_status IS NOT INITIAL.

    LOOP AT lt_alv_aux WHERE refnr = lt_alv-refnr.
      lv_tabix = sy-tabix.

      lt_alv_aux-status = lv_status.

      MODIFY lt_alv_aux INDEX lv_tabix TRANSPORTING status.
    ENDLOOP.
  ENDLOOP.

** Dados ALV TREE
  SORT lt_alv_aux BY drdat refnr kunnr lznum matnr tbnum tanum.

  gt_alv[] = lt_alv_aux[].

** Dados ALV Grid
  REFRESH gt_alv_table.

  CALL FUNCTION 'ZWM_CREATE_TO_REPLENISH_BPE'
    EXPORTING
      i_lgnum    = p_lgnum
      i_process  = ''
    IMPORTING
      e_npal_bpe = scr1-npal
      e_tpal_bpe = scr1-tpal
    TABLES
      t_nt       = gt_alv_table.

ENDFORM.                    " get_data
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

  CALL SCREEN 0100.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree.

  DATA: l_tree_container_name(30) TYPE c,
        l_custom_container        TYPE REF TO cl_gui_custom_container.

  DATA: lt_expand_nodes     TYPE lvc_t_nkey,
        l_toolbar_excluding TYPE ui_functions.

  DATA: l_hierarchy_header TYPE treev_hhdr.

  DATA: lt_list_commentary TYPE slis_t_listheader,
        l_logo             TYPE sdydo_value.

  DATA: ls_variant TYPE disvariant.

  REFRESH gt_alv1.

** Criar o container da tela para inserir o ALV-TREE
*  l_tree_container_name = 'TREE_CONTAINER'.
*
*  IF sy-batch IS INITIAL.
*    CREATE OBJECT l_custom_container
*      EXPORTING
*        container_name              = l_tree_container_name
*      EXCEPTIONS
*        cntl_error                  = 1
*        cntl_system_error           = 2
*        create_error                = 3
*        lifetime_error              = 4
*        lifetime_dynpro_dynpro_link = 5.
*    IF sy-subrc <> 0.
**      MESSAGE e072(zbc_rf_messages).
*    ENDIF.
*  ENDIF.

** Criar o TREE control
  CREATE OBJECT tree
    EXPORTING
      parent                      = gcl_container1
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single "node_sel_mode_multiple
      item_selection              = 'X'
      no_html_header              = 'X'
      no_toolbar                  = ' '
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.

  IF sy-subrc <> 0.
*    MESSAGE e073(zbc_rf_messages).
  ENDIF.

** Criar Fieldcalog
  PERFORM build_fieldcatalog.

** Ajustar propriedades
  PERFORM adjust_proprieties.

* Create Hierarchy-header
  PERFORM build_hierarchy_header CHANGING l_hierarchy_header.

* repid for saving variants
  ls_variant-report = sy-repid.

  PERFORM define_toolbar_excluding CHANGING l_toolbar_excluding.

* create empty tree-control
  CALL METHOD tree->set_table_for_first_display
    EXPORTING
      is_hierarchy_header  = l_hierarchy_header
      it_list_commentary   = lt_list_commentary
      i_logo               = l_logo
      i_background_id      = 'ALV_BACKGROUND'
      i_save               = 'A'
      is_variant           = ls_variant
      it_toolbar_excluding = l_toolbar_excluding
    CHANGING
      it_outtab            = gt_alv1 "table must be empty !!
      it_fieldcatalog      = gt_fieldcatalog.

  PERFORM change_toolbar.

  PERFORM register_events.

  CALL METHOD tree->frontend_update.

ENDFORM.                    " INIT_TREE
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog.

  DATA: pos TYPE i VALUE 1.
  DATA: aux_cat TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE.
  REFRESH aux_cat.

  CLEAR gt_fieldcatalog[].
  REFRESH gt_fieldcatalog[].

** Status
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'STATUS'.
*  nao aparece a esquerda
  aux_cat-no_out        = ' '.
  aux_cat-no_zero       = 'X'.
  aux_cat-icon          = 'X'.
  aux_cat-coltext       = 'St.'.
  aux_cat-outputlen     = '6'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** Status Prioridade
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'ST_PRI'.
*  nao aparece a esquerda
  aux_cat-no_out        = ' '.
  aux_cat-no_zero       = 'X'.
  aux_cat-icon          = 'X'.
  aux_cat-coltext       = 'Pri.'.
  aux_cat-scrtext_l     = 'Prioridade'.
  aux_cat-outputlen     = '6'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** Cliente
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'KUNNR'.
  aux_cat-no_out        = ' '.
*  aux_cat-no_zero       = 'X'.
  aux_cat-coltext       = 'Cliente'.
  aux_cat-outputlen     = '10'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** Descrição
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'DESCR'.
  aux_cat-no_out        = ' '.
*  aux_cat-no_zero       = 'X'.
  aux_cat-coltext       = 'Descrição'.
  aux_cat-outputlen     = '40'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** Data
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'DRDAT'.
  aux_cat-no_out        = ' '.
  aux_cat-no_zero       = 'X'.
  aux_cat-coltext       = 'Data'.
  aux_cat-outputlen     = '10'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** Paletes
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PAL'.
*  nao aparece a esquerda
  aux_cat-no_out        = ' '.
  aux_cat-no_zero       = 'X'.
  aux_cat-just          = 'R'.
  aux_cat-coltext       = 'Nº Pal'.
  aux_cat-outputlen     = '10'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** Paletização Especial
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PAL_PE'.
*  nao aparece a esquerda
  aux_cat-no_out        = ' '.
  aux_cat-no_zero       = 'X'.
  aux_cat-just          = 'R'.
  aux_cat-coltext       = 'Nº Pal Esp.'.
  aux_cat-outputlen     = '10'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** Quantidade Remessa
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'MENGE'.
*  nao aparece a esquerda
  aux_cat-no_out        = ' '.
  aux_cat-no_zero       = 'X'.
  aux_cat-just          = 'R'.
  aux_cat-coltext       = 'Quantidade'.
  aux_cat-outputlen     = '15'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** Unidade
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'MEINS'.
*  aux_cat-ref_field     = 'MEINS'.
*  aux_cat-ref_table     = 'GT_ALV'.
  aux_cat-fieldname     = 'MEINS'.
  aux_cat-tabname       = 'ZWM_010'.
  aux_cat-no_out        = ' '.
*  aux_cat-no_zero       = 'X'.
  aux_cat-coltext       = 'Uni.'.
  aux_cat-outputlen     = '6'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** Status NT/OT
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'ST_MOV'.
*  nao aparece a esquerda
  aux_cat-no_out        = ' '.
  aux_cat-no_zero       = 'X'.
  aux_cat-icon          = 'X'.
  aux_cat-coltext       = 'St.'.
  aux_cat-outputlen     = '6'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** Tipo Depósito de Origem
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'VLTYP'.
*  nao aparece a esquerda
  aux_cat-no_out        = ' '.
  aux_cat-no_zero       = 'X'.
  aux_cat-icon          = ''.
  aux_cat-coltext       = 'TpDepOrig'.
  aux_cat-outputlen     = '8'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

** Posição de Origem
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'VLPLA'.
*  nao aparece a esquerda
  aux_cat-no_out        = ' '.
  aux_cat-no_zero       = 'X'.
  aux_cat-icon          = ''.
  aux_cat-coltext       = 'Pos.origem'.
  aux_cat-outputlen     = '10'.
  aux_cat-fix_column    = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

  gt_fieldcatalog[] = aux_cat[].

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  ADJUST_PROPRIETIES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM adjust_proprieties.

  DATA: pos TYPE i VALUE 1.
  DATA ls_sort_wa TYPE lvc_s_sort.

* Ordenação
  REFRESH gt_sort.

*  CLEAR pos.
*
** create sort-table
*  ADD 1 TO pos.
*  ls_sort_wa-spos = pos.
*  ls_sort_wa-fieldname = 'VBELN'.
*  ls_sort_wa-up = 'X'.
*  ls_sort_wa-subtot = 'X'.
*  APPEND ls_sort_wa TO gt_sort.
*
*  ADD 1 TO pos.
*  ls_sort_wa-spos = pos.
*  ls_sort_wa-fieldname = 'HU_CAIXA'.
*  ls_sort_wa-up = 'X'.
*  ls_sort_wa-subtot = 'X'.
*  APPEND ls_sort_wa TO gt_sort.
*
*  ADD 1 TO pos.
*  ls_sort_wa-spos = pos.
*  ls_sort_wa-fieldname = 'POSTO'.
*  ls_sort_wa-up = 'X'.
*  ls_sort_wa-subtot = 'X'.
*  APPEND ls_sort_wa TO gt_sort.
*
*  ADD 1 TO pos.
*  ls_sort_wa-spos = pos.
*  ls_sort_wa-fieldname = 'MATERIAL'.
*  ls_sort_wa-up = 'X'.
*  ls_sort_wa-subtot = 'X'.
*  APPEND ls_sort_wa TO gt_sort.


ENDFORM.                    " ADJUST_PROPRIETIES
*&---------------------------------------------------------------------*
*&      Form  BUILD_HIERARCHY_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_HIERARCHY_HEADER  text
*----------------------------------------------------------------------*
FORM build_hierarchy_header  CHANGING
                             p_hierarchy_header TYPE treev_hhdr.

  p_hierarchy_header-heading   = 'Grupo/Referência/Material/NT/OT'.
  p_hierarchy_header-tooltip   = ''.
  p_hierarchy_header-width     = 60.
  p_hierarchy_header-width_pix = 'X'.


ENDFORM.                    " BUILD_HIERARCHY_HEADER
*&---------------------------------------------------------------------*
*&      Form  DEFINE_TOOLBAR_EXCLUDING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_TOOLBAR_EXCLUDING  text
*----------------------------------------------------------------------*
FORM define_toolbar_excluding  CHANGING
                               pt_toolbar_excluding TYPE ui_functions.

*  APPEND cl_alv_tree_base=>mc_fc_calculate
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_calculate_avg
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_calculate_min
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_calculate_max
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_calculate_sum
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_expand
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_collapse
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_col_optimize
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_col_invisible
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_find
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_help
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_current_variant
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_load_variant
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_save_variant
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_maintain_variant
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_print_back
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_print_back_all
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_print_prev
*    TO pt_toolbar_excluding.
*
*  APPEND cl_alv_tree_base=>mc_fc_print_prev_all
*    TO pt_toolbar_excluding.


ENDFORM.                    " DEFINE_TOOLBAR_EXCLUDING
*&---------------------------------------------------------------------*
*&      Form  CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_toolbar.

  CALL METHOD tree->get_toolbar_object
    IMPORTING
      er_toolbar = g_toolbar.

  CHECK NOT g_toolbar IS INITIAL. "could happen if you do not use the
  "standard toolbar

* Modify toolbar with methods of CL_GUI_TOOLBAR:
* Separador
  CALL METHOD g_toolbar->add_button
    EXPORTING
      fcode     = ''
      icon      = ''
      butn_type = cntb_btype_sep.

* Botão para consumos
*  CALL METHOD g_toolbar->add_button
*    EXPORTING
*      fcode     = 'REFRESH'
*      icon      = '@42@'
*      butn_type = cntb_btype_button
*      text      = ''
*      quickinfo = 'Actualizar'.    "Information
*
** Modify toolbar with methods of CL_GUI_TOOLBAR:
** Separador
*  CALL METHOD g_toolbar->add_button
*    EXPORTING
*      fcode     = ''
*      icon      = ''
*      butn_type = cntb_btype_sep.

* Botão para Criar NTs
  CALL METHOD g_toolbar->add_button
    EXPORTING
      fcode     = 'CREATE_NT'
      icon      = icon_operation
      butn_type = cntb_btype_button
      text      = 'Necessidade'
      quickinfo = 'Criar Necessidade'.    "Information

** Botão dar prioridade ao grupo

  CALL METHOD g_toolbar->add_button
    EXPORTING
      fcode     = 'PRIORITY_NT'
      icon      = icon_status_critical
      butn_type = cntb_btype_dropdown
      text      = 'Prioridade'
      quickinfo = 'Aumentar Prioridade'.    "Information

ENDFORM.                    " CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  REGISTER_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_events .

  DATA: lv_tree_evt_rec    TYPE REF TO lcl_tree_event_receiver,
        lv_toolbar_evt_rec TYPE REF TO lcl_toolbar_event_receiver.


  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.


* Get register Events
  CALL METHOD tree->get_registered_events
    IMPORTING
      events = lt_events.

* define the events which will be passed to the backend
  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_checkbox_change.
  APPEND l_event TO lt_events.
  l_event-eventid =
  cl_gui_column_tree=>eventid_header_context_men_req.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_header_click.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_keypress.
  APPEND l_event TO lt_events.

  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  l_event-appl_event = 'X'.
  APPEND l_event TO lt_events.

* register events on frontend
  CALL METHOD tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.
  ENDIF.

  CREATE OBJECT lv_tree_evt_rec.
  SET HANDLER lv_tree_evt_rec->handle_item_double_click FOR tree.
*
  CREATE OBJECT lv_toolbar_evt_rec.
  SET HANDLER lv_toolbar_evt_rec->on_button_clicked   FOR g_toolbar.
  SET HANDLER lv_toolbar_evt_rec->on_dropdown_clicked FOR g_toolbar.


ENDFORM.                    " REGISTER_EVENTS
*&---------------------------------------------------------------------*
*&      Form  CREAT_HIERARCHY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM creat_hierarchy.
  DATA: lt_children   TYPE lvc_t_nkey.
  DATA: ls_children   TYPE LINE OF lvc_t_nkey.

  DATA: child_node TYPE lvc_nkey.

** Criar Hierarquia
**********************************************************************
  CALL METHOD tree->delete_all_nodes.

* create hierarchy
  PERFORM create_hierarchy.

** Expand nodes
  LOOP AT gt_expand_nodes.
* Check if have child node
    CLEAR child_node.

    CALL METHOD tree->get_first_child
      EXPORTING
        i_node_key       = gt_expand_nodes-node
      IMPORTING
        e_child_node_key = child_node.

    IF child_node IS NOT INITIAL.

* Expand first level
      CALL METHOD tree->expand_node
        EXPORTING
          i_node_key = gt_expand_nodes-node.
    ENDIF.
  ENDLOOP.

** optimize column-width
  CALL METHOD tree->column_optimize
    EXPORTING
      i_include_heading = 'X'.


ENDFORM.                    " CREAT_HIERARCHY
*&---------------------------------------------------------------------*
*&      Form  CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_hierarchy.

  DATA: ls_alv LIKE LINE OF gt_alv.
  DATA: lv_refnr TYPE lvs_refnr.
  DATA: lv_kunnr TYPE kunnr.
  DATA: lv_lznum TYPE lznum.
  DATA: lv_matnr TYPE matnr.
  DATA: lv_tbnum TYPE tbnum.
  DATA: lv_tanum TYPE tanum.

  DATA: l_item_key  TYPE lvc_nkey,
        l_item1_key TYPE lvc_nkey,
        l_item2_key TYPE lvc_nkey,
        l_item3_key TYPE lvc_nkey,
        l_item4_key TYPE lvc_nkey.

  REFRESH: gt_alv1.
  REFRESH: gt_expand_nodes.

** Criar Hierarquia
**********************************************************************
  LOOP AT gt_alv.
    MOVE gt_alv TO ls_alv.

** Nó Grupo
    IF lv_refnr <> ls_alv-refnr.
      PERFORM add_refnr_line USING   ls_alv
                                     g_top_node_key
                            CHANGING l_item_key.

      lv_refnr = ls_alv-refnr.

      CLEAR: lv_lznum, lv_matnr, lv_tbnum, lv_tanum.
    ENDIF.

** Nó Referencia
    IF lv_lznum <> ls_alv-lznum.
      PERFORM add_lznum_line USING    ls_alv
                                      l_item_key
                             CHANGING l_item1_key.

      lv_lznum = ls_alv-lznum.

      CLEAR: lv_matnr, lv_tbnum, lv_tanum.
    ENDIF.

** Nó Material
    IF lv_matnr <> ls_alv-matnr.
      PERFORM add_matnr_line USING    ls_alv
                                      l_item1_key
                             CHANGING l_item2_key.

      lv_matnr = ls_alv-matnr.

      CLEAR: lv_tbnum, lv_tanum.
    ENDIF.

** Nó NT
    IF lv_tbnum <> ls_alv-tbnum AND ls_alv-tbnum IS NOT INITIAL.
      PERFORM add_tbnum_line USING    ls_alv
                                      l_item2_key
                             CHANGING l_item3_key.

      lv_tbnum = ls_alv-tbnum.

      CLEAR: lv_tanum.
    ENDIF.

** Nó OT
    IF lv_tanum <> ls_alv-tanum AND ls_alv-tanum IS NOT INITIAL.
      PERFORM add_tanum_line USING    ls_alv
                                      l_item3_key
                             CHANGING l_item4_key.

      lv_tanum = ls_alv-tanum.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CREATE_HIERARCHY
*&---------------------------------------------------------------------
*&      Form  ADD_REFNR_LINE
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*      -->P_LS_ALV_REFNR  text
*      -->P_G_TOP_NODE_KEY  text
*      <--P_L_ITEM_KEY  text
*----------------------------------------------------------------------
FORM add_refnr_line  USING   p_alv        TYPE st_alv
                             p_node_key   TYPE lvc_nkey
                    CHANGING p_child_key  TYPE lvc_nkey.

  DATA: lv_date     TYPE char10.
  DATA: lv_tbnum    TYPE tbnum.
  DATA: l_node_text TYPE lvc_value,
        ls_out      LIKE gt_alv,
        ls_ltbk     TYPE ltbk.

  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  DATA: ls_node_layout TYPE lvc_s_layn.

  DATA: lt_alv         TYPE TABLE OF st_alv WITH HEADER LINE.

** Adicionar Grupo
**********************************************************************
  ls_node_layout-exp_image = icon_transport.
  ls_node_layout-n_image   = icon_transport.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_alv-refnr
    IMPORTING
      output = l_node_text.

  ls_out-refnr  = p_alv-refnr.
  ls_out-refnt  = p_alv-refnt.
  ls_out-drdat  = p_alv-drdat.
  ls_out-status = p_alv-status.
  ls_out-descr  = p_alv-refnt.

  lt_alv[] = gt_alv[].

  DELETE lt_alv WHERE refnr <> p_alv-refnr.

  SORT lt_alv BY refnr lznum matnr.
  DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING refnr lznum matnr.

  LOOP AT lt_alv.
    ls_out-pal    = ls_out-pal    + lt_alv-pal.
    ls_out-pal_pe = ls_out-pal_pe + lt_alv-pal_pe.

    IF lt_alv-tbnum IS NOT INITIAL.
      lv_tbnum = lt_alv-tbnum.
    ENDIF.

  ENDLOOP.

  " Prioridade do Grupo
  IF lv_tbnum IS NOT INITIAL.

    SELECT SINGLE *
      FROM ltbk INTO ls_ltbk
      WHERE lgnum = p_lgnum
      AND   tbnum = lv_tbnum.

    IF ls_ltbk-tbpri IS NOT INITIAL.
      ls_out-st_pri = icon_status_critical.
    ENDIF.
  ENDIF.

  CALL METHOD tree->add_node
    EXPORTING
      i_relat_node_key = p_node_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
      is_node_layout   = ls_node_layout
    IMPORTING
      e_new_node_key   = p_child_key.

  CLEAR gt_expand_nodes.
  gt_expand_nodes = p_child_key.
  APPEND gt_expand_nodes.

ENDFORM.                    " ADD_REFNR_LINE
*&---------------------------------------------------------------------
*&      Form  ADD_LZNUM_LINE
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*      -->P_LS_ALV_REFNR  text
*      -->P_G_TOP_NODE_KEY  text
*      <--P_L_ITEM_KEY  text
*----------------------------------------------------------------------
FORM add_lznum_line  USING   p_alv        TYPE st_alv
                             p_node_key   TYPE lvc_nkey
                    CHANGING p_child_key  TYPE lvc_nkey.

  DATA: lv_date     TYPE char10.
  DATA: l_node_text TYPE lvc_value,
        ls_out      LIKE gt_alv.

  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  DATA: ls_node_layout TYPE lvc_s_layn.

  DATA: lt_alv         TYPE TABLE OF st_alv WITH HEADER LINE.

*  ls_node_layout-exp_image = gc_icon_ordem.
*  ls_node_layout-n_image   = gc_icon_ordem.
*

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_alv-lznum
    IMPORTING
      output = l_node_text.

  ls_out-refnr = p_alv-refnr.
  ls_out-descr = p_alv-name1.
  ls_out-kunnr = p_alv-kunnr.
  ls_out-lznum = p_alv-lznum.

  lt_alv[] = gt_alv[].

  DELETE lt_alv WHERE refnr <> p_alv-refnr.
  DELETE lt_alv WHERE lznum <> p_alv-lznum.

  SORT lt_alv BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING matnr.

  LOOP AT lt_alv.
    ls_out-pal    = ls_out-pal    + lt_alv-pal.
    ls_out-pal_pe = ls_out-pal_pe + lt_alv-pal_pe.
  ENDLOOP.

  CALL METHOD tree->add_node
    EXPORTING
      i_relat_node_key = p_node_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
      is_node_layout   = ls_node_layout
    IMPORTING
      e_new_node_key   = p_child_key.

  CLEAR gt_expand_nodes.
  gt_expand_nodes = p_child_key.
  APPEND gt_expand_nodes.

ENDFORM.                    " ADD_LZNUM_LINE
*&---------------------------------------------------------------------
*&      Form  ADD_MATNR_LINE
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*      -->P_LS_ALV_REFNR  text
*      -->P_G_TOP_NODE_KEY  text
*      <--P_L_ITEM_KEY  text
*----------------------------------------------------------------------
FORM add_matnr_line  USING   p_alv        TYPE st_alv
                             p_node_key   TYPE lvc_nkey
                    CHANGING p_child_key  TYPE lvc_nkey.

  DATA: lv_date     TYPE char10.
  DATA: l_node_text TYPE lvc_value,
        ls_out      LIKE gt_alv.

  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  DATA: ls_node_layout TYPE lvc_s_layn.

  ls_node_layout-exp_image =  icon_material.
  ls_node_layout-n_image   =  icon_material.
*
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_alv-matnr
    IMPORTING
      output = l_node_text.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = p_alv-meins
    IMPORTING
      output         = p_alv-meins
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.

  ls_out-descr  = p_alv-maktx.
  ls_out-refnr  = p_alv-refnr.
  ls_out-kunnr  = p_alv-kunnr.
  ls_out-matnr  = p_alv-matnr.
  ls_out-menge  = p_alv-menge.
  ls_out-pal    = p_alv-pal.
  ls_out-pal_pe = p_alv-pal_pe.
  ls_out-meins  = p_alv-meins.

  CALL METHOD tree->add_node
    EXPORTING
      i_relat_node_key = p_node_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
      is_node_layout   = ls_node_layout
    IMPORTING
      e_new_node_key   = p_child_key.

*  CLEAR gt_expand_nodes.
*  gt_expand_nodes = p_child_key.
*  APPEND gt_expand_nodes.

ENDFORM.                    " ADD_MATNR_LINE
*&---------------------------------------------------------------------
*&      Form  ADD_TBNUM_LINE
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*      -->P_LS_ALV_REFNR  text
*      -->P_G_TOP_NODE_KEY  text
*      <--P_L_ITEM_KEY  text
*----------------------------------------------------------------------
FORM add_tbnum_line  USING   p_alv        TYPE st_alv
                             p_node_key   TYPE lvc_nkey
                    CHANGING p_child_key  TYPE lvc_nkey.

  DATA: lv_date     TYPE char10.
  DATA: lv_menge    TYPE menge_d.
  DATA: l_node_text TYPE lvc_value,
        ls_out      LIKE gt_alv,
        ls_marm     TYPE marm,
        ls_zwm031   TYPE zwm031.

  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  DATA: lt_ltbp TYPE TABLE OF ltbp WITH HEADER LINE.

  DATA: ls_node_layout TYPE lvc_s_layn.

  ls_node_layout-exp_image = icon_operation.
  ls_node_layout-n_image   = icon_operation.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_alv-tbnum
    IMPORTING
      output = l_node_text.

  SELECT *
    FROM ltbp INTO TABLE lt_ltbp
    WHERE lgnum = p_lgnum
    AND   tbnum = p_alv-tbnum.

  ls_out-refnr  = p_alv-refnr.
  ls_out-kunnr  = p_alv-kunnr.
  ls_out-lznum  = p_alv-lznum.
  ls_out-matnr  = p_alv-matnr.

  LOOP AT lt_ltbp WHERE matnr = p_alv-matnr.
    ls_out-menge = ls_out-menge + lt_ltbp-menge.
    ls_out-meins = lt_ltbp-meins.
  ENDLOOP.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = ls_out-meins
    IMPORTING
      output         = ls_out-meins
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.

  SELECT SINGLE *
    FROM marm INTO ls_marm
    WHERE matnr = p_alv-matnr
    AND   meinh = 'PAL'.

  SELECT SINGLE *
    FROM zwm031 INTO ls_zwm031
    WHERE lgnum = p_lgnum
    AND   kunnr = p_alv-kunnr
    AND   matnr = p_alv-matnr.

  ls_out-pal    = ls_out-menge / ls_marm-umrez.
  lv_menge      = ls_out-menge / ls_zwm031-unporpal.
  ls_out-pal_pe = ceil( lv_menge ).

  ls_out-tbnum  = p_alv-tbnum.
  ls_out-st_mov = p_alv-st_nt.

  CALL METHOD tree->add_node
    EXPORTING
      i_relat_node_key = p_node_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
      is_node_layout   = ls_node_layout
    IMPORTING
      e_new_node_key   = p_child_key.

*  CLEAR gt_expand_nodes.
*  gt_expand_nodes = p_child_key.
*  APPEND gt_expand_nodes.

ENDFORM.                    " ADD_TBNUM_LINE
*&---------------------------------------------------------------------
*&      Form  ADD_TANUM_LINE
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*      -->P_LS_ALV_REFNR  text
*      -->P_G_TOP_NODE_KEY  text
*      <--P_L_ITEM_KEY  text
*----------------------------------------------------------------------
FORM add_tanum_line  USING   p_alv        TYPE st_alv
                             p_node_key   TYPE lvc_nkey
                    CHANGING p_child_key  TYPE lvc_nkey.

  DATA: lv_date     TYPE char10.
  DATA: l_node_text TYPE lvc_value,
        ls_out      LIKE gt_alv,
        ls_marm     TYPE marm,
        ls_zwm031   TYPE zwm031.

  DATA: lt_ltap TYPE TABLE OF ltap WITH HEADER LINE.

  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  DATA: ls_node_layout TYPE lvc_s_layn.
*
  ls_node_layout-exp_image = icon_ws_start_whse_proc_backgr.
  ls_node_layout-n_image   = icon_ws_start_whse_proc_backgr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_alv-tanum
    IMPORTING
      output = l_node_text.

  ls_out-refnr  = p_alv-refnr.
  ls_out-kunnr  = p_alv-kunnr.
  ls_out-lznum  = p_alv-lznum.
  ls_out-matnr  = p_alv-matnr.

  SELECT *
    FROM ltap INTO TABLE lt_ltap
    WHERE lgnum = p_lgnum
    AND   tanum = p_alv-tanum.

  LOOP AT lt_ltap WHERE matnr = p_alv-matnr.
    ls_out-menge = ls_out-menge + lt_ltap-vsolm.
    ls_out-meins = lt_ltap-meins.
  ENDLOOP.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = ls_out-meins
    IMPORTING
      output         = ls_out-meins
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.

  SELECT SINGLE *
    FROM marm INTO ls_marm
    WHERE matnr = p_alv-matnr
    AND   meinh = 'PAL'.

  SELECT SINGLE *
    FROM zwm031 INTO ls_zwm031
    WHERE lgnum = p_lgnum
    AND   kunnr = p_alv-kunnr
    AND   matnr = p_alv-matnr.

  ls_out-pal    = ls_out-menge / ls_marm-umrez.
*  ls_out-pal_pe = ls_out-menge / ls_zwm031-unporpal.

  ls_out-tbnum  = p_alv-tbnum.
  ls_out-tanum  = p_alv-tanum.
  ls_out-vltyp  = p_alv-vltyp.
  ls_out-vlpla  = p_alv-vlpla.
  ls_out-st_mov = p_alv-st_ot.


  CALL METHOD tree->add_node
    EXPORTING
      i_relat_node_key = p_node_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
      is_node_layout   = ls_node_layout
    IMPORTING
      e_new_node_key   = p_child_key.
*
*  CLEAR gt_expand_nodes.
*  gt_expand_nodes = p_child_key.
*  APPEND gt_expand_nodes.

ENDFORM.                    " ADD_TANUM_LINE
*&---------------------------------------------------------------------*
*&      Form  EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_KEY  text
*      -->P_FIELDNAME  text
*----------------------------------------------------------------------*
FORM event_double_click  USING  p_node_key  TYPE  lvc_nkey
                                p_fieldname.

  DATA:ls_node_alv1 LIKE LINE OF gt_alv1.

*** Selecciona a linha da tabela gt_alv1 correspondente ao nó
  CALL METHOD tree->get_outtab_line
    EXPORTING
      i_node_key    = p_node_key
    IMPORTING
      e_outtab_line = ls_node_alv1.

  IF p_fieldname = '&Hierarchy'.

    IF ls_node_alv1-tbnum IS NOT INITIAL AND ls_node_alv1-tanum IS INITIAL.
      SET PARAMETER ID 'TBN' FIELD ls_node_alv1-tbnum.
      SET PARAMETER ID 'TBP' FIELD ' '.
      SET PARAMETER ID 'LGN' FIELD p_lgnum.
      CALL TRANSACTION 'LB03' AND SKIP FIRST SCREEN.

    ELSEIF ls_node_alv1-tanum IS NOT INITIAL.
      SET PARAMETER ID 'TAN' FIELD ls_node_alv1-tanum.
      SET PARAMETER ID 'TAP' FIELD ' '.
      SET PARAMETER ID 'LGN' FIELD p_lgnum.
      CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.
*
*  IF ls_node_alv1-ordem IS NOT INITIAL AND
*     ls_node_alv1-matnr IS NOT INITIAL.
*
*    IF p_fieldname = 'QTD_D'.
*      CHECK ls_node_alv1-qtd_d IS NOT INITIAL.
*
*      PERFORM list_ot_descong USING ls_node_alv1 'F'.
*    ELSEIF p_fieldname = 'QTD_RFP'.
*      CHECK ls_node_alv1-qtd_rfp IS NOT INITIAL.
*
*      PERFORM list_ot_descong USING ls_node_alv1 'P'.
*    ELSE.
*      EXIT.
*    ENDIF.
*  ENDIF.

*** Carregar dados para remessa
*  IF ls_node_alv1-vbeln IS NOT INITIAL AND
*     ls_node_alv1-hu_caixa  IS INITIAL.
*
*    PERFORM load_data_vbeln USING ls_node_alv1-vbeln.
*
*** Carregar dados para remessa e para a caixa
*  ELSEIF ls_node_alv1-vbeln IS INITIAL AND
*     ls_node_alv1-hu_caixa  IS NOT INITIAL.
*
*    PERFORM load_data_hu_caixa USING ls_node_alv1-hu_caixa.
*
*  ENDIF.

ENDFORM.                    " EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  CREATE_NT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_nt.

  DATA: lv_lines         TYPE i.
  DATA: lv_rc            TYPE char1.
  DATA: lv_anfme         LIKE rl03t-anfme.
  DATA: lv_aufnr         TYPE aufnr.
  DATA: lv_menge         TYPE menge_d.
  DATA: lv_tanum         TYPE tanum.
  DATA: lv_text          TYPE char255.
  DATA: lv_selected_node TYPE lvc_nkey.
  DATA: lt_selected      TYPE lvc_t_nkey.
  DATA: lv_item_name     TYPE lvc_fname.
  DATA: lv_return_code   TYPE char1.
  DATA: ls_alv           TYPE st_alv.
  DATA: ls_ltbk          TYPE ltbk.
  DATA: ls_ltap          LIKE ltap.
  DATA: lt_fields        LIKE sval       OCCURS 0 WITH HEADER LINE.
  DATA: lt_lqua          LIKE lqua       OCCURS 0 WITH HEADER LINE.
  DATA: lt_lqua_ot       LIKE lqua       OCCURS 0 WITH HEADER LINE.
  DATA: lt_alv           TYPE st_alv     OCCURS 0 WITH HEADER LINE.
  DATA: lt_return        LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: lt_alv_aux       TYPE st_alv     OCCURS 0 WITH HEADER LINE.
  DATA: lt_log           TYPE st_log     OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltba          TYPE ltba       OCCURS 0 WITH HEADER LINE.

  DATA: flag_error       TYPE char1.

** Obter selecção
**********************************************************************
  CALL METHOD tree->get_selected_item
    IMPORTING
      e_selected_node = lv_selected_node
      e_fieldname     = lv_item_name.

  IF lv_selected_node IS INITIAL.
    CALL METHOD tree->get_selected_nodes
      CHANGING
        ct_selected_nodes = lt_selected
      EXCEPTIONS
        cntl_system_error = 1
        dp_error          = 2
        failed            = 3
        OTHERS            = 4.

    LOOP AT lt_selected INTO lv_selected_node.

      CALL METHOD tree->get_outtab_line
        EXPORTING
          i_node_key    = lv_selected_node
        IMPORTING
          e_outtab_line = ls_alv.

      APPEND ls_alv TO lt_alv.

    ENDLOOP.

  ELSE.
    CALL METHOD tree->get_outtab_line
      EXPORTING
        i_node_key    = lv_selected_node
      IMPORTING
        e_outtab_line = ls_alv.

    APPEND ls_alv TO lt_alv.
  ENDIF.

** Validar linha
  CHECK lt_alv[] IS NOT INITIAL.

  DESCRIBE TABLE lt_alv LINES lv_lines.

  IF lv_lines > 1.
    MESSAGE i000 WITH 'ERRO: Seleccionar uma linha' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT lt_alv WHERE kunnr IS NOT INITIAL.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    MESSAGE i000 WITH 'ERRO: Seleccionar linha de Grupo' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE lt_alv INDEX 1.

  LOOP AT gt_alv WHERE refnr = lt_alv-refnr AND tbnum IS NOT INITIAL.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    MESSAGE i000 WITH 'ERRO: Grupo já tem Necessidade criada' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      textline1      = 'Deseja criar necessidade para o grupo'
      textline2      = lt_alv-refnr
      titel          = 'Criar Necessidade'
      cancel_display = 'X'
    IMPORTING
      answer         = lv_rc.

  CHECK lv_rc = 'J'.

  CLEAR lv_rc.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      textline1      = 'Quer dar prioridade ao grupo'
      textline2      = lt_alv-refnr
      titel          = 'Prioridade'
      cancel_display = 'X'
    IMPORTING
      answer         = lv_rc.

** Criar NTs
**********************************************************************
  CLEAR flag_error.

  LOOP AT gt_alv INTO ls_alv WHERE refnr = lt_alv-refnr .

    REFRESH: lt_ltba.

    CLEAR lt_ltba.
    lt_ltba-tabix = sy-tabix.
    lt_ltba-bwlvs = gv_bwlvs.
    lt_ltba-betyp = 'S'.
    lt_ltba-benum = ls_alv-refnr.
    lt_ltba-lznum = ls_alv-lznum.
    lt_ltba-lgnum = p_lgnum.
    lt_ltba-werks = gv_werks.
    lt_ltba-lgort = gv_lgort.

    IF lv_rc = 'J'.
      lt_ltba-tbpri = '9'.
    ENDIF.

    lt_ltba-matnr = ls_alv-matnr.
    lt_ltba-menga = ls_alv-menge.
    lt_ltba-altme = ls_alv-meins.
    APPEND lt_ltba.

    CALL FUNCTION 'L_TR_CREATE'
      EXPORTING
        i_single_item         = 'X'
        i_commit_work         = ''
      TABLES
        t_ltba                = lt_ltba
      EXCEPTIONS
        item_error            = 1
        no_entry_in_int_table = 2
        item_without_number   = 3
        no_update_item_error  = 4
        OTHERS                = 5.

    IF sy-subrc <> 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      flag_error = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  " Tem de fazer NT para todos clientes/Materiais
  IF flag_error = 'X'.
    READ TABLE lt_ltba INDEX 1.
    IF sy-subrc = 0.
      MESSAGE ID lt_ltba-msgid TYPE 'I' NUMBER lt_ltba-msgno
      WITH lt_ltba-msgv1 lt_ltba-msgv2 lt_ltba-msgv3 lt_ltba-msgv4  DISPLAY LIKE 'E'.

      EXIT.
    ENDIF.
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.

** Mensagem de sucesso
  DO 10 TIMES.
    SELECT SINGLE *
      FROM ltbk INTO ls_ltbk
      WHERE lgnum = p_lgnum
      AND   betyp = 'S'
      AND   benum = lt_alv-refnr.

    IF sy-subrc = 0.
      EXIT.
    ENDIF.

    WAIT UP TO 1 SECONDS.
  ENDDO.

  MESSAGE s000(zwmrf) WITH 'NTs criadas com sucesso'.

  PERFORM get_data.

ENDFORM.                    " CREATE_NT
*&---------------------------------------------------------------------*
*&      Form  PRIORIY_NT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prioriy_nt USING p_fcode.

  DATA: lv_rc            TYPE c.
  DATA: lv_lines         TYPE i.
  DATA: lv_anfme         LIKE rl03t-anfme.
  DATA: lv_aufnr         TYPE aufnr.
  DATA: lv_menge         TYPE menge_d.
  DATA: lv_tanum         TYPE tanum.
  DATA: lv_text          TYPE char255.
  DATA: lv_selected_node TYPE lvc_nkey.
  DATA: lt_selected      TYPE lvc_t_nkey.
  DATA: lv_item_name     TYPE lvc_fname.
  DATA: lv_return_code   TYPE char1.
  DATA: ls_alv           TYPE st_alv.
  DATA: ls_ltbk          TYPE ltbk.
  DATA: ls_ltap          LIKE ltap.
  DATA: lt_fields        LIKE sval       OCCURS 0 WITH HEADER LINE.
  DATA: lt_lqua          LIKE lqua       OCCURS 0 WITH HEADER LINE.
  DATA: lt_lqua_ot       LIKE lqua       OCCURS 0 WITH HEADER LINE.
  DATA: lt_alv           TYPE st_alv     OCCURS 0 WITH HEADER LINE.
  DATA: lt_return        LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: lt_alv_aux       TYPE st_alv     OCCURS 0 WITH HEADER LINE.
  DATA: lt_log           TYPE st_log     OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltba          TYPE ltba       OCCURS 0 WITH HEADER LINE.

  DATA: flag_error       TYPE char1.
  DATA: lv_tbpri         TYPE tbpri.

** Obter selecção
**********************************************************************
  CALL METHOD tree->get_selected_item
    IMPORTING
      e_selected_node = lv_selected_node
      e_fieldname     = lv_item_name.

  IF lv_selected_node IS INITIAL.
    CALL METHOD tree->get_selected_nodes
      CHANGING
        ct_selected_nodes = lt_selected
      EXCEPTIONS
        cntl_system_error = 1
        dp_error          = 2
        failed            = 3
        OTHERS            = 4.

    LOOP AT lt_selected INTO lv_selected_node.

      CALL METHOD tree->get_outtab_line
        EXPORTING
          i_node_key    = lv_selected_node
        IMPORTING
          e_outtab_line = ls_alv.

      APPEND ls_alv TO lt_alv.

    ENDLOOP.

  ELSE.
    CALL METHOD tree->get_outtab_line
      EXPORTING
        i_node_key    = lv_selected_node
      IMPORTING
        e_outtab_line = ls_alv.

    APPEND ls_alv TO lt_alv.
  ENDIF.

** Validar linha
  CHECK lt_alv[] IS NOT INITIAL.

  DESCRIBE TABLE lt_alv LINES lv_lines.

  IF lv_lines > 1.
    MESSAGE i000 WITH 'ERRO: Seleccionar uma linha' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT lt_alv WHERE kunnr IS NOT INITIAL.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    MESSAGE i000 WITH 'ERRO: Seleccionar linha de Grupo' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE lt_alv INDEX 1.

  LOOP AT gt_alv WHERE refnr = lt_alv-refnr AND tbnum IS NOT INITIAL.
    EXIT.
  ENDLOOP.

  IF sy-subrc <> 0.
    MESSAGE i000 WITH 'ERRO: Grupo sem necessidade criada' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT gt_alv WHERE refnr = lt_alv-refnr AND tanum IS NOT INITIAL.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    MESSAGE i000 WITH 'ERRO: Grupo já tem OTs criadas' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

** Questionar
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      textline1      = 'Deseja alterar prioridade do grupo'
      textline2      = lt_alv-refnr
      titel          = 'Prioridade'
      cancel_display = 'X'
    IMPORTING
      answer         = lv_rc.

  CHECK lv_rc = 'J'.

  IF p_fcode = 'REM_PRIORITY_NT'.
    CLEAR lv_tbpri.
  ELSE.
    lv_tbpri = '9'.
  ENDIF.

** Prioridade
**********************************************************************
  LOOP AT gt_alv WHERE refnr = lt_alv-refnr AND tbnum IS NOT INITIAL.
    lt_alv_aux = gt_alv.
    APPEND lt_alv_aux.
  ENDLOOP.

  SORT lt_alv_aux BY tbnum.
  DELETE ADJACENT DUPLICATES FROM lt_alv_aux COMPARING tbnum.

  LOOP AT lt_alv_aux.

    CALL FUNCTION 'ZWM_TBPRI_CHANGE'
      EXPORTING
        i_lgnum = p_lgnum
        i_tbnum = lt_alv_aux-tbnum
        i_tbpri = lv_tbpri
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.

    IF sy-subrc <> 0.
      MESSAGE i000 WITH 'ERRO: Modificar a prioridade na necessidade' lt_alv_aux-tbnum  DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDLOOP.

  MESSAGE s000(zwmrf) WITH 'Prioridade alterada com sucesso'.

  PERFORM get_data.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM GET_PARAMETER                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(MODULE)                                                 *
*  -->  VALUE(PARAM)                                                  *
*  -->  VALOR                                                         *
*---------------------------------------------------------------------*
FORM get_parameter  USING    whs
                             module
                             param
                             valor.

  IF gt_zwm001[] IS INITIAL.
    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
      EXPORTING
        whs       = whs
      TABLES
        ti_zwm001 = gt_zwm001.
  ENDIF.

  READ TABLE gt_zwm001 WITH KEY armazem   = whs
                                processo  = module
                                parametro = param
                                BINARY SEARCH.
  IF sy-subrc = 0.
    MOVE gt_zwm001-valor TO valor.
  ENDIF.

ENDFORM.                    " GET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  INIT_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_container.

  CHECK gcl_container IS INITIAL.

  CREATE OBJECT gcl_container
    EXPORTING
      container_name = 'ALV_CONTAINER'.

  CREATE OBJECT gcl_splitter
    EXPORTING
      parent  = gcl_container
      rows    = 1
      columns = 2.
**                  orientation = 0.

  CALL METHOD gcl_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = gcl_container1
    EXCEPTIONS
      OTHERS    = 1.

  CALL METHOD gcl_splitter->get_container
    EXPORTING
      row       = 1
      column    = 2
    RECEIVING
      container = gcl_container2
    EXCEPTIONS
      OTHERS    = 1.


  CALL METHOD gcl_splitter->set_column_width
    EXPORTING
      id     = 1
      width  = '80'
    EXCEPTIONS
      OTHERS = 1.

*  CALL METHOD gcl_splitter->set_row_height
*    EXPORTING
*      id     = 2
*      height = '30'
*    EXCEPTIONS
*      OTHERS = 1.
*
*  CALL METHOD gcl_splitter->set_row_height
*    EXPORTING
*      id     = 3
*      height = '50'
*    EXCEPTIONS
*      OTHERS = 1.

ENDFORM.
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

*  IF gcl_container IS INITIAL.
*    CREATE OBJECT gcl_container
*      EXPORTING
*        container_name = 'CONTAINER_ALV'.
*  ENDIF.

  CREATE OBJECT gcl_alv_grid
    EXPORTING
      i_parent = gcl_container2.

** Cabeçalho
  ls_layout-zebra      = abap_true.
  ls_layout-sel_mode   = 'D'.
  ls_layout-cwidth_opt = abap_true. "Optimize

** Variante
*  ls_variant          = us_variant.
  ls_variant-report   = sy-repid.
  ls_variant-username = sy-uname.

** Field catalog
  PERFORM get_fldcat  USING 'GT_ALV_TABLE' 'ZWM_011' gt_alv_table
                      CHANGING lt_fieldcat.

  PERFORM change_fldcat USING 'GT_ALV_TABLE' CHANGING lt_fieldcat.

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

ENDFORM.
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
FORM change_fldcat USING    pu_table_nm TYPE slis_tabname
                   CHANGING pt_fieldcat TYPE lvc_t_fcat.

  FIELD-SYMBOLS: <ls_fcat> TYPE lvc_s_fcat.

** Ajusta o Fieldcatalog
***********************************************************************
  LOOP AT pt_fieldcat ASSIGNING <ls_fcat>.

    CASE <ls_fcat>-fieldname.
*
*      WHEN 'MANDT'.
*        <ls_fcat>-no_out = 'X'.
*
**      WHEN ''.
**        <ls_fcat>-tech      = 'X'.
**        <ls_fcat>-key       = 'X'.
**        <fs_fcat>-icon      = 'X'.
*
      WHEN 'ICON'.
        <ls_fcat>-coltext   = 'Status'.
        <ls_fcat>-icon      = 'X'.
        <ls_fcat>-outputlen = '8'.


*      WHEN 'ICON_SEND'.
*        <ls_fcat>-coltext   = 'St. Envio'.
**        <ls_fcat>-hotspot  = 'X'.
*        <ls_fcat>-icon      = 'X'.
*        <ls_fcat>-outputlen = '4'.

      WHEN 'VOLTA'.
        IF pu_table_nm = 'GT_ALV_TABLE'.
          <ls_fcat>-hotspot   = 'X'.
        ENDIF.

*      WHEN 'CONTRACT_ID'.
*        <ls_fcat>-hotspot   = 'X'.
*
      WHEN 'LZNUM'.
        <ls_fcat>-coltext   = 'Referência'.
        <ls_fcat>-scrtext_s = 'Referência'.
        <ls_fcat>-scrtext_m = 'Referência'.
        <ls_fcat>-scrtext_l = 'Referência'.

      WHEN 'TABCOMP' OR 'PESCOMP' OR
           'TABPICK' OR 'PESPICK'.
        <ls_fcat>-do_sum  = 'X'.

      WHEN 'VEMNG_TOT'.
        <ls_fcat>-do_sum  = 'X'.

      WHEN 'PES' OR 'PESTOT'.
        <ls_fcat>-do_sum  = 'X'.

    ENDCASE.

  ENDLOOP.

ENDFORM.                    " CHANGE_FLDCAT
*&---------------------------------------------------------------------*
*&      Form  REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh.

  DATA: ls_stable TYPE lvc_s_stbl.

  PERFORM get_data.

  ls_stable-row = 'X'.

  CALL METHOD gcl_alv_grid->refresh_table_display
    EXPORTING
      is_stable = ls_stable.

ENDFORM.
