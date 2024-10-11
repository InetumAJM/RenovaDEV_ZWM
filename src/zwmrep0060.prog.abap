************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0060                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Numero de Paletes                                        *
* Criado por: Sérgio Garcias                                           *
* Criado em.: 20/05/2005                                               *
* Tipo PRG..: Report                                                   *
************************************************************************

REPORT  zwmrep0060 MESSAGE-ID zwmmsg001.

************************************************************************
** Tabelas DD
************************************************************************
TABLES : vbsk, vbss, vbpa, adrc, t311, usr01, zwm026.

************************************************************************
** Types e Includes
************************************************************************
TYPE-POOLS: slis.

INCLUDE rlmobinc.

************************************************************************
** Tabelas Internas
************************************************************************

** Items das várias remessas
DATA: BEGIN OF l_lips OCCURS 0.
        INCLUDE STRUCTURE zpalete_picking.
        DATA:   descricao LIKE zwm026-descricao.
DATA: quant_remessa(20) TYPE i,  " DECIMALS 3.
      pal_inc(20)       TYPE i,
      uni_inc(20)       TYPE i,
      pal_comp(20)      TYPE i.
DATA : END OF l_lips.

DATA: l_likp LIKE likp OCCURS 0 WITH HEADER LINE.


DATA: BEGIN OF lt_lips OCCURS 0.
DATA:   wadat LIKE likp-wadat_ist.
DATA:   orgvd LIKE likp-vkorg.
        INCLUDE STRUCTURE l_lips.
        DATA : END OF lt_lips.

************************************************************************
*   Dados p/ALV
************************************************************************
DATA: fieldcat_linha TYPE slis_fieldcat_alv,
      fieldcat_tab   TYPE slis_t_fieldcat_alv,
      wa_eventos     TYPE slis_alv_event,
      eventos        TYPE slis_t_event,
      layout         TYPE slis_layout_alv,
      is_variant     TYPE disvariant,
      reprepid       TYPE slis_reprep_id,
      it_sort        TYPE slis_t_sortinfo_alv,
      is_sort        TYPE slis_sortinfo_alv,
      grid_title     TYPE lvc_title.

************************************************************************
** Variáveis
************************************************************************
DATA: idx      LIKE sy-tabix,
      l_act    LIKE zwm_aux-actualiza,
      programa LIKE sy-repid.

************************************************************************
** Constantes
************************************************************************

************************************************************************
** Parâmetros de entrada
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-000.
SELECTION-SCREEN: BEGIN OF BLOCK b0 WITH FRAME TITLE text-008.

SELECTION-SCREEN SKIP.
SELECT-OPTIONS : s_vkorg FOR likp-vkorg.
SELECT-OPTIONS : s_wadat FOR likp-wadat_ist OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN SKIP 1.
PARAMETERS p_varia LIKE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK blk1.

PARAMETERS p_cwidth DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_hrzgln DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_vrtgln DEFAULT 'X' NO-DISPLAY.

************************************************************************
** Initialization
************************************************************************


************************************************************************
** At Selection-screen.
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  is_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = is_variant
    IMPORTING
      es_variant    = is_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc = 0.
    p_varia = is_variant-variant.
  ELSE.
    MESSAGE i000 WITH 'Ainda não existem variantes de exibição'.
  ENDIF.

AT SELECTION-SCREEN.

************************************************************************
** Start-of-Selection
************************************************************************
START-OF-SELECTION.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = text-001.

  PERFORM user_own_data.
*check se user esta ou nao associado ao armazem
  IF xuser-lgnum IS INITIAL.
    MESSAGE e003.
  ENDIF.

  PERFORM seleciona_items_remessa_ind.

  CLEAR l_act.
** Cálculo das paletes
  CALL FUNCTION 'ZWM_PAL_PICKING_COMPLETE'
    EXPORTING
      i_lgnum         = xuser-lgnum
      i_actualiza     = l_act
    TABLES
      zpalete_picking = l_lips[].

  CLEAR lt_lips.
  REFRESH lt_lips.

  LOOP AT l_lips.
    MOVE-CORRESPONDING l_lips TO lt_lips.
    APPEND lt_lips.
  ENDLOOP.
*--------------------------------------------------ins 17Mar
  PERFORM get_desc.
*----------------------------------------------------------

  PERFORM change_dados.

************************************************************************
** End-of-Selection
************************************************************************
END-OF-SELECTION.


** Prepara Listagem
  PERFORM ajusta_propriedades.

** Colunas Listagem
  PERFORM catalogo.

** Ordenação da listagem
  PERFORM get_layout_sort CHANGING it_sort.

** Lista Dados
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat              = fieldcat_tab
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      it_events                = eventos
      is_layout                = layout
      it_sort                  = it_sort
      i_grid_title             = grid_title
      i_callback_pf_status_set = 'PFSTATUS'
      is_variant               = is_variant
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_program       = programa
      i_save                   = 'X'
      i_background_id          = 'ALV_BACKGROUND'
    TABLES
*     t_outtab                 = l_lips
      t_outtab                 = lt_lips
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  CHECK sy-ucomm = '&F03'.

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_PROPRIEDADES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ajusta_propriedades.

  layout-colwidth_optimize = p_cwidth.
  layout-window_titlebar = sy-title.
  layout-zebra = 'X'.
  layout-no_vline = p_vrtgln.
  layout-no_vline = p_vrtgln.
  layout-def_status = 'A'.

  programa = sy-repid.

** Eventos a capturar
  REFRESH eventos.
  wa_eventos-name = 'USER_COMMAND'.
  wa_eventos-form = 'HOTSPOT_ACTIVO'.
  APPEND wa_eventos TO eventos.

ENDFORM.                    " AJUSTA_PROPRIEDADES

*&---------------------------------------------------------------------*
*&      Form   CATALOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  catalogo.

  FREE fieldcat_tab.
  CLEAR fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'WADAT'.
  fieldcat_linha-reptext_ddic = 'Data Saída'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
*  fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'ORGVD'.
  fieldcat_linha-reptext_ddic = 'Org. Vendas'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
*  fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'REFNR'.
  fieldcat_linha-reptext_ddic = 'Nº Grupo'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
*  fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'KUNAG'.
  fieldcat_linha-reptext_ddic = 'Cod.Emissor'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'VBELN'.
  fieldcat_linha-reptext_ddic = 'Nº Remessa'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
*  fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'MATNR'.
  fieldcat_linha-reptext_ddic = 'Material'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
*  fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'DESCRICAO'.
  fieldcat_linha-reptext_ddic = 'Descrição'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'QUANT_REMESSA'.
  fieldcat_linha-reptext_ddic = 'Qtd.Remessa'.
  fieldcat_linha-do_sum = 'X'.
  fieldcat_linha-just = 'R'.
  fieldcat_linha-datatype = 'QUAN'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'MEINS'.
  fieldcat_linha-reptext_ddic = 'Uni.Remessa'.
  fieldcat_linha-no_convext = 'X'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'PAL_COMP'.
  fieldcat_linha-reptext_ddic = 'Pal. Comp.'.
  fieldcat_linha-do_sum = 'X'.
  fieldcat_linha-just = 'R'.
  fieldcat_linha-datatype = 'QUAN'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'UNI_INC'.
  fieldcat_linha-reptext_ddic = 'Unid. Incomp.'.
  fieldcat_linha-do_sum = 'X'.
  fieldcat_linha-just = 'R'.
  fieldcat_linha-datatype = 'QUAN'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'PAL_INC'.
  fieldcat_linha-reptext_ddic = 'Pal. Incomp.'.
  fieldcat_linha-do_sum = 'X'.
  fieldcat_linha-just = 'R'.
  fieldcat_linha-datatype = 'QUAN'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'VOLUM_ACUMULADO'.
  fieldcat_linha-reptext_ddic = 'Volume'.
  fieldcat_linha-do_sum = 'X'.
  fieldcat_linha-just = 'R'.
  fieldcat_linha-datatype = 'QUAN'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'NOME_EMISSOR'.
  fieldcat_linha-reptext_ddic = 'Emissor Mercad.'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'KUNNR'.
  fieldcat_linha-reptext_ddic = 'Cod.Recebedor'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'NOME_RECEBEDOR'.
  fieldcat_linha-reptext_ddic = 'Rec. Mercad.'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

ENDFORM.                    "catalogo

************************************************************************
*   Form PFSTATUS
************************************************************************
FORM pfstatus USING lt_extab.

  SET PF-STATUS 'STANDARD'.

ENDFORM.                    "pfstatus

*** PROCESSO DE EVENTOS
*&---------------------------------------------------------------------*
*&      Form TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.

  STATICS: lt_list_commentary TYPE slis_t_listheader,
           ls_list_commentary TYPE slis_listheader.

  DATA: l_datum(10),
        l_linhas    LIKE sy-tabix,
        l_uzeit(8).

  FREE: lt_list_commentary.
  CLEAR: lt_list_commentary.

  DESCRIBE TABLE l_lips LINES l_linhas.

  CASE usr01-datfm.
    WHEN '1' OR '2' OR '3'.
      WRITE sy-datum TO l_datum USING EDIT MASK '__.__.____'.
    WHEN OTHERS.
      WRITE sy-datum TO l_datum USING EDIT MASK '____.__.__'.
  ENDCASE.

  WRITE sy-uzeit TO l_uzeit USING EDIT MASK '__:__:__'.

* Título
  ls_list_commentary-typ  = 'H'.
  ls_list_commentary-info = 'Número Paletes'.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'A'.
  ls_list_commentary-info = 'Warehouse Management'.
  APPEND ls_list_commentary TO lt_list_commentary.

* Informações adicionais
  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-004.
  ls_list_commentary-info = sy-uname.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-005.
  ls_list_commentary-info = l_datum.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-006.
  ls_list_commentary-info = l_uzeit.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-007.
  ls_list_commentary-info = l_linhas.
  APPEND ls_list_commentary TO lt_list_commentary.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_list_commentary
      i_logo             = 'RENOVA_LOGO'.

ENDFORM.                    " TOP_OF_PAGE

************************************************************************
*   Form HOTSPOT_ACTIVO
************************************************************************
FORM hotspot_activo USING r_ucomm
                 CHANGING ls_selfield TYPE slis_selfield.

*  DATA: l_qmnum LIKE qmel-qmnum.
*
*  READ TABLE itab INDEX ls_selfield-tabindex.
*
*  CHECK sy-subrc EQ 0.
*
*  CHECK r_ucomm = '&IC1'.
*
*  CASE ls_selfield-fieldname.
*    WHEN 'QMNUM'.
*      CHECK NOT itab-qmnum IS INITIAL.
*      SET PARAMETER ID 'IQM' FIELD itab-qmnum.
*      CALL TRANSACTION 'QM02' AND SKIP FIRST SCREEN.
*
*** Converter a nota para o formato interno
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = itab-qmnum
*        IMPORTING
*          output = l_qmnum.
*
*** Verificar se a Nota possui observações
*      PERFORM check_obs USING l_qmnum
*                        CHANGING itab-obs.
*
*** Verificar se a Nota possui motivos de desperdício
*      PERFORM check_motivo USING l_qmnum
*                           CHANGING itab-motivo1
*                                    itab-motivo2.
*
*      MODIFY itab INDEX ls_selfield-tabindex.
*
*    WHEN 'FERTAUFNR'.
*      CHECK NOT itab-fertaufnr IS INITIAL.
*      SET PARAMETER ID 'ANR' FIELD itab-fertaufnr.
*      CALL TRANSACTION 'CO03' AND SKIP FIRST SCREEN.
*  ENDCASE.

  ls_selfield-refresh = 'X'.

ENDFORM.                    "hotspot_activo

*&---------------------------------------------------------------------*
*&      Form  seleciona_remessas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_remessas .


*** Verifica se o(s) grupo(s) de remessas existe(m)
*  SELECT refnr sammg FROM t311 INTO TABLE l_t311
*           WHERE refnr IN s_grupo.
*  IF sy-subrc <> 0.
*    MESSAGE i095.
*    EXIT.
*  ELSE.
*** Carregar remessas respectivas ao grupo
*    SELECT * FROM vbss "INTO TABLE l_vbss
*             FOR ALL ENTRIES IN l_t311 WHERE sammg = l_t311-sammg.
*      IF sy-subrc <> 0.
*        MESSAGE i096.
*        EXIT.
*      ELSE.
*        l_vbss-sammg = vbss-sammg.
*        l_vbss-vbeln = vbss-vbeln.
*        l_vbss-sortf = vbss-sortf.
*        READ TABLE l_t311 WITH KEY sammg = vbss-sammg.
*        l_vbss-refnr = l_t311-refnr.
*        APPEND l_vbss.
*      ENDIF.
*    ENDSELECT.
*  ENDIF.

ENDFORM.                    " seleciona_remessas
*&---------------------------------------------------------------------*
*&      Form  seleciona_items_remessa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_items_remessa .

*  IF NOT l_vbss[] IS INITIAL.
*
*    LOOP AT l_vbss.
*** Selecionar os items da(s) remessa(s)
*      SELECT SINGLE *  FROM likp
*               WHERE vbeln = l_vbss-vbeln.
*
*      MOVE-CORRESPONDING likp TO l_lips.
*
*      SELECT *  FROM lips
*               WHERE vbeln = l_vbss-vbeln AND
*                     ( pstyv <> 'ZPAS' AND
*                       pstyv <> 'ZPAL' ).
*        IF sy-subrc = 0.
*          MOVE-CORRESPONDING lips TO l_lips.
*          l_lips-sammg = l_vbss-sammg.
*          l_lips-refnr = l_vbss-refnr.
*          CLEAR : vbpa-kunnr.
*** Recebedor
*          SELECT SINGLE adrnr kunnr FROM vbpa
*          INTO (vbpa-adrnr, vbpa-kunnr)
*          WHERE vbeln = lips-vbeln
*            AND parvw = 'WE'.
*          IF sy-subrc = 0.
*            SELECT SINGLE name1 FROM adrc INTO l_lips-nome_recebedor
*                                WHERE addrnumber = vbpa-adrnr.
*            CLEAR : vbpa-adrnr.
*            IF l_lips-nome_recebedor IS INITIAL.
*** Leitura à KNA1
*              SELECT SINGLE name1 FROM kna1 INTO l_lips-nome_recebedor
*                                  WHERE kunnr = vbpa-kunnr.
*            ENDIF.
*            l_lips-kunnr = vbpa-kunnr.
*          ENDIF.
*
*** Emissor
*          SELECT SINGLE adrnr kunnr FROM vbpa INTO (vbpa-adrnr,
*          vbpa-kunnr)
*                              WHERE vbeln = lips-vbeln AND
*                                    parvw = 'AG'.
*          IF sy-subrc = 0.
*            SELECT SINGLE name1 FROM adrc INTO l_lips-nome_emissor
*                                WHERE addrnumber = vbpa-adrnr.
*            CLEAR : vbpa-adrnr.
*            IF l_lips-nome_emissor IS INITIAL.
*** Leitura à KNA1
*              SELECT SINGLE name1 FROM kna1 INTO l_lips-nome_emissor
*                                  WHERE kunnr = vbpa-kunnr.
*            ENDIF.
*            l_lips-kunag = vbpa-kunnr.
*            CLEAR : vbpa-kunnr.
*          ENDIF.
*
*          APPEND l_lips.
*
*        ENDIF.
*      ENDSELECT.
*    ENDLOOP.
*  ENDIF.

ENDFORM.                    " seleciona_items_remessa
*&---------------------------------------------------------------------*
*&      Form  seleciona_items_remessa_ind
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_items_remessa_ind .

  DATA : save_index LIKE sy-tabix.
  CLEAR : save_index.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE l_lips BYPASSING BUFFER
           FROM lips AS s INNER JOIN likp AS p
           ON p~vbeln = s~vbeln
           WHERE p~vkorg IN s_vkorg AND
                 p~wadat_ist IN s_wadat AND
                 ( s~pstyv <> 'ZPAS' AND
                   s~pstyv <> 'ZPAL' ).
  IF sy-subrc = 0.
** Verificar se estas remessas já têm algum grupo de remessas
** atribuído
    LOOP AT l_lips.
      save_index = sy-tabix.

      SELECT * FROM vbss
               WHERE vbeln = l_lips-vbeln.
        SELECT SINGLE * FROM vbsk
                        WHERE sammg = vbss-sammg AND
                              smart = 'W'.
        IF sy-subrc = 0.
** Grupo de WM
          l_lips-refnr = vbsk-sammg.
        ELSE.
** Grupo de MM
          l_lips-sammg = vbss-sammg.
        ENDIF.
      ENDSELECT.

      CLEAR : vbpa-kunnr.
** Recebedor
      SELECT SINGLE adrnr FROM vbpa INTO vbpa-adrnr
                          WHERE vbeln = l_lips-vbeln AND
                                parvw = 'WE'.
      IF sy-subrc = 0.
        SELECT SINGLE name1 FROM adrc INTO l_lips-nome_recebedor
                            WHERE addrnumber = vbpa-adrnr.
        CLEAR : vbpa-adrnr.
        IF l_lips-nome_recebedor IS INITIAL.
** Leitura à KNA1
          SELECT SINGLE name1 FROM kna1 INTO l_lips-nome_recebedor
                              WHERE kunnr = vbpa-kunnr.
          l_lips-kunnr = vbpa-kunnr.
          CLEAR : vbpa-kunnr.
        ENDIF.
      ENDIF.

      CLEAR : vbpa-kunnr.
** Emissor
      SELECT SINGLE adrnr FROM vbpa INTO vbpa-adrnr
                          WHERE vbeln = l_lips-vbeln AND
                                parvw = 'AG'.
      IF sy-subrc = 0.
        SELECT SINGLE name1 FROM adrc INTO l_lips-nome_emissor
                            WHERE addrnumber = vbpa-adrnr.
        CLEAR : vbpa-adrnr.
        IF l_lips-nome_emissor IS INITIAL.
** Leitura à KNA1
          SELECT SINGLE name1 FROM kna1 INTO l_lips-nome_emissor
                              WHERE kunnr = vbpa-kunnr.
          l_lips-kunag = vbpa-kunnr.
          CLEAR : vbpa-kunnr.
        ENDIF.
      ENDIF.

      MODIFY l_lips INDEX save_index.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " seleciona_items_remessa_ind
*&---------------------------------------------------------------------*
*&      Form  get_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_desc .

  DATA: l_tabix LIKE sy-tabix.

  LOOP AT lt_lips.
    CLEAR l_tabix. l_tabix = sy-tabix.
** descrição do Material
    SELECT SINGLE maktx INTO lt_lips-descricao FROM  makt
           WHERE  matnr  = lt_lips-matnr
           AND    spras  = sy-langu.

    IF sy-subrc = 0.
      MODIFY lt_lips INDEX l_tabix.
    ENDIF.
** Organização de Vendas + Data Saída
    SELECT SINGLE vkorg wadat_ist
        INTO (lt_lips-orgvd, lt_lips-wadat)
            FROM  likp
               WHERE  vbeln  = lt_lips-vbeln.

    IF sy-subrc = 0.
      MODIFY lt_lips INDEX l_tabix.
    ENDIF.
  ENDLOOP.

*  LOOP AT l_lips.
*    CLEAR l_tabix. l_tabix = sy-tabix.
*** descrição do Material
*    SELECT SINGLE maktx INTO l_lips-descricao FROM  makt
*           WHERE  matnr  = l_lips-matnr
*           AND    spras  = sy-langu.
*
*    IF sy-subrc = 0.
*      MODIFY l_lips INDEX l_tabix.
*    ENDIF.
*** Organização de Vendas + Data Saída
*    SELECT SINGLE vkorg wadat_ist
*        INTO (l_lips-orgvd, l_lips-wadat)
*            FROM  likp
*               WHERE  vbeln  = l_lips-vbeln.
*
*    IF sy-subrc = 0.
*      MODIFY l_lips INDEX l_tabix.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " get_desc

*&---------------------------------------------------------------------*
*&      Form  get_layout_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LAYOUT  text
*      <--P_SORT  text
*----------------------------------------------------------------------*
FORM get_layout_sort CHANGING p_sort TYPE slis_t_sortinfo_alv.

  DATA l_sort TYPE slis_sortinfo_alv.

** Ordenação
  FREE p_sort.
  l_sort-spos = 1.
  l_sort-fieldname = 'WADAT'.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  FREE p_sort.
  l_sort-spos = 2.
  l_sort-fieldname = 'ORGVD'.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  FREE p_sort.
  l_sort-spos = 3.
  l_sort-fieldname = 'REFNR'.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  CLEAR: l_sort.
  l_sort-spos = 4.
  l_sort-fieldname = 'KUNAG'.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  CLEAR: l_sort.
  l_sort-spos = 5.
  l_sort-fieldname = 'VBELN'.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

ENDFORM.                    " get_layout_sort
*&---------------------------------------------------------------------*
*&      Form  change_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_dados .

  LOOP AT lt_lips.

** RL -> INS 03.05.2005
    lt_lips-quant_remessa = lt_lips-lfimg.
    lt_lips-pal_inc = lt_lips-pal_incompleta.
    lt_lips-uni_inc = lt_lips-uni_incompleta.
    lt_lips-pal_comp = lt_lips-pal_completa.
** RL <- INS 03.05.2005

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lt_lips-refnr
      IMPORTING
        output = lt_lips-refnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lt_lips-vbeln
      IMPORTING
        output = lt_lips-vbeln.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lt_lips-matnr
      IMPORTING
        output = lt_lips-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lt_lips-kunnr
      IMPORTING
        output = lt_lips-kunnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lt_lips-kunag
      IMPORTING
        output = lt_lips-kunag.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = lt_lips-meins
        language       = sy-langu
      IMPORTING
        output         = lt_lips-meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    MODIFY lt_lips.
  ENDLOOP.


*  LOOP AT l_lips.
*
*** RL -> INS 03.05.2005
*    l_lips-quant_remessa = l_lips-lfimg.
*    l_lips-pal_inc = l_lips-pal_incompleta.
*    l_lips-uni_inc = l_lips-uni_incompleta.
*    l_lips-pal_comp = l_lips-pal_completa.
*** RL <- INS 03.05.2005
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = l_lips-refnr
*      IMPORTING
*        output = l_lips-refnr.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = l_lips-vbeln
*      IMPORTING
*        output = l_lips-vbeln.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = l_lips-matnr
*      IMPORTING
*        output = l_lips-matnr.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = l_lips-kunnr
*      IMPORTING
*        output = l_lips-kunnr.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = l_lips-kunag
*      IMPORTING
*        output = l_lips-kunag.
*
*    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*      EXPORTING
*        input          = l_lips-meins
*        language       = sy-langu
*      IMPORTING
*        output         = l_lips-meins
*      EXCEPTIONS
*        unit_not_found = 1
*        OTHERS         = 2.
*
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    MODIFY l_lips.
*  ENDLOOP.

ENDFORM.                    " change_dados
