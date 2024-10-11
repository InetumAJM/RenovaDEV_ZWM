************************************************************************
*                                                                      *
* Criação                                                              *
*  Autor: Ricardo Lopes - ROFF                                         *
*  Data:  28.04.2005                                                   *
*  Descrição: Lista cargas que já possuem saída de mercadoria.         *
*
* Modificações
*  Autor:
*  Data:
*  Descrição:
*  Pesquisar alterações por:
*
************************************************************************

REPORT zwmrep0051 MESSAGE-ID zwmmsg001 .

************************************************************************
** Tabelas DD
************************************************************************
TABLES: likp, t311a, tvkot, usr01.

************************************************************************
** Types e Includes
************************************************************************
TYPE-POOLS: slis.

************************************************************************
** Tabelas Internas
************************************************************************
DATA: BEGIN OF itab OCCURS 0,
        vkorg     LIKE tvkot-vkorg,
        vtext     LIKE tvkot-vtext,
        wadat_ist LIKE likp-wadat_ist,
        num_reg   LIKE sy-tabix,
      END OF itab.

DATA: BEGIN OF itab_dados OCCURS 0,
        vkorg     LIKE tvkot-vkorg,
        vbeln     LIKE likp-vbeln,
        wadat_ist LIKE likp-wadat_ist,
      END OF itab_dados.

DATA: BEGIN OF itab_aux OCCURS 0,
        refnr     LIKE t311-refnr,
      END OF itab_aux.

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
      programa  LIKE sy-repid.

************************************************************************
** Constantes
************************************************************************
*constants: .

************************************************************************
** Parâmetros de entrada
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-000.
PARAMETERS: p_lgnum LIKE t311a-lgnum DEFAULT '100' OBLIGATORY.
SELECT-OPTIONS: s_vkorg FOR tvkot-vkorg,
                s_data  FOR sy-datum OBLIGATORY.
**
SELECTION-SCREEN SKIP 1.
PARAMETERS p_varia LIKE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK blk1.
**
PARAMETERS p_cwidth DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_hrzgln DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_vrtgln DEFAULT 'X' NO-DISPLAY.

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

************************************************************************
** Start-of-Selection
************************************************************************
START-OF-SELECTION.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = text-001.

  CLEAR: usr01.

  SELECT SINGLE * FROM usr01
  WHERE bname = sy-uname.

** Obtem dados a listar
  PERFORM get_dados.

  IF itab[] IS INITIAL.
    MESSAGE i000 WITH
    'Não existem dados para as opções indicadas'.
    EXIT.
  ENDIF.

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
      t_outtab                 = itab
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  CHECK sy-ucomm = '&F03'.

*&---------------------------------------------------------------------*
*&      Form  get_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dados .

  FREE: itab, itab_dados, itab_aux.
  CLEAR: itab, itab_dados, itab_aux.

** Obter as remessas criadas para a data e org de vendas selecionados
  SELECT * FROM likp
  INTO CORRESPONDING FIELDS OF TABLE itab_dados
  WHERE vkorg IN s_vkorg
    AND wadat_ist IN s_data.

  SORT itab_dados BY wadat_ist.

  LOOP AT itab_dados.
    CLEAR itab.
    MOVE-CORRESPONDING itab_dados TO itab.

** Só o 1º grupo da remessa
    CLEAR: t311a.
    SELECT * FROM t311a
    WHERE lgnum EQ p_lgnum
      AND rbnum EQ itab_dados-vbeln.
      itab_aux-refnr = t311a-refnr.
      EXIT.
    ENDSELECT.

    CHECK sy-subrc EQ 0.

** Verifica se o grupo já existe
    READ TABLE itab_aux WITH KEY refnr = t311a-refnr BINARY SEARCH.

    CHECK sy-subrc NE 0.

    COLLECT itab_aux.
    CLEAR: itab_aux.
    SORT itab_aux.

** Obter a descrição da org. de vendas
    CLEAR: tvkot.
    SELECT SINGLE vtext FROM tvkot
    INTO itab-vtext
    WHERE spras EQ sy-langu
      AND vkorg EQ itab_dados-vkorg.

    itab-num_reg = 1.
    COLLECT itab.
    CLEAR: itab.
  ENDLOOP.

  SORT itab BY wadat_ist vkorg.

ENDFORM.                    " get_dados

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
  fieldcat_linha-fieldname = 'WADAT_IST'.
  fieldcat_linha-reptext_ddic = 'Data saída'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-datatype = 'DATS'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'VKORG'.
  fieldcat_linha-reptext_ddic = 'Org.Vendas'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'VTEXT'.
  fieldcat_linha-reptext_ddic = 'Nome'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'NUM_REG'.
  fieldcat_linha-reptext_ddic = 'Total'.
  fieldcat_linha-just = 'R'.
  fieldcat_linha-datatype = 'QUANT'.
  fieldcat_linha-do_sum = 'X'.
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
        l_linhas LIKE sy-tabix,
        l_uzeit(8).

  FREE: lt_list_commentary.
  CLEAR: lt_list_commentary.

  DESCRIBE TABLE itab LINES l_linhas.

  CASE usr01-datfm.
    WHEN '1' OR '2' OR '3'.
      WRITE sy-datum TO l_datum USING EDIT MASK '__.__.____'.
    WHEN OTHERS.
      WRITE sy-datum TO l_datum USING EDIT MASK '____.__.__'.
  ENDCASE.

  WRITE sy-uzeit TO l_uzeit USING EDIT MASK '__:__:__'.

* Título
  ls_list_commentary-typ  = 'H'.
  ls_list_commentary-info = text-002.
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

*  INCLUDE zbci_eventos_alv.

  CASE r_ucomm.
    WHEN 'REFR'.
      PERFORM get_dados.
  ENDCASE.

  ls_selfield-refresh = 'X'.

ENDFORM.                    "hotspot_activo

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
  l_sort-fieldname = 'WADAT_IST'.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

ENDFORM.                    " get_layout_sort
