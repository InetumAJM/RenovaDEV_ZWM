************************************************************************
*                                                                      *
* Criação                                                              *
*  Autor: Ricardo Lopes - ROFF                                         *
*  Data:  11.04.2005                                                   *
*  Descrição: Lista e apaga dados da tabele ZWM013 e ZWM020.           *
*
* Modificações
*  Autor:
*  Data:
*  Descrição:
*  Pesquisar alterações por:
*
************************************************************************

REPORT zwmrep0048 MESSAGE-ID zmsg .

************************************************************************
** Tabelas DD
************************************************************************
TABLES: zwm013, lein, zwm020, usr01, vekp.

************************************************************************
** Types e Includes
************************************************************************
TYPE-POOLS: slis.

************************************************************************
** Tabelas Internas
************************************************************************
DATA: BEGIN OF itab OCCURS 0.
DATA:   marca.
        INCLUDE STRUCTURE zwm013.
DATA:   p2    LIKE zwm020-p2,
        bin   LIKE zwm020-bin,
        erdat LIKE vekp-erdat,
      END OF itab.

DATA: itab_dados LIKE itab OCCURS 0 WITH HEADER LINE,
      itab_del   LIKE itab OCCURS 0 WITH HEADER LINE.

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
SELECT-OPTIONS: s_sscc  FOR zwm013-sscc,
                s_erdat FOR sy-datum.
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

  FREE: itab_del.
  CLEAR: itab_del, usr01.

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

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat              = fieldcat_tab
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      it_events                = eventos
      is_layout                = layout
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

  FREE: itab, itab_dados.
  CLEAR: itab, itab_dados.

  SELECT * FROM zwm013
  INTO CORRESPONDING FIELDS OF TABLE itab_dados
  WHERE sscc    IN s_sscc
    AND destino EQ space.

  LOOP AT itab_dados.
    CLEAR: itab.

    MOVE-CORRESPONDING itab_dados TO itab.

    SELECT SINGLE * FROM vekp
    WHERE exidv EQ itab_dados-sscc
      AND erdat IN s_erdat.

    CHECK sy-subrc EQ 0.

    CLEAR: zwm020.
    SELECT * FROM zwm020
    WHERE armazem EQ '100'
      AND p1      EQ itab_dados-sscc.
      itab-p2  = zwm020-p2.
      itab-bin = zwm020-bin.
    ENDSELECT.

    itab-erdat = vekp-erdat.

    APPEND itab.
  ENDLOOP.

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
  fieldcat_linha-fieldname = 'MARCA'.
  fieldcat_linha-reptext_ddic = 'MARCA'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-key = 'X'.
  fieldcat_linha-checkbox = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'SSCC'.
  fieldcat_linha-reptext_ddic = 'SSCC(1)'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
*  fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'P2'.
  fieldcat_linha-reptext_ddic = 'SSCC(2)'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'BIN'.
  fieldcat_linha-reptext_ddic = 'BIN'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'ERDAT'.
  fieldcat_linha-reptext_ddic = 'DT.REGISTO'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-datatype = 'DATS'.
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

  INCLUDE zbci_eventos_alv.

** Não permitir selecionar linhas que tenham erros
  CLEAR: itab_del.
  FREE: itab_del.
  SORT itab_del.

  LOOP AT itab WHERE marca = 'X'.
    MOVE-CORRESPONDING itab TO itab_del.
    APPEND itab_del.
    CLEAR itab_del.
  ENDLOOP.

  CASE r_ucomm.
    WHEN 'REFR'.
      FREE: itab.
      CLEAR: itab.
      PERFORM get_dados.

    WHEN 'DEL'.
      PERFORM apaga_zwm013.
      PERFORM apaga_zwm020.

      PERFORM get_dados.

  ENDCASE.

  ls_selfield-refresh = 'X'.

ENDFORM.                    "hotspot_activo
*&---------------------------------------------------------------------*
*&      Form  apaga_zwm013
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM apaga_zwm013 .

  DATA: wa_del LIKE zwm013.

  LOOP AT itab_del.
    MOVE-CORRESPONDING itab_del TO wa_del.
    DELETE zwm013 FROM wa_del.
    CLEAR: wa_del.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.                    " apaga_zwm013
*&---------------------------------------------------------------------*
*&      Form  apaga_zwm020
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM apaga_zwm020 .

  DATA: wa_zwm020 LIKE zwm020.

  LOOP AT itab_del.
    wa_zwm020-armazem = itab_del-armazem.
    wa_zwm020-p1  = itab_del-sscc.
    wa_zwm020-p2  = itab_del-p2.
    wa_zwm020-bin = itab_del-bin.

    DELETE zwm020 FROM wa_zwm020.
    CLEAR: wa_zwm020.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.                    " apaga_zwm020
