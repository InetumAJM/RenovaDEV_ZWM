************************************************************************
*                                                                      *
* Criação                                                              *
*  Autor: Fernando Lopes - ROFF                                        *
*  Data:  24.11.2005                                                   *
*  Descrição: Mapa de User's VS Picking                                *
*
************************************************************************
*
* Modificações
*  Autor:
*  Data:
*  Descrição:
*  Pesquisar alterações por:
*
************************************************************************

REPORT zwmrep0065 MESSAGE-ID zwmmsg001 .

************************************************************************
** Tabelas DD
************************************************************************
TABLES: usr02.

************************************************************************
** Types e Includes
************************************************************************
TYPE-POOLS: slis.

************************************************************************
** Tabelas Internas
************************************************************************
DATA: BEGIN OF itab OCCURS 0,
        marca,
        armazem      LIKE zwm026-armazem,
        grupo        LIKE zwm026-grupo,
        remessa      LIKE zwm026-remessa,
        material     LIKE zwm026-material,
        descricao    LIKE zwm026-descricao,
        to_number    LIKE zwm026-to_number,
        estado       LIKE zwm026-estado,
        status_to(4),
        sscc         LIKE zwm026-sscc,
      END OF itab.

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
      programa LIKE sy-repid,
      l_aut.

************************************************************************
** Constantes
************************************************************************
CONSTANTS: c_verde(4)    VALUE '@5B@',
           c_amarelo(4)  VALUE '@5D@',
           c_vermelho(4) VALUE '@5C@'.

************************************************************************
** Parâmetros de entrada
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-000.
PARAMETERS: p_lgnum LIKE ltak-lgnum  OBLIGATORY DEFAULT '100',
            p_user  LIKE usr02-bname OBLIGATORY.
**
SELECTION-SCREEN SKIP 1.
PARAMETERS p_varia LIKE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK blk1.
**
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b03.
PARAMETERS: p_vis RADIOBUTTON GROUP list DEFAULT 'X'.
PARAMETERS: p_mod RADIOBUTTON GROUP list.
SELECTION-SCREEN END OF BLOCK b1.
**
PARAMETERS p_cwidth DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_hrzgln DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_vrtgln DEFAULT 'X' NO-DISPLAY.

************************************************************************
** Initialization
************************************************************************
*INITIALIZATION.

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

  IF p_mod = 'X'.
    PERFORM check_bloqueios.
  ENDIF.

** Obtem dados a listar
  PERFORM get_dados.

  IF itab[] IS INITIAL.
    MESSAGE i000 WITH 'Não existem dados para as opções indicadas'.
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

  DATA: BEGIN OF it_dados OCCURS 0,
          armazem   LIKE zwm026-armazem,
          grupo     LIKE zwm026-grupo,
          remessa   LIKE zwm026-remessa,
          material  LIKE zwm026-material,
          descricao LIKE zwm026-descricao,
          to_number LIKE zwm026-to_number,
          estado    LIKE zwm026-estado,
          sscc      LIKE zwm026-sscc,
        END OF it_dados.

  DATA: BEGIN OF it_ltak OCCURS 0,
          tanum LIKE ltak-tanum,
          kquit LIKE ltak-kquit,
        END OF it_ltak.

  DATA: BEGIN OF it_ltap OCCURS 0,
          tanum LIKE ltap-tanum,
          pvqui LIKE ltap-pvqui,
        END OF it_ltap.

  REFRESH: itab, it_dados, it_ltak, it_ltap.
  CLEAR:   itab, it_dados, it_ltak, it_ltap.

  SELECT * FROM zwm026
           INTO CORRESPONDING FIELDS OF TABLE it_dados
          WHERE armazem   EQ p_lgnum
            AND user_name EQ p_user
            AND estado    NE 'T'.

  CHECK NOT it_dados[] IS INITIAL.

  SELECT tanum kquit FROM ltak
     INTO CORRESPONDING FIELDS OF TABLE it_ltak
     FOR ALL ENTRIES IN it_dados
     WHERE lgnum EQ p_lgnum
       AND tanum EQ it_dados-to_number.

  SORT it_ltak BY tanum.

  SELECT tanum pvqui FROM ltap
     INTO CORRESPONDING FIELDS OF TABLE it_ltap
     FOR ALL ENTRIES IN it_dados
     WHERE lgnum EQ p_lgnum
       AND tanum EQ it_dados-to_number.

  SORT it_ltak BY tanum.

  LOOP AT it_dados.
    MOVE-CORRESPONDING it_dados TO itab.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = it_dados-material
      IMPORTING
        output = itab-material.

    READ TABLE it_ltak WITH KEY tanum = it_dados-to_number.
    IF sy-subrc EQ 0.
      READ TABLE it_ltap WITH KEY tanum = it_dados-to_number.
      IF sy-subrc EQ 0.
        IF it_ltak-kquit EQ 'X' AND it_ltap-pvqui EQ ' '.
          itab-status_to = c_verde.
        ELSEIF it_ltak-kquit EQ 'X' AND it_ltap-pvqui EQ 'X'.
          itab-status_to = c_verde.
        ELSEIF it_ltak-kquit EQ ' ' AND it_ltap-pvqui EQ 'X'.
          itab-status_to = c_amarelo.
        ELSEIF it_ltak-kquit EQ ' ' AND it_ltap-pvqui EQ ' '.
          itab-status_to = c_vermelho.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND itab.
    CLEAR: itab.
  ENDLOOP.

  SORT itab BY grupo remessa material to_number.

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
  layout-window_titlebar   = sy-title.
  layout-zebra             = 'X'.
  layout-no_vline          = p_vrtgln.
  layout-no_vline          = p_vrtgln.
  layout-def_status        = 'A'.

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
  fieldcat_linha-reptext_ddic = 'SEL.'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-key = 'X'.
  fieldcat_linha-checkbox = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'ARMAZEM'.
  fieldcat_linha-reptext_ddic = 'ARMAZÉM'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'GRUPO'.
  fieldcat_linha-reptext_ddic = 'GRUPO'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'REMESSA'.
  fieldcat_linha-reptext_ddic = 'REMESSA'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'MATERIAL'.
  fieldcat_linha-reptext_ddic = 'MATERIAL'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'DESCRICAO'.
  fieldcat_linha-reptext_ddic = 'DESCRIÇÃO'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'TO_NUMBER'.
  fieldcat_linha-reptext_ddic = 'TO'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'ESTADO'.
  fieldcat_linha-reptext_ddic = 'ESTADO'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'STATUS_TO'.
  fieldcat_linha-reptext_ddic = 'STATUS TO'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-icon = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'SSCC'.
  fieldcat_linha-reptext_ddic = 'SSCC'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.


ENDFORM.                    "catalogo

************************************************************************
*   Form PFSTATUS
************************************************************************
FORM pfstatus USING lt_extab.

  IF p_mod = 'X'.
    SET PF-STATUS 'STANDARD'.
  ELSE.
    SET PF-STATUS 'STANDARD_EXIB'.
  ENDIF.

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
        l_uzeit(8),
        l_datfm     LIKE usr01-datfm.

  FREE:  lt_list_commentary.
  CLEAR: lt_list_commentary, l_datfm.

  DESCRIBE TABLE itab LINES l_linhas.

  SELECT SINGLE datfm FROM usr01 INTO l_datfm
    WHERE bname = sy-uname.

  CASE l_datfm.
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
  ls_list_commentary-info = p_user.
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

  DATA: BEGIN OF it_corr OCCURS 0,
          to_number LIKE zwm026-to_number,
          estado    LIKE zwm026-estado,
        END OF it_corr.

  DATA: l_idx   LIKE sy-tabix,
        l_tabix LIKE sy-tabix.

** Limpa a selecção no modo exibir
  IF p_vis = 'X'.
    LOOP AT itab WHERE marca EQ 'X'.
      CLEAR: itab-marca.
      MODIFY itab.
    ENDLOOP.
  ENDIF.

  CASE r_ucomm.
    WHEN '&IC1'.
      CHECK ls_selfield-fieldname = 'TO_NUMBER'.
      READ TABLE itab INDEX ls_selfield-tabindex.
      CHECK sy-subrc EQ 0.
      CHECK NOT itab-to_number IS INITIAL.
      SET PARAMETER ID 'TAN' FIELD itab-to_number.
*     SET PARAMETER ID 'TAP' FIELD '0001'.
      SET PARAMETER ID 'LGN' FIELD p_lgnum.
      CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN.

    WHEN 'CORRIGE'.
      IF p_mod EQ 'X'.
        REFRESH it_corr.
        CLEAR   it_corr.

        LOOP AT itab WHERE marca  = 'X'.
          IF itab-status_to EQ c_verde.
            it_corr-to_number = itab-to_number.
            it_corr-estado    = 'T'.
            APPEND it_corr.
            CLEAR  it_corr.
          ELSEIF itab-status_to EQ c_amarelo AND itab-estado <> 'P'.
            it_corr-to_number = itab-to_number.
            it_corr-estado    = 'P'.
            APPEND it_corr.
            CLEAR  it_corr.
          ELSEIF itab-status_to EQ c_vermelho AND itab-estado <> 'C'.
            it_corr-to_number = itab-to_number.
            it_corr-estado    = 'C'.
            APPEND it_corr.
            CLEAR  it_corr.
          ENDIF.
        ENDLOOP.

        IF NOT it_corr[] IS INITIAL.
          LOOP AT it_corr.
            UPDATE zwm026 SET estado = it_corr-estado
                    WHERE armazem   EQ p_lgnum
                      AND user_name EQ p_user
                      AND to_number EQ it_corr-to_number.
            IF sy-subrc EQ 0.
              COMMIT WORK.
            ENDIF.
          ENDLOOP.
** Refresh
          REFRESH: itab.
          CLEAR:   itab.
          PERFORM get_dados.
        ENDIF.
      ENDIF.

    WHEN 'REFR'.
      REFRESH: itab.
      CLEAR:   itab.
      PERFORM get_dados.

  ENDCASE.

  ls_selfield-refresh = 'X'.

ENDFORM.                    "hotspot_activo

*&---------------------------------------------------------------------*
*&      Form  check_bloqueios
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_bloqueios .

  DATA: l_gtarg LIKE seqg3-gtarg,
        l_user  LIKE sy-uname.

** Verifica se já existe bloqueio e bloqueia se livre
  CONCATENATE sy-mandt 'ZWMREP0065' INTO l_gtarg.
  CONDENSE l_gtarg NO-GAPS.

  CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = l_gtarg
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    l_user = sy-msgv1.
    CLEAR: p_mod.
    p_vis = 'X'.
    MESSAGE s000 WITH 'O Prog. já está a ser processado pelo usuário'
                      l_user.
    EXIT.
  ENDIF.

ENDFORM.                    " check_bloqueios
