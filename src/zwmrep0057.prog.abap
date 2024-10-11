************************************************************************
*                                                                      *
* Criação                                                              *
*  Autor: Ricardo Lopes - ROFF                                         *
*  Data:  12.05.2005                                                   *
*  Descrição: Eliminar bloqueios da tabela ZWM011.                     *
*
* Modificações
*  Autor:
*  Data:
*  Descrição:
*  Pesquisar alterações por:
*
************************************************************************

REPORT zwmrep0057 MESSAGE-ID zwmmsg001 .

************************************************************************
** Tabelas DD
************************************************************************
TABLES: zwm011, ltak, usr01.

************************************************************************
** Types e Includes
************************************************************************
TYPE-POOLS: slis.

************************************************************************
** Tabelas Internas
************************************************************************
DATA: BEGIN OF itab OCCURS 0.
DATA: marca.
        INCLUDE STRUCTURE zwm011.
DATA: status_to(4).
DATA: END OF itab.

DATA: itab_dados LIKE itab   OCCURS 0 WITH HEADER LINE,
      itab_del   LIKE zwm011 OCCURS 0 WITH HEADER LINE.

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
*constants: .

************************************************************************
** Parâmetros de entrada
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-000.
PARAMETERS: p_lgnum LIKE ltak-lgnum OBLIGATORY DEFAULT '100'.
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

  SELECT * FROM zwm011
  INTO CORRESPONDING FIELDS OF TABLE itab_dados
  WHERE armazem EQ p_lgnum.

  LOOP AT itab_dados.

    MOVE-CORRESPONDING itab_dados TO itab.

    CLEAR: ltak.
    SELECT SINGLE * FROM ltak
    WHERE lgnum EQ p_lgnum
      AND tanum EQ itab_dados-to_number.
*      AND tapos EQ itab_dados-to_item.

    IF ltak-kquit = ' '.
      itab-status_to = '@5C@'. "Criada
    ELSE.
*      IF ltap-pquit = 'X'.
      itab-status_to = '@5B@'. "Finalizada
*      ELSE.
*        itab-status_to = '@5D@'. "Picada
*      ENDIF.
    ENDIF.

    APPEND itab.
    CLEAR: itab.
  ENDLOOP.

  SORT itab BY equipamento.

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
  fieldcat_linha-fieldname = 'EQUIPAMENTO'.
  fieldcat_linha-reptext_ddic = 'EQUIPAMENTO'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'USER_NAME'.
  fieldcat_linha-reptext_ddic = 'USER'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'TO_NUMBER'.
  fieldcat_linha-reptext_ddic = 'TO'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-key = 'X'.
  fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'TO_ITEM'.
  fieldcat_linha-reptext_ddic = 'ITEM'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'STATUS'.
  fieldcat_linha-reptext_ddic = 'STATUS'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'STATUS_TO'.
  fieldcat_linha-reptext_ddic = 'STATUS TO'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-icon = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'QUEUE'.
  fieldcat_linha-reptext_ddic = 'QUEUE'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'ULTIMO_TIPO_DEP'.
  fieldcat_linha-reptext_ddic = 'TIPO DEP.'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'ULTIMO_BIN'.
  fieldcat_linha-reptext_ddic = 'BIN'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'TERMINAL_ID'.
  fieldcat_linha-reptext_ddic = 'TERMINAL'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'HOST_ADDR'.
  fieldcat_linha-reptext_ddic = 'HOST'.
  fieldcat_linha-just = 'C'.
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

  DATA: l_idx   LIKE sy-tabix,
        l_tabix LIKE sy-tabix.

** Não permitir selecionar linhas que tenham erros
  CLEAR: itab_del.
  FREE: itab_del.
  SORT itab_del.

  IF p_mod = 'X'.
    LOOP AT itab WHERE marca  = 'X'.
      l_idx = sy-tabix.
      IF itab-status_to = '@5B@'. "Finalizadas
        MOVE-CORRESPONDING itab TO itab_del.
        APPEND itab_del.
        CLEAR itab_del.
      ELSE.
        CLEAR itab-marca.
        MESSAGE i000 WITH 'Somente TO finalizadas podem ser'
                          'selecionadas'.
        MODIFY itab INDEX l_idx.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT itab.
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
      CHECK NOT itab-to_item IS INITIAL.
      SET PARAMETER ID 'TAN' FIELD itab-to_number.
*      SET PARAMETER ID 'TAP' FIELD itab-to_item.
      SET PARAMETER ID 'LGN' FIELD p_lgnum.
      CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN.

    WHEN 'REFR'.
      FREE: itab.
      CLEAR: itab.
      PERFORM get_dados.

    WHEN 'ELIMINAR'.
      IF p_mod = 'X'.
        IF NOT itab_del[] IS INITIAL.
          PERFORM elimina_zwm011.
          PERFORM get_dados.
        ELSE.
          MESSAGE i000 WITH 'Para eliminar registos tem que selecionar'
                            'TO finalizadas'.
        ENDIF.
      ENDIF.
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

  DATA: itab_enq LIKE seqg3 OCCURS 0 WITH HEADER LINE,
        l_user   LIKE sy-uname.

*  CLEAR: l_aut.

  DATA: lv_gtarg TYPE eqegtarg.
  CONCATENATE 'G_ZWM011_' p_lgnum INTO lv_gtarg.
  CONDENSE lv_gtarg NO-GAPS.

** Verifica se já existe bloqueio
  CALL FUNCTION 'ENQUEUE_READ'
    EXPORTING
      gclient               = sy-mandt
      gname                 = 'KEYWORD'
    TABLES
      enq                   = itab_enq
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    LOOP AT itab_enq WHERE gtarg EQ lv_gtarg.
      l_user = itab_enq-guname.
*      l_aut = 'X'.
      MESSAGE i000 WITH 'Prog. já está a ser processado pelo user'
            l_user 'apenas possibilidade de exibição'.
      CLEAR: p_mod.
      p_vis = 'X'.
      EXIT.
    ENDLOOP.
  ENDIF.

** Ainda não existe bloqueio, vai criar o bloqueio
  CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = lv_gtarg
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    IF itab_enq[] IS INITIAL.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      l_user = sy-msgv1.
*      l_aut = 'X'.

      MESSAGE i000 WITH 'Prog. já está a ser processado pelo user'
            l_user 'apenas possibilidade de exibição'.
    ENDIF.
    CLEAR: p_mod.
    p_vis = 'X'.
  ENDIF.

ENDFORM.                    " check_bloqueios
*&---------------------------------------------------------------------*
*&      Form  elimina_zwm011
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM elimina_zwm011 .

  DELETE zwm011 FROM TABLE itab_del.

  COMMIT WORK AND WAIT.

ENDFORM.                    " elimina_zwm011
