************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0062                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Visualização Estado das Mensulas                         *
* Criado por: Sérgio Garcias                                           *
* Criado em.: 30/05/2005                                               *
* Tipo PRG..: Report                                                   *
************************************************************************

REPORT zwmrep0062 MESSAGE-ID zwmmsg001 .

************************************************************************
** Tabelas DD
************************************************************************
TABLES: zwm011, ltak, ltap, usr01, lqua.

************************************************************************
** Types e Includes
************************************************************************
TYPE-POOLS: slis.

************************************************************************
** Tabelas Internas
************************************************************************
DATA: BEGIN OF itab OCCURS 0.
DATA: marca.
        INCLUDE STRUCTURE zwm014.
DATA: status_to(4).
DATA: in_out(1).
DATA: matnr LIKE ltap-matnr.
DATA: maktx LIKE ltap-maktx.
DATA: vltyp LIKE ltap-vltyp.
DATA: vlpla LIKE ltap-vlpla.
DATA: nltyp LIKE ltap-nltyp.
DATA: nlpla LIKE ltap-nlpla.
DATA: refnr LIKE ltak-refnr.
DATA: END OF itab.

DATA: itab_dados LIKE itab   OCCURS 0 WITH HEADER LINE,
      itab_del   LIKE zwm014 OCCURS 0 WITH HEADER LINE,
      l_ltak     LIKE ltak OCCURS 0 WITH HEADER LINE,
      l_ltap     LIKE ltap OCCURS 0 WITH HEADER LINE,
      l_lqua     LIKE lqua OCCURS 0 WITH HEADER LINE.

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
      programa  LIKE sy-repid,
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

  SELECT * INTO CORRESPONDING FIELDS OF TABLE itab_dados
      FROM zwm014
          WHERE armazem EQ p_lgnum.

  LOOP AT itab_dados.

    MOVE-CORRESPONDING itab_dados TO itab.

    IF NOT itab_dados-su IS INITIAL.

      CLEAR lqua.
      SELECT SINGLE *
         FROM lqua
             WHERE lgnum = p_lgnum AND
                   lenum = itab_dados-su.

      IF sy-subrc = 0.

        CLEAR ltap.
        SELECT SINGLE *
            FROM ltap
                WHERE lgnum = lqua-lgnum AND
                      tanum = lqua-btanr.

      ELSE.

        CLEAR:   l_ltap, l_ltak.
        REFRESH: l_ltap, l_ltak.

        SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_ltap
          FROM ltap
              WHERE lgnum = p_lgnum AND
                    vlenr = itab_dados-su.

        SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_ltap
          FROM ltap
              WHERE lgnum = p_lgnum AND
                    nlenr = itab_dados-su.

*       DELETE l_ltap WHERE vorga EQ 'ST'.

        IF NOT l_ltap[] IS INITIAL.
          SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_ltak
              FROM ltak
                FOR ALL ENTRIES IN l_ltap
                  WHERE lgnum = l_ltap-lgnum AND
                        tanum = l_ltap-tanum.


          SORT l_ltak BY bdatu DESCENDING
                         bzeit DESCENDING.
          READ TABLE l_ltak INDEX 1.

          CLEAR ltap.
          SELECT SINGLE *
              FROM ltap
                  WHERE lgnum = l_ltak-lgnum AND
                        tanum = l_ltak-tanum.
        ELSE.
          CLEAR ltap.
        ENDIF.

      ENDIF.

      MOVE-CORRESPONDING ltap TO itab.
      MOVE ltap-tanum TO itab-to_number.
      MOVE ltap-tapos TO itab-to_item.

      IF ltap-vltyp = 'TRI'.
        itab-in_out = 'S'.
      ELSEIF ltap-nltyp = 'TRI'.
        itab-in_out = 'E'.
      ENDIF.

      CLEAR ltak.
      SELECT SINGLE *
          FROM ltak
              WHERE lgnum EQ ltap-lgnum AND
                    tanum EQ ltap-tanum.

      IF ltak-kquit = ' '.
        itab-status_to = '@5C@'. "Criada
      ELSE.
        itab-status_to = '@5B@'. "Finalizada
      ENDIF.

      itab-refnr = ltak-refnr.

    ENDIF.

    APPEND itab.
    CLEAR: itab.
  ENDLOOP.

  SORT itab BY mensula.

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
  layout-window_titlebar = 'Estado das Mensulas'.
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
  fieldcat_linha-reptext_ddic = 'Sel.'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-key = 'X'.
  fieldcat_linha-checkbox = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'ARMAZEM'.
  fieldcat_linha-reptext_ddic = 'Armazem'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'MENSULA'.
  fieldcat_linha-reptext_ddic = 'Mensula'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'SU'.
  fieldcat_linha-reptext_ddic = 'Su'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'TO_NUMBER'.
  fieldcat_linha-reptext_ddic = 'To'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-key = 'X'.
  fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'TO_ITEM'.
  fieldcat_linha-reptext_ddic = 'To_Item'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'STATUS_TO'.
  fieldcat_linha-reptext_ddic = 'Status To'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-icon = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'PRIORIDADE'.
  fieldcat_linha-reptext_ddic = 'Prioridade'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'IN_OUT'.
  fieldcat_linha-reptext_ddic = 'Ent\Saida'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'MATNR'.
  fieldcat_linha-reptext_ddic = 'Material'.
  fieldcat_linha-no_zero = 'X'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'MAKTX'.
  fieldcat_linha-reptext_ddic = 'Descrição'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'VLTYP'.
  fieldcat_linha-reptext_ddic = 'St.Origem'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'VLPLA'.
  fieldcat_linha-reptext_ddic = 'Pos.Origem'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'NLTYP'.
  fieldcat_linha-reptext_ddic = 'St.Destino'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'NLPLA'.
  fieldcat_linha-reptext_ddic = 'Pos.Destino'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'REFNR'.
  fieldcat_linha-reptext_ddic = 'Grupo'.
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
        CLEAR itab_del-su.
        CLEAR itab_del-estado.
        CLEAR itab_del-to_number.
        CLEAR itab_del-to_item.
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
          PERFORM elimina_zwm014.
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
    LOOP AT itab_enq WHERE gtarg EQ 'G_ZWM014'.
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
      keyword_       = 'G_ZWM014'
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
*&      Form  elimina_zwm014
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM elimina_zwm014 .

  CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = 'ZWM014'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  MODIFY zwm014 FROM TABLE itab_del.

  COMMIT WORK AND WAIT.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = 'ZWM014'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

ENDFORM.                    " elimina_zwm014
