************************************************************************
*                                                                      *
* Criação                                                              *
*  Autor: Ricardo Lopes - ROFF                                         *
*  Data:  12.05.2005                                                   *
*  Descrição: Gestão de Cargas e Descargas.                            *
*
* Modificações
*  Autor:
*  Data:
*  Descrição:
*  Pesquisar alterações por:
*
************************************************************************

REPORT zwmrep0058 MESSAGE-ID zwmmsg001 .

************************************************************************
** Tabelas DD
************************************************************************
TABLES: zwm006_aux, vttk, vttp, likp, adrc, kna1, lfa1, t311a, vbsk,
        usr01, tvkot.

************************************************************************
** Types e Includes
************************************************************************
TYPE-POOLS: slis.

************************************************************************
** Tabelas Internas
************************************************************************
DATA: BEGIN OF itab OCCURS 0,
        tknum       LIKE vttk-tknum,  "Nº transporte
        dpreg       LIKE vttk-dpreg,  "Data
        tdlnr       LIKE vttk-tdlnr,  "Agente de Frete
        signi       LIKE vttk-signi,  "Matricula
        num_entrada LIKE zwm006_aux-num_entrada, "Talão
        data_reg    LIKE zwm006_aux-data_reg,
        hora_reg    LIKE zwm006_aux-hora_reg,
        refnr       LIKE t311-refnr,  "Grupo
        vbeln       LIKE vttp-vbeln,  "Remessa
        vkorg       LIKE likp-vkorg,  "Org. Vendas
        kunnr_ag    LIKE kna1-kunnr,  "Emissor
        kunnr_we    LIKE kna1-kunnr,  "Recebdor
        nome_ag     LIKE adrc-name1,
        nome_we     LIKE adrc-name1,
        nome_trn    LIKE adrc-name1,
        vtext       LIKE vbsk-vtext,  "Descrição Grupo
        nome_vkg    LIKE tvkot-vtext,
        btgew       LIKE likp-btgew,
        gewei       LIKE likp-gewei,
        observacoes LIKE zwm006_aux-observacoes,
      END OF itab.

DATA: itab_dados LIKE itab OCCURS 0 WITH HEADER LINE,
      gt_adrc    LIKE adrc OCCURS 0 WITH HEADER LINE.

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
PARAMETERS: p_lgnum LIKE ltak-lgnum DEFAULT '100' OBLIGATORY.
SELECT-OPTIONS: s_vkorg FOR likp-vkorg,
                s_tknum FOR vttk-tknum,
                s_dpreg FOR vttk-dpreg OBLIGATORY.
**
SELECTION-SCREEN SKIP 1.
PARAMETERS p_varia LIKE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK blk1.
**
PARAMETERS p_cwidth DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_hrzgln DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_vrtgln DEFAULT 'X' NO-DISPLAY.

************************************************************************
** Initialization
************************************************************************
INITIALIZATION.

  s_dpreg-sign = 'I'.
  s_dpreg-option = 'BT'.
  s_dpreg-low = sy-datum - 7.
  s_dpreg-high = sy-datum.
  APPEND s_dpreg.

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

  DATA: l_adrnr LIKE adrc-addrnumber.

  FREE: itab, itab_dados.
  CLEAR: itab, itab_dados.

  SELECT *
  FROM vttk AS h INNER JOIN vttp AS i ON h~tknum = i~tknum
         INTO CORRESPONDING FIELDS OF TABLE itab_dados
         WHERE h~tknum IN s_tknum
           AND h~dpreg IN s_dpreg
           AND h~stdis EQ 'X'
           AND h~stten EQ ' '.

  CHECK sy-subrc EQ 0.

  LOOP AT itab_dados.
    CLEAR: itab.
    MOVE-CORRESPONDING itab_dados TO itab.

** Dados das remessas
    CLEAR: likp.
    SELECT SINGLE * FROM likp
    WHERE vbeln EQ itab_dados-vbeln
      AND vkorg IN s_vkorg.

    CHECK sy-subrc EQ 0.

    itab-kunnr_ag = likp-kunnr.
    itab-kunnr_we = likp-kunag.
    itab-vkorg    = likp-vkorg.
    itab-btgew    = likp-btgew.
    itab-gewei    = likp-gewei.

** Descrição Org. Vendas
    CLEAR: tvkot.
    SELECT SINGLE vtext FROM tvkot
    INTO itab-nome_vkg
    WHERE spras EQ sy-langu
      AND vkorg EQ itab-vkorg.

** Nome do Recebedor
    FREE: gt_adrc.
    CLEAR: gt_adrc, kna1, l_adrnr.

    SELECT SINGLE * FROM kna1
    WHERE kunnr EQ itab-kunnr_ag.

    l_adrnr = kna1-adrnr.

    CALL FUNCTION 'ADDR_SELECT_ADRC_SINGLE'
      EXPORTING
        addrnumber        = l_adrnr
      TABLES
        et_adrc           = gt_adrc
      EXCEPTIONS
        address_not_exist = 1
        parameter_error   = 2
        internal_error    = 3
        OTHERS            = 4.

    IF sy-subrc = 0.
      READ TABLE gt_adrc INDEX 1.
      IF sy-subrc = 0.
        itab-nome_ag = gt_adrc-name1.
      ELSE.
        itab-nome_ag = kna1-name1.
      ENDIF.
    ENDIF.

** Nome do Emissor
    IF itab-kunnr_ag EQ itab-kunnr_we.
      itab-nome_we = itab-nome_ag.
    ELSE.
      FREE: gt_adrc.
      CLEAR: gt_adrc, kna1, l_adrnr.

      SELECT SINGLE * FROM kna1
      WHERE kunnr EQ itab-kunnr_ag.

      l_adrnr = kna1-adrnr.

      CALL FUNCTION 'ADDR_SELECT_ADRC_SINGLE'
        EXPORTING
          addrnumber        = l_adrnr
        TABLES
          et_adrc           = gt_adrc
        EXCEPTIONS
          address_not_exist = 1
          parameter_error   = 2
          internal_error    = 3
          OTHERS            = 4.

      IF sy-subrc = 0.
        READ TABLE gt_adrc INDEX 1.
        IF sy-subrc = 0.
          itab-nome_we = gt_adrc-name1.
        ELSE.
          itab-nome_we = kna1-name1.
        ENDIF.
      ENDIF.
    ENDIF.

** Nome do transportador
    FREE: gt_adrc.
    CLEAR: gt_adrc, lfa1, l_adrnr.

    SELECT SINGLE * FROM lfa1
    WHERE lifnr EQ itab-tdlnr.

    l_adrnr = lfa1-adrnr.

    CALL FUNCTION 'ADDR_SELECT_ADRC_SINGLE'
      EXPORTING
        addrnumber        = l_adrnr
      TABLES
        et_adrc           = gt_adrc
      EXCEPTIONS
        address_not_exist = 1
        parameter_error   = 2
        internal_error    = 3
        OTHERS            = 4.

    IF sy-subrc = 0.
      READ TABLE gt_adrc INDEX 1.
      IF sy-subrc = 0.
        itab-nome_trn = gt_adrc-name1.
      ELSE.
        itab-nome_trn = lfa1-name1.
      ENDIF.
    ENDIF.

** Dados do Talão
    CLEAR: zwm006_aux.
    SELECT * FROM zwm006_aux
    WHERE armazem      EQ p_lgnum
      AND n_transporte EQ itab_dados-tknum
      AND finalizada   EQ space.

      itab-num_entrada = zwm006_aux-num_entrada.
      itab-data_reg    = zwm006_aux-data_reg.
      itab-hora_reg    = zwm006_aux-hora_reg.
      itab-observacoes = zwm006_aux-observacoes.
      EXIT.
    ENDSELECT.

** Dados do Grupo
    CLEAR: t311a.
    SELECT * FROM t311a
    WHERE lgnum EQ p_lgnum
      AND rbtyp EQ 'L'
      AND rbnum EQ itab-vbeln.
      itab-refnr = t311a-refnr.
      EXIT.
    ENDSELECT.

** Descrição do grupo
    CLEAR: vbsk.
    SELECT SINGLE vtext FROM vbsk
    INTO itab-vtext
    WHERE sammg EQ itab-refnr.

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
  fieldcat_linha-fieldname = 'TKNUM'.
  fieldcat_linha-reptext_ddic = 'Nº de Transporte'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-key = 'X'.
  fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'DPREG'.
  fieldcat_linha-reptext_ddic = 'Data Planeada'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-datatype = 'DATS'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'TDLNR'.
  fieldcat_linha-reptext_ddic = 'Agente de Frete'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'NOME_TRN'.
  fieldcat_linha-reptext_ddic = 'Nome Agente'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'SIGNI'.
  fieldcat_linha-reptext_ddic = 'Matrícula'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'NUM_ENTRADA'.
  fieldcat_linha-reptext_ddic = 'Talão'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'DATA_REG'.
  fieldcat_linha-reptext_ddic = 'Data Entrada Talão'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-datatype = 'DATS'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'HORA_REG'.
  fieldcat_linha-reptext_ddic = 'Hora Entrada Talão'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-datatype = 'TIMS'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'REFNR'.
  fieldcat_linha-reptext_ddic = 'Grupo'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'VTEXT'.
  fieldcat_linha-reptext_ddic = 'Descrição'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'VKORG'.
  fieldcat_linha-reptext_ddic = 'Org. Vendas'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'NOME_VKG'.
  fieldcat_linha-reptext_ddic = 'Descrição Org. Vendas'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'VBELN'.
  fieldcat_linha-reptext_ddic = 'Remessa'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'KUNNR_AG'.
  fieldcat_linha-reptext_ddic = 'Emissor'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'NOME_AG'.
  fieldcat_linha-reptext_ddic = 'Nome Emissor'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'KUNNR_WE'.
  fieldcat_linha-reptext_ddic = 'Nome Recebedor'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'NOME_WE'.
  fieldcat_linha-reptext_ddic = 'Nome Recebedor'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'BTGEW'.
  fieldcat_linha-reptext_ddic = 'Peso'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'GEWEI'.
  fieldcat_linha-reptext_ddic = 'Uni'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'OBSERVACOES'.
  fieldcat_linha-reptext_ddic = 'Observações'.
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

*  DATA: l_idx   LIKE sy-tabix,
*        l_tabix LIKE sy-tabix.

  READ TABLE itab INDEX ls_selfield-tabindex.

  CASE r_ucomm.
    WHEN '&IC1'.
      CASE ls_selfield-fieldname.
        WHEN 'TKNUM'.
          CHECK NOT itab-tknum IS INITIAL.
          SET PARAMETER ID 'TNR' FIELD itab-tknum.
          CALL TRANSACTION 'VT02N' AND SKIP FIRST SCREEN.

        WHEN 'REFNR'.
          CHECK NOT itab-refnr IS INITIAL.
          SET PARAMETER ID 'GRN' FIELD itab-refnr.
          CALL TRANSACTION 'VG03' AND SKIP FIRST SCREEN.

        WHEN 'VBELN'.
          CHECK NOT itab-vbeln IS INITIAL.
          SET PARAMETER ID 'VL' FIELD itab-vbeln.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.

      ENDCASE.
    WHEN 'REFR'.
      PERFORM get_dados.
  ENDCASE.

  ls_selfield-refresh = 'X'.

ENDFORM.                    "hotspot_activo
