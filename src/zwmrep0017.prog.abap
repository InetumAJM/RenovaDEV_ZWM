************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0017                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Criação de paletes de picking - WM                       *
* Criado por: Bruno Simões                                             *
* Criado em.: 03/02/2004                                               *
* Tipo PRG..: Report                                                   *
************************************************************************
* Modificado: Fernando Lopes                                           *
* Em........: 13/01/2006                                               *
* Motivo....: Optimização performance                                  *
************************************************************************

REPORT  zwmrep0017 MESSAGE-ID zwmmsg001.

************************************************************************
** Tabelas DD
************************************************************************
TABLES : vbsk, vbss, vbpa, t311, usr01, zwm026.

************************************************************************
** Types e Includes
************************************************************************
TYPE-POOLS: slis.

INCLUDE rlmobinc.

************************************************************************
** Tabelas Internas
************************************************************************
** Remessas pertencentes ao grupo
DATA: BEGIN OF l_vbss OCCURS 0.
        INCLUDE STRUCTURE vbss.
        DATA:   descricao LIKE zwm026-descricao.            "ins Mar2005
DATA : refnr LIKE t311-refnr.
DATA : END OF l_vbss.

** Items das várias remessas
DATA: BEGIN OF l_lips OCCURS 0.
        INCLUDE STRUCTURE zpalete_picking.
        DATA:   descricao LIKE zwm026-descricao.
** RL -> INS 03.05.2005
DATA: quant_remessa(20) TYPE i,  " DECIMALS 3.
      pal_inc(20)       TYPE i,
      uni_inc(20)       TYPE i,
      pal_comp(20)      TYPE i.
** RL <- INS 03.05.2005
DATA : END OF l_lips.

** Remessas pertencentes ao grupo
DATA : BEGIN OF l_t311 OCCURS 0,
         refnr LIKE t311-refnr,
         sammg LIKE t311-sammg.
DATA : END OF l_t311.

DATA: lt_zwm026 TYPE TABLE OF zwm026,
      ls_zwm026 TYPE zwm026.

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
*constants: .

************************************************************************
** Parâmetros de entrada
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-000.
SELECTION-SCREEN: BEGIN OF BLOCK b0 WITH FRAME TITLE text-008.
** Grupos de remessas
SELECTION-SCREEN SKIP.
SELECT-OPTIONS : s_grupo FOR t311-refnr .
SELECTION-SCREEN END OF BLOCK b0.

** Remessas individuais
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS : s_rem FOR likp-vbeln NO-EXTENSION,
                 s_dat FOR likp-lddat NO-EXTENSION,
                 s_hor FOR likp-lduhr NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.

**
SELECTION-SCREEN SKIP 1.
PARAMETERS p_varia LIKE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK blk1.
**
PARAMETERS p_noalv  DEFAULT ''  NO-DISPLAY.
PARAMETERS p_cwidth DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_hrzgln DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_vrtgln DEFAULT 'X' NO-DISPLAY.

************************************************************************
** Initialization
************************************************************************
*initialization.

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
    MESSAGE i000 WITH 'Ainda não existem variantes de exibição'(009).
  ENDIF.

AT SELECTION-SCREEN.

  IF NOT s_rem IS INITIAL AND NOT s_grupo IS INITIAL.
    MESSAGE e097.
    EXIT.
  ENDIF.

************************************************************************
** Start-of-Selection
************************************************************************
START-OF-SELECTION.

  IMPORT s_grupo FROM MEMORY ID 'ZWM024'.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = text-001.

  PERFORM user_own_data.
*check se user esta ou nao associado ao armazem
  IF xuser-lgnum IS INITIAL.
    MESSAGE e003.
  ENDIF.

  CLEAR zwm026.

  IF sy-tcode NE 'ZWM024'.
    SELECT SINGLE num_recorrido FROM zwm026
             INTO zwm026-num_recorrido
            WHERE armazem EQ xuser-lgnum
              AND grupo   IN s_grupo.

    IF sy-subrc = 0.
      IF NOT zwm026-num_recorrido IS INITIAL.
        IF p_noalv IS INITIAL.
          MESSAGE i183.
        ENDIF.
        EXIT.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM t311
    WHERE lgnum EQ xuser-lgnum
      AND refnr IN s_grupo.

    IF t311-lgnum <> '100'.
      IF NOT t311-kzerl IS INITIAL.
        IF p_noalv IS INITIAL.
          MESSAGE i195.
        ENDIF.
        EXIT.
      ENDIF.
    ENDIF.

    IF p_noalv EQ abap_true OR xuser-lgnum = '150'.
      l_act = 'X'.
    ENDIF.
  ELSE.
    CLEAR: l_act.
  ENDIF.

  IF NOT s_grupo IS INITIAL.
    PERFORM seleciona_remessas.
    PERFORM seleciona_items_remessa.
  ELSEIF NOT s_rem IS INITIAL.
    PERFORM seleciona_items_remessa_ind.
  ENDIF.

** Cálculo das paletes
  CALL FUNCTION 'ZWM_PAL_PICKING_COMPLETE'
    EXPORTING
      i_lgnum         = xuser-lgnum
      i_actualiza     = l_act
    TABLES
      zpalete_picking = l_lips[].

**  correção entradas duplicadas.
  LOOP AT l_lips WHERE sub_item = '000002'.
    CLEAR: l_lips-lfimg.
    CLEAR: l_lips-volum.
    CLEAR: l_lips-volum_acumulado.
    MODIFY l_lips INDEX sy-tabix.
  ENDLOOP.


*--------------------------------------------------ins 17Mar
  PERFORM get_desc.
*----------------------------------------------------------

  PERFORM change_dados.

************************************************************************
** End-of-Selection
************************************************************************
END-OF-SELECTION.

** So visualiza qd não é para actualizar
*  CHECK l_act IS INITIAL.

** Prepara Listagem
  PERFORM ajusta_propriedades.

** Colunas Listagem
  PERFORM catalogo.

** Ordenação da listagem
  PERFORM get_layout_sort CHANGING it_sort.

** Lista Dados
  IF p_noalv EQ abap_false.
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
        t_outtab                 = l_lips
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.

    CHECK sy-ucomm = '&F03'.
  ENDIF.

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
  fieldcat_linha-fieldname = 'REFNR'.
  fieldcat_linha-reptext_ddic = 'Nº Grupo'(010).
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
* fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'KUNAG'.
  fieldcat_linha-reptext_ddic = 'Cod.Emissor'(011).
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'VBELN'.
  fieldcat_linha-reptext_ddic = 'Nº Remessa'(012).
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
* fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'MATNR'.
  fieldcat_linha-reptext_ddic = 'Material'(013).
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
* fieldcat_linha-hotspot = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'DESCRICAO'.
  fieldcat_linha-reptext_ddic = 'Descrição'(014).
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'QUANT_REMESSA'.
  fieldcat_linha-reptext_ddic = 'Qtd.Remessa'(015).
  fieldcat_linha-do_sum = 'X'.
  fieldcat_linha-just = 'R'.
  fieldcat_linha-datatype = 'QUAN'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'MEINS'.
  fieldcat_linha-reptext_ddic = 'Uni.Remessa'(016).
  fieldcat_linha-no_convext = 'X'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'PAL_COMP'.
  fieldcat_linha-reptext_ddic = 'Pal. Comp.'(017).
  fieldcat_linha-do_sum = 'X'.
  fieldcat_linha-just = 'R'.
  fieldcat_linha-datatype = 'QUAN'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'UNI_INC'.
  fieldcat_linha-reptext_ddic = 'Unid. Incomp.'(018).
  fieldcat_linha-do_sum = 'X'.
  fieldcat_linha-just = 'R'.
  fieldcat_linha-datatype = 'QUAN'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'PAL_INC'.
  fieldcat_linha-reptext_ddic = 'Pal. Incomp.'(019).
  fieldcat_linha-do_sum = 'X'.
  fieldcat_linha-just = 'R'.
  fieldcat_linha-datatype = 'QUAN'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'PAL_PICKING'.
  fieldcat_linha-reptext_ddic = 'Pal. Picking'(020).
  fieldcat_linha-do_sum = 'X'.
  fieldcat_linha-just = 'R'.
  fieldcat_linha-datatype = 'QUAN'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'VOLUM_ACUMULADO'.
  fieldcat_linha-reptext_ddic = 'Volume'(021).
  fieldcat_linha-do_sum = 'X'.
  fieldcat_linha-just = 'R'.
  fieldcat_linha-datatype = 'QUAN'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'NOME_EMISSOR'.
  fieldcat_linha-reptext_ddic = 'Emissor Mercad.'(022).
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'KUNNR'.
  fieldcat_linha-reptext_ddic = 'Cod.Recebedor'(023).
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'NOME_RECEBEDOR'.
  fieldcat_linha-reptext_ddic = 'Rec. Mercad.'(024).
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

ENDFORM.                    "catalogo

************************************************************************
*   Form PFSTATUS
************************************************************************
FORM pfstatus USING lt_extab TYPE slis_t_extab.
  APPEND '&F12' TO lt_extab.
  APPEND '&F15' TO lt_extab.

  SET PF-STATUS 'STANDARD' EXCLUDING lt_extab.
*  SET PF-STATUS 'ZWM001'.


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
  ls_list_commentary-info = 'Paletes Completas e Paletes de Picking'(025).
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

** Verifica se o(s) grupo(s) de remessas existe(m)
  SELECT refnr sammg FROM t311 INTO TABLE l_t311
           WHERE lgnum EQ xuser-lgnum
             AND refnr IN s_grupo.
  IF sy-subrc <> 0.
    MESSAGE i095.
    EXIT.
  ELSE.
** Carregar remessas respectivas ao grupo
    SELECT * FROM vbss "INTO TABLE l_vbss
             FOR ALL ENTRIES IN l_t311 WHERE sammg = l_t311-sammg.
      IF sy-subrc <> 0.
        MESSAGE i096.
        EXIT.
      ELSE.
        l_vbss-sammg = vbss-sammg.
        l_vbss-vbeln = vbss-vbeln.
        l_vbss-sortf = vbss-sortf.
        READ TABLE l_t311 WITH KEY sammg = vbss-sammg.
        l_vbss-refnr = l_t311-refnr.
        APPEND l_vbss.
      ENDIF.
    ENDSELECT.
  ENDIF.

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

  CHECK NOT l_vbss[] IS INITIAL.

  DATA: it_likp LIKE likp OCCURS 0 WITH HEADER LINE,
        it_lips LIKE lips OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF it_vbpa OCCURS 0,
          vbeln LIKE vbpa-vbeln,
          adrnr LIKE vbpa-adrnr,
          kunnr LIKE vbpa-kunnr,
          parvw LIKE vbpa-parvw,
        END OF it_vbpa.

  DATA: BEGIN OF it_adrc OCCURS 0,
          addrnumber LIKE adrc-addrnumber,
          name1      LIKE adrc-name1,
        END OF it_adrc.

  DATA: BEGIN OF it_kna1 OCCURS 0,
          kunnr LIKE kna1-kunnr,
          name1 LIKE kna1-name1,
        END OF it_kna1.

  REFRESH: it_likp, it_lips, it_vbpa, it_adrc.
  CLEAR:   it_likp, it_lips, it_vbpa, it_adrc.

  SELECT * FROM likp
           INTO TABLE it_likp
            FOR ALL ENTRIES IN l_vbss
          WHERE vbeln = l_vbss-vbeln.

  SORT it_likp BY vbeln.

  SELECT * FROM lips
           INTO TABLE it_lips
            FOR ALL ENTRIES IN l_vbss
          WHERE vbeln = l_vbss-vbeln.

  DELETE it_lips WHERE pstyv = 'ZPAS'
                    OR pstyv = 'ZPAL'.

***************************************************
* Eliminar itens que não sejam do CD - Inicio
***************************************************
  DELETE it_lips WHERE lgort <> 'CD'.

***************************************************
* Eliminar itens que não sejam do CD - Fim
***************************************************
  SORT it_lips BY vbeln.

  IF NOT it_lips[] IS INITIAL.
    SELECT vbeln adrnr kunnr parvw FROM vbpa
           INTO CORRESPONDING FIELDS OF TABLE it_vbpa
            FOR ALL ENTRIES IN it_lips
          WHERE vbeln = it_lips-vbeln.

    DELETE it_vbpa WHERE parvw <> 'WE'
                     AND parvw <> 'AG'.

    SORT it_vbpa BY vbeln parvw.

  ENDIF.

  IF NOT it_vbpa[] IS INITIAL.

    SELECT addrnumber name1
           FROM adrc
           INTO CORRESPONDING FIELDS OF TABLE it_adrc
            FOR ALL ENTRIES IN it_vbpa
           WHERE addrnumber = it_vbpa-adrnr.

    SORT it_adrc BY addrnumber.

    SELECT kunnr name1
           FROM kna1
           INTO CORRESPONDING FIELDS OF TABLE it_kna1
            FOR ALL ENTRIES IN it_vbpa
          WHERE kunnr = it_vbpa-kunnr.

    SORT it_kna1 BY kunnr.

  ENDIF.


  LOOP AT l_vbss.

** Selecionar os items da(s) remessa(s)
    CLEAR it_likp.
    READ TABLE it_likp WITH KEY vbeln = l_vbss-vbeln BINARY SEARCH.

    MOVE-CORRESPONDING it_likp TO l_lips.

    LOOP AT it_lips WHERE vbeln = l_vbss-vbeln.

      MOVE-CORRESPONDING it_lips TO l_lips.
      l_lips-sammg = l_vbss-sammg.
      l_lips-refnr = l_vbss-refnr.

** Recebedor
      CLEAR: it_vbpa.
      READ TABLE it_vbpa WITH KEY vbeln = it_lips-vbeln
                                  parvw = 'WE'.
      IF sy-subrc = 0.
        l_lips-kunnr = it_vbpa-kunnr.

        CLEAR it_adrc.
        READ TABLE it_adrc WITH KEY addrnumber = it_vbpa-adrnr
                           BINARY SEARCH.

        l_lips-nome_recebedor = it_adrc-name1.

        IF l_lips-nome_recebedor IS INITIAL.

** Leitura à KNA1
          CLEAR it_kna1.
          READ TABLE it_kna1 WITH KEY kunnr = it_vbpa-kunnr
                             BINARY SEARCH.
          l_lips-nome_recebedor = it_kna1-name1.

        ENDIF.
      ENDIF.

** Emissor
      CLEAR it_vbpa.
      READ TABLE it_vbpa WITH KEY vbeln = it_lips-vbeln
                                  parvw = 'AG'.

      IF sy-subrc = 0.
        l_lips-kunag = it_vbpa-kunnr.

        CLEAR it_adrc.
        READ TABLE it_adrc WITH KEY addrnumber = it_vbpa-adrnr
                           BINARY SEARCH.
        l_lips-nome_emissor = it_adrc-name1.

        IF l_lips-nome_emissor IS INITIAL.

** Leitura à KNA1
          CLEAR it_kna1.
          READ TABLE it_kna1 WITH KEY kunnr = it_vbpa-kunnr
                             BINARY SEARCH.
          l_lips-nome_emissor = it_kna1-name1.

        ENDIF.

      ENDIF.

      APPEND l_lips.

    ENDLOOP.
  ENDLOOP.

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
           WHERE p~vbeln IN s_rem AND
                 p~lddat IN s_dat AND
                 p~lduhr IN s_hor AND
                 s~lgort = 'CD' AND
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

  DATA: BEGIN OF it_makt OCCURS 0,
          matnr LIKE makt-matnr,
          maktx LIKE makt-maktx,
        END OF it_makt.

  CHECK NOT l_lips[] IS INITIAL.

  CLEAR:   it_makt.
  REFRESH: it_makt.

  SELECT matnr maktx
         FROM  makt
         INTO CORRESPONDING FIELDS OF TABLE it_makt
          FOR ALL ENTRIES IN l_lips
        WHERE matnr = l_lips-matnr
          AND spras = sy-langu.

  SORT it_makt BY matnr.

  LOOP AT l_lips.
    l_tabix = sy-tabix.

** Descrição do Material
    READ TABLE it_makt WITH KEY matnr = l_lips-matnr BINARY SEARCH.

    IF sy-subrc = 0.
      l_lips-descricao = it_makt-maktx.
      MODIFY l_lips INDEX l_tabix.
    ENDIF.
  ENDLOOP.

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
  l_sort-fieldname = 'REFNR'.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  CLEAR: l_sort.
  l_sort-spos = 2.
  l_sort-fieldname = 'KUNAG'.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  CLEAR: l_sort.
  l_sort-spos = 3.
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

  LOOP AT l_lips.

** RL -> INS 03.05.2005
    l_lips-quant_remessa = l_lips-lfimg.
    l_lips-pal_inc = l_lips-pal_incompleta.
    l_lips-uni_inc = l_lips-uni_incompleta.
    l_lips-pal_comp = l_lips-pal_completa.
** RL <- INS 03.05.2005

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = l_lips-refnr
      IMPORTING
        output = l_lips-refnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = l_lips-vbeln
      IMPORTING
        output = l_lips-vbeln.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = l_lips-matnr
      IMPORTING
        output = l_lips-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = l_lips-kunnr
      IMPORTING
        output = l_lips-kunnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = l_lips-kunag
      IMPORTING
        output = l_lips-kunag.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = l_lips-meins
        language       = sy-langu
      IMPORTING
        output         = l_lips-meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    MODIFY l_lips.
  ENDLOOP.

ENDFORM.                    " change_dados

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command.
  BREAK-POINT.
ENDFORM.                    "user_command
