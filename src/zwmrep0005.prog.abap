************************************************************************
*                                                                      *
* Criação                                                              *
*  Autor: Ricardo Lopes - ROFF                                         *
*  Data:  19.05.2005                                                   *
*  Descrição: Cópia do programa ZWMREP0005 com base nas novas          *
*             funcionalidades da gestão da portaria.                   *
*
* Modificações
*  Autor:
*  Data:
*  Descrição:
*  Pesquisar alterações por:
*
************************************************************************

REPORT  zwmrep0005_new MESSAGE-ID zwmmsg001.

************************************************************************
** Types e Includes
************************************************************************
INCLUDE <icon>.

************************************************************************
** Tabelas DD
************************************************************************
TABLES : zwm005, zwm006_aux, vttp, likp, kna1, vbfa, tvkot, vttk, tvrot,
         tvsakt, zwm004.

************************************************************************
** Tabelas internas
************************************************************************
DATA: BEGIN OF ti_dados OCCURS 0,
          armazem      LIKE zwm006_aux-armazem,
          num_entrada  LIKE zwm006_aux-num_entrada,
          n_transporte LIKE zwm006_aux-n_transporte,
          matricula    LIKE zwm006_aux-matricula,
          hora_reg     LIKE zwm006_aux-hora_reg,
          data_reg     LIKE zwm006_aux-data_reg,
          hora_ent     LIKE zwm004-hora_saida,
          data_ent     LIKE zwm004-data,
          hora_saida   LIKE zwm006_aux-hora_saida,
          data_saida   LIKE zwm006_aux-data_saida,
          dalen        LIKE vttk-dalen,   "hora final do transporte
          porta        LIKE zwm006_aux-porta,
** Verde - finalizada
** Amarela - esta na porta mas ainda nao finalizada
** Vermelha - nao esta na porta e nao finalizada
          status(4),
          transportador LIKE vttk-tdlnr,
          desc_transp   LIKE zwm005-desc_transp,
          tipo_camiao   LIKE vttk-sdabw,
          bezei         LIKE tvsakt-bezei,      "desc. tipo camião
*          destino       LIKE zwm006_aux-destino,
          cliente       LIKE kna1-kunnr,
          ord_compra    LIKE zwm005-ord_compra,
          picking       LIKE vbfa-vbeln,
          remessa       LIKE vttp-vbeln,
          vkorg         LIKE likp-vkorg,      "org.vendas
          vtext         LIKE tvkot-vtext,     "descrição org.vendas
          add01         LIKE vttk-add01,      "num.clientes
          finalizada    LIKE zwm006_aux-finalizada,
          operacao(10),
          descricao     LIKE tvrot-bezei,          "descricao itinerario
          data          LIKE zwm006_aux-data_reg,
          hora_entrada  LIKE zwm006_aux-hora_saida,
          observacoes   LIKE zwm006_aux-observacoes,
END OF ti_dados.

DATA: itab_dados LIKE ti_dados OCCURS 0 WITH HEADER LINE.

RANGES: r_fim FOR zwm006_aux-finalizada.

DATA: BEGIN OF itab_count OCCURS 0,
        operacao(10),
        num_reg LIKE sy-tabix,
      END OF itab_count.

************************************************************************
*   Dados p/ALV
************************************************************************
DATA: ok_code      LIKE sy-ucomm,
      grid_control TYPE REF TO cl_gui_alv_grid,
      container    TYPE REF TO cl_gui_custom_container,
      control      TYPE REF TO i_oi_container_control,
      variant      LIKE disvariant,
      repid        LIKE sy-repid,
      wa_layout    TYPE lvc_s_layo,
      wa_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      layout       TYPE slis_layout_alv,
      it_fieldcat  TYPE slis_t_fieldcat_alv,
      sort         TYPE slis_t_sortinfo_alv,
      fieldcat     TYPE lvc_t_fcat,
      wa_fieldcat  TYPE lvc_s_fcat,
      itab_sort    TYPE lvc_t_sort,
      wa_sort      TYPE lvc_s_sort.

DATA: it_header TYPE kkblo_t_listheader.
DATA: v_linno   LIKE sy-linno.
DATA: header    TYPE kkblo_listheader.

************************************************************************
** Parâmetros de selecção
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-001.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS : s_arm FOR zwm006_aux-armazem
                 NO INTERVALS NO-EXTENSION DEFAULT '100'.
SELECTION-SCREEN END OF BLOCK b0.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: s_transp FOR vttk-tdlnr NO-EXTENSION,
                 "s_tipo   FOR zwm006_aux-tipo_camiao NO-EXTENSION,
                 s_data   FOR zwm006_aux-data_reg NO-EXTENSION
                          DEFAULT sy-datum,
                 s_hora   FOR zwm006_aux-hora_reg NO-EXTENSION,
                 s_matr   FOR zwm006_aux-matricula NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.

** Escolha de cargas/descargas/tudo
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
PARAMETERS : p_car  RADIOBUTTON GROUP g1,
             p_desc RADIOBUTTON GROUP g1,
             p_tudo RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

** Completas/Imcompletas/tudo
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-004.
PARAMETERS : p_com RADIOBUTTON GROUP g2,
             p_inc RADIOBUTTON GROUP g2,
             p_all RADIOBUTTON GROUP g2 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b3.


************************************************************************
** Start-of-Selection
************************************************************************
START-OF-SELECTION.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = text-t01.

** Verifica o status
  PERFORM cria_ranges.

** Obtem dados a listar
  IF p_car = 'X'.
    PERFORM carrega_informacao_carga.
  ELSEIF p_desc = 'X'.
    PERFORM carrega_informacao_descarga.
  ELSEIF p_tudo = 'X'.
    PERFORM carrega_informacao_total.
  ENDIF.

** Modifica dados
  PERFORM actualiza_status.

  IF ti_dados[] IS INITIAL.
    MESSAGE i000 WITH
    'Não existem dados para as opções indicadas'.
    EXIT.
  ENDIF.

************************************************************************
** End-of-Selection
************************************************************************
END-OF-SELECTION.

** Sub-rotinas para a impressão de dados
  PERFORM get_event CHANGING events.
  PERFORM f_monta_header.

  PERFORM get_campos CHANGING it_fieldcat.
  PERFORM get_layout_sort CHANGING layout sort.
  PERFORM imprime_dados.


*&---------------------------------------------------------------------*
*&      Form  CARREGA_INFORMACAO_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_informacao_carga .

  FREE: itab_dados.
  CLEAR: itab_dados.

 SELECT * FROM zwm006_aux INTO CORRESPONDING FIELDS OF TABLE itab_dados
          WHERE armazem       IN s_arm AND
                data_reg      IN s_data AND
                hora_reg      IN s_hora AND
                matricula     IN s_matr AND
                finalizada    IN r_fim.

** Actualização das várias remessas/pickings ...
  IF NOT itab_dados[] IS INITIAL.

    LOOP AT itab_dados. " WHERE NOT n_transporte IS INITIAL.

      CLEAR: ti_dados.
      MOVE-CORRESPONDING itab_dados TO ti_dados.

** Obter o trasnportador
      CLEAR: vttk.
      SELECT SINGLE * FROM vttk
      WHERE tknum EQ ti_dados-n_transporte
        AND tdlnr IN s_transp.

      CHECK sy-subrc EQ 0.

      ti_dados-operacao      = 'CARGA'.
      ti_dados-add01         = vttk-add01.
      ti_dados-dalen         = vttk-dalen.
      ti_dados-tipo_camiao   = vttk-sdabw.
      ti_dados-transportador = vttk-tdlnr.

** Obter a descrição do transportador
      SELECT SINGLE name1 FROM lfa1
      INTO ti_dados-desc_transp
      WHERE lifnr = ti_dados-transportador.

** Obter dados da remessa
      SELECT * FROM vttp
      WHERE tknum EQ ti_dados-n_transporte.

        ti_dados-remessa = vttp-vbeln.

** Nome do cliente
        SELECT SINGLE * FROM likp
        WHERE vbeln EQ vttp-vbeln.

        SELECT SINGLE * FROM kna1
        WHERE kunnr EQ likp-kunnr.

        ti_dados-cliente = kna1-name1.

** Descricao de itinerario
        SELECT SINGLE * FROM tvrot
        WHERE  spras EQ sy-langu AND
               route EQ likp-route.

        IF sy-subrc = 0.
          ti_dados-descricao = tvrot-bezei.
        ENDIF.

        SELECT SINGLE * FROM vbfa
        WHERE vbelv   EQ vttp-vbeln AND
              vbtyp_n EQ 'Q' AND
              vbtyp_v EQ 'J'.
** Picking
        IF sy-subrc = 0.
          ti_dados-picking = vbfa-vbeln.
        ENDIF.

        APPEND ti_dados.
      ENDSELECT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " CARREGA_INFORMACAO_CARGA

*&-------------------------------------------------------------
*&      Form  GET_EVENT
*&-------------------------------------------------------------
*       text
*--------------------------------------------------------------
*      <--P_EVENTS  text
*--------------------------------------------------------------
FORM get_event CHANGING p_events TYPE slis_t_event.

  wa_events-name = slis_ev_user_command.
  wa_events-form = 'USER_COMMAND'.
  APPEND wa_events TO p_events.

ENDFORM.                    " GET_EVENT

*&---------------------------------------------------------------------*
*&      Form  f_monta_header
*&---------------------------------------------------------------------*
FORM f_monta_header.

  DATA: l_carga    LIKE sy-tabix,
        l_descarga LIKE sy-tabix.

  CLEAR: itab_count.
  READ TABLE itab_count WITH KEY operacao = 'CARGA'
      BINARY SEARCH.
  IF sy-subrc EQ 0.
    l_carga = itab_count-num_reg.
  ENDIF.

  CLEAR: itab_count.
  READ TABLE itab_count WITH KEY operacao = 'DESCARGA'
      BINARY SEARCH.
  IF sy-subrc EQ 0.
    l_descarga = itab_count-num_reg.
  ENDIF.

  CLEAR header.
  header-typ  = 'H'.
  header-info = 'Estatísticas de Cargas e Descargas'.
  APPEND header TO it_header.

  CLEAR header.
  header-typ  = 'A'.
  header-key  = ' '.
  header-info = 'SapConsole : '.
  APPEND header TO it_header.

  CLEAR header.
  header-typ  = 'A'.
  header-key  = ' '.
  header-info = 'Warehouse Management'.
  APPEND header TO it_header.

  IF NOT l_carga IS INITIAL.
    CLEAR header.
    header-typ  = 'S'.
    header-key  = 'Total de Cargas'.
    header-info = l_carga.
    APPEND header TO it_header.
  ENDIF.

  IF NOT l_descarga IS INITIAL.
    CLEAR header.
    header-typ  = 'S'.
    header-key  = 'Total de Descargas'.
    header-info = l_descarga.
    APPEND header TO it_header.
  ENDIF.

ENDFORM.                    " f_monta_header


*&-------------------------------------------------------------
*&      Form  get_campos
*&-------------------------------------------------------------
*       text
*--------------------------------------------------------------
*      <--P_IT_FIELDCAT  text
*--------------------------------------------------------------
FORM get_campos CHANGING  p_it_fieldcat TYPE
                                slis_t_fieldcat_alv.

  DATA l_fieldcat TYPE slis_fieldcat_alv.

  MOVE sy-repid TO repid.

  SET TITLEBAR 'LISTAGEM DE CARGAS/DESCARGAS' OF PROGRAM repid.

  CLEAR : p_it_fieldcat.
  REFRESH : p_it_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = repid
      i_internal_tabname     = 'TI_DADOS'
      i_inclname             = repid
    CHANGING
      ct_fieldcat            = p_it_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.


  LOOP AT p_it_fieldcat INTO l_fieldcat.
    l_fieldcat-col_pos = sy-tabix.
    CASE l_fieldcat-fieldname.

      WHEN 'ARMAZEM'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = 'X'.
        l_fieldcat-seltext_l = 'Armazém'.
        l_fieldcat-seltext_m = 'Armazém'.
        l_fieldcat-seltext_s = 'Armazém'.
        l_fieldcat-reptext_ddic = 'Armazem'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'NUM_ENTRADA'.
        l_fieldcat-outputlen = 8.
        l_fieldcat-just = 'R'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = 'X'.
        l_fieldcat-seltext_l = 'Talão'.
        l_fieldcat-seltext_m = 'Talão'.
        l_fieldcat-seltext_s = 'Talão'.
        l_fieldcat-reptext_ddic = 'Talão'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'N_TRANSPORTE'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'R'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = 'X'.
        l_fieldcat-seltext_l = 'Transporte'.
        l_fieldcat-seltext_m = 'Transporte'.
        l_fieldcat-seltext_s = 'Transporte'.
        l_fieldcat-reptext_ddic = 'Transporte'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'MATRICULA'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Matrícula'.
        l_fieldcat-seltext_m = 'Matrícula'.
        l_fieldcat-seltext_s = 'Matrícula'.
        l_fieldcat-reptext_ddic = 'Matrícula'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'DESC_TRANSP'.
        l_fieldcat-outputlen = 20.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Transportador'.
        l_fieldcat-seltext_m = 'Transportador'.
        l_fieldcat-seltext_s = 'Transportador'.
        l_fieldcat-reptext_ddic = 'Transportador'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'BEZEI'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Tipo camião'.
        l_fieldcat-seltext_m = 'Tipo camião'.
        l_fieldcat-seltext_s = 'Tipo camião'.
        l_fieldcat-reptext_ddic = 'Tipo camião'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'PICKING'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'R'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Picking'.
        l_fieldcat-seltext_m = 'Picking'.
        l_fieldcat-seltext_s = 'Picking'.
        l_fieldcat-reptext_ddic = 'Picking'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'REMESSA'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'R'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Remessa'.
        l_fieldcat-seltext_m = 'Remessa'.
        l_fieldcat-seltext_s = 'Remessa'.
        l_fieldcat-reptext_ddic = 'Remessa'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'VKORG'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'R'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Org.Vendas'.
        l_fieldcat-seltext_m = 'Org.Vendas'.
        l_fieldcat-seltext_s = 'Org.Vendas'.
        l_fieldcat-reptext_ddic = 'Org.Vendas'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'DATA_REG'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-datatype = 'DATS'.
        l_fieldcat-seltext_l = 'Data Chegada'.
        l_fieldcat-seltext_m = 'Data Chegada'.
        l_fieldcat-seltext_s = 'Data Chegada'.
        l_fieldcat-reptext_ddic = 'Data Chegada'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'HORA_REG'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-datatype = 'TIMS'.
        l_fieldcat-seltext_l = 'Hora Chegada'.
        l_fieldcat-seltext_m = 'Hora Chegada'.
        l_fieldcat-seltext_s = 'Hora Chegada'.
        l_fieldcat-reptext_ddic = 'Hora Chegada'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'DATA_ENT'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-datatype = 'DATS'.
        l_fieldcat-seltext_l = 'Data Entrada'.
        l_fieldcat-seltext_m = 'Data Entrada'.
        l_fieldcat-seltext_s = 'Data Entrada'.
        l_fieldcat-reptext_ddic = 'Data Entrada'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'HORA_ENT'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-datatype = 'TIMS'.
        l_fieldcat-seltext_l = 'Hora Entrada'.
        l_fieldcat-seltext_m = 'Hora Entrada'.
        l_fieldcat-seltext_s = 'Hora Entrada'.
        l_fieldcat-reptext_ddic = 'Hora Entrada'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'DATA_SAIDA'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-datatype = 'DATS'.
        l_fieldcat-seltext_l = 'Data Saída'.
        l_fieldcat-seltext_m = 'Data Saída'.
        l_fieldcat-seltext_s = 'Data Saída'.
        l_fieldcat-reptext_ddic = 'Data Saída'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'HORA_SAIDA'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-datatype = 'TIMS'.
        l_fieldcat-seltext_l = 'Hora saída'.
        l_fieldcat-seltext_m = 'Hora saída'.
        l_fieldcat-seltext_s = 'Hora saída'.
        l_fieldcat-reptext_ddic = 'Hora saída'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'CLIENTE'.
        l_fieldcat-outputlen = 20.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Cliente'.
        l_fieldcat-seltext_m = 'Cliente'.
        l_fieldcat-seltext_s = 'Cliente'.
        l_fieldcat-reptext_ddic = 'Cliente'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'VTEXT'.
        l_fieldcat-outputlen = 15.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Org.vendas'.
        l_fieldcat-seltext_m = 'Org.vendas'.
        l_fieldcat-seltext_s = 'Org.vendas'.
        l_fieldcat-reptext_ddic = 'Org.vendas'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'ADD01'.
        l_fieldcat-outputlen = 5.
        l_fieldcat-just = 'R'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Num.clientes'.
        l_fieldcat-seltext_m = 'Num.clientes'.
        l_fieldcat-seltext_s = 'Num.clientes'.
        l_fieldcat-reptext_ddic = 'Num.clientes'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'DESCRICAO'.
        l_fieldcat-outputlen = 40.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Itinerário'.
        l_fieldcat-seltext_m = 'Itinerário'.
        l_fieldcat-seltext_s = 'Itinerário'.
        l_fieldcat-reptext_ddic = 'Itinerário'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'OPERACAO'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Operação'.
        l_fieldcat-seltext_m = 'Operação'.
        l_fieldcat-seltext_s = 'Operação'.
        l_fieldcat-reptext_ddic = 'Operação'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'STATUS'.
        l_fieldcat-outputlen = 4.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Status'.
        l_fieldcat-seltext_m = 'Status'.
        l_fieldcat-seltext_s = 'Status'.
        l_fieldcat-reptext_ddic = 'Status'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'OBSERVACOES'.
        l_fieldcat-outputlen = 12.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Observações'.
        l_fieldcat-seltext_m = 'Observações'.
        l_fieldcat-seltext_s = 'Observações'.
        l_fieldcat-reptext_ddic = 'Observações'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN OTHERS.
        DELETE p_it_fieldcat INDEX sy-tabix.

    ENDCASE.
  ENDLOOP.

ENDFORM.                    " get_campos

*&-------------------------------------------------------------
*&      Form  GET_LAYOUT_SORT
*&-------------------------------------------------------------
*       text
*--------------------------------------------------------------
*      <--P_LAYOUT  text
*      <--P_SORT  text
*--------------------------------------------------------------
FORM get_layout_sort CHANGING  p_layout TYPE slis_layout_alv
                                  p_sort TYPE slis_t_sortinfo_alv.

  DATA l_sort TYPE slis_sortinfo_alv.

* Layout
  p_layout-get_selinfos = 'X'.
  p_layout-key_hotspot = 'X'.
  p_layout-totals_before_items = ' '.
  p_layout-group_change_edit = 'X'.
  p_layout-detail_popup = 'X'.
  p_layout-zebra = 'X'.

* Ordenação
  REFRESH p_sort.
  l_sort-spos = 1.
  l_sort-fieldname = 'ARMAZEM'.
  l_sort-up = 'X'.
  l_sort-subtot = ' '.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  l_sort-spos = 2.
  l_sort-fieldname = 'NUM_ENTRADA'.
  l_sort-up = 'X'.
  l_sort-subtot = ' '.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  l_sort-spos = 3.
  l_sort-fieldname = 'N_TRANSPORTE'.
  l_sort-up = 'X'.
  l_sort-subtot = ' '.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  l_sort-spos = 4.
  l_sort-fieldname = 'MATRICULA'.
  l_sort-up = 'X'.
  l_sort-subtot = ' '.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  l_sort-spos = 5.
  l_sort-fieldname = 'DATA_REG'.
  l_sort-up = 'X'.
  l_sort-subtot = ' '.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  l_sort-spos = 6.
  l_sort-fieldname = 'HORA_REG'.
  l_sort-up = 'X'.
  l_sort-subtot = ' '.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

ENDFORM.                    " GET_LAYOUT_SORT

*&-------------------------------------------------------------
*&      Form  IMPRIME_DADOS
*&-------------------------------------------------------------
*       text
*--------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------
FORM imprime_dados.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
*         I_INTERFACE_CHECK        = ' '
          i_callback_program       = repid
*            I_CALLBACK_PF_STATUS_SET = 'STATUS'         "Nome SUBROTINA
          i_callback_user_command  = 'USER_COMMAND'   "Nome SUBROTINA
          i_callback_top_of_page  = 'TOP_OF_PAGE'
*         I_STRUCTURE_NAME         =
          is_layout                = layout
          it_fieldcat              = it_fieldcat       " esta
          i_background_id         = 'ALV_BACKGROUND'
*         IT_EXCLUDING             =
*         IT_SPECIAL_GROUPS        =
            it_sort                  = sort
*         IT_FILTER                =
*         IS_SEL_HIDE              =
*         I_DEFAULT                = 'X'
*         I_SAVE                   = ' '
*         IS_VARIANT               = VARIANT
            it_events                = events
*         IT_EVENT_EXIT            =
*         IS_PRINT                 =
*         IS_REPREP_ID             =
*         I_SCREEN_START_COLUMN    = 0
*          i_screen_start_line      = 2
*         I_SCREEN_END_COLUMN      = 0
*         I_SCREEN_END_LINE        = 0
*    IMPORTING
*         E_EXIT_CAUSED_BY_CALLER  =
*         ES_EXIT_CAUSED_BY_USER   =
  TABLES
            t_outtab                 =  ti_dados[]
  EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.

ENDFORM.                    " IMPRIME_DADOS


*--------------------------------------------------------------
*       FORM USER_COMMAND
*--------------------------------------------------------------
* Função que tem como objectivo "saltar" para a transacção
* correspondente  depois de o utilizador clicar no nº de documento
*--------------------------------------------------------------
FORM user_command USING r_ucomm LIKE sy-ucomm
                              rs_selfield TYPE slis_selfield.
  CASE r_ucomm.

** Transporte
    WHEN '&IC1'.

      CASE rs_selfield-fieldname.
        WHEN 'N_TRANSPORTE'.
          CHECK NOT rs_selfield-value IS INITIAL.
          SET PARAMETER ID 'TNR' FIELD rs_selfield-value.
          CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.

        WHEN 'REMESSA'.
          CHECK NOT rs_selfield-value IS INITIAL.
          SET PARAMETER ID 'VL' FIELD rs_selfield-value.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.


        WHEN OTHERS.
          MESSAGE s000(zwmmsg001) WITH 'Opção não selecionável'.
      ENDCASE.
  ENDCASE.

ENDFORM.                    "user_command


*&---------------------------------------------------------------------*
*&       FORM TOP_OF_PAGE                                              *
*&---------------------------------------------------------------------*
FORM top_of_page.
* Para criar um logotipo, deve-se entrar na transação 0FPM002 e
* preencher:
* - Classe = PICTURES
* - Objeto = OT
* - Item   = Nome do ID da figura

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_header
      i_logo             = 'RENOVA_LOGO'.

*  V_LINNO = SY-LINNO.
*
*  SKIP TO LINE 1.
*  WRITE: AT 70  'Página:', SY-PAGNO.
*
*  SKIP TO LINE V_LINNO.
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  ACTUALIZA_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_status .

  DATA: l_idx LIKE sy-tabix.

  LOOP AT ti_dados.

    l_idx = sy-tabix.

    IF ti_dados-finalizada = 'X'.
      WRITE icon_green_light AS ICON TO ti_dados-status.
    ELSEIF ti_dados-finalizada = ' ' AND ti_dados-porta IS INITIAL.
      WRITE icon_red_light AS ICON TO ti_dados-status.
    ELSEIF ti_dados-finalizada = ' ' AND NOT ti_dados-porta IS INITIAL.
      WRITE icon_yellow_light AS ICON TO ti_dados-status.
    ENDIF.

** Organização de transporte
    CLEAR: likp.
    SELECT SINGLE vkorg FROM likp
    INTO ti_dados-vkorg
    WHERE vbeln EQ ti_dados-remessa.

    IF sy-subrc = 0.
      SELECT SINGLE vtext FROM tvkot
      INTO ti_dados-vtext
      WHERE spras EQ sy-langu
        AND vkorg EQ ti_dados-vkorg.
    ENDIF.

** Descrição do tipo de camião
    SELECT SINGLE bezei FROM tvsakt
    INTO ti_dados-bezei
    WHERE spras EQ sy-langu
      AND sdabw EQ ti_dados-tipo_camiao.

** Hora e Data de Entrada para a Porta.
    CLEAR zwm004.
    SELECT SINGLE *
        FROM zwm004
            WHERE armazem = ti_dados-armazem AND
                  num_entrada = ti_dados-num_entrada.

    ti_dados-hora_ent = zwm004-hora_saida.
    ti_dados-data_ent = zwm004-data.

    MODIFY ti_dados INDEX l_idx.

    itab_count-operacao = ti_dados-operacao.
    itab_count-num_reg  = 1.
    COLLECT itab_count.
    CLEAR: itab_count.
    SORT itab_count BY operacao.


  ENDLOOP.

  SORT ti_dados BY operacao.

ENDFORM.                    " ACTUALIZA_STATUS
*&---------------------------------------------------------------------*
*&      Form  CARREGA_INFORMACAO_DESCARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_informacao_descarga .

  FREE: itab_dados.
  CLEAR: itab_dados.

*** Descargas Completas
*  IF p_desc = 'X' AND p_com = 'X' AND p_tudo = ' '.
*
*    SELECT * FROM zwm005 INTO CORRESPONDING FIELDS OF TABLE ti_dados
*             WHERE armazem       IN s_arm AND
*                   transportador IN s_transp AND
*                   data          IN s_data AND
*                   hora_entrada  IN s_hora AND
*                   matricula     IN s_matr AND
*                   finalizada    EQ 'X'.
*
*    IF sy-subrc = 0.
*      LOOP AT ti_dados.
*        ti_dados-operacao = 'DESCARGA'.
*        MODIFY ti_dados.
*      ENDLOOP.
*    ENDIF.
*** Descargas Imcompletas
*  ELSEIF p_desc = 'X' AND p_com = ' ' AND p_tudo = ' ' .
*
*
*    SELECT * FROM zwm005 INTO CORRESPONDING FIELDS OF TABLE ti_dados
*             WHERE armazem       IN s_arm AND
*                   transportador IN s_transp AND
**                   tipo_camiao IN s_tipo AND
*                   data          IN s_data AND
*                   hora_entrada  IN s_hora AND
*                   matricula     IN s_matr AND
*                   finalizada    EQ ' '.
*    IF sy-subrc = 0.
*      LOOP AT ti_dados.
*        ti_dados-operacao = 'DESCARGA'.
*        MODIFY ti_dados.
*      ENDLOOP.
*    ENDIF.
*** Descargas completas/Descargas incompletas
*  ELSEIF p_desc = 'X' AND p_tudo = 'X'.
*
*    SELECT * FROM zwm005 INTO CORRESPONDING FIELDS OF TABLE ti_dados
*             WHERE armazem       IN s_arm AND
*                   transportador IN s_transp AND
**                   tipo_camiao IN s_tipo AND
*                   data          IN s_data AND
*                   hora_entrada  IN s_hora AND
*                   matricula     IN s_matr.
*    IF sy-subrc = 0.
*      LOOP AT ti_dados.
*        ti_dados-operacao = 'DESCARGA'.
*        MODIFY ti_dados.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.

  SELECT * FROM zwm005
  INTO CORRESPONDING FIELDS OF TABLE ti_dados
  WHERE armazem       IN s_arm
    AND transportador IN s_transp
    AND data          IN s_data
    AND hora_entrada  IN s_hora
    AND matricula     IN s_matr
    AND finalizada    IN r_fim.

  IF sy-subrc = 0.
    LOOP AT ti_dados.
      ti_dados-operacao = 'DESCARGA'.
      ti_dados-data_reg = ti_dados-data.
      ti_dados-hora_reg = ti_dados-hora_entrada.
      MODIFY ti_dados.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " CARREGA_INFORMACAO_DESCARGA
*&---------------------------------------------------------------------*
*&      Form  CARREGA_INFORMACAO_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_informacao_total .

  FREE: itab_dados.
  CLEAR: itab_dados.

** Cargas
  SELECT * FROM zwm006_aux
  INTO CORRESPONDING FIELDS OF TABLE itab_dados
   WHERE armazem      IN s_arm
     AND data_reg     IN s_data
     AND hora_reg     IN s_hora
     AND matricula    IN s_matr
     AND finalizada   IN r_fim.

** Actualização das várias remessas/pickings ... para cargas
  IF NOT itab_dados[] IS INITIAL.
    LOOP AT itab_dados. " WHERE NOT n_transporte IS INITIAL.
      CLEAR: ti_dados.
      MOVE-CORRESPONDING itab_dados TO ti_dados.

** Dados do transportador
      CLEAR: vttk.
      SELECT SINGLE * FROM vttk
      WHERE tknum EQ ti_dados-n_transporte
        AND tdlnr IN s_transp.

      CHECK sy-subrc EQ 0.

      ti_dados-operacao      = 'CARGA'.
      ti_dados-add01         = vttk-add01.
      ti_dados-dalen         = vttk-dalen.
      ti_dados-tipo_camiao   = vttk-sdabw.
      ti_dados-transportador = vttk-tdlnr.

      SELECT SINGLE name1 FROM lfa1
      INTO ti_dados-desc_transp
      WHERE lifnr EQ ti_dados-transportador.

      IF ti_dados-finalizada = 'X'.
        WRITE icon_green_light AS ICON TO ti_dados-status.
      ELSE.
        WRITE icon_red_light AS ICON TO ti_dados-status.
      ENDIF.

      SELECT * FROM vttp
               WHERE tknum EQ ti_dados-n_transporte.

*        MOVE-CORRESPONDING ti_dados TO ti_dados_aux.
        ti_dados-remessa = vttp-vbeln.

** Nome do cliente
        SELECT SINGLE * FROM likp
        WHERE vbeln EQ vttp-vbeln.

        SELECT SINGLE * FROM kna1
        WHERE kunnr EQ likp-kunnr.

        ti_dados-cliente = kna1-name1.

        SELECT SINGLE * FROM vbfa
        WHERE vbelv   EQ vttp-vbeln AND
              vbtyp_n EQ 'Q' AND
              vbtyp_v EQ 'J'.

** descricao de itinerario
        SELECT SINGLE * FROM tvrot
        WHERE  spras EQ sy-langu AND
               route EQ likp-route.

        IF sy-subrc = 0.
          ti_dados-descricao = tvrot-bezei.
        ENDIF.

** Picking
        IF sy-subrc = 0.
          ti_dados-picking = vbfa-vbeln.
        ENDIF.

        APPEND ti_dados.
      ENDSELECT.
    ENDLOOP.
  ENDIF.

  FREE: itab_dados.
  CLEAR: itab_dados.

** Descargas
  SELECT * FROM zwm005
  INTO CORRESPONDING FIELDS OF TABLE itab_dados
  WHERE armazem       IN s_arm
    AND transportador IN s_transp
    AND data          IN s_data
    AND hora_entrada  IN s_hora
    AND matricula     IN s_matr
    AND finalizada    IN r_fim.

  IF sy-subrc = 0.
    LOOP AT itab_dados.

      CLEAR: ti_dados.
      MOVE-CORRESPONDING itab_dados TO ti_dados.

      ti_dados-operacao = 'DESCARGA'.
      ti_dados-data_reg = ti_dados-data.
      ti_dados-hora_reg = ti_dados-hora_entrada.

      IF ti_dados-finalizada = 'X'.
        WRITE icon_green_light AS ICON TO ti_dados-status.
      ELSE.
        WRITE icon_red_light AS ICON TO ti_dados-status.
      ENDIF.

      APPEND ti_dados.
    ENDLOOP.
  ENDIF.

*
*** Tudo completo
*  IF p_tudo = 'X' AND p_com = 'X' AND p_all = ' '.
*
*** Cargas
*    SELECT * FROM zwm006_aux
*    INTO CORRESPONDING FIELDS OF TABLE itab_dados
*     WHERE armazem      IN s_arm
**       and transportador IN s_transp
**       and tipo_camiao IN s_tipo
*       AND data_reg     IN s_data
*       AND hora_reg     IN s_hora
*       AND matricula    IN s_matr
*       AND finalizada   EQ 'X'.
*
**    IF sy-subrc = 0.
**      MOVE-CORRESPONDING zwm006_aux TO ti_dados.
**      ti_dados-operacao = 'CARGA'.
**
**** Dados do transportador
**      CLEAR: vttk.
**      SELECT SINGLE * FROM vttk
**      WHERE tknum EQ ti_dados-n_transporte
**        AND tdlnr IN s_transp.
**
**      CHECK sy-subrc EQ 0.
**
**      SELECT SINGLE name1 FROM lfa1 INTO ti_dados-desc_transp
**                  WHERE lifnr = zwm006-transportador.
**
**      IF ti_dados-finalizada = 'X'.
**        WRITE icon_green_light AS ICON TO ti_dados-status.
**      ELSEIF ti_dados-finalizada = ' '.
**        WRITE icon_red_light AS ICON TO ti_dados-status.
**      ENDIF.
**      APPEND ti_dados.
**    ENDIF.
**  ENDSELECT.
*
*** Actualização das várias remessas/pickings ... para cargas
*    IF NOT itab_dados[] IS INITIAL.
*      LOOP AT itab_dados. " WHERE NOT n_transporte IS INITIAL.
*        CLEAR: ti_dados.
*        MOVE-CORRESPONDING itab_dados TO ti_dados.
*
*** Dados do transportador
*        CLEAR: vttk.
*        SELECT SINGLE * FROM vttk
*        WHERE tknum EQ ti_dados-n_transporte
*          AND tdlnr IN s_transp.
*
*        CHECK sy-subrc EQ 0.
*
*        ti_dados-operacao = 'CARGA'.
*        ti_dados-transportador = vttk-tdlnr.
*
*        SELECT SINGLE name1 FROM lfa1
*        INTO ti_dados-desc_transp
*        WHERE lifnr EQ ti_dados-transportador.
*
*        IF ti_dados-finalizada = 'X'.
*          WRITE icon_green_light AS ICON TO ti_dados-status.
*        ELSE.
**        ELSEIF ti_dados-finalizada = ' '.
*          WRITE icon_red_light AS ICON TO ti_dados-status.
*        ENDIF.
*
*        SELECT * FROM vttp
*                 WHERE tknum EQ ti_dados-n_transporte.
*
*          MOVE-CORRESPONDING ti_dados TO ti_dados_aux.
*          ti_dados-remessa = vttp-vbeln.
*
*** Nome do cliente
*          SELECT SINGLE * FROM likp
*          WHERE vbeln EQ vttp-vbeln.
*
*          SELECT SINGLE * FROM kna1
*          WHERE kunnr EQ likp-kunnr.
*
*          ti_dados-cliente = kna1-name1.
*
*          SELECT SINGLE * FROM vbfa
*          WHERE vbelv   EQ vttp-vbeln AND
*                vbtyp_n EQ 'Q' AND
*                vbtyp_v QE 'J'.
*
*** descricao de itinerario
*          SELECT SINGLE * FROM tvrot
*          WHERE  spras EQ sy-langu AND
*                 route QE likp-route.
*
*          IF sy-subrc = 0.
*            ti_dados-descricao = tvrot-bezei.
*          ENDIF.
*
*** Picking
*          IF sy-subrc = 0.
*            ti_dados-picking = vbfa-vbeln.
*          ENDIF.
*
*          APPEND ti_dados.
*        ENDSELECT.
*      ENDLOOP.
*    ENDIF.
*
*** Descargas
*    SELECT * FROM zwm005
*    WHERE armazem       IN s_arm
*      AND transportador IN s_transp
**      and tipo_camiao IN s_tipo
*      AND data          IN s_data
*      AND hora_entrada  IN s_hora
*      AND matricula     IN s_matr
*      AND finalizada    EQ 'X'.
*      IF sy-subrc = 0.
*        MOVE-CORRESPONDING zwm005 TO ti_dados.
*        ti_dados-operacao = 'DESCARGA'.
*        IF ti_dados-finalizada = 'X'.
*          WRITE icon_green_light AS ICON TO ti_dados-status.
*        ELSEIF ti_dados-finalizada = ' '.
*          WRITE icon_red_light AS ICON TO ti_dados-status.
*        ENDIF.
*        APPEND ti_dados.
*      ENDIF.
*    ENDSELECT.
*
*** Incompletas
*  ELSEIF p_tudo = 'X' AND p_com = ' ' AND p_all = ' '.
*
*** Cargas
*    SELECT * FROM zwm006
*             WHERE armazem IN s_arm AND
*                   transportador IN s_transp AND
*                   tipo_camiao IN s_tipo AND
*                   data IN s_data AND
*                   hora_entrada IN s_hora AND
*                   matricula IN s_matr AND
*                   finalizada = ' '.
*      IF sy-subrc = 0.
*        MOVE-CORRESPONDING zwm006 TO ti_dados.
*        ti_dados-operacao = 'CARGA'.
*        IF ti_dados-finalizada = 'X'.
*
*          SELECT SINGLE name1 FROM lfa1 INTO ti_dados-desc_transp
*                      WHERE lifnr = zwm006-transportador.
*
*          WRITE icon_green_light AS ICON TO ti_dados-status.
*        ELSEIF ti_dados-finalizada = ' '.
*          WRITE icon_red_light AS ICON TO ti_dados-status.
*        ENDIF.
*        APPEND ti_dados.
*      ENDIF.
*    ENDSELECT.
*
*
*** Actualização das várias remessas/pickings ...
*    IF NOT ti_dados[] IS INITIAL.
*      LOOP AT ti_dados WHERE NOT n_transporte IS INITIAL.
*
*
*        SELECT * FROM vttp
*                 WHERE tknum = ti_dados-n_transporte.
*
*          MOVE-CORRESPONDING ti_dados TO ti_dados_aux.
*          ti_dados_aux-remessa = vttp-vbeln.
*** Nome do cliente
*          SELECT SINGLE * FROM likp
*          WHERE vbeln = vttp-vbeln.
*
*          SELECT SINGLE * FROM kna1
*          WHERE kunnr = likp-kunnr.
*
*          ti_dados_aux-cliente = kna1-name1.
*
*          SELECT SINGLE * FROM vbfa
*          WHERE vbelv   = vttp-vbeln AND
*                vbtyp_n = 'Q' AND
*                vbtyp_v = 'J'.
*
*** descricao de itinerario
*          SELECT SINGLE * FROM tvrot
*          WHERE  spras = sy-langu AND
*                 route = likp-route.
*
*          IF sy-subrc = 0.
*            ti_dados_aux-descricao = tvrot-bezei.
*          ENDIF.
*
*** Picking
*          IF sy-subrc = 0.
*            ti_dados_aux-picking = vbfa-vbeln.
*          ENDIF.
*
*          APPEND ti_dados_aux.
*
*        ENDSELECT.
*
*      ENDLOOP.
*    ENDIF.
*
*    CLEAR : ti_dados.
*    REFRESH: ti_dados.
*
*    ti_dados[] = ti_dados_aux[].
*
*    CLEAR : ti_dados_aux.
*    REFRESH : ti_dados_aux.
*
*
*** Descargas
*    SELECT * FROM zwm005
*             WHERE armazem IN s_arm AND
*                   transportador IN s_transp AND
*                   tipo_camiao IN s_tipo AND
*                   data IN s_data AND
*                   hora_entrada IN s_hora AND
*                   matricula IN s_matr AND
*                   finalizada = ' '.
*      IF sy-subrc = 0.
*        MOVE-CORRESPONDING zwm005 TO ti_dados.
*        ti_dados-operacao = 'DESCARGA'.
*        IF ti_dados-finalizada = 'X'.
*          WRITE icon_green_light AS ICON TO ti_dados-status.
*        ELSEIF ti_dados-finalizada = ' '.
*          WRITE icon_red_light AS ICON TO ti_dados-status.
*        ENDIF.
*        APPEND ti_dados.
*      ENDIF.
*    ENDSELECT.
*
*
*** Tudo completo/incompleto
*  ELSEIF p_tudo = 'X' AND p_all = 'X'.
*
*** Cargas
*    SELECT * FROM zwm006
*             WHERE armazem IN s_arm AND
*                   transportador IN s_transp AND
*                   tipo_camiao IN s_tipo AND
*                   data IN s_data AND
*                   hora_entrada IN s_hora AND
*                   matricula IN s_matr.
*      IF sy-subrc = 0.
*        MOVE-CORRESPONDING zwm006 TO ti_dados.
*        ti_dados-operacao = 'CARGA'.
*        IF ti_dados-finalizada = 'X'.
*          WRITE icon_green_light AS ICON TO ti_dados-status.
*        ELSEIF ti_dados-finalizada = ' '.
*          WRITE icon_red_light AS ICON TO ti_dados-status.
*        ENDIF.
*
*        SELECT SINGLE name1 FROM lfa1 INTO ti_dados-desc_transp
*                    WHERE lifnr = zwm006-transportador.
*
*        APPEND ti_dados.
*      ENDIF.
*    ENDSELECT.
*
*** Actualização das várias remessas/pickings ...
*    IF NOT ti_dados[] IS INITIAL.
*      LOOP AT ti_dados WHERE NOT n_transporte IS INITIAL.
*
*
*        SELECT * FROM vttp
*                 WHERE tknum = ti_dados-n_transporte.
*
*          MOVE-CORRESPONDING ti_dados TO ti_dados_aux.
*          ti_dados_aux-remessa = vttp-vbeln.
*** Nome do cliente
*          SELECT SINGLE * FROM likp
*          WHERE vbeln = vttp-vbeln.
*
*          SELECT SINGLE * FROM kna1
*          WHERE kunnr = likp-kunnr.
*
*          ti_dados_aux-cliente = kna1-name1.
*
*          SELECT SINGLE * FROM vbfa
*          WHERE vbelv   = vttp-vbeln AND
*                vbtyp_n = 'Q' AND
*                vbtyp_v = 'J'.
*
*** descricao de itinerario
*          SELECT SINGLE * FROM tvrot
*          WHERE  spras = sy-langu AND
*                 route = likp-route.
*
*          IF sy-subrc = 0.
*            ti_dados_aux-descricao = tvrot-bezei.
*          ENDIF.
*
*** Picking
*          IF sy-subrc = 0.
*            ti_dados_aux-picking = vbfa-vbeln.
*          ENDIF.
*
*          APPEND ti_dados_aux.
*
*        ENDSELECT.
*
*      ENDLOOP.
*    ENDIF.
*
*    CLEAR : ti_dados.
*    REFRESH: ti_dados.
*
*    ti_dados[] = ti_dados_aux[].
*
*    CLEAR : ti_dados_aux.
*    REFRESH : ti_dados_aux.
*
*
*** Descargas
*    SELECT * FROM zwm005
*             WHERE armazem IN s_arm AND
*                   transportador IN s_transp AND
*                   tipo_camiao IN s_tipo AND
*                   data IN s_data AND
*                   hora_entrada IN s_hora AND
*                   matricula IN s_matr.
*      IF sy-subrc = 0.
*        MOVE-CORRESPONDING zwm005 TO ti_dados.
*        ti_dados-operacao = 'DESCARGA'.
*        IF ti_dados-finalizada = 'X'.
*          WRITE icon_green_light AS ICON TO ti_dados-status.
*        ELSEIF ti_dados-finalizada = ' '.
*          WRITE icon_red_light AS ICON TO ti_dados-status.
*        ENDIF.
*        APPEND ti_dados.
*      ENDIF.
*    ENDSELECT.
*  ENDIF.

ENDFORM.                    " CARREGA_INFORMACAO_TOTAL
*&---------------------------------------------------------------------*
*&      Form  cria_ranges
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_ranges .

  FREE: r_fim.
  CLEAR: r_fim.

  IF p_com = 'X'.
    r_fim-low = 'X'.
    r_fim-sign = 'I'.
    r_fim-option = 'EQ'.
    APPEND r_fim.
  ELSEIF p_inc = 'X'.
    r_fim-low = ' '.
    r_fim-sign = 'I'.
    r_fim-option = 'EQ'.
    APPEND r_fim.
  ENDIF.

*** Cargas Completas
*  IF p_car = 'X' AND p_com = 'X' AND p_tudo = ' '.
*    r_fim-low = 'X'.
*    r_fim-sign = 'I'.
*    r_fim-option = 'EQ'.
*    APPEND r_fim.
*** Cargas Incompletas
*  ELSEIF p_car = 'X' AND p_com = ' ' AND p_tudo = ' ' .
*    r_fim-low = ' '.
*    r_fim-sign = 'I'.
*    r_fim-option = 'EQ'.
*    APPEND r_fim.
*** Descargas Completas
*  ELSEIF p_desc = 'X' AND p_com = 'X' AND p_tudo = ' '.
*    r_fim-low = 'X'.
*    r_fim-sign = 'I'.
*    r_fim-option = 'EQ'.
*    APPEND r_fim.
*** Descargas Incompletas
*  ELSEIF p_desc = 'X' AND p_com = ' ' AND p_tudo = ' ' .
*    r_fim-low = ' '.
*    r_fim-sign = 'I'.
*    r_fim-option = 'EQ'.
*    APPEND r_fim.
*** Tudo completo
*  ELSEIF p_com = 'X' AND p_all = ' ' AND p_tudo = 'X'.
*    r_fim-low = 'X'.
*    r_fim-sign = 'I'.
*    r_fim-option = 'EQ'.
*    APPEND r_fim.
*** Incompletas
*  ELSEIF p_com = ' ' AND p_all = ' ' AND p_tudo = 'X'.
*    r_fim-low = ' '.
*    r_fim-sign = 'I'.
*    r_fim-option = 'EQ'.
*    APPEND r_fim.
*  ENDIF.

ENDFORM.                    " cria_ranges
