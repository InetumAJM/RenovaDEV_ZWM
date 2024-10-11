*&---------------------------------------------------------------------*
*& Report  ZWMREP0073                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT  ZWMREP0073 MESSAGE-ID zwmmsg001.

INCLUDE ZWMREP0073_TOP.   " global Data

START-OF-SELECTION.

  PERFORM load_inv.

END-OF-SELECTION.

  CHECK result = 0.
** Sub-rotinas para a impressão de dados
  PERFORM get_event CHANGING events.
  PERFORM f_monta_header.

  PERFORM get_campos CHANGING it_fieldcat.
  PERFORM get_layout_sort CHANGING layout sort.
  PERFORM imprime_dados.

*&---------------------------------------------------------------------*
*&      Form  load_inv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_inv .
  CLEAR : result,
          sy-tabix.

  SELECT * FROM zwm038
           INTO CORRESPONDING FIELDS OF TABLE cont_inv
           WHERE lgnum = p_lgnum AND
                 data_contagem = p_data.
  IF sy-subrc = 0.
    LOOP AT cont_inv.
      save_index = sy-tabix.
      IF cont_inv-qtd_existente <> cont_inv-qtd_contada.
        MOVE '@2B@' TO cont_inv-status.
      ELSE.
        MOVE '@20@' TO cont_inv-status.
      ENDIF.
      IF NOT cont_inv-ivnum IS INITIAL.
        SELECT SINGLE istat uname FROM link
                            INTO (cont_inv-istat,cont_inv-uname)
                            WHERE lgnum = cont_inv-lgnum AND
                                  ivnum = cont_inv-ivnum.
        IF sy-subrc = 0.
          MOVE cont_inv-istat TO domvalue.
          CALL FUNCTION 'DOMAIN_VALUE_GET'
            EXPORTING
              i_domname  = 'LVS_ISTAT'
              i_domvalue = domvalue
            IMPORTING
              e_ddtext   = cont_inv-descricao
            EXCEPTIONS
              not_exist  = 1
              OTHERS     = 2.
        ENDIF.
      ENDIF.
      MODIFY cont_inv INDEX save_index.
      CLEAR cont_inv.
    ENDLOOP.
  ELSE.
    MESSAGE i000 WITH 'Não existem dados seleccionados!'.
    result = 4.
    EXIT.
  ENDIF.
ENDFORM.                    " load_inv

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

  wa_events-name = slis_ev_pf_status_set.
  wa_events-form = 'STANDARD_FULLSCREEN'.
  APPEND wa_events TO p_events.

ENDFORM.                    " GET_EVENT
*&---------------------------------------------------------------------*
*&      Form  f_monta_header
*&---------------------------------------------------------------------*
FORM f_monta_header.

  CLEAR header.
  header-typ  = 'H'.
  header-info = 'Monitor de Inventário'.
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

  CLEAR header.
  header-typ  = 'A'.
  header-key  = ' '.
  header-info = ' '.
  APPEND header TO it_header.

  CLEAR header.
  header-typ  = 'A'.
  header-key  = ' '.
  header-info = ' '.
  APPEND header TO it_header.

  CLEAR header.
  header-typ  = 'A'.
  header-key  = ' '.
  header-info = ' '.
  APPEND header TO it_header.

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

  SET TITLEBAR 'MONITOR DE INVENTÁRIO' OF PROGRAM repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = repid
      i_internal_tabname     = 'CONT_INV'
      i_inclname             = repid
    CHANGING
      ct_fieldcat            = p_it_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  LOOP AT p_it_fieldcat INTO l_fieldcat.
*    l_fieldcat-col_pos = sy-tabix.
    CASE l_fieldcat-fieldname.

      WHEN 'LGNUM'.
        l_fieldcat-col_pos = '1'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = 'X'.
        l_fieldcat-seltext_l = 'Armazém'.
        l_fieldcat-seltext_m = 'Armazém'.
        l_fieldcat-seltext_s = 'Armazém'.
        l_fieldcat-reptext_ddic = 'Armazém'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'LGTYP'.
        l_fieldcat-col_pos = '2'.
        l_fieldcat-outputlen = 5.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = 'X'.
        l_fieldcat-seltext_l = 'Tipo Dep.'.
        l_fieldcat-seltext_m = 'Tipo Dep.'.
        l_fieldcat-seltext_s = 'Tipo Dep.'.
        l_fieldcat-reptext_ddic = 'Tipo Dep.'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'LGPLA'.
        l_fieldcat-col_pos = '3'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = 'X'.
        l_fieldcat-seltext_l = 'Posição'.
        l_fieldcat-seltext_m = 'Posição'.
        l_fieldcat-seltext_s = 'Posição'.
        l_fieldcat-reptext_ddic = 'Posição'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'QTD_EXISTENTE'.
        l_fieldcat-col_pos = '4'.
        l_fieldcat-outputlen = 12.
        l_fieldcat-just = 'R'.
        l_fieldcat-no_sum = ' '.
        l_fieldcat-do_sum = 'X'.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Qtd.Posição'.
        l_fieldcat-seltext_m = 'Qtd.Posição'.
        l_fieldcat-seltext_s = 'Qtd.Posição'.
        l_fieldcat-reptext_ddic = 'Qtd.Posição'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'QTD_CONTADA'.
        l_fieldcat-col_pos = '5'.
        l_fieldcat-outputlen = 12.
        l_fieldcat-just = 'R'.
        l_fieldcat-no_sum = ' '.
        l_fieldcat-do_sum = 'X'.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Qtd.Contada'.
        l_fieldcat-seltext_m = 'Qtd.Contada'.
        l_fieldcat-seltext_s = 'Qtd.Contada'.
        l_fieldcat-reptext_ddic = 'Qtd.Contada'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'UNI'.
        l_fieldcat-col_pos = '6'.
        l_fieldcat-outputlen = 5.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Unidade'.
        l_fieldcat-seltext_m = 'Unidade'.
        l_fieldcat-seltext_s = 'Unidade'.
        l_fieldcat-reptext_ddic = 'Unidade'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'STATUS'.
        l_fieldcat-col_pos = '7'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'C'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-icon = 'X'.
        l_fieldcat-seltext_l = 'Pré-Estado'.
        l_fieldcat-seltext_m = 'Pré-Estado'.
        l_fieldcat-seltext_s = 'Pré-Estado'.
        l_fieldcat-reptext_ddic = 'Pré-Estado'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.


      WHEN 'DATA_CONTAGEM'.
        l_fieldcat-col_pos = '8'.
        l_fieldcat-outputlen = 12.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Data Contagem'.
        l_fieldcat-seltext_m = 'Data Contagem'.
        l_fieldcat-seltext_s = 'Data Contagem'.
        l_fieldcat-reptext_ddic = 'Data Contagem'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.


      WHEN 'IVNUM'.
        l_fieldcat-col_pos = '9'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Doc.Inventário'.
        l_fieldcat-seltext_m = 'Doc.Inventário'.
        l_fieldcat-seltext_s = 'Doc.Inventário'.
        l_fieldcat-reptext_ddic = 'Doc.Inventário'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'ISTAT'.
        l_fieldcat-col_pos = '10'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Est.Inventário'.
        l_fieldcat-seltext_m = 'Est.Inventário'.
        l_fieldcat-seltext_s = 'Est.Inventário'.
        l_fieldcat-reptext_ddic = 'Est.Inventário'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'DESCRICAO'.
        l_fieldcat-col_pos = '11'.
        l_fieldcat-outputlen = 15.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Descrição'.
        l_fieldcat-seltext_m = 'Descrição'.
        l_fieldcat-seltext_s = 'Descrição'.
        l_fieldcat-reptext_ddic = 'Descrição'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'UNAME'.
        l_fieldcat-col_pos = '12'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Contador'.
        l_fieldcat-seltext_m = 'Contador'.
        l_fieldcat-seltext_s = 'Contador'.
        l_fieldcat-reptext_ddic = 'Contador'.
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
  p_layout-no_vline = 'X'.
  p_layout-no_hline = 'X'.
  p_layout-box_fieldname = 'CHK'.

* Ordenação
  REFRESH p_sort.
  l_sort-spos = 1.
  l_sort-fieldname = 'LGNUM'.
  l_sort-up = 'X'.
  l_sort-subtot = ' '.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  l_sort-spos = 2.
  l_sort-fieldname = 'LGTYP'.
  l_sort-up = 'X'.
  l_sort-subtot = ' '.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  l_sort-spos = 3.
  l_sort-fieldname = 'LGPLA'.
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
      i_callback_program       = repid
      i_callback_pf_status_set = 'STANDARD_FULLSCREEN'
      i_callback_user_command  = 'USER_COMMAND'   "Nome SUBROTINA
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      is_layout                = layout
      it_fieldcat              = it_fieldcat       " esta
      i_background_id          = 'ALV_BACKGROUND'
      it_sort                  = sort
      it_events                = events
    TABLES
      t_outtab                 = cont_inv[]
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

    WHEN 'CRIA_INV'.
      PERFORM verifica_criacao_doc_inv.
      CHECK result = 0.
      PERFORM cria_doc_inventario.
      PERFORM refresh.
    WHEN '&IC1'.
      CASE rs_selfield-fieldname.
** Documento inventário
        WHEN 'IVNUM'.
          CHECK NOT rs_selfield-value IS INITIAL.
          SET PARAMETER ID 'LGN' FIELD p_lgnum.
          SET PARAMETER ID 'IVN' FIELD rs_selfield-value.
          CALL TRANSACTION 'LI03N' AND SKIP FIRST SCREEN.
      ENDCASE.
    WHEN OTHERS.
      MESSAGE s000.
  ENDCASE.

  rs_selfield-refresh = 'X'.

ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&       FORM TOP_OF_PAGE                                              *
*&---------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_header
      i_logo             = 'RENOVA_LOGO'.
*      i_logo             = 'SAPLOGO_DEMO1'.

ENDFORM.                    "top_of_page

*---------------------------------------------------------------------*
*       FORM STANDARD_FULLSCREEN                                      *
*---------------------------------------------------------------------**
*---------------------------------------------------------------------*
*  -->  EXTAB                                                         *
*---------------------------------------------------------------------*
FORM standard_fullscreen USING  extab TYPE slis_t_extab.

  SET PF-STATUS 'ZSTANDARD' EXCLUDING extab.

ENDFORM.                    "STANDARD_FULLSCREEN

*&---------------------------------------------------------------------*
*&      Form  cria_doc_inventario
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_doc_inventario .
  SORT cont_inv BY chk.
  LOOP AT cont_inv WHERE chk = 'X'.
    PERFORM post_inv.
  ENDLOOP.
ENDFORM.                    " cria_doc_inventario

*&---------------------------------------------------------------------*
*&      Form  post_inv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_inv .
  PERFORM get_next_inv.
  PERFORM fill_structures.
ENDFORM.                    " post_inv

*&---------------------------------------------------------------------*
*&      Form  get_next_inv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_next_inv .

  CLEAR ivnum.

  SELECT SINGLE nukiv FROM t340d INTO t340d-nukiv
                      WHERE lgnum = p_lgnum.

  IF NOT t340d-nukiv IS INITIAL.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr        = t340d-nukiv
        object             = 'LVS_IVNUM'
        subobject          = p_lgnum
      IMPORTING
        number             = ivnum
      EXCEPTIONS
        interval_not_found = 1.
  ELSE.
** Mensagem de ERRO
  ENDIF.

ENDFORM.                    " get_next_inv

*&---------------------------------------------------------------------*
*&      Form  fill_structures
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_structures .
  REFRESH: xlink_aux, linp_aux.
** actualização da estrutura para o módulo de função
  SELECT * FROM lqua
        WHERE lgnum = cont_inv-lgnum
          AND lgtyp = cont_inv-lgtyp
          AND lgpla = cont_inv-lgpla.
    IF ( lqua-ausme <> 0 ) OR ( lqua-einme <> 0 ).
*      inv_not_possible = 'X'.
*      REFRESH linv_aux.
*      EXIT.
    ELSE.
      MOVE-CORRESPONDING lqua TO linv_aux.
** Para a unidade ser impressa no documento de inventário
      linv_aux-altme = lqua-meins.
      linv_aux-umrez = 1.
      linv_aux-umren = 1.
      APPEND linv_aux.
    ENDIF.
  ENDSELECT.

  xlink_aux-lgnum = cont_inv-lgnum.
  xlink_aux-ivnum = ivnum.
  xlink_aux-istat = 'N'.
  xlink_aux-dstat = 'X'.
  xlink_aux-lgtyp = cont_inv-lgtyp.
  xlink_aux-pdatu = sy-datum.
  xlink_aux-ivakt = 'X'.
  xlink_aux-irnum = 'RF'.
*  xlink_aux-uname = sy-uname.
  CLEAR xlink_aux-uname.

  SELECT SINGLE anzle anzqu FROM lagp
         INTO (lagp-anzle,lagp-anzqu)
         WHERE lgnum = cont_inv-lgnum AND
               lgtyp = cont_inv-lgtyp AND
               lgpla = cont_inv-lgpla.

  linp_aux-lgnum = cont_inv-lgnum.
  linp_aux-ivnum = ivnum.
  linp_aux-ivpos = '0001'.
  linp_aux-lgpla = cont_inv-lgpla.
  linp_aux-anzle = lagp-anzle.
  linp_aux-gebkz = 'B'.
  linp_aux-anzqu = lagp-anzqu.
  linp_aux-dstat = 'X'.
  linp_aux-istat = 'N'.
  APPEND linp_aux.

  IF NOT linv_aux[] IS INITIAL.
    LOOP AT linv_aux.
      linv_aux-ivnum = ivnum.
      linv_aux-ivpos = '0001'.
      linv_aux-istat = 'N'.
      MODIFY linv_aux.
    ENDLOOP.
  ENDIF.

** Post inventory document
  CALL FUNCTION 'L_AKTIVIEREN_HINZUFUEGEN'
    EXPORTING
      xlink = xlink_aux
    TABLES
      inp   = linp_aux
      inv   = linv_aux.

  IF sy-subrc = 0.
    SELECT SINGLE * FROM zwm038
                    WHERE lgnum = cont_inv-lgnum AND
                          lgtyp = cont_inv-lgtyp AND
                          lgpla = cont_inv-lgpla.
    IF sy-subrc = 0.
      zwm038-ivnum = ivnum.
      MODIFY zwm038.
      COMMIT WORK.
    ENDIF.
    CLEAR : xlink_aux,linp_aux,linv_aux,ivnum.
    REFRESH : xlink_aux,linp_aux,linv_aux.
  ENDIF.

ENDFORM.                    " fill_structures

*&---------------------------------------------------------------------*
*&      Form  verifica_criacao_doc_inv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_criacao_doc_inv .

  READ TABLE cont_inv WITH KEY chk = 'X'.
  IF sy-subrc <> 0.
** Não existe nada para processar
    MESSAGE i091.
    result = 4.
    EXIT.
  ENDIF.

*  SORT cont_inv BY chk.
  LOOP AT cont_inv WHERE chk = 'X'.
** Verificar se já existe um doc. de inventário para a posição
    IF NOT cont_inv-ivnum IS INITIAL.
** Verificar em que estado se encontra o doc. inventário
      IF cont_inv-istat <> 'L' AND cont_inv-istat <> 'S'.
        MESSAGE i089 WITH cont_inv-lgtyp cont_inv-lgpla.
        result = 4.
        EXIT.
      ENDIF.
    ENDIF.
** Verificar se as qts estão iguais
    IF cont_inv-qtd_existente = cont_inv-qtd_contada.
      MESSAGE i090 WITH cont_inv-lgtyp cont_inv-lgpla.
      result = 4.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " verifica_criacao_doc_inv

*&---------------------------------------------------------------------*
*&      Form  refresh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh .

  CLEAR : cont_inv,
          xlink_aux,
          linp_aux,
          linv_aux,
          ivnum,
          result.

  REFRESH : cont_inv,
            xlink_aux,
            linp_aux,
            linv_aux.

  PERFORM load_inv.
  CHECK result = 0.

** Sub-rotinas para a impressão de dados
*  PERFORM get_event CHANGING events.
*  PERFORM f_monta_header.

*  PERFORM get_campos CHANGING it_fieldcat.
*  PERFORM get_layout_sort CHANGING layout sort.
*  PERFORM imprime_dados.

ENDFORM.                    " refresh
