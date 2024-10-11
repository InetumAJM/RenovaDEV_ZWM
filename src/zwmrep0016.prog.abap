************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0016                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Visualização de devoluções e impressão do form           *
* Criado por: Bruno Simões                                             *
* Criado em.: 19/01/2004                                               *
* Tipo PRG..: Report                                                   *
************************************************************************

REPORT  zwmrep0016.

TABLES : zwm025, likp.
INCLUDE <icon>.

** Dados globais
DATA : lvs_itcpo LIKE itcpo,
       result LIKE sy-subrc,
       desc_cliente(30),
       desc_material(30).

DATA : BEGIN OF ti_zwm025 OCCURS 0.
        INCLUDE STRUCTURE zwm025.
DATA : status(4).
DATA : END OF ti_zwm025.

***********************************************************************
** Variáveis auxiliares para o ALV
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

DATA: it_header     TYPE kkblo_t_listheader.
DATA: v_linno           LIKE sy-linno.
DATA: header        TYPE kkblo_listheader.
***************************************************************


** Parametros de selecção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN SKIP.
PARAMETERS: p_armaz LIKE zwm025-armazem OBLIGATORY,
            p_cli LIKE zwm025-cliente   OBLIGATORY.
SELECT-OPTIONS: s_vbeln FOR likp-vbeln OBLIGATORY MATCHCODE OBJECT vmvl,
                s_data  FOR zwm025-data NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS : p_imp RADIOBUTTON GROUP g1,
             p_vis RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.


START-OF-SELECTION.
  CLEAR : result.

** Impressão do documento
  IF p_imp = 'X'.

    PERFORM carrega_dados_imp.
    PERFORM ajusta_dados.
    IF result = 0.
      PERFORM impressao.
    ENDIF.

** Visualização do report
  ELSEIF p_vis = 'X'.

    PERFORM carrega_dados.
    PERFORM ajusta_dados.
    IF result = 0.
** Sub-rotinas para a impressão de dados
      PERFORM get_event CHANGING events.
      PERFORM f_monta_header.

      PERFORM get_campos CHANGING it_fieldcat.
      PERFORM get_layout_sort CHANGING layout sort.
      PERFORM imprime_dados.
    ENDIF.

  ENDIF.

END-OF-SELECTION.

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

  CLEAR header.
  header-typ  = 'H'.
  header-info = 'Listagem de Devoluções'.
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

  SET TITLEBAR 'LISTAGEM DE DEVOLUÇõES' OF PROGRAM repid.

  CLEAR : p_it_fieldcat.
  REFRESH : p_it_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = repid
      i_internal_tabname     = 'TI_ZWM025'
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
        l_fieldcat-reptext_ddic = 'Armazém'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'CLIENTE'.
        l_fieldcat-outputlen = 15.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-key = 'X'.
        l_fieldcat-seltext_l = 'Cliente'.
        l_fieldcat-seltext_m = 'Cliente'.
        l_fieldcat-seltext_s = 'Cliente'.
        l_fieldcat-reptext_ddic = 'Cliente'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.


      WHEN 'MATERIAL'.
        l_fieldcat-outputlen = 18.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Material'.
        l_fieldcat-seltext_m = 'Material'.
        l_fieldcat-seltext_s = 'Material'.
        l_fieldcat-reptext_ddic = 'Material'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'QUANTIDADE'.
        l_fieldcat-outputlen = 18.
        l_fieldcat-just = 'R'.
        l_fieldcat-do_sum = 'X'.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Quantidade'.
        l_fieldcat-seltext_m = 'Quantidade'.
        l_fieldcat-seltext_s = 'Quantidade'.
        l_fieldcat-reptext_ddic = 'Quantidade'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'UNIDADE'.
        l_fieldcat-outputlen = 5.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Unidade'.
        l_fieldcat-seltext_m = 'Unidade'.
        l_fieldcat-seltext_s = 'Unidade'.
        l_fieldcat-reptext_ddic = 'Unidade'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'LOTE'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Lote'.
        l_fieldcat-seltext_m = 'Lote'.
        l_fieldcat-seltext_s = 'Lote'.
        l_fieldcat-reptext_ddic = 'Lote'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'FACTURA_RENOVA'.
        l_fieldcat-outputlen = 12.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Remessa'.
        l_fieldcat-seltext_m = 'Remessa'.
        l_fieldcat-seltext_s = 'Remessa'.
        l_fieldcat-reptext_ddic = 'Remessa'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'STATUS'.
        l_fieldcat-outputlen = 5.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Status'.
        l_fieldcat-seltext_m = 'Status'.
        l_fieldcat-seltext_s = 'Status'.
        l_fieldcat-reptext_ddic = 'Status'.
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

  REFRESH p_sort.
  l_sort-spos = 1.
  l_sort-fieldname = 'CLIENTE'.
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
            t_outtab                 =  ti_zwm025[]
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
*
*      CASE RS_SELFIELD-FIELDNAME.
*        WHEN 'N_TRANSPORTE'.
*          CHECK NOT RS_SELFIELD-VALUE IS INITIAL.
*          SET PARAMETER ID 'TNR' FIELD RS_SELFIELD-VALUE.
*          CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
*
*        WHEN 'REMESSA'.
*          CHECK NOT RS_SELFIELD-VALUE IS INITIAL.
*          SET PARAMETER ID 'VL' FIELD RS_SELFIELD-VALUE.
*          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
*
*
*        WHEN OTHERS.
*          MESSAGE S000(ZWMMSG001) WITH 'Opção não selecionável'.
*      ENDCASE.
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

ENDFORM.                    "top_of_page

*&---------------------------------------------------------------------*
*&      Form  carrega_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_dados .

  SELECT * FROM zwm025 INTO TABLE ti_zwm025
           WHERE armazem = p_armaz AND
                 cliente = p_cli AND
                 data IN s_data.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '090'.
    result = 4.
    EXIT.
  ELSE.
    LOOP AT ti_zwm025.
      IF ti_zwm025-finalizada = 'X'.
        WRITE icon_green_light AS ICON TO ti_zwm025-status.
      ELSEIF ti_zwm025-finalizada = ' '.
        WRITE icon_red_light AS ICON TO ti_zwm025-status.
      ENDIF.

      MODIFY ti_zwm025 INDEX sy-tabix.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " carrega_dados
*&---------------------------------------------------------------------*
*&      Form  impressao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM impressao .

  lvs_itcpo-tdimmed = 'X'.
  lvs_itcpo-tdnewid = 'X'.

** Criação do formulário
  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device                      = 'PRINTER'
      dialog                      = 'X'
      form                        = 'ZWMFORM004'
      language                    = sy-langu
      options                     = lvs_itcpo
    EXCEPTIONS
      canceled                    = 1
      device                      = 2
      form                        = 3
      options                     = 4
      unclosed                    = 5
      mail_options                = 6
      archive_error               = 7
      invalid_fax_number          = 8
      more_params_needed_in_batch = 9
      spool_error                 = 10
      OTHERS                      = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  CALL FUNCTION 'START_FORM'
    EXPORTING
      form        = 'ZWMFORM004'
      language    = sy-langu
      program     = 'ZWMREP0016'
    EXCEPTIONS
      form        = 1
      format      = 2
      unended     = 3
      unopened    = 4
      unused      = 5
      spool_error = 6
      OTHERS      = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SORT ti_zwm025 BY cliente.
  LOOP AT ti_zwm025.

    SELECT SINGLE name1 FROM kna1 INTO desc_cliente
                        WHERE kunnr = ti_zwm025-cliente.

    AT NEW cliente.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element                  = 'ITEMS_HEADER'
          window                   = 'CLIENTE'
        EXCEPTIONS
          element                  = 1
          function                 = 2
          type                     = 3
          unopened                 = 4
          unstarted                = 5
          window                   = 6
          bad_pageformat_for_print = 7
          spool_error              = 8
          OTHERS                   = 9.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDAT.

    SELECT SINGLE maktx FROM makt INTO desc_material
                        WHERE matnr = ti_zwm025-material AND
                              spras = sy-langu.

** Impressão dos items
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element                  = 'ITEMS'
        window                   = 'MAIN'
      EXCEPTIONS
        element                  = 1
        function                 = 2
        type                     = 3
        unopened                 = 4
        unstarted                = 5
        window                   = 6
        bad_pageformat_for_print = 7
        spool_error              = 8
        OTHERS                   = 9.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CLEAR : desc_material.

  ENDLOOP.

  CALL FUNCTION 'END_FORM'
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      spool_error              = 3
      OTHERS                   = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

** fecho do formulario
  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      send_error               = 3
      spool_error              = 4
      OTHERS                   = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR : desc_cliente, ti_zwm025.
  REFRESH : ti_zwm025.

ENDFORM.                    " impressao
*&---------------------------------------------------------------------*
*&      Form  carrega_dados_imp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_dados_imp .

  SELECT * FROM zwm025 INTO TABLE ti_zwm025
           WHERE armazem        EQ p_armaz
             AND cliente        EQ p_cli
             AND factura_renova IN s_vbeln
             AND data           IN s_data
             AND finalizada     EQ ' '.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '090'.
    result = 4.
    EXIT.
  ENDIF.

ENDFORM.                    " carrega_dados_imp
*&---------------------------------------------------------------------*
*&      Form  ajusta_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ajusta_dados .
  DATA : l_total_qtd LIKE zwm025-quantidade.

  SORT ti_zwm025 BY cliente material.
  LOOP AT ti_zwm025.
    AT NEW material.
      SUM.
      MOVE ti_zwm025-quantidade TO l_total_qtd.
    ENDAT.

    MOVE l_total_qtd TO ti_zwm025-quantidade.
    MODIFY ti_zwm025.

    CLEAR ti_zwm025.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM ti_zwm025 COMPARING material.

ENDFORM.                    " ajusta_dados
