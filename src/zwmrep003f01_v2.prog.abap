*&---------------------------------------------------------------------*
*&  Include           ZWMREP003F01                                     *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  FILL_LISTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_lista .

  REFRESH izwm003.
  CALL FUNCTION 'ZWM_MANAGE_PARKING_V1'
    EXPORTING
      operacao                = '1'
      armazem                 = xuser-lgnum
    TABLES
      l_zwm003                = izwm003
      return_msg              = return_msg
    EXCEPTIONS
      tab_zwm003_not_filled   = 1
      invalid_parameter       = 2
      tab_l_zwm003_filled     = 3
      tab_l_zwm003_not_filled = 4
      no_warehouse            = 5
      OTHERS                  = 6.

  IF sy-subrc <> 0.
*erro
    READ TABLE return_msg INDEX 1.
    IF sy-subrc = 0.
      MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
      NUMBER return_msg-msgnr WITH return_msg-msgv1 return_msg-msgv2
      return_msg-msgv3 return_msg-msgv4.
    ENDIF.
  ELSEIF izwm003[] IS INITIAL.
*lista de espera vazia
    READ TABLE return_msg INDEX 1.
    IF sy-subrc = 0.
      MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
      NUMBER return_msg-msgnr WITH return_msg-msgv1 return_msg-msgv2
      return_msg-msgv3 return_msg-msgv4.
    ENDIF.

  ELSE.

    SORT izwm003 BY num_entrada ASCENDING.
*chama ecran LISTA DE ESPERA
    CALL SCREEN '0002' STARTING AT 23 07 ENDING AT 138 25.
  ENDIF.

ENDFORM.                    " FILL_LISTA

**&---------------------------------------------------------------------
**
**&      Form  funcao
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM funcao.
*
*  DATA: flag_refresh,resposta(1),
*        text(30),
*        diag(60).
*
*  DATA: izwm002 LIKE zwm002 OCCURS 0 WITH HEADER LINE.
*
*  CLEAR flag_refresh.
*
*  READ TABLE tab_f INDEX indice_tab.
*  IF tab_f-estado_interno = 'O'.
**************************************************
****ATRIBUIR NOVO CAMIÃO A PORTA SEMAFORO VERMELHO
**************************************************
*    MOVE: tab_f-porta     TO porta_d,
*          tab_f-matricula TO matricula_o.
****          tab_f-tipo      TO tipo_porta.
*
*    SELECT SINGLE * FROM zwm006_aux
*    WHERE armazem   = xuser-lgnum AND
*          matricula = matricula_o.
*
*    IF sy-subrc = 0.
*      """"""""""""""""""""""CARRO DE CARGA"""""""""""""""""""""""
*      "O CARRO EM QUE SE CLICOU É DE CARGA
*      "DETERMINA NºTRANSPORTE
*
*      SELECT SINGLE * FROM vttk
*      WHERE tknum = zwm006_aux-n_transporte.
*
*      IF sy-subrc = 0.
*        IF NOT vttk-dalbg IS INITIAL.
*          "O TRANSPORTE ASSOCIADO JA TEM DATA DE INICIO DE CARREGAMENTO
*          MESSAGE i023 WITH matricula_o.
*        ELSE.
*          "AINDA NAO COMEÇOU A CARREGAR, VER SE EXISTE EM FILA DE
*ESPERA
*          "UM POSSIVEL CARRO PARA ATRIBUIR (para depois chamar ecran)
*
*          SELECT SINGLE * FROM zwm003_aux
*          WHERE armazem  = xuser-lgnum AND
*                operacao = 'CARGA' AND
*                estado   = 'E'.
*
*          IF sy-subrc = 0.
*            "existem carros em fila de espera de carga para atribuir
*            CALL SCREEN '0006' STARTING AT  30 10 ENDING AT 105 20.
*          ELSE.
*           "NAO EXISTEM CARROS DE CARGA PARA ATRIBUIR, MAS PODEM
*EXISTIR
*            "CARRO DE DESCARGA CASO A PORTA QUE SE CLICOU SEJA
*CARGA/DES
*            IF tipo_porta = 'Carga/Des'.
*              SELECT SINGLE * FROM zwm003_aux
*              WHERE armazem  = xuser-lgnum AND
*                   operacao = 'DESCARGA' AND
*                   estado   = 'E'.
*              IF sy-subrc = 0.
*                "existem carros em fila de espera de descarg p/
*atribuir
*                CALL SCREEN '0006' STARTING AT  30 10 ENDING AT 105 20.
*              ELSE.
*                " NAO EXISTEM CARROS DE DESCARGA PARA ATRIBUIR ERRO
*                MESSAGE i027.
*                SET SCREEN '0001'.LEAVE SCREEN.
*              ENDIF.
*            ELSE.
*              "ERRO TEMOS PORTA DE CARGA E NAO TEMOS CARROS DE CARGA EM
*              "FILA DE ESPERA COM STATUS 'E'
*              MESSAGE i025.
*              SET SCREEN '0001'.LEAVE SCREEN.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ELSE.
**so acontece se tivermos um carro atribuido a uma porta que n t
**transporte
*
*      ENDIF.
*    ELSE.
*      """"""""""""""""""""""CARRO DE DESCARGA"""""""""""""""""""""""
*      "VER SE EXISTE EM FILA DE ESPERA
*      "UM POSSIVEL CARRO PARA ATRIBUIR (para depois chamar ecran)
*
*      SELECT SINGLE * FROM zwm003_aux
*      WHERE armazem  = xuser-lgnum AND
*            operacao = 'DESCARGA' AND
*            estado   = 'E'.
*      IF sy-subrc = 0.
*        "existem carros em fila de espera de carga para atribuir
*        CALL SCREEN '0006' STARTING AT  30 10 ENDING AT 105 20.
*      ELSE.
*        "NAO EXISTEM CARROS DE DESCARGA PARA ATRIBUIR, MAS PODEM
*EXISTIR
*        "CARRO DE CARGA CASO A PORTA QUE SE CLICOU SEJA CARGA/DES
*
*        IF tipo_porta = 'Carga/Des'.
*          SELECT SINGLE * FROM zwm003_aux
*          WHERE armazem  = xuser-lgnum AND
*               operacao = 'CARGA' AND
*               estado   = 'E'.
*          IF sy-subrc = 0.
*            "existem carros em fila de espera de descarg p/ atribuir
*            CALL SCREEN '0006' STARTING AT  30 10 ENDING AT 105 20.
*          ELSE.
*            " NAO EXISTEM CARROS DE CARGA PARA ATRIBUIR ERRO
*            MESSAGE i027.
*            SET SCREEN '0001'.LEAVE SCREEN.
*          ENDIF.
*        ELSE.
*          "ERRO TEMOS PORTA DE DESCAR E NAO TEMOS CARROS DE DESCARGA
*          "EM FILA DE ESPERA COM STATUS 'E'
*          MESSAGE i026.
*          SET SCREEN '0001'.LEAVE SCREEN.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*  ELSEIF tab_f-estado_interno = 'D'.
**************************************************
****NAO FAZER NADA SEMAFORO VERDE -
**************************************************
*    MESSAGE i016.
*
*  ELSEIF tab_f-estado_interno = 'C'.
**************************************************
****SEMAFORO AMARELO - CARRO QUE ESTA NA PORTARIA E QUE FOI ATRIBUIDO A
****UMA PORTA SEM IR PARA LISTA DE ESPERA
**************************************************
*    MOVE 'Aceita a atribuição?' TO text.
*    CONCATENATE 'O Camião' tab_f-matricula
* 'foi proposto para a porta' tab_f-porta INTO diag SEPARATED BY space.
*
*    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
*      EXPORTING
**   DEFAULTOPTION        = 'Y'
*        diagnosetext1        = diag
**   DIAGNOSETEXT2        = ' '
**   DIAGNOSETEXT3        = ' '
*        textline1            = text
**   TEXTLINE2            = ' '
*        titel                = 'Confirmação de Associação de Camião'
*       start_column         = 25
*       start_row            = 6
*       cancel_display       = ' '
*     IMPORTING
*       answer               = resposta.
*
*    IF resposta = 'J'.
**responsavel de armazem aceita atribuir carro à porta
**altera estado das portas altera estado da porta para 'O'
**actualiza tabela das portas.
*      SELECT SINGLE * FROM zwm002 INTO izwm002
*      WHERE armazem = xuser-lgnum AND
*            porta   = tab_f-porta.
*
*      IF sy-subrc = 0.
*        izwm002-estado = 'O'.
*
*** BS - INS 03.11.2003
*** Descobrir qual o primeiro conjunto de pulmões assigando a esta porta
*** em concreto
*
*** Dados de tabelas de parametrização
*
*** Tipo de depósito dos pulmões
**        DATA : ST_PUL LIKE LAGP-LGTYP.
**        CLEAR : ST_PUL.
**        PERFORM GET_PARAMETER USING XUSER-LGNUM
**                                'ENTRADA_ARMAZEM'
**                                'ST_PUL'
**                                 ST_PUL.
**
**
**        DATA : PULMAO LIKE LAGP-LGPLA.
**        DATA : PULMAO_AUX LIKE LAGP-LGBER, NUM_QUANTOS TYPE I.
**        CLEAR : PULMAO,PULMAO_AUX, NUM_QUANTOS.
**
**        SELECT SINGLE * FROM ZWM007
**                        WHERE ARMAZEM = XUSER-LGNUM AND
**                              PORTA = TAB_F-PORTA.
**        IF SY-SUBRC = 0.
**          DO 8 TIMES VARYING PULMAO FROM
**                             ZWM007-PULMAO1 NEXT
**                             ZWM007-PULMAO2.
**
**** Lê os três caracteres do meio q correspondem à área e identifica
**** sempre os pulmões dois a dois
**            CLEAR : PULMAO_AUX.
**            PULMAO_AUX = PULMAO+4(3).
**
**            SELECT * FROM LAGP
**                      WHERE LGNUM = XUSER-LGNUM AND
**                            LGTYP = ST_PUL AND
**                            LGBER = PULMAO_AUX AND
**                            BRAND = ' '.
**
**              IF SY-SUBRC = 0.
**                NUM_QUANTOS = NUM_QUANTOS + LAGP-ANZQU.
**              ENDIF.
**            ENDSELECT.
**
**** Verificar se este pulmão está vazio
**            IF NUM_QUANTOS = 0 AND SY-SUBRC = 0.
**              CLEAR : NUM_QUANTOS.
**              EXIT.
**            ELSE.
**              CLEAR : NUM_QUANTOS.
**            ENDIF.
**
**          ENDDO.
**
**
****TABELA AUXILIAR
**          DATA : BEGIN OF LAGP_AUX OCCURS 2,
**                  LGPLA LIKE LAGP-LGPLA.
**          DATA : END OF LAGP_AUX.
**
**** Seleccionar os pulmões atribuídos à porta
**          SELECT * FROM LAGP
**                    WHERE LGNUM = XUSER-LGNUM AND
**                          LGTYP = ST_PUL AND
**                          LGBER = PULMAO_AUX AND
**                          BRAND = ' '.
**            IF SY-SUBRC = 0.
**              LAGP_AUX-LGPLA = LAGP-LGPLA.
**              APPEND LAGP_AUX.
**            ENDIF.
**          ENDSELECT.
**
**          READ TABLE LAGP_AUX INDEX 1.
**          IF SY-SUBRC = 0.
**            IZWM002-PULMAO_1 = LAGP_AUX-LGPLA.
**          ENDIF.
**
**          READ TABLE LAGP_AUX INDEX 2.
**          IF SY-SUBRC = 0.
**            IZWM002-PULMAO_2 = LAGP_AUX-LGPLA.
**          ENDIF.
**** Seleccionar os pulmões atribuídos à porta
**
**** Actualizar tabela de posições de depósito (lagp) com o indicador de
**** OCUPADO para os dois pulmões de descarga
**          LOOP AT LAGP_AUX.
**            UPDATE LAGP SET BRAND = 'X'
**                        WHERE LGNUM = XUSER-LGNUM AND
**                              LGTYP = ST_PUL AND
**                              LGPLA = LAGP_AUX-LGPLA.
**            COMMIT WORK.
**          ENDLOOP.
**
**
**          CLEAR: PULMAO_AUX, LAGP_AUX.
**          REFRESH : LAGP_AUX.
**
**        ENDIF.
*** BS - INS 03.11.2003
*        APPEND izwm002.
*      ENDIF.
*
**actualiza estado das portas (ZWM002)
*      IF NOT izwm002[] IS INITIAL.
*        CALL FUNCTION 'ZWM_MANAGE_DOORS'
*          EXPORTING
*            operacao                = '2'
*            armazem                 = xuser-lgnum
*          TABLES
*            l_zwm002                = izwm002
*            return_msg              = return_msg
*          EXCEPTIONS
*            no_warehouse            = 1
*            tab_zwm002_not_filled   = 2
*            tab_l_zwm002_filled     = 3
*            tab_l_zwm002_not_filled = 4
*            invalid_parameter       = 5
*            OTHERS                  = 6.
*
*        IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ENDIF.
*      ENDIF.
*
** actualizar a tabela zwm028 com a porta
**      BREAK-POINT.
*      SELECT SINGLE * FROM vttp
*                      WHERE tknum = zwm006_aux-n_transporte.
*
*      IF sy-subrc = 0.
*        SELECT * FROM vbss WHERE vbeln = vttp-vbeln.
*
*          SELECT SINGLE * FROM vbsk
*                          WHERE sammg = vbss-sammg AND
*                                smart = 'W'.
*          IF sy-subrc = 0.
*            EXIT.
*          ELSE.
*            CLEAR vbss.
*          ENDIF.
*        ENDSELECT.
*        SELECT SINGLE * FROM zwm028
*                        WHERE lgnum = xuser-lgnum
*                        AND   refnr = vbss-sammg.
*
**              actualiza a tabela zwm028 com a porta
**        BREAK-POINT.
*        CONCATENATE '000-000-' tab_f-porta+1(2) INTO zwm028-porta.
*        MODIFY zwm028.
*
*        IF sy-subrc = 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*      ENDIF.
*
*      SET SCREEN '0001'.LEAVE SCREEN.
*
*    ELSEIF resposta = 'N'.
**altera estado das portas passa para 'D' e ' 'limpa ,matricula
**coloca carro em fila de espera
**altera tabela das cargas/descargas
*      PERFORM recusa_atribuicao.
*      SET SCREEN '0001'.LEAVE SCREEN.
*
*    ENDIF.
*
**    MESSAGE I015 WITH TAB_F-MATRICULA TAB_F-PORTA.
*  ENDIF.
*
**  PERFORM SELECT_DATA.
**  SORT TAB_F BY PORTA ASCENDING.
**  PERFORM TAB_FINAL.
**  CALL METHOD TREE->REFRESH_TABLE_DISPLAY.
*ENDFORM.                    " funcao
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data .

  REFRESH tab_f.
  SELECT * FROM zwm002 WHERE armazem = xuser-lgnum.
    MOVE-CORRESPONDING zwm002 TO tab_f.
    MOVE zwm002-estado TO tab_f-estado_interno.

    IF zwm002-bloqueio EQ 'X'.

      WRITE icon_red_light AS ICON TO tab_f-estado.

      SELECT        * FROM  zwm006_aux UP TO 1 ROWS
             WHERE  armazem      = xuser-lgnum
             AND    num_entrada  = zwm002-num_entrada
             AND    finalizada   = ' '.

      ENDSELECT.

      IF sy-subrc = 0.
        tab_f-matricula = zwm006_aux-matricula.
      ELSE.

        SELECT * FROM  zwm005 UP TO 1 ROWS
               WHERE  armazem      = xuser-lgnum
               AND    num_entrada  = zwm002-num_entrada
               AND    finalizada   = ' '.

        ENDSELECT.
        IF sy-subrc = 0.
          tab_f-matricula = zwm005-matricula.
        ENDIF.
      ENDIF.

    ELSE.
      WRITE icon_green_light AS ICON TO tab_f-estado.
    ENDIF.

    APPEND tab_f. CLEAR tab_f.
  ENDSELECT.

*-----------------------------------------------------------ins Abr2005
  CLEAR: zwm002, zwm007.
*----------------------------------------------------------------------

ENDFORM.                    " SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  TAB_FINAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tab_final .
  SORT tab_f BY porta ASCENDING.
  gt_tab = tab_f[].
ENDFORM.                    " TAB_FINAL

*---------------------------------------------------------------------*
*       FORM fieldcat_init                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fieldcat_init.

  DATA: pos TYPE i VALUE 1.
  DATA: aux_cat TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE.
  REFRESH aux_cat.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'ARMAZEM'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
*  AUX_CAT-DO_SUM        = 'X'.
  aux_cat-coltext       = 'Armazem'(003).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PORTA'.
*  AUX_CAT-ICON          = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_out        = 'X'.
*  AUX_CAT-DATATYPE      ='CHAR'.
*  AUX_CAT-OUTPUTLEN     = '8'.
  aux_cat-coltext       = 'Portas'(004).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'ESTADO'.
*  AUX_CAT-DO_SUM         = 'X'.
  aux_cat-icon           = 'X'.
  aux_cat-datatype       = 'CHAR'.
  aux_cat-outputlen     = '8'.
  aux_cat-coltext       = 'Estado da Porta'(005).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'NUM_ENTRADA'.
*  AUX_CAT-DO_SUM         = 'X'.
  aux_cat-datatype       ='NUMC'.
  aux_cat-outputlen     = '5'.
  aux_cat-coltext       = 'Talão'(006).
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'MATRICULA'.
*  AUX_CAT-DO_SUM         = 'X'.
  aux_cat-datatype       ='CHAR'.
  aux_cat-outputlen     = '15'.                "20
  aux_cat-coltext       = 'Matricula'(007).
  APPEND aux_cat.
  ADD 1 TO pos.

*  CLEAR aux_cat.
*  aux_cat-col_pos       =  pos.
*  aux_cat-fieldname     = 'TIPO'.
**  AUX_CAT-DO_SUM         = 'X'.
*  aux_cat-datatype       ='CHAR'.
*  aux_cat-outputlen     = '8'.
*  aux_cat-coltext       = 'Tipo Porta'.
*  APPEND aux_cat.
*  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'ESTADO_INTERNO'.
  aux_cat-no_out        = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'COL'.
  aux_cat-no_out        = 'X'.
  APPEND aux_cat.
  ADD 1 TO pos.

  gt_fieldcatalog[] = aux_cat[].

ENDFORM.                    "FIELDCAT_INIT

*---------------------------------------------------------------------*
*       FORM ajusta_propriedades                                      *
*---------------------------------------------------------------------*
FORM ajusta_propriedades. " TABLES sort     TYPE slis_t_sortinfo_alv.

*  data: slis_layout_alv.

  DATA ls_sort_wa TYPE lvc_s_sort.
* Ordenação
  REFRESH gt_sort.

* create sort-table
  ls_sort_wa-spos = 1.
  ls_sort_wa-fieldname = 'ARMAZEM'.
  ls_sort_wa-up = 'X'.
  ls_sort_wa-subtot = 'X'.
  APPEND ls_sort_wa TO gt_sort.

  ls_sort_wa-spos = 2.
  ls_sort_wa-fieldname = 'PORTA'.
  ls_sort_wa-up = 'X'.
  ls_sort_wa-subtot = 'X'.
  APPEND ls_sort_wa TO gt_sort.

*  ls_sort_wa-spos = 3.
*  ls_sort_wa-fieldname = 'NUM_ENTRADA'.
*  ls_sort_wa-up = 'X'.
*  ls_sort_wa-subtot = 'X'.
*  APPEND ls_sort_wa TO gt_sort.

ENDFORM.                    " AJUSTA_PROPRIEDADES
*&---------------------------------------------------------------------*
*&      Form  init_tree
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree.

  PERFORM fieldcat_init.

  PERFORM ajusta_propriedades.

* create Event Receiver
*  CREATE OBJECT tree_event_receiver.

* create container for alv-tree
  DATA: l_tree_container_name(30) TYPE c,
        l_custom_container        TYPE REF TO cl_gui_custom_container.
  l_tree_container_name = 'TREE'.

  CREATE OBJECT l_custom_container
    EXPORTING
      container_name              = l_tree_container_name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

* create tree control
  CREATE OBJECT tree
    EXPORTING
      i_parent                    = l_custom_container
      i_node_selection_mode       =
                                    cl_gui_column_tree=>node_sel_mode_multiple
      i_item_selection            = 'X'
*     i_no_html_header            = ''
*     i_no_toolbar                = ''
      i_no_html_header            = ''
      i_no_toolbar                = 'X'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.

* create info-table for html-header
  DATA: lt_list_commentary TYPE slis_t_listheader,
        l_logo             TYPE sdydo_value.

*-----------------------------------------------------------Com Mai2005
  PERFORM build_comment USING
                 lt_list_commentary
                 l_logo.
*----------------------------------------------------------------------

* repid for saving variants
  DATA: ls_variant TYPE disvariant.
  ls_variant-report = sy-repid.

* register events
  PERFORM register_events.

* create hierarchy
  CALL METHOD tree->set_table_for_first_display
    EXPORTING
      it_list_commentary = lt_list_commentary
*     i_logo             = l_logo
      i_background_id    = 'ALV_BACKGROUND'
      i_save             = 'A'
      is_variant         = ls_variant
    CHANGING
      it_sort            = gt_sort
      it_outtab          = gt_tab
      it_fieldcatalog    = gt_fieldcatalog.

**   flush
*    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS EXPORTING CONTROL = GRID.
*    CALL METHOD CL_GUI_CFW=>FLUSH.

* expand first level
  CALL METHOD tree->expand_tree
    EXPORTING
      i_level = 1.

* optimize column-width
  CALL METHOD tree->column_optimize
    EXPORTING
      i_start_column = tree->c_hierarchy_column_name
      i_end_column   = tree->c_hierarchy_column_name.

ENDFORM.                    " init_tree

*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_LIST_COMMENTARY  text
*      -->P_L_LOGO  text
*----------------------------------------------------------------------*
FORM build_comment USING
      pt_list_commentary TYPE slis_t_listheader
      p_logo             TYPE sdydo_value.

  DATA: ls_line  TYPE slis_listheader,
        text(60).

* LIST HEADING LINE: TYPE H
  CLEAR ls_line.
  ls_line-typ  = 'A'.
  ls_line-info = 'PORTAS - ESTADO'.
  APPEND ls_line TO pt_list_commentary.

  CLEAR ls_line.
  ls_line-typ  = 'A'.
  GET TIME.
  WRITE sy-datlo TO aux_s_date.
  CONCATENATE 'Data de Execução :'(008) aux_s_date
         INTO text SEPARATED BY space.
  ls_line-info = text.
  APPEND ls_line TO pt_list_commentary.

*-------------------------------------------------------------

  CLEAR: ls_line.
  ls_line-typ  = 'A'.
  GET TIME.
  WRITE sy-timlo TO aux_s_time.
  CONCATENATE 'Hora de Execução :'(009) aux_s_time
         INTO text SEPARATED BY space.

  ls_line-info = text.
  APPEND ls_line TO pt_list_commentary.

*  CLEAR: ls_line, AUX_S_user.
*  ls_line-typ  = 'A'.
*  GET TIME.
*  WRITE sy-uname TO AUX_S_user.
*  CONCATENATE 'Utilizador: ' AUX_S_user INTO text.
*  ls_line-info = text.
*  APPEND ls_line TO pt_list_commentary.

*-------------------------------------------------------------

  CLEAR ls_line.
  ls_line-typ  = 'A'.
  ls_line-info = ' '.
  APPEND ls_line TO pt_list_commentary.

  p_logo = 'RENOVA_LOGO'.
ENDFORM.                    " build_comment


*&---------------------------------------------------------------------*
*&      Form  register_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_events.
* define the events which will be passed to the backend
  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.

* define the events which will be passed to the backend
  CLEAR l_event.
  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_header_context_men_req.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_header_click.
  APPEND l_event TO lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_keypress.
  APPEND l_event TO lt_events.

  CALL METHOD tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.


*  DATA: l_event_receiver TYPE REF TO cl_tree_event_receiver.
*  CREATE OBJECT l_event_receiver.
*  SET HANDLER l_event_receiver->on_add_hierarchy_node FOR tree.
*  SET HANDLER l_event_receiver->handle_double_click
*                                                        FOR tree.
ENDFORM.                    " register_events


**&---------------------------------------------------------------------
**
**&      Form  exit_program
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM exit_program.
*
*  CALL METHOD tree->free.
*  SET SCREEN 0.
*  LEAVE SCREEN.
*
*ENDFORM.                    " exit_program
**---------------------------------------------------------------------*
*
**       FORM START_REFRESH                                            *
*
**---------------------------------------------------------------------*
*
*FORM start_refresh USING taskname.
**  BREAK-POINT.
**  SET USER-COMMAND 'REFR'.
*  ok_code_0001 = 'REFR'.
**  SET SCREEN '0001'.
**  LEAVE SCREEN.
**  LEAVE TO TRANSACTION 'ZWM004'.
*
*
*ENDFORM. "start_refresh

*&---------------------------------------------------------------------*
*&      Form  select_descargas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_descargas .
  DATA: aux_entrada   LIKE zwm005-num_entrada.

  REFRESH izwm005.
  CLEAR izwm005.

  SELECT * FROM zwm005
           WHERE armazem    = xuser-lgnum AND
                 finalizada = ' '.

    MOVE-CORRESPONDING zwm005 TO izwm005.
    izwm005-all = 'Talão'.

    SELECT SINGLE name1 FROM  lfa1 INTO izwm005-desc_transp
           WHERE  lifnr  = izwm005-transportador.

    APPEND izwm005.
  ENDSELECT.

*-----------------------------------------------------------ins Abr2005
  CLEAR zwm005.
*----------------------------------------------------------------------
ENDFORM.                    " select_descargas

*&---------------------------------------------------------------------*
*&      Form  SELECT_CARGAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_cargas.

  REFRESH: carga1,datas,izwm006.

  CLEAR flag.

  dat_aux = sy-datum.
*inicializa range
  datas-sign   = 'I'.
  datas-option = 'BT'.

*--------------------------------------------------------Ins Mai2005

  datas-low    = dat_aux - 60.
  datas-high   = dat_aux + 365.
  APPEND datas.


  DATA: t_vttk   LIKE vttk OCCURS 0 WITH HEADER LINE,
        t_zwm006 LIKE zwm006_aux OCCURS 0 WITH HEADER LINE,
        t_zwm003 LIKE zwm003_aux OCCURS 0 WITH HEADER LINE.

  CLEAR  : t_vttk, t_zwm006, t_zwm003.
  REFRESH: t_vttk, t_zwm006, t_zwm003.

  CLEAR: carga1, carga1[], flag.
  CLEAR zwm006_aux.

  SELECT * FROM zwm006_aux
      WHERE  armazem      = xuser-lgnum
      AND    data_reg    IN datas
      AND    num_entrada NE space
      AND    finalizada   = ' '.

    IF sy-subrc = 0.

      CLEAR carga1.
      carga1-num_entrada  = zwm006_aux-num_entrada.
      carga1-matricula    = zwm006_aux-matricula.
      carga1-porta        = zwm006_aux-porta.
      carga1-observacoes  = zwm006_aux-observacoes.
      carga1-hora_reg     = zwm006_aux-hora_reg.
      carga1-dia_reg      = zwm006_aux-data_reg.
      "Ini-06/06/24-Inetum-Ajm
      carga1-observacoes2  = zwm006_aux-observacoes2.
      "peso bascula
      carga1-pesini = zwm006_aux-pesini.
      carga1-pesfim = zwm006_aux-pesfim.
      carga1-pestg  = zwm006_aux-pestg .
      carga1-difkg  = zwm006_aux-difkg .
      carga1-difp   = zwm006_aux-difp  .
      carga1-meins  = zwm006_aux-meins.
      carga1-carregada  = zwm006_aux-carregada.
      "Ini-25/06/24-Inetum-Ajm - peso Bascula
      carga1-meins = 'KG'.
*      SELECT SUM( btgew )
*        FROM likp
*          INTO @carga1-pestg
*        WHERE vbeln IN ( SELECT vbeln
*                          FROM vttp
*                            WHERE tknum EQ @zwm006_aux-n_transporte ).
      "Fim-25/06/24-Inetum-Ajm - peso Bascula
      "Fim-06/06/24-Inetum-Ajm

      CLEAR vttk.
      SELECT SINGLE * FROM  vttk
             WHERE  tknum  = zwm006_aux-n_transporte.

*      IF sy-subrc = 0 AND NOT vttk-stdis IS INITIAL
*            AND vttk-stten IS INITIAL.

      carga1-matricula    = vttk-signi.

*      ELSE.
*        CLEAR carga1.
*      ENDIF.

    ENDIF.

*---------------------------------------seleccionar dados coplementares
    PERFORM dados_complemantares.

    CLEAR carga1.

  ENDSELECT.

*-----------------------------------------------------------ins Abr2005
  PERFORM inicializa.
*----------------------------------------------------------------------

ENDFORM.                    " SELECT_CARGAS

*&---------------------------------------------------------------------*
*&      Form  fill_tables
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_tables .

  REFRESH: fila_porta, zwm_return_truck,return_msg, tab_portas1.
  CLEAR  : fila_porta, zwm_return_truck,return_msg, tab_portas1.
*TC da fila de espera
  SELECT * FROM zwm003_aux
           WHERE armazem = xuser-lgnum.


    MOVE: zwm003_aux-num_entrada  TO fila_porta-talao,
          zwm003_aux-matricula    TO fila_porta-matricula,
          zwm003_aux-observacoes  TO fila_porta-observacao,
          zwm003_aux-operacao     TO fila_porta-operacao,
          zwm003_aux-hora_entrada TO fila_porta-chegada,
          zwm003_aux-data         TO fila_porta-dia_reg.

    CLEAR: zwm028, fila_porta-pulmoes.
    SELECT  pulmao1 pulmao2 kober total_paletes paletes_pulmao st_dck st_pul
    INTO (zwm028-pulmao1,
          zwm028-pulmao2,
          zwm028-kober,
          zwm028-total_paletes,
          zwm028-paletes_pulmao,
          zwm028-st_dck,
          zwm028-st_pul)
                            FROM  zwm028 AS a
                            INNER JOIN zwm006_aux AS b
                            ON    b~n_transporte = a~transporte
           WHERE  b~armazem      = xuser-lgnum
           AND    b~num_entrada  = zwm003_aux-num_entrada
           AND    a~remessa      EQ space.

    ENDSELECT.

    IF sy-subrc = 0.

      IF NOT zwm028-pulmao1 IS INITIAL AND NOT zwm028-pulmao2 IS INITIAL.
        CONCATENATE zwm028-pulmao1 zwm028-pulmao2 INTO fila_porta-pulmoes
           SEPARATED BY '/'.
      ELSEIF NOT zwm028-pulmao1 IS INITIAL AND zwm028-pulmao2 IS INITIAL.
        fila_porta-pulmoes = zwm028-pulmao1.
        fila_porta-kober = zwm028-kober.
      ELSEIF zwm028-pulmao1 IS INITIAL AND NOT zwm028-pulmao2 IS INITIAL.
        fila_porta-pulmoes = zwm028-pulmao2.
        fila_porta-kober = zwm028-kober.
      ENDIF.

*-------------------------------------------se diferente carga directa *

*                                           verifica nº de paletes
      IF zwm028-st_dck NE space.

      ELSEIF zwm028-paletes_pulmao LT zwm028-total_paletes.

        IF zwm028-st_pul <> 'PUA'.
          CLEAR fila_porta.
        ENDIF.
      ENDIF.

    ELSEIF zwm003_aux-operacao NE 'DESCARGA'.
      CLEAR fila_porta.
    ENDIF.

    IF NOT fila_porta-talao IS INITIAL.
      APPEND fila_porta.
    ENDIF.

  ENDSELECT.

  IF fila_porta[] IS INITIAL.

    MESSAGE i000 WITH 'Não foram seleccionados transportes'(010).

    PERFORM desbloqueio.
    SET SCREEN '0000'. LEAVE SCREEN.
  ENDIF.

*TC da atribuição porta/camiao

  IF tab_portas1[] IS INITIAL.
    CALL FUNCTION 'ZWM_NEXT_TRUCK_S'
      EXPORTING
        armazem                  = xuser-lgnum
      TABLES
        return_truck             = zwm_return_truck
        return_msg               = return_msg
      EXCEPTIONS
        no_door_unlocked         = 1
        exception_manage_parking = 2
        no_queue_exists          = 3
        no_door_truck            = 4
        error_in_get_door        = 5
        error_in_manage_doors    = 6
        OTHERS                   = 7.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

*erro
      DATA: lv_gtarg TYPE eqegtarg.
      CONCATENATE 'G_PORTARIA_' xuser-lgnum INTO lv_gtarg.
      CONDENSE lv_gtarg NO-GAPS.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lv_gtarg
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      READ TABLE return_msg INDEX 1.
      IF sy-subrc = 0.
        MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
       NUMBER return_msg-msgnr WITH return_msg-msgv1 return_msg-msgv2
        return_msg-msgv3 return_msg-msgv4.
        SET SCREEN '0000'.LEAVE SCREEN.
      ENDIF.
    ELSE.

*---------------------------se foi seleccionada uma linha de transporte
*---------------------------vamos, com base no Talão do transporte
*---------------------------seleccionar todas as portas possiveis
      IF NOT indice_fila IS INITIAL.
        READ TABLE fila_porta INDEX indice_fila.
        MOVE-CORRESPONDING fila_porta TO g_port_wa2.
        LOOP AT zwm_return_truck
                          WHERE num_entrada = fila_porta-talao.
          MOVE-CORRESPONDING zwm_return_truck TO tab_portas1.
          APPEND tab_portas1.
        ENDLOOP.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " fill_tables

*&---------------------------------------------------------------------*
*&      Form  actualiza_estado_tabelas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_estado_tabelas .

  DATA: up_zwm002 TYPE zwm002     OCCURS 0 WITH HEADER LINE.
  DATA: up_zwm003 TYPE zwm003_aux OCCURS 0 WITH HEADER LINE.
  DATA: up_zwm005 TYPE zwm005     OCCURS 0 WITH HEADER LINE.
  DATA: up_zwm006 TYPE zwm006_aux OCCURS 0 WITH HEADER LINE.
  DATA: talao    TYPE zwm003_aux-num_entrada,
        operacao TYPE zwm003_aux-operacao,
        data     LIKE zwm006_aux-data_reg,
        hora     LIKE zwm006_aux-hora_reg.

  REFRESH:up_zwm003,up_zwm002,up_zwm005,up_zwm006.
  CLEAR:up_zwm003,up_zwm002,up_zwm005,up_zwm006.

**CARRO QUE DEIXA DE ESTAR ASSOCIADO
**VAR: MATRICULA_O
*actualiza tabela de descargas/cargas
  SELECT SINGLE * FROM zwm005 INTO up_zwm005
  WHERE armazem = xuser-lgnum AND
        matricula = matricula_o.

  IF sy-subrc = 0.
*carro de descarga.
    up_zwm005-porta = '   '.
    APPEND up_zwm005.
    MOVE up_zwm005-num_entrada TO talao.
    hora = up_zwm005-hora_entrada.
    data = up_zwm005-data.
    operacao = 'DESCARGA'.
  ELSE.
*carro de carga
*    CLEAR ZWM006.
    SELECT SINGLE * FROM zwm006_aux INTO up_zwm006
    WHERE armazem = xuser-lgnum AND
          matricula = matricula_o.

    up_zwm006-porta = '   '.
    MOVE up_zwm006-num_entrada TO talao.
    hora = up_zwm006-hora_reg.
    data = up_zwm006-data_reg.
    operacao = 'CARGA'.
    APPEND up_zwm006.
  ENDIF.

*actualiza fila de espera
  CLEAR zwm003_aux.
  SELECT SINGLE * FROM zwm003_aux INTO up_zwm003
                  WHERE armazem  = xuser-lgnum
                  AND   matricula = matricula_o.

  IF sy-subrc = 0.
*actualiza fila de espera
    up_zwm003-estado = 'E'.
    up_zwm003-porta  = '   '.
    GET TIME.
    up_zwm003-hora_saida = ' '.
    APPEND up_zwm003.
  ELSE.
*novo registo na fila de espera.
    up_zwm003-armazem = xuser-lgnum.
    up_zwm003-num_entrada = talao.
    up_zwm003-matricula = matricula_o.
*up_zwm003-OBSERVACOES
    up_zwm003-porta = '   '.
    up_zwm003-operacao = operacao.
    GET TIME.
*fica com o valor que esta na carga/descarga
    up_zwm003-hora_entrada = hora.
    up_zwm003-data = data.

*    UP_ZWM003-HORA_SAIDA
    up_zwm003-estado = 'E'.
    APPEND up_zwm003.
  ENDIF.

*actualiza tabela de fila de espera
  IF NOT up_zwm003[] IS INITIAL.

    CALL FUNCTION 'ZWM_MANAGE_PARKING_V1'
      EXPORTING
        operacao                = '2'
        armazem                 = xuser-lgnum
      TABLES
        l_zwm003                = up_zwm003
        return_msg              = return_msg
      EXCEPTIONS
        tab_zwm003_not_filled   = 1
        invalid_parameter       = 2
        tab_l_zwm003_filled     = 3
        tab_l_zwm003_not_filled = 4
        no_warehouse            = 5
        OTHERS                  = 6.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

*actualiza tabela de descargas
  IF NOT up_zwm005[] IS INITIAL.

    CALL FUNCTION 'ZWM_MANAGE_UNLOADING'
      EXPORTING
        operacao                = '2'
        armazem                 = xuser-lgnum
      TABLES
        l_zwm005                = up_zwm005
        return_msg              = return_msg
      EXCEPTIONS
        no_warehouse            = 1
        tab_l_zwm005_filled     = 2
        tab_zwm005_not_filled   = 3
        tab_l_zwm005_not_filled = 4
        invalid_parameter       = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

*actualiza tabela de Cargas
  IF NOT up_zwm006[] IS INITIAL.

    CALL FUNCTION 'ZWM_MANAGE_LOADING_V1'
      EXPORTING
        operacao                = '2'
        armazem                 = xuser-lgnum
      TABLES
        l_zwm006                = up_zwm006
        return_msg              = return_msg
      EXCEPTIONS
        no_warehouse            = 1
        tab_l_zwm005_filled     = 2
        tab_zwm005_not_filled   = 3
        tab_l_zwm005_not_filled = 4
        invalid_parameter       = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

  REFRESH:up_zwm003,up_zwm002,up_zwm005,up_zwm006.
  CLEAR:up_zwm003,up_zwm002,up_zwm005,up_zwm006.

**CARRO QUE FICA A ESTAR ASSOCIADO
**VAR: MATRICULA_D
  SELECT SINGLE * FROM zwm003_aux INTO up_zwm003
  WHERE armazem   = xuser-lgnum
  AND   matricula = matricula_d.

*actualiza fila de espera
  up_zwm003-estado = 'P'.
  up_zwm003-porta  = porta_d.
  GET TIME.
  up_zwm003-hora_saida = sy-timlo.
  APPEND up_zwm003.

*actualiza tabela de descargas/cargas
  CLEAR zwm005.
  SELECT SINGLE * FROM zwm005 INTO up_zwm005
  WHERE armazem = xuser-lgnum AND
        num_entrada = up_zwm003-num_entrada.

  IF sy-subrc = 0.
*carro de descarga.
    up_zwm005-porta = porta_d.
    APPEND up_zwm005.
  ELSE.
*carro de carga
    CLEAR zwm006_aux.
    SELECT SINGLE * FROM zwm006_aux INTO up_zwm006
                    WHERE armazem     = xuser-lgnum
                    AND   num_entrada = up_zwm003-num_entrada.

    up_zwm006-porta = porta_d.
    APPEND up_zwm006.
  ENDIF.

*actualiza tabela das portas.
  SELECT SINGLE * FROM zwm002 INTO up_zwm002
                  WHERE armazem = xuser-lgnum
                  AND   porta   = porta_d.

  IF sy-subrc = 0.
    up_zwm002-bloqueio = 'X'.
    up_zwm002-estado = 'O'.
*    up_zwm002-matricula = matricula_d.
*    up_zwm002-matricula = matricula_d.

    APPEND up_zwm002.
  ENDIF.



*actualiza estado das portas (ZWM002)
  IF NOT up_zwm002[] IS INITIAL.

    CALL FUNCTION 'ZWM_MANAGE_DOORS'
      EXPORTING
        operacao                = '2'
        armazem                 = xuser-lgnum
      TABLES
        l_zwm002                = up_zwm002
        return_msg              = return_msg
      EXCEPTIONS
        no_warehouse            = 1
        tab_zwm002_not_filled   = 2
        tab_l_zwm002_filled     = 3
        tab_l_zwm002_not_filled = 4
        invalid_parameter       = 5
        OTHERS                  = 6.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

*actualiza tabela de fila de espera
  IF NOT up_zwm003[] IS INITIAL.

    CALL FUNCTION 'ZWM_MANAGE_PARKING_V1'
      EXPORTING
        operacao                = '2'
        armazem                 = xuser-lgnum
      TABLES
        l_zwm003                = up_zwm003
        return_msg              = return_msg
      EXCEPTIONS
        tab_zwm003_not_filled   = 1
        invalid_parameter       = 2
        tab_l_zwm003_filled     = 3
        tab_l_zwm003_not_filled = 4
        no_warehouse            = 5
        OTHERS                  = 6.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

*actualiza tabela de descargas
  IF NOT up_zwm005[] IS INITIAL.

    CALL FUNCTION 'ZWM_MANAGE_UNLOADING'
      EXPORTING
        operacao                = '2'
        armazem                 = xuser-lgnum
      TABLES
        l_zwm005                = up_zwm005
        return_msg              = return_msg
      EXCEPTIONS
        no_warehouse            = 1
        tab_l_zwm005_filled     = 2
        tab_zwm005_not_filled   = 3
        tab_l_zwm005_not_filled = 4
        invalid_parameter       = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

*actualiza tabela de Cargas
  IF NOT up_zwm006[] IS INITIAL.

    CALL FUNCTION 'ZWM_MANAGE_LOADING_V1'
      EXPORTING
        operacao                = '2'
        armazem                 = xuser-lgnum
      TABLES
        l_zwm006                = up_zwm006
        return_msg              = return_msg
      EXCEPTIONS
        no_warehouse            = 1
        tab_l_zwm005_filled     = 2
        tab_zwm005_not_filled   = 3
        tab_l_zwm005_not_filled = 4
        invalid_parameter       = 5
        OTHERS                  = 6.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

ENDFORM.                    " actualiza_estado_tabelas

**&---------------------------------------------------------------------
**
**&      Form  recusa_atribuicao
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM recusa_atribuicao .
*
*
*** Tipo de depósito dos pulmões
*  DATA : st_pul LIKE lagp-lgtyp.
*  CLEAR : st_pul.
*  PERFORM get_parameter USING xuser-lgnum
*                          'ENTRADA_ARMAZEM'
*                          'ST_PUL'
*                           st_pul.
*
*  DATA: up_zwm002 TYPE zwm002     OCCURS 0 WITH HEADER LINE.
*  DATA: up_zwm003 TYPE zwm003_aux OCCURS 0 WITH HEADER LINE.
*  DATA: up_zwm005 TYPE zwm005     OCCURS 0 WITH HEADER LINE.
*  DATA: up_zwm006 TYPE zwm006_aux OCCURS 0 WITH HEADER LINE.
*  DATA: talao TYPE zwm003-num_entrada,
*        operacao TYPE zwm003-operacao,
*        data LIKE zwm006-data,
*        hora LIKE zwm006-hora_entrada.
*
*  REFRESH:up_zwm003,up_zwm002,up_zwm005,up_zwm006.
*  CLEAR:up_zwm003,up_zwm002,up_zwm005,up_zwm006.
*
***CARRO DEIXA DE ESTAR ASSOCIADO
*
**actualiza tabela de descargas/cargas
*  SELECT * FROM zwm005 INTO TABLE up_zwm005
*  WHERE armazem = xuser-lgnum AND
*        matricula = tab_f-matricula AND
*        finalizada = ' '.
*
*  IF sy-subrc = 0.
**carro de descarga.
*    LOOP AT up_zwm005.
*      CLEAR up_zwm005-porta.
**    UP_ZWM005-PORTA = '   '.
**    APPEND UP_ZWM005.
*      MODIFY up_zwm005 INDEX sy-tabix.
*    ENDLOOP.
*
*    READ TABLE up_zwm005 INDEX 1.
*    MOVE up_zwm005-num_entrada TO talao.
*    hora = up_zwm005-hora_entrada.
*    data = up_zwm005-data.
*    operacao = 'DESCARGA'.
*
*  ELSE.
**carro de carga
**    CLEAR ZWM006.
*    SELECT * FROM zwm006_aux INTO TABLE up_zwm006
*             WHERE armazem    = xuser-lgnum
*             AND   matricula  = tab_f-matricula
*             AND   finalizada = ' '.
*
*    LOOP AT up_zwm006.
*      CLEAR up_zwm006-porta.
**    UP_ZWM006-PORTA = '   '.
*      MODIFY up_zwm006 INDEX sy-tabix.
*    ENDLOOP.
*
*    READ TABLE up_zwm006 INDEX 1.
*    MOVE up_zwm006-num_entrada TO talao.
*    hora = up_zwm006-hora_reg.
*    data = up_zwm006-data_reg.
*    operacao = 'CARGA'.
*    APPEND up_zwm006.
*  ENDIF.
*
**actualiza fila de espera
*  CLEAR zwm003_aux.
*  SELECT SINGLE * FROM zwm003_aux INTO up_zwm003
*                  WHERE armazem  = xuser-lgnum
*                  AND   matricula = tab_f-matricula.
*
*  IF sy-subrc = 0.
**actualiza fila de espera
*    up_zwm003-estado = 'E'.
*    up_zwm003-porta  = '   '.
*    GET TIME.
*    up_zwm003-hora_saida = ' '.
*    APPEND up_zwm003.
*  ELSE.
**novo registo na fila de espera.
*    up_zwm003-armazem     = xuser-lgnum.
*    up_zwm003-num_entrada = talao.
*    up_zwm003-matricula   = tab_f-matricula.
**up_zwm003-OBSERVACOES
*    up_zwm003-porta       = '   '.
*    up_zwm003-operacao    = operacao.
*    GET TIME.
**fica com o valor que esta na carga/descarga
*    up_zwm003-hora_entrada = hora.
*    up_zwm003-data = data.
*
**    UP_ZWM003-HORA_SAIDA
*    up_zwm003-estado = 'E'.
*    APPEND up_zwm003.
*  ENDIF.
*
**actualiza tabela das portas.
*  SELECT SINGLE * FROM zwm002 INTO up_zwm002
*  WHERE armazem = xuser-lgnum AND
*        porta   = tab_f-porta.
*  IF sy-subrc = 0.
**actualiza BRAND para o bin
*    DATA: pulmao LIKE zwm002-pulmao_1.
*    CLEAR pulmao.
*
*    DO 2 TIMES VARYING pulmao FROM
*                                        up_zwm002-pulmao_1 NEXT
*                                        up_zwm002-pulmao_2.
*      UPDATE lagp SET brand = ' '
*                  WHERE lgnum = xuser-lgnum AND
*                        lgtyp = st_pul AND
*                        lgpla = pulmao.
*      COMMIT WORK.
*    ENDDO.
*
*
*    up_zwm002-bloqueio = ' '.
*    up_zwm002-estado = 'D'.
*    CLEAR: up_zwm002-pulmao_1,up_zwm002-pulmao_2,up_zwm002-user_name.
****    CLEAR up_zwm002-matricula.
*    APPEND up_zwm002.
*
*  ENDIF.
*
**actualiza estado das portas (ZWM002)
*  IF NOT up_zwm002[] IS INITIAL.
*
*    CALL FUNCTION 'ZWM_MANAGE_DOORS'
*      EXPORTING
*        operacao                = '2'
*        armazem                 = xuser-lgnum
*      TABLES
*        l_zwm002                = up_zwm002
*        return_msg              = return_msg
*      EXCEPTIONS
*        no_warehouse            = 1
*        tab_zwm002_not_filled   = 2
*        tab_l_zwm002_filled     = 3
*        tab_l_zwm002_not_filled = 4
*        invalid_parameter       = 5
*        OTHERS                  = 6.
*
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*  ENDIF.
*
**actualiza tabela de fila de espera
*  IF NOT up_zwm003[] IS INITIAL.
*
*    CALL FUNCTION 'ZWM_MANAGE_PARKING_V1'
*      EXPORTING
*        operacao                = '2'
*        armazem                 = xuser-lgnum
*      TABLES
*        l_zwm003                = up_zwm003
*        return_msg              = return_msg
*      EXCEPTIONS
*        tab_zwm003_not_filled   = 1
*        invalid_parameter       = 2
*        tab_l_zwm003_filled     = 3
*        tab_l_zwm003_not_filled = 4
*        no_warehouse            = 5
*        OTHERS                  = 6.
*
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*  ENDIF.
*
**actualiza tabela de descargas
*  IF NOT up_zwm005[] IS INITIAL.
*
*    CALL FUNCTION 'ZWM_MANAGE_UNLOADING'
*      EXPORTING
*        operacao                = '2'
*        armazem                 = xuser-lgnum
*      TABLES
*        l_zwm005                = up_zwm005
*        return_msg              = return_msg
*      EXCEPTIONS
*        no_warehouse            = 1
*        tab_l_zwm005_filled     = 2
*        tab_zwm005_not_filled   = 3
*        tab_l_zwm005_not_filled = 4
*        invalid_parameter       = 5
*        OTHERS                  = 6.
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*  ENDIF.
*
**actualiza tabela de Cargas
*  IF NOT up_zwm006[] IS INITIAL.
*    CALL FUNCTION 'ZWM_MANAGE_LOADING_V1'
*      EXPORTING
*        operacao                = '2'
*        armazem                 = xuser-lgnum
*      TABLES
*        l_zwm006                = up_zwm006
*        return_msg              = return_msg
*      EXCEPTIONS
*        no_warehouse            = 1
*        tab_l_zwm005_filled     = 2
*        tab_zwm005_not_filled   = 3
*        tab_l_zwm005_not_filled = 4
*        invalid_parameter       = 5
*        OTHERS                  = 6.
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*  ENDIF.
*ENDFORM.                    " recusa_atribuicao

**&---------------------------------------------------------------------
**
**&      Module  help  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE help INPUT.
*
*  DATA:index TYPE sy-tabix.
*  DATA: zzwm003 TYPE zwm003 OCCURS 0 WITH HEADER LINE.
*  DATA: opera TYPE zwm003-operacao.
*
*  CLEAR opera.
*  IF tipo_porta = 'Carga'.
*    opera = 'CARGA'.
*  ELSEIF tipo_porta = 'Descarga'.
*    opera = 'DESCARGA'.
*  ELSE.
*    CLEAR opera.
*  ENDIF.
*
*  REFRESH zzwm003.
*
*  IF NOT opera IS INITIAL.
*    SELECT * FROM zwm003 INTO TABLE zzwm003
*    WHERE armazem  = xuser-lgnum AND
*          estado   = 'E' AND
*          operacao = opera.
*  ELSE.
*    SELECT * FROM zwm003 INTO TABLE zzwm003
*    WHERE armazem  = xuser-lgnum AND
*        estado   = 'E'.
*  ENDIF.
*
**retirar da lista as cargas que nao tem n de transporte
*
**  LOOP AT ZZWM003.
**    MOVE SY-TABIX TO INDEX.
**    IF ZZWM003-OPERACAO = 'CARGA'.
**      SELECT SINGLE * FROM ZWM006
**      WHERE ARMAZEM     = XUSER-LGNUM AND
**            NUM_ENTRADA = ZZWM003-NUM_ENTRADA.
**
**      IF SY-SUBRC = 0 AND ZWM006-N_TRANSPORTE IS INITIAL.
**        DELETE ZZWM003 INDEX index.
**      ENDIF.
**    ENDIF.
**  ENDLOOP.
*
*  CLEAR index.
*
*  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
*    EXPORTING
**   CUCOL                               = 20
*   curow                               = 10
**   DISPLAY                             = ' '
*      selectfield                         = 'MATRICULA'
*      tablename                           = 'ZWM003'
*      given_value                         = ' '
**   SHOW_ALL_VALUES_AT_FIRST_TIME       = ' '
*   titel                               = 'Carros em Fila de Espera'
*   IMPORTING
*     ind                                 = index
*    TABLES
*      full_table                          = zzwm003
*   EXCEPTIONS
*     no_tablefields_in_dictionary        = 1
*     no_tablestructure_given             = 2
*     more_then_one_selectfield           = 3
*     no_selectfield                      = 4
*     OTHERS                              = 5.
*
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ELSE.
*    READ TABLE zzwm003 INDEX index.
*    IF sy-subrc = 0.
*      matricula_d = zzwm003-matricula.
*    ENDIF.
*  ENDIF.
*
*ENDMODULE.                 " help  INPUT

*&---------------------------------------------------------------------*
*&      Form  check_carga_pulmao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_camiao_pulmao CHANGING tipoc.
*  BREAK-POINT.
  CLEAR: vttp, vbss, vbsk, zwm028.

  SELECT SINGLE * FROM vttp
                  WHERE tknum = doc_carga.

  IF sy-subrc = 0.
    SELECT * FROM vbss WHERE vbeln = vttp-vbeln.

      SELECT SINGLE * FROM vbsk
                      WHERE sammg = vbss-sammg
                      AND   smart = 'W'.
      IF sy-subrc = 0.
        EXIT.
      ELSE.
        CLEAR vbss.
      ENDIF.
    ENDSELECT.

    SELECT SINGLE * FROM zwm028
                    WHERE lgnum = xuser-lgnum
                    AND   refnr = vbss-sammg.

    IF zwm028-st_pul = 'PUL' OR
       zwm028-st_dck = 'PUL' OR
       zwm028-st_pul = 'PLT' OR
       zwm028-st_pul = 'PLM' OR
       zwm028-st_pul = 'PUA'.
      tipoc = 'P'.
    ELSEIF zwm028-st_dck = 'DCK'.
      tipoc = 'E'.
    ELSE.
** nao tem nada definido
      tipoc = 'O'.
    ENDIF.
  ELSE.
    tipoc = 'O'.
*     tipoc = 'E'.
  ENDIF.
ENDFORM.                    " check_carga_pulmao

*&---------------------------------------------------------------------*
*&      Form  ACTUALIZA_ORDENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_ordens .

  IF NOT doc_compra1 IS INITIAL.
    ordens-ebeln = doc_compra1.
    APPEND ordens.
  ELSEIF NOT doc_compra2 IS INITIAL.
    ordens-ebeln = doc_compra2.
    APPEND ordens.
  ELSEIF NOT doc_compra3 IS INITIAL.
    ordens-ebeln = doc_compra3.
    APPEND ordens.
  ELSEIF NOT doc_compra4 IS INITIAL.
    ordens-ebeln = doc_compra4.
    APPEND ordens.
  ELSEIF NOT doc_compra5 IS INITIAL.
    ordens-ebeln = doc_compra5.
    APPEND ordens.
  ELSEIF NOT doc_compra6 IS INITIAL.
    ordens-ebeln = doc_compra6.
    APPEND ordens.
  ELSEIF NOT doc_compra7 IS INITIAL.
    ordens-ebeln = doc_compra7.
    APPEND ordens.
  ELSEIF NOT doc_compra8 IS INITIAL.
    ordens-ebeln = doc_compra8.
    APPEND ordens.
  ELSEIF NOT doc_compra9 IS INITIAL.
    ordens-ebeln = doc_compra9.
    APPEND ordens.
  ELSEIF NOT doc_compra10 IS INITIAL.
    ordens-ebeln = doc_compra10.
    APPEND ordens.
  ENDIF.

ENDFORM.                    " ACTUALIZA_ORDENS

**&---------------------------------------------------------------------
**
**&      Form  ACTUALIZA_SAIDA_CAMIAO
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM actualiza_saida_camiao .
*
*
*  CLEAR : pulmao, st_pul.
*
*  PERFORM get_parameter USING xuser-lgnum
*                          'ENTRADA_ARMAZEM'
*                          'ST_PUL'
*                           st_pul.
*
*  GET TIME.
*
*** Libertar Pulmão(oes) de Descarga
*  SELECT SINGLE * FROM zwm005
*                  WHERE armazem = xuser-lgnum AND
*                        num_entrada = talao_saida.
*  IF sy-subrc <> 0.
*
*** Verificar se é um camião de Carga
*    SELECT SINGLE * FROM zwm006_aux
*                    WHERE armazem = xuser-lgnum
*                    AND   num_entrada = talao_saida.
*    IF sy-subrc <> 0.
*
*      PERFORM desbloqueio.
*
*** Erro a indicar que o talão não deu entrada no armazém
*      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '060'.
*      CLEAR : talao_saida.
*      EXIT.
*
*    ELSE.
*
***  Se o Camião não tiver porta não é necessária nenhuma validação para
***  fazer saida
*      IF zwm006_aux-porta IS INITIAL.
*        DELETE FROM zwm006_aux
*               WHERE armazem     = xuser-lgnum
*               AND   num_entrada = talao_saida.
*
*        DELETE FROM zwm003_aux
*               WHERE armazem     = xuser-lgnum
*               AND   num_entrada = talao_saida.
*
*        COMMIT WORK.
*
*        PERFORM desbloqueio.
*
*        CLEAR talao_saida.
*        EXIT.
*      ENDIF.
*** Só pode fazer a saída se a carga estiver completa.
*
*      SELECT SINGLE * FROM vttp
*                      WHERE tknum = zwm006_aux-n_transporte.
*
*      IF sy-subrc = 0.
*        SELECT * FROM vbss WHERE vbeln = vttp-vbeln.
*
**----------------seleccionar todas as remessas e verificar se todas
**----------------tem status de saida de mercadoria
*
*          PERFORM get_status_de_remessas USING vbss-sammg  g_rc.
*
*          IF g_rc NE 0.
*            MESSAGE e000 WITH 'O Grupo ' vbss-sammg
*                              'tem remessas sem Saida de Mercadoria' .
*          ENDIF.
*
*
*        ENDSELECT.
*      ENDIF.
**      break roffd.
*      IF g_rc NE 0.
*
*        PERFORM desbloqueio.
*
*        CLEAR : talao_saida.
*        EXIT.
*      ENDIF.
*
*** Já tenho a porta e vou libertar pulmões - actualizar LAGP
*      CLEAR zwm002.
*      SELECT SINGLE * FROM zwm002
*                      WHERE armazem = xuser-lgnum AND
*                            porta = zwm006_aux-porta.
*      IF sy-subrc = 0.
*** Tabela para actualização do estado das portas
*        MOVE-CORRESPONDING zwm002 TO ti_zwm002.
*        APPEND ti_zwm002.
*
*        DO 2 TIMES VARYING pulmao FROM
*                                       zwm002-pulmao_1 NEXT
*                                       zwm002-pulmao_2.
*          UPDATE lagp SET brand = ' '
*                      WHERE lgnum = xuser-lgnum AND
*                            lgtyp = st_pul AND
*                            lgpla = pulmao.
*
*
**** apagar as paletes que estão no pulmão
*          DATA pulmao1(14).
*          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
*            EXPORTING
*              lgtyp = st_pul
*              lgpla = pulmao
*            IMPORTING
*              bin   = pulmao1.
*
**          DELETE FROM zwm013
**            WHERE armazem = xuser-lgnum AND destino = pulmao1.
*
*          COMMIT WORK.
*        ENDDO.
*      ENDIF.
*
*** Libertar Porta
*      READ TABLE ti_zwm002 INDEX 1.
*
*      CLEAR ti_zwm002-bloqueio.
*      ti_zwm002-estado = 'D'.
****      CLEAR ti_zwm002-matricula.
*      CLEAR ti_zwm002-pulmao_1.
*      CLEAR ti_zwm002-pulmao_2.
*      CLEAR ti_zwm002-user_name.
*      MODIFY ti_zwm002 INDEX 1.
*
*      MODIFY zwm002 FROM TABLE ti_zwm002.
*      COMMIT WORK.
*
*** Actualizar tabela de descarga com o pisco a indicar que está
*** finalizada
*      UPDATE zwm006_aux SET finalizada = 'X'
*                        hora_saida = sy-uzeit
*                        data_saida = sy-datum
*                  WHERE armazem     = xuser-lgnum
*                  AND   num_entrada = talao_saida.
*      COMMIT WORK.
*
**actualiza o transporte.
**regista hora de saida
*      SELECT SINGLE * FROM vttk
*                      WHERE tknum = zwm006_aux-n_transporte.
*
*      IF sy-subrc = 0.
*
*        CALL FUNCTION 'ZWM_CHANGE_SHIPMENT'
*          EXPORTING
*            matricula               = ' '
*            tipo_camiao             = ' '
*            n_transporte            = vttk-tknum
*            transportador           = ' '
*            dalen                   = sy-datum
*            ualen                   = sy-uzeit
*            datbg                   = sy-datum
*            uatbg                   = sy-uzeit
*          TABLES
*            return_msg              = return_msg
*          EXCEPTIONS
*            shipment_does_not_exist = 1
*            OTHERS                  = 2.
*
*
*        IF sy-subrc <> 0.
*
*          PERFORM desbloqueio.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*
*  ELSE. """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*** Talão de Descarga
*
***  Se o Camião não tiver porta não é necessária nenhuma validação para
***  fazer saida
*    IF zwm005-porta IS INITIAL.
*
*      DELETE FROM zwm005
*             WHERE armazem = xuser-lgnum AND
*                 num_entrada = talao_saida.
*
*      DELETE FROM zwm003_aux
*             WHERE armazem     = xuser-lgnum
*             AND   num_entrada = talao_saida.
*
*      COMMIT WORK.
*
*      PERFORM desbloqueio.
*
*      CLEAR talao_saida.
*
*      EXIT.
*    ENDIF.
*
*    DATA aux_porta LIKE zwm016-porta.
*    CONCATENATE 'DCK 000-000-' zwm005-porta+1(2) INTO aux_porta.
*
*** Verificar se a descarga ja foi finalizada.
*    SELECT SINGLE * FROM zwm016
*                    WHERE armazem = xuser-lgnum
*                    AND   porta   = aux_porta.
*
*    IF zwm016-finalizada IS INITIAL.
*
*      PERFORM desbloqueio.
*
*      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '057' WITH talao_saida.
*      CLEAR : talao_saida.
*      EXIT.
*    ENDIF.
*
*** Já tenho a porta e vou libertar pulmões - actualizar LAGP
*    SELECT SINGLE * FROM zwm002
*                    WHERE armazem = xuser-lgnum
*                    AND   porta   = zwm005-porta.
*    IF sy-subrc = 0.
*** Tabela para actualização do estado das portas
*      MOVE-CORRESPONDING zwm002 TO ti_zwm002.
*      APPEND ti_zwm002.
*
*      DO 2 TIMES VARYING pulmao FROM
*                                     zwm002-pulmao_1 NEXT
*                                     zwm002-pulmao_2.
*        UPDATE lagp SET brand = ' '
*                    WHERE lgnum = xuser-lgnum AND
*                          lgtyp = st_pul AND
*                          lgpla = pulmao.
*        COMMIT WORK.
*      ENDDO.
*    ENDIF.
*
*** Libertar Porta
*    READ TABLE ti_zwm002 INDEX 1.
*
*    CLEAR ti_zwm002-bloqueio.
*    ti_zwm002-estado = 'D'.
****    CLEAR ti_zwm002-matricula.
*    CLEAR ti_zwm002-pulmao_1.
*    CLEAR ti_zwm002-pulmao_2.
*    CLEAR ti_zwm002-user_name.
*    MODIFY ti_zwm002 INDEX 1.
*
*    MODIFY zwm002 FROM TABLE ti_zwm002.
*    COMMIT WORK.
*
*** Actualizar tabela de descarga com o pisco a indicar que está
*** finalizada
*    UPDATE zwm005 SET finalizada = 'X'
*                      hora_saida = sy-uzeit
*                  WHERE armazem = xuser-lgnum AND
*                        num_entrada = talao_saida.
*    COMMIT WORK.
*
**  limpar as tabelas zwm0017, zwm0018, zwm023
*
*    DATA clearporta(14).
*    DATA t_zwm017 LIKE zwm017 OCCURS 0 WITH HEADER LINE.
*
*    CLEAR: clearporta, t_zwm017.
*    REFRESH t_zwm017.
*
*    CONCATENATE 'DCK 000-000-' zwm005-porta+1(2) INTO clearporta.
*
*    SELECT * INTO TABLE t_zwm017
*        FROM zwm017
*            WHERE armazem = xuser-lgnum AND
*                  num_entrada = talao_saida.
*
*    IF sy-subrc = 0.
*      LOOP AT t_zwm017.
*        DELETE FROM zwm023
*          WHERE armazem = t_zwm017-armazem AND
*                ebeln = t_zwm017-ebeln AND
*                ebelp = t_zwm017-ebelp.
*        COMMIT WORK.
*      ENDLOOP.
*    ENDIF.
*
*    DELETE FROM zwm016
*        WHERE armazem = xuser-lgnum AND porta = clearporta.
*    COMMIT WORK.
*
*    DELETE FROM zwm017
*        WHERE armazem = xuser-lgnum AND num_entrada = talao_saida.
*    COMMIT WORK.
*
*  ENDIF.
*
*
*** Retirar a entrada ( caso exista ) da fila de espera
*  SELECT SINGLE * FROM zwm003_aux
*                  WHERE armazem     = xuser-lgnum
*                  AND   num_entrada = talao_saida.
*  IF sy-subrc = 0.
*** Existe entrada ... actualizar fila de espera e colocar entrada no
*** histórico.
*    REFRESH : return_msg.
*
*    MOVE-CORRESPONDING zwm003_aux TO ti_zwm003.
*    APPEND ti_zwm003.
*
*    CALL FUNCTION 'ZWM_MANAGE_PARKING_V1'
*      EXPORTING
*        operacao                = '3'
*        armazem                 = xuser-lgnum
*      TABLES
*        l_zwm003                = ti_zwm003
*        return_msg              = return_msg
*      EXCEPTIONS
*        tab_zwm003_not_filled   = 1
*        invalid_parameter       = 2
*        tab_l_zwm003_filled     = 3
*        tab_l_zwm003_not_filled = 4
*        no_warehouse            = 5
*        OTHERS                  = 6.
*    IF sy-subrc <> 0.
*
*      PERFORM desbloqueio.
*
*      RAISE error.
*    ENDIF.
*
*    CLEAR : ti_zwm003.
*    REFRESH : ti_zwm003.
*
*  ENDIF.
*
*
*  PERFORM desbloqueio.
*
*  CLEAR : talao_saida, ti_zwm002, zwm028, zwm002,
*          zwm005, zwm003_aux, zwm016, zwm017, zwm006_aux,
*          lagp.
*  REFRESH : ti_zwm002.
*ENDFORM.                    " ACTUALIZA_SAIDA_CAMIAO

*&---------------------------------------------------------------------*
*&      FORM  GET_PARAMETER
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_WAREHOUSE  TEXT
*      -->P_0035   TEXT
*      -->P_0036   TEXT
*      -->P_HOST   TEXT
*----------------------------------------------------------------------*
FORM get_parameter  USING    whs
                             module
                             param
                             valor.

  IF ti_zwm001[] IS INITIAL.
    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
      EXPORTING
        whs       = whs
      TABLES
        ti_zwm001 = ti_zwm001.
  ENDIF.

  CLEAR zwm001.
  READ TABLE ti_zwm001 WITH KEY      armazem   = whs
                                     processo  = module
                                     parametro = param
                                     BINARY SEARCH.
  IF sy-subrc = 0.
    MOVE ti_zwm001 TO zwm001.
  ENDIF.
  MOVE zwm001-valor TO valor.

ENDFORM.                    " GET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  ACTUALIZA_PULMAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_pulmao .

  break roffd.
  DATA: pulmao1(14),
        pulmao2(14).
** Tipo de depósito dos pulmões
  DATA : st_pul LIKE lagp-lgtyp.

  DATA t_lagp LIKE lagp OCCURS 0 WITH HEADER LINE.
  DATA num_pulmao TYPE i.
  DATA : BEGIN OF lagp_aux OCCURS 2,
           lgpla LIKE lagp-lgpla.
  DATA : END OF lagp_aux.

  CLEAR : st_pul.
  PERFORM get_parameter USING xuser-lgnum
                          'ENTRADA_ARMAZEM'
                          'ST_PUL'
                           st_pul.


  DATA : pulmao LIKE lagp-lgpla.
  DATA : pulmao_aux  LIKE lagp-lgber, num_quantos TYPE i.
  CLEAR : pulmao,pulmao_aux, num_quantos.

  SELECT SINGLE * FROM zwm006_aux
          WHERE num_entrada = num_entrada
          AND   matricula   = matricula
          AND   finalizada  = ' '.

  IF sy-subrc = 0.
    CLEAR zwm028.
    SELECT SINGLE * FROM zwm028
                    WHERE lgnum = xuser-lgnum
                    AND   remessa = ' '
                    AND   transporte = zwm006_aux-n_transporte.

    IF sy-subrc = 0.
      IF zwm028-st_pul = 'PUL'.
        izwm002-pulmao_1 = zwm028-pulmao1.
        izwm002-pulmao_2 = zwm028-pulmao2.
      ELSE.
        CLEAR: izwm002-pulmao_1, izwm002-pulmao_2.
      ENDIF.
    ENDIF.
*  ENDIF.
*
*  IF zwm028-st_pul = 'PUL'.

*    izwm002-pulmao_1 = zwm028-pulmao1.
*    izwm002-pulmao_2 = zwm028-pulmao2.

  ELSE.
    SELECT SINGLE * FROM zwm007
                    WHERE armazem = xuser-lgnum AND
                          porta = porta.
    IF sy-subrc = 0.
      DO 12 TIMES VARYING pulmao FROM
                         zwm007-pulmao1 NEXT
                         zwm007-pulmao2.

** Lê os três caracteres do meio q correspondem à área e identifica
** sempre os pulmões dois a dois
        CLEAR : pulmao_aux.
        pulmao_aux = pulmao+4(3).

        CLEAR: t_lagp, lagp_aux, num_pulmao.
        REFRESH: t_lagp, lagp_aux.

        SELECT * INTO CORRESPONDING FIELDS OF TABLE t_lagp
            FROM lagp
                WHERE lgnum = xuser-lgnum AND
                      lgtyp = 'PUL' AND
                      lgber = pulmao_aux AND
                      brand = ' '.

        IF t_lagp[] IS INITIAL.
          CONTINUE.
        ENDIF.

        DESCRIBE TABLE t_lagp LINES num_pulmao.
        IF num_pulmao = 1.
          CONTINUE.
        ENDIF.

        READ TABLE t_lagp INDEX 1.
        CLEAR: pulmao1, pulmao2.

        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = t_lagp-lgtyp
            lgpla = t_lagp-lgpla
          IMPORTING
            bin   = pulmao1.

        READ TABLE t_lagp INDEX 2.
        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = t_lagp-lgtyp
            lgpla = t_lagp-lgpla
          IMPORTING
            bin   = pulmao2.

        SELECT COUNT(*) INTO num_quantos
                FROM zwm013
                    WHERE armazem = xuser-lgnum AND
                          ( destino = pulmao1 OR
                            destino = pulmao2 ).
        IF sy-subrc = 0.
          IF num_quantos <> 0.
            CONTINUE.
          ENDIF.
        ENDIF.

        READ TABLE t_lagp INDEX 1.
        izwm002-pulmao_1 = t_lagp-lgpla.
        lagp_aux-lgpla = t_lagp-lgpla.
        APPEND lagp_aux.

        READ TABLE t_lagp INDEX 2.
        izwm002-pulmao_2 = t_lagp-lgpla.
        lagp_aux-lgpla = t_lagp-lgpla.
        APPEND lagp_aux.

        EXIT.


      ENDDO.


** Actualizar tabela de posições de depósito (lagp) com o indicador de
** OCUPADO para os dois pulmões de descarga
      LOOP AT lagp_aux.

** FL -> 18/01/2006
*        UPDATE lagp SET brand = 'X'
*                    WHERE lgnum = xuser-lgnum AND
*                          lgtyp = st_pul AND
*                          lgpla = lagp_aux-lgpla.
*        COMMIT WORK.

        CLEAR wa_lagpv.
        SELECT SINGLE * FROM lagp
                WHERE lgnum = xuser-lgnum
                  AND lgtyp = st_pul
                  AND lgpla = lagp_aux-lgpla.

        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING lagp TO wa_lagpv.
          wa_lagpv-brand = 'X'.
          CALL FUNCTION 'L_LAGP_VERAENDERN'
            EXPORTING
              xlagpv = wa_lagpv.

          COMMIT WORK AND WAIT.
        ENDIF.
** FL <- 18/01/2006

      ENDLOOP.

      CLEAR: pulmao_aux, lagp_aux, t_lagp.
      REFRESH : lagp_aux, t_lagp.


    ENDIF.
  ENDIF.
** BS - INS 03.11.2003

ENDFORM.                    " ACTUALIZA_PULMAO

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra1 INPUT.

  IF NOT doc_compra1 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra1.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra1.
      MOVE 'DOC_COMPRA1' TO cursorfield.
      CLEAR doc_compra1.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra1.
      MOVE 'DOC_COMPRA1' TO cursorfield.
      CLEAR doc_compra1.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra1  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra2 INPUT.

  IF NOT doc_compra2 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra2.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra2.
      MOVE 'DOC_COMPRA2' TO cursorfield.
      CLEAR doc_compra2.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra2.
      MOVE 'DOC_COMPRA2' TO cursorfield.
      CLEAR doc_compra2.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra2  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra3 INPUT.

  IF NOT doc_compra3 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra3.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra3.
      MOVE 'DOC_COMPRA3' TO cursorfield.
      CLEAR doc_compra3.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra3.
      MOVE 'DOC_COMPRA3' TO cursorfield.
      CLEAR doc_compra3.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra3  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra4 INPUT.

  IF NOT doc_compra4 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra4.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra4.
      MOVE 'DOC_COMPRA4' TO cursorfield.
      CLEAR doc_compra4.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra4.
      MOVE 'DOC_COMPRA4' TO cursorfield.
      CLEAR doc_compra4.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra4  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra5  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra5 INPUT.

  IF NOT doc_compra5 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra5.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra5.
      MOVE 'DOC_COMPRA5' TO cursorfield.
      CLEAR doc_compra5.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra5.
      MOVE 'DOC_COMPRA5' TO cursorfield.
      CLEAR doc_compra5.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra5  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra6  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra6 INPUT.

  IF NOT doc_compra5 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra6.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra6.
      MOVE 'DOC_COMPRA6' TO cursorfield.
      CLEAR doc_compra6.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra6.
      MOVE 'DOC_COMPRA6' TO cursorfield.
      CLEAR doc_compra6.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra6  INPUT


*&---------------------------------------------------------------------*
*&      Module  check_doc_compra7  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra7 INPUT.

  IF NOT doc_compra7 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra7.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra7.
      MOVE 'DOC_COMPRA7' TO cursorfield.
      CLEAR doc_compra7.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra7.
      MOVE 'DOC_COMPRA7' TO cursorfield.
      CLEAR doc_compra7.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra7  INPUT


*&---------------------------------------------------------------------*
*&      Module  check_doc_compra8  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra8 INPUT.

  IF NOT doc_compra8 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra8.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra8.
      MOVE 'DOC_COMPRA8' TO cursorfield.
      CLEAR doc_compra8.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra8.
      MOVE 'DOC_COMPRA8' TO cursorfield.
      CLEAR doc_compra8.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra8  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_doc_compra9  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra9 INPUT.

  IF NOT doc_compra9 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra9.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra9.
      MOVE 'DOC_COMPRA9' TO cursorfield.
      CLEAR doc_compra9.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra9.
      MOVE 'DOC_COMPRA9' TO cursorfield.
      CLEAR doc_compra9.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra9  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_compra10  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra10 INPUT.

  IF NOT doc_compra10 IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra10.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra10.
      MOVE 'DOC_COMPRA10' TO cursorfield.
      CLEAR doc_compra10.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra10.
      MOVE 'DOC_COMPRA10' TO cursorfield.
      CLEAR doc_compra10.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_doc_compra10  INPUT

**&---------------------------------------------------------------------
**
**&      Module  f4_cod_mot  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE f4_cod_mot INPUT.
*
*  DATA: itab_f4 LIKE zwm036 OCCURS 0 WITH HEADER LINE.
*
*  DATA: BEGIN OF itab_dados OCCURS 0,
*          cod_mot LIKE zwm036-cod_motivo,
*          motivo  LIKE zwm036-motivo,
*        END OF itab_dados.
*
*  DATA: l_cod_dev LIKE zwm036-cod_dev.
*
*  DATA: l_tit(40).
*
*  FREE: itab_dynp, itab_return, itab_f4, itab_dados.
*  CLEAR: itab_dynp, itab_return, itab_f4, itab_dados.
*
*  CHECK NOT cod_dev IS INITIAL.
*
*  itab_dynp-fieldname = 'COD_DEV'.
*  APPEND itab_dynp.
*
*  itab_dynp-fieldname = 'COD_MOT'.
*  APPEND itab_dynp.
*
*** Obtém valores para o search-help
*  CALL FUNCTION 'DYNP_VALUES_READ'
*    EXPORTING
*      dyname               = sy-cprog
*      dynumb               = sy-dynnr
*    TABLES
*      dynpfields           = itab_dynp
*    EXCEPTIONS
*      invalid_abapworkarea = 1
*      invalid_dynprofield  = 2
*      invalid_dynproname   = 3
*      invalid_dynpronummer = 4
*      invalid_request      = 5
*      no_fielddescription  = 6
*      invalid_parameter    = 7
*      undefind_error       = 8
*      double_conversion    = 9
*      stepl_not_found      = 10
*      OTHERS               = 11.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  CLEAR: itab_dynp.
*  READ TABLE itab_dynp INDEX 1.
*  l_cod_dev = itab_dynp-fieldvalue.
*
*  PERFORM converte_formato_interno USING l_cod_dev
*                                   CHANGING l_cod_dev.
*
*  CLEAR: zwm036.
*  SELECT * FROM zwm036
*  INTO CORRESPONDING FIELDS OF TABLE itab_f4
*  WHERE cod_dev EQ cod_dev.
*
*  LOOP AT itab_f4.
*    MOVE-CORRESPONDING itab_f4 TO itab_dados.
*    itab_dados-cod_mot = itab_f4-cod_motivo.
*    APPEND itab_dados.
*  ENDLOOP.
*
*  CONCATENATE 'Motivos de devolução para' cod_dev
*        INTO l_tit SEPARATED BY space.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'COD_MOT'
*      dynpprog        = sy-cprog
*      dynpnr          = sy-dynnr
*      dynprofield     = 'COD_MOT'
*      value_org       = 'S'
*      window_title    = l_tit
*    TABLES
*      value_tab       = itab_dados
*      return_tab      = itab_return
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    READ TABLE itab_return INDEX 1.
*    READ TABLE itab_dynp INDEX 2.
*    itab_dynp-fieldvalue = itab_return-fieldval.
*    MODIFY itab_dynp INDEX 2.
*  ENDIF.
*
*** Passar valores para o search-help
*  CALL FUNCTION 'DYNP_VALUES_UPDATE'
*    EXPORTING
*      dyname               = sy-cprog
*      dynumb               = sy-dynnr
*    TABLES
*      dynpfields           = itab_dynp
*    EXCEPTIONS
*      invalid_abapworkarea = 1
*      invalid_dynprofield  = 2
*      invalid_dynproname   = 3
*      invalid_dynpronummer = 4
*      invalid_request      = 5
*      no_fielddescription  = 6
*      undefind_error       = 7
*      OTHERS               = 8.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDMODULE.                 " f4_cod_mot  INPUT

**&---------------------------------------------------------------------
**
**&      Form  converte_formato_interno
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**      -->P_IN   text
**      <--P_OUT  text
**----------------------------------------------------------------------
**
*FORM converte_formato_interno  USING    p_in
*                               CHANGING p_out.
*  CLEAR: p_out.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = p_in
*    IMPORTING
*      output = p_out.
*
*ENDFORM.                    " converte_formato_interno

**&---------------------------------------------------------------------
**
**&      Module  f4_lote  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE f4_lote INPUT.
*
*  DATA: BEGIN OF itab_lote_f4 OCCURS 0,
*          charg LIKE mch1-charg,
*        END OF itab_lote_f4.
*
*  FREE: itab_dynp, itab_return, itab_lote_f4.
*  CLEAR: itab_dynp, itab_return, itab_lote_f4.
*
*  CHECK NOT material IS INITIAL.
*
*  itab_dynp-fieldname = 'MATERIAL'.
*  APPEND itab_dynp.
*
*  itab_dynp-fieldname = 'LOTE'.
*  APPEND itab_dynp.
*
**** Obtém valores para o search-help
*  CALL FUNCTION 'DYNP_VALUES_READ'
*    EXPORTING
*      dyname               = sy-cprog
*      dynumb               = sy-dynnr
*    TABLES
*      dynpfields           = itab_dynp
*    EXCEPTIONS
*      invalid_abapworkarea = 1
*      invalid_dynprofield  = 2
*      invalid_dynproname   = 3
*      invalid_dynpronummer = 4
*      invalid_request      = 5
*      no_fielddescription  = 6
*      invalid_parameter    = 7
*      undefind_error       = 8
*      double_conversion    = 9
*      stepl_not_found      = 10
*      OTHERS               = 11.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  PERFORM converte_formato_interno USING doc_renova
*                                   CHANGING l_vbeln.
*
*  PERFORM converte_formato_interno USING material
*                                   CHANGING l_matnr.
*
*  CLEAR: lips.
*  SELECT * FROM lips
*  INTO CORRESPONDING FIELDS OF TABLE itab_lote_f4
*  WHERE vbeln EQ l_vbeln
*    AND matnr EQ l_matnr.
*
*  IF sy-subrc NE 0.
*    CLEAR: mch1.
*    SELECT * FROM mch1
*    INTO CORRESPONDING FIELDS OF TABLE itab_lote_f4
*    WHERE matnr EQ l_matnr.
*
*    CONCATENATE 'Lotes para o material' material
*          INTO l_tit SEPARATED BY space.
*  ELSE.
*
*    CONCATENATE 'Lotes para a remessa' doc_renova
*                'e material' material
*          INTO l_tit SEPARATED BY space.
*  ENDIF.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'LOTE'
*      dynpprog        = sy-cprog
*      dynpnr          = sy-dynnr
*      dynprofield     = 'LOTE'
*      value_org       = 'S'
*      window_title    = l_tit
*    TABLES
*      value_tab       = itab_lote_f4
*      return_tab      = itab_return
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    READ TABLE itab_return INDEX 1.
*    READ TABLE itab_dynp INDEX 2.
*    itab_dynp-fieldvalue = itab_return-fieldval.
*    MODIFY itab_dynp INDEX 2.
*  ENDIF.
*
*** Passar valores para o search-help
*  CALL FUNCTION 'DYNP_VALUES_UPDATE'
*    EXPORTING
*      dyname               = sy-cprog
*      dynumb               = sy-dynnr
*    TABLES
*      dynpfields           = itab_dynp
*    EXCEPTIONS
*      invalid_abapworkarea = 1
*      invalid_dynprofield  = 2
*      invalid_dynproname   = 3
*      invalid_dynpronummer = 4
*      invalid_request      = 5
*      no_fielddescription  = 6
*      undefind_error       = 7
*      OTHERS               = 8.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDMODULE.                 " f4_lote  INPUT

**&---------------------------------------------------------------------
**
**&      Form  check_cod_dev
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM check_cod_dev .
*
*  CLEAR: l_text.
*
*  CHECK NOT cod_dev IS INITIAL.
*
*  PERFORM converte_formato_interno USING cod_dev
*                                   CHANGING l_cod.
*
*  CALL FUNCTION 'DOMAIN_VALUE_GET'
*    EXPORTING
*      i_domname  = 'ZWMD_COD_DEV'
*      i_domvalue = l_cod
*    IMPORTING
*      e_ddtext   = l_text
*    EXCEPTIONS
*      not_exist  = 1
*      OTHERS     = 2.
*
*  IF sy-subrc <> 0.
*    MOVE 'COD_DEV' TO cursorfield.
*    MESSAGE e000
**    MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '000'
*          WITH 'Erro! O código' cod_dev 'não existe'.
*  ELSE.
*    MOVE 'COD_MOT' TO cursorfield.
*  ENDIF.
*
*ENDFORM.                    " check_cod_dev

**&---------------------------------------------------------------------
**
**&      Form  check_cod_mot
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM check_cod_mot .
*
*  CHECK NOT cod_mot IS INITIAL.
*
*  CLEAR: zwm036, motivo.
*  SELECT SINGLE motivo FROM zwm036
*  INTO motivo
*  WHERE cod_dev    EQ cod_dev
*    AND cod_motivo EQ cod_mot.
*
*  IF sy-subrc NE 0.
*    MESSAGE e000
**    ID 'ZWMMSG001' TYPE 'I' NUMBER '000'
*        WITH 'Erro! O código' cod_mot 'não existe para o cód. motivo'
*             cod_dev.
*  ENDIF.
*
*ENDFORM.                    " check_cod_mot

**&---------------------------------------------------------------------
**
**&      Form  get_f4_remessas
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM get_f4_remessas .
*
*  DATA: BEGIN OF itab_f4 OCCURS 0,
*          vbeln LIKE likp-vbeln,
*        END OF itab_f4.
*
*  FREE: itab_dynp, itab_return, itab_f4.
*  CLEAR: itab_dynp, itab_return, itab_f4.
*
*  CHECK NOT cliente IS INITIAL.
*
*  itab_dynp-fieldname = 'DOC_RENOVA'.
*  APPEND itab_dynp.
*
*** Obtém valores para o search-help
*  CALL FUNCTION 'DYNP_VALUES_READ'
*    EXPORTING
*      dyname               = sy-cprog
*      dynumb               = sy-dynnr
*    TABLES
*      dynpfields           = itab_dynp
*    EXCEPTIONS
*      invalid_abapworkarea = 1
*      invalid_dynprofield  = 2
*      invalid_dynproname   = 3
*      invalid_dynpronummer = 4
*      invalid_request      = 5
*      no_fielddescription  = 6
*      invalid_parameter    = 7
*      undefind_error       = 8
*      double_conversion    = 9
*      stepl_not_found      = 10
*      OTHERS               = 11.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  PERFORM converte_formato_interno USING cliente
*                                   CHANGING l_cliente.
*
*  CLEAR: likp.
*  SELECT * FROM likp
*  INTO CORRESPONDING FIELDS OF TABLE itab_f4
*  WHERE vbtyp EQ 'J'
*    AND kunnr EQ l_cliente.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'DOC_RENOVA'
*      dynpprog        = sy-cprog
*      dynpnr          = sy-dynnr
*      dynprofield     = 'DOC_RENOVA'
*      value_org       = 'S'
*      window_title    = 'Guias de Remessa'
*    TABLES
*      value_tab       = itab_f4
*      return_tab      = itab_return
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    READ TABLE itab_return INDEX 1.
*    READ TABLE itab_dynp INDEX 1.
*    itab_dynp-fieldvalue = itab_return-fieldval.
*    MODIFY itab_dynp INDEX 1.
*  ENDIF.
*
*** Passar valores para o search-help
*  CALL FUNCTION 'DYNP_VALUES_UPDATE'
*    EXPORTING
*      dyname               = sy-cprog
*      dynumb               = sy-dynnr
*    TABLES
*      dynpfields           = itab_dynp
*    EXCEPTIONS
*      invalid_abapworkarea = 1
*      invalid_dynprofield  = 2
*      invalid_dynproname   = 3
*      invalid_dynpronummer = 4
*      invalid_request      = 5
*      no_fielddescription  = 6
*      undefind_error       = 7
*      OTHERS               = 8.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDFORM.                    " get_f4_remessas

**&---------------------------------------------------------------------
**
**&      Form  get_f4_material
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM get_f4_material .
*
*  DATA: BEGIN OF itab_f4 OCCURS 0,
**         maktx LIKE makt-maktx,
*          matnr LIKE mara-matnr,
*        END OF itab_f4.
*
*  DATA: itab_dados LIKE itab_f4 OCCURS 0 WITH HEADER LINE.
*
*  FREE: itab_dynp, itab_return, itab_f4, itab_dados.
*  CLEAR: itab_dynp, itab_return, itab_f4, itab_dados.
*
*  CHECK NOT doc_renova IS INITIAL.
*
*  itab_dynp-fieldname = 'MATERIAL'.
*  APPEND itab_dynp.
*
**  itab_dynp-fieldname = 'DESCRICAO'.
**  APPEND itab_dynp.
*
*** Obtém valores para o search-help
*  CALL FUNCTION 'DYNP_VALUES_READ'
*    EXPORTING
*      dyname               = sy-cprog
*      dynumb               = sy-dynnr
*    TABLES
*      dynpfields           = itab_dynp
*    EXCEPTIONS
*      invalid_abapworkarea = 1
*      invalid_dynprofield  = 2
*      invalid_dynproname   = 3
*      invalid_dynpronummer = 4
*      invalid_request      = 5
*      no_fielddescription  = 6
*      invalid_parameter    = 7
*      undefind_error       = 8
*      double_conversion    = 9
*      stepl_not_found      = 10
*      OTHERS               = 11.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  PERFORM converte_formato_interno USING doc_renova
*                                   CHANGING l_vbeln.
*
*  CLEAR: lips.
*  SELECT * FROM lips
*  INTO CORRESPONDING FIELDS OF TABLE itab_dados
*  WHERE vbeln EQ l_vbeln.
*
*  SORT itab_dados.
*
*  LOOP AT itab_dados.
*    MOVE-CORRESPONDING itab_dados TO itab_f4.
*    COLLECT itab_f4.
*  ENDLOOP.
*
*  CLEAR: l_tit.
*  CONCATENATE 'Materiais para a Remessa' doc_renova INTO l_tit
*  SEPARATED BY space.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'MATERIAL'
*      dynpprog        = sy-cprog
*      dynpnr          = sy-dynnr
*      dynprofield     = 'MATERIAL'
*      value_org       = 'S'
*      window_title    = l_tit
*    TABLES
*      value_tab       = itab_f4
*      return_tab      = itab_return
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    READ TABLE itab_return INDEX 1.
*    READ TABLE itab_dynp INDEX 1.
*    itab_dynp-fieldvalue = itab_return-fieldval.
*    MODIFY itab_dynp INDEX 1.
*  ENDIF.
*
*** Passar valores para o search-help
*  CALL FUNCTION 'DYNP_VALUES_UPDATE'
*    EXPORTING
*      dyname               = sy-cprog
*      dynumb               = sy-dynnr
*    TABLES
*      dynpfields           = itab_dynp
*    EXCEPTIONS
*      invalid_abapworkarea = 1
*      invalid_dynprofield  = 2
*      invalid_dynproname   = 3
*      invalid_dynpronummer = 4
*      invalid_request      = 5
*      no_fielddescription  = 6
*      undefind_error       = 7
*      OTHERS               = 8.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDFORM.                    " get_f4_material

**&---------------------------------------------------------------------
**
**&      Form  check_cliente
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM check_cliente .
*
*  CHECK NOT cliente IS INITIAL.
*
*  PERFORM converte_formato_interno USING cliente
*                                   CHANGING l_cliente.
*
*  CLEAR: kna1.
*  SELECT SINGLE name1 FROM kna1
*  INTO descricao_cliente
*  WHERE kunnr EQ l_cliente.
*
*  IF sy-subrc <> 0.
*    MESSAGE e082 WITH cliente.
*    CLEAR: cliente, descricao_cliente.
*    MOVE 'CLIENTE' TO cursorfield.
*  ELSE.
*    MOVE 'DOC_RENOVA' TO cursorfield.
*  ENDIF.
*
*ENDFORM.                    " check_cliente

**&---------------------------------------------------------------------
**
**&      Form  check_remessa
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM check_remessa .
*
*** Verificar se a factura/Remessa da renova existe ...
*** Caso o campo seja preenchido
*  CHECK NOT doc_renova IS INITIAL.
*
*  PERFORM converte_formato_interno USING doc_renova
*                                   CHANGING l_vbeln.
*
*  PERFORM converte_formato_interno USING cliente
*                                   CHANGING l_cliente.
*
*  SELECT SINGLE * FROM likp
*                  WHERE vbeln EQ l_vbeln
*                    AND kunnr EQ l_cliente.
*  IF sy-subrc <> 0.
*    MESSAGE e000
*        WITH 'A remessa' doc_renova
*             'não existe ou não é do cliente' cliente.
*    CLEAR doc_renova.
*    MOVE 'DOC_RENOVA' TO cursorfield.
**    EXIT.
*  ELSE.
*    MOVE 'MATERIAL' TO cursorfield.
*  ENDIF.
*
*ENDFORM.                    " check_remessa

**&---------------------------------------------------------------------
**
**&      Form  check_material_remessa
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM check_material_remessa .
*
*  CHECK NOT material IS INITIAL.
*
*  PERFORM converte_formato_interno USING material
*                                   CHANGING l_material.
*
*  SELECT SINGLE * FROM mara
*  WHERE matnr EQ l_material.
*
*  IF sy-subrc <> 0.
*** Material inexistente
*    MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '070' WITH material.
*    CLEAR material.
*    MOVE 'MATERIAL' TO cursorfield.
*    EXIT.
*  ELSE.
*
*** Material com status 'APAGADO'
*    IF mara-lvorm = 'X'.
*      MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '070' WITH material.
*      CLEAR material.
*      MOVE 'MATERIAL' TO cursorfield.
*      EXIT.
*    ELSE.
*      SELECT SINGLE maktx FROM makt INTO descricao
*                          WHERE matnr = l_material AND
*                                spras = sy-langu.
*      uni = mara-meins.
*      MOVE 'LOTE' TO cursorfield.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " check_material_remessa

**&---------------------------------------------------------------------
**
**&      Form  check_lote_dev
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM check_lote_dev .
*
*  CHECK NOT lote IS INITIAL.
*
*  PERFORM converte_formato_interno USING material
*                                   CHANGING l_matnr.
*
*  CLEAR: mch1.
*  SELECT SINGLE * FROM mch1
*  WHERE matnr EQ l_matnr
*    AND charg EQ lote.
*
*  IF sy-subrc NE 0.
*    MOVE 'LOTE' TO cursorfield.
*    MESSAGE e000 WITH
*      'O lote' lote 'não é um lote do material' material.
*  ELSE.
*    MOVE 'QUANTIDADE' TO cursorfield.
*  ENDIF.
*
*ENDFORM.                    " check_lote_dev

**&---------------------------------------------------------------------
**
**&      Form  check_material_lote_remessa
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM check_material_lote_remessa .
*
*  CHECK NOT material IS INITIAL.
*
*** Verifica o material
*  PERFORM converte_formato_interno USING doc_renova
*                                   CHANGING l_vbeln.
*
*  PERFORM converte_formato_interno USING material
*                                   CHANGING l_matnr.
*
*  SELECT * FROM lips
*  WHERE vbeln EQ l_vbeln
*    AND matnr EQ l_matnr.
*    EXIT.
*  ENDSELECT.
*  IF sy-subrc NE 0.
*    MESSAGE e000 WITH
*      'O material' material 'não é um material da remessa' doc_renova.
*    MOVE 'MATERIAL' TO cursorfield.
*  ENDIF.
*
*ENDFORM.                    " check_material_lote_remessa

**&---------------------------------------------------------------------
**
**&      Form  ajusta_propriedades_cargas
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM ajusta_propriedades_cargas .
*  DATA ls_sort_wa_c TYPE lvc_s_sort.
** Ordenação
*  REFRESH gt_sort_c.
*
** create sort-table
*  ls_sort_wa_c-spos = 1.
*  ls_sort_wa_c-fieldname = 'NUM_ENTRADA'.
*  ls_sort_wa_c-up = 'X'.
*  ls_sort_wa_c-subtot = 'X'.
**  ls_sort_wa_c-seltext = 'Talão'.
*  APPEND ls_sort_wa_c TO gt_sort_c.
*
**  ls_sort_wa_c-spos = 2.
**  ls_sort_wa_c-fieldname = 'TRANSPORTE'.
**  ls_sort_wa_c-up = 'X'.
**  ls_sort_wa_c-subtot = 'X'.
***  ls_sort_wa_c-seltext = 'Transporte'.
*
*  APPEND ls_sort_wa_c TO gt_sort_c.
*
*  ls_sort_wa_c-spos = 2.
*  ls_sort_wa_c-fieldname = 'REMESSA'.
*  ls_sort_wa_c-up = 'X'.
**  ls_sort_wa_c-subtot = 'X'.
**  ls_sort_wa_c-seltext = 'Remessa'.
*
*  APPEND ls_sort_wa_c TO gt_sort_c.
*
*
*ENDFORM.                    " ajusta_propriedades_cargas

*&---------------------------------------------------------------------*
*&      Form  pop_confirma_saida_merc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pop_confirma_saida_merc USING n_rc p_rc talao grupo.
  DATA : l_text(50).


  CLEAR: rc, l_text, n_rc.
  CONCATENATE talao '-' 'O Grupo'(011) grupo 'sem saida de mercadoria,'(012)
              'Deseja Continuar?'(013) INTO l_text SEPARATED BY space.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Saida de Mercadoria NÃO efectuada'(014)
      text_question         = l_text
      text_button_1         = 'Sim'(001)
*     ICON_BUTTON_1         = ' '
      text_button_2         = 'Não'(002)
*     ICON_BUTTON_2         = ' '
      default_button        = '2'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
*     POPUP_TYPE            =
    IMPORTING
      answer                = n_rc.

  IF n_rc = 2.

    CLEAR: talao_saida, rc, ok_code_0010, n_rc, p_rc.
    MESSAGE e000 WITH 'O Grupo'(015) vbss-sammg
                      'não tem Saida de Mercadoria'(016).

    PERFORM desbloqueio.


    SET SCREEN '0000'. LEAVE SCREEN.

    EXIT.

  ELSE.
*----------mesmo com validação da popup se o nº de paletes
*----------não estiver completo deve resultar erro
    PERFORM valida_numero_paletes.

    CLEAR: rc, l_text, n_rc.
  ENDIF.

ENDFORM.                    " pop_confirma_saida_merc

*&---------------------------------------------------------------------*
*&      Form  confirma_saida_sem_porta
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirma_saida_sem_porta .

  DATA : l_text(50).

  CLEAR: l_text, n_rc.
  CONCATENATE 'Confirma saida do transporte ?'(017) '-' talao_saida '-'
         INTO l_text SEPARATED BY space.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Saida de Transporte'(018)
      text_question         = l_text
      text_button_1         = 'Sim'(001)
*     ICON_BUTTON_1         = ' '
      text_button_2         = 'Não'(002)
*     ICON_BUTTON_2         = ' '
      default_button        = '2'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
*     POPUP_TYPE            =
    IMPORTING
      answer                = n_rc.

  IF n_rc = 2.

    CLEAR: talao_saida, rc, ok_code_0010, n_rc.

    PERFORM desbloqueio.


    SET SCREEN '0000'.  LEAVE SCREEN.

    EXIT.

  ELSE.
    CLEAR: l_text, n_rc.
  ENDIF.

ENDFORM.                    " confirma_saida_sem_porta

*&---------------------------------------------------------------------*
*&      Form  valida_numero_paletes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_numero_paletes .

  CLEAR vbsk.
  SELECT SINGLE *
      FROM vbsk
          WHERE sammg = vbss-sammg AND
                smart = 'W'.


  IF sy-subrc = 0.

    CLEAR zwm028.
    SELECT SINGLE * FROM zwm028
            WHERE lgnum = xuser-lgnum AND
                  refnr = vbss-sammg AND
                  remessa = ' '.

** Carga ainda nao esta preparada no carro
    IF zwm028-total_paletes > zwm028-paletes_carro.
      DATA: lv_gtarg TYPE eqegtarg.
      CONCATENATE 'G_PORTARIA_' xuser-lgnum INTO lv_gtarg.
      CONDENSE lv_gtarg NO-GAPS.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lv_gtarg
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      MESSAGE i164.
      CLEAR: talao_saida, rc, ok_code_0010, n_rc.

      SET SCREEN '0000'. LEAVE SCREEN.

      EXIT.

    ENDIF.

  ELSE.
    MESSAGE i164.
    CLEAR: talao_saida, rc, ok_code_0010, n_rc.

    SET SCREEN '0000'. LEAVE SCREEN.

    EXIT.

  ENDIF.

ENDFORM.                    " valida_numero_paletes

*&---------------------------------------------------------------------*
*&      Form  MODIFICAR_CARGA
*&---------------------------------------------------------------------*
FORM modificar_carga .

  DATA ls_carga TYPE zwm_aux_cargas.
  PERFORM get_sel_line CHANGING ls_carga.

  CHECK ls_carga-transporte IS NOT INITIAL.

  SELECT SINGLE *
    FROM vttk
    INTO @DATA(ls_vttk)
    WHERE tknum EQ @ls_carga-transporte.

  CHECK sy-subrc IS INITIAL.

  PERFORM clear_screen_0004.

  doc_carga     = ls_carga-transporte.
  matricula     = ls_carga-matricula.
  SPLIT ls_carga-matricula
    AT '|'
      INTO matricula_scr
           galera.
  tipo_camiao     = ls_vttk-sdabw.
  transportador   = ls_vttk-tdlnr.
  nome_transp     = ls_carga-transportador.
  observacao      = ls_carga-observacoes.
  observacao2     = ls_carga-observacoes2.

*  gs_0004-pestg  = lv_btgew.
  gs_0004-pesini = ls_carga-pesini.
  gs_0004-pesfim = ls_carga-pesfim.
  gs_0004-pestg  = ls_carga-pestg .
  gs_0004-difkg  = ls_carga-difkg .
  gs_0004-difp   = ls_carga-difp  .
  gs_0004-meins  = 'KG'. "'KG'.

  SELECT SINGLE *
    FROM zwm006_aux
    INTO @DATA(ls_zwm006_aux)
    WHERE n_transporte EQ @ls_carga-transporte.
  IF sy-subrc IS INITIAL.
    gs_0004-dtini = ls_zwm006_aux-dtini.
    gs_0004-hrini = ls_zwm006_aux-hrini.
    gs_0004-dtfim = ls_zwm006_aux-dtfim.
    gs_0004-hrfim = ls_zwm006_aux-hrfim.
  ENDIF.

  gv_scr0004_mode = gc_scr0004-modificar.
  PERFORM call_screen_0004.

  SELECT SINGLE *
    FROM zwm006_aux
    INTO @ls_zwm006_aux
    WHERE n_transporte EQ @ls_carga-transporte.
  IF sy-subrc IS INITIAL AND
     ls_zwm006_aux-porta       IS NOT INITIAL AND
     ls_zwm006_aux-observacoes IS NOT INITIAL.

    REFRESH up_zwm042.
    up_zwm042[] = VALUE #( (
    armazem     = ls_zwm006_aux-armazem
    porta       = ls_zwm006_aux-porta
    num_entrada = ls_zwm006_aux-num_entrada
    matricula   = ls_zwm006_aux-matricula
    observacoes = ls_zwm006_aux-observacoes
    data        = sy-datum
    hora        = sy-uzeit
    ) ).

    CALL FUNCTION 'ZWM_RFC_DISPLAY_PORTARIA'
      EXPORTING
        modo        = 'I'
        chamada_rfc = ' '
*       porta       =
        i_lgnum     = xuser-lgnum " << INS ROFF(SDF):TMGP:29.01.2016 12:43:11
      TABLES
        registos    = up_zwm042
      EXCEPTIONS
        erro        = 1
        erro_modo   = 2
        OTHERS      = 3.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_0004
*&---------------------------------------------------------------------*
FORM call_screen_0004 .

*  CALL SCREEN '0004' STARTING AT  30 10 ENDING AT 108 22.
  CALL SCREEN '0004' STARTING AT  40 02 ENDING AT 118 30.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLEAR_SCREEN_0004
*&---------------------------------------------------------------------*
FORM clear_screen_0004 .

  CLEAR: doc_carga,matricula, tipo_camiao, transportador,
         observacao,nome_transp,
         matricula_scr, galera,
         gv_scr0004_mode,
         observacao2.
  CLEAR gs_0004.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  OBTER_PESO_BASCULA
*&---------------------------------------------------------------------*
FORM obter_peso_bascula  USING VALUE(p_ini_fim).

  DATA lv_parametro  TYPE zwm001-parametro.

  CLEAR ok_code_0004.

  IF p_ini_fim EQ 'I'.
    ASSIGN gs_0004-pesini TO FIELD-SYMBOL(<lfv_peso>).
    ASSIGN gs_0004-dtini TO FIELD-SYMBOL(<lfv_dt>).
    ASSIGN gs_0004-hrini TO FIELD-SYMBOL(<lfv_hr>).
    lv_parametro = 'ENTRADA'.
  ELSEIF p_ini_fim EQ 'F'.
    IF gs_0004-pesini IS INITIAL.
      MESSAGE i000 WITH 'Obter primeiro peso inicial'. "Não existem parametros definidos para este processo (tabela &)
      EXIT.
    ENDIF.
    ASSIGN gs_0004-pesfim TO <lfv_peso>.
    ASSIGN gs_0004-dtfim TO <lfv_dt>.
    ASSIGN gs_0004-hrfim TO <lfv_hr>.
    lv_parametro = 'SAIDA'.
  ENDIF.

  SELECT SINGLE *
    FROM zwm001
    INTO @DATA(ls_balanca)
    WHERE armazem   EQ @xuser-lgnum
      AND processo  EQ 'BASCULA'
      AND parametro EQ @lv_parametro.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE i156 WITH 'ZWM001'. "Não existem parametros definidos para este processo (tabela &)
    EXIT.
  ENDIF.

  CLEAR <lfv_peso>.
  CALL FUNCTION 'ZWMMP_GET_WEIGHT_RFC'
    EXPORTING
      rfc_dest      = 'Z_BASCULA_MP'
      bal_id        = ls_balanca-valor(4)
    IMPORTING
      out_weight    = <lfv_peso>
    EXCEPTIONS
      error         = 1
      error_message = 99
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  <lfv_dt> = sy-datum.
  <lfv_hr> = sy-uzeit.

  PERFORM peso_bascula_dif.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PESO_BASCULA_DIF
*&---------------------------------------------------------------------*
FORM peso_bascula_dif.

  CLEAR gs_0004-difkg.
  CLEAR gs_0004-difp.
  CLEAR gs_0004-pestg.

  IF gs_0004-pesfim IS NOT INITIAL.
    DATA(lv_diff) = gs_0004-pesfim - gs_0004-pesini.
    SELECT SUM( btgew )
      FROM likp
        INTO @gs_0004-pestg "carga1-pestg
      WHERE vbeln IN ( SELECT vbeln
                        FROM vttp
*                          WHERE tknum EQ @zwm006_aux-n_transporte ).
                          WHERE tknum EQ @doc_carga ).

    gs_0004-difkg = gs_0004-pestg - lv_diff. "gs_0004-pesfim. "lv_diff

    TRY.
*      gs_0004-difp = ( gs_0004-pestg / gs_0004-pesfim ) * 100.
*        gs_0004-difp = 100 - ( ( lv_diff / gs_0004-pestg ) * 100 ).
        gs_0004-difp = ( ( lv_diff / gs_0004-pestg ) * 100 ) - 100.
      CATCH cx_sy_zerodivide.

    ENDTRY.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_ALV_TEXT
*&---------------------------------------------------------------------*
FORM set_alv_text  USING VALUE(p_text) CHANGING cs_fcat TYPE lvc_s_fcat.

  cs_fcat-coltext   = p_text.
  cs_fcat-scrtext_l = p_text.
  cs_fcat-scrtext_m = p_text.
  cs_fcat-scrtext_s = p_text.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_SEL_LINE
*&---------------------------------------------------------------------*
FORM get_sel_line  CHANGING pcs_carga TYPE zwm_aux_cargas.

  DATA lt_selected_nodes  TYPE lvc_t_nkey.

  CLEAR pcs_carga.

  treec->get_selected_nodes(
    CHANGING
      ct_selected_nodes = lt_selected_nodes
    EXCEPTIONS
      cntl_system_error = 1
      dp_error          = 2
      failed            = 3
      OTHERS            = 4
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK lt_selected_nodes IS NOT INITIAL.

  READ TABLE lt_selected_nodes INTO DATA(lv_node) INDEX 1.


  treec->get_outtab_line(
    EXPORTING
      i_node_key     = lv_node
    IMPORTING
      e_outtab_line  = pcs_carga
      e_node_text    = DATA(lv_node_text)
      et_item_layout = DATA(lt_item_layout)
      es_node_layout = DATA(ls_node_layout)
    EXCEPTIONS
      node_not_found = 1
      OTHERS         = 2
  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  UC_CONFIRMAR_ATRIBUIR_PORTA
*&---------------------------------------------------------------------*
FORM uc_confirmar_atribuir_porta.

*actualiza alterações efectuadas:
  READ TABLE tab_portas1 INDEX indice_porta.
  IF sy-subrc = 0.
    READ TABLE fila_porta INDEX indice_fila.
    IF sy-subrc = 0.
*---------------------------------------------------------------------
*----------------------No novo processamento a tabela tab_portas1 tem
*----------------------todas as portas disponiveis para cada Talão
*----------------------só é permitido associar porta e transportes com
*----------------------o mesmo Talão
*---------------------------------------------------------------------

      IF tab_portas1-num_entrada <> fila_porta-talao.
        MESSAGE e029.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR aux_index.
  READ TABLE tab_portas1 WITH KEY index = 'X'.
  IF sy-subrc = 0.
    aux_index = 'X'.
  ELSE.
    aux_index = ' '.
    MESSAGE e000 WITH 'Seleccionar um Transporte'(073).
  ENDIF.

  LOOP AT tab_portas1 WHERE index = aux_index.

    REFRESH: up_zwm002, return_msg, up_zwm003, up_zwm005, up_zwm006.
    CLEAR:   up_zwm002, return_msg, up_zwm003, up_zwm005, up_zwm006.

** Actualiza tabela de portas
    up_zwm002-armazem   = xuser-lgnum.
    up_zwm002-porta     = tab_portas1-porta.
    up_zwm002-bloqueio  = 'X'.
    up_zwm002-estado    = 'O'.
    up_zwm002-num_entrada = tab_portas1-num_entrada.

** Pulmões atribuídos às portas de DESCARGA
    up_zwm002-pulmao_1 = tab_portas1-pulmao_1.
    up_zwm002-pulmao_2 = tab_portas1-pulmao_2.
** Pulmões atribuídos às portas de DESCARGA
    APPEND up_zwm002.

*actualiza fila de espera
    CLEAR zwm003_aux.
    SELECT SINGLE * FROM zwm003_aux INTO up_zwm003
                    WHERE armazem = xuser-lgnum
                    AND   num_entrada = tab_portas1-num_entrada.
    IF sy-subrc = 0.
      up_zwm003-estado     = 'P'.
      up_zwm003-porta      = tab_portas1-porta.
      GET TIME.
      up_zwm003-hora_saida = sy-timlo.
      APPEND up_zwm003.
    ENDIF.

*actualiza tabela de descargas
    CLEAR zwm005.
    SELECT  * FROM zwm005 INTO TABLE up_zwm005
              WHERE armazem     = xuser-lgnum
              AND   num_entrada = tab_portas1-num_entrada.

    IF sy-subrc = 0.
      LOOP AT up_zwm005.
        up_zwm005-porta = tab_portas1-porta.
        MODIFY up_zwm005 INDEX sy-tabix.
      ENDLOOP.
*-------------------------actualização das descargas - pulmão -
      IF NOT up_zwm005[] IS INITIAL.

        DATA : BEGIN OF lagp_aux OCCURS 2,
                 lgpla LIKE lagp-lgpla.
        DATA : END OF lagp_aux.

        PERFORM get_parameter USING xuser-lgnum
                                'ENTRADA_ARMAZEM'
                                'ST_PUL'
                                 st_pul.

        SELECT * FROM lagp
                        WHERE lgnum = xuser-lgnum
                        AND   lgtyp = st_pul
                        AND   lgber = tab_portas1-pulmao_1+4(3)
                        AND   brand = ' '.

          IF sy-subrc = 0.
            lagp_aux-lgpla = lagp-lgpla.
            APPEND lagp_aux.
          ENDIF.
        ENDSELECT.
      ENDIF.
    ENDIF.

*actualiza tabela de Cargas
    CLEAR zwm006_aux.
    SELECT SINGLE * FROM zwm006_aux INTO up_zwm006
                    WHERE armazem = xuser-lgnum
                    AND   num_entrada = tab_portas1-num_entrada.

    IF sy-subrc = 0.
      up_zwm006-porta = tab_portas1-porta.
      APPEND up_zwm006.

** Data e Hora de Chegada á Porta
      CALL FUNCTION 'ZWM_CHANGE_SHIPMENT'
        EXPORTING
          n_transporte            = up_zwm006-n_transporte
          dalbg                   = sy-datlo
          ualbg                   = sy-timlo
        TABLES
          return_msg              = return_msg
        EXCEPTIONS
          shipment_does_not_exist = 1
          OTHERS                  = 2.

      IF sy-subrc <> 0.
*            CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*              EXPORTING
*                mode_keyword   = 'X'
*                keyword_       = 'G_PORTARIA'
*              EXCEPTIONS
*                foreign_lock   = 1
*                system_failure = 2
*                OTHERS         = 3.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

*      actualizar a tabela zwm028 com a porta
      SELECT SINGLE * FROM vttp
                      WHERE tknum = up_zwm006-n_transporte.

      IF sy-subrc = 0.
        SELECT * FROM vbss WHERE vbeln = vttp-vbeln.

          SELECT SINGLE * FROM vbsk
                          WHERE sammg = vbss-sammg
                          AND   smart = 'W'.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
            CLEAR vbss.
          ENDIF.
        ENDSELECT.

        SELECT SINGLE * FROM zwm028
                        WHERE lgnum = xuser-lgnum
                        AND   refnr = vbss-sammg.

        CONCATENATE '000-000-' tab_portas1-porta+1(2)
            INTO zwm028-porta.
        MODIFY zwm028.

        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.

    ENDIF.

*actualiza estado das portas (ZWM002)
    IF NOT up_zwm002[] IS INITIAL.

      CALL FUNCTION 'ZWM_MANAGE_DOORS'
        EXPORTING
          operacao                = '2'
          armazem                 = xuser-lgnum
        TABLES
          l_zwm002                = up_zwm002
          return_msg              = return_msg
        EXCEPTIONS
          no_warehouse            = 1
          tab_zwm002_not_filled   = 2
          tab_l_zwm002_filled     = 3
          tab_l_zwm002_not_filled = 4
          invalid_parameter       = 5
          OTHERS                  = 6.

      IF sy-subrc <> 0.
*            CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*              EXPORTING
*                mode_keyword   = 'X'
*                keyword_       = 'G_PORTARIA'
*              EXCEPTIONS
*                foreign_lock   = 1
*                system_failure = 2
*                OTHERS         = 3.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.

*actualiza tabela de fila de espera
    IF NOT up_zwm003[] IS INITIAL.

      CALL FUNCTION 'ZWM_MANAGE_PARKING_V1'
        EXPORTING
          operacao                = '3'              "'2'
          armazem                 = xuser-lgnum
        TABLES
          l_zwm003                = up_zwm003
          return_msg              = return_msg
        EXCEPTIONS
          tab_zwm003_not_filled   = 1
          invalid_parameter       = 2
          tab_l_zwm003_filled     = 3
          tab_l_zwm003_not_filled = 4
          no_warehouse            = 5
          OTHERS                  = 6.

      IF sy-subrc <> 0.
*            CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*              EXPORTING
*                mode_keyword   = 'X'
*                keyword_       = 'G_PORTARIA'
*              EXCEPTIONS
*                foreign_lock   = 1
*                system_failure = 2
*                OTHERS         = 3.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.

*actualiza tabela de descargas
    IF NOT up_zwm005[] IS INITIAL.

** FL -> 10/01/2006
      REFRESH: up_zwm042.
      CLEAR:   up_zwm042.
      LOOP AT up_zwm005.
        MOVE-CORRESPONDING up_zwm005 TO up_zwm042.
        APPEND up_zwm042.
        CLEAR  up_zwm042.
      ENDLOOP.
** FL <- 10/01/2006

      CALL FUNCTION 'ZWM_MANAGE_UNLOADING'
        EXPORTING
          operacao                = '2'
          armazem                 = xuser-lgnum
        TABLES
          l_zwm005                = up_zwm005
          return_msg              = return_msg
        EXCEPTIONS
          no_warehouse            = 1
          tab_l_zwm005_filled     = 2
          tab_zwm005_not_filled   = 3
          tab_l_zwm005_not_filled = 4
          invalid_parameter       = 5
          OTHERS                  = 6.
      IF sy-subrc <> 0.
*            CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*              EXPORTING
*                mode_keyword   = 'X'
*                keyword_       = 'G_PORTARIA'
*              EXCEPTIONS
*                foreign_lock   = 1
*                system_failure = 2
*                OTHERS         = 3.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

      ELSE.

** FL -> 10/01/2006
        CALL FUNCTION 'ZWM_RFC_DISPLAY_PORTARIA'
          EXPORTING
            modo        = 'I'
            chamada_rfc = ' '
*           porta       =
            i_lgnum     = xuser-lgnum " << INS ROFF(SDF):TMGP:29.01.2016 12:43:11
          TABLES
            registos    = up_zwm042
          EXCEPTIONS
            erro        = 1
            erro_modo   = 2
            OTHERS      = 3.

        IF sy-subrc <> 0.
          MESSAGE i242.
        ENDIF.
** FL <- 10/01/2006

*---------------------------------------actualizar os pulmões
        IF NOT lagp_aux[] IS INITIAL.

          LOOP AT lagp_aux.
** FL -> 18/01/2006
            CLEAR wa_lagpv.
            SELECT SINGLE * FROM lagp
                    WHERE lgnum = xuser-lgnum
                      AND lgtyp = st_pul
                      AND lgpla = lagp_aux-lgpla.

            IF sy-subrc EQ 0.
              MOVE-CORRESPONDING lagp TO wa_lagpv.
              wa_lagpv-brand = 'X'.
              CALL FUNCTION 'L_LAGP_VERAENDERN'
                EXPORTING
                  xlagpv = wa_lagpv.

              COMMIT WORK AND WAIT.
            ENDIF.
** FL <- 18/01/2006

          ENDLOOP.

          CLEAR : lagp_aux[], lagp_aux.

        ENDIF.
*------------------------------------------------------------
      ENDIF.

    ENDIF.

*actualiza tabela de Cargas
    IF NOT up_zwm006[] IS INITIAL.

** FL -> 10/01/2006
      REFRESH: up_zwm042.
      CLEAR:   up_zwm042.
      LOOP AT up_zwm006.
        MOVE-CORRESPONDING up_zwm006 TO up_zwm042.
        APPEND up_zwm042.
        CLEAR  up_zwm042.
        EXIT.
      ENDLOOP.
** FL <- 10/01/2006

      CALL FUNCTION 'ZWM_MANAGE_LOADING_V1'
        EXPORTING
          operacao                = '2'
          armazem                 = xuser-lgnum
        TABLES
          l_zwm006                = up_zwm006
          return_msg              = return_msg
        EXCEPTIONS
          no_warehouse            = 1
          tab_l_zwm005_filled     = 2
          tab_zwm005_not_filled   = 3
          tab_l_zwm005_not_filled = 4
          invalid_parameter       = 5
          OTHERS                  = 6.
      IF sy-subrc <> 0.
*            CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*              EXPORTING
*                mode_keyword   = 'X'
*                keyword_       = 'G_PORTARIA'
*              EXCEPTIONS
*                foreign_lock   = 1
*                system_failure = 2
*                OTHERS         = 3.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

** FL -> 10/01/2006
      ELSE.
        CALL FUNCTION 'ZWM_RFC_DISPLAY_PORTARIA'
          EXPORTING
            modo        = 'I'
            chamada_rfc = ' '
*           porta       =
            i_lgnum     = xuser-lgnum " << INS ROFF(SDF):TMGP:29.01.2016 12:43:11
          TABLES
            registos    = up_zwm042
          EXCEPTIONS
            erro        = 1
            erro_modo   = 2
            OTHERS      = 3.

        IF sy-subrc <> 0.
          MESSAGE i242.
        ENDIF.
** FL <- 10/01/2006

      ENDIF.
    ENDIF.

    zcl_wm_gestao_cais_de_carga=>ws_upd_transporte_porta( porta = tab_portas1-porta ).

  ENDLOOP.

  CONCATENATE 'G_PORTARIA_' xuser-lgnum INTO lv_gtarg.
  CONDENSE lv_gtarg NO-GAPS.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = lv_gtarg
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  CLEAR : tab_portas1, fila_porta,up_zwm006,up_zwm005,
          up_zwm003,up_zwm002, g_port_wa2.
  REFRESH: tab_portas1, fila_porta,up_zwm006,up_zwm005,
           up_zwm003,up_zwm002.

  SET SCREEN '0000'.LEAVE SCREEN.

ENDFORM.
