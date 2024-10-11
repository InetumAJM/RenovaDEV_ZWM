*&---------------------------------------------------------------------*
*&  Include           ZWMREP003I01                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit1 INPUT.

  DATA: selected_node TYPE lvc_nkey,
        item_name     TYPE lvc_fname.

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


  IF NOT tab_f IS INITIAL.
    CALL METHOD tree->free.
  ENDIF.

  IF NOT carga1 IS INITIAL.
    CALL METHOD treec->free.
  ENDIF.

  IF NOT izwm005 IS INITIAL.
    CALL METHOD treed->free.
  ENDIF.
  LEAVE PROGRAM.

ENDMODULE.                 " EXIT1  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code_0001.
    WHEN 'REFRESH'.
      CLEAR: flag_tree, flag_treec, flag_tree_d.
    WHEN 'FILA'.
      CLEAR ok_code_0001.
      WRITE icon_system_okay TO v1 AS ICON.
      CLEAR : v2,v3,v4,v5,v6,v7.
*chama ecran Fila de Espera
      PERFORM fill_lista.

    WHEN 'CARGA'.

      CONCATENATE 'G_CARGA_' xuser-lgnum INTO lv_gtarg.
      CONDENSE lv_gtarg NO-GAPS.

      DO.
        CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = lv_gtarg
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc = 0.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDDO.

*      DO.
*        CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
*          EXPORTING
*            mode_keyword   = 'X'
*            keyword_       = 'G_PORTARIA'
*          EXCEPTIONS
*            foreign_lock   = 1
*            system_failure = 2
*            OTHERS         = 3.
*        IF sy-subrc = 0.
*          EXIT.
*        ELSE.
*          WAIT UP TO 1 SECONDS.
*        ENDIF.
*      ENDDO.

*      CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
*        EXPORTING
*          mode_keyword   = 'X'
*          keyword_       = 'G_PORTARIA'
*        EXCEPTIONS
*          foreign_lock   = 1
*          system_failure = 2
*          OTHERS         = 3.
*      IF sy-subrc <> 0.
*        l_user = sy-msgv1.
*        MESSAGE i249 WITH l_user.
*        EXIT.
*      ENDIF.

      CLEAR ok_code_0001.
*chama ecran Regista Descarga
*      CLEAR: doc_carga,matricula, tipo_camiao, transportador,
*             observacao,nome_transp.
      PERFORM clear_screen_0004.
      WRITE icon_system_okay TO v2 AS ICON.
      CLEAR : v1,v3,v4,v5,v6,v7.
*      CALL SCREEN '0004' STARTING AT  30 10 ENDING AT 108 22.
      PERFORM call_screen_0004.

    WHEN 'DESCARGA'.

      CONCATENATE 'G_DESCARGA_' xuser-lgnum INTO lv_gtarg.
      CONDENSE lv_gtarg NO-GAPS.

      DO.
        CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = lv_gtarg
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc = 0.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDDO.

*      DO.
*        CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
*          EXPORTING
*            mode_keyword   = 'X'
*            keyword_       = 'G_PORTARIA'
*          EXCEPTIONS
*            foreign_lock   = 1
*            system_failure = 2
*            OTHERS         = 3.
*        IF sy-subrc = 0.
*          EXIT.
*        ELSE.
*          WAIT UP TO 1 SECONDS.
*        ENDIF.
*      ENDDO.

*      CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
*        EXPORTING
*          mode_keyword   = 'X'
*          keyword_       = 'G_PORTARIA'
*        EXCEPTIONS
*          foreign_lock   = 1
*          system_failure = 2
*          OTHERS         = 3.
*      IF sy-subrc <> 0.
*        l_user = sy-msgv1.
*        MESSAGE i249 WITH l_user.
*        EXIT.
*      ENDIF.

      CLEAR ok_code_0001.
*chama ecran Regista Carga
      CLEAR: doc_compra,matricula, tipo_camiao, transportador,
             observacao,nome_transp.
      WRITE icon_system_okay TO v3 AS ICON.
      CLEAR : v1,v2,v4,v5,v6,v7.
      CALL SCREEN '0003' STARTING AT  32 8 ENDING AT 112 23.

    WHEN 'PORTA'.
*      DO.
*        CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
*          EXPORTING
*            mode_keyword   = 'X'
*            keyword_       = 'G_PORTARIA'
*          EXCEPTIONS
*            foreign_lock   = 1
*            system_failure = 2
*            OTHERS         = 3.
*        IF sy-subrc = 0.
*          EXIT.
*        ELSE.
*          WAIT UP TO 1 SECONDS.
*        ENDIF.
*      ENDDO.

      CONCATENATE 'G_PORTARIA_' xuser-lgnum INTO lv_gtarg.
      CONDENSE lv_gtarg NO-GAPS.

      CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lv_gtarg
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        l_user = sy-msgv1.
        MESSAGE i249 WITH l_user.
        EXIT.
      ENDIF.

      CLEAR ok_code_0001.
*chama ecran Associa Porta
      WRITE icon_system_okay TO v4 AS ICON.
      CLEAR : v1,v2,v3,v5,v6,v7.
***      CALL SCREEN '0005' STARTING AT 23 05 ENDING AT 130 25.
      CALL SCREEN '9005' STARTING AT 12 05 ENDING AT 134 22.

    WHEN 'SAIDA'.

      DATA ls_carga TYPE zwm_aux_cargas.
      PERFORM get_sel_line CHANGING ls_carga.
      IF ls_carga-num_entrada IS NOT INITIAL.
        talao_saida = ls_carga-num_entrada.
      ENDIF.

      CONCATENATE 'G_PORTARIA_' xuser-lgnum INTO lv_gtarg.
      CONDENSE lv_gtarg NO-GAPS.

      CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lv_gtarg
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        l_user = sy-msgv1.
        MESSAGE i249 WITH l_user.
        EXIT.
      ENDIF.

      CLEAR ok_code_0001.
*chama ecran Regista saida
      WRITE icon_system_okay TO v5 AS ICON.
      CLEAR : v1,v2,v3,v4,v6,v7.
**      CALL SCREEN '0010' STARTING AT 30 10 ENDING AT 105 20.
      CALL SCREEN '0010' STARTING AT 30 10 ENDING AT 65 15.

    WHEN 'MOD_CARGA'.
      PERFORM modificar_carga.
    WHEN 'HISTORICO'.
      CALL TRANSACTION 'ZWM075H'.
    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit2 INPUT.


  CLEAR ok_code_0002.
  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT2  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0002 INPUT.

  CASE ok_code_0002.
    WHEN 'BACK'.
      CLEAR ok_code_0002.
      SET SCREEN '0000'.
      LEAVE SCREEN.

    WHEN 'NEXT'.
      CLEAR ok_code_0002.
      SET SCREEN '0000'.
      LEAVE SCREEN.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0002  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit3 INPUT.

  CLEAR: doc_compra,matricula,tipo_camiao,transportador,
         observacao,ok_code_0003,cursorfield,nome_transp.

  CONCATENATE 'G_DESCARGA_' xuser-lgnum INTO lv_gtarg.
  CONDENSE lv_gtarg NO-GAPS.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = lv_gtarg
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT3  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0003 INPUT.

  CASE ok_code_0003.

    WHEN 'BACK'.

      CONCATENATE 'G_DESCARGA_' xuser-lgnum INTO lv_gtarg.
      CONDENSE lv_gtarg NO-GAPS.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lv_gtarg
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      CLEAR cursorfield.
      CLEAR ok_code_0003.
      SET SCREEN '0000'.
      LEAVE SCREEN.

** Colocar mais ordens de compra
    WHEN 'MAIS_ORDEM'.
      CLEAR ok_code_0003.
      CLEAR cursorfield.
      REFRESH return_msg.
** Falta actualizar tabela com as várias ordens de compra escolhidas
      IF NOT doc_compra IS INITIAL.
        CALL SCREEN '0009' STARTING AT 30 10 ENDING AT 100 25.
      ENDIF.

    WHEN 'CONFIRMAR'.
      DATA: rad1(1), rad2(1).

      IF NOT rad1 IS INITIAL.
        IF doc_compra IS INITIAL.
          MESSAGE i000 WITH
               'Tem de preencher o numero da Ordem de Compra'(054).

          MOVE 'DOC_COMPRA' TO cursorfield.
          EXIT.
        ENDIF.
      ENDIF.
      IF NOT matricula IS INITIAL.
        CLEAR ok_code_0003.
        CLEAR cursorfield.
        REFRESH return_msg.
*INSERE DESCARGAS
****REGISTA ENTRADA --- tem de devolver um numero de entrada (talao)

        CALL FUNCTION 'ZWM_ENTRY_UNLOADING'
          EXPORTING
            armazem         = xuser-lgnum
            ord_compra      = doc_compra
            matricula       = matricula
            tipo_camiao     = tipo_camiao
            transportador   = transportador
            observacoes     = observacao
            observacoes2    = observacao2
          IMPORTING
            num_entrada     = num_entrada
          TABLES
            return_msg      = return_msg
            ordens          = ordens
          EXCEPTIONS
            no_warehouse    = 1
            duplicate_entry = 2
            OTHERS          = 3.

        IF sy-subrc <> 0.
*erro

          CONCATENATE 'G_DESCARGA_' xuser-lgnum INTO lv_gtarg.
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
            NUMBER return_msg-msgnr WITH return_msg-msgv1
            return_msg-msgv2 return_msg-msgv3 return_msg-msgv4.
            MOVE 'MATRICULA' TO cursorfield.
          ENDIF.

          CLEAR : ordens.
          REFRESH : ordens.

        ELSE.
          REFRESH : return_msg, ordens.
          CLEAR porta.
*verifica se camião vai para lista de espera
          CALL FUNCTION 'ZWM_ENTRY_PARKING_V1'
            EXPORTING
              armazem           = xuser-lgnum
              tipo_camiao       = 'R'
              matricula         = matricula
              num_entrada       = num_entrada
              observacoes       = observacao
            IMPORTING
              porta             = porta
            TABLES
              return_msg        = return_msg
            EXCEPTIONS
              no_warehouse      = 1
              wrong_tipo_camiao = 2
              OTHERS            = 3.


          IF sy-subrc <> 0.

            CONCATENATE 'G_DESCARGA_' xuser-lgnum INTO lv_gtarg.
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
              NUMBER return_msg-msgnr WITH return_msg-msgv1
              return_msg-msgv2 return_msg-msgv3 return_msg-msgv4.
            ENDIF.

          ELSEIF NOT porta IS INITIAL.
*porta vem preenchida
*actualizar estado das portas

            REFRESH: izwm002,return_msg,tab_zwm005.
            CLEAR: izwm002,return_msg,tab_zwm005.

** Impressão do talão ...
            IF NOT num_entrada IS INITIAL.

              CONCATENATE 'G_DESCARGA_' xuser-lgnum INTO lv_gtarg.
              CONDENSE lv_gtarg NO-GAPS.

              CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
                EXPORTING
                  mode_keyword   = 'X'
                  keyword_       = lv_gtarg
                EXCEPTIONS
                  foreign_lock   = 1
                  system_failure = 2
                  OTHERS         = 3.

              SUBMIT zwmrep0004_pdf WITH p_talao = num_entrada "Nuno Bairro 01/07/2024 Talao PDF
                              AND RETURN.

            ENDIF.

            MESSAGE i000 WITH
            'Impressão em curso  do numero de entrada'(055)
            num_entrada '. É favor dirigir-se à porta'(056) porta.
            CLEAR: doc_compra,matricula,tipo_camiao,transportador,
                   observacao,nome_transp.
            SET SCREEN '0000'.
            LEAVE SCREEN.
          ELSE.
** Impressão do talão ...
            IF NOT num_entrada IS INITIAL.

              CONCATENATE 'G_DESCARGA_' xuser-lgnum INTO lv_gtarg.
              CONDENSE lv_gtarg NO-GAPS.

              CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
                EXPORTING
                  mode_keyword   = 'X'
                  keyword_       = lv_gtarg
                EXCEPTIONS
                  foreign_lock   = 1
                  system_failure = 2
                  OTHERS         = 3.

              SUBMIT zwmrep0004_pdf WITH p_talao = num_entrada "Nuno Bairro 01/07/2024 Talao PDF
                              AND RETURN.

            ENDIF.
*porta não vem preenchida, camiao vai para fila de espera
            MESSAGE i000 WITH 'Fila de espera, documento em impressão'(057)
             num_entrada.

            CLEAR: doc_compra,matricula,tipo_camiao,transportador,
                   observacao,nome_transp.

            IF NOT ordens[] IS INITIAL.
              CLEAR ordens.
              REFRESH ordens.
            ENDIF.

            CLEAR : doc_compra1,
                    doc_compra2,
                    doc_compra3,
                    doc_compra4,
                    doc_compra5,
                    doc_compra6,
                    doc_compra7,
                    doc_compra8,
                    doc_compra9,
                    doc_compra10.

            SET SCREEN '0000'.
            LEAVE SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0003  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_DOC_COMPRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_compra INPUT.

  MOVE 'MATRICULA' TO cursorfield.
  IF NOT doc_compra IS INITIAL.
    SELECT SINGLE * FROM ekko
    WHERE ebeln = doc_compra.

    IF sy-subrc <> 0.
      MESSAGE i007 WITH doc_compra.
      MOVE 'DOC_COMPRA' TO cursorfield.
      CLEAR doc_compra.
    ELSEIF ekko-autlf = 'X'.
      MESSAGE i009 WITH doc_compra.
      MOVE 'DOC_COMPRA' TO cursorfield.
      CLEAR doc_compra.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_DOC_COMPRA  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_matricula  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matricula INPUT.
  IF NOT matricula IS INITIAL.
    TRANSLATE matricula TO UPPER CASE.
    MOVE 'TIPO_CAMIAO' TO cursorfield.
  ELSE.
    MOVE 'MATRICULA' TO cursorfield.
    MESSAGE i010.
  ENDIF.

ENDMODULE.                 " CHECK_matricula  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0004 INPUT.
  DATA: tipoc.

  CASE ok_code_0004.

    WHEN 'OBTER_PESOI' OR 'OBTER_PESOF'.
      PERFORM obter_peso_bascula USING ok_code_0004+10(1).

    WHEN 'BACK'.

      PERFORM inicializa.
      CLEAR: g_del_all, flag_tree, flag_treec, flag_tree_d.

*      PERFORM desbloqueio.

      CONCATENATE 'G_CARGA_' xuser-lgnum INTO lv_gtarg.
      CONDENSE lv_gtarg NO-GAPS.

      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = lv_gtarg
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      CLEAR: cursorfield, ok_code_0004.
      SET SCREEN '0000'. LEAVE SCREEN.

    WHEN 'CONFIRMAR'.

      IF NOT matricula IS INITIAL AND NOT doc_carga IS INITIAL.

        CLEAR: ok_code_0004,tipoc, cursorfield, return_msg.
        REFRESH return_msg.

        IF NOT doc_carga IS INITIAL.
*actualiza o transporte com hora de chegada na portaria.
          CALL FUNCTION 'ZWM_CHANGE_SHIPMENT'
            EXPORTING
              matricula               = matricula
              tipo_camiao             = tipo_camiao
              n_transporte            = doc_carga
              transportador           = transportador
            TABLES
              return_msg              = return_msg
            EXCEPTIONS
              shipment_does_not_exist = 1
              OTHERS                  = 2.

          IF sy-subrc <> 0.

            PERFORM erro USING 'Erro na Actualização do Transporte'(058).

            CONCATENATE 'G_CARGA_' xuser-lgnum INTO lv_gtarg.
            CONDENSE lv_gtarg NO-GAPS.

            CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
              EXPORTING
                mode_keyword   = 'X'
                keyword_       = lv_gtarg
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

*            PERFORM desbloqueio.

*            MOVE 'DOC_CARGA' TO cursorfield.
            EXIT.

          ENDIF.

          COMMIT WORK.

          CALL FUNCTION 'Z_WM_DEQUEUE_TKNUM_WAIT'
            EXPORTING
              i_tknum = doc_carga.

          CALL FUNCTION 'Z_WM_SHIPMENT_SET_STATUS'
            EXPORTING
              i_tknum  = doc_carga
              i_status = 2
            EXCEPTIONS
              error    = 1
              OTHERS   = 2.

        ENDIF.

*regista tabela de cargas --------------------------------- Obter TALÂO
        CALL FUNCTION 'ZWM_ENTRY_LOADING_V1'
          EXPORTING
            armazem         = xuser-lgnum
            matricula       = matricula
            n_transporte    = doc_carga
            tipo_camiao     = tipo_camiao
            observacoes     = observacao
            observacoes2    = observacao2
            modo            = gv_scr0004_mode
            is_zwm_018      = gs_0004
          IMPORTING
            num_entrada     = num_entrada
          TABLES
            return_msg      = return_msg
          EXCEPTIONS
            no_warehouse    = 1
            duplicate_entry = 2
            OTHERS          = 3.


        IF sy-subrc <> 0.

*          PERFORM desbloqueio.

          CONCATENATE 'G_CARGA_' xuser-lgnum INTO lv_gtarg.
          CONDENSE lv_gtarg NO-GAPS.

          CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
            EXPORTING
              mode_keyword   = 'X'
              keyword_       = lv_gtarg
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          PERFORM erro USING ''.

        ELSE.

          REFRESH return_msg.
          CLEAR porta.
*verifica se camião vai para lista de espera

*-------------------------Obter tipo de Carga - Pulmão ou Carga directa
          PERFORM check_camiao_pulmao CHANGING tipoc.
*----------------------------------------------------------------------

          CALL FUNCTION 'ZWM_ENTRY_PARKING_V1'
            EXPORTING
              armazem           = xuser-lgnum
              tipo_camiao       = tipoc
              matricula         = matricula
              num_entrada       = num_entrada
              observacoes       = observacao
              observacoes2      = observacao2
            IMPORTING
              porta             = porta
            TABLES
              return_msg        = return_msg
            EXCEPTIONS
              no_warehouse      = 1
              wrong_tipo_camiao = 2
              OTHERS            = 3.

          IF sy-subrc <> 0.

*            PERFORM desbloqueio.

            CONCATENATE 'G_CARGA_' xuser-lgnum INTO lv_gtarg.
            CONDENSE lv_gtarg NO-GAPS.

            CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
              EXPORTING
                mode_keyword   = 'X'
                keyword_       = lv_gtarg
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            PERFORM erro USING 'Erro na Actualização da Lista de Espera'(059).

          ELSE.
*-----------------------Após Registar carga efectuar Impressão do talão
            PERFORM impressao_talao USING num_entrada.

          ENDIF.

        ENDIF.

      ELSE.
*----------------Valida que o Transporte e a Matricula não estão vazios
        PERFORM valida_campos_vazios.

      ENDIF.

      PERFORM inicializa.
      CLEAR: g_del_all.
      PERFORM clear_screen_0004.
  ENDCASE.

  CLEAR: flag_tree, flag_treec, flag_tree_d.

ENDMODULE.                 " USER_COMMAND_0004  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_DOC_CARGA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_carga INPUT.
  DATA: "itab_dynp   LIKE dynpread    OCCURS 0 WITH HEADER LINE,
    update_dynp.

  DATA: pergunta(100),
        resposta.

  CLEAR: datas, datas[], itab_dynp, itab_dynp[].

** Para a selecção
  dat_aux = sy-datum.
*inicializa range
  datas-sign   = 'I'.
  datas-option = 'BT'.
  datas-low    = dat_aux - 60.
  datas-high   = dat_aux + 365.
  APPEND datas.


  MOVE 'MATRICULA' TO cursorfield.
  IF NOT doc_carga IS INITIAL.

*------------------------------Validar se o transporte já foi registado
    SELECT * FROM  zwm006_aux
             WHERE  n_transporte  = doc_carga
             AND    finalizada    = space.

    ENDSELECT.

    IF sy-dbcnt GE 1.

      PERFORM erro USING 'O Transporte já foi registado'(060).
      PERFORM inicializa.

    ELSE.

*-------------------------------Este transporte ainda não foi registado
      g_novo_trans = 'X'.

    ENDIF.

*----------------------------------------------------------------------
    CLEAR vttk.

    CHECK g_novo_trans = 'X'.

*    SELECT * FROM  vttk
*             WHERE dpreg IN datas
*             AND   dalen = '00000000'
*             AND   tknum = doc_carga
*             AND   stdis = 'X'
*             AND   daten = '00000000'.
*
*    ENDSELECT.
    SELECT * FROM  vttk
             WHERE dpreg IN datas
             AND   stten = ' '
             AND   tknum = doc_carga
             AND   stdis = 'X'.

    ENDSELECT.

*----------------------------------------------------------------****
    IF sy-dbcnt GT 1.

      PERFORM erro USING 'Existem varios transportes, só 1 permitido'(061).
      PERFORM inicializa.

    ELSEIF sy-dbcnt = 1.

** Confirma se transporte já esteve registado
      SELECT * FROM  zwm006_aux
               WHERE  n_transporte  = doc_carga
               AND    finalizada    = 'X'.
      ENDSELECT.
      IF sy-subrc EQ 0.

        CLEAR: pergunta, resposta.
        CONCATENATE 'O transporte'(062) doc_carga 'já foi registado.'(063)
                    'Deseja continuar o registo?'(064)
               INTO pergunta SEPARATED BY space.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Portaria'(065)
*           DIAGNOSE_OBJECT       = ' '
            text_question         = pergunta
            text_button_1         = 'Sim'(001)
*           ICON_BUTTON_1         = ' '
            text_button_2         = 'Não'(002)
*           ICON_BUTTON_2         = ' '
            default_button        = '2'
            display_cancel_button = ' '
*           USERDEFINED_F1_HELP   = ' '
*           START_COLUMN          = 25
*           START_ROW             = 6
*           POPUP_TYPE            =
          IMPORTING
            answer                = resposta
*        TABLES
*           PARAMETER             =
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF resposta NE '1'.
          PERFORM erro USING ''.
          EXIT.
        ENDIF.

      ENDIF.



*----------------------------------Validar Grupo
      PERFORM valida_grupo USING vttk-tknum.

      IF matricula NE space.
        update_dynp = 'X'.
      ENDIF.

      doc_carga     = vttk-tknum.
      matricula     = vttk-signi.

      SPLIT vttk-signi AT '|' INTO matricula_scr galera.

      observacao    = vttk-tpbez.
      tipo_camiao   = vttk-sdabw.
      transportador = vttk-tdlnr.


*-------------------------------------------------Nome do transportador
      SELECT SINGLE * FROM lfa1 WHERE lifnr = vttk-tdlnr.
      MOVE lfa1-name1 TO nome_transp.

      CLEAR : new_matricula, new_observacao, new_tipo_camiao,
              new_transportador.

      new_matricula     = vttk-signi.
      new_observacao    = vttk-tpbez.
      new_tipo_camiao   = vttk-sdabw.
      new_transportador = vttk-tdlnr.
      new_nome_transp   = nome_transp.

      MOVE 'MATRICULA' TO cursorfield.

    ELSEIF sy-subrc NE 0.

      PERFORM erro USING 'Não existe Transporte, ou foi Finalizado'(066).
      PERFORM inicializa.

    ENDIF.

  ENDIF.

ENDMODULE.                 " CHECK_DOC_CARGA  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit4 INPUT.

  CLEAR: matricula,doc_carga,tipo_camiao,observacao,
         transportador,ok_code_0004,cursorfield,nome_transp, lfa1,
         vttk, g_del_all.

*  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*    EXPORTING
*      mode_keyword   = 'X'
*      keyword_       = 'G_PORTARIA'
*    EXCEPTIONS
*      foreign_lock   = 1
*      system_failure = 2
*      OTHERS         = 3.

  CONCATENATE 'G_CARGA_' xuser-lgnum INTO lv_gtarg.
  CONDENSE lv_gtarg NO-GAPS.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = lv_gtarg
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT4  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_tipo_camiao  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_tipo_camiao INPUT.
  IF NOT matricula IS INITIAL.
    MOVE 'TRANSPORTADOR' TO cursorfield.
  ENDIF.

  IF NOT tipo_camiao IS INITIAL.
    SELECT SINGLE * FROM  tvsak
           WHERE  sdabw  = tipo_camiao.

    IF sy-subrc NE 0.

      PERFORM erro USING 'Tipo de camião não existe!'(067).

    ENDIF.

  ENDIF.
ENDMODULE.                 " CHECK_tipo_camiao  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_transportador  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_transportador INPUT.

  CHECK NOT transportador IS INITIAL.
  CLEAR: lfa1, nome_transp.
  SELECT SINGLE * FROM lfa1
                  WHERE lifnr = transportador.

  IF sy-subrc IS INITIAL.
    MOVE lfa1-name1 TO nome_transp.
  ELSE.
    PERFORM erro USING 'Não existe o Transportador'(068).
  ENDIF.

  IF NOT matricula IS INITIAL AND nome_transp IS INITIAL.
    MOVE 'OBSERVACAO' TO cursorfield.
  ENDIF.

  CLEAR: vttk, lfa1.

ENDMODULE.                 " CHECK_transportador  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_MATRICULA1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matricula1 INPUT.

  DATA: matricula_aux TYPE zwm004-matricula, l_cont TYPE i.

*------------------------------------------------------------------
  CHECK vttk-signi NE matricula AND vttk-signi IS INITIAL.
*------------------------------------------------------------------

  CLEAR : dat_aux, flag, l_cont, datas, datas[].
  FREE dat_aux.

** Para a selecção
  dat_aux = sy-datum.
*inicializa range
  datas-sign   = 'I'.
  datas-option = 'BT'.
  datas-low    = dat_aux - 60.
  datas-high   = dat_aux + 365.
  APPEND datas.

*  CONCATENATE MATRICULA '%' INTO MATRICULA_AUX.
  CLEAR flag.

*  IF NOT matricula IS INITIAL.

*---------------------Só vai validar a matricula se esta foi modificada
  IF NOT matricula IS INITIAL.

    TRANSLATE matricula TO UPPER CASE.
    MOVE 'TIPO_CAMIAO' TO cursorfield.


*------------------------------Validar se o transporte já foi registado
    SELECT * FROM  zwm006_aux
             WHERE  matricula  = matricula
             AND    finalizada = space.

    ENDSELECT.

    IF sy-dbcnt GE 1.

      PERFORM erro USING 'O Transporte já foi registado'(069).
      PERFORM inicializa.
    ELSE.
      g_novo_trans = 'X'.
    ENDIF.
*---------------------------------------------------------------------

    CLEAR vttk.

*------------------------selecciona no periodo de 60 dias todos os
*------------------------transportes criados para esta matricula se
*----------------------existir mais que 1 com organização do transporte
*------------------------e data fim do transporte vazia
*    SELECT * FROM  vttk
*             WHERE dpreg IN datas
*             AND   dalen = '00000000'
*             AND   signi = matricula
*             AND   stdis = 'X'
*             AND   daten = '00000000'.
*
*    ENDSELECT.
    SELECT * FROM  vttk
             WHERE dpreg IN datas
             AND   stten = ' '
             AND   signi = matricula
             AND   stdis = 'X'.

    ENDSELECT.

*------------------------------Se encontra um ou mais registos erro
    IF sy-dbcnt GT 1.

      PERFORM erro USING 'Existem varios transportes, só 1 permitido'(070).
      PERFORM inicializa.
*------------------------------Se o transporte não foi registado e
*------------------------------não foi inserido o nº do transporte

*------------------------------deve respeitar a selecção feita com
*------------------------------base no nº de transporte
    ELSEIF sy-dbcnt EQ 1 AND NOT g_novo_trans IS INITIAL.

*----------------------------------Validar Grupo
      PERFORM valida_grupo USING vttk-tknum.


*-------------------------------------------------Nome do transportador
      SELECT SINGLE * FROM lfa1 WHERE lifnr = vttk-tdlnr.

      IF NOT doc_carga IS INITIAL AND vttk-tknum NE doc_carga.
        PERFORM erro USING 'Matricula associada a outro transporte'(071).
        PERFORM inicializa.
      ELSE.
        doc_carga     = vttk-tknum.
        matricula     = vttk-signi.
        observacao    = vttk-tpbez.
        tipo_camiao   = vttk-sdabw.
        transportador = vttk-tdlnr.
        nome_transp   = lfa1-name1.
      ENDIF.

      IF NOT doc_carga IS INITIAL AND vttk-tknum NE doc_carga.
        PERFORM erro USING 'Matricula associada a outro transporte'(072).
        PERFORM inicializa.
      ELSE.
        new_doc_carga     = vttk-tknum.
        new_matricula     = vttk-signi.
        new_observacao    = vttk-tpbez.
        new_tipo_camiao   = vttk-sdabw.
        new_transportador = vttk-tdlnr.
        new_nome_transp   = nome_transp.
        nome_transp       = lfa1-name1.
      ENDIF.

    ELSEIF sy-subrc NE 0 .

      IF NOT doc_carga IS INITIAL.
        new_matricula     = vttk-signi.
      ENDIF.

*      PERFORM erro USING 'Não existe Transporte, ou foi Finalizado'.
*      PERFORM inicializa.

    ENDIF.

  ENDIF.

  CLEAR: vttk, lfa1.

ENDMODULE.                 " CHECK_MATRICULA1  INPUT

MODULE atualiza_dthrfim INPUT.

  gs_0004-dtfim = sy-datum.
  gs_0004-hrfim = sy-uzeit.

ENDMODULE.                 " ATUALIZA_DTHRINI  INPUT

MODULE atualiza_dthrini INPUT.

  gs_0004-dtini = sy-datum.
  gs_0004-hrini = sy-uzeit.

ENDMODULE.                 " ATUALIZA_DTHRINI  INPUT

MODULE atualiza_calc_peso INPUT.
  PERFORM peso_bascula_dif.
ENDMODULE.                 " ATUALIZA_CALC_PESO  INPUT

MODULE concat_matricula_galera INPUT.
  matricula = matricula_scr.
  IF galera IS NOT INITIAL.
    matricula = |{ matricula }\|{ galera }|.
  ENDIF.
ENDMODULE.                 " CONCAT_MATRICULA_GALERA  INPUT

MODULE check_galera INPUT.
ENDMODULE.                 " CHECK_GALERA  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT5  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit5 INPUT.

  CLEAR: ok_code_0005,cursorfield, g_port_wa2.

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

  CLEAR  : tab_portas1, fila_porta, flag_tree, flag_treec, flag_tree_d.
  REFRESH: tab_portas1, fila_porta.

  SET SCREEN '0000'.
  LEAVE SCREEN.
ENDMODULE.                 " EXIT5  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0005 INPUT.

  DATA: up_zwm002 TYPE zwm002     OCCURS 0 WITH HEADER LINE.
  DATA: up_zwm003 TYPE zwm003_aux OCCURS 0 WITH HEADER LINE.
  DATA: up_zwm005 TYPE zwm005     OCCURS 0 WITH HEADER LINE.
  DATA: up_zwm006 TYPE zwm006_aux OCCURS 0 WITH HEADER LINE.
  DATA: up_zwm042 TYPE zwm042     OCCURS 0 WITH HEADER LINE.
  DATA ret_code(1).
  DATA aux_index(1).

  CASE ok_code_0005.


    WHEN 'LINK' OR 'CHECK'.


    WHEN 'CONFIRMAR' OR 'SAVE'.
      PERFORM uc_confirmar_atribuir_porta.


    WHEN 'VPORTA'.

      IF NOT indice_fila IS INITIAL.
        READ TABLE fila_porta INDEX indice_fila.
        MOVE-CORRESPONDING fila_porta TO g_port_wa2.
        g_port_wa2-num_entrada = fila_porta-talao.

        LOOP AT zwm_return_truck
                WHERE num_entrada = g_port_wa2-num_entrada.
          MOVE-CORRESPONDING zwm_return_truck TO tab_portas1.
          APPEND tab_portas1.
        ENDLOOP.

      ELSE.

        MESSAGE w000 WITH 'Seleccinar um Transporte'(074).

      ENDIF.

  ENDCASE.
  CLEAR: flag_tree, flag_treec, flag_tree_d.

ENDMODULE.                 " USER_COMMAND_0005  INPUT

**&---------------------------------------------------------------------
**
**&      Module  USER_COMMAND_0006  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE user_command_0006 INPUT.
*  DATA: tipo(1).
*  CASE ok_code_0006.
*    WHEN 'CONFIRMAR'.
*
*      IF matricula_d IS INITIAL.
*        MESSAGE i017.
*        CLEAR matricula_d.
*      ELSE.
*        IF NOT matricula_d EQ matricula_o.
**ve se o carro inserido é um carro que esteja na lista de espera
**com status
*          CLEAR zwm003_aux.
*          SELECT SINGLE * FROM zwm003_aux
*                          WHERE armazem = xuser-lgnum
*                          AND   matricula = matricula_d.
*
*          IF sy-subrc = 0.
*            IF zwm003_aux-operacao = 'DESCARGA'.
*              IF tipo_porta = 'CARGA'.
*                MESSAGE i021.
*                CLEAR matricula_d.
*              ELSE.
*                PERFORM actualiza_estado_tabelas.
*                MESSAGE i019 WITH matricula_d porta_d.
*                CLEAR: ok_code_0006,cursorfield.
*                CLEAR : matricula_d,matricula_o,tipo_porta,porta_d.
*                LEAVE TO TRANSACTION 'ZWM004'.
*              ENDIF.
*            ELSE.
*              IF tipo_porta = 'DESCARGA'.
*                MESSAGE i020.
*                CLEAR matricula_d.
*              ELSE.
*                PERFORM actualiza_estado_tabelas.
*                MESSAGE i019 WITH matricula_d porta_d.
*                CLEAR: ok_code_0006,cursorfield.
*                CLEAR : matricula_d,matricula_o,tipo_porta,porta_d.
*                LEAVE TO TRANSACTION 'ZWM004'.
*              ENDIF.
*            ENDIF.
*          ELSE.
**erro
*            MESSAGE i018.
*            CLEAR matricula_d.
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.
*
*  ENDCASE.
*
*ENDMODULE.                 " USER_COMMAND_0006  INPUT
*
**&---------------------------------------------------------------------
**
**&      Module  exit6  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE exit6 INPUT.
*
*  CLEAR: ok_code_0006,cursorfield.
*  CLEAR : matricula_d,matricula_o,tipo_porta,porta_d.
*  SET SCREEN '0000'.
*  LEAVE SCREEN.
*
*ENDMODULE.                 " exit6  INPUT

**&---------------------------------------------------------------------
**
**&      Module  CHECK_MATRICULA_click  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_matricula_click INPUT.
*
*  DATA: operaca TYPE zwm003-operacao.
*
*  IF NOT  matricula_d IS INITIAL.
*    CLEAR operaca.
*    IF tipo_porta = 'Carga'.
*      operaca = 'CARGA'.
*    ELSEIF tipo_porta = 'Descarga'.
*      operaca = 'DESCARGA'.
*    ELSE.
*      CLEAR operaca.
*    ENDIF.
*
*    IF NOT operaca IS INITIAL.
*      SELECT SINGLE * FROM zwm003_aux
*      WHERE armazem   = xuser-lgnum
*      AND   estado    = 'E'
*      AND   operacao  = operaca
*      AND   matricula = matricula_d.
*
*      IF sy-subrc <> 0.
*        MESSAGE i024 WITH matricula_d.
*        CLEAR matricula_d.
*      ENDIF.
*    ELSE.
*      SELECT SINGLE * FROM zwm003_aux
*      WHERE armazem   = xuser-lgnum
*      AND   estado    = 'E'
*      AND   matricula = matricula_d.
*
*      IF sy-subrc <> 0.
*        MESSAGE i024 WITH matricula_d.
*        CLEAR matricula_d.
*      ENDIF.
*    ENDIF.
*
*  ENDIF.
*
*ENDMODULE.                 " CHECK_MATRICULA_click  INPUT

**&---------------------------------------------------------------------
**
**&      Module  EXIT7  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE exit7 INPUT.
*
*  CLEAR: ok_code_0007,cursorfield.
*  CLEAR : numero_transporte.
*  SET SCREEN '0000'.
*  LEAVE SCREEN.
*
*ENDMODULE.                 " EXIT7  INPUT
**&---------------------------------------------------------------------
**
**&      Module  USER_COMMAND_0007  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE user_command_0007 INPUT.
*
*  CASE ok_code_0007.
*    WHEN 'CONFIRMAR'.
*      CLEAR: ok_code_0007.
*      MOVE vttk-tknum TO numero_transporte.
*      SET PARAMETER ID 'TNR' FIELD numero_transporte.
*      CALL TRANSACTION 'VT02N' AND SKIP FIRST SCREEN.
*      CLEAR numero_transporte.
*      SET SCREEN '0000'.
*      LEAVE SCREEN.
*  ENDCASE.
*
*ENDMODULE.                 " USER_COMMAND_0007  INPUT

**&---------------------------------------------------------------------
**
**&      Module  EXIT8  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE exit8 INPUT.
*  CLEAR: doc_compra,matricula,tipo_camiao,transportador,
*          observacao,ok_code_0003,cursorfield,nome_transp.
*
*  SET SCREEN '0000'.
*  LEAVE SCREEN.
*
*ENDMODULE.                 " EXIT8  INPUT

**&---------------------------------------------------------------------
**
**&      Module  CHECK_TALAO  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_talao INPUT.
*
*  CHECK NOT talao IS INITIAL.
*
*  SELECT SINGLE * FROM zwm005
*    WHERE num_entrada = talao AND finalizada = ' ' .
*  IF sy-subrc <> 0.
*    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '056' WITH talao.
*    CLEAR talao.
*    MOVE 'TALAO' TO cursorfield.
*    EXIT.
*  ELSE.
*    SELECT SINGLE *
*      FROM zwm002
*        WHERE armazem = zwm005-armazem AND
*              porta = zwm005-porta.
*    IF zwm002-estado <> 'O'.
*      MESSAGE ID 'ZWMMSG001' TYPE 'I'
*               NUMBER '084' WITH talao.
*      CLEAR talao.
*      MOVE 'TALAO' TO cursorfield.
*      EXIT.
*
*    ELSE.
*      SELECT SINGLE * FROM zwm017 WHERE flag_mm_wm = ' ' .
*      IF sy-subrc = 0.
*        IF zwm017-num_entrada <> talao AND
*           NOT zwm017-num_entrada IS INITIAL.
*          MESSAGE ID 'ZWMMSG001' TYPE 'I'
*                 NUMBER '057' WITH zwm017-num_entrada.
*          CLEAR talao.
*          MOVE 'TALAO' TO cursorfield.
*          EXIT.
*        ENDIF.
*      ELSE.
*        SELECT SINGLE * FROM zwm017
*            WHERE num_entrada = talao AND flag_mm_wm = 'T' .
*        IF sy-subrc = 0.
*          MESSAGE ID 'ZWMMSG001' TYPE 'I'
*                   NUMBER '129' WITH talao.
*          CLEAR talao.
*          MOVE 'TALAO' TO cursorfield.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  MOVE 'CLIENTE' TO cursorfield.
*
*ENDMODULE.                 " CHECK_TALAO  INPUT

**&---------------------------------------------------------------------
**
**&      Module  CHECK_EBELN  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_ebeln INPUT.
*
*  CHECK NOT pedido IS INITIAL.
*
*  SELECT SINGLE *
*      FROM ekko
*          WHERE ebeln = pedido AND
*                loekz <> 'X'.   " nao eliminado
*  IF sy-subrc <> 0.
*    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '049' WITH pedido.
*    CLEAR: pedido, lote, material, item, descricao, uni, quantidade.
*    EXIT.
*  ENDIF.
*
*  SELECT SINGLE * FROM zwm005
*      WHERE num_entrada = talao AND
*            ord_compra = pedido AND
*            finalizada = ' '.
*  IF sy-subrc <> 0.
*    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '052' WITH pedido.
*    CLEAR: pedido, lote, material, item, descricao, uni, quantidade.
*    EXIT.
*  ELSE.
*    lote = pedido.
*  ENDIF.
*
*ENDMODULE.                 " CHECK_EBELN  INPUT

**&---------------------------------------------------------------------
**
**&      Module  USER_COMMAND_0008  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE user_command_0008 INPUT.
*
*  DATA: items LIKE zwm018 OCCURS 0 WITH HEADER LINE,
*        wa_zwm017 LIKE zwm017 OCCURS 0 WITH HEADER LINE,
*        i_zwm017 LIKE zwm017 OCCURS 0 WITH HEADER LINE,
*        i_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.
*
*  DATA: mblnr LIKE mkpf-mblnr,
*        gjahr LIKE mkpf-mjahr,
*        code LIKE bapi2017_gm_code,
*        mov_mm TYPE bwlvs,
*        mov_wm TYPE bwlvs,
*        porta_desc TYPE ablad,
*        to TYPE tanum,
*        st_type_o TYPE lgtyp,
*        st_type_d TYPE lgtyp,
*        bin_destino LIKE ltap-nlpla,
*        bin_origem LIKE ltap-vlpla,
*        range LIKE inri-nrrangenr,
*        plant TYPE werks_d,
*        s_loc TYPE lgort_d,
*        valor(20),
*        text1(40).
*
*  REFRESH: items, wa_zwm017, i_zwm017, i_sscc.
*
*  PERFORM user_own_data.
*
*  CASE ok_code_0008.
*    WHEN 'SAVE'.
*      CHECK NOT talao IS INITIAL AND
*            NOT pedido IS INITIAL AND
*            NOT material IS INITIAL AND
*            NOT quantidade IS INITIAL AND
*            NOT lote IS INITIAL.
*
*      items-ebeln = pedido.
*      items-ebelp = item.
*      items-material = material.
*      items-quantidade = quantidade.
*      items-uni = uni.
*
*      CALL FUNCTION 'ZWM_BATCH_CREATE'
*        EXPORTING
*          armazem           = xuser-lgnum
*          material          = material
*          lote              = lote
*        TABLES
*          return_msg        = return_msg
*        EXCEPTIONS
*          erro              = 1
*          batch_not_created = 2
*          OTHERS            = 3.
*      IF sy-subrc <> 0.
*
*        IF sy-subrc = 1.
*          MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '083' WITH lote.
*          CLEAR lote.
*          EXIT.
*        ELSEIF sy-subrc = 2.
*          READ TABLE return_msg INDEX 1.
*          MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER return_msg-msgnr
*           WITH return_msg-msgv1.
*          EXIT.
*
*
*        ENDIF.
*      ELSE.
*        items-lote = lote.
*
*      ENDIF.
*
*
*      APPEND items.
*
*      CLEAR valor.
*      PERFORM get_parameter USING xuser-lgnum
*                              'ENTRADA_TERCEIROS'
*                              'CODE'
*                              valor.
*      MOVE valor TO code.
*      CLEAR valor.
*      PERFORM get_parameter USING xuser-lgnum
*                              'ENTRADA_TERCEIROS'
*                              'MOV'
*                              valor.
*
*      MOVE valor TO mov_mm.
*      CLEAR valor.
*
**      SELECT SINGLE PORTA INTO PORTA_DESC
**        FROM ZWM005 WHERE ARMAZEM = XUSER-LGNUM AND
**                          NUM_ENTRADA = TALAO AND
**                          ORD_COMPRA = PEDIDO AND
**                          FINALIZADA = ' '.
**      CONCATENATE '000-000-' PORTA_DESC+1(2) INTO PORTA_DESC.
*
*      CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
*        EXPORTING
*          lgnum            = xuser-lgnum
**          PORTA            = PORTA_DESC
*          code             = code
*          mov_mm           = mov_mm
*          testrun          = 'X'
*        IMPORTING
*          materialdocument = mblnr
*          matdocumentyear  = gjahr
*        TABLES
*          return_msg       = return_msg
*          items            = items
*        EXCEPTIONS
*          error            = 1
*          OTHERS           = 2.
*      IF sy-subrc <> 0.
*        LOOP AT return_msg WHERE msgtyp = 'E'.
*          MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
*                  NUMBER return_msg-msgnr
*                  WITH return_msg-msgv1 return_msg-msgv2.
*        ENDLOOP.
*      ELSE.
**      inserir na tabela zwm017
*        wa_zwm017-armazem = xuser-lgnum.
*        wa_zwm017-num_entrada = talao.
*        wa_zwm017-ebeln = pedido.
*        wa_zwm017-ebelp = item.
*        wa_zwm017-material = material.
*        wa_zwm017-quantidade = quantidade.
*        wa_zwm017-uni = uni.
*        wa_zwm017-lote = lote.
*        wa_zwm017-flag_mm_wm = 'M'.
*        APPEND wa_zwm017.
*        INSERT INTO zwm017 VALUES wa_zwm017.
*        IF sy-subrc <> 0.
*         MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '055' WITH pedido talao
*.
*          CLEAR: pedido, talao.
*          EXIT.
*        ELSE.
*          COMMIT WORK.
*        ENDIF.
*      ENDIF.
*      REFRESH return_msg.
*      CLEAR: pedido,item, material, descricao, quantidade, uni,
*             ok_code_0008, lote.
*      SET SCREEN '0008'.
*      LEAVE SCREEN.
*
*    WHEN 'CREATE_OT'.
*      CLEAR: code, mov_mm, mov_wm.
*      IF talao IS INITIAL.
*        MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '059' WITH talao.
*      ELSE.
*        SELECT * APPENDING CORRESPONDING FIELDS OF TABLE i_zwm017
*          FROM zwm017 WHERE num_entrada = talao.
*
*        IF i_zwm017[] IS INITIAL.
*          MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '053'.
*        ELSE.
*          CALL SCREEN '0012' STARTING AT  30 10 ENDING AT 105 20.
**          DELETE I_ZWM017 WHERE FLAG_MM_WM <> ' '.
**          SORT I_ZWM017 BY EBELN.
**          LOOP AT I_ZWM017 .
**            AT NEW EBELN.
**              SELECT * FROM ZWM017
**                WHERE EBELN = I_ZWM017-EBELN AND FLAG_MM_WM = ' '.
**                MOVE ZWM017-EBELN TO ITEMS-EBELN.
**                MOVE ZWM017-EBELP TO ITEMS-EBELP.
**                MOVE ZWM017-MATERIAL TO ITEMS-MATERIAL.
**                MOVE ZWM017-QUANTIDADE TO ITEMS-QUANTIDADE.
**                MOVE ZWM017-UNI TO ITEMS-UNI.
**                MOVE ZWM017-LOTE TO ITEMS-LOTE.
**                APPEND ITEMS.
**              ENDSELECT.
**
**              CLEAR VALOR.
**              PERFORM GET_PARAMETER USING XUSER-LGNUM
**                             'ENTRADA_TERCEIROS'
**                             'CODE'
**                             VALOR.
**              MOVE VALOR TO CODE.
**              CLEAR VALOR.
**              PERFORM GET_PARAMETER USING XUSER-LGNUM
**                                      'ENTRADA_TERCEIROS'
**                                      'MOV'
**                                      VALOR.
**              MOVE VALOR TO MOV_MM.
**              CLEAR VALOR.
**              CLEAR PORTA_DESC.
**              SELECT SINGLE PORTA INTO PORTA_DESC
**                      FROM ZWM005 WHERE ARMAZEM = XUSER-LGNUM AND
**                                        ORD_COMPRA = ZWM017-EBELN AND
**NUM_ENTRADA = ZWM017-NUM_ENTRADA
**                                        AND FINALIZADA = ' '.
**
**              CONCATENATE '000-000-' PORTA_DESC+1(2) INTO PORTA_DESC.
**
**              CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
**                EXPORTING
**                  LGNUM            = XUSER-LGNUM
**                  PORTA            = PORTA_DESC
**                  CODE             = CODE
**                  MOV_MM           = MOV_MM
**                IMPORTING
**                  MATERIALDOCUMENT = MBLNR
**                  MATDOCUMENTYEAR  = GJAHR
**                TABLES
**                  RETURN_MSG       = RETURN_MSG
**                  ITEMS            = ITEMS
**                EXCEPTIONS
**                  ERROR            = 1
**                  OTHERS           = 2.
**              IF SY-SUBRC <> 0.
**                LOOP AT RETURN_MSG WHERE MSGTYP = 'E'.
**                  MESSAGE ID RETURN_MSG-MSGID TYPE RETURN_MSG-MSGTYP
**                          NUMBER RETURN_MSG-MSGNR
**                          WITH RETURN_MSG-MSGV1 RETURN_MSG-MSGV2.
**                ENDLOOP.
**              ELSE.
**                READ TABLE ITEMS INDEX 1.
**                UPDATE ZWM017 SET FLAG_MM_WM = 'M'
**                              WHERE NUM_ENTRADA = TALAO AND
**                                    EBELN = ITEMS-EBELN AND
**                                    FLAG_MM_WM = ' '.
**                COMMIT WORK.
**                REFRESH: ITEMS.
**              ENDIF.
**              REFRESH RETURN_MSG.
**            ENDAT.
**          ENDLOOP.
*
**        criar to para uma descarga
*          REFRESH i_zwm017.
**          SELECT *
**            FROM ZWM017
**              INTO CORRESPONDING FIELDS OF TABLE I_ZWM017
**              WHERE NUM_ENTRADA = TALAO AND FLAG_MM_WM = 'M'.
**
**          IF SY-SUBRC = 0.
**            MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '054'.
**          ELSE.
**            REFRESH I_ZWM017.
**            SELECT *
**              FROM ZWM017
**                INTO CORRESPONDING FIELDS OF TABLE I_ZWM017
**                WHERE NUM_ENTRADA = TALAO AND FLAG_MM_WM = 'M'.
*
**            LOOP AT I_ZWM017.
**              MOVE I_ZWM017-MATERIAL TO I_SSCC-MATERIAL.
**              MOVE I_ZWM017-QUANTIDADE TO I_SSCC-QUANTIDADE.
**              MOVE I_ZWM017-UNI TO I_SSCC-UNI.
**              MOVE I_ZWM017-LOTE TO I_SSCC-LOTE_PRODUCAO.
**I_SSCC-TIPO_SU = 'P6'."por defeito pq nao interessa o tipo
**              CLEAR VALOR.
**              PERFORM GET_PARAMETER USING XUSER-LGNUM
**                                      'RANGE_EXT'
**                                      'NUMBER'
**                                      VALOR.
**              MOVE VALOR TO RANGE.
**              CLEAR VALOR.
**              CALL FUNCTION 'NUMBER_GET_NEXT'
**                EXPORTING
**                  NR_RANGE_NR             = RANGE
**                  OBJECT                  = 'LVS_LENUM'
**                  QUANTITY                = '1'
**                IMPORTING
**                  NUMBER                  = I_SSCC-SSCC
**                EXCEPTIONS
**                  INTERVAL_NOT_FOUND      = 1
**                  NUMBER_RANGE_NOT_INTERN = 2
**                  OBJECT_NOT_FOUND        = 3
**                  QUANTITY_IS_0           = 4
**                  QUANTITY_IS_NOT_1       = 5
**                  INTERVAL_OVERFLOW       = 6
**                  BUFFER_OVERFLOW         = 7
**                  OTHERS                  = 8.
**              IF SY-SUBRC <> 0.
**                MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**              ENDIF.
**              APPEND I_SSCC.
**            ENDLOOP.
**            IF PORTA_DESC IS INITIAL.
**              READ TABLE I_ZWM017 INDEX 1.
**              SELECT SINGLE PORTA INTO PORTA_DESC
**                  FROM ZWM005
**                      WHERE ARMAZEM = XUSER-LGNUM AND
**                            ORD_COMPRA = I_ZWM017-EBELN AND
**                            NUM_ENTRADA = I_ZWM017-NUM_ENTRADA AND
**                            FINALIZADA = ' '.
**              DATA PORTA_AUX(3).
**              UNPACK PORTA_DESC TO PORTA_AUX.
**              CONCATENATE '000-000-' PORTA_DESC+1(2) INTO PORTA_DESC.
**            ELSE.
**              MOVE PORTA_DESC+8(2) TO PORTA_AUX.
**              CONCATENATE '000-000-' PORTA_DESC+1(2) INTO PORTA_DESC.
**            ENDIF.
**
**            SELECT SINGLE PULMAO_1 INTO BIN_DESTINO
**              FROM ZWM002 WHERE PORTA = PORTA_AUX.
**
**            WRITE PORTA_DESC TO BIN_ORIGEM.
*
**            PERFORM GET_PARAMETER USING XUSER-LGNUM
**                       'ENTRADA_ARMAZEM'
**                       'ST_902'
**                       VALOR.
**            MOVE VALOR TO ST_TYPE_O.
**            CLEAR VALOR.
**            PERFORM GET_PARAMETER USING XUSER-LGNUM
**                        'ENTRADA_ARMAZEM'
**                        'ST_PUL'
**                        VALOR.
**
**            MOVE VALOR TO ST_TYPE_D.
**            CLEAR VALOR.
*
*
**         Não esquecer rever isto
*
*          MOVE 'PORTA' TO i_sscc-material.
*          MOVE '1' TO i_sscc-quantidade.
*          MOVE 'UN' TO i_sscc-uni.
**          MOVE '1990026116' TO i_sscc-lote_producao.
*          i_sscc-tipo_su = 'P6'."por defeito pq nao interessa o tipo
*          APPEND i_sscc.
*
*          READ TABLE i_sscc INDEX 1.
*          CALL FUNCTION 'ZWM_GET_MATERIAL_PLANT_SLOC'
*            EXPORTING
*              warehouse    = xuser-lgnum
*              material     = i_sscc-material
*            IMPORTING
*              plant        = plant
*              s_loc        = s_loc
*            TABLES
*              return_msg   = return_msg
*            EXCEPTIONS
*              not_found    = 1
*              indetermined = 2
*              OTHERS       = 3.
*          IF sy-subrc <> 0.
*            LOOP AT return_msg WHERE msgtyp = 'E'.
*              MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
*                      NUMBER return_msg-msgnr
*                      WITH return_msg-msgv1 return_msg-msgv2.
*            ENDLOOP.
*          ENDIF.
*
*          REFRESH return_msg.
*
*          CLEAR valor.
*          PERFORM get_parameter USING xuser-lgnum
*                      'ENTRADA_TERCEIROS'
*                      'MOV_WM'
*                      mov_wm.
*
*          MOVE valor TO mov_mm.
*          CLEAR valor.
*
*          SELECT SINGLE porta INTO porta_desc
*                FROM zwm005
*                    WHERE armazem = xuser-lgnum AND
*                          num_entrada = talao AND
*                          finalizada = ' '.
**            MOVE PORTA_DESC+8(2) TO PORTA_AUX.
*          CONCATENATE '000-000-' porta_desc+1(2) INTO bin_origem.
*          CONCATENATE '000-000-' porta_desc+1(2) INTO bin_destino.
*
*          SELECT SINGLE lzone INTO certificado
*              FROM lagp WHERE lgnum = xuser-lgnum AND
*                              lgtyp = 'DCK' AND
*                              lgpla = bin_origem.
*
*          CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
*            EXPORTING
*              warehouse   = xuser-lgnum
*              mov_type    = mov_wm
**                ST_TYPE_O   = ST_TYPE_O
*              bin_origem  = bin_origem
**                ST_TYPE_D   = ST_TYPE_D
*              bin_destino = bin_destino
*              plant       = plant
*              s_loc       = s_loc
*              certificado = certificado
*            IMPORTING
*              to          = to
*            TABLES
*              return_msg  = return_msg
*              sscc        = i_sscc
*            EXCEPTIONS
*              error       = 1
*              OTHERS      = 2.
*          IF sy-subrc <> 0.
*            LOOP AT return_msg WHERE msgtyp = 'E'.
*              MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
*                      NUMBER return_msg-msgnr
*                      WITH return_msg-msgv1 return_msg-msgv2.
*            ENDLOOP.
*          ELSE.
*            UPDATE zwm017 SET flag_mm_wm = 'T'
*                WHERE num_entrada = talao.
*            COMMIT WORK.
*
*            SUBMIT zwmrep0009 WITH p_armaz = xuser-lgnum
*                              WITH p_talao = talao
*                              AND RETURN.
*            CLEAR talao.
*          ENDIF.
*        ENDIF.
**        ENDIF.
*      ENDIF.
*      CLEAR ok_code_0008.
*      SET SCREEN '0008'.
*      LEAVE SCREEN.
*  ENDCASE.
*
*ENDMODULE.                 " USER_COMMAND_0008  INPUT

**&---------------------------------------------------------------------
**
**&      Module  HELP_talao  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE help_talao INPUT.
*  DATA zzwm005 LIKE zwm005 OCCURS 0 WITH HEADER LINE.
*  DATA index2 TYPE sy-tabix.
*
**  CHECK NOT TALAO IS INITIAL.
*  REFRESH zzwm005.
*  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE zzwm005
*      FROM zwm005 WHERE finalizada = ' ' AND porta <> '   ' AND
*                        ord_compra <> '          '.
*
*  LOOP AT zzwm005.
*    SELECT SINGLE *
*      FROM zwm002
*        WHERE armazem = zzwm005-armazem AND porta = zzwm005-porta.
*    IF zwm002-estado <> 'O'.
*      DELETE zzwm005 INDEX sy-tabix.
*    ENDIF.
*  ENDLOOP.
*  CLEAR index2.
*
*  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
*    EXPORTING
**   CUCOL                               = 20
*   curow                               = 10
**   DISPLAY                             = ' '
*      selectfield                         = 'ARMAZEM'
*      tablename                           = 'ZWM005'
*      given_value                         = ' '
**   SHOW_ALL_VALUES_AT_FIRST_TIME       = ' '
*   titel                               = 'Dados de Descarga'
*   IMPORTING
*     ind                                 = index2
*    TABLES
*      full_table                          = zzwm005
*   EXCEPTIONS
*     no_tablefields_in_dictionary        = 1
*     no_tablestructure_given             = 2
*     more_then_one_selectfield           = 3
*     no_selectfield                      = 4
*     OTHERS                              = 5.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    READ TABLE zzwm005 INDEX index2.
*    IF sy-subrc = 0.
*      talao = zzwm005-num_entrada.
*    ENDIF.
*  ENDIF.
*ENDMODULE.                 " HELP_talao  INPUT

**&---------------------------------------------------------------------
**
**&      Module  CHECK_MATNR  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_matnr INPUT.
*
*  DATA zzwm019 LIKE zwm019  OCCURS 0 WITH HEADER LINE.
*  DATA index1 TYPE sy-tabix.
*
*  CHECK NOT material IS INITIAL.
*  REFRESH zzwm019.
*
*  SELECT *
*      FROM ekpo
*          WHERE ebeln = pedido AND
*                matnr = material AND
*                loekz <> 'X' AND " nao eliminado
*                elikz <> 'X'.     " codigo de remessa final
*    IF sy-subrc = 0.
*      MOVE ekpo-ebeln TO zzwm019-ebeln.
*      MOVE ekpo-ebelp TO zzwm019-ebelp.
*      MOVE ekpo-matnr TO zzwm019-matnr.
*      MOVE ekpo-meins TO zzwm019-uni.
*      APPEND zzwm019.
*    ENDIF.
*  ENDSELECT.
*  IF zzwm019[] IS INITIAL.
*    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '085' WITH material pedido.
*    CLEAR: material, item, descricao, uni.
*    EXIT.
*  ENDIF.
*
*  READ TABLE zzwm019 INDEX index1.
*  IF sy-subrc = 0.
*    SELECT SINGLE maktx INTO descricao
*        FROM makt WHERE matnr = material.
*    item = zzwm019-ebelp.
*    uni = zzwm019-uni.
*  ENDIF.
*
*ENDMODULE.                 " CHECK_MATNR  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT9  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit9 INPUT.

  IF doc_compra1 IS INITIAL.

    CLEAR : doc_compra1,
            doc_compra2,
            doc_compra3,
            doc_compra4,
            doc_compra5,
            doc_compra6,
            doc_compra7,
            doc_compra8,
            doc_compra9,
            doc_compra10.

  ENDIF.

  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT9  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0009  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0009 INPUT.

  CASE ok_code_0009.

    WHEN 'CONFIRMAR'.
** Actualização da tabela global ORDENS com todas as ordens de compra
** que estão associadas à chegada do camião de descarga
      PERFORM actualiza_ordens.

      MOVE 'MATRICULA' TO cursorfield.

      SET SCREEN '0000'.
      LEAVE SCREEN.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0009  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT10  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit10 INPUT.

  CLEAR: ok_code_0010, cursorfield, talao_saida,
         flag_tree, flag_treec, flag_tree_d.

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

  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT10  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0010 INPUT.

  CASE ok_code_0010.

    WHEN 'CONFIRMAR'.

** Actualização da saída do camião da portaria
*      PERFORM actualiza_saida_camiao.

      PERFORM efectua_saida_camiao.
      CLEAR: flag_tree, flag_treec, flag_tree_d.

      SET SCREEN '0000'.
      LEAVE SCREEN.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0010  INPUT

**&---------------------------------------------------------------------
**
**&      Module  EXIT11  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE exit11 INPUT.
*
*  CLEAR: doc_compra,matricula,tipo_camiao,transportador,
*          observacao,ok_code_0003,cursorfield,nome_transp,
*          doc_renova, cliente.
*
*  SET SCREEN '0000'.
*  LEAVE SCREEN.
*
*ENDMODULE.                 " EXIT11  INPUT

**&---------------------------------------------------------------------
**
**&      Module  CHECK_REMESSA  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_remessa INPUT.
*
*  CHECK NOT remessa IS INITIAL.
*
*  SELECT SINGLE * FROM likp
*  WHERE vbeln EQ remessa.
*  IF sy-subrc NE 0.
*    MESSAGE e061 WITH remessa.
*    CLEAR remessa.
*  ENDIF.
*
*  SELECT SINGLE * FROM zwm005
*      WHERE num_entrada = talao AND
*            ord_compra = remessa AND
*            finalizada = ' '.
*  IF sy-subrc <> 0.
*    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '062' WITH remessa.
*    CLEAR remessa.
*    EXIT.
*  ENDIF.
**
*ENDMODULE.                 " CHECK_REMESSA  INPUT

**&---------------------------------------------------------------------
**
**&      Module  CHECK_MATNR_REMESSA  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_matnr_remessa INPUT.
*
*  PERFORM check_material_remessa.
*
*ENDMODULE.                 " CHECK_MATNR_REMESSA  INPUT

**&---------------------------------------------------------------------
**
**&      Module  USER_COMMAND_0011  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE user_command_0011 INPUT.
*
*  CLEAR: mblnr, gjahr, code, mov_mm, mov_wm, porta_desc, to,
*         st_type_o, st_type_d, bin_destino, bin_origem, range,
*         plant, s_loc.
*
*  CLEAR: items, wa_zwm017, i_zwm017, i_sscc, zwm025.
*  REFRESH: items, wa_zwm017, i_zwm017, i_sscc.
*
*  PERFORM user_own_data.
*
*  CASE ok_code_0011.
*
*** Registo das entradas de devolução
*    WHEN 'SAVE'.
*      CHECK NOT cliente IS INITIAL AND
*            NOT material IS INITIAL AND
*            NOT quantidade IS INITIAL AND
*            NOT uni IS INITIAL AND
*            NOT lote IS INITIAL.
*
*** Actualização da tabela zwm025 que contém as entradas teóricas
*** de uma devolução
*      zwm025-armazem = xuser-lgnum.
*      zwm025-cliente = cliente.
*      zwm025-material = material.
*      zwm025-data = sy-datum.
*      GET TIME.
*      zwm025-hora = sy-uzeit.
*      zwm025-quantidade = quantidade.
*      zwm025-unidade = uni.
*      zwm025-lote = lote.
**      IF NOT factura_renova IS INITIAL.
**        zwm025-factura_renova = factura_renova.
**      ENDIF.
*      IF NOT doc_renova IS INITIAL.
*        PERFORM converte_formato_interno USING doc_renova
*                               CHANGING l_vbeln.
*
*        zwm025-factura_renova = l_vbeln.
*      ENDIF.
*      CLEAR zwm025-finalizada.
*
*      zwm025-cod_dev = cod_dev.
*      zwm025-cod_motivo = cod_mot.
*
*      MODIFY zwm025.
*      COMMIT WORK.
*
*      CLEAR : material, quantidade, uni, lote,
*              doc_renova, descricao, cliente, descricao_cliente,
*              cod_dev, l_text, cod_mot, motivo,
*              cursorfield, ok_code_0011.
*
*      MOVE 'CLIENTE' TO cursorfield.
*
**** Impressão das entradas de devolução
**    WHEN 'IMP_DEV'.
**
**      IF NOT cliente IS INITIAL.
**
**** Report para impressão
**        SUBMIT zwmrep0016 WITH p_armaz = xuser-lgnum
**                          WITH p_cli = cliente
**                          WITH s_data-low = sy-datum
**                          WITH p_vis = ' '
**                          WITH p_imp = 'X'
**                          AND RETURN.
**
**        CLEAR : material, quantidade, uni, lote,
**                doc_renova, descricao, cliente, descricao_cliente,
**                cod_dev, l_text, cod_mot, motivo,
**                cursorfield, ok_code_0011.
**
**      ELSEIF cliente IS INITIAL.
**        MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '091'.
**        MOVE 'CLIENTE' TO cursorfield.
**        EXIT.
**      ENDIF.
*  ENDCASE.
*
*ENDMODULE.                 " USER_COMMAND_0011  INPUT

**&---------------------------------------------------------------------
**
**&      Module  USER_COMMAND_0012  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE user_command_0012 INPUT.
*  DATA wazwm017 LIKE zwm017 OCCURS 0 WITH HEADER LINE.
*
*  CASE ok_code_0012.
*    WHEN 'OK'.
**     inserir na tabela zwm017
*      IF NOT material1 IS INITIAL.
*
*        PERFORM get_parameter USING xuser-lgnum
*                          'ENTRADA_TERCEIROS'
*                          material1
*                          valor.
*
*        wazwm017-armazem = xuser-lgnum.
*        MOVE valor TO wazwm017-ebeln.
*        MOVE valor+5(5) TO wazwm017-ebelp.
*        wazwm017-num_entrada = talao.
*        wazwm017-material = material1.
*        wazwm017-quantidade = qtd1.
*        wazwm017-uni = uni1.
*        CLEAR wazwm017-lote.
*        wazwm017-flag_mm_wm = 'T'.
*        APPEND wazwm017.
*        CLEAR wazwm017.
*      ENDIF.
*
*      IF NOT material2 IS INITIAL.
*
*        PERFORM get_parameter USING xuser-lgnum
*                          'ENTRADA_TERCEIROS'
*                          material2
*                          valor.
*
*        wazwm017-armazem = xuser-lgnum.
*        MOVE valor TO wazwm017-ebeln.
*        MOVE valor+5(5) TO wazwm017-ebelp.
*        wazwm017-num_entrada = talao.
*        wazwm017-material = material2.
*        wazwm017-quantidade = qtd2.
*        wazwm017-uni = uni2.
*        CLEAR wazwm017-lote.
*        wazwm017-flag_mm_wm = 'T'.
*        APPEND wazwm017.
*        CLEAR wazwm017.
*      ENDIF.
*
*      IF NOT material3 IS INITIAL.
*
*        PERFORM get_parameter USING xuser-lgnum
*                          'ENTRADA_TERCEIROS'
*                          material3
*                          valor.
*
*        wazwm017-armazem = xuser-lgnum.
*        MOVE valor TO wazwm017-ebeln.
*        MOVE valor+5(5) TO wazwm017-ebelp.
*        wazwm017-num_entrada = talao.
*        wazwm017-material = material3.
*        wazwm017-quantidade = qtd3.
*        wazwm017-uni = uni3.
*        CLEAR wazwm017-lote.
*        wazwm017-flag_mm_wm = 'T'.
*        APPEND wazwm017.
*        CLEAR wazwm017.
*      ENDIF.
*
*      MODIFY zwm017 FROM TABLE wazwm017.
*      COMMIT WORK.
*      SET SCREEN '0000'.
*      LEAVE SCREEN.
*  ENDCASE.
*ENDMODULE.                 " USER_COMMAND_0012  INPUT

**&---------------------------------------------------------------------
**
**&      Module  CHECK_MATNR1  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_matnr1 INPUT.
*
*  CHECK NOT material1 IS INITIAL.
*
*  SELECT SINGLE *
*    FROM mara
*        WHERE matnr = material1 AND mtart = 'PALT'.
*  IF sy-subrc <> 0.
*    MESSAGE i077 WITH material1.
*  ELSE.
*    SELECT SINGLE maktx INTO des1 FROM makt WHERE matnr = material1.
*    uni1 = mara-meins.
*  ENDIF.
*ENDMODULE.                 " CHECK_MATNR1  INPUT

**&---------------------------------------------------------------------
**
**&      Module  CHECK_MATNR2  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_matnr2 INPUT.
*
*  CHECK NOT material2 IS INITIAL.
*
*  SELECT SINGLE *
*    FROM mara
*        WHERE matnr = material2 AND mtart = 'PALT'.
*  IF sy-subrc <> 0.
*    MESSAGE i077 WITH material2.
*  ELSE.
*    SELECT SINGLE maktx INTO des2 FROM makt WHERE matnr = material2.
*    uni2 = mara-meins.
*  ENDIF.
*ENDMODULE.                 " CHECK_MATNR2  INPUT

**&---------------------------------------------------------------------
**
**&      Module  CHECK_MATNR1  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_matnr3 INPUT.
*
*  CHECK NOT material3 IS INITIAL.
*
*  SELECT SINGLE *
*    FROM mara
*        WHERE matnr = material3 AND mtart = 'PALT'.
*  IF sy-subrc <> 0.
*    MESSAGE i077 WITH material3.
*  ELSE.
*    SELECT SINGLE maktx INTO des3 FROM makt WHERE matnr = material3.
*    uni3 = mara-meins.
*  ENDIF.
*ENDMODULE.                 " CHECK_MATNR3  INPUT

**&---------------------------------------------------------------------
**
**&      Module  EXIt12  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE exit12 INPUT.
*  SET SCREEN '0000'.
*  LEAVE SCREEN.
*ENDMODULE.                 " EXIt12  INPUT
*
**&---------------------------------------------------------------------
**
**&      Module  HELP_TALAO_DEV  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE help_talao_dev INPUT.
*
**  DATA ZZWM005 LIKE ZWM005 OCCURS 0 WITH HEADER LINE.
**  DATA INDEX2 TYPE SY-TABIX.
*
*
**  CHECK NOT TALAO IS INITIAL.
*  REFRESH zzwm005.
*  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE zzwm005
*      FROM zwm005 WHERE finalizada = ' ' AND porta <> '   ' AND
*                        ord_compra = '          '.
*
*  CLEAR index2.
*
*  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
*    EXPORTING
**   CUCOL                               = 20
*   curow                               = 10
**   DISPLAY                             = ' '
*      selectfield                         = 'ARMAZEM'
*      tablename                           = 'ZWM005'
*      given_value                         = ' '
**   SHOW_ALL_VALUES_AT_FIRST_TIME       = ' '
*   titel                               = 'Dados de Descarga'
*   IMPORTING
*     ind                                 = index2
*    TABLES
*      full_table                          = zzwm005
*   EXCEPTIONS
*     no_tablefields_in_dictionary        = 1
*     no_tablestructure_given             = 2
*     more_then_one_selectfield           = 3
*     no_selectfield                      = 4
*     OTHERS                              = 5.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    READ TABLE zzwm005 INDEX index2.
*    IF sy-subrc = 0.
*      talao = zzwm005-num_entrada.
*    ENDIF.
*  ENDIF.
*
*
*ENDMODULE.                 " HELP_TALAO_DEV  INPUT

**&---------------------------------------------------------------------
**
**&      Module  check_cliente  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_cliente INPUT.
*
*  PERFORM check_cliente.
*
*ENDMODULE.                 " check_cliente  INPUT

**&---------------------------------------------------------------------
**
**&      Module  check_quantidade  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_quantidade INPUT.
*
*  CHECK NOT quantidade IS INITIAL.
*
*  MOVE 'COD_DEV' TO cursorfield.
*
*ENDMODULE.                 " check_quantidade  INPUT
*
**&---------------------------------------------------------------------
**
**&      Module  check_doc_renova  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_doc_renova INPUT.
*
*  PERFORM check_remessa.
*
*ENDMODULE.                 " check_factura_renova  INPUT
**&---------------------------------------------------------------------
**
**&      Module  check_lote_dev  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_lote_dev INPUT.
*
*  PERFORM check_lote_dev.
*
*ENDMODULE.                 " check_lote_dev  INPUT

*&---------------------------------------------------------------------*
**&      Module  EXIT13  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE exit13 INPUT.
**  CLEAR: doc_compra,matricula,tipo_camiao,transportador,
**          observacao,ok_code_0003,cursorfield,nome_transp.
*
*  SET SCREEN '0000'.
*  LEAVE SCREEN.
*
*ENDMODULE.                 " EXIT13  INPUT

**&---------------------------------------------------------------------
**
**&      Module  HELP_talao  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE help_talao_fab1 INPUT.
**  DATA zzwm005 LIKE zwm005 OCCURS 0 WITH HEADER LINE.
**  DATA index2 TYPE sy-tabix.
*
**  CHECK NOT TALAO IS INITIAL.
*  REFRESH zzwm005.
*  CLEAR index2.
*  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE zzwm005
*      FROM zwm005 WHERE finalizada = ' ' AND porta <> '   ' AND
*                        ord_compra = '          '.
*
*  LOOP AT zzwm005.
*    SELECT SINGLE *
*      FROM zwm002
*        WHERE armazem = zzwm005-armazem AND porta = zzwm005-porta.
*    IF zwm002-estado <> 'O'.
*      DELETE zzwm005 INDEX sy-tabix.
*    ENDIF.
*  ENDLOOP.
*  CLEAR index2.
*
*  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
*    EXPORTING
**   CUCOL                               = 20
*   curow                               = 10
**   DISPLAY                             = ' '
*      selectfield                         = 'ARMAZEM'
*      tablename                           = 'ZWM005'
*      given_value                         = ' '
**   SHOW_ALL_VALUES_AT_FIRST_TIME       = ' '
*   titel                               = 'Dados de Descarga Fabrica 1'
*   IMPORTING
*     ind                                 = index2
*    TABLES
*      full_table                          = zzwm005
*   EXCEPTIONS
*     no_tablefields_in_dictionary        = 1
*     no_tablestructure_given             = 2
*     more_then_one_selectfield           = 3
*     no_selectfield                      = 4
*     OTHERS                              = 5.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    READ TABLE zzwm005 INDEX index2.
*    IF sy-subrc = 0.
*      talao = zzwm005-num_entrada.
*    ENDIF.
*  ENDIF.
*ENDMODULE.                 " HELP_talao_fab1  INPUT

**&---------------------------------------------------------------------
**
**&      Module  CHECK_TALAO_FAB1  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_talao_fab1 INPUT.
*
*  CHECK NOT talao IS INITIAL.
*
*  SELECT SINGLE * FROM zwm005
*    WHERE num_entrada = talao AND finalizada = ' '
*           AND porta <> '   '
*           AND ord_compra = '          '.
*  IF sy-subrc <> 0.
*    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '056' WITH talao.
*    CLEAR: talao, deposito.
*    MOVE 'TALAO' TO cursorfield.
*    EXIT.
*  ELSE.
*    SELECT SINGLE *
*      FROM zwm002
*        WHERE armazem = zwm005-armazem AND
*              porta = zwm005-porta.
*    IF zwm002-estado <> 'O'.
*      MESSAGE ID 'ZWMMSG001' TYPE 'I'
*               NUMBER '084' WITH talao.
*      CLEAR: talao, deposito.
*      MOVE 'TALAO' TO cursorfield.
*      EXIT.
*
*    ENDIF.
*  ENDIF.
*  MOVE 'CLIENTE' TO cursorfield.
*
*ENDMODULE.                 " CHECK_TALAO_fab1  INPUT

**&---------------------------------------------------------------------
**
**&      Module  check_deposito  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_deposito INPUT.
*
*  DATA t_zwm032 LIKE zwm032.
*
*  CHECK NOT deposito IS INITIAL.
*
*  SELECT SINGLE *
*      FROM t001l
*          WHERE werks = 'RENV' AND
*                lgort = deposito.
*  IF sy-subrc <> 0.
*    MESSAGE ID 'ZWMMSG001' TYPE 'I'
*        NUMBER '157' WITH deposito 'RENV'.
*    CLEAR deposito.
*    MOVE 'DEPOSITO' TO cursorfield.
*    EXIT.
*  ENDIF.
*
*ENDMODULE.                 " check_deposito  INPUT

**&---------------------------------------------------------------------
**
**&      Module  USER_COMMAND_0013  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE user_command_0013 INPUT.
*
*
*  CHECK NOT talao IS INITIAL AND
*        NOT deposito IS INITIAL.
*
*  REFRESH: items, wa_zwm017, i_zwm017, i_sscc.
*
*  CLEAR:mov_mm,
*        mov_wm,
*        to,
*        st_type_o,
*        st_type_d,
*        bin_destino,
*        bin_origem,
*        plant,
*        s_loc,
*        valor,
*        text1.
*
*  CASE ok_code_0013.
*
*    WHEN 'CREATE_OT'.
*      CLEAR: code, mov_mm, mov_wm.
*      IF talao IS INITIAL.
*        MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '059' WITH talao.
*      ELSE.
*
**          Rever se é preciso fazer transferencia de paletes chep, etc
**          CALL SCREEN '0012' STARTING AT  30 10 ENDING AT 105 20.
*
**        criar to para uma descarga da fabrica 1
*
**         Não esquecer rever isto
*
*        MOVE 'PORTA' TO i_sscc-material.
*        MOVE '1' TO i_sscc-quantidade.
*        MOVE 'UN' TO i_sscc-uni.
**          MOVE '1990026116' TO i_sscc-lote_producao.
*        i_sscc-tipo_su = 'P6'."por defeito pq nao interessa o tipo
*        APPEND i_sscc.
*
*        READ TABLE i_sscc INDEX 1.
*        CALL FUNCTION 'ZWM_GET_MATERIAL_PLANT_SLOC'
*          EXPORTING
*            warehouse    = xuser-lgnum
*            material     = i_sscc-material
*          IMPORTING
*            plant        = plant
*            s_loc        = s_loc
*          TABLES
*            return_msg   = return_msg
*          EXCEPTIONS
*            not_found    = 1
*            indetermined = 2
*            OTHERS       = 3.
*        IF sy-subrc <> 0.
*          LOOP AT return_msg WHERE msgtyp = 'E'.
*            MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
*                    NUMBER return_msg-msgnr
*                    WITH return_msg-msgv1 return_msg-msgv2.
*          ENDLOOP.
*        ENDIF.
*
*        REFRESH return_msg.
*
*        CLEAR valor.
*        PERFORM get_parameter USING xuser-lgnum
*                    'ENTRADA_FABRICA1'
*                    'MOV_WM'
*                    mov_wm.
*
*        MOVE valor TO mov_mm.
*        CLEAR valor.
*
*        SELECT SINGLE porta INTO porta_desc
*              FROM zwm005
*                  WHERE armazem = xuser-lgnum AND
*                        num_entrada = talao AND
*                        finalizada = ' '.
**            MOVE PORTA_DESC+8(2) TO PORTA_AUX.
*        CONCATENATE '000-000-' porta_desc+1(2) INTO bin_origem.
*        CONCATENATE '000-000-' porta_desc+1(2) INTO bin_destino.
*
*        SELECT SINGLE *
*            FROM ltap
*                WHERE lgnum = xuser-lgnum AND
*                      vltyp = 'DCK' AND
*                      nlpla = bin_origem AND
*                      nltyp = 'DCK' AND
*                      vlpla = bin_destino AND
*                      pquit = ' '.
*
*        IF sy-subrc = 0.
*          MESSAGE ID 'ZWMMSG001' TYPE 'I'
*           NUMBER '092' WITH porta_desc.
*          CLEAR: talao, deposito.
*          MOVE 'TALAO' TO cursorfield.
*          EXIT.
*        ENDIF.
*
*        SELECT SINGLE lzone INTO certificado
*            FROM lagp WHERE lgnum = xuser-lgnum AND
*                            lgtyp = 'DCK' AND
*                            lgpla = bin_origem.
*
*        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
*          EXPORTING
*            warehouse   = xuser-lgnum
*            mov_type    = mov_wm
*            bin_origem  = bin_origem
*            bin_destino = bin_destino
*            plant       = plant
*            s_loc       = s_loc
*            certificado = certificado
*          IMPORTING
*            to          = to
*          TABLES
*            return_msg  = return_msg
*            sscc        = i_sscc
*          EXCEPTIONS
*            error       = 1
*            OTHERS      = 2.
*        IF sy-subrc <> 0.
*          LOOP AT return_msg WHERE msgtyp = 'E'.
*            MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
*                    NUMBER return_msg-msgnr
*                    WITH return_msg-msgv1 return_msg-msgv2.
*          ENDLOOP.
*        ELSE.
*          t_zwm032-armazem = xuser-lgnum.
*          t_zwm032-talao = talao.
*          t_zwm032-deposito = deposito.
*          t_zwm032-ot = to.
*          INSERT INTO zwm032 VALUES t_zwm032.
*          IF sy-subrc = 0.
*            COMMIT WORK.
*          ELSE.
*            ROLLBACK WORK.
*          ENDIF.
*        ENDIF.
*
*      ENDIF.
*      CLEAR: ok_code_0013, talao, deposito.
*      SET SCREEN '0013'.
*      LEAVE SCREEN.
*  ENDCASE.
*
*ENDMODULE.                 " USER_COMMAND_0013  INPUT

**&---------------------------------------------------------------------
**
**&      Module  check_cod_devol  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_cod_devol INPUT.
*
*  PERFORM check_cod_dev.
*
*ENDMODULE.                 " check_cod_devol  INPUT

**&---------------------------------------------------------------------
**
**&      Module  check_cod_mot  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_cod_mot INPUT.
*
*  PERFORM check_cod_mot.
*
*ENDMODULE.                 " check_cod_mot  INPUT
*
**&---------------------------------------------------------------------
**
**&      Module  f4_remessa  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE f4_remessa INPUT.
*
*  PERFORM get_f4_remessas.
*
*ENDMODULE.                 " f4_remessa  INPUT

**&---------------------------------------------------------------------
**
**&      Module  HELP_MATERIAL  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE help_material INPUT.
*
*  REFRESH zzwm019.
*
*  SELECT *
*      FROM ekpo
*          WHERE ebeln = pedido AND
**                matnr = material AND
*                loekz <> 'X' AND " nao eliminado
*                elikz <> 'X'.     " codigo de remessa final
*    IF sy-subrc = 0.
*      MOVE ekpo-ebeln TO zzwm019-ebeln.
*      MOVE ekpo-ebelp TO zzwm019-ebelp.
*      MOVE ekpo-matnr TO zzwm019-matnr.
*      MOVE ekpo-meins TO zzwm019-uni.
*      APPEND zzwm019.
*    ENDIF.
*  ENDSELECT.
**  IF zzwm019[] IS INITIAL.
**    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '085' WITH material pedido.
**    CLEAR material.
**    EXIT.
**  ENDIF.
*
*  CLEAR index1.
*
*  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
*    EXPORTING
*      curow                        = 10
*      selectfield                  = 'MATNR'
*      tablename                    = 'ZWM019'
*      given_value                  = ' '
*      titel                        = 'Dados do Pedido'
*    IMPORTING
*      ind                          = index1
*    TABLES
*      full_table                   = zzwm019
*    EXCEPTIONS
*      no_tablefields_in_dictionary = 1
*      no_tablestructure_given      = 2
*      more_then_one_selectfield    = 3
*      no_selectfield               = 4
*      OTHERS                       = 5.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    READ TABLE zzwm019 INDEX index1.
*    IF sy-subrc = 0.
*      SELECT SINGLE maktx INTO descricao
*          FROM makt WHERE matnr = zzwm019-matnr.
*      item = zzwm019-ebelp.
*      uni = zzwm019-uni.
*      material = zzwm019-matnr.
*    ENDIF.
*  ENDIF.
*
*  SELECT SINGLE * FROM zwm017
*    WHERE num_entrada = talao AND
*          ebeln = pedido AND
*          ebelp = item.
*  IF sy-subrc = 0.
*    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '058' WITH pedido item talao
*.
*    CLEAR: pedido, item, descricao, uni, material.
*    EXIT.
*  ENDIF.
*
*ENDMODULE.                 " HELP_MATERIAL  INPUT

**&---------------------------------------------------------------------
**
**&      Module  f4_material  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE f4_material INPUT.
*
*  PERFORM get_f4_material.
*
*ENDMODULE.                 " f4_material  INPUT
**&---------------------------------------------------------------------
**
**&      Module  check_dados_globais  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_dados_globais INPUT.
*
*  PERFORM check_material_lote_remessa.
*
*ENDMODULE.                 " check_dados_globais  INPUT

**&---------------------------------------------------------------------
**
**&      Module  USER_COMMAND_9004  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE user_command_9004 INPUT.
*
*  CASE ok_code_0004.
*
*    WHEN 'BACK'.
*
*      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*        EXPORTING
*          mode_keyword   = 'X'
*          keyword_       = 'G_PORTARIA'
*        EXCEPTIONS
*          foreign_lock   = 1
*          system_failure = 2
*          OTHERS         = 3.
*
*      CLEAR cursorfield.
*      CLEAR ok_code_0004.
*      SET SCREEN '0000'.
*      LEAVE SCREEN.
*
*    WHEN 'CONFIRMAR'.
*
*      IF NOT matricula IS INITIAL.
*        CLEAR: ok_code_0004,tipoc.
*        CLEAR cursorfield.
*        REFRESH return_msg.
*
*        IF NOT doc_carga IS INITIAL.
**actualiza o transporte com hora de chegada na portaria.
*          CALL FUNCTION 'ZWM_CHANGE_SHIPMENT'
*            EXPORTING
*              matricula               = matricula
*              tipo_camiao             = tipo_camiao
*              n_transporte            = doc_carga
*              transportador           = transportador
*              dareg                   = sy-datum
*              uareg                   = sy-uzeit
*            TABLES
*              return_msg              = return_msg
*            EXCEPTIONS
*              shipment_does_not_exist = 1
*              OTHERS                  = 2.
*
*          IF sy-subrc <> 0.
*
*            CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*              EXPORTING
*                mode_keyword   = 'X'
*                keyword_       = 'G_PORTARIA'
*              EXCEPTIONS
*                foreign_lock   = 1
*                system_failure = 2
*                OTHERS         = 3.
*
*            READ TABLE return_msg INDEX 1.
*            IF sy-subrc = 0.
*              MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
*         NUMBER return_msg-msgnr WITH return_msg-msgv1 return_msg-msgv2
*              return_msg-msgv3 return_msg-msgv4.
*              CLEAR: doc_carga, matricula, return_msg.
*              REFRESH return_msg.
*              MOVE 'DOC_CARGA' TO cursorfield.
*              EXIT.
*            ENDIF.
*          ENDIF.
*
*        ENDIF.
**regista tabela de cargas --------------------------------- Obter TALÂO
*        CALL FUNCTION 'ZWM_ENTRY_LOADING_V1'
*          EXPORTING
*            armazem         = xuser-lgnum
*            matricula       = matricula
*            n_transporte    = doc_carga
*            tipo_camiao     = tipo_camiao
*            observacoes     = observacao
*          IMPORTING
*            num_entrada     = num_entrada
*          TABLES
*            return_msg      = return_msg
*          EXCEPTIONS
*            no_warehouse    = 1
*            duplicate_entry = 2
*            OTHERS          = 3.
*
*
*        IF sy-subrc <> 0.
**erro
*
*          CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*            EXPORTING
*              mode_keyword   = 'X'
*              keyword_       = 'G_PORTARIA'
*            EXCEPTIONS
*              foreign_lock   = 1
*              system_failure = 2
*              OTHERS         = 3.
*
*          READ TABLE return_msg INDEX 1.
*          IF sy-subrc = 0.
*            MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
*         NUMBER return_msg-msgnr WITH return_msg-msgv1 return_msg-msgv2
*            return_msg-msgv3 return_msg-msgv4.
*            MOVE 'MATRICULA' TO cursorfield.
*          ENDIF.
*
*        ELSE.
*
*          REFRESH return_msg.
*          CLEAR porta.
**verifica se camião vai para lista de espera
*
**-------------------------Obter tipo de Carga - Pulmão ou Carga directa
*          PERFORM check_camiao_pulmao CHANGING tipoc.
**----------------------------------------------------------------------
*
*          CALL FUNCTION 'ZWM_ENTRY_PARKING_V1'
*            EXPORTING
*              armazem           = xuser-lgnum
*              tipo_camiao       = tipoc
*              matricula         = matricula
*              num_entrada       = num_entrada
*              observacoes       = observacao
*            IMPORTING
*              porta             = porta
*            TABLES
*              return_msg        = return_msg
*            EXCEPTIONS
*              no_warehouse      = 1
*              wrong_tipo_camiao = 2
*              OTHERS            = 3.
*
*          IF sy-subrc <> 0.
*            CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*              EXPORTING
*                mode_keyword   = 'X'
*                keyword_       = 'G_PORTARIA'
*              EXCEPTIONS
*                foreign_lock   = 1
*                system_failure = 2
*                OTHERS         = 3.
*
*            READ TABLE return_msg INDEX 1.
*            IF sy-subrc = 0.
*              MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
*              NUMBER return_msg-msgnr WITH return_msg-msgv1
*              return_msg-msgv2 return_msg-msgv3 return_msg-msgv4.
*            ENDIF.
*          ELSEIF NOT porta IS INITIAL.
**porta vem preenchida
**actualizar estado das portas
*            REFRESH: izwm002,return_msg,izwm005.
*            CLEAR: izwm002,return_msg,izwm005.
*
*            SELECT * FROM zwm002 INTO
*            CORRESPONDING FIELDS OF TABLE izwm002
*            WHERE porta = porta.
*
*            READ TABLE izwm002 INDEX 1.
*            IF sy-subrc = 0.
*              IF tipoc <> 'E'.
*                PERFORM actualiza_pulmao.
*              ENDIF.
*
*              izwm002-bloqueio  = 'X'.
*              izwm002-estado    = 'C'.
*              MODIFY izwm002 INDEX 1.
*            ENDIF.
*
*            CALL FUNCTION 'ZWM_MANAGE_DOORS'
*              EXPORTING
*                operacao                = '2'
*                armazem                 = xuser-lgnum
*              TABLES
*                l_zwm002                = izwm002
*                return_msg              = return_msg
*              EXCEPTIONS
*                no_warehouse            = 1
*                tab_zwm002_not_filled   = 2
*                tab_l_zwm002_filled     = 3
*                tab_l_zwm002_not_filled = 4
*                invalid_parameter       = 5
*                OTHERS                  = 6.
*
*            IF sy-subrc <> 0.
*              CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*                EXPORTING
*                  mode_keyword   = 'X'
*                  keyword_       = 'G_PORTARIA'
*                EXCEPTIONS
*                  foreign_lock   = 1
*                  system_failure = 2
*                  OTHERS         = 3.
*              READ TABLE return_msg INDEX 1.
*              IF sy-subrc = 0.
*                MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
*                NUMBER return_msg-msgnr WITH return_msg-msgv1
*                return_msg-msgv2 return_msg-msgv3 return_msg-msgv4.
*              ENDIF.
*            ELSE.
***actualizar estado da lista de cargas
*              REFRESH:return_msg, izwm006.
*              CLEAR izwm006.
*
*              SELECT * INTO CORRESPONDING FIELDS OF TABLE izwm006
*                  FROM zwm006
*                      WHERE num_entrada = num_entrada AND
*                            matricula = matricula.
*
*              READ TABLE izwm006 INDEX 1.
*              IF sy-subrc = 0.
*                izwm006-porta = porta.
*                MODIFY izwm006 INDEX 1.
*              ENDIF.
*
*              CALL FUNCTION 'ZWM_MANAGE_LOADING_V1'
*                EXPORTING
*                  operacao                = '2'
*                  armazem                 = xuser-lgnum
*                TABLES
*                  l_zwm006                = izwm006
*                  return_msg              = return_msg
*                EXCEPTIONS
*                  no_warehouse            = 1
*                  tab_l_zwm005_filled     = 2
*                  tab_zwm005_not_filled   = 3
*                  tab_l_zwm005_not_filled = 4
*                  invalid_parameter       = 5
*                  OTHERS                  = 6.
*
*              IF sy-subrc <> 0.
*                CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*                  EXPORTING
*                    mode_keyword   = 'X'
*                    keyword_       = 'G_PORTARIA'
*                  EXCEPTIONS
*                    foreign_lock   = 1
*                    system_failure = 2
*                    OTHERS         = 3.
*                READ TABLE return_msg INDEX 1.
*                IF sy-subrc = 0.
*                  MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
*                  NUMBER return_msg-msgnr WITH return_msg-msgv1
*                  return_msg-msgv2 return_msg-msgv3 return_msg-msgv4.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*** Data e Hora de Chegada á Porta
*            CALL FUNCTION 'ZWM_CHANGE_SHIPMENT'
*              EXPORTING
*                n_transporte            = doc_carga
*                dalbg                   = sy-datum
*                ualbg                   = sy-uzeit
*              TABLES
*                return_msg              = return_msg
*              EXCEPTIONS
*                shipment_does_not_exist = 1
*                OTHERS                  = 2.
*
*            IF sy-subrc <> 0.
*              CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*                EXPORTING
*                  mode_keyword   = 'X'
*                  keyword_       = 'G_PORTARIA'
*                EXCEPTIONS
*                  foreign_lock   = 1
*                  system_failure = 2
*                  OTHERS         = 3.
*              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*            ENDIF.
*** Impressão do talão ...
*            IF NOT num_entrada IS INITIAL.
*              CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*                EXPORTING
*                  mode_keyword   = 'X'
*                  keyword_       = 'G_PORTARIA'
*                EXCEPTIONS
*                  foreign_lock   = 1
*                  system_failure = 2
*                  OTHERS         = 3.
*              SUBMIT zwmrep0004 WITH p_talao = num_entrada
*                              AND RETURN.
*            ENDIF.
*
*            MESSAGE i000 WITH
*            'Impressão em curso  do numero de entrada '
*            num_entrada '. É favor dirigir-se à porta' porta.
*
*            CLEAR: matricula,doc_carga,tipo_camiao,observacao,
*            transportador,ok_code_0004,cursorfield,nome_transp.
*            SET SCREEN '0000'.
*            LEAVE SCREEN.
*          ELSE.
**porta não vem preenchida, camiao vai para fila de espera
*** Impressão do talão ...
*            IF NOT num_entrada IS INITIAL.
*              CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*                EXPORTING
*                  mode_keyword   = 'X'
*                  keyword_       = 'G_PORTARIA'
*                EXCEPTIONS
*                  foreign_lock   = 1
*                  system_failure = 2
*                  OTHERS         = 3.
*              SUBMIT zwmrep0004 WITH p_talao = num_entrada
*                              AND RETURN.
*            ENDIF.
*            MESSAGE i000 WITH 'Fila de espera documento em impressão'
*            num_entrada.
*            CLEAR: doc_compra,matricula.
*            SET SCREEN '0000'.
*            LEAVE SCREEN.
*          ENDIF.
*
*
*
*        ENDIF.
*      ENDIF.
*  ENDCASE.
*
*ENDMODULE.                 " USER_COMMAND_9004  INPUT
