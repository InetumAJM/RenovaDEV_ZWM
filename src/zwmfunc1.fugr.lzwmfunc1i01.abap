*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit1 INPUT.

  CLEAR: ok_code_0001,su,su1,descricao,bin_origem,matnr,
         texto_processo, bin_input,bin_output_o,
         bin_output_d, bin_output_kober_d, cursorfield,posicao_pulmao1,
         posicao_pulmao2.

  CLEAR tab_zwm010.
  REFRESH tab_zwm010.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = 'ZWM014'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
*apaga entrada do dicionario de dados para o user
*QUE PROCESSA TO PARA MENSULA OU DE MENSULA
  DELETE FROM zwm011
  WHERE armazem   = xuser-lgnum AND
        user_name = sy-uname    AND
        ( status <> 'P' AND
          status <> 'E' ). " WCS - Status Criada Elevador

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
*  SET SCREEN '0000'. LEAVE SCREEN.
  f3_activo = 'X'.
  SET SCREEN '0001'. LEAVE SCREEN.

ENDMODULE.                 " EXIT1  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_SU  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* ALTERAÇÃO: 20090112_BUG_SSCC_DIFERENTE
* POR      : ROFF-Paulo Saraiva
* DESCRIÇÃO: Teste SU(pistolada) <> SU1(system) e SU1(system) <> vazio
*            erro, como 1ª condição
***********************************************************************
MODULE check_su INPUT.
  DATA: lv_charg TYPE lqua-charg, " << INS ROFF(SDF):TMGP:21.01.2016 11:13:24
        lv_subrc TYPE sysubrc.

  CLEAR : destino_su1,
          destino_su,
          lgtyp,
          lgpla.

  CALL FUNCTION 'CONVERSION_EXIT_LENUM_INPUT'
    EXPORTING
      input           = su
    IMPORTING
      output          = su
    EXCEPTIONS
      check_failed    = 1
      not_numeric     = 2
      t344_get_failed = 3
      wrong_length    = 4
      OTHERS          = 5.

*& Begin of Modification by Tiago Pateiro - ROFF @ 21.01.2016 10:05:58
  IF gv_show_charg EQ abap_true.
    CLEAR lv_charg.

    SELECT charg UP TO 1 ROWS
      FROM lqua INTO lv_charg
      WHERE lgnum EQ xuser-lgnum
        AND lenum EQ su.  " Index LQUA~ZSU
    ENDSELECT.

    IF lv_charg NE gv_charg.
      PERFORM check_to_swap CHANGING lv_subrc. "Valida Swap de TO
      IF lv_subrc <> 0.

        IF lv_subrc EQ 99.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMFR001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '051'.
        ENDIF.

        CLEAR su.
        CLEAR ok_code_0001.
        MOVE 'SU' TO cursorfield.

        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 21.01.2016 10:05:58

  CALL FUNCTION 'ZWM_SPLIT_BIN'
    EXPORTING
      bin   = bin_output_o
    IMPORTING
      lgtyp = lgtyp
      lgpla = lgpla.

  IF NOT su IS INITIAL.

** Pilha de paletes vazias
    IF lgtyp = 'PAL'.

      IF su <> matnr.
        " Material & de Palete inválido!
        CLEAR text.
        WRITE su TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '367'
            message_var1   = text.

        CLEAR: su, ok_code_0001.
        MOVE 'SU' TO cursorfield.
        EXIT.
      ENDIF.

    ENDIF.

*   20090112_BUG_SSCC_DIFERENTE Teste posicionado logo à cabeça >>
    IF su <> su1 AND su1 IS NOT INITIAL.
      CLEAR text.
      WRITE su TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '033'
          message_var1   = text.
      CLEAR: su, ok_code_0001.
      MOVE 'SU' TO cursorfield.

*    COMENTADO
*    IF su <> su1 AND lgtyp = 'PRO'.
*   INSERIDO

** Entrada Produção
**********************************************************************
    ELSEIF su <> su1 AND lgtyp = 'PRO'.
*   20090112_BUG_SSCC_DIFERENTE <<


** Registo de incidências
** Verificar se a mesa está equivocada
** Mesa de destino da palete correcta
      SELECT SINGLE destino FROM zwm013 INTO destino_su1
                            WHERE sscc = su1.
** Só regista esta incidência para paletes saídas da produção
      IF sy-subrc = 0.
        IF NOT destino_su1 IS INITIAL.
          SELECT SINGLE destino FROM zwm013 INTO destino_su
                                WHERE sscc = su.
          IF NOT destino_su IS INITIAL.
            CHECK destino_su1 <> destino_su.
          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR text.
      WRITE destino_su1 TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '066'
          message_var1   = text.
      CLEAR su.
      MOVE 'SU' TO cursorfield.

*    ELSEIF lgtyp = 'PRO'.
*
*      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
*        EXPORTING
*          armazem            = xuser-lgnum
*          to_number          = to_ret
*          to_item            = '0001'
*          status             = 'P'
*        EXCEPTIONS
*          error_update_table = 1
*          OTHERS             = 2.

** Cross Docking
**********************************************************************
    ELSEIF lgtyp = 'CRD'.

** RL -> INS 26.05.2005
      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
        EXPORTING
          armazem            = xuser-lgnum
          to_number          = to_ret
          to_item            = '0001'
          status             = 'P'
        EXCEPTIONS
          error_update_table = 1
          OTHERS             = 2.
** RL <- INS 26.05.2005

      MOVE 'BIN_INPUT' TO cursorfield.

** Chamadas de Empilhador a Elevadores Arm. Automático (WCS)
**********************************************************************
    ELSEIF lgtyp = 'CHE' OR lgtyp = 'AUT'.

      PERFORM check_chd_elv USING lgpla CHANGING su.

      IF su IS INITIAL.
        CLEAR: su, ok_code_0001.
        MOVE 'SU' TO cursorfield.
        EXIT.
      ELSE.
*        CLEAR: to_ret, item_ret.
        MOVE 'BIN_INPUT' TO cursorfield.
        EXIT.
      ENDIF.

** Expedição nas Gravíticas Automático (WCS)
**********************************************************************
*    ELSEIF bin_output_d(3) = 'PUA'.

*      PERFORM check_exp_pal_aut USING lgtyp lgpla CHANGING su.
*
*      IF su IS INITIAL.
*        CLEAR: su, ok_code_0001.
*        MOVE 'SU' TO cursorfield.
*        EXIT.
*      ELSE.
**        CLEAR: to_ret, item_ret.
*        MOVE 'BIN_INPUT' TO cursorfield.
*        EXIT.
*      ENDIF.

** Saídas do Drive IN
**********************************************************************
    ELSEIF su <> su1 AND ( lgtyp = 'DRI' OR lgtyp = 'BLK' ).
      IF bin_output_d(3) <> 'REP' AND bin_output_d(3) <> 'CPK'.
        PERFORM valida_saida_dri USING lgtyp lgpla.
*        PERFORM cria_to2 USING lgtyp lgpla.
      ELSE.
        PERFORM valida_saida_dri_to_rep USING lgtyp lgpla.
      ENDIF.

**   20090112_BUG_SSCC_DIFERENTE POSICIONADO LOGO À CABEÇA >>
*    ELSEIF su <> su1.
*
*      CLEAR text.
*      WRITE su TO text LEFT-JUSTIFIED.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWMMSG001'
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = '033'
*          message_var1   = text.
*      CLEAR: su, ok_code_0001.
*      MOVE 'SU' TO cursorfield.
**   20090112_BUG_SSCC_DIFERENTE <<

    ELSE.
** Processamento especial para o caso de uma
** recepção de terceiros - qd confirma o SSCC
** cria uma nova TO para o que ainda está no
** PULMÃO
** excepto nos casos em que vai para o PRM

      IF bin_output_o(3) = 'PUL' AND bin_output_d(3) <> 'PRM'.
        PERFORM cria_nova_to_pulmao.
        MOVE 'BIN_INPUT' TO cursorfield.
      ENDIF.

** Mensula
**********************************************************************
** Processamento especial para o caso
** de uma saida de palete dos Trilaterais
** que passe pela mensula
*      break roffd.

      IF lgtyp = 'MEN' AND bin_output_d(3) <> 'REP'.

* Verificar se é uma PALETE REMONTADA ou é apenas INDIVIDUAL
        SELECT SINGLE * FROM zwm020
                        WHERE armazem = xuser-lgnum AND
                              ( p1 = su OR
                                p2 = su ).
        IF sy-subrc = 0.
          to3 = 'X'.
        ELSE.
          CLEAR to3.
        ENDIF.

        PERFORM confirma_to_parcialmente USING lgpla.

      ELSEIF lgtyp = 'MEN' AND bin_output_d(3) = 'REP'.

** Fazer lock antes de actualizar a tabela das mensulas
        PERFORM free_mensula.

        CALL FUNCTION 'ZWM_MODIFY_ZWM011'
          EXPORTING
            armazem            = xuser-lgnum
            to_number          = to_ret
            to_item            = '0001'
            status             = 'P'
          EXCEPTIONS
            error_update_table = 1
            OTHERS             = 2.

        MOVE 'BIN_INPUT' TO cursorfield.

        CLEAR : return_msg.
        REFRESH : return_msg.

** VER SE TO PASSA POR MENSULA
      ELSE.
        CLEAR zwm014.
        SELECT SINGLE * FROM zwm014
          WHERE armazem   = xuser-lgnum AND
                su        = su AND
                estado    = 'X'.

        IF sy-subrc <> 0.
*é uma TO que não passa por mensula
*logo é preciso fazer o PICK DA TO

          CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
            EXPORTING
              armazem              = xuser-lgnum
              confirm_type         = 'P'
            TABLES
              return_msg           = return_msg
            CHANGING
              to                   = to_ret
            EXCEPTIONS
              confirm_type_error   = 1
              to_not_found         = 2
              to_already_confirmed = 3
              to_already_picked    = 4
              to_not_picked        = 5
              wrong_su             = 6
              missing_su           = 7
              error                = 8
              OTHERS               = 9.

          IF sy-subrc <> 0.
            READ TABLE return_msg INDEX 1.
            IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
                EXPORTING
                  message_id     = return_msg-msgid
                  message_lang   = sy-langu
                  message_type   = return_msg-msgtyp
                  message_number = return_msg-msgnr
                  message_var1   = return_msg-msgv1.

              CLEAR: su, cursorfield, ok_code_0001.
              MOVE 'SU' TO cursorfield.
              EXIT.
*              SET SCREEN '0000'.LEAVE SCREEN.
            ENDIF.
          ELSE.
*PROCESSAMENTO PICK OK
*actualizar tabela ZWM011 com STATUS P pq é TO
*que nao passa por mensula
            CALL FUNCTION 'ZWM_MODIFY_ZWM011'
              EXPORTING
                armazem            = xuser-lgnum
                to_number          = to_ret
                to_item            = item_ret
                status             = 'P'
              EXCEPTIONS
                error_update_table = 1
                OTHERS             = 2.

            IF sy-subrc <> 0.

            ELSE.
*Processamento OK
              MOVE 'BIN_INPUT' TO cursorfield.
            ENDIF.
          ENDIF.
          MOVE 'BIN_INPUT' TO cursorfield.
        ELSE.
** Não passa por mensula pick so na 11
          CALL FUNCTION 'ZWM_MODIFY_ZWM011'
            EXPORTING
              armazem            = xuser-lgnum
              to_number          = to_ret
              to_item            = '0001'
              status             = 'P'
            EXCEPTIONS
              error_update_table = 1
              OTHERS             = 2.

        ENDIF.
        MOVE 'BIN_INPUT' TO cursorfield.
      ENDIF.
*      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

  ELSE.
*falta introdução da palete
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '037'
        message_var1   = text.
    CLEAR ok_code_0001.
    MOVE 'SU' TO cursorfield.
  ENDIF.

ENDMODULE.                 " CHECK_SU  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code_0001.
    WHEN 'NEXT'.
      PERFORM confirm.
    WHEN 'INCID' OR 'INCIDENCIA'.
      PERFORM incidencia.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_BIN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_bin INPUT.

  DATA: numpal TYPE i.
  DATA: chave_bloq LIKE keyword-keyword.

  DATA: lt_messages TYPE tab_bdcmsgcoll.

  DATA: ls_message TYPE bdcmsgcoll.

**********************************************************************

  CLEAR : lgtyp,lgpla, posicao_pulmao1, posicao_pulmao2.

  IF NOT bin_input IS INITIAL.

    CALL FUNCTION 'ZWM_SPLIT_BIN'
      EXPORTING
        bin   = bin_output_o
      IMPORTING
        lgtyp = lgtyp
        lgpla = lgpla.

** Descodifica Bin
    CALL FUNCTION 'ZWM_DECODE_BIN'
      EXPORTING
        iv_lgnum    = xuser-lgnum
        iv_bin_code = bin_input
      IMPORTING
        ev_bin      = bin_input
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      READ TABLE lt_messages INTO DATA(ls_messages) INDEX 1.
      IF sy-subrc = 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = ls_messages-msgid
            message_lang   = sy-langu
            message_type   = ls_messages-msgtyp
            message_number = ls_messages-msgnr
            message_var1   = ls_messages-msgv1
            message_var2   = ls_messages-msgv2
            message_var3   = ls_messages-msgv3
            message_var4   = ls_messages-msgv4.
        CLEAR bin_input.
        REFRESH lt_messages.
        MOVE 'BIN_INPUT' TO cursorfield.
        RETURN.
      ENDIF.
    ENDIF.

    IF bin_input <> bin_output_d.
*bin invalido
      CLEAR text.
      WRITE bin_input TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '034'
          message_var1   = text.

      CLEAR: bin_input, ok_code_0001.
      MOVE 'BIN_INPUT' TO cursorfield.
    ELSE.

      IF ( bin_output_o(3) = 'DRI' OR bin_output_o(3) = 'BLK' ) AND
         bin_output_d(3) = 'DCK'.

        SELECT SINGLE *
            FROM ltap
                WHERE lgnum = xuser-lgnum AND
                      tanum = to_ret AND
                      vlenr = su AND
                      vorga <> 'ST' AND
                      pquit = ' '.
        IF sy-subrc <> 0.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '213'
              message_var1   = text1.
          CLEAR: bin_input, ok_code_0001.
          MOVE 'BIN_INPUT' TO cursorfield.
          EXIT.
        ENDIF.
      ENDIF.

**  Verificar se no caso de paletes remontadas existe to
**  para a palete de cima
      IF ( bin_output_o(3) = 'DRI' OR bin_output_o(3) = 'BLK' OR bin_output_o(3) = 'MEN' ) AND
         ( bin_output_d(3) = 'PUL' OR bin_output_d(3) = 'DCK' ).

        CLEAR zwm020.
        SELECT SINGLE *
            FROM zwm020
                WHERE armazem = xuser-lgnum AND
                      p1 = su.
        IF sy-subrc = 0.
          CLEAR ltap.
          SELECT SINGLE *
              FROM ltap
                  WHERE lgnum = xuser-lgnum AND
                        vlenr = zwm020-p2 AND
                        vorga <> 'ST' AND
                        pquit = ' '.

          IF sy-subrc <> 0.
            WRITE zwm020-p2 TO text1.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWM001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '078'
                message_var1   = text1.
**            CLEAR: bin_input, ok_code_0001.
**            MOVE 'BIN_INPUT' TO cursorfield.
**            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.


      IF bin_output_d(3) = 'PUL'.

** FL -> 28/03/2007
** Bloqueia a "remessa" entre a introdução da posição de destino (PULL)
** e conclusão do processo de modo a não dar a 2 users diferentes a
** mesma posição de pulmão
        CONCATENATE 'ZWM028_' remessa INTO chave_bloq.
        CONDENSE chave_bloq NO-GAPS.
        DO.
          CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
            EXPORTING
              mode_keyword   = 'X'
              keyword_       = chave_bloq
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
** FL <- 28/03/2007

        SELECT SINGLE *
            FROM zwm028
                WHERE lgnum = xuser-lgnum AND
                      refnr = grupo AND
                      remessa = remessa.



** Desbloqueio Por Ordem de Venda e Tipo de Carro
***********************************************************************
        DO  1 TIMES.
          CALL FUNCTION 'Z_WM_IS_SPECIAL_PICK_TRANS_OV'
            EXPORTING
              i_lgnum = xuser-lgnum
              i_refnr = grupo
              i_tknum = zwm028-transporte
            EXCEPTIONS
              error   = 1
              OTHERS  = 2.

          CHECK sy-subrc EQ 0.


          SELECT SINGLE *
              FROM zwm028
                  WHERE lgnum = xuser-lgnum AND
                        refnr = grupo AND
                        remessa = ''.

          IF zwm028-posicao_ini_pul IS INITIAL.
            zwm028-posicao_ini_pul = 1.
          ENDIF.
        ENDDO.

*--------------------------------------------------------------------*
* Início de Alteração - ROFF SDF Carlos Fernandes - 09.03.2016
*--------------------------------------------------------------------*
*        posicao_pulmao1 =
*            zwm028-posicao_ini_pul + zwm028-paletes_pulmao.

        PERFORM f_is_new_calc
          USING xuser-lgnum
          CHANGING gv_new_calc.
        IF gv_new_calc EQ abap_true.
          PERFORM f_get_lung_position
            USING xuser-lgnum
                  grupo
            CHANGING posicao_pulmao1.
        ELSE.
          posicao_pulmao1 = zwm028-posicao_ini_pul + zwm028-paletes_pulmao.
        ENDIF.
*--------------------------------------------------------------------*
* Fim de Alteração - ROFF SDF Carlos Fernandes - 09.03.2016
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 31.05.2012 16:37:37
*  Motivo: Inverte Posição em Meio Pulmão
*--------------------------------------------------------------------*
        CALL FUNCTION 'ZWM_MPUL_POS_PUL_CHANGE'
          EXPORTING
            i_lgnum      = xuser-lgnum
            is_zwm028    = zwm028
          CHANGING
            c_pos_pulmao = posicao_pulmao1.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

        PERFORM f_posicao_pulmao_skip USING xuser-lgnum posicao_pulmao1
                                      CHANGING posicao_pulmao2.

        IF posicao_pulmao1 > 17.

          IF bin_output_d+12(2) <> '02'.
            CLEAR text.
            CONCATENATE bin_output_d(12) '02' INTO bin_output_d.
            WRITE bin_output_d TO text LEFT-JUSTIFIED.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '176'
                message_var1   = text.

            CLEAR: bin_input, posicao_pulmao1,
                   posicao_pulmao2, ok_code_0001.

            CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
              EXPORTING
                mode_keyword   = 'X'
                keyword_       = chave_bloq
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            MOVE 'BIN_INPUT' TO cursorfield.
          ELSE.
            posicao_pulmao1 = posicao_pulmao1 - 17.
            MOVE 'POSICAO_PULMAO2' TO cursorfield.
          ENDIF.
        ELSE.
          MOVE 'POSICAO_PULMAO2' TO cursorfield.
        ENDIF.
      ELSE.
        MOVE 'POSICAO_PULMAO2' TO cursorfield.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR text.
    WRITE bin_input TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '035'
        message_var1   = text.
    CLEAR bin_input.
    MOVE 'BIN_INPUT' TO cursorfield.
  ENDIF.

ENDMODULE.                 " CHECK_BIN  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit3 INPUT.

  DATA: wa_zwm014 TYPE zwm014.

  IF atribuida = 'TRUE'.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '045'
        message_var1   = text.

  ELSE.

    CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'ZWM014'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
*apaga entrada do dicionario de dados para o user
    DELETE FROM zwm011
    WHERE user_name = sy-uname AND
          status <> 'P'.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

    IF tipo_tri = 'S'.
*    apagar a entrada da tabela da mensula
      DO.
        CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'ZWM014'
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

      CLEAR: zwm014, wa_zwm014.

      SELECT SINGLE * INTO wa_zwm014
          FROM zwm014
              WHERE armazem = xuser-lgnum AND
                    su = su1.

      IF sy-subrc = 0.
        CLEAR: wa_zwm014-su,
               wa_zwm014-to_number,
               wa_zwm014-to_item,
               wa_zwm014-estado,
               wa_zwm014-bin,
               wa_zwm014-etiqueta,
               wa_zwm014-su_transito,
               wa_zwm014-prioridade.

        MODIFY zwm014 FROM wa_zwm014.
        IF sy-subrc = 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.

**      Repoe Prioridades das Mensulas do mesmo corredor
        CLEAR: corredor, n_mensulas_ocupadas.

        CONCATENATE wa_zwm014-mensula(3) '%' INTO corredor.
        SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
                        WHERE armazem = xuser-lgnum AND
                              mensula LIKE corredor AND
                              estado = 'X'.
        IF n_mensulas_ocupadas < 3.

          UPDATE zwm014 SET prioridade = ' '
                        WHERE armazem = xuser-lgnum AND
                              mensula LIKE corredor AND
                              estado = 'X'.
          COMMIT WORK.
        ENDIF.
        CLEAR: corredor, n_mensulas_ocupadas.

      ENDIF.

**    Desbloqueia Tabela
      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'ZWM014'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

    ENDIF.

    CLEAR: ok_code_0003,su,su1,descricao,bin_origem,matnr,
           texto_processo,bin_input,bin_output_o,bin_output_d,
           bin_output_kober_d.

    f3_activo = 'X'.
    SET SCREEN '0000'. LEAVE SCREEN.

  ENDIF.
ENDMODULE.                 " EXIT3  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_SU3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_su3 INPUT.

  DATA: su_aux(20).
  CLEAR: corredor,
         n_mensulas_ocupadas.
  CLEAR su_aux.


  IF NOT su IS INITIAL.
    IF su <> su1.
      CLEAR text.
      WRITE su TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '033'
          message_var1   = text.
      CLEAR su.
      MOVE 'SU' TO cursorfield.

** 04.03.2005 - ROFFD ** DEL
** Regista Incidência de posição errada
*      CALL FUNCTION 'ZWM_INSERT_ERROR'
*        EXPORTING
*          armazem    = xuser-lgnum
*          incidencia = '1'
*          posicao    = bin_output_o
*          sscc       = su1
*        EXCEPTIONS
*          no_commit  = 1
*          OTHERS     = 2.
** 04.03.2005 - ROFFD ** DEL

    ELSE.
*OK, ver se esta a fazer saida ou entrada
*para entao actualizar a tabela zwm014
*para desbloquear a mensula.
*      IF tipo_tri = 'E'.
*entrada

** Fazer lock antes de actualizar a tabela das mensulas
*        DO.
*          CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
*            EXPORTING
*              mode_keyword   = 'X'
*              keyword_       = 'ZWM014'
*            EXCEPTIONS
*              foreign_lock   = 1
*              system_failure = 2
*              OTHERS         = 3.
*          IF sy-subrc = 0.
*            EXIT.
*          ELSE.
*            WAIT UP TO 1 SECONDS.
*          ENDIF.
*        ENDDO.
*** SG
*        UPDATE zwm014 SET estado = ' ' su = su_aux prioridade = ' '
*        WHERE armazem     = xuser-lgnum AND
*              su_transito = su.
****
*        atribuida = 'TRUE'.
*        IF sy-subrc = 0.
*          COMMIT WORK.
*        ELSE.
*          ROLLBACK WORK.
*        ENDIF.
*
*
** Verificar se o corredor depois da retirada da SU em análise
** ainda deve manter a prioridade máxima
*
***** SG
*        CLEAR : corredor,
*                n_mensulas_ocupadas.
*        SELECT SINGLE mensula FROM zwm014 INTO zwm014-mensula
*                              WHERE armazem = xuser-lgnum AND
*                                    su_transito = su.
*        IF sy-subrc = 0.
*          CONCATENATE zwm014-mensula(3) '%' INTO corredor.
*          SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
*                          WHERE armazem = xuser-lgnum AND
*                                mensula LIKE corredor AND
*                                estado = 'X'.
*          IF n_mensulas_ocupadas < 3.
*
*            UPDATE zwm014 SET prioridade = ' '
*                          WHERE armazem = xuser-lgnum AND
*                                mensula LIKE corredor AND
*                                estado = 'X'.
*            COMMIT WORK.
*
*          ENDIF.
*          CLEAR : corredor.
*        ENDIF.
****** SG
*        CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*          EXPORTING
*            mode_keyword   = 'X'
*            keyword_       = 'ZWM014'
*          EXCEPTIONS
*            foreign_lock   = 1
*            system_failure = 2
*            OTHERS         = 3.
*
*        CALL FUNCTION 'ZWM_MODIFY_ZWM011'
*          EXPORTING
*            armazem            = xuser-lgnum
*            to_number          = to_tri
*            to_item            = '0001'
*            status             = 'P'
*          EXCEPTIONS
*            error_update_table = 1
*            OTHERS             = 2.
*
*      ELSE.
*
*        atribuida = 'TRUE'.
*        CALL FUNCTION 'ZWM_MODIFY_ZWM011'
*          EXPORTING
*            armazem            = xuser-lgnum
*            to_number          = to_tri
*            to_item            = '0001'
*            status             = 'P'
*          EXCEPTIONS
*            error_update_table = 1
*            OTHERS             = 2.
*      ENDIF.

** -> FL
      atribuida = 'TRUE'.
      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
        EXPORTING
          armazem            = xuser-lgnum
          to_number          = to_tri
          to_item            = '0001'
          status             = 'P'
        EXCEPTIONS
          error_update_table = 1
          OTHERS             = 2.

** <- FL

      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.
  ELSE.
*falta introdução da palete
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '037'
        message_var1   = text.
    MOVE 'SU' TO cursorfield.
  ENDIF.

ENDMODULE.                 " CHECK_SU3  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0003 INPUT.

  DATA: lv_exp_aut     TYPE flag.
  DATA: lv_queue_tri_g TYPE ltak-queue.
  DATA flag.

  CASE ok_code_0003.
    WHEN 'NEXT'.

      IF NOT bin_input IS INITIAL AND NOT su IS INITIAL AND
         bin_input = bin_output_d.
        REFRESH return_msg.

** Entrada
**********************************************************************
        IF tipo_tri = 'E'.

          " Fazer lock antes de actualizar a tabela das mensulas
          DO.
            CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
              EXPORTING
                mode_keyword   = 'X'
                keyword_       = 'ZWM014'
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

*vai fazer transfer quando arrumar no bin destino
*to de entrada na zona de trilaterais
          CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
            EXPORTING
              armazem              = armazem_tri
              confirm_type         = 'T'
            TABLES
              return_msg           = return_msg
            CHANGING
              to                   = to_tri
            EXCEPTIONS
              confirm_type_error   = 1
              to_not_found         = 2
              to_already_confirmed = 3
              to_already_picked    = 4
              to_not_picked        = 5
              wrong_su             = 6
              missing_su           = 7
              error                = 8
              OTHERS               = 9.

          IF sy-subrc <> 0.

            CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
              EXPORTING
                mode_keyword   = 'X'
                keyword_       = 'ZWM014'
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.

            READ TABLE return_msg INDEX 1.
            IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.
              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
                EXPORTING
                  message_id     = return_msg-msgid
                  message_lang   = sy-langu
                  message_type   = return_msg-msgtyp
                  message_number = return_msg-msgnr
                  message_var1   = return_msg-msgv1
                  message_var2   = return_msg-msgv2
                  message_var3   = return_msg-msgv3.

*LIMPA os campos PARA TODOS OS ECRANS.
              CLEAR:to_tri,item_tri,su,su1,descricao,bin_origem,
                    texto_processo,bin_destino,bin_destino1,matnr,
                   store_destino,store_origem,bin_output_o,bin_output_d,
                    bin_input,bin_output_kober_d.

              MOVE 'BIN_INPUT' TO cursorfield.
              EXIT.
*              SET SCREEN '0000'.LEAVE SCREEN.
            ENDIF.
          ELSE.
*PROCESSAMENTO OK
*actualizar tabela ZWM011 com STATUS T
            CALL FUNCTION 'ZWM_MODIFY_ZWM011'
              EXPORTING
                armazem            = xuser-lgnum
                to_number          = to_tri
*               TO_ITEM            = ITEM_TRI
                to_item            = '0001'
                status             = 'T'
              EXCEPTIONS
                error_update_table = 1
                OTHERS             = 2.

** Verificar se é remontada

            SELECT SINGLE *
                FROM zwm020
                    WHERE armazem = xuser-lgnum AND
                          ( p1 = su OR p2 = su ).
            IF sy-subrc = 0.
              DELETE FROM zwm013
              WHERE armazem   = xuser-lgnum AND
                    ( sscc = zwm020-p1 OR sscc = zwm020-p2 ).
              COMMIT WORK.
            ELSE.
              DELETE FROM zwm013
              WHERE armazem   = xuser-lgnum AND
                    sscc = su.
              COMMIT WORK.
            ENDIF.

*********************************************************
** Fazer lock antes de actualizar a tabela das mensulas
*            DO.
*              CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
*                EXPORTING
*                  mode_keyword   = 'X'
*                  keyword_       = 'ZWM014'
*                EXCEPTIONS
*                  foreign_lock   = 1
*                  system_failure = 2
*                  OTHERS         = 3.
*              IF sy-subrc = 0.
*                EXIT.
*              ELSE.
*                WAIT UP TO 1 SECONDS.
*              ENDIF.
*            ENDDO.


** SG
            DATA w_zwm014 LIKE zwm014 OCCURS 0 WITH HEADER LINE.
            CLEAR w_zwm014.

            SELECT SINGLE *
                FROM zwm014
                    WHERE armazem = xuser-lgnum AND
                          su = su.

            MOVE-CORRESPONDING zwm014 TO w_zwm014.

            CLEAR w_zwm014-estado.
            w_zwm014-su = su_aux.
            CLEAR w_zwm014-prioridade.
            CLEAR w_zwm014-su_transito.
            CLEAR w_zwm014-bin.
            MODIFY zwm014 FROM w_zwm014.
            IF sy-subrc = 0.
              COMMIT WORK.
            ELSE.
              ROLLBACK WORK.
            ENDIF.

*            UPDATE zwm014 SET estado = ' ' su = su_aux prioridade = ' '
*            WHERE armazem     = xuser-lgnum AND
*                  su_transito = su.
***
*        atribuida = 'TRUE'.



** Verificar se o corredor depois da retirada da SU em análise
** ainda deve manter a prioridade máxima

**** SG
            CLEAR : corredor,
                    n_mensulas_ocupadas.
*            SELECT SINGLE mensula FROM zwm014 INTO zwm014-mensula
*                                  WHERE armazem = xuser-lgnum AND
*                                        su_transito = su.
*            IF sy-subrc = 0.
            CONCATENATE w_zwm014-mensula(3) '%' INTO corredor.
            SELECT COUNT(*) FROM zwm014 INTO n_mensulas_ocupadas
                            WHERE armazem = xuser-lgnum AND
                                  mensula LIKE corredor AND
                                  estado = 'X'.
            IF n_mensulas_ocupadas < 3.

              UPDATE zwm014 SET prioridade = ' '
                            WHERE armazem = xuser-lgnum AND
                                  mensula LIKE corredor AND
                                  estado = 'X'.
              COMMIT WORK.
            ENDIF.
            CLEAR : corredor.
*            ENDIF.
***** SG
            CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
              EXPORTING
                mode_keyword   = 'X'
                keyword_       = 'ZWM014'
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.
*          ENDIF.


********************************************************
            MOVE 'SU' TO cursorfield.
            CLEAR: to_tri,item_tri,atribuida.
          ENDIF.

** Saída
**********************************************************************
        ELSEIF tipo_tri = 'S'.

** Validar se é uma expedição para gravitica do Automático (WCS)
          PERFORM check_exp_aut_wcs CHANGING lv_exp_aut.


          " Mudar Fila para Saida do Trilateral para Gravítica
          " Não faz pick à OT -> A fila não aparece na atribuição de tarefas do Trilateral
*          IF lv_exp_aut IS NOT INITIAL.
*
*            PERFORM get_parameter USING armazem_tri
*                                        'GESTAO_FILAS'
*                                        'FILA_SAIDA_TRI_GRA'
*                                        lv_queue_tri_g.
*
*
*
*            CALL FUNCTION 'L_RFMON_TO_REASSIGN'
*              EXPORTING
*                iv_lgnum    = armazem_tri
*                iv_tanum    = to_tri
*                iv_queue    = lv_queue_tri_g
*              EXCEPTIONS
*                to_complete = 2.
*
*            IF sy-subrc = 0.
*              COMMIT WORK AND WAIT.
*            ELSE.
*              CLEAR return_msg.
*              return_msg-msgid = sy-msgid.
*              return_msg-msgnr = sy-msgno.
*              return_msg-msgv1 = sy-msgv1.
*              return_msg-msgv2 = sy-msgv2.
*              return_msg-msgv3 = sy-msgv3.
*
*              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*                EXPORTING
*                  message_id     = return_msg-msgid
*                  message_lang   = sy-langu
*                  message_type   = return_msg-msgtyp
*                  message_number = return_msg-msgnr
*                  message_var1   = return_msg-msgv1
*                  message_var2   = return_msg-msgv2
*                  message_var3   = return_msg-msgv3.
*
*              CLEAR: bin_input.
*              MOVE 'BIN_INPUT' TO cursorfield.
*              EXIT.
*            ENDIF.
*
*            " Validar Palete remontada
*            SELECT SINGLE *
*              FROM zwm020
*              WHERE armazem = xuser-lgnum AND
*               p1 = su.
*            IF sy-subrc = 0.
*              SELECT SINGLE *
*                  FROM ltap
*                      WHERE lgnum = xuser-lgnum AND
*                            vlenr = zwm020-p2   AND
*                            pquit = ' '.
*
*              IF sy-subrc = 0.
*                CALL FUNCTION 'L_RFMON_TO_REASSIGN'
*                  EXPORTING
*                    iv_lgnum    = ltap-lgnum
*                    iv_tanum    = ltap-tanum
*                    iv_queue    = lv_queue_tri_g
*                  EXCEPTIONS
*                    to_complete = 2.
*
*                IF sy-subrc = 0.
*                  COMMIT WORK AND WAIT.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*
*          ELSE.

          " vai fazer pick quando arrumar na mensula
          " to de saida da zona de trilaterais
          CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
            EXPORTING
              armazem              = armazem_tri
              confirm_type         = 'P'
            TABLES
              return_msg           = return_msg
            CHANGING
              to                   = to_tri
            EXCEPTIONS
              confirm_type_error   = 1
              to_not_found         = 2
              to_already_confirmed = 3
              to_already_picked    = 4
              to_not_picked        = 5
              wrong_su             = 6
              missing_su           = 7
              error                = 8
              OTHERS               = 9.

          IF sy-subrc <> 0.
            READ TABLE return_msg INDEX 1.
            IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.
              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
                EXPORTING
                  message_id     = return_msg-msgid
                  message_lang   = sy-langu
                  message_type   = return_msg-msgtyp
                  message_number = return_msg-msgnr
                  message_var1   = return_msg-msgv1
                  message_var2   = return_msg-msgv2
                  message_var3   = return_msg-msgv3.

              CLEAR: bin_input.
              MOVE 'BIN_INPUT' TO cursorfield.
              EXIT.
            ENDIF.
          ENDIF.

          " processamento OK
          " verificar se é remontada e se tem que confirmar a segunda palete actualiza tabelas
          " chama nova tarefa
          " actualizar tabela ZWM011 com STATUS T
          " pq embora faça pick à TO, se houver crash
          " nao é para dar a tarefa novamente ao utilizador
*            break roffd.
          SELECT SINGLE *
              FROM ltak
                  WHERE lgnum = xuser-lgnum AND
                        tanum = to_tri.

*            IF ltak-queue <> 'QUEUEPT'.
          SELECT SINGLE *
              FROM zwm020
                  WHERE armazem = xuser-lgnum AND
                        p1 = su .
          IF sy-subrc = 0.
            SELECT SINGLE *
                FROM ltap
                    WHERE lgnum = xuser-lgnum AND
                          vlenr = zwm020-p2 AND
                          pquit = ' '.

            IF sy-subrc = 0.
              IF ltap-tanum <> to_tri.
                IF ltap-nltyp <> 'PRM' OR ltap-nltyp <> 'INC'.
                  DO 30 TIMES.
                    CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
                      EXPORTING
                        armazem              = ltap-lgnum
                        confirm_type         = 'P'
                      TABLES
                        return_msg           = return_msg
                      CHANGING
                        to                   = ltap-tanum
                      EXCEPTIONS
                        confirm_type_error   = 1
                        to_not_found         = 2
                        to_already_confirmed = 3
                        to_already_picked    = 4
                        to_not_picked        = 5
                        wrong_su             = 6
                        missing_su           = 7
                        error                = 8
                        OTHERS               = 9.
                    IF sy-subrc = 0.
                      EXIT.
                    ELSE.
                      WAIT UP TO 1 SECONDS.
                    ENDIF.
                  ENDDO.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
*          ENDIF.

*            ENDIF.
          CALL FUNCTION 'ZWM_MODIFY_ZWM011'
            EXPORTING
              armazem            = xuser-lgnum
              to_number          = to_tri
              to_item            = item_tri
              status             = 'T'
            EXCEPTIONS
              error_update_table = 1
              OTHERS             = 2.


          IF ltak-queue <> 'QUEUEPT'.

            CLEAR i_sscc.
            REFRESH i_sscc.

            SELECT SINGLE matnr charg vsola meins werks
                          lgort vlenr vltyp vlpla
              INTO (i_sscc-material, i_sscc-lote_producao,
                      i_sscc-quantidade, i_sscc-uni,
                      ltap-werks, ltap-lgort, ltap-vlenr,
                      ltap-vltyp, ltap-vlpla)
                          FROM ltap
                              WHERE lgnum = xuser-lgnum AND
                                    tanum = to_tri AND
                                    tapos = item_tri.

            APPEND i_sscc.
            CLEAR i_sscc.


            SELECT SINGLE lzone INTO lagp-lzone
                FROM lagp
                    WHERE lgnum = xuser-lgnum AND
                          lgtyp = ltap-vltyp AND
                          lgpla = ltap-vlpla.
            CLEAR mov.
            PERFORM get_parameter USING xuser-lgnum
                                        'SAIDA_ARMAZEM_TRI'
                                        'MOV_WM'
                                         mov.
            CLEAR flag.
            WHILE flag IS INITIAL.

              CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
                EXPORTING
                  warehouse      = xuser-lgnum
                  mov_type       = mov
                  plant          = ltap-werks
                  s_loc          = ltap-lgort
                  certificado    = lagp-lzone
                  sscc_adicional = ltap-vlenr
                TABLES
                  return_msg     = return_msg
                  sscc           = i_sscc
                EXCEPTIONS
                  error          = 1
                  OTHERS         = 2.
              IF sy-subrc = 0.
                flag = 'X'.
              ELSE.
                CLEAR flag.
              ENDIF.
            ENDWHILE.

            MOVE 'SU' TO cursorfield.
            CLEAR: to_tri,item_tri,atribuida.
          ELSE.
            MOVE 'SU' TO cursorfield.
            CLEAR: to_tri,item_tri,atribuida.
          ENDIF.

        ENDIF.
      ENDIF.

** Registo das incidências
    WHEN 'INCID'.

*      IF NOT su IS INITIAL.
      CLEAR ecra_chamador.
      IF lrf_wkqu-devty = '8X40'.
        CALL SCREEN '0009'.
      ELSEIF lrf_wkqu-devty = '16X20'.
        CALL SCREEN '0010'.
      ENDIF.

*      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0003  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT7  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit7 INPUT.

  CLEAR: ok_code_0007,
         pulmao1,
         pulmao2,
         ped_compra,
         pedido,
         item,
         cursorfield,
         to_ret,
         item_ret,
         tab_zwm011.


** apaga entrada do dicionario de dados para o user
  DELETE FROM zwm011
  WHERE user_name = sy-uname AND
        status <> 'P'.

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  f3_activo = 'X'.

  SET SCREEN '0001'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT7  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0007  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0007 INPUT.

  CASE ok_code_0007.

    WHEN 'NEXT'.

      IF NOT pulmao2 IS INITIAL AND ped_compra IS INITIAL.
        MOVE 'PED_COMPRA' TO cursorfield.
        PERFORM confirma_to.
      ELSEIF NOT pulmao2 IS INITIAL AND NOT ped_compra IS INITIAL.
*        PERFORM REGISTA_DADOS.

** Actualização do lote - no caso da entrada de terceiros o lote é igual
** ao pedido de compra
        MOVE ped_compra(10) TO lote.
        CLEAR : cursorfield.
** Ecrã para conferência das entradas de terceiros
        IF lrf_wkqu-devty(5) = '16X20'.
          CALL SCREEN '0013'.
        ELSE.
          CALL SCREEN '0014'.
        ENDIF.
      ENDIF.

    WHEN OTHERS.

  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0007  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_PULMAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pulmao INPUT.
  CLEAR : text1,
          text2.

  IF NOT pulmao2 IS INITIAL.

** Descodifica Bin
    CALL FUNCTION 'ZWM_DECODE_BIN'
      EXPORTING
        iv_lgnum    = xuser-lgnum
        iv_bin_code = pulmao2
      IMPORTING
        ev_bin      = pulmao2
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      READ TABLE lt_messages INTO ls_messages INDEX 1.
      IF sy-subrc = 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = ls_messages-msgid
            message_lang   = sy-langu
            message_type   = ls_messages-msgtyp
            message_number = ls_messages-msgnr
            message_var1   = ls_messages-msgv1
            message_var2   = ls_messages-msgv2
            message_var3   = ls_messages-msgv3
            message_var4   = ls_messages-msgv4.
        CLEAR : pulmao2.
        REFRESH lt_messages.
        MOVE 'PULMAO2' TO cursorfield.
        RETURN.
      ENDIF.
    ENDIF.

    IF pulmao2 <> pulmao1.
** ERRO

      WRITE pulmao2 TO text1 LEFT-JUSTIFIED.
      WRITE pulmao1 TO text2 LEFT-JUSTIFIED.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '036'
          message_var1   = text1
          message_var2   = text2.

** 04.03.2005 - ROFFD ** DEL
** Registo da incidencia
*      CALL FUNCTION 'ZWM_INSERT_ERROR'
*        EXPORTING
*          armazem    = xuser-lgnum
*          incidencia = '1'
*          posicao    = pulmao2
*        EXCEPTIONS
*          no_commit  = 1
*          OTHERS     = 2.
*      IF sy-subrc = 0.
      CLEAR : pulmao2.
      MOVE 'PULMAO2' TO cursorfield.
*      ENDIF.
** 04.03.2005 - ROFFD ** DEL

    ELSE.
      MOVE 'PED_COMPRA' TO cursorfield.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_PULMAO  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_BIN3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_bin3 INPUT.

  IF NOT bin_input IS INITIAL.

** Descodifica Bin
    CALL FUNCTION 'ZWM_DECODE_BIN'
      EXPORTING
        iv_lgnum    = xuser-lgnum
        iv_bin_code = bin_input
      IMPORTING
        ev_bin      = bin_input
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      READ TABLE lt_messages INTO ls_messages INDEX 1.
      IF sy-subrc = 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = ls_messages-msgid
            message_lang   = sy-langu
            message_type   = ls_messages-msgtyp
            message_number = ls_messages-msgnr
            message_var1   = ls_messages-msgv1
            message_var2   = ls_messages-msgv2
            message_var3   = ls_messages-msgv3
            message_var4   = ls_messages-msgv4.
        CLEAR bin_input.
        REFRESH lt_messages.
        MOVE 'BIN_INPUT' TO cursorfield.
        RETURN.
      ENDIF.
    ENDIF.

    IF bin_input <> bin_output_d.
*bin invalido
      CLEAR text.
      WRITE bin_input TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '034'
          message_var1   = text.

** 04.03.2005 - ROFFD ** DEL
**regista incidencia
*      CALL FUNCTION 'ZWM_INSERT_ERROR'
*        EXPORTING
*          armazem          = xuser-lgnum
*          incidencia       = '1'
*          posicao          = bin_input
**         SSCC             =
*       EXCEPTIONS
*         no_commit        = 1
*         OTHERS           = 2.
** 04.03.2005 - ROFFD ** DEL

      CLEAR bin_input.
      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.
  ELSE.
    CLEAR text.
    WRITE bin_input TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '035'
        message_var1   = text.
    CLEAR bin_input.
    MOVE 'BIN_INPUT' TO cursorfield.
  ENDIF.

ENDMODULE.                 " CHECK_BIN3  INPUT
*&---------------------------------------------------------------------*
*&      Form  cria_nova_to_pulmao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_nova_to_pulmao .

** Este campo necessita de estar actualizado
  CLEAR pulmao2.
  pulmao2 = bin_output_o.
  PERFORM cria_to.
  CLEAR pulmao2.

ENDFORM.                    " cria_nova_to_pulmao
