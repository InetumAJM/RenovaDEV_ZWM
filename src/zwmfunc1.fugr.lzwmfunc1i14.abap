*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I14 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT21  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit21 INPUT.
*apaga entrada do dicionario de dados para o user
  DELETE FROM zwm011
  WHERE user_name = sy-uname AND
        status <> 'P'.

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  CLEAR: ok_code_0021.
  CLEAR:su,su1,descricao,bin_origem,texto_processo,
        bin_input,bin_output_o,bin_output_d,matnr,
        posicao_pulmao1, posicao_pulmao2,bin_output_kober_d.

  f3_activo = 'X'.
  SET SCREEN '0001'. LEAVE SCREEN.

ENDMODULE.                 " EXIT21  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0021  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0021 INPUT.

  DATA: lt_zwm026 TYPE TABLE OF zwm026 WITH HEADER LINE.
  DATA: lt_ltap   TYPE TABLE OF ltap   WITH HEADER LINE.
  DATA: lt_ltap_creat TYPE TABLE OF ltap_creat WITH HEADER LINE.

  CLEAR: zwm020, to3, to_remontada.

  CASE ok_code_0021.

    WHEN 'NEXT'.

      IF bin_output_d(3) = 'PUL'.
        CHECK NOT posicao_pulmao2 IS INITIAL.
      ENDIF.

      CHECK NOT su IS INITIAL AND NOT bin_input IS INITIAL.

      " Validar Palete em porta com portico
      IF bin_output_d(3) = 'DCK'.

        PERFORM check_pal_porta_portico CHANGING sy-subrc.
        IF sy-subrc <> 0.
          CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2, ok_code_0001.
          MOVE 'BIN_INPUT' TO cursorfield.
          EXIT.
        ENDIF.

      ENDIF.

      " Confirmar totalmente segunda TO
      CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
        EXPORTING
          armazem              = xuser-lgnum
          confirm_type         = 'T'
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
              message_var1   = return_msg-msgv1
              message_var2   = return_msg-msgv2
              message_var3   = return_msg-msgv3.

          CLEAR: bin_input, posicao_pulmao1, posicao_pulmao2,
                               ok_code_0001.
          MOVE 'BIN_INPUT' TO cursorfield.
          EXIT.
        ENDIF.
      ENDIF.

** Modificar status da tabela zwm011 - T
      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
        EXPORTING
          armazem            = xuser-lgnum
          to_number          = to_ret
          to_item            = '0001'
          status             = 'T'
        EXCEPTIONS
          error_update_table = 1
          OTHERS             = 2.

      " Confirmação BPK
      IF bin_output_d(3) = 'BPK'.

*          PERFORM create_ot_bpk.

        " Cais
      ELSEIF bin_output_d(3) = 'DCK'.

        PERFORM confirm_ots_pal_picking.

        PERFORM actualiza_paletes_carro.
      ELSE.
        PERFORM actualiza_paletes_pulmao.
      ENDIF.


      IF gv_only_one EQ abap_true.
        LEAVE TO SCREEN 0.
      ENDIF.



*** Actualizar número de paletes no pulmão
*        PERFORM actualiza_paletes_pulmao.

** Avaliar de novo as TOs em aberto para atribuir nova TAREFA
      MOVE 'SU' TO cursorfield.
      CLEAR: pack_material, quantidade, unidade, material,
             lote,plant, lgort, tipo_palete, su, paletes_dif,
             pulmao2,pedido,item,ped_compra,posicao_pulmao,
             tab_zwm010,kober.
      CLEAR : tab_zwm011, return_msg.

      REFRESH : return_msg,tab_zwm010.

      SELECT * FROM zwm010 INTO TABLE tab_zwm010
      WHERE armazem = xuser-lgnum AND
            equipamento = equipamento_.

      CALL FUNCTION 'ZWM_GET_TO_RET'
        EXPORTING
          armazem           = xuser-lgnum
          tamanho           = lrf_wkqu-devty(5)
        IMPORTING
          nova_to           = tab_zwm011
          tipo_queue        = tipo
        TABLES
          l_zwm010          = tab_zwm010
          return_msg        = return_msg
        EXCEPTIONS
          no_equipment      = 1
          no_work_available = 2
          OTHERS            = 3.
      IF sy-subrc <> 0.
*            BREAK-POINT.
        READ TABLE return_msg INDEX 1.
        IF sy-subrc = 0.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = return_msg-msgid
              message_lang   = sy-langu
              message_type   = return_msg-msgtyp
              message_number = return_msg-msgnr
              message_var1   = return_msg-msgv1.
          CLEAR equipamento_.
** Para saltar para o ecrã correcto
          f3_activo = 'X'.
          IF lrf_wkqu-devty(5) = '16X20'.
            SET SCREEN '0001'.LEAVE SCREEN.
          ELSE.
            SET SCREEN '0002'.LEAVE SCREEN.
          ENDIF.
        ENDIF.
      ELSE.

** Atribuição de nova tarefa
        CALL FUNCTION 'ZWM_CALL_TASK_RET'
          EXPORTING
            armazem     = xuser-lgnum
            ecran       = ecran
            tab_zwm011  = tab_zwm011
            tipo_queue  = tipo
            equipamento = equipamento_.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0021  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_SU21  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_su21 INPUT.

*  DATA: su_aux(20).
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
    ELSE.
** Confirmação da segunda TO - PICKED
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
              message_var1   = return_msg-msgv1
              message_var2   = return_msg-msgv2
              message_var3   = return_msg-msgv3.

          SET SCREEN '0000'.LEAVE SCREEN.
        ENDIF.
      ELSE.
** Modificar status da tabela zwm011 - P
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


ENDMODULE.                 " CHECK_SU21  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_BIN21  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_bin21 INPUT.

  CLEAR : lgtyp,lgpla.

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

      CLEAR text.
      WRITE bin_input TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '034'
          message_var1   = text.

      CLEAR bin_input.
      MOVE 'BIN_INPUT' TO cursorfield.
    ELSE.
      IF bin_output_d(3) = 'PUL'.

        CLEAR: zwm040.
        SELECT SINGLE * FROM zwm040
        WHERE lgnum EQ xuser-lgnum
          AND refnr EQ grupo
          AND remessa EQ remessa.

        IF sy-subrc EQ 0.
          SELECT SINGLE *
              FROM zwm028
                  WHERE lgnum = xuser-lgnum AND
                        refnr = grupo AND
                        remessa = zwm040-id_servisan.
        ELSE.
          SELECT SINGLE *
              FROM zwm028
                  WHERE lgnum = xuser-lgnum AND
                        refnr = grupo AND
                        remessa = remessa.
        ENDIF.
** RL <- MOD 07.04.2005 -----------------------------------------


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
*  Data: 05.06.2012 16:59:51
*  Motivo: Altera posição em Meio Pulmão
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

            CLEAR: bin_input, posicao_pulmao2, ok_code_0001.
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



*      CLEAR: numpal_r, numpal.
*      IF bin_output_d(3) = 'PUL'.
*        IF bin_output_d+12(2) <> '02'.
*          SELECT SINGLE COUNT(*) INTO numpal_r
*              FROM zwm013
*                  WHERE armazem = xuser-lgnum AND
*                        destino = bin_output_d AND
*                        ( tipo_palete = 'X2' OR Apagado o P para nao aparecer no search usar is_remontada
*                          tipo_palete = 'X5' ).
*          IF sy-subrc = 0.
*            numpal_r = numpal_r DIV 2.
*          ENDIF.
*
*          SELECT SINGLE COUNT(*) INTO numpal
*              FROM zwm013
*                  WHERE armazem = xuser-lgnum AND
*                        destino = bin_output_d AND
*                        tipo_palete <> 'X2' AND Apagado o P para nao aparecer no search usar is_remontada
*                        tipo_palete <> 'X5'.
*
*          numpal = numpal + numpal_r.
*
*        ENDIF.
*
*        IF numpal >= 17.
*          CLEAR text.
*          CONCATENATE bin_output_d(12) '02' INTO bin_output_d.
*          WRITE bin_output_d TO text LEFT-JUSTIFIED.
*          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*            EXPORTING
*              message_id     = 'ZWMMSG001'
*              message_lang   = sy-langu
*              message_type   = 'E'
*              message_number = '176'
*              message_var1   = text.
*
*          CLEAR: bin_input, posicao_pulmao2, ok_code_0001.
*          MOVE 'BIN_INPUT' TO cursorfield.
*
*        ELSE.
*      CLEAR: t_zwm013, pul, numpal.
*      REFRESH: t_zwm013.
*      CONCATENATE bin_output_d+12(2) '%' INTO pul.
*
*      SELECT * INTO TABLE t_zwm013
*          FROM zwm013
*              WHERE armazem = xuser-lgnum AND
*                    destino LIKE pul.
*
*      SORT t_zwm013 BY destino posicao_pulmao.
*
*      DELETE ADJACENT DUPLICATES
*          FROM t_zwm013 COMPARING destino posicao_pulmao.
*
*
*      DESCRIBE TABLE t_zwm013 LINES numpal.
*
*      IF numpal >= 17.
*        CLEAR text.
*        CONCATENATE bin_output_d(12) '02' INTO bin_output_d.
*        WRITE bin_output_d TO text LEFT-JUSTIFIED.
*        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*          EXPORTING
*            message_id     = 'ZWMMSG001'
*            message_lang   = sy-langu
*            message_type   = 'E'
*            message_number = '176'
*            message_var1   = text.
*
*        CLEAR: bin_input, posicao_pulmao2, ok_code_0001.
*        MOVE 'BIN_INPUT' TO cursorfield.
*
*      ELSE.
** RL -> MOD 07.04.2005 -----------------------------------------
*          SELECT SINGLE *
*              FROM zwm028
*                  WHERE lgnum = xuser-lgnum AND
*                        refnr = grupo AND
*                        remessa = remessa.



ENDMODULE.                 " CHECK_BIN21  INPUT

*&---------------------------------------------------------------------*
*&      Form  actualiza_paletes_pulmao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_paletes_pulmao .
  DATA lv_glock_active TYPE abap_bool. " << INS ROFF(SDF):TMGP:22.01.2016 14:41:50

  DATA : wait_time(20), lock_key(50).
  DATA: chave_bloq LIKE keyword-keyword.

  DATA: lt_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE,
        lt_vbpa   LIKE vbpa   OCCURS 0 WITH HEADER LINE.
  DATA: next_desbloq(1), l_kunnr LIKE vbpa-kunnr.

  DATA: lv_2spart  TYPE flag,
        lv_lgtyp_d TYPE lgtyp,
        lv_add_su2 TYPE flag.

  DATA: ls_zwm066 TYPE zwm066,
        ls_zwm028 TYPE zwm028.

  CLEAR : wait_time, lock_key, chave_bloq.


  CALL FUNCTION 'ZWM_SPLIT_BIN'
    EXPORTING
      bin   = bin_output_d
    IMPORTING
      lgtyp = lv_lgtyp_d.

  IF lv_lgtyp_d EQ 'PRM'.
    EXIT.
  ENDIF.

*& Begin of Modification by Carlos Fernandes - ROFF @ 22.02.2016
**& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:42:12
** lv_glock_active = abap_false.
**
**  SELECT valor UP TO 1 ROWS
**    FROM zwm001 INTO lv_glock_active
**    WHERE armazem EQ xuser-lgnum
**      AND processo EQ 'ALTERA_GLOCK'
**      AND parametro EQ 'ACTIVAR'.
**  ENDSELECT.
*
**& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:42:12

  SELECT mandt UP TO 1 ROWS
    FROM zwm001 INTO sy-mandt
    WHERE armazem EQ xuser-lgnum
      AND processo EQ 'CHANGE_ORDEM'
      AND parametro EQ 'NLTYP_OK'
      AND valor EQ zwm011-ultimo_tipo_dep.
  ENDSELECT.
  IF sy-subrc EQ 0.
    lv_glock_active = abap_true.
  ELSE.
    lv_glock_active = abap_false.
  ENDIF.
*& End of Modification by Carlos Fernandes - ROFF @ 22.02.2016

** Palete remontada foram as duas para o carro
  IF NOT to3 IS INITIAL AND to_remontada IS INITIAL.
    lv_add_su2 = 'X'.
  ENDIF.

** Atualizar Paletes Pulmão
  CALL FUNCTION 'ZWM_CHANGE_PAL_PUL'
    EXPORTING
      i_lgnum          = xuser-lgnum
      i_grupo          = grupo
      i_remessa        = remessa
      i_bin_output_d   = bin_output_d
      i_tipo_palete    = tipo_palete
      i_su             = su
      i_posicao_pulmao = posicao_pulmao2
      i_glock_active   = lv_glock_active
      i_add_su2        = lv_add_su2
      i_su2            = zwm020-p2.

** Envio de IDOC para liberar próxima sequencia do grupo
  CALL FUNCTION 'ZWM_IDOC_FREE_WORK_WCS'
    EXPORTING
      i_lgnum = xuser-lgnum
      i_refnr = grupo
      i_step  = '2'.


*** Tempo de espera no lock
*  PERFORM get_parameter
*          USING xuser-lgnum
*                'ATRIBUICAO_TO'
*                'WAIT_TIME'
*                wait_time.
*
*  MOVE 'PALETES_PULMAO' TO lock_key.
*** Fazer Lock para não existirem dois ou
*** mais operários a actualizar o número
*** de paletes no pulmão
*  DO.
*    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
*      EXPORTING
*        mode_keyword   = 'X'
*        keyword_       = 'ZWM028'
*      EXCEPTIONS
*        foreign_lock   = 1
*        system_failure = 2
*        OTHERS         = 3.
*    IF sy-subrc = 0.
*      EXIT.
*    ELSE.
*      WAIT UP TO wait_time SECONDS.
*    ENDIF.
*  ENDDO.
*
*** RL -> MOD 26.05.2005
*** Se a palete ja estiver no pulmao não actualiza as paletes n pulmao
** SELECT SINGLE * FROM zwm013 WHERE armazem = xuser-lgnum AND sscc = su.
*
*  DATA: l_dest LIKE zwm013-destino.
*
*  CONCATENATE bin_output_d(3) '%' INTO l_dest.
*
*  SELECT SINGLE * FROM zwm013
*  WHERE armazem EQ   xuser-lgnum
*    AND sscc    EQ   su
*    AND destino LIKE l_dest.
*
*** RL <- MOD 26.05.2005
*
*  IF sy-subrc <> 0.
*** Actualizar entrada do grupo com as paletes q já estão no pulmão
*
*    DATA: aux_pal TYPE i,
*          l_ordem LIKE zwm028-ordem,
*          l_lock  LIKE zwm028-zlock.
*    CLEAR: aux_pal, zwm028.
*
*    SELECT SINGLE * FROM zwm028
*                    WHERE lgnum = xuser-lgnum AND
*                          refnr = grupo AND
*                          remessa = ' '.
*    IF sy-subrc = 0.
*      aux_pal = zwm028-paletes_pulmao + 1.
*
*      UPDATE zwm028 SET paletes_pulmao = aux_pal
*          WHERE lgnum = xuser-lgnum AND
*                refnr = grupo AND
*                remessa = ' '.
*      COMMIT WORK AND WAIT.
*    ENDIF.
*
*** Actualizar entrada da remessa com as paletes q já estão no pulmão
*    CLEAR zwm028.
*    SELECT SINGLE * FROM zwm028
*                      WHERE lgnum = xuser-lgnum AND
*                            refnr = grupo AND
*                            remessa = remessa.
*    IF sy-subrc = 0.
*      CLEAR aux_pal.
*      aux_pal = zwm028-paletes_pulmao + 1.
*
*      UPDATE zwm028 SET paletes_pulmao = aux_pal
*          WHERE lgnum = xuser-lgnum AND
*                refnr = grupo AND
*                remessa = remessa.
*      COMMIT WORK AND WAIT.
*    ENDIF.
*
*** Verificar se as paletes no pulmao são iguais á da Remessa para
*** desbloquear a remessa de ordem n + 1
*
*    CLEAR zwm028.
*    SELECT SINGLE * FROM zwm028
*        WHERE lgnum = xuser-lgnum AND
*              refnr = grupo AND
*              remessa = ' '.
*    IF zwm028-st_pul = 'PLT' AND zwm028-tipo_lock = 'R'.
*      IF zwm028-paletes_pulmao < zwm028-total_paletes.
*
*        CLEAR: lt_zwm028.
*        REFRESH: lt_zwm028.
*
*        SELECT * INTO TABLE lt_zwm028
*            FROM zwm028
*                WHERE lgnum = xuser-lgnum
*                  AND refnr = grupo
*                  AND remessa <> ' '.
*
*        CLEAR next_desbloq.
*        LOOP AT lt_zwm028 WHERE zlock <> '1'.
*          IF lt_zwm028-paletes_pulmao < lt_zwm028-total_paletes.
*            CLEAR next_desbloq.
*            EXIT.
*          ELSE.
*            next_desbloq = 'X'.
*          ENDIF.
*        ENDLOOP.
*
***  As remessas desbloqueadas já estão todas finalizadas,
***  vamos encontrar a proxima a desbloquear
*        IF NOT next_desbloq IS INITIAL.
*          DELETE lt_zwm028 WHERE zlock <> '1'.
*
*          IF NOT lt_zwm028[] IS INITIAL.
*
*            CLEAR lt_vbpa.
*            REFRESH lt_vbpa.
*            SELECT * INTO TABLE lt_vbpa
*                FROM vbpa
*                    FOR ALL ENTRIES IN lt_zwm028
*                        WHERE vbeln EQ lt_zwm028-remessa
*                          AND posnr = '000000'
*                          AND parvw EQ 'W1'.
*
**& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:01
*            IF lv_glock_active EQ abap_false.
**& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:02
*              SORT lt_zwm028 BY ordem.
**& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23
*            ELSE.
*              SORT lt_zwm028[] BY ordem DESCENDING.
*            ENDIF.
**& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23
*
*            CLEAR lt_zwm028.
*            READ TABLE lt_zwm028 INDEX 1.
*
*            CLEAR l_kunnr.
*            LOOP AT lt_vbpa WHERE vbeln = lt_zwm028-remessa.
*              l_kunnr = lt_vbpa-kunnr.
*              EXIT.
*            ENDLOOP.
*
*            LOOP AT lt_vbpa WHERE kunnr = l_kunnr.
*              UPDATE zwm028 SET zlock = zwm028-zlock
*                            WHERE lgnum = zwm028-lgnum
*                              AND refnr = zwm028-refnr
*                              AND remessa = lt_vbpa-vbeln.
*              COMMIT WORK AND WAIT.
*
*              CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
*                EXPORTING
*                  i_lgnum = lt_zwm028-lgnum
*                  i_refnr = lt_zwm028-refnr
*                  i_vbeln = lt_zwm028-remessa.
*            ENDLOOP.
*
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ELSEIF  zwm028-st_pul = 'PUL' AND zwm028-tipo_lock = 'R'.
*      IF zwm028-paletes_pulmao < zwm028-total_paletes.
*
*        CLEAR: lt_zwm028.
*        REFRESH: lt_zwm028.
*
*        SELECT * INTO TABLE lt_zwm028
*            FROM zwm028
*                WHERE lgnum = xuser-lgnum
*                  AND refnr = grupo
*                  AND remessa <> ' '.
*
*        CLEAR next_desbloq.
*        LOOP AT lt_zwm028 WHERE zlock <> '1'.
*          IF lt_zwm028-paletes_pulmao < lt_zwm028-total_paletes.
*            CLEAR next_desbloq.
*            EXIT.
*          ELSE.
*            next_desbloq = 'X'.
*          ENDIF.
*        ENDLOOP.
*
***  As remessas desbloqueadas já estão todas finalizadas,
***  vamos encontrar a proxima a desbloquear
*        IF NOT next_desbloq IS INITIAL.
*          DELETE lt_zwm028 WHERE zlock <> '1'.
*
*          IF NOT lt_zwm028[] IS INITIAL.
*
**& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:01
*            IF lv_glock_active EQ abap_false.
**& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:02
*              SORT lt_zwm028 BY ordem.
**& Begin of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23
*            ELSE.
*              SORT lt_zwm028[] BY ordem DESCENDING.
*            ENDIF.
**& End of Modification by Tiago Pateiro - ROFF @ 22.01.2016 14:44:23
*
*            LOOP AT lt_zwm028.
**            CLEAR lt_zwm028.
**            READ TABLE lt_zwm028 INDEX 1.
*
*              IF lt_zwm028-paletes_pulmao < lt_zwm028-total_paletes.
*                UPDATE zwm028 SET zlock = zwm028-zlock
*                              WHERE lgnum = zwm028-lgnum
*                                AND refnr = zwm028-refnr
*                                AND remessa = lt_zwm028-remessa.
*                COMMIT WORK AND WAIT.
*
*                CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
*                  EXPORTING
*                    i_lgnum = lt_zwm028-lgnum
*                    i_refnr = lt_zwm028-refnr
*                    i_vbeln = lt_zwm028-remessa.
*                EXIT.
*              ENDIF.
*            ENDLOOP.
*          ENDIF.
*        ENDIF.
*
**        CLEAR zwm028.
**        SELECT SINGLE * FROM zwm028
**            WHERE lgnum = xuser-lgnum AND
**                  refnr = grupo AND
**                  remessa = remessa.
**        IF zwm028-paletes_pulmao >= zwm028-total_paletes.
**
**          CLEAR: l_ordem, l_lock.
**          l_lock = zwm028-zlock.
**          l_ordem = zwm028-ordem + 1.
**          SELECT SINGLE * FROM zwm028
**              WHERE lgnum = xuser-lgnum
**                AND refnr = grupo
**                AND ordem = l_ordem.
**          IF sy-subrc = 0.
**            UPDATE zwm028 SET zlock = l_lock
**                          WHERE lgnum = zwm028-lgnum
**                            AND refnr = zwm028-refnr
**                            AND remessa = zwm028-remessa.
**            COMMIT WORK AND WAIT.
**          ENDIF.
**
**        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*** Actualizar a posicao no pulmao onde se encontra a palete
*  IF ( bin_output_d(3) = 'PUL' OR bin_output_d(3) = 'PPK' OR
*       bin_output_d(3) = 'PLM' OR bin_output_d(3) = 'PLT') AND
*      NOT su IS INITIAL.
*    DATA: x_zwm013 LIKE zwm013 OCCURS 0 WITH HEADER LINE.
*    CLEAR x_zwm013.
*    REFRESH x_zwm013.
*    x_zwm013-armazem = xuser-lgnum.
*    x_zwm013-sscc = su.
*    x_zwm013-destino = bin_output_d.
*    x_zwm013-bloqueado = 'X'.
*    x_zwm013-tipo_palete = tipo_palete.
*    x_zwm013-posicao_pulmao = posicao_pulmao2.
*    CLEAR x_zwm013-fabrica_1.
*    CLEAR x_zwm013-trans_terc.
*    APPEND x_zwm013.
*    CLEAR x_zwm013.
*    IF NOT to3 IS INITIAL AND to_remontada IS INITIAL.
*** as duas paletes vao para o carro
*      x_zwm013-armazem = xuser-lgnum.
*      x_zwm013-sscc = zwm020-p2.
*      x_zwm013-destino = bin_output_d.
*      x_zwm013-bloqueado = 'X'.
*      x_zwm013-tipo_palete = tipo_palete.
*      x_zwm013-posicao_pulmao = posicao_pulmao2.
*      CLEAR x_zwm013-fabrica_1.
*      CLEAR x_zwm013-trans_terc.
*      APPEND x_zwm013.
*      CLEAR x_zwm013.
*    ELSE.
*** uma palete é devolvida para o PRM
*      DELETE FROM zwm020
*          WHERE armazem = xuser-lgnum AND
*                ( p1 = su OR p2 = zwm020-p2 ).
*      COMMIT WORK.
*    ENDIF.
*    MODIFY zwm013 FROM TABLE x_zwm013.
*    COMMIT WORK.
*  ENDIF.
*
*** Update de Paletes de Picking 2 Passos Parcial
************************************************************************
*  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
*    EXPORTING
*      i_lgnum  = xuser-lgnum
*      i_refnr  = grupo
*    IMPORTING
*      e_2spart = lv_2spart
*    EXCEPTIONS
*      error    = 1
*      OTHERS   = 2.
*
*  IF lv_2spart EQ abap_true.
*    SELECT * FROM zwm028
*             INTO TABLE lt_zwm028
*             WHERE lgnum = xuser-lgnum AND
*                   refnr = grupo.
*
*    SORT lt_zwm028 BY zlock DESCENDING.
*    READ TABLE lt_zwm028
*          INTO ls_zwm028
*          INDEX 1.
*
*    DELETE lt_zwm028 WHERE zlock <> ls_zwm028-zlock.
*
*    SORT lt_zwm028 BY ordem DESCENDING.
*
*    CLEAR: ls_zwm028.
*    READ TABLE lt_zwm028
*          INTO ls_zwm028
*          INDEX 1.
*    GET TIME.
*    CLEAR: ls_zwm066.
*    ls_zwm066-lgnum = xuser-lgnum.
*    ls_zwm066-refnr = grupo.
*    ls_zwm066-vbeln = ls_zwm028-remessa.
*    ls_zwm066-lenum = su.
*    ls_zwm066-erdat = sy-datum.
*    ls_zwm066-erzet = sy-uzeit.
*    ls_zwm066-ernam = sy-uname.
**    INSERT zwm066 FROM ls_zwm066.
**    COMMIT WORK.
*  ENDIF.
*
*
*
** Unlock ao lock efectuado na atribuicao da posicao do pulmao
*  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
*    EXPORTING
*      mode_keyword   = 'X'
*      keyword_       = 'ZWM028'
*    EXCEPTIONS
*      foreign_lock   = 1
*      system_failure = 2
*      OTHERS         = 3.

  CLEAR : lock_key, wait_time,cursorfield.

** FL -> 28/03/2007
** Libertar do bloqueio existente entre a introdução da posição (PULL)
** e a conclusão da tarefa (para não atribuição da mesma posição do
** pulmão a 2 users diferentes)
  CONCATENATE 'ZWM028_' remessa INTO chave_bloq.
  CONDENSE chave_bloq NO-GAPS.
  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = chave_bloq
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
** FL <- 28/03/2007

ENDFORM.                    " actualiza_paletes_pulmao
