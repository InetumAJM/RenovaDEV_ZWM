*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I16 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  exit25  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit25 INPUT.
  f3_activo = 'X'.
  SET SCREEN '0001'.
  LEAVE SCREEN.
ENDMODULE.                 " exit25  INPUT
*&---------------------------------------------------------------------*
*&      Module  checksu  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE checksu INPUT.

  CHECK NOT su IS INITIAL.

  IF bin_output_o(3) = 'PLT'.

    LOOP AT l_ltap WHERE vlenr = su AND pulmao = bin_output_o.
    ENDLOOP.
    IF sy-subrc <> 0.
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
      CLEAR: descricao,matnr.
      IF l_ltap-vltyp = 'PCK' OR l_ltap-vltyp = 'PKB'.
        descricao = 'Pal. Multireferencia'.
      ELSE.
        SELECT SINGLE maktx INTO descricao
            FROM makt
                WHERE matnr = l_ltap-matnr AND
                      spras = sy-langu.
        matnr = l_ltap-matnr.
      ENDIF.

      su1 = su.
      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.

  ELSE.
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
      MOVE 'BIN_INPUT' TO cursorfield.
    ENDIF.
  ENDIF.

ENDMODULE.                 " checksu  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0025  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0025 INPUT.

  DATA: conf_t(1).
  DATA: lv_queue_sai_aut TYPE lrf_queue.
  DATA: lv_lgpla     TYPE lgpla.
  DATA: lv_dummy     TYPE char10.
  DATA: lv_lznum     TYPE lznum.
  DATA: ls_lagp_pua  TYPE lagp.
  DATA: ls_zwm013    TYPE zwm013.
  DATA: ls_zwm081    TYPE zwm081.
  DATA: lt_ltap_conf TYPE TABLE OF ltap_conf WITH HEADER LINE.

** Confirmação
**********************************************************************
  CLEAR: i_sscc, conf_t, c_sscc,flag.
  REFRESH: i_sscc, c_sscc.
  CLEAR: mov, plant, lgort, valor.

  CHECK NOT su IS INITIAL AND
        NOT bin_input IS INITIAL.

** Validar Palete em porta com portico
  PERFORM check_pal_porta_portico CHANGING sy-subrc.
  IF sy-subrc <> 0.
    CLEAR: bin_input, cursorfield.
    MOVE 'BIN_INPUT' TO cursorfield.
    EXIT.
  ENDIF.

** Fila saída do automático torres novas (WCS)
  PERFORM get_parameter
      USING xuser-lgnum
            'GESTAO_FILAS'
            'FILA_SAIDA_AUT'
            lv_queue_sai_aut.

  " Carga na Gravitica
  lv_lgpla = bin_output_o+4(10).

  SELECT SINGLE *
    FROM lagp INTO ls_lagp_pua
    WHERE lgnum = xuser-lgnum
    AND   lgtyp = 'PUA'
    AND   lgpla = lv_lgpla.

  LOOP AT l_ltap WHERE vlenr = su.

    REFRESH return_msg.
    CLEAR return_msg.

    CLEAR ltak.
    SELECT SINGLE *
        FROM ltak
            WHERE lgnum = l_ltap-lgnum AND
                  tanum = l_ltap-tanum.

    IF ltak-betyp = 'H'.
      conf_t = 'B'.
    ELSE.
      conf_t = 'T'.
    ENDIF.

    CLEAR flag.
    WHILE flag IS INITIAL.

      REFRESH return_msg.

      " Validar se é saida do automático Torres Novas (WCS)
      IF ltak-queue = lv_queue_sai_aut.

        REFRESH lt_ltap_conf.
        CLEAR lt_ltap_conf.
        lt_ltap_conf-tanum = l_ltap-tanum.
        lt_ltap_conf-tapos = l_ltap-tapos.
        lt_ltap_conf-altme = l_ltap-meins.
        lt_ltap_conf-nista = l_ltap-vsolm.
        APPEND lt_ltap_conf.

        CALL FUNCTION 'L_TO_CONFIRM'
          EXPORTING
            i_lgnum       = l_ltap-lgnum
            i_tanum       = l_ltap-tanum
            i_quknz       = '2'
            i_subst       = 'X'
          TABLES
            t_ltap_conf   = lt_ltap_conf
          EXCEPTIONS
            error_message = 99.

        IF sy-subrc <> 0.
          CLEAR return_msg.
          return_msg-msgid  = sy-msgid.
          return_msg-msgtyp = 'E'.
          return_msg-msgnr  = sy-msgno.
          return_msg-msgv1  = sy-msgv1.
          return_msg-msgv2  = sy-msgv2.
          return_msg-msgv3  = sy-msgv3.
          APPEND return_msg.
        ENDIF.

      ELSE.
        CALL FUNCTION 'ZWM_CONFIRM_TO'
          EXPORTING
            armazem              = l_ltap-lgnum
            confirm_type         = conf_t
          TABLES
            return_msg           = return_msg
          CHANGING
            to                   = l_ltap-tanum
            to_item              = l_ltap-tapos
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
      ENDIF.

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

          CLEAR: bin_input, cursorfield.
          MOVE 'BIN_INPUT' TO cursorfield.
          RETURN.
*          IF lrf_wkqu-devty(5) = '16X20'.
*            SET SCREEN '0025'.LEAVE SCREEN.
*          ELSE.
*            SET SCREEN '0026'.LEAVE SCREEN.
*          ENDIF.
        ENDIF.

      ELSE.

        DO 10 TIMES .
          CLEAR ltap.
          SELECT SINGLE * FROM ltap
                WHERE lgnum = l_ltap-lgnum AND
                      tanum = l_ltap-tanum AND
                      tapos = l_ltap-tapos AND
                      pquit = 'X'.

          IF sy-subrc = 0.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDDO.

        flag = 'X'.
      ENDIF.
    ENDWHILE.

    IF l_ltap-nltyp <> '932'.  " Palete picking saida automático
      i_sscc-material      = l_ltap-matnr.
      i_sscc-quantidade    = l_ltap-vsola.
      i_sscc-uni           = l_ltap-altme.
      i_sscc-lote_producao = l_ltap-charg.
      APPEND i_sscc.
      CLEAR i_sscc.
    ENDIF.

** Verificar se é remontada
    CLEAR: pal_remontada, zwm020.
    SELECT SINGLE *
        FROM zwm020
            WHERE armazem = l_ltap-lgnum AND
                  ( p1 = l_ltap-vlenr OR
                    p2 = l_ltap-vlenr ).
    IF sy-subrc = 0.
      SORT l_ltap BY vlenr.
      IF su = zwm020-p1.
        READ TABLE l_ltap WITH KEY vlenr = zwm020-p2.
      ELSEIF su = zwm020-p2.
        READ TABLE l_ltap WITH KEY vlenr = zwm020-p1.
      ENDIF.
      IF sy-subrc = 0.

        pal_remontada = l_ltap-vlenr.

        CLEAR flag.

        CLEAR: return_msg.
        REFRESH: return_msg.

        WHILE flag IS INITIAL.

          IF ltak-queue = lv_queue_sai_aut.

            REFRESH lt_ltap_conf.
            CLEAR lt_ltap_conf.
            lt_ltap_conf-tanum = l_ltap-tanum.
            lt_ltap_conf-tapos = l_ltap-tapos.
            lt_ltap_conf-altme = l_ltap-meins.
            lt_ltap_conf-nista = l_ltap-vsolm.
            APPEND lt_ltap_conf.

            CALL FUNCTION 'L_TO_CONFIRM'
              EXPORTING
                i_lgnum       = l_ltap-lgnum
                i_tanum       = l_ltap-tanum
                i_quknz       = '2'
                i_subst       = 'X'
              TABLES
                t_ltap_conf   = lt_ltap_conf
              EXCEPTIONS
                error_message = 99.

            IF sy-subrc <> 0.
              CLEAR return_msg.
              return_msg-msgid  = sy-msgid.
              return_msg-msgtyp = 'E'.
              return_msg-msgnr  = sy-msgno.
              return_msg-msgv1  = sy-msgv1.
              return_msg-msgv2  = sy-msgv2.
              return_msg-msgv3  = sy-msgv3.
              APPEND return_msg.
            ENDIF.

          ELSE.
            CALL FUNCTION 'ZWM_CONFIRM_TO'
              EXPORTING
                armazem              = l_ltap-lgnum
                confirm_type         = 'T'
              TABLES
                return_msg           = return_msg
              CHANGING
                to                   = l_ltap-tanum
                to_item              = l_ltap-tapos
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
          ENDIF.

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
            ENDIF.

          ELSE.

            DO 10 TIMES .
              CLEAR ltap.
              SELECT SINGLE * FROM ltap
                    WHERE lgnum = l_ltap-lgnum AND
                          tanum = l_ltap-tanum AND
                          tapos = l_ltap-tapos AND
                          pquit = 'X'.

              IF sy-subrc = 0.
                EXIT.
              ELSE.
                WAIT UP TO 1 SECONDS.
              ENDIF.
            ENDDO.

            flag = 'X'.
          ENDIF.

        ENDWHILE.

      ENDIF.
    ENDIF.

  ENDLOOP.

** Carga Gravítica
  IF flag = 'X'.

    IF ls_lagp_pua IS NOT INITIAL.
      lv_lznum = su.

      CONCATENATE bin_output_o+4(10) '.' posicao_pulmao INTO lv_lgpla.

      CALL FUNCTION 'ZWM_CREATE_TO_CHD'
        EXPORTING
          i_lgnum    = l_ltap-lgnum
          i_lgpla    = lv_lgpla
          i_lznum    = lv_lznum
          i_type_mov = 'G'
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.
    ENDIF.

    " Atualizar tabela de controlo de expedição do automatico
    GET TIME.

    UPDATE zwm078 SET carga = abap_true
                      cdatu = sy-datum
                      czeit = sy-uzeit
                      cname = sy-uname
                      WHERE  lgnum = l_ltap-lgnum
                      AND    tanum = l_ltap-tanum.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.

  ENDIF.

  SORT l_ltap BY vlenr.

  " Não apanhar OTs de palete de picking do automático e BPK
  LOOP AT l_ltap WHERE vlenr = su AND
                       nltyp <> '932'.
    EXIT.
  ENDLOOP.

*  READ TABLE l_ltap WITH KEY vlenr = su.
  CLEAR valor.
  PERFORM get_parameter
             USING xuser-lgnum
                   'GERAL'
                   'PLANT'
                   valor.
  MOVE valor TO plant.

  CLEAR valor.
  PERFORM get_parameter
             USING xuser-lgnum
                   'GERAL'
                   'LGORT'
                   valor.
  MOVE valor TO lgort.

  CLEAR mov.
  CASE l_ltap-vltyp.
    WHEN 'DRI' OR 'BLK'.
      CLEAR valor.
      PERFORM get_parameter
                 USING xuser-lgnum
                       'SAIDA_ARMAZEM_DRI'
                       'MOV_WM_INV'
                       valor.
      MOVE valor TO mov.
    WHEN 'TRI'.
      CLEAR valor.
      PERFORM get_parameter
                 USING xuser-lgnum
                       'SAIDA_ARMAZEM_TRI'
                       'MOV_WM_INV'
                       valor.
      MOVE valor TO mov.
    WHEN 'PCK' OR 'PKB'.
      CLEAR valor.
      IF st_ppk IS INITIAL.
        PERFORM get_parameter
                   USING xuser-lgnum
                         'SAIDA_ARMAZEM_PCK'
                         'MOV_WM_INV'
                         valor.
      ELSE.
        PERFORM get_parameter
               USING xuser-lgnum
                     'SAIDA_ARMAZEM_PPK'
                     'MOV_WM_INV'
                     valor.
      ENDIF.
      MOVE valor TO mov.
  ENDCASE.

** Neste caso não é necessário fazer OT inversa
  IF NOT mov IS INITIAL.

    CLEAR ltak.
    SELECT SINGLE *
        FROM ltak
            WHERE lgnum = xuser-lgnum AND
                  lznum = su AND
                  kquit = 'X'.

    CLEAR ltap.
    SELECT SINGLE *
        FROM ltap
            WHERE lgnum = xuser-lgnum AND
                  tanum = ltak-tanum.

    SORT i_sscc BY material lote_producao.
    LOOP AT i_sscc.
      COLLECT i_sscc INTO c_sscc.
    ENDLOOP.

    REFRESH i_sscc.
    CLEAR i_sscc.

    i_sscc[] = c_sscc[].

    CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse      = xuser-lgnum
        mov_type       = mov
        st_type_o      = ltap-nltyp
        bin_origem     = ltap-nlpla
        plant          = plant
        s_loc          = lgort
        sscc_adicional = l_ltap-vlenr
      TABLES
        return_msg     = return_msg
        sscc           = i_sscc
      EXCEPTIONS
        error          = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.

    ENDIF.
  ENDIF.

** Apagar Tabela ZWM013
  GET TIME.

  SELECT SINGLE *
    FROM zwm013 INTO ls_zwm013
    WHERE armazem = xuser-lgnum
    AND   sscc    = l_ltap-vlenr.

  IF sy-subrc = 0.
    MOVE-CORRESPONDING ls_zwm013 TO ls_zwm081.

    ls_zwm081-datum = sy-datum.
    ls_zwm081-uzeit = sy-uzeit.

    MODIFY zwm081 FROM ls_zwm081.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDIF.

  DELETE FROM zwm013
      WHERE armazem   = xuser-lgnum AND
            sscc = l_ltap-vlenr.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

*  ENDIF.

  IF NOT pal_remontada IS INITIAL.
** Apagar Tabela ZWM013

    SELECT SINGLE *
        FROM zwm013 INTO ls_zwm013
        WHERE armazem = xuser-lgnum
        AND   sscc    = pal_remontada.

    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_zwm013 TO ls_zwm081.

      ls_zwm081-datum = sy-datum.
      ls_zwm081-uzeit = sy-uzeit.

      MODIFY zwm081 FROM ls_zwm081.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.


    DELETE FROM zwm013
        WHERE armazem   = xuser-lgnum AND
              sscc = pal_remontada.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

** Apagar Tabela ZWM020
*    DELETE FROM zwm020
*        WHERE armazem = xuser-lgnum AND
*              p1 = l_ltap-vlenr AND
*              p2 = pal_remontada.
*    IF sy-subrc = 0.
*      COMMIT WORK.
*    ELSE.
*      ROLLBACK WORK.
*    ENDIF.
  ENDIF.

  PERFORM get_parameter USING xuser-lgnum
                              'GESTAO_FILAS'
                              'FILA_AUT_SD'
                              gv_queue_aut.



** Actualizar numero de paletes no carro
  IF ltak-betyp = ' ' OR
     ltak-queue = gv_queue_aut OR   " Aut. França
     ltak-queue = lv_queue_sai_aut. " Aut. Torres Novas

    UPDATE zwm028 SET paletes_carro = paletes_carro + 1
    WHERE lgnum = xuser-lgnum AND
          refnr = l_ltap-refnr AND
          remessa = ' '.

    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 21.09.2012 16:02:05
*  Motivo: Valida se todas as paletes estão no pulmão e se deve
*          fazer abastecimento
*--------------------------------------------------------------------*
    CALL FUNCTION 'ZWM_AUTO_TO_DELEVERY_ABAST'
      EXPORTING
        i_lgnum = xuser-lgnum
        i_refnr = l_ltap-refnr.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

  ELSE.

    CLEAR ltap.
    SELECT SINGLE * FROM ltap
            WHERE lgnum = xuser-lgnum
              AND vlenr = su
              AND nltyp IN ( '916', '815' ).

    IF ltap-vltyp = 'PRM' OR ltap-vltyp = 'CRD'.
      UPDATE zwm028 SET paletes_carro = paletes_carro + 1
      WHERE lgnum = xuser-lgnum AND
            refnr = l_ltap-refnr AND
            remessa = ' '.

      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 21.09.2012 16:02:05
*  Motivo: Valida se todas as paletes estão no pulmão e se deve
*          fazer abastecimento
*--------------------------------------------------------------------*
      CALL FUNCTION 'ZWM_AUTO_TO_DELEVERY_ABAST'
        EXPORTING
          i_lgnum = xuser-lgnum
          i_refnr = l_ltap-refnr.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
    ENDIF.
  ENDIF.

  CLEAR: bin_output_o, posicao_pulmao, su1, su, descricao,
         bin_output_d, bin_input,ok_code_0025, cursorfield,
         bin_output_kober_d, kober, matnr.
  MOVE 'SU' TO cursorfield.
  IF lrf_wkqu-devty(5) = '16X20'.
    SET SCREEN '0025'.LEAVE SCREEN.
  ELSE.
    SET SCREEN '0026'.LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " user_command_0025  INPUT
*&---------------------------------------------------------------------*
*&      Module  checkbin  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE checkbin INPUT.
*  CLEAR : lgtyp,lgpla.

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
        MOVE 'BIN_INPUT' TO cursorfield.
        REFRESH lt_messages.
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
*          armazem    = xuser-lgnum
*          incidencia = '1'
*          posicao    = bin_input
*          sscc       = su
*        EXCEPTIONS
*          no_commit  = 1
*          OTHERS     = 2.
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


ENDMODULE.                 " checkbin  INPUT

*&--------------------------------------------------------------------*
*&      Form  registar_saida_paletes
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM registar_saida_paletes.

  DATA t_vbss LIKE vbss OCCURS 0 WITH HEADER LINE.
  DATA t_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF t_mat OCCURS 0,
          vbeln LIKE lips-vbeln,
          matnr LIKE lips-matnr,
          lfimg LIKE lips-lfimg,
          meins LIKE lips-meins,
        END OF t_mat.

  CLEAR: t_vbss, t_mat.
  REFRESH: t_vbss, t_mat.

  SELECT * INTO TABLE t_vbss
      FROM vbss
          WHERE sammg = zwm028-refnr.


  LOOP AT t_vbss.
    CLEAR lips.
    SELECT *
        FROM lips
            WHERE vbeln = t_vbss-vbeln AND
                  pstyv = 'ZPAL'.

      MOVE-CORRESPONDING lips TO t_mat.
      COLLECT t_mat.
      CLEAR t_mat.
    ENDSELECT.

  ENDLOOP.

  LOOP AT t_mat.

    CLEAR t_sscc.
    REFRESH t_sscc.

    t_sscc-material = t_mat-matnr.
    t_sscc-quantidade = t_mat-lfimg.
    t_sscc-uni = t_mat-meins.
    APPEND t_sscc.

    DATA bin_origem LIKE lqua-lgpla.
    CLEAR bin_origem.
    bin_origem = t_mat-matnr.

    CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse  = xuser-lgnum
        mov_type   = '989'
*       ST_TYPE_O  =
        bin_origem = bin_origem
*       ST_TYPE_D  =
*       BIN_DESTINO          =
*       STOCK_CAT  =
        plant      = 'RENV'
        s_loc      = 'CD'
*       CERTIFICADO          =
*       ORIGEM     =
        req_number = t_mat-vbeln
        req_type   = 'L'
*       SSCC_ADICIONAL       =
      IMPORTING
        to         = to
      TABLES
        return_msg = return_msg
        sscc       = t_sscc
* EXCEPTIONS
*       ERROR      = 1
*       OTHERS     = 2
      .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


  ENDLOOP.

ENDFORM.                    "registar_saida_paletes
