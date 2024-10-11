************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0028                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Reabastecimento da Reserva de Picking - RF               *
* Criado por: Sérgio Garcias                                           *
* Criado em.: 16/01/2006                                               *
* Tipo PRG..: Report                                                   *
************************************************************************
REPORT  zwmrep0067 MESSAGE-ID zwmmsg001.

INCLUDE: rlmobinc.

TABLES: mlgt,
        lqua,
        zwm001,
        zwm020,
        mard.

DATA: gt_mlgt      LIKE mlgt OCCURS 0 WITH HEADER LINE,
      gt_lqua      LIKE lqua OCCURS 0 WITH HEADER LINE,
      gt_ltap      LIKE ltap OCCURS 0 WITH HEADER LINE,
      t_sscc       LIKE zwm_sscc OCCURS 0 WITH HEADER LINE,
      t_ltap_cancl LIKE ltap_cancl OCCURS 0 WITH HEADER LINE.

DATA: gt_return_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA: gt_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

DATA lt_mlgt LIKE mlgt OCCURS 0 WITH HEADER LINE.

DATA: g_werks        LIKE ltap-werks,
      g_lgort        LIKE ltap-lgort,
      g_tanum        LIKE ltak-tanum,
      g_lgtyp1       LIKE mlgt-lgtyp,
      g_lgtyp2       LIKE lqua-lgtyp,
      g_bwlvs        LIKE ltak-bwlvs,
      num_pal_reab_v TYPE i,
      num_pal_reab_f TYPE i.

DATA: g_num   LIKE sy-tabix,
      g_lines LIKE sy-tabix,
      n_pal   TYPE i,
      to      LIKE ltap-tanum.

DATA n_posi TYPE i.

DATA: ok_code_0001 LIKE sy-ucomm.

DATA: text TYPE  bdcmsgcoll-msgv1.

DATA: cursorfield(20),
      return_msg TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA: in_bin(14),
      out_matnr     LIKE mara-matnr,
      out_desc1(20),
      out_desc2(20).

DATA: gv_lgtyp LIKE mlgt-lgtyp,
      gv_lgpla LIKE mlgt-lgpla.

START-OF-SELECTION.
  message_lang = sy-langu.
  PERFORM user_own_data.

  PERFORM ler_parametros.

  CALL SCREEN '0001'.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

  IF xuser-lgnum IS INITIAL.
    PERFORM user_own_data.
  ENDIF.

ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CLEAR: in_bin, out_desc1, out_desc2,
         out_matnr, ok_code_0001.

  SET SCREEN '0000'.
  LEAVE SCREEN.
ENDMODULE.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_in_bin  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_in_bin INPUT.

  DATA: lt_messages TYPE tab_bdcmsgcoll.

**********************************************************************

  CHECK NOT in_bin IS INITIAL.

  CLEAR: gv_lgtyp, gv_lgpla, g_werks, g_lgort.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = xuser-lgnum
      iv_bin_code = in_bin
    IMPORTING
      ev_bin      = in_bin
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
      CLEAR in_bin.
      MOVE 'IN_BIN' TO cursorfield.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ZWM_SPLIT_BIN'
    EXPORTING
      bin   = in_bin
    IMPORTING
      lgtyp = gv_lgtyp
      lgpla = gv_lgpla.

  CLEAR: n_posi, lt_mlgt.
  REFRESH lt_mlgt.
  SELECT * INTO TABLE lt_mlgt
      FROM mlgt
          WHERE lgnum = xuser-lgnum
            AND lgtyp = gv_lgtyp
            AND lgpla = gv_lgpla.
  IF sy-subrc <> 0.
    CLEAR text.
    WRITE in_bin TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '223'
        message_var1   = text.

    CLEAR: ok_code_0001, in_bin, out_matnr,
           out_desc1, out_desc2.
    MOVE 'IN_BIN' TO cursorfield.
    EXIT.
  ENDIF.

  DESCRIBE TABLE lt_mlgt LINES n_posi.

  IF n_posi > 1.
*  Posição & tem mais que um material associado!
    CLEAR text.
    WRITE in_bin TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '238'
        message_var1   = text.

    CLEAR: ok_code_0001, in_bin, out_matnr,
           out_desc1, out_desc2.
    MOVE 'IN_BIN' TO cursorfield.
    EXIT.
  ENDIF.

  READ TABLE lt_mlgt INDEX 1.
  out_matnr = lt_mlgt-matnr.

  SELECT SINGLE *
      FROM makt
          WHERE matnr = lt_mlgt-matnr AND
                spras = sy-langu.

  out_desc1 = makt-maktx(20).
  out_desc2 = makt-maktx+20(20).

** Determna Centro e Deposito
***********************************************************************
  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user      = sy-uname
      i_matnr     = lt_mlgt-matnr
      i_recall    = 'X'
      i_usewm     = 'X'
      i_userf     = 'X'
      i_usemm     = 'X'
      i_useaut    = 'X'
      i_get_lgnum = 'X'
      i_get_werks = 'X'
      i_get_lgort = 'X'
      i_sel_werks = 'X'
      i_sel_lgort = 'X'
    CHANGING
      c_lgnum     = xuser-lgnum
      c_werks     = g_werks
      c_lgort     = g_lgort
    EXCEPTIONS
      error       = 1
      user_back   = 2
      OTHERS      = 3.

ENDMODULE.                 " check_in_bin  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CHECK NOT in_bin IS INITIAL.

  CASE ok_code_0001.

    WHEN 'CLEAR'.

      CLEAR: ok_code_0001, in_bin, out_matnr,
             out_desc1, out_desc2.
      MOVE 'IN_BIN' TO cursorfield.

    WHEN 'SAVE'.

      CLEAR n_posi.
      SELECT COUNT(*) FROM lagp INTO n_posi
          WHERE lgnum = xuser-lgnum AND
                lgtyp = 'REP' AND
                kzler = 'X' AND
                kzvol = ' ' AND
                anzqu = 0   AND
                skzue = ' ' AND
                skzsi = ' ' AND
                skzse = ' '.

      IF n_posi <= 6.
        CLEAR text.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '206'
            message_var1   = text.

        CLEAR: ok_code_0001, in_bin, out_matnr,
               out_desc1, out_desc2.
        MOVE 'IN_BIN' TO cursorfield.
        EXIT.
      ENDIF.

      REFRESH: gt_lqua, gt_ltap.
      SELECT * FROM lqua INTO TABLE gt_lqua
              WHERE lgnum = xuser-lgnum
                AND lgtyp = g_lgtyp2
                AND matnr = out_matnr.

      DESCRIBE TABLE gt_lqua LINES g_lines.

      CLEAR mlgn.
      SELECT SINGLE *
          FROM mlgn
              WHERE matnr = out_matnr AND
                    lgnum = xuser-lgnum.

      IF z_wm_cl_management=>is_remontada( is_data = mlgn ) eq abap_true.
        num_pal_reab_f = num_pal_reab_f * 2.
      ENDIF.

*      CHECK g_lines < num_pal_reab_f.

      IF g_lines >= num_pal_reab_f.
*  Quantidade de reabastecimento do material & atingida!
        CLEAR text.
        WRITE out_matnr TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '228'
            message_var1   = text.

        CLEAR: ok_code_0001, in_bin, out_matnr,
               out_desc1, out_desc2.
        MOVE 'IN_BIN' TO cursorfield.
        EXIT.
      ENDIF.

      SELECT * FROM ltap INTO TABLE gt_ltap
                       WHERE lgnum = xuser-lgnum
                         AND matnr = out_matnr
                         AND pquit = ' '.

      IF NOT gt_ltap[] IS INITIAL.
        LOOP AT gt_ltap.
          CLEAR ltak.
          SELECT SINGLE * FROM  ltak
                 WHERE lgnum = gt_ltap-lgnum
                   AND tanum = gt_ltap-tanum.

          IF sy-subrc = 0 AND ltak-bwlvs = g_bwlvs
                          AND ltak-kquit = ' '.
            g_lines = g_lines + 1.
          ENDIF.
        ENDLOOP.
      ENDIF.

*      CHECK g_lines < num_pal_reab_f.
      IF g_lines >= num_pal_reab_f.
*  Quantidade de reabastecimento do material & atingida!
        CLEAR text.
        WRITE out_matnr TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '228'
            message_var1   = text.

        CLEAR: ok_code_0001, in_bin, out_matnr,
               out_desc1, out_desc2.
        MOVE 'IN_BIN' TO cursorfield.
        EXIT.
      ENDIF.

      g_num = num_pal_reab_f - g_lines.

      SELECT SINGLE *
             FROM mard
                 WHERE matnr = out_matnr AND
                       werks = g_werks AND
                       lgort = g_lgort.

*      CHECK NOT mard-labst IS INITIAL.
      IF mard-labst IS INITIAL.
*  Não existe stock em MM para o material & !
        CLEAR text.
        WRITE out_matnr TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '231'
            message_var1   = text.

        CLEAR: ok_code_0001, in_bin, out_matnr,
               out_desc1, out_desc2.
        MOVE 'IN_BIN' TO cursorfield.
        EXIT.
      ENDIF.

      DATA lt_lqua LIKE lqua OCCURS 0 WITH HEADER LINE.
      DATA num_quantos TYPE i.

      CLEAR num_quantos.
      SELECT COUNT(*) FROM lqua INTO num_quantos
              WHERE lgnum = xuser-lgnum AND
                    matnr = out_matnr AND
                    ( lgtyp = 'TRI' OR
                      lgtyp = 'DRI' OR lgtyp = 'BLK' ) AND
                      verme > 0.

*      CHECK g_num <= num_quantos.

      IF g_num > num_quantos.
*  Não existe stock em WM para o material & !
        CLEAR text.
        WRITE out_matnr TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '232'
            message_var1   = text.

        CLEAR: ok_code_0001, in_bin, out_matnr,
               out_desc1, out_desc2.
        MOVE 'IN_BIN' TO cursorfield.
        EXIT.
      ENDIF.

      DO g_num TIMES.

        REFRESH: gt_return_msg, gt_sscc.
        CLEAR: gt_return_msg, gt_sscc.
        CLEAR g_tanum.

        SELECT SINGLE * FROM mara WHERE matnr = out_matnr.
        CHECK sy-subrc = 0.

        gt_sscc-material   = out_matnr.
        gt_sscc-quantidade = 1.

        SELECT SINGLE *
            FROM mlgn
                WHERE lgnum = xuser-lgnum AND
                      matnr = out_matnr.

        gt_sscc-tipo_su = mlgn-lety1.

        IF z_wm_cl_management=>is_remontada( is_data = mlgn ) eq abap_true.
          gt_sscc-quantidade = 1.
          gt_sscc-uni        = 'PAL'.
          APPEND gt_sscc.
          gt_sscc-quantidade = 1.
          gt_sscc-uni        = 'PAL'.
        ELSE.
          gt_sscc-uni        = mara-meins.
        ENDIF.
        APPEND gt_sscc.

        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
          EXPORTING
            warehouse  = xuser-lgnum
            mov_type   = g_bwlvs                  "827
            plant      = g_werks                  "RENV
            s_loc      = g_lgort                  "CD
          IMPORTING
            to         = g_tanum
          TABLES
            return_msg = gt_return_msg
            sscc       = gt_sscc
          EXCEPTIONS
            error      = 1
            OTHERS     = 2.

        IF sy-subrc <> 0.
          READ TABLE gt_return_msg INDEX 1.
          IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = gt_return_msg-msgid
                message_lang   = sy-langu
                message_type   = gt_return_msg-msgtyp
                message_number = gt_return_msg-msgnr
                message_var1   = gt_return_msg-msgv1
                message_var2   = gt_return_msg-msgv2
                message_var3   = gt_return_msg-msgv3.

            CLEAR: in_bin, out_matnr, out_desc1,
                   out_desc2,ok_code_0001.
            MOVE 'IN_BIN' TO cursorfield.
            EXIT.
          ENDIF.
        ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 25.06.2012 12:32:09
*  Motivo: Prioridade de Reabastecimento
*--------------------------------------------------------------------*
        CALL FUNCTION 'ZWM_TO_SET_LET_DOWN_PRIORITY'
          EXPORTING
            i_lgnum  = xuser-lgnum
            i_tanum  = g_tanum
            i_commit = 'X'
          EXCEPTIONS
            error    = 1
            OTHERS   = 2.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


*** Verificar se é remontada
        DATA t_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.
        DATA c_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.
        CLEAR: t_ltap, c_ltap.
        REFRESH: t_ltap, c_ltap.

        SELECT * APPENDING TABLE t_ltap
            FROM ltap
                WHERE lgnum = xuser-lgnum AND
                      tanum = g_tanum.

        LOOP AT t_ltap
            WHERE vltyp = 'TRI' AND
                  ( letyp in z_wm_cl_management=>r_letyp_remontada( xuser-lgnum ) ) AND
                  vbeln = ' '.

          CLEAR zwm020.
          SELECT SINGLE * FROM zwm020
              WHERE armazem = t_ltap-lgnum AND
                    ( p1 = t_ltap-vlenr OR
                      p2 = t_ltap-vlenr ).

          IF sy-subrc = 0.
            CLEAR c_ltap.
            REFRESH c_ltap.
            MOVE-CORRESPONDING t_ltap TO c_ltap.
            APPEND c_ltap.
            CLEAR c_ltap.

            IF zwm020-p1 = t_ltap-vlenr.
** Verificar se existem duas to´s para as duas paletes remontadas

** Se tiverem as duas paletes tem de se estornar as duas tos e voltar a
** criar as duas to's sendo a primeira a da to de baixo

              READ TABLE t_ltap WITH KEY vlenr = zwm020-p2.
              IF sy-subrc = 0.
                MOVE-CORRESPONDING t_ltap TO c_ltap.
                APPEND c_ltap.
                CLEAR c_ltap.
              ENDIF.

            ELSEIF zwm020-p2 = t_ltap-vlenr.

              READ TABLE t_ltap WITH KEY vlenr = zwm020-p1.
              IF sy-subrc = 0.
                MOVE-CORRESPONDING t_ltap TO c_ltap.
                APPEND c_ltap.
                CLEAR c_ltap.
              ENDIF.
            ENDIF.
          ENDIF.

** Verificar se estao as duas paletes remontadas para a saida.
** Se for as duas paletes para saida verificar se ja estao criadas pela
** ordem correcta
          CLEAR n_pal.

          DESCRIBE TABLE c_ltap LINES n_pal.

          IF n_pal = 2.
            SORT c_ltap BY lgnum tanum tapos.
            READ TABLE c_ltap INDEX 1.
            IF c_ltap-vlenr = zwm020-p1.
              CONTINUE.
            ENDIF.
          ENDIF.

          CLEAR t_sscc.
          REFRESH t_sscc.
          LOOP AT c_ltap.

**************************
**   Cancelar as OT
**************************

            CLEAR t_ltap_cancl.
            REFRESH t_ltap_cancl.

            t_ltap_cancl-tanum = c_ltap-tanum.
            t_ltap_cancl-tapos = c_ltap-tapos.
            APPEND t_ltap_cancl.
            CLEAR t_ltap_cancl.

            CALL FUNCTION 'ZWM_CANCEL_TO'
              EXPORTING
                armazem      = c_ltap-lgnum
              TABLES
                t_ltap_cancl = t_ltap_cancl
              EXCEPTIONS
                error        = 1
                OTHERS       = 2.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

          ENDLOOP.

**************************
**   Tabela para Criar Novas OT´s
**************************
          READ TABLE c_ltap WITH KEY vlenr = zwm020-p1.
          IF sy-subrc = 0.
            t_sscc-sscc = c_ltap-vlenr.
            t_sscc-tipo_su = c_ltap-letyp.
            t_sscc-material = c_ltap-matnr.
            t_sscc-quantidade = c_ltap-vsola.
            t_sscc-uni = c_ltap-altme.
            t_sscc-lote_producao = c_ltap-charg.
            APPEND t_sscc.
            CLEAR t_sscc.
          ENDIF.

          CLEAR c_ltap.
          READ TABLE c_ltap WITH KEY vlenr = zwm020-p2.
          IF sy-subrc = 0.
            t_sscc-sscc = c_ltap-vlenr.
            t_sscc-tipo_su = c_ltap-letyp.
            t_sscc-material = c_ltap-matnr.
            t_sscc-quantidade = c_ltap-vsola.
            t_sscc-uni = c_ltap-altme.
            t_sscc-lote_producao = c_ltap-charg.
            APPEND t_sscc.
            CLEAR t_sscc.
          ENDIF.

          CLEAR gt_return_msg.
          REFRESH gt_return_msg.
          CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
            EXPORTING
              warehouse   = xuser-lgnum
              mov_type    = g_bwlvs         "827
              st_type_o   = c_ltap-vltyp
              bin_origem  = c_ltap-vlpla
              st_type_d   = c_ltap-nltyp
              bin_destino = c_ltap-nlpla
              plant       = c_ltap-werks
              s_loc       = c_ltap-lgort
              origem      = 'X'
            IMPORTING
              to          = to
            TABLES
              return_msg  = gt_return_msg
              sscc        = t_sscc
            EXCEPTIONS
              error       = 1
              OTHERS      = 2.
          IF sy-subrc <> 0.
            READ TABLE gt_return_msg INDEX 1.
            IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
                EXPORTING
                  message_id     = gt_return_msg-msgid
                  message_lang   = sy-langu
                  message_type   = gt_return_msg-msgtyp
                  message_number = gt_return_msg-msgnr
                  message_var1   = gt_return_msg-msgv1
                  message_var2   = gt_return_msg-msgv2
                  message_var3   = gt_return_msg-msgv3.

              CLEAR: in_bin, out_matnr,
                     out_desc1, out_desc2, ok_code_0001.
              MOVE 'IN_BIN' TO cursorfield.
              EXIT.
            ENDIF.
          ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 25.06.2012 12:32:09
*  Motivo: Prioridade de Reabastecimento
*--------------------------------------------------------------------*
          CALL FUNCTION 'ZWM_TO_SET_LET_DOWN_PRIORITY'
            EXPORTING
              i_lgnum  = xuser-lgnum
              i_tanum  = to
              i_commit = 'X'
            EXCEPTIONS
              error    = 1
              OTHERS   = 2.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

          CLEAR t_sscc.
          REFRESH t_sscc.

          DELETE t_ltap WHERE vlenr = zwm020-p1 OR vlenr = zwm020-p2.
          CLEAR c_ltap.
          REFRESH c_ltap.

        ENDLOOP.

      ENDDO.

      CLEAR: ok_code_0001, in_bin, out_matnr,
             out_desc1, out_desc2.
      MOVE 'IN_BIN' TO cursorfield.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&--------------------------------------------------------------------*
*&      Form  ler_parametros
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM ler_parametros.
  CLEAR: g_werks, g_lgort, g_lgtyp1, g_lgtyp2, g_bwlvs.

**  SELECT SINGLE * FROM  zwm001
**         WHERE armazem   = xuser-lgnum
**           AND processo  = 'GERAL'
**           AND parametro = 'PLANT'.
**  IF sy-subrc = 0.
**    g_werks = zwm001-valor.
**  ENDIF.
**  SELECT SINGLE * FROM  zwm001
**         WHERE armazem   = xuser-lgnum
**           AND processo  = 'GERAL'
**           AND parametro = 'LGORT'.
**  IF sy-subrc = 0.
**    g_lgort = zwm001-valor.
**  ENDIF.
  SELECT SINGLE * FROM  zwm001
         WHERE armazem   = xuser-lgnum
           AND processo  = 'REABASTECIMENTO'
           AND parametro = 'MOV'.
  IF sy-subrc = 0.
    g_bwlvs = zwm001-valor.
  ENDIF.
  SELECT SINGLE * FROM  zwm001
         WHERE armazem   = xuser-lgnum
           AND processo  = 'REABASTECIMENTO'
           AND parametro = 'ST_PCK'.
  IF sy-subrc = 0.
    g_lgtyp1 = zwm001-valor.
  ENDIF.
  SELECT SINGLE * FROM  zwm001
         WHERE armazem   = xuser-lgnum
           AND processo  = 'REABASTECIMENTO'
           AND parametro = 'ST_PKR'.
  IF sy-subrc = 0.
    g_lgtyp2 = zwm001-valor.
  ENDIF.

  SELECT SINGLE * FROM  zwm001
         WHERE armazem   = xuser-lgnum
           AND processo  = 'REABASTECIMENTO'
           AND parametro = 'PKB'.
  IF sy-subrc = 0.
    num_pal_reab_v = zwm001-valor.
  ENDIF.

  SELECT SINGLE * FROM  zwm001
         WHERE armazem   = xuser-lgnum
           AND processo  = 'REABASTECIMENTO'
           AND parametro = 'PKR'.
  IF sy-subrc = 0.
    num_pal_reab_f = zwm001-valor.
  ENDIF.

ENDFORM.                    " ler_parametros
