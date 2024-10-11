*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1O10 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  status_0025  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0025 OUTPUT.
  DATA: lv_2step TYPE flag,
        lv_tabix TYPE sytabix.

  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

  DATA: st_ppk LIKE zwm028-st_ppk,
        porta  LIKE zwm002-porta.

  IF xuser-lgnum IS INITIAL.
    PERFORM user_own_data.
  ENDIF.

  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs = xuser-lgnum.

** Quando um operario quer sair da pistola
  IF NOT f3_activo IS INITIAL.
    SET SCREEN '0000'.
    LEAVE SCREEN.
  ENDIF.

** apanhar todas as to´s de uma determinada carga para carregar o carro.

  CLEAR ltap.
  SELECT SINGLE *
      FROM ltap
          WHERE lgnum = xuser-lgnum AND
                tanum = to_ret AND
                tapos = item_ret.

  CONCATENATE '0' ltap-nlpla+8(2) INTO porta.

  CLEAR zwm002.
  SELECT SINGLE *
      FROM zwm002
          WHERE armazem = xuser-lgnum AND
          porta = porta.

  CLEAR zwm028.
  SELECT SINGLE refnr st_ppk st_pul pulmao1
      FROM zwm028 INTO (zwm028-refnr, st_ppk, zwm028-st_pul, zwm028-pulmao1)
          WHERE lgnum = xuser-lgnum AND
                remessa = ' ' AND
*                st_pul = 'PUL' AND
                pulmao1 = zwm002-pulmao_1 AND
                ot = ltap-tanum.

  CLEAR l_ltap.
  REFRESH l_ltap.

** Get To´s Pal Especial

  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_ltap
           BYPASSING BUFFER
               FROM ltak AS k INNER JOIN ltap AS p
               ON  k~lgnum = p~lgnum AND
                   k~tanum = p~tanum
                   WHERE k~lgnum = xuser-lgnum AND
                         k~kquit = ' ' AND
                         k~betyp = 'H' AND
                         k~benum = zwm028-refnr AND
                         p~pquit = ' ' AND
                         p~pvqui = ' ' AND
                         vorga <> 'ST'.

** Carga Gravítica
  IF zwm028-st_pul = 'PUA'.

    WHILE 1 > 0.

      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_ltap
             BYPASSING BUFFER
                 FROM ltak AS k INNER JOIN ltap AS p
                 ON  k~lgnum = p~lgnum AND
                     k~tanum = p~tanum
                     WHERE k~lgnum = xuser-lgnum AND
                           k~kquit = ' ' AND
                           k~refnr = zwm028-refnr AND
                           p~pquit = ' ' AND
                           vorga <> 'ST'.

      IF l_ltap[] IS NOT INITIAL.
        READ TABLE l_ltap WITH KEY pvqui = 'X'.
        IF sy-subrc = 0.
          DELETE l_ltap WHERE pvqui IS INITIAL.
          EXIT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.

      " Aguardar chegada de palete à posição & da gravitica.
      text = zwm028-pulmao1.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '369'
          message_var1   = text.

      REFRESH l_ltap.
    ENDWHILE.

** Carga Pulmão
  ELSE.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE l_ltap
             BYPASSING BUFFER
                 FROM ltak AS k INNER JOIN ltap AS p
                 ON  k~lgnum = p~lgnum AND
                     k~tanum = p~tanum
                     WHERE k~lgnum = xuser-lgnum AND
                           k~kquit = ' ' AND
                           k~refnr = zwm028-refnr AND
                           p~pquit = ' ' AND
                           p~pvqui = 'X' AND
                           vorga <> 'ST'.
  ENDIF.

  IF NOT l_ltap[] IS INITIAL.

    LOOP AT l_ltap.
      lv_tabix = sy-tabix.

      CLEAR ltak.
      SELECT SINGLE *
          FROM ltak
              WHERE lgnum = xuser-lgnum AND
                    tanum = l_ltap-tanum.
** Processo Standard

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 29.05.2012 14:15:34
*  Motivo: Picking em 2 Passos
*--------------------------------------------------------------------*
      CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
        EXPORTING
          i_lgnum = xuser-lgnum
          i_refnr = l_ltap-refnr
        IMPORTING
          e_2step = lv_2step
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF lv_2step EQ abap_true.

        CLEAR zwm028.
        SELECT SINGLE ordem kober INTO (l_ltap-pos_remessa,
                                       l_ltap-kober)
               FROM zwm028
                   WHERE lgnum = xuser-lgnum AND
                         refnr = l_ltap-refnr.

        IF l_ltap-vltyp = 'PCK' OR l_ltap-vltyp = 'PKB' OR l_ltap-vltyp = 'PKL'.
          SELECT SINGLE sscc FROM zwm026 INTO l_ltap-vlenr
                              WHERE armazem = xuser-lgnum AND
                                    grupo = l_ltap-refnr AND
                                    to_number = l_ltap-tanum.
        ENDIF.

        IF l_ltap-nltyp <> 'PPK'.
          CLEAR zwm013.
          SELECT SINGLE posicao_pulmao destino second_pulmao
              FROM zwm013 INTO (l_ltap-pos_pulmao, l_ltap-pulmao, l_ltap-second_pulmao)
                  WHERE armazem = l_ltap-lgnum AND
                        sscc = l_ltap-vlenr.

        ELSE.

          CLEAR ltak.
          SELECT SINGLE *
              FROM ltak
                  WHERE lgnum = xuser-lgnum AND
                        lznum = l_ltap-vlenr AND
                        kquit = ' '.

          CLEAR ltap.
          SELECT SINGLE nltyp nlpla INTO (ltap-nltyp, ltap-nlpla)
              FROM ltap
                  WHERE lgnum = ltak-lgnum AND
                        tanum = ltak-tanum.


          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = ltap-nltyp
              lgpla = ltap-nlpla
            IMPORTING
              bin   = l_ltap-pulmao.

          CLEAR l_ltap-pos_pulmao.
        ENDIF.

        MODIFY l_ltap INDEX lv_tabix.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
      ELSEIF ltak-betyp = 'L'.
        CLEAR zwm028.
        SELECT SINGLE ordem kober INTO (l_ltap-pos_remessa,
                                       l_ltap-kober)
               FROM zwm028
                   WHERE lgnum = xuser-lgnum AND
                         refnr = l_ltap-refnr AND
                         remessa = l_ltap-vbeln.

        IF sy-subrc <> 0.
          CLEAR zwm040.
          SELECT SINGLE *
              FROM zwm040
                  WHERE lgnum = xuser-lgnum AND
                        refnr = l_ltap-refnr AND
                        remessa = l_ltap-vbeln.
          IF sy-subrc = 0.
            SELECT SINGLE ordem kober INTO (l_ltap-pos_remessa, l_ltap-kober)
                FROM zwm028
                    WHERE lgnum = xuser-lgnum AND
                          refnr = l_ltap-refnr AND
                          remessa = zwm040-id_servisan.
          ENDIF.
        ENDIF.

        IF l_ltap-vltyp = 'PCK' OR l_ltap-vltyp = 'PKB' OR l_ltap-vltyp = 'PKL'.

          SELECT SINGLE sscc FROM zwm026 INTO l_ltap-vlenr
                              WHERE armazem = xuser-lgnum AND
                                    grupo = l_ltap-refnr AND
                                    remessa = l_ltap-vbeln AND
                                    to_number = l_ltap-tanum.
        ENDIF.

        IF l_ltap-nltyp <> 'PPK'.
          CLEAR zwm013.
          SELECT SINGLE posicao_pulmao destino second_pulmao
              FROM zwm013 INTO (l_ltap-pos_pulmao, l_ltap-pulmao, l_ltap-second_pulmao)
                  WHERE armazem = l_ltap-lgnum AND
                        sscc = l_ltap-vlenr.

        ELSE.

          CLEAR ltak.
          SELECT SINGLE *
              FROM ltak
                  WHERE lgnum = xuser-lgnum AND
                        lznum = l_ltap-vlenr AND
                        kquit = ' '.

          CLEAR ltap.
          SELECT SINGLE nltyp nlpla INTO (ltap-nltyp, ltap-nlpla)
              FROM ltap
                  WHERE lgnum = ltak-lgnum AND
                        tanum = ltak-tanum.


          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = ltap-nltyp
              lgpla = ltap-nlpla
            IMPORTING
              bin   = l_ltap-pulmao.

          CLEAR l_ltap-pos_pulmao.
        ENDIF.
        MODIFY l_ltap INDEX lv_tabix.
      ELSEIF ltak-betyp = 'H'.
** Paletização Especial.

        CLEAR ltak.
        SELECT SINGLE *
            FROM ltak
                WHERE lgnum = xuser-lgnum AND
                      tanum = l_ltap-tanum AND
                      kquit = ' '.

        CLEAR zwm013.
        SELECT SINGLE posicao_pulmao destino second_pulmao
            FROM zwm013 INTO (l_ltap-pos_pulmao, l_ltap-pulmao, l_ltap-second_pulmao)
                WHERE armazem = l_ltap-lgnum AND
                      sscc = ltak-lznum.

        SELECT SINGLE ordem INTO l_ltap-pos_remessa
               FROM zwm028
                   WHERE lgnum = xuser-lgnum AND
                         refnr = ltak-benum AND
                         remessa = l_ltap-vlpla.

        l_ltap-vlenr = ltak-lznum.
        l_ltap-vbeln = l_ltap-vlpla.
        l_ltap-refnr = ltak-benum.
        MODIFY l_ltap INDEX lv_tabix.

      ELSE.

        " Saída do Automático - Palete Picking
        IF l_ltap-nltyp = '932'.

          CLEAR zwm013.
          SELECT SINGLE posicao_pulmao destino second_pulmao
              FROM zwm013 INTO (l_ltap-pos_pulmao, l_ltap-pulmao, l_ltap-second_pulmao)
                  WHERE armazem = l_ltap-lgnum AND
                        sscc    = l_ltap-vlenr.

          MODIFY l_ltap INDEX lv_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 06.06.2012 10:32:59
*  Motivo: Ordena Para Meio Pulmão
*--------------------------------------------------------------------*
    CALL FUNCTION 'ZWM_MPUL_CAMIAO_SEQ_CARGA_CHAN'
      EXPORTING
        i_lgnum   = l_ltap-lgnum
        i_refnr   = l_ltap-refnr
      CHANGING
        ct_l_ltap = l_ltap[].
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

    READ TABLE l_ltap INDEX 1.
    SELECT SINGLE *
        FROM zwm020
            WHERE armazem = xuser-lgnum AND
                  ( p1 = l_ltap-vlenr OR
                    p2 = l_ltap-vlenr ).
    IF sy-subrc = 0.
      READ TABLE l_ltap WITH KEY vlenr = zwm020-p1.
    ENDIF.
***   mudar este sort SG
*    READ TABLE l_ltap INDEX 1.

    MOVE l_ltap-pulmao TO bin_output_o.
    WRITE l_ltap-pos_pulmao TO posicao_pulmao RIGHT-JUSTIFIED.
    WRITE l_ltap-kober TO kober LEFT-JUSTIFIED.
    CONDENSE kober.

    IF l_ltap-pulmao(3) = 'PLT'.
      IF su IS INITIAL.
        CLEAR su1.
      ENDIF.
    ELSE.
      MOVE l_ltap-vlenr TO su1.
    ENDIF.

    " Carga na Gravítica
    IF l_ltap-pulmao(3) = 'PUA'.
      CLEAR posicao_pulmao.

      SPLIT bin_output_o AT '.' INTO bin_output_o posicao_pulmao.
    ENDIF.

    CLEAR: descricao,matnr.
    IF l_ltap-vltyp = 'PCK' OR l_ltap-vltyp = 'PKB' OR l_ltap-vltyp = 'PKL'.
      descricao = 'Pal. Multireferencia'.
    ELSE.
      SELECT SINGLE maktx INTO descricao
          FROM makt
              WHERE matnr = l_ltap-matnr AND
                    spras = sy-langu.
      matnr = l_ltap-matnr.
    ENDIF.

    IF l_ltap-pulmao(3) = 'PLT'.
      IF su IS INITIAL.
        CLEAR: descricao, matnr.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = 'DCK'
        lgpla = ltap-vlpla
      IMPORTING
        bin   = bin_output_d.
  ELSE.

** Fazer o registo de saida das Paletes (material de embalgem )

*    PERFORM registar_saida_paletes.

** Confirmar a TO de Carga e voltar a chamar outra tarefa
    CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 06.06.2012 12:31:27
*  Motivo: Criação de OT's Para Remessa (PCK 2 Passos)
*--------------------------------------------------------------------*
    CALL FUNCTION 'ZWM_TO_DELEVERY_ABAST'
      EXPORTING
        i_lgnum  = xuser-lgnum
        i_refnr  = zwm028-refnr
        i_commit = abap_true
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

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

        SET SCREEN '0000'.LEAVE SCREEN.
        CLEAR : cursorfield.
      ENDIF.

    ELSE.
*PROCESSAMENTO OK
*actualizar tabela ZWM011 com STATUS T
      CALL FUNCTION 'ZWM_MODIFY_ZWM011'
        EXPORTING
          armazem            = xuser-lgnum
          to_number          = to_ret
          to_item            = '0001'
          status             = 'T'
        EXCEPTIONS
          error_update_table = 1
          OTHERS             = 2.

    ENDIF.
* chamar uma nova tarefa
    MOVE 'SU' TO cursorfield.

    CLEAR : tab_zwm011, return_msg.

    REFRESH : return_msg.
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

  ENDIF.
ENDMODULE.                 " status_0025  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0032  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0032 OUTPUT.
  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

ENDMODULE.                 " STATUS_0032  OUTPUT
