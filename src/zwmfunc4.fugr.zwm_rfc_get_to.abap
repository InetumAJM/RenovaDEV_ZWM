FUNCTION zwm_rfc_get_to.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_SSCC) LIKE  ZWM_AUX-SSCC
*"     VALUE(MESA) LIKE  ZWM_AUX-MESA
*"     VALUE(I_LINHA) TYPE  FEVOR OPTIONAL
*"  EXPORTING
*"     VALUE(E_SSCC) LIKE  ZWM_AUX-SSCC
*"     VALUE(E_TO) LIKE  LTAK-TANUM
*"     VALUE(RETORNO) LIKE  ZWM_AUX-RETORNO
*"     VALUE(MSG) TYPE  ZWM_MSG
*"----------------------------------------------------------------------

** Apagar, so para testes
*  e_sscc = i_sscc.
*  retorno = 00.

  DATA : mat_struct   LIKE zwm_material OCCURS 0 WITH HEADER LINE,
         x_sscc       LIKE zwm_sscc     OCCURS 0 WITH HEADER LINE,
         result_msg   LIKE bdcmsgcoll   OCCURS 0 WITH HEADER LINE,
         lt_ltap_conf TYPE ltap_conf    OCCURS 0 WITH HEADER LINE,
         lt_ltap      TYPE ltap         OCCURS 0 WITH HEADER LINE.

  DATA: wa_zwm013      LIKE zwm013,
        to             LIKE ltak-tanum,
        mov            LIKE ltak-bwlvs,
        mov_aut_man    LIKE ltak-bwlvs,
        st_type        TYPE lgtyp,
        dest_bin       TYPE lgpla,
        plant          LIKE mard-werks,
        lgort          LIKE mard-lgort,
        letyp          LIKE lein-letyp,
        su_invalida(1),
        lgtyp          LIKE lagp-lgtyp,
        lgpla          LIKE lagp-lgpla,
        certificado    LIKE ltap-zeugn,
        valor          LIKE zwm001-valor,
        l_to           LIKE ltap-tanum.

  DATA: w_subrc     LIKE sy-subrc,
        xuser-lgnum LIKE ltak-lgnum,
        l_sscc      TYPE lenum.

  FREE: itab_zwm001.
  CLEAR: itab_zwm001, retorno, wa_log, e_to, e_sscc, l_msg, g_mblnr,
         g_mjahr, x_mblnr, x_mjahr.

  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs       = '100'
    TABLES
      ti_zwm001 = itab_zwm001.

  xuser-lgnum = '100'.

  e_sscc = i_sscc.

** Dados table de LOG do interface
  GET TIME.
  wa_log-data     = sy-datum.
  wa_log-processo = 'GET_TO'.
  wa_log-hora     = sy-uzeit.
  wa_log-mesa     = mesa.


** Verifica se a Mesa é a correcta
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = e_sscc
    IMPORTING
      output = l_sscc.

  wa_log-sscc = l_sscc.

  IF mesa IS INITIAL.
    retorno = 41.
    wa_log-retorno = retorno.
    wa_log-msg = 'Mesa de Destino Vazia'.
    msg = wa_log-msg.
    MODIFY zwm_log_efacec FROM wa_log.
    EXIT.
  ENDIF.

  CLEAR: zwm013.
  SELECT SINGLE * FROM zwm013
  WHERE armazem EQ xuser-lgnum
    AND sscc    EQ l_sscc.

  IF sy-subrc NE 0.
** Não existe o SSCC
    retorno = 50.
    wa_log-retorno = retorno.
    wa_log-msg = 'SSCC não existe'.
    msg = wa_log-msg.
    MODIFY zwm_log_efacec FROM wa_log.
    EXIT.
  ELSE.
    wa_zwm013 = zwm013.
  ENDIF.

  IF zwm013-destino NE mesa.

** Elimina o registo da tabela zwm013
    DELETE zwm013 FROM zwm013.

** Têm que lêr sempre com a palete de baixo
    retorno = 40.
    wa_log-retorno = retorno.
    wa_log-msg = 'Mesa errada'.
    msg = wa_log-msg.
    MODIFY zwm_log_efacec FROM wa_log.
    EXIT.
  ENDIF.

  PERFORM get_zwm001 TABLES itab_zwm001
                     USING xuser-lgnum
                           'ENTRADA_ARMAZEM'
                           'MOV_WM'
                     CHANGING valor.
  MOVE valor TO mov.

  CLEAR valor.
  PERFORM get_zwm001 TABLES itab_zwm001
                       USING xuser-lgnum
                             'ENTRADA_ARMAZEM'
                             'MOV_WM_AUT_MAN'
                       CHANGING valor.
  MOVE valor TO mov_aut_man.

  CLEAR: lgtyp, lgpla.

  CALL FUNCTION 'ZWM_SPLIT_BIN'
    EXPORTING
      bin   = mesa
    IMPORTING
      lgtyp = lgtyp
      lgpla = lgpla.

  SELECT SINGLE lzone INTO certificado
    FROM lagp WHERE lgnum = xuser-lgnum AND
                    lgtyp = lgtyp AND
                    lgpla = lgpla.

* verificar se é palete remontada
  SELECT SINGLE *
      FROM zwm020
          WHERE armazem = xuser-lgnum AND
                p1 = i_sscc.

** Palete Remontada
**********************************************************************
  IF sy-subrc = 0.

    wa_log-pal_remontada = 'X'.

    SELECT SINGLE p~vemng p~vemeh p~matnr
                    p~werks p~lgort p~charg k~vhilm
          INTO (x_sscc-quantidade, x_sscc-uni, x_sscc-material,
                plant, lgort, x_sscc-lote_producao, x_sscc-vhilm)
             FROM vekp AS k INNER JOIN vepo AS p
                  ON  k~venum = p~venum
                      WHERE  p~vepos = '000001' AND
                             k~exidv = zwm020-p1.

    wa_log-matnr = x_sscc-material.
    wa_log-matnr_pal = x_sscc-vhilm.

    x_sscc-sscc = zwm020-p1.
    SELECT SINGLE tipo_palete INTO x_sscc-tipo_su
        FROM zwm013
          WHERE armazem = xuser-lgnum AND
                sscc = zwm020-p1.
    APPEND x_sscc.
    CLEAR x_sscc.

    SELECT SINGLE p~vemng p~vemeh p~matnr
                   p~werks p~lgort p~charg k~vhilm
       INTO (x_sscc-quantidade, x_sscc-uni, x_sscc-material,
              plant, lgort, x_sscc-lote_producao, x_sscc-vhilm)
          FROM vekp AS k INNER JOIN vepo AS p
               ON  k~venum = p~venum
                   WHERE  p~vepos = '000001' AND
                          k~exidv = zwm020-p2.

    wa_log-matnr2 = x_sscc-material.
    wa_log-matnr_pal2 = x_sscc-vhilm.

    x_sscc-sscc = zwm020-p2.
    SELECT SINGLE tipo_palete INTO x_sscc-tipo_su
        FROM zwm013
            WHERE armazem = xuser-lgnum AND
                  sscc = zwm020-p1.
    APPEND x_sscc.
    CLEAR x_sscc.

***** Transferir dados do armazem BA para o CD ****************
    PERFORM transferencia_stock TABLES x_sscc
                                          itab_zwm001
                                   USING xuser-lgnum
                                         mesa+4(10)
                                         i_linha
                                         ' ' "Entrada de Palete
                                         ' ' "Estorno
                                   CHANGING w_subrc
                                            wa_log-msg
                                            g_mblnr
                                            g_mjahr.

    IF NOT w_subrc IS INITIAL.
** Elimina o registo da tabela zwm013
      DELETE zwm013 FROM zwm013.

      retorno = w_subrc.
      wa_log-retorno = w_subrc.
*      wa_log-msg = 'Erro na transferência de Stock'.
      msg = 'Erro na transferência de Stock'.
      MODIFY zwm_log_efacec FROM wa_log.
      EXIT.
    ENDIF.

    wa_log-doc_transf = g_mblnr.
    wa_log-ano_transf = g_mjahr.

*************** Dar entrada da palete ***********************
    PERFORM transferencia_stock TABLES x_sscc
                                          itab_zwm001
                                   USING xuser-lgnum
                                         mesa+4(10)
                                         i_linha
                                         'X' "Entrada de Palete
                                         ' ' "Estorno
                                   CHANGING w_subrc
                                          wa_log-msg
                                          x_mblnr
                                          x_mjahr.

    wa_log-doc_palete = x_mblnr.
    wa_log-ano_palete = x_mjahr.

    IF NOT w_subrc IS INITIAL.
** Elimina o registo da tabela zwm013
*          DELETE zwm013 FROM zwm013.

      IF w_subrc = '98'.
        retorno = '00'.
      ELSE.
        retorno = w_subrc.
      ENDIF.

      wa_log-retorno = w_subrc.
*          wa_log-msg = 'Erro na transferência de Stock das Paletes'.
      msg = 'Erro na transferência de Stock das Paletes'.
      MODIFY zwm_log_efacec FROM wa_log.
    ENDIF.

***** CROSS DOCKING *******
    READ TABLE x_sscc INDEX 1.

    CLEAR: l_to.

    " Não permite na Entrada para o automático (Torres Novas)
    IF lgpla <> 'AUT'.
      CALL FUNCTION 'ZWM_CROSS_DOCKING'
        EXPORTING
          lgnum       = xuser-lgnum
          material    = x_sscc-material
          lote        = x_sscc-lote_producao
          quantidade  = x_sscc-quantidade
          uni         = x_sscc-uni
          tipo_palete = x_sscc-tipo_su
          su          = zwm020-p1
          su2         = zwm020-p2
          st_origem   = lgtyp
          bin_origem  = lgpla
        IMPORTING
          to          = l_to.

      IF NOT l_to IS INITIAL.
        wa_log-ordem = l_to.
        e_to = l_to.
      ENDIF.
    ENDIF.

******** Criar TO ********************
    IF l_to IS INITIAL.

      CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'ZWM014'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF lgpla <> 'AUT'. " Armazém Automático Torres Novas (WCS)
        SELECT SINGLE * FROM mlgn
            WHERE lgnum = xuser-lgnum
              AND matnr = x_sscc-material.

        IF mlgn-ltkze = 'TMP'.
          st_type = 'BLK'.

          IF mlgn-lgbkz = 'TM1'.
            dest_bin = '000-000-01'.
          ELSEIF mlgn-lgbkz = 'TM2'.
            dest_bin = '000-000-02'.
          ENDIF.

        ELSEIF mlgn-ltkze = 'AUT'.
          mov = mov_aut_man.
        ENDIF.
      ELSE.
        st_type = dest_bin = 'AUT'.
      ENDIF.

      CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
        EXPORTING
          warehouse   = xuser-lgnum
          mov_type    = mov
          st_type_o   = st_type
          bin_destino = dest_bin
          plant       = plant
          s_loc       = lgort
          certificado = certificado
          req_number  = lgpla
          req_type    = 'E'
        IMPORTING
          to          = to
        TABLES
          return_msg  = result_msg
          sscc        = x_sscc
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      IF sy-subrc <> 0.
        CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'ZWM014'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

** Elimina o registo da tabela zwm013
        DELETE zwm013 FROM zwm013.

        retorno = 10.
        wa_log-retorno = retorno.

        READ TABLE result_msg INDEX 1.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = result_msg-msgid
            msgnr               = result_msg-msgnr
            msgv1               = result_msg-msgv1
            msgv2               = result_msg-msgv2
            msgv3               = result_msg-msgv3
            msgv4               = result_msg-msgv4
          IMPORTING
            message_text_output = l_msg.

******* Fazer o estorno da transferência do BA para o CD ************
        PERFORM estorno_stock USING g_mblnr
                                    g_mjahr
                              CHANGING w_subrc
                                       wa_log-msg
                                       wa_log-doc_estorno
                                       wa_log-ano_estorno.

******* Fazer o estorno da transferência da Palete ************
        PERFORM estorno_stock USING x_mblnr
                                    x_mjahr
                              CHANGING w_subrc
                                       wa_log-msg
                                       wa_log-doc_estr_pal
                                       wa_log-ano_estr_pal.

        IF NOT w_subrc IS INITIAL.
** Elimina o registo da tabela zwm013
          DELETE zwm013 FROM zwm013.

          retorno = w_subrc.
          wa_log-retorno = w_subrc.
        ENDIF.
        wa_log-msg = l_msg.
        msg = wa_log-msg.
        MODIFY zwm_log_efacec FROM wa_log.

        CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'ZWM014'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        EXIT.
      ELSE.
** Verificar se a zona ou armazem cheio.
        SELECT SINGLE *
          FROM ltap
            WHERE lgnum = xuser-lgnum AND tanum = to.

        wa_log-ordem = to.
        e_to = to.

        IF ltap-nltyp = 'CMA'.
** Elimina o registo da tabela zwm013
          DELETE zwm013 FROM zwm013.

          retorno = 20.
          wa_log-retorno = retorno.
          wa_log-msg = 'Zona Cheia'.
          msg = wa_log-msg.
          MODIFY zwm_log_efacec FROM wa_log.
        ENDIF.
      ENDIF.
    ENDIF.

** Não é Palete Remontada
**********************************************************************
  ELSE.
    SELECT SINGLE p~vemng p~vemeh p~matnr p~charg
                  p~werks p~lgort k~vhilm
              INTO (x_sscc-quantidade, x_sscc-uni, x_sscc-material,
                    x_sscc-lote_producao, plant, lgort, x_sscc-vhilm)
                 FROM vekp AS k INNER JOIN vepo AS p
                      ON  k~venum = p~venum
                          WHERE  p~vepos = '000001' AND
                                 k~exidv = i_sscc.

    x_sscc-sscc = i_sscc.
    SELECT SINGLE tipo_palete INTO x_sscc-tipo_su
        FROM zwm013
          WHERE armazem = xuser-lgnum AND
                sscc = i_sscc.
    APPEND x_sscc.
    CLEAR x_sscc.

    READ TABLE x_sscc INDEX 1.
    mat_struct-material = x_sscc-material.
    mat_struct-menge = x_sscc-quantidade.
    mat_struct-meins = x_sscc-uni.
    mat_struct-charg = x_sscc-lote_producao.
    APPEND mat_struct.

    IF lgpla <> 'AUT'. " Armazém Automático Torres Novas (WCS)

      SELECT SINGLE *
         FROM marm
             WHERE matnr = x_sscc-material AND
*                 meinh = x_sscc-tipo_su.
                   meinh = 'PAL'.

      IF x_sscc-quantidade <> marm-umrez.
*    a palete é incompleta

        PERFORM get_zwm001 TABLES itab_zwm001
                           USING xuser-lgnum
                                 'ENTRADA_ARMAZEM'
                                 'ST_REP'
                           CHANGING valor.
        MOVE valor TO st_type.
      ELSE.
        SELECT SINGLE * FROM mlgn
            WHERE lgnum = xuser-lgnum
              AND matnr = x_sscc-material.

        IF mlgn-ltkze = 'TMP'.
          st_type = 'BLK'.
          IF mlgn-lgbkz = 'TM1'.
            dest_bin = '000-000-01'.
          ELSEIF mlgn-lgbkz = 'TM2'.
            dest_bin = '000-000-02'.
          ELSEIF mlgn-lgbkz = 'TM3'.
            dest_bin = '000-000-03'.
          ENDIF.

        ELSEIF mlgn-ltkze = 'AUT'.
          mov = mov_aut_man.
        ENDIF.
      ENDIF.

    ELSE.
      st_type = dest_bin = 'AUT'.
    ENDIF.

***** Transferir dados do armazem BA para o CD ****************
    PERFORM transferencia_stock TABLES x_sscc
                                          itab_zwm001
                                   USING xuser-lgnum
                                         mesa+4(10)
                                         i_linha
                                         ' ' "Entrada de Palete
                                         ' ' "Estorno
                                   CHANGING w_subrc
                                              wa_log-msg
                                              g_mblnr
                                              g_mjahr.

    IF NOT w_subrc IS INITIAL.
** Elimina o registo da tabela zwm013
      DELETE zwm013 FROM zwm013.

      retorno = w_subrc.
      wa_log-retorno = w_subrc.
*      wa_log-msg = 'Erro na transferência de Stock'.
      msg = 'Erro na transferência de Stock'.
      MODIFY zwm_log_efacec FROM wa_log.
      EXIT.
    ENDIF.

    wa_log-doc_transf = g_mblnr.
    wa_log-ano_transf = g_mjahr.

*************** Dar entrada da palete ***********************
    PERFORM transferencia_stock TABLES x_sscc
                                          itab_zwm001
                                   USING xuser-lgnum
                                         mesa+4(10)
                                         i_linha
                                         'X' "Entrada de Palete
                                         ' ' "Estorno
                                   CHANGING w_subrc
                                          wa_log-msg
                                          x_mblnr
                                          x_mjahr.

    wa_log-doc_palete = x_mblnr.
    wa_log-ano_palete = x_mjahr.

    IF NOT w_subrc IS INITIAL.
** Elimina o registo da tabela zwm013
*          DELETE zwm013 FROM zwm013.

      IF w_subrc = '98'.
        retorno = '00'.
      ELSE.
        retorno = w_subrc.
      ENDIF.

      wa_log-retorno = w_subrc.
      msg = 'Erro na transferência de Stock das Paletes'.
*          wa_log-msg = 'Erro na transferência de Stock das Paletes'.
      MODIFY zwm_log_efacec FROM wa_log.
    ENDIF.

***** CROSS DOCKING *******
    READ TABLE x_sscc INDEX 1.

    CLEAR: l_to.

    " Não permite na Entrada para o automático (Torres Novas)
    IF lgpla <> 'AUT'.
      CALL FUNCTION 'ZWM_CROSS_DOCKING'
        EXPORTING
          lgnum       = xuser-lgnum
          material    = x_sscc-material
          lote        = x_sscc-lote_producao
          quantidade  = x_sscc-quantidade
          uni         = x_sscc-uni
          tipo_palete = x_sscc-tipo_su
          su          = i_sscc
          st_origem   = lgtyp
          bin_origem  = lgpla
        IMPORTING
          to          = l_to.

      IF NOT l_to IS INITIAL.
        wa_log-ordem = l_to.
        e_to = l_to.
      ENDIF.
    ENDIF.

******* CRIAR TO *********
    IF l_to IS INITIAL.
      CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
        EXPORTING
          mode_keyword   = 'X'
          keyword_       = 'ZWM014'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      CALL FUNCTION 'ZWM_CREATE_STORAGE_UNIT'
        EXPORTING
          warehouse   = xuser-lgnum
          mov_type    = mov
          st_type     = st_type
          dest_bin    = dest_bin
          plant       = plant
          s_loc       = lgort
          su_type     = x_sscc-tipo_su
          certificado = certificado
          mat_struct  = mat_struct
          req_number  = lgpla
          req_type    = 'E'
        IMPORTING
          to          = to
        TABLES
          result_msg  = result_msg
        CHANGING
          su_number   = x_sscc-sscc
        EXCEPTIONS
          OTHERS      = 1.

      IF sy-subrc <> 0.
        CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'ZWM014'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

** Elimina o registo da tabela zwm013
        DELETE zwm013 FROM zwm013.

        retorno = 30.
        wa_log-retorno = retorno.

        READ TABLE result_msg INDEX 1.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = result_msg-msgid
            msgnr               = result_msg-msgnr
            msgv1               = result_msg-msgv1
            msgv2               = result_msg-msgv2
            msgv3               = result_msg-msgv3
            msgv4               = result_msg-msgv4
          IMPORTING
            message_text_output = l_msg.

*      wa_log-msg = 'Erro na criação da TO'.

******* Fazer o estorno da transferência do BA para o CD ************
        PERFORM estorno_stock USING g_mblnr
                                    g_mjahr
                              CHANGING w_subrc
                                       wa_log-msg
                                       wa_log-doc_estorno
                                       wa_log-ano_estorno.

******* Fazer o estorno da transferência da Palete ************
        PERFORM estorno_stock USING x_mblnr
                                    x_mjahr
                              CHANGING w_subrc
                                       wa_log-msg
                                       wa_log-doc_estr_pal
                                       wa_log-ano_estr_pal.

        IF NOT w_subrc IS INITIAL.
** Elimina o registo da tabela zwm013
          DELETE zwm013 FROM zwm013.

          retorno = w_subrc.
          wa_log-retorno = w_subrc.
        ENDIF.
        wa_log-msg = l_msg.
        msg        = wa_log-msg.

        MODIFY zwm_log_efacec FROM wa_log.
        EXIT.
      ELSE.

        CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'ZWM014'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

** Verificar se a zona ou armazem cheio.
        SELECT SINGLE *
          FROM ltap
            WHERE lgnum = xuser-lgnum AND tanum = to.

        wa_log-ordem = to.
        e_to = to.

        IF ltap-nltyp = 'CMA'.
** Elimina o registo da tabela zwm013
          DELETE zwm013 FROM zwm013.

          retorno = 20.
          wa_log-retorno = retorno.
          wa_log-msg = 'Zona Cheia'.
          MODIFY zwm_log_efacec FROM wa_log.
        ENDIF.
      ENDIF.
    ENDIF. "Cross-Docking
  ENDIF.

** Armazém Automático Torres Novas (WCS)
  IF xuser-lgnum = '100' AND lgpla = 'AUT' AND e_to IS NOT INITIAL.

    REFRESH: lt_ltap.

    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = xuser-lgnum
      AND   tanum = e_to.

    LOOP AT lt_ltap.
      CLEAR lt_ltap_conf.
      lt_ltap_conf-tanum = lt_ltap-tanum.
      lt_ltap_conf-tapos = lt_ltap-tapos.
      lt_ltap_conf-altme = lt_ltap-meins.
      lt_ltap_conf-nista = lt_ltap-vsolm.
      APPEND lt_ltap_conf.
    ENDLOOP.

    " Confirmar Automática de OT de arrumação
    CALL FUNCTION 'L_TO_CONFIRM'
      EXPORTING
        i_lgnum       = xuser-lgnum
        i_tanum       = e_to
        i_quknz       = '4' "lv_quknz
        i_commit_work = 'X'
      TABLES
        t_ltap_conf   = lt_ltap_conf
      EXCEPTIONS
        error_message = 99.

  ENDIF.

  MODIFY zwm_log_efacec FROM wa_log.

ENDFUNCTION.
