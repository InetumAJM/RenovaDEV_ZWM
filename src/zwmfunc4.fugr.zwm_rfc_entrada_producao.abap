FUNCTION zwm_rfc_entrada_producao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IN_STRING) LIKE  ZWM_AUX-IN_STRING OPTIONAL
*"     VALUE(SIMULACAO) LIKE  ZWM_AUX-SIMULA OPTIONAL
*"     VALUE(POSTO) LIKE  ZWM_AUX-POSTO OPTIONAL
*"     VALUE(NAO_IMPRIME) LIKE  ZWM_AUX-SIMULA OPTIONAL
*"     VALUE(SSCC) LIKE  VEKP-EXIDV OPTIONAL
*"     VALUE(SSCC2) LIKE  VEKP-EXIDV OPTIONAL
*"  EXPORTING
*"     VALUE(OUT_STRING) LIKE  ZWM_AUX-OUT_STRING
*"     VALUE(OUT_PRODUCAO) LIKE  ZWM_AUX-OUT_PRODUCAO
*"     VALUE(RETORNO) LIKE  ZWM_AUX-RETORNO
*"     VALUE(ALTURA) LIKE  ZWM_LOG_EFACEC-ALTURA
*"     VALUE(COR) LIKE  ZWM_LOG_EFACEC-COR
*"     VALUE(EAN11) LIKE  MEAN-EAN11
*"     VALUE(EAN128_CB1) LIKE  ZWM_AUX-OUT_CB_TX
*"     VALUE(EAN128_CB2) LIKE  ZWM_AUX-OUT_CB_TX
*"     VALUE(EAN128_CB3) LIKE  ZWM_AUX-OUT_CB_TX
*"     VALUE(EAN128_CB4) LIKE  ZWM_AUX-OUT_CB_TX
*"     VALUE(EAN128_TX1) LIKE  ZWM_AUX-OUT_CB_TX
*"     VALUE(EAN128_TX2) LIKE  ZWM_AUX-OUT_CB_TX
*"     VALUE(EAN128_TX3) LIKE  ZWM_AUX-OUT_CB_TX
*"     VALUE(EAN128_TX4) LIKE  ZWM_AUX-OUT_CB_TX
*"     VALUE(LAY_ETIQUETA) LIKE  ZWM_AUX-OUT_CB_TX
*"     VALUE(E_SSCC) LIKE  VEKP-EXIDV
*"     VALUE(E_SSCC2) LIKE  VEKP-EXIDV
*"     VALUE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      EXCEPTION
*"----------------------------------------------------------------------
  TABLES: aufk, jest, tj02t.

  DATA lt_z02rpsessao LIKE z02rpsessao OCCURS 0 WITH HEADER LINE.
  DATA: l_select(1) TYPE c,
        l_lock,
        l_index     LIKE sy-tabix,
        hlp_uname   LIKE sy-uname,
        ean128_cba  LIKE zwm_aux-out_cb_tx,
        ean128_cbb  LIKE zwm_aux-out_cb_tx,
        ean128_cbc  LIKE zwm_aux-out_cb_tx,
        ean128_txa  LIKE zwm_aux-out_cb_tx,
        ean128_txb  LIKE zwm_aux-out_cb_tx,
        ean128_txc  LIKE zwm_aux-out_cb_tx.

  DATA: lv_lznum TYPE lznum.

  DATA: BEGIN OF itab_tj02t OCCURS 0,
          txt04 LIKE tj02t-txt04,
          istat LIKE tj02t-istat,
        END OF itab_tj02t.

*  DATA: BEGIN OF it_mean OCCURS 0,
*          matnr LIKE mean-matnr,
*          eantp LIKE mean-eantp,
*          meinh LIKE mean-meinh,
*          hpean LIKE mean-hpean,
*          ean11 LIKE mean-ean11,
*        END OF it_mean.

  DATA: ls_message      TYPE bdcmsgcoll,
        lv_consolidacao TYPE flag,
        lv_aufnr        TYPE aufnr,
        lv_skip_entrada TYPE flag,
        mat_manipulado  TYPE matnr.

  DATA: zgrp1 LIKE mvke-mvgr1, zlay LIKE zwm043-lay_etiqueta.
  CLEAR: zgrp1, zlay.

  CLEAR: gt_zwm030, itab_zwm001, gs_reg, zwm_log_efacec, wa_log,
         retorno, gt_registo, out_producao, out_string, l_select,
         itab_tj02t.

  CLEAR: altura, cor, ean11, ean128_cb1, ean128_cb2, ean128_cb3,
         ean128_cb4, ean128_tx1, ean128_tx2, ean128_tx3,
         ean128_tx4, lay_etiqueta, w_subrc, mat_manipulado.

  FREE: itab_tj02t, itab_zwm001, gt_zwm030.

* valor default para etiqueta.
  lay_etiqueta = 'renova'.

  IF in_string IS INITIAL.
*   Erro na transferência (input string vazia)
    retorno = 70.
    ls_message-msgid = 'ZWM001'.
    ls_message-msgnr = '099'.
    ls_message-msgtyp = 'E'.
    APPEND ls_message TO et_messages.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs       = '100'
    TABLES
      ti_zwm001 = itab_zwm001.

* Atribui campos
  MOVE in_string(18)    TO gt_registo-codigo.
  MOVE in_string+18(3)  TO gt_registo-linha.
  MOVE in_string+21(4)  TO gt_registo-altura.
  MOVE in_string+25(6)  TO gt_registo-cor.
  MOVE in_string+31(4)  TO gt_registo-quantidade.
  MOVE in_string+35(12) TO gt_registo-aufnr.
  MOVE in_string+47(1)  TO gt_registo-pal_completa.
  cor = gt_registo-cor. " cor é a variavel da etiqueta

** Dados table de LOG do interface
  MOVE-CORRESPONDING gt_registo TO wa_log.

  GET TIME.
  wa_log-data = sy-datum.
  IF simulacao = 'X'.
    wa_log-processo = 'SIMULACAO'.
  ELSE.
    wa_log-processo = 'ENTRADA_PRODUCAO'.
  ENDIF.

  wa_log-hora = sy-uzeit.
  wa_log-simulacao = simulacao.
  wa_log-posto = posto.
  wa_log-in_string = in_string.

* Verifica se pelo menos a Linha ou o Código estão preenchidos
  IF gt_registo-linha  IS INITIAL AND
     gt_registo-codigo IS INITIAL.
    retorno = 71.
    wa_log-retorno = retorno.
    wa_log-msg = 'Sem Linha/Código preenchidos'.
    MODIFY zwm_log_efacec FROM wa_log.
    ls_message-msgid = 'ZWM001'.
    ls_message-msgnr = '100'.
    ls_message-msgtyp = 'E'.
    APPEND ls_message TO et_messages.
    EXIT.
  ENDIF.

  IF gt_registo-linha = 'GMA'.
    PERFORM check_manipulado USING gs_reg-matnr gt_registo-codigo '100' CHANGING mat_manipulado.
    IF mat_manipulado IS NOT INITIAL.
      gs_reg-matnr = mat_manipulado.
*      gs_reg-lgnum = '100'.
*      gs_reg-werks = 'RENV'.
*      lv_consolidacao = abap_true.
      " Esta chamada a FM é necessária para obter o lote
      CALL FUNCTION 'ZWM_ENTRADAS_CONSOLIDACAO'
        EXPORTING
          i_lgnum           = '100'
          i_werks           = 'RENV'
          i_ean             = gt_registo-codigo
          i_test            = abap_true
        IMPORTING
          e_matnr           = gs_reg-matnr
          e_charg           = gs_reg-charg
        EXCEPTIONS
          not_consolidation = 1
          error             = 2
          OTHERS            = 3.

      IF sy-subrc EQ 1.
        CLEAR: lv_consolidacao.
      ELSE.
        lv_consolidacao = abap_true.
        gs_reg-lgnum = '100'.
        gs_reg-werks = 'RENV'.
      ENDIF.
    ENDIF.
  ENDIF.

** Linha é vazia, verifica se a ordem está preenchida
** Linha e Ordem Vazia obter tudo
  IF gt_registo-linha IS INITIAL. "Linha Vazia
    IF gt_registo-aufnr IS INITIAL. "Ordem Vazia
      l_select = '1'. "Lêr tudo
    ELSE.
      l_select = '2'. "Lêr só com a Ordem
    ENDIF.
  ELSE.
** Linha não é vazia, verifica se a ordem está preenchida
** Linha Preenchida e Ordem Vazia
    IF gt_registo-aufnr IS INITIAL.
      l_select = '3'. "Lêr só com a linha
    ELSE.
      l_select = '4'. "Lêr com a linha e ordem
    ENDIF.
  ENDIF.

** 1 = Lêr tudo
** 2 = Lêr só com a Ordem de Produção
** 3 = Lêr só com a Linha
** 4 = Lêr com Linha e Ordem
  " Ler ordens de produção de zwm030
  IF lv_consolidacao EQ abap_false.
    CASE l_select.
      WHEN '1'.
        SELECT * FROM zwm030 INTO TABLE gt_zwm030
          WHERE bloqueio NE 'X' AND werks = 'RENV'.
        IF sy-subrc NE 0.
          retorno = 10.
          wa_log-retorno = retorno.
          wa_log-msg = 'Tabela sem registos'.
          MODIFY zwm_log_efacec FROM wa_log.
          CLEAR ls_message.
          ls_message-msgid = 'ZWM001'.
          ls_message-msgnr = '101'.
          ls_message-msgtyp = 'E'.
          APPEND ls_message TO et_messages.
          EXIT.
        ENDIF.
      WHEN '2'.
        SELECT * FROM zwm030 INTO TABLE gt_zwm030
        WHERE aufnr EQ gt_registo-aufnr
          AND bloqueio NE 'X'
          AND werks = 'RENV'.
        IF sy-subrc NE 0.
          retorno = 10.
          wa_log-retorno = retorno.
          wa_log-msg = 'Ordem não existe'.
          MODIFY zwm_log_efacec FROM wa_log.
          CLEAR ls_message.
          ls_message-msgid = 'ZWM001'.
          ls_message-msgnr = '102'.
          ls_message-msgtyp = 'E'.
          ls_message-msgv1 = gt_registo-aufnr.
          APPEND ls_message TO et_messages.
          EXIT.
        ENDIF.
      WHEN '3'.
        SELECT * FROM zwm030 INTO TABLE gt_zwm030
        WHERE linha EQ gt_registo-linha
          AND bloqueio NE 'X'
          AND werks = 'RENV'.
        IF sy-subrc NE 0.
          retorno = 10.
          wa_log-retorno = retorno.
          wa_log-msg = 'Linha não existe ou é incorrecta'.
          MODIFY zwm_log_efacec FROM wa_log.
          CLEAR ls_message.
          ls_message-msgid = 'ZWM001'.
          ls_message-msgnr = '103'.
          ls_message-msgtyp = 'E'.
          ls_message-msgv1 = gt_registo-linha.
          APPEND ls_message TO et_messages.
          EXIT.
        ENDIF.
      WHEN '4'.
        SELECT * FROM zwm030 INTO TABLE gt_zwm030
        WHERE aufnr EQ gt_registo-aufnr
          AND linha EQ gt_registo-linha
          AND bloqueio NE 'X'
          AND werks = 'RENV'.
        IF sy-subrc NE 0.
          retorno = 10.
          wa_log-retorno = retorno.
          wa_log-msg = 'Ordem e linha não existem'.
          MODIFY zwm_log_efacec FROM wa_log.
          CLEAR ls_message.
          ls_message-msgid = 'ZWM001'.
          ls_message-msgnr = '104'.
          ls_message-msgtyp = 'E'.
          ls_message-msgv1 = gt_registo-aufnr.
          ls_message-msgv2 = gt_registo-linha.
          APPEND ls_message TO et_messages.
          EXIT.
        ENDIF.
    ENDCASE.

** Excluir as ordens de produção que estejam encerradas técnicamente
    CLEAR: tj02t.
    SELECT * FROM tj02t INTO CORRESPONDING FIELDS OF TABLE itab_tj02t
      WHERE spras EQ sy-langu
        AND ( txt04 EQ 'ENTE' OR txt04 EQ 'ENCE' ).

    CLEAR: l_index.

    LOOP AT gt_zwm030.
      l_index = sy-tabix.
      " Verifica se a Ordem de Produção não está encerrada Técnicamente
      CLEAR: aufk.
      SELECT SINGLE * FROM aufk
      WHERE aufnr EQ gt_zwm030-aufnr.

      " Selecionar ordens que não estejam com status não fechado
      " técnicamente
      LOOP AT itab_tj02t.
        CLEAR: jest.
        SELECT SINGLE * FROM jest
        WHERE objnr EQ aufk-objnr
          AND stat  EQ itab_tj02t-istat
          AND inact EQ ' '.

        CHECK sy-subrc EQ 0.
        DELETE gt_zwm030 INDEX l_index.
      ENDLOOP.
    ENDLOOP.

    "* Verifica Linha ou Código EAN
    CLEAR w_subrc.
    IF NOT gt_registo-linha IS INITIAL.
      "*   Verifica Linha
      PERFORM verifica_linha USING w_subrc
                             CHANGING cor.
      IF w_subrc NE 0.
        retorno = w_subrc.
        wa_log-retorno = retorno.
        wa_log-msg = 'Linha não existe ou é incorrecta'.
        MODIFY zwm_log_efacec FROM wa_log.
        CLEAR ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '103'.
        ls_message-msgtyp = 'E'.
        ls_message-msgv1 = gt_registo-linha.
        APPEND ls_message TO et_messages.
        EXIT.
      ENDIF.
      "Verifica Código
      PERFORM verifica_codigo USING w_subrc CHANGING out_producao.
    ELSE.
      IF NOT gt_registo-codigo IS INITIAL.
        PERFORM verifica_codigo USING w_subrc CHANGING out_producao.
      ENDIF.
    ENDIF.
  ELSE.
    PERFORM ler_descricao USING gs_reg-matnr.
  ENDIF.

  wa_log-matnr = gs_reg-matnr.
  wa_log-aufnr = gs_reg-aufnr.

  IF w_subrc NE 0.
    retorno = w_subrc.
    CASE retorno.
      WHEN 20.
        wa_log-msg = 'EAN Não existe em SAP'.
        CLEAR ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '105'.
        ls_message-msgtyp = 'E'.
        ls_message-msgv1 = gt_registo-codigo.
        APPEND ls_message TO et_messages.
      WHEN 21.
        wa_log-msg = 'Material não existe'.
        CLEAR ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '106'.
        ls_message-msgtyp = 'E'.
        ls_message-msgv1 = gs_reg-matnr.
        APPEND ls_message TO et_messages.
      WHEN 22.
        wa_log-msg = 'Material em mais do que uma ordem produção'.
        CLEAR ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '107'.
        ls_message-msgtyp = 'E'.
        ls_message-msgv1 = gs_reg-matnr.
        APPEND ls_message TO et_messages.
    ENDCASE.

    wa_log-retorno = retorno.
    wa_log-out_producao = out_producao.
    MODIFY zwm_log_efacec FROM wa_log.
    EXIT.
  ENDIF.

** Caso a linha seja vazia e só exista 1 Ordem de produção

** Identificar cor (tipo) da palete
  IF gt_registo-linha IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_reg-aufnr
      IMPORTING
        output = lv_aufnr.

    CLEAR: afko.
    SELECT SINGLE fevor FROM afko INTO gt_registo-linha
      WHERE aufnr EQ lv_aufnr.

    "* Substitui tipo de palete se existir na tabela ZTIPOPALETE
    " apenas paletizador central
    PERFORM check_tipo_palete CHANGING cor.       "AMALCATA 20071128.
    IF w_subrc NE 0.
      retorno = w_subrc.
      wa_log-retorno = retorno.
      wa_log-msg = 'Linha não existe ou é incorrecta'.
      MODIFY zwm_log_efacec FROM wa_log.
      CLEAR ls_message.
      ls_message-msgid = 'ZWM001'.
      ls_message-msgnr = '103'.
      ls_message-msgtyp = 'E'.
      ls_message-msgv1 = gt_registo-linha.
      APPEND ls_message TO et_messages.
      EXIT.
    ENDIF.
    CLEAR: out_producao.
  ENDIF.

** Passar a cor para o formato de output
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    EXPORTING
      input  = cor
    IMPORTING
      output = cor.

  wa_log-linha = gt_registo-linha.
  wa_log-cor   = cor.

  MODIFY zwm_log_efacec FROM wa_log.
  "-- Fim tipo de palete

  "--selecionar tipo de layout de etiqueta por material - Paulo Sousa em 29.08.2006
  SELECT SINGLE mvgr1 INTO zgrp1 FROM mvke WHERE matnr = gs_reg-matnr.
  IF zgrp1 = 'MDD'.  " marcas da distribuicao
    zlay = 'SEMLOGO'.
  ELSE.
    SELECT SINGLE lay_etiqueta INTO zlay FROM zwm043 WHERE matnr = gs_reg-matnr.
  ENDIF.
  IF zlay IS NOT INITIAL.
    lay_etiqueta = zlay.
    CONDENSE lay_etiqueta.
  ENDIF.
  "--Fim label

  " Ler descrição do material
  PERFORM ler_descricao USING gs_reg-matnr.

  "-- Obter codigos EAN do material
  PERFORM get_ean USING gs_reg-matnr gt_registo-codigo CHANGING ean11.

*  REFRESH: it_mean.
*  CLEAR:   it_mean, ean11.
*
*  SELECT matnr eantp meinh hpean ean11 FROM mean
*         INTO CORRESPONDING FIELDS OF TABLE it_mean
*          WHERE matnr EQ gs_reg-matnr.
*
*  SORT it_mean BY matnr eantp meinh hpean ean11.
*
*  READ TABLE it_mean WITH KEY matnr = gs_reg-matnr
*                              eantp = 'IC'
*                              meinh = 'PAL'
*                              hpean = 'X'.
*** 2015.03.04 - paulo sousa********
*  IF sy-subrc <> 0.
*    READ TABLE it_mean WITH KEY matnr = gs_reg-matnr
*                                eantp = 'IC'
*                                meinh = 'pal'
*                                hpean = 'X'.
*  ENDIF.
*** 2015.03.04 - paulo sousa******** FIM
*  IF sy-subrc EQ 0.
*    ean11 = it_mean-ean11.
*  ELSE.
*    READ TABLE it_mean WITH KEY matnr = gs_reg-matnr
*                                eantp = 'IC'
*                                hpean = 'X'
*                                ean11 = gt_registo-codigo.
*    IF sy-subrc EQ 0.
*      ean11 = it_mean-ean11.
*    ELSE.
*      READ TABLE it_mean WITH KEY matnr = gs_reg-matnr
*                                  eantp = 'IC'
*                                  hpean = 'X'.
*      IF sy-subrc EQ 0.
*        ean11 = it_mean-ean11.
*      ELSE.
*        READ TABLE it_mean WITH KEY matnr = gs_reg-matnr
*                                    eantp = 'HE'
*                                    hpean = 'X'.
*        IF sy-subrc EQ 0.
*          ean11 = it_mean-ean11.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
** Ler MLGN
  IF lv_consolidacao EQ abap_false.
    gs_reg-lgnum = gt_zwm030-lgnum.
  ENDIF.
  CLEAR: mlgn.
  SELECT SINGLE * FROM mlgn WHERE matnr = gs_reg-matnr
                              AND lgnum = gs_reg-lgnum.
  IF sy-subrc NE 0.
    "*   Não existe entrada na MLGN
    retorno = '80'.
    wa_log-retorno = retorno.
    wa_log-msg = 'Material não está expandido no depósito'.
    MODIFY zwm_log_efacec FROM wa_log.
    CLEAR ls_message.
    ls_message-msgid = 'ZWM001'.
    ls_message-msgnr = '108'.
    ls_message-msgtyp = 'E'.
    ls_message-msgv1 = gs_reg-matnr.
    ls_message-msgv2 = gs_reg-lgnum.
    APPEND ls_message TO et_messages.
    EXIT.
  ENDIF.

* Verificar se é Palete Completa
  IF NOT gt_registo-pal_completa IS INITIAL.
    PERFORM get_quantidade_completa USING w_subrc.
  ELSE.
    "* Verifica Quantidade ou Altura
    IF gt_registo-quantidade IS INITIAL.
      " Verifica altura
      PERFORM verifica_altura USING w_subrc CHANGING altura.
    ELSE.
      " Verifica quantidade
      PERFORM verifica_quantidade USING mlgn-lgnum mlgn-lety1 w_subrc.
    ENDIF.
  ENDIF.

  IF w_subrc NE 0.
    retorno = w_subrc.
    CASE retorno.
      WHEN 30.
        wa_log-msg = 'Material sem Unid. Medida PAL'.
        CLEAR ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '109'.
        ls_message-msgtyp = 'E'.
        ls_message-msgv1 = gs_reg-matnr.
        APPEND ls_message TO et_messages.
      WHEN 31.
        wa_log-msg = 'Altura inválida'.
        CLEAR ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '110'.
        ls_message-msgtyp = 'E'.
        APPEND ls_message TO et_messages.
      WHEN 40.
        wa_log-msg = 'Quant. incorrecta'.
        CLEAR ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '111'.
        ls_message-msgtyp = 'E'.
        APPEND ls_message TO et_messages.
      WHEN 91.
        wa_log-msg =
        'Paletes remontadas - Somente quantidades completas'.
        CLEAR ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '112'.
        ls_message-msgtyp = 'E'.
        APPEND ls_message TO et_messages.
    ENDCASE.

    wa_log-retorno = retorno.
    MODIFY zwm_log_efacec FROM wa_log.
    EXIT.
  ENDIF.

  wa_log-quantidade = gs_reg-quantidade.

* Verifica côr (tipo de palete)
  PERFORM verifica_cor USING w_subrc.
  IF w_subrc NE 0.
    retorno = w_subrc.
    CASE retorno.
      WHEN 50.
        wa_log-msg = 'Côr Palete não existe'.
        CLEAR ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '113'.
        ls_message-msgtyp = 'E'.
        ls_message-msgv1 = gt_registo-cor.
        APPEND ls_message TO et_messages.
      WHEN 51.
        wa_log-msg = 'Côr Palete não é a do Material'.
        CLEAR ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '113'.
        ls_message-msgtyp = 'E'.
        APPEND ls_message TO et_messages.
    ENDCASE.
    wa_log-retorno = retorno.
    MODIFY zwm_log_efacec FROM wa_log.
    EXIT.
  ENDIF.

  IF lv_consolidacao EQ abap_false.
* Atribui valores
    gs_reg-code  = gt_zwm030-code.
    gs_reg-bwart = gt_zwm030-bwart.
    gs_reg-werks = gt_zwm030-werks.
    gs_reg-charg = gt_zwm030-charg.
  ENDIF.

** Valida se o Lote está aberto no armazém CD
  PERFORM check_lote USING gs_reg-matnr
                           '100'
                           gs_reg-charg
                           w_subrc
                           wa_log-msg.
  IF w_subrc NE 0.
    retorno = w_subrc.
    wa_log-retorno = retorno.
    MODIFY zwm_log_efacec FROM wa_log.
    EXIT.
  ENDIF.

** Não cria SSCC
  IF nao_imprime NE 'X' OR simulacao   NE 'X'.
    " Cria o SSCC (HU)
    PERFORM cria_sscc USING w_subrc sscc.
    IF w_subrc NE 0.
      retorno = w_subrc.
      wa_log-msg = 'Erro na criação do SSCC'.
      wa_log-retorno = retorno.
      MODIFY zwm_log_efacec FROM wa_log.
      CLEAR ls_message.
      ls_message-msgid = 'ZWM001'.
      ls_message-msgnr = '114'.
      ls_message-msgtyp = 'E'.
      APPEND ls_message TO et_messages.
      EXIT.
    ENDIF.
    retorno = 00.
    gs_reg-hukey1 = g_hukey.
    e_sscc = g_hukey.

*** Dá apenas entradas de produção e não cria SSCC

    PERFORM actualiza_zwm013.

*   Se LETY1 = P2 ou P5 cria mais uma SSCC
    IF z_wm_cl_management=>is_remontada( is_data = mlgn ) EQ abap_true.
      PERFORM cria_sscc USING w_subrc sscc2.
      IF w_subrc = 0.
        gs_reg-hukey2 = g_hukey.
        e_sscc2 = g_hukey.

        "** Dá apenas entradas de produção e não cria SSCC
        IF nao_imprime NE 'X' OR
           simulacao   NE 'X'.
          PERFORM actualiza_zwm020.
        ENDIF.
      ELSE.
        retorno = w_subrc.
        wa_log-msg = 'Erro na criação do SSCC da remontada'.
        wa_log-retorno = retorno.
        MODIFY zwm_log_efacec FROM wa_log.
        CLEAR ls_message.
        ls_message-msgid = 'ZWM001'.
        ls_message-msgnr = '114'.
        ls_message-msgtyp = 'E'.
        APPEND ls_message TO et_messages.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF simulacao NE 'X'.
    " Regista entrada de Consolidação
    lv_skip_entrada = abap_false.
    IF lv_consolidacao = abap_true.
      lv_skip_entrada = abap_true.
      CALL FUNCTION 'ZWM_ENTRADAS_CONSOLIDACAO'
        EXPORTING
          i_lgnum           = '100'
          i_werks           = 'RENV'
          i_matnr           = gs_reg-matnr
          i_menge           = gs_reg-quantidade
          i_meins           = gs_reg-unidade
          i_charg           = gs_reg-charg
          i_exidv           = gs_reg-hukey1
          i_exidv2          = gs_reg-hukey2
        IMPORTING
          e_mblnr           = gs_reg-mblnr
          e_mjahr           = gs_reg-mjahr
          et_messages       = et_messages
        EXCEPTIONS
          not_consolidation = 1
          error             = 2
          OTHERS            = 3.

      IF sy-subrc <> 0 AND sy-subrc <> 1.
        CLEAR: ls_message.
        READ TABLE et_messages
              INTO ls_message
              INDEX 1.

        retorno = '60'.

        MESSAGE ID ls_message-msgid
              TYPE 'E'
              NUMBER ls_message-msgnr
              WITH ls_message-msgv1 ls_message-msgv2
                   ls_message-msgv3 ls_message-msgv4
              INTO wa_log-msg.

        wa_log-retorno = retorno.
        MODIFY zwm_log_efacec FROM wa_log.
        EXIT.
      ELSEIF sy-subrc EQ 1.
        CLEAR: lv_skip_entrada.
      ENDIF.
    ENDIF.

    IF lv_skip_entrada EQ abap_false.
      PERFORM regista_entrada_armazem USING 'X' w_subrc
                                            mlgn-lety1
                                      CHANGING wa_log-msg.
      IF w_subrc NE 0.
        retorno = '60'.
        " wa_log-msg = 'Erro entrada produção - Modo Teste'.
        wa_log-retorno = retorno.
        MODIFY zwm_log_efacec FROM wa_log.
        EXIT.
      ENDIF.
      " Depois em modo NORMAL
      PERFORM regista_entrada_armazem USING ' ' w_subrc mlgn-lety1
                                      CHANGING wa_log-msg.

      IF w_subrc NE 0 OR gs_reg-mblnr IS INITIAL.
        retorno = '61'.
        " wa_log-msg = 'Erro entrada produção - Modo Real'.
        wa_log-retorno = retorno.
        MODIFY zwm_log_efacec FROM wa_log.
        EXIT.
      ENDIF.
    ENDIF.

    wa_log-doc_entrada = gs_reg-mblnr.
    wa_log-ano_entrada = gs_reg-mjahr.

* Se o lote não estiver preenchido ler no Doc. Material
    IF gs_reg-charg IS INITIAL.
      SELECT * FROM mseg WHERE mblnr = gs_reg-mblnr
                           AND mjahr = gs_reg-mjahr.
        CHECK mseg-matnr = gs_reg-matnr.
        gs_reg-werks = mseg-werks.
        gs_reg-charg = mseg-charg.
        gs_reg-lgort = mseg-lgort.
        EXIT.
      ENDSELECT.
    ENDIF.

** Actualiza quantidades
    SELECT SINGLE * FROM zwm030 WHERE aufnr = gs_reg-aufnr.
    IF sy-subrc = 0.
      CLEAR gs_zwm030.
      gs_zwm030 = zwm030.
      wa_log-meins = gs_zwm030-meins_p.

      GET TIME.
      gs_zwm030-data = sy-datum.

** Paletes remontadas, multiplica por 2
      IF NOT gs_reg-hukey2 IS INITIAL.
        gs_zwm030-prodp = gs_zwm030-prodp + gs_reg-quantidade * 2.
        wa_log-pal_remontada = 'X'.
      ELSE.
        gs_zwm030-prodp = gs_zwm030-prodp + gs_reg-quantidade.
      ENDIF.

      IF gs_zwm030-aufnr IS INITIAL.
        retorno = '87'.
        wa_log-msg = 'Sem Ordem de Produção'.
        wa_log-aufnr = gs_zwm030-aufnr.
        wa_log-retorno = retorno.
        MODIFY zwm_log_efacec FROM wa_log.
        EXIT.
      ELSE.
        CLEAR: hlp_uname, l_lock.

        CALL FUNCTION 'ENQUEUE_EZ_WM_AUFNR'
          EXPORTING
            mode_zwm030    = 'E'
            mandt          = sy-mandt
            aufnr          = gs_zwm030-aufnr
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        hlp_uname = sy-msgv1.

        CASE sy-subrc.
          WHEN 0.
            l_lock = 'X'.
          WHEN OTHERS.
            retorno = '88'.
            CONCATENATE 'Ordem Bloqueada pelo user' hlp_uname
            INTO wa_log-msg SEPARATED BY space.
            wa_log-aufnr = gs_zwm030-aufnr.
            wa_log-retorno = retorno.
            MODIFY zwm_log_efacec FROM wa_log.
            EXIT.
        ENDCASE.

        UPDATE zwm030 FROM gs_zwm030.

        COMMIT WORK.

        CALL FUNCTION 'DEQUEUE_EZ_WM_AUFNR'
          EXPORTING
            mode_zwm030 = 'E'
            mandt       = sy-mandt
            aufnr       = gs_zwm030-aufnr.
        IF sy-subrc EQ 0.
          CLEAR: l_lock.
        ENDIF.

      ENDIF. "Sem Ordem de Produção
    ENDIF. "Erro Sem registo na tabela ZWM030
  ENDIF. "Simulacao ne 'X'


** Aviso de Entrada de Produção no Automático WCS
  IF simulacao NE 'X' AND  gs_reg-hukey1 IS NOT INITIAL.
    lv_lznum = gs_reg-hukey1.

    CALL FUNCTION 'ZWM_CREATE_TO_CHD_WCS_IN'
      EXPORTING
        i_lgnum  = gs_reg-lgnum
        i_lznum  = lv_lznum
        i_linha  = gt_registo-linha
      TABLES
        t_return = et_messages
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF sy-subrc <> 0.
*      retorno = '90'.
*
*      READ TABLE et_messages INDEX 1 INTO ls_message.
*      IF sy-subrc = 0.
*        MESSAGE ID ls_message-msgid TYPE ls_message-msgtyp NUMBER ls_message-msgnr
*            INTO wa_log-msg
*            WITH ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
*      ENDIF.
*
*      wa_log-retorno = retorno.
*      MODIFY zwm_log_efacec FROM wa_log.
*      EXIT.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    EXPORTING
      input  = gs_reg-matnr
    IMPORTING
      output = g_matnr.

  SELECT SINGLE vfdat INTO gs_reg-vfdat
      FROM mcha
          WHERE matnr = gs_reg-matnr
            AND werks = gs_reg-werks
            AND charg = gs_reg-charg.

  WRITE gs_reg-quantidade TO g_quantidade DECIMALS 0.
  CONDENSE g_quantidade.

  CLEAR out_string.

  IF NOT gs_reg-hukey1 IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_reg-hukey1
      IMPORTING
        output = gs_reg-hukey1.
  ENDIF.

  IF NOT gs_reg-hukey2 IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_reg-hukey2
      IMPORTING
        output = gs_reg-hukey2.
  ENDIF.

  MOVE gs_reg-hukey1 TO out_string+0(20).
  MOVE gs_reg-hukey2 TO out_string+20(20).
  MOVE g_matnr       TO out_string+40(18).
  MOVE gs_reg-maktx  TO out_string+58(40).
  MOVE gs_reg-charg  TO out_string+98(10).
  MOVE g_quantidade  TO out_string+108(6).
  MOVE '111'         TO out_string+114(3).
  MOVE gs_reg-makt2  TO out_string+117(40).
  MOVE gs_reg-makt3  TO out_string+157(40).
  MOVE gs_reg-vfdat  TO out_string+197(8).

*** VER MEINS
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = gt_zwm030-meins_p
      language       = sy-langu
    IMPORTING
      output         = gt_zwm030-meins_p
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  MOVE gt_zwm030-meins_p TO out_string+205(3).
****
  MOVE '*'           TO out_string+208(1).

*** Encode EAN128
  PERFORM ean128_encode USING    gs_reg-hukey1
                        CHANGING ean128_cba
                                 ean128_cbb
                                 ean128_cbc
                                 ean128_txa
                                 ean128_txb
                                 ean128_txc.
  ean128_cb1 = ean128_cba.
  ean128_cb2 = ean128_cbb.
  ean128_cb4 = ean128_cbc.
  ean128_tx1 = ean128_txa.
  ean128_tx2 = ean128_txb.
  ean128_tx4 = ean128_txc.

*** Encode EAN128
  PERFORM ean128_encode USING    gs_reg-hukey2
                        CHANGING ean128_cba
                                 ean128_cbb
                                 ean128_cbc
                                 ean128_txa
                                 ean128_txb
                                 ean128_txc.

  ean128_cb3 = ean128_cbb.
  ean128_tx3 = ean128_txb.


  CONCATENATE ean128_cb1 ean128_cb2 ean128_cb3 ean128_cb4
         INTO wa_log-out_string_cb
         SEPARATED BY space.

  CONCATENATE ean128_tx1 ean128_tx2 ean128_tx3 ean128_tx4
         INTO wa_log-out_string_tx
         SEPARATED BY space.

  wa_log-out_string = out_string.
  wa_log-retorno = retorno.
  wa_log-meins = gs_zwm030-meins_p.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_reg-hukey1
    IMPORTING
      output = wa_log-sscc.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_reg-hukey2
    IMPORTING
      output = wa_log-sscc1.

  IF nao_imprime = 'X'.
    wa_log-msg = 'ENTRADA MANUAL SEM CRIACAO DE SSCC'.
  ENDIF.

  wa_log-mesa = l_porta.

** Get equipa e turno
  CLEAR lt_z02rpsessao.
  REFRESH lt_z02rpsessao.
  SELECT * INTO TABLE lt_z02rpsessao FROM z02rpsessao
          WHERE ( aufnr1 = wa_log-aufnr
             OR   aufnr2 = wa_log-aufnr  ).

  SORT lt_z02rpsessao BY datum DESCENDING uzeit DESCENDING.

  READ TABLE lt_z02rpsessao INDEX 1.
  MOVE lt_z02rpsessao-equipa TO wa_log-equipa.
  MOVE lt_z02rpsessao-turno  TO wa_log-turno.
  MOVE lt_z02rpsessao-pernr  TO wa_log-pernr.
  MODIFY zwm_log_efacec FROM wa_log.

  CLEAR: simulacao.
ENDFUNCTION.
