*&---------------------------------------------------------------------*
*&  Include           Z10MM05F_V1                                      *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  LE_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM le_picking.
  DATA wa_linha TYPE t_linha.
* tabi com lotes
  PERFORM cria_tabi_guias.
* tabi com lotes agrupados
  PERFORM cria_tabi_guias_por_item.
* carrega dados da encomenda e oferta
  PERFORM carrega_dados_enc_of.        " nova versão
* calcula total por linha vendida (quantidade total vendida + oferecida)
  PERFORM calc_tot_por_linha_vendida.
  READ TABLE tabi_guias_por_item INDEX 1.
* num. guias
  DESCRIBE TABLE tabi_guias_por_item LINES num_guias.
  tabix = 1.
  PERFORM carrega_guia_ecra.
ENDFORM.                               " LE_PICKING

*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 19.06.2012 15:41:06
*  Motivo: Retorna LIPS
*--------------------------------------------------------------------*
  DO 1 TIMES.
    CLEAR: gt_lips.

    CHECK NOT tabi_guias_por_item IS INITIAL.

    SELECT * FROM lips
       INTO TABLE gt_lips
       FOR ALL ENTRIES IN tabi_guias_por_item
       WHERE vbeln = tabi_guias_por_item-guia.
  ENDDO.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

  PERFORM bi_conf_picking.
  PERFORM inicializa.
ENDFORM.                               " SAVE

*&---------------------------------------------------------------------*
*&      Form  REC_OF_MERCADORIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rec_of_mercadoria.
  CALL FUNCTION 'Z_1OM_CALC_OFERTA_MERCADORIA'
    EXPORTING
      tipo_documento     = 'P'
    TABLES
      tabi_of_mercadoria = tabi_guias_por_item-linha
    EXCEPTIONS
      OTHERS             = 1.
  DESCRIBE TABLE tabi_guias_por_item-linha LINES tab_lines.
  IF sy-subrc NE 0.
    MESSAGE i014(z1).
  ELSE.
    CLEAR quant_alterada.
    tabi_guias_por_item-oferta_processada = 'X'.
*-----------controlar var tabix
    MODIFY tabi_guias_por_item INDEX tabix.

  ENDIF.
ENDFORM.                               " REC_OF_MERCADORIA

*&---------------------------------------------------------------------*
*&      Form  CRIA_TABI_GUIAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_tabi_guias.
* cria tabela interna com guias do picking
* só com linhas não confirmadas (por lote)
  DATA: wa_linha TYPE t_linha.

  DATA: BEGIN OF documentos OCCURS 0,
          doc TYPE vbeln_vl,
        END OF documentos.

  DATA: bloq_g,
        user_bloq LIKE seqg3-gname.

  REFRESH tabi_guias.
  CLEAR: zwmlog, zwmlog02, t311.

*-------------------------------Validar que foi introduzido grupo de WM
  SELECT SINGLE * FROM  t311
*         WHERE  lgnum  = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:46:08
         WHERE  lgnum  = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:46:11
         AND    refnr  = vbss-sammg.

  IF sy-subrc NE 0.

    CLEAR text.
    text = 'Não é Grupo WM'(002).

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

    LEAVE TO SCREEN 100.

  ENDIF.

  CHECK sy-subrc = 0.

*---------------------------------------------------------------------
  SELECT * FROM vbss WHERE sammg = vbss-sammg.
* lista de picking (guias do picking)
    SELECT SINGLE vbeln wadat kunnr  INTO CORRESPONDING FIELDS OF likp
                                        FROM likp
                                        WHERE vbeln = vbss-vbeln.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING likp TO tabi_guias.

* recebedor da mercadoria
    SELECT SINGLE * FROM kna1 WHERE kunnr = likp-kunnr.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING kna1 TO tabi_guias.
    ENDIF.

* itens da guia
    REFRESH tabi_guias-linha. CLEAR vbuk.       CLEAR zwmlog.

    SELECT SINGLE vbeln pkstk wbstk INTO (vbuk-vbeln,
                                          vbuk-pkstk,
                                          vbuk-wbstk) FROM vbuk
                                  WHERE vbeln = vbss-vbeln
                                  AND   kostk = z1om_kostk
                                  AND   lvstk = z1om_lvstk. "ins Abr2005

    IF sy-subrc NE 0.
*----------Se o status for dif C verificar se já foi processado
      CLEAR zwmlog.
      SELECT SINGLE * FROM  zwmlog
             WHERE  sammg    = vbss-sammg
             AND    vbeln    = vbss-vbeln
             AND    zztpmsg  = 'E'.

      IF sy-subrc NE 0.
        CLEAR vttk-daten. g_rc = 4.
*-----Se a remessa não tem status correcto nenhuma remessa é processada

*------------------------------------Validar o Status Global de Picking
        PERFORM mensagem_status USING 'kostk' vbuk-kostk.
      ENDIF.
    ENDIF.

*-------------------------------Validar o Status de Saida de Mercadoria
    IF vbuk-wbstk EQ 'C'.

      PERFORM mensagem_status USING 'wbstk' vbuk-wbstk.
    ENDIF.
*---------------------------------------Validar o Status de Embalamento
    IF vbuk-pkstk EQ 'C'.

      PERFORM mensagem_status USING 'pkstk' vbuk-pkstk.
    ENDIF.

    CHECK vbuk-pkstk NE 'C'.
*----------------------------------------------------------------------


    IF zwmlog-zztpmsg = 'E'.

      SELECT * FROM zwmlipslog INTO lips WHERE vbeln = vbss-vbeln.

        IF NOT lips-charg IS INITIAL.
* linhas com lotes cosiderar o nº item superior sem lotes
          lips-posnr = lips-uecha.
        ENDIF.
* dados do item da guia
        CHECK lips-lfimg NE 0 OR lips-pstyv = z1om_pstyv_oferta.
* linhas com quantidade e linhas de oferta (mesmo sem quantidade)
        MOVE-CORRESPONDING lips TO wa_linha.
        APPEND wa_linha TO tabi_guias-linha.

      ENDSELECT.

    ELSE.

      SELECT * FROM lips WHERE vbeln = vbss-vbeln.

        IF NOT lips-charg IS INITIAL.
* linhas com lotes cosiderar o nº item superior sem lotes
          lips-posnr = lips-uecha.
        ENDIF.
* dados do item da guia
        CHECK lips-lfimg NE 0 OR lips-pstyv = z1om_pstyv_oferta.
* linhas com quantidade e linhas de oferta (mesmo sem quantidade)
        MOVE-CORRESPONDING lips TO wa_linha.
        APPEND wa_linha TO tabi_guias-linha.

      ENDSELECT.

    ENDIF.


    IF NOT tabi_guias-linha[] IS INITIAL.
      APPEND tabi_guias.
    ENDIF.

  ENDSELECT.


*----------------------sempre que a um uge for dif da uni. med. de venda
*----------------------vamos converter as qtd
  LOOP AT tabi_guias.
    CLEAR idx_lips. idx_lips = sy-tabix.
    LOOP AT tabi_guias-linha INTO wa_linha.

      MOVE-CORRESPONDING wa_linha TO z1com_s.

      CLEAR: mara, l_tabix. l_tabix = sy-tabix.

      SELECT SINGLE meins INTO mara-meins FROM  mara
             WHERE  matnr  = z1com_s-matnr.

      IF mara-meins NE z1com_s-vrkme.

        PERFORM converte_uni_med USING    z1com_s-lfimg z1com_s-vrkme
                                          mara-meins    z1com_s-matnr
                                 CHANGING wa_linha-lfimg wa_linha-vrkme.

        MODIFY tabi_guias-linha INDEX l_tabix FROM wa_linha .

      ENDIF.


    ENDLOOP.
    MODIFY tabi_guias INDEX idx_lips.
  ENDLOOP.


  IF tabi_guias[] IS INITIAL.

    CLEAR text.
    text = 'Grupo sem Remessas Validas para processo'(003).

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

    LEAVE TO SCREEN 100.

** FL -> 24/11/2005
  ELSE.
** Verifica bloqueios para guias e ordens de venda
    CLEAR:   documentos.
    REFRESH: documentos.

    LOOP AT tabi_guias.
      documentos-doc = tabi_guias-vbeln.
      APPEND documentos.
      CLEAR documentos.
      LOOP AT tabi_guias-linha INTO wa_linha.
        documentos-doc = wa_linha-vgbel.
        APPEND documentos.
        CLEAR documentos.
      ENDLOOP.
    ENDLOOP.

    SORT documentos BY doc.
    DELETE documentos WHERE doc IS INITIAL.
    DELETE ADJACENT DUPLICATES FROM documentos COMPARING doc.

    LOOP AT documentos.
      PERFORM bloqueio_global USING documentos-doc CHANGING bloq_g
                                                            user_bloq.
      IF NOT bloq_g IS INITIAL.
        CONCATENATE 'Doc.'(004) documentos-doc 'bloqueado por'(005) user_bloq
               INTO text SEPARATED BY space.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '000'
            message_var1   = text.
        LEAVE TO SCREEN 100.

      ENDIF.
    ENDLOOP.

** FL <- 24/11/2005
  ENDIF.


  CLEAR: l_tabix, idx_lips.
ENDFORM.                               " CRIA_TABI_GUIAS

*&---------------------------------------------------------------------*
*&      Form  CRIA_TABI_GUIAS_POR_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_tabi_guias_por_item.
* cria tabela interna apartir da tabela interna tabi_guias
* com itens agrupados (sem lotes)
  DATA wa_linha TYPE t_linha.
  REFRESH tabi_guias_por_item. CLEAR tabi_guias_por_item.
  LOOP AT tabi_guias.
    tabi_guias_por_item-guia = tabi_guias-vbeln.
*ROFF::SDF::CDFR::24/07/2020
*    MOVE-CORRESPONDING tabi_guias TO tabi_guias_por_item.

    tabi_guias_por_item-guia = tabi_guias-vbeln.
    tabi_guias_por_item-vbelv = tabi_guias-kunnr.
    tabi_guias_por_item-kunnr = tabi_guias-vbeln.
    tabi_guias_por_item-name1 = tabi_guias-name1.
    tabi_guias_por_item-ort01 = tabi_guias-ort01.
*ROFF::SDF::CDFR::24/07/2020
    REFRESH tabi_guias_por_item-linha.
    LOOP AT tabi_guias-linha INTO wa_linha.
* dados dos itens da guia
      MOVE-CORRESPONDING wa_linha TO z1com_s.
      z1com_s-quant_cf = z1com_s-lfimg.
      z1com_s-vbeln = wa_linha-vgbel.
      z1com_s-vgpos = wa_linha-vgpos.
      COLLECT z1com_s INTO tabi_guias_por_item-linha.
    ENDLOOP.
    CHECK sy-subrc = 0.
    SORT tabi_guias_por_item-linha BY vbeln DESCENDING vgpos.
    APPEND tabi_guias_por_item.
  ENDLOOP.

ENDFORM.                               " CRIA_TABI_GUIAS_POR_ITEM

*&---------------------------------------------------------------------*
*&      Form  CALC_TOT_POR_LINHA_VENDIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_tot_por_linha_vendida.
  DATA: quantidade_conf_of LIKE z1com_s-quant_cf,
        wa_linha           LIKE z1com_s.
  LOOP AT tabi_guias_por_item.
    LOOP AT tabi_guias_por_item-linha INTO wa_linha.
      CHECK wa_linha-pstyv EQ z1om_pstyv_oferta.
      quantidade_conf_of = wa_linha-quant_cf.
* item filho
      READ TABLE tabi_guias_por_item-linha INTO wa_linha
                                 WITH KEY vgpos = wa_linha-uepos.
      IF sy-subrc = 0.
* item pai
        wa_linha-total_cf = quantidade_conf_of + wa_linha-quant_cf.
        wa_linha-total_cfi = wa_linha-total_cf.
        wa_linha-itm_oferta = 'X'.      " item com linha oferta
        MODIFY tabi_guias_por_item-linha FROM wa_linha INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
    MODIFY tabi_guias_por_item.
  ENDLOOP.

*-----------------------------------------------------------------
ENDFORM.                               " CALC_TOT_POR_LINHA_VENDIDA

*&---------------------------------------------------------------------*
*&      Form  BI_CONF_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bi_conf_picking.
  DATA: lv_2step TYPE flag.

  DATA: l_erro,
        flag_guia_alterada,
        num_encomenda      LIKE vbak-vbeln,
        erro_guia.

  DATA: BEGIN OF it_vbuk OCCURS 0,
          vbeln LIKE vbuk-vbeln,
          pkstk LIKE vbuk-vbeln,
        END OF it_vbuk.

  DATA: it_aux LIKE tabi_guias_por_item OCCURS 0 WITH HEADER LINE.

  DATA: lt_messages TYPE tab_bdcmsgcoll,
        lv_error    TYPE flag,
        lv_subrc    TYPE sysubrc,
        lv_type     TYPE c.

  REFRESH: tabi_creditos, it_zwmlog02.
  CLEAR:   tabi_creditos, z1pom_log, z_tabix, zwmlog, zwmlog02,
           it_zwmlog02, erro_guia, g_erro.

  CLEAR: gt_vepo.

  g_bloq = 2.

  CLEAR: gt_vbeln_skip.

  LOOP AT tabi_guias_por_item WHERE guia_bloqueada    = ' '
                              AND   oferta_processada ='X'.

    IF NOT g_erro IS INITIAL.
      erro_guia = 'X'.
    ENDIF.

    CLEAR: zwmlog, g_erro.
    z_tabix = sy-tabix.

    PERFORM desembalar.

*----------------------------------Verifica se a remessa está bloqueada
    WHILE g_bloq = 2.
      PERFORM gr_bloqueado USING 'INI_PROC' CHANGING g_bloq.
    ENDWHILE.
*----------------------------------------MODIFICAR ITEM DO FORNECIMENTO
*======================================================================
    PERFORM modifica_guia_2 USING flag_guia_alterada num_encomenda.
*======================================================================
*----------------------------------se houver erro deve parar o processo
    IF NOT g_erro IS INITIAL.
      CONTINUE.
    ENDIF.

*----------------------------------Verifica se a remessa está bloqueada
    g_bloq = 2.
    WHILE g_bloq = 2.
      PERFORM gr_bloqueado USING 'REM_SAVE' CHANGING g_bloq.
    ENDWHILE.
*-----------------------------------------------------PROCESSA CREDITOS
    PERFORM cria_tabi_creditos.

*-------------------------------------CHAMADA A VL02 PARA MODIFICAR QTD
    CHECK g_erro IS INITIAL.

***--------------------------------------------------PROCESSA CREDITOS

*----após processar todos os items vai criar as OT, HU, e o Crédito
    AT END OF guia.

      CHECK g_erro IS INITIAL.
*-----------Criamos novas OT´s para gerar atribuição de lotes aos items

*      IF NOT it_ot[] IS INITIAL.
*        PERFORM cria_ot.
*      ENDIF.

      CHECK g_erro IS INITIAL.

*-------------------------------------no final associamos as embalagens

*----------------Os clientes servisan que estão costumizados na tabela
*----------------zwm039 invalidam a embalagem das remessas onde existem
*----------------como parceiros AG
      CALL FUNCTION 'ZWM_DEBUG_ONLINE'.


      PERFORM verifica_servisan USING g_rc.
      IF NOT g_erro IS INITIAL.
        RETURN.
      ENDIF.

      IF g_rc IS INITIAL.
        PERFORM cria_embalagens.
        tabi_guias_por_item-servis = 'X'.
        MODIFY tabi_guias_por_item INDEX z_tabix.
      ELSE.
        PERFORM pack_in_transportation_append.
        CLEAR g_rc.

        CHECK g_erro IS INITIAL.

*--> Valida se é em 2 Passos
        CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
          EXPORTING
*           i_lgnum = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:46:27
            i_lgnum = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:46:28
            i_refnr = vbss-sammg
          IMPORTING
            e_2step = lv_2step
          EXCEPTIONS
            error   = 1
            OTHERS  = 2.

        IF lv_2step EQ abap_true.
          EXIT.
        ENDIF.

      ENDIF.

*--------------------------------------------------------cria o crédito
      CHECK g_erro IS INITIAL.

*      PERFORM modifica_encomenda USING l_erro.

    ENDAT.

  ENDLOOP.

  CHECK g_erro IS INITIAL.

  PERFORM cria_linha_de_remessa USING ''.

  PERFORM pack_in_transportation.
  CHECK g_erro IS INITIAL.

*  BREAK-POINT.

*  PERFORM cria_linha_de_remessa USING ''.
* PERFORM msg_embalamento.

** Validação de status de embalamento
  it_aux[] = tabi_guias_por_item[].
  DELETE it_aux WHERE servis IS INITIAL.

  IF NOT it_aux[] IS INITIAL.
    CLEAR   it_vbuk.
    REFRESH it_vbuk.
    SELECT vbeln pkstk FROM vbuk
            INTO CORRESPONDING FIELDS OF TABLE it_vbuk
             FOR ALL ENTRIES IN it_aux
           WHERE vbeln EQ it_aux-guia.

    SORT it_vbuk BY vbeln.
  ENDIF.

  LOOP AT it_aux.
    CLEAR it_vbuk.
    READ TABLE it_vbuk WITH KEY vbeln = it_aux-guia.
    CHECK it_vbuk-pkstk <> 'C'.

    PERFORM check_packing_skip USING it_aux-guia CHANGING lv_subrc lv_type.
    CHECK lv_subrc EQ 0.


    CONCATENATE 'Remessa'(006) it_aux-guia 'sem embalamento finalizado!'(007)
           INTO text SEPARATED BY space.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'S'
        message_number = '000'
        message_var1   = text.
  ENDLOOP.

  FREE it_aux.
**

***********************************************************************
** Criação de Numeração de Caixas - Paletes Multi Destino
***********************************************************************
  IF erro_guia IS INITIAL.
    CALL FUNCTION 'Z_GRUPO_MULTI_DESTINO'
      EXPORTING
        grupo = vbss-sammg.
  ENDIF.
***********************************************************************
**
***********************************************************************

** Embalamento Final
***********************************************************************
  PERFORM pack_transportation_final.

** Saida de Paletes para operador logistico

*  PERFORM saida_pal_operador.

** Mensagem
***********************************************************************
  PERFORM msg USING erro_guia. "g_erro.

ENDFORM.                               " BI_CONF_PICKING

*&---------------------------------------------------------------------*
*&      Form  FUNC_F11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM func_f11.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/11'.
  APPEND tabi_bdcdata.
ENDFORM.                                                    " FUNC_F11

*&---------------------------------------------------------------------*
*&      Form  CARREGA_GUIA_ECRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_guia_ecra.
  READ TABLE tabi_guias_por_item INDEX tabix.
  MOVE-CORRESPONDING tabi_guias_por_item TO likp.
  MOVE-CORRESPONDING tabi_guias_por_item TO kna1.
  likp-vbeln = tabi_guias_por_item-guia.
  DESCRIBE TABLE tabi_guias_por_item-linha LINES tab_lines.
  CLEAR page_begin.
ENDFORM.                               " CARREGA_GUIA_ECRA

*&---------------------------------------------------------------------*
*&      Form  CARREGA_DADOS_ENC_OF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_dados_enc_of.
  DATA wa_linha LIKE z1com_s.
  LOOP AT tabi_guias_por_item.
    LOOP AT tabi_guias_por_item-linha INTO wa_linha.
* dados da oferta
      MOVE-CORRESPONDING wa_linha TO z1com_s.
      CLEAR vbap.
      SELECT SINGLE pstyv werks netpr waerk uepos zzoferta zzprliq abgru
               INTO (vbap-pstyv, vbap-werks, vbap-netpr, vbap-waerk,
                     vbap-uepos, vbap-zzoferta,
                     vbap-zzprliq, vbap-abgru)
                                FROM vbap
                                WHERE vbeln = z1com_s-vbeln
                                AND   posnr = z1com_s-vgpos.
*----------------------------------------------------------Ins Mai2005
      IF NOT vbap-abgru IS INITIAL AND vbap-pstyv = 'ZOF'.

        CLEAR: tabi_guias_por_item, tabi_guias_por_item[],
               tabi_guias, tabi_guias[].
        CLEAR text.

        CONCATENATE 'Encomenda'(008) z1com_s-vbeln z1com_s-vgpos
                    'com motivo recusa'(009) vbap-abgru INTO text
                    SEPARATED BY space.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '000'
            message_var1   = text.

        LEAVE TO SCREEN 100.


      ELSE.
*---------------------------------------------------------------
        z1com_s-oferta = vbap-zzoferta.
        z1com_s-prliq = vbap-zzprliq.
        z1com_s-pstyv = vbap-pstyv.
        z1com_s-werks = vbap-werks.
        z1com_s-netpr = vbap-netpr.
        z1com_s-waerk = vbap-waerk.
        z1com_s-uepos = vbap-uepos.
        z1com_s-total_cf = z1com_s-quant_cf.
        IF NOT z1com_s-uepos IS INITIAL.
* itens de oferta
          CLEAR z1com_s-total_cf.
        ENDIF.
        z1com_s-total_cfi = z1com_s-total_cf.
        MODIFY tabi_guias_por_item-linha FROM z1com_s.

      ENDIF.
    ENDLOOP.
    MODIFY tabi_guias_por_item.
  ENDLOOP.
ENDFORM.                               " CARREGA_DADOS_ENC_OF

*&---------------------------------------------------------------------*
*&      Form  MODIFICA_ENCOMENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modifica_encomenda USING p_erro.

  SORT tabi_creditos BY vbeln vgpos.

  LOOP AT tabi_creditos.
    AT NEW vbeln.
      REFRESH: tabi_bdcdata, it_creditos_log.
      PERFORM ecra_inicial_enc.
    ENDAT.

    PERFORM posicionar_item_enc.
    PERFORM ins_condicao.
    PERFORM log_credito.

    AT END OF vbeln.
      PERFORM func_f11.
      PERFORM call_mod_encomenda USING p_erro.
    ENDAT.
  ENDLOOP.

  CLEAR: tabi_creditos, tabi_creditos[], tabi_bdcdata, tabi_bdcdata[],
         it_msg, it_msg[].
ENDFORM.                               " MODIFICA_ENCOMENDA

*&---------------------------------------------------------------------*
*&      Form  CALL_MOD_ENCOMENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_mod_encomenda USING p_erro.
  DATA mode_bi.

  GET PARAMETER ID 'ZBI' FIELD mode_bi.
  IF sy-subrc NE 0 OR mode_bi IS INITIAL.
    mode_bi = 'N'.
  ENDIF.

  CLEAR : it_msg, it_msg[].

  CALL TRANSACTION 'VA02' USING tabi_bdcdata
                          UPDATE 'S'
                          MODE mode_bi
                          MESSAGES INTO it_msg.
  CLEAR l_zztpdoc.
  l_zztpdoc = 'ORD_CREDI'.

  PERFORM log USING tabi_creditos-vbeln ' '.

  IF it_msg-msgtyp = 'S'.
    COMMIT WORK AND WAIT.
  ELSE.
    PERFORM gera_reg_log USING 'CR' ' '.
  ENDIF.

ENDFORM.                               " CALL_MOD_ENCOMENDA

*&---------------------------------------------------------------------*
*&      Form  ECRA_INICIAL_ENC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ecra_inicial_enc.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV45A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0102'.
  APPEND tabi_bdcdata.

  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'VBAK-VBELN'.
  tabi_bdcdata-fval = tabi_creditos-vbeln.
  APPEND tabi_bdcdata.

  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV45A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '4001'.
  APPEND tabi_bdcdata.
ENDFORM.                               " ECRA_INICIAL_ENC

*&---------------------------------------------------------------------*
*&      Form  POSICIONAR_ITEM_ENC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posicionar_item_enc.
* posicionar
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = 'POPO'.
  APPEND tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV45A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0251'.
  APPEND tabi_bdcdata.
* nº item
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'RV45A-POSNR'.
  tabi_bdcdata-fval = tabi_creditos-vgpos.
  APPEND tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV45A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '4001'.
  APPEND tabi_bdcdata.
* marcar linha
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'RV45A-VBAP_SELKZ(1)'.
  tabi_bdcdata-fval = 'X'.
  APPEND tabi_bdcdata.

ENDFORM.                               " POSICIONAR_ITEM_ENC

*&---------------------------------------------------------------------*
*&      Form  INS_CONDICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ins_condicao.
  DATA: valor_char(16).

* condições
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = 'PKO1'.
  APPEND tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV45A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '5003'.
  APPEND tabi_bdcdata.
* criar
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = 'V69A_KOAN'.

  APPEND tabi_bdcdata.

  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV45A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '5003'.
  APPEND tabi_bdcdata.
* condição crédito
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'KOMV-KSCHL(2)'.
  tabi_bdcdata-fval = z1om_kschl_credito.
  APPEND tabi_bdcdata.
* montante
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'KOMV-KBETR(2)'.
  WRITE tabi_creditos-kbetr TO valor_char CURRENCY tabi_creditos-waerk.
  tabi_bdcdata-fval = valor_char.
  APPEND tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV45A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '5003'.
  APPEND tabi_bdcdata.
* voltar
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/3'.
  APPEND tabi_bdcdata.
* ecrã principal
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV45A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '4001'.
  APPEND tabi_bdcdata.

ENDFORM.                               " INS_CONDICAO

*&---------------------------------------------------------------------*
*&      Form  CRIA_TABI_CREDITOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_tabi_creditos.
  DATA: valor_char(16),
        valor_p        TYPE p DECIMALS 0,
        valor_p2       TYPE p DECIMALS 2.
  DATA i TYPE i.

  CLEAR: tabi_creditos, tabi_creditos[].

  LOOP AT tabi_guias_por_item-linha INTO z1com_s.
    CHECK z1com_s-quant_cr NE 0.

    MOVE-CORRESPONDING z1com_s TO tabi_creditos.
    SELECT SINGLE * FROM tcurx WHERE currkey = tabi_creditos-waerk.
    IF sy-subrc NE 0.
      i = 0.
    ELSE.
      i = 2 - tcurx-currdec.
    ENDIF.
    IF i = 2.                          " 0 casas decimais
      valor_p = z1com_s-quant_cr * z1com_s-prliq * 100.
      tabi_creditos-kbetr = valor_p.
    ELSE.
      valor_p2 =  z1com_s-prliq * 10 ** i.
      valor_p2 = z1com_s-quant_cr * valor_p2.
      WRITE valor_p2 TO tabi_creditos-kbetr CURRENCY tabi_creditos-waerk
      .
    ENDIF.
    tabi_creditos-guia = tabi_guias_por_item-guia.
    COLLECT tabi_creditos.

  ENDLOOP.
ENDFORM.                               " CRIA_TABI_CREDITOS

*&---------------------------------------------------------------------*
*&      Form  LOCK_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lock_carga.

  SELECT SINGLE * FROM  t311
*         WHERE  lgnum  = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:46:43
         WHERE  lgnum  = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:46:44
         AND    refnr  = vbss-sammg.

  IF sy-subrc NE 0.
    CLEAR: text.
    CONCATENATE vbss-sammg 'Não é Grupo WM'(002) INTO text
      SEPARATED BY space.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

    LEAVE TO SCREEN 100.
  ENDIF.

  IF old_group IS INITIAL.
    old_group = vbss-sammg.
  ELSE.
    IF old_group NE vbss-sammg.

      CALL FUNCTION 'DEQUEUE_EVVBSK'
        EXPORTING
          sammg          = old_group
        EXCEPTIONS
          system_failure = 8.

      old_group = vbss-sammg.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ENQUEUE_EVVBSK'
    EXPORTING
      sammg          = vbss-sammg
    EXCEPTIONS
      foreign_lock   = 4
      system_failure = 8.

  IF sy-subrc NE 0.
    CLEAR: text, old_group.
    CONCATENATE 'Grupo'(010) vbss-sammg 'já em processamento'(011) INTO text
      SEPARATED BY space.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

    LEAVE TO SCREEN 100.
  ENDIF.

ENDFORM.                               " LOCK_CARGA

*&---------------------------------------------------------------------*
*&      Form  DESEMBALAR
*&---------------------------------------------------------------------*
*       Desembalar HU via bapi
*----------------------------------------------------------------------*
FORM desembalar .

*antes de desmbalar guardamos o histórico de embalagens do fornecimento
  CLEAR : it_hu, it_hu[], it_lips_old, it_lips_old[],
          it_lips_aux, it_lips_aux[], it_zwmlog, it_zwmlog[].


  SELECT * INTO TABLE it_zwmlog FROM  zwmlog
         WHERE  sammg  = vbss-sammg
         AND    vbeln  = tabi_guias_por_item-guia.

*---quando reprocessamos ver tabela zwmlipslog------------ins Abr2005
  READ TABLE it_zwmlog WITH KEY zztpmsg = 'E'.

  IF sy-subrc = 0.
*guardar a informação do fornecimento antes de qualquer modificação
    SELECT  * APPENDING CORRESPONDING FIELDS OF TABLE it_lips_aux
              FROM  zwmlipslog
           WHERE  vbeln  = tabi_guias_por_item-guia.

  ELSE.
*--------------------------------------------------------------------


*guardar a informação do fornecimento antes de qualquer modificação
    SELECT  * APPENDING CORRESPONDING FIELDS OF TABLE it_lips_aux
              FROM  lips
           WHERE  vbeln  = tabi_guias_por_item-guia.

*Após processar correctamente todos os item da remessa podemos eliminar
*a entrada na zwmlipslog.
    IF NOT it_lips_aux[] IS INITIAL.
      SELECT vbeln INTO zwmlipslog-vbeln FROM  zwmlipslog UP TO 1 ROWS
             WHERE  vbeln  = tabi_guias_por_item-guia.

      ENDSELECT.
      IF sy-subrc NE 0.
        INSERT zwmlipslog FROM TABLE it_lips_aux.
      ENDIF.
    ENDIF.
*----------------------------------------------------------ins Abr2005
  ENDIF.
*-------------------------------------------------------------end ins
  LOOP AT it_lips_aux.

    CLEAR idx_lips. idx_lips = sy-tabix.

    MOVE-CORRESPONDING it_lips_aux TO it_lips_old.
    APPEND it_lips_old.

    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = it_lips_aux-matnr
        i_in_me              = it_lips_aux-vrkme
        i_out_me             = it_lips_aux-meins
        i_menge              = it_lips_aux-lfimg
      IMPORTING
        e_menge              = it_lips_aux-lfimg
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.

    IF sy-subrc EQ 0.
      MODIFY it_lips_aux INDEX idx_lips.
    ENDIF.

  ENDLOOP.

  CLEAR idx_lips.

*------------------------------------------------------
ENDFORM.                    " DESEMBALAR

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  CRIA_EMBALAGENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cria_embalagens .

  PERFORM get_paletes.
  PERFORM associa_paletes_remessas.

ENDFORM.                    " CRIA_EMBALAGENS

*&---------------------------------------------------------------------*
*&      Form  CRIA_OT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cria_ot .

  CLEAR : bdcdata, bdcdata[], l_tabix, l_num.
  CLEAR : it_lips, it_lips[], it_lqua, it_lqua[].

*-----------------------------Determinar quais os lotes e qtd a usar
  PERFORM determina_lote_para_ot.

*-------------------------PROCESSAR O FORNECIMENTO POR ITEM
  PERFORM gera_ot.

  CLEAR : bdcdata, bdcdata[], l_tabix, l_num.
  CLEAR : it_lips, it_lips[], it_lqua, it_lqua[], bi_tab, bi_tab[],
          it_ot, it_ot[].

ENDFORM.                    " CRIA_OT

*&---------------------------------------------------------------------*
*&      Form  determina_lote_para_ot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM determina_lote_para_ot .

  CLEAR : it_lips, it_lips[], it_lqua, it_lqua[].
*--------------------------------------As QTD DISPONIVEIS Para cada LOTE
  SELECT * INTO TABLE it_lqua FROM  lqua
*         WHERE  lgnum  = z1om_lgnum                         "'100' " << DEL ROFF(SDF):TMGP:06.01.2016 16:47:19
         WHERE  lgnum  = gv_lgnum                         "'100' " << INS ROFF(SDF):TMGP:06.01.2016 16:47:20
         AND    vbeln  = tabi_guias_por_item-guia
         AND    lgtyp  = z1om_lgtyp.                        "'916'.


*------------------seleccionar os items do fornecimento após o recalculo
  SELECT  * APPENDING CORRESPONDING FIELDS OF TABLE it_lips
            FROM  lips
         WHERE  vbeln  = tabi_guias_por_item-guia.

*--Só com ocorrencia de erro-------------------------------------Abr2005
  IF it_lips[] IS INITIAL.

    LOOP AT it_ot.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF it_lips
                      FROM  zwmlipslog
             WHERE  vbeln  = it_ot-vbeln
             AND    posnr  = it_ot-posnr.

      it_lips-lfimg = zwmlog02-lfimg.
      APPEND it_lips.
    ENDLOOP.
  ENDIF.
*--------------------------------------------------------------

*----------------------------------filtrar os items que vão criar items
  SORT it_lips_old BY posnr ASCENDING.
  SORT it_lips BY posnr ASCENDING.
*------------------------***************--------------------------------
*---

  LOOP AT it_lips.

    CLEAR idx_lips. idx_lips = sy-tabix.
    READ TABLE it_ot WITH KEY posnr = it_lips-posnr
                              vbeln = it_lips-vbeln.
    IF sy-subrc NE 0.
      DELETE it_lips INDEX idx_lips.
      DELETE it_lips_old INDEX idx_lips.

    ENDIF.

  ENDLOOP.
  CLEAR idx_lips.

  SORT it_lips_old BY vgpos ASCENDING.
  SORT it_lips BY posnr ASCENDING.

*----------------------------------------Recuperar info do fornecimento
*-----------seleccionamos um lote na tab it_lips_old, para um dado item

  LOOP AT it_lips_old WHERE vgpos GT 0
                      AND   charg NE space.

    READ TABLE it_lips WITH KEY vbeln = it_lips_old-vbeln
                                matnr = it_lips_old-matnr
                                posnr = it_lips_old-vgpos.

    CHECK it_lips-lfimg GT 0 AND sy-subrc = 0.

    CLEAR : idx_lips, bi_tab.

    idx_lips = sy-tabix.


*-------------------------escluir os items de oferta sem item superior
    CHECK NOT ( it_lips-uepos IS INITIAL AND it_lips-pstyv = 'ZOF' ).


    CLEAR l_tabix. l_tabix = sy-tabix.
*--------seleccionar os lotes que satisfazem as necessidades deste item
    LOOP AT it_lqua WHERE matnr = it_lips-matnr
                    AND   lgtyp = it_lips-lgtyp
                    AND   vbeln = it_lips-vbeln
                    AND   charg = it_lips_old-charg
                    AND   verme GT 0.

      IF sy-subrc = 0.

        CLEAR idx_lqua. idx_lqua = sy-tabix.
*----------------------------verifica se a qtd do lote é suficiente

*------------------------Se qtd do fornecimento menor que a qtd do lote
        IF it_lips-lfimg LE it_lqua-verme AND it_lqua-verme GT 0.

*--------------------------------------cria entrada de lote para lt03
          bi_tab-vbeln = it_lips-vbeln.
          bi_tab-posnr = it_lips-posnr.
          bi_tab-lgtyp = it_lips-lgtyp.
          bi_tab-lfimg = it_lips-lfimg.
          bi_tab-charg = it_lqua-charg.

          it_lqua-verme = it_lqua-verme - it_lips-lfimg.

          CLEAR it_lips-lfimg.

          APPEND bi_tab. CLEAR: bi_tab.

*----------actualiza a qtd disponivel do lote(it_lqua) e qtd necessária
*----------para o item

          MODIFY it_lqua INDEX idx_lqua.
          MODIFY it_lips INDEX idx_lips.
*--se a qtd do lote for nula não necessitamos de o processar mais vezes


*------------------Se a qtd do fornecimento for maior que a qtd do lote
        ELSEIF it_lips-lfimg GT it_lqua-verme AND it_lqua-verme GT 0.

          it_lips-lfimg = it_lips-lfimg - it_lqua-verme.


          bi_tab-vbeln = it_lips-vbeln.
          bi_tab-posnr = it_lips-posnr.
          bi_tab-lgtyp = it_lips-lgtyp.
          bi_tab-lfimg = it_lqua-verme.
          bi_tab-charg = it_lqua-charg.

          APPEND bi_tab. CLEAR bi_tab.

          CLEAR it_lqua-verme.

*----------actualiza a qtd disponivel do lote
*          MODIFY it_lqua INDEX idx_lqua.
          MODIFY it_lips INDEX idx_lips.
*--se a qtd do lote for nula não necessitamos de o processar mais vezes
          DELETE it_lqua INDEX idx_lqua.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

*quando os lotes associados a um item não são suficientes para
*satisfazer a qtd, vamos procurar um(qualquer) lote disponivel

  PERFORM falta_stock.

ENDFORM.                    " determina_lote_para_ot

*&---------------------------------------------------------------------*
*&      Form  gera_ot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_ot .

  DATA : aa_modo     VALUE 'N', l_anfme(17), campo(30),
         c_anfme(30), c_vltyp(30), c_vlpla(30), c_charg(30).

  CLEAR : bdcdata, bdcdata[], campo, l_zztpdoc, l_idx.

  SORT bi_tab BY posnr ASCENDING.

  LOOP AT bi_tab.

    CLEAR: l_tabix, campo, l_anfme, c_anfme, c_vltyp, c_vlpla, c_charg,
           l_zztpdoc.

    l_tabix = sy-tabix.

    AT NEW posnr.
      ADD 1 TO l_idx.

      READ TABLE  bi_tab INDEX l_tabix. CLEAR : l_num, l_pos.
*------------------------------------Ecram inicial
      PERFORM bdc_dynpro   USING 'SAPML03T'            '0151'.
      PERFORM bdc_field    USING 'BDC_CURSOR'          '*VBLKP-POSNR'.
      PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
*      PERFORM bdc_field    USING 'LTAK-LGNUM'          '100'. " << DEL ROFF(SDF):TMGP:06.01.2016 16:56:03
      PERFORM bdc_field    USING 'LTAK-LGNUM'          gv_lgnum. " << INS ROFF(SDF):TMGP:06.01.2016 16:56:06
      PERFORM bdc_field    USING 'VBLKK-VBELN'         bi_tab-vbeln.
      PERFORM bdc_field    USING '*VBLKP-POSNR'        bi_tab-posnr.
      PERFORM bdc_field    USING 'RL03T-ALAKT'         'X'.

    ENDAT.

    ADD 1 TO l_num.
    ADD 1 TO l_pos.

    CONCATENATE 'LTAPA-CHARG(' l_pos ')' INTO campo.
    CONDENSE campo NO-GAPS.

    CONCATENATE 'LTAPA-ANFME(' l_pos ')' INTO c_anfme.
    CONDENSE c_anfme NO-GAPS.

    CONCATENATE 'LTAPA-VLTYP(' l_pos ')' INTO c_vltyp.
    CONDENSE c_anfme NO-GAPS.

    CONCATENATE 'LTAPA-VLPLA(' l_pos ')' INTO c_vlpla.
    CONDENSE c_anfme NO-GAPS.

    CONCATENATE 'LTAPA-CHARG(' l_pos ')' INTO c_charg.
    CONDENSE c_anfme NO-GAPS.



    WRITE bi_tab-lfimg TO l_anfme UNIT 'UN'.

    PERFORM bdc_dynpro   USING 'SAPML03T'            '0105'.
    PERFORM bdc_field    USING 'BDC_CURSOR'          campo.
    PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
    PERFORM bdc_field    USING 'T334T-LGTY0'         bi_tab-lgtyp.
    PERFORM bdc_field    USING 'T334T-LGTY1'         ''.
    PERFORM bdc_field    USING 'T334T-LGTY2'         ''.
    PERFORM bdc_field    USING 'T334T-LGTY3'         ''.
    PERFORM bdc_field    USING 'T334T-LGTY4'         ''.
    PERFORM bdc_field    USING  c_anfme              l_anfme.
    PERFORM bdc_field    USING  c_vltyp              bi_tab-lgtyp.
    PERFORM bdc_field    USING  c_vlpla              bi_tab-vbeln.
    PERFORM bdc_field    USING  c_charg              bi_tab-charg.
*-------------------ver necessidade da tela
    PERFORM bdc_dynpro   USING 'SAPML03T'            '0105'.
    PERFORM bdc_field    USING 'BDC_CURSOR'          campo.
    PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.

    AT END OF posnr.

      READ TABLE  bi_tab INDEX l_tabix.

      PERFORM bdc_dynpro   USING 'SAPML03T'            '0105'.
      PERFORM bdc_field    USING 'BDC_CURSOR'          'T334T-LGTY0'.
      PERFORM bdc_field    USING 'BDC_OKCODE'          '=TAH2'.

*-------------------------------nº de confirmações = nº de lotes usado
      DO l_num TIMES.

        PERFORM bdc_dynpro   USING 'SAPML03T'            '0102'.
        PERFORM bdc_field    USING 'BDC_CURSOR'          'RL03T-SQUIT'.
        PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
        PERFORM bdc_field    USING 'RL03T-SQUIT'         'X'.
        PERFORM bdc_field    USING 'LTAP-VLPLA'          bi_tab-vbeln.
*-------------------ver necessidade da tela
*        PERFORM bdc_dynpro   USING 'SAPML03T'            '0102'.
*        PERFORM bdc_field    USING 'BDC_CURSOR'          'RL03T-SQUIT'.
*        PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.

      ENDDO.

      PERFORM bdc_dynpro   USING 'SAPML03T'            '0105'.
      PERFORM bdc_field    USING 'BDC_CURSOR'          'T334T-LGTY0'.
      PERFORM bdc_field    USING 'BDC_OKCODE'          '=BU'.
      PERFORM bdc_field    USING 'T334T-LGTY0'         '916'.

      CLEAR : it_msg, it_msg[].

      CALL TRANSACTION 'LT03' USING bdcdata
                              UPDATE 's'
                              MODE aa_modo
                              MESSAGES INTO it_msg.
*----------------------------------------------DETERMINAR A OT
      CLEAR l_zztpdoc.
      CONCATENATE 'OT_' l_idx INTO l_zztpdoc.
      CONDENSE l_zztpdoc NO-GAPS.
*------------------------------------------------------------

      PERFORM log USING bi_tab-vbeln bi_tab-posnr.

      IF it_msg-msgtyp = 'S'.
        COMMIT WORK AND WAIT.  WAIT UP TO 5 SECONDS.
      ELSE.
        PERFORM gera_reg_log USING 'OT' ' '.
        PERFORM gera_reg_log USING 'HU' ' '.
        PERFORM gera_reg_log USING 'CR' ' '.

      ENDIF.

      CLEAR : bdcdata, bdcdata[].

    ENDAT.

  ENDLOOP.

ENDFORM.                    " gera_ot

*&---------------------------------------------------------------------*
*&      Form  falta_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM falta_stock .

  SORT it_lqua BY verme DESCENDING.
*-----------------------------------------procurar items não concluidos
  LOOP AT it_lips WHERE lfimg GT 0.
*-------------------------escluir os items de oferta sem item superior
    CHECK NOT ( it_lips-uepos IS INITIAL AND it_lips-pstyv = 'ZOF' ).

    CLEAR idx_lips. idx_lips = sy-tabix.
*-----------------------------------------procurar um lote disponivel

    LOOP AT it_lqua WHERE verme GT 0.

      IF sy-subrc = 0.
        CLEAR idx_lqua. idx_lqua = sy-tabix.

*------------------------Se qtd do fornecimento menor que a qtd do lote
        IF it_lips-lfimg LE it_lqua-verme AND it_lqua-verme GT 0.

*--------------------------------------cria entrada de lote para lt03
          bi_tab-vbeln = it_lips-vbeln.
          bi_tab-posnr = it_lips-posnr.
          bi_tab-lgtyp = it_lips-lgtyp.
          bi_tab-lfimg = it_lips-lfimg.
          bi_tab-charg = it_lqua-charg.

          it_lqua-verme = it_lqua-verme - it_lips-lfimg.

          CLEAR it_lips-lfimg.

          APPEND bi_tab. CLEAR: bi_tab.

*----------actualiza a qtd disponivel do lote(it_lqua) e qtd necessária
*----------para o item

          MODIFY it_lqua INDEX idx_lqua.
          MODIFY it_lips INDEX idx_lips.
*--se a qtd do lote for nula não necessitamos de o processar mais vezes

          EXIT.
*------------------Se a qtd do fornecimento for maior que a qtd do lote
        ELSEIF it_lips-lfimg GT it_lqua-verme AND it_lqua-verme GT 0
        .

          it_lips-lfimg = it_lips-lfimg - it_lqua-verme.


          bi_tab-vbeln = it_lips-vbeln.
          bi_tab-posnr = it_lips-posnr.
          bi_tab-lgtyp = it_lips-lgtyp.
          bi_tab-lfimg = it_lqua-verme.
          bi_tab-charg = it_lqua-charg.

          APPEND bi_tab. CLEAR bi_tab.

          CLEAR it_lqua-verme.

*----------actualiza a qtd disponivel do lote
****              MODIFY it_lqua INDEX idx_lqua.
          MODIFY it_lips INDEX idx_lips.
*--se a qtd do lote for nula não necessitamos de o processar mais vezes
          DELETE it_lqua INDEX idx_lqua.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " falta_stock

*&---------------------------------------------------------------------*
*&      Form  get_paletes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_paletes.
  DATA: lv_2step  TYPE flag,
        lv_2spart TYPE flag,
        lv_tabix  TYPE sytabix.

  DATA: lt_vekp   TYPE TABLE OF vekp,
        lt_vepo   TYPE TABLE OF vepo,
        lt_zwm066 TYPE TABLE OF zwm066,
        lt_zwm020 TYPE TABLE OF zwm020.

  DATA: ls_vekp   TYPE vekp,
        ls_vepo   TYPE vepo,
        ls_zwm066 TYPE zwm066,
        ls_zwm020 TYPE zwm020.

  DATA: BEGIN OF tab_ltak OCCURS 0,
          tanum LIKE ltak-tanum,
          benum LIKE ltak-benum,
          vbeln LIKE ltak-vbeln,
        END OF tab_ltak.

  DATA ls_ltak LIKE LINE OF tab_ltak.

*  DATA: BEGIN OF tab_ltap OCCURS 0,
*           vlenr LIKE ltap-vlenr,
*           vorga LIKE ltap-vorga,
*        END OF tab_ltap.
  DATA: tab_ltap TYPE TABLE OF ltap WITH HEADER LINE.

  DATA: ls_ltap TYPE ltap.
*------------------------------------------------Inicializar estruturas

  CLEAR : it_ltap, it_ltap[], it_ltak, it_ltak[].
*-----------------------1º seleccionar da zwm026 as paletes incompletas

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 18.06.2012 17:16:44
*  Motivo: Validação de Grupos em 2 Passos
*--------------------------------------------------------------------*
  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
*     i_lgnum  = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:47:51
      i_lgnum  = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:47:52
      i_refnr  = vbss-sammg
    IMPORTING
      e_2step  = lv_2step
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  IF lv_2step EQ abap_true AND
     NOT it_serv IS INITIAL.
    EXIT.
  ENDIF.

  IF lv_2step EQ abap_true AND
     lv_2spart EQ abap_false.
    SELECT  * INTO CORRESPONDING FIELDS OF TABLE it_ltap
              FROM zwm026
*              WHERE  armazem  = z1om_lgnum                  "'100' " << DEL ROFF(SDF):TMGP:06.01.2016 16:47:51
              WHERE  armazem  = gv_lgnum                  "'100' " << INS ROFF(SDF):TMGP:06.01.2016 16:47:52
              AND    grupo    = vbss-sammg
              AND    sscc     NE space.
  ELSE.
    SELECT  * INTO CORRESPONDING FIELDS OF TABLE it_ltap
              FROM zwm026
*              WHERE  armazem  = z1om_lgnum                  "'100' " << DEL ROFF(SDF):TMGP:06.01.2016 16:47:51
              WHERE  armazem  = gv_lgnum                  "'100' " << INS ROFF(SDF):TMGP:06.01.2016 16:47:52
              AND    remessa  = tabi_guias_por_item-guia
              AND    sscc     NE space.
  ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

  LOOP AT it_ltap WHERE ord IS INITIAL.
    CLEAR ls_ltap.
    SELECT SINGLE * FROM ltap INTO ls_ltap
     WHERE lgnum = it_ltap-armazem   AND
           tanum = it_ltap-to_number.

    IF ls_ltap-meins <> ls_ltap-altme.
      CALL FUNCTION 'ROUND'
        EXPORTING
          decimals      = 0
          input         = ls_ltap-vsola
          sign          = 'X'
        IMPORTING
          output        = it_ltap-quantidade
        EXCEPTIONS
          input_invalid = 1
          overflow      = 2
          type_invalid  = 3
          OTHERS        = 4.
      it_ltap-unidade = ls_ltap-altme.
    ENDIF.

    it_ltap-ord = '3'.
    MODIFY it_ltap.

  ENDLOOP.
*----------------------2º seleccionar da ltak-ltap as paletes completas

  REFRESH: tab_ltak, tab_ltap.
  CLEAR:   tab_ltak, tab_ltap, it_ltap.

  SELECT tanum benum vbeln FROM ltak
          INTO CORRESPONDING FIELDS OF TABLE tab_ltak
*         WHERE lgnum = z1om_lgnum  " << DEL ROFF(SDF):TMGP:06.01.2016 16:48:31
         WHERE lgnum = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:48:33
           AND refnr = vbss-sammg.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 20.06.2012 09:42:02
*  Motivo: Apaga Entradas somente em Pck em 1 Passo
*--------------------------------------------------------------------*
  IF lv_2step EQ abap_false.
    DELETE tab_ltak WHERE vbeln <> tabi_guias_por_item-guia.
  ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
  IF lv_2step EQ abap_true AND
     lv_2spart EQ abap_true.
    LOOP AT tab_ltak INTO ls_ltak.
      CHECK z_wm_cl_management=>get_vbeln( i_lgnum = gv_lgnum is_data = ls_ltak ) <> tabi_guias_por_item-guia.
      DELETE tab_ltak WHERE tanum = ls_ltak-tanum.
    ENDLOOP.
  ENDIF.

  IF NOT tab_ltak[] IS INITIAL.

*    SELECT vlenr vorga  FROM  ltap
*           INTO CORRESPONDING FIELDS OF TABLE tab_ltap
*           FOR ALL ENTRIES IN tab_ltak
*           WHERE  lgnum = z1om_lgnum
*             AND  tanum = tab_ltak-tanum.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 20.06.2012 09:57:10
*  Motivo: Seleciona Todos os Campos
*--------------------------------------------------------------------*
    SELECT * FROM ltap
       INTO TABLE tab_ltap
       FOR ALL ENTRIES IN tab_ltak
*       WHERE  lgnum = z1om_lgnum AND " << DEL ROFF(SDF):TMGP:06.01.2016 16:48:49
       WHERE  lgnum = gv_lgnum AND " << INS ROFF(SDF):TMGP:06.01.2016 16:48:51
              tanum = tab_ltak-tanum.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
  ENDIF.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.


  DELETE tab_ltap WHERE vorga <> z1om_vorga
                    AND vorga <> z1om_vorga2
                    AND vorga <> space.

  DELETE tab_ltap WHERE vorga EQ space AND "DSilva - Roff - 03/07/2012
                        nltyp <> '815'."DSilva - Roff - 03/07/2012

  DELETE tab_ltap WHERE nltyp <> '815' AND
                        nltyp <> '916'.

  DELETE tab_ltap WHERE vlenr EQ space.

**  DO 1 TIMES.
**    CHECK lv_2spart EQ abap_true.
**    SELECT * FROM zwm066
**             INTO TABLE lt_zwm066
**             WHERE lgnum = gv_lgnum AND
**                   refnr = vbss-sammg.
**    CHECK sy-subrc EQ 0.
**
**    DELETE lt_zwm066 WHERE vbeln <> tabi_guias_por_item-guia.
**
**    CHECK NOT lt_zwm066 IS INITIAL.
**
**    SELECT * FROM zwm020
**             INTO TABLE lt_zwm020
**             FOR ALL ENTRIES IN lt_zwm066
**             WHERE ARMAZEM = gv_lgnum AND
**                  ( p1 = lt_zwm066-lenum OR p2 = lt_zwm066-lenum ).
**
**    LOOP AT lt_zwm020 INTO ls_zwm020.
**      CLEAR: ls_zwm066.
**      ls_zwm066-lgnum = gv_lgnum.
**      ls_zwm066-refnr = vbss-sammg.
**      ls_zwm066-vbeln = tabi_guias_por_item-guia.
**      ls_zwm066-lenum = ls_zwm020-p1.
**      APPEND ls_zwm066 TO lt_zwm066.
**
**      CLEAR: ls_zwm066.
**      ls_zwm066-lgnum = gv_lgnum.
**      ls_zwm066-refnr = vbss-sammg.
**      ls_zwm066-vbeln = tabi_guias_por_item-guia.
**      ls_zwm066-lenum = ls_zwm020-p2.
**      APPEND ls_zwm066 TO lt_zwm066.
**    ENDLOOP.
**
**    SORT lt_zwm066 BY lenum.
**    DELETE ADJACENT DUPLICATES FROM lt_zwm066 COMPARING lenum.
**
**
**    LOOP AT tab_ltap.
**      lv_tabix = sy-tabix.
**
**      READ TABLE lt_zwm066
**        WITH KEY lenum = tab_ltap-vlenr
**        BINARY SEARCH
**        TRANSPORTING NO FIELDS.
**      CHECK sy-subrc <> 0.
**
**      DELETE tab_ltap INDEX lv_tabix.
**    ENDLOOP.
**  ENDDO.




  LOOP AT tab_ltap.
    it_ltap-sscc       = tab_ltap-vlenr.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 20.06.2012 10:00:15
*  Motivo: Campos Extra
*--------------------------------------------------------------------*
    it_ltap-remessa    = tab_ltap-vbeln.
    it_ltap-posnr      = tab_ltap-posnr.
    it_ltap-material   = tab_ltap-matnr.
    it_ltap-lote       = tab_ltap-charg.
    it_ltap-quantidade = tab_ltap-nistm.
    it_ltap-unidade    = tab_ltap-meins.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
    APPEND it_ltap.
    CLEAR it_ltap.
  ENDLOOP.

  LOOP AT it_ltap WHERE ord IS INITIAL.
    it_ltap-ord = '1'.
    MODIFY it_ltap.
  ENDLOOP.

  CLEAR it_ltap.
*-------------------------3º PALETES RESULTANTES DA PALETIZAÇÃO ESPECIAL
*  SELECT tanum INTO ltap-tanum FROM  ltap
*         WHERE  lgnum  = z1om_lgnum
*         AND    vlpla = tabi_guias_por_item-guia
*         AND    nlpla = tabi_guias_por_item-guia
*         AND    vltyp = z1om_lgtyp                          "916
*         AND    nltyp = z1om_lgtyp
*         AND   ( vorga  = z1om_vorga  OR vorga  = z1om_vorga2 ).
*
*    IF sy-subrc = 0.
*
*      SELECT SINGLE lznum INTO it_ltap-sscc FROM  ltak
*             WHERE  lgnum  = z1om_lgnum
*             AND    bwlvs  = z1om_bwlvs                     "'991'
*             AND    tanum  = ltap-tanum
*             AND    lznum  NE space.
*
*      IF sy-subrc = 0.
*        APPEND it_ltap.
*      ENDIF.
*
*    ENDIF.
*
*  ENDSELECT.
*
*  LOOP AT it_ltap WHERE ord IS INITIAL.
*    it_ltap-ord = '2'.
*    MODIFY it_ltap.
*  ENDLOOP.
*
*  CLEAR it_ltap.

** INI - Nova Paletização Especial
  DATA lt_zwm044 LIKE zwm044 OCCURS 0 WITH HEADER LINE.
  DATA lv_vbeln LIKE likp-vbeln.
  CLEAR lt_zwm044.
  REFRESH lt_zwm044.

  CLEAR lv_vbeln.
  IF lv_2step EQ abap_true AND
     lv_2spart EQ abap_false.
    lv_vbeln = '9999999999'.
  ELSE.
    lv_vbeln = tabi_guias_por_item-guia.
  ENDIF.

*  CLEAR lv_vbeln.
*  IF lv_2step EQ abap_true.
*    lv_vbeln = '9999999999'.
*  ELSE.
*    lv_vbeln = tabi_guias_por_item-guia.
*  ENDIF.

  SELECT * INTO TABLE lt_zwm044
      FROM zwm044
*          WHERE lgnum = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:49:24
          WHERE lgnum = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:49:26
            AND refnr = vbss-sammg
            AND vbeln = lv_vbeln.

  IF NOT lt_zwm044[] IS INITIAL.

    DO  1 TIMES.
      CHECK NOT lt_zwm044[] IS INITIAL.

      SELECT * FROM vekp
               INTO TABLE lt_vekp
               FOR ALL ENTRIES IN lt_zwm044
               WHERE exidv = lt_zwm044-exidv.
      CHECK sy-subrc EQ 0.

      SELECT * FROM vepo
               INTO TABLE lt_vepo
               FOR ALL ENTRIES IN lt_vekp
               WHERE venum = lt_vekp-venum.
    ENDDO.

    LOOP AT it_ltap.
      LOOP AT lt_zwm044 WHERE exidv = it_ltap-sscc
                          AND status = 'I'.
        DELETE it_ltap WHERE sscc = lt_zwm044-exidv.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_zwm044 WHERE status = 'F'.
      CLEAR: ls_vekp.
      READ TABLE lt_vekp
            INTO ls_vekp
            WITH KEY exidv = lt_zwm044-exidv.

      LOOP AT lt_vepo INTO ls_vepo WHERE venum = ls_vekp-venum.
        CLEAR: it_ltap.
        it_ltap-remessa = ls_vepo-vbeln.
        it_ltap-posnr = ls_vepo-posnr.
        it_ltap-material = ls_vepo-matnr.
        it_ltap-lote     = ls_vepo-charg.
        it_ltap-quantidade = ls_vepo-vemng.
        it_ltap-unidade    = ls_vepo-vemeh.
        it_ltap-ord = '2'.
        it_ltap-sscc = lt_zwm044-exidv.
        APPEND it_ltap.
      ENDLOOP.
    ENDLOOP.

    CLEAR it_ltap.
  ENDIF.

** FIM - Nova Paletização Especial

*----------------------3º embalar paletes
  SORT it_ltap BY sscc.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Ricardo Sousa <<ROFF>>
*  Data: 01.11.2013 15:11:13
*  Motivo: Validar se existem modificações de SSCC (HU)
*--------------------------------------------------------------------*
  PERFORM check_sscc.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Ricardo Sousa <<ROFF>>
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 20.06.2012 09:50:13
*  Motivo: Limpa duplicados pelas entradas que existem actualmente
*--------------------------------------------------------------------*
*  DELETE ADJACENT DUPLICATES FROM it_ltap.
  it_ltap_all[] = it_ltap[].
  DELETE ADJACENT DUPLICATES FROM it_ltap COMPARING sscc ord.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
  SORT it_ltap BY ord ASCENDING.
  IF lv_2spart EQ abap_false.
    LOOP AT it_ltap.

      it_serv-vbeln_gen = tabi_guias_por_item-guia.
      it_serv-exidv     = it_ltap-sscc.

      SELECT SINGLE kunnr INTO it_serv-kunnr FROM  likp
             WHERE  vbeln  = it_serv-vbeln_gen.

      APPEND it_serv.
    ENDLOOP.

    SORT it_ltap BY ord ASCENDING.
  ENDIF.

ENDFORM.                    " get_paletes

*&---------------------------------------------------------------------*
*&      Form  ASSOCIA_PALETES_REMESSAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM associa_paletes_remessas .
  break roffd.
  DATA: my_subrc LIKE sy-subrc,
        it_sscc  LIKE zwm_ltap OCCURS 0 WITH HEADER LINE.
  DATA: ls_message TYPE bdcmsgcoll.
  DATA: lv_subrc TYPE sysubrc.
  DATA: lv_type TYPE c.

  CLEAR:   it_ltap1, l_idx, l_vbeln, it_sscc_pick.
  REFRESH: it_ltap1, it_sscc_pick.

  SORT it_ltap.
  DELETE ADJACENT DUPLICATES FROM it_ltap.

  PERFORM check_packing_skip USING tabi_guias_por_item-guia CHANGING lv_subrc lv_type.
  IF lv_subrc EQ 0.
    DELETE it_serv WHERE vbeln_gen = tabi_guias_por_item-guia.
  ENDIF.

  l_vbeln = tabi_guias_por_item-guia.

  LOOP AT it_ltap.
    it_sscc-sscc = it_ltap-sscc.
    APPEND it_sscc.
  ENDLOOP.

  CHECK NOT it_sscc[] IS INITIAL.
  PERFORM check_packing_skip USING tabi_guias_por_item-guia CHANGING lv_subrc lv_type.
  IF lv_subrc EQ 0.

    CALL FUNCTION 'ZWM_HU_SPLIT_REPACK'
      EXPORTING
        remessa          = l_vbeln
      TABLES
        it_ltap          = it_sscc
        it_sscc_pick     = it_sscc_pick
      EXCEPTIONS
        no_data          = 1
        delivery_no_good = 2
        OTHERS           = 3.
  ENDIF.

  CLEAR:   it_msg.
  REFRESH: it_msg.
  CALL FUNCTION 'ZWM_DELIVERY_CHANGE'
    EXPORTING
      i_vbeln     = l_vbeln
      i_commit    = 'X'
    IMPORTING
      et_messages = it_message
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    READ TABLE it_message INTO ls_message WITH KEY msgtyp = 'E'.
    ADD 1 TO l_idx.
    CONCATENATE 'HU_' l_idx INTO l_zztpdoc.
    CONDENSE l_zztpdoc NO-GAPS.
    PERFORM log_processo
       USING ls_message-msgtyp l_zztpdoc ls_message-msgv1 ' '.
    g_erro = 'X'.
  ENDIF.

  CLEAR:   it_msg.
  REFRESH: it_msg.

  flag_erro = 'X'.

  PERFORM check_packing_skip USING l_vbeln CHANGING lv_subrc lv_type.
  IF lv_subrc EQ 0.
    CALL FUNCTION 'ZWM_DELIVERY_UPDATE'
      EXPORTING
*       lgnum      = l_warehouse " << DEL ROFF(SDF):TMGP:06.01.2016 16:56:39
        lgnum      = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:56:40
        remessa    = l_vbeln
      TABLES
        it_sscc    = it_sscc
        return_msg = it_msg
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

  ENDIF.

  CLEAR flag_erro.

  IF sy-subrc <> 0.
    ADD 1 TO l_idx.
    CONCATENATE 'HU_' l_idx INTO l_zztpdoc.
    CONDENSE l_zztpdoc NO-GAPS.
    PERFORM log_processo
       USING it_msg-msgtyp l_zztpdoc it_msg-msgv1 ' '.
    g_erro = 'X'.
  ELSE.
    PERFORM modifica_guia_meia_paletes.
  ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 27.09.2012 18:03:54
*  Motivo: Remessa Embaladas nas remessas
*--------------------------------------------------------------------*
  APPEND l_vbeln TO gt_vbeln_skip.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


  PERFORM gera_reg_log USING 'HU' ' '.
  PERFORM gera_reg_log USING 'CR' ' '.
  it_ltap1-guia = l_vbeln.
  APPEND it_ltap1.

  CLEAR:   it_msg, bdcdata, it_ltap, it_ltak.
  REFRESH: it_msg, bdcdata, it_ltap, it_ltak.

ENDFORM.                    " ASSOCIA_PALETES_REMESSAS

*&---------------------------------------------------------------------*
*&      Form  log_processo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MSG_MSGTYP  text
*      -->P_3357   text
*----------------------------------------------------------------------*
FORM log_processo  USING    p_msgtyp
                            VALUE(p_evento)
                            p_doc
                            p_item.

  DATA: l_texto LIKE t100-text.

  CLEAR: l_texto.

*------------------------------NAS OT o nº criado vem no campo
*it_msg-msgv1
  IF p_evento(2) = 'OT'.
    p_doc = it_msg-msgv1.
    zwmlog-vgpos   = bi_tab-posnr.
  ELSE.
    zwmlog-vgpos   = p_item.
  ENDIF.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = it_msg-msgid
      msgnr               = it_msg-msgnr
      msgv1               = it_msg-msgv1
      msgv2               = it_msg-msgv2
      msgv3               = it_msg-msgv3
      msgv4               = it_msg-msgv4
    IMPORTING
      message_text_output = l_texto.

  zwmlog-vbeln = tabi_guias_por_item-guia.

  zwmlog-sammg   = vbss-sammg.
  zwmlog-zztpdoc = p_evento.
  zwmlog-zzdoc   = p_doc.
  zwmlog-vgpos   = p_item.
  zwmlog-zztpmsg = p_msgtyp.
  zwmlog-zzmsg   = l_texto.
  zwmlog-uname   = sy-uname.
  zwmlog-datum   = sy-datum.
  zwmlog-uzeit   = sy-uzeit.

  MODIFY zwmlog.

ENDFORM.                    " log_processo

*&---------------------------------------------------------------------*
*&      Form  act_it_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM act_it_msg .

  it_msg-msgtyp = sy-msgty.
  it_msg-msgid  = sy-msgid.
  it_msg-msgnr  = sy-msgno.
  it_msg-msgv1  = sy-msgv1.
  it_msg-msgv2  = sy-msgv2.
  it_msg-msgv3  = sy-msgv3.
  it_msg-msgv4  = sy-msgv4.

ENDFORM.                    " act_it_msg

*&---------------------------------------------------------------------*
*&      Form  log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM log USING p_docu p_item.

  IF it_msg[] IS INITIAL.
    PERFORM act_it_msg.
    PERFORM log_processo USING it_msg-msgtyp l_zztpdoc p_docu p_item.
  ELSE.
    SORT it_msg BY msgtyp ASCENDING.
    LOOP AT it_msg WHERE msgtyp = 'E' OR msgtyp = 'A' OR  msgtyp = 'S'.
*---------------------------------------------A msg de sucesso que
*interessa é a nº 311
      IF it_msg-msgtyp = 'S' AND l_zztpdoc(3) = 'REM'.
        CHECK it_msg-msgnr = '311'.
      ENDIF.

      PERFORM log_processo USING it_msg-msgtyp l_zztpdoc p_docu p_item.
      EXIT.

    ENDLOOP.

  ENDIF.

  IF it_msg-msgtyp = 'E' OR it_msg-msgtyp = 'A'.
    g_erro = 'X'.
  ENDIF.

ENDFORM.                    " log

*&---------------------------------------------------------------------*
*&      Form  GR_bloqueado
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gr_bloqueado USING p_momento  CHANGING p_bloq.

  DATA : l_enq       LIKE seqg3 OCCURS 0 WITH HEADER LINE,
         l_gname     LIKE seqg3-gname VALUE 'LIKP',  l_dummy(18),
         l_garg      LIKE seqg3-garg,
         l_guname    LIKE seqg3-guname.

  CONCATENATE sy-mandt tabi_guias_por_item-guia INTO l_garg.

  CONDENSE l_garg NO-GAPS.

  CLEAR: l_enq, l_enq[], l_guname.

  CALL FUNCTION 'ENQUE_READ'
    EXPORTING
      gclient = sy-mandt
      gname   = l_gname
      garg    = l_garg
      guname  = l_guname
*   IMPORTING
*     NUMBER  =
*     SUBRC   =
    TABLES
      enq     = l_enq.

  READ TABLE l_enq INDEX 1.

  IF sy-subrc = 0.
    p_bloq = 2.
    CONCATENATE 'Bloqueio'(012) tabi_guias_por_item-guia INTO
    l_dummy SEPARATED BY space.
  ELSEIF sy-subrc NE 0.
    CLEAR p_bloq.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = l_dummy.

ENDFORM.                    " GR_bloqueado

*&---------------------------------------------------------------------*
*&      Form  bloqueio_global
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bloqueio_global USING doc CHANGING p_bloq
                                        p_user.

  DATA: l_enq   LIKE seqg3 OCCURS 0 WITH HEADER LINE,
        l_gname LIKE seqg3-gname,
        l_garg  LIKE seqg3-garg,
        guname  LIKE sy-uname.

  CONCATENATE sy-mandt doc INTO l_garg.

  CONDENSE l_garg NO-GAPS.

  CLEAR:   l_enq, p_bloq, guname, p_user.
  REFRESH: l_enq.

  CALL FUNCTION 'ENQUE_READ'
    EXPORTING
      gclient = sy-mandt
      gname   = l_gname
      garg    = l_garg
      guname  = guname
*   IMPORTING
*     NUMBER  =
*     SUBRC   =
    TABLES
      enq     = l_enq.

  IF NOT l_enq[] IS INITIAL.
    p_bloq = 'X'.
    p_user = l_enq-guname.
  ENDIF.

ENDFORM.                    " bloqueio_geral

*&---------------------------------------------------------------------*
*&      Form  converte_uni_med
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM converte_uni_med  USING    p_lfimg
                                p_vrkme_de
                                p_meins_para
                                p_matnr
                       CHANGING ps_lfimg
                                ps_vrkme.

  CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
    EXPORTING
      i_matnr              = p_matnr
      i_in_me              = p_vrkme_de
      i_out_me             = p_meins_para
      i_menge              = p_lfimg
    IMPORTING
      e_menge              = ps_lfimg
    EXCEPTIONS
      error_in_application = 1
      error                = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO g_dummy.
  ELSE.
    ps_vrkme = p_meins_para.
  ENDIF.

ENDFORM.                    " converte_uni_med

*&---------------------------------------------------------------------*
*&      Form  proc_bonus
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM proc_bonus.

  LOOP AT tabi_guias_por_item.
    CLEAR tabix. tabix = sy-tabix.
    PERFORM rec_of_mercadoria.
  ENDLOOP.

*--Tabela com os valores agrupados por item a modificar----------
  LOOP AT tabi_guias_por_item-linha INTO it_grp_item.
    it_grp_item-guia = tabi_guias_por_item-guia.
    CLEAR: it_grp_item-posnr, it_grp_item-uepos.
    COLLECT it_grp_item.
  ENDLOOP.

ENDFORM.                    " proc_bonus

*&---------------------------------------------------------------------*
*&      Form  gera_reg_reprocesso
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gera_reg_reprocesso USING p_tpdoc p_item p_qtd
                               p_guia  p_encom p_grupo
                               p_matnr p_uni p_ctitem.
  DATA : l_ind(2).

  CLEAR: zwmlog02, l_ind.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = p_uni
      language       = sy-langu
    IMPORTING
*     LONG_TEXT      =
      output         = p_uni
*     SHORT_TEXT     =
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO g_dummy.
  ENDIF.

  CASE p_tpdoc.

    WHEN 'REM_COPI'.
*-----------------------------------------repõe item
      zwmlog02-zztpdoc   = p_tpdoc.
      zwmlog02-sammg     = p_grupo.
      zwmlog02-vbeln     = p_guia.
      zwmlog02-posnr     = p_item.
      zwmlog02-vbeln_enc = p_encom.
      zwmlog02-lfimg     = p_qtd.
      zwmlog02-vrkme     = p_uni.
      zwmlog02-pstyv     = p_ctitem.

      MODIFY zwmlog02.

      COMMIT WORK AND WAIT.

    WHEN 'REM_SAVE'.
*--------------------------------------Só repõe qtd
      zwmlog02-zztpdoc   = p_tpdoc.
      zwmlog02-sammg     = p_grupo.
      zwmlog02-vbeln     = p_guia.
      zwmlog02-posnr     = p_item.
      zwmlog02-lfimg     = p_qtd.
      zwmlog02-vrkme     = p_uni.

      MODIFY zwmlog02.

      COMMIT WORK AND WAIT.

    WHEN 'OT_'.

      CLEAR : it_ot, it_ot[], bi_tab, bi_tab[].

      it_ot-vbeln = p_guia.
      it_ot-posnr = p_item.
      APPEND it_ot.

      zwmlog02-lfimg     = p_qtd.

*--------------------------------------Determinação de lotes
      PERFORM determina_lote_para_ot.

      CLEAR zwmlog02-lfimg.

      LOOP AT bi_tab.

        CLEAR l_ind.
        l_ind = sy-tabix.
        CONCATENATE 'OT_' l_ind INTO zwmlog02-zztpdoc.

        zwmlog02-sammg     = p_grupo.
        zwmlog02-vbeln     = p_guia.
        zwmlog02-posnr     = bi_tab-posnr.
        zwmlog02-vgpos     = bi_tab-posnr.
        zwmlog02-vbeln_enc = p_encom.
        zwmlog02-lfimg     = bi_tab-lfimg.
        zwmlog02-lgtyp     = z1om_lgtyp.
        zwmlog02-matnr     = p_matnr.
        zwmlog02-charg     = bi_tab-charg.
        zwmlog02-vrkme     = p_uni.
        zwmlog02-pstyv     = p_ctitem.

        MODIFY zwmlog02.

        COMMIT WORK AND WAIT.

      ENDLOOP.

      CLEAR : it_ot, it_ot[], bi_tab, bi_tab[].

    WHEN 'HU_'.

      PERFORM get_paletes.

      LOOP AT it_ltap.

        CLEAR l_ind.
        l_ind = sy-tabix.
        CONCATENATE 'HU_' l_ind INTO zwmlog02-zztpdoc.

        zwmlog02-sammg     = p_grupo.
        zwmlog02-vbeln     = p_guia.
        zwmlog02-posnr     = p_item.
        zwmlog02-vgpos     = p_item.
        zwmlog02-lgtyp     = z1om_lgtyp.
        zwmlog02-hu        = it_ltap-sscc.
        zwmlog02-pstyv     = p_ctitem.

        MODIFY zwmlog02.

        COMMIT WORK AND WAIT.

      ENDLOOP.
*VRKME
      CLEAR : it_ltap, it_ltap[], it_ltak, it_ltak[].

    WHEN 'CR'.
      PERFORM credito_item USING z1com_s.
      PERFORM credito_item USING wa_z1com_s.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " gera_reg_reprocesso

*&---------------------------------------------------------------------*
*&      Form  gera_reg_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_reg_log USING tpdoc p_item.

  CASE tpdoc.
    WHEN 'OT'.

      LOOP AT bi_tab.

        CLEAR l_idx.
        l_idx = sy-tabix.
        CONCATENATE 'OT_' l_idx INTO zwmlog02-zztpdoc.

        zwmlog02-sammg     = vbss-sammg.
        zwmlog02-vbeln     = tabi_guias_por_item-guia.
        zwmlog02-posnr     = bi_tab-posnr.
        zwmlog02-vbeln_enc = z1com_s-vbeln.
        zwmlog02-lfimg     = bi_tab-lfimg.
        zwmlog02-lgtyp     = z1om_lgtyp.
        zwmlog02-matnr     = bi_tab-matnr.
        zwmlog02-charg     = bi_tab-charg.
        zwmlog02-vrkme     = z1com_s-vrkme.
        zwmlog02-pstyv     = z1com_s-pstyv.

        MODIFY zwmlog02.

        COMMIT WORK AND WAIT.

      ENDLOOP.
      CLEAR l_idx.

    WHEN 'HU'.

      LOOP AT it_ltap.

        CLEAR l_idx.
        l_idx = sy-tabix.
        CONCATENATE 'HU_' l_idx INTO zwmlog02-zztpdoc.

        zwmlog02-sammg     = vbss-sammg.
        zwmlog02-vbeln     = tabi_guias_por_item-guia.
*       zwmlog02-posnr     = p_item.
        zwmlog02-lgtyp     = z1om_lgtyp.
        zwmlog02-hu        = it_ltap-sscc.
*       zwmlog02-pstyv     = p_ctitem.

        MODIFY zwmlog02.

        COMMIT WORK AND WAIT.

      ENDLOOP.

    WHEN 'CR'.

      IF it_creditos_log[] IS INITIAL.

        LOOP AT tabi_creditos.
          MOVE-CORRESPONDING tabi_creditos TO it_creditos_log.
          APPEND it_creditos_log.
        ENDLOOP.

      ENDIF.

      LOOP AT it_creditos_log.
*----------------------------------------------------Ins Mai2005
        CLEAR i_string.

        i_string = it_creditos_log-kbetr.

        CALL FUNCTION 'C14W_CHAR_NUMBER_CONVERSION'
          EXPORTING
            i_string                   = i_string
          IMPORTING
*           E_FLOAT                    =
            e_dec                      = g_dec
*           E_DECIMALS                 =
          EXCEPTIONS
            wrong_characters           = 1
            first_character_wrong      = 2
            arithmetic_sign            = 3
            multiple_decimal_separator = 4
            thousandsep_in_decimal     = 5
            thousand_separator         = 6
            number_too_big             = 7
            OTHERS                     = 8.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO g_dummy.

          CLEAR g_dummy.
        ENDIF.

        zwmlog02-sammg     = vbss-sammg.
        zwmlog02-vbeln_enc = it_creditos_log-vbeln.
        zwmlog02-vbeln     = it_creditos_log-guia.
        zwmlog02-vgpos     = it_creditos_log-vgpos.
        zwmlog02-posnr     = it_creditos_log-vgpos.
        zwmlog02-kbetr     = g_dec.
        zwmlog02-waerk     = it_creditos_log-waerk.

        MODIFY zwmlog02.

        COMMIT WORK AND WAIT.

      ENDLOOP.

      CLEAR: it_creditos_log, it_creditos_log[],
             tabi_creditos, tabi_creditos[].

    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " gera_reg_log

*&---------------------------------------------------------------------*
*&      Form  log_credito
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM log_credito .
  MOVE-CORRESPONDING tabi_creditos TO it_creditos_log.
  APPEND it_creditos_log.
ENDFORM.                    " log_credito

*&---------------------------------------------------------------------*
*&      Form  credito_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM credito_item USING st_item STRUCTURE z1com_s.

  DATA: valor_char(16),
        valor_p        TYPE p DECIMALS 0,
        valor_p2       TYPE p DECIMALS 2.
  DATA i TYPE i.

  CLEAR : tabi_creditos, tabi_creditos[].

  CHECK st_item-quant_cr NE 0.
  MOVE-CORRESPONDING st_item TO tabi_creditos.

  SELECT SINGLE * FROM tcurx WHERE currkey = tabi_creditos-waerk.

  IF sy-subrc NE 0.
    i = 0.
  ELSE.
    i = 2 - tcurx-currdec.
  ENDIF.
  IF i = 2.                          " 0 casas decimais
    valor_p = st_item-quant_cr * st_item-prliq * 100.
    tabi_creditos-kbetr = valor_p.

  ELSE.
    valor_p2 =  st_item-prliq * 10 ** i.
    valor_p2 = st_item-quant_cr * valor_p2.
    WRITE valor_p2 TO tabi_creditos-kbetr CURRENCY tabi_creditos-waerk
    .
  ENDIF.
  tabi_creditos-guia = tabi_guias_por_item-guia.
  APPEND tabi_creditos.

ENDFORM.                    " credito_item

*&---------------------------------------------------------------------*
*&      Form  cria_reg_credito
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_reg_credito USING p_item st_item STRUCTURE z1com_s.

  DATA: valor_char(16),
        valor_p        TYPE p DECIMALS 0,
        valor_p2       TYPE p DECIMALS 2.
  DATA i TYPE i.

  CHECK st_item-quant_cr NE 0.
  MOVE-CORRESPONDING st_item TO tabi_creditos.

  SELECT SINGLE * FROM tcurx WHERE currkey = tabi_creditos-waerk.
  IF sy-subrc NE 0.
    i = 0.
  ELSE.
    i = 2 - tcurx-currdec.
  ENDIF.
  IF i = 2.                          " 0 casas decimais
    valor_p = st_item-quant_cr * st_item-prliq * 100.
    tabi_creditos-kbetr = valor_p.
  ELSE.
    valor_p2 =  st_item-prliq * 10 ** i.
    valor_p2 = st_item-quant_cr * valor_p2.
    WRITE valor_p2 TO tabi_creditos-kbetr CURRENCY tabi_creditos-waerk
    .
  ENDIF.
  tabi_creditos-guia = tabi_guias_por_item-guia.
  APPEND tabi_creditos.

ENDFORM.                    " cria_reg_credito

*&---------------------------------------------------------------------*
*&      Form  verifica_servican
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_servisan USING rc.

  DATA: l_kunnr LIKE vbpa-kunnr.

  CLEAR: l_kunnr, rc, g_erro.

  DATA: lv_2chang TYPE flag.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = gv_lgnum
      i_vbeln  = tabi_guias_por_item-guia
    IMPORTING
      e_2chang = lv_2chang
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  SELECT kunnr INTO vbpa-kunnr FROM vbpa UP TO 1 ROWS
         WHERE  vbeln  = tabi_guias_por_item-guia
         AND    parvw  = z1om_parvw.
  ENDSELECT.

  IF sy-subrc EQ 0.

    SELECT SINGLE * FROM likp WHERE vbeln = tabi_guias_por_item-guia.
    IF likp-vkorg = 'SER1' AND lv_2chang EQ abap_false.
      SELECT SINGLE * FROM tvko WHERE vkorg = likp-vkorg.
      IF tvko-kunnr IS NOT INITIAL.
        vbpa-kunnr = tvko-kunnr.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM  zwm039
*           WHERE  lgnum  = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:49:40
           WHERE  lgnum  = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:49:42
           AND    kunnr  = vbpa-kunnr.

    IF sy-subrc = 0.
*----Ao entrar neste processo de embalamento não executa o embalamento
*----normal
      rc = 4.

      PERFORM remessa_com_mat_pal USING vbss-sammg.

      CHECK g_erro IS INITIAL.

      IF NOT zwm039-idser IS INITIAL.
*------------------antes de iniciar o processo verificar
*------------------se a remessa já tem o material palete

*---se o cliente for servisan 1º verificar quantas remessas tem o grupo
*---se o nº de remessas for maior que 1, resulta erro
*---se não, contar o nº de paletes , agregar por cliente ,
*---e por cliente agregar por tipo de palete( pchep, prouge, peuro )
*--- seleccionar uma remessa e incluir o nº de linhas do tipo de paletes
*--------------------------se for cliente Servisan não embalamos

        PERFORM calcula_num_remessas USING vbss-sammg g_pal.

        IF g_pal GT 1.

          CLEAR likp-vkorg.
          SELECT SINGLE vkorg INTO likp-vkorg
              FROM likp
                  WHERE vbeln = tabi_guias_por_item-guia.

          IF likp-vkorg = 'RP08'.
            PERFORM get_paletes.
          ELSE.
            CLEAR: l_zztpdoc, g_dummy.
            MESSAGE s000(zwmmsg001) WITH 'Cliente Servisan, não Embalar!'
               INTO g_dummy.

            CONCATENATE 'SR_' l_idx INTO l_zztpdoc.
            CONDENSE l_zztpdoc NO-GAPS.
*------------------------------------------------------------

            PERFORM log USING z1com_s-vbeln ' '.

            rc = 4.
          ENDIF.
        ELSE.
          PERFORM get_paletes.
        ENDIF.

      ELSE.
        PERFORM get_paletes.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " verifica_servican

*&---------------------------------------------------------------------*
*&      Form  MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM msg USING g_erro.

  CLEAR text.
  IF g_erro IS INITIAL.

    text = 'Fim do Processo com sucesso!'(013).
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'S'
        message_number = '000'
        message_var1   = text.

  ELSE.

    text = 'Fim do Processo com erros!'(014).
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

  ENDIF.

ENDFORM.                    " MSG

*&---------------------------------------------------------------------*
*&      Form  mensagem_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0482   text
*      -->P_VBUK_PKSTK  text
*----------------------------------------------------------------------*
FORM mensagem_status  USING    VALUE(p_status)
                               p_valor.
  CLEAR text.

  CASE p_status.
    WHEN 'pkstk'.

      CONCATENATE 'Remessa'(015) vbss-vbeln 'já foi embalada'(016)
             INTO text SEPARATED BY space.

      CLEAR: g_dummy, l_zztpdoc.
      MESSAGE e000(zwmmsg001) WITH text INTO g_dummy.
      l_zztpdoc = 'VAL_EMBAL'.

    WHEN 'wbstk'.

      CONCATENATE 'Remessa'(015) vbss-vbeln 'com Saida de mercadoria'(017)
             INTO text SEPARATED BY space.

      CLEAR: g_dummy, l_zztpdoc.
      MESSAGE e000(zwmmsg001) WITH text INTO g_dummy.
      l_zztpdoc = 'VAL_SM'.

    WHEN 'kostk'.

      CONCATENATE 'Remessa'(015) vbss-vbeln
                  'Status Picking Global Inválido'(018)
             INTO text SEPARATED BY space.

      CLEAR: g_dummy, l_zztpdoc.
      MESSAGE e000(zwmmsg001) WITH text INTO g_dummy.
      l_zztpdoc = 'VAL_GRUPO'.

  ENDCASE.


  PERFORM log USING vbss-vbeln ' '.

  CLEAR: g_dummy, l_zztpdoc, tabi_guias, tabi_guias[], vbss-vbeln.

  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWMMSG001'
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = '000'
      message_var1   = text.

  LEAVE TO SCREEN 100.

ENDFORM.                    " mensagem_status

*&---------------------------------------------------------------------*
*&      Form  modifica_guia_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FLAG_GUIA_ALTERADA  text
*      -->P_NUM_ENCOMENDA  text
*----------------------------------------------------------------------*
FORM modifica_guia_2  USING    flag_guia_alterada
                               num_encomenda.

  REFRESH: tabi_bdcdata, it_zwmlog02.

  CLEAR: flag_guia_alterada, wa_z1com_s, idx_tabi, it_zwmlog02,
         l_ultimo, l_primeiro, g_erro.

  LOOP AT tabi_guias_por_item-linha INTO z1com_s.

    CLEAR idx_tabi. idx_tabi = sy-tabix.

    CHECK g_erro IS INITIAL.
    CLEAR g_rc2.

*-------------------------processar apenas os items com qtd modificadas
    IF z1com_s-posnr IS INITIAL.
      PERFORM item_qtd_diferente USING z1com_s-vgpos CHANGING g_rc2.
    ELSE.
      PERFORM item_qtd_diferente USING z1com_s-posnr CHANGING g_rc2.
    ENDIF.

    IF NOT g_rc2 IS INITIAL.
      CONTINUE.
    ENDIF.
*-----------------------------------------------------------------------

    IF NOT z1com_s-vbeln IS INITIAL.
      num_encomenda = z1com_s-vbeln.
    ENDIF.
*-----------------------------Os items sem oferta não são processados
    IF NOT z1com_s-itm_oferta = 'X' AND z1com_s-pstyv NE 'ZOF'.
      CONTINUE.
    ENDIF.

*---------------------------------não processar os items oferta sem pai
    IF z1com_s-pstyv = 'ZOF' AND z1com_s-uepos IS INITIAL.
      CONTINUE.
    ENDIF.
*-----------------------------------------------eliminar os items nulos

    IF z1com_s-lfimg NE z1com_s-quant_cf OR z1com_s-quant_cf NE 0.
*-----------------------------------------------efectuar apenas Crédito
      IF z1com_s-total_cf = z1com_s-lfimg AND
         z1com_s-lfimg = z1com_s-quant_cf.

        CONTINUE.
      ENDIF.
    ENDIF.

*---------------------------------seleccionar todos os subitems do item
*---------------------------------oferta que estamos a tratar
    IF z1com_s-pstyv NE 'ZOF'.
*---------------------------verificar se indice inicial sofre alteração

      CLEAR: sub_item, sub_item[], g_item.

      IF z1com_s-posnr IS INITIAL.

        g_item = z1com_s-vgpos.

      ELSE.
        g_item = z1com_s-posnr.

      ENDIF.

      LOOP AT tabi_guias_por_item-linha INTO sub_item
                                        WHERE uepos = g_item.

        COLLECT sub_item. CLEAR sub_item.
      ENDLOOP.

      READ TABLE sub_item INDEX 1.
*------------------------------------------exsitem subitems - Ofertas -
      IF NOT sub_item[] IS INITIAL.

        CLEAR : l_primeiro, l_ultimo, bdcdata, bdcdata[].

*-----eliminar todos os items relacionados com o item da encomenda que
*-----estamos a analisar

*-------------------------------------------------Eliminar item bonus
        PERFORM modifica_items USING sub_item-vgpos 'ZOF'.

*-------------------------------------------------Eliminar item normal
        PERFORM modifica_items USING g_item 'TAN'.

      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " modifica_guia_2

*&---------------------------------------------------------------------*
*&      Form  item_qtd_diferente
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_Z1COM_S_VGPOS  text
*      <--P_G_RC2  text
*----------------------------------------------------------------------*
FORM item_qtd_diferente  USING    p_item
                         CHANGING g_rc2.

  READ TABLE it_lips_aux WITH KEY vbeln = tabi_guias_por_item-guia
                                   vgpos = p_item
                                  matnr = z1com_s-matnr.

  IF sy-subrc NE 0.
    g_rc2 = 4.
  ELSE.
    IF it_lips_aux-kcmeng GT 0.

      IF it_lips_aux-kcmeng EQ z1com_s-quant_cf.
        g_rc2 = 4.
      ELSE.
        CLEAR g_rc2.
      ENDIF.

    ELSE.

      IF it_lips_aux-lfimg EQ z1com_s-quant_cf.
        g_rc2 = 4.
      ELSE.
        CLEAR g_rc2.
      ENDIF.

    ENDIF.
  ENDIF.
ENDFORM.                    " item_qtd_diferente

*&---------------------------------------------------------------------*
*&      Form  elimina_item_r
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_G_ERRO  text
*----------------------------------------------------------------------*
FORM elimina_item_r USING p_sitem l_ultimo l_primeiro CHANGING g_erro.

  DATA: d_modo   VALUE 'N', wa_posnr TYPE posnr.


  IF l_primeiro = 1.

    PERFORM bdc_dynpro   USING 'SAPMV50A'              '4004'.
    PERFORM bdc_field    USING 'BDC_CURSOR'            'LIKP-VBELN'.
    PERFORM bdc_field    USING 'BDC_OKCODE'            '/00'.
    PERFORM bdc_field    USING 'LIKP-VBELN'   tabi_guias_por_item-guia.

    CLEAR l_primeiro.

  ENDIF.

  IF l_ultimo EQ space.

    PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
    PERFORM bdc_field    USING 'BDC_OKCODE'            '=POPO_T'.
    PERFORM bdc_field    USING 'BDC_CURSOR'            'LIPS-MATNR(01)'.

    PERFORM bdc_dynpro   USING 'SAPMV50A'              '0111'.
    PERFORM bdc_field    USING 'BDC_CURSOR'            'RV50A-POSNR'.
    PERFORM bdc_field    USING 'BDC_OKCODE'            '=WEIT'.
    PERFORM bdc_field    USING 'RV50A-POSNR'           p_sitem.

    PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
    PERFORM bdc_field    USING 'BDC_OKCODE'            '=POLO_T'.
    PERFORM bdc_field    USING 'BDC_CURSOR'            'LIPS-POSNR(01)'.
    PERFORM bdc_field    USING 'RV50A-LIPS_SELKZ(01)'  'X'.

  ENDIF.

  IF l_ultimo NE space.
    CLEAR l_ultimo.

    PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
    PERFORM bdc_field    USING 'BDC_OKCODE'            '=SICH_T'.


    CLEAR: it_msg, it_msg[].

*----------------------------------Verifica se a remessa está bloqueada
    g_bloq = 2.
    WHILE g_bloq = 2.
      PERFORM gr_bloqueado USING 'REM_DELE' CHANGING g_bloq.
    ENDWHILE.


*    CALL TRANSACTION 'VL02N' USING bdcdata
*                            UPDATE 's'
*                            MODE d_modo
*                            MESSAGES INTO it_msg.
    CLEAR l_zztpdoc.
    l_zztpdoc = 'REM_DELE'.
*
*    PERFORM log USING tabi_guias_por_item-guia z1com_s-vgpos.
*
**--------a estrutura wa_z1com_s-quant_cf tem a informação sobre o bonus
*
*    COMMIT WORK AND WAIT. WAIT UP TO 5 SECONDS.

    CLEAR: bdcdata, bdcdata[], it_msg, it_msg[].

  ENDIF.

ENDFORM.                    " elimina_item_r

*&---------------------------------------------------------------------*
*&      Form  cria_item_ref_encomenda
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SUB_ITEM_VGPOS  text
*      <--P_G_ERRO  text
*----------------------------------------------------------------------*
FORM cria_item_ref_encomenda  USING  item_vgpos item_qtd item_vrkme
                                     p_tip

                              CHANGING g_erro.

  DATA : l_posnr    LIKE lips-posnr, zx_modo VALUE 'N', l_data(10).

  CLEAR: l_posnr, bdcdata, bdcdata[], g_item_of, g_doc, g_erro, l_data.

  DATA: quant_char(17), quant_char_zof(17).

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = item_vrkme
      language       = sy-langu
    IMPORTING
*     LONG_TEXT      =
      output         = item_vrkme
*     SHORT_TEXT     =
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  WRITE item_qtd TO quant_char.

  g_item_of = l_posnr.
  g_doc     = tabi_guias_por_item-guia.
*-----------------------------------------------------------------

  CLEAR likp.
  SELECT SINGLE wadat INTO likp-wadat FROM  likp
         WHERE  vbeln  = tabi_guias_por_item-guia.
*------------------------------------------------------Com Mai2005
  IF likp-wadat GT sy-datum.
    WRITE likp-wadat TO l_data.
  ELSE.
    WRITE sy-datum TO l_data.
  ENDIF.
*-----------------------------------------------------End Ins Mai2005

  PERFORM bdc_dynpro   USING 'SAPMV50A'            '4004'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'LIKP-VBELN'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
  PERFORM bdc_field USING 'LIKP-VBELN'        tabi_guias_por_item-guia.

  PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=RAUF_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'LIPS-MATNR(01)'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'            '0105'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'LV50C-BIPOS'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=ENT1'.
  PERFORM bdc_field    USING 'LV50C-VBELN'         z1com_s-vbeln.
*----------------------------------------------------------------------
  PERFORM bdc_field    USING 'LV50C-DATBI'         l_data.

*----------------------------------------------------------------------
  PERFORM bdc_field    USING 'LV50C-ABPOS'         item_vgpos.
*'000010'.
  PERFORM bdc_field    USING 'LV50C-BIPOS'         item_vgpos.
*----------------------------------------------------------------------
*------------------------------------------------------teste-----------
*-------POSICIONAR NO ITEM COPIADO
  PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=POPO_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'LIPS-MATNR(01)'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '0111'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'RV50A-POSNR'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=WEIT'.
  PERFORM bdc_field    USING 'RV50A-POSNR'           item_vgpos.
*-------FIM

  PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'LIPSD-G_LFIMG(01)'.
  PERFORM bdc_field    USING 'LIPSD-G_LFIMG(01)'   quant_char.
  PERFORM bdc_field    USING 'LIPS-VRKME(01)'      item_vrkme.
*-------------------------------------------------------
  IF p_tip = 'TAN'.
    READ TABLE sub_item INDEX 1.
    READ TABLE it_grp_item WITH KEY vgpos = sub_item-vgpos.

*------------se o item zof não foi criado não vamos tentar actualiza-lo
    IF it_grp_item-quant_cf GT 0.

      PERFORM bdc_dynpro   USING 'SAPMV50A'           '1000'.
      PERFORM bdc_field    USING 'BDC_OKCODE'         '=POPO_T'.
      PERFORM bdc_field    USING 'BDC_CURSOR'         'LIPS-MATNR(01)'.

      PERFORM bdc_dynpro   USING 'SAPMV50A'           '0111'.
      PERFORM bdc_field    USING 'BDC_CURSOR'         'RV50A-POSNR'.
      PERFORM bdc_field    USING 'BDC_OKCODE'         '=WEIT'.
      PERFORM bdc_field    USING 'RV50A-POSNR'        sub_item-vgpos.

      PERFORM bdc_dynpro   USING 'SAPMV50A'           '1000'.
      PERFORM bdc_field    USING 'BDC_OKCODE'         '/00'.
      PERFORM bdc_field    USING 'BDC_CURSOR'          'LIPSD-G_LFIMG(01)'.

      PERFORM bdc_field    USING 'LIPS-UEPOS(01)'      g_item.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------
*----------------
*'000011'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=SICH_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'LIPS-MATNR(03)'.

  CLEAR : it_msg, it_msg[].

*----------------------------------Verifica se a remessa está bloqueada
  g_bloq = 2.
  WHILE g_bloq = 2.
    PERFORM gr_bloqueado USING 'REM_COPI' CHANGING g_bloq.
  ENDWHILE.


*  CALL TRANSACTION 'VL02N' USING bdcdata
*                          UPDATE 's'
*                          MODE zx_modo
*                          MESSAGES INTO it_msg.
  CLEAR l_zztpdoc.
  l_zztpdoc = 'REM_COPI'.
*
*  PERFORM log USING tabi_guias_por_item-guia item_vgpos.
*
*  IF it_msg-msgtyp = 'S'.
*    COMMIT WORK AND WAIT.  WAIT UP TO 5 SECONDS.
**--------------------------------------APÓS A CÓPIA DOS ITEMS
**-----------GUARDAMOS A INFORMAÇÃO SOBRE A GRAVAÇÃO DOS ITEMS
**porque se conseguimos copiar com sucesso, se na gravação da da remessa
**ocorrer um erro, só assim sabemos quais os items processados.
*
*    it_zwmlog02-zztpdoc   = 'REM_SAVE'.
*    it_zwmlog02-sammg     = vbss-sammg.
*    it_zwmlog02-vbeln     = tabi_guias_por_item-guia.
*    it_zwmlog02-posnr     = z1com_s-vgpos.
*    it_zwmlog02-lfimg     = z1com_s-quant_cf.
*    it_zwmlog02-vrkme     = z1com_s-vrkme.
*
*    APPEND it_zwmlog02. CLEAR it_zwmlog02.
*
*    IF wa_z1com_s-quant_cf GT 0.
*      it_zwmlog02-zztpdoc   = 'REM_SAVE'.
*      it_zwmlog02-sammg     = vbss-sammg.
*      it_zwmlog02-vbeln     = tabi_guias_por_item-guia.
*      it_zwmlog02-posnr     = wa_z1com_s-vgpos.
*      it_zwmlog02-lfimg     = wa_z1com_s-quant_cf.
*      it_zwmlog02-vrkme     = wa_z1com_s-vrkme.
*
*      APPEND it_zwmlog02. CLEAR it_zwmlog02.
*    ENDIF.
*
*  ELSE.
*    CLEAR : it_ot[], it_ot.
**--------------------------------------------------Entrada do Item
*
**-------------------------Criar registo para recuperar items da remessa
*    PERFORM gera_reg_reprocesso USING 'REM_COPI' item_vgpos
*                                       z1com_s-quant_cf
*                                       tabi_guias_por_item-guia
*                                       z1com_s-vbeln vbss-sammg ' '
*                                       z1com_s-vrkme
*                                       z1com_s-pstyv.
*
**---------------------------Criar registo para recuperar OT_ da remessa
*    PERFORM gera_reg_reprocesso USING 'OT_' item_vgpos
*                                z1com_s-quant_cf
*                                tabi_guias_por_item-guia
*                                z1com_s-vbeln vbss-sammg
*                                z1com_s-matnr
*                                z1com_s-vrkme
*                                z1com_s-pstyv.
*
*
**---------------------------Criar registo para recuperar HU_ da remessa
*    PERFORM gera_reg_reprocesso USING 'HU_' item_vgpos
*                                z1com_s-quant_cf
*                                tabi_guias_por_item-guia
*                                z1com_s-vbeln vbss-sammg ' '
*                                z1com_s-vrkme
*                                z1com_s-pstyv.
*
**-------------------------------------------criar os creditos dos items
*    PERFORM cria_reg_credito USING item_vgpos z1com_s.
*
*    PERFORM gera_reg_log USING 'CR' ' '.
*
**--------------------------------------------------
*    g_erro = 'X'.
*  ENDIF.


  CLEAR : it_msg, it_msg[], bdcdata, bdcdata[].

ENDFORM.                    " cria_item_ref_encomenda

*&---------------------------------------------------------------------*
*&      Form  actualiza_tab_de_ot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_tab_de_ot USING item.

*------------------------------------------------------items a criar ot
  it_ot-posnr = item.
  it_ot-vbeln = tabi_guias_por_item-guia.
  APPEND it_ot. CLEAR it_ot.

ENDFORM.                    " actualiza_tab_de_ot

*&---------------------------------------------------------------------*
*&      Form  modifica_items
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SUB_ITEM_VGPOS  text
*----------------------------------------------------------------------*
FORM modifica_items  USING p_item p_tip.

  CLEAR l_primeiro. l_primeiro = 1.
  CLEAR: bdcdata, bdcdata[], l_ultimo.

*-----------------------------------processar todos os items a eliminar
  LOOP AT it_lips_aux WHERE vgpos = p_item.    "sub_item-vgpos.


    PERFORM elimina_item_r USING it_lips_aux-posnr l_ultimo
                                 l_primeiro
                        CHANGING g_erro.
  ENDLOOP.

*--------------------------------Gravar os items marcados para eliminar
  IF NOT bdcdata[] IS INITIAL.

    l_ultimo = 'X'.
    PERFORM elimina_item_r USING p_item l_ultimo
                                 l_primeiro
                        CHANGING g_erro.

    CLEAR: bdcdata, bdcdata[], l_ultimo.


  ENDIF.
*----------No final do processamento de um item tan vamos fazer a cópia
  IF g_erro IS INITIAL.

*-----------------------------copiar para a remessa o item da encomenda
    READ TABLE it_grp_item WITH
                           KEY guia  = tabi_guias_por_item-guia
                               vbeln = z1com_s-vbeln
                               vgpos = p_item.   "sub_item-vgpos
    .
*    IF p_tip = 'TAN'.
*      it_grp_item-quant_cf = z1com_s-quant_cf.
*    ENDIF.
    IF sy-subrc = 0 AND
       it_grp_item-quant_cf GT 0.
*--------------------------------------------------------------Criar OT
      PERFORM actualiza_tab_de_ot USING it_grp_item-vgpos.

      PERFORM cria_item_ref_encomenda USING it_grp_item-vgpos
                                            it_grp_item-quant_cf
                                            it_grp_item-vrkme
                                            p_tip
                                   CHANGING g_erro.
    ENDIF.

  ENDIF.

ENDFORM.                    " modifica_items

*&---------------------------------------------------------------------*
*&      Form  inicializa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inicializa .
  CLEAR: it_grp_item[], it_grp_item, sub_item, sub_item[],
         it_serv, it_serv[], it_ltap_all[].
ENDFORM.                    " inicializa

*&---------------------------------------------------------------------*
*&      Form  calcula_num_remessas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VBSS_SAMMG  text
*      -->P_G_PAL  text
*----------------------------------------------------------------------*
FORM calcula_num_remessas  USING    p_grupo
                                    p_pal.
  CLEAR p_pal.
  SELECT COUNT( DISTINCT rbnum ) INTO p_pal FROM  t311a
*         WHERE  lgnum  = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:49:55
         WHERE  lgnum  = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:49:57
         AND    refnr  = p_grupo.


ENDFORM.                    " calcula_num_remessas

*&---------------------------------------------------------------------*
*&      Form  cria_linha_de_remessa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_linha_de_remessa USING rc.
  break roffd.
  DATA : n_pal(3), xc_modo VALUE 'N'.
  DATA: lv_find.
  DATA: lt_vepo LIKE vepo OCCURS 0 WITH HEADER LINE.
  DATA lv_werks TYPE werks_d. " << INS ROFF(SDF):TMGP:06.01.2016 17:09:38
  DATA lv_lgort TYPE lgort_d.
  DATA: lv_pstyv  TYPE pstyv_vl.
  DATA: lv_subrc TYPE sysubrc.
  DATA: lv_type TYPE c.

  CLEAR l_tabix.

  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user      = sy-uname
      i_refnr     = vbss-sammg
      i_recall    = 'X'
      i_usewm     = 'X'
      i_userf     = 'X'
      i_usemm     = 'X'
      i_useaut    = 'X'
      i_get_lgnum = 'X'
      i_get_werks = 'X'
      i_get_lgort = 'X'
    CHANGING
      c_lgnum     = gv_lgnum
      c_werks     = lv_werks
      c_lgort     = lv_lgort
    EXCEPTIONS
      error       = 1
      user_back   = 2
      OTHERS      = 3.


*& Begin of Modification by Tiago Pateiro - ROFF @ 06.01.2016 17:08:24
**  SELECT werks UP TO 1 ROWS
**    FROM t320 INTO lv_werks
**    WHERE lgort EQ 'CD'
**      AND lgnum EQ gv_lgnum.
**  ENDSELECT.
**  IF sy-subrc NE 0.
**    lv_werks = 'RENV'.
**  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 06.01.2016 17:08:24

  CALL FUNCTION 'ZWM_CHECK_ADD_PALLET_TO_DELV'
    EXPORTING
*     i_lgnum = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:50:07
      i_lgnum = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:50:08
      i_refnr = vbss-sammg
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.


  SORT it_serv BY exidv.
  DELETE ADJACENT DUPLICATES FROM it_serv COMPARING exidv.

  SORT it_serv BY kunnr.

*--------------------para cada sscc vai obter a informação complementar
  LOOP AT it_serv.

    CLEAR l_tabix.
    l_tabix = sy-tabix.

    SELECT venum vhilm INTO (it_serv-venum, it_serv-vhilm) UP TO 1 ROWS
                 FROM  vekp
                 WHERE exidv  = it_serv-exidv.
    ENDSELECT.

    MODIFY it_serv INDEX l_tabix.
    CLEAR l_tabix.

  ENDLOOP.
*------------------------contar as paletes por cliente, criar o registo
*----------------------------que vai dar origem ao novo item da remessa
  SORT it_serv BY kunnr vhilm vbeln_gen exidv .
  LOOP AT it_serv.
    CLEAR emb_serv.
    emb_serv = it_serv.

    AT NEW kunnr.
      CLEAR n_pal.
    ENDAT.

    CLEAR zwm026.
    SELECT SINGLE * FROM zwm026
*        WHERE armazem = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:50:18
        WHERE armazem = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:50:22
          AND sscc = emb_serv-exidv.
    IF sy-subrc = 0.
      ADD 1 TO n_pal.
***      NS ROFF  - nova regra para paletes de picking... Agosto 2013.


      CLEAR lt_vepo. REFRESH: lt_vepo.

      SELECT  * INTO TABLE lt_vepo
          FROM vepo
              WHERE venum = emb_serv-venum.

      LOOP AT lt_vepo.

        CLEAR vbpa.
        SELECT SINGLE kunnr INTO vbpa-kunnr
            FROM vbpa
                WHERE vbeln = emb_serv-vbeln_gen
                  AND posnr = 0
                  AND parvw = 'W1'.

        CLEAR zwm046.

        lv_find = 'X'.
        SELECT SINGLE *
            FROM zwm046
                WHERE kunnr = vbpa-kunnr
                  AND matnr = lt_vepo-matnr.
        IF sy-subrc <> 0.
          SELECT SINGLE *
            FROM zwm046
                WHERE kunnr = ''
                  AND matnr = lt_vepo-matnr.
          IF sy-subrc <> 0.
            CLEAR: lv_find.
          ENDIF.
        ENDIF.


        IF lv_find = 'X'.

          IF NOT zwm046-mpalete IS INITIAL.
            CLEAR zwm001.
            SELECT SINGLE *
                FROM zwm001
*                    WHERE armazem = z1om_lgnum AND " << DEL ROFF(SDF):TMGP:06.01.2016 16:50:53
                    WHERE armazem = gv_lgnum AND " << INS ROFF(SDF):TMGP:06.01.2016 16:50:34
                          processo = 'MEIA-PALETE' AND
                          parametro = emb_serv-vhilm.
            IF sy-subrc = 0.
              SORT it_linrem.
              READ TABLE it_linrem WITH KEY vhilm = zwm001-valor
                                            vbeln = emb_serv-vbeln_gen.
              IF sy-subrc = 0.
                ADD lt_vepo-vemng TO it_linrem-qtd.
                MODIFY it_linrem INDEX sy-tabix.
              ELSE.
                it_linrem-vhilm = zwm001-valor.
                it_linrem-qtd   = 2.
                it_linrem-vbeln = emb_serv-vbeln_gen.
                APPEND it_linrem.
                CLEAR it_linrem.
              ENDIF.
            ENDIF.

          ELSEIF NOT zwm046-qpalete IS INITIAL.
            CLEAR zwm001.
            SELECT SINGLE *
                FROM zwm001
*                    WHERE armazem = z1om_lgnum       AND " << DEL ROFF(SDF):TMGP:06.01.2016 16:50:57
                    WHERE armazem = gv_lgnum       AND " << INS ROFF(SDF):TMGP:06.01.2016 16:50:58
                          processo = 'QUARTO-PALETE' AND
                          parametro = emb_serv-vhilm.
            IF sy-subrc = 0.
              SORT it_linrem.
              READ TABLE it_linrem WITH KEY vhilm = zwm001-valor
                                            vbeln = emb_serv-vbeln_gen.
              IF sy-subrc = 0.
                ADD lt_vepo-vemng TO it_linrem-qtd.
                MODIFY it_linrem INDEX sy-tabix.
              ELSE.
                it_linrem-vhilm = zwm001-valor.
                it_linrem-qtd   = 4.
                it_linrem-vbeln = emb_serv-vbeln_gen.
                APPEND it_linrem.
                CLEAR it_linrem.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ELSE.
      CLEAR vepo.
      SELECT SINGLE *
          FROM vepo
              WHERE venum = emb_serv-venum.

      CLEAR vbpa.
      SELECT SINGLE kunnr INTO vbpa-kunnr
          FROM vbpa
              WHERE vbeln = emb_serv-vbeln_gen
                AND posnr = 0
                AND parvw = 'W1'.

      CLEAR zwm046.

      lv_find = 'X'.
      SELECT SINGLE *
          FROM zwm046
              WHERE kunnr = vbpa-kunnr
                AND matnr = vepo-matnr.
      IF sy-subrc <> 0.
        SELECT SINGLE *
          FROM zwm046
              WHERE kunnr = ''
                AND matnr = vepo-matnr.
        IF sy-subrc <> 0.
          CLEAR: lv_find.
          ADD 1 TO n_pal.
        ENDIF.
      ENDIF.


      IF lv_find = 'X'.
        IF NOT zwm046-palete IS INITIAL.
          ADD 1 TO n_pal.
        ENDIF.

        IF NOT zwm046-mpalete IS INITIAL.
          CLEAR zwm001.
          SELECT SINGLE *
              FROM zwm001
*                  WHERE armazem = z1om_lgnum AND " << DEL ROFF(SDF):TMGP:06.01.2016 16:51:09
                  WHERE armazem = gv_lgnum AND " << INS ROFF(SDF):TMGP:06.01.2016 16:51:10
                        processo = 'MEIA-PALETE' AND
                        parametro = emb_serv-vhilm.
          IF sy-subrc = 0.
            SORT it_linrem.
            READ TABLE it_linrem WITH KEY vhilm = zwm001-valor
                                          vbeln = emb_serv-vbeln_gen.
            IF sy-subrc = 0.
              ADD 2 TO it_linrem-qtd.
              MODIFY it_linrem INDEX sy-tabix.
            ELSE.
              it_linrem-vhilm = zwm001-valor.
              it_linrem-qtd   = 2.
              it_linrem-vbeln = emb_serv-vbeln_gen.
              APPEND it_linrem.
              CLEAR it_linrem.
            ENDIF.
          ENDIF.

        ELSEIF NOT zwm046-qpalete IS INITIAL.
          CLEAR zwm001.
          SELECT SINGLE *
              FROM zwm001
*                  WHERE armazem = z1om_lgnum       AND " << DEL ROFF(SDF):TMGP:06.01.2016 16:51:19
                  WHERE armazem = gv_lgnum       AND " << INS ROFF(SDF):TMGP:06.01.2016 16:51:22
                        processo = 'QUARTO-PALETE' AND
                        parametro = emb_serv-vhilm.
          IF sy-subrc = 0.
            SORT it_linrem.
            READ TABLE it_linrem WITH KEY vhilm = zwm001-valor
                                          vbeln = emb_serv-vbeln_gen.
            IF sy-subrc = 0.
              ADD 4 TO it_linrem-qtd.
              MODIFY it_linrem INDEX sy-tabix.
            ELSE.
              it_linrem-vhilm = zwm001-valor.
              it_linrem-qtd   = 4.
              it_linrem-vbeln = emb_serv-vbeln_gen.
              APPEND it_linrem.
              CLEAR it_linrem.
            ENDIF.
          ENDIF.
        ENDIF.
*
*      ELSE.
*        ADD 1 TO n_pal.
*      ENDIF.
      ENDIF.

    ENDIF.

    AT END OF vhilm.

      it_linrem-vhilm = emb_serv-vhilm.
      it_linrem-qtd   = n_pal.
      it_linrem-vbeln = emb_serv-vbeln_gen.
      APPEND it_linrem.
      CLEAR : it_linrem, n_pal.

    ENDAT.

  ENDLOOP.

** Verificar se para o cliente/material precisam de ser inseridas
** as semi-paletes

**


  LOOP AT it_linrem.
    lv_pstyv = 'ZPAS'.
    PERFORM check_packing_skip USING it_linrem-vbeln CHANGING lv_subrc lv_type.
    IF lv_subrc <> 0.
*      AND lv_type EQ 'T'.
      lv_pstyv = 'ZPA1'.
    ENDIF.


    CLEAR: bdcdata[], bdcdata.

    PERFORM bdc_dynpro   USING 'SAPMV50A'            '4004'.
    PERFORM bdc_field    USING 'BDC_CURSOR'          'LIKP-VBELN'.
    PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
    PERFORM bdc_field    USING 'LIKP-VBELN'          it_linrem-vbeln.

    PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
    PERFORM bdc_field    USING 'BDC_OKCODE'          '=POAN_T'.

    PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
    PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
    PERFORM bdc_field    USING 'LIPS-MATNR(03)'      it_linrem-vhilm.
*    'PCHEP'.
    PERFORM bdc_field    USING 'LIPSD-G_LFIMG(03)'   it_linrem-qtd.
*    '4'.
    PERFORM bdc_field    USING 'LIPS-PSTYV(03)'      lv_pstyv.
*    PERFORM bdc_field    USING 'LIPS-WERKS(03)'      'RENV'. " << DEL ROFF(SDF):TMGP:06.01.2016 17:09:13
    PERFORM bdc_field    USING 'LIPS-WERKS(03)'      lv_werks. " << INS ROFF(SDF):TMGP:06.01.2016 17:09:14
    PERFORM bdc_field    USING 'LIPS-LGORT(03)'      lv_lgort.

    PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
    PERFORM bdc_field    USING 'BDC_OKCODE'          '=SICH_T'.
    CLEAR: it_msg, it_msg[].

*----------------------------------Verifica se a remessa está bloqueada
    g_bloq = 2.
    WHILE g_bloq = 2.
      PERFORM gr_bloqueado USING 'REM_SERV' CHANGING g_bloq.
    ENDWHILE.


    CALL TRANSACTION 'VL02N' USING bdcdata
                            UPDATE 's'
                            MODE xc_modo
                            MESSAGES INTO it_msg.
    CLEAR l_zztpdoc.
    l_zztpdoc = 'REM_SERV'.

    PERFORM log USING tabi_guias_por_item-guia ''.

*--------a estrutura wa_z1com_s-quant_cf tem a informação sobre o bonus

    COMMIT WORK AND WAIT. WAIT UP TO 5 SECONDS.


    CLEAR: bdcdata, bdcdata[], it_msg, it_msg[].

  ENDLOOP.

  CLEAR   it_linrem.
  REFRESH it_linrem.

ENDFORM.                    " cria_linha_de_remessa

*&---------------------------------------------------------------------*
*&      Form  remessa_com_mat_pal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VBSS_SAMMG  text
*----------------------------------------------------------------------*
FORM remessa_com_mat_pal  USING    p_grupo.

  DATA : it_rem LIKE tabi_guias_por_item OCCURS 0 WITH HEADER LINE.
  DATA : wa_item LIKE z1com_s.

  CLEAR : it_rem, it_rem[], it_rem-linha, it_rem-linha[].
  it_rem[] = tabi_guias_por_item[].
  LOOP AT it_rem.

    CHECK g_erro IS INITIAL.

    LOOP AT it_rem-linha INTO wa_item.

      IF wa_item-matnr = 'PCHEP' OR wa_item-matnr = 'PEURO' OR
         wa_item-matnr = 'PROUGE' OR wa_item-matnr = 'PROUG' OR
         wa_item-matnr = 'MCHEP'.

        CLEAR: l_zztpdoc, g_dummy.
        MESSAGE e000(zwmmsg001) WITH 'A Remessa'(019) it_rem-guia
                                     'já tem material'(020) wa_item-matnr
           INTO g_dummy.

        CONCATENATE 'SR_' l_idx INTO l_zztpdoc.
        CONDENSE l_zztpdoc NO-GAPS.
*------------------------------------------------------------

        PERFORM log USING z1com_s-vbeln ' '.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = it_msg-msgid
            message_lang   = sy-langu
            message_type   = it_msg-msgtyp
            message_number = it_msg-msgnr
            message_var1   = it_msg-msgv1
            message_var2   = it_msg-msgv2
            message_var3   = it_msg-msgv3
            message_var4   = it_msg-msgv4.

      ENDIF.
      IF g_erro NE space.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " remessa_com_mat_pal
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_GUIA_MEIA_PALETES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modifica_guia_meia_paletes .
  break roffd.
** Verificar se o cliente/material têm gestão de meias-paletes
  DATA: BEGIN OF st_pal,
          vbeln LIKE likp-vbeln,
          vhilm LIKE vekp-vhilm,
          n_pal LIKE lips-lfimg,
          exida TYPE exida,
        END OF st_pal.

  DATA: lt_vekp LIKE vekp    OCCURS 0 WITH HEADER LINE,
        lt_vepo LIKE vepo    OCCURS 0 WITH HEADER LINE,
        lt_ltap LIKE st_ltap OCCURS 0 WITH HEADER LINE,
        lt_pal  LIKE st_pal  OCCURS 0 WITH HEADER LINE,
        lt_mpal LIKE st_pal  OCCURS 0 WITH HEADER LINE,
        lt_lips LIKE lips    OCCURS 0 WITH HEADER LINE,
        wa_vepo TYPE vepo.

  DATA: save_index LIKE sy-tabix.
  DATA: d_modo     VALUE 'N'.

*** Paletes de picking.
*  CLEAR:   lt_vekp, lt_vepo, lt_ltap, lt_pal, lt_mpal, it_linrem.
*  REFRESH: lt_vekp, lt_vepo, lt_ltap, lt_pal, lt_mpal, it_linrem.
*
*  LOOP AT it_ltap WHERE ord = 3.
*    MOVE-CORRESPONDING it_ltap TO lt_ltap.
*    APPEND lt_ltap.
*  ENDLOOP.
*
*  IF NOT lt_ltap[] IS INITIAL.
*    SELECT * INTO TABLE lt_vekp
*        FROM vekp
*            FOR ALL ENTRIES IN lt_ltap
*                WHERE exidv = lt_ltap-sscc.
*
*    IF NOT lt_vekp[] IS INITIAL.
*      SELECT * INTO TABLE lt_vepo
*          FROM vepo
*              FOR ALL ENTRIES IN lt_vekp
*              WHERE venum = lt_vekp-venum.
*
*      IF NOT lt_vepo[] IS INITIAL.
*
*        SORT: lt_vekp, lt_vepo.
*        LOOP AT lt_vepo.
*
*          CLEAR vbpa.
*          SELECT SINGLE kunnr INTO vbpa-kunnr
*              FROM vbpa
*                  WHERE vbeln =  tabi_guias_por_item-guia
*                    AND posnr = 0
*                    AND parvw = 'W1'.
*
*          SELECT SINGLE *
*              FROM zwm046
*                  WHERE kunnr = vbpa-kunnr
*                     AND matnr = lt_vepo-matnr.
*          IF sy-subrc = 0.
*
*            IF zwm046-palete IS INITIAL.
*              READ TABLE lt_vekp WITH KEY venum = lt_vepo-venum.
*              IF sy-subrc = 0.
*                CLEAR lt_pal.
*                lt_pal-vbeln = tabi_guias_por_item-guia.
*                lt_pal-vhilm = lt_vekp-vhilm.
*                lt_pal-n_pal = 1.
*                COLLECT lt_pal.
*              ENDIF.
*            ENDIF.
*
*            IF NOT zwm046-mpalete IS INITIAL.
*              READ TABLE lt_vekp WITH KEY venum = lt_vepo-venum.
*              IF sy-subrc = 0.
*                CLEAR zwm001.
*                SELECT SINGLE *
*                    FROM zwm001
*                        WHERE armazem = z1om_lgnum AND
*                              processo = 'MEIA-PALETE' AND
*                              parametro = lt_vekp-vhilm.
*
*                IF sy-subrc = 0.
*                  CLEAR lt_mpal.
*                  lt_mpal-vbeln = tabi_guias_por_item-guia.
*                  lt_mpal-vhilm = zwm001-valor.
*                  lt_mpal-n_pal = 2.
*                  COLLECT lt_mpal.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*    ENDIF.
*  ENDIF.

** Paletes Completas
  CLEAR:   lt_vekp, lt_vepo, lt_ltap, lt_pal, lt_mpal, it_linrem, wa_vepo.
  REFRESH: lt_vekp, lt_vepo, lt_ltap, lt_pal, lt_mpal, it_linrem.

  LOOP AT it_ltap WHERE ord <> 2.
    MOVE-CORRESPONDING it_ltap TO lt_ltap.
    APPEND lt_ltap.
  ENDLOOP.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  IF NOT lt_ltap[] IS INITIAL.
    SELECT * INTO TABLE lt_vekp
        FROM vekp
            FOR ALL ENTRIES IN lt_ltap
                WHERE exidv = lt_ltap-sscc.

    IF NOT lt_vekp[] IS INITIAL.
      SELECT * INTO TABLE lt_vepo
          FROM vepo
              FOR ALL ENTRIES IN lt_vekp
                  WHERE venum = lt_vekp-venum.

      IF NOT lt_vepo[] IS INITIAL.

        SORT: lt_vekp, lt_vepo.
        DELETE ADJACENT DUPLICATES FROM lt_vepo COMPARING venum.
        LOOP AT lt_vepo.

          CLEAR vbpa.
          SELECT SINGLE kunnr INTO vbpa-kunnr
              FROM vbpa
                  WHERE vbeln =  tabi_guias_por_item-guia
                    AND posnr = 0
                    AND parvw = 'W1'.

**  no caso de hierarqui de palete com duas meias paletes
** a palete pai nao tem referencia ao material:
          IF lt_vepo-matnr IS INITIAL.
            SELECT SINGLE * INTO wa_vepo FROM vepo
            WHERE venum = lt_vepo-unvel.
            IF sy-subrc = 0.
              MOVE wa_vepo-matnr TO lt_vepo-matnr.
            ENDIF.
          ENDIF.
***
          SELECT SINGLE *
              FROM zwm046
                  WHERE kunnr = vbpa-kunnr
                    AND matnr = lt_vepo-matnr.
          IF sy-subrc <> 0.

*         para um determinado material que esteja devidamente parametrizado no código depósito bloco,
*         que este seja expedido em qchep para qualquer cliente (através do embalamento-ZWM064).

            SELECT SINGLE *
              FROM zwm046
                  WHERE kunnr = '' AND
                        matnr = lt_vepo-matnr .
            IF sy-subrc <> 0.
**            tabela de execpcoes.
*              caso nao exista na tabela entao nao queremos mexer em nada
*             logo vamos contabilizar o numero que queremos que fique em pack mat.
*             nas linhas da remessa
              zwm046-palete  = 'X'.
*              zwm046-mpalete = 'X'.
*              zwm046-qpalete = 'X'.
            ENDIF.

          ENDIF.

          IF NOT zwm046-palete IS INITIAL.
            READ TABLE lt_vekp WITH KEY venum = lt_vepo-venum.
            IF sy-subrc = 0.
              CLEAR lt_pal.
              lt_pal-vbeln = tabi_guias_por_item-guia.
              lt_pal-vhilm = lt_vekp-vhilm.
              lt_pal-n_pal = 1.
*              lt_pal-exida = lt_vekp-exida.
              COLLECT lt_pal.
            ENDIF.
          ENDIF.


          IF NOT zwm046-mpalete IS INITIAL.
            READ TABLE lt_vekp WITH KEY venum = lt_vepo-venum.
            IF sy-subrc = 0.
              CLEAR zwm001.
              SELECT SINGLE *
                  FROM zwm001
*                      WHERE armazem   = z1om_lgnum    AND " << DEL ROFF(SDF):TMGP:06.01.2016 16:51:33
                      WHERE armazem   = gv_lgnum    AND " << INS ROFF(SDF):TMGP:06.01.2016 16:51:34
                            processo  = 'MEIA-PALETE' AND
                            parametro = lt_vekp-vhilm.
              IF sy-subrc = 0.
                CLEAR lt_mpal.
                lt_mpal-vbeln = tabi_guias_por_item-guia.
                lt_mpal-vhilm = zwm001-valor.
                READ TABLE it_ltap WITH KEY sscc = lt_vekp-exidv.

**              Verificar quantas Meias paletes de picking
                IF it_ltap-ord = 3.
                  lt_mpal-n_pal = 1.

                ELSE.
                  CLEAR mlgn.
                  SELECT SINGLE *
                      FROM mlgn
                          WHERE matnr = lt_vepo-matnr
*                            AND lgnum = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:51:44
                            AND lgnum = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:51:46
                            AND ( block = '03' OR block = '04' OR
                                  block = '05' OR block = '06').
                  IF mlgn-block = '03' OR mlgn-block = '04'.
                    lt_mpal-n_pal = 2.  " palete completa
                  ELSEIF mlgn-block = '05' OR mlgn-block = '06'.
                    lt_mpal-n_pal = 4.  " palete completa
                  ENDIF.
                ENDIF.
                COLLECT lt_mpal.
              ENDIF.
            ENDIF.

** INI - Entrada de Quarto Palete.
          ELSEIF NOT zwm046-qpalete IS INITIAL.

            READ TABLE lt_vekp WITH KEY venum = lt_vepo-venum.
            IF sy-subrc = 0.

              SELECT SINGLE *
                FROM zwm001
*                WHERE armazem   = z1om_lgnum      AND " << DEL ROFF(SDF):TMGP:06.01.2016 16:51:57
                WHERE armazem   = gv_lgnum      AND " << INS ROFF(SDF):TMGP:06.01.2016 16:51:59
                      processo  = 'QUARTO-PALETE' AND
                      parametro = lt_vekp-vhilm.
              IF sy-subrc = 0.
                CLEAR lt_mpal.
                lt_mpal-vbeln = tabi_guias_por_item-guia.
                lt_mpal-vhilm = zwm001-valor.

                READ TABLE it_ltap WITH KEY sscc = lt_vekp-exidv.
                IF it_ltap-ord = 3.

**                Verificar quantas quarto de paletes de picking
                  READ TABLE it_sscc_pick WITH KEY sscc = lt_vekp-exidv.
                  IF sy-subrc = 0.
                    lt_mpal-n_pal = it_sscc_pick-n_qchep.
                  ELSE.
                    lt_mpal-n_pal = 1.
                  ENDIF.

                ELSE.
                  CLEAR mlgn.
                  SELECT SINGLE *
                      FROM mlgn
                          WHERE matnr = lt_vepo-matnr
*                            AND lgnum = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:52:11
                            AND lgnum = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:52:13
                            AND ( block = '07' OR block = '08' OR
                                  block = '09' OR block = '10').
                  IF mlgn-block = '07' OR mlgn-block = '08'.
                    lt_mpal-n_pal = 4.  " palete completa
                  ELSEIF mlgn-block = '09' OR mlgn-block = '10'.
                    lt_mpal-n_pal = 8.  " palete completa
                  ENDIF.
                ENDIF.
                COLLECT lt_mpal.
              ENDIF.
            ENDIF.
** FIM - Entrada de Quarto Palete.
          ENDIF.

*          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

*  DATA: BEGIN OF lin_rem,
*    vhilm     LIKE vekp-vhilm,
*    qtd(17),
*    vbeln     LIKE likp-vbeln,
*  END OF lin_rem.
*
*  DATA it_linrem LIKE lin_rem OCCURS 0 WITH HEADER LINE.
*
*  SELECT * INTO TABLE lt_lips
*      FROM lips
*          WHERE vbeln = tabi_guias_por_item-guia.
*
*  DELETE lt_lips WHERE pstyv <> 'ZPAL' AND pstyv <> 'ZPAS'.
*
*  SORT lt_lips BY matnr.
*
*
*
*
***** nao podemos criar linahs à mão temos de manter o packing material que foi passado automaticamente
***** linhas do tipo 9000x.
***** no maximo mudamos a quantidade ou eliminamos a linha por completo.
*  DATA: n_pal TYPE i.
*
*  LOOP AT lt_lips.
*    READ TABLE lt_pal WITH KEY vhilm = lt_lips-matnr.
*    IF sy-subrc = 0.
*
**--------------------------------------------------------------------*
**  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
**  Data: 13.11.2012 16:52:38
**  Motivo: Toptal de MPAL
**--------------------------------------------------------------------*
*      READ TABLE lt_mpal WITH KEY vhilm = lt_lips-matnr.
*
*      IF sy-subrc EQ 0.
*        n_pal = lt_pal-n_pal + lt_mpal-n_pal.
*        PERFORM update_item USING lt_lips-posnr n_pal.
*
*      ELSE.
**--------------------------------------------------------------------*
** FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
**--------------------------------------------------------------------*
***   é palete pai e temos quantidade registada
*        IF lt_lips-lfimg <> lt_pal-n_pal.
*          PERFORM update_item USING lt_lips-posnr lt_pal-n_pal.
*        ENDIF.
*
*      ENDIF.
*
*
*
*
*    ELSE.
*      READ TABLE lt_mpal WITH KEY vhilm = lt_lips-matnr.
*      IF sy-subrc = 0.
***   é palete filha e temos quantidade registada
*        IF lt_lips-lfimg <> lt_mpal-n_pal.
*          PERFORM update_item USING lt_lips-posnr lt_mpal-n_pal.
*        ENDIF.
*      ELSE.
***    nem é pai nem filha apaga!
*        PERFORM delete_item USING lt_lips-posnr.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.



*** versao antiga ...


*
*** Verificar se o numero de paletes embaladas é o correcto
*  IF NOT lt_pal[] IS INITIAL.
*
**    SELECT * INTO TABLE lt_lips
**        FROM lips
**            WHERE vbeln = tabi_guias_por_item-guia.
**
**    DELETE lt_lips WHERE pstyv <> 'ZPAL' AND pstyv <> 'ZPAS'.
**
**    SORT lt_lips BY matnr.
*
*    CLEAR save_index.
*
*    LOOP AT lt_pal.
*      save_index = sy-tabix.
*      READ TABLE lt_lips WITH KEY matnr = lt_pal-vhilm.
*
*      MOVE-CORRESPONDING lt_pal TO it_linrem.
*      it_linrem-qtd = lt_lips-lfimg - lt_pal-n_pal.
*      APPEND it_linrem.
*      CLEAR it_linrem.
*** Apagar a linha da remessa
*      PERFORM delete_item USING lt_lips-posnr.
*
*    ENDLOOP.
*
*  ENDIF.
*  break roffd.
**** Para ficar generico para todos os casos caso existam
**** APAGAR AS MEIAS PALETES QUE JÁ EXISTIREM NA GUIA
*  LOOP AT lt_lips.
*    READ TABLE lt_mpal WITH KEY vhilm = lt_lips-matnr.
*    CHECK sy-subrc = 0.
*** Apagar a linha da remessa
*    PERFORM delete_item USING lt_lips-posnr.
*
*  ENDLOOP.
*
*
*  LOOP AT lt_mpal.
*    MOVE-CORRESPONDING lt_mpal TO it_linrem.
*    WRITE lt_mpal-n_pal TO it_linrem-qtd.
*    APPEND it_linrem.
*    CLEAR it_linrem.
*  ENDLOOP.
*
*  DELETE it_linrem WHERE qtd IS INITIAL.
*
*  LOOP AT it_linrem.
*    CLEAR: bdcdata[], bdcdata.
*
*    PERFORM bdc_dynpro   USING 'SAPMV50A'            '4004'.
*    PERFORM bdc_field    USING 'BDC_CURSOR'          'LIKP-VBELN'.
*    PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
*    PERFORM bdc_field    USING 'LIKP-VBELN'          it_linrem-vbeln.
*
*    PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
*    PERFORM bdc_field    USING 'BDC_OKCODE'          '=POAN_T'.
*
*    PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
*    PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
*    PERFORM bdc_field    USING 'LIPS-MATNR(03)'      it_linrem-vhilm.
*
*    PERFORM bdc_field    USING 'LIPSD-G_LFIMG(03)'   it_linrem-qtd.
*
*    PERFORM bdc_field    USING 'LIPS-PSTYV(03)'      'ZPAS'.
*    PERFORM bdc_field    USING 'LIPS-WERKS(03)'      'RENV'.
*    PERFORM bdc_field    USING 'LIPS-LGORT(03)'      'CD'.
*
*    PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
*    PERFORM bdc_field    USING 'BDC_OKCODE'          '=SICH_T'.
*    CLEAR: it_msg, it_msg[].
*
*    g_bloq = 2.
*    WHILE g_bloq = 2.
*      PERFORM gr_bloqueado USING 'REM_SAVE' CHANGING g_bloq.
*    ENDWHILE.
*
*    CALL TRANSACTION 'VL02N' USING bdcdata
*          UPDATE 's'
*          MODE d_modo
*          MESSAGES INTO it_msg.
*
*    COMMIT WORK AND WAIT. WAIT UP TO 5 SECONDS.
*    CLEAR: bdcdata, bdcdata[], it_msg, it_msg[].
*
*  ENDLOOP.

  CLEAR   it_linrem.
  REFRESH it_linrem.
ENDFORM.                    " MODIFICA_GUIA_MEIA_PALETES
*&---------------------------------------------------------------------*
*&      Form  DELETE_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_item USING item.

  DATA: d_modo   VALUE 'N', wa_posnr TYPE posnr.

  CLEAR: bdcdata, bdcdata[], wa_posnr.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '4004'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'LIKP-VBELN'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '/00'.
  PERFORM bdc_field    USING 'LIKP-VBELN'            tabi_guias_por_item-guia.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=POPO_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'LIPS-MATNR(01)'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '0111'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'RV50A-POSNR'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=WEIT'.

  PERFORM bdc_field    USING 'RV50A-POSNR'           item.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=POLO_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'LIPS-POSNR(01)'.
  PERFORM bdc_field    USING 'RV50A-LIPS_SELKZ(01)'  'X'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=SICH_T'.

  g_bloq = 2.
  WHILE g_bloq = 2.
    PERFORM gr_bloqueado USING 'REM_DELE' CHANGING g_bloq.
  ENDWHILE.

  CALL TRANSACTION 'VL02N' USING bdcdata
        UPDATE 's'
        MODE d_modo
        MESSAGES INTO it_msg.

  COMMIT WORK AND WAIT. WAIT UP TO 5 SECONDS.

  CLEAR: bdcdata, bdcdata[], it_msg, it_msg[].

ENDFORM.                    " DELETE_ITEM


*&---------------------------------------------------------------------*
*&      Form  update_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ITEM       text
*      -->QUANT      text
*----------------------------------------------------------------------*
FORM update_item USING item quant.

  DATA: d_modo   VALUE 'N', wa_posnr TYPE posnr.
  DATA: qtd(10).
  WRITE quant TO qtd.
  CLEAR: bdcdata, bdcdata[], wa_posnr.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '4004'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'LIKP-VBELN'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '/00'.
  PERFORM bdc_field    USING 'LIKP-VBELN'            tabi_guias_por_item-guia.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=POPO_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'LIPS-MATNR(01)'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '0111'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'RV50A-POSNR'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=WEIT'.

  PERFORM bdc_field    USING 'RV50A-POSNR'           item.


  PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
  PERFORM bdc_field    USING 'LIPSD-G_LFIMG(01)'     qtd.
  PERFORM bdc_field    USING 'BDC_SUBSCR'
        'SAPMV50A                                1502SUBSCREEN_HEADER'.
  PERFORM bdc_field    USING 'BDC_SUBSCR'
        'SAPMV50A                                1102SUBSCREEN_BODY'.
  PERFORM bdc_field    USING 'BDC_SUBSCR'
        'SAPMV50A                                0611SUBSCREEN_BOTTOM'.
  PERFORM bdc_field    USING 'BDC_SUBSCR'
        'SAPMV50A                                1708SUBSCREEN_ICONBAR'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=SICH_T'.

  g_bloq = 2.
  WHILE g_bloq = 2.
    PERFORM gr_bloqueado USING 'REM_DELE' CHANGING g_bloq.
  ENDWHILE.

  CALL TRANSACTION 'VL02N' USING bdcdata
        UPDATE 's'
        MODE d_modo
        MESSAGES INTO it_msg.

  COMMIT WORK AND WAIT. WAIT UP TO 5 SECONDS.

  CLEAR: bdcdata, bdcdata[], it_msg, it_msg[].

ENDFORM.                    " update_ITEM


*&---------------------------------------------------------------------*
*&      Form  pack_in_transportation_append
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pack_in_transportation_append.
  DATA: lt_vekp TYPE TABLE OF vekp.

  DATA: ls_vekp TYPE vekp,
        ls_vepo TYPE zwm051.

  DATA: lv_2step     TYPE flag,
        lv_packtranp TYPE flag,
        lv_tabix     TYPE sytabix.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

** Valida se Grupo deve ser Embalado
***********************************************************************
  CALL FUNCTION 'ZWM_GROUP_CHECK_PACK_TRANSP'
    EXPORTING
*     i_lgnum     = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:52:25
      i_lgnum     = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:52:26
      i_refnr     = vbss-sammg
    IMPORTING
      e_packtranp = lv_packtranp.

  CHECK lv_packtranp EQ abap_true.

** Retorna Paletes
***********************************************************************
  PERFORM get_paletes.
  CHECK NOT it_serv IS INITIAL.

** Paletes de Grupo
***********************************************************************
*  DO 1 TIMES.
*    CHECK NOT it_serv IS INITIAL.
*
**--> Header
*    SELECT * FROM vekp
*       INTO TABLE lt_vekp
*       FOR ALL ENTRIES IN it_serv
*       WHERE exidv = it_serv-exidv.
*
*    CHECK sy-subrc EQ 0.
*    SORT lt_vekp BY venum.
*
**--> Items
*    SELECT venum vepos matnr
*           charg vbeln posnr
*           vemng vemeh       FROM vepo
*                             INTO CORRESPONDING FIELDS OF TABLE lt_vepo
*                             FOR ALL ENTRIES IN lt_vekp
*                             WHERE venum = lt_vekp-venum.
*
*    lt_vekp_back = lt_vekp.
*
**--> Items com entradas na VEPO
*    LOOP AT lt_vekp INTO ls_vekp.
*      lv_tabix = sy-tabix.
*
*      LOOP AT lt_vepo ASSIGNING <ls_vepo> WHERE venum = ls_vekp-venum.
*        <ls_vepo>-exidv = ls_vekp-exidv.
*      ENDLOOP.
*      CHECK sy-subrc EQ 0.
*
*      DELETE lt_vekp_back INDEX lv_tabix.
*    ENDLOOP.
*
**--> Items sem Entrada na VEPO
*    CHECK NOT lt_vekp_back IS INITIAL.
*
*    SELECT * FROM zwm051
*       INTO TABLE lt_vepo
*       FOR ALL ENTRIES IN lt_vekp_back
*       WHERE venum = lt_vekp_back-venum.
*  ENDDO.
*
*  SORT lt_vepo BY venum.
*
*  lt_vepo_backup = lt_vepo.

** Alimenta Paletes
***********************************************************************
  LOOP AT it_ltap_all.
*    ls_vepo-lgnum = z1om_lgnum. " << DEL ROFF(SDF):TMGP:06.01.2016 16:52:36
    ls_vepo-lgnum = gv_lgnum. " << INS ROFF(SDF):TMGP:06.01.2016 16:52:37
    ls_vepo-refnr = vbss-sammg.
    ls_vepo-vbeln = it_ltap_all-remessa.
    ls_vepo-posnr = it_ltap_all-posnr.
    ls_vepo-exidv = it_ltap_all-sscc.
    ls_vepo-matnr = it_ltap_all-material.
    ls_vepo-charg = it_ltap_all-lote.
    ls_vepo-vemng = it_ltap_all-quantidade.
    ls_vepo-vemeh = it_ltap_all-unidade.
*    APPEND ls_vepo TO gt_vepo.
    COLLECT ls_vepo INTO gt_vepo.
  ENDLOOP.
ENDFORM.                    "pack_in_transportation_append
*&---------------------------------------------------------------------*
*&      Form  DELEVERY_ABAST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delevery_abast CHANGING cv_subrc TYPE sysubrc.

  DATA: lt_messages TYPE tab_bdcmsgcoll.

  CLEAR: cv_subrc.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

  CALL FUNCTION 'ZWM_TO_DELEVERY_ABAST'
    EXPORTING
*     i_lgnum     = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:52:49
      i_lgnum     = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:52:50
      i_refnr     = vbss-sammg
      i_commit    = 'X'
    IMPORTING
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    cv_subrc = 4.
  ENDIF.
ENDFORM.                    " DELEVERY_ABAST
*&---------------------------------------------------------------------*
*&      Form  PACK_IN_TRANSPORTATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pack_in_transportation.
  DATA: lt_messages TYPE tab_bdcmsgcoll,
        lt_zwm051   TYPE TABLE OF zwm051.

  DATA: ls_vepo           TYPE zwm051,
        ls_zwm051         TYPE zwm051,
        ls_pack_in_transp TYPE zwm01_pack_in_transp.

  DATA: lv_vemng TYPE vemng,
        lv_error TYPE flag.

  FIELD-SYMBOLS: <ls_lips> TYPE lips,
                 <ls_vepo> TYPE zwm051.

  CLEAR gt_pack_in_transp.

  CHECK NOT gt_vepo IS INITIAL.

  SORT gt_vbeln_skip.

** Retorna Backups
***********************************************************************
  SELECT * FROM zwm051
     INTO TABLE lt_zwm051
*     WHERE lgnum = z1om_lgnum AND " << DEL ROFF(SDF):TMGP:06.01.2016 16:52:56
     WHERE lgnum = gv_lgnum AND " << INS ROFF(SDF):TMGP:06.01.2016 16:52:57
           refnr = vbss-sammg.

** Adiciona Paletes Apagadas
***********************************************************************
  LOOP AT lt_zwm051 INTO ls_zwm051.
    READ TABLE gt_vepo
      WITH KEY vbeln = ls_zwm051-vbeln
               posnr = ls_zwm051-posnr
               exidv = ls_zwm051-exidv
      TRANSPORTING NO FIELDS.

    CHECK sy-subrc <> 0.

    CLEAR ls_vepo.
    MOVE-CORRESPONDING ls_zwm051 TO ls_vepo.

    APPEND ls_vepo TO gt_vepo.
  ENDLOOP.

** Guarda Backups
***********************************************************************
*  DELETE FROM zwm051 WHERE lgnum = z1om_lgnum AND " << DEL ROFF(SDF):TMGP:06.01.2016 16:53:15
  DELETE FROM zwm051 WHERE lgnum = gv_lgnum AND " << INS ROFF(SDF):TMGP:06.01.2016 16:53:16
                           refnr = vbss-sammg.

  GET TIME.
  CLEAR: lt_zwm051, ls_zwm051.

  LOOP AT gt_vepo INTO ls_vepo.
    READ TABLE gt_vbeln_skip
      WITH KEY table_line = ls_vepo-vbeln
      TRANSPORTING NO FIELDS.

    CHECK sy-subrc <> 0.

    MOVE-CORRESPONDING ls_vepo TO ls_zwm051.

    ls_zwm051-erdat  = sy-datum.
    ls_zwm051-erzeit = sy-uzeit.
    ls_zwm051-ername = sy-uname.

    APPEND ls_zwm051 TO lt_zwm051.
  ENDLOOP.

  MODIFY zwm051 FROM TABLE lt_zwm051.
  COMMIT WORK.

** Separa Remessas
***********************************************************************
  LOOP AT gt_vepo ASSIGNING <ls_vepo>.
    CLEAR ls_pack_in_transp.

    ls_pack_in_transp-exidv = <ls_vepo>-exidv.

    LOOP AT gt_lips ASSIGNING <ls_lips> WHERE matnr = <ls_vepo>-matnr AND
                                              charg = <ls_vepo>-charg AND
                                              lfimg > 0.

      READ TABLE gt_vbeln_skip
        WITH KEY table_line = <ls_lips>-vbeln
        TRANSPORTING NO FIELDS.

      CHECK sy-subrc <> 0.

      ls_pack_in_transp-vbeln = <ls_lips>-vbeln.
      ls_pack_in_transp-posnr = <ls_lips>-posnr.

      IF <ls_lips>-lfimg > <ls_vepo>-vemng.
        lv_vemng = <ls_vepo>-vemng.
      ELSE.
        lv_vemng = <ls_lips>-lfimg.
      ENDIF.

      <ls_lips>-lfimg = <ls_lips>-lfimg - lv_vemng.
      <ls_vepo>-vemng = <ls_vepo>-vemng - lv_vemng.

      ls_pack_in_transp-vemng = lv_vemng.
      ls_pack_in_transp-vrkme = <ls_vepo>-vemeh.

      APPEND ls_pack_in_transp TO gt_pack_in_transp.

      IF <ls_vepo>-vemng EQ 0.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

** parametro se deve parar processo em erro
***********************************************************************
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
*     i_lgnum     = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:53:32
      i_lgnum     = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:53:35
      i_processo  = 'BAPI_SHIPMENT_CHANGE'
      i_parametro = 'ERRORS'
    IMPORTING
      e_valor     = lv_error
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.


** Embala
***********************************************************************
  CALL FUNCTION 'ZWM_PACK_TRANSPORTATION'
    EXPORTING
*     i_lgnum     = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:53:32
      i_lgnum     = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:53:35
      i_refnr     = vbss-sammg
      it_items    = gt_pack_in_transp
    IMPORTING
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.

    IF NOT lv_error IS INITIAL.
      g_erro = 'X'.
    ENDIF.

    EXIT.
  ENDIF.

  CLEAR: gt_vepo.
ENDFORM.                    " PACK_IN_TRANSPORTATION

*&---------------------------------------------------------------------*
*&      Form  PACK_TRANSPORTATION_FINAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pack_transportation_final.
  DATA: lt_t311a            TYPE TABLE OF t311a,
        lt_zwm051           TYPE TABLE OF zwm051,
        lt_lips             TYPE TABLE OF lips,
        lt_pack_in_transp   TYPE zwm01_t_pack_in_transp,
        lt_pack_in_transp_h TYPE zwm01_t_pack_in_transp,
        lt_pack_in_transp_c TYPE zwm01_t_pack_in_transp,
        lt_vekp             TYPE TABLE OF vekp,
        lt_vepo             TYPE TABLE OF vepo,
        lt_vepo_c           TYPE TABLE OF vepo.

  DATA: ls_zwm051           TYPE zwm051,
        ls_pack_in_transp   TYPE zwm01_pack_in_transp,
        ls_pack_in_transp_h TYPE zwm01_pack_in_transp,
        ls_vekp             TYPE vekp,
        ls_vepo             TYPE vepo.

  DATA: lv_locked      TYPE flag,
        lv_vemng       TYPE vemng,
        lv_message_var TYPE bdc_vtext1,
        lv_continue    TYPE flag,
        lv_lines_pt    TYPE sytabix,
        lv_lines_vepo  TYPE sytabix,
        lv_lock_data   TYPE char50.


  FIELD-SYMBOLS: <ls_lips>   TYPE lips,
                 <ls_zwm051> TYPE zwm051.

** Retorna Remessas
***********************************************************************
  SELECT * FROM t311a
     INTO TABLE lt_t311a
*     WHERE lgnum = z1om_lgnum AND " << DEL ROFF(SDF):TMGP:06.01.2016 16:54:20
     WHERE lgnum = gv_lgnum AND " << INS ROFF(SDF):TMGP:06.01.2016 16:54:22
           refnr = vbss-sammg.

** Retorna Backups
***********************************************************************
  SELECT * FROM zwm051
     INTO TABLE lt_zwm051
*     WHERE lgnum = z1om_lgnum AND " << DEL ROFF(SDF):TMGP:06.01.2016 16:54:36
     WHERE lgnum = gv_lgnum AND " << INS ROFF(SDF):TMGP:06.01.2016 16:54:37
           refnr = vbss-sammg.


  CHECK NOT lt_zwm051 IS INITIAL.

** Retorna Remessas
***********************************************************************
  SELECT * FROM lips
     INTO TABLE lt_lips
     FOR ALL ENTRIES IN lt_t311a
     WHERE vbeln = lt_t311a-rbnum.

  CHECK sy-subrc EQ 0.

** Eliminar Remessas

  READ TABLE lt_zwm051 ASSIGNING <ls_zwm051> INDEX 1.
  IF <ls_zwm051>-vbeln = '9999999999' OR <ls_zwm051>-vbeln IS INITIAL.

  ELSE.
    LOOP AT lt_lips ASSIGNING <ls_lips>.

      LOOP AT lt_zwm051 ASSIGNING <ls_zwm051> WHERE vbeln = <ls_lips>-vbeln.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        DELETE lt_lips WHERE vbeln = <ls_lips>-vbeln.
      ENDIF.
    ENDLOOP.

  ENDIF.
** Separa Remessas
***********************************************************************
  LOOP AT lt_zwm051 ASSIGNING <ls_zwm051>.
    CLEAR ls_pack_in_transp.

    ls_pack_in_transp-exidv = <ls_zwm051>-exidv.

    LOOP AT lt_lips ASSIGNING <ls_lips> WHERE matnr = <ls_zwm051>-matnr AND
                                              charg = <ls_zwm051>-charg AND
                                              lfimg > 0.

      ls_pack_in_transp-vbeln = <ls_lips>-vbeln.
      ls_pack_in_transp-posnr = <ls_lips>-posnr.

      IF <ls_lips>-lfimg > <ls_zwm051>-vemng.
        lv_vemng = <ls_zwm051>-vemng.
      ELSE.
        lv_vemng = <ls_lips>-lfimg.
      ENDIF.

      <ls_lips>-lfimg = <ls_lips>-lfimg - lv_vemng.
      <ls_zwm051>-vemng = <ls_zwm051>-vemng - lv_vemng.

      ls_pack_in_transp-vemng = lv_vemng.
      ls_pack_in_transp-vrkme = <ls_zwm051>-vemeh.

      APPEND ls_pack_in_transp TO lt_pack_in_transp.

      IF <ls_zwm051>-vemng EQ 0.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  CHECK NOT lt_pack_in_transp IS INITIAL.

  DO 30 TIMES.
    CLEAR lv_continue.

** Cria Bloqueio
***********************************************************************
    lv_locked = abap_true.
    CONCATENATE 'ZPACK_TRANSP'
                vbss-sammg
           INTO lv_lock_data.

    EXPORT lv_locked TO DATABASE indx(zp) ID lv_lock_data.

** Embala
***********************************************************************
    CALL FUNCTION 'ZWM_PACK_TRANSPORTATION_BG'
      IN BACKGROUND TASK AS SEPARATE UNIT
      EXPORTING
*       i_lgnum           = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:54:55
        i_lgnum           = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:54:57
        i_refnr           = vbss-sammg
        it_pack_in_transp = lt_pack_in_transp
        i_lock_key        = lv_lock_data.

    COMMIT WORK.

** Espera Por Desbloqueio
***********************************************************************
    DO 120 TIMES.
      CLEAR lv_locked.

      IF sy-index > 1.
        WAIT UP TO 1 SECONDS.
      ENDIF.

      IMPORT lv_locked TO lv_locked FROM DATABASE indx(zp) ID lv_lock_data.

      IF lv_locked IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.

    DELETE FROM DATABASE indx(zp) ID lv_lock_data.

** Valida Embalamento
***********************************************************************
    SORT lt_pack_in_transp BY exidv.
    lt_pack_in_transp_h = lt_pack_in_transp.
    DELETE ADJACENT DUPLICATES FROM lt_pack_in_transp_h COMPARING exidv.

    DO 10 TIMES.
      IF sy-index > 1.
        WAIT UP TO 1 SECONDS.
      ENDIF.

      SELECT * FROM vekp
         INTO TABLE lt_vekp
         BYPASSING BUFFER
         FOR ALL ENTRIES IN lt_pack_in_transp_h
         WHERE exidv = lt_pack_in_transp_h-exidv.

      CHECK sy-subrc EQ 0.
      SORT lt_vekp BY exidv.

      SELECT * FROM vepo
         INTO TABLE lt_vepo
         BYPASSING BUFFER
         FOR ALL ENTRIES IN lt_vekp
         WHERE venum = lt_vekp-venum.

      CHECK sy-subrc EQ 0.
      SORT lt_vepo BY venum.


      LOOP AT lt_pack_in_transp_h INTO ls_pack_in_transp_h.
        CLEAR lv_continue.

        lt_pack_in_transp_c = lt_pack_in_transp.
        DELETE lt_pack_in_transp_c WHERE exidv <> ls_pack_in_transp_h-exidv.
        DESCRIBE TABLE lt_pack_in_transp_c LINES lv_lines_pt.

        CLEAR ls_vekp.
        READ TABLE lt_vekp
              INTO ls_vekp
              WITH KEY exidv = ls_pack_in_transp_h-exidv
              BINARY SEARCH.

        CHECK sy-subrc EQ 0.

        lt_vepo_c = lt_vepo.
        DELETE lt_vepo_c WHERE venum <> ls_vekp-venum.
        DESCRIBE TABLE lt_vepo_c LINES lv_lines_vepo.

        IF lv_lines_vepo EQ lv_lines_pt.
          lv_continue = abap_true.
        ELSE.
          CLEAR lv_continue.
          EXIT.
        ENDIF.
      ENDLOOP.

      CHECK lv_continue EQ abap_true.
      EXIT.
    ENDDO.

    CHECK lv_continue EQ abap_true.
    EXIT.
  ENDDO.
ENDFORM.                    " PACK_TRANSPORTATION_FINAL
*&---------------------------------------------------------------------*
*&      Form  CHECK_SSCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_sscc.

  DATA: lv_tabix    LIKE sy-tabix.
  DATA: lv_menge    TYPE menge_d.
  DATA: lt_vekp     TYPE vekp    OCCURS 0 WITH HEADER LINE.
  DATA: lt_vepo     TYPE vepo    OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm056   LIKE zwm056  OCCURS 0 WITH HEADER LINE.
  DATA: lt_sscc     LIKE st_ltap OCCURS 0 WITH HEADER LINE.
  DATA: lt_sscc_aux LIKE st_ltap OCCURS 0 WITH HEADER LINE.

** Validar SSCC
**********************************************************************
  CHECK it_ltap[] IS NOT INITIAL.

  LOOP AT it_ltap.
    CLEAR lt_sscc.
    lt_sscc-sscc       = it_ltap-sscc.
    lt_sscc-material   = it_ltap-material.
    lt_sscc-lote       = it_ltap-lote.
    lt_sscc-quantidade = it_ltap-quantidade.
    lt_sscc-unidade    = it_ltap-unidade.
    COLLECT lt_sscc.
  ENDLOOP.

** Validar se SSCC foram retirados do Grupo/Remessa
  CHECK lt_sscc[] IS NOT INITIAL.

  lt_sscc_aux[] = lt_sscc[].

  SELECT *
    FROM zwm056 INTO TABLE lt_zwm056
    FOR ALL ENTRIES IN lt_sscc_aux
    WHERE exidv = lt_sscc_aux-sscc.

  SORT lt_zwm056 BY exidv.

  LOOP AT lt_sscc_aux.
    lv_tabix = sy-tabix.

    CLEAR lt_zwm056.
    READ TABLE lt_zwm056 WITH KEY exidv = lt_sscc_aux-sscc.
    CHECK sy-subrc = 0.

    IF lt_zwm056-vbeln IS NOT INITIAL. " Remessa
      IF lt_zwm056-refnr = vbss-sammg AND
         lt_zwm056-vbeln = tabi_guias_por_item-guia.

        DELETE lt_sscc_aux INDEX lv_tabix.
      ENDIF.

    ELSE. " Grupo
      IF lt_zwm056-refnr = vbss-sammg.
        DELETE lt_sscc_aux INDEX lv_tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.

** Obter dados da HU
  IF lt_sscc_aux[] IS NOT INITIAL.
    SELECT *
      FROM vekp INTO TABLE lt_vekp
      FOR ALL ENTRIES IN lt_sscc_aux
      WHERE exidv = lt_sscc_aux-sscc.

    IF sy-subrc = 0.
      SELECT *
        FROM vepo INTO TABLE lt_vepo
        FOR ALL ENTRIES IN lt_vekp
        WHERE venum = lt_vekp-venum.
    ENDIF.

    SORT lt_vekp BY exidv.
    SORT lt_vepo BY venum matnr charg.
  ENDIF.

** Actualizar dados
**********************************************************************
  SORT lt_sscc BY sscc material lote.

  LOOP AT lt_sscc.
    lv_tabix = sy-tabix.

*   HU não existe
    READ TABLE lt_vekp WITH KEY exidv = lt_sscc-sscc BINARY SEARCH.
    IF sy-subrc <> 0.
      DELETE lt_sscc WHERE sscc = lt_sscc-sscc.
      DELETE it_ltap WHERE sscc = lt_sscc-sscc.
      CONTINUE.
    ENDIF.

    CLEAR lv_menge.
    LOOP AT lt_vepo WHERE venum = lt_vekp-venum    AND
                          matnr = lt_sscc-material AND
                          charg = lt_sscc-lote.

      lv_menge = lv_menge + lt_vepo-vemng.
    ENDLOOP.

*   Item não existe na HU
    IF lv_menge IS INITIAL.
      DELETE lt_sscc INDEX lv_tabix.

      DELETE it_ltap WHERE sscc     = lt_sscc-sscc     AND
                           material = lt_sscc-material AND
                           lote     = lt_sscc-lote.

    ELSE.
      IF lt_vepo-vemeh <> lt_sscc-unidade.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = lt_sscc-material
            i_in_me              = lt_vepo-vemeh
            i_out_me             = lt_sscc-unidade
            i_menge              = lv_menge
          IMPORTING
            e_menge              = lv_menge
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
      ENDIF.

*     Modificar Quantidade
      IF lv_menge <> lt_sscc-quantidade.

        lt_sscc-quantidade = lv_menge.

        LOOP AT it_ltap WHERE sscc     = lt_sscc-sscc     AND
                              material = lt_sscc-material AND
                              lote     = lt_sscc-lote.

          IF lt_sscc-quantidade = 0.
            DELETE it_ltap INDEX sy-tabix.
          ELSE.
            lv_menge = lt_sscc-quantidade - it_ltap-quantidade.

            IF lv_menge < 0.
              it_ltap-quantidade = lt_sscc-quantidade.
              lt_sscc-quantidade = 0.

              MODIFY it_ltap INDEX sy-tabix TRANSPORTING quantidade.
            ELSE.
              lt_sscc-quantidade = lv_menge.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT it_ltap BY sscc.

ENDFORM.                    " CHECK_SSCC
*&---------------------------------------------------------------------*
*&      Form  SAIDA_PAL_OPERADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM saida_pal_operador .

  DATA: l_kunnr LIKE vbpa-kunnr.
  DATA: l_vkorg LIKE likp-vkorg.
  DATA: l_vtweg LIKE lips-vtweg.
  DATA: l_spart LIKE lips-spart.
  DATA  lv_werks TYPE werks_d. " << INS ROFF(SDF):TMGP:06.01.2016 17:07:30
  DATA  lv_lgort  TYPE lgort_d.

  DATA: flag_ativo(1).

  DATA: lt_t311a         LIKE t311a    OCCURS 0 WITH HEADER LINE,
        lt_lips          LIKE lips     OCCURS 0 WITH HEADER LINE,
        lt_vekp          LIKE vekp     OCCURS 0 WITH HEADER LINE,
        lt_return        LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        lt_ce4almo_acct  LIKE ce4almo_acct OCCURS 0 WITH HEADER LINE,
        lt_goodsmvt_item LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE.

  DATA: lv_matdocument     TYPE bapi2017_gm_head_ret-mat_doc.
  DATA: lv_matdocyear      TYPE bapi2017_gm_head_ret-doc_year.
  DATA: lv_goodsmvt_code   LIKE bapi2017_gm_code.
  DATA: ls_goodsmvt_header LIKE bapi2017_gm_head_01.

  DATA: lv_2chang TYPE flag.

  CLEAR:   l_kunnr, lt_t311a, flag_ativo, lt_vekp.
  REFRESH: lt_t311a, lt_vekp.

  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user      = sy-uname
      i_refnr     = vbss-sammg
      i_recall    = 'X'
      i_usewm     = 'X'
      i_userf     = 'X'
      i_usemm     = 'X'
      i_useaut    = 'X'
      i_get_lgnum = 'X'
      i_get_werks = 'X'
      i_get_lgort = 'X'
** IMPORTING
**   ET_MESSAGES         =
    CHANGING
      c_lgnum     = gv_lgnum
      c_werks     = lv_werks
      c_lgort     = lv_lgort
    EXCEPTIONS
      error       = 1
      user_back   = 2
      OTHERS      = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


*& Begin of Modification by Tiago Pateiro - ROFF @ 06.01.2016 17:08:24
**  SELECT werks UP TO 1 ROWS
**    FROM t320 INTO lv_werks
**    WHERE lgort EQ 'CD'
**      AND lgnum EQ gv_lgnum.
**  ENDSELECT.
**  IF sy-subrc NE 0.
**    lv_werks = 'RENV'.
**  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 06.01.2016 17:08:24

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = gv_lgnum
      i_vbeln  = tabi_guias_por_item-guia
    IMPORTING
      e_2chang = lv_2chang
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.


  SELECT * FROM t311a
     INTO TABLE lt_t311a
*     WHERE lgnum = z1om_lgnum AND " << DEL ROFF(SDF):TMGP:06.01.2016 16:55:09
     WHERE lgnum = gv_lgnum AND " << INS ROFF(SDF):TMGP:06.01.2016 16:55:10
           refnr = vbss-sammg.

  LOOP AT lt_t311a.
    SELECT kunnr INTO vbpa-kunnr FROM vbpa UP TO 1 ROWS
           WHERE  vbeln  = lt_t311a-rbnum
           AND    parvw  = z1om_parvw.
    ENDSELECT.

    IF sy-subrc EQ 0.

      SELECT SINGLE * FROM likp WHERE vbeln = lt_t311a-rbnum.
      IF likp-vkorg = 'SER1' AND lv_2chang EQ abap_false.
        SELECT SINGLE * FROM tvko WHERE vkorg = likp-vkorg.
        IF tvko-kunnr IS NOT INITIAL.
          vbpa-kunnr = tvko-kunnr.
        ENDIF.
      ENDIF.

      SELECT SINGLE * FROM  zwm059
*             WHERE  lgnum  = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:55:20
             WHERE  lgnum  = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:55:21
             AND    kunnr  = vbpa-kunnr.

      IF sy-subrc = 0.
        flag_ativo = 'X'.
      ELSE.
        CLEAR flag_ativo.
        EXIT.
      ENDIF.
    ENDIF.

  ENDLOOP.

  CHECK flag_ativo IS NOT INITIAL.

  SELECT * INTO TABLE lt_lips
      FROM lips
          FOR ALL ENTRIES IN lt_t311a
              WHERE vbeln = lt_t311a-rbnum.

  READ TABLE lt_lips INDEX 1.
  IF sy-subrc = 0.
    l_vtweg = lt_lips-vtweg.
    l_spart = lt_lips-spart.
  ENDIF.

  DELETE lt_lips WHERE pstyv <> 'ZPAL'.

  CHECK lt_lips[] IS INITIAL.

  CLEAR:   ls_goodsmvt_header, lt_goodsmvt_item.
  REFRESH: lt_goodsmvt_item.

  READ TABLE lt_t311a INDEX 1.

  SELECT SINGLE tknum INTO (ls_goodsmvt_header-ref_doc_no) "nº DO TRANSPORTE
      FROM vttp WHERE vbeln = lt_t311a-rbnum.

  SELECT SINGLE tdlnr INTO vttk-tdlnr
      FROM vttk
          WHERE tknum = ls_goodsmvt_header-ref_doc_no.

  SELECT SINGLE kunnr INTO lfa1-kunnr
      FROM lfa1
          WHERE lifnr = vttk-tdlnr.

  CHECK lfa1-kunnr IS NOT INITIAL.

  SELECT * INTO TABLE lt_vekp
      FROM vekp
          WHERE vpobj = '04'
            AND vpobjkey = ls_goodsmvt_header-ref_doc_no.

*  SELECT * INTO TABLE lt_ce4almo_acct
*      FROM ce4almo_acct
*          FOR ALL ENTRIES IN lt_vekp
*              WHERE kndnr = lfa1-kunnr
*                AND artnr = lt_vekp-vhilm
*                AND fkart = ' '
*                AND vkorg = likp-vkorg
*                AND vtweg = l_vtweg
*                AND spart = l_spart.
*
*  SORT lt_ce4almo_acct BY artnr.


** Dados Cabeçalho
  ls_goodsmvt_header-doc_date   = sy-datum.   "Data de lançamento
  ls_goodsmvt_header-pstng_date = sy-datum.   "Data de documento

  LOOP AT lt_vekp.

    CLEAR lt_goodsmvt_item.
*    lt_goodsmvt_item-plant      = 'RENV'. " << DEL ROFF(SDF):TMGP:06.01.2016 17:08:48
    lt_goodsmvt_item-plant      = lv_werks.
    lt_goodsmvt_item-stge_loc   = lv_lgort.
    lt_goodsmvt_item-move_type  = 'ZSP'.
    lt_goodsmvt_item-gr_rcpt = vttk-tdlnr.
*   Material
    lt_goodsmvt_item-material =      lt_vekp-vhilm.
    lt_goodsmvt_item-entry_qnt =     1.
    lt_goodsmvt_item-entry_uom     = lt_vekp-meins.
    lt_goodsmvt_item-entry_uom_iso = lt_goodsmvt_item-entry_uom.

    READ TABLE lt_ce4almo_acct WITH KEY artnr = lt_goodsmvt_item-material.
    IF sy-subrc = 0.
      lt_goodsmvt_item-profit_segm_no = lt_ce4almo_acct-paobjnr.
    ENDIF.
    COLLECT lt_goodsmvt_item.

  ENDLOOP.

  CLEAR: lv_matdocument, lv_matdocyear.
  CLEAR: lt_return.
  REFRESH lt_return.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = ls_goodsmvt_header
      goodsmvt_code    = '06'
    IMPORTING
      materialdocument = lv_matdocument
      matdocumentyear  = lv_matdocyear
    TABLES
      goodsmvt_item    = lt_goodsmvt_item
      return           = lt_return.

  READ TABLE lt_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    MESSAGE ID lt_return-id TYPE 'E' NUMBER lt_return-number
           WITH lt_return-message_v1 lt_return-message_v2
                lt_return-message_v3 lt_return-message_v4.
  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

ENDFORM.                    " SAIDA_PAL_OPERADOR
*& Begin of Modification by Tiago Pateiro - ROFF @ 06.01.2016 15:27:58
*&---------------------------------------------------------------------*
*&      Form  F_USER_GET_LGNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_user_get_lgnum.
  DATA lt_xuser TYPE STANDARD TABLE OF lrf_wkqu.

  DATA ls_xuser TYPE lrf_wkqu.

  IF gv_lgnum IS NOT INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = lt_xuser[]
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc NE 0.
    gv_lgnum  = z1om_lgnum.
  ENDIF.

  SORT lt_xuser[] BY statu.
  DELETE lt_xuser[] WHERE statu NE 'X'.

  IF lt_xuser[] IS INITIAL.
    gv_lgnum  = z1om_lgnum.
  ENDIF.

  READ TABLE lt_xuser[] INTO ls_xuser INDEX 1.  " only on entry with active status is allowed

  gv_lgnum  = ls_xuser-lgnum.
ENDFORM.                    " F_USER_GET_LGNUM
*& End of Modification by Tiago Pateiro - ROFF @ 06.01.2016 15:27:58
*&---------------------------------------------------------------------*
*&      Form  CHECK_PACKING_SKIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM check_packing_skip USING uv_vbeln TYPE vbeln
                        CHANGING cv_subrc TYPE sysubrc
                                 cv_type  TYPE c.
  DATA: lt_vbss TYPE TABLE OF vbss,
        lt_vbpa TYPE TABLE OF vbpa,
        lt_lips TYPE TABLE OF lips,
        lt_ekpo TYPE TABLE OF ekpo.

  DATA: ls_lips TYPE lips,
        ls_ekpo TYPE ekpo.

  DATA: lv_lines   TYPE sytabix,
        lv_werks_1 TYPE werks_d,
        lv_werks_2 TYPE werks_d,
        lv_bukrs_1 TYPE bukrs,
        lv_bukrs_2 TYPE bukrs,
        lv_kunnr   TYPE kunnr.

  CLEAR: cv_subrc.


  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = gv_lgnum
      i_processo  = 'PACKING'
      i_parametro = 'KUNNR_SKIP'
    IMPORTING
      e_valor     = lv_kunnr
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.


  DO 1 TIMES.

    SELECT * FROM vbss
             INTO TABLE lt_vbss
             WHERE sammg = vbss-sammg.

    CHECK sy-subrc EQ 0.

    IF NOT uv_vbeln IS INITIAL.
      DELETE lt_vbss WHERE vbeln <> uv_vbeln.
    ENDIF.

    CHECK NOT lt_vbss IS INITIAL.

    SELECT * FROM vbpa
             INTO TABLE lt_vbpa
             FOR ALL ENTRIES IN lt_vbss
             WHERE vbeln = lt_vbss-vbeln.


    IF NOT lv_kunnr IS INITIAL.
      LOOP AT lt_vbpa TRANSPORTING NO FIELDS WHERE parvw = 'W1' AND
                                                   kunnr = lv_kunnr.
        cv_subrc = 4. "No Packing
        cv_type = 'K'.
        RETURN.
      ENDLOOP.
    ENDIF.

    SELECT * FROM lips
             INTO TABLE lt_lips
             FOR ALL ENTRIES IN lt_vbss
             WHERE vbeln = lt_vbss-vbeln.

    CHECK sy-subrc EQ 0.

    SELECT * FROM ekpo
             INTO TABLE lt_ekpo
             FOR ALL ENTRIES IN lt_lips
             WHERE ebeln = lt_lips-vgbel.

    CHECK sy-subrc EQ 0.

    SORT lt_lips BY werks.
    DELETE ADJACENT DUPLICATES FROM lt_lips COMPARING werks.
    DESCRIBE TABLE lt_lips LINES lv_lines.
    CHECK lv_lines EQ 1.

    CLEAR: ls_lips.
    READ TABLE lt_lips
          INTO ls_lips
          INDEX 1.

    lv_werks_1 = ls_lips-werks.
    CHECK NOT lv_werks_1 IS INITIAL.

    SORT lt_ekpo BY werks.
    DELETE ADJACENT DUPLICATES FROM lt_ekpo COMPARING werks.
    DESCRIBE TABLE lt_ekpo LINES lv_lines.
    CHECK lv_lines EQ 1.

    CLEAR: ls_ekpo.
    READ TABLE lt_ekpo
          INTO ls_ekpo
          INDEX 1.

    lv_werks_2 = ls_ekpo-werks.
    CHECK NOT lv_werks_2 IS INITIAL.

*    CHECK lv_werks_1 EQ lv_werks_2.

    CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
      EXPORTING
        i_werks                  = lv_werks_1
        i_gsber                  = ' '
      IMPORTING
        e_bukrs                  = lv_bukrs_1
*       E_KOKRS                  =
      EXCEPTIONS
        plant_not_valid          = 1
        valuation_area_not_valid = 2
        no_kokrs_found           = 3
        OTHERS                   = 4.



    CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
      EXPORTING
        i_werks                  = lv_werks_2
        i_gsber                  = ' '
      IMPORTING
        e_bukrs                  = lv_bukrs_2
*       E_KOKRS                  =
      EXCEPTIONS
        plant_not_valid          = 1
        valuation_area_not_valid = 2
        no_kokrs_found           = 3
        OTHERS                   = 4.

    CHECK lv_bukrs_1 EQ lv_bukrs_2.
    cv_subrc = 4. "No Packing
    cv_type = 'T'.
  ENDDO.

ENDFORM.                    " CHECK_PACKING_SKIP
