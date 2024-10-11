*&---------------------------------------------------------------------*
*&  Include           Z10MM05F_V1                                      *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  LE_PICKING
*&---------------------------------------------------------------------*
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
  IF sy-subrc NE 0.

***    MESSAGE e020(z1) WITH vbss-sammg.
  ENDIF.
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
  DATA resposta.

  PERFORM bi_conf_picking.

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

*-------------------------------------------------------------
*    CLEAR text.
*    text = 'Oferta de mercadoria recalculada'.
*
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'W'
*        message_number = '000'
*        message_var1   = text.
*---------------------------------------------------------------
***    MESSAGE s015(z1) WITH 'recalculada'.
  ENDIF.
ENDFORM.                               " REC_OF_MERCADORIA
*&---------------------------------------------------------------------*
*&      Form  GUIA_SEGUINTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM guia_seguinte.
  IF tabix < num_guias.
    tabix = tabix + 1.
    PERFORM carrega_guia_ecra.
  ENDIF.
ENDFORM.                               " GUIA_SEGUINTE
*&---------------------------------------------------------------------*
*&      Form  GUIA_ANTERIOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM guia_anterior.
  IF tabix > 1.
    tabix = tabix - 1.
    PERFORM carrega_guia_ecra.
  ENDIF.
ENDFORM.                               " GUIA_ANTERIOR
*&---------------------------------------------------------------------*
*&      Form  SAIR_VOLTAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sair_voltar.
  DATA resposta.
  save_okcode = okcode.
  CLEAR okcode.
  IF save_okcode = 'EXIT'.
    READ TABLE tabi_guias_por_item WITH KEY oferta_processada = 'X'.
    IF quant_alterada = 'X' OR sy-subrc = 0.
      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
        EXPORTING
          textline1 = 'Confirma sair?'(001)
          titel     = 'Fim do processamento'(002)
        IMPORTING
          answer    = resposta
        EXCEPTIONS
          OTHERS    = 1.
      CHECK resposta = 'J'.
    ENDIF.
    LEAVE PROGRAM.
  ELSEIF save_okcode = 'BACK'.
    LEAVE TO SCREEN 100.
  ENDIF.

ENDFORM.                               " SAIR_VOLTAR
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
  REFRESH tabi_guias. CLEAR: zwmlog, zwmlog02, t311.

*-------------------------------Validar que foi introduzido grupo de WM
  SELECT SINGLE * FROM  t311 CLIENT SPECIFIED
*         WHERE  lgnum  = z1om_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 16:58:40
         WHERE  lgnum  = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 16:58:42
         AND    refnr  = vbss-sammg.

  IF sy-subrc NE 0.

    CLEAR text.
    text = 'Não é Grupo WM'(003).

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

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
*   clear vbfa-vbelv.
*   select single vbelv into vbfa-vbelv from vbfa
*                       where vbeln = vbss-vbeln.
*   tabi_guias-vbelv = vbfa-vbelv.
* recebedor da mercadoria
    SELECT SINGLE * FROM kna1 WHERE kunnr = likp-kunnr.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING kna1 TO tabi_guias.
    ENDIF.
* itens da guia
    REFRESH tabi_guias-linha. CLEAR vbuk.       CLEAR zwmlog.

    SELECT SINGLE vbeln pkstk INTO (vbuk-vbeln, vbuk-pkstk) FROM vbuk
                                  WHERE vbeln = vbss-vbeln
*                                 and   koquk ne 'C'. " guia não conf.
****                                  AND   KOQUK = 'B'.  " já impressa
****                                  AND   koquk = z1om_koquk
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

*        CLEAR : tabi_guias, tabi_guias[], vbss-vbeln.
*-----------------------------------------------------------Ins Mai2005

*------Se a remessa não tem status correcto nenhuma remessa do grupo
*------pode ser processada
        CLEAR text.

        CONCATENATE 'Remessa'(004) vbss-vbeln 'tem Status Inválido'(005)
               INTO text SEPARATED BY space.

        CLEAR: g_dummy, l_zztpdoc.
        MESSAGE e000(zwmmsg001) WITH text INTO g_dummy.
*   & & & &
        l_zztpdoc = 'VAL_GRUPO'.

        PERFORM log USING vbss-vbeln ' '.

        CLEAR: g_dummy, l_zztpdoc.

        CLEAR : tabi_guias, tabi_guias[], vbss-vbeln.


        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '000'
            message_var1   = text.

        RETURN.
*-----------------------------------------------------------end Mai2005

      ENDIF.
***      EXIT.
    ENDIF.


    CHECK vbuk-pkstk NE 'C'.

********    CHECK sy-subrc = 0.
* status de donfirmação do picking da guia de remessa (vbeln-koquk)
** 'A' Grupo não impresso
** 'B' Impresso
** 'C' Confirmado
* guias não confirmadas e já impressas


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

*-----------se o grupo já foi reprocessado mas não seleccionamos
*-----------remessas podemos passar ao transporte
  SELECT SINGLE * FROM  zwmlog
         WHERE  sammg    = vbss-sammg
         AND    vbeln    = vbss-vbeln.

  IF sy-subrc = 0 AND tabi_guias-linha[] IS INITIAL.
    tabi_guias-vbeln = vbss-vbeln.
    SET SCREEN 150.
  ENDIF.
*----------------------------------------------------------------------
  IF tabi_guias[] IS INITIAL.

    CLEAR text.
    text = 'Status do Grupo invalido'(006).

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

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
  REFRESH tabi_guias_por_item.
  LOOP AT tabi_guias.
    tabi_guias_por_item-guia = tabi_guias-vbeln.
*ROFF::SDF::CDFR::27/07/2020
*    MOVE-CORRESPONDING tabi_guias TO tabi_guias_por_item.

    tabi_guias_por_item-guia = tabi_guias-vbeln.
    tabi_guias_por_item-vbelv = tabi_guias-kunnr.
    tabi_guias_por_item-kunnr = tabi_guias-vbeln.
    tabi_guias_por_item-name1 = tabi_guias-name1.
    tabi_guias_por_item-ort01 = tabi_guias-ort01.
*ROFF::SDF::CDFR::27/07/2020
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

ENDFORM.                               " CALC_TOT_POR_LINHA_VENDIDA
*&---------------------------------------------------------------------*
*&      Form  GUIAS_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM guias_picking.
  CALL SCREEN 250 STARTING AT 20 10 ENDING AT 35 15.
  PERFORM carrega_guia_ecra.
ENDFORM.                               " GUIAS_PICKING
*&---------------------------------------------------------------------*
*&      Form  EXIBIR_GUIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exibir_guia.
  SET PARAMETER ID 'VL' FIELD likp-vbeln.
  CALL TRANSACTION 'VL03' AND SKIP FIRST SCREEN.
ENDFORM.                               " EXIBIR_GUIA
*&---------------------------------------------------------------------*
*&      Form  BI_CONF_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bi_conf_picking.
  DATA: l_erro, flag_guia_alterada, num_encomenda LIKE vbak-vbeln.

  REFRESH: tabi_creditos, it_zwmlog02.
  CLEAR: z1pom_log, z_tabix, zwmlog, zwmlog02, it_zwmlog02.


  CLEAR g_bloq. g_bloq = 2.
  LOOP AT tabi_guias_por_item WHERE guia_bloqueada = ' '
                              AND   oferta_processada ='X'.

    CLEAR: z_tabix, zwmlog, g_erro.    z_tabix = sy-tabix.


    PERFORM desembalar.

*----------------------------------Verifica se a remessa está bloqueada
    WHILE g_bloq = 2.
      PERFORM gr_bloqueado USING 'INI_PROC' CHANGING g_bloq.
    ENDWHILE.

*----------------------------------------MODIFICAR ITEM DO FORNECIMENTO
*======================================================================
    PERFORM modifica_guia USING flag_guia_alterada num_encomenda.
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
    PERFORM call_mod_guia USING l_erro.

    CHECK g_erro IS INITIAL.

***--------------------------------------------------PROCESSA CREDITOS
**    PERFORM cria_tabi_creditos.

*----após processar todos os items vai criar as OT, HU, e o Crédito
    AT END OF guia.

      CHECK g_erro IS INITIAL.
*-----------Criamos novas OT´s para gerar atribuição de lotes aos items
      IF NOT it_ot[] IS INITIAL.
        PERFORM cria_ot.
      ENDIF.

      CHECK g_erro IS INITIAL.

*-------------------------------------no final associamos as embalagens

*----------------Os clientes servisan que estão costumizados na tabela
*----------------zwm039 invalidam a embalagem das remessas onde existem
*----------------como parceiros AG
      PERFORM verifica_servisan USING g_rc.

      IF g_rc IS INITIAL.

        PERFORM cria_embalagens.

      ELSE.

        CLEAR g_rc.
      ENDIF.

*--------------------------------------------------------cria o crédito
      CHECK g_erro IS INITIAL.

      PERFORM modifica_encomenda USING l_erro.

      PERFORM bloqueia_remessa.

    ENDAT.

  ENDLOOP.
*--------------------------------------Se todo o processo correu sem
*--------------------------------------erros chama-mos o processamento
*--------------------------------------do Transporte e Saida Mercadoria
*--------condição para temporáriamente desactivar chamada a tela 150
  g_erro = '9'.
  IF g_erro IS INITIAL .
    SET SCREEN '0150'.
  ENDIF.

ENDFORM.                               " BI_CONF_PICKING
*&---------------------------------------------------------------------*
*&      Form  ECRA_INICIAL_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ecra_inicial_picking.
* confirmação do picking ecrã inicial
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50B'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0200'.
  APPEND tabi_bdcdata.
* guia de remessa
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'MV50B-VBELN'.
  tabi_bdcdata-fval = tabi_guias_por_item-guia. "tabi_guias-vbeln.
  APPEND tabi_bdcdata.
* ordem de picking
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'MV50B-KOMAU'.
* tabi_bdcdata-fval = tabi_guias-komau.
  APPEND tabi_bdcdata.
* transferir quantidades
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'MV50B-KOMUE'.
  tabi_bdcdata-fval = 'X'.
  APPEND tabi_bdcdata.
ENDFORM.                               " ECRA_INICIAL_PICKING
*&---------------------------------------------------------------------*
*&      Form  PREPARA_DADOS_BI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepara_dados_bi.
ENDFORM.                               " PREPARA_DADOS_BI
*&---------------------------------------------------------------------*
*&      Form  CALL_CONF_PICKING_VL08
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_conf_picking_vl08 USING p_erro.
  CALL TRANSACTION 'VL08' USING tabi_bdcdata
                          UPDATE 'S'
                          MODE 'A'.
  IF sy-subrc NE 0.
    p_erro = 'X'.
    z1pom_log-pick_c = 'E'.
  ELSE.
    z1pom_log-pick_c = 'P'.
  ENDIF.
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = sy-msgid
      msgnr               = sy-msgno
      msgv1               = sy-msgv1
      msgv2               = sy-msgv2
      msgv3               = sy-msgv3
      msgv4               = sy-msgv4
    IMPORTING
      message_text_output = z1pom_log-msg_p
    EXCEPTIONS
      OTHERS              = 1.
  IF sy-subrc NE 0.
    z1pom_log-msg_p = 'Erro na função MESSAGE_TEXT_BUILD'.
  ENDIF.
ENDFORM.                               " CALL_CONF_PICKING_VL08
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_GUIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modifica_guia USING flag_guia_alterada num_encomenda.


  REFRESH: tabi_bdcdata, it_zwmlog02.
  CLEAR: flag_guia_alterada, wa_z1com_s, idx_tabi, it_zwmlog02.

* modifica guia 1º ecrã
  PERFORM ecra_inicial.
  LOOP AT tabi_guias_por_item-linha INTO z1com_s.

    CLEAR idx_tabi. idx_tabi = sy-tabix.

    CHECK g_erro IS INITIAL.

*--------verifica se ha modificação de QTD------------------ins Mar2005
    READ TABLE it_lips_aux WITH KEY vbeln = tabi_guias_por_item-guia
                                    posnr = z1com_s-vgpos
                                    matnr = z1com_s-matnr.
    IF it_lips_aux-kcmeng GT 0.
      CHECK it_lips_aux-kcmeng NE z1com_s-quant_cf.
    ELSE.
      CHECK it_lips_aux-lfimg NE z1com_s-quant_cf.
    ENDIF.
*-----------------------------------------------------------ins Mar2005

    CLEAR g_item_del. g_item_del = sy-tabix.

*----------------------------------------------info sobre o proximo item
    IF z1com_s-itm_oferta = 'X'.
      g_idx_bonus = idx_tabi + 1.

      READ TABLE tabi_guias_por_item-linha INTO wa_z1com_s INDEX
      g_idx_bonus.
*---23 OS ITEMS NÃO OFERTA EFECTUA APENAS O CRÉDITO E EMBALAGENS Mar2005
    ELSEIF z1com_s-pstyv NE 'ZOF'.
      CONTINUE.
    ENDIF.
*-----------------------------------------------------------------------

* guarda o nº. da encomenda
    IF NOT z1com_s-vbeln IS INITIAL.
      num_encomenda = z1com_s-vbeln.
    ELSE.
*----------------------------------------------NÃO TRATA PALETES
      CONTINUE.
*---------------------------------------------------------------------
    ENDIF.

*-----------------------------------------------eliminar os items nulos
    IF z1com_s-quant_cf EQ 0.

*---------------------------------não processar os items oferta sem pai
      IF z1com_s-pstyv = 'ZOF' AND z1com_s-uepos IS INITIAL.
        CONTINUE.
      ENDIF.

      PERFORM elimina_item_bonus CHANGING g_erro.
      CONTINUE.
    ENDIF.

*---------------------se o pai só tem crédito o bonus não é processado.
    IF g_pai = z1com_s-uepos.
      IF g_so_credito = 'X'.
        CONTINUE.
      ENDIF.
    ELSE.
      CLEAR : g_pai, g_so_credito.
    ENDIF.

    IF z1com_s-lfimg NE z1com_s-quant_cf OR z1com_s-quant_cf NE 0.
*--------------------------------------------efectuar apenas Crédito
      IF z1com_s-total_cf = z1com_s-lfimg AND
         z1com_s-lfimg = z1com_s-quant_cf.

        g_pai = z1com_s-posnr.
        g_so_credito = 'X'.
        CONTINUE.
      ENDIF.

*-------- Mod. linha qtd encomendada diferente da quantidade confirmada
*----------------------------------------------------------------------
      PERFORM altera_quantidade.
*----------------------------------------------------------------------

      IF g_erro IS INITIAL.
        flag_guia_alterada = 'X'.

*------------------------------------------------------items a criar ot
        it_ot-posnr = z1com_s-vgpos.
        it_ot-vbeln = tabi_guias_por_item-guia. "tabi_guias-vbeln.
        APPEND it_ot. CLEAR it_ot.
*----------------------------------------------------------------------

      ENDIF.

    ELSEIF z1com_s-quant_cf = 0.
* apaga linha a 0

    ENDIF.
  ENDLOOP.

  IF flag_guia_alterada IS INITIAL OR NOT g_erro IS INITIAL.
    CLEAR : tabi_bdcdata, tabi_bdcdata[].
  ELSE.
* gravar
    PERFORM func_f11.
  ENDIF.

ENDFORM.                               " MODIFICA_GUIA
*&---------------------------------------------------------------------*
*&      Form  CONF_PICKING_VL08
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM conf_picking_vl08.
  REFRESH tabi_bdcdata.
* confirma picking
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50B'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0200'.
  APPEND tabi_bdcdata.
* nº guia
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'MV50B-VBELN'.
  tabi_bdcdata-fval = tabi_guias_por_item-guia.
  APPEND tabi_bdcdata.
* ordem de picking
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'MV50B-KOMAU'.
  tabi_bdcdata-fval = tabi_guias_por_item-vbelv.
  APPEND tabi_bdcdata.
* transfere quantidades
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'MV50B-KOMUE'.
  tabi_bdcdata-fval = 'X'.
  APPEND tabi_bdcdata.
* Confirma picking
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50B'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0300'.
  APPEND tabi_bdcdata.
* função confirma picking
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/11'.
  APPEND tabi_bdcdata.
ENDFORM.                               " CONF_PICKING_VL08
*&---------------------------------------------------------------------*
*&      Form  ALTERA_QUANTIDADE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITEM_GUIA  text                                            *
*----------------------------------------------------------------------*
FORM altera_quantidade.
  DATA num_lotes TYPE i.
* determina nº de lotes do item
  PERFORM det_num_lotes_item USING num_lotes.
  PERFORM posicionar.
* verifica se material tem lotes
  SELECT SINGLE xchpf INTO marc-xchpf FROM marc
                      WHERE matnr = z1com_s-matnr
                      AND   werks = z1com_s-werks.
  IF sy-subrc NE 0.
*   message a305(m3) with z1com_s-matnr.
    CLEAR marc-xchpf.
  ELSE.
    IF marc-xchpf = 'X' AND z1com_s-lfimg NE 0.
* anula quantidade lotes

*------------ELIMINAR LOTES----------------------
      PERFORM elimina_lotes.
*------------------------------------------------

    ENDIF.
  ENDIF.

  IF NOT z1com_s-vbeln IS INITIAL.
* inserir nova quantidade de picking(item com encomenda)
    PERFORM nova_quant_picking.
  ENDIF.
  IF z1com_s-quant_cf NE 0.
* nova quantidade
    PERFORM quant_confirmada.
  ENDIF.



ENDFORM.                               " ALTERA_QUANTIDADE
*&---------------------------------------------------------------------*
*&      Form  POSICIONAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITEM_GUIA  text                                            *
*----------------------------------------------------------------------*
FORM posicionar.
* posicionar

  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = 'POPO'.
  APPEND tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0111'.
  APPEND tabi_bdcdata.
* nº item
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'RV50A-POSNR'.
*---------------------------------------------
  IF z1com_s-posnr GT '000000'.
*---------------------------------------------
    tabi_bdcdata-fval = z1com_s-posnr.

  ELSE.

    tabi_bdcdata-fval = z1com_s-vgpos.

  ENDIF.

  APPEND tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0200'.
  APPEND tabi_bdcdata.
ENDFORM.                               " ALTERA_QUANTIDADE
*&---------------------------------------------------------------------*
*&      Form  ANULA_QUANT_LOTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM anula_quant_lotes USING num_lotes.
* função partição de lotes
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = 'CHSP'.
  APPEND tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0250'.
  APPEND tabi_bdcdata.

  DO num_lotes TIMES.
************ limpar quantidades e apagar item
* marcar item
    CLEAR tabi_bdcdata.
    tabi_bdcdata-fnam = 'RV50A-LIPS_SELKZ(1)'.
    tabi_bdcdata-fval = 'X'.
    APPEND tabi_bdcdata.
* limpar quantidade

    CLEAR tabi_bdcdata.
    tabi_bdcdata-fnam = 'LIPS-LFIMG(1)'.
    tabi_bdcdata-fval = '                 '.
    APPEND tabi_bdcdata.

* eliminar item
    CLEAR tabi_bdcdata.
    tabi_bdcdata-fnam = 'bdc_okcode'.
    tabi_bdcdata-fval = 'POLE'.
    APPEND tabi_bdcdata.

    CLEAR tabi_bdcdata.
    tabi_bdcdata-program = 'SAPMV50A'.
    tabi_bdcdata-dynbegin = 'X'.
    tabi_bdcdata-dynpro = '0250'.
    APPEND tabi_bdcdata.

    CLEAR tabi_bdcdata.
    tabi_bdcdata-fnam = 'bdc_okcode'.
    tabi_bdcdata-fval = '/3'.
    APPEND tabi_bdcdata.

    CLEAR tabi_bdcdata.
    tabi_bdcdata-program = 'SAPMV50A'.
    tabi_bdcdata-dynbegin = 'X'.
    tabi_bdcdata-dynpro = '0200'.
    APPEND tabi_bdcdata.

    PERFORM posicionar.
    PERFORM marcar_item.

* função partição de lotes
    CLEAR tabi_bdcdata.
    tabi_bdcdata-fnam = 'bdc_okcode'.
    tabi_bdcdata-fval = 'CHSP'.
    APPEND tabi_bdcdata.
*
    CLEAR tabi_bdcdata.
    tabi_bdcdata-program = 'SAPMV50A'.
    tabi_bdcdata-dynbegin = 'X'.
    tabi_bdcdata-dynpro = '0250'.
    APPEND tabi_bdcdata.
*   clear tabi_bdcdata.
*   tabi_bdcdata-fnam = 'bdc_okcode'.
*   tabi_bdcdata-fval = 'POLE'.
*   append tabi_bdcdata.

*   clear tabi_bdcdata.
*   tabi_bdcdata-program = 'SAPMV50A'.
*   tabi_bdcdata-dynbegin = 'X'.
*   tabi_bdcdata-dynpro = '0250'.
*   append tabi_bdcdata.
  ENDDO.
************
* função nova determinação de lotes
* clear tabi_bdcdata.
* tabi_bdcdata-fnam = 'bdc_okcode'.
* tabi_bdcdata-fval = 'CHFD'.
* append tabi_bdcdata.
*
* clear tabi_bdcdata.
* tabi_bdcdata-program = 'SAPLV01F'.
* tabi_bdcdata-dynbegin = 'X'.
* tabi_bdcdata-dynpro = '0100'.
* append tabi_bdcdata.
* anular quantidades
* clear tabi_bdcdata.
* tabi_bdcdata-fnam = 'bdc_okcode'.
* tabi_bdcdata-fval = 'DMNG'.
* append tabi_bdcdata.

* clear tabi_bdcdata.
* tabi_bdcdata-program = 'SAPLV01F'.
* tabi_bdcdata-dynbegin = 'X'.
* tabi_bdcdata-dynpro = '0100'.
* append tabi_bdcdata.
* transferir
* clear tabi_bdcdata.
* tabi_bdcdata-fnam = 'bdc_okcode'.
* tabi_bdcdata-fval = '/5'.
* append tabi_bdcdata.

* clear tabi_bdcdata.
* tabi_bdcdata-program = 'SAPMV50A'.
* tabi_bdcdata-dynbegin = 'X'.
* tabi_bdcdata-dynpro = '0250'.
* append tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/3'.
  APPEND tabi_bdcdata.

  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0200'.
  APPEND tabi_bdcdata.

ENDFORM.                               " ANULA_QUANT_LOTES
*&---------------------------------------------------------------------*
*&      Form  MARCAR_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM marcar_item.
* marcar item
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'RV50A-LIPS_SELKZ(1)'.
  tabi_bdcdata-fval = 'X'.
  APPEND tabi_bdcdata.
ENDFORM.                               " POSICIONAR
*&---------------------------------------------------------------------*
*&      Form  QUANT_CONFIRMADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM quant_confirmada.
* quantidade confirmada
  DATA quant_char(17).
  WRITE z1com_s-quant_cf TO quant_char.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'LIPSD-G_LFIMG(1)'.
  tabi_bdcdata-fval = quant_char.
  APPEND tabi_bdcdata.

*30--------------****************--------------------ins Mar2005
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'LIPS-VRKME(1)'.
  tabi_bdcdata-fval = z1com_s-vrkme.
  APPEND tabi_bdcdata.


*--------------********************--------------------------------

*-------------enter-----------------------------ins Mar2005
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/00'.
  APPEND tabi_bdcdata.
*-----------------------------------------------fim

ENDFORM.                               " ANULA_QUANT_LOTES
*&---------------------------------------------------------------------*
*&      Form  NOVA_DET_LOTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nova_det_lotes.

*--------------------------------------------------------
***  CHECK z1com_s-posnr NE '000000'.
*--------------------------------------------------------

* função partição de lotes
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = 'CHSP'.
  APPEND tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0250'.
  APPEND tabi_bdcdata.
* função nova determinação de lotes
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = 'CHKO'.
  APPEND tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPLV01F'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0100'.
  APPEND tabi_bdcdata.
* transferir
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/5'.
  APPEND tabi_bdcdata.

  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0250'.
  APPEND tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/3'.
  APPEND tabi_bdcdata.

  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0200'.
  APPEND tabi_bdcdata.

ENDFORM.                               " NOVA_DET_LOTES
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
*&      Form  CALL_MOD_GUIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_mod_guia USING p_erro.
*{   INSERT         TSTK900002                                        1
  DATA mode_bi.
*  GET PARAMETER ID 'ZBI' FIELD MODE_BI.
*  if sy-subrc ne 0 or mode_bi is initial.
*    mode_bi = 'N'.
*  endif.

  SET UPDATE TASK LOCAL.

  CHECK NOT tabi_bdcdata IS INITIAL.

  mode_bi = 'N'. CLEAR : it_msg, it_msg[].
  IF sy-tcode = 'Z1O4'.
    CALL TRANSACTION 'VL02' USING tabi_bdcdata
                            UPDATE 'S'
                            MODE 'A'.
  ELSE.

    CALL TRANSACTION 'VL02' USING tabi_bdcdata
                            UPDATE 'S'
                            MODE mode_bi
                            MESSAGES INTO it_msg.
  ENDIF.

  CLEAR l_zztpdoc.
  l_zztpdoc = 'REM_SAVE'.

  PERFORM log USING tabi_guias_por_item-guia ' '.

  IF it_msg-msgtyp = 'S'.
    COMMIT WORK AND WAIT.
  ELSE.

    LOOP AT it_zwmlog02.
*------Criar registo para recuperar items da remessa
      PERFORM gera_reg_reprocesso USING 'REM_SAVE' it_zwmlog02-vgpos
                                        it_zwmlog02-lfimg
                                        it_zwmlog02-vbeln
                                        ' ' it_zwmlog02-sammg ' '
                                        it_zwmlog02-vrkme ' '.

      PERFORM gera_reg_reprocesso_save USING 'OT' it_zwmlog02-vgpos
                                             it_zwmlog02-lfimg
                                             it_zwmlog02-vbeln
                                             ' ' it_zwmlog02-sammg ' '
                                             it_zwmlog02-vrkme ' '.

      PERFORM gera_reg_reprocesso_save USING 'HU' it_zwmlog02-vgpos
                                             it_zwmlog02-lfimg
                                             it_zwmlog02-vbeln
                                             ' ' it_zwmlog02-sammg ' '
                                             it_zwmlog02-vrkme ' '.



    ENDLOOP.
    PERFORM gera_reg_log USING 'CR' ' '.
  ENDIF.


ENDFORM.                               " CALL_MOD_GUIA
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
      SELECT SINGLE pstyv werks netpr waerk uepos zzoferta zzprliq
               INTO (vbap-pstyv, vbap-werks, vbap-netpr, vbap-waerk,
                     vbap-uepos, vbap-zzoferta, vbap-zzprliq)
                                FROM vbap
                                WHERE vbeln = z1com_s-vbeln
                                AND   posnr = z1com_s-vgpos.
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
    ENDLOOP.
    MODIFY tabi_guias_por_item.
  ENDLOOP.
ENDFORM.                               " CARREGA_DADOS_ENC_OF
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
*&      Form  ELIMINA_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM elimina_item.
  PERFORM marcar_item.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = 'POLO'.
  APPEND tabi_bdcdata.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0200'.
  APPEND tabi_bdcdata.
ENDFORM.                               " ELIMINA_ITEM
*&---------------------------------------------------------------------*
*&      Form  CONF_PICKING_VL02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM conf_picking_vl02.
  REFRESH tabi_bdcdata.
* modifica guia
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0101'.
  APPEND tabi_bdcdata.
* nº da encomenda
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'LIKP-VBELN'.
  tabi_bdcdata-fval = tabi_guias_por_item-guia.
  APPEND tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = 'QUIT'.
  APPEND tabi_bdcdata.
* modifica guia
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0101'.
  APPEND tabi_bdcdata.

ENDFORM.                               " CONF_PICKING_VL02
*&---------------------------------------------------------------------*
*&      Form  ECRA_INICIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ecra_inicial.


* modifica guia
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0101'.
  APPEND tabi_bdcdata.
* nº da encomenda
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'LIKP-VBELN'.
  tabi_bdcdata-fval = tabi_guias_por_item-guia.
  APPEND tabi_bdcdata.
*------------------------------------ins
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/00'.
  APPEND tabi_bdcdata.

  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0200'.
  APPEND tabi_bdcdata.

ENDFORM.                               " CONF_PICKING
*&---------------------------------------------------------------------*
*&      Form  CALL_CONF_PICKING_VL02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_conf_picking_vl02 USING p_erro.
*{   INSERT         TSTK900002                                        1
  DATA mode_bi.
  GET PARAMETER ID 'ZBI' FIELD mode_bi.
  IF sy-subrc NE 0 OR mode_bi IS INITIAL.
    mode_bi = 'N'.
  ENDIF.
*}   INSERT
  DO.
    CALL TRANSACTION 'VL02' USING tabi_bdcdata
                            UPDATE 'S'
*{   REPLACE        TSTK900002                                        2
*\                            MODE 'N'.
                            MODE mode_bi.
*}   REPLACE
****                        mode 'A'.
    IF sy-subrc = 0 OR sy-index = 100.
      EXIT.
    ENDIF.
  ENDDO.
  IF sy-subrc NE 0.
    p_erro = 'X'.
    z1pom_log-pick_c = 'E'.
  ELSE.
    z1pom_log-pick_c = 'P'.
  ENDIF.
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = sy-msgid
      msgnr               = sy-msgno
      msgv1               = sy-msgv1
      msgv2               = sy-msgv2
      msgv3               = sy-msgv3
      msgv4               = sy-msgv4
    IMPORTING
      message_text_output = z1pom_log-msg_p
    EXCEPTIONS
      OTHERS              = 1.
  IF sy-subrc NE 0.
    z1pom_log-msg_p = 'Erro na função MESSAGE_TEXT_BUILD'.
  ENDIF.
ENDFORM.                               " CALL_CONF_PICKING_VL02
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
*{   REPLACE        TSTK900002                                        1
*\  TABI_BDCDATA-FVAL = 'PKON'.
  tabi_bdcdata-fval = 'PKO1'.
*}   REPLACE
  APPEND tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
*{   REPLACE        TSTK900002                                        2
*\  TABI_BDCDATA-PROGRAM = 'SAPMV61A'.
  tabi_bdcdata-program = 'SAPMV45A'.
*}   REPLACE
  tabi_bdcdata-dynbegin = 'X'.
*{   REPLACE        TSTK900002                                        3
*\  TABI_BDCDATA-DYNPRO = '6200'.
  tabi_bdcdata-dynpro = '5003'.
*}   REPLACE
  APPEND tabi_bdcdata.
* criar
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
*{   REPLACE        TSTK900002                                        4
*\  TABI_BDCDATA-FVAL = 'KOAN'.
  tabi_bdcdata-fval = 'V69A_KOAN'.
*}   REPLACE
  APPEND tabi_bdcdata.
*
  CLEAR tabi_bdcdata.
*{   REPLACE        TSTK900002                                        5
*\  TABI_BDCDATA-PROGRAM = 'SAPMV61A'.
  tabi_bdcdata-program = 'SAPMV45A'.
*}   REPLACE
  tabi_bdcdata-dynbegin = 'X'.
*{   REPLACE        TSTK900002                                        6
*\  TABI_BDCDATA-DYNPRO = '6200'.
  tabi_bdcdata-dynpro = '5003'.
*}   REPLACE
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
*{   REPLACE        TSTK900002                                        7
*\  TABI_BDCDATA-PROGRAM = 'SAPMV61A'.
  tabi_bdcdata-program = 'SAPMV45A'.
*}   REPLACE
  tabi_bdcdata-dynbegin = 'X'.
*{   REPLACE        TSTK900002                                        8
*\  TABI_BDCDATA-DYNPRO = '6200'.
  tabi_bdcdata-dynpro = '5003'.
*}   REPLACE
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
*&      Form  INS_REGISTO_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ins_registo_log USING num_encomenda.
  z1pom_log-sammg = vbss-sammg.
  z1pom_log-vbeln_g = tabi_guias_por_item-guia.
  z1pom_log-vbeln_e = num_encomenda.
  z1pom_log-uname = sy-uname.
  z1pom_log-datum = sy-datum.
  z1pom_log-uzeit = sy-uzeit.
  INSERT z1pom_log.
ENDFORM.                               " INS_REGISTO_LOG
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
**  clear z1com_s-prliq.
*   select single prliq into z1com_s-prliq from z1mom
*                       where vbeln = tabi_creditos-vbeln
*                       and   posnr = tabi_creditos-vgpos.
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
    APPEND tabi_creditos.
  ENDLOOP.
ENDFORM.                               " CRIA_TABI_CREDITOS
*&---------------------------------------------------------------------*
*&      Form  MOD_REGISTO_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mod_registo_log.
  UPDATE z1pom_log.
ENDFORM.                               " MOD_REGISTO_LOG
*&---------------------------------------------------------------------*
*&      Form  APAGA_QUANT_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM apaga_quant_picking.

* picking
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/8'.
  APPEND tabi_bdcdata.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0220'.
  APPEND tabi_bdcdata.
  PERFORM posicionar.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0220'.
  APPEND tabi_bdcdata.
* apaga quantidades
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'LIPSD-PIKMG(1)'.
  tabi_bdcdata-fval = '                 '.
  APPEND tabi_bdcdata.
* voltar
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/5'.
  APPEND tabi_bdcdata.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0200'.
  APPEND tabi_bdcdata.
ENDFORM.                               " APAGA_QUANT_PICKING
*&---------------------------------------------------------------------*
*&      Form  LOCK_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lock_carga.

  IF old_group IS INITIAL.

    old_group = vbss-sammg.

  ELSE.

    IF old_group NE vbss-sammg.

      CALL FUNCTION 'DEQUEUE_EVVBSK'
        EXPORTING
          sammg          = old_group
        EXCEPTIONS
          system_failure = 8.
      CASE sy-subrc.
*        WHEN 8. MESSAGE a808 WITH vbss-sammg INTO g_dummy.
      ENDCASE.

      old_group = vbss-sammg.

    ENDIF.

  ENDIF.

  CALL FUNCTION 'ENQUEUE_EVVBSK'
    EXPORTING
      sammg          = vbss-sammg
    EXCEPTIONS
      foreign_lock   = 4
      system_failure = 8.
  CASE sy-subrc.
    WHEN 4.
*** MESSAGE e807(vl) WITH vbss-sammg.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = sy-msgid
*          message_lang   = sy-langu
*          message_type   = sy-msgty
*          message_number = sy-msgno
*          message_var1   = sy-msgv1.

    WHEN 8.
*** MESSAGE a808(vl) WITH vbss-sammg.

*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = sy-msgid
*          message_lang   = sy-langu
*          message_type   = sy-msgty
*          message_number = sy-msgno
*          message_var1   = sy-msgv1.

  ENDCASE.
ENDFORM.                               " LOCK_CARGA
*&---------------------------------------------------------------------*
*&      Form  INSERIR_QUANT_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inserir_quant_picking.
  DATA quant_char(17).
* picking
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/8'.
  APPEND tabi_bdcdata.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0220'.
  APPEND tabi_bdcdata.
  PERFORM posicionar.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0220'.
  APPEND tabi_bdcdata.
* quantidade
  WRITE z1com_s-quant_cf TO quant_char.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'LIPSD-PIKMG(1)'.
  tabi_bdcdata-fval = quant_char.
  APPEND tabi_bdcdata.
* voltar
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/5'.
  APPEND tabi_bdcdata.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0200'.
  APPEND tabi_bdcdata.
ENDFORM.                               " INSERIR_QUANT_PICKING
*&---------------------------------------------------------------------*
*&      Form  LE_STATUS_GUIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM le_status_guia.
  REFRESH tabi_status_guia.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE tabi_status_guia
           FROM vbup WHERE vbeln = tabi_guias_por_item-guia.
ENDFORM.                               " LE_STATUS_GUIA
*&---------------------------------------------------------------------*
*&      Form  ACT_STATUS_GUIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM act_status_guia.
* loop at tabi_status_guia.
*   update vbup set kosta = tabi_status_guia-kosta
*                   gbsta = tabi_status_guia-gbsta
*          where    vbeln = tabi_status_guia-vbeln
*          and      posnr = tabi_status_guia-posnr.
* endloop.
* read table tabi_status_guia index 1.
  UPDATE vbup SET kosta = 'C'
                  gbsta = 'B'
                  koqua = 'C'
         WHERE    vbeln = tabi_guias_por_item-guia
         AND      kosta = 'A'.
ENDFORM.                               " ACT_STATUS_GUIA
*&---------------------------------------------------------------------*
*&      Form  DET_NUM_LOTES_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NUM_LOTES  text                                            *
*----------------------------------------------------------------------*
FORM det_num_lotes_item USING    p_num_lotes.
  DATA linha_guia TYPE t_linha.
  CLEAR p_num_lotes.
  LOOP AT tabi_guias WHERE vbeln = tabi_guias_por_item-guia.
    LOOP AT tabi_guias-linha INTO linha_guia
                             WHERE posnr = z1com_s-posnr.
      CHECK NOT linha_guia-charg IS INITIAL.
      p_num_lotes = p_num_lotes + 1.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                               " DET_NUM_LOTES_ITEM
*&---------------------------------------------------------------------*
*&      Form  NOVA_QUANT_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nova_quant_picking.
  DATA quant_char(17).

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = z1com_s-vrkme
      language       = sy-langu
    IMPORTING
*     LONG_TEXT      =
      output         = z1com_s-vrkme
*     SHORT_TEXT     =
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  WRITE z1com_s-quant_cf TO quant_char.
* picking
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/8'.
  APPEND tabi_bdcdata.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0220'.
  APPEND tabi_bdcdata.
  PERFORM posicionar.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0220'.
  APPEND tabi_bdcdata.
* quantidades
*------------------------------------------Tela não necessária para WM
******  CLEAR tabi_bdcdata.
******  tabi_bdcdata-fnam = 'LIPSD-PIKMG(1)'.
******  tabi_bdcdata-fval = '0'.
******  APPEND tabi_bdcdata.
*---------------------------------------------------------------------
  PERFORM posicionar.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0220'.
  APPEND tabi_bdcdata.
* quantidades
*------------------------------------------Tela não necessária para WM
***  if  lips-lgnum is initial.
*---------------------------------------------------------------------
*    CLEAR TABI_BDCDATA.
*    TABI_BDCDATA-FNAM = 'LIPSD-PIKMG(1)'.
*    TABI_BDCDATA-FVAL = QUANT_CHAR.
*    APPEND TABI_BDCDATA.
**  else.

  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'LIPSD-G_LFIMG(1)'.
  tabi_bdcdata-fval = quant_char.
  APPEND tabi_bdcdata.

*30--------------****************--------------------ins Mar2005
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'LIPS-VRKME(1)'.
  tabi_bdcdata-fval = z1com_s-vrkme.
  APPEND tabi_bdcdata.


*--------------********************--------------------------------

**  endif.
*-------------enter-----------------------------ins Mar2005
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/00'.
  APPEND tabi_bdcdata.
*-----------------------------------------------fim
*-----------------------------------------------------------
* voltar
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = '/5'.
  APPEND tabi_bdcdata.
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0200'.
  APPEND tabi_bdcdata.
ENDFORM.                               " NOVA_QUANT_PICKING
*&---------------------------------------------------------------------*
*&      Form  ACERTO_VALOR_CREDITO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM acerto_valor_credito.
*
ENDFORM.                               " ACERTO_VALOR_CREDITO
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_CREDITOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_creditos.
  DATA flag_guia_com_creditos.
* caso a guia já tenha sido confirmada em outro picking
* e tenha sido gerado crédito, encomenda alterada,
* exibe mensagem de posível erro nos créditos
  LOOP AT tabi_creditos.
    SELECT * FROM z1pom_log WHERE vbeln_g = tabi_creditos-guia
                            AND   sammg NE vbss-sammg
                            AND   enc_a = 'P'.
* já existe uma guia (em outro picking) confirmada com
* crédito lançado na encomenda
      flag_guia_com_creditos = 'X'.
      EXIT.
    ENDSELECT.
    IF flag_guia_com_creditos = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
*  IF flag_guia_com_creditos = 'X'.
*    MESSAGE i046(z1).
*  ENDIF.

  CLEAR : tabi_creditos, tabi_creditos[].
ENDFORM.                               " VERIFICA_CREDITOS
*&---------------------------------------------------------------------*
*&      Form  INSERE_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insere_item.
* item com base em encomenda
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'bdc_okcode'.
  tabi_bdcdata-fval = 'RAUF'.
  APPEND tabi_bdcdata.
* ecra para inserir novo item
  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0105'.
  APPEND tabi_bdcdata.
* nº da encomenda
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'LV50C-VBELN'.
  tabi_bdcdata-fval = z1com_s-vbeln.
  APPEND tabi_bdcdata.
* nº item (do)
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'LV50C-ABPOS'.
  tabi_bdcdata-fval = z1com_s-vgpos.
  APPEND tabi_bdcdata.
* nº item (ao)
  CLEAR tabi_bdcdata.
  tabi_bdcdata-fnam = 'LV50C-BIPOS'.
  tabi_bdcdata-fval = z1com_s-vgpos.
  APPEND tabi_bdcdata.

  CLEAR tabi_bdcdata.
  tabi_bdcdata-program = 'SAPMV50A'.
  tabi_bdcdata-dynbegin = 'X'.
  tabi_bdcdata-dynpro = '0200'.
  APPEND tabi_bdcdata.

ENDFORM.                               " INSERE_ITEM
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

    IF sy-subrc <> 0.
*-------------------------------------------------------------
*      CLEAR text.
*      text = sy-msgv1.
*
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = sy-msgid
*          message_lang   = sy-langu
*          message_type   = sy-msgty
*          message_number = sy-msgno
*          message_var1   = text.
*---------------------------------------------------------------


*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
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
*&      Form  EMBALAGENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2741   text
*----------------------------------------------------------------------*
FORM embalagens  USING    VALUE(p_val).

  DATA : l_modo VALUE 'N'.
  CLEAR: bdcdata, bdcdata[].

  PERFORM bdc_dynpro   USING 'SAPMV50A'           '4004'.
  PERFORM bdc_field    USING 'BDC_CURSOR'         'LIKP-VBELN'.
  PERFORM bdc_field    USING 'BDC_OKCODE'         '/00'.
  PERFORM bdc_field    USING 'LIKP-VBELN'
  tabi_guias_por_item-guia. "tabi_guias-vbeln.
*  '2299210794'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'           '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'         '=VERP_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'         'LIPS-MATNR(04)'.

  PERFORM bdc_dynpro   USING 'SAPLV51G'           '6000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'         '=HUMARKHU'.
  PERFORM bdc_field    USING 'BDC_CURSOR'         'V51VE-VHILM(01)'.

  IF p_val = 'DES'.

    PERFORM bdc_dynpro   USING 'SAPLV51G'           '6000'.
    PERFORM bdc_field    USING 'BDC_OKCODE'         '=HULEE'.
    PERFORM bdc_field    USING 'BDC_CURSOR'         'V51VE-VHILM(01)'.

  ELSEIF p_val = 'DEL'.

    PERFORM bdc_dynpro      USING 'SAPLV51G'           '6000'.
    PERFORM bdc_field       USING 'BDC_OKCODE'         '=HULOE'.
    PERFORM bdc_field       USING 'BDC_CURSOR'         'V51VE-EXIDV(01)'
    .

    PERFORM bdc_dynpro      USING 'SAPLSPO1'           '0100'.
    PERFORM bdc_field       USING 'BDC_OKCODE'         '=YES'.

  ENDIF.

  PERFORM bdc_dynpro   USING 'SAPLV51G'           '6000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'         '=SICH'.
  PERFORM bdc_field    USING 'BDC_CURSOR'      'V51VE-VHILM(01)'.

  CALL TRANSACTION 'VL02N' USING bdcdata
                          UPDATE 'S'
                          MODE l_modo.

ENDFORM.                    " EMBALAGENS
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
*&      Form  elimina_lotes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM elimina_lotes .

  DATA: z_modo VALUE 'N'.

  CLEAR: bdcdata, bdcdata[].

  CHECK NOT z1com_s-pstyv = 'ZOF'.
  CLEAR g_erro.

  READ TABLE it_zwmlog WITH KEY zztpdoc = 'REM_DELE'
                                vgpos   = z1com_s-vgpos
                                zztpmsg = 'S'.
  IF sy-subrc EQ 0.

    IF z1com_s-posnr GT '000000'.
      PERFORM cria_item_ref_ordem USING z1com_s-posnr.

    ELSE.
      PERFORM cria_item_ref_ordem USING z1com_s-vgpos.

    ENDIF.

  ELSE.
*----------------------------------------------------
    PERFORM elimina_item_bonus CHANGING g_erro.

    CHECK g_erro IS INITIAL.

    IF g_erro IS INITIAL.
      IF z1com_s-posnr GT '000000'.
        PERFORM cria_item_ref_ordem USING z1com_s-posnr.

      ELSE.
        PERFORM cria_item_ref_ordem USING z1com_s-vgpos.

      ENDIF.
    ELSE.
      CLEAR g_erro.
    ENDIF.

  ENDIF.


ENDFORM.                    " elimina_lotes
*&---------------------------------------------------------------------*
*&      Form  elimina_item_bonus
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM elimina_item_bonus CHANGING g_erro .
  DATA: d_modo   VALUE 'N', wa_posnr TYPE posnr.

  CLEAR: bdcdata, bdcdata[], wa_posnr.


  PERFORM bdc_dynpro   USING 'SAPMV50A'              '4004'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'LIKP-VBELN'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '/00'.
  PERFORM bdc_field    USING 'LIKP-VBELN'
  tabi_guias_por_item-guia. "tabi_guias-vbeln.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=POPO_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'LIPS-MATNR(01)'.

*-------------------------------------------------posicionar no bonus
  PERFORM bdc_dynpro   USING 'SAPMV50A'              '0111'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'RV50A-POSNR'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=WEIT'.

  IF wa_z1com_s-posnr GT '000000'.
    PERFORM bdc_field    USING 'RV50A-POSNR'
    wa_z1com_s-posnr.
  ELSE.
    PERFORM bdc_field    USING 'RV50A-POSNR'
    wa_z1com_s-vgpos.
  ENDIF.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=POLO_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'LIPS-POSNR(01)'.
  PERFORM bdc_field    USING 'RV50A-LIPS_SELKZ(01)'  'X'.

  PERFORM bdc_dynpro     USING 'SAPMV50A'              '1000'.
  PERFORM bdc_field      USING 'BDC_OKCODE'            '/00'.
  PERFORM bdc_field      USING 'BDC_CURSOR'
  'LIPS-MATNR(02)'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=POPO_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'LIPS-MATNR(01)'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '0111'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'RV50A-POSNR'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=WEIT'.

  IF z1com_s-posnr GT '000000'.
    PERFORM bdc_field    USING 'RV50A-POSNR'           z1com_s-posnr.
  ELSE.
    PERFORM bdc_field    USING 'RV50A-POSNR'           z1com_s-vgpos.
  ENDIF.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=POLO_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'            'LIPS-POSNR(01)'.
  PERFORM bdc_field    USING 'RV50A-LIPS_SELKZ(01)'  'X'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'              '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'            '=SICH_T'.

  CLEAR: it_msg, it_msg[].

*----------------------------------Verifica se a remessa está bloqueada
  g_bloq = 2.
  WHILE g_bloq = 2.
    PERFORM gr_bloqueado USING 'REM_DELE' CHANGING g_bloq.
  ENDWHILE.


  CALL TRANSACTION 'VL02N' USING bdcdata
                          UPDATE 's'
                          MODE d_modo
                          MESSAGES INTO it_msg.
  CLEAR l_zztpdoc.
  l_zztpdoc = 'REM_DELE'.

  PERFORM log USING tabi_guias_por_item-guia z1com_s-vgpos.

*--------a estrutura wa_z1com_s-quant_cf tem a informação sobre o bonus

  IF it_msg-msgtyp = 'S'  AND wa_z1com_s-quant_cf EQ 0.
    DELETE tabi_guias_por_item-linha INDEX g_idx_bonus.
  ENDIF.

  COMMIT WORK AND WAIT. WAIT UP TO 5 SECONDS.

  CLEAR: bdcdata, bdcdata[], it_msg, it_msg[].

ENDFORM.                    " elimina_item_bonus
*&---------------------------------------------------------------------*
*&      Form  determina_lote_para_ot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM determina_lote_para_ot .

  CLEAR : it_lips, it_lips[], it_lqua, it_lqua[].
*--------------------------------------As QTD DISPONIVEIS Para cada LOTE
  SELECT * INTO TABLE it_lqua FROM  lqua
*         WHERE  lgnum  = z1om_lgnum                         "'100' " << DEL ROFF(SDF):TMGP:06.01.2016 16:58:52
         WHERE  lgnum  = gv_lgnum                         "'100' " << INS ROFF(SDF):TMGP:06.01.2016 16:58:54
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
*      PERFORM bdc_field    USING 'LTAK-LGNUM'          '100'. " << DEL ROFF(SDF):TMGP:06.01.2016 17:00:55
      PERFORM bdc_field    USING 'LTAK-LGNUM'          gv_lgnum. " << INS ROFF(SDF):TMGP:06.01.2016 17:00:58
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
*&      Form  cria_item_ref_ordem
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cria_item_ref_ordem USING posnr .

  DATA : l_posnr    LIKE lips-posnr, zx_modo VALUE 'N', l_data(10).
*l_data(8).

  CLEAR: l_posnr, bdcdata, bdcdata[], g_item_of, g_doc, g_erro, l_data.


  l_posnr = posnr + 1.

*--o item oferta ao ser eliminado aqui não deve voltar a ser processado
*por guardamos a sua referencia
  g_item_of = l_posnr.
  g_doc     = tabi_guias_por_item-guia.
*-----------------------------------------------------------------

  CLEAR likp.
  SELECT SINGLE wadat INTO likp-wadat FROM  likp
         WHERE  vbeln  = tabi_guias_por_item-guia.
*------------------------------------------------------Com Mai2005
*  WRITE likp-wadat TO l_data  DDMMYY.
  WRITE likp-wadat TO l_data.
*-----------------------------------------------------End Ins Mai2005

  PERFORM bdc_dynpro   USING 'SAPMV50A'            '4004'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'LIKP-VBELN'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
  PERFORM bdc_field USING 'LIKP-VBELN'        tabi_guias_por_item-guia.
*  '2299210985'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=RAUF_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'LIPS-MATNR(01)'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'            '0105'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'LV50C-BIPOS'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=ENT1'.
  PERFORM bdc_field    USING 'LV50C-VBELN'         z1com_s-vbeln.
*  '1299195522'.
*----------------------------------------------------------------------
  PERFORM bdc_field    USING 'LV50C-DATBI'         l_data.

*----------------------------------------------------------------------
  PERFORM bdc_field    USING 'LV50C-ABPOS'         posnr.
*'000010'.
*-----------------------------------se o item de oferta for gt 0, copia
  IF wa_z1com_s-quant_cf GT 0.
    PERFORM bdc_field    USING 'LV50C-BIPOS'         l_posnr.
  ELSE.
    PERFORM bdc_field    USING 'LV50C-BIPOS'         posnr.

  ENDIF.
*----------------------------------------------------------------------

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


  CALL TRANSACTION 'VL02N' USING bdcdata
                          UPDATE 's'
                          MODE zx_modo
                          MESSAGES INTO it_msg.
  CLEAR l_zztpdoc.
  l_zztpdoc = 'REM_COPI'.

  PERFORM log USING tabi_guias_por_item-guia posnr.

  IF it_msg-msgtyp = 'S'.
    COMMIT WORK AND WAIT.  WAIT UP TO 5 SECONDS.
*--------------------------------------APÓS A CÓPIA DOS ITEMS
*-----------GUARDAMOS A INFORMAÇÃO SOBRE A GRAVAÇÃO DOS ITEMS
*porque se conseguimos copiar com sucesso, se na gravação da da remessa
*ocorrer um erro, só assim sabemos quais os items processados.

    it_zwmlog02-zztpdoc   = 'REM_SAVE'.
    it_zwmlog02-sammg     = vbss-sammg.
    it_zwmlog02-vbeln     = tabi_guias_por_item-guia.
    it_zwmlog02-posnr     = z1com_s-vgpos.
    it_zwmlog02-lfimg     = z1com_s-quant_cf.
    it_zwmlog02-vrkme     = z1com_s-vrkme.

    APPEND it_zwmlog02. CLEAR it_zwmlog02.

    IF wa_z1com_s-quant_cf GT 0.
      it_zwmlog02-zztpdoc   = 'REM_SAVE'.
      it_zwmlog02-sammg     = vbss-sammg.
      it_zwmlog02-vbeln     = tabi_guias_por_item-guia.
      it_zwmlog02-posnr     = wa_z1com_s-vgpos.
      it_zwmlog02-lfimg     = wa_z1com_s-quant_cf.
      it_zwmlog02-vrkme     = wa_z1com_s-vrkme.

      APPEND it_zwmlog02. CLEAR it_zwmlog02.
    ENDIF.

  ELSE.

*--------------------------------------------------Entrada do Item

*-------------------------Criar registo para recuperar items da remessa
    PERFORM gera_reg_reprocesso USING 'REM_COPI' posnr z1com_s-quant_cf
                                 tabi_guias_por_item-guia
                                 z1com_s-vbeln vbss-sammg ' '
                                 z1com_s-vrkme
                                 z1com_s-pstyv.

*---------------------------Criar registo para recuperar OT_ da remessa
    PERFORM gera_reg_reprocesso USING 'OT_' posnr z1com_s-quant_cf
                                tabi_guias_por_item-guia
                                z1com_s-vbeln vbss-sammg
                                z1com_s-matnr
                                z1com_s-vrkme
                                z1com_s-pstyv.


*---------------------------Criar registo para recuperar HU_ da remessa
    PERFORM gera_reg_reprocesso USING 'HU_' posnr z1com_s-quant_cf
                                tabi_guias_por_item-guia
                                z1com_s-vbeln vbss-sammg ' '
                                z1com_s-vrkme
                                z1com_s-pstyv.

*-------------------------------------------criar os creditos dos items
    PERFORM cria_reg_credito USING posnr z1com_s.

*------------------------------------------------------Entrada do Bonus
    IF wa_z1com_s-quant_cf GT 0.

*-------------------Criar registo para recuperar items bonus da remessa
      PERFORM gera_reg_reprocesso USING 'REM_COPI' l_posnr
                                         wa_z1com_s-quant_cf
                                         tabi_guias_por_item-guia
                                         z1com_s-vbeln vbss-sammg ' '
                                         wa_z1com_s-vrkme
                                         z1com_s-pstyv.

*---------------------------Criar registo para recuperar OT_ da remessa
      PERFORM gera_reg_reprocesso USING 'OT_' l_posnr
                                         wa_z1com_s-quant_cf
                                         tabi_guias_por_item-guia
                                         z1com_s-vbeln vbss-sammg
                                         wa_z1com_s-matnr
                                         wa_z1com_s-vrkme
                                         z1com_s-pstyv.

*-------------------------------------------criar os creditos dos items
      PERFORM cria_reg_credito USING l_posnr wa_z1com_s.


    ENDIF.


    PERFORM gera_reg_log USING 'CR' ' '.

*--------------------------------------------------
    g_erro = 'X'.
  ENDIF.


  CLEAR : it_msg, it_msg[], bdcdata, bdcdata[].

ENDFORM.                    " cria_item_ref_ordem
*&---------------------------------------------------------------------*
*&      Form  get_paletes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_paletes .
*------------------------------------------------Inicializar estruturas

  CLEAR : it_ltap, it_ltap[], it_ltak, it_ltak[].
*-----------------------1º seleccionar da zwm026 as paletes incompletas

  SELECT  * INTO CORRESPONDING FIELDS OF TABLE it_ltap
            FROM zwm026
*            WHERE  armazem  = z1om_lgnum                    "'100' " << DEL ROFF(SDF):TMGP:06.01.2016 16:59:02
            WHERE  armazem  = gv_lgnum                    "'100' " << INS ROFF(SDF):TMGP:06.01.2016 16:59:04
            AND    remessa  = tabi_guias_por_item-guia
            AND    sscc     NE space.

  LOOP AT it_ltap WHERE ord IS INITIAL.
    it_ltap-ord = '3'.
    MODIFY it_ltap.
  ENDLOOP.
*----------------------2º seleccionar da ltak-ltap as paletes completas
  CLEAR it_ltap.
  SELECT * FROM  ltak
*         WHERE  lgnum  = z1om_lgnum                         "'100' " << DEL ROFF(SDF):TMGP:06.01.2016 16:59:12
         WHERE  lgnum  = gv_lgnum                         "'100' " << INS ROFF(SDF):TMGP:06.01.2016 16:59:13
         AND    vbeln  = tabi_guias_por_item-guia.

    SELECT  vlenr INTO it_ltap-sscc FROM  ltap
*           WHERE  lgnum  = z1om_lgnum                       "'100' " << DEL ROFF(SDF):TMGP:06.01.2016 16:59:25
           WHERE  lgnum  = gv_lgnum                       "'100' " << INS ROFF(SDF):TMGP:06.01.2016 16:59:26
           AND    tanum  = ltak-tanum
           AND   ( vorga  = z1om_vorga  OR vorga  = z1om_vorga2 )
           AND    vlenr  NE space.

      IF sy-subrc = 0.
        APPEND it_ltap.
      ENDIF.

    ENDSELECT.


  ENDSELECT.

  LOOP AT it_ltap WHERE ord IS INITIAL.
    it_ltap-ord = '1'.
    MODIFY it_ltap.
  ENDLOOP.

  CLEAR it_ltap.
*-------------------------3º PALETES RESULTANTES DA PALETIZAÇÃO ESPECIAL
  SELECT tanum INTO ltap-tanum FROM  ltap
*         WHERE  lgnum  = z1om_lgnum                         "'100' " << DEL ROFF(SDF):TMGP:06.01.2016 16:59:38
         WHERE  lgnum  = gv_lgnum                         "'100' " << INS ROFF(SDF):TMGP:06.01.2016 16:59:39
         AND    vlpla = tabi_guias_por_item-guia
         AND    nlpla = tabi_guias_por_item-guia
         AND    vltyp = z1om_lgtyp                          "916
         AND    nltyp = z1om_lgtyp
         AND   ( vorga  = z1om_vorga  OR vorga  = z1om_vorga2 ).

    IF sy-subrc = 0.

      SELECT SINGLE lznum INTO it_ltap-sscc FROM  ltak
*             WHERE  lgnum  = z1om_lgnum                     "'100' " << DEL ROFF(SDF):TMGP:06.01.2016 16:59:46
             WHERE  lgnum  = gv_lgnum                     "'100' " << INS ROFF(SDF):TMGP:06.01.2016 16:59:47
             AND    bwlvs  = z1om_bwlvs                     "'991'
             AND    tanum  = ltap-tanum
             AND    lznum  NE space.

      IF sy-subrc = 0.
        APPEND it_ltap.
      ENDIF.

    ENDIF.

  ENDSELECT.

  LOOP AT it_ltap WHERE ord IS INITIAL.
    it_ltap-ord = '2'.
    MODIFY it_ltap.
  ENDLOOP.

  CLEAR it_ltap.

*----------------------3º embalar paletes
  SORT it_ltap BY sscc.
  DELETE ADJACENT DUPLICATES FROM it_ltap.

  SORT it_ltap BY ord ASCENDING.

ENDFORM.                    " get_paletes
*&---------------------------------------------------------------------*
*&      Form  ASSOCIA_PALETES_REMESSAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM associa_paletes_remessas .
  DATA: zdummy(30).
  CLEAR: l_idx, l_vbeln.


  LOOP AT it_ltap.

    ADD 1 TO l_idx.

    l_vbeln = tabi_guias_por_item-guia.
    l_hu    = it_ltap-sscc.

*-----------------------------------Verifica se uma HU já foi associada
*-------------------------------------------------------DETERMINAR A OT
    CONCATENATE 'HU_' l_idx INTO l_zztpdoc.
    CONDENSE l_zztpdoc NO-GAPS.
*------------------------------------------------------------

***    READ TABLE it_zwmlog WITH KEY sammg = vbss-sammg
***                                  vbeln = tabi_guias_por_item-guia
***                                  zzdoc = l_hu
***                                  zztpmsg = 'S'.
***
****----------------------------------Se não existe uma entrada de
**sucesso
****--------------------------------------------podemos iniciar o
**processo
***    IF sy-subrc NE 0.


    CLEAR : it_msg, it_msg[].

    CALL FUNCTION 'ZWM_EMBALAR_HU'
      EXPORTING
*       warehouse                  = l_warehouse " << DEL ROFF(SDF):TMGP:06.01.2016 17:02:01
        warehouse                  = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 17:02:03
        hu                         = l_hu
        vbeln                      = l_vbeln
      TABLES
        return_msg                 = it_msg
      EXCEPTIONS
        empty_table                = 1
        reference_document_differs = 2
        empty_delivery_item        = 3
        item_not_found             = 4
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO zdummy.
    ENDIF.

    PERFORM log USING l_hu ' '.

    IF it_msg-msgtyp = 'S'.
      COMMIT WORK AND WAIT.  WAIT UP TO 5 SECONDS.
    ELSE.
      PERFORM gera_reg_log USING 'HU' ' '.
      PERFORM gera_reg_log USING 'CR' ' '.
    ENDIF.

***    ENDIF.

    CLEAR: bdcdata, bdcdata[], it_msg, it_msg[].

    AT LAST.
      PERFORM repete_ultimo.
    ENDAT.
  ENDLOOP.

  CLEAR : it_ltap, it_ltap[], it_ltak, it_ltak[].

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

  zwmlog-sammg   = vbss-sammg.
  zwmlog-zztpdoc = p_evento.
  zwmlog-zzdoc   = p_doc.
  zwmlog-vbeln   = p_doc.
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
*&      Form  repor_remessa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM repor_remessa USING pos_item pos_bon.
*sempre que não for possivel copiar os items da encomenda, vamos repor
*a situação inicial da remessa.
*  DATA: delivery_items LIKE lips OCCURS 0 WITH HEADER LINE,
*        prot LIKE prott OCCURS 0 WITH HEADER LINE,
*        con_x                 TYPE c   VALUE 'X'.
*
*  CLEAR : delivery_items, delivery_items[].
**---------------------------------------Verifica se a remessa está
**bloqueada
*  g_bloq = 2.
*  WHILE g_bloq = 2.
*    PERFORM gr_bloqueado USING 'REM_REPO' CHANGING g_bloq.
*  ENDWHILE.
*
**--------------------------------------------------------------seleccio
*n
**ar os items a repor
*  LOOP AT it_lips_aux WHERE vgpos = pos_item OR vgpos = pos_bon.
*
*    MOVE-CORRESPONDING it_lips_aux TO delivery_items.
*    APPEND delivery_items.
*
*  ENDLOOP.
*
*  CALL FUNCTION 'LE_MOB_DELIVERY_UPDATE'
*    EXPORTING
*      do_commit                = con_x
*    TABLES
*      t_delivery_items         = delivery_items
*      prot                     = prot
*    EXCEPTIONS
*      conversion_overflow      = 1
*      essential_data_missing   = 2
*      error                    = 3
*      nothing_to_update        = 4
*      lock_after_update_failed = 5
*      error_in_delivery_update = 6
*      OTHERS                   = 7.
*
*  CASE sy-subrc.
*
*    WHEN 0.
*      CLEAR sy-tfill.
*      DESCRIBE TABLE prot LINES sy-tfill.
*      IF sy-tfill NE 0 .
** error message from protocol
*        READ TABLE prot INDEX 1.
*
*        sy-msgty = 'E'.
*        sy-msgid = prot-msgid.
*        sy-msgno = prot-msgno.
*        sy-msgv1 = prot-msgv1.
*        sy-msgv2 = prot-msgv2.
*        sy-msgv3 = prot-msgv3.
*        sy-msgv4 = prot-msgv4.
*      ELSE.
**---SE a actualização da remessa correu bem eliminar os registos de log
*
*        DELETE FROM zwmlipslog WHERE vbeln = tabi_guias_por_item-guia.
*        "tabi_guias-vbeln.
*      ENDIF.
*
*    WHEN 1 OR 2 OR 3 OR 7.
** failed to update delivery &1
*      sy-msgty = 'E'.
*      sy-msgno = '313'.
*      sy-msgv1 = tabi_guias_por_item-guia.           "tabi_guias-vbeln.
*
*    WHEN 6.
** failed to update delivery &1 - after delivery unlock
*
**MESSAGE_ID = SY-MSGID.
**MESSAGE_NUMBER = SY-MSGNO.
**MESSAGE_VAR1 = SY-MSGV1.
**MESSAGE_VAR2 = SY-MSGV2.
**MESSAGE_VAR3 = SY-MSGV3.
**MESSAGE_VAR4 = SY-MSGV4.
**PERFORM ERROR_MESSAGE.
**PERFORM LOCK_DELIVERY USING DELV_ID .
*
*    WHEN 4.
*      sy-msgty = 'E'.
*      sy-msgno = '314'.
*      sy-msgv1 = tabi_guias_por_item-guia.
*      "tabi_guias-vbeln.
*
*
*    WHEN 5.
** failed to lock delivery &1 after update
*      sy-msgty = 'E'.
*      sy-msgno = '286'.
*      sy-msgv1 = tabi_guias_por_item-guia.
*      "tabi_guias-vbeln.
*
*  ENDCASE.
*
*  CLEAR l_zztpdoc.
*  l_zztpdoc = 'REM_REPO'.
*
*  PERFORM log USING tabi_guias_por_item-guia.
*
*ENDFORM.                    " repor_remessa
**&---------------------------------------------------------------------
**
**&      Form  apaga_items_remessa
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*FORM apaga_items_remessa .
*  DATA: vv_modo VALUE 'N'.
*
*  CLEAR : bdcdata, bdcdata[].
*
*  PERFORM bdc_dynpro   USING 'SAPMV50A'            '4004'.
*  PERFORM bdc_field    USING 'BDC_CURSOR'          'LIKP-VBELN'.
*  PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
*  PERFORM bdc_field    USING 'LIKP-VBELN'
*  tabi_guias_por_item-guia.    "tabi_guias-vbeln.
*
*  PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
*  PERFORM bdc_field    USING 'BDC_OKCODE'          '=MKAL_T'.
*
*  PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
*  PERFORM bdc_field    USING 'BDC_OKCODE'          '=POLO_T'.
*
*  PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
*  PERFORM bdc_field    USING 'BDC_OKCODE'          '=SICH_T'.
*
*  CLEAR : it_msg, it_msg[].
*
*  CALL TRANSACTION 'VL02N' USING bdcdata
*                          UPDATE 's'
*                          MODE vv_modo
*                          MESSAGES INTO it_msg.
*
*  CLEAR l_zztpdoc.
*  l_zztpdoc = 'REM_DITEM'.
*
*  PERFORM log USING tabi_guias_por_item-guia.
*  "tabi_guias-vbeln.
*
*  IF it_msg-msgtyp = 'S'.
*    COMMIT WORK AND WAIT.  WAIT UP TO 5 SECONDS.
*  ENDIF.
*
*  CLEAR : bdcdata, bdcdata[].

ENDFORM.                    " apaga_items_remessa
*&---------------------------------------------------------------------*
*&      Form  GR_bloqueado
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gr_bloqueado USING p_momento  CHANGING p_bloq.

  DATA : l_enq       LIKE seqg3 OCCURS 0 WITH HEADER LINE,
         l_gname     LIKE seqg3-gname VALUE 'LIKP',  l_dummy(18),
         l_garg      LIKE seqg3-garg.


*  CONCATENATE sy-mandt tabi_guias-vbeln INTO l_garg.
  CONCATENATE sy-mandt tabi_guias_por_item-guia INTO l_garg.


  CONDENSE l_garg NO-GAPS.

  CLEAR: l_enq, l_enq[].

  CALL FUNCTION 'ENQUE_READ'
    EXPORTING
      gclient = sy-mandt
      gname   = l_gname
      garg    = l_garg
*     GUNAME  = SY-UNAME
* IMPORTING
*     NUMBER  =
*     SUBRC   =
    TABLES
      enq     = l_enq.

  READ TABLE l_enq INDEX 1.

  IF sy-subrc = 0.
    p_bloq = 2.
    CONCATENATE 'Bloqueio'(008) tabi_guias_por_item-guia INTO
    l_dummy SEPARATED BY space.
  ELSEIF sy-subrc NE 0.
    CLEAR p_bloq.
*    CONCATENATE 'Forn desb' tabi_guias_por_item-guia
*    INTO l_dummy SEPARATED BY space.
  ENDIF.

*  CONCATENATE l_dummy p_momento INTO l_dummy SEPARATED BY space.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = l_dummy.


ENDFORM.                    " GR_bloqueado
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

*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = sy-msgid
*        message_lang   = sy-langu
*        message_type   = sy-msgty
*        message_number = sy-msgno
*        message_var1   = sy-msgv1.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO g_dummy.
  ELSE.
    ps_vrkme = p_meins_para.

  ENDIF.

ENDFORM.                    " converte_uni_med
*&---------------------------------------------------------------------*
*&      Form  bloqueia_remessa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bloqueia_remessa .
*-------------------------------------------no final do processo
*bloqueia a
*-------------------------------------------remessa se correr tudo bem

  READ TABLE tabi_guias_por_item INDEX z_tabix.

  IF g_erro IS INITIAL.
    tabi_guias_por_item-guia_bloqueada = 'X'.
    MODIFY tabi_guias_por_item INDEX z_tabix.
  ENDIF.

ENDFORM.                    " bloqueia_remessa
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

ENDFORM.                    " proc_bonus
*&---------------------------------------------------------------------*
*&      Form  FECHO_TRANSPORTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fecho_transporte .
  DATA: t_mode     VALUE 'E', l_data(10), l_hora(5).

  CHECK it_vttp-tknum NE space.

  CLEAR : bdcdata, bdcdata[], l_data.

  WRITE vttk-daten TO l_data.
  WRITE vttk-uaten TO l_hora USING EDIT MASK '__:__'.

  PERFORM bdc_dynpro   USING 'SAPMV56A'            '1011'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'VTTK-TKNUM'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=MM_TKAL'.
  PERFORM bdc_field    USING 'VTTK-TKNUM'          it_vttp-tknum.

  PERFORM bdc_dynpro   USING 'SAPMV56A'            '1020'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=MM_ST06'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'VTTK-DATBG'.
*-----------------------------------------------------------------
  PERFORM bdc_field    USING 'VTTK-DATBG'          l_data.

  IF l_hora NE '00:00'.
    PERFORM bdc_field    USING 'VTTK-UATBG'          l_hora.
  ENDIF.
*-----------------------------------------------------------------
  PERFORM bdc_dynpro   USING 'SAPMV56A'            '1020'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=MM_ST07'.

  PERFORM bdc_dynpro   USING 'SAPLV56F'            '0100'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=ONLI'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'VTTK-DATEN'.
  PERFORM bdc_field    USING 'VTTK-DATEN'          l_data.

  IF l_hora NE '00:00'.
    PERFORM bdc_field    USING 'VTTK-UATEN'          vttk-uaten.
  ENDIF.

  PERFORM bdc_dynpro   USING 'SAPMV56A'            '1020'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=MM_SICH'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'VTTK-TDLNR'.


  CLEAR :it_msg, it_msg[], zwmlog.

  CALL TRANSACTION 'VT02N' USING bdcdata
                   MODE   t_mode
                   UPDATE 'S'
                   MESSAGES INTO it_msg.

  CLEAR l_zztpdoc.
  l_zztpdoc = 'TRANSP'.

  PERFORM log USING t311a-rbnum ' '.

  CLEAR text.
  text = zwmlog-zzmsg.

  CLEAR: vttk-daten, vttk-uaten, vbss-sammg.
  setcursor = 'VBSS-SAMMG'.

  IF zwmlog-zztpmsg EQ 'E'.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

    g_rc = 4.

    LEAVE TO SCREEN 150.

  ELSE.
    text = 'Transporte Finalizado'(007).

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'S'
        message_number = '000'
        message_var1   = text.

    LEAVE TO SCREEN 150.


  ENDIF.


  CLEAR : bdcdata, bdcdata[].

ENDFORM.                    " FECHO_TRANSPORTE
*&---------------------------------------------------------------------*
*&      Form  SAIDA_MERCADORIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM saida_mercadoria .

  LOOP AT it_t311a.

    CLEAR : bdcdata, bdcdata[], g_rc.

    PERFORM sm USING it_t311a-rbnum.

  ENDLOOP.

ENDFORM.                    " SAIDA_MERCADORIA
*&---------------------------------------------------------------------*
*&      Form  VALIDA_TRANSPORTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM valida_transporte .

  DATA: l_num   LIKE sy-tfill, l_tknum LIKE vttp-tknum.

  CLEAR: l_num, l_tknum, g_rc, it_vttp, it_vttp[].


  SELECT tknum vbeln INTO TABLE it_vttp FROM  vttp
                     FOR ALL ENTRIES IN it_t311a
         WHERE  vbeln  = it_t311a-rbnum.


  READ TABLE it_vttp INDEX 1.

  SELECT SINGLE stdis INTO vttk-stdis FROM  vttk
         WHERE  tknum  = it_vttp-tknum
         AND    stdis  NE space.

  IF sy-subrc NE 0.
    CLEAR: text, g_rc.
    CONCATENATE 'Não foi seleccionado Organização de Transporte'(009)
                it_vttp-tknum INTO text SEPARATED BY space.

    setcursor = 'VBSS-SAMMG'.
    CLEAR: vttk-daten, vttk-uaten, vbss-sammg.
**
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

    MESSAGE e000(zwmmsg001) WITH text INTO g_dummy.
    sy-msgv1 = g_dummy.

    g_rc = '4'.

    CLEAR l_zztpdoc.
    l_zztpdoc = 'TP_ORGTRP'.

    PERFORM log USING t311a-rbnum ' '.

    LEAVE TO SCREEN 150.
*----------------------------------------------------Ins Mai2005

  ENDIF.

*  DESCRIBE TABLE it_vttp LINES sy-tfill.
*
*  IF l_num <> sy-tfill.
*    CLEAR g_dummy.
*    CLEAR: text, g_rc.
*    CONCATENATE 'Não foram processados todos os Fornecimentos do Grupo'
*                vbss-sammg INTO text SEPARATED BY space.
***
***      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
***        EXPORTING
***          message_id     = 'ZWMMSG001'
***          message_lang   = sy-langu
***          message_type   = 'E'
***          message_number = '000'
***          message_var1   = text.
*
*    MESSAGE e000(zwmmsg001) WITH text INTO g_dummy.
*    sy-msgv1 = g_dummy.
**   & & & &
*
*    g_rc = '4'.
*
*    CLEAR l_zztpdoc.
*    l_zztpdoc = 'TP_NFORN'.
*
*    PERFORM log USING t311a-rbnum ' '.
*
*    LEAVE TO SCREEN 150.
*
*    LEAVE SCREEN.
*    SET SCREEN 100.
*  ENDIF.

***  ENDIF.

  CHECK g_rc IS INITIAL.
*---Validar que num Grupo todos os Fornecimentos tem o mesmo transporte

  SORT it_vttp BY tknum.
  DELETE ADJACENT DUPLICATES FROM it_vttp COMPARING tknum.
  DESCRIBE TABLE it_vttp LINES sy-tfill.
  IF sy-tfill GT 1.
    CLEAR text.
    CLEAR: g_dummy, g_rc.

    CONCATENATE 'Fornecimentos do Grupo'(010) vbss-sammg
             'tem transportes diferentes'(011) INTO text SEPARATED BY space.

    setcursor = 'VBSS-SAMMG'.
    CLEAR: vttk-daten, vttk-uaten, vbss-sammg.

*
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.


    MESSAGE e000(zwmmsg001) WITH text INTO g_dummy.
    sy-msgv1 = g_dummy.
*   & & & &

    g_rc = '4'.

    CLEAR l_zztpdoc.
    l_zztpdoc = 'TP_DIFERE'.

    PERFORM log USING t311a-rbnum ' '.

    LEAVE TO SCREEN 150.

  ENDIF.

ENDFORM.                    " VALIDA_TRANSPORTE
*&---------------------------------------------------------------------*
*&      Form  sm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sm USING p_guia.

  DATA : sm_modo VALUE 'N'.
** MR-alterar data de saida de merc
  DATA : l_data(10).

*  PERFORM bdc_dynpro   USING 'SAPMV50A'            '4004'.
*  PERFORM bdc_field    USING 'BDC_CURSOR'          'LIKP-VBELN'.
*  PERFORM bdc_field    USING 'BDC_OKCODE'          '=WABU_T'.
*  PERFORM bdc_field    USING 'LIKP-VBELN'          p_guia.

**  PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
**  PERFORM bdc_field    USING 'BDC_OKCODE'          '=WABU_T'.

  PERFORM bdc_dynpro   USING 'SAPMV50A'            '4004'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'LIKP-VBELN'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '/00'.
  PERFORM bdc_field    USING 'LIKP-VBELN'          p_guia.


  WRITE vttk-daten TO l_data.
  PERFORM bdc_dynpro   USING 'SAPMV50A'            '1000'.
  PERFORM bdc_field    USING 'BDC_OKCODE'          '=WABU_T'.
  PERFORM bdc_field    USING 'BDC_CURSOR'          'LIKP-WADAT_IST'.
  PERFORM bdc_field    USING 'LIKP-WADAT_IST'       l_data.
***

  CLEAR :it_msg, it_msg[], zwmlog.

  CALL TRANSACTION 'VL02N' USING bdcdata
                   MODE   sm_modo
                   UPDATE 'S'
                   MESSAGES INTO it_msg.


  CLEAR l_zztpdoc.
  l_zztpdoc = 'SAI_MERC'.

  PERFORM log USING p_guia ' '.

  IF zwmlog-zztpmsg EQ 'E'.
    CLEAR text.
    text = zwmlog-zzmsg.

    CLEAR: vttk-daten, vttk-uaten, vbss-sammg.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.


    LEAVE TO SCREEN 150.
  ELSE.

    COMMIT WORK AND WAIT.
    WAIT UP TO 3 SECONDS.

  ENDIF.

ENDFORM.                    " sm
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
      .....
  ENDCASE.


ENDFORM.                    " gera_reg_reprocesso
*&---------------------------------------------------------------------*
*&      Form  credito_reprocesso
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM credito_reprocesso USING p_tpdoc p_item p_qtd
                              p_guia  p_encom p_grupo
                              p_matnr p_uni p_ctitem.

  LOOP AT tabi_creditos.

    CLEAR l_pos.
    l_pos = sy-tabix.
    CONCATENATE 'CR_' l_pos INTO zwmlog02-zztpdoc.
    MOVE-CORRESPONDING tabi_creditos TO zwmlog02.

    zwmlog02-sammg     = p_grupo.
    zwmlog02-vbeln     = p_guia.
    zwmlog02-posnr     = bi_tab-posnr.
    zwmlog02-vbeln_enc = p_encom.

    MODIFY zwmlog02.

    COMMIT WORK AND WAIT.

  ENDLOOP.
ENDFORM.                    " credito_reprocesso
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
*        zwmlog02-posnr     = p_item.
        zwmlog02-lgtyp     = z1om_lgtyp.
        zwmlog02-hu        = it_ltap-sscc.
*        zwmlog02-pstyv     = p_ctitem.

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
        CALL FUNCTION 'C14W_CHAR_NUMBER_CONVERSION'
          EXPORTING
            i_string                   = it_creditos_log-kbetr
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
      .....
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
**  clear z1com_s-prliq.
*   select single prliq into z1com_s-prliq from z1mom
*                       where vbeln = tabi_creditos-vbeln
*                       and   posnr = tabi_creditos-vgpos.
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
FORM verifica_servisan_tx USING remessa rc.

  DATA: l_kunnr LIKE vbpa-kunnr.
  CLEAR: l_kunnr, rc.

*  SELECT v~kunnr INTO l_kunnr FROM vbpa AS v
*   INNER JOIN zwm039 AS z
*   ON z~kunnr = v~kunnr
*   WHERE v~vbeln = remessa
*   AND   v~parvw = z1om_parvw
*   AND   z~lgnum = z1om_lgnum.
*
*  ENDSELECT.
*
*  IF sy-subrc = 0.
*    rc = 4.
*  ENDIF.
  CLEAR rc.

  SELECT kunnr INTO vbpa-kunnr FROM  vbpa
         WHERE  vbeln  = remessa
         AND    parvw = z1om_parvw.

  ENDSELECT.

  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM  zwm039
*           WHERE  lgnum  = z1om_lgnum  " << DEL ROFF(SDF):TMGP:06.01.2016 16:59:59
           WHERE  lgnum  = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 17:00:00
           AND    kunnr  = vbpa-kunnr.

    IF sy-subrc = 0.
      rc = 4.
    ENDIF.

  ENDIF.

ENDFORM.                    " verifica_servican
*&---------------------------------------------------------------------*
*&      Form  gera_reg_reprocesso_save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P1  text
*      -->P_P2  text
*      -->P_P3  text
*      -->P_P4  text
*      -->P_P5  text
*      -->P_P6  text
*      -->P_P7  text
*      -->P_P8  text
*      -->P_P9  text
*----------------------------------------------------------------------*
FORM gera_reg_reprocesso_save  USING    p_tpdoc p_item p_qtd
                               p_guia  p_encom p_grupo
                               p_matnr p_uni p_ctitem.
  CASE p_tpdoc.

    WHEN 'OT'.

      PERFORM determina_lote_para_ot.

      LOOP AT bi_tab.

        CLEAR l_idx.
        l_idx = sy-tabix.
        CONCATENATE 'OT_' l_idx INTO zwmlog02-zztpdoc.

        zwmlog02-sammg     = p_grupo.
        zwmlog02-vbeln     = p_guia.
        zwmlog02-posnr     = bi_tab-posnr.
        zwmlog02-vbeln_enc = p_encom.
        zwmlog02-lfimg     = bi_tab-lfimg.
        zwmlog02-lgtyp     = z1om_lgtyp.
        zwmlog02-matnr     = bi_tab-matnr.
        zwmlog02-charg     = bi_tab-charg.
        zwmlog02-vrkme     = p_uni.
        zwmlog02-pstyv     = p_ctitem.

        MODIFY zwmlog02.

        COMMIT WORK AND WAIT.

      ENDLOOP.

      CLEAR : it_ot, it_ot[], bi_tab, bi_tab[], l_idx.

    WHEN 'HU'.

      PERFORM get_paletes.

      LOOP AT it_ltap.

        CLEAR l_idx.
        l_idx = sy-tabix.
        CONCATENATE 'HU_' l_idx INTO zwmlog02-zztpdoc.

        zwmlog02-sammg     = p_grupo.
        zwmlog02-vbeln     = p_guia.
        zwmlog02-posnr     = p_item.
        zwmlog02-lgtyp     = z1om_lgtyp.
        zwmlog02-hu        = it_ltap-sscc.
        zwmlog02-pstyv     = p_ctitem.

        MODIFY zwmlog02.

        COMMIT WORK AND WAIT.

      ENDLOOP.

      CLEAR: it_ltap, it_ltap[].

    WHEN OTHERS.
      .....
  ENDCASE.


ENDFORM.                    " gera_reg_reprocesso_save
*&---------------------------------------------------------------------*
*&      Form  repete_ultimo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM repete_ultimo .

  CLEAR : it_msg, it_msg[].

  CALL FUNCTION 'ZWM_EMBALAR_HU'
    EXPORTING
*     warehouse                  = l_warehouse " << DEL ROFF(SDF):TMGP:06.01.2016 17:02:20
      warehouse                  = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 17:02:22
      hu                         = l_hu
      vbeln                      = l_vbeln
    TABLES
      return_msg                 = it_msg
    EXCEPTIONS
      empty_table                = 1
      reference_document_differs = 2
      empty_delivery_item        = 3
      item_not_found             = 4
      OTHERS                     = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO g_dummy.
  ENDIF.

  PERFORM log USING l_hu ' '.

  IF it_msg-msgtyp = 'S'.
    COMMIT WORK AND WAIT.  WAIT UP TO 5 SECONDS.
  ELSE.
    PERFORM gera_reg_log USING 'HU' ' '.
    PERFORM gera_reg_log USING 'CR' ' '.
  ENDIF.

  CLEAR: bdcdata, bdcdata[], it_msg, it_msg[].
ENDFORM.                    " repete_ultimo
*&---------------------------------------------------------------------*
*&      Form  verifica_servisan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_RC  text
*----------------------------------------------------------------------*
FORM verifica_servisan  USING    rc.
  DATA: l_kunnr LIKE vbpa-kunnr.
  CLEAR: l_kunnr, rc.


  SELECT v~kunnr INTO l_kunnr FROM vbpa AS v
   INNER JOIN zwm039 AS z
   ON z~kunnr = v~kunnr
   WHERE v~vbeln = z1com_s-vbeln
   AND   v~parvw = z1om_parvw
*   AND   z~lgnum = z1om_lgnum. " << DEL ROFF(SDF):TMGP:06.01.2016 17:00:06
   AND   z~lgnum = gv_lgnum. " << INS ROFF(SDF):TMGP:06.01.2016 17:00:07

  ENDSELECT.

  IF sy-subrc = 0.
*--------------------------se for cliente Servisan não embalamos
    CLEAR: l_zztpdoc, g_dummy.
    MESSAGE s000(zwmmsg001) WITH 'Cliente Servisan, não Embalar!'
       INTO g_dummy.
*   & & & &


    CONCATENATE 'SR_' l_idx INTO l_zztpdoc.
    CONDENSE l_zztpdoc NO-GAPS.
*------------------------------------------------------------

    PERFORM log USING z1com_s-vbeln ' '.

    rc = 4.
  ENDIF.

ENDFORM.                    " verifica_servisan
*&---------------------------------------------------------------------*
*&      Form  VALIDA_GRUPO_EXISTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_grupo_existe .

  IF vbss-sammg IS INITIAL.
    CLEAR text.
    text = 'Inserir um Grupo'(012).

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

    setcursor = 'VBSS-SAMMG'.

  ENDIF.

ENDFORM.                    " VALIDA_GRUPO_EXISTE
*&---------------------------------------------------------------------*
*&      Form  get_tipo_grupo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_tipo_grupo .

  SELECT SINGLE * FROM  t311 CLIENT SPECIFIED
*         WHERE  lgnum  = z1om_lgnum  " << DEL ROFF(SDF):TMGP:06.01.2016 17:00:20
         WHERE  lgnum  = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 17:00:22
         AND    refnr  = vbss-sammg.

  IF sy-subrc NE 0.

    setcursor = 'VBSS-SAMMG'.

    CLEAR text.
    text = 'Não é Grupo WM'(013).

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

  ENDIF.

*  CHECK sy-subrc = 0.

ENDFORM.                    " get_tipo_grupo
*&---------------------------------------------------------------------*
*&      Form  valida_transporte_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_transporte_1 .
  DATA: l_num   LIKE sy-tfill, l_tknum LIKE vttp-tknum.


  CLEAR: l_num, l_tknum, g_rc.

  SELECT tknum vbeln INTO (vttp-tknum, vttp-vbeln)
                      FROM  vttp UP TO 1 ROWS
         WHERE  vbeln  = t311a-rbnum.


    SELECT SINGLE stdis INTO vttk-stdis FROM  vttk
           WHERE  tknum  = vttp-tknum
           AND    stdis  NE space.

    IF sy-subrc NE 0.
      CLEAR: text, g_rc.
      CONCATENATE 'Não foi seleccionado Organização de Transporte'(014)
                  vttp-tknum INTO text SEPARATED BY space.
**
      setcursor = 'VBSS-SAMMG'.
      CLEAR: vttk-daten, vttk-uaten, vbss-sammg.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '000'
          message_var1   = text.

*      MESSAGE e000(zwmmsg001) WITH text INTO g_dummy.
*      sy-msgv1 = g_dummy.

      g_rc = '4'.

      CLEAR: l_zztpdoc, t311a, it_t311a[], it_t311a.
*      l_zztpdoc = 'TP_ORGTRP'.
*
*      PERFORM log USING t311a-rbnum ' '.

      LEAVE TO SCREEN 150.
*----------------------------------------------------Ins Mai2005

    ENDIF.

    MOVE-CORRESPONDING vttp TO it_vttp.
    APPEND it_vttp. CLEAR it_vttp.

  ENDSELECT.

*------------------------------------VALIDAR A EXISTENCIA DE TRANSPORTE
  IF sy-subrc NE 0.

    CLEAR: text, g_rc.
    CONCATENATE 'Não existe Transporte para a remessa'(015)
                t311a-rbnum INTO text SEPARATED BY space.

    setcursor = 'VBSS-SAMMG'.
    CLEAR: vttk-daten, vttk-uaten, vbss-sammg.

**
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

*    MESSAGE e000(zwmmsg001) WITH text INTO g_dummy.
*    sy-msgv1 = g_dummy.

    g_rc = '4'.

    CLEAR l_zztpdoc.
    l_zztpdoc = 'TP_ORGTRP'.

*    PERFORM log USING t311a-rbnum ' '.
    CLEAR: l_zztpdoc, t311a, it_t311a[], it_t311a.
    LEAVE TO SCREEN 150.

  ENDIF.

ENDFORM.                    " valida_transporte_1
*&---------------------------------------------------------------------*
*&      Form  VALIDA_TRANSPORTES_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_transportes_2 .

  DATA: l_num   LIKE sy-tfill, l_tknum LIKE vttp-tknum.

  CLEAR: l_num, l_tknum, g_rc, it_vttp.

  CHECK NOT it_vttp[] IS INITIAL.
  SORT it_vttp BY tknum.
  DELETE ADJACENT DUPLICATES FROM it_vttp COMPARING tknum.

  DESCRIBE TABLE it_vttp LINES sy-tfill.
  IF sy-tfill GT 1.

    CLEAR: g_dummy, g_rc, text.
    g_rc = '4'.
    CONCATENATE 'Fornecimentos do Grupo'(016) vbss-sammg
             'tem transportes diferentes'(017) INTO text SEPARATED BY space.

    setcursor = 'VBSS-SAMMG'.
    CLEAR: vttk-daten, vttk-uaten, vbss-sammg.

*
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

    CLEAR: l_zztpdoc, t311a, it_t311a[], it_t311a.
    LEAVE TO SCREEN 150.

  ENDIF.

  CLEAR : it_vttp, it_vttp[], l_num, l_tknum.
*------------------------------------------------------------------

ENDFORM.                    " VALIDA_TRANSPORTES_2
*&---------------------------------------------------------------------*
*&      Form  VALIDA_EMBALAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_embalamento .
  CLEAR text.
  CLEAR: g_dummy, g_rc, it_t311a, it_t311a[], t311a.

  setcursor = 'VBSS-SAMMG'.

  CLEAR text.
  CONCATENATE 'A Remessa'(018) t311a-rbnum
              'não foi Embalada'(019) INTO  text
    SEPARATED BY space.

  CLEAR: vttk-daten, vttk-uaten, vbss-sammg.

  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWMMSG001'
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = '000'
      message_var1   = text.

  LEAVE TO SCREEN 150.


ENDFORM.                    " VALIDA_EMBALAMENTO
*&---------------------------------------------------------------------*
*&      Form  VALIDA_STATUS_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_status_picking .

  setcursor = 'VBSS-SAMMG'.

  CLEAR:  text.

  CONCATENATE 'A Remessa'(018) t311a-rbnum
              'Picking não concluido'(020) INTO  text
    SEPARATED BY space.

  CLEAR: vttk-daten, vttk-uaten, vbss-sammg.
  CLEAR: t311a, it_t311a[], it_t311a.


  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWMMSG001'
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = '000'
      message_var1   = text.

  LEAVE TO SCREEN 150.

ENDFORM.                    " VALIDA_STATUS_PICKING
*&---------------------------------------------------------------------*
*&      Form  VALIDA_SAIDA_MERCADORIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_saida_mercadoria .

  CLEAR text.

  CONCATENATE 'A Remessa'(018) t311a-rbnum
              'tem Saida de Mercadoria efectuada'(021) INTO  text
    SEPARATED BY space.

  setcursor = 'VBSS-SAMMG'.
  CLEAR: vttk-daten, vttk-uaten, vbss-sammg.
  CLEAR: t311a, it_t311a[], it_t311a.

  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWMMSG001'
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = '000'
      message_var1   = text.

  LEAVE TO SCREEN 150.


ENDFORM.                    " VALIDA_SAIDA_MERCADORIA

*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Form  VALIDA_REMESSAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_remessas.
  DATA: lv_vkorg TYPE vkorg.

  CLEAR : g_rc.

** Loja online - Valida alterações à remessa
**********************************************************************
  LOOP AT it_t311a.

    SELECT SINGLE vkorg FROM likp
                        INTO lv_vkorg
                        WHERE vbeln = it_t311a-rbnum.

    CHECK lv_vkorg EQ 'RP12'.

    CALL FUNCTION 'ZWM_DELIVERY_CHANGE'
      EXPORTING
        i_vbeln = it_t311a-rbnum
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.

    IF sy-subrc <> 0.
      g_rc = 4.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF g_rc IS NOT INITIAL.
    LEAVE TO SCREEN 150.
  ENDIF.
ENDFORM.                    " VALIDA_REMESSAS

*}   INSERT
*&---------------------------------------------------------------------*
*&      Form  AJUSTES_STOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ajustes_stock.

  CONSTANTS: lc_lgnum TYPE lgnum VALUE '100'.
  CONSTANTS: lc_rem   TYPE lgtyp VALUE '916'.
  CONSTANTS: lc_lgtyp TYPE lgtyp VALUE 'PKL'.
  CONSTANTS: lc_lgpla TYPE lgpla VALUE 'ACERTO PKL'.
  CONSTANTS: lc_dest  TYPE lgtyp VALUE '999'.
  CONSTANTS: lc_bwlvs TYPE bwlvs VALUE '999'.

  DATA: lv_2step      TYPE flag.
  DATA: lv_cond       TYPE string.
  DATA: lv_tanum      TYPE tanum.
  DATA: ls_return     TYPE bdcmsgcoll.
  DATA: lt_ltak       LIKE ltak       OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap       LIKE ltap       OCCURS 0 WITH HEADER LINE.
  DATA: lt_lqua       LIKE lqua       OCCURS 0 WITH HEADER LINE.
  DATA: lt_lqua_pkl   LIKE lqua       OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap_creat LIKE ltap_creat OCCURS 0 WITH HEADER LINE.

  WAIT UP TO 20 SECONDS.
** Validar Stock
**********************************************************************
  CHECK it_t311a[] IS NOT INITIAL.

  SELECT *
    FROM lqua INTO TABLE lt_lqua
    FOR ALL ENTRIES IN it_t311a
*    WHERE lgnum = lc_lgnum  AND " << DEL ROFF(SDF):TMGP:06.01.2016 17:03:11
    WHERE lgnum = gv_lgnum  AND " << INS ROFF(SDF):TMGP:06.01.2016 17:03:13
          lgtyp = lc_rem    AND
          lgpla = it_t311a-rbnum.

  CHECK lt_lqua[] IS NOT INITIAL.

** Validar Grupo 2 passos
  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
*     i_lgnum = lc_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 17:03:24
      i_lgnum = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 17:03:25
      i_refnr = vbss-sammg
    IMPORTING
      e_2step = lv_2step
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.

** Obter OTs
  SELECT *
    FROM ltak INTO TABLE lt_ltak
*    WHERE lgnum = lc_lgnum AND " << DEL ROFF(SDF):TMGP:06.01.2016 17:03:39
    WHERE lgnum = gv_lgnum AND " << INS ROFF(SDF):TMGP:06.01.2016 17:03:40
          refnr = vbss-sammg.

  IF lt_ltak[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      FOR ALL ENTRIES IN lt_ltak
      WHERE lgnum = lt_ltak-lgnum AND
            tanum = lt_ltak-tanum.
  ENDIF.

  DELETE lt_ltap WHERE vorga <> 'LF' AND
                       vorga <> 'QB' AND
                       vorga <> space.

  DELETE lt_ltap WHERE vltyp <> lc_lgtyp.

** Stock do tipo deposito PKL
  IF lt_ltap[] IS NOT INITIAL.
    SELECT *
      FROM lqua INTO TABLE lt_lqua_pkl
      FOR ALL ENTRIES IN lt_ltap
         WHERE lgnum = lt_ltap-lgnum  AND
               lgtyp = lt_ltap-vltyp  AND
               lgpla = lt_ltap-vlpla.
  ENDIF.

** Retornar Stock
**********************************************************************
  SORT lt_lqua     BY lgpla matnr charg.
  SORT lt_lqua_pkl BY lgpla matnr charg.
  SORT lt_ltap     BY nlpla matnr charg vlpla.

  REFRESH: lt_ltap_creat.

  LOOP AT lt_lqua.

    IF lv_2step = 'X'.
      LOOP AT lt_ltap WHERE matnr = lt_lqua-matnr AND
                            charg = lt_lqua-charg.

        CLEAR lt_lqua_pkl.
        READ TABLE lt_lqua_pkl WITH KEY lgpla = lt_ltap-vlpla
                                        matnr = lt_ltap-matnr
                                        charg = lt_ltap-charg.

        IF lt_lqua_pkl-verme IS NOT INITIAL.
          EXIT.
        ENDIF.
      ENDLOOP.

    ELSE.
      LOOP AT lt_ltap WHERE nlpla = lt_lqua-lgpla AND
                            matnr = lt_lqua-matnr AND
                            charg = lt_lqua-charg.

        CLEAR lt_lqua_pkl.
        READ TABLE lt_lqua_pkl WITH KEY lgpla = lt_ltap-vlpla
                                        matnr = lt_ltap-matnr
                                        charg = lt_ltap-charg.

        IF lt_lqua_pkl-verme IS NOT INITIAL.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    CHECK lt_lqua_pkl-verme IS NOT INITIAL.

    CLEAR lt_ltap_creat.
    lt_ltap_creat-werks = lt_lqua-werks.
    lt_ltap_creat-lgort = lt_lqua-lgort.
    lt_ltap_creat-matnr = lt_lqua-matnr.
    lt_ltap_creat-charg = lt_lqua-charg.
    lt_ltap_creat-anfme = lt_lqua-verme.
    lt_ltap_creat-altme = lt_lqua-meins.

    lt_ltap_creat-vltyp = lt_lqua-lgtyp.
    lt_ltap_creat-vlpla = lt_lqua-lgpla.
*    lt_ltap_creat-nltyp = lt_lqua_pkl-lgtyp.
*    lt_ltap_creat-nlpla = lt_lqua_pkl-lgpla.
    lt_ltap_creat-nltyp = lc_dest.
    lt_ltap_creat-nlpla = lc_lgpla.
    lt_ltap_creat-squit = 'X'.
    APPEND lt_ltap_creat.
  ENDLOOP.

** Criar OT
  IF lt_ltap_creat[] IS NOT INITIAL.

    CLEAR lv_tanum.
    CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
      EXPORTING
*       i_lgnum       = lc_lgnum " << DEL ROFF(SDF):TMGP:06.01.2016 17:03:56
        i_lgnum       = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 17:03:57
        i_bwlvs       = lc_bwlvs
      IMPORTING
        e_tanum       = lv_tanum
      TABLES
        t_ltap_creat  = lt_ltap_creat
      EXCEPTIONS
        error_message = 99.

    IF sy-subrc <> 0 OR lv_tanum IS INITIAL.
      CLEAR ls_return.
      ls_return-msgid = sy-msgid.
      ls_return-msgnr = sy-msgno.
      ls_return-msgv1 = sy-msgv1.
      ls_return-msgv2 = sy-msgv2.
      ls_return-msgv3 = sy-msgv3.
      ls_return-msgv4 = sy-msgv4.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_return-msgid
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = ls_return-msgnr
          message_var1   = ls_return-msgv1
          message_var2   = ls_return-msgv2
          message_var3   = ls_return-msgv3
          message_var4   = ls_return-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.                    " AJUSTES_STOCK
*&---------------------------------------------------------------------*
*&      Form  VALIDA_TODAS_SAIDAS_MERC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_todas_saidas_merc .
  DATA: lt_vbuk  TYPE TABLE OF vbuk,
        ls_vbuk  TYPE vbuk,
        ls_t311a TYPE t311a.

**********************************************************************

  SELECT * FROM  vbuk INTO TABLE lt_vbuk
    FOR ALL ENTRIES IN  it_t311a
    WHERE  vbeln  = it_t311a-rbnum.

  LOOP AT it_t311a INTO ls_t311a.
    READ TABLE lt_vbuk INTO ls_vbuk WITH KEY vbeln = ls_t311a-rbnum.
    IF ls_vbuk-wbstk = 'C'.
      DELETE TABLE it_t311a FROM ls_t311a.
    ENDIF.
  ENDLOOP.

  IF it_t311a[] IS INITIAL.
    CLEAR text.
    MOVE 'Grupo sem remessas a processar.'(022) TO text.
    setcursor = 'VBSS-SAMMG'.
    CLEAR: vttk-daten, vttk-uaten, vbss-sammg.
    CLEAR: t311a, it_t311a[], it_t311a.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = text.

    LEAVE TO SCREEN 150.

  ENDIF.
ENDFORM.                    " VALIDA_TODAS_SAIDAS_MERC
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

  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user      = sy-uname
      i_recall    = 'X'
      i_usewm     = 'X'
      i_userf     = 'X'
      i_usemm     = 'X'
      i_useaut    = 'X'
      i_get_lgnum = 'X'
      i_get_werks = ''
      i_get_lgort = ''
    CHANGING
      c_lgnum     = gv_lgnum
    EXCEPTIONS
      error       = 1
      user_back   = 2
      OTHERS      = 3.

ENDFORM.                    " F_USER_GET_LGNUM
*& End of Modification by Tiago Pateiro - ROFF @ 06.01.2016 15:27:58
*&---------------------------------------------------------------------*
*&      Form  FREE_DOOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_door .
  CHECK gv_lgnum EQ 100.

  z_wm_cl_management=>free_door( i_lgnum = gv_lgnum i_refnr = vbss-sammg ).

ENDFORM.
