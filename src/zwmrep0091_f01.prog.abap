*&---------------------------------------------------------------------*
*&  Include           ZWMREP0090_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_DADOS
*&---------------------------------------------------------------------*
FORM get_dados .


  DATA: lv_qtd                    TYPE i,
        lv_qtd_falta_completas    TYPE i,
        lv_qtd_falta_picking      TYPE p DECIMALS 2,
        lv_rest                   TYPE p DECIMALS 2,
        lv_remessa_com_picking(1),
        lv_valor(5)               TYPE c,
        lv_idx_dif                LIKE sy-tabix,
        lv_idx_fal                LIKE sy-tabix,
        lv_qtd_fornecida          TYPE rfmng,
        lv_2step                  TYPE flag,
        lv_2spart                 TYPE flag,
        lv_tabix                  TYPE sytabix,
        lv_count                  TYPE i.



  DATA: BEGIN OF lt_diferencas OCCURS 0,
          ordem           TYPE vbeln_va,
          item            TYPE vgpos,
          remessa         TYPE vbeln_vl,
          matnr           TYPE matnr,
          arktx           TYPE arktx,
          kwmeng          TYPE kwmeng,
          lfimg           TYPE lfimg,
          item_fechado(1),
        END OF lt_diferencas.


  DATA:
    lt_diferencas_aux LIKE lt_diferencas OCCURS 0 WITH HEADER LINE,
    lt_vbfa_faltas    LIKE vbfa OCCURS 0 WITH HEADER LINE,
    lt_vbfa_fal_aux   LIKE vbfa OCCURS 0 WITH HEADER LINE,
    lt_ekbe_fal_aux   LIKE ekbe OCCURS 0 WITH HEADER LINE,
    wa_lips           LIKE lips,
    lt_paletes        LIKE zpalete_picking OCCURS 0 WITH HEADER LINE,
    wa_paletes        LIKE lt_paletes.

  DATA: lv_last_refnr TYPE lvs_refnr.

  FIELD-SYMBOLS: <ls_dados> LIKE gt_dados_ant.

  RANGES: r_ordem FOR vbak-vbeln,
          r_vbeln FOR vbfa-vbelv.



  CLEAR:   gt_t311, gt_t311a, gt_vttk, gt_vttp, gt_likp,
           gt_lips, gt_kna1,  gt_vbak, gt_vbap, gt_vbfa,
           gt_dados_ant, gt_header, gt_items, lt_paletes, lt_diferencas,
           wa_lips, gt_faltas, r_vbeln.

  REFRESH: gt_t311, gt_t311a, gt_vttk, gt_vttp, gt_likp,
           gt_lips, gt_kna1,  gt_vbak, gt_vbap, gt_vbfa,
           gt_dados_ant, gt_header, gt_items, lt_paletes, lt_diferencas,
           gt_faltas, r_vbeln.



  PERFORM progresso USING '5'
                          text-007.

  SELECT * INTO TABLE gt_t311 FROM t311
           WHERE lgnum EQ p_lgnum
             AND refnr IN s_refnr
             AND refnt IN s_refnt
             AND datum IN s_datum
    ORDER BY PRIMARY KEY.
  IF sy-subrc NE 0 OR gt_t311[] IS INITIAL.
*   274  Sem dados para processar para os criterios utilizados!
    MESSAGE s274 DISPLAY LIKE 'E'.
    gv_exit = 'X'.
    EXIT.
  ENDIF.

  SELECT * INTO TABLE gt_t311a FROM t311a
           FOR ALL ENTRIES IN gt_t311
           WHERE lgnum EQ gt_t311-lgnum
             AND refnr EQ gt_t311-refnr
    ORDER BY PRIMARY KEY.
  IF sy-subrc NE 0 OR gt_t311a[] IS INITIAL.
*   274  Sem dados para processar para os criterios utilizados!
    MESSAGE s274 DISPLAY LIKE 'E'.
    gv_exit = 'X'.
    EXIT.
  ENDIF.

  PERFORM progresso USING '10'
                          text-008.

  SELECT likp~* INTO TABLE @gt_likp FROM likp
***x01
    INNER JOIN vbuk ON vbuk~vbeln EQ likp~vbeln
***x01
           FOR ALL ENTRIES IN @gt_t311a
           WHERE likp~vbeln EQ @gt_t311a-rbnum
             AND vkorg      IN @s_vkorg
             AND vstel      IN @s_vstel
***x01
          AND vbuk~wbstk IN @s_wbstk.
  SORT gt_likp BY vbeln.
***x01

  IF sy-subrc NE 0 OR gt_likp[] IS INITIAL.
*   275	Sem remessas para os criterios utilizados!
    MESSAGE s275 DISPLAY LIKE 'E'.
    gv_exit = 'X'.
    EXIT.
  ENDIF.

  LOOP AT gt_t311a.
    READ TABLE gt_likp WITH KEY vbeln = gt_t311a-rbnum.
    IF sy-subrc NE 0.
      DELETE gt_t311a WHERE refnr = gt_t311a-refnr.
      DELETE gt_t311  WHERE refnr = gt_t311a-refnr.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  PERFORM progresso USING '15'
                          text-009.

  SELECT * INTO TABLE gt_kna1 FROM kna1
           FOR ALL ENTRIES IN gt_likp
           WHERE kunnr EQ gt_likp-kunnr
    ORDER BY PRIMARY KEY.

  PERFORM progresso USING '20'
                          text-010.

  SELECT * INTO TABLE gt_lips FROM lips
          FOR ALL ENTRIES IN gt_likp
          WHERE vbeln EQ gt_likp-vbeln.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 19.07.2012 11:58:53
*  Motivo: Apaga Items Não relevantes Para Picking
*--------------------------------------------------------------------*
  DELETE gt_lips WHERE lgnum EQ ''.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


  IF gt_lips[] IS INITIAL.
*   275	Sem remessas para os criterios utilizados!
    MESSAGE s275 DISPLAY LIKE 'E'.
    gv_exit = 'X'.
    EXIT.
  ENDIF.

  PERFORM progresso USING '25'
                          text-011.

  SELECT * INTO TABLE gt_vttp FROM vttp
           FOR ALL ENTRIES IN gt_likp
           WHERE vbeln EQ gt_likp-vbeln
***x01
             AND tknum IN s_tknum.
***x01
  SORT gt_vttp BY vbeln.

  IF gt_vttp[] IS NOT INITIAL.
    SELECT * INTO TABLE gt_vttk FROM vttk
             FOR ALL ENTRIES IN gt_vttp
             WHERE tknum EQ gt_vttp-tknum
               AND sdabw IN s_sdabw
               AND tdlnr IN s_tdlnr
               AND dtdis IN s_dtdis
***x01
          AND vttk~dalbg IN s_dalbg
          AND vttk~dplbg IN s_dplbg
      ORDER BY PRIMARY KEY.
***x01
  ENDIF.

*GT_VTTK, GT_VTTP, GT_LIPS, GT_LIKP, GT_T311A, GT_T311

  LOOP AT gt_vttp.
    READ TABLE gt_vttk WITH KEY tknum = gt_vttp-tknum.
    IF sy-subrc NE 0.
      DELETE gt_vttk WHERE tknum EQ gt_vttp-tknum.
      DELETE gt_vttp WHERE tknum EQ gt_vttp-tknum.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_likp.
    READ TABLE gt_vttp WITH KEY vbeln = gt_likp-vbeln.
    IF sy-subrc NE 0.
      DELETE gt_lips WHERE vbeln EQ gt_likp-vbeln.
      DELETE gt_likp WHERE vbeln EQ gt_likp-vbeln.
      CONTINUE.
    ENDIF.
  ENDLOOP.

* RENUPG00053 -> Carlos Carloto - ROFF 18.11.2020
  IF gt_lips[] IS INITIAL.
*   275	Sem remessas para os criterios utilizados!
    MESSAGE s275 DISPLAY LIKE 'E'.
    gv_exit = 'X'.
    EXIT.
  ENDIF.
* RENUPG00053 <- Carlos Carloto - ROFF 18.11.2020

  LOOP AT gt_t311a.
    READ TABLE gt_likp WITH KEY vbeln = gt_t311a-rbnum.
    IF sy-subrc NE 0.
      DELETE gt_t311a WHERE refnr = gt_t311a-refnr.
      DELETE gt_t311  WHERE refnr = gt_t311a-refnr.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  PERFORM progresso USING '30'
                          text-012.
  SELECT * INTO TABLE gt_vbak FROM vbak
        FOR ALL ENTRIES IN gt_lips
        WHERE vbeln EQ gt_lips-vgbel
    ORDER BY PRIMARY KEY.
  IF sy-subrc NE 0 OR gt_vbak[] IS INITIAL.
*   Verificar se são provenientes de pedido
    SELECT * INTO TABLE gt_ekko FROM ekko
             FOR ALL ENTRIES IN gt_lips
             WHERE ebeln EQ gt_lips-vgbel
      ORDER BY PRIMARY KEY.
    IF sy-subrc NE 0.
*   276	Sem ordens de cliente para os criterios utilizados!
      MESSAGE s276 DISPLAY LIKE 'E'.
      gv_exit = 'X'.
      EXIT.
    ENDIF.
  ELSE.
* Verificar se existem remessas provenientes de pedido
    SELECT * INTO TABLE gt_ekko FROM ekko
             FOR ALL ENTRIES IN gt_lips
             WHERE ebeln EQ gt_lips-vgbel
      ORDER BY PRIMARY KEY.
  ENDIF.



  PERFORM progresso USING '35'
                          text-013.

  IF NOT gt_vbak[] IS INITIAL.
    SELECT * INTO TABLE gt_vbap FROM vbap
          FOR ALL ENTRIES IN gt_vbak
          WHERE vbeln EQ gt_vbak-vbeln
      ORDER BY PRIMARY KEY.
*          AND abgru NE '20'.
    IF sy-subrc NE 0 OR gt_vbap[] IS INITIAL.
*   276	Sem ordens de cliente para os criterios utilizados!
      MESSAGE s276 DISPLAY LIKE 'E'.
      gv_exit = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

  IF NOT gt_ekko[] IS INITIAL.
    SELECT * INTO TABLE gt_ekpo FROM ekpo
        FOR ALL ENTRIES IN gt_ekko
        WHERE ebeln EQ gt_ekko-ebeln
      ORDER BY PRIMARY KEY.
  ENDIF.

  PERFORM progresso USING '40'
                          text-014.

  LOOP AT gt_lips WHERE lfimg GT 0.
    IF NOT gt_lips-vgbel IS INITIAL.
      r_vbeln-sign   = 'I'.
      r_vbeln-option = 'EQ'.
      r_vbeln-low    = gt_lips-vgbel.
      APPEND r_vbeln.
    ENDIF.
  ENDLOOP.

  IF NOT r_vbeln[] IS INITIAL.
    SELECT * INTO TABLE gt_vbfa FROM vbfa
      FOR ALL ENTRIES IN r_vbeln
              WHERE vbelv EQ r_vbeln-low
               AND vbtyp_n EQ 'J'.
    CLEAR: lt_vbfa_fal_aux.
    REFRESH: lt_vbfa_fal_aux.
    lt_vbfa_fal_aux[] = gt_vbfa[].
  ENDIF.

  IF NOT r_vbeln[] IS INITIAL.
    SELECT * INTO TABLE gt_ekbe FROM ekbe
      FOR ALL ENTRIES IN r_vbeln
              WHERE ebeln EQ r_vbeln-low
                AND vgabe EQ '8'.
    CLEAR: lt_ekbe_fal_aux.
    REFRESH: lt_ekbe_fal_aux.
    lt_ekbe_fal_aux[] = gt_ekbe[].
  ENDIF.

* Calculo 1
* Quantidades de diferença entre ordem e remessa
  LOOP AT gt_lips WHERE lfimg GT 0.
    CLEAR: lv_qtd_fornecida.
    READ TABLE gt_vbap WITH KEY vbeln = gt_lips-vgbel
                                posnr = gt_lips-vgpos.
    IF sy-subrc EQ 0.
      READ TABLE lt_diferencas WITH KEY ordem   = gt_vbap-vbeln
                                        item    = gt_lips-vgpos.
*                                        remessa = gt_lips-vbeln
*                                        matnr   = gt_lips-matnr.
*
      IF sy-subrc NE 0.
        CLEAR: lt_diferencas.
        lt_diferencas-ordem   = gt_vbap-vbeln.
        lt_diferencas-item    = gt_lips-vgpos.
        lt_diferencas-remessa = gt_lips-vbeln.
        lt_diferencas-matnr   = gt_lips-matnr.
        lt_diferencas-arktx   = gt_vbap-arktx.
        lt_diferencas-kwmeng  = gt_vbap-kwmeng.
        IF gt_vbap-abgru EQ '20' OR gt_vbap-abgru EQ '07' OR gt_vbap-abgru EQ '24'.
          lt_diferencas-item_fechado = 'X'.
        ENDIF.
        COLLECT lt_diferencas. CLEAR lt_diferencas.
        LOOP AT gt_lips INTO wa_lips WHERE vbeln EQ gt_lips-vbeln
                                       AND matnr EQ gt_lips-matnr
                                       AND vgbel EQ gt_lips-vgbel
                                       AND vgpos EQ gt_lips-vgpos
                                       AND lfimg GT 0.

          LOOP AT lt_vbfa_fal_aux WHERE vbelv EQ gt_vbap-vbeln
                                    AND posnv EQ gt_vbap-posnr.
            ADD lt_vbfa_fal_aux-rfmng TO lv_qtd_fornecida.
          ENDLOOP.
          DELETE lt_vbfa_fal_aux WHERE vbelv EQ gt_vbap-vbeln
                                    AND posnv EQ gt_vbap-posnr.

          lt_diferencas-ordem   = gt_vbap-vbeln.
          lt_diferencas-item    = gt_lips-vgpos.
          lt_diferencas-remessa = gt_lips-vbeln.
          lt_diferencas-matnr   = gt_lips-matnr.
          lt_diferencas-arktx   = gt_vbap-arktx.
*          lt_diferencas-lfimg   = wa_lips-lfimg.
          lt_diferencas-lfimg   = lv_qtd_fornecida.
          IF gt_vbap-abgru EQ '20' OR gt_vbap-abgru EQ '07' OR gt_vbap-abgru EQ '24'.
            lt_diferencas-item_fechado = 'X'.
          ENDIF.
          COLLECT lt_diferencas. CLEAR: lt_diferencas, lv_qtd_fornecida.
        ENDLOOP.
      ENDIF.
    ELSE.
*   Verificar se é Pedido
      READ TABLE gt_ekpo WITH KEY ebeln = gt_lips-vgbel
                                  ebelp = gt_lips-vgpos.
      IF sy-subrc EQ 0.
        READ TABLE lt_diferencas WITH KEY ordem   = gt_ekpo-ebeln
                                          item    = gt_lips-vgpos.
*                                        remessa = gt_lips-vbeln
*                                        matnr   = gt_lips-matnr.
*
        IF sy-subrc NE 0.
          CLEAR: lt_diferencas.
          lt_diferencas-ordem   = gt_ekpo-ebeln.
          lt_diferencas-item    = gt_lips-vgpos.
          lt_diferencas-remessa = gt_lips-vbeln.
          lt_diferencas-matnr   = gt_lips-matnr.
          lt_diferencas-arktx   = gt_ekpo-txz01.
          lt_diferencas-kwmeng  = gt_ekpo-menge.
          IF NOT gt_ekpo-loekz IS INITIAL OR NOT gt_ekpo-elikz IS INITIAL.
            lt_diferencas-item_fechado = 'X'.
          ENDIF.
          COLLECT lt_diferencas. CLEAR lt_diferencas.
          LOOP AT gt_lips INTO wa_lips WHERE vbeln EQ gt_lips-vbeln
                                         AND matnr EQ gt_lips-matnr
                                         AND vgbel EQ gt_lips-vgbel
                                         AND vgpos EQ gt_lips-vgpos
                                         AND lfimg GT 0.

            LOOP AT lt_ekbe_fal_aux WHERE ebeln EQ gt_ekpo-ebeln
                                      AND ebelp EQ gt_ekpo-ebelp.
              ADD lt_ekbe_fal_aux-menge TO lv_qtd_fornecida.
            ENDLOOP.
            DELETE lt_ekbe_fal_aux WHERE ebeln EQ gt_ekpo-ebeln
                                      AND ebelp EQ gt_ekpo-ebelp.

            lt_diferencas-ordem   = gt_ekpo-ebeln.
            lt_diferencas-item    = gt_lips-vgpos.
            lt_diferencas-remessa = gt_lips-vbeln.
            lt_diferencas-matnr   = gt_lips-matnr.
            lt_diferencas-arktx   = gt_ekpo-txz01.
*          lt_diferencas-lfimg   = wa_lips-lfimg.
            lt_diferencas-lfimg   = lv_qtd_fornecida.
            IF NOT gt_ekpo-loekz IS INITIAL OR NOT gt_ekpo-elikz IS INITIAL.
              lt_diferencas-item_fechado = 'X'.
            ENDIF.
            COLLECT lt_diferencas. CLEAR: lt_diferencas, lv_qtd_fornecida.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM progresso USING '45'
                          text-015.
* Calculo 2
* Faltas Totais. Material/Qtd que esta na ordem mas nao esta na remessa

  CLEAR: lv_qtd_fornecida.
  LOOP AT gt_vbap.
    CLEAR: lv_qtd_fornecida.
    LOOP AT gt_vbfa WHERE vbelv EQ gt_vbap-vbeln
                      AND posnv EQ gt_vbap-posnr.
      ADD gt_vbfa-rfmng TO lv_qtd_fornecida.
    ENDLOOP.
    IF lv_qtd_fornecida EQ 0 OR
       lv_qtd_fornecida < gt_vbap-kwmeng. "DS: Items não fornecidos

      READ TABLE lt_diferencas
        WITH KEY ordem = gt_vbap-vbeln
                 item  = gt_vbap-posnr.

      CHECK sy-subrc <> 0.

      gt_faltas-ordem = gt_vbap-vbeln.
      gt_faltas-item  = gt_vbap-posnr.
      gt_faltas-werks = gt_vbap-werks.
      gt_faltas-matnr = gt_vbap-matnr.
      gt_faltas-arktx = gt_vbap-arktx.

      IF gt_vbap-abgru EQ '20' OR gt_vbap-abgru EQ '07' OR gt_vbap-abgru EQ '24'.
        CONTINUE.
      ENDIF.
      gt_faltas-lfimg = gt_vbap-kwmeng - lv_qtd_fornecida.
      COLLECT gt_faltas. CLEAR: gt_faltas, lv_qtd_fornecida.
    ELSEIF lv_qtd_fornecida EQ gt_vbap-kwmeng.
      gt_faltas-ordem = gt_vbap-vbeln.
      gt_faltas-item  = gt_vbap-posnr.
      gt_faltas-werks = gt_vbap-werks.
      gt_faltas-matnr = gt_vbap-matnr.
      gt_faltas-arktx = gt_vbap-arktx.
      gt_faltas-finalizada = 'X'.
      COLLECT gt_faltas. CLEAR: gt_faltas, lv_qtd_fornecida.
    ENDIF.
  ENDLOOP.

  CLEAR: lv_qtd_fornecida.
  LOOP AT gt_ekpo.
    CLEAR: lv_qtd_fornecida.
    LOOP AT gt_ekbe WHERE ebeln EQ gt_ekpo-ebeln
                      AND ebelp EQ gt_ekpo-ebelp.
      ADD gt_ekbe-menge TO lv_qtd_fornecida.
    ENDLOOP.
    IF lv_qtd_fornecida EQ 0.
      gt_faltas-ordem = gt_ekpo-ebeln.
      gt_faltas-item  = gt_ekpo-ebelp.
      gt_faltas-werks = gt_ekpo-werks.

      gt_faltas-matnr = gt_ekpo-matnr.
      gt_faltas-arktx = gt_ekpo-txz01.

      IF NOT gt_ekpo-loekz IS INITIAL OR NOT gt_ekpo-elikz IS INITIAL.
        CONTINUE.
      ENDIF.
      gt_faltas-lfimg = gt_ekpo-menge - lv_qtd_fornecida.
      COLLECT gt_faltas. CLEAR: gt_faltas, lv_qtd_fornecida.

    ELSEIF lv_qtd_fornecida EQ gt_ekpo-menge.
      gt_faltas-ordem = gt_ekpo-ebeln.
      gt_faltas-item  = gt_ekpo-ebelp.
      gt_faltas-werks = gt_ekpo-werks.

      gt_faltas-matnr = gt_ekpo-matnr.
      gt_faltas-arktx = gt_ekpo-txz01.
      gt_faltas-finalizada = 'X'.
      COLLECT gt_faltas. CLEAR: gt_faltas, lv_qtd_fornecida.
    ENDIF.
  ENDLOOP.

* Apagar
  LOOP AT gt_faltas WHERE finalizada = 'X'.
    DELETE lt_diferencas WHERE ordem EQ gt_faltas-ordem
                           AND item  EQ gt_faltas-item
                           AND matnr EQ gt_faltas-matnr.
  ENDLOOP.

  PERFORM progresso USING '50'
                          text-006.


* Construir a tabela Final
*  SORT gt_t311a BY refnr.
  CLEAR: lv_last_refnr.

  LOOP AT gt_t311a.
    CLEAR: lt_paletes, vbpa. REFRESH: lt_paletes.

    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = gt_t311a-lgnum
        i_refnr  = gt_t311a-refnr
        i_vbeln  = gt_t311a-rbnum
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.


    gt_dados_ant-refnr = gt_t311a-refnr.
    gt_dados_ant-vbeln = gt_t311a-rbnum.

    READ TABLE gt_t311 WITH KEY refnr = gt_t311a-refnr.
    IF sy-subrc EQ 0.
      gt_dados_ant-refnt = gt_t311-refnt.
    ENDIF.

    READ TABLE gt_vttp WITH KEY vbeln = gt_dados_ant-vbeln.
    IF sy-subrc EQ 0.
      gt_dados_ant-tknum = gt_vttp-tknum.
    ENDIF.

    READ TABLE gt_vttk WITH KEY tknum = gt_vttp-tknum.
    IF sy-subrc EQ 0.
      gt_dados_ant-clientes = gt_vttk-add01.
      gt_dados_ant-dtdis    = gt_vttk-dtdis.
      gt_dados_ant-observ   = gt_vttk-exti1.
      gt_dados_ant-signi    = gt_vttk-signi.

      SELECT SINGLE bezei INTO gt_dados_ant-tp_carro FROM tvsakt
             WHERE spras EQ sy-langu
               AND sdabw EQ gt_vttk-sdabw.
    ENDIF.

    READ TABLE gt_likp WITH KEY vbeln = gt_dados_ant-vbeln.
    IF sy-subrc EQ 0.
      gt_dados_ant-kunnr     = gt_likp-kunnr.
      gt_dados_ant-vkorg     = gt_likp-vkorg.
      gt_dados_ant-peso      = gt_likp-btgew.
      gt_dados_ant-btgew      = gt_likp-btgew.
      gt_dados_ant-un_peso   = gt_likp-gewei.
      gt_dados_ant-gewei   = gt_likp-gewei.
      gt_dados_ant-volume    = gt_likp-volum.
      gt_dados_ant-volum    = gt_likp-volum.
      gt_dados_ant-un_volume = gt_likp-voleh.
      gt_dados_ant-voleh = gt_likp-voleh.
    ENDIF.

    SELECT SINGLE adrnr INTO vbpa-adrnr FROM vbpa
           WHERE vbeln EQ gt_dados_ant-vbeln
             AND parvw = 'WE'.
    IF sy-subrc EQ 0.
      CLEAR adrc.
      SELECT SINGLE * FROM adrc WHERE addrnumber = vbpa-adrnr.
      gt_dados_ant-name1_rm1 = adrc-name1.
      gt_dados_ant-city1 = adrc-city1.
      gt_dados_ant-post_code1 = adrc-post_code1.

      READ TABLE gt_vttk WITH KEY tknum = gt_dados_ant-tknum.
      IF sy-subrc EQ 0.
        SELECT SINGLE name1 INTO gt_dados_ant-trans FROM lfa1
               WHERE lifnr EQ gt_vttk-tdlnr.

        SELECT SINGLE trfzn INTO gt_dados_ant-trfzn FROM tvfptz
               WHERE tplst = gt_vttk-tplst
                 AND vsart = '01'
                 AND land1 = adrc-country
                 AND fpstlz <= adrc-post_code1
                 AND tpstlz >= adrc-post_code1.
        IF sy-subrc EQ 0.
          SELECT SINGLE bezei INTO gt_dados_ant-bezei
                 FROM tvftzt
                 WHERE trfzn EQ gt_dados_ant-trfzn.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR vbpa.
    SELECT SINGLE adrnr INTO vbpa-adrnr FROM vbpa
       WHERE vbeln EQ gt_dados_ant-vbeln
         AND parvw = 'W1'.
    IF sy-subrc EQ 0.
      SELECT SINGLE * FROM adrc WHERE addrnumber = vbpa-adrnr.
      gt_dados_ant-name1_rm2 = adrc-name1.
    ENDIF.
    READ TABLE gt_kna1 WITH KEY kunnr = gt_likp-kunnr.
    IF sy-subrc EQ 0.
      gt_dados_ant-land1 = gt_kna1-land1.
    ENDIF.

*   Calcular Paletes
*    LOOP AT gt_lips WHERE vbeln EQ gt_dados_ant-vbeln.
*      MOVE-CORRESPONDING gt_lips TO lt_paletes.
*      lt_paletes-refnr = gt_t311a-refnr.
*
*      APPEND lt_paletes.
*    ENDLOOP.
*
*
*    CALL FUNCTION 'ZWM_PAL_PICKING'
*      EXPORTING
*        armazem         = gt_t311a-lgnum
*        actualiza       = ''
*      TABLES
*        zpalete_picking = lt_paletes.
*
*    LOOP AT lt_paletes.
*      ADD lt_paletes-pal_completa   TO gt_dados_ant-palcp.
*      ADD lt_paletes-pal_incompleta TO gt_dados_ant-palpk.
*    ENDLOOP.


*   concatenar os Produtos em Falta
    CLEAR: lv_qtd, lv_valor.

    READ TABLE gt_lips WITH KEY vbeln = gt_dados_ant-vbeln.
    IF sy-subrc EQ 0.

      LOOP AT gt_faltas WHERE ordem = gt_lips-vgbel AND
                              werks = gt_lips-werks.

        IF gt_faltas-finalizada EQ 'X'.
          CONTINUE.
        ENDIF.

        MOVE gt_faltas-lfimg TO lv_qtd.
        MOVE lv_qtd TO lv_valor.

        CLEAR mlgn.
        SELECT SINGLE * FROM mlgn WHERE matnr EQ gt_faltas-matnr.
        IF mlgn-lhmg1 IS NOT INITIAL.
          CLEAR: lv_rest.

          lv_qtd_falta_completas = gt_faltas-lfimg / mlgn-lhmg1.

*          IF mlgn-lety1 EQ 'P2' OR mlgn-lety1 EQ 'P5'.
*          IF z_wm_cl_management=>is_remontada( is_data = mlgn ) EQ abap_true.
          IF mlgn-lety1 IN z_wm_cl_management=>r_letyp_remontada( mlgn-lgnum ).

            lv_qtd_falta_completas = lv_qtd_falta_completas / 2.
            lv_rest = lv_qtd_falta_completas MOD 2.
*            IF lv_rest GT 0.
*              ADD 1 TO lv_qtd_falta_completas.
*            ENDIF.
          ENDIF.

          lv_qtd_falta_picking = gt_faltas-lfimg MOD mlgn-lhmg1.
          IF lv_qtd_falta_picking GT 0.
            lv_remessa_com_picking = 'X'.
          ENDIF.
        ENDIF.

        IF gt_dados_ant-pfalt IS INITIAL.
          CONCATENATE lv_valor '-' gt_faltas-arktx
                      INTO gt_dados_ant-pfalt
                      SEPARATED BY space.
        ELSE.
          CONCATENATE gt_dados_ant-pfalt '; ' lv_valor '-'
                      gt_faltas-arktx
                      INTO gt_dados_ant-pfalt
                      SEPARATED BY space.
        ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 24.09.2012 17:24:28
*  Motivo: Adiciona Remessas
*--------------------------------------------------------------------*
*        IF lv_2step EQ abap_true.
*          CONCATENATE gt_dados_ant-pfalt
*                      ' ('
*                      gt_dados_ant-vbeln
*                      ')'
*                      INTO gt_dados_ant-pfalt.
*        ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*        23-02-2010
*        gt_dados_ant-palcp = gt_dados_ant-palcp + lv_qtd_falta_completas.
        CLEAR: lv_qtd_falta_completas.
      ENDLOOP.
    ENDIF.

* Diferencças
    LOOP AT lt_diferencas WHERE remessa EQ gt_dados_ant-vbeln.
      CLEAR: lv_qtd, lv_valor.
      IF lt_diferencas-item_fechado EQ 'X'.
        lv_qtd = 0.
      ELSE.
        lv_qtd = lt_diferencas-kwmeng - lt_diferencas-lfimg.
      ENDIF.
      IF lv_qtd GT 0.
        CLEAR mlgn.
        SELECT SINGLE * FROM mlgn WHERE matnr EQ lt_diferencas-matnr.
        IF mlgn-lhmg1 IS NOT INITIAL.
          CLEAR: lv_rest.
          lv_qtd_falta_completas = lv_qtd / mlgn-lhmg1.

*          IF mlgn-lety1 EQ 'P2' OR mlgn-lety1 EQ 'P5'.
*          IF z_wm_cl_management=>is_remontada( is_data = mlgn ) EQ abap_true.
          IF mlgn-lety1 IN z_wm_cl_management=>r_letyp_remontada( mlgn-lgnum ).
            lv_qtd_falta_completas = lv_qtd_falta_completas / 2.
            lv_rest = lv_qtd_falta_completas MOD 2.
*            IF lv_rest GT 0.
*              ADD 1 TO lv_qtd_falta_completas.
*            ENDIF.
          ENDIF.
          lv_qtd_falta_picking = lv_qtd MOD mlgn-lhmg1.
          IF lv_qtd_falta_picking GT 0.
            lv_remessa_com_picking = 'X'.
          ENDIF.
        ENDIF.
        MOVE lv_qtd TO lv_valor.
        IF gt_dados_ant-pfalt IS INITIAL.
          CONCATENATE lv_valor '-' lt_diferencas-arktx
                      INTO gt_dados_ant-pfalt
                      SEPARATED BY space.
        ELSE.
          CONCATENATE gt_dados_ant-pfalt '; ' lv_valor '-'
                      lt_diferencas-arktx
                      INTO gt_dados_ant-pfalt
                      SEPARATED BY space.
        ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 24.09.2012 17:24:28
*  Motivo: Adiciona Remessas
*--------------------------------------------------------------------*
*        IF lv_2step EQ abap_true.
*          CONCATENATE gt_dados_ant-pfalt
*                      ' ('
*                      gt_dados_ant-vbeln
*                      ')'
*                      INTO gt_dados_ant-pfalt.
*        ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

      ENDIF.
*     23-02-2010
*      gt_dados_ant-palcp = gt_dados_ant-palcp + lv_qtd_falta_completas.
      CLEAR: lv_qtd_falta_completas.
    ENDLOOP.
    IF lv_remessa_com_picking = 'X'.
*     23-02-2010
*      ADD 1 TO gt_dados_ant-palpk.
      CLEAR: lv_remessa_com_picking.
    ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 26.09.2012 10:57:12
*  Motivo: Linha de Cabeçalho em 2 passos
*--------------------------------------------------------------------*
    CLEAR: ls_dados_header.
    IF lv_2step EQ abap_true AND
       lv_2spart IS INITIAL.

      IF lv_last_refnr IS INITIAL OR
         lv_last_refnr <> gt_t311a-refnr.

        lv_last_refnr = gt_t311a-refnr.

        ls_dados_header-vkorg    = gt_dados_ant-vkorg.
        ls_dados_header-refnr    = gt_dados_ant-refnr.
        ls_dados_header-tknum    = gt_dados_ant-tknum.
        ls_dados_header-trans    = gt_dados_ant-trans.
        ls_dados_header-tp_carro = gt_dados_ant-tp_carro.
        APPEND ls_dados_header TO gt_dados_ant.
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*   23-02-2010
*    gt_dados_ant-palto = gt_dados_ant-palcp + gt_dados_ant-palpk.
    APPEND gt_dados_ant. CLEAR gt_dados_ant.
  ENDLOOP.

* Tabelas auxiliares para diferenças e faltas.
  gt_faltas_aux[]     = gt_faltas[].
  lt_diferencas_aux[] = lt_diferencas[].

  LOOP AT gt_faltas WHERE finalizada IS INITIAL.
    r_ordem-sign = 'I'.
    r_ordem-option = 'EQ'.
    r_ordem-low = gt_faltas-ordem.
    APPEND r_ordem.
  ENDLOOP.

  CLEAR: lt_vbfa_faltas. REFRESH: lt_vbfa_faltas.

  IF NOT r_ordem[] IS INITIAL.
    SELECT * INTO TABLE lt_vbfa_faltas FROM vbfa
             FOR ALL ENTRIES IN r_ordem
             WHERE vbelv EQ r_ordem-low
               AND vbtyp_n EQ 'J'.
  ENDIF.

  LOOP AT gt_t311.
    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = gt_t311-lgnum
        i_refnr  = gt_t311-refnr
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.


    LOOP AT gt_t311a WHERE refnr EQ gt_t311-refnr.

      LOOP AT gt_lips WHERE vbeln EQ gt_t311a-rbnum.
        READ TABLE gt_likp WITH KEY vbeln = gt_lips-vbeln.
        IF sy-subrc EQ 0.
          lt_paletes-kunnr = gt_likp-kunnr.
          lt_paletes-kunag = gt_likp-kunag.
        ENDIF.
        MOVE-CORRESPONDING gt_lips TO lt_paletes.
        lt_paletes-refnr = gt_t311a-refnr.
        lt_paletes-sammg = gt_t311a-refnr.

**      Adicionar as Diferenças para o calculo de paletes
        LOOP AT lt_diferencas_aux WHERE remessa EQ gt_t311a-rbnum
                                    AND matnr   EQ gt_lips-matnr
                                    AND item_fechado IS INITIAL.
          lv_idx_dif = sy-tabix.
          lt_paletes-lfimg =
                           lt_paletes-lfimg +
               ( lt_diferencas_aux-kwmeng - lt_diferencas_aux-lfimg ).
          DELETE lt_diferencas_aux INDEX lv_idx_dif.
          CONTINUE.
        ENDLOOP.

        APPEND lt_paletes. CLEAR: lt_paletes.
      ENDLOOP.
**      Adicionar as Faltas para o calculo de paletes
      READ TABLE lt_vbfa_faltas WITH KEY vbeln = gt_t311a-rbnum.
      IF sy-subrc EQ 0.
        LOOP AT gt_faltas_aux WHERE ordem EQ lt_vbfa_faltas-vbelv
                                AND finalizada IS INITIAL.
          lv_idx_fal = sy-tabix.
*         Criar novas linhas.
          CLEAR: wa_paletes.
          READ TABLE lt_paletes INTO wa_paletes
                                WITH KEY vbeln = gt_t311a-rbnum.
          IF sy-subrc EQ 0.
            READ TABLE gt_vbap WITH KEY vbeln = gt_faltas_aux-ordem
                                        matnr = gt_faltas_aux-matnr.
            IF sy-subrc EQ 0.
              CLEAR: wa_paletes-matnr, wa_paletes-charg,
                     wa_paletes-lfimg, wa_paletes-meins,
                     wa_paletes-vrkme, wa_paletes-voleh,
                     wa_paletes-volum.

              wa_paletes-matnr = gt_vbap-matnr.
              wa_paletes-lfimg = gt_faltas_aux-lfimg.
              wa_paletes-meins = gt_vbap-meins.
              wa_paletes-vrkme = gt_vbap-vrkme.
              wa_paletes-voleh = gt_vbap-voleh.
              wa_paletes-volum = gt_vbap-volum.
              wa_paletes-nodel = abap_true.

              APPEND wa_paletes TO lt_paletes.

              DELETE gt_faltas_aux INDEX lv_idx_fal.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.


    ENDLOOP.

    CALL FUNCTION 'ZWM_PAL_PICKING'
      EXPORTING
        armazem         = gt_t311-lgnum
        actualiza       = ''
        2step_red       = ''
      TABLES
        zpalete_picking = lt_paletes.

    LOOP AT lt_paletes.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 13.07.2012 11:07:29
*  Motivo: Picking em 2 passos
*--------------------------------------------------------------------*
      lv_tabix = sy-tabix.

      IF lv_2step EQ abap_true AND
         lv_2spart EQ abap_false.

*        CLEAR: lv_count.
*        LOOP AT gt_dados_ant WHERE refnr = lt_paletes-refnr.
*          lv_count = lv_count + 1.
*
*          IF lv_count > 1.
*
*            IF NOT gt_dados_ant-pfalt IS INITIAL.
*              CONCATENATE <ls_dados>-pfalt
*                          ';'
*                          gt_dados_ant-pfalt
*                     INTO <ls_dados>-pfalt
*                     SEPARATED BY space.
*            ENDIF.
*
*            DELETE gt_dados_ant INDEX sy-tabix.
*          ELSE.
*            READ TABLE gt_dados_ant
*             ASSIGNING <ls_dados>
*             INDEX sy-tabix.
*
*            <ls_dados>-vbeln = ''.
*          ENDIF.
*        ENDLOOP.

        READ TABLE gt_dados_ant WITH KEY refnr = lt_paletes-refnr.
      ELSE.
        READ TABLE gt_dados_ant WITH KEY refnr = lt_paletes-refnr
                                     vbeln = lt_paletes-vbeln.
      ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

      IF sy-subrc EQ 0.
        DATA(lv_tabix_aux) = sy-tabix.
        SELECT SINGLE * FROM mlgn WHERE matnr EQ lt_paletes-matnr.
        IF mlgn-lhmg1 IS NOT INITIAL.
          CLEAR: lv_rest.
*          IF mlgn-lety1 EQ 'P2' OR mlgn-lety1 EQ 'P5'.
*          IF z_wm_cl_management=>is_remontada( is_data = mlgn ) EQ abap_true.
          IF mlgn-lety1 IN z_wm_cl_management=>r_letyp_remontada( mlgn-lgnum ).
            lt_paletes-pal_completa = lt_paletes-pal_completa / 2.
            lv_rest = lt_paletes-pal_completa MOD 2.
*            IF lv_rest GT 0.
*              ADD 1 TO lt_paletes-pal_completa.
*            ENDIF.
          ENDIF.
        ENDIF.
        ADD lt_paletes-pal_completa   TO gt_dados_ant-palcp.
        ADD lt_paletes-pal_picking TO gt_dados_ant-palpk.

        gt_dados_ant-palto = gt_dados_ant-palcp + gt_dados_ant-palpk.

        gt_dados_ant-palpk_s = ceil( gt_dados_ant-palpk ).

        gt_dados_ant-palto_s = gt_dados_ant-palcp + gt_dados_ant-palpk_s.

        MODIFY gt_dados_ant INDEX lv_tabix_aux. "sy-tabix.
      ENDIF.
    ENDLOOP.
    CLEAR: lt_paletes. REFRESH: lt_paletes.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PROGRESSO
*&---------------------------------------------------------------------*
FORM progresso  USING    VALUE(p_0031)
                         p_text_003.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_0031
      text       = p_text_003.

ENDFORM.                    " PROGRESSO
