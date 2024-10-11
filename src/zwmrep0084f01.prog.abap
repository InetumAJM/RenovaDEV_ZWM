*&---------------------------------------------------------------------*
*&  Include           ZWMREP0084F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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

  DATA: BEGIN OF lt_faltas OCCURS 0,
          ordem         TYPE vbeln_va,
          item          TYPE vgpos,
          matnr         TYPE matnr,
          arktx         TYPE arktx,
          lfimg         TYPE lfimg,
          werks         TYPE werks_d,
          finalizada(1),
        END OF lt_faltas.

  DATA:
    lt_diferencas_aux LIKE lt_diferencas OCCURS 0 WITH HEADER LINE,
    lt_faltas_aux     LIKE lt_faltas     OCCURS 0 WITH HEADER LINE,
    lt_vbfa_faltas    LIKE vbfa OCCURS 0 WITH HEADER LINE,
    lt_vbfa_fal_aux   LIKE vbfa OCCURS 0 WITH HEADER LINE,
    lt_ekbe_fal_aux   LIKE ekbe OCCURS 0 WITH HEADER LINE,
    wa_lips           LIKE lips,
    lt_paletes        LIKE zpalete_picking OCCURS 0 WITH HEADER LINE,
    wa_paletes        LIKE lt_paletes.

  DATA: lv_last_refnr TYPE lvs_refnr.

  FIELD-SYMBOLS: <ls_dados> LIKE gt_dados.

  RANGES: r_ordem FOR vbak-vbeln,
          r_vbeln FOR vbfa-vbelv.



  CLEAR:   gt_t311, gt_t311a, gt_vttk, gt_vttp, gt_likp,
           gt_lips, gt_kna1,  gt_vbak, gt_vbap, gt_vbfa,
           gt_dados, gt_header, gt_items, lt_paletes, lt_diferencas,
           wa_lips, lt_faltas, r_vbeln.

  REFRESH: gt_t311, gt_t311a, gt_vttk, gt_vttp, gt_likp,
           gt_lips, gt_kna1,  gt_vbak, gt_vbap, gt_vbfa,
           gt_dados, gt_header, gt_items, lt_paletes, lt_diferencas,
           lt_faltas, r_vbeln.



  PERFORM progresso USING '5'
                          text-007.

  SELECT * INTO TABLE gt_t311 FROM t311
           WHERE lgnum EQ p_lgnum
             AND refnr IN s_refnr
             AND refnt IN s_refnt
             AND datum IN s_datum.
  IF sy-subrc NE 0 OR gt_t311[] IS INITIAL.
*   274  Sem dados para processar para os criterios utilizados!
    MESSAGE s274 DISPLAY LIKE 'E'.
    gv_exit = 'X'.
    EXIT.
  ENDIF.

  SELECT * INTO TABLE gt_t311a FROM t311a
           FOR ALL ENTRIES IN gt_t311
           WHERE lgnum EQ gt_t311-lgnum
             AND refnr EQ gt_t311-refnr.
  IF sy-subrc NE 0 OR gt_t311a[] IS INITIAL.
*   274  Sem dados para processar para os criterios utilizados!
    MESSAGE s274 DISPLAY LIKE 'E'.
    gv_exit = 'X'.
    EXIT.
  ENDIF.

  PERFORM progresso USING '10'
                          text-008.

  SELECT * INTO TABLE gt_likp FROM likp
           FOR ALL ENTRIES IN gt_t311a
           WHERE vbeln EQ gt_t311a-rbnum
             AND vkorg IN s_vkorg
             AND vstel IN s_vstel.
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
           WHERE kunnr EQ gt_likp-kunnr.

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
           WHERE vbeln EQ gt_likp-vbeln.


  SELECT * INTO TABLE gt_vttk FROM vttk
           FOR ALL ENTRIES IN gt_vttp
           WHERE tknum EQ gt_vttp-tknum
             AND sdabw IN s_sdabw
             AND tdlnr IN s_tdlnr
             AND dtdis IN s_dtdis.

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
        WHERE vbeln EQ gt_lips-vgbel.
  IF sy-subrc NE 0 OR gt_vbak[] IS INITIAL.
*   Verificar se são provenientes de pedido
    SELECT * INTO TABLE gt_ekko FROM ekko
             FOR ALL ENTRIES IN gt_lips
             WHERE ebeln EQ gt_lips-vgbel.
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
             WHERE ebeln EQ gt_lips-vgbel.
  ENDIF.



  PERFORM progresso USING '35'
                          text-013.

  IF NOT gt_vbak[] IS INITIAL.
    SELECT * INTO TABLE gt_vbap FROM vbap
          FOR ALL ENTRIES IN gt_vbak
          WHERE vbeln EQ gt_vbak-vbeln.
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
        WHERE ebeln EQ gt_ekko-ebeln.
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

      lt_faltas-ordem = gt_vbap-vbeln.
      lt_faltas-item  = gt_vbap-posnr.
      lt_faltas-werks = gt_vbap-werks.
      lt_faltas-matnr = gt_vbap-matnr.
      lt_faltas-arktx = gt_vbap-arktx.

      IF gt_vbap-abgru EQ '20' OR gt_vbap-abgru EQ '07' OR gt_vbap-abgru EQ '24'.
        CONTINUE.
      ENDIF.
      lt_faltas-lfimg = gt_vbap-kwmeng - lv_qtd_fornecida.
      COLLECT lt_faltas. CLEAR: lt_faltas, lv_qtd_fornecida.
    ELSEIF lv_qtd_fornecida EQ gt_vbap-kwmeng.
      lt_faltas-ordem = gt_vbap-vbeln.
      lt_faltas-item  = gt_vbap-posnr.
      lt_faltas-werks = gt_vbap-werks.
      lt_faltas-matnr = gt_vbap-matnr.
      lt_faltas-arktx = gt_vbap-arktx.
      lt_faltas-finalizada = 'X'.
      COLLECT lt_faltas. CLEAR: lt_faltas, lv_qtd_fornecida.
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
      lt_faltas-ordem = gt_ekpo-ebeln.
      lt_faltas-item  = gt_ekpo-ebelp.
      lt_faltas-werks = gt_ekpo-werks.

      lt_faltas-matnr = gt_ekpo-matnr.
      lt_faltas-arktx = gt_ekpo-txz01.

      IF NOT gt_ekpo-loekz IS INITIAL OR NOT gt_ekpo-elikz IS INITIAL.
        CONTINUE.
      ENDIF.
      lt_faltas-lfimg = gt_ekpo-menge - lv_qtd_fornecida.
      COLLECT lt_faltas. CLEAR: lt_faltas, lv_qtd_fornecida.

    ELSEIF lv_qtd_fornecida EQ gt_ekpo-menge.
      lt_faltas-ordem = gt_ekpo-ebeln.
      lt_faltas-item  = gt_ekpo-ebelp.
      lt_faltas-werks = gt_ekpo-werks.

      lt_faltas-matnr = gt_ekpo-matnr.
      lt_faltas-arktx = gt_ekpo-txz01.
      lt_faltas-finalizada = 'X'.
      COLLECT lt_faltas. CLEAR: lt_faltas, lv_qtd_fornecida.
    ENDIF.
  ENDLOOP.

* Apagar
  LOOP AT lt_faltas WHERE finalizada = 'X'.
    DELETE lt_diferencas WHERE ordem EQ lt_faltas-ordem
                           AND item  EQ lt_faltas-item
                           AND matnr EQ lt_faltas-matnr.
  ENDLOOP.

  PERFORM progresso USING '50'
                          text-006.


* Construir a tabela Final
  SORT gt_t311a BY refnr.
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


    gt_dados-refnr = gt_t311a-refnr.
    gt_dados-vbeln = gt_t311a-rbnum.

    READ TABLE gt_t311 WITH KEY refnr = gt_t311a-refnr.
    IF sy-subrc EQ 0.
      gt_dados-refnt = gt_t311-refnt.
    ENDIF.

    READ TABLE gt_vttp WITH KEY vbeln = gt_dados-vbeln.
    IF sy-subrc EQ 0.
      gt_dados-tknum = gt_vttp-tknum.
    ENDIF.

    READ TABLE gt_vttk WITH KEY tknum = gt_vttp-tknum.
    IF sy-subrc EQ 0.
      gt_dados-clientes = gt_vttk-add01.
      gt_dados-dtdis    = gt_vttk-dtdis.
      gt_dados-observ   = gt_vttk-exti1.
      gt_dados-signi    = gt_vttk-signi.

      SELECT SINGLE bezei INTO gt_dados-tp_carro FROM tvsakt
             WHERE spras EQ sy-langu
               AND sdabw EQ gt_vttk-sdabw.
    ENDIF.

    READ TABLE gt_likp WITH KEY vbeln = gt_dados-vbeln.
    IF sy-subrc EQ 0.
      gt_dados-kunnr     = gt_likp-kunnr.
      gt_dados-vkorg     = gt_likp-vkorg.
      gt_dados-peso      = gt_likp-btgew.
      gt_dados-un_peso   = gt_likp-gewei.
      gt_dados-volume    = gt_likp-volum.
      gt_dados-un_volume = gt_likp-voleh.
    ENDIF.

    SELECT SINGLE adrnr INTO vbpa-adrnr FROM vbpa
           WHERE vbeln EQ gt_dados-vbeln
             AND parvw = 'WE'.
    IF sy-subrc EQ 0.
      CLEAR adrc.
      SELECT SINGLE * FROM adrc WHERE addrnumber = vbpa-adrnr.
      gt_dados-name1_rm1 = adrc-name1.

      READ TABLE gt_vttk WITH KEY tknum = gt_dados-tknum.
      IF sy-subrc EQ 0.
        SELECT SINGLE name1 INTO gt_dados-trans FROM lfa1
               WHERE lifnr EQ gt_vttk-tdlnr.

        SELECT SINGLE trfzn INTO gt_dados-trfzn FROM tvfptz
               WHERE tplst = gt_vttk-tplst
                 AND vsart = '01'
                 AND land1 = adrc-country
                 AND fpstlz <= adrc-post_code1
                 AND tpstlz >= adrc-post_code1.
        IF sy-subrc EQ 0.
          SELECT SINGLE bezei INTO gt_dados-bezei
                 FROM tvftzt
                 WHERE trfzn EQ gt_dados-trfzn.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR vbpa.
    SELECT SINGLE adrnr INTO vbpa-adrnr FROM vbpa
       WHERE vbeln EQ gt_dados-vbeln
         AND parvw = 'W1'.
    IF sy-subrc EQ 0.
      SELECT SINGLE * FROM adrc WHERE addrnumber = vbpa-adrnr.
      gt_dados-name1_rm2 = adrc-name1.
    ENDIF.
    READ TABLE gt_kna1 WITH KEY kunnr = gt_likp-kunnr.
    IF sy-subrc EQ 0.
      gt_dados-land1 = gt_kna1-land1.
    ENDIF.

*   Calcular Paletes
*    LOOP AT gt_lips WHERE vbeln EQ gt_dados-vbeln.
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
*      ADD lt_paletes-pal_completa   TO gt_dados-palcp.
*      ADD lt_paletes-pal_incompleta TO gt_dados-palpk.
*    ENDLOOP.


*   concatenar os Produtos em Falta
    CLEAR: lv_qtd, lv_valor.

    READ TABLE gt_lips WITH KEY vbeln = gt_dados-vbeln.
    IF sy-subrc EQ 0.

      LOOP AT lt_faltas WHERE ordem = gt_lips-vgbel AND
                              werks = gt_lips-werks.

        IF lt_faltas-finalizada EQ 'X'.
          CONTINUE.
        ENDIF.

        MOVE lt_faltas-lfimg TO lv_qtd.
        MOVE lv_qtd TO lv_valor.

        CLEAR mlgn.
        SELECT SINGLE * FROM mlgn WHERE matnr EQ lt_faltas-matnr.
        IF mlgn-lhmg1 IS NOT INITIAL.
          CLEAR: lv_rest.

          lv_qtd_falta_completas = lt_faltas-lfimg / mlgn-lhmg1.

          IF z_wm_cl_management=>is_remontada( is_data = mlgn ) eq abap_true.
            lv_qtd_falta_completas = lv_qtd_falta_completas / 2.
            lv_rest = lv_qtd_falta_completas MOD 2.
*            IF lv_rest GT 0.
*              ADD 1 TO lv_qtd_falta_completas.
*            ENDIF.
          ENDIF.

          lv_qtd_falta_picking = lt_faltas-lfimg MOD mlgn-lhmg1.
          IF lv_qtd_falta_picking GT 0.
            lv_remessa_com_picking = 'X'.
          ENDIF.
        ENDIF.

        IF gt_dados-pfalt IS INITIAL.
          CONCATENATE lv_valor '-' lt_faltas-arktx
                      INTO gt_dados-pfalt
                      SEPARATED BY space.
        ELSE.
          CONCATENATE gt_dados-pfalt '; ' lv_valor '-'
                      lt_faltas-arktx
                      INTO gt_dados-pfalt
                      SEPARATED BY space.
        ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 24.09.2012 17:24:28
*  Motivo: Adiciona Remessas
*--------------------------------------------------------------------*
*        IF lv_2step EQ abap_true.
*          CONCATENATE gt_dados-pfalt
*                      ' ('
*                      gt_dados-vbeln
*                      ')'
*                      INTO gt_dados-pfalt.
*        ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*        23-02-2010
*        gt_dados-palcp = gt_dados-palcp + lv_qtd_falta_completas.
        CLEAR: lv_qtd_falta_completas.
      ENDLOOP.
    ENDIF.

* Diferencças
    LOOP AT lt_diferencas WHERE remessa EQ gt_dados-vbeln.
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

          IF z_wm_cl_management=>is_remontada( is_data = mlgn ) eq abap_true.
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
        IF gt_dados-pfalt IS INITIAL.
          CONCATENATE lv_valor '-' lt_diferencas-arktx
                      INTO gt_dados-pfalt
                      SEPARATED BY space.
        ELSE.
          CONCATENATE gt_dados-pfalt '; ' lv_valor '-'
                      lt_diferencas-arktx
                      INTO gt_dados-pfalt
                      SEPARATED BY space.
        ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 24.09.2012 17:24:28
*  Motivo: Adiciona Remessas
*--------------------------------------------------------------------*
*        IF lv_2step EQ abap_true.
*          CONCATENATE gt_dados-pfalt
*                      ' ('
*                      gt_dados-vbeln
*                      ')'
*                      INTO gt_dados-pfalt.
*        ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

      ENDIF.
*     23-02-2010
*      gt_dados-palcp = gt_dados-palcp + lv_qtd_falta_completas.
      CLEAR: lv_qtd_falta_completas.
    ENDLOOP.
    IF lv_remessa_com_picking = 'X'.
*     23-02-2010
*      ADD 1 TO gt_dados-palpk.
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

        ls_dados_header-vkorg    = gt_dados-vkorg.
        ls_dados_header-refnr    = gt_dados-refnr.
        ls_dados_header-tknum    = gt_dados-tknum.
        ls_dados_header-trans    = gt_dados-trans.
        ls_dados_header-tp_carro = gt_dados-tp_carro.
        APPEND ls_dados_header TO gt_dados.
      ENDIF.
    ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*   23-02-2010
*    gt_dados-palto = gt_dados-palcp + gt_dados-palpk.

"TFARIA 2023.06.27 ADICIONAR CAMPO QUE INFORMA SE O REGISTO ESTA NO PORTAL
  select single desbloquear into gt_dados-desbloquear from ztransp_h where tknum = gt_dados-tknum.
""""""""""""""""""""""""""""""""
    APPEND gt_dados. CLEAR gt_dados.
  ENDLOOP.

* Tabelas auxiliares para diferenças e faltas.
  lt_faltas_aux[]     = lt_faltas[].
  lt_diferencas_aux[] = lt_diferencas[].

  LOOP AT lt_faltas WHERE finalizada IS INITIAL.
    r_ordem-sign = 'I'.
    r_ordem-option = 'EQ'.
    r_ordem-low = lt_faltas-ordem.
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
        LOOP AT lt_faltas_aux WHERE ordem EQ lt_vbfa_faltas-vbelv
                                AND finalizada IS INITIAL.
          lv_idx_fal = sy-tabix.
*         Criar novas linhas.
          CLEAR: wa_paletes.
          READ TABLE lt_paletes INTO wa_paletes
                                WITH KEY vbeln = gt_t311a-rbnum.
          IF sy-subrc EQ 0.
            READ TABLE gt_vbap WITH KEY vbeln = lt_faltas_aux-ordem
                                        matnr = lt_faltas_aux-matnr.
            IF sy-subrc EQ 0.
              CLEAR: wa_paletes-matnr, wa_paletes-charg,
                     wa_paletes-lfimg, wa_paletes-meins,
                     wa_paletes-vrkme, wa_paletes-voleh,
                     wa_paletes-volum.

              wa_paletes-matnr = gt_vbap-matnr.
              wa_paletes-lfimg = lt_faltas_aux-lfimg.
              wa_paletes-meins = gt_vbap-meins.
              wa_paletes-vrkme = gt_vbap-vrkme.
              wa_paletes-voleh = gt_vbap-voleh.
              wa_paletes-volum = gt_vbap-volum.
              wa_paletes-nodel = abap_true.

              APPEND wa_paletes TO lt_paletes.

              DELETE lt_faltas_aux INDEX lv_idx_fal.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'ZWM_PAL_PICKING_COMPLETE'
      EXPORTING
        i_lgnum         = gt_t311-lgnum
      TABLES
        zpalete_picking = lt_paletes[].

**    CALL FUNCTION 'ZWM_PAL_PICKING'
**      EXPORTING
**        armazem         = gt_t311-lgnum
**        actualiza       = ''
**      TABLES
**        zpalete_picking = lt_paletes.

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
*        LOOP AT gt_dados WHERE refnr = lt_paletes-refnr.
*          lv_count = lv_count + 1.
*
*          IF lv_count > 1.
*
*            IF NOT gt_dados-pfalt IS INITIAL.
*              CONCATENATE <ls_dados>-pfalt
*                          ';'
*                          gt_dados-pfalt
*                     INTO <ls_dados>-pfalt
*                     SEPARATED BY space.
*            ENDIF.
*
*            DELETE gt_dados INDEX sy-tabix.
*          ELSE.
*            READ TABLE gt_dados
*             ASSIGNING <ls_dados>
*             INDEX sy-tabix.
*
*            <ls_dados>-vbeln = ''.
*          ENDIF.
*        ENDLOOP.

        READ TABLE gt_dados WITH KEY refnr = lt_paletes-refnr.
      ELSE.
        READ TABLE gt_dados WITH KEY refnr = lt_paletes-refnr
                                     vbeln = lt_paletes-vbeln.
      ENDIF.
      lv_tabix = sy-tabix.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


      IF sy-subrc EQ 0.
        SELECT SINGLE * FROM mlgn WHERE matnr EQ lt_paletes-matnr.
        IF mlgn-lhmg1 IS NOT INITIAL.
          CLEAR: lv_rest.
          IF z_wm_cl_management=>is_remontada( is_data = mlgn ) eq abap_true.
            lt_paletes-pal_completa = lt_paletes-pal_completa / 2.
            lv_rest = lt_paletes-pal_completa MOD 2.
*            IF lv_rest GT 0.
*              ADD 1 TO lt_paletes-pal_completa.
*            ENDIF.
          ENDIF.
        ENDIF.
        ADD lt_paletes-pal_completa   TO gt_dados-palcp.
        ADD lt_paletes-pal_picking TO gt_dados-palpk.

        gt_dados-palto = gt_dados-palcp + gt_dados-palpk.

        gt_dados-palpk_s = ceil( gt_dados-palpk ).

        gt_dados-palto_s = gt_dados-palcp + gt_dados-palpk_s.

*        MODIFY gt_dados INDEX sy-tabix.
        MODIFY gt_dados INDEX lv_tabix.
      ENDIF.
    ENDLOOP.
    CLEAR: lt_paletes. REFRESH: lt_paletes.
  ENDLOOP.


ENDFORM.                    " GET_DADOS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_dados .
  DATA: lv_tabix    TYPE sytabix,
        lv_contador TYPE i,
        wa_dados    LIKE gt_dados.


  CLEAR: wa_alv_fieldcat, wa_sort.  REFRESH: alv_fieldcat, alv_sort.

  wa_alv_fieldcat-key = 'X'.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'VKORG'.
  wa_alv_fieldcat-seltext_s = 'Org.V.'.
  wa_alv_fieldcat-seltext_m = 'Org. Vendas'.
  wa_alv_fieldcat-seltext_l = 'Organização de Vendas'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'TKNUM'.
  wa_alv_fieldcat-seltext_s = 'Transp.'.
  wa_alv_fieldcat-seltext_m = 'Transporte'.
  wa_alv_fieldcat-seltext_l = 'Transporte'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'NUMERO'.
  wa_alv_fieldcat-seltext_s = 'N.Carga'.
  wa_alv_fieldcat-seltext_m = 'Nr.Carga'.
  wa_alv_fieldcat-seltext_l = 'Nr.Carga'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'BEZEI'.
  wa_alv_fieldcat-seltext_s = 'Z. Tarifa'.
  wa_alv_fieldcat-seltext_m = 'Zona Tarifa'.
  wa_alv_fieldcat-seltext_l = 'Zona Tarifa'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'TRANS'.
  wa_alv_fieldcat-seltext_s = 'Transp.'.
  wa_alv_fieldcat-seltext_m = 'Transportador'.
  wa_alv_fieldcat-seltext_l = 'Transportador'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'SIGNI'.
  wa_alv_fieldcat-seltext_s = 'Matri.'.
  wa_alv_fieldcat-seltext_m = 'Matricula'.
  wa_alv_fieldcat-seltext_l = 'Matricula'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'CLIENTES'.
  wa_alv_fieldcat-seltext_s = 'N. Cli.'.
  wa_alv_fieldcat-seltext_m = 'Nr. Clientes'.
  wa_alv_fieldcat-seltext_l = 'Nr. Clientes'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'PESO'.
  wa_alv_fieldcat-seltext_s = 'Peso'.
  wa_alv_fieldcat-seltext_m = 'Peso'.
  wa_alv_fieldcat-seltext_l = 'Peso'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'UN_PESO'.
  wa_alv_fieldcat-seltext_s = 'Un.'.
  wa_alv_fieldcat-seltext_m = 'Un.'.
  wa_alv_fieldcat-seltext_l = 'Un.'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'VOLUME'.
  wa_alv_fieldcat-seltext_s = 'Volume'.
  wa_alv_fieldcat-seltext_m = 'Volume'.
  wa_alv_fieldcat-seltext_l = 'Volume'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'UN_VOLUME'.
  wa_alv_fieldcat-seltext_s = 'Un.'.
  wa_alv_fieldcat-seltext_m = 'Un.'.
  wa_alv_fieldcat-seltext_l = 'Un.'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'PAL'.
  wa_alv_fieldcat-seltext_s = 'Paletes'.
  wa_alv_fieldcat-seltext_m = 'Paletes'.
  wa_alv_fieldcat-seltext_l = 'Paletes'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'TP_CARRO'.
  wa_alv_fieldcat-seltext_s = 'Tp. Carro'.
  wa_alv_fieldcat-seltext_m = 'Tp. Carro'.
  wa_alv_fieldcat-seltext_l = 'Tp. Carro'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'DTDIS'.
  wa_alv_fieldcat-seltext_s = 'Dt.Entrega'.
  wa_alv_fieldcat-seltext_m = 'Dt.Entrega'.
  wa_alv_fieldcat-seltext_l = 'Dt.Entrega'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'OBSERV'.
  wa_alv_fieldcat-seltext_s = 'Observações'.
  wa_alv_fieldcat-seltext_m = 'Observações'.
  wa_alv_fieldcat-seltext_l = 'Observações'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = 'X'.
  wa_alv_fieldcat-tabname   = 'GT_HEADER'.
  wa_alv_fieldcat-fieldname = 'DESBLOQUEAR'.
  wa_alv_fieldcat-seltext_s = ' '.
  wa_alv_fieldcat-seltext_m = ' '.
  wa_alv_fieldcat-seltext_l = ' '.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key       = ''.
  wa_alv_fieldcat-tabname   = 'GT_ITEMS'.
  wa_alv_fieldcat-fieldname = 'REFNR'.
  wa_alv_fieldcat-seltext_s = 'Grupo'.
  wa_alv_fieldcat-seltext_m = 'Grupo'.
  wa_alv_fieldcat-seltext_l = 'Grupo'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key       = ''.
  wa_alv_fieldcat-tabname   = 'GT_ITEMS'.
  wa_alv_fieldcat-fieldname = 'REFNT'.
  wa_alv_fieldcat-seltext_s = 'Desig.'.
  wa_alv_fieldcat-seltext_m = 'Designação'.
  wa_alv_fieldcat-seltext_l = 'Designação'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_ITEMS'.
  wa_alv_fieldcat-fieldname = 'VBELN'.
  wa_alv_fieldcat-seltext_s = 'Remessa'.
  wa_alv_fieldcat-seltext_m = 'Remessa'.
  wa_alv_fieldcat-seltext_l = 'Remessa'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

*  wa_alv_fieldcat-key = ''.
*  wa_alv_fieldcat-tabname   = 'GT_ITEMS'.
*  wa_alv_fieldcat-fieldname = 'KUNNR'.
*  wa_alv_fieldcat-seltext_s = 'Cliente'.
*  wa_alv_fieldcat-seltext_m = 'Cliente'.
*  wa_alv_fieldcat-seltext_l = 'Cliente'.
*  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_ITEMS'.
  wa_alv_fieldcat-fieldname = 'LAND1'.
  wa_alv_fieldcat-seltext_s = 'Pais'.
  wa_alv_fieldcat-seltext_m = 'Pais'.
  wa_alv_fieldcat-seltext_l = 'Pais'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

*  wa_alv_fieldcat-key = ''.
*  wa_alv_fieldcat-tabname   = 'GT_ITEMS'.
*  wa_alv_fieldcat-fieldname = 'NAME1_RM1'.
*  wa_alv_fieldcat-seltext_s = 'Rec. Merc'.
*  wa_alv_fieldcat-seltext_m = 'Rec. Merc. 1'.
*  wa_alv_fieldcat-seltext_l = 'Recebedor Mercadoria 1'.
*  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_ITEMS'.
  wa_alv_fieldcat-fieldname = 'NAME1_RM2'.
  wa_alv_fieldcat-seltext_s = 'Rec. Merc'.
  wa_alv_fieldcat-seltext_m = 'Rec. Merc. '.
  wa_alv_fieldcat-seltext_l = 'Recebedor Mercadoria'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_ITEMS'.
  wa_alv_fieldcat-fieldname = 'PALCP'.
  wa_alv_fieldcat-seltext_s = 'Pal.Comp'.
  wa_alv_fieldcat-seltext_m = 'Pal. Completas'.
  wa_alv_fieldcat-seltext_l = 'Pal. Completas'.
  wa_alv_fieldcat-do_sum    = 'X'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_ITEMS'.
  wa_alv_fieldcat-fieldname = 'PALPK'.
  wa_alv_fieldcat-seltext_s = 'P.Pick.1/2'.
  wa_alv_fieldcat-seltext_m = 'Pal. Pick. 1/2'.
  wa_alv_fieldcat-seltext_l = 'Palaletes Picking 1/2'.
  wa_alv_fieldcat-do_sum    = 'X'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_ITEMS'.
  wa_alv_fieldcat-fieldname = 'PALTO'.
  wa_alv_fieldcat-seltext_s = 'T.Pal.1/2'.
  wa_alv_fieldcat-seltext_m = 'T. Pal. 1/2'.
  wa_alv_fieldcat-seltext_l = 'Total Paletes 1/2'.
  wa_alv_fieldcat-do_sum    = 'X'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.
  lv_repid = sy-repid.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_ITEMS'.
  wa_alv_fieldcat-fieldname = 'PALPK_S'.
  wa_alv_fieldcat-seltext_s = 'Pal.Pick.'.
  wa_alv_fieldcat-seltext_m = 'Pal. Picking'.
  wa_alv_fieldcat-seltext_l = 'Pal. Picking'.
  wa_alv_fieldcat-do_sum    = 'X'.
  wa_alv_fieldcat-inttype = 'P'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_ITEMS'.
  wa_alv_fieldcat-fieldname = 'PALTO_S'.
  wa_alv_fieldcat-seltext_s = 'T. Pal.'.
  wa_alv_fieldcat-seltext_m = 'Total Pal.'.
  wa_alv_fieldcat-seltext_l = 'Total Paletes'.
  wa_alv_fieldcat-do_sum    = 'X'.
  wa_alv_fieldcat-inttype = 'P'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.
  lv_repid = sy-repid.

  wa_alv_fieldcat-key = ''.
  wa_alv_fieldcat-tabname   = 'GT_ITEMS'.
  wa_alv_fieldcat-fieldname = 'PFALT'.
  wa_alv_fieldcat-seltext_s = 'P. Falta'.
  wa_alv_fieldcat-seltext_m = 'Prod. Falta'.
  wa_alv_fieldcat-seltext_l = 'Produto em falta'.
  APPEND wa_alv_fieldcat TO alv_fieldcat.

  alv_layout-colwidth_optimize = 'X'.
  alv_layout-zebra             = 'X'.
  alv_layout-no_min_linesize   = 'X'.

*  wa_sort-fieldname = 'VKORG'.
*  wa_sort-subtot    = 'X'.
*  APPEND wa_sort TO alv_sort.

  SORT gt_dados BY vkorg ASCENDING tknum ASCENDING.


  IF p_estilo EQ '3'.
    DATA: lv_vtext TYPE vtxtk.

    LOOP AT gt_dados.
      CLEAR: lv_vtext.
      MOVE-CORRESPONDING gt_dados TO tab_out.

      tab_out-all = 'Org. Vendas'.
      CONCATENATE gt_dados-refnr '-' gt_dados-refnt
                  INTO tab_out-refnr_out.

      SELECT SINGLE vtext INTO lv_vtext FROM tvkot
             WHERE spras EQ sy-langu
               AND vkorg EQ gt_dados-vkorg.
      CONCATENATE gt_dados-vkorg '-' lv_vtext
                  INTO tab_out-vkorg_out.
      APPEND tab_out. CLEAR: tab_out.
    ENDLOOP.

    CLEAR:   gt_t311, gt_t311a, gt_vttk, gt_vttp, gt_likp,
              gt_lips, gt_kna1,  gt_vbak, gt_vbap, gt_vbfa,
              gt_ekko, gt_ekpo,  gt_ekbe, gt_header, gt_items.

    REFRESH: gt_t311, gt_t311a, gt_vttk, gt_vttp, gt_likp,
             gt_lips, gt_kna1,  gt_vbak, gt_vbap, gt_vbfa,
             gt_ekko, gt_ekpo,  gt_ekbe, gt_header, gt_items.

    PERFORM alv_tree.
  ELSEIF p_estilo EQ '2'.
    CLEAR: lv_tabix, lv_contador, wa_dados.

    SORT gt_dados BY vkorg ASCENDING
                     tknum ASCENDING
                     refnr ASCENDING
                     vbeln ASCENDING.

    LOOP AT gt_dados INTO wa_dados.
      lv_tabix = sy-tabix.
      AT NEW vkorg.
        CLEAR lv_contador.
        lv_contador = 1.
      ENDAT.

      wa_dados-numero = lv_contador.
      MODIFY gt_dados FROM wa_dados INDEX lv_tabix.

      AT NEW tknum.
        ADD 1 TO lv_contador.
      ENDAT.
    ENDLOOP.
    PERFORM alv_hier.
  ELSE.
    CLEAR: lv_tabix, lv_contador, wa_dados.

    SORT gt_dados BY vkorg ASCENDING
                     tknum ASCENDING
                     refnr ASCENDING
                     vbeln ASCENDING.

    LOOP AT gt_dados INTO wa_dados.
      lv_tabix = sy-tabix.
      AT NEW vkorg.
        CLEAR lv_contador.
        lv_contador = 1.
      ENDAT.
      wa_dados-numero = lv_contador.
      MODIFY gt_dados FROM wa_dados INDEX lv_tabix.
      ADD 1 TO lv_contador.
    ENDLOOP.

    CLEAR:   gt_t311, gt_t311a, gt_vttk, gt_vttp, gt_likp,
             gt_lips, gt_kna1,  gt_vbak, gt_vbap, gt_vbfa,
             gt_ekko, gt_ekpo,  gt_ekbe, gt_header, gt_items.

    REFRESH: gt_t311, gt_t311a, gt_vttk, gt_vttp, gt_likp,
             gt_lips, gt_kna1,  gt_vbak, gt_vbap, gt_vbfa,
             gt_ekko, gt_ekpo,  gt_ekbe, gt_header, gt_items.

    PERFORM alv_grid.
  ENDIF.

ENDFORM.                    " DISPLAY_DADOS
*&---------------------------------------------------------------------*
*&      Form  CREATE_COMBOBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_combobox .
  FREE:  lt_list, wa_value.
  CLEAR: lt_list, wa_value.

  lv_name = 'P_ESTILO'.

  wa_value-key  = '1'.
  wa_value-text = 'ALV Grid'.
  APPEND wa_value TO lt_list.

  wa_value-key  = '2'.
  wa_value-text = 'ALV Hier'.
  APPEND wa_value TO lt_list.

*  wa_value-key  = '3'.
*  wa_value-text = 'ALV Tree'.
*  APPEND wa_value TO lt_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_name
      values = lt_list.
ENDFORM.                    " CREATE_COMBOBOX
*&---------------------------------------------------------------------*
*&      Form  GET_VARIANTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_VARIA  text
*----------------------------------------------------------------------*
FORM get_variante  CHANGING p_p_varia.
  is_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = is_variant
    IMPORTING
      es_variant    = is_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc = 0.
    p_p_varia = is_variant-variant.
  ENDIF.
ENDFORM.                    " GET_VARIANTE
*&---------------------------------------------------------------------*
*&      Form  INICIALIZACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inicializacao .
  s_datum-sign   = 'I'.
  s_datum-option = 'GE'.
  s_datum-low    = sy-datum - 2 .
  APPEND s_datum.

  mod_desc = 'Estilo'.

  DATA: lv_dec(11).

  s_refnt-sign   = 'I'.
  s_refnt-option = 'CP'.
  CONCATENATE '*' sy-datum+6(2) '-' sy-datum+4(2) '-* X*'
  INTO s_refnt-low.
  APPEND s_refnt.

  s_vkorg-sign   = 'I'.
  s_vkorg-option = 'CP'.
  s_vkorg-low    = 'RP*'.
  APPEND s_vkorg.

  p_varia = '/TOTAIS'.
ENDFORM.                    " INICIALIZACAO
*&---------------------------------------------------------------------*
*&      Form  PROGRESSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0031   text
*      -->P_TEXT_003  text
*----------------------------------------------------------------------*
FORM progresso  USING    VALUE(p_0031)
                         p_text_003.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_0031
      text       = p_text_003.
ENDFORM.                    " PROGRESSO
*&---------------------------------------------------------------------*
*&      Form  top_of_page_setup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page_setup.
  DATA: t_header  TYPE slis_t_listheader,
        wa_header TYPE slis_listheader.

  wa_header-typ  = 'H'.
  wa_header-info = 'Mapa de Planeamento'.
  APPEND wa_header TO t_header.

  CLEAR wa_header.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header.
ENDFORM.                    " top_of_page_setup
*&---------------------------------------------------------------------*
*&      Form  standard_fullscreen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->EXTAB      text
*----------------------------------------------------------------------*
FORM standard_fullscreen USING  extab TYPE slis_t_extab.

  SET PF-STATUS 'STANDARD_FULLSCREEN' .
*EXCLUDING extab
ENDFORM.                    "STANDARD_FULLSCREEN
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_grid .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = lv_repid
      i_callback_pf_status_set = 'STANDARD_FULLSCREEN'
      i_callback_top_of_page   = 'TOP_OF_PAGE_SETUP'
      is_layout                = alv_layout
      it_fieldcat              = alv_fieldcat
      it_sort                  = alv_sort
    TABLES
      t_outtab                 = gt_dados
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  ALV_HIER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_hier .
  DATA: wa_dados LIKE gt_dados,
        lv_total TYPE i.

  DATA: lt_vbap   LIKE vbap  OCCURS 0 WITH HEADER LINE,
        lt_vbap_c TYPE TABLE OF vbap.

  lt_vbap[] = gt_vbap[].

  LOOP AT gt_dados.
    MOVE-CORRESPONDING gt_dados TO gt_items.
    gt_header-bezei     = gt_dados-bezei.
    gt_header-vkorg     = gt_dados-vkorg.
    gt_header-trans     = gt_dados-trans.
    gt_header-numero    = gt_dados-numero.
    gt_header-clientes  = gt_dados-clientes.
    gt_header-un_peso   = gt_dados-un_peso.
    gt_header-un_volume = gt_dados-un_volume.
    gt_header-tp_carro  = gt_dados-tp_carro.
    gt_header-dtdis     = gt_dados-dtdis.
    gt_header-observ    = gt_dados-observ.
    gt_header-signi     = gt_dados-signi.
    gt_header-desbloquear     = gt_dados-desbloquear.

    APPEND gt_items. CLEAR gt_items.
    AT NEW tknum.
      LOOP AT gt_vttp WHERE tknum EQ gt_dados-tknum.
        READ TABLE gt_likp WITH KEY vbeln = gt_vttp-vbeln.
        IF sy-subrc EQ 0.
*          ADD gt_likp-btgew TO gt_header-peso.
          ADD gt_likp-volum TO gt_header-volume.
        ENDIF.

        LOOP AT gt_lips WHERE vbeln = gt_likp-vbeln.
          lt_vbap_c = gt_vbap[].
          DELETE lt_vbap_c WHERE vbeln <> gt_lips-vgbel.
          IF lt_vbap_c IS INITIAL.
            ADD gt_lips-brgew TO gt_header-peso.
          ELSE.
            LOOP AT lt_vbap WHERE vbeln = gt_lips-vgbel.
              gt_header-peso = gt_header-peso + lt_vbap-brgew.
              DELETE lt_vbap INDEX sy-tabix.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      CLEAR lv_total.
      LOOP AT gt_dados INTO wa_dados WHERE tknum EQ gt_dados-tknum.
        ADD wa_dados-palto TO lv_total.
      ENDLOOP.
      gt_header-tknum = gt_dados-tknum.
      gt_header-pal = lv_total.
*      """""""""""""""""""""Adicionar campo que informa se o registo foi para o portal TFARIA 2023.06.27
*      select single DESBLOQUEAR into gt_header-desbloquear from ztransp_h where tknum = gt_header-tknum.
*      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      APPEND gt_header. CLEAR: gt_header.
    ENDAT.
  ENDLOOP.

  DATA: BEGIN OF lt_transp OCCURS 0,
          tknum TYPE tknum,
        END OF lt_transp.

  CLEAR: lt_transp. REFRESH: lt_transp.

  LOOP AT gt_header.
    MOVE-CORRESPONDING gt_header TO lt_transp.
    COLLECT lt_transp.
  ENDLOOP.

  CLEAR: gv_cargas.
  DESCRIBE TABLE lt_transp LINES gv_cargas.

  CLEAR:   gt_t311, gt_t311a, gt_vttk, gt_vttp, gt_likp,
            gt_lips, gt_kna1,  gt_vbak, gt_vbap, gt_vbfa,
            gt_ekko, gt_ekpo,  gt_ekbe.

  REFRESH: gt_t311, gt_t311a, gt_vttk, gt_vttp, gt_likp,
           gt_lips, gt_kna1,  gt_vbak, gt_vbap, gt_vbfa,
           gt_ekko, gt_ekpo,  gt_ekbe.

  SET TITLEBAR 'Z001' WITH gv_cargas.

  alv_keyinfo-header01 = 'VKORG'.
  alv_keyinfo-item01   = 'VKORG'.
  alv_keyinfo-header02 = 'TKNUM'.
  alv_keyinfo-item02   = 'TKNUM'.

  is_variant-report = sy-repid.
  is_variant-variant = p_varia.

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      i_callback_program       = lv_repid
      i_callback_pf_status_set = 'STANDARD_FULLSCREEN'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = alv_layout
      it_fieldcat              = alv_fieldcat
      it_sort                  = alv_sort
      is_variant               = is_variant
      i_tabname_header         = 'GT_HEADER'
      i_tabname_item           = 'GT_ITEMS'
*     I_STRUCTURE_NAME_HEADER  =
*     I_STRUCTURE_NAME_ITEM    =
      is_keyinfo               = alv_keyinfo
    TABLES
      t_outtab_header          = gt_header
      t_outtab_item            = gt_items
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " ALV_HIER
*&---------------------------------------------------------------------*
*&      Form  ALV_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_tree .
  CALL SCREEN 0001.
ENDFORM.                    " ALV_TREE
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'GERAL'.
  IF tree1 IS INITIAL.
    PERFORM init_tree.
  ENDIF.

*  IF NOT all_tasks[] IS INITIAL.
  IF flag_tree IS INITIAL.
    PERFORM cria_hierarquia.
    flag_tree = 'X'.
  ENDIF.
*  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_tree .
  DATA: lt_expand_nodes TYPE lvc_t_nkey.

  DATA: l_toolbar_excluding TYPE ui_functions.

  REFRESH gt_out.

* create fieldcatalog for structure t001
  PERFORM build_fieldcatalog.

*  perform ajusta_propriedades.

* create container for alv-tree
  DATA: l_tree_container_name(30) TYPE c,
        l_custom_container        TYPE REF TO cl_gui_custom_container.

  l_tree_container_name = 'TREE1'.

  IF sy-batch IS INITIAL.
    CREATE OBJECT l_custom_container
      EXPORTING
        container_name              = l_tree_container_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc <> 0.

    ENDIF.
  ENDIF.

* create tree control
  CREATE OBJECT tree1
    EXPORTING
      parent                      = l_custom_container
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
      item_selection              = 'X'
      no_html_header              = ''
      no_toolbar                  = ''
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.
  IF sy-subrc <> 0.

  ENDIF.

* create Hierarchy-header
  DATA l_hierarchy_header TYPE treev_hhdr.
  PERFORM build_hierarchy_header CHANGING l_hierarchy_header.

* create info-table for html-header
  DATA: lt_list_commentary TYPE slis_t_listheader,
        l_logo             TYPE sdydo_value.

  PERFORM build_comment USING
                 lt_list_commentary
                 l_logo.

* repid for saving variants
  DATA: ls_variant TYPE disvariant.
  ls_variant-report = sy-repid.

*  perform define_toolbar_excluding changing l_toolbar_excluding.

  l_logo = 'RENOVA_LOGO'.

** Opções da Toolbar a excluir
  PERFORM exluir_toolbar CHANGING l_toolbar_excluding.

* create empty tree-control
  CALL METHOD tree1->set_table_for_first_display
    EXPORTING
      is_hierarchy_header  = l_hierarchy_header
      it_list_commentary   = lt_list_commentary
      i_logo               = l_logo
      i_background_id      = 'ALV_BACKGROUND'
      i_save               = 'A'
      is_variant           = ls_variant
      it_toolbar_excluding = l_toolbar_excluding
    CHANGING
      it_outtab            = gt_out "table must be empty !!
      it_fieldcatalog      = gt_fieldcatalog.

* register events
  PERFORM register_events.

* add own functioncodes to the toolbar
  PERFORM change_toolbar.
ENDFORM.                    " INIT_TREE
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .
  DATA: pos TYPE i VALUE 1.

  DATA: aux_cat TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE.

  REFRESH aux_cat.
  CLEAR tab_out.

*--------------organizar por Org. Vendas--------------------Mar2005
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'ALL'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Org. Vendas'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'VKORG_OUT'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Org. Vendas'.
  APPEND aux_cat.
  ADD 1 TO pos.

*-------------------------------------------------------------------end
  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'TKNUM'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Transporte'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'REFNR_OUT'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Grupo'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'VBELN'.
  aux_cat-no_out        = 'X'.
  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-do_sum        = 'X'.
  aux_cat-coltext       = 'Remessa'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'BEZEI'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
*  aux_cat-no_zero       = 'X'.
  aux_cat-outputlen     = 20.
  aux_cat-coltext       = 'Zona Tarifa'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'TRANS'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
*  aux_cat-no_zero       = 'X'.
  aux_cat-outputlen     = 20.
  aux_cat-coltext       = 'Transportador'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'LAND1'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
*  aux_cat-no_zero       = 'X'.
  aux_cat-outputlen     = 6.
  aux_cat-coltext       = 'Pais'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'NAME1_RM1'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
*  aux_cat-no_zero       = 'X'.
  aux_cat-outputlen     = 25.
  aux_cat-coltext       = 'Rec. Mercadoria 1'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'NAME1_RM2'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
*  aux_cat-no_zero       = 'X'.
  aux_cat-outputlen     = 25.
  aux_cat-coltext       = 'Rec. Mercadoria 2'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PALCP'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-outputlen     = 8.
  aux_cat-coltext       = 'Pal. Completas'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PALPK'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-outputlen     = 8.
  aux_cat-coltext       = 'Pal. Picking'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PALTO'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-outputlen     = 8.
  aux_cat-coltext       = 'Total Pal.'.
  APPEND aux_cat.
  ADD 1 TO pos.


  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PALPK_S'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-outputlen     = 8.
  aux_cat-coltext       = 'Pal. Picking'.
  APPEND aux_cat.
  ADD 1 TO pos.

  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PALTO_S'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
  aux_cat-no_zero       = 'X'.
  aux_cat-outputlen     = 8.
  aux_cat-coltext       = 'Total Pal.'.
  APPEND aux_cat.
  ADD 1 TO pos.


  CLEAR aux_cat.
  aux_cat-col_pos       =  pos.
  aux_cat-fieldname     = 'PFALT'.
*  aux_cat-no_out        = 'X'.
*  aux_cat-key           = 'X'.
*  aux_cat-no_zero       = 'X'.
  aux_cat-outputlen     = 200.
  aux_cat-coltext       = 'Produto em Falta'.
  APPEND aux_cat.
  ADD 1 TO pos.
*---------------------------------------------------------
  gt_fieldcatalog[] = aux_cat[].
ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_HIERARCHY_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_HIERARCHY_HEADER  text
*----------------------------------------------------------------------*
FORM build_hierarchy_header CHANGING
                               p_hierarchy_header TYPE treev_hhdr.

*  p_hierarchy_header-heading =
*             'Grupo Entrega/Grupo/Remessa/Ordem Transf.'.
*  p_hierarchy_header-tooltip =
*             'Grupo Entrega/Grupo/Remessa/Ordem Transf.'.
*  p_hierarchy_header-width = 50.
*  p_hierarchy_header-width_pix = ' '.
*---------------------------------------------------------------
  p_hierarchy_header-heading =
             'Org. Vendas/Transporte/Grupo/Remessa'.
  p_hierarchy_header-tooltip =
             'Org. Vendas/Transporte/Grupo/Remessa'.
  p_hierarchy_header-width = 80.
  p_hierarchy_header-width_pix = ' '.

ENDFORM.                               " build_hierarchy_header
*&---------------------------------------------------------------------*
*&      Form  BUILD_COMMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_LIST_COMMENTARY  text
*      -->P_L_LOGO  text
*----------------------------------------------------------------------*
FORM build_comment USING
      pt_list_commentary TYPE slis_t_listheader
      p_logo             TYPE sdydo_value.

  DATA: ls_line  TYPE slis_listheader,
        text(60),
        l_lines  LIKE sy-tabix,
        l_tabix  LIKE sy-tabix.

  CLEAR : text,
          aux_s_date.

* LIST HEADING LINE: TYPE H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = 'Mapa Planeamento'(t00).
  APPEND ls_line TO pt_list_commentary.

* STATUS LINE: TYPE S
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Armazém:'(t01).
  ls_line-info = p_lgnum.
  APPEND ls_line TO pt_list_commentary.

* Grupo: Inicio
  CLEAR ls_line.
  DESCRIBE TABLE s_refnr LINES l_lines.
  IF l_lines > 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Grupo(s):'(t02).
    CASE l_lines.
      WHEN 1.
        l_tabix = 1.
        PERFORM fill_title USING 'C'
                                 s_refnr-low
                                 s_refnr-high
                                 l_tabix
                        CHANGING ls_line-info.
      WHEN OTHERS.
        LOOP AT s_refnr.
          l_tabix = sy-tabix.
          PERFORM fill_title USING 'C'
                                   s_refnr-low
                                   s_refnr-high
                                   l_tabix
                          CHANGING ls_line-info.
        ENDLOOP.
    ENDCASE.
    APPEND ls_line TO pt_list_commentary.
  ENDIF.
* Grupo: Fim

* Descrição Grupo: Inicio
  CLEAR ls_line.
  DESCRIBE TABLE s_refnt LINES l_lines.
  IF l_lines > 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Descrição Grupo:'(t03).
    CASE l_lines.
      WHEN 1.
        l_tabix = 1.
        PERFORM fill_title USING 'C'
                                 s_refnt-low
                                 s_refnt-high
                                 l_tabix
                        CHANGING ls_line-info.
      WHEN OTHERS.
        LOOP AT s_refnt.
          l_tabix = sy-tabix.
          PERFORM fill_title USING 'C'
                                   s_refnt-low
                                   s_refnt-high
                                   l_tabix
                          CHANGING ls_line-info.
        ENDLOOP.
    ENDCASE.
    APPEND ls_line TO pt_list_commentary.
  ENDIF.
* Descrição Grupo: Fim

* Organização Vendas: Inicio
  CLEAR ls_line.
  DESCRIBE TABLE s_vkorg LINES l_lines.
  IF l_lines > 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Organização de Vendas:'(t03).
    CASE l_lines.
      WHEN 1.
        l_tabix = 1.
        PERFORM fill_title USING 'C'
                                 s_vkorg-low
                                 s_vkorg-high
                                 l_tabix
                        CHANGING ls_line-info.
      WHEN OTHERS.
        LOOP AT s_vkorg.
          l_tabix = sy-tabix.
          PERFORM fill_title USING 'C'
                                   s_vkorg-low
                                   s_vkorg-high
                                   l_tabix
                          CHANGING ls_line-info.
        ENDLOOP.
    ENDCASE.
    APPEND ls_line TO pt_list_commentary.
  ENDIF.
* Descrição Grupo: Fim

* Data: Inicio
  CLEAR ls_line.
  DESCRIBE TABLE s_datum LINES l_lines.
  IF l_lines > 0.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Data Criação da OT:'(t04).
    CASE l_lines.
      WHEN 1.
        l_tabix = 1.
        PERFORM fill_title USING 'D'
                                 s_datum-low
                                 s_datum-high
                                 l_tabix
                        CHANGING ls_line-info.
      WHEN OTHERS.
        LOOP AT s_datum.
          l_tabix = sy-tabix.
          PERFORM fill_title USING 'D'
                                   s_datum-low
                                   s_datum-high
                                   l_tabix
                          CHANGING ls_line-info.
        ENDLOOP.
    ENDCASE.
    APPEND ls_line TO pt_list_commentary.
  ENDIF.
* Data: Fim


*** ACTION LINE: TYPE A
*  CLEAR ls_line.
*  ls_line-typ  = 'A'.
*  GET TIME.
*  WRITE sy-datum TO aux_s_date.
** TEXT-T02 DATA DE EXECUÇÃO
*  CONCATENATE text-t06 aux_s_date INTO text
*                         SEPARATED BY space.
*  ls_line-info = text.
*  APPEND ls_line TO pt_list_commentary.

ENDFORM.                    "build_comment
*&---------------------------------------------------------------------*
*&      Form  FILL_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2777   text
*      -->P_DATUM_LOW  text
*      -->P_DATUM_HIGH  text
*      -->P_L_TABIX  text
*      <--P_LS_LINE_INFO  text
*----------------------------------------------------------------------*
FORM fill_title USING f_tipo f_low f_high f_tabix f_info.

  DATA: l_text(60),
        l_text_low(60),
        l_text_high(60).

  DATA: l_len  TYPE i,
        l_offs TYPE i,
        l_ctrl TYPE i.

  CLEAR: l_text, l_text_low, l_text_high.
  IF f_low IS INITIAL.
    PERFORM trata_tipo USING f_tipo f_high
                    CHANGING l_text_high.
    CONCATENATE 'até' l_text_high INTO l_text SEPARATED BY space.
  ELSE.
    IF f_high IS INITIAL.
      PERFORM trata_tipo USING f_tipo f_low
                      CHANGING l_text_low.
      IF f_tabix = 1.
        MOVE l_text_low TO l_text.
      ELSE.
        CONCATENATE '/' l_text_low INTO l_text
                                    SEPARATED BY space.
      ENDIF.
    ELSE.
      PERFORM trata_tipo USING f_tipo f_low
                      CHANGING l_text_low.
      PERFORM trata_tipo USING f_tipo f_high
                      CHANGING l_text_high.
      IF f_tabix = 1.
        CONCATENATE 'de' l_text_low 'a' l_text_high INTO l_text
                                      SEPARATED BY space.
      ELSE.
        CONCATENATE '/ de' l_text_low 'a' l_text_high INTO l_text
                                      SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDIF.
  IF f_tabix = 1.
    MOVE l_text TO f_info.
  ELSE.
    l_offs = strlen( f_info ) + 1.
    l_len = strlen( l_text ).
    l_ctrl = l_offs + l_len.
    CHECK l_ctrl LE 60.
    MOVE l_text TO f_info+l_offs(l_len).
  ENDIF.
ENDFORM.                    "fill_title
*&---------------------------------------------------------------------*
*&      Form  TRATA_TIPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_F_TIPO  text
*      -->P_F_LOW  text
*      <--P_L_TEXT_LOW  text
*----------------------------------------------------------------------*
FORM trata_tipo USING f_typ f_input f_output.
  CASE f_typ.
    WHEN 'D'.
      WRITE f_input TO f_output DD/MM/YYYY.
    WHEN 'C'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = f_input
        IMPORTING
          output = f_output.
    WHEN OTHERS.
      MOVE f_input TO f_output.
  ENDCASE.
ENDFORM.                    "trata_tipo
*&---------------------------------------------------------------------*
*&      Form  EXLUIR_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_TOOLBAR_EXCLUDING  text
*----------------------------------------------------------------------*
FORM exluir_toolbar  CHANGING ct_excl_func
                                     TYPE ui_functions.

  CLEAR ct_excl_func.

  APPEND '&CALC'             TO ct_excl_func.
*  APPEND '&GRAPH'               TO ct_excl_func.
*  APPEND '&REFRESH'             TO ct_excl_func.
*  APPEND '&INFO'                TO ct_excl_func.
*  APPEND '&LOCAL&PASTE'         TO ct_excl_func.
*  APPEND '&LOCAL&PASTE_NEW_ROW' TO ct_excl_func.
*  APPEND '&LOCAL&UNDO'          TO ct_excl_func.
*  APPEND '&LOCAL&APPEND'        TO ct_excl_func.
*  APPEND '&LOCAL&INSERT_ROW'    TO ct_excl_func.
*  APPEND '&LOCAL&DELETE_ROW'    TO ct_excl_func.
*  APPEND '&LOCAL&COPY_ROW'      TO ct_excl_func.
*  APPEND '&LOCAL&CUT'           TO ct_excl_func.
*  APPEND '&LOCAL&COPY'          TO ct_excl_func.

ENDFORM.                    " exluir_toolbar
*&---------------------------------------------------------------------*
*&      Form  REGISTER_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_events .
* define the events which will be passed to the backend
  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.

* define the events which will be passed to the backend
  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  APPEND l_event TO lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_checkbox_change.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_header_context_men_req.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
*  append l_event to lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
*  append l_event to lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_header_click.
  APPEND l_event TO lt_events.
*  l_event-eventid = cl_gui_column_tree=>eventid_item_keypress.
*  append l_event to lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  APPEND l_event TO lt_events.

  CALL METHOD tree1->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
*    MESSAGE e074(zdialogrf).
  ENDIF.

* set Handler
  DATA: l_event_receiver TYPE REF TO lcl_tree_event_receiver.
  CREATE OBJECT l_event_receiver.

  SET HANDLER l_event_receiver->handle_item_double_click FOR tree1.
ENDFORM.                    " REGISTER_EVENTS
*&---------------------------------------------------------------------*
*&      Form  CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_toolbar .
** get toolbar control
*  CALL METHOD tree1->get_toolbar_object
*    IMPORTING
*      er_toolbar = mr_toolbar.
*
*  CHECK NOT mr_toolbar IS INITIAL.
*
** add separator to toolbar
*  IF p_mod = 'X'.
*    CALL METHOD mr_toolbar->add_button
*      EXPORTING
*        fcode     = ''
*        icon      = ''
*        butn_type = cntb_btype_sep
*        text      = ''
*        quickinfo = 'Separador'.
*
** add Standard Button to toolbar (for Priority)
*    CALL METHOD mr_toolbar->add_button
*      EXPORTING
*        fcode     = 'PRIO'
*        icon      = icon_status_critical
*        butn_type = cntb_btype_button
*        text      = 'Prioridade'
*        quickinfo = 'Prioridade'.
*  ENDIF.
*
** add separator to toolbar
*  CALL METHOD mr_toolbar->add_button
*    EXPORTING
*      fcode     = ''
*      icon      = ''
*      butn_type = cntb_btype_sep
*      text      = ''
*      quickinfo = 'Separador'.
*
*  IF p_mod = 'X'.
*
*** add Standard Button to toolbar (for Tipo de Carga)
**    CALL METHOD mr_toolbar->add_button
**      EXPORTING
**        fcode     = 'CARGA'
**        icon      = icon_proposition
**        butn_type = cntb_btype_button
**        text      = 'Tipo de Carga'
**        quickinfo = 'Tipo de Carga'.
**
*** add separator to toolbar
**    CALL METHOD mr_toolbar->add_button
**      EXPORTING
**        fcode     = ''
**        icon      = ''
**        butn_type = cntb_btype_sep
**        text      = ''
**        quickinfo = 'Separador'.
*
** add Standard Button to toolbar (for Lock Total)
*    CALL METHOD mr_toolbar->add_button
*      EXPORTING
*        fcode     = 'LOCK1'
*        icon      = icon_unspecified_one
*        butn_type = cntb_btype_button
*        text      = 'Bloqueio Total'
*        quickinfo = 'Bloqueio Total'.
*
** add Standard Button to toolbar (for Unlock Picking)
*    CALL METHOD mr_toolbar->add_button
*      EXPORTING
*        fcode     = 'LOCK2'
*        icon      = icon_unspecified_two
*        butn_type = cntb_btype_button
*        text      = 'Picking'
*        quickinfo = 'Desbloquear Picking'.
*
** add Standard Button to toolbar (for Unlock Paletes Completas)
*    CALL METHOD mr_toolbar->add_button
*      EXPORTING
*        fcode     = 'LOCK3'
*        icon      = icon_unspecified_three
*        butn_type = cntb_btype_button
*        text      = 'Pal. Completas'
*        quickinfo = 'Desbloquear Paletes Completas'.
*
** add Standard Button to toolbar (for Unlock Pick & Pal. Completas)
*    CALL METHOD mr_toolbar->add_button
*      EXPORTING
*        fcode     = 'LOCK4'
*        icon      = icon_unspecified_four
*        butn_type = cntb_btype_button
*        text      = 'Picking+Pal.Comp.'
*        quickinfo = 'Desbl. Pick. e Pal. Completas'.
*
** add Standard Button to toolbar (for Unlock Carga)
*    CALL METHOD mr_toolbar->add_button
*      EXPORTING
*        fcode     = 'LOCK5'
*        icon      = icon_unspecified_five
*        butn_type = cntb_btype_button
*        text      = 'Carga'
*        quickinfo = 'Desbloquear Carga'.
*
** add separator to toolbar
*    CALL METHOD mr_toolbar->add_button
*      EXPORTING
*        fcode     = ''
*        icon      = ''
*        butn_type = cntb_btype_sep
*        text      = ''
*        quickinfo = 'Separador'.
*
** add Standard Button to toolbar (for correct to's)
*    CALL METHOD mr_toolbar->add_button
*      EXPORTING
*        fcode     = 'TO_PAR'
*        icon      = icon_generate
*        butn_type = cntb_btype_button
*        text      = 'OT Partida'
*        quickinfo = 'OT Partida'.
*
** add separator to toolbar
*    CALL METHOD mr_toolbar->add_button
*      EXPORTING
*        fcode     = ''
*        icon      = ''
*        butn_type = cntb_btype_sep
*        text      = ''
*        quickinfo = 'Separador'.
*  ENDIF.
*
** add Standard Button to toolbar (for Print Paletização Especial)
*  CALL METHOD mr_toolbar->add_button
*    EXPORTING
*      fcode     = 'PRINT_PAL'
*      icon      = icon_print
*      butn_type = cntb_btype_button
*      text      = 'Pal. Especial'
*      quickinfo = 'Imprime Paletização Especial'.
*
** set event-handler for toolbar-control
*  CREATE OBJECT toolbar_event_receiver.
*  SET HANDLER toolbar_event_receiver->on_function_selected
*                                                      FOR mr_toolbar.
**  set handler toolbar_event_receiver->on_toolbar_dropdown
*                                                      for mr_toolbar.
ENDFORM.                    " CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  CRIA_HIERARQUIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_hierarquia .
  CALL METHOD tree1->delete_all_nodes.

* create hierarchy
  PERFORM create_hierarchy.

* this method must be called to send the data to the frontend
*  call method tree1->frontend_update.

* Expand first level
  CALL METHOD tree1->expand_node
    EXPORTING
      i_node_key          = g_top_node_key
    EXCEPTIONS
      failed              = 1
      illegal_level_count = 2
      cntl_system_error   = 3
      node_not_found      = 4
      cannot_expand_leaf  = 5.

* Determina top_node
  CALL METHOD tree1->get_top_node
    IMPORTING
      e_node_key = g_top_node.

* adjust column_width
  CALL METHOD tree1->column_optimize.


*  check not reg_ger is initial.

*  clear g_selected_node.
*  loop at gt_out into gs_out.
*    if gs_out-refnr   = reg_ger-refnr   and
*       gs_out-ordem   = reg_ger-ordem   and
*       gs_out-remessa = reg_ger-remessa and
*       gs_out-sscc is initial.
*      write sy-tabix to g_selected_node.
*      g_selected_node = g_top_node_key + g_selected_node - 1.
*      exit.
*    endif.
*  endloop.
*  check not g_selected_node is initial.
*
*  if not g_fieldname is initial.
*    call method tree1->set_selected_item
*      exporting
*        i_node_key  = g_selected_node
*        i_fieldname = g_fieldname.
*  else.
*    clear g_selected_nodes.
*    append g_selected_node to g_selected_nodes.
*    call method tree1->set_selected_nodes
*      exporting
*        it_selected_nodes = g_selected_nodes.
*  endif.
ENDFORM.                    " CRIA_HIERARQUIA
*&---------------------------------------------------------------------*
*&      Form  CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_hierarchy .
  DATA: ls_out  TYPE t_out,
        lsx_out TYPE t_out.

  DATA: l_all_key   TYPE lvc_nkey,
        l_tknum_key TYPE lvc_nkey,
        l_refnr_key TYPE lvc_nkey,
        l_vkorg_key TYPE lvc_nkey,
        l_last_key  TYPE lvc_nkey.

  IF not_first_time = 'X'.
*    PERFORM select_data.

*    CHECK NOT all_tasks[] IS INITIAL.

*    PERFORM tab_final.

*    PERFORM nova_ordenacao.

    IF tab_out[] IS INITIAL.
*     Não existem dados para processar !
      MESSAGE s196(zwmmsg001).
*     Coloca apenas 1o Nível
      ls_out-all = 'Org. Vendas'.
      PERFORM add_all_line USING ls_out
                                 ''
                        CHANGING l_all_key.
      g_top_node = l_all_key.
    ENDIF.
  ENDIF.

** Não lista se não existirem dados
  CHECK NOT tab_out[] IS INITIAL.

  REFRESH gt_out.

  CLEAR: tab_out, ls_out.

  SORT tab_out BY all vkorg refnr vbeln.
  break roffd.
  LOOP AT tab_out INTO ls_out.

    AT NEW all.
*    on change of ls_out-all.
      PERFORM add_all_line USING ls_out
                                 ''
                        CHANGING l_all_key.
      g_top_node_key = l_all_key.
*    endon.
    ENDAT.
    lsx_out = ls_out.

    AT NEW vkorg.
      PERFORM add_vkorg_line USING lsx_out
                                   l_all_key
                          CHANGING l_vkorg_key.
    ENDAT.

    AT NEW tknum.
      PERFORM add_tknum_line USING lsx_out
                                 l_vkorg_key
                        CHANGING l_tknum_key.
    ENDAT.



    AT NEW refnr.
      PERFORM add_refnr_line USING lsx_out
                                   l_tknum_key
                          CHANGING l_refnr_key.
    ENDAT.

    PERFORM add_complete_line USING ls_out
                                    l_refnr_key
                           CHANGING l_last_key.
  ENDLOOP.

  not_first_time = 'X'.
ENDFORM.                    " CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.
  DATA: selected_node TYPE lvc_nkey,
        item_name     TYPE lvc_fname.

  CASE ok_code.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      PERFORM exit_program.
    WHEN 'REFR'.
      CLEAR flag_tree.
      not_first_time = 'X'.

    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.

  ENDCASE.
  CLEAR ok_code.
*  call method cl_gui_cfw=>flush.
ENDMODULE.                 " USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Form  ADD_ALL_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUT  text
*      -->P_3005   text
*      <--P_L_ALL_KEY  text
*----------------------------------------------------------------------*
FORM add_all_line USING     ps_out TYPE t_out
                               p_relat_key TYPE lvc_nkey
                     CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.
*  ls_item_layout-t_image = '@3P@'.
  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
*  ls_item_layout-style   =
*                        cl_gui_column_tree=>style_intensifd_critical.
  APPEND ls_item_layout TO lt_item_layout.

  MOVE ps_out-all TO ls_out-all.

*** Paletes DRI, TRI, PRM e Picking
*  CLEAR: paletes_dri, paletes_tri, paletes_prm, paletes_crd.
*  LOOP AT lt_paletes.
*    paletes_dri = paletes_dri + lt_paletes-pal_dri.
*    paletes_tri = paletes_tri + lt_paletes-pal_tri.
*    paletes_prm = paletes_prm + lt_paletes-pal_prm.
*    paletes_crd = paletes_crd + lt_paletes-pal_crd.
*  ENDLOOP.
*
*  WRITE paletes_dri TO ls_out-paletes_dri DECIMALS 0.
*  WRITE paletes_tri TO ls_out-paletes_tri DECIMALS 0.
*  WRITE paletes_prm TO ls_out-paletes_prm DECIMALS 0.
*  WRITE paletes_crd TO ls_out-paletes_crd DECIMALS 0.
*
*  CLEAR lt_zwm026.
*  REFRESH lt_zwm026.
*  SELECT * INTO TABLE lt_zwm026
*      FROM zwm026
*        FOR ALL ENTRIES IN lt_paletes
*            WHERE armazem = lgnum
*              AND grupo = lt_paletes-refnr
*              AND estado <> 'T'.
*  IF NOT  lt_zwm026[] IS INITIAL.
*    SORT lt_zwm026.
*    DELETE ADJACENT DUPLICATES FROM lt_zwm026 COMPARING n_pal_picking.
*    DESCRIBE TABLE lt_zwm026 LINES ls_out-paletes_picking.
*  ENDIF.

*  CLEAR: ls_out-prioridade.

* add node
  l_node_text =  ps_out-all.
  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    "add_all_line
*&---------------------------------------------------------------------*
*&      Form  ADD_VKORG_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LSX_OUT  text
*      -->P_L_ALL_KEY  text
*      <--P_L_VKORG_KEY  text
*----------------------------------------------------------------------*
FORM add_vkorg_line USING  ps_out TYPE t_out
                           p_relat_key TYPE lvc_nkey
                 CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.

  MOVE ps_out-all TO ls_out-all.

*  CLEAR: ls_out-prioridade.
*  CLEAR: ls_out-to_partida.

  MOVE ps_out-vkorg TO ls_out-vkorg.

  CLEAR l_node_text.

  MOVE ps_out-vkorg_out TO l_node_text.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    " add_vkorg_line
*&---------------------------------------------------------------------*
*&      Form  ADD_COMPLETE_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUT  text
*      -->P_L_REFNR_KEY  text
*      <--P_L_LAST_KEY  text
*----------------------------------------------------------------------*
FORM add_complete_line USING   ps_out TYPE t_out
                               p_relat_key TYPE lvc_nkey
                     CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out.

  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.

  APPEND ls_item_layout TO lt_item_layout.

  ls_out = ps_out.

  l_node_text =  ps_out-vbeln.
  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      is_outtab_line   = ls_out
      i_node_text      = l_node_text
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                               " add_complete_line
*&---------------------------------------------------------------------*
*&      Form  ADD_REFNR_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LSX_OUT  text
*      -->P_L_VKORG_KEY  text
*      <--P_L_REFNR_KEY  text
*----------------------------------------------------------------------*
FORM add_refnr_line USING  ps_out TYPE t_out
                           p_relat_key TYPE lvc_nkey
                 CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        l_ctrl,
        ls_out      TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.


  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.

*  SORT lt_to_part BY refnr.
*  READ TABLE lt_to_part WITH KEY refnr = ps_out-refnr
*                        BINARY SEARCH.
*  IF sy-subrc = 0.
*    ls_item_layout-style   =
*                       cl_gui_column_tree=>style_emphasized_c.
*  ENDIF.

*  APPEND ls_item_layout TO lt_item_layout.

  MOVE ps_out-all TO ls_out-all.

  MOVE ps_out-refnr TO ls_out-refnr.



* add node
  CLEAR: l_node_text, ls_out.
*  write ps_out-refnr to aux_refnr.
*  concatenate  aux_refnr '-' ps_out-refnt into l_node_text.

  MOVE ps_out-refnr_out TO l_node_text.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    "add_refnr_out_line
*&---------------------------------------------------------------------*
*&      Form  EXIT_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_program .
  CALL METHOD tree1->free.
  SET SCREEN 0. LEAVE SCREEN.
ENDFORM.                    " EXIT_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  ADD_TKNUM_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LSX_OUT  text
*      -->P_L_ALL_KEY  text
*      <--P_L_TKNUM_KEY  text
*----------------------------------------------------------------------*
FORM add_tknum_line  USING  ps_out TYPE t_out
                           p_relat_key TYPE lvc_nkey
                 CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
        ls_out      TYPE t_out.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.

  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.

  MOVE ps_out-all TO ls_out-all.

*  MOVE-CORRESPONDING ps_out TO ls_out.
*  MOVE ps_out-tknum TO ls_out-tknum.

  CLEAR l_node_text.

  MOVE ps_out-tknum TO l_node_text.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_out
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                    " add_tknum_line

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command USING     rf_ucomm  LIKE sy-ucomm
                            rs        TYPE slis_selfield.
  DATA: lv_refnr     TYPE lvs_refnr,
        lv_tknum     TYPE tknum,
        lv_vbeln     TYPE vbeln_vl,
        lv_desc_t311 TYPE lvs_refnt,
        lv_desc_vbsk TYPE bezei30.

  CASE rf_ucomm.
    WHEN '&IC1'.
      CASE rs-fieldname.
        WHEN 'REFNR'.
          IF NOT rs-value IS INITIAL.
            MOVE rs-value TO lv_refnr.

            SET PARAMETER ID 'GRN' FIELD lv_refnr.
            CALL TRANSACTION 'VG03' AND SKIP FIRST SCREEN.
          ENDIF.
          WAIT UP TO 1 SECONDS.

          SELECT SINGLE refnt INTO lv_desc_t311
                 FROM t311
                 WHERE refnr EQ lv_refnr.
          IF sy-subrc EQ 0.
            SELECT SINGLE vtext INTO lv_desc_vbsk
                   FROM vbsk
                   WHERE sammg EQ lv_refnr.
            IF sy-subrc EQ 0.
              IF lv_desc_vbsk NE lv_desc_t311(30).
                MESSAGE i000 WITH 'Descrição do Grupo'
                                  lv_refnr
                                  'não foi actualizada com sucesso!'.
              ENDIF.
            ENDIF.
          ENDIF.
        WHEN 'TKNUM'.
          IF NOT rs-value IS INITIAL.
            MOVE rs-value TO lv_tknum.
            SET PARAMETER ID 'TNR' FIELD lv_tknum.
            CALL TRANSACTION 'VT02N' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'VBELN'.
          IF NOT rs-value IS INITIAL.
            MOVE rs-value TO lv_vbeln.
            SET PARAMETER ID 'VL' FIELD lv_vbeln.
            CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

    WHEN 'REFRESH'.
      PERFORM get_dados.
      PERFORM display_dados.
      rs-refresh = 'X'.
      rs-exit = 'X'.

    WHEN 'IMPRIMIR'.
      PERFORM print_layout.

    WHEN OTHERS.
  ENDCASE.
  CLEAR: rf_ucomm.
ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  PRINT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_layout .
  DATA: gt_zwm_003 LIKE zwm_003 OCCURS 0 WITH HEADER LINE,
        gt_zwm_004 LIKE zwm_004 OCCURS 0 WITH HEADER LINE,
        gt_zwm_005 LIKE zwm_005 OCCURS 0 WITH HEADER LINE,
        gt_zwm_006 LIKE zwm_006 OCCURS 0 WITH HEADER LINE.

  DATA: f_funcao_sf      TYPE rs38l_fnam,
        fs_control       LIKE ssfctrlop,
        fs_options       TYPE ssfcompop,
        fs_print_options LIKE zwm017,
        lv_user_settings TYPE tdsfflag.


  CLEAR:   gt_zwm_003, gt_zwm_004, gt_zwm_005, gt_zwm_006.
  REFRESH: gt_zwm_003, gt_zwm_004, gt_zwm_005, gt_zwm_006.

*  BREAK-POINT.
  DATA: str1 TYPE string,
        str2 TYPE string,
        str3 TYPE string,
        itab TYPE TABLE OF string,
        text TYPE string.

*VKORG
*TKNUM
*NUMERO
*TRFZN
*BEZEI
*TRANS
*SIGNI
*PAL
*CLIENTES
*PESO
*UN_PESO
*VOLUME
*UN_VOLUME
*TP_CARRO
*DTDIS
*OBSERV

  DATA: wa_dados_aux LIKE gt_dados.
  LOOP AT gt_dados.
    MOVE-CORRESPONDING: gt_dados TO gt_zwm_003,
                        gt_dados TO gt_zwm_004,
                        gt_dados TO gt_zwm_005,
                        gt_dados TO gt_zwm_006.
    CLEAR: gt_zwm_004-pal, gt_zwm_004-volume, gt_zwm_004-peso.
    LOOP AT gt_dados INTO wa_dados_aux
                     WHERE vkorg EQ gt_dados-vkorg
                       AND tknum EQ gt_dados-tknum.
      ADD wa_dados_aux-palto  TO gt_zwm_004-pal.
      ADD wa_dados_aux-volume TO gt_zwm_004-volume.
      ADD wa_dados_aux-peso   TO gt_zwm_004-peso.
    ENDLOOP.

    SPLIT gt_dados-pfalt AT ';' INTO: gt_zwm_006-qtd
                                      gt_zwm_006-arktx,
                                      TABLE itab.
    LOOP AT itab INTO text.
      SPLIT text AT ' - ' INTO: gt_zwm_006-qtd
                                gt_zwm_006-arktx.
      APPEND gt_zwm_006.
    ENDLOOP.
    COLLECT gt_zwm_003.
    READ TABLE gt_zwm_004 WITH KEY tknum = gt_dados-tknum.
    IF sy-subrc NE 0.
      COLLECT gt_zwm_004.
    ENDIF.
    COLLECT gt_zwm_005.
  ENDLOOP.

** Opções de Impressão
***********************************************************************
  fs_options-tdimmed = 'X'.
  fs_options-tdnewid = ''.

*  IF p_imp IS INITIAL.
  lv_user_settings = 'X'.
*  ELSE.
*    fs_options-tddest = p_imp.
*    lv_user_settings = ''.
*  ENDIF.

**  Pede opções de impressão
  CALL FUNCTION 'SSF_SHOW_DIALOG'
    EXPORTING
      user_settings        = lv_user_settings
      output_options       = fs_options
      control_parameters   = fs_control
    IMPORTING
      e_output_options     = fs_options
      e_control_parameters = fs_control
    EXCEPTIONS
      formatting_error     = 1
      internal_error       = 2
      send_error           = 3
      user_canceled        = 4
      OTHERS               = 5.

  IF sy-subrc <> 0.
    IF NOT sy-msgid IS INITIAL AND NOT sy-msgno IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
         DISPLAY LIKE sy-msgty
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    EXIT.
  ENDIF.


  fs_control-no_dialog = 'X'.


** Obtem Função do Smartform
***********************************************************************
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZWM_MAPA_PLANEAMENTO'
    IMPORTING
      fm_name            = f_funcao_sf
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

** Executa o Smartform
***********************************************************************
* SMARTFORM: /1BCDWB/SF00000005
  CALL FUNCTION f_funcao_sf
    EXPORTING
      control_parameters = fs_control
      output_options     = fs_options
      user_settings      = ''
    TABLES
      t_vkorg            = gt_zwm_003
      t_tknum            = gt_zwm_004
      t_refnr            = gt_zwm_005
      t_faltas           = gt_zwm_006
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " PRINT_LAYOUT
