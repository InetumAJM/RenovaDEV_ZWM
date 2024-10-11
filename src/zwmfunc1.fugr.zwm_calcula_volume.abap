FUNCTION zwm_calcula_volume.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"     REFERENCE(ACTUALIZA) LIKE  ZWM_AUX-ACTUALIZA
*"  TABLES
*"      ZPALETE_PICKING STRUCTURE  ZPALETE_PICKING
*"      ITAB_MARA STRUCTURE  ZWM_MARA
*"----------------------------------------------------------------------

  TABLES: tvko.

  CONSTANTS: gc_gewei_base TYPE gewei VALUE 'KG'.

  DATA : volume_maximo_aux  LIKE zwm001-valor,
         volume_maximo      LIKE mara-volum,
         volume_maximo_u    LIKE mara-volum,
         save_index         LIKE sy-tabix,
         last_index         LIKE sy-tabix,
         save_index1        LIKE sy-tabix,
         volume_acumulado   LIKE mara-volum,
         volume_acumulado2  LIKE mara-volum,
         num_palete         TYPE i,
         l_number           LIKE inrdp-tonumber,
         i_pal_picking      TYPE i,
         matnr_pal          LIKE mara-matnr,
         l_vbeln            LIKE lips-vbeln,
         l_total_qtd        LIKE lips-lfimg, ordem TYPE i,
** RL -> INS 04.04.2005 -----------------------------------------------
         l_total_servisan   LIKE lips-lfimg,
         l_refnr            LIKE zwm028-refnr,
         l_ordem            LIKE zwm028-ordem,
         l_index            LIKE sy-tabix,
         l_idx              LIKE sy-tabix,
         l_servisan,
         l_contador         LIKE sy-tabix,
         l_id_servisan      LIKE zwm028-remessa,
         l_remessa          LIKE zwm028-remessa,
         l_number_aux       LIKE inrdp-tonumber,
         l_kunnr            LIKE kna1-kunnr,
         lt_zpalete_picking TYPE TABLE OF zpalete_picking,
         ls_zpalete_picking TYPE zpalete_picking,
         ls_itab_inc_last   TYPE zpalete_picking,
         ls_zwm028          TYPE zwm028,
         lv_sort_processed  TYPE flag,
         lv_breaker         TYPE flag,
         lv_break_pal       TYPE flag,
         lt_zwm062          TYPE SORTED TABLE OF zwm062 WITH UNIQUE KEY kunnr matnr,
         ls_zwm062          TYPE zwm062,
         lv_2spart          TYPE flag,
         lv_2step           TYPE flag.

** RL <- INS 04.04.2005 -----------------------------------------------

  DATA: num_unidades       TYPE i,
        num_unidades_resto TYPE i,
        t_zpalete_picking  LIKE zpalete_picking OCCURS 0 WITH HEADER LINE.

  DATA: itab_aux  LIKE zpalete_picking OCCURS 0 WITH HEADER LINE,
** RL -> DEL 04.04.2005 ------------------------------------------------
*        itab_inc   LIKE zpalete_picking OCCURS 0 WITH HEADER LINE,
** RL <- DEL 04.04.2005 ------------------------------------------------
        itab_comp LIKE zpalete_picking OCCURS 0 WITH HEADER LINE.

  DATA : BEGIN OF zpalete_picking_aux OCCURS 0.
           INCLUDE STRUCTURE zpalete_picking.
** RL -> INS 04.04.2005 ------------------------------------------------
           DATA: servisan.
  DATA: seq TYPE zroutyn_seq.
** RL <- INS 04.04.2005 ------------------------------------------------
  DATA : END OF zpalete_picking_aux.

** RL -> INS 04.04.2005 ------------------------------------------------
  DATA: itab_servisan LIKE zwm040 OCCURS 0 WITH HEADER LINE,
        itab_delete   LIKE zwm040 OCCURS 0 WITH HEADER LINE,
        itab_inc      LIKE zpalete_picking_aux OCCURS 0 WITH HEADER LINE,
        i_not_serv    LIKE zpalete_picking_aux OCCURS 0 WITH HEADER LINE,
        i_serv        LIKE zpalete_picking_aux OCCURS 0 WITH HEADER LINE.

  DATA: lt_zroutyn_t01_i TYPE zroutyn_t01_i OCCURS 0 WITH HEADER LINE.
  DATA: lt_zroutyn_t01_h TYPE zroutyn_t01_h OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF itab_kun OCCURS 0.
  DATA: kunnr_ag LIKE kna1-kunnr.
        INCLUDE STRUCTURE zpalete_picking.
        DATA: END OF itab_kun.
** RL <- INS 04.04.2005 ------------------------------------------------

  DATA: BEGIN OF itab_vbeln OCCURS 0,
          vbeln LIKE zpalete_picking-vbeln,
        END OF itab_vbeln.

  DATA: BEGIN OF itab_refnr OCCURS 0,
          kunag LIKE kna1-kunnr,
          refnr LIKE zwm028-refnr,
        END OF itab_refnr.

  DATA: BEGIN OF itab_serv OCCURS 0,
          kunag       LIKE zpalete_picking-kunag,
          id_servisan LIKE zwm028-remessa,
        END OF itab_serv.

** Quantidade total de paletes por grupo
  DATA : BEGIN OF l_zwm028 OCCURS 0.
           INCLUDE STRUCTURE zwm028.
         DATA : END OF l_zwm028.

** FL -> 16/01/2006 Optimização
  DATA: BEGIN OF it_zwm039 OCCURS 0,
          lgnum LIKE zwm039-lgnum,
          kunnr LIKE zwm039-kunnr,
        END OF it_zwm039.
** FL <- 16/01/2006 Optimização

  DATA lt_likp LIKE likp OCCURS 0 WITH HEADER LINE.
  DATA lt_vbak LIKE vbak OCCURS 0 WITH HEADER LINE.

  CLEAR : save_index.

  FREE: itab_serv, itab_refnr, itab_vbeln, itab_kun.
  CLEAR: itab_serv, itab_refnr, itab_vbeln, itab_kun.

  DATA: wa_zwm026 LIKE zwm026.

  TYPES: BEGIN OF lty_refnr_tknum,
           refnr TYPE lvs_refnr,
           tknum TYPE tknum,
         END OF lty_refnr_tknum.

  DATA: lt_t311        TYPE HASHED TABLE OF t311 WITH UNIQUE KEY refnr,
        lt_t311_2step  TYPE HASHED TABLE OF t311 WITH UNIQUE KEY refnr,
        lt_t311a       TYPE TABLE OF t311a,
        lt_vttp_2step  TYPE TABLE OF vttp,
        lt_refnr_tknum TYPE HASHED TABLE OF lty_refnr_tknum WITH UNIQUE KEY refnr,
        lt_zwm054      TYPE HASHED TABLE OF zwm054 WITH UNIQUE KEY matnr,
        lt_zwm068      TYPE TABLE OF zwm068.

  DATA: ls_t311        TYPE t311,
        ls_t311a       TYPE t311a,
        ls_vttp        TYPE vttp,
        ls_message     TYPE bdcmsgcoll,
        ls_refnr_tknum TYPE lty_refnr_tknum,
        ls_zwm054      TYPE zwm054,
        ls_zwm068      TYPE zwm068.

  DATA: lv_2step_test TYPE flag,
        lv_last_tknum TYPE tknum,
        lv_set_last   TYPE flag,
        lv_force_full TYPE flag,
        lv_vkorg      TYPE vkorg.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 29.05.2012 09:37:12
*  Motivo: Meias Paletes
*--------------------------------------------------------------------*
  DATA: volume_max_mpal_aux      LIKE zwm001-valor,
        volume_max_mpal          LIKE mara-volum,
        volume_max_mpal_u        LIKE mara-volum,
        lv_updater               TYPE sytabix,
        zpalete_picking_back     TYPE TABLE OF zpalete_picking,
        ls_zpalete_picking_back  TYPE zpalete_picking,
        ls_zpalete_picking_first TYPE zpalete_picking,
        ls_zpalete_picking_last  TYPE zpalete_picking,
        lv_tabix                 TYPE sy-tabix,
        lv_vbeln                 TYPE vbeln,
        lv_sort_routyn           TYPE flag,
        lv_lines                 TYPE sytabix,
        lv_brgew_calc            TYPE brgew,
        lv_brgew_max_mpal        TYPE brgew,
        lv_2chang                TYPE flag.


  DATA: matnr_mpal TYPE mara-matnr.

  DATA: ls_mara_mpal TYPE mara.

  FIELD-SYMBOLS: <ls_zpalete_picking>   LIKE zpalete_picking_aux,
                 <ls_zpalete_picking_b> LIKE zpalete_picking.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

** Cálculo do volume por remessa
  PERFORM get_parameter
          USING armazem
                'PICKING'
                'VOLUME_PAL_PICKING'
                volume_maximo_aux.

** Cálculo meia Palete
  PERFORM get_parameter
          USING armazem
                'PICKING'
                'VOLUME_M_PAL_PICKING'
                volume_max_mpal_aux.

** Retirar ao volume maximo o volume da palete
  PERFORM get_parameter
          USING armazem
                'CALCULO_VOLUME'
                'VOLUME_PAL'
                matnr_pal.

** Retirar ao volume maximo o volume da palete
  PERFORM get_parameter
          USING armazem
                'PICKING'
                'PESO_MAX_MPAL_PICK'
                lv_brgew_max_mpal.

** Retirar ao volume maximo o volume da palete
  PERFORM get_parameter
          USING armazem
                'CALCULO_VOLUME'
                'VOLUME_MEIA_PAL'
                matnr_mpal.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = matnr_pal
    IMPORTING
      output = matnr_pal.

  CLEAR: mara.
  SELECT SINGLE * FROM mara WHERE matnr = matnr_pal.

  volume_maximo_aux = volume_maximo_aux - mara-volum.

  MOVE volume_maximo_aux TO volume_maximo.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 29.05.2012 09:44:49
*  Motivo: Meia Palete
*--------------------------------------------------------------------*
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = matnr_mpal
    IMPORTING
      output = matnr_mpal.

  CLEAR: ls_mara_mpal.
  SELECT SINGLE * FROM mara
                  INTO ls_mara_mpal
                  WHERE matnr = matnr_mpal.

  volume_max_mpal_aux =  volume_max_mpal_aux - ls_mara_mpal-volum.
  MOVE volume_max_mpal_aux TO volume_max_mpal.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

** Verificar se a palete tem volume superior ao volume maximo
  CLEAR: num_unidades,
         num_unidades_resto,
         t_zpalete_picking.


  LOOP AT zpalete_picking.
    save_index = sy-tabix.

    CLEAR lv_vkorg.

    SELECT SINGLE *
      FROM likp
      WHERE vbeln = zpalete_picking-vbeln.

    IF sy-subrc = 0.
      lv_vkorg = likp-vkorg.
    ELSE.
** >> RSousa- 02-11-2018 - Routyn
      SELECT SINGLE vkorg
        FROM vbak INTO lv_vkorg
        WHERE vbeln = zpalete_picking-vbeln.
** << RSousa
    ENDIF.

    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = armazem
        i_vbeln  = zpalete_picking-vbeln
      IMPORTING
        e_2chang = lv_2chang
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.


    IF lv_vkorg = 'SER1' AND lv_2chang EQ abap_false.  "likp-vkorg = 'SER1'.
      SELECT SINGLE * FROM tvko WHERE vkorg = lv_vkorg.
      IF tvko-kunnr IS NOT INITIAL.
        zpalete_picking-kunag = tvko-kunnr.
        MODIFY zpalete_picking INDEX save_index.
      ENDIF.
    ELSE.
      IF zpalete_picking-vbeln EQ gc_vbeln_2step_dummy.
        SELECT SINGLE * FROM tvko WHERE vkorg = 'SER1'.
        IF tvko-kunnr IS NOT INITIAL.
          zpalete_picking-kunag = tvko-kunnr.
          MODIFY zpalete_picking INDEX save_index.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.


  LOOP AT zpalete_picking.

    volume_maximo_u   = volume_maximo.
    volume_max_mpal_u = volume_max_mpal.
    CALL FUNCTION 'ZWM_CUSTOMER_MAX_VOLUME'
      EXPORTING
        i_lgnum    = armazem
        i_kunnr    = zpalete_picking-kunnr
      CHANGING
        c_vol_pal  = volume_maximo_u
        c_vol_mpal = volume_max_mpal_u.

    CLEAR itab_aux.
    IF zpalete_picking-uni_incompleta IS INITIAL.
      MOVE-CORRESPONDING zpalete_picking TO itab_aux.
      APPEND itab_aux.
      CONTINUE.
    ENDIF.

    FREE t_zpalete_picking.

    IF zpalete_picking-volum > volume_maximo_u AND
       1 EQ 0. "DS - ROFF Desligado 22/10/2012

*      CLEAR: mara.
*      READ TABLE itab_mara WITH KEY matnr = zpalete_picking-matnr
*                           BINARY SEARCH.
*
*      MOVE-CORRESPONDING itab_mara TO mara.
*
*      num_unidades = volume_maximo DIV mara-volum.
*      num_unidades_resto = zpalete_picking-uni_incompleta -
*                              num_unidades.
**      zpalete_picking-lfimg = num_unidades.
*      zpalete_picking-sub_item = 1.
*      zpalete_picking-uni_incompleta = num_unidades.
*      zpalete_picking-volum =
*                    zpalete_picking-uni_incompleta * mara-volum.
*
*      MOVE-CORRESPONDING zpalete_picking TO itab_aux.
*      APPEND itab_aux.
*
**      zpalete_picking-lfimg = num_unidades_resto.
*      zpalete_picking-sub_item = 2.
*      zpalete_picking-uni_incompleta = num_unidades_resto.
*      zpalete_picking-volum =
*                    zpalete_picking-uni_incompleta * mara-volum.
*
*      CLEAR zpalete_picking-pal_completa.
*      MOVE-CORRESPONDING zpalete_picking TO itab_aux.
*
*      APPEND itab_aux.

    ELSE.
      zpalete_picking-sub_item = 1.
      MOVE-CORRESPONDING zpalete_picking TO itab_aux.
      APPEND itab_aux.
    ENDIF.
  ENDLOOP.
**
  FREE: zpalete_picking.
  CLEAR: zpalete_picking.
  zpalete_picking[] = itab_aux[].
  FREE: itab_aux.
  CLEAR: itab_aux.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 20.03.2015 10:14:40
*  Motivo: Ordenação de Agrupamento por Cliente Material
*--------------------------------------------------------------------*
  SELECT * FROM zwm062
           INTO TABLE lt_zwm062
           WHERE lgnum = armazem AND
                 inact = abap_false.

  CLEAR: lt_zpalete_picking.
  lt_zpalete_picking = zpalete_picking[].

  LOOP AT lt_zpalete_picking ASSIGNING <ls_zpalete_picking_b>.
    READ TABLE lt_zwm062
          WITH TABLE KEY kunnr = <ls_zpalete_picking_b>-kunag
                         matnr = <ls_zpalete_picking_b>-matnr
          TRANSPORTING NO FIELDS.
    CHECK sy-subrc EQ 0.

    CONCATENATE <ls_zpalete_picking_b>-kunag <ls_zpalete_picking_b>-matnr INTO <ls_zpalete_picking_b>-breaker.
  ENDLOOP.

  zpalete_picking[] = lt_zpalete_picking.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


** Ordenação pelas posições fixas do produto
  SORT zpalete_picking BY vbeln    ASCENDING
                          sorlp    ASCENDING
                          posnr    ASCENDING
                          sub_item ASCENDING.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 22.06.2012 10:14:40
*  Motivo: Ordenação de Caso Especial
*--------------------------------------------------------------------*
  lt_zpalete_picking = zpalete_picking[].

  CALL FUNCTION 'ZWM_PALLET_SPECIAL_CASE_SORT'
    IMPORTING
      e_processed        = lv_sort_processed
    CHANGING
      ct_zpalete_picking = lt_zpalete_picking.

  zpalete_picking[] = lt_zpalete_picking.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*




*  zpalete_picking_aux[] = zpalete_picking[].

** RL -> MOD 04.04.2005 ------------------------------------------------
** Alterações tendo em conta as caracteristicas da SERVISAN

*  LOOP AT zpalete_picking.
*      itab_vbeln-vbeln = zpalete_picking-vbeln.
*      COLLECT itab_vbeln.
*      CLEAR: itab_vbeln.
*  ENDLOOP.

** FL -> 16/01/2006 Optimização
  CLEAR:   it_zwm039.
  REFRESH: it_zwm039.

  IF NOT zpalete_picking[] IS INITIAL.
    SELECT lgnum kunnr FROM zwm039
           INTO CORRESPONDING FIELDS OF TABLE it_zwm039
            FOR ALL ENTRIES IN zpalete_picking
          WHERE lgnum EQ armazem
            AND kunnr EQ zpalete_picking-kunag.

    SORT it_zwm039 BY lgnum kunnr.
** FL <- 16/01/2006 Optimização

*********************************
*break roffd.
    CLEAR lt_likp.
    REFRESH lt_likp.

    SELECT * INTO TABLE lt_likp
        FROM likp
            FOR ALL ENTRIES IN zpalete_picking
                WHERE vbeln = zpalete_picking-vbeln.

    SORT lt_likp.
    DELETE ADJACENT DUPLICATES FROM lt_likp COMPARING vbeln.

** Ordem Venda
** >> RSousa- 02-11-2018 - Routyn
    CLEAR lt_vbak.
    REFRESH lt_vbak.

    SELECT * INTO TABLE lt_vbak
        FROM vbak
            FOR ALL ENTRIES IN zpalete_picking
                WHERE vbeln = zpalete_picking-vbeln.

    SORT lt_vbak BY vbeln.
** << RSousa
**--> BMIGUEL
  ENDIF. " DUMP no select lt_likp
**<-- BMIGUEL
*********************************
  LOOP AT zpalete_picking.
** Validar se o Emissor da Ordem é Servisan

** FL -> 16/01/2006 Optimização
*    CLEAR: zwm039.
*    SELECT SINGLE * FROM zwm039
*    WHERE lgnum EQ armazem
*      AND kunnr EQ zpalete_picking-kunag.

    CLEAR: it_zwm039.
    READ TABLE it_zwm039 WITH KEY lgnum = armazem
                                  kunnr = zpalete_picking-kunag.
** FL <- 16/01/2006 Optimização


** Excluir as remessas que pertencem a clientes Servisan
    IF sy-subrc NE 0.
      itab_vbeln-vbeln = zpalete_picking-vbeln.
      COLLECT itab_vbeln.
      CLEAR: itab_vbeln.
      SORT itab_vbeln.
    ELSE.
** Se o emissor da ordem for servisan mas se a organização de vendas
** for RP08 o grupo não é considerado servisan, são marcas brancas
      CLEAR: lt_likp, lt_vbak, lv_vkorg.

      READ TABLE lt_likp WITH KEY vbeln = zpalete_picking-vbeln.
      IF sy-subrc = 0.
        lv_vkorg = lt_likp-vkorg.
      ELSE.
        READ TABLE lt_vbak WITH KEY vbeln = zpalete_picking-vbeln.
        IF sy-subrc = 0.
          lv_vkorg = lt_vbak-vkorg.
        ENDIF.
      ENDIF.

      IF lv_vkorg = 'RP08'. "lt_likp-vkorg = 'RP08'.
        itab_vbeln-vbeln = zpalete_picking-vbeln.
        COLLECT itab_vbeln.
        CLEAR: itab_vbeln.
        SORT itab_vbeln.
      ELSE.
        itab_serv-kunag = zpalete_picking-kunag.
        COLLECT itab_serv.
        CLEAR: itab_serv.
        SORT itab_serv.

        itab_refnr-kunag = zpalete_picking-kunag.
        itab_refnr-refnr = zpalete_picking-refnr.
        COLLECT itab_refnr.
        CLEAR: itab_refnr.
        SORT itab_refnr.
      ENDIF.
    ENDIF.

    IF NOT zpalete_picking-uni_incompleta IS INITIAL.
      MOVE-CORRESPONDING zpalete_picking TO itab_inc.
      APPEND itab_inc.
      CLEAR: itab_inc.
    ELSE.
      MOVE-CORRESPONDING zpalete_picking TO itab_comp.
      APPEND itab_comp.
      CLEAR: itab_comp.
    ENDIF.
  ENDLOOP.
** RL <- MOD 04.04.2005 ------------------------------------------------

****** NÃO SERVISAN *******

  FREE: zpalete_picking_aux.
  CLEAR: zpalete_picking_aux.
  IF NOT itab_inc[] IS INITIAL.
    SELECT * FROM zwm068
       INTO TABLE lt_zwm068
       FOR ALL ENTRIES IN itab_inc
       WHERE lgnum = armazem
         AND matnr = itab_inc-matnr.
  ENDIF.

** ACM -> 2/06/2022
** Verifica quais os clientes que não aceitam Meias Paletes
  SELECT *
    FROM zwm072
    INTO TABLE @DATA(lt_zwm072)
    FOR ALL ENTRIES IN @itab_inc
    WHERE lgnum = @armazem
      AND kunnr = @itab_inc-kunnr.
  IF sy-subrc = 0 AND lt_zwm072 IS NOT INITIAL.
    SORT lt_zwm072 BY lgnum kunnr.
  ENDIF.
** ACM -> 2/06/2022

  LOOP AT itab_inc.
    save_index = sy-tabix.

    volume_maximo_u   = volume_maximo.
    volume_max_mpal_u = volume_max_mpal.
    CALL FUNCTION 'ZWM_CUSTOMER_MAX_VOLUME'
      EXPORTING
        i_lgnum    = armazem
        i_kunnr    = itab_inc-kunnr
      CHANGING
        c_vol_pal  = volume_maximo_u
        c_vol_mpal = volume_max_mpal_u.
    .


    CLEAR ls_zwm068.
    READ TABLE lt_zwm068
          INTO ls_zwm068
          WITH KEY lgnum = armazem
                   matnr = itab_inc-matnr.
    IF sy-subrc = 0.

      MOVE-CORRESPONDING itab_inc TO zpalete_picking_aux.

      LOOP AT itab_serv WHERE kunag = itab_inc-kunag.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        zpalete_picking_aux-servisan = 'X'.
      ENDIF.

*** ACM -> 2/06/2022
*      READ TABLE lt_zwm072 WITH KEY kunnr = itab_inc-kunnr TRANSPORTING NO FIELDS.
*      IF sy-subrc = 0.
*        zpalete_picking_aux-pal_picking = '1'.
*      ELSE.
*        IF zpalete_picking_aux-vol_aumulado_rea <= volume_max_mpal_u AND
*           zpalete_picking_aux-peso_acumulado   <= lv_brgew_max_mpal.
*          zpalete_picking_aux-pallet_type = 'M'.
*          zpalete_picking_aux-pal_picking = '0.5'.
*        ELSE.
*          zpalete_picking_aux-pal_picking = '1'.
*        ENDIF.
*      ENDIF.
*** ACM -> 2/06/2022

      IF zpalete_picking_aux-vol_aumulado_rea <= volume_max_mpal_u AND
         zpalete_picking_aux-peso_acumulado   <= lv_brgew_max_mpal.
        READ TABLE lt_zwm072 WITH KEY kunnr = itab_inc-kunnr TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          zpalete_picking_aux-pal_picking = '1'.
        ELSE.
          zpalete_picking_aux-pallet_type = 'M'.
          zpalete_picking_aux-pal_picking = '0.5'.
        ENDIF.
      ELSE.
        zpalete_picking_aux-pal_picking = '1'.
      ENDIF.

      zpalete_picking_aux-pal_incompleta = '1'.
      APPEND zpalete_picking_aux.
      CLEAR: zpalete_picking_aux.
      DELETE itab_inc INDEX save_index.
    ENDIF.

  ENDLOOP.

  DATA pal_count TYPE sytabix.

  CLEAR: save_index.
  LOOP AT itab_vbeln.
    CLEAR: volume_acumulado, num_palete, l_vbeln, pal_count.
    LOOP AT itab_inc
      WHERE vbeln = itab_vbeln-vbeln.

      CLEAR: lv_break_pal.

      lv_tabix   = sy-tabix.
      last_index = lv_tabix - 1.

      volume_maximo_u   = volume_maximo.
      volume_max_mpal_u = volume_max_mpal.
      CALL FUNCTION 'ZWM_CUSTOMER_MAX_VOLUME'
        EXPORTING
          i_lgnum    = armazem
          i_kunnr    = itab_inc-kunnr
        CHANGING
          c_vol_pal  = volume_maximo_u
          c_vol_mpal = volume_max_mpal_u.

      CLEAR: ls_itab_inc_last.
      IF last_index > 0.
        READ TABLE itab_inc
              INTO ls_itab_inc_last
              INDEX last_index.

        IF ls_itab_inc_last-vbeln <> itab_vbeln-vbeln.
          CLEAR: ls_itab_inc_last.
        ENDIF.
      ENDIF.

      CHECK itab_inc-pal_picking IS INITIAL.

      volume_acumulado = volume_acumulado + itab_inc-volum.

      IF volume_acumulado < volume_maximo_u.
        pal_count = pal_count + 1.
        itab_inc-volum_acumulado = volume_acumulado.
        MODIFY itab_inc INDEX lv_tabix.
        CONTINUE.
      ELSEIF volume_acumulado > volume_maximo_u.
        volume_acumulado = itab_inc-volum.

        itab_inc-volum_acumulado = volume_acumulado.

        IF pal_count > 0." Tem mais que uma palete... Resolve anterior
          IF NOT ls_itab_inc_last IS INITIAL.
            "Se tiver Palete anterior termina a palete
            ls_itab_inc_last-pal_incompleta = 1.
            MODIFY itab_inc FROM ls_itab_inc_last INDEX last_index.
          ENDIF.
        ELSE.
          itab_inc-pal_incompleta = 1.
        ENDIF.
        itab_inc-volum_acumulado = volume_acumulado.
        MODIFY itab_inc INDEX lv_tabix.

        IF volume_acumulado >= volume_maximo_u." Se este volume já for superior, não iniciamos uma nova palete
          itab_inc-pal_incompleta = 1.
          MODIFY itab_inc INDEX lv_tabix.
          CLEAR pal_count.
          CLEAR volume_acumulado.
        ELSE.
          pal_count = 1.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF sy-subrc EQ 0.
      "Termina sempre ultima linha
      READ TABLE itab_inc
           INDEX lv_tabix.

      itab_inc-pal_incompleta = 1.
      MODIFY itab_inc INDEX lv_tabix.
      CLEAR: pal_count.
    ENDIF.

**    IF save_index > 0.
**      READ TABLE itab_inc
**           INDEX save_index.
**
**      itab_inc-pal_incompleta = 1.
**      MODIFY itab_inc INDEX save_index.
**    ENDIF.
  ENDLOOP.

** RL -> INS 04.04.2005 ------------------------------------------------
****** SERVISAN *******
*  break roffd.

  IF NOT itab_serv[] IS INITIAL.

    SORT itab_inc BY kunag sorlp.
    SORT itab_serv BY kunag.

    CLEAR save_index1.

** Clientes Servisan
    LOOP AT itab_serv.
      l_contador = sy-tabix.

      volume_maximo_u   = volume_maximo.
      volume_max_mpal_u = volume_max_mpal.
      CALL FUNCTION 'ZWM_CUSTOMER_MAX_VOLUME'
        EXPORTING
          i_lgnum    = armazem
          i_kunnr    = itab_serv-kunag
        CHANGING
          c_vol_pal  = volume_maximo_u
          c_vol_mpal = volume_max_mpal_u.

      IF actualiza = 'X'.

        CLEAR: l_id_servisan.

** Verificar se já existe algum ID
        READ TABLE itab_refnr WITH KEY kunag = itab_serv-kunag
                              BINARY SEARCH.

        FREE: itab_delete.
        CLEAR: itab_delete.

        CLEAR: zwm040.
        SELECT * FROM zwm040
        INTO CORRESPONDING FIELDS OF TABLE itab_delete
        WHERE lgnum EQ armazem
          AND refnr EQ itab_refnr-refnr
          AND kunnr EQ itab_serv-kunag.

        DELETE zwm040 FROM TABLE itab_delete.
        COMMIT WORK AND WAIT.

        CLEAR: zwm028.
        SELECT * FROM zwm028
        WHERE lgnum    EQ armazem
          AND refnr    EQ itab_refnr-refnr
          AND servisan EQ 'X'
          AND emissor  EQ itab_serv-kunag.
          l_id_servisan = zwm028-remessa.
          EXIT.
        ENDSELECT.

        IF sy-subrc NE 0.

** Obter o ID para o Cliente
          CLEAR: l_number_aux, l_id_servisan.

          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '06'
              object                  = 'LVS_LENUM'
              quantity                = '1'
            IMPORTING
              number                  = l_number_aux
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = l_number_aux
            IMPORTING
              output = l_number_aux.

          l_id_servisan = l_number_aux+10(10).
        ENDIF.

        itab_serv-id_servisan = l_id_servisan.

        MODIFY itab_serv INDEX l_contador.
      ENDIF.

** Todas as Remessas do Emissor
      CLEAR: volume_acumulado, num_palete, l_kunnr.

      LOOP AT itab_inc WHERE kunag = itab_serv-kunag.
        CLEAR: save_index, save_index1.
        save_index = sy-tabix.

        itab_inc-servisan = 'X'.
        MODIFY itab_inc INDEX save_index.
*        itab_dados_serv-vbeln = itab_inc-vbeln.
*        itab_dados_serv-posnr = itab_inc-posnr.

        volume_acumulado = volume_acumulado + itab_inc-volum.

        IF volume_acumulado <= volume_maximo_u.
          CLEAR save_index1.
          itab_inc-pal_incompleta = 1.
          itab_inc-volum_acumulado = volume_acumulado.
          save_index1 = save_index.
*          l_vbeln = itab_inc-vbeln.
          l_kunnr = itab_inc-kunag.
          MODIFY itab_inc INDEX save_index1.
          CLEAR : itab_inc.

** Verifica se existe alguma linha imediatamente anterior que possua
** um volume inferior ao máximo por palete e limpa a palete incompleta
          save_index1 = save_index1 - 1.
          READ TABLE itab_inc INDEX save_index1.
          IF sy-subrc EQ 0.
            CHECK l_kunnr EQ itab_inc-kunag.
            CLEAR itab_inc-pal_incompleta.
            MODIFY itab_inc INDEX save_index1.
            CLEAR : itab_inc, save_index1.
          ENDIF.
        ELSEIF volume_acumulado > volume_maximo_u.
** Actualizar posição actual
*          volume_acumulado2 = volume_acumulado -
*                              itab_inc-volum.

          volume_acumulado = itab_inc-volum.
          itab_inc-pal_incompleta = 1.
          itab_inc-volum_acumulado = volume_acumulado."volume_acumulado.
          MODIFY itab_inc INDEX save_index.


** Actualizar posição anterior
*          itab_inc-volum_acumulado = volume_acumulado2.
          save_index = save_index - 1.
          IF NOT save_index IS INITIAL.
            READ TABLE itab_inc INDEX save_index.
            IF sy-subrc = 0.

*              itab_inc-volum_acumulado = volume_acumulado2.
              itab_inc-pal_incompleta = 1.
              MODIFY itab_inc INDEX save_index.
              CLEAR itab_inc.
            ENDIF.
          ENDIF. "save_index <> 0

          IF NOT save_index1 IS INITIAL.
            MODIFY itab_inc INDEX save_index1.
            CLEAR : itab_inc, save_index1.
          ENDIF.
        ENDIF. "volume_acumulado < volume_maximo
      ENDLOOP.

    ENDLOOP.
  ENDIF.
** RL <- INS 04.04.2005 ------------------------------------------------

** RL -> MOD 04.04.2005 ------------------------------------------------
****** SERVISAN *******
** Garantir que os registos que não sejam da SERVISAN são ordenados
** de modo diferente

*  zpalete_picking_aux[] = itab_comp[].
*  LOOP AT itab_inc.
*    MOVE-CORRESPONDING itab_inc TO zpalete_picking_aux.
*    APPEND zpalete_picking_aux.
*    CLEAR: zpalete_picking_aux.
*  ENDLOOP.
*  SORT zpalete_picking_aux BY vbeln sorlp posnr sub_item ASCENDING.

*  break roffd.

*  zpalete_picking_aux[] = itab_comp[].


  LOOP AT itab_comp.
    MOVE-CORRESPONDING itab_comp TO zpalete_picking_aux.
    APPEND zpalete_picking_aux.
  ENDLOOP.

  LOOP AT itab_inc WHERE servisan NE 'X'.
    MOVE-CORRESPONDING itab_inc TO zpalete_picking_aux.
    APPEND zpalete_picking_aux.
    CLEAR: zpalete_picking_aux.
  ENDLOOP.

  IF lv_sort_processed EQ abap_false.
    SORT zpalete_picking_aux BY vbeln sorlp posnr sub_item ASCENDING.
  ENDIF.

  CLEAR: l_servisan.

  LOOP AT itab_inc WHERE servisan EQ 'X'.
    MOVE-CORRESPONDING itab_inc TO zpalete_picking_aux.
    APPEND zpalete_picking_aux.
    CLEAR: zpalete_picking_aux.
*    l_servisan = 'X'.
  ENDLOOP.

  IF NOT itab_serv[] IS INITIAL.
    l_servisan = 'X'.
  ENDIF.

*** ACM - (07/06/2022) - Picking com Paletização Especial
***********************************************************************
*
*  IF NOT zpalete_picking_aux[] IS INITIAL.
*
*    SELECT *
*     FROM zwm031
*     INTO TABLE @DATA(lt_zwm031)
*     FOR ALL ENTRIES IN @zpalete_picking_aux
*     WHERE lgnum = @armazem
*      AND kunnr = @zpalete_picking_aux-kunnr
*      AND matnr = @zpalete_picking_aux-matnr.
*    IF sy-subrc = 0 AND lt_zwm031 IS NOT INITIAL.
*
*      SORT lt_zwm031 BY lgnum kunnr matnr.
*
*      SELECT matnr, volum
*        FROM mara
*        INTO TABLE @DATA(lt_mara)
*        FOR ALL ENTRIES IN @zpalete_picking_aux
*        WHERE matnr = @zpalete_picking_aux-matnr.
*
*    ENDIF.
*
*    LOOP AT zpalete_picking_aux WHERE uni_incompleta IS NOT INITIAL.
*
*      DATA(lv_index) = sy-tabix.
*
*      READ TABLE lt_zwm031 INTO DATA(ls_zwm031) WITH KEY kunnr = zpalete_picking_aux-kunnr
*                                                         matnr = zpalete_picking_aux-matnr.
*      IF sy-subrc = 0.
*
*        IF zpalete_picking_aux-uni_incompleta > ls_zwm031-unporpal.
*
*          DATA(lv_qtd_aux) = zpalete_picking_aux-uni_incompleta.
*
*          READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = zpalete_picking_aux-matnr.
*          IF sy-subrc = 0.
*            "zpalete_picking_aux-volum = ( zpalete_picking_aux-uni_incompleta - ls_zwm031-unporpal ) * ls_mara-volum.
*            zpalete_picking_aux-uni_incompleta = ls_zwm031-unporpal.
*            zpalete_picking_aux-volum = zpalete_picking_aux-uni_incompleta * ls_mara-volum.
*            MODIFY zpalete_picking_aux INDEX lv_index.
*
*            SUBTRACT ls_zwm031-unporpal FROM lv_qtd_aux.
*
*            WHILE lv_qtd_aux > 0.
*
*              CLEAR: zpalete_picking_aux-pal_completa,
*                     zpalete_picking_aux-volum_acumulado.
*
*              IF lv_qtd_aux > ls_zwm031-unporpal.
*                zpalete_picking_aux-uni_incompleta = ls_zwm031-unporpal.
*              ELSE.
*                zpalete_picking_aux-uni_incompleta = lv_qtd_aux.
*              ENDIF.
*
*              zpalete_picking_aux-sub_item = 99.
*              zpalete_picking_aux-pal_incompleta = '1'.
*              "zpalete_picking_aux-uni_incompleta = lv_qtd_aux.
*              zpalete_picking_aux-volum = zpalete_picking_aux-uni_incompleta * ls_mara-volum.
*              APPEND zpalete_picking_aux.
*
*              SUBTRACT zpalete_picking_aux-uni_incompleta FROM lv_qtd_aux.
*
*            ENDWHILE.
*
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDIF.
*
*** ACM - (07/06/2022) - Picking com Paletização Especial
************************************************************************

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 22.06.2012 12:54:53
*  Motivo: Total de Linhas
*--------------------------------------------------------------------*
  DESCRIBE TABLE zpalete_picking_aux LINES lv_lines.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 01.10.2012 17:24:12
*  Motivo: Materiais de Paletes Inteiras
*--------------------------------------------------------------------*
  IF NOT zpalete_picking_aux[] IS INITIAL.
    SELECT * FROM zwm054
       INTO TABLE lt_zwm054
       FOR ALL ENTRIES IN zpalete_picking_aux
       WHERE matnr = zpalete_picking_aux-matnr.
  ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

  SORT itab_inc BY refnr sammg kunag vbeln.

** ACM -> 2/06/2022
** Verifica quais os clientes que não aceitam Meias Paletes
  SELECT *
    FROM zwm072
    INTO TABLE @DATA(lt_zwm072_zpalete)
    FOR ALL ENTRIES IN @zpalete_picking_aux
    WHERE lgnum = @armazem
      AND kunnr = @zpalete_picking_aux-kunnr.
  IF sy-subrc = 0 AND lt_zwm072 IS NOT INITIAL.
    SORT lt_zwm072_zpalete BY lgnum kunnr.
  ENDIF.
** ACM -> 2/06/2022

** RL <- MOD 04.04.2005 ------------------------------------------------
  LOOP AT zpalete_picking_aux.
    save_index = sy-tabix.

    volume_maximo_u   = volume_maximo.
    volume_max_mpal_u = volume_max_mpal.
    CALL FUNCTION 'ZWM_CUSTOMER_MAX_VOLUME'
      EXPORTING
        i_lgnum    = armazem
        i_kunnr    = zpalete_picking_aux-kunnr
      CHANGING
        c_vol_pal  = volume_maximo_u
        c_vol_mpal = volume_max_mpal_u.

    IF zpalete_picking_aux-pal_completa IS INITIAL.
      READ TABLE lt_zwm068 INTO ls_zwm068
          WITH KEY matnr = zpalete_picking_aux-matnr.
      CHECK sy-subrc <> 0.
    ENDIF.

** RL -> MOD 04.04.2005 ------------------------------------------------
** Valida todos as remessas servisan
    CLEAR: itab_inc.
    READ TABLE itab_inc WITH KEY refnr = zpalete_picking_aux-refnr
                                 sammg = zpalete_picking_aux-sammg
                                 kunag = zpalete_picking_aux-kunag
                                 vbeln = zpalete_picking_aux-vbeln
                          BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF itab_inc-servisan = 'X'.
        zpalete_picking_aux-servisan = 'X'.
      ELSE.
        CLEAR: zpalete_picking_aux-servisan.
      ENDIF.
    ELSE.
      READ TABLE itab_serv WITH KEY kunag = zpalete_picking_aux-kunag
                              BINARY SEARCH.
      IF sy-subrc EQ 0.
        zpalete_picking_aux-servisan = 'X'.
      ENDIF.
    ENDIF.
** RL <- MOD 04.04.2005 ------------------------------------------------

    CLEAR: mara.
    READ TABLE itab_mara WITH KEY matnr = zpalete_picking_aux-matnr
                         BINARY SEARCH.

    MOVE-CORRESPONDING itab_mara TO mara.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 01.10.2012 16:39:34
*  Motivo: Calcula Pesos
*--------------------------------------------------------------------*
    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = zpalete_picking_aux-matnr
        i_in_me              = zpalete_picking_aux-meins
        i_out_me             = gc_gewei_base
        i_menge              = zpalete_picking_aux-lfimg
      IMPORTING
        e_menge              = zpalete_picking_aux-peso
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.

    zpalete_picking_aux-gewei = gc_gewei_base.

    lv_brgew_calc = lv_brgew_calc + zpalete_picking_aux-peso.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 29.05.2012 09:55:43
*  Motivo: Mantem Volume Acumulado e Calculo de Meias paletes
*--------------------------------------------------------------------*


    IF lv_updater EQ 0.
      lv_updater = save_index.
    ENDIF.

    CLEAR ls_zwm054.
    READ TABLE lt_zwm054
          INTO ls_zwm054
          WITH TABLE KEY matnr = zpalete_picking_aux-matnr.

    IF sy-subrc EQ 0.
      lv_force_full = abap_true.
    ENDIF.

    IF zpalete_picking_aux-pal_completa > 0.
      CLEAR: lv_force_full, lv_brgew_calc.
    ENDIF.

    IF zpalete_picking_aux-pal_incompleta > 0.

      zpalete_picking_aux-vol_aumulado_rea = zpalete_picking_aux-volum_acumulado.
      zpalete_picking_aux-peso_acumulado   = lv_brgew_calc.
      CLEAR lv_brgew_calc.

*** ACM -> 2/06/2022
*      READ TABLE lt_zwm072_zpalete WITH KEY kunnr = zpalete_picking_aux-kunnr TRANSPORTING NO FIELDS.
*      IF sy-subrc = 0.
*        zpalete_picking_aux-pal_picking = '1'.
*        CLEAR: lv_updater.
*      ELSE.
*        IF ( zpalete_picking_aux-pal_incompleta EQ 1 OR save_index = lv_lines ) AND
*            zpalete_picking_aux-vol_aumulado_rea <= volume_max_mpal_u AND
*            zpalete_picking_aux-peso_acumulado <= lv_brgew_max_mpal AND
*            lv_force_full IS INITIAL.
*
*          LOOP AT zpalete_picking_aux ASSIGNING <ls_zpalete_picking> FROM lv_updater TO save_index.
*            <ls_zpalete_picking>-pallet_type = 'M'.
*          ENDLOOP.
*
*          zpalete_picking_aux-pallet_type = 'M'.
*          zpalete_picking_aux-pal_picking = '0.5'.
*
*          CLEAR: lv_updater.
*        ELSE.
*          zpalete_picking_aux-pal_picking = '1'.
*          CLEAR: lv_updater.
*        ENDIF.
*      ENDIF.
*** ACM -> 2/06/2022

      IF ( zpalete_picking_aux-pal_incompleta EQ 1 OR save_index = lv_lines ) AND
         zpalete_picking_aux-vol_aumulado_rea <= volume_max_mpal_u AND
         zpalete_picking_aux-peso_acumulado <= lv_brgew_max_mpal AND
         lv_force_full IS INITIAL.

        READ TABLE lt_zwm072_zpalete WITH KEY kunnr = zpalete_picking_aux-kunnr TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          zpalete_picking_aux-pal_picking = '1'.
          CLEAR: lv_updater.
        ELSE.
          LOOP AT zpalete_picking_aux ASSIGNING <ls_zpalete_picking> FROM lv_updater TO save_index.
            <ls_zpalete_picking>-pallet_type = 'M'.
          ENDLOOP.

          zpalete_picking_aux-pallet_type = 'M'.
          zpalete_picking_aux-pal_picking = '0.5'.

          CLEAR: lv_updater.
        ENDIF.
      ELSE.
        zpalete_picking_aux-pal_picking = '1'.
        CLEAR: lv_updater.
      ENDIF.

      CLEAR: lv_force_full.

    ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

    zpalete_picking_aux-volum_acumulado =
        ( zpalete_picking_aux-lfimg * mara-volum ).
    MODIFY zpalete_picking_aux INDEX save_index.
  ENDLOOP.

** RL -> MOD 16.05.2005
  FREE: zpalete_picking.
  CLEAR: zpalete_picking.

  LOOP AT zpalete_picking_aux.
    MOVE-CORRESPONDING zpalete_picking_aux TO zpalete_picking.
    APPEND zpalete_picking.
  ENDLOOP.
** RL <- MOD 16.05.2005

** Só efectua actualização se a flag estiver marcada
**********************************************************************
  CHECK NOT actualiza IS INITIAL.

** Actualização na tabela de mapeamento de destinos com remessas com a
** quantidade de paletes completas por grupo
  CLEAR: l_total_qtd, ordem.

*  break roffd.
  SORT zpalete_picking_aux BY sammg
                              lfdat DESCENDING
                              lfuhr DESCENDING
                              vbeln.

** RL -> MOD 04.04.2005 ------------------------------------------------
*  LOOP AT zpalete_picking_aux.
*    AT NEW vbeln.
*      SUM.
***    Ordem de Carga do camiao
*      ordem = ordem + 1.
*      l_zwm028-ordem = ordem.
*      l_total_qtd = zpalete_picking_aux-pal_completa +
*                    zpalete_picking_aux-pal_incompleta.
*
*      MOVE armazem TO l_zwm028-lgnum.
*      MOVE zpalete_picking_aux-refnr TO l_zwm028-refnr.
*      MOVE zpalete_picking_aux-vbeln TO l_zwm028-remessa.
*      MOVE l_total_qtd TO l_zwm028-total_paletes.
*** Bloqueio total
*      SELECT SINGLE tknum INTO l_zwm028-transporte
*          FROM vttp
*              WHERE vbeln = zpalete_picking_aux-vbeln.
*      l_zwm028-zlock = '1'.
*      APPEND l_zwm028.
*
*    ENDAT.
*
*    CLEAR : zpalete_picking,
*            l_total_qtd.
*  ENDLOOP.

  FREE: i_not_serv, i_serv.
  CLEAR: i_not_serv, i_serv.

  LOOP AT zpalete_picking_aux.
    IF zpalete_picking_aux-servisan NE 'X'.
      MOVE-CORRESPONDING zpalete_picking_aux TO i_not_serv.
      APPEND i_not_serv.
      CLEAR: i_not_serv.
    ELSE.
      MOVE-CORRESPONDING zpalete_picking_aux TO i_serv.
      APPEND i_serv.
      CLEAR: i_serv.
    ENDIF.
  ENDLOOP.

  LOOP AT i_not_serv.
    l_index = sy-tabix.

    AT NEW vbeln.
      READ TABLE i_not_serv INDEX l_index.
      SUM.
**    Ordem de Carga do camiao
      ordem = ordem + 1.
      l_zwm028-ordem = ordem.

      l_total_qtd = i_not_serv-pal_completa +
                    ceil( i_not_serv-pal_incompleta ).

      MOVE armazem TO l_zwm028-lgnum.
      MOVE i_not_serv-refnr TO l_zwm028-refnr.
      MOVE i_not_serv-vbeln TO l_zwm028-remessa.

      SELECT SINGLE *
         FROM zwm028 INTO ls_zwm028
         WHERE lgnum   = l_zwm028-lgnum
         AND   refnr   = l_zwm028-refnr
         AND   remessa = l_zwm028-remessa.

      IF sy-subrc <> 0.
        l_zwm028-zlock = '1'.
      ELSE.
        l_zwm028 = ls_zwm028.
      ENDIF.

      MOVE l_total_qtd TO l_zwm028-total_paletes.

      SELECT SINGLE tknum INTO l_zwm028-transporte
          FROM vttp
              WHERE vbeln = i_not_serv-vbeln.

      l_zwm028-emissor = i_not_serv-kunag.

      APPEND l_zwm028.
      CLEAR: l_zwm028.
    ENDAT.

    CLEAR: zpalete_picking, l_total_qtd.
  ENDLOOP.
** RL <- MOD 04.04.2005 ------------------------------------------------

** RL -> INS 04.04.2005 ------------------------------------------------
** Actualiza Registo na Tabela ZWM040, pq existem registos para
** clientes com paletização especial
*  break roffd.

  IF l_servisan = 'X'.

    CLEAR: l_total_servisan, itab_servisan, l_number_aux.
    FREE: itab_servisan.

*    CALL FUNCTION 'NUMBER_GET_NEXT'
*      EXPORTING
*        nr_range_nr             = '06'
*        object                  = 'LVS_LENUM'
*        quantity                = '1'
*      IMPORTING
*        number                  = l_number_aux
*      EXCEPTIONS
*        interval_not_found      = 1
*        number_range_not_intern = 2
*        object_not_found        = 3
*        quantity_is_0           = 4
*        quantity_is_not_1       = 5
*        interval_overflow       = 6
*        buffer_overflow         = 7
*        OTHERS                  = 8.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = l_number_aux
*      IMPORTING
*        output = l_number_aux.
*
*    l_id_servisan = l_number_aux+10(10).

*    break roffd.

    SORT itab_serv BY kunag.

    LOOP AT i_serv.
      MOVE-CORRESPONDING i_serv TO itab_kun.
      itab_kun-kunnr_ag = i_serv-kunag.
      APPEND itab_kun.
      CLEAR: itab_kun.

      READ TABLE itab_serv WITH KEY kunag = i_serv-kunag
                                BINARY SEARCH.

      itab_servisan-id_servisan = itab_serv-id_servisan.
      itab_servisan-kunnr = i_serv-kunag.
** Ordem de Carga do camião
      itab_servisan-remessa = i_serv-vbeln.
      MOVE armazem TO itab_servisan-lgnum.
      MOVE i_serv-refnr TO itab_servisan-refnr.
      COLLECT itab_servisan.
      CLEAR: itab_servisan.
      SORT itab_servisan.
    ENDLOOP.

    CLEAR: l_idx.

    SORT itab_kun BY kunnr_ag.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 22.05.2012 16:08:41
*  Motivo: Documentos de Transporte em Picking 2 Passos
*--------------------------------------------------------------------*
    DO 1 TIMES.
      CHECK NOT itab_kun[] IS INITIAL.

      SELECT * FROM t311
         INTO TABLE lt_t311
         FOR ALL ENTRIES IN itab_kun
         WHERE lgnum = armazem AND
               refnr = itab_kun-refnr.

      CHECK sy-subrc EQ 0.

      LOOP AT lt_t311 INTO ls_t311.
        CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
          EXPORTING
            is_t311 = ls_t311
          IMPORTING
            e_2step = lv_2step_test
          EXCEPTIONS
            error   = 1
            OTHERS  = 2.

        CHECK lv_2step_test EQ abap_true.

        INSERT ls_t311 INTO TABLE lt_t311_2step.
      ENDLOOP.

      CHECK NOT lt_t311_2step IS INITIAL.

      SELECT * FROM t311a
         INTO TABLE lt_t311a
         FOR ALL ENTRIES IN lt_t311_2step
         WHERE lgnum = lt_t311_2step-lgnum AND
               refnr = lt_t311_2step-refnr.

      CHECK NOT lt_t311a IS INITIAL.
      SORT lt_t311a BY refnr.

      SELECT * FROM vttp
         INTO TABLE lt_vttp_2step
         FOR ALL ENTRIES IN lt_t311a
         WHERE vbeln = lt_t311a-rbnum.

      SORT lt_vttp_2step BY vbeln.
    ENDDO.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

    LOOP AT itab_kun.
      CLEAR lv_set_last.

      l_index = sy-tabix.

      AT NEW kunnr_ag.
        READ TABLE itab_kun INDEX l_index.
        SUM.
** Ordem de Carga do camião
        l_total_qtd = itab_kun-pal_completa +
                      ceil( itab_kun-pal_incompleta ).
      ENDAT.

      l_total_servisan = l_total_servisan + l_total_qtd.
      CLEAR: l_total_qtd, l_remessa.

      AT END OF kunnr_ag.
*        break roffd.

        READ TABLE itab_kun INDEX l_index.

** O processamento é feito apenas por 1 grupo
        DESCRIBE TABLE l_zwm028 LINES l_idx.

        READ TABLE l_zwm028 INDEX l_idx.

        IF sy-subrc EQ 0.
          l_ordem   = l_zwm028-ordem.
          l_refnr   = l_zwm028-refnr.
          l_remessa = l_zwm028-remessa.
        ELSE.
*          l_ordem   = '01'.
          l_refnr   = itab_kun-refnr.
          l_remessa = itab_kun-vbeln.
        ENDIF.

** Obter o ID por cliente
        READ TABLE itab_serv WITH KEY kunag = itab_kun-kunag
                             BINARY SEARCH.

        CLEAR: l_zwm028.

        CLEAR: zwm028.
        SELECT SINGLE * FROM zwm028
        WHERE lgnum   EQ armazem
          AND refnr   EQ l_refnr
          AND remessa EQ itab_serv-id_servisan.

        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING zwm028 TO l_zwm028.
        ENDIF.
** Transporte

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 22.05.2012 16:27:32
*  Motivo: Picking 2 Passos
*--------------------------------------------------------------------*
        READ TABLE lt_t311_2step
              WITH TABLE KEY refnr = l_refnr
              TRANSPORTING NO FIELDS.

        IF sy-subrc EQ 0.

          READ TABLE lt_t311a
            WITH KEY refnr = l_refnr
            BINARY SEARCH
            TRANSPORTING NO FIELDS.

          LOOP AT lt_t311a INTO ls_t311a FROM sy-tabix.
            IF ls_t311a-refnr <> l_refnr.
              EXIT.
            ENDIF.

            CLEAR ls_refnr_tknum.
            READ TABLE lt_refnr_tknum
                  INTO ls_refnr_tknum
                  WITH TABLE KEY refnr = ls_t311a-refnr.

            IF sy-subrc EQ 0.
              lv_last_tknum = ls_refnr_tknum-tknum.
              EXIT.
            ENDIF.

            CLEAR ls_vttp.
            READ TABLE lt_vttp_2step
                  INTO ls_vttp
                  WITH KEY vbeln = ls_t311a-rbnum
                  BINARY SEARCH.

            IF lv_set_last IS INITIAL.
              lv_last_tknum = ls_vttp-tknum.
              lv_set_last = abap_true.
            ENDIF.

            IF ls_vttp-tknum <> lv_last_tknum.
              CLEAR lv_last_tknum.
              EXIT.
            ENDIF.
          ENDLOOP.

          l_zwm028-transporte = lv_last_tknum.

          IF ls_refnr_tknum IS INITIAL.
            ls_refnr_tknum-refnr = l_refnr.
            ls_refnr_tknum-tknum = lv_last_tknum.
            INSERT ls_refnr_tknum INTO TABLE lt_refnr_tknum.
          ENDIF.

        ELSE.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

          SELECT SINGLE tknum INTO l_zwm028-transporte
                   FROM vttp
                       WHERE vbeln = itab_kun-vbeln.
        ENDIF.

**
        l_zwm028-lgnum         = armazem.
        l_zwm028-refnr         = l_refnr.
        l_zwm028-remessa       = itab_serv-id_servisan.

        SELECT SINGLE *
          FROM zwm028 INTO ls_zwm028
          WHERE lgnum   = l_zwm028-lgnum
          AND   refnr   = l_zwm028-refnr
          AND   remessa = l_zwm028-remessa.

        IF sy-subrc <> 0.
          l_zwm028-zlock = '1'.
        ELSE.
          l_zwm028 = ls_zwm028.
        ENDIF.

        l_zwm028-total_paletes = l_total_servisan.

        IF l_remessa NE itab_serv-id_servisan.
          l_zwm028-ordem = l_ordem + 1.
        ELSE.
          l_zwm028-ordem = l_ordem.
        ENDIF.

        l_zwm028-servisan      = 'X'.
        l_zwm028-emissor       = itab_kun-kunag.

        APPEND l_zwm028.
        CLEAR: l_zwm028, l_total_servisan, l_ordem, l_refnr.
*        ENDIF.
      ENDAT.

    ENDLOOP.

  ENDIF.
** RL <- INS 04.04.2005 ------------------------------------------------

** Validar Ordenação da Carga pelo resultado do Routyn
**********************************************************************
  IF l_zwm028[] IS NOT INITIAL.
    SELECT *
      FROM zroutyn_t01_i INTO TABLE lt_zroutyn_t01_i
      FOR ALL ENTRIES IN l_zwm028
      WHERE vbeln = l_zwm028-remessa.
  ENDIF.

  IF itab_servisan[] IS NOT INITIAL.
    SELECT *
     FROM zroutyn_t01_i APPENDING TABLE lt_zroutyn_t01_i
     FOR ALL ENTRIES IN itab_servisan
     WHERE vbeln = itab_servisan-remessa.
  ENDIF.

  SORT lt_zroutyn_t01_i BY refnr vbeln.

  IF lt_zroutyn_t01_i[] IS NOT INITIAL.
    SELECT *
      FROM zroutyn_t01_h INTO TABLE lt_zroutyn_t01_h
      FOR ALL ENTRIES IN lt_zroutyn_t01_i
      WHERE ntrans = lt_zroutyn_t01_i-ntrans.

    SORT lt_zroutyn_t01_h BY ntrans.
  ENDIF.

  " Validar se Transporte foi executado pelo o Routyn
  CLEAR lv_sort_routyn.

  LOOP AT l_zwm028.

    lv_tabix = sy-tabix.

    READ TABLE lt_zroutyn_t01_i WITH KEY refnr = l_zwm028-refnr BINARY SEARCH.
    CHECK sy-subrc = 0.

    CLEAR lt_zroutyn_t01_h.

    READ TABLE lt_zroutyn_t01_h WITH KEY ntrans = lt_zroutyn_t01_i-ntrans BINARY SEARCH.

    CHECK lt_zroutyn_t01_h-tripid IS NOT INITIAL.

    lv_sort_routyn = 'X'.

    READ TABLE itab_servisan WITH KEY id_servisan = l_zwm028-remessa
                                      refnr       = l_zwm028-refnr.
    IF sy-subrc = 0.
      lv_vbeln = itab_servisan-remessa.
    ELSE.
      lv_vbeln = l_zwm028-remessa.
    ENDIF.

    READ TABLE lt_zroutyn_t01_i WITH KEY refnr = l_zwm028-refnr
                                         vbeln = lv_vbeln.
    IF sy-subrc = 0.
      l_zwm028-ordem = lt_zroutyn_t01_i-seq.
    ELSE.
      l_zwm028-ordem = '99'.
    ENDIF.

    MODIFY l_zwm028 INDEX lv_tabix TRANSPORTING ordem.
  ENDLOOP.

  IF lv_sort_routyn = 'X'.

    CLEAR l_ordem.

    SORT l_zwm028 BY ordem DESCENDING.

    LOOP AT l_zwm028.
      lv_tabix = sy-tabix.

      l_ordem = l_ordem + 1.

      l_zwm028-ordem = l_ordem.

      MODIFY l_zwm028 INDEX lv_tabix TRANSPORTING ordem.
    ENDLOOP.
  ENDIF.

** Guardar dados na tabela
**********************************************************************
  IF NOT l_zwm028[] IS INITIAL.

    MODIFY zwm028 FROM TABLE l_zwm028.
*    COMMIT WORK AND WAIT.
  ENDIF.

** RL -> INS 04.04.2005 ------------------------------------------------
  IF NOT itab_servisan[] IS INITIAL.
    MODIFY zwm040 FROM TABLE itab_servisan.
*    COMMIT WORK AND WAIT.
  ENDIF.

  COMMIT WORK AND WAIT.

*  break roffd.
** RL <- INS 04.04.2005 ------------------------------------------------

***Apagar as entradas deste grupo
*  READ TABLE l_zwm028 INDEX 1.
*  IF sy-subrc = 0.
*    DELETE FROM zwm026
*        WHERE armazem = l_zwm028-lgnum AND
*              grupo = l_zwm028-refnr.
*    COMMIT WORK.
*  ENDIF.
** Registo do cálculo das paletes de picking
** Verificar se já esxiste uma entrada com a mesma chave

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '03'
      object                  = 'LVS_LENUM'
      quantity                = '1'
    IMPORTING
      number                  = l_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 30.05.2012 17:32:42
*  Motivo: No Caso de Ser remessas iguais, fica com a info da ultima linha
*--------------------------------------------------------------------*
*  zpalete_picking_back = zpalete_picking[].
*
*  SORT zpalete_picking_back BY refnr matnr vbeln posnr sub_item
*                               pal_incompleta DESCENDING.
*
*  DELETE ADJACENT DUPLICATES FROM zpalete_picking_back
*                        COMPARING refnr matnr vbeln
*                                  posnr sub_item.

*  LOOP AT zpalete_picking_back INTO ls_zpalete_picking_back.
*    lv_tabix = sy-tabix.
*
**--> Retorna Ultima Linha da Palete
*    CLEAR ls_zpalete_picking_last.
*    READ TABLE zpalete_picking
*          INTO ls_zpalete_picking_last
*          WITH KEY refnr     = ls_zpalete_picking_back-refnr
*                   matnr     = ls_zpalete_picking_back-matnr
*                   vbeln     = ls_zpalete_picking_back-vbeln
*                   posnr     = ls_zpalete_picking_back-posnr
*                   sub_item  = ls_zpalete_picking_back-sub_item
*                   pal_incompleta = 1.
*
*    CHECK sy-subrc EQ 0.
*
**--> Retorna Primeira Linha da Palete
*    CLEAR ls_zpalete_picking_first.
*    READ TABLE zpalete_picking
*          INTO ls_zpalete_picking_first
*          WITH KEY refnr     = ls_zpalete_picking_back-refnr
*                   matnr     = ls_zpalete_picking_back-matnr
*                   vbeln     = ls_zpalete_picking_back-vbeln
*                   posnr     = ls_zpalete_picking_back-posnr
*                   sub_item  = ls_zpalete_picking_back-sub_item.
*
*    CHECK sy-subrc EQ 0.
*
*    MODIFY zpalete_picking FROM ls_zpalete_picking_last INDEX sy-tabix.
*  ENDLOOP.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*  break roffd.
  DESCRIBE TABLE zpalete_picking LINES lv_lines.

  LOOP AT zpalete_picking.
    save_index = sy-tabix.

    SELECT SINGLE * FROM zwm026
                    WHERE armazem = armazem AND
                          grupo = zpalete_picking-refnr AND
                          material = zpalete_picking-matnr AND
                          remessa = zpalete_picking-vbeln AND
                          posnr = zpalete_picking-posnr AND
                          sub_item = zpalete_picking-sub_item.
    IF sy-subrc = 0.
** Não faz nada pq já existe uma entrada com a mesma chave
      CONTINUE.
    ELSE.

** Só realiza a inserção na tabela para as entradas que tenham
** quantidades de picking
      IF NOT zpalete_picking-uni_incompleta IS INITIAL.

        CLEAR : wa_zwm026, i_pal_picking.
        wa_zwm026-armazem = armazem.

*        IF zpalete_picking-pal_incompleta = 1.
*          CLEAR: l_number.
*          CALL FUNCTION 'NUMBER_GET_NEXT'
*            EXPORTING
*              nr_range_nr             = '03'
*              object                  = 'LVS_LENUM'
*              quantity                = '1'
*            IMPORTING
*              number                  = l_number
*            EXCEPTIONS
*              interval_not_found      = 1
*              number_range_not_intern = 2
*              object_not_found        = 3
*              quantity_is_0           = 4
*              quantity_is_not_1       = 5
*              interval_overflow       = 6
*              buffer_overflow         = 7
*              OTHERS                  = 8.
*          IF sy-subrc <> 0.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*          ENDIF.
*        ENDIF.

        wa_zwm026-n_pal_picking = l_number.
** Item da palete de picking
        SELECT COUNT( DISTINCT i_pal_picking )
               FROM zwm026 INTO i_pal_picking
               WHERE armazem = armazem
                 AND n_pal_picking = l_number
                 AND grupo = zpalete_picking-refnr.
** RL -> DEL 04.04.2005 ------------------------------------------------
*                 AND remessa = zpalete_picking-vbeln.
** RL <- DEL 04.04.2005 ------------------------------------------------
        IF sy-subrc = 0.
          wa_zwm026-i_pal_picking = i_pal_picking + 1.
        ELSE.
          wa_zwm026-i_pal_picking = 1.
        ENDIF.

        wa_zwm026-grupo = zpalete_picking-refnr.
        wa_zwm026-remessa = zpalete_picking-vbeln.
        wa_zwm026-posnr = zpalete_picking-posnr.
        IF zpalete_picking-sub_item IS INITIAL.
          wa_zwm026-sub_item = 1.
        ELSE.
          wa_zwm026-sub_item = zpalete_picking-sub_item.
        ENDIF.
        wa_zwm026-material = zpalete_picking-matnr.
        wa_zwm026-quantidade = zpalete_picking-uni_incompleta.
        wa_zwm026-unidade = zpalete_picking-meins.
        SELECT SINGLE maktx FROM makt INTO wa_zwm026-descricao
                            WHERE matnr = zpalete_picking-matnr AND
                                  spras = sy-langu.
        wa_zwm026-sorlp = zpalete_picking-sorlp.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 29.05.2012 11:35:11
*  Motivo: Meias paletes
*--------------------------------------------------------------------*
        wa_zwm026-pallet_type =  zpalete_picking-pallet_type.

*        IF zpalete_picking-pal_incompleta EQ 1 OR save_index EQ lv_lines.
        wa_zwm026-pal_picking = zpalete_picking-pal_picking.
*        ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

        CLEAR wa_zwm026-to_number.

        CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
          EXPORTING
            i_lgnum  = wa_zwm026-armazem
            i_refnr  = wa_zwm026-grupo
            i_vbeln  = wa_zwm026-remessa
          IMPORTING
            e_2step  = lv_2step
            e_2spart = lv_2spart
          EXCEPTIONS
            error    = 1
            OTHERS   = 2.

        IF lv_2spart EQ abap_true AND lv_2step = abap_true.
          wa_zwm026-posnr = gc_posnr_2step_dummy.
        ENDIF.


*        MODIFY zwm026.
        INSERT INTO zwm026 VALUES wa_zwm026.
        COMMIT WORK.
        CLEAR: wa_zwm026.

        IF zpalete_picking-pal_incompleta = 1.

          CLEAR : l_number.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '03'
              object                  = 'LVS_LENUM'
              quantity                = '1'
            IMPORTING
              number                  = l_number
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFUNCTION.
