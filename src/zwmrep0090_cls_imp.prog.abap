*&---------------------------------------------------------------------*
*&  Include           ZWMREP0090_CLS
*&---------------------------------------------------------------------*

CLASS lcl_alv IMPLEMENTATION.
  METHOD inicialization.
    APPEND VALUE #( sign   = 'I' option = 'GE'
                    low    = sy-datum - 2
                  ) TO s_datum.

    APPEND VALUE #( sign   = 'I' option = 'CP'
                    low    = '*' && sy-datum+6(2) && '-' && sy-datum+4(2) && '-* X*'
                  ) TO s_refnt.

    APPEND VALUE #( sign   = 'I' option = 'CP'
                    low    = 'RP*'
                  ) TO s_vkorg.

*    LOOP AT SCREEN.
*      IF screen-name EQ 'P_FALTAT'.
*        screen-invisible = 1.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.

  METHOD show_progress.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = iv_perc
        text       = iv_text.
  ENDMETHOD.

  METHOD set_lock.
    IF cv_keyword IS INITIAL.
      cv_keyword = sy-uname.
    ENDIF.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = cv_keyword
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
*      MESSAGE e000 WITH 'A transacção ja está a ser executado pelo user' sy-uname .
    ENDIF.
  ENDMETHOD.
  METHOD remove_lock.
    IF cv_keyword IS INITIAL.
      cv_keyword = sy-uname.
    ENDIF.
    CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = cv_keyword
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
  ENDMETHOD.

  METHOD get_variante.
    gs_variant-report = sy-repid.
*    gs_variant-handle = .
    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
      EXPORTING
        is_variant    = gs_variant
      IMPORTING
        es_variant    = gs_variant
      EXCEPTIONS
        not_found     = 1
        program_error = 2
        OTHERS        = 3.

    IF sy-subrc = 0.
      rv_variant = gs_variant-variant.
    ENDIF.
  ENDMETHOD.

  METHOD get_dados.

*    lcl_alv=>show_progress( iv_perc = 05 iv_text = text-007 ). "Ler grupos...
*    lcl_alv=>show_progress( iv_perc = 05 iv_text = text-008 ). "Ler cabeçalho de remessas...
*    lcl_alv=>show_progress( iv_perc = 15 iv_text = text-009 ). "Ler Clientes...
*    lcl_alv=>show_progress( iv_perc = 20 iv_text = text-010 ). "Ler detalhe das Remessas...
*    lcl_alv=>show_progress( iv_perc = 25 iv_text = text-011 ). "Ler transportes...
*    lcl_alv=>show_progress( iv_perc = 25 iv_text = text-012 ). "Ler cabeçalho das ordens de venda...
*    lcl_alv=>show_progress( iv_perc = 25 iv_text = text-013 ). "Ler detalhe das ordens de venda...
*    lcl_alv=>show_progress( iv_perc = 25 iv_text = text-014 ). "Pesquisar produtos em rotura...

    REFRESH: gt_dados,
*             gt_vbfa,
*             gt_ekbe,
             gt_vbpa_adrc,
             gt_tvfptz,
             gt_ztransp_h,
             gr_abgru,
             gt_grupo_palestes,
             gt_zroutyn_t01_i.

    init_icons( ).

    SELECT 0 AS lvl,
           t311~lgnum,
           t311~refnr,
           t311~refnt,
           t311~datum,
            t311a~rbnum,
              likp~vbeln,
              likp~vstel,
              likp~vkorg,
              likp~kunnr,
              likp~kunag,
              likp~btgew,
              likp~gewei AS gewei_likp,
              likp~volum AS volum_likp,
              likp~voleh AS voleh_likp,
                lips~posnr,
                lips~pstyv,
                lips~ernam,
                lips~erzet,
                lips~erdat,
                lips~matnr,
                lips~werks,
                lips~lgort,
                lips~charg,
                lips~lfimg,
                lips~meins,
                lips~vrkme,
                lips~ntgew,
                lips~brgew,
                lips~gewei,
                lips~volum,
                lips~voleh,
                lips~vgbel,
                lips~vgpos,
                  kna1~land1,
                  kna1~name1,
                  kna1~name2,
                    vttp~tknum,
                    vttp~tpnum,
                      vttk~vbtyp,
                      vttk~shtyp,
                      vttk~tplst,
                      vttk~ernam AS ernam_vttk,
                      vttk~erdat AS erdat_vttk,
                      vttk~erzet AS erzet_vttk,
                      vttk~signi,
                      vttk~exti1,
                      vttk~exti2,
                      vttk~dtdis,
                      vttk~tdlnr,
                      vttk~sdabw,
                      vttk~add01,
                      vttk~add02,
                      vttk~add03,
                      vttk~add04,
                      vttk~dalbg,
                      vttk~tndrrc,
                      vttk~tndr_actp,
                        dd07t~ddtext AS tndrrc_t,
                                tvsakt~bezei AS tp_carro,
                                  lfa1~name1 AS name1_tdlnr,
                                    makt~maktx,
              tvkot~vtext,
               vbuk~kostk,
               vbuk~wbstk,
                zwm028~zlock
      FROM t311
        INNER JOIN t311a
        ON  t311a~lgnum EQ t311~lgnum
        AND t311a~refnr EQ t311~refnr
          INNER JOIN likp
          ON likp~vbeln EQ t311a~rbnum
            INNER JOIN lips
            ON  lips~vbeln EQ likp~vbeln
            AND lips~lgnum NE ''
             INNER JOIN kna1
             ON kna1~kunnr EQ likp~kunnr
              INNER JOIN vttp
              ON  vttp~vbeln EQ likp~vbeln
                INNER JOIN vttk
                ON  vttk~tknum EQ vttp~tknum
                LEFT OUTER JOIN tvsakt
                ON  tvsakt~spras EQ @sy-langu
                AND tvsakt~sdabw EQ vttk~sdabw
                  LEFT OUTER JOIN lfa1
                  ON  lfa1~lifnr EQ vttk~tdlnr
                    LEFT OUTER JOIN makt
                    ON  makt~matnr EQ lips~matnr
                    AND makt~spras EQ @sy-langu
                     LEFT OUTER JOIN tvkot
                     ON  tvkot~spras EQ @sy-langu
                     AND tvkot~vkorg EQ likp~vkorg
                       LEFT OUTER JOIN dd07t
                       ON  domname    EQ 'TNDRRC'
                       AND ddlanguage EQ 'P'
                       AND as4local   EQ 'A'
                       AND domvalue_l EQ vttk~tndrrc
        INNER JOIN vbuk
        ON  vbuk~vbeln EQ likp~vbeln
           LEFT OUTER JOIN zwm028
           ON  zwm028~lgnum   EQ t311~lgnum
           AND zwm028~refnr   EQ t311~refnr
           AND zwm028~remessa EQ t311a~rbnum

      INTO TABLE @gt_dados
      WHERE t311~lgnum EQ @p_lgnum
        AND t311~refnr IN @s_refnr
        AND t311~refnt IN @s_refnt
        AND t311~datum IN @s_datum
        AND likp~vkorg IN @s_vkorg
        AND likp~vstel IN @s_vstel
*        AND lips~uecha EQ 0 "exclude batch partition
        AND vttk~tknum IN @s_tknum
        AND vttk~sdabw IN @s_sdabw
        AND vttk~tdlnr IN @s_tdlnr
        AND vttk~dtdis IN @s_dtdis
        AND vttk~dalbg IN @s_dalbg
        AND vttk~dplbg IN @s_dplbg
        AND vbuk~wbstk IN @s_wbstk.


    IF gt_dados IS NOT INITIAL.

*      DATA: lr_vgbel TYPE RANGE OF lips-vgbel.
*      lr_vgbel = VALUE #( FOR ls_doc IN gt_dados WHERE ( vgbel IS NOT INITIAL )
*                          sign = 'I' option = 'EQ' ( low = ls_doc-vgbel ) ).
*      SORT lr_vgbel BY low.
*      DELETE ADJACENT DUPLICATES FROM lr_vgbel COMPARING low.

*      IF lr_vgbel IS NOT INITIAL.
      SELECT vbak~vbeln,
             vbak~vkorg,
             vbak~vtweg,
             vbak~spart,
             vbak~vkgrp,
             vbak~vkbur,
             vbak~gsber,
             vbak~kunnr,
               vbap~posnr,
               vbap~matnr,
               vbap~matwa,
               vbap~charg,
               vbap~matkl,
               vbap~arktx,
               vbap~pstyv,
               vbap~abgru,
               vbap~meins,
               vbap~kwmeng,
               vbap~vrkme,
               vbap~brgew,
               vbap~ntgew,
               vbap~gewei,
               vbap~volum,
               vbap~voleh,
               vbap~vbelv,
               vbap~posnv,
               vbap~vstel,
               vbap~vgbel,
               vbap~vgpos,
               vbap~werks,
               vbap~lgort,
                vbep~edatu,
                  makt~maktx
        INTO TABLE @DATA(lt_vbap)
         FROM vbak
          INNER JOIN vbap
          ON vbap~vbeln EQ vbak~vbeln
                    LEFT OUTER JOIN makt
                    ON  makt~matnr EQ vbap~matnr
                    AND makt~spras EQ @sy-langu
             INNER JOIN vbep
             ON  vbep~vbeln EQ vbap~vbeln
             AND vbep~posnr EQ vbap~posnr
             AND vbep~etenr EQ 0001
        FOR ALL ENTRIES IN @gt_dados
            WHERE vbak~vbeln EQ @gt_dados-vgbel
              AND vbap~lgort IN ( '', 'CD' )
              AND vbap~taxm1 EQ '1'
              AND vbep~edatu IN @s_etdat.
*            WHERE vbak~vbeln IN @lr_vgbel.
*        ORDER BY vbap~vbeln, vbap~posnr.
      SORT lt_vbap BY vbeln posnr.
*      inner join vbep on 0001924806  VBELN
*000090	POSNR
*0001	ETENR <<< fixo

      SELECT ekko~ebeln,
              ekpo~ebelp,
              ekpo~loekz,
              ekpo~statu,
              ekpo~aedat,
              ekpo~txz01,
              ekpo~matnr,
              ekpo~werks,
              ekpo~lgort,
              ekpo~menge,
              ekpo~meins,
              ekpo~elikz,
                makt~maktx
        INTO TABLE @DATA(lt_ekko)
        FROM ekko
         INNER JOIN ekpo
         ON ekpo~ebeln EQ ekko~ebeln
                    LEFT OUTER JOIN makt
                    ON  makt~matnr EQ ekpo~matnr
                    AND makt~spras EQ @sy-langu
        FOR ALL ENTRIES IN @gt_dados
        WHERE ekpo~ebeln EQ @gt_dados-vgbel.
*        WHERE ekpo~ebeln IN @lr_vgbel
*        ORDER BY ekpo~ebeln, ekpo~ebelp.
      SORT lt_ekko BY ebeln ebelp.

*      SELECT vbelv,
*             posnv,
**             VBELN,
**             POSNN,
**             VBTYP_N,
*             SUM( vbfa~rfmng ) AS rfmng_sum,
*             meins
*        INTO TABLE @gt_vbfa
*        FROM vbfa
**        FOR ALL ENTRIES IN @lt_docs
*                WHERE vbelv   IN @lr_vgbel "EQ @lt_docs-vgbel
*                  AND vbtyp_n EQ 'J'
*        GROUP BY vbelv,
*                 posnv,
*                 meins
*        ORDER BY vbelv,
*                 posnv.

*      SELECT ekbe~ebeln,
*             ekbe~ebelp,
**             ZEKKN,
**             VGABE,
**             GJAHR,
**             BELNR,
**             BUZEI
*             SUM( ekbe~menge ) AS menge_sum,
*             ekpo~meins
*        INTO TABLE @gt_ekbe
*        FROM ekbe
*         INNER JOIN ekpo
*         ON  ekpo~ebeln EQ ekbe~ebeln
*         AND ekpo~ebelp EQ ekbe~ebelp
**        FOR ALL ENTRIES IN @lt_docs
*                WHERE ekbe~ebeln IN @lr_vgbel "EQ @lt_docs-vgbel
*                  AND ekbe~vgabe EQ '8'
*        GROUP BY ekbe~ebeln,
*                 ekbe~ebelp,
*                 meins
*        ORDER BY ekbe~ebeln,
*                 ekbe~ebelp.

      IF lt_vbap IS NOT INITIAL.
        SELECT ntrans,
               itrans,
               ltrans,
               doc,
               item,
               doctype,
               vbeln,
               posnr,
               refnr,
               ernam,
               erdat,
               erzet,
               seq
          INTO TABLE @gt_zroutyn_t01_i
          FROM zroutyn_t01_i
          FOR ALL ENTRIES IN @lt_vbap "gt_dados
         WHERE vbeln EQ @lt_vbap-vbeln. "gt_dados-vgbel.
*       WHERE vbeln IN lr_vgbel
*        ORDER BY vbeln.
        SORT gt_zroutyn_t01_i BY vbeln.

*      ENDIF.
      ENDIF.

      SELECT vbeln,
             posnr,
             parvw,
             kunnr,
             lifnr,
             adrnr,
              adrc~addrnumber,
              adrc~date_from,
              adrc~nation,
              adrc~date_to,
              adrc~title,
              adrc~name1,
              adrc~name2,
              adrc~name3,
              adrc~name4,
              adrc~name_text,
              adrc~name_co,
              adrc~city1,
              adrc~city2,
              adrc~city_code,
              adrc~cityp_code,
              adrc~home_city,
              adrc~cityh_code,
              adrc~chckstatus,
              adrc~regiogroup,
              adrc~post_code1,
              adrc~country
          INTO TABLE @gt_vbpa_adrc
          FROM vbpa
            INNER JOIN adrc
            ON adrc~addrnumber EQ vbpa~adrnr
        FOR ALL ENTRIES  IN @gt_dados
             WHERE vbeln EQ @gt_dados-rbnum
*               AND parvw EQ 'WE'.
               AND parvw IN ( 'WE', 'W1' ).
      SORT gt_vbpa_adrc BY vbeln
                           parvw.

      DATA: lr_tplst TYPE RANGE OF tvfptz-tplst.
      lr_tplst = VALUE #( FOR ls_doc IN gt_dados WHERE ( tplst IS NOT INITIAL )
                          sign = 'I' option = 'EQ' ( low = ls_doc-tplst ) ).
      SORT lr_tplst BY low.
      DELETE ADJACENT DUPLICATES FROM lr_tplst COMPARING low.

      IF lr_tplst IS NOT INITIAL.
        SELECT tvfptz~tplst,
               tvfptz~tdlnr,
               tvfptz~vsart,
               tvfptz~land1,
               tvfptz~fpstlz,
               tvfptz~tpstlz,
               tvfptz~trfzn,
                tvftzt~bezei
          INTO TABLE @gt_tvfptz
           FROM tvfptz
            LEFT OUTER JOIN tvftzt
            ON  tvftzt~spras EQ @sy-langu
            AND tvftzt~trfzn EQ tvfptz~trfzn
*      FOR ALL ENTRIES  IN @gt_dados
*             WHERE tplst  EQ @gt_dados-tplst
               WHERE tplst  IN @lr_tplst
                 AND vsart  EQ '01'.
*                 AND land1  eq adrc-country
*                 AND fpstlz le adrc-post_code1
*                 AND tpstlz ge adrc-post_code1.
        SORT gt_tvfptz BY tplst land1 fpstlz.
      ENDIF.

      DATA: lr_tknum TYPE RANGE OF ztransp_h-tknum.
      lr_tknum = VALUE #( FOR ls_doc IN gt_dados WHERE ( tknum IS NOT INITIAL )
                          sign = 'I' option = 'EQ' ( low = ls_doc-tknum ) ).
      SORT lr_tknum BY low.
      DELETE ADJACENT DUPLICATES FROM lr_tknum COMPARING low.

      IF lr_tknum IS NOT INITIAL.
        SELECT ztransp_h~idt,
               ztransp_h~ntrans,
               ztransp_h~tknum,
               ztransp_h~desbloquear,
               ztransp_h~ddtext,
               ztransp_h~zonadist,
               ztransp_h~tndrst,
                       zcust_trsp~netwr
          FROM ztransp_h
          LEFT OUTER JOIN zcust_trsp
            ON zcust_trsp~idt   EQ ztransp_h~idt AND
               zcust_trsp~tknum EQ ztransp_h~tknum AND
               zcust_trsp~tdlnr EQ ztransp_h~tdlnr
          INTO TABLE @gt_ztransp_h
*            FOR ALL ENTRIES IN @gt_dados
*            WHERE tknum EQ @gt_dados-tknum.
            WHERE ztransp_h~tknum IN @lr_tknum.
        SORT gt_ztransp_h BY tknum.


*        DATA(lt_tknum_cont) = VALUE edoc_tknum_tab(
*                                FOR ls_tkn
*                                  IN gt_dados ( ls_tkn-tknum ) ).
*        SORT lt_tknum_cont.
*        DELETE ADJACENT DUPLICATES FROM lt_tknum_cont.
*        gv_cargas = lines( lt_tknum_cont ).
*        gv_cargas = lines( lr_tknum ).

        SELECT *
          FROM zwm006_aux
          INTO TABLE gt_zwm006_aux
*          FOR ALL ENTRIES IN gt_dados
          WHERE armazem      EQ p_lgnum
*            AND n_transporte EQ gt_dados-tknum.
            AND n_transporte IN lr_tknum.
        SORT gt_zwm006_aux BY n_transporte.
      ENDIF.

    ENDIF.

*    lcl_alv=>show_progress( iv_perc = 25 iv_text = text-015 ). "Pesquisar produtos em falta...

*    DATA Gr_abgru TYPE RANGE OF vbap-abgru. "( '20' OR gt_vbap-abgru EQ '07' OR gt_vbap-abgru EQ '24'.
    gr_abgru = VALUE #( sign = 'I' option = 'EQ'
                        ( low = '07' ) "Item Informativo
                        ( low = '20' ) "Anulação de produto - Roturas
                        ( low = '24' ) "Anulação de Encomenda
                          ).

**********************************************************************
    LOOP AT gt_dados ASSIGNING FIELD-SYMBOL(<lfs_reg>).
      LOOP AT gt_dados TRANSPORTING NO FIELDS
        WHERE vkorg NE <lfs_reg>-vkorg
          AND tknum EQ <lfs_reg>-tknum.
        MODIFY gt_dados FROM <lfs_reg>
         TRANSPORTING vkorg
           WHERE tknum EQ <lfs_reg>-tknum.
        EXIT.
      ENDLOOP.

      READ TABLE lt_vbap TRANSPORTING NO FIELDS WITH KEY vbeln = <lfs_reg>-vgbel.
      IF sy-subrc IS INITIAL.
        <lfs_reg>-vbeln = <lfs_reg>-vgbel.
      ELSE.
        READ TABLE lt_ekko TRANSPORTING NO FIELDS WITH KEY ebeln = <lfs_reg>-vgbel.
        IF sy-subrc IS INITIAL.
          <lfs_reg>-ebeln = <lfs_reg>-vgbel.
        ENDIF.
      ENDIF.

      valida_complete_data( CHANGING cs_data = <lfs_reg> ).
    ENDLOOP.
**********************************************************************
**********************************************************************
    SORT gt_dados BY vgbel
                     vgpos
                     posnr .

    LOOP AT lt_vbap INTO DATA(ls_vbap).
      IF ls_vbap-abgru IN gr_abgru.
        DELETE gt_dados WHERE vgbel EQ ls_vbap-vbeln AND
                              vgpos EQ ls_vbap-posnr.
        CONTINUE.
      ENDIF.
      valida_add_doc_itm( EXPORTING  iv_doc = ls_vbap-vbeln
                                     iv_itm = ls_vbap-posnr
                                     iv_qtd = ls_vbap-kwmeng
                                     is_str = ls_vbap ).

    ENDLOOP.

    LOOP AT lt_ekko INTO DATA(ls_ekko).
      IF ls_ekko-loekz IS NOT INITIAL OR
         ls_ekko-elikz IS NOT INITIAL.
        DELETE gt_dados WHERE vgbel EQ ls_ekko-ebeln AND
                              vgpos EQ ls_ekko-ebelp.
        CONTINUE.
      ENDIF.

      valida_add_doc_itm( EXPORTING  iv_doc = ls_ekko-ebeln
                                     iv_itm = ls_ekko-ebelp
                                     iv_qtd = ls_ekko-menge
                                     is_str = ls_ekko ).

    ENDLOOP.

    DELETE gt_dados WHERE edatu NOT IN s_etdat.
    IF s_etdat[] IS NOT INITIAL.
      DELETE gt_dados WHERE edatu IS INITIAL.
    ENDIF.

    IF s_dtext[] IS NOT INITIAL.
      DELETE gt_dados WHERE tndrrc_t NOT IN s_dtext.
    ENDIF.

**********************************************************************
**********************************************************************
    SORT gt_dados BY refnr rbnum posnr.
    LOOP AT gt_dados INTO DATA(ls_dados).
      DATA(lv_dif) = ls_dados-kwmeng - ls_dados-qtd_fornecida.
      READ TABLE gt_grupo_palestes
        ASSIGNING FIELD-SYMBOL(<lfs_grupo_palete>)
          WITH KEY refnr = ls_dados-refnr.
      IF sy-subrc IS NOT INITIAL.
        APPEND INITIAL LINE TO gt_grupo_palestes
          ASSIGNING <lfs_grupo_palete>.
        <lfs_grupo_palete>-refnr = ls_dados-refnr.
      ENDIF.
      READ TABLE <lfs_grupo_palete>-paletes
        ASSIGNING FIELD-SYMBOL(<lfs_palete>)
          WITH KEY vbeln = ls_dados-rbnum
                   matnr = ls_dados-matnr
                   posnr = ls_dados-posnr.
      IF sy-subrc IS NOT INITIAL.
        APPEND INITIAL LINE TO <lfs_grupo_palete>-paletes
          ASSIGNING <lfs_palete>.
        <lfs_palete> = CORRESPONDING #( ls_dados ).
        <lfs_palete>-vbeln = ls_dados-rbnum.
        <lfs_palete>-kunnr = ls_dados-kunnr.
        <lfs_palete>-kunag = ls_dados-kunag.
        <lfs_palete>-refnr = ls_dados-refnr.
        <lfs_palete>-sammg = ls_dados-refnr.
        <lfs_palete>-meins = ls_dados-meins.
        <lfs_palete>-vrkme = ls_dados-vrkme.
        <lfs_palete>-voleh = ls_dados-voleh.
        <lfs_palete>-volum = ls_dados-volum.
        <lfs_palete>-nodel = ls_dados-nodel.
        IF ls_dados-falta_comp EQ abap_true.
          <lfs_palete>-lfimg = ls_dados-kwmeng. "iv_qtd.
        ELSEIF ls_dados-falta_parc EQ abap_true.
*          <lfs_palete>-lfimg = ls_dados-kwmeng. "iv_qtd.
          IF ls_dados-ebeln IS INITIAL.
            <lfs_palete>-lfimg = ls_dados-lfimg + lv_dif.
*                 ( ls_dados-kwmeng - ls_dados-qtd_fornecida ).
          ELSE.
*            IF <lfs_palete>-lfimg IS INITIAL
*              OR ls_dados-vgpos   EQ ls_dados-posnr.        "gt 900000.
            IF ls_dados-vbeln IS NOT INITIAL.
              <lfs_palete>-lfimg = ls_dados-lfimg + lv_dif.
*                   ( ls_dados-kwmeng - ls_dados-qtd_fornecida ).
            ENDIF.
            LOOP AT gt_dados ASSIGNING FIELD-SYMBOL(<lfs_dados_aux>)
              WHERE vgbel EQ ls_dados-vgbel
                AND vgpos EQ ls_dados-vgpos.
              ADD lv_dif TO <lfs_dados_aux>-qtd_fornecida.
            ENDLOOP.
**            IF ls_dados-vgpos EQ ls_dados-posnr.
*            LOOP AT gt_dados ASSIGNING FIELD-SYMBOL(<lfs_dados_aux>)
*              WHERE vgbel EQ ls_dados-vgbel
*                AND vgpos EQ ls_dados-vgpos.
**                  AND posnr EQ ls_dados-posnr
**                  AND vbeln NE ls_dados-vbeln.
*              ADD lv_dif TO <lfs_dados_aux>-qtd_fornecida.
*            ENDLOOP.
**            ENDIF.
          ENDIF.
*          <lfs_palete>-lfimg = ls_dados-qtd_dif.
        ENDIF.
      ELSE.
        IF ls_dados-falta_comp EQ abap_true.
          <lfs_palete>-lfimg = ls_dados-kwmeng. "iv_qtd.
        ELSE.
          <lfs_palete>-lfimg = <lfs_palete>-lfimg +
                                ( ls_dados-kwmeng "iv_qtd.iv_qtd "ls_vbap-kwmeng
                                - ls_dados-qtd_fornecida ).
        ENDIF.
      ENDIF.
    ENDLOOP.
    DELETE gt_dados WHERE vgbel IS INITIAL.
    DELETE gt_dados WHERE posnr GE 900000.
**********************************************************************
**********************************************************************
*    DATA: lv_2step  TYPE flag,
*          lv_2spart TYPE flag.
    CLEAR gs_pal_sum.
    SORT gt_grupo_palestes BY refnr.
    LOOP AT gt_grupo_palestes INTO DATA(ls_grupo_palete).

*      CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
*        EXPORTING
*          i_lgnum  = p_lgnum
*          i_refnr  = ls_grupo_palete-refnr
*        IMPORTING
*          e_2step  = lv_2step
*          e_2spart = lv_2spart
*        EXCEPTIONS
*          error    = 1
*          OTHERS   = 2.
      DELETE ls_grupo_palete-paletes WHERE "lfimg IS INITIAL OR
                                           vrkme IS INITIAL.

      CALL FUNCTION 'ZWM_PAL_PICKING'
        EXPORTING
          armazem         = p_lgnum
          actualiza       = abap_false
*         REMONTADA       =
        TABLES
          zpalete_picking = ls_grupo_palete-paletes.
*      CALL FUNCTION 'ZWM_PAL_PICKING_COMPLETE'
*        EXPORTING
*          i_lgnum         = p_lgnum
*        TABLES
*          zpalete_picking = ls_grupo_palete-paletes. "lt_paletes[].

      DATA: lv_rest                   TYPE p DECIMALS 2.

*      LOOP AT lt_paletes INTO DATA(ls_palete).
      LOOP AT ls_grupo_palete-paletes
          INTO DATA(ls_palete).
***        READ TABLE gt_dados ASSIGNING FIELD-SYMBOL(<lfs_dados>)
***           WITH KEY refnr = ls_palete-refnr.
**        READ TABLE gt_dados ASSIGNING FIELD-SYMBOL(<lfs_dados>)
**           WITH KEY refnr = ls_palete-refnr
***                    vbeln = ls_palete-vbeln
**                    rbnum = ls_palete-vbeln
**                    matnr = ls_palete-matnr.
**        IF sy-subrc IS NOT INITIAL.
**          READ TABLE gt_dados ASSIGNING <lfs_dados>
**             WITH KEY refnr = ls_palete-refnr
**                      matnr = ls_palete-matnr
**                      charg = ls_palete-charg.
**          IF sy-subrc IS NOT INITIAL.
**            READ TABLE gt_dados ASSIGNING <lfs_dados>
**               WITH KEY refnr = ls_palete-refnr
**                        matnr = ls_palete-matnr.
**          ENDIF.
**        ENDIF.
        READ TABLE gt_dados ASSIGNING FIELD-SYMBOL(<lfs_dados>)
           WITH KEY refnr = ls_palete-refnr
                    rbnum = ls_palete-vbeln.
        IF sy-subrc IS NOT INITIAL.
          READ TABLE gt_dados ASSIGNING <lfs_dados>
             WITH KEY refnr = ls_palete-refnr.
        ENDIF.
        IF sy-subrc IS INITIAL.
          SELECT SINGLE *
              FROM mlgn
            INTO @DATA(ls_mlgn)
              WHERE matnr EQ @ls_palete-matnr.
          IF ls_mlgn-lhmg1 IS NOT INITIAL.
            CLEAR: lv_rest.
*            IF ls_mlgn-lety1 EQ 'P2' OR ls_mlgn-lety1 EQ 'P5'.
            IF z_wm_cl_management=>is_remontada( is_data = ls_mlgn ) EQ abap_true.
              ls_palete-pal_completa = ls_palete-pal_completa / 2.
              lv_rest = ls_palete-pal_completa MOD 2.
*            IF lv_rest GT 0.
*              ADD 1 TO lt_paletes-pal_completa.
*            ENDIF.
            ENDIF.
          ENDIF.
          ADD ls_palete-pal_completa TO <lfs_dados>-palcp.
          ADD ls_palete-pal_picking  TO <lfs_dados>-palpk.
          <lfs_dados>-palto   = <lfs_dados>-palcp + <lfs_dados>-palpk.
          <lfs_dados>-palpk_s = ceil( <lfs_dados>-palpk ).
          <lfs_dados>-palto_s = <lfs_dados>-palcp + <lfs_dados>-palpk_s.

**********************************************************************
*          ADD ls_palete-pal_completa TO <lfs_dados>-palcp_grp.
*          ADD ls_palete-pal_picking  TO <lfs_dados>-palpk_grp.
*          <lfs_dados>-palto_grp   = <lfs_dados>-palcp_grp + <lfs_dados>-palpk_grp.
*          <lfs_dados>-palpk_s_grp = ceil( <lfs_dados>-palpk_grp ).
*          <lfs_dados>-palto_s_grp = <lfs_dados>-palcp_grp + <lfs_dados>-palpk_s_grp.
**********************************************************************
*          MODIFY gt_dados FROM <lfs_dados> TRANSPORTING palcp_grp
*                                                        palpk_grp
*                                                        palto_grp
*                                                        palpk_s_grp
*                                                        palto_s_grp
*                 WHERE refnr = ls_palete-refnr.

          MODIFY gt_dados FROM <lfs_dados> TRANSPORTING palcp
                                                        palpk
                                                        palto
                                                        palpk_s
                                                        palto_s
                 WHERE refnr = ls_palete-refnr
                   AND rbnum = ls_palete-vbeln.

          add_soma_pal( EXPORTING iv_palcomp = ls_palete-pal_completa iv_palpick = ls_palete-pal_picking
                        CHANGING cs_pal_sum = gs_pal_sum-head ).

          READ TABLE gs_pal_sum-vkorg ASSIGNING FIELD-SYMBOL(<lfs_pal_tot_vkorg>)
            WITH KEY vkorg = <lfs_dados>-vkorg.
          IF sy-subrc IS NOT INITIAL.
            APPEND VALUE #( vkorg = <lfs_dados>-vkorg ) TO gs_pal_sum-vkorg ASSIGNING <lfs_pal_tot_vkorg>.
          ENDIF.
          add_soma_pal( EXPORTING iv_palcomp = ls_palete-pal_completa iv_palpick = ls_palete-pal_picking
                        CHANGING cs_pal_sum = <lfs_pal_tot_vkorg>-totais ).

          READ TABLE gs_pal_sum-tknum ASSIGNING FIELD-SYMBOL(<lfs_pal_tot_tknum>)
            WITH KEY tknum = <lfs_dados>-tknum.
          IF sy-subrc IS NOT INITIAL.
            APPEND VALUE #( tknum = <lfs_dados>-tknum ) TO gs_pal_sum-tknum ASSIGNING <lfs_pal_tot_tknum>.
          ENDIF.
          add_soma_pal( EXPORTING iv_palcomp = ls_palete-pal_completa iv_palpick = ls_palete-pal_picking
                        CHANGING cs_pal_sum = <lfs_pal_tot_tknum>-totais ).

          READ TABLE gs_pal_sum-refnr ASSIGNING FIELD-SYMBOL(<lfs_pal_tot_refnr>)
            WITH KEY refnr = <lfs_dados>-refnr.
          IF sy-subrc IS NOT INITIAL.
            APPEND VALUE #( refnr = <lfs_dados>-refnr ) TO gs_pal_sum-refnr ASSIGNING <lfs_pal_tot_refnr>.
          ENDIF.
          add_soma_pal( EXPORTING iv_palcomp = ls_palete-pal_completa iv_palpick = ls_palete-pal_picking
                        CHANGING cs_pal_sum = <lfs_pal_tot_refnr>-totais ).
**********************************************************************
        ENDIF.
      ENDLOOP.
    ENDLOOP.

*    IF sy-uname EQ 'ROFF-SGARCIA'.
    SORT gt_dados BY vkorg tknum refnr seq rbnum.
    DATA(ls_dados_aux) = ls_dados.
    CLEAR ls_dados_aux.
    LOOP AT gt_dados INTO ls_dados.

*        IF ls_dados-refnr NE ls_dados_aux-refnr.
*        ENDIF.

      IF ls_dados-rbnum NE ls_dados_aux-rbnum.
        READ TABLE gs_pal_sum-refnr ASSIGNING <lfs_pal_tot_refnr>
          WITH KEY refnr = ls_dados-refnr.
        IF ls_dados-tknum NE ls_dados_aux-tknum OR
           <lfs_pal_tot_tknum> IS NOT ASSIGNED.
          READ TABLE gs_pal_sum-tknum ASSIGNING <lfs_pal_tot_tknum>
            WITH KEY tknum = ls_dados-tknum.
        ENDIF.
        IF ls_dados-vkorg NE ls_dados_aux-vkorg OR
           <lfs_pal_tot_vkorg> IS NOT ASSIGNED.
          READ TABLE gs_pal_sum-vkorg ASSIGNING <lfs_pal_tot_vkorg>
            WITH KEY vkorg = ls_dados-vkorg.
        ENDIF.

        ADD ls_dados-palpk_s TO gs_pal_sum-head-palpk_s.
        ADD ls_dados-palto_s TO gs_pal_sum-head-palto_s.

        ADD ls_dados-palpk_s TO <lfs_pal_tot_refnr>-totais-palpk_s.
        ADD ls_dados-palto_s TO <lfs_pal_tot_refnr>-totais-palto_s.

        ADD ls_dados-palpk_s TO <lfs_pal_tot_tknum>-totais-palpk_s.
        ADD ls_dados-palto_s TO <lfs_pal_tot_tknum>-totais-palto_s.

        ADD ls_dados-palpk_s TO <lfs_pal_tot_vkorg>-totais-palpk_s.
        ADD ls_dados-palto_s TO <lfs_pal_tot_vkorg>-totais-palto_s.
      ENDIF.

      ls_dados_aux = ls_dados.
    ENDLOOP.
*    ENDIF.

    SORT gs_pal_sum-vkorg BY vkorg.
    SORT gs_pal_sum-tknum BY tknum.
    SORT gs_pal_sum-refnr BY refnr.

    IF s_matnr[] IS NOT INITIAL.
      DELETE gt_dados WHERE matnr NOT IN s_matnr.
    ENDIF.

    IF s_trfzn[] IS NOT INITIAL.
      DELETE gt_dados WHERE trfzn NOT IN s_trfzn.
    ENDIF.

    IF p_falta EQ abap_true.
      DELETE gt_dados WHERE qtd_dif EQ 0.
    ENDIF.

    lr_tknum = VALUE #( FOR ls_doc IN gt_dados WHERE ( tknum IS NOT INITIAL )
                        sign = 'I' option = 'EQ' ( low = ls_doc-tknum ) ).
    SORT lr_tknum BY low.
    DELETE ADJACENT DUPLICATES FROM lr_tknum COMPARING low.

    gv_cargas = lines( lr_tknum ).

  ENDMETHOD.

  METHOD add_soma_pal.

    ADD iv_palcomp TO cs_pal_sum-palcp.
    ADD iv_palpick TO cs_pal_sum-palpk.
    cs_pal_sum-palto   = cs_pal_sum-palcp + cs_pal_sum-palpk.
*    IF sy-uname EQ 'ROFF-SGARCIA'.
*    ELSE.
*      cs_pal_sum-palpk_s = ceil( cs_pal_sum-palpk ).
*      cs_pal_sum-palto_s = cs_pal_sum-palcp + cs_pal_sum-palpk_s.
*    ENDIF.

  ENDMETHOD.

  METHOD valida_add_doc_itm.

    TYPES: BEGIN OF lty_doc,
             vbeln TYPE vbap-vbeln,
             posnr TYPE vbap-posnr,
             abgru TYPE vbap-abgru,
             ebeln TYPE ekpo-ebeln,
             ebelp TYPE ekpo-ebelp,
             loekz TYPE ekpo-loekz,
             elikz TYPE ekpo-elikz,
           END OF lty_doc.
    DATA ls_doc TYPE lty_doc.

    DATA lv_hours      TYPE /sdf/cmo_time-hours.

    ls_doc = CORRESPONDING #( is_str ).

    READ TABLE gt_dados ASSIGNING FIELD-SYMBOL(<lfs_dados>)
     WITH KEY vgbel = iv_doc
              vgpos = iv_itm
              BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      IF p_faltat IS INITIAL.
*        CONTINUE.
        EXIT.
      ENDIF.
      "adiciona linha de falta total posicionada na linha correta
      READ TABLE gt_dados INTO DATA(ls_doc_aux)
        WITH KEY vgbel = iv_doc
          BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        DATA(lv_tabix) = sy-tabix.
        "Busca linha correta para adicioncar
        DO.
          IF ls_doc_aux-vgpos LT iv_itm.
            lv_tabix = lv_tabix + 1.
            READ TABLE gt_dados INTO DATA(ls_doc_aux_2)
              INDEX lv_tabix.
            IF sy-subrc IS NOT INITIAL OR
               ls_doc_aux_2-vgbel NE iv_doc.
              EXIT.
            ELSE.
              ls_doc_aux = ls_doc_aux_2.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.

        INSERT INITIAL LINE INTO gt_dados
          ASSIGNING <lfs_dados>
            INDEX lv_tabix.
      ELSE.
        CLEAR ls_doc_aux.
        APPEND INITIAL LINE TO gt_dados
          ASSIGNING <lfs_dados>.
      ENDIF.

*        <lfs_doc> = CORRESPONDING #( BASE ( <lfs_doc> ) ls_vbap ).
      <lfs_dados> = CORRESPONDING #( is_str ).
      <lfs_dados>-nodel      = abap_true.
      <lfs_dados>-vgbel      = iv_doc.
      <lfs_dados>-vgpos      = iv_itm.

      IF ls_doc_aux IS NOT INITIAL.
        <lfs_dados>-lgnum  = ls_doc_aux-lgnum.
        <lfs_dados>-refnr  = ls_doc_aux-refnr.
        <lfs_dados>-refnt  = ls_doc_aux-refnt.
        <lfs_dados>-datum  = ls_doc_aux-datum.
        <lfs_dados>-rbnum  = ls_doc_aux-rbnum.
***        <lfs_dados>-vbeln  = ls_doc_aux-rbnum.
        <lfs_dados>-kunnr  = ls_doc_aux-kunnr.
        <lfs_dados>-kunag  = ls_doc_aux-kunag.
        <lfs_dados>-tknum  = ls_doc_aux-tknum.
        <lfs_dados>-land1  = ls_doc_aux-land1.
        <lfs_dados>-vkorg  = ls_doc_aux-vkorg.
        <lfs_dados>-vtext  = ls_doc_aux-vtext.
        <lfs_dados>-zlock  = ls_doc_aux-zlock.
        <lfs_dados>-wbstk  = ls_doc_aux-wbstk.
        <lfs_dados>-kostk  = ls_doc_aux-kostk.
*        <lfs_dados>-name1_rm1  = ls_doc_aux-name1_rm1.
*        <lfs_dados>-name1_rm2  = ls_doc_aux-name1_rm2.
        <lfs_dados>-vbtyp       = ls_doc_aux-vbtyp.
        <lfs_dados>-shtyp       = ls_doc_aux-shtyp.
        <lfs_dados>-tplst       = ls_doc_aux-tplst.
        <lfs_dados>-ernam_vttk  = ls_doc_aux-ernam_vttk.
        <lfs_dados>-erdat_vttk  = ls_doc_aux-erdat_vttk.
        <lfs_dados>-erzet_vttk  = ls_doc_aux-erzet_vttk.
        <lfs_dados>-signi       = ls_doc_aux-signi.
        <lfs_dados>-exti1       = ls_doc_aux-exti1.
        <lfs_dados>-exti2       = ls_doc_aux-exti2.
        <lfs_dados>-dtdis       = ls_doc_aux-dtdis.
        <lfs_dados>-tdlnr       = ls_doc_aux-tdlnr.
        <lfs_dados>-sdabw       = ls_doc_aux-sdabw.
        <lfs_dados>-add01       = ls_doc_aux-add01.
        <lfs_dados>-add02       = ls_doc_aux-add02.
        <lfs_dados>-add03       = ls_doc_aux-add03.
        <lfs_dados>-add04       = ls_doc_aux-add04.
        <lfs_dados>-dalbg       = ls_doc_aux-dalbg.
        <lfs_dados>-tndrrc      = ls_doc_aux-tndrrc.
        <lfs_dados>-tndr_actp      = ls_doc_aux-tndr_actp.
        <lfs_dados>-tndrrc_t    = ls_doc_aux-tndrrc_t.
        <lfs_dados>-tndrst      = ls_doc_aux-tndrst.
*        <lfs_dados>-netwr      = ls_doc_aux-netwr.
        <lfs_dados>-tp_carro    = ls_doc_aux-tp_carro.
        <lfs_dados>-name1_tdlnr = ls_doc_aux-name1_tdlnr.
        <lfs_dados>-volum_likp = ls_doc_aux-volum_likp.
        <lfs_dados>-voleh_likp = ls_doc_aux-voleh_likp.

*      ELSE.
*        BREAK-POINT.
      ENDIF.
    ENDIF.

*      APPEND INITIAL LINE TO gt_dados
*        ASSIGNING FIELD-SYMBOL(<lfs_dados>).

    READ TABLE gt_vbpa_adrc INTO DATA(ls_vbpa_adrc_we)
      WITH KEY vbeln = <lfs_dados>-rbnum
               parvw = 'WE'
        BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      LOOP AT gt_tvfptz INTO DATA(ls_tvfptz)
        WHERE tplst  EQ <lfs_dados>-tplst
          AND land1  EQ ls_vbpa_adrc_we-country
          AND fpstlz LE ls_vbpa_adrc_we-post_code1
          AND tpstlz GE ls_vbpa_adrc_we-post_code1.
        EXIT.
      ENDLOOP.
      IF sy-subrc IS NOT INITIAL.
        CLEAR ls_tvfptz.
      ENDIF.
    ELSE.
      CLEAR ls_vbpa_adrc_we.
      CLEAR ls_tvfptz.
    ENDIF.

    READ TABLE gt_vbpa_adrc INTO DATA(ls_vbpa_adrc_w1)
      WITH KEY vbeln = <lfs_dados>-rbnum
               parvw = 'W1'
        BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_vbpa_adrc_w1.
    ENDIF.

    SELECT SINGLE
           vbelv,
           posnv,
           SUM( vbfa~rfmng ) AS rfmng_sum,
           meins
      INTO @DATA(ls_vbfa)
      FROM vbfa
              WHERE vbelv   EQ @iv_doc
                AND posnv   EQ @iv_itm
                AND vbtyp_n EQ 'J'
      GROUP BY vbelv, posnv, meins.

*    READ TABLE gt_vbfa INTO DATA(ls_vbfa)
*      WITH KEY vbelv = iv_doc
*               posnv = iv_itm
*               BINARY SEARCH.
*    IF sy-subrc IS NOT INITIAL.
*      CLEAR ls_vbfa.
*    ENDIF.

    SELECT SINGLE
           ekbe~ebeln,
           ekbe~ebelp,
           SUM( ekbe~menge ) AS menge_sum,
           ekpo~meins
      INTO @DATA(ls_ekbe)
      FROM ekbe
       INNER JOIN ekpo
       ON  ekpo~ebeln EQ ekbe~ebeln
       AND ekpo~ebelp EQ ekbe~ebelp
              WHERE ekbe~ebeln EQ @iv_doc
                AND ekbe~ebelp EQ @iv_itm
                AND ekbe~vgabe EQ '8'
      GROUP BY ekbe~ebeln, ekbe~ebelp, ekpo~meins.
*    READ TABLE gt_ekbe INTO DATA(ls_ekbe)
*      WITH KEY ebeln = iv_doc
*               ebelp = iv_itm
*               BINARY SEARCH.
*    IF sy-subrc IS NOT INITIAL.
*      CLEAR ls_ekbe.
*    ENDIF.

    READ TABLE gt_ztransp_h INTO DATA(ls_ztransp)
      WITH KEY tknum = <lfs_dados>-tknum
               BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_ztransp.
    ELSE.
      <lfs_dados>-netwr   = ls_ztransp-netwr.
      IF <lfs_dados>-tndrrc_t IS INITIAL.
        <lfs_dados>-tndrrc_t = ls_ztransp-ddtext.
        <lfs_dados>-tndrst   = ls_ztransp-tndrst.
      ENDIF.
    ENDIF.

    READ TABLE gt_zroutyn_t01_i INTO DATA(ls_zroutyn_t01_i)
      WITH KEY vbeln = iv_doc
               BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_zroutyn_t01_i.
    ENDIF.

    READ TABLE gt_zwm006_aux INTO DATA(ls_zwm006_aux)
      WITH KEY n_transporte = <lfs_dados>-tknum
               BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_zwm006_aux.
    ENDIF.

    <lfs_dados>-qtd_forn_vbfa = ls_vbfa-rfmng_sum.
    <lfs_dados>-qtd_forn_ekbe = ls_ekbe-menge_sum.
    <lfs_dados>-qtd_fornecida = ls_vbfa-rfmng_sum
                              + ls_ekbe-menge_sum.
*    <lfs_dados>-qtd_fornecida = ls_vbfa-rfmng_sum.
    <lfs_dados>-qtd_dif       = iv_qtd "ls_vbap-kwmeng
                              - <lfs_dados>-qtd_fornecida.

    <lfs_dados>-ebeln = ls_doc-ebeln.

    IF ls_doc-vbeln IS NOT INITIAL.
      IF ls_doc-abgru IN gr_abgru.
        <lfs_dados>-item_fechado = abap_true.
      ENDIF.
    ELSEIF ls_doc-ebeln IS NOT INITIAL.
      IF ls_doc-loekz IS NOT INITIAL OR
         ls_doc-elikz IS NOT INITIAL.
        <lfs_dados>-item_fechado = abap_true.
      ELSE.
        <lfs_dados>-qtd_dif = iv_qtd "<lfs_doc>-kwmeng
                            - <lfs_dados>-qtd_fornecida.
      ENDIF.
    ENDIF.

    IF <lfs_dados>-qtd_fornecida EQ 0.
      <lfs_dados>-falta_comp = abap_true.
      "@0W\QAAA 645@
      <lfs_dados>-status_ic  = gs_icon-falta_comp. "'@3C\Q@'. "icon_pattern_exclude_red.
    ELSEIF <lfs_dados>-qtd_fornecida GE iv_qtd.
      <lfs_dados>-finalizada = abap_true.
      <lfs_dados>-status_ic  = gs_icon-finalizada. "'@3C\Q@'. "icon_led_green.
    ELSE.
      <lfs_dados>-falta_parc = abap_true.
      <lfs_dados>-status_ic  = gs_icon-falta_parc. "'@3C\Q@'. "icon_led_yellow.
    ENDIF.

    <lfs_dados>-seq          = ls_zroutyn_t01_i-seq.
    <lfs_dados>-kwmeng       = iv_qtd.

    IF ls_vbpa_adrc_we IS NOT INITIAL.
      <lfs_dados>-name1_rm1   = ls_vbpa_adrc_we-name1.
      <lfs_dados>-city1       = ls_vbpa_adrc_we-city1.
      <lfs_dados>-post_code1  = ls_vbpa_adrc_we-post_code1.
    ENDIF.
    IF ls_vbpa_adrc_w1 IS NOT INITIAL.
      <lfs_dados>-name1_rm2   = ls_vbpa_adrc_w1-name1.
    ENDIF.

    <lfs_dados>-trfzn       = ls_tvfptz-trfzn.
    <lfs_dados>-bezei       = ls_tvfptz-bezei.

    <lfs_dados>-desbloquear = ls_ztransp-desbloquear.
    <lfs_dados>-zonadist    = ls_ztransp-zonadist.
    IF ls_ztransp-desbloquear EQ space.
      <lfs_dados>-icon_desb = icon_red_light.
    ELSEIF ls_ztransp-desbloquear EQ abap_true.
      <lfs_dados>-icon_desb = icon_green_light.
    ENDIF.

    <lfs_dados>-hora_reg   = ls_zwm006_aux-hora_reg  .
    <lfs_dados>-data_reg   = ls_zwm006_aux-data_reg  .
    <lfs_dados>-matricula  = ls_zwm006_aux-matricula .
    <lfs_dados>-hora_saida = ls_zwm006_aux-hora_saida.
    <lfs_dados>-data_saida = ls_zwm006_aux-data_saida.
    IF ls_zwm006_aux-data_reg IS NOT INITIAL.
      IF <lfs_dados>-data_saida IS INITIAL.
        DATA(lv_data_saida) = sy-datum.
        DATA(lv_hora_saida) = sy-uzeit.
      ELSE.
        lv_data_saida   = <lfs_dados>-data_saida.
        lv_hora_saida   = <lfs_dados>-hora_saida.
      ENDIF.
      CLEAR lv_hours.

      DATA lv_duration TYPE  sytabix.
      CALL FUNCTION 'SWI_DURATION_DETERMINE'
        EXPORTING
          start_date = ls_zwm006_aux-data_reg
          end_date   = lv_data_saida
          start_time = <lfs_dados>-hora_reg
          end_time   = lv_hora_saida
        IMPORTING
          duration   = lv_duration.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.
        DATA lv_round TYPE f.
        lv_round = lv_duration / 3600.
        <lfs_dados>-time_wait = round( val = lv_round dec = 0 mode = cl_abap_math=>round_down ).
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD init_icons.
    gs_icon-falta_comp = icon_create( iv_icon = icon_pattern_exclude_red iv_info = 'Falta Total' ).
    gs_icon-finalizada = icon_create( iv_icon = icon_led_green           iv_info = 'Finalizada' ).
    gs_icon-falta_parc = icon_create( iv_icon = icon_led_yellow          iv_info = 'Falta Parcial' ).

    gs_icon-lock_one   = icon_create( iv_icon = icon_unspecified_one     iv_info = 'Bloqueio Total' ).
    gs_icon-lock_two   = icon_create( iv_icon = icon_unspecified_two     iv_info = 'Desbloqueio PCK' ).
    gs_icon-lock_three = icon_create( iv_icon = icon_unspecified_three   iv_info = 'Desbloqueio PC' ).
    gs_icon-lock_four  = icon_create( iv_icon = icon_unspecified_four    iv_info = 'Desbloqueio PCK e PC' ).
    gs_icon-lock_five  = icon_create( iv_icon = icon_unspecified_five    iv_info = 'Desbloqueio Carga' ).

    gs_icon-transp_atu  = icon_create( iv_icon = icon_refresh            iv_info = 'Atualizar transporte' ).

    gs_icon-select_all  = icon_create( iv_icon = icon_select_all ).
    gs_icon-deselect_all  = icon_create( iv_icon = icon_deselect_all ).

  ENDMETHOD.

  METHOD ucomm_refresh.

    TRY.

*... §replace the complete data
        DATA(nodes) = gr_tree->get_nodes( ).
*   delete the existing data
        nodes->delete_all( ).

        get_dados( ).

        supply_data( ).

      CATCH cx_salv_error.
    ENDTRY.

  ENDMETHOD.

  METHOD ucomm_status_carga.
    BREAK-POINT.

  ENDMETHOD.

  METHOD icon_create.
    DATA lv_add_stdinf  TYPE icon_int.

    IF iv_info IS INITIAL.
      lv_add_stdinf = abap_true.
    ENDIF.
*      DATA icon TYPE c LENGTH 255.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = iv_icon
*       text                  = SPACE
        info                  = iv_info
        add_stdinf            = lv_add_stdinf "space
      IMPORTING
        result                = rv_icon
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        error_message         = 99
        OTHERS                = 3.

  ENDMETHOD.

  METHOD init_tree.

    IF gr_container IS NOT BOUND.
      IF cl_salv_tree=>is_offline( ) EQ if_salv_c_bool_sap=>false.
        CREATE OBJECT gr_container
          EXPORTING
            container_name = 'TREE1'.
      ENDIF.

*... §1 create an ALV table
      TRY.
          cl_salv_tree=>factory(
            EXPORTING
              r_container  = gr_container
            IMPORTING
              r_salv_tree = gr_tree
            CHANGING
              t_table      = gt_alv ).
        CATCH cx_salv_no_new_data_allowed cx_salv_error.
          EXIT.
      ENDTRY.

*      PERFORM create_tree.
      create_tree( ).

      TRY.
*... §3 Functions
*... §3.1 activate ALV generic Functions
          DATA: lr_functions TYPE REF TO cl_salv_functions_tree.

          lr_functions = gr_tree->get_functions( ).
          lr_functions->set_all( abap_true ).

*@42@	ICON_REFRESH
          lr_functions->add_function(
            EXPORTING
              name               = 'REFRESH'
              icon               = '@42@'	"ICON_REFRESH
*          text               = 'REFRESH'
              tooltip            = 'Refresh'
              position           = if_salv_c_function_position=>right_of_salv_functions
          ).
*@CB@	ICON_DELIVERY_DATE
          lr_functions->add_function(
            EXPORTING
              name               = 'FORNECER'
              icon               = '@CB@' "icon_delivery_date
              text               = 'Fornecer Faltas'
              tooltip            = 'Fornecer Faltas'
              position           = if_salv_c_function_position=>right_of_salv_functions
          ).
**@FC@  ICON_AVAILABILITY_CHECK
*          lr_functions->add_function(
*            EXPORTING
*              name               = 'STATUS_CARGA'
*              icon               = '@FC@' "ICON_AVAILABILITY_CHECK
*              text               = 'Status Carga'
*              tooltip            = 'Status Carga'
*              position           = if_salv_c_function_position=>right_of_salv_functions
*          ).

*@4B@	ICON_SELECT_ALL
          lr_functions->add_function(
            EXPORTING
              name               = 'SELECT_ALL'
              icon               = '@4B@' "ICON_SELECT_ALL
              text               = ''
              tooltip            = 'Marcar tudo'
              position           = if_salv_c_function_position=>right_of_salv_functions
          ).
*'@4D@'	"ICON_DESELECT_ALL
          lr_functions->add_function(
            EXPORTING
              name               = 'DESELECT_ALL'
              icon               = '@4D@'	"ICON_DESELECT_ALL
              text               = ''
              tooltip            = 'Desmarcar tudo'
              position           = if_salv_c_function_position=>right_of_salv_functions
          ).
*'@5Y@'	"ICON_RELEASE
          lr_functions->add_function(
            EXPORTING
              name               = 'DESBLOQ'
              icon               = '@5Y@'	"ICON_RELEASE
              text               = 'Desbloquear'
              tooltip            = 'Desbloquear Selecionados'
              position           = if_salv_c_function_position=>right_of_salv_functions
          ).
*'@3W@'	"ICON_LIST @B_LIST@ @3W@
          lr_functions->add_function(
            EXPORTING
              name               = 'UCOMM_ALV1'
              icon               = '@3W@'	"ICON_LIST
              text               = ''
              tooltip            = 'ALV'
              position           = if_salv_c_function_position=>right_of_salv_functions
          ).
*'@3W@'	"ICON_LIST @B_LIST@ @3W@
          lr_functions->add_function(
            EXPORTING
              name               = 'UCOMM_ALV2'
              icon               = '@3W@'	"ICON_LIST
              text               = ''
              tooltip            = 'ALV 2'
              position           = if_salv_c_function_position=>right_of_salv_functions
          ).
        CATCH cx_salv_existing.
        CATCH cx_salv_wrong_call.
      ENDTRY.



      register_events( ).

*%_GC 135 1
*&CALC_AVG
*&CALC_SUM

*      DATA(sel) = gr_tree->get_selections( ).
*      sel->set_selected_columns( value = VALUE #( ( 'BTGEW' ) ) ).


      set_layout( ).


*... §4 display the table
      gr_tree->display( ).

*      gr_tree->set_function( value = '&CALC_SUM' ).

    ENDIF.

*    CALL METHOD cl_gui_cfw=>flush.
*CALL METHOD CL_GUI_CFW=>dispatch.

  ENDMETHOD.

  METHOD set_layout.

    DATA: r_layout TYPE REF TO cl_salv_layout,
          key      TYPE salv_s_layout_key.

    key-report = sy-repid.

*... §4 set layout  YI3K140721
    DATA: lr_layout TYPE REF TO cl_salv_layout,
          ls_key    TYPE salv_s_layout_key.

    lr_layout = gr_tree->get_layout( ).

*... §4.1 set the Layout Key
    ls_key-report = key-report.
    lr_layout->set_key( ls_key ).

*... §4.2 set usage of default Layouts
    lr_layout->set_default( abap_true ).

*... §4.3 set Layout save restriction
    lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    IF p_varia IS NOT INITIAL.
      lr_layout->set_initial_layout( p_varia ).
    ENDIF.

  ENDMETHOD.

  METHOD set_columnname.
    gv_columnname = iv_columnname.
  ENDMETHOD.

  METHOD set_col_position.
    CHECK gv_columnname IS NOT INITIAL.
    TRY.
        IF go_columns IS NOT INITIAL.
          go_columns->set_column_position( columnname = gv_columnname
                                           position   = iv_position ).
        ENDIF.
        IF go_cols IS NOT INITIAL.
          go_cols->set_column_position( columnname = gv_columnname
                                        position   = iv_position ).
        ENDIF.
      CATCH cx_root.
        BREAK-POINT.
    ENDTRY.
  ENDMETHOD.

  METHOD set_title_text.
    IF go_columns IS NOT INITIAL.
      DATA(lo_col) = go_columns->get_column( gv_columnname ).
    ENDIF.
    IF go_cols IS NOT INITIAL.
      lo_col = go_cols->get_column( gv_columnname ).
    ENDIF.
    lo_col->set_long_text( CONV #( iv_text ) ).
    lo_col->set_medium_text( CONV #( iv_text ) ).
    lo_col->set_short_text( CONV #( iv_text ) ).
    lo_col->set_tooltip( CONV #( iv_text ) ).
  ENDMETHOD.

  METHOD build_fieldcatalog.

*if_salv_c_aggregation=>none
    CLEAR: go_columns,
           go_cols.

    go_columns = gr_tree->get_columns( ).
    go_columns->set_optimize( if_salv_c_bool_sap=>true ).

    go_aggregations = gr_tree->get_aggregations( ).
    go_aggregations->clear( ).

    DATA(lr_cols) = gr_tree->get_columns( )->get( ).

*    DATA lv_pos TYPE i VALUE 300.
*    LOOP AT lr_cols ASSIGNING FIELD-SYMBOL(<lo_column>).
*      set_columnname( <lo_column>-columnname ).
*      set_col_position( lv_pos ).
*      ADD 1 TO lv_pos.
*    ENDLOOP.

    lr_cols = gr_tree->get_columns( )->get( ).

    LOOP AT lr_cols ASSIGNING FIELD-SYMBOL(<lo_column>). " <lo_column>.
*      <lo_column>-r_column->set_f1_rollname( value = '' ).
*      data(lv_f1) = <lo_column>-r_column->get_f1_rollname( ).
      set_columnname( <lo_column>-columnname ).

      CASE <lo_column>-columnname.
        WHEN 'LVL'.
          <lo_column>-r_column->set_technical( if_salv_c_bool_sap=>true ).
*        WHEN 'VKORG' OR
*             'TKNUM' OR
*             'REFNR' OR
*             'RBNUM'.
*          <lo_column>-r_column->set_visible( abap_false ).

        WHEN 'NUMERO'     .               set_col_position( 005 ). set_title_text( 'Qtd.' ).
        WHEN 'STATUS_IC'     .            set_col_position( 006 ). set_title_text( 'Status' ).

        WHEN 'KWMENG'.
          set_col_position( 007 ).
          go_aggregations->add_aggregation( <lo_column>-columnname ).
          <lo_column>-r_column->set_decimals( '0' ).
        WHEN 'QTD_FORN_VBFA'.
*          set_col_position( 008 ).
          set_title_text( 'Forn.OV' ).
          <lo_column>-r_column->set_visible( abap_false ).
          <lo_column>-r_column->set_decimals( '0' ).
        WHEN 'QTD_FORN_EKBE'.
*          set_col_position( 009 ).
          set_title_text( 'Forn.Ped' ).
          <lo_column>-r_column->set_visible( abap_false ).
          <lo_column>-r_column->set_decimals( '0' ).
        WHEN 'TNDR_ACTP'.
          set_col_position( 009 ).
          set_title_text( <lo_column>-r_column->get_medium_text( ) ).
        WHEN 'TNDRRC_T'.
          set_col_position( 009 ).
          set_title_text( 'Status da proposta' ).
        WHEN 'ICON_DESB'.
          set_col_position( 009 ).
          set_title_text( 'Desbloquear Reg.' ).
        WHEN 'NETWR'.
          set_col_position( 009 ).
          set_title_text( 'Custo Frete' ).
        WHEN 'LFIMG'.
          set_col_position( 010 ).
          go_aggregations->add_aggregation( <lo_column>-columnname ).
          <lo_column>-r_column->set_decimals( '0' ).
        WHEN 'QTD_FORNECIDA'.
          set_col_position( 011 ).
*          set_title_text( 'Qtd.Fornecida' ).
          set_title_text( 'Qtd.Forn.Total' ).
          go_aggregations->add_aggregation( <lo_column>-columnname ).
          <lo_column>-r_column->set_visible( abap_false ).
          <lo_column>-r_column->set_decimals( '0' ).
        WHEN 'QTD_DIF'      .
          set_col_position( 012 ).
          set_title_text( 'Qtd. Pendente' ).
          go_aggregations->add_aggregation( <lo_column>-columnname ).
          <lo_column>-r_column->set_decimals( '0' ).

        WHEN 'VRKME'        .
          set_col_position( 010 ).
        WHEN 'TRFZN'      .               set_col_position( 014 ).
        WHEN 'BEZEI'      .               set_col_position( 015 ).
        WHEN 'TRANS'      .               set_col_position( 020 ).
        WHEN 'SIGNI'      .               set_col_position( 025 ).
        WHEN 'PAL'        .               set_col_position( 030 ).
        WHEN 'CLIENTES'  OR 'ADD01'.      set_col_position( 035 ).
*        WHEN 'PESO'      OR 'BTGEW'.     set_col_position( 08 ). go_aggregations->add_aggregation( <lo_column>-columnname ).
        WHEN 'BRGEW'.
          set_col_position( 040 ).
          go_aggregations->add_aggregation( <lo_column>-columnname ).
*        WHEN 'UN_PESO'   OR 'GEWEI_LIKP'. set_col_position( 045 ).
        WHEN 'GEWEI'.                     set_col_position( 045 ).

*        WHEN 'VOLUME'    OR 'VOLUM_LIKP'. set_col_position( 10 ).
*        WHEN 'UN_VOLUME' OR 'VOLEH_LIKP'. set_col_position( 11 ).
        WHEN 'VOLUM'.                     set_col_position( 050 ). go_aggregations->add_aggregation( <lo_column>-columnname ).
        WHEN 'VOLEH'.                     set_col_position( 055 ).
        WHEN 'TP_CARRO'.                  set_col_position( 060 ).
        WHEN 'DTDIS'      .               set_col_position( 065 ).
        WHEN 'DALBG'      .               set_col_position( 066 ).
        WHEN 'OBSERV'    OR 'EXTI1'.      set_col_position( 070 ).
*        WHEN 'REFNR'      .               set_col_position( 075 ).
*        WHEN 'REFNT'      .               set_col_position( 080 ).
*        WHEN 'VBELN'      .               set_col_position( 085 ).
        WHEN 'KUNNR'      .               set_col_position( 090 ).
        WHEN 'LAND1'      .               set_col_position( 095 ).
        WHEN 'NAME1_RM1'  .               set_col_position( 100 ).
        WHEN 'CITY1'  .                   set_col_position( 101 ).
        WHEN 'POST_CODE1'  .               set_col_position( 102 ).
        WHEN 'NAME1_RM2'  .               set_col_position( 105 ).
        WHEN 'PALCP'      .
          set_col_position( 110 ).
          set_title_text( 'Pal.Comp' ).
*          go_aggregations->add_aggregation( <lo_column>-columnname ).
        WHEN 'PALPK'      .
          set_col_position( 115 ).
          set_title_text( 'P.Pick.1/2' ).
*          go_aggregations->add_aggregation( <lo_column>-columnname ).
        WHEN 'PALTO'      .
          set_col_position( 120 ).
          set_title_text( 'T.Pal.1/2' ).
*          go_aggregations->add_aggregation( <lo_column>-columnname ).
        WHEN 'PALPK_S'    .
          set_col_position( 125 ).
          set_title_text( 'Pal.Pick.' ).
*          go_aggregations->add_aggregation( <lo_column>-columnname ).
          <lo_column>-r_column->set_visible( abap_false ).
        WHEN 'PALTO_S'    .
          set_col_position( 130 ).
          set_title_text( 'T. Pal.' ).
*          go_aggregations->add_aggregation( <lo_column>-columnname ).
          <lo_column>-r_column->set_visible( abap_false ).
        WHEN 'DESBLOQUEAR'.
          set_col_position( 135 ).
          set_title_text( 'Desbloquear' ).
        WHEN 'SEQ'.   set_col_position( 140 ).
        WHEN 'KOSTK'. set_col_position( 145 ).
        WHEN 'WBSTK'. set_col_position( 150 ).
        WHEN 'ZLOCK'. set_col_position( 155 ).

        WHEN 'MATRICULA'.
          set_col_position( 165 ).
          set_title_text( 'Matricula' ).
        WHEN 'DATA_REG'.
          set_col_position( 170 ).
          set_title_text( 'Dt.Reg Portaria' ).
        WHEN 'HORA_REG'.
          set_col_position( 171 ).
          set_title_text( 'Hr.Reg Portaria' ).
*        WHEN 'HORA_SAIDA'. set_col_position( 175 ).
*        WHEN 'DATA_SAIDA'. set_col_position( 180 ).
        WHEN 'TIME_WAIT'.
          set_col_position( 175 ).
          set_title_text( 'Espera(Em Horas)' ).

        WHEN OTHERS.
          <lo_column>-r_column->set_visible( abap_false ).

*          <lo_column>-r_column->set_technical( if_salv_c_bool_sap=>true ).
*          set_col_position( lv_pos ).
*          lv_pos = lv_pos + 1.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD create_tree.

    build_fieldcatalog( ).
    build_header( ).
    supply_data( ).

  ENDMETHOD.

  METHOD register_events.
*... §4 register to the events of cl_salv_table
    DATA: lr_events TYPE REF TO cl_salv_events_tree.

    lr_events = gr_tree->get_event( ).

    CREATE OBJECT gr_events EXPORTING ir_tree = gr_tree.
*                                      io_alv  = me.

*... §4.1 register to the event USER_COMMAND
    SET HANDLER gr_events->on_user_command FOR lr_events.

    SET HANDLER gr_events->on_before_user_command FOR lr_events.

    SET HANDLER gr_events->on_after_user_command FOR lr_events.

    SET HANDLER gr_events->on_link_click      FOR lr_events.
    SET HANDLER gr_events->on_double_click    FOR lr_events.
    SET HANDLER gr_events->on_checkbox_change FOR lr_events.

  ENDMETHOD.

  METHOD build_header.

*... § 2.1 build the hierarchy header
    DATA: settings TYPE REF TO cl_salv_tree_settings.

    settings = gr_tree->get_tree_settings( ).
    settings->set_hierarchy_header( 'Org. Vendas/Transporte/Grupo/Remessa/Material'(hd1) ).
    settings->set_hierarchy_tooltip( 'Hierarquia'(ht1) ).
    settings->set_hierarchy_size( 45 ).

*  settings->set_hierarchy_icon( '@3Q@' ). "ICON_OVERVIEW

    DATA: title TYPE salv_de_tree_text.
    title = sy-title.
    settings->set_header( title ).


*    rs_header-heading = 'Org. Vendas/Transporte/Grupo/Remessa/Material'(hd1).
*    rs_header-tooltip = 'Hierarquia'(ht1).
*
*    rs_header-width = 50.
*    rs_header-width_pix = ''.
  ENDMETHOD.

  METHOD supply_data.

    SORT gt_dados BY vkorg tknum refnr seq rbnum matnr.

    DATA ls_dados_aux LIKE LINE OF gt_dados.
    DATA: lv_head_key TYPE salv_de_node_key.
    DATA: lv_vkorg_key TYPE salv_de_node_key.
    DATA: lv_tknum_cont TYPE i.

    DATA: BEGIN OF ls_head_line,
            palcp   TYPE zwm_s001-palcp,
            palpk   TYPE zwm_s001-palpk,
            palto   TYPE zwm_s001-palto,
            palpk_s TYPE zwm_s001-palpk_s,
            palto_s TYPE zwm_s001-palto_s,
          END OF ls_head_line.
    DATA: BEGIN OF ls_vkorg_line,
            lvl     TYPE zwm_s001-lvl,
            lgnum   TYPE t311-lgnum,
            refnr   TYPE t311-refnr,
            refnt   TYPE t311-refnt,
            datum   TYPE t311-datum,
*            rbnum TYPE t311a-rbnum,
            vbeln   TYPE likp-vbeln,
            vstel   TYPE likp-vstel,
            vkorg   TYPE likp-vkorg,
*            kunnr TYPE likp-kunnr,
*            kunag TYPE likp-kunag,
*            btgew TYPE likp-btgew,
            vtext   TYPE tvkot-vtext,
            palcp   TYPE zwm_s001-palcp,
            palpk   TYPE zwm_s001-palpk,
            palto   TYPE zwm_s001-palto,
            palpk_s TYPE zwm_s001-palpk_s,
            palto_s TYPE zwm_s001-palto_s,
          END OF ls_vkorg_line.
    DATA: BEGIN OF ls_tknum_line,
            lvl         TYPE zwm_s001-lvl,
            tknum       TYPE vttp-tknum,
            tpnum       TYPE vttp-tpnum,
            vbtyp       TYPE vttk-vbtyp,
            shtyp       TYPE vttk-shtyp,
            tplst       TYPE vttk-tplst,
            ernam_vttk  TYPE vttk-ernam,
            erdat_vttk  TYPE vttk-erdat,
            erzet_vttk  TYPE vttk-erzet,
            signi       TYPE vttk-signi,
            exti1       TYPE vttk-exti1,
            exti2       TYPE vttk-exti2,
            dtdis       TYPE vttk-dtdis,
            dalbg       TYPE vttk-dalbg,
            tdlnr       TYPE vttk-tdlnr,
            sdabw       TYPE vttk-sdabw,
            add01       TYPE vttk-add01,
            add02       TYPE vttk-add02,
            add03       TYPE vttk-add03,
            add04       TYPE vttk-add04,
            trfzn       TYPE tvfptz-trfzn,
            bezei       TYPE tvftzt-bezei,
            tndrrc      TYPE zwm_s001-tndrrc,
            tndrrc_t    TYPE zwm_s001-tndrrc_t,
            tndr_actp   TYPE zwm_s001-tndr_actp,
            tndrst      TYPE zwm_s001-tndrst,
            netwr       TYPE zwm_s001-netwr,
            icon_desb   TYPE zwm_s001-icon_desb,
            hora_reg    TYPE zwm_s001-hora_reg,
            data_reg    TYPE zwm_s001-data_reg,
            matricula   TYPE zwm_s001-matricula,
            hora_saida  TYPE zwm_s001-hora_saida,
            data_saida  TYPE zwm_s001-data_saida,
            time_wait   TYPE zwm_s001-time_wait,
            name1_tdlnr TYPE zwm_s001-name1_tdlnr,
            palcp       TYPE zwm_s001-palcp,
            palpk       TYPE zwm_s001-palpk,
            palto       TYPE zwm_s001-palto,
            palpk_s     TYPE zwm_s001-palpk_s,
            palto_s     TYPE zwm_s001-palto_s,
          END OF ls_tknum_line.
    DATA: BEGIN OF ls_refnr_line,
            lvl     TYPE zwm_s001-lvl,
            lgnum   TYPE t311-lgnum,
            refnr   TYPE t311-refnr,
            refnt   TYPE t311-refnt,
            datum   TYPE t311-datum,
            palcp   TYPE zwm_s001-palcp,
            palpk   TYPE zwm_s001-palpk,
            palto   TYPE zwm_s001-palto,
            palpk_s TYPE zwm_s001-palpk_s,
            palto_s TYPE zwm_s001-palto_s,
*            palcp_grp   TYPE zwm_s001-palcp,
*            palpk_grp   TYPE zwm_s001-palpk,
*            palto_grp   TYPE zwm_s001-palto,
*            palpk_s_grp TYPE zwm_s001-palpk_s,
*            palto_s_grp TYPE zwm_s001-palto_s,
          END OF ls_refnr_line.
    DATA: BEGIN OF ls_rbnum_line,
            lvl        TYPE zwm_s001-lvl,
            rbnum      TYPE t311a-rbnum,
            vbeln      TYPE likp-vbeln,
            vstel      TYPE likp-vstel,
            vkorg      TYPE likp-vkorg,
            kunnr      TYPE likp-kunnr,
            kunag      TYPE likp-kunag,
            btgew      TYPE likp-btgew,
            vtweg      TYPE vbak-vtweg,
            spart      TYPE vbak-spart,
            vkgrp      TYPE vbak-vkgrp,
            vkbur      TYPE vbak-vkbur,
            gsber      TYPE vbak-gsber,
            name1_rm1  TYPE zwm_s001-name1_rm1,
            city1      TYPE zwm_s001-city1,
            post_code1 TYPE zwm_s001-post_code1,
            name1_rm2  TYPE zwm_s001-name1_rm2,
            kostk      TYPE zwm_s001-kostk,
            wbstk      TYPE zwm_s001-wbstk,
            status_ic  TYPE zwm_s001-status_ic,
            zlock      TYPE zwm_s001-zlock,
            palcp      TYPE zwm_s001-palcp,
            palpk      TYPE zwm_s001-palpk,
            palto      TYPE zwm_s001-palto,
            palpk_s    TYPE zwm_s001-palpk_s,
            palto_s    TYPE zwm_s001-palto_s,
            volum_likp TYPE likp-volum,
            voleh_likp TYPE likp-voleh,
          END OF ls_rbnum_line.
    DATA: BEGIN OF ls_item_line,
            lvl           TYPE zwm_s001-lvl,
            rbnum         TYPE t311a-rbnum,
            vbeln         TYPE lips-vbeln,
            posnr         TYPE lips-posnr,
            ebeln         TYPE ekpo-ebeln,
            ebelp         TYPE ekpo-ebelp,
            pstyv         TYPE lips-pstyv,
            ernam         TYPE lips-ernam,
            erzet         TYPE lips-erzet,
            erdat         TYPE lips-erdat,
            matnr         TYPE lips-matnr,
            werks         TYPE lips-werks,
            lgort         TYPE lips-lgort,
            charg         TYPE lips-charg,
            lfimg         TYPE lips-lfimg,
            meins         TYPE lips-meins,
            vrkme         TYPE lips-vrkme,
            ntgew         TYPE lips-ntgew,
            brgew         TYPE lips-brgew,
            gewei         TYPE lips-gewei,
            volum         TYPE lips-volum,
            voleh         TYPE lips-voleh,
            vgbel         TYPE lips-vgbel,
            vgpos         TYPE lips-vgpos,
            matwa         TYPE vbap-matwa,
            matkl         TYPE vbap-matkl,
            arktx         TYPE vbap-arktx,
            abgru         TYPE vbap-abgru,
            kwmeng        TYPE vbap-kwmeng,
            vbelv         TYPE vbap-vbelv,
            posnv         TYPE vbap-posnv,
            vstel         TYPE vbap-vstel,
            maktx         TYPE makt-maktx,
            qtd_dif       TYPE zwm_s001-qtd_dif,
            qtd_fornecida TYPE zwm_s001-qtd_fornecida,
            qtd_forn_ekbe TYPE zwm_s001-qtd_forn_ekbe,
            qtd_forn_vbfa TYPE zwm_s001-qtd_forn_vbfa,
            status_ic     TYPE zwm_s001-status_ic,
            desbloquear   TYPE zwm_s001-desbloquear,
            seq           TYPE zwm_s001-seq,
          END OF ls_item_line.

    DATA: BEGIN OF ls_data_chg,
            vkorg,
            tknum,
            refnr,
            rbnum,
          END OF ls_data_chg.

    IF gt_dados IS INITIAL.
      EXIT.
    ENDIF.

    ls_head_line = CORRESPONDING #( gs_pal_sum-head ).

    lv_head_key = add_first_line( is_data = CORRESPONDING #( ls_head_line ) ).
*    DATA lv_icon_rbnum TYPE icon-id.
    LOOP AT gt_dados INTO DATA(ls_dados).
      "'Org. Vendas/Transporte/Grupo/Remessa/Material'
      "
      "'Org. Vendas
      IF ls_dados_aux-vkorg NE ls_dados-vkorg.
        ls_data_chg-vkorg = abap_true.
        set_total_carga_vkorg( iv_key  = lv_vkorg_key
                               iv_cont = lv_tknum_cont ).
        CLEAR lv_tknum_cont.
        ls_vkorg_line = CORRESPONDING #( ls_dados ).
        ls_vkorg_line-lvl = 0.
        lv_vkorg_key = add_vkorg_line( is_data = CORRESPONDING #( ls_vkorg_line )
                                       iv_key  = lv_head_key ).
      ENDIF.
      "           /Transporte
      IF ls_dados_aux-tknum NE ls_dados-tknum OR
         ls_data_chg-vkorg EQ abap_true.
        ls_data_chg-tknum = abap_true.
        lv_tknum_cont = lv_tknum_cont + 1.
        ls_tknum_line = CORRESPONDING #( ls_dados ).
        ls_tknum_line-lvl = 1.
        DATA(lv_tknum_key) = add_new_line( is_data       = CORRESPONDING #( ls_tknum_line )
                                           iv_key        = lv_vkorg_key
                                           iv_text_field = 'TKNUM' ).
      ENDIF.
      "                       /Grupo
      IF ls_dados_aux-refnr NE ls_dados-refnr OR
         ls_data_chg-vkorg  EQ abap_true       OR
         ls_data_chg-tknum  EQ abap_true.
        ls_data_chg-refnr = abap_true.
        ls_refnr_line     = CORRESPONDING #( ls_dados ).
        ls_refnr_line-lvl = 2.
        DATA(lv_refnr_key) = add_new_line( is_data       = CORRESPONDING #( ls_refnr_line )
                                           iv_key        = lv_tknum_key
                                           iv_text_field = 'REFNR' ).
      ENDIF.

      "                               /Remessa
      IF ls_dados_aux-rbnum NE ls_dados-rbnum OR
         ls_data_chg-vkorg EQ abap_true       OR
         ls_data_chg-tknum EQ abap_true       OR
         ls_data_chg-refnr EQ abap_true.
        ls_rbnum_line     = CORRESPONDING #( ls_dados ).
        ls_rbnum_line-lvl = 3.
        ls_rbnum_line-status_ic  = COND #( WHEN ls_rbnum_line-status_ic NE gs_icon-finalizada
                                           THEN gs_icon-falta_parc
                                           ELSE gs_icon-finalizada ).
        DATA(lv_icon_rbnum) = ls_rbnum_line-status_ic.
        DATA(lv_rbnum_key) = add_new_line( is_data       = CORRESPONDING #( ls_rbnum_line )
                                           iv_key        = lv_refnr_key
                                           iv_text_field = 'RBNUM' ).
      ENDIF.
      ls_item_line     = CORRESPONDING #( ls_dados ).
      ls_item_line-lvl = 4.
      add_new_line( is_data       = CORRESPONDING #( ls_item_line )
                    iv_key        = lv_rbnum_key
                    iv_text_field = 'MATNR' ).

      IF ls_dados-status_ic NE gs_icon-finalizada AND
         lv_icon_rbnum      EQ gs_icon-finalizada.
        lv_icon_rbnum = gs_icon-falta_parc.
        gr_tree->get_nodes( )->get_node( lv_rbnum_key )->get_item( 'STATUS_IC' )->set_icon( value = lv_icon_rbnum ).
      ENDIF.

      ls_dados_aux = ls_dados.
      CLEAR ls_data_chg.

    ENDLOOP.
    set_total_carga_vkorg( iv_key  = lv_vkorg_key
                           iv_cont = lv_tknum_cont ).

    "expandir nós
    IF p_explvl IS NOT INITIAL.
      DATA(nodes) = gr_tree->get_nodes( ).
      DATA(all_nodes) = nodes->get_all_nodes( ).
      READ TABLE all_nodes INTO DATA(ls_node) INDEX 1.
      ls_node-node->expand(
*        EXPORTING
*          complete_subtree = ABAP_FALSE
          level            = p_explvl
      ).
    ENDIF.

* calculate totals
*    CALL METHOD gr_tree->UPDATE_CALCULATIONS.

  ENDMETHOD.

  METHOD set_total_carga_vkorg.
    FIELD-SYMBOLS <lfs_alv> TYPE zwm_s001.
    IF iv_key IS NOT INITIAL.
      DATA(lr_node) = gr_tree->get_nodes( )->get_node( iv_key ).
      DATA(ls_data_row) = lr_node->get_data_row( ).
*      ASSIGN ls_data_row->* TO FIELD-SYMBOL(<ls_data>).
*      ASSIGN COMPONENT 'NUMERO'
*        OF STRUCTURE <ls_data>
*          TO FIELD-SYMBOL(<lfv_num>).
*      <lfv_num> = iv_cont.
*      lr_node->set_data_row( <ls_data> ).
      ASSIGN ls_data_row->* TO <lfs_alv>.
      <lfs_alv>-numero = iv_cont.
      lr_node->set_data_row( <lfs_alv> ).
    ENDIF.
  ENDMETHOD.

  METHOD add_first_line.

*... §0 working with nodes
    DATA(nodes)   = gr_tree->get_nodes( ).
*    DATA(ls_data) = is_data.

    TRY.
*  ... §0.1 add a new node
*  ... §0.3 set the data for the nes node
        DATA(node) = nodes->add_node( related_node = ''
                                      data_row     = is_data "ls_data
                                      relationship = cl_gui_column_tree=>relat_last_child ).
        DATA(item) = node->get_hierarchy_item( ).

*        item->set_type( value = if_salv_c_item_type=>link ).
*        item->set_icon( CONV #( gs_icon-select_all )  ).

        node->set_text( CONV #( |HEAD| ) ).
*        node->expand( ).
*        node->expand(
*          EXPORTING
**            complete_subtree = abap_true
*            level            = 2
*        ).
**          CATCH cx_salv_msg.    "


        rv_key = node->get_key( ).
      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD.

  METHOD add_vkorg_line.

*... §0 working with nodes
    DATA(nodes) = gr_tree->get_nodes( ).
    DATA(ls_data) = is_data.
    CLEAR ls_data-status_ic.


    READ TABLE gs_pal_sum-vkorg ASSIGNING FIELD-SYMBOL(<lfs_pal_tot_vkorg>)
      WITH KEY vkorg = ls_data-vkorg
      BINARY SEARCH.
    ls_data = CORRESPONDING #( BASE ( ls_data ) <lfs_pal_tot_vkorg>-totais ).
*    ls_data-palcp   = ls_totais-palcp.
*    ls_data-palpk   = ls_totais-palpk.
*    ls_data-palto   = ls_totais-palto.
*    ls_data-palpk_s = ls_totais-palpk_s.
*    ls_data-palto_s = ls_totais-palto_s.


    TRY.
*  ... §0.1 add a new node
*  ... §0.3 set the data for the nes node
        DATA(node) = nodes->add_node( related_node = iv_key "''
                                      data_row     = ls_data
                                      relationship = cl_gui_column_tree=>relat_last_child ).
        DATA(item) = node->get_hierarchy_item( ).

        item->set_type( value = if_salv_c_item_type=>link ).
        item->set_icon( CONV #( gs_icon-select_all )  ).

        node->set_text( CONV #( |{ is_data-vkorg }-{ is_data-vtext }| ) ).
*        node->expand( ).
*        node->expand(
*          EXPORTING
**            complete_subtree = abap_true
*            level            = 2
*        ).
**          CATCH cx_salv_msg.    "


        rv_key = node->get_key( ).
      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD.

  METHOD add_new_line.

    DATA lv_text TYPE lvc_value.

    DATA(nodes) = gr_tree->get_nodes( ).

    DATA(ls_data) = is_data.
    CLEAR ls_data-numero.

    DATA(lv_ic) = ls_data-status_ic.
    CLEAR ls_data-status_ic.

    DATA(lv_ic2) = ls_data-icon_desb.
    CLEAR ls_data-icon_desb.

    DATA lv_ic_zlock TYPE salv_de_tree_image.
    CASE ls_data-zlock.
      WHEN 1. lv_ic_zlock = gs_icon-lock_one.
      WHEN 2. lv_ic_zlock = gs_icon-lock_two.
      WHEN 3. lv_ic_zlock = gs_icon-lock_three.
      WHEN 4. lv_ic_zlock = gs_icon-lock_four.
      WHEN 5. lv_ic_zlock = gs_icon-lock_five.
      WHEN OTHERS. lv_ic_zlock = abap_false.
    ENDCASE.
    CLEAR ls_data-zlock.

    CASE iv_text_field.
      WHEN 'TKNUM'.
        ls_data-numero = 1.
        READ TABLE gs_pal_sum-tknum ASSIGNING FIELD-SYMBOL(<lfs_pal_tot_tknum>)
          WITH KEY tknum = ls_data-tknum
          BINARY SEARCH.
        DATA(ls_totais) = <lfs_pal_tot_tknum>-totais.
      WHEN 'REFNR'.
        READ TABLE gs_pal_sum-refnr ASSIGNING FIELD-SYMBOL(<lfs_pal_tot_refnr>)
          WITH KEY refnr = ls_data-refnr
          BINARY SEARCH.
        ls_totais = <lfs_pal_tot_refnr>-totais.
*        ls_data-palcp   = ls_data-palcp_grp  .
*        ls_data-palpk   = ls_data-palpk_grp  .
*        ls_data-palto   = ls_data-palto_grp  .
*        ls_data-palpk_s = ls_data-palpk_s_grp.
*        ls_data-palto_s = ls_data-palto_s_grp.
      WHEN OTHERS.
        ls_totais = CORRESPONDING #( ls_data ).
    ENDCASE.
    ls_data-palcp   = ls_totais-palcp.
    ls_data-palpk   = ls_totais-palpk.
    ls_data-palto   = ls_totais-palto.
    ls_data-palpk_s = ls_totais-palpk_s.
    ls_data-palto_s = ls_totais-palto_s.

    TRY.
        DATA(node) = nodes->add_node( related_node = iv_key
                                      data_row     = ls_data
                                      relationship = cl_gui_column_tree=>relat_last_child ).
        DATA(item) = node->get_hierarchy_item( ).

        ASSIGN COMPONENT iv_text_field
          OF STRUCTURE is_data
            TO FIELD-SYMBOL(<lfv_text_field>).
        CASE iv_text_field.
          WHEN 'TKNUM'.
            lv_text = <lfv_text_field>.
            IF is_data-name1_tdlnr IS NOT INITIAL.
              lv_text = |{ lv_text }-{ is_data-name1_tdlnr }|.
            ENDIF.
            item->set_type( value = if_salv_c_item_type=>link ).
*            node->get_parent( )->get_data_row( ).
*          node->get_item( 'TNDRRC_T' )->set_icon( value = lv_ic ).


            node->get_item( 'TNDRRC_T' )->set_icon( gs_icon-transp_atu ).
            node->get_item( 'TNDRRC_T' )->set_type( if_salv_c_item_type=>link ).

            node->get_item( 'ICON_DESB' )->set_icon( lv_ic2 ).
            node->get_item( 'ICON_DESB' )->set_type( if_salv_c_item_type=>link ).

*            IF ls_data-tndrrc_t EQ 'Não oferecido ao agente de frete'.
*              node->get_item( 'TNDRRC_T' )->set_style( cl_gui_column_tree=>style_emphasized ). "amarelo
*            ELSE.
*              node->get_item( 'TNDRRC_T' )->set_style( cl_gui_column_tree=>style_emphasized_positive ). "verde
*            ENDIF.

            CASE ls_data-tndrrc_t. "ls_data-tndrst.
              WHEN 'Não oferecido ao agente de frete'.
                DATA(lv_style) = cl_gui_column_tree=>style_emphasized.
              WHEN 'Aceito pelo agente de frete'.
                lv_style = cl_gui_column_tree=>style_emphasized_positive.
              WHEN 'Disponivel no Portal - Publicado'.
                lv_style = cl_gui_column_tree=>style_emphasized_a.
              WHEN 'Recusado pelo agente de frete'.
                lv_style = cl_gui_column_tree=>style_emphasized_b.
              WHEN 'Atribuido ao Transportador'.
                lv_style = cl_gui_column_tree=>style_intensified.
              WHEN 'Disponivel no Portal - Publicado'.
                lv_style = cl_gui_column_tree=>style_default.
              WHEN OTHERS.
                lv_style = cl_gui_column_tree=>style_inactive.
            ENDCASE.
            node->get_item( 'TNDRRC_T' )->set_style( lv_style ).

            IF ls_data-time_wait GE 3.
              node->get_item( 'TIME_WAIT' )->set_style( cl_gui_column_tree=>style_emphasized_negative ).
            ELSE.
              node->get_item( 'TIME_WAIT' )->set_style( cl_gui_column_tree=>style_emphasized_positive ).
            ENDIF.

            node->set_row_style( value = cl_gui_column_tree=>style_emphasized_c  ).

          WHEN 'REFNR'. "Grupo
            lv_text = |{ is_data-refnr }-{ is_data-refnt }|.
            item->set_type( value = if_salv_c_item_type=>link ).
*            node->set_row_style( value = cl_gui_column_tree=>style_emphasized_positive  ).
*            node->expand( ).
          WHEN 'RBNUM'.
            lv_text = |{ is_data-rbnum ALPHA = OUT }|.
***            node->get_hierarchy_item( )->set_type( value = if_salv_c_item_type=>link ).
*... § add a checkbox to this node in the hierarchy column
            item->set_type( if_salv_c_item_type=>checkbox ).
            item->set_editable( abap_true ).
            IF lv_ic_zlock IS NOT INITIAL.
              node->get_item( 'ZLOCK' )->set_icon( value = lv_ic_zlock ).
            ENDIF.
          WHEN 'MATNR'.
            DATA(lv_matnr) = |{ is_data-matnr ALPHA = OUT }|. CONDENSE lv_matnr NO-GAPS.
            lv_text = |{ lv_matnr }-{ is_data-maktx }|.
*            node->set_row_style( value = cl_gui_column_tree=>style_emphasized_positive  ).
*            item->set_type( if_salv_c_item_type=>link ).
            item->set_type( if_salv_c_item_type=>checkbox ).
            item->set_editable( abap_true ).
          WHEN OTHERS.
        ENDCASE.
        node->set_text( lv_text ).

        IF lv_ic IS NOT INITIAL.
          node->get_item( 'STATUS_IC' )->set_icon( value = lv_ic ).
*          node->get_item( 'TRFZN' )->set_type( value = if_salv_c_item_type=>link ).
        ENDIF.

*        node->set_row_style( gv_col ).
*        ADD 1 TO gv_col.

        rv_key = node->get_key( ).
      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD.

  METHOD ucomm_alv.

    DATA: gr_layout TYPE REF TO cl_salv_layout.
    DATA: key       TYPE salv_s_layout_key.

    gv_tipo_alv = iv_tipo.
    IF iv_tipo EQ '1'.
      key-handle = 'ALV2'.
      DATA(lt_dados) = gt_dados.
      CASE p_explvl.
        WHEN 4.
          DELETE ADJACENT DUPLICATES FROM lt_dados COMPARING rbnum.
        WHEN 3.
          DELETE ADJACENT DUPLICATES FROM lt_dados COMPARING refnr.
        WHEN 2.
          DELETE ADJACENT DUPLICATES FROM lt_dados COMPARING tknum.
        WHEN 1.
          DELETE ADJACENT DUPLICATES FROM lt_dados COMPARING vkorg.
      ENDCASE.
      ASSIGN lt_dados TO <gft_alv2>.
    ELSEIF iv_tipo EQ '2'.
      key-handle = 'ALV3'.
      DATA(lt_alv) = gt_alv.
*      CASE p_explvl.
*        WHEN 4.
*          DELETE lt_alv WHERE rbnum IS INITIAL.
*        WHEN 3.
*          DELETE lt_alv WHERE refnr IS INITIAL.
*        WHEN 2.
*          DELETE lt_alv WHERE tknum IS INITIAL.
*        WHEN 1.
*          DELETE lt_alv WHERE vkorg IS INITIAL.
*      ENDCASE.
      ASSIGN lt_alv TO <gft_alv2>.
    ENDIF.

    CALL METHOD cl_salv_table=>factory
*    EXPORTING
*      list_display = if_salv_c_bool_sap=>true    " ALV Displayed in List Mode
      IMPORTING
        r_salv_table = gr_table_2
      CHANGING
        t_table      = <gft_alv2>.
*        t_table      = gt_dados. "gt_alv
*        t_table      = gt_alv. "gt_alv

    DATA(gr_functions) = gr_table_2->get_functions( ).
    gr_functions->set_all( abap_true ).

*    DATA(lr_cols) = gr_table_2->get_columns( ).
    CLEAR: go_columns,
           go_cols.
    go_cols = gr_table_2->get_columns( ).
    go_cols->set_optimize( ).

    "gravar layout
    key-report = sy-repid.
    gr_layout = gr_table_2->get_layout( ).
    gr_layout->set_key( key ).
    gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    "zebra
    DATA gr_display TYPE REF TO cl_salv_display_settings.
    gr_display = gr_table_2->get_display_settings( ).
    gr_display->set_striped_pattern( cl_salv_display_settings=>true ).

*... §5 object for handling the events of cl_salv_table
*    DATA: gr_events TYPE REF TO lcl_handle_events.
*... §6 register to the events of cl_salv_table
    DATA(lr_events) = gr_table_2->get_event( ).
    IF gr_events IS INITIAL.
      CREATE OBJECT gr_events.
    ENDIF.
*... §6.1 register to the event USER_COMMAND
    SET HANDLER gr_events->on_user_command FOR lr_events.
*... §6.5 register to the event LINK_CLICK
    SET HANDLER gr_events->on_link_click_2 FOR lr_events.

**********************************************************************
**********************************************************************
    go_aggregations = gr_table_2->get_aggregations( ).
    DATA: lo_col_tab  TYPE REF TO cl_salv_column_table.
    LOOP AT go_cols->get( ) ASSIGNING FIELD-SYMBOL(<lo_column>).
      <lo_column>-r_column->set_f1_rollname( value = '' ).
      set_columnname( <lo_column>-columnname ).

*...HotSpot
      lo_col_tab ?= <lo_column>-r_column.

      CASE <lo_column>-columnname.
        WHEN 'LVL'.
          <lo_column>-r_column->set_technical( if_salv_c_bool_sap=>true ).
        WHEN 'NUMERO'     .          set_col_position( 005 ). set_title_text( 'Qtd.' ).
        WHEN 'STATUS_IC'     .       set_col_position( 006 ). set_title_text( 'Status' ).
        WHEN 'KWMENG'.
          set_col_position( 007 ).
        WHEN 'QTD_FORN_VBFA'.
          set_title_text( 'Forn.OV' ).
          <lo_column>-r_column->set_visible( abap_false ).
        WHEN 'QTD_FORN_EKBE'.
          set_title_text( 'Forn.Ped' ).
          <lo_column>-r_column->set_visible( abap_false ).
        WHEN 'TNDRRC_T'.
          set_col_position( 009 ).
          set_title_text( 'Status da proposta' ).
        WHEN 'ICON_DESB'.
          set_col_position( 009 ).
          set_title_text( 'Desbloquear Reg.' ).
        WHEN 'LFIMG'.
          set_col_position( 010 ).
        WHEN 'QTD_FORNECIDA'.
          set_col_position( 011 ).
          set_title_text( 'Qtd.Forn.Total' ).
          <lo_column>-r_column->set_visible( abap_false ).
        WHEN 'QTD_DIF'      .
          set_col_position( 012 ).
          set_title_text( 'Qtd. Pendente' ).
        WHEN 'VRKME'        .
          set_col_position( 010 ).
        WHEN 'TRFZN'      .               set_col_position( 014 ).
        WHEN 'BEZEI'      .               set_col_position( 015 ).
        WHEN 'TRANS'      .               set_col_position( 020 ).
        WHEN 'SIGNI'      .               set_col_position( 025 ).
        WHEN 'PAL'        .               set_col_position( 030 ).
        WHEN 'CLIENTES'  OR 'ADD01'.      set_col_position( 035 ).
        WHEN 'BRGEW'.
          set_col_position( 040 ).
        WHEN 'GEWEI'.                     set_col_position( 045 ).
        WHEN 'VOLUM'.                     set_col_position( 050 ).
        WHEN 'VOLEH'.                     set_col_position( 055 ).
        WHEN 'TP_CARRO'.                  set_col_position( 060 ).
        WHEN 'DTDIS'      .               set_col_position( 065 ).
        WHEN 'DALBG'      .               set_col_position( 066 ).
        WHEN 'OBSERV'    OR 'EXTI1'.      set_col_position( 070 ).
        WHEN 'KUNNR'      .               set_col_position( 090 ).
        WHEN 'LAND1'      .               set_col_position( 095 ).
        WHEN 'NAME1_RM1'  .               set_col_position( 100 ).
        WHEN 'NAME1_RM2'  .               set_col_position( 105 ).
        WHEN 'PALCP'      .
          set_col_position( 110 ).
          set_title_text( 'Pal.Comp' ).
        WHEN 'PALPK'      .
          set_col_position( 115 ).
          set_title_text( 'P.Pick.1/2' ).
        WHEN 'PALTO'      .
          set_col_position( 120 ).
          set_title_text( 'T.Pal.1/2' ).
        WHEN 'PALPK_S'    .
          set_col_position( 125 ).
          set_title_text( 'Pal.Pick.' ).
          <lo_column>-r_column->set_visible( abap_false ).
        WHEN 'PALTO_S'    .
          set_col_position( 130 ).
          set_title_text( 'T. Pal.' ).
          <lo_column>-r_column->set_visible( abap_false ).
        WHEN 'DESBLOQUEAR'.
          set_col_position( 135 ).
          set_title_text( 'Desbloquear' ).
        WHEN 'SEQ'.   set_col_position( 140 ).
        WHEN 'KOSTK'. set_col_position( 145 ).
        WHEN 'WBSTK'. set_col_position( 150 ).
        WHEN 'ZLOCK'. set_col_position( 155 ).

        WHEN 'MATRICULA'.
          set_col_position( 165 ).
          set_title_text( 'Matricula' ).
        WHEN 'DATA_REG'.
          set_col_position( 170 ).
          set_title_text( 'Dt.Reg Portaria' ).
        WHEN 'HORA_REG'.
          set_col_position( 171 ).
          set_title_text( 'Hr.Reg Portaria' ).
        WHEN 'TIME_WAIT'.
          set_col_position( 175 ).
          set_title_text( 'Espera(Em Horas)' ).
        WHEN 'TKNUM' OR
             'REFNR' OR
             'MATNR' OR
             'RBNUM' OR
             'VBELN' OR
             'VGBEL'.
          lo_col_tab->set_cell_type( if_salv_c_cell_type=>hotspot ).
        WHEN 'NETWR'.
          set_col_position( 009 ).
          set_title_text( 'Custo Frete' ).
        WHEN OTHERS.
          <lo_column>-r_column->set_visible( abap_false ).
      ENDCASE.
    ENDLOOP.

**********************************************************************
**********************************************************************

    CALL METHOD gr_table_2->display.

  ENDMETHOD.                    "ucomm_alv


  METHOD valida_complete_data.

    DATA lv_hours      TYPE /sdf/cmo_time-hours.

    ASSIGN cs_data TO FIELD-SYMBOL(<lfs_dados>).

    READ TABLE gt_vbpa_adrc INTO DATA(ls_vbpa_adrc_we)
      WITH KEY vbeln = <lfs_dados>-rbnum
               parvw = 'WE'
        BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      LOOP AT gt_tvfptz INTO DATA(ls_tvfptz)
        WHERE tplst  EQ <lfs_dados>-tplst
          AND land1  EQ ls_vbpa_adrc_we-country
          AND fpstlz LE ls_vbpa_adrc_we-post_code1
          AND tpstlz GE ls_vbpa_adrc_we-post_code1.
        EXIT.
      ENDLOOP.
      IF sy-subrc IS NOT INITIAL.
        CLEAR ls_tvfptz.
      ENDIF.
    ELSE.
      CLEAR ls_vbpa_adrc_we.
      CLEAR ls_tvfptz.
    ENDIF.

    READ TABLE gt_vbpa_adrc INTO DATA(ls_vbpa_adrc_w1)
      WITH KEY vbeln = <lfs_dados>-rbnum
               parvw = 'W1'
        BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_vbpa_adrc_w1.
    ENDIF.

    SELECT SINGLE
           vbelv,
           posnv,
           SUM( vbfa~rfmng ) AS rfmng_sum,
           meins
      INTO @DATA(ls_vbfa)
      FROM vbfa
              WHERE vbelv   EQ @<lfs_dados>-vgbel "iv_doc
                AND posnv   EQ @<lfs_dados>-vgpos "iv_itm
                AND vbtyp_n EQ 'J'
      GROUP BY vbelv, posnv, meins.

*    READ TABLE gt_vbfa INTO DATA(ls_vbfa)
*      WITH KEY vbelv = iv_doc
*               posnv = iv_itm
*               BINARY SEARCH.
*    IF sy-subrc IS NOT INITIAL.
*      CLEAR ls_vbfa.
*    ENDIF.

    SELECT SINGLE
           ekbe~ebeln,
           ekbe~ebelp,
           SUM( ekbe~menge ) AS menge_sum,
           ekpo~meins
      INTO @DATA(ls_ekbe)
      FROM ekbe
       INNER JOIN ekpo
       ON  ekpo~ebeln EQ ekbe~ebeln
       AND ekpo~ebelp EQ ekbe~ebelp
              WHERE ekbe~ebeln EQ @<lfs_dados>-vgbel "iv_doc
                AND ekbe~ebelp EQ @<lfs_dados>-vgpos "iv_itm
                AND ekbe~vgabe EQ '8'
      GROUP BY ekbe~ebeln, ekbe~ebelp, ekpo~meins.
*    READ TABLE gt_ekbe INTO DATA(ls_ekbe)
*      WITH KEY ebeln = iv_doc
*               ebelp = iv_itm
*               BINARY SEARCH.
*    IF sy-subrc IS NOT INITIAL.
*      CLEAR ls_ekbe.
*    ENDIF.

    READ TABLE gt_ztransp_h INTO DATA(ls_ztransp)
      WITH KEY tknum = <lfs_dados>-tknum
               BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_ztransp.
    ELSE.
      <lfs_dados>-netwr   = ls_ztransp-netwr.
      IF <lfs_dados>-tndrrc_t IS INITIAL.
        <lfs_dados>-tndrrc_t = ls_ztransp-ddtext.
        <lfs_dados>-tndrst   = ls_ztransp-tndrst.
      ENDIF.
    ENDIF.

    READ TABLE gt_zroutyn_t01_i INTO DATA(ls_zroutyn_t01_i)
      WITH KEY vbeln = <lfs_dados>-vgbel "iv_doc
               BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_zroutyn_t01_i.
    ENDIF.

    READ TABLE gt_zwm006_aux INTO DATA(ls_zwm006_aux)
      WITH KEY n_transporte = <lfs_dados>-tknum
               BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ls_zwm006_aux.
    ENDIF.

    SELECT SINGLE kwmeng
      FROM vbap
      INTO @DATA(lv_kwmeng)
      WHERE vbeln EQ @<lfs_dados>-vgbel
        AND posnr EQ @<lfs_dados>-vgpos.
    SELECT SINGLE menge
      FROM ekpo
      INTO @DATA(lv_menge)
      WHERE ebeln EQ @<lfs_dados>-vgbel
        AND ebelp EQ @<lfs_dados>-vgpos.
    <lfs_dados>-kwmeng = lv_kwmeng + lv_menge.

    <lfs_dados>-qtd_forn_vbfa = ls_vbfa-rfmng_sum.
    <lfs_dados>-qtd_forn_ekbe = ls_ekbe-menge_sum.
    <lfs_dados>-qtd_fornecida = ls_vbfa-rfmng_sum
                              + ls_ekbe-menge_sum.
*    <lfs_dados>-qtd_fornecida = ls_vbfa-rfmng_sum.
    <lfs_dados>-qtd_dif       = <lfs_dados>-kwmeng "iv_qtd "ls_vbap-kwmeng
                              - <lfs_dados>-qtd_fornecida.

    IF <lfs_dados>-vbeln IS NOT INITIAL.
      IF <lfs_dados>-abgru IN gr_abgru.
        <lfs_dados>-item_fechado = abap_true.
      ENDIF.
    ELSEIF <lfs_dados>-ebeln IS NOT INITIAL.
      IF <lfs_dados>-loekz IS NOT INITIAL OR
         <lfs_dados>-elikz IS NOT INITIAL.
        <lfs_dados>-item_fechado = abap_true.
*      ELSE.
*        <lfs_dados>-qtd_dif = <lfs_dados>-kwmeng "iv_qtd "<lfs_doc>-kwmeng
*                            - <lfs_dados>-qtd_fornecida.
      ENDIF.
    ENDIF.

    IF <lfs_dados>-qtd_fornecida EQ 0.
      <lfs_dados>-falta_comp = abap_true.
      "@0W\QAAA 645@
      <lfs_dados>-status_ic  = gs_icon-falta_comp. "'@3C\Q@'. "icon_pattern_exclude_red.
    ELSEIF <lfs_dados>-qtd_fornecida GE <lfs_dados>-kwmeng. "iv_qtd.
      <lfs_dados>-finalizada = abap_true.
      <lfs_dados>-status_ic  = gs_icon-finalizada. "'@3C\Q@'. "icon_led_green.
    ELSE.
      <lfs_dados>-falta_parc = abap_true.
      <lfs_dados>-status_ic  = gs_icon-falta_parc. "'@3C\Q@'. "icon_led_yellow.
    ENDIF.

    <lfs_dados>-seq          = ls_zroutyn_t01_i-seq.
    <lfs_dados>-kwmeng       = <lfs_dados>-kwmeng. "iv_qtd.

    IF ls_vbpa_adrc_we IS NOT INITIAL.
      <lfs_dados>-name1_rm1   = ls_vbpa_adrc_we-name1.
      <lfs_dados>-city1       = ls_vbpa_adrc_we-city1.
      <lfs_dados>-post_code1  = ls_vbpa_adrc_we-post_code1.
    ENDIF.
    IF ls_vbpa_adrc_w1 IS NOT INITIAL.
      <lfs_dados>-name1_rm2   = ls_vbpa_adrc_w1-name1.
    ENDIF.

    <lfs_dados>-trfzn       = ls_tvfptz-trfzn.
    <lfs_dados>-bezei       = ls_tvfptz-bezei.

    <lfs_dados>-desbloquear = ls_ztransp-desbloquear.
    <lfs_dados>-zonadist    = ls_ztransp-zonadist.
    IF ls_ztransp-desbloquear EQ space.
      <lfs_dados>-icon_desb = icon_red_light.
    ELSEIF ls_ztransp-desbloquear EQ abap_true.
      <lfs_dados>-icon_desb = icon_green_light.
    ENDIF.

    <lfs_dados>-hora_reg   = ls_zwm006_aux-hora_reg  .
    <lfs_dados>-data_reg   = ls_zwm006_aux-data_reg  .
    <lfs_dados>-matricula  = ls_zwm006_aux-matricula .
    <lfs_dados>-hora_saida = ls_zwm006_aux-hora_saida.
    <lfs_dados>-data_saida = ls_zwm006_aux-data_saida.
    IF ls_zwm006_aux-data_reg IS NOT INITIAL.
      IF <lfs_dados>-data_saida IS INITIAL.
        DATA(lv_data_saida) = sy-datum.
        DATA(lv_hora_saida) = sy-uzeit.
      ELSE.
        lv_data_saida   = <lfs_dados>-data_saida.
        lv_hora_saida   = <lfs_dados>-hora_saida.
      ENDIF.
      CLEAR lv_hours.

      DATA lv_duration TYPE  sytabix.
      CALL FUNCTION 'SWI_DURATION_DETERMINE'
        EXPORTING
          start_date = ls_zwm006_aux-data_reg
          end_date   = lv_data_saida
          start_time = <lfs_dados>-hora_reg
          end_time   = lv_hora_saida
        IMPORTING
          duration   = lv_duration.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.
        DATA lv_round TYPE f.
        lv_round = lv_duration / 3600.
        <lfs_dados>-time_wait = round( val = lv_round dec = 0 mode = cl_abap_math=>round_down ).
      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
