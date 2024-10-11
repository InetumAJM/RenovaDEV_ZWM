*&---------------------------------------------------------------------*
*&  Include           ZWMREP0090_CLS
*&---------------------------------------------------------------------*

CLASS lcl_alv IMPLEMENTATION.
  METHOD inicialization.

    IF sy-tcode <> 'ZWM031B'.
      APPEND VALUE #( sign   = 'I' option = 'GE'
                      low    = sy-datum - 2
                    ) TO s_datum.

      APPEND VALUE #( sign   = 'I' option = 'CP'
                      low    = '*' && sy-datum+6(2) && '-' && sy-datum+4(2) && '-* X*'
                    ) TO s_refnt.

      APPEND VALUE #( sign   = 'I' option = 'CP'
                      low    = 'RP*'
                    ) TO s_vkorg.
    ENDIF.

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

    DATA: ls_likp TYPE likp.
    DATA: ls_kna1 TYPE kna1.
    DATA: ls_vttp TYPE vttp.
    DATA: ls_vttk TYPE vttk.
    DATA lv_hours      TYPE /sdf/cmo_time-hours.
    FIELD-SYMBOLS: <lfs_pal_tot_refnr> TYPE y_refnr_tot.
    FIELD-SYMBOLS: <lfs_pal_tot_tknum> TYPE y_tknum_tot.
    FIELD-SYMBOLS: <lfs_pal_tot_vkorg> TYPE y_vkorg_tot.

    DATA: lr_tknum TYPE RANGE OF ztransp_h-tknum.

    PERFORM get_dados.

**********************************************************************
    LOOP AT gt_dados_ant ASSIGNING FIELD-SYMBOL(<lfs_reg>).
      LOOP AT gt_dados_ant TRANSPORTING NO FIELDS
        WHERE vkorg NE <lfs_reg>-vkorg
          AND tknum EQ <lfs_reg>-tknum.
        MODIFY gt_dados_ant FROM <lfs_reg>
         TRANSPORTING vkorg
           WHERE tknum EQ <lfs_reg>-tknum.
        EXIT.
      ENDLOOP.
    ENDLOOP.
**********************************************************************

    REFRESH: gt_dados,
             gt_vbpa_adrc,
             gt_tvfptz,
             gt_ztransp_h,
             gr_abgru,
             gt_grupo_palestes,
             gt_zroutyn_t01_i.

    CLEAR gs_pal_sum.

    init_icons( ).

    DATA lr_pstyv_palete TYPE RANGE OF lips-pstyv.
    lr_pstyv_palete = VALUE #( sign = 'I' option = 'EQ'
                                ( low = 'ZPAL' )
                                ( low = 'ZPAS' )
                               ).


    lr_tknum = VALUE #( FOR ls_doc IN gt_vttk
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
          WHERE ztransp_h~tknum IN @lr_tknum.
      SORT gt_ztransp_h BY tknum.

      SELECT *
        FROM zwm006_aux
        INTO TABLE gt_zwm006_aux
        WHERE armazem      EQ p_lgnum
          AND n_transporte IN lr_tknum.
      SORT gt_zwm006_aux BY n_transporte.
    ENDIF.

    IF gt_vbap IS NOT INITIAL.
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
        FOR ALL ENTRIES IN @gt_vbap "gt_dados
       WHERE vbeln EQ @gt_vbap-vbeln. "gt_dados-vgbel.
      SORT gt_zroutyn_t01_i BY vbeln.

      SELECT vbeln,
             posnr,
             etenr,
             edatu
        FROM vbep
        INTO TABLE @DATA(lt_vbep)
        FOR ALL ENTRIES IN @gt_vbap
        WHERE vbeln EQ @gt_vbap-vbeln
          AND posnr EQ @gt_vbap-posnr
          AND etenr EQ 0001.
    ENDIF.

    gr_abgru = VALUE #( sign = 'I' option = 'EQ'
                        ( low = '07' ) "Item Informativo
                        ( low = '20' ) "Anulação de produto - Roturas
                        ( low = '24' ) "Anulação de Encomenda
                          ).

    DATA lr_lgort TYPE RANGE OF vbap-lgort.
    lr_lgort = VALUE #( sign = 'I' option = 'EQ'
                        ( low = '' ) "Item Informativo
                        ( low = 'CD' )
                          ).
*    gt_dados[] = gt_dados_ant[].
    LOOP AT gt_dados_ant ASSIGNING FIELD-SYMBOL(<lfs_ant>). " INTO DATA(ls_ant).

**********************************************************************
      IF <lfs_pal_tot_tknum> IS NOT ASSIGNED OR
         <lfs_ant>-tknum NE <lfs_pal_tot_tknum>-tknum.
        READ TABLE gs_pal_sum-tknum ASSIGNING <lfs_pal_tot_tknum>
          WITH KEY tknum = <lfs_ant>-tknum.
        IF sy-subrc IS NOT INITIAL.
          APPEND VALUE #( tknum = <lfs_ant>-tknum ) TO gs_pal_sum-tknum ASSIGNING <lfs_pal_tot_tknum>.
        ENDIF.
      ENDIF.

      IF <lfs_pal_tot_refnr> IS NOT ASSIGNED  OR
         <lfs_ant>-tknum NE <lfs_pal_tot_refnr>-tknum OR
         <lfs_ant>-refnr NE <lfs_pal_tot_refnr>-refnr.
        READ TABLE gs_pal_sum-refnr ASSIGNING <lfs_pal_tot_refnr>
          WITH KEY tknum = <lfs_ant>-tknum
                   refnr = <lfs_ant>-refnr.
        IF sy-subrc IS NOT INITIAL.
          APPEND VALUE #( tknum = <lfs_ant>-tknum
                          refnr = <lfs_ant>-refnr ) TO gs_pal_sum-refnr ASSIGNING <lfs_pal_tot_refnr>.
        ENDIF.
      ENDIF.

      IF <lfs_pal_tot_vkorg> IS NOT ASSIGNED OR
         <lfs_ant>-vkorg NE <lfs_pal_tot_vkorg>-vkorg.
        READ TABLE gs_pal_sum-vkorg ASSIGNING <lfs_pal_tot_vkorg>
          WITH KEY vkorg = <lfs_ant>-vkorg.
        IF sy-subrc IS NOT INITIAL.
          APPEND VALUE #( vkorg = <lfs_ant>-vkorg ) TO gs_pal_sum-vkorg ASSIGNING <lfs_pal_tot_vkorg>.
        ENDIF.
      ENDIF.

      ADD <lfs_ant>-palpk_s TO gs_pal_sum-head-palpk_s.
      ADD <lfs_ant>-palto_s TO gs_pal_sum-head-palto_s.
      ADD <lfs_ant>-palcp   TO gs_pal_sum-head-palcp.
      ADD <lfs_ant>-palpk   TO gs_pal_sum-head-palpk.
      ADD <lfs_ant>-palto   TO gs_pal_sum-head-palto.

      ADD <lfs_ant>-palpk_s TO <lfs_pal_tot_refnr>-totais-palpk_s.
      ADD <lfs_ant>-palto_s TO <lfs_pal_tot_refnr>-totais-palto_s.
      ADD <lfs_ant>-palcp   TO <lfs_pal_tot_refnr>-totais-palcp.
      ADD <lfs_ant>-palpk   TO <lfs_pal_tot_refnr>-totais-palpk.
      ADD <lfs_ant>-palto   TO <lfs_pal_tot_refnr>-totais-palto.

      ADD <lfs_ant>-palpk_s TO <lfs_pal_tot_tknum>-totais-palpk_s.
      ADD <lfs_ant>-palto_s TO <lfs_pal_tot_tknum>-totais-palto_s.
      ADD <lfs_ant>-palcp   TO <lfs_pal_tot_tknum>-totais-palcp.
      ADD <lfs_ant>-palpk   TO <lfs_pal_tot_tknum>-totais-palpk.
      ADD <lfs_ant>-palto   TO <lfs_pal_tot_tknum>-totais-palto.

      ADD <lfs_ant>-palpk_s TO <lfs_pal_tot_vkorg>-totais-palpk_s.
      ADD <lfs_ant>-palto_s TO <lfs_pal_tot_vkorg>-totais-palto_s.
      ADD <lfs_ant>-palcp   TO <lfs_pal_tot_vkorg>-totais-palcp.
      ADD <lfs_ant>-palpk   TO <lfs_pal_tot_vkorg>-totais-palpk.
      ADD <lfs_ant>-palto   TO <lfs_pal_tot_vkorg>-totais-palto.

**********************************************************************

      READ TABLE gt_t311 INTO DATA(ls_t311)
        WITH KEY refnr = <lfs_ant>-refnr BINARY SEARCH.

      <lfs_ant>-lgnum = ls_t311-lgnum.
      <lfs_ant>-refnr = ls_t311-refnr.
      <lfs_ant>-refnt = ls_t311-refnt.
      <lfs_ant>-datum = ls_t311-datum.

*            t311a~rbnum,
      LOOP AT gt_lips INTO DATA(ls_lips)
        WHERE vbeln EQ <lfs_ant>-vbeln.

        IF ls_lips-posnr GE '900000'.
          CONTINUE.
        ENDIF.
        IF ls_lips-pstyv IN lr_pstyv_palete.
          CONTINUE.
        ENDIF.

        READ TABLE gt_t311a INTO DATA(ls_t311a)
          WITH KEY refnr = ls_t311-refnr
                   rbnum = ls_lips-vbeln
          BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        IF ls_likp-vbeln NE ls_lips-vbeln.
          READ TABLE gt_likp INTO ls_likp
            WITH KEY vbeln = ls_lips-vbeln
              BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            CLEAR ls_likp.
          ENDIF.
        ENDIF.
        IF ls_kna1-kunnr NE ls_likp-kunnr.
          READ TABLE gt_kna1 INTO ls_kna1
            WITH KEY kunnr = ls_likp-kunnr
              BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            CLEAR ls_kna1.
          ENDIF.
        ENDIF.

        READ TABLE gt_vttp INTO ls_vttp
          WITH KEY vbeln = ls_likp-vbeln
            BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_vttp.
        ENDIF.
        IF ls_vttk-tknum NE ls_vttp-tknum.
          READ TABLE gt_vttk INTO ls_vttk
            WITH KEY tknum = ls_vttp-tknum
              BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            CLEAR ls_vttk.
          ENDIF.
        ENDIF.

        READ TABLE gt_zroutyn_t01_i INTO DATA(ls_zroutyn_t01_i)
          WITH KEY vbeln = ls_lips-vgbel "iv_doc
                   BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_zroutyn_t01_i.
        ENDIF.

        READ TABLE gt_zwm006_aux INTO DATA(ls_zwm006_aux)
          WITH KEY n_transporte = ls_vttp-tknum
                   BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_zwm006_aux.
        ENDIF.

        READ TABLE gt_vbak INTO DATA(ls_vbak)
              WITH KEY vbeln = ls_lips-vgbel
              BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_vbak.
        ENDIF.

        READ TABLE gt_vbap INTO DATA(ls_vbap)
              WITH KEY vbeln = ls_lips-vgbel
                       posnr = ls_lips-vgpos
                       BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_vbap.
        ENDIF.

        READ TABLE lt_vbep INTO DATA(ls_vbep)
              WITH KEY vbeln = ls_lips-vgbel
                       posnr = ls_lips-vgpos
                       BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_vbep.
        ENDIF.

        READ TABLE gt_ekko INTO DATA(ls_ekko)
              WITH KEY ebeln = ls_lips-vgbel
              BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_ekko.
        ENDIF.

        READ TABLE gt_ekpo INTO DATA(ls_ekpo)
              WITH KEY ebeln = ls_lips-vgbel
                       ebelp = ls_lips-vgpos
                       BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_ekpo.
        ENDIF.

        IF ls_vbap IS NOT INITIAL.
          IF ls_vbap-abgru IN gr_abgru.
            CONTINUE.
          ENDIF.

          IF ls_vbap-lgort NOT IN lr_lgort.
            CONTINUE.
          ENDIF.

          IF ls_lips-lgort NE 'CD'.
            IF ls_vbap-taxm1 NE '1'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

        IF ls_ekpo IS NOT INITIAL.
          IF ls_ekpo-loekz IS NOT INITIAL OR
             ls_ekpo-elikz IS NOT INITIAL.
***        DELETE gt_dados WHERE vgbel EQ ls_ekko-ebeln AND
***                              vgpos EQ ls_ekko-ebelp.
            CONTINUE.
          ENDIF.
        ENDIF.


**********************************************************************
        APPEND <lfs_ant> TO gt_dados
          ASSIGNING FIELD-SYMBOL(<lfs_new>).

        ls_likp-vkorg      = <lfs_ant>-vkorg.

        "likp
        <lfs_new>-rbnum      = ls_likp-vbeln.
        <lfs_new>-vbeln      = ls_likp-vbeln.
        <lfs_new>-vstel      = ls_likp-vstel.
        <lfs_new>-vkorg      = ls_likp-vkorg.
        <lfs_new>-kunnr      = ls_likp-kunnr.
        <lfs_new>-kunag      = ls_likp-kunag.
        <lfs_new>-btgew      = ls_likp-btgew.
        <lfs_new>-gewei_likp = ls_likp-gewei.
        <lfs_new>-volum_likp = ls_likp-volum.
        <lfs_new>-voleh_likp = ls_likp-voleh.
        "lips
        <lfs_new>-posnr = ls_lips-posnr.
        <lfs_new>-pstyv = ls_lips-pstyv.
        <lfs_new>-ernam = ls_lips-ernam.
        <lfs_new>-erzet = ls_lips-erzet.
        <lfs_new>-erdat = ls_lips-erdat.
        <lfs_new>-matnr = ls_lips-matnr.
        <lfs_new>-werks = ls_lips-werks.
        <lfs_new>-lgort = ls_lips-lgort.
        <lfs_new>-charg = ls_lips-charg.
*        <lfs_new>-lfimg = ls_lips-lfimg.
        <lfs_new>-lfimg = ls_lips-lfimg
                        + ls_lips-kcmeng.
        <lfs_new>-meins = ls_lips-meins.
        <lfs_new>-vrkme = ls_lips-vrkme.
        <lfs_new>-ntgew = ls_lips-ntgew.
        <lfs_new>-brgew = ls_lips-brgew.
        <lfs_new>-gewei = ls_lips-gewei.
        <lfs_new>-volum = ls_lips-volum.
        <lfs_new>-voleh = ls_lips-voleh.
        <lfs_new>-vgbel = ls_lips-vgbel.
        <lfs_new>-vgpos = ls_lips-vgpos.
        "kna1
        <lfs_new>-land1 = ls_kna1-land1.
        <lfs_new>-name1 = ls_kna1-name1.
        <lfs_new>-name2 = ls_kna1-name2.
        "vttp vttk
        <lfs_new>-tknum = ls_vttp-tknum.
        <lfs_new>-tpnum = ls_vttp-tpnum.
        <lfs_new>-vbtyp = ls_vttk-vbtyp.
        <lfs_new>-shtyp = ls_vttk-shtyp.
        <lfs_new>-tplst = ls_vttk-tplst.
        <lfs_new>-ernam_vttk = ls_vttk-ernam.
        <lfs_new>-erdat_vttk = ls_vttk-erdat.
        <lfs_new>-erzet_vttk = ls_vttk-erzet.
        <lfs_new>-signi = ls_vttk-signi.
        <lfs_new>-exti1 = ls_vttk-exti1.
        <lfs_new>-exti2 = ls_vttk-exti2.
        <lfs_new>-dtdis = ls_vttk-dtdis.
        <lfs_new>-tdlnr = ls_vttk-tdlnr.
        <lfs_new>-sdabw = ls_vttk-sdabw.
        <lfs_new>-add01 = ls_vttk-add01.
        <lfs_new>-add02 = ls_vttk-add02.
        <lfs_new>-add03 = ls_vttk-add03.
        <lfs_new>-add04 = ls_vttk-add04.
        <lfs_new>-dalbg = ls_vttk-dalbg.
        <lfs_new>-tndrrc = ls_vttk-tndrrc.
        <lfs_new>-tndr_actp = ls_vttk-tndr_actp.
        "oth
        <lfs_new>-edatu = ls_vbep-edatu.

*        IF <lfs_new>-ebeln IS NOT INITIAL.
*          <lfs_new>-lfimg = <lfs_new>-lfimg + <lfs_new>-menge.
*        ENDIF.

**********************************************************************

        SELECT SINGLE
          dd07t~ddtext AS tndrrc_t
          FROM dd07t
          INTO <lfs_new>-tndrrc_t
            WHERE  domname    EQ 'TNDRRC'
               AND ddlanguage EQ 'P'
               AND as4local   EQ 'A'
               AND domvalue_l EQ ls_vttk-tndrrc
          .

        SELECT SINGLE
                tvsakt~bezei AS tp_carro
          FROM tvsakt
          INTO @<lfs_new>-tp_carro
                        WHERE tvsakt~spras EQ @sy-langu
                          AND tvsakt~sdabw EQ @ls_vttk-sdabw.
        SELECT SINGLE
          lfa1~name1 AS name1_tdlnr
          FROM lfa1
          INTO @<lfs_new>-name1_tdlnr
                WHERE lfa1~lifnr EQ @ls_vttk-tdlnr.

        SELECT SINGLE
          makt~maktx
          FROM makt
          INTO @<lfs_new>-maktx
        WHERE makt~matnr EQ @ls_lips-matnr
          AND makt~spras EQ @sy-langu.

        SELECT SINGLE
          tvkot~vtext
          FROM tvkot
        INTO @<lfs_new>-vtext
            WHERE tvkot~spras EQ @sy-langu
              AND tvkot~vkorg EQ @ls_likp-vkorg.

        SELECT SINGLE
          vbuk~kostk,
          vbuk~wbstk
                FROM vbuk
                INTO (@<lfs_new>-kostk, @<lfs_new>-wbstk)
         WHERE vbuk~vbeln EQ @ls_likp-vbeln.

        SELECT SINGLE
         zwm028~zlock
          FROM zwm028
          INTO @<lfs_new>-zlock
              WHERE zwm028~lgnum   EQ @ls_t311-lgnum
                AND zwm028~refnr   EQ @ls_t311-refnr
                AND zwm028~remessa EQ @ls_likp-vbeln. "ls_t311a-vbeln.

**********************************************************************
        READ TABLE gt_ztransp_h INTO DATA(ls_ztransp)
          WITH KEY tknum = <lfs_new>-tknum
                   BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_ztransp.
        ELSE.
          <lfs_new>-netwr   = ls_ztransp-netwr.
          IF <lfs_new>-tndrrc_t IS INITIAL.
            <lfs_new>-tndrrc_t = ls_ztransp-ddtext.
            <lfs_new>-tndrst   = ls_ztransp-tndrst.
          ENDIF.
          <lfs_new>-desbloquear   = ls_ztransp-desbloquear.
          <lfs_new>-zonadist   = ls_ztransp-zonadist.
        ENDIF.

        IF ls_ztransp-desbloquear EQ space.
          <lfs_new>-icon_desb = icon_red_light.
        ELSEIF ls_ztransp-desbloquear EQ abap_true.
          <lfs_new>-icon_desb = icon_green_light.
        ENDIF.

**********************************************************************

        SELECT SINGLE
               vbelv,
               posnv,
               SUM( vbfa~rfmng ) AS rfmng_sum,
               meins
          INTO @DATA(ls_vbfa)
          FROM vbfa
                  WHERE vbelv   EQ @ls_lips-vgbel "iv_doc
                    AND posnv   EQ @ls_lips-vgpos "iv_itm
                    AND vbtyp_n EQ 'J'
          GROUP BY vbelv, posnv, meins.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_vbfa.
        ENDIF.

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
                  WHERE ekbe~ebeln EQ @ls_lips-vgbel "iv_doc
                    AND ekbe~ebelp EQ @ls_lips-vgpos "iv_itm
                    AND ekbe~vgabe EQ '8'
          GROUP BY ekbe~ebeln, ekbe~ebelp, ekpo~meins.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_ekbe.
        ENDIF.

        <lfs_new>-kwmeng = ls_vbap-kwmeng + ls_ekpo-menge.

        <lfs_new>-qtd_forn_vbfa = ls_vbfa-rfmng_sum.
        <lfs_new>-qtd_forn_ekbe = ls_ekbe-menge_sum.
        <lfs_new>-qtd_fornecida = ls_vbfa-rfmng_sum
                                + ls_ekbe-menge_sum.

        <lfs_new>-qtd_dif       = ls_vbap-kwmeng
                                  - <lfs_new>-qtd_fornecida.

**********************************************************************

        IF <lfs_new>-vbeln IS NOT INITIAL.
          IF <lfs_new>-abgru IN gr_abgru.
            <lfs_new>-item_fechado = abap_true.
          ENDIF.
        ELSEIF <lfs_new>-ebeln IS NOT INITIAL.
          IF <lfs_new>-loekz IS NOT INITIAL OR
             <lfs_new>-elikz IS NOT INITIAL.
            <lfs_new>-item_fechado = abap_true.
          ENDIF.
        ENDIF.

        IF <lfs_new>-qtd_fornecida EQ 0.
          <lfs_new>-falta_comp = abap_true.
          "@0W\QAAA 645@
          <lfs_new>-status_ic  = gs_icon-falta_comp. "'@3C\Q@'. "icon_pattern_exclude_red.
        ELSEIF <lfs_new>-qtd_fornecida GE <lfs_new>-kwmeng. "iv_qtd.
          <lfs_new>-finalizada = abap_true.
          <lfs_new>-status_ic  = gs_icon-finalizada. "'@3C\Q@'. "icon_led_green.
        ELSE.
          <lfs_new>-falta_parc = abap_true.
          <lfs_new>-status_ic  = gs_icon-falta_parc. "'@3C\Q@'. "icon_led_yellow.
        ENDIF.

        <lfs_new>-seq          = ls_zroutyn_t01_i-seq.

        <lfs_new>-hora_reg   = ls_zwm006_aux-hora_reg  .
        <lfs_new>-data_reg   = ls_zwm006_aux-data_reg  .
        <lfs_new>-matricula  = ls_zwm006_aux-matricula .
        <lfs_new>-hora_saida = ls_zwm006_aux-hora_saida.
        <lfs_new>-data_saida = ls_zwm006_aux-data_saida.
        IF ls_zwm006_aux-data_reg IS NOT INITIAL.
          IF <lfs_new>-data_saida IS INITIAL.
            DATA(lv_data_saida) = sy-datum.
            DATA(lv_hora_saida) = sy-uzeit.
          ELSE.
            lv_data_saida   = <lfs_new>-data_saida.
            lv_hora_saida   = <lfs_new>-hora_saida.
          ENDIF.
          CLEAR lv_hours.

          DATA lv_duration TYPE  sytabix.
          CALL FUNCTION 'SWI_DURATION_DETERMINE'
            EXPORTING
              start_date = ls_zwm006_aux-data_reg
              end_date   = lv_data_saida
              start_time = <lfs_new>-hora_reg
              end_time   = lv_hora_saida
            IMPORTING
              duration   = lv_duration.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ELSE.
            DATA lv_round TYPE f.
            lv_round = lv_duration / 3600.
            <lfs_new>-time_wait = round( val = lv_round dec = 0 mode = cl_abap_math=>round_down ).
          ENDIF.

        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT gt_faltas INTO DATA(ls_faltas_tot) WHERE lfimg GT 0.
*      READ TABLE gt_dados_ant INTO DATA(ls_ref)
      READ TABLE gt_dados INTO DATA(ls_ref)
        WITH KEY vgbel = ls_faltas_tot-ordem.
      IF sy-subrc IS INITIAL.
        INSERT ls_ref INTO gt_dados INDEX ( sy-tabix )
          ASSIGNING <lfs_new>.
        CLEAR <lfs_new>-lfimg.
        CLEAR <lfs_new>-falta_comp.
        CLEAR <lfs_new>-finalizada.
        CLEAR <lfs_new>-falta_parc.
        CLEAR <lfs_new>-status_ic.

        READ TABLE gt_vbak INTO ls_vbak
              WITH KEY vbeln = ls_faltas_tot-ordem
              BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE gt_vbap INTO ls_vbap
            WITH KEY vbeln = ls_faltas_tot-ordem
                     posnr = ls_faltas_tot-item
                     BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            CLEAR ls_vbap.
          ENDIF.

          MOVE-CORRESPONDING ls_vbak TO <lfs_new>.
          MOVE-CORRESPONDING ls_vbap TO <lfs_new>.
          <lfs_new>-kwmeng = ls_vbap-kwmeng.
        ELSE.
          CLEAR ls_vbak.
        ENDIF.

        <lfs_new>-vkorg = ls_ref-vkorg.

        READ TABLE gt_ekko INTO ls_ekko
              WITH KEY ebeln = ls_faltas_tot-ordem
              BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_ekko.
        ELSE.
          READ TABLE gt_ekpo INTO ls_ekpo
                WITH KEY ebeln = ls_faltas_tot-ordem
                         ebelp = ls_faltas_tot-item
                         BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE-CORRESPONDING ls_ekko TO <lfs_new>.
            MOVE-CORRESPONDING ls_ekpo TO <lfs_new>.
            <lfs_new>-kwmeng = ls_ekpo-menge.
          ENDIF.
        ENDIF.

        SELECT SINGLE
          makt~maktx
          FROM makt
          INTO @<lfs_new>-maktx
        WHERE makt~matnr EQ @<lfs_new>-matnr
          AND makt~spras EQ @sy-langu.

**********************************************************************
        SELECT SINGLE
               vbelv,
               posnv,
               SUM( vbfa~rfmng ) AS rfmng_sum,
               meins
          INTO @ls_vbfa
          FROM vbfa
                  WHERE vbelv   EQ @ls_faltas_tot-ordem
                    AND posnv   EQ @ls_faltas_tot-item
                    AND vbtyp_n EQ 'J'
          GROUP BY vbelv, posnv, meins.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_vbfa.
        ENDIF.

        SELECT SINGLE
               ekbe~ebeln,
               ekbe~ebelp,
               SUM( ekbe~menge ) AS menge_sum,
               ekpo~meins
          INTO @ls_ekbe
          FROM ekbe
           INNER JOIN ekpo
           ON  ekpo~ebeln EQ ekbe~ebeln
           AND ekpo~ebelp EQ ekbe~ebelp
                  WHERE ekbe~ebeln EQ @ls_faltas_tot-ordem
                    AND ekbe~ebelp EQ @ls_faltas_tot-item
                    AND ekbe~vgabe EQ '8'
          GROUP BY ekbe~ebeln, ekbe~ebelp, ekpo~meins.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_ekbe.
        ENDIF.
**********************************************************************

        <lfs_new>-kwmeng = ls_vbap-kwmeng + ls_ekpo-menge.

        <lfs_new>-qtd_forn_vbfa = ls_vbfa-rfmng_sum.
        <lfs_new>-qtd_forn_ekbe = ls_ekbe-menge_sum.
        <lfs_new>-qtd_fornecida = ls_vbfa-rfmng_sum
                                  + ls_ekbe-menge_sum.

        <lfs_new>-qtd_dif       = ls_vbap-kwmeng
                                  - <lfs_new>-qtd_fornecida.

        IF <lfs_new>-qtd_fornecida EQ 0.
          <lfs_new>-falta_comp = abap_true.
          "@0W\QAAA 645@
          <lfs_new>-status_ic  = gs_icon-falta_comp. "'@3C\Q@'. "icon_pattern_exclude_red.
        ELSEIF <lfs_new>-qtd_fornecida GE <lfs_new>-kwmeng. "iv_qtd.
          <lfs_new>-finalizada = abap_true.
          <lfs_new>-status_ic  = gs_icon-finalizada. "'@3C\Q@'. "icon_led_green.
        ELSE.
          <lfs_new>-falta_parc = abap_true.
          <lfs_new>-status_ic  = gs_icon-falta_parc. "'@3C\Q@'. "icon_led_yellow.
        ENDIF.

*        <lfs_new>-qtd_forn_vbfa = 0.
*        <lfs_new>-qtd_forn_ekbe = 0.
*        <lfs_new>-qtd_fornecida = 0.
*        <lfs_new>-qtd_dif       = <lfs_new>-kwmeng.

*        <lfs_new>-falta_comp = abap_true.
*        "@0W\QAAA 645@
*        <lfs_new>-status_ic  = gs_icon-falta_comp. "'@3C\Q@'. "icon_pattern_exclude_red.
**********************************************************************

      ENDIF.
    ENDLOOP.

**********************************************************************
**********************************************************************
**********************************************************************
*s_dalbg
*s_dplbg
*s_matnr
*s_trfzn
*s_wbstk
*
*s_etdat
*s_dtext
    IF s_dtext[] IS NOT INITIAL.
      DELETE gt_dados WHERE tndrrc_t NOT IN s_dtext.
    ENDIF.

    IF s_etdat[] IS NOT INITIAL.
      DELETE gt_dados WHERE edatu NOT IN s_etdat.
    ENDIF.

*    IF s_tknum[] IS NOT INITIAL.
*      DELETE gt_dados WHERE tknum NOT IN s_tknum.
*    ENDIF.

    IF s_matnr[] IS NOT INITIAL.
      DELETE gt_dados WHERE matnr NOT IN s_matnr.
    ENDIF.

    IF s_trfzn[] IS NOT INITIAL.
      DELETE gt_dados WHERE trfzn NOT IN s_trfzn.
    ENDIF.

    IF p_falta EQ abap_true.
      DELETE gt_dados WHERE qtd_dif EQ 0.
    ENDIF.

**********************************************************************
    lr_tknum = VALUE #( FOR ls_doc2 IN gt_dados WHERE ( tknum IS NOT INITIAL )
                        sign = 'I' option = 'EQ' ( low = ls_doc2-tknum ) ).
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
    "ver ZWMREP0090_CLS_IMP

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

*    SORT gt_dados BY vkorg tknum refnr seq rbnum matnr.
    SORT gt_dados BY vkorg dtdis tknum refnr seq rbnum matnr.
    SORT gs_pal_sum-vkorg BY vkorg.
    SORT gs_pal_sum-tknum BY tknum.
    SORT gs_pal_sum-refnr BY tknum refnr.

    ls_head_line = CORRESPONDING #( gs_pal_sum-head ).

    lv_head_key = add_first_line( is_data = CORRESPONDING #( ls_head_line ) ).
*    DATA lv_icon_rbnum TYPE icon-id.
    LOOP AT gt_dados INTO DATA(ls_dados).
      gs_dados_aux = ls_dados.
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
      WITH KEY vkorg = ls_data-vkorg.
*      BINARY SEARCH.
    IF sy-subrc IS INITIAL AND
       <lfs_pal_tot_vkorg> IS ASSIGNED.
*      ls_data = CORRESPONDING #( BASE ( ls_data ) <lfs_pal_tot_vkorg>-totais ).
      ls_data-palcp   = <lfs_pal_tot_vkorg>-totais-palcp.
      ls_data-palpk   = <lfs_pal_tot_vkorg>-totais-palpk.
      ls_data-palto   = <lfs_pal_tot_vkorg>-totais-palto.
      ls_data-palpk_s = <lfs_pal_tot_vkorg>-totais-palpk_s.
      ls_data-palto_s = <lfs_pal_tot_vkorg>-totais-palto_s.
    ENDIF.


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
    DATA ls_totais TYPE y_pal_totais.

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

    CLEAR ls_totais.
    CASE iv_text_field.
      WHEN 'TKNUM'.
        ls_data-numero = 1.
        READ TABLE gs_pal_sum-tknum ASSIGNING FIELD-SYMBOL(<lfs_pal_tot_tknum>)
          WITH KEY tknum = ls_data-tknum
          BINARY SEARCH.
        IF sy-subrc IS INITIAL AND
           <lfs_pal_tot_tknum> IS ASSIGNED.
*          DATA(ls_totais) = <lfs_pal_tot_tknum>-totais.
          ls_totais = <lfs_pal_tot_tknum>-totais.
        ENDIF.
      WHEN 'REFNR'.
        READ TABLE gs_pal_sum-refnr ASSIGNING FIELD-SYMBOL(<lfs_pal_tot_refnr>)
          WITH KEY tknum = gs_dados_aux-tknum
                   refnr = ls_data-refnr
          BINARY SEARCH.
        IF sy-subrc IS INITIAL AND
           <lfs_pal_tot_refnr> IS ASSIGNED.
          ls_totais = <lfs_pal_tot_refnr>-totais.
        ENDIF.
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
