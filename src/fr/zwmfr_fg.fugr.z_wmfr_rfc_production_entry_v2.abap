FUNCTION z_wmfr_rfc_production_entry_v2.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_EAN11) TYPE  EAN11
*"     VALUE(I_LINE) TYPE  FEVOR
*"     VALUE(I_VHILM) TYPE  VHILM
*"     VALUE(I_AUFNR) TYPE  AUFNR
*"     VALUE(I_QUANTITY) TYPE  MENGE_D
*"     VALUE(I_SIMUL) TYPE  ZWM_AUX-SIMULA
*"     VALUE(I_NOPRINT) TYPE  FLAG
*"     VALUE(I_GETPDF) TYPE  FLAG DEFAULT ABAP_FALSE
*"     VALUE(I_GETZPL) TYPE  FLAG DEFAULT ABAP_FALSE
*"  EXPORTING
*"     VALUE(E_OUTSTRING) TYPE  ZWM_AUX-OUT_STRING
*"     VALUE(E_OUTPROD) TYPE  ZWM_AUX-OUT_PRODUCAO
*"     VALUE(E_HOEHE) TYPE  HOEHE
*"     VALUE(E_MATNR) TYPE  MATNR
*"     VALUE(E_EAN11) TYPE  MEAN-EAN11
*"     VALUE(E_EAN128_BC1) TYPE  ZWM_AUX-OUT_CB_TX
*"     VALUE(E_EAN128_BC2) TYPE  ZWM_AUX-OUT_CB_TX
*"     VALUE(E_EAN128_BC3) TYPE  ZWM_AUX-OUT_CB_TX
*"     VALUE(E_EAN128_BC4) TYPE  ZWM_AUX-OUT_CB_TX
*"     VALUE(E_EAN128_TX1) TYPE  ZWM_AUX-OUT_CB_TX
*"     VALUE(E_EAN128_TX2) TYPE  ZWM_AUX-OUT_CB_TX
*"     VALUE(E_EAN128_TX3) TYPE  ZWM_AUX-OUT_CB_TX
*"     VALUE(E_EAN128_TX4) TYPE  ZWM_AUX-OUT_CB_TX
*"     VALUE(E_LABEL) TYPE  ZWM_AUX-OUT_CB_TX
*"     VALUE(E_TANUM) TYPE  TANUM
*"     VALUE(E_PDF_1) TYPE  XSTRING
*"     VALUE(E_PDF_2) TYPE  XSTRING
*"     VALUE(E_ZPL_1) TYPE  ZTLINE_T
*"     VALUE(E_ZPL_2) TYPE  ZTLINE_T
*"     VALUE(E_RETURN) TYPE  ZWM_AUX-RETORNO
*"     VALUE(R_RETURN_MSG) TYPE  BAPI_MSG
*"----------------------------------------------------------------------
  DATA lv_select      TYPE i.
  DATA lv_subrc       TYPE sysubrc.
  DATA lv_lay_label   TYPE zwm043-lay_etiqueta.
  DATA lv_lety1       TYPE mlgn-lety1.
  DATA lv_matnr       TYPE mara-matnr.
  DATA lv_menge       TYPE char4.
  DATA lv_meins_p     TYPE meins.
  DATA lv_ean128_cba  TYPE zwm_aux-out_cb_tx.
  DATA lv_ean128_cbb  TYPE zwm_aux-out_cb_tx.
  DATA lv_ean128_cbc  TYPE zwm_aux-out_cb_tx.
  DATA lv_ean128_txa  TYPE zwm_aux-out_cb_tx.
  DATA lv_ean128_txb  TYPE zwm_aux-out_cb_tx.
  DATA lv_ean128_txc  TYPE zwm_aux-out_cb_tx.
  DATA lv_table       TYPE zwm_aux-mesa.
  DATA lv_sscc        TYPE zwm_aux-sscc.
  DATA lv_lenum       TYPE lenum.
  DATA lv_lenum_zpl   TYPE string.
  DATA lv_lgnum       TYPE lgnum.
  DATA lv_werks       TYPE werks_d.
  DATA lv_lgort       TYPE lgort_d.
  DATA lt_items       TYPE TABLE OF zwm018.
  DATA ls_item        TYPE zwm018.
  DATA ls_mlgn        TYPE mlgn.
  DATA ls_zwm001      TYPE zwm001.
  DATA lv_arrum_aut   TYPE flag.
  DATA lt_ltap        TYPE TABLE OF ltap.
  DATA ls_ltap        TYPE ltap.
  DATA lv_bwlvs_aut   TYPE bwlvs.
  DATA lv_valor       TYPE zwm_valor.

  DATA ls_zwm030      TYPE zwm030.
  DATA: ls_zwm030_aux TYPE zwm030. " RENSAF00039 - JCO@ROFFSDF
  DATA ls_z02rpsessao TYPE z02rpsessao.
  DATA ls_t024f       TYPE t024f.
  DATA ls_caufv       TYPE caufv.

  DATA lt_aufk        TYPE ty_ht_aufk.
  DATA lt_istat       TYPE STANDARD TABLE OF tj02t-istat.
  DATA lt_jest        TYPE ty_st_jest.
  DATA lt_mean        TYPE STANDARD TABLE OF ty_ean.
  DATA lt_zwm001      TYPE STANDARD TABLE OF zwm001.
  DATA lt_mseg        TYPE STANDARD TABLE OF ty_mseg.
  DATA lt_z02rpsessao TYPE STANDARD TABLE OF z02rpsessao.

  DATA lr_txt04 TYPE RANGE OF tj02t-txt04.
  DATA lr_istat TYPE RANGE OF tj02t-istat.

  FIELD-SYMBOLS <fs_zwm030>   LIKE LINE OF gt_zwm030[].
  FIELD-SYMBOLS <fs_aufk>     LIKE LINE OF lt_aufk[].
  FIELD-SYMBOLS <fs_txt04>    LIKE LINE OF lr_txt04[].
  FIELD-SYMBOLS <fs_istat>    LIKE LINE OF lt_istat[].
  FIELD-SYMBOLS <fs_ristat>   LIKE LINE OF lr_istat[].
  FIELD-SYMBOLS <fs_mean>     LIKE LINE OF lt_mean[].
  FIELD-SYMBOLS <fs_zwm001>   LIKE LINE OF gt_zwm001[].
  FIELD-SYMBOLS <fs_mseg>     LIKE LINE OF lt_mseg[].
*"----------------------------------------------------------------------
  CLEAR gs_ifprod.
  CLEAR gs_reg.
  CLEAR: gs_log.

  FREE gt_zwm030[].
  FREE gt_zwm001[].

  GET TIME.
  gs_log-data = sy-datum.
  gs_log-hora = sy-uzeit.
  gs_log-simulacao = i_simul.
  gs_log-codigo = i_ean11.
  gs_log-utilizador = sy-uname.

  IF i_simul EQ abap_true.
    gs_log-processo = 'SIMULACAO'.
  ELSE.
    gs_log-processo = 'ENTRADA_PRODUCAO'.
  ENDIF.

  IF i_ean11 IS INITIAL OR i_line IS INITIAL OR i_vhilm IS INITIAL OR i_aufnr IS INITIAL.
    e_return = 70. " Erro na transferência (tabela vazia)
    gs_log-retorno = e_return.
    MODIFY zwm_log_efacec FROM gs_log.
    RETURN.
  ENDIF.

  gs_ifprod-codigo        = i_ean11.
  gs_ifprod-linha         = i_line.
  gs_ifprod-cor           = i_vhilm.
  gs_ifprod-aufnr         = i_aufnr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input  = gs_ifprod-cor
    IMPORTING
      output = gs_ifprod-cor.

  CALL FUNCTION 'CONVERSION_EXIT_EAN11_INPUT'
    EXPORTING
      input  = gs_ifprod-codigo
    IMPORTING
      output = gs_ifprod-codigo.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_ifprod-aufnr
    IMPORTING
      output = gs_ifprod-aufnr.

  e_label = c_label_default.
  e_matnr = gs_ifprod-cor.

* Verifica se pelo menos a Linha ou o Código estão preenchidos
  IF gs_ifprod-linha IS INITIAL AND gs_ifprod-codigo IS INITIAL.
    e_return      = 71. " Sem Linha/Código preenchido
    r_return_msg  = 'Sem Linha/Código preenchidos'(018).
    gs_log-retorno = e_return.
    gs_log-msg = r_return_msg.
    MODIFY zwm_log_efacec FROM gs_log.
    RETURN.
  ENDIF.

* Linha é vazia, verifica se a ordem está preenchida
* Linha e Ordem Vazia obter tudo
  IF gs_ifprod-linha IS INITIAL. "Linha Vazia
    IF gs_ifprod-aufnr IS INITIAL. "Ordem Vazia
      lv_select = 1. "Ler tudo
    ELSE.
      lv_select = 2. "Ler só com a Ordem
    ENDIF.
  ELSE.
* Linha não é vazia, verifica se a ordem está preenchida
* Linha Preenchida e Ordem Vazia
    IF gs_ifprod-aufnr IS INITIAL.
      lv_select = 3. "Ler só com a linha
    ELSE.
      lv_select = 4. "Ler com a linha e ordem
    ENDIF.
  ENDIF.

** Valida Ordem
**********************************************************************
  SELECT SINGLE *
           FROM caufv
           INTO ls_caufv
           WHERE aufnr EQ i_aufnr.

  IF sy-subrc <> 0.
    r_return_msg  = 'Ordem de Produção Inválida'(035).
    e_return      = 10.
    gs_log-retorno = e_return.
    gs_log-msg = r_return_msg.
    MODIFY zwm_log_efacec FROM gs_log.
    RETURN.
  ENDIF.

  IF ls_caufv-fevor <> i_line.
    r_return_msg  = 'Linha não existe ou é incorrecta'(020).
    e_return      = 10.
    gs_log-retorno = e_return.
    gs_log-msg = r_return_msg.
    MODIFY zwm_log_efacec FROM gs_log.
    RETURN.
  ENDIF.


* 1 = Ler tudo
* 2 = Ler só com a Ordem de Produção
* 3 = Ler só com a Linha
* 4 = Ler com Linha e Ordem
  CASE lv_select.
    WHEN 1.
      SELECT *
        FROM zwm030 INTO TABLE gt_zwm030[]
        WHERE bloqueio EQ abap_false. "#EC CI_NOFIELD " um indice deveria ser criado
    WHEN 2.
      SELECT *
        FROM zwm030 INTO TABLE gt_zwm030[]
        WHERE aufnr EQ gs_ifprod-aufnr
          AND bloqueio EQ abap_false.
    WHEN 3.
      SELECT *
        FROM zwm030 INTO TABLE gt_zwm030[]
        WHERE linha EQ gs_ifprod-linha
          AND bloqueio EQ abap_false. "#EC CI_NOFIELD " um indice deveria ser criado
    WHEN 4.
      SELECT *
        FROM zwm030 INTO TABLE gt_zwm030[]
        WHERE aufnr EQ gs_ifprod-aufnr
          AND linha EQ gs_ifprod-linha
          AND bloqueio EQ abap_false.
  ENDCASE.
  IF gt_zwm030[] IS INITIAL.
    r_return_msg  = 'Tabela sem registos'(019).
    e_return      = 10.
    gs_log-retorno = e_return.
    gs_log-msg = r_return_msg.
    MODIFY zwm_log_efacec FROM gs_log.
    RETURN.
  ENDIF.

  APPEND INITIAL LINE TO lr_istat[] ASSIGNING <fs_ristat>.
  <fs_ristat>-sign   = c_rsig_i.
  <fs_ristat>-option = c_ropt_eq.
  <fs_ristat>-low    = 'I0045'.

  APPEND INITIAL LINE TO lr_istat[] ASSIGNING <fs_ristat>.
  <fs_ristat>-sign   = c_rsig_i.
  <fs_ristat>-option = c_ropt_eq.
  <fs_ristat>-low    = 'I0046'.

  SELECT aufnr werks objnr
    FROM aufk INTO TABLE lt_aufk[]
    FOR ALL ENTRIES IN gt_zwm030[]
    WHERE aufnr EQ gt_zwm030-aufnr.
  IF sy-subrc EQ 0.
    SELECT objnr stat inact
      FROM jest INTO TABLE lt_jest[]
      FOR ALL ENTRIES IN lt_aufk[]
      WHERE objnr EQ lt_aufk-objnr
        AND stat IN lr_istat[]
        AND inact EQ abap_false.
  ENDIF.

  LOOP AT gt_zwm030[] ASSIGNING <fs_zwm030>.
    IF lv_lgnum IS INITIAL.
      lv_lgnum = <fs_zwm030>-lgnum.
      lv_werks = <fs_zwm030>-werks.
    ENDIF.

    READ TABLE lt_aufk[] ASSIGNING <fs_aufk>
      WITH TABLE KEY aufnr = <fs_zwm030>-aufnr.
    IF sy-subrc NE 0.
      DELETE TABLE gt_zwm030[] FROM <fs_zwm030>.
      CONTINUE.
    ENDIF.

    READ TABLE lt_jest[] TRANSPORTING NO FIELDS
      WITH TABLE KEY objnr = <fs_aufk>-objnr.
    CHECK sy-subrc EQ 0.
    DELETE TABLE gt_zwm030[] FROM <fs_zwm030>.
  ENDLOOP.

  IF gt_zwm030[] IS INITIAL.
    r_return_msg  = 'A Ordem não tem o status correcto!'(036).
    e_return      = 90.
    gs_log-retorno = e_return.
    gs_log-msg = r_return_msg.
    MODIFY zwm_log_efacec FROM gs_log.
    RETURN.
  ENDIF.

  IF gs_ifprod-linha IS NOT INITIAL.
    PERFORM f_check_prodline  CHANGING lv_subrc e_matnr.
    IF lv_subrc NE 0.
      r_return_msg  = 'Linha não existe ou é incorrecta'(020).
      e_return      = lv_subrc.
      gs_log-retorno = e_return.
      gs_log-msg = r_return_msg.
      MODIFY zwm_log_efacec FROM gs_log.
      RETURN.
    ENDIF.

    IF gs_ifprod-codigo IS NOT INITIAL.
      PERFORM f_check_code CHANGING lv_subrc e_outprod.
      IF lv_subrc NE 0.
        CASE lv_subrc.
          WHEN 20.
            r_return_msg = 'EAN Não existe em SAP'(021).
          WHEN 21.
            r_return_msg = 'Material não existe'(022).
          WHEN 22.
            r_return_msg = 'Material em mais do que uma ordem produção'(023).
        ENDCASE.
        e_return = lv_subrc.
        gs_log-retorno = e_return.
        gs_log-msg = r_return_msg.
        MODIFY zwm_log_efacec FROM gs_log.
        RETURN.
      ENDIF.
    ENDIF.
  ELSE.
    IF gs_ifprod-codigo IS NOT INITIAL.
      PERFORM f_check_code CHANGING lv_subrc e_outprod.
    ENDIF.
    IF lv_subrc NE 0.
      CASE lv_subrc.
        WHEN 20.
          r_return_msg = 'EAN Não existe em SAP'(021).
        WHEN 21.
          r_return_msg = 'Material não existe'(022).
        WHEN 22.
          r_return_msg = 'Material em mais do que uma ordem produção'(023).
      ENDCASE.

      e_return = lv_subrc.
      gs_log-retorno = e_return.
      gs_log-msg = r_return_msg.
      MODIFY zwm_log_efacec FROM gs_log.
      RETURN.
    ENDIF.

** Caso a linha seja vazia e só exista 1 Ordem de produção
** Então devemos ir obter a linha para verificar a Côr da palete
    SELECT SINGLE fevor
      FROM afko INTO gs_ifprod-linha
      WHERE aufnr EQ gs_reg-aufnr.
    IF sy-subrc EQ 0.
* Substitui tipo de palete se existir na tabela ZTIPOPALETE
      PERFORM f_check_paltype USING gs_reg-matnr gs_ifprod-linha CHANGING e_matnr gs_ifprod-cor.
    ENDIF.
  ENDIF.

  CASE lv_subrc.
    WHEN 0.
      "OK
    WHEN 21.
      r_return_msg = 'Não Existe Entrada para a Ordem na ZWM030 para o Material Indicado'(020).
      e_return     = lv_subrc.
      gs_log-msg = r_return_msg.
      gs_log-retorno = e_return.
      MODIFY zwm_log_efacec FROM gs_log.
      RETURN.
    WHEN 22.
      r_return_msg = 'Multiplas Entradas para Ordens na ZWM030 para o Material Indicado'(020).
      e_return     = lv_subrc.
      gs_log-retorno = e_return.
      gs_log-msg = r_return_msg.
      MODIFY zwm_log_efacec FROM gs_log.
      RETURN.
    WHEN OTHERS.
      r_return_msg = 'Linha não existe ou é incorrecta'(020).
      e_return     = lv_subrc.
      gs_log-retorno = e_return.
      gs_log-msg = r_return_msg.
      MODIFY zwm_log_efacec FROM gs_log.
      RETURN.
  ENDCASE.


  gs_log-matnr = gs_reg-matnr.
  gs_log-aufnr = gs_reg-aufnr.
  gs_log-linha = gs_ifprod-linha.
  gs_log-cor   = gs_ifprod-cor.

  " Paulo Sousa em 2017.07.12
  " determinar automaticamente layout para materiais MDD
  DATA: zgrp1 LIKE mvke-mvgr1, zlay LIKE zwm043-lay_etiqueta.
  CLEAR: zgrp1, zlay.
  SELECT SINGLE mvgr1 INTO zgrp1 FROM mvke WHERE matnr = gs_reg-matnr.
  IF zgrp1 = 'MDD'.  " marcas da distribuicao
    zlay = 'SEMLOGO'.
  ELSE.
    SELECT SINGLE lay_etiqueta INTO zlay FROM zwm043 WHERE matnr = gs_reg-matnr.
  ENDIF.
  IF zlay IS NOT INITIAL.
    e_label = zlay.
    CONDENSE e_label.
  ENDIF.

  SELECT matnr meinh ean11 eantp hpean
    FROM mean INTO TABLE lt_mean[]
    WHERE matnr EQ gs_reg-matnr.

  SORT lt_mean[] BY hpean.
  DELETE lt_mean[] WHERE hpean NE abap_true.
  SORT lt_mean[] BY matnr eantp meinh.

  READ TABLE lt_mean[] ASSIGNING <fs_mean>
    WITH KEY matnr = gs_reg-matnr
             eantp = c_eantp_ic
             meinh = c_meinh_pal
             BINARY SEARCH.
  IF sy-subrc NE 0.
    READ TABLE lt_mean[] ASSIGNING <fs_mean>
      WITH KEY matnr = gs_reg-matnr
               eantp = c_eantp_ic
               meinh = c_meinh_pal_alt
               BINARY SEARCH.
  ENDIF.
  IF sy-subrc EQ 0.
    e_ean11 = <fs_mean>-ean11.
  ELSE.
    SORT lt_mean[] BY matnr eantp ean11.

    READ TABLE lt_mean[] ASSIGNING <fs_mean>
      WITH KEY matnr = gs_reg-matnr
               eantp = c_eantp_ic
               ean11 = gs_ifprod-codigo
               BINARY SEARCH.
    IF sy-subrc EQ 0.
      e_ean11 = <fs_mean>-ean11.
    ELSE.
      SORT lt_mean[] BY matnr eantp.

      READ TABLE lt_mean[] ASSIGNING <fs_mean>
        WITH KEY matnr = gs_reg-matnr
                 eantp = c_eantp_ic
                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        e_ean11 = <fs_mean>-ean11.
      ELSE.
        READ TABLE lt_mean[] ASSIGNING <fs_mean>
          WITH KEY matnr = gs_reg-matnr
                   eantp = c_eantp_he
                   BINARY SEARCH.
        IF sy-subrc EQ 0.
          e_ean11 = <fs_mean>-ean11.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT SINGLE matnr lety1
    FROM mlgn INTO (gs_reg-matnr, lv_lety1)
    WHERE matnr EQ gs_reg-matnr
      AND lgnum EQ gs_reg-lgnum.
  IF sy-subrc NE 0.
    r_return_msg = 'Material não está expandido no depósito'(024).
    e_return     = 80.  " Não existe entrada na MLGN
    gs_log-retorno = e_return.
    gs_log-msg = r_return_msg.
    MODIFY zwm_log_efacec FROM gs_log.
    RETURN.
  ENDIF.

  CALL FUNCTION 'ZWM_REFRESH_BUFFER'
    EXPORTING
      whs       = gs_reg-lgnum
    TABLES
      ti_zwm001 = lt_zwm001[].
  LOOP AT lt_zwm001[] ASSIGNING <fs_zwm001>.
    INSERT <fs_zwm001> INTO TABLE gt_zwm001[].
  ENDLOOP.

  gs_reg-menge = i_quantity.
  PERFORM f_check_complete_pal CHANGING lv_subrc.

  IF lv_subrc NE 0.
    CASE lv_subrc.
      WHEN 30.
        r_return_msg = 'Material sem Unid. Medida PAL'(025).
      WHEN 31.
        r_return_msg = 'Altura inválida'(026).
      WHEN 40.
        r_return_msg = 'Quant. incorrecta'(027).
      WHEN 91.
        r_return_msg = 'Paletes remontadas - Somente quantidades completas'(028).
    ENDCASE.

    e_return = lv_subrc.
    gs_log-retorno = e_return.
    gs_log-msg = r_return_msg.
    MODIFY zwm_log_efacec FROM gs_log.
    RETURN.
  ENDIF.

  IF gs_ifprod-cor IS INITIAL.
    r_return_msg = 'Tipo de palete (cor) não especificado'(034).
    e_return = 50. " Cor não especificada
    gs_log-retorno = e_return.
    gs_log-msg = r_return_msg.
    MODIFY zwm_log_efacec FROM gs_log.
    RETURN.
  ENDIF.

  gs_log-quantidade = gs_reg-menge.

  SELECT SINGLE matnr
    FROM mara INTO gs_reg-pack
    WHERE matnr EQ gs_ifprod-cor.
  IF sy-subrc NE 0.
    r_return_msg = 'Côr Palete não é a do Material'(029).
    e_return     = 51. " Cor invalida
    gs_log-retorno = e_return.
    gs_log-msg = r_return_msg.
    MODIFY zwm_log_efacec FROM gs_log.
    RETURN.
  ENDIF.

* Valida se o Lote está aberto no armazém CD
* INETUM - NR - 02.03.2022 - RENPRJ00032 - Inicio
***  PERFORM f_check_batch USING gs_reg-matnr gs_reg-lgnum gs_reg-charg CHANGING lv_subrc.
  PERFORM f_check_batch_v2 USING gs_reg-matnr gs_reg-lgnum gs_reg-charg gs_reg-werks CHANGING lv_subrc.
* INETUM - NR - 02.03.2022 - RENPRJ00032 - Fim
  IF lv_subrc NE 0.
    r_return_msg = 'Lote não está aberto no armazém'(036).
    e_return = lv_subrc.
    gs_log-retorno = e_return.
    gs_log-msg = r_return_msg.
    MODIFY zwm_log_efacec FROM gs_log.
    RETURN.
  ENDIF.

  IF i_noprint NE abap_true OR i_simul NE abap_true.
    PERFORM f_create_sscc CHANGING gs_reg-hukey1 lv_subrc.
    IF lv_subrc NE 0.
      r_return_msg = 'Erro na criação do SSCC'(030).
      e_return     = lv_subrc.
      gs_log-retorno = e_return.
      gs_log-msg = r_return_msg.
      MODIFY zwm_log_efacec FROM gs_log.
      RETURN.
    ENDIF.

    e_return = 0.

    PERFORM f_update_zwm013 USING lv_lety1.

    IF lv_lety1 EQ c_lety1_p2 OR lv_lety1 EQ c_lety1_p5.
      PERFORM f_create_sscc CHANGING gs_reg-hukey2 lv_subrc.
      IF lv_subrc NE 0.
        r_return_msg = 'Erro na criação do SSCC da remontada'(031).
        e_return     = lv_subrc.
        gs_log-msg = r_return_msg.
        MODIFY zwm_log_efacec FROM gs_log.
        RETURN.
      ENDIF.

      PERFORM f_update_zwm020.
    ENDIF.
  ENDIF.

  IF i_simul NE abap_true.
    " Regista entrada da Palete contra a Ordem de Produção em modo de TESTE...
* INETUM - NR - 04.03.2022 - RENPRJ00032 - Inicio
***    PERFORM f_do_goodsmvmnt USING abap_true lv_lety1 CHANGING lv_subrc r_return_msg.
    PERFORM f_do_goodsmvmnt_v2 USING abap_true lv_lety1 CHANGING lv_subrc r_return_msg.
* INETUM - NR - 04.03.2022 - RENPRJ00032 - Fim
    IF lv_subrc NE 0.
      e_return  = 60. " Erro entrada produção
      gs_log-retorno = e_return.
      MODIFY zwm_log_efacec FROM gs_log.
      RETURN.
    ENDIF.
    " Depois Regista entrada da Palete contra a Ordem de Produção em modo NORMAL
* INETUM - NR - 04.03.2022 - RENPRJ00032 - Inicio
***    PERFORM f_do_goodsmvmnt USING abap_false lv_lety1 CHANGING lv_subrc r_return_msg.
    PERFORM f_do_goodsmvmnt_v2 USING abap_false lv_lety1 CHANGING lv_subrc r_return_msg.
* INETUM - NR - 04.03.2022 - RENPRJ00032 - Fim
    IF lv_subrc NE 0 OR gs_reg-mblnr IS INITIAL.
      e_return  = 61. " Erro entrada produção
      gs_log-retorno = e_return.
      MODIFY zwm_log_efacec FROM gs_log.
      RETURN.
    ENDIF.

    gs_log-doc_entrada = gs_reg-mblnr.
    gs_log-ano_entrada = gs_reg-mjahr.

    "Se o lote não estiver preenchido ler no Doc. Material
    IF gs_reg-charg IS INITIAL.
      SELECT mblnr mjahr zeile matnr werks lgort charg
        FROM mseg INTO TABLE lt_mseg[]
        WHERE mblnr EQ gs_reg-mblnr
          AND mjahr EQ gs_reg-mjahr.
      SORT lt_mseg[] BY matnr.
      DELETE lt_mseg[] WHERE matnr NE gs_reg-matnr.
      READ TABLE lt_mseg[] ASSIGNING <fs_mseg> INDEX 1.
      IF sy-subrc EQ 0.
        gs_reg-werks = <fs_mseg>-werks.
        gs_reg-charg = <fs_mseg>-charg.
        gs_reg-lgort = <fs_mseg>-lgort.
      ENDIF.
    ENDIF.

    " Actualiza quantidades
    SELECT SINGLE *
      FROM zwm030 INTO ls_zwm030
      WHERE aufnr EQ gs_reg-aufnr.
    IF sy-subrc EQ 0.
      ls_zwm030-data = sy-datum.

      gs_log-meins = ls_zwm030-meins_p.
      " Paletes remontadas, multiplica por 2
      IF gs_reg-hukey2 IS NOT INITIAL.
        ls_zwm030-prodp = ls_zwm030-prodp + gs_reg-menge * 2.
        gs_log-pal_remontada = abap_true.
      ELSE.
        ls_zwm030-prodp = ls_zwm030-prodp + gs_reg-menge.
      ENDIF.

      CALL FUNCTION 'ENQUEUE_EZ_WM_AUFNR'
        EXPORTING
          aufnr          = ls_zwm030-aufnr
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc NE 0.
        e_return = 88.
        gs_log-retorno = e_return.
        MODIFY zwm_log_efacec FROM gs_log.
        RETURN.
      ENDIF.

      UPDATE zwm030 FROM ls_zwm030.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      CALL FUNCTION 'DEQUEUE_EZ_WM_AUFNR'
        EXPORTING
          aufnr = ls_zwm030-aufnr.
    ENDIF.
    IF gs_reg-mblnr IS INITIAL OR gs_reg-mjahr IS INITIAL.
      e_return  = 60. " Erro entrada produção
      gs_log-retorno = e_return.
      MODIFY zwm_log_efacec FROM gs_log.
      RETURN.
    ENDIF.
  ENDIF.

* Paulo sousa 2017.12.15 - apenas se nao simulação. Mudado para linhas acima
*  IF gs_reg-mblnr IS INITIAL OR
*     gs_reg-mjahr IS INITIAL.
*    e_return  = 60. " Erro entrada produção
*    RETURN.
*  ENDIF.

  SELECT SINGLE vfdat FROM mcha INTO gs_reg-vfdat
    WHERE matnr EQ gs_reg-matnr
      AND werks EQ gs_reg-werks
      AND charg EQ gs_reg-charg.

  WRITE gs_reg-menge TO lv_menge DECIMALS 0.            "#EC UOM_IN_MES
  CONDENSE lv_menge.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    EXPORTING
      input  = gs_reg-matnr
    IMPORTING
      output = lv_matnr.

  IF gs_reg-hukey1 IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_reg-hukey1
      IMPORTING
        output = gs_reg-hukey1.
  ENDIF.

  IF gs_reg-hukey2 IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_reg-hukey2
      IMPORTING
        output = gs_reg-hukey2.
  ENDIF.

  CLEAR e_outstring.
  e_outstring(20)     = gs_reg-hukey1.
  e_outstring+20(20)  = gs_reg-hukey2.
  e_outstring+40(18)  = lv_matnr.
  e_outstring+58(40)  = gs_reg-maktx.
  e_outstring+98(10)  = gs_reg-charg.
  e_outstring+108(6)  = lv_menge.
  e_outstring+114(3)  = '111'.
  e_outstring+117(40) = gs_reg-makt2.
  e_outstring+157(40) = gs_reg-makt3.
  e_outstring+197(8)  = gs_reg-vfdat.
  e_outstring+205(3)  = lv_meins_p.
  e_outstring+208(1)  = '*'.

  PERFORM f_ean128_encode USING gs_reg-hukey1 CHANGING lv_subrc lv_ean128_cba lv_ean128_cbb lv_ean128_cbc lv_ean128_txa lv_ean128_txb lv_ean128_txc.

  e_ean128_bc1 = lv_ean128_cba.
  e_ean128_bc2 = lv_ean128_cbb.
  e_ean128_bc4 = lv_ean128_cbc.
  e_ean128_tx1 = lv_ean128_txa.
  e_ean128_tx2 = lv_ean128_txb.
  e_ean128_tx4 = lv_ean128_txc.

  PERFORM f_ean128_encode USING gs_reg-hukey2 CHANGING lv_subrc lv_ean128_cba lv_ean128_cbb lv_ean128_cbc lv_ean128_txa lv_ean128_txb lv_ean128_txc.

  e_ean128_bc3 = lv_ean128_cbb.
  e_ean128_tx3 = lv_ean128_txb.


  CONCATENATE e_ean128_bc1 e_ean128_bc2 e_ean128_bc3 e_ean128_bc4
         INTO gs_log-out_string_cb
         SEPARATED BY space.

  CONCATENATE e_ean128_tx1 e_ean128_tx2 e_ean128_tx3 e_ean128_tx4
         INTO gs_log-out_string_tx
         SEPARATED BY space.


  gs_log-out_string = e_outstring.
  gs_log-retorno    = e_return.
  gs_log-meins      = ls_zwm030-meins_p.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_reg-hukey1
    IMPORTING
      output = gs_log-sscc.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_reg-hukey2
    IMPORTING
      output = gs_log-sscc1.

  IF i_noprint = abap_true.
    gs_log-msg = 'ENTRADA MANUAL SEM CRIACAO DE SSCC'.
  ENDIF.

  gs_log-mesa = gv_ablad.


  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.

  SET UPDATE TASK LOCAL.

  READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_movmm_ablad
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_table  = <fs_zwm001>-valor.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_reg-hukey1
    IMPORTING
      output = lv_sscc.

  IF i_simul NE abap_true.
    CALL FUNCTION 'Z_WMFR_RFC_GET_TO'
      EXPORTING
        i_lgnum      = lv_lgnum
        i_sscc       = lv_sscc
        i_table      = lv_table
      IMPORTING
        e_to         = e_tanum
        e_return     = e_return
        r_return_msg = r_return_msg.

    IF e_return IS NOT INITIAL.
      gs_log-retorno = e_return.
      gs_log-msg = r_return_msg.
      MODIFY zwm_log_efacec FROM gs_log.
      RETURN.
    ENDIF.
  ENDIF.

  IF i_getpdf EQ abap_true.
    lv_lenum  = gs_reg-hukey1.
    CALL FUNCTION 'ZPDF_GET_SSCC_LABEL'
      EXPORTING
        sscc = lv_lenum
      IMPORTING
        pdf  = e_pdf_1.

    IF gs_reg-hukey2 IS NOT INITIAL.
      lv_lenum  = gs_reg-hukey2.
      CALL FUNCTION 'ZPDF_GET_SSCC_LABEL'
        EXPORTING
          sscc = lv_lenum
        IMPORTING
          pdf  = e_pdf_2.
    ENDIF.
  ENDIF.

  IF i_getzpl EQ abap_true.
    lv_lenum_zpl  = gs_reg-hukey1.
    CALL FUNCTION 'ZPDF_GET_SSCC_ZPL'
      EXPORTING
        sscc = lv_lenum_zpl
      TABLES
        zpl  = e_zpl_1[].

    IF gs_reg-hukey2 IS NOT INITIAL.
      lv_lenum_zpl  = gs_reg-hukey2.
      CALL FUNCTION 'ZPDF_GET_SSCC_ZPL'
        EXPORTING
          sscc = lv_lenum_zpl
        TABLES
          zpl  = e_zpl_2[].
    ENDIF.
  ENDIF.

  gs_log-ordem = e_tanum.
  " Em modo de simulação nada mais a fazer
  IF i_simul = abap_true.
    RETURN.
  ENDIF.

* JCO - ROFFSDF - 01.08.2016 - RENSAF00039
* Obter dinamicamente a divisao
  READ TABLE gt_zwm030 INTO ls_zwm030_aux WITH KEY aufnr = i_aufnr. " JCO - ROFFSDF - 27.07.2016 - RENSAF00039
  IF sy-subrc EQ 0.
    SELECT * FROM z02rpsessao
             INTO TABLE lt_z02rpsessao
             WHERE divisao = ls_zwm030_aux-divisao AND " JCO - ROFFSDF - 27.07.2016 - RENSAF00039
*           WHERE divisao = 'DTAF' AND " OLD
                   linha   = i_line AND
                   aufnr1  = i_aufnr.
    IF sy-subrc NE 0.
      r_return_msg  = text-037.
      e_return      = 10.
      gs_log-retorno = e_return.
      gs_log-msg = r_return_msg.
      MODIFY zwm_log_efacec FROM gs_log.
      RETURN.
    ENDIF.
  ENDIF.
*END: JCO - ROFFSDF - 01.08.2016 - RENSAF00039
** Movimento de Arrumação no Automático
***********************************************************************
  DO 1 TIMES.
    CHECK NOT e_tanum IS INITIAL.

    SELECT SINGLE valor FROM zwm001
                        INTO lv_arrum_aut
                        WHERE armazem   = lv_lgnum AND
                              processo  = 'ENTRADA_PRODUCAO'  AND
                              parametro = 'CRIA_OT_AUT'.

    CHECK sy-subrc EQ 0.
    CHECK lv_arrum_aut EQ abap_true.

    SELECT SINGLE valor FROM zwm001
                        INTO lv_valor
                        WHERE armazem   = lv_lgnum AND
                              processo  = 'ENTRADA_PRODUCAO'  AND
                              parametro = 'MOV_CRIA_OT_AUT'.

    CHECK sy-subrc EQ 0.
    lv_bwlvs_aut = lv_valor.

    SELECT * FROM ltap
             INTO TABLE lt_ltap
             WHERE lgnum = lv_lgnum AND
                   tanum = e_tanum.

    CHECK sy-subrc EQ 0.
    DELETE lt_ltap WHERE nlenr IS INITIAL.
    CHECK NOT lt_ltap IS INITIAL.

    SORT lt_ltap BY nlenr.
    DELETE ADJACENT DUPLICATES FROM lt_ltap COMPARING nlenr.

    LOOP AT lt_ltap INTO ls_ltap.
      CALL FUNCTION 'L_TO_CREATE_MOVE_SU'
        EXPORTING
          i_lenum = ls_ltap-nlenr
          i_bwlvs = lv_bwlvs_aut
        EXCEPTIONS
          OTHERS  = 2.

      IF sy-subrc <> 0.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
            msgv3               = sy-msgv3
            msgv4               = sy-msgv4
          IMPORTING
            message_text_output = r_return_msg.

        e_return      = 10.
        gs_log-retorno = e_return.
        gs_log-msg = r_return_msg.
        MODIFY zwm_log_efacec FROM gs_log.
        RETURN.
      ENDIF.

    ENDLOOP.
  ENDDO.



** Salva LOG
***********************************************************************

** Obter dinamicamente a divisao
*  READ TABLE gt_zwm030 INTO ls_zwm030_aux WITH KEY aufnr = i_aufnr. " JCO - ROFFSDF - 27.07.2016 - RENSAF00039
*  IF sy-subrc EQ 0.
*    SELECT * FROM z02rpsessao
*             INTO TABLE lt_z02rpsessao
*             WHERE divisao = ls_zwm030_aux-divisao AND " JCO - ROFFSDF - 27.07.2016 - RENSAF00039
**           WHERE divisao = 'DTAF' AND " OLD
*                   linha   = i_line AND
*                   aufnr1  = i_aufnr.

  IF lt_z02rpsessao[] IS NOT INITIAL.
    SORT lt_z02rpsessao BY data DESCENDING uzeit DESCENDING.
    READ TABLE lt_z02rpsessao
          INTO ls_z02rpsessao
          INDEX 1.

    gs_log-divisao  = ls_z02rpsessao-divisao. " JCO - ROFFSDF - 27.07.2016 - RENSAF00039
    gs_log-sessao   = ls_z02rpsessao-sessao.  " JCO - ROFFSDF - 27.07.2016 - RENSAF00039
    gs_log-pernr    = ls_z02rpsessao-pernr.
    gs_log-equipa   = ls_z02rpsessao-equipa.
    gs_log-turno    = ls_z02rpsessao-turno.

    MODIFY zwm_log_efacec FROM gs_log.
  ENDIF.
*<- JCO - ROFFSDF - 27.07.2016
** Entarda de Paletes
***********************************************************************
  CHECK NOT i_vhilm IS INITIAL.

  SELECT SINGLE valor FROM zwm001
                      INTO lv_werks
                      WHERE armazem   = lv_lgnum AND
                            processo  = 'GERAL'  AND
                            parametro = 'PLANT'.

  SELECT SINGLE valor FROM zwm001
                      INTO lv_lgort
                      WHERE armazem   = lv_lgnum AND
                            processo  = 'GERAL'  AND
                            parametro = 'LGORT_BA'.



  ls_item-material = i_vhilm.
  ls_item-quantidade = 1.
  ls_item-uni = 'PAL'.
  COLLECT ls_item INTO lt_items.

  SELECT SINGLE *
          FROM mlgn
          INTO ls_mlgn
          WHERE matnr = gs_reg-matnr
            AND lgnum = lv_lgnum
            AND ( block = '03' OR block = '04' OR
                  block = '05' OR block = '06').
  IF sy-subrc = 0.
    SELECT SINGLE *
        FROM zwm001
        INTO ls_zwm001
            WHERE armazem = lv_lgnum AND
                  processo = 'MEIA-PALETE' AND
                  parametro = i_vhilm.
    IF sy-subrc = 0.
      ls_item-material = ls_zwm001-valor.
      IF ls_mlgn-block = '03' OR  ls_mlgn-block = '04'.
        ls_item-quantidade = 2.
      ELSEIF ls_mlgn-block = '05' OR  ls_mlgn-block = '06'.
        ls_item-quantidade = 4.
      ENDIF.
      ls_item-uni = 'PAL'.
      COLLECT ls_item INTO lt_items.
    ENDIF.
  ENDIF.
** FIM - Entrada de Meia Palete.

** Verificar se o material é gerido à quarto-palete e fazer transferencia
** das semi-paletes.
** INI - Entrada de Quarto Palete.
  SELECT SINGLE *
           FROM mlgn
           INTO ls_mlgn
               WHERE matnr = gs_reg-matnr
                 AND lgnum = lv_lgnum
                 AND ( block = '07' OR block = '08' OR
                       block = '09' OR block = '10').
  IF sy-subrc = 0.
    SELECT SINGLE *
        FROM zwm001
        INTO ls_zwm001
            WHERE armazem = lv_lgnum AND
                  processo = 'QUARTO-PALETE' AND
                  parametro = i_vhilm.
    IF sy-subrc = 0.
      ls_item-material = ls_zwm001-valor.
      IF ls_mlgn-block = '07' OR  ls_mlgn-block = '08'.
        ls_item-quantidade = 4.
      ELSEIF ls_mlgn-block = '09' OR  ls_mlgn-block = '10'.
        ls_item-quantidade = 8.
      ENDIF.
      ls_item-uni = 'PAL'.
      COLLECT ls_item INTO lt_items.
    ENDIF.
  ENDIF.
** FIM - Entrada de Quarto Palete.


  CALL FUNCTION 'ZWM_ENTRADAS_PALETES_PRODUCAO'
    EXPORTING
      i_lgnum  = lv_lgnum
      i_werks  = lv_werks
      i_lgort  = lv_lgort
      it_items = lt_items.

ENDFUNCTION.
