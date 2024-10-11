FUNCTION z_wmfr_rfc_production_entry.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_INSTRING) TYPE  ZWM_AUX-IN_STRING
*"     VALUE(I_SIMUL) TYPE  ZWM_AUX-SIMULA
*"     VALUE(I_NOPRINT) TYPE  FLAG
*"     VALUE(I_GETPDF) TYPE  FLAG DEFAULT 'X'
*"     VALUE(I_QUANTITY) TYPE  MENGE_D OPTIONAL
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
*"     VALUE(E_RETURN) TYPE  ZWM_AUX-RETORNO
*"----------------------------------------------------------------------
  DATA lv_select      TYPE i.
  DATA lv_subrc       TYPE sysubrc.
  DATA lv_msg         TYPE bapi_msg.
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

  DATA ls_zwm030  TYPE zwm030.

  DATA lt_aufk    TYPE ty_ht_aufk.
  DATA lt_istat   TYPE STANDARD TABLE OF tj02t-istat.
  DATA lt_jest    TYPE ty_st_jest.
  DATA lt_mean    TYPE STANDARD TABLE OF ty_ean.
  DATA lt_zwm001  TYPE STANDARD TABLE OF zwm001.
  DATA lt_mseg    TYPE STANDARD TABLE OF ty_mseg.

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

  FREE gt_zwm030[].
  FREE gt_zwm001[].

  IF i_instring IS INITIAL.
    e_return = 70. " Erro na transferência (tabela vazia)
    RETURN.
  ENDIF.

  gs_ifprod-codigo        = i_instring(18).
  gs_ifprod-linha         = i_instring+18(3).
  gs_ifprod-altura        = i_instring+21(4).
  gs_ifprod-cor           = i_instring+25(6).
  gs_ifprod-quantidade    = i_instring+31(4).
  gs_ifprod-aufnr         = i_instring+35(12).
  gs_ifprod-pal_completa  = i_instring+47(1).

  e_label = c_label_default.
  e_matnr = gs_ifprod-cor.

* Verifica se pelo menos a Linha ou o Código estão preenchidos
  IF gs_ifprod-linha IS INITIAL AND gs_ifprod-codigo IS INITIAL.
    e_return = 71. " Sem Linha/Código preenchido
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
    e_return = 10.
    RETURN.
  ENDIF.

  APPEND INITIAL LINE TO lr_txt04[] ASSIGNING <fs_txt04>.
  <fs_txt04>-sign   = c_rsig_i.
  <fs_txt04>-option = c_ropt_eq.
  <fs_txt04>-low    = 'ENTE'.
  APPEND INITIAL LINE TO lr_txt04[] ASSIGNING <fs_txt04>.
  <fs_txt04>-sign   = c_rsig_i.
  <fs_txt04>-option = c_ropt_eq.
  <fs_txt04>-low    = 'ENCE'.

  SELECT istat
    FROM tj02t INTO TABLE lt_istat[]
    WHERE txt04 IN lr_txt04[]
      AND spras EQ sy-langu.             "#EC CI_SGLSELECT  " Index 001
  LOOP AT lt_istat[] ASSIGNING <fs_istat>.
    APPEND INITIAL LINE TO lr_istat[] ASSIGNING <fs_ristat>.
    <fs_ristat>-sign   = c_rsig_i.
    <fs_ristat>-option = c_ropt_eq.
    <fs_ristat>-low    = <fs_istat>.
  ENDLOOP.

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

  IF gs_ifprod-linha IS NOT INITIAL.
    PERFORM f_check_prodline  CHANGING lv_subrc e_matnr.
    IF lv_subrc NE 0.
      e_return = lv_subrc.
      RETURN.
    ENDIF.

    IF gs_ifprod-codigo IS NOT INITIAL.
      PERFORM f_check_code CHANGING lv_subrc e_outprod.
    ENDIF.
  ELSE.
    IF gs_ifprod-codigo IS NOT INITIAL.
      PERFORM f_check_code CHANGING lv_subrc e_outprod.
    ENDIF.
    IF lv_subrc NE 0.
      e_return = lv_subrc.
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

  IF lv_subrc NE 0.
    e_return = lv_subrc.
    RETURN.
  ENDIF.
  " alterado Paulo Sousa 2017.07.12- SEMLOGO automaticamente para materiais MDD
  data: zgrp1 like mvke-mvgr1, zlay like zwm043-lay_etiqueta.
  clear: zgrp1, zlay.
  select single mvgr1 into zgrp1 from mvke where matnr = gs_reg-matnr.
  if zgrp1 = 'MDD'.  " marcas da distribuicao
    zlay = 'SEMLOGO'.
  else.
    SELECT SINGLE lay_etiqueta INTO zlay FROM zwm043 WHERE matnr = gs_reg-matnr.
  endif.
  if zlay is not initial.
    e_label = zlay.
    CONDENSE e_label.
  endif.

*  SELECT SINGLE lay_etiqueta
*    FROM zwm043 INTO lv_lay_label
*    WHERE matnr EQ gs_reg-matnr.
*  IF sy-subrc EQ 0.
*    IF lv_lay_label IS NOT INITIAL.
*      e_label = lv_lay_label.
*      CONDENSE e_label.
*    ENDIF.
*  ENDIF.

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
    e_return = 80.  " Não existe entrada na MLGN
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

* Verificar se é Palete Completa
  IF gs_ifprod-pal_completa IS NOT INITIAL.
    PERFORM f_get_total_quantity CHANGING lv_subrc.
  ELSE.
* Verifica Quantidade ou Altura
    IF gs_ifprod-quantidade EQ 0.
*   Verifica altura
      PERFORM f_check_height USING lv_lety1 CHANGING lv_subrc e_hoehe.
    ELSE.
*   Verifica quantidade
      PERFORM f_check_quantity USING lv_lety1 CHANGING lv_subrc.
    ENDIF.
  ENDIF.

  IF lv_subrc NE 0.
    e_return = lv_subrc.
    RETURN.
  ENDIF.

  IF gs_ifprod-cor IS INITIAL.
    e_return = 50. " Cor não especificada
    RETURN.
  ENDIF.

  SELECT SINGLE matnr
    FROM mara INTO gs_reg-pack
    WHERE matnr EQ gs_ifprod-cor.
  IF sy-subrc NE 0.
    e_return = 51. " Cor invalida
    RETURN.
  ENDIF.

* Valida se o Lote está aberto no armazém CD
  PERFORM f_check_batch USING gs_reg-matnr gs_reg-lgnum gs_reg-charg CHANGING lv_subrc.
  IF lv_subrc NE 0.
    e_return = lv_subrc.
    RETURN.
  ENDIF.

  IF i_noprint NE abap_true OR i_simul NE abap_true.
    PERFORM f_create_sscc CHANGING gs_reg-hukey1 lv_subrc.
    IF lv_subrc NE 0.
      e_return = lv_subrc.
      RETURN.
    ENDIF.

    e_return = 0.

    PERFORM f_update_zwm013 USING lv_lety1.

    IF lv_lety1 EQ c_lety1_p2 OR lv_lety1 EQ c_lety1_p5.
      PERFORM f_create_sscc CHANGING gs_reg-hukey2 lv_subrc.
      IF lv_subrc NE 0.
        e_return = lv_subrc.
        RETURN.
      ENDIF.

      PERFORM f_update_zwm020.
    ENDIF.
  ENDIF.

  IF i_simul NE abap_true.
    PERFORM f_do_goodsmvmnt USING abap_true lv_lety1 CHANGING lv_subrc lv_msg.
    IF lv_subrc NE 0.
      e_return  = 60. " Erro entrada produção
      RETURN.
    ENDIF.

    PERFORM f_do_goodsmvmnt USING abap_false lv_lety1 CHANGING lv_subrc lv_msg.
    IF lv_subrc NE 0 OR gs_reg-mblnr IS INITIAL.
      e_return  = 61. " Erro entrada produção
      RETURN.
    ENDIF.

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

    SELECT SINGLE *
      FROM zwm030 INTO ls_zwm030
      WHERE aufnr EQ gs_reg-aufnr.
    IF sy-subrc EQ 0.
      ls_zwm030-data = sy-datum.

      IF gs_reg-hukey2 IS NOT INITIAL.
        ls_zwm030-prodp = ls_zwm030-prodp + gs_reg-menge * 2.
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
  ENDIF.

  SELECT SINGLE vfdat
    FROM mcha INTO gs_reg-vfdat
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

  CALL FUNCTION 'Z_WMFR_RFC_GET_TO'
    EXPORTING
      i_sscc   = lv_sscc
      i_table  = lv_table
    IMPORTING
      e_to     = e_tanum
      e_return = e_return.

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
ENDFUNCTION.
