FUNCTION zwm_entradas_consolidacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_WERKS) TYPE  WERKS_D
*"     REFERENCE(I_EAN) TYPE  EAN11 OPTIONAL
*"     REFERENCE(I_MATNR) TYPE  MATNR OPTIONAL
*"     REFERENCE(I_CHARG) TYPE  CHARG_D OPTIONAL
*"     REFERENCE(I_MENGE) TYPE  MENGE_D OPTIONAL
*"     REFERENCE(I_MEINS) TYPE  MEINS OPTIONAL
*"     REFERENCE(I_TEST) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_EXIDV) OPTIONAL
*"     REFERENCE(I_EXIDV2) OPTIONAL
*"     REFERENCE(I_AUFNR) TYPE  AUFNR OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_MBLNR) TYPE  MBLNR
*"     REFERENCE(E_MJAHR) TYPE  MJAHR
*"     REFERENCE(E_MATNR) TYPE  MATNR
*"     REFERENCE(E_CHARG) TYPE  CHARG_D
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      NOT_CONSOLIDATION
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lt_data           TYPE TABLE OF gty_data_consolidation,
        lt_data_reads     TYPE TABLE OF gty_data_consolidation,
        lt_items          TYPE tab_bapi_goodsmvt_item,
        lt_mseg           TYPE TABLE OF mseg,
        lt_mseg_old       TYPE TABLE OF mseg,
        lt_msegc          TYPE TABLE OF mseg,
        lt_accountgl      TYPE TABLE OF bapiacgl09,
        lt_currencyamount	TYPE TABLE OF bapiaccr09,
        lt_return	        TYPE TABLE OF bapiret2,
        lt_ammount        TYPE TABLE OF dmbtr,
        lt_bkpf           TYPE TABLE OF bkpf.

  DATA: ls_message        TYPE bdcmsgcoll,
        ls_data           TYPE gty_data_consolidation,
        ls_data_reads     TYPE gty_data_consolidation,
        ls_item           TYPE bapi2017_gm_item_create,
        ls_t001           TYPE t001,
        ls_mlgn           TYPE mlgn,
        ls_bkpf           TYPE bkpf,
        ls_documentheader	TYPE bapiache09,
        ls_accountgl      TYPE bapiacgl09,
        ls_currencyamount	TYPE bapiaccr09,
        ls_return	        TYPE bapiret2,
        ls_mseg_old       TYPE mseg.


  DATA: lv_bwart_in   TYPE bwart,
        lv_bwart_out  TYPE bwart,
        lv_charg      TYPE charg_d,
        lv_lgort_f_in TYPE lgort_d,
        lv_quant      TYPE sytabix,
        lv_menge_in   TYPE menge_d,

        lv_kostl      TYPE kostl,
        lv_lgort_comp TYPE lgort_d,

        lv_bukrs      TYPE bukrs,
        lv_blart      TYPE blart,

        lv_hkont_c    TYPE hkont,
        lv_hkont_d    TYPE hkont,
        lv_mwskz      TYPE mwskz.

  DATA: lt_stpo        TYPE TABLE OF stpo,
        lt_mchb        TYPE TABLE OF mchb,
        lt_matnr       TYPE TABLE OF matnr,
        lt_zwm067      TYPE TABLE OF zwm067.

  DATA: ls_stko        TYPE stko,
        ls_stpo        TYPE stpo,
        ls_mast        TYPE mast,
        ls_zwm013      TYPE zwm013.

  DATA: ls_zwm067 TYPE zwm067,
        ls_mseg   TYPE mseg.

  DATA: lv_menge      TYPE menge_d,
        lv_preis      TYPE cprei,
        lv_itemno     TYPE posnr_acc,
        lv_matnr      TYPE matnr,
        lv_lines      TYPE sytabix,
        lv_exidv      TYPE exidv,
        lv_awkey      TYPE awkey,
        lv_amt_doccur	TYPE bapidoccur,
        lv_ammount    TYPE dmbtr.

  DATA: lr_budat     TYPE RANGE OF datum,
        lr_budat_old TYPE RANGE OF datum.

  DATA: ls_r_budat     LIKE LINE OF lr_budat,
        ls_r_budat_old LIKE LINE OF lr_budat.


  DATA: l_mat_ordem like afko-plnbez.

  FIELD-SYMBOLS: <ls_mchb>        TYPE mchb,
                 <ls_data>        TYPE gty_data_consolidation.

  lv_charg = i_charg.
  lv_matnr = i_matnr.

  CLEAR: et_messages, e_mblnr, e_mjahr, e_charg.

** Parametros
**********************************************************************
  DO 1 TIMES.
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'MOV_CODE_IN'
      IMPORTING
        e_valor     = lv_bwart_in
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'MOV_CODE_OUT'
      IMPORTING
        e_valor     = lv_bwart_out
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'STG_LOC_FINAL'
      IMPORTING
        e_valor     = lv_lgort_f_in
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'COST_CENTER'
      IMPORTING
        e_valor     = lv_kostl
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'STG_LOC_COMPONENTS'
      IMPORTING
        e_valor     = lv_lgort_comp
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'COMPANY'
      IMPORTING
        e_valor     = lv_bukrs
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'DOC_TYPE'
      IMPORTING
        e_valor     = lv_blart
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'CONTA_RASAO_C'
      IMPORTING
        e_valor     = lv_hkont_c
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'CONTA_RASAO_D'
      IMPORTING
        e_valor     = lv_hkont_d
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'TAX_CODE'
      IMPORTING
        e_valor     = lv_mwskz
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.
  ENDDO.

  IF sy-subrc <> 0.
    RAISE not_consolidation.
  ENDIF.

  IF NOT lv_matnr IS INITIAL.
    APPEND lv_matnr TO lt_matnr.
  ELSEIF NOT i_ean IS INITIAL.
    SELECT matnr FROM mean
                 INTO TABLE lt_matnr
                 WHERE ean11 = i_ean.

  ENDIF.

  "---------2019.10.25  -resolver conflito entre entradas das linhas e alvembal
  clear: l_mat_ordem.
  if i_aufnr is not initial.
    select single plnbez into l_mat_ordem from afko where aufnr = i_aufnr.
  endif.
  "-----------------------------------------------------------------------------

  DO 1 TIMES.
    CHECK NOT lt_matnr IS INITIAL.

    SELECT * FROM zwm067
             INTO TABLE lt_zwm067
             FOR ALL ENTRIES IN lt_matnr
             WHERE lgnum = i_lgnum AND
                   matnr = lt_matnr-table_line.

    DELETE lt_zwm067 WHERE datuv > sy-datum AND begti > sy-uzeit.

    DELETE lt_zwm067 WHERE datbe < sy-datum AND endeuz < sy-uzeit.

    DELETE lt_zwm067 WHERE inactive = abap_true.

    DELETE lt_zwm067 WHERE deleted = abap_true.

    CHECK lt_zwm067[] IS NOT INITIAL.

    DESCRIBE TABLE lt_zwm067 LINES lv_lines.
    CHECK lv_lines EQ 1.

    CLEAR: ls_zwm067.
    READ TABLE lt_zwm067
          INTO ls_zwm067
          INDEX 1.

  ENDDO.

  IF ls_zwm067-stlnr IS INITIAL.
    RAISE not_consolidation.
  ENDIF.

  e_matnr = ls_zwm067-matnr.
  lv_matnr = e_matnr.

  "--- 2019.10.25 - conflito alvembal - linhas
  if l_mat_ordem is not initial.
    if lv_matnr <> l_mat_ordem.
      RAISE not_consolidation.
    endif.
  endif.
  "-----------------------------

** Quantidade
**********************************************************************
  SELECT SINGLE * FROM mlgn
                  INTO ls_mlgn
                  WHERE matnr = lv_matnr AND
                        lgnum = i_lgnum.

  IF z_wm_cl_management=>is_remontada( is_data = ls_mlgn ) eq abap_true.
    lv_quant = 2.
  ELSE.
    lv_quant = 1.
  ENDIF.

** Dados de Empresa
**********************************************************************
  SELECT SINGLE * FROM t001
                  INTO ls_t001
                  WHERE bukrs = lv_bukrs.

** Retorna Lista Tecnica
**********************************************************************
  DO 1 TIMES.
    SELECT SINGLE * FROM stko
                    INTO ls_stko
                    WHERE stlnr = ls_zwm067-stlnr AND
                          stlal = ls_zwm067-stlal AND
                          datuv <= sy-datum.

    CHECK sy-subrc EQ 0.

    SELECT * FROM stpo
             INTO TABLE lt_stpo
             WHERE stlty = ls_stko-stlty AND
                   stlnr = ls_stko-stlnr AND
                   datuv <= sy-datum.

  ENDDO.

  IF sy-subrc <> 0.
    CLEAR: ls_message.
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = 'ZWM001'.
    ls_message-msgnr  = '064'.
    ls_message-msgv1  = ls_stko-stlnr.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  DO 1 TIMES.
    IF i_menge IS INITIAL AND i_test EQ abap_true.
      EXIT.
    ELSEIF i_menge IS INITIAL.
**    Indicar quantidade
      CLEAR: ls_message.
      ls_message-msgtyp = 'E'.
      ls_message-msgid  = 'ZWM001'.
      ls_message-msgnr  = '065'.
      APPEND ls_message TO et_messages.
      RAISE error.
    ENDIF.

    lv_menge_in = i_menge * lv_quant.

    LOOP AT lt_stpo INTO ls_stpo.
      "Conversões
      CLEAR: lv_menge.
      lv_menge = ( lv_menge_in * ls_stpo-menge ) / ls_stko-bmeng.
      ls_stpo-menge = lv_menge.

      CLEAR: lv_preis.
      lv_preis = ( lv_menge_in * ls_stpo-preis ) / ls_stko-bmeng.
      ls_stpo-preis = lv_preis.


      CLEAR: ls_data.

      "Dados
      ls_data-s_stpo = ls_stpo.
      APPEND ls_data TO lt_data  ASSIGNING <ls_data>.
      <ls_data>-index = sy-tabix.
    ENDLOOP.


    IF lt_data IS INITIAL.
      CLEAR: ls_message.
      ls_message-msgtyp = 'E'.
      ls_message-msgid  = 'ZWM001'.
      ls_message-msgnr  = '064'.
      ls_message-msgv1  = ls_stko-stlnr.
      APPEND ls_message TO et_messages.
      RAISE error.
    ENDIF.
  ENDDO.


** Determina Lotes de Stock
**********************************************************************
  IF NOT lt_data IS INITIAL.

    SELECT * FROM mchb
             INTO TABLE lt_mchb
             FOR ALL ENTRIES IN lt_data
             WHERE matnr = lt_data-s_stpo-idnrk AND
                   werks = i_werks AND
                   lgort = lv_lgort_comp.

    DELETE lt_mchb WHERE clabs <= 0.
    SORT lt_mchb BY ersda ASCENDING.

    LOOP AT lt_data INTO ls_data WHERE s_stpo-postp = 'L'.

      PERFORM update_0001_quantity USING lt_data_reads
                                   CHANGING lt_data
                                            ls_data.
      CHECK ls_data-menge_f > 0.


      LOOP AT lt_mchb ASSIGNING <ls_mchb> WHERE matnr = ls_data-s_stpo-idnrk AND
                                                werks = i_werks AND
                                                clabs > 0.

        IF ls_data-menge_f <= 0.
          EXIT.
        ENDIF.

        IF ls_data-menge_f >= <ls_mchb>-clabs.
          ls_data-menge = <ls_mchb>-clabs.
        ELSE.
          ls_data-menge = ls_data-menge_f.
        ENDIF.

        <ls_mchb>-clabs = <ls_mchb>-clabs - ls_data-menge.
        ls_data-s_mchb = <ls_mchb>.


        APPEND ls_data TO lt_data_reads.
        PERFORM update_0001_quantity USING lt_data_reads
                                     CHANGING lt_data
                                              ls_data.

      ENDLOOP.
    ENDLOOP.

    PERFORM update_0001_quantity USING lt_data_reads
                             CHANGING lt_data
                                      ls_data.

    LOOP AT lt_data INTO ls_data  WHERE s_stpo-postp = 'L' AND
                                        menge_f > 0.

**    Não é possivel consumir o Componente &, stock insuficiente
      CLEAR: ls_message.
      ls_message-msgtyp = 'E'.
      ls_message-msgid  = 'ZWM001'.
      ls_message-msgnr  = '061'.
      ls_message-msgv1  = ls_data-s_stpo-idnrk.
      APPEND ls_message TO et_messages.
    ENDLOOP.
    IF sy-subrc EQ 0.

      RAISE error.
    ENDIF.

  ENDIF.

** Get Batch
**********************************************************************
  DO 1 TIMES.
    CHECK lv_charg IS INITIAL.

    "Usa Lote da tabela
    lv_charg = ls_zwm067-charg.

    DO 1 TIMES.
      "Caso não tenha determina Lotes do mesmo mês
      CHECK lv_charg IS INITIAL.

      CLEAR: ls_r_budat.
      ls_r_budat-sign = 'I'.
      ls_r_budat-option = 'BT'.
      CONCATENATE sy-datum(6) '01' INTO ls_r_budat-low.
      CONDENSE ls_r_budat-low NO-GAPS.

      CALL FUNCTION 'SG_PS_GET_LAST_DAY_OF_MONTH'
        EXPORTING
          day_in            = sy-datum
        IMPORTING
          last_day_of_month = ls_r_budat-high
        EXCEPTIONS
          day_in_not_valid  = 1
          OTHERS            = 2.
      CHECK sy-subrc EQ 0.

      APPEND ls_r_budat TO lr_budat.


      SELECT * FROM mseg
               INTO TABLE lt_mseg
               WHERE budat_mkpf IN lr_budat AND
                     matnr = lv_matnr AND
                     werks = i_werks AND
                     lgort = lv_lgort_f_in AND
                     bwart = lv_bwart_in.
      CHECK sy-subrc EQ 0.

      SORT lt_mseg BY budat_mkpf DESCENDING budat_mkpf DESCENDING.

      CLEAR: ls_mseg.
      READ TABLE lt_mseg
            INTO ls_mseg
            INDEX 1.

      lv_charg = ls_mseg-charg.
    ENDDO.

    DO 1 TIMES.
      "Valida passagem de mês e batch a ser usado
      CHECK NOT lv_charg IS INITIAL.


      CLEAR: ls_r_budat_old.
      ls_r_budat_old-sign = 'I'.
      ls_r_budat_old-option = 'BT'.
      CONCATENATE sy-datum(6) '01' INTO ls_r_budat_old-high.
      ls_r_budat_old-high = ls_r_budat_old-high - 1.
      CONCATENATE ls_r_budat_old-high(6) '01' INTO ls_r_budat_old-low.
      CONDENSE ls_r_budat_old-low NO-GAPS.
      APPEND ls_r_budat_old TO lr_budat_old.


      SELECT SINGLE * FROM mseg
                      INTO ls_mseg_old
                      WHERE budat_mkpf IN lr_budat_old AND
                            matnr = lv_matnr AND
                            werks = i_werks AND
                            lgort = lv_lgort_f_in AND
                            bwart = lv_bwart_in AND
                            charg = lv_charg.
      CHECK sy-subrc EQ 0.
      CLEAR lv_charg.
    ENDDO.

    CHECK lv_charg IS INITIAL.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'BATCH_CLT'
      IMPORTING
        number                  = lv_charg
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    CHECK sy-subrc EQ 0.

    GET TIME.
    UPDATE zwm067 SET charg = lv_charg
                      chadt = sy-datum
                  WHERE lgnum = i_lgnum AND
                        matnr = lv_matnr AND
                        datbe = ls_zwm067-datbe.

    COMMIT WORK.
  ENDDO.

  e_charg = lv_charg.
  CHECK i_test EQ abap_false.

  DO lv_quant TIMES.
    IF sy-index = 1.
      lv_exidv = i_exidv.
    ELSE.
      lv_exidv = i_exidv2.
    ENDIF.

    IF lv_exidv IS INITIAL.
**    Numero de Palete & não informado
      CLEAR: ls_message.
      ls_message-msgtyp = 'E'.
      ls_message-msgid  = 'ZWM001'.
      ls_message-msgnr  = '066'.
      ls_message-msgv1  = sy-index.
      APPEND ls_message TO et_messages.
      PERFORM goodsmvt_cancel USING lt_msegc.
      RAISE error.
    ENDIF.
  ENDDO.

  lv_exidv = i_exidv.

** Consumo de Componentes
**********************************************************************
  CLEAR: ls_item, lt_items.
  LOOP AT lt_data_reads INTO ls_data.
    CLEAR ls_item.
    ls_item-move_type  = lv_bwart_out.
    ls_item-plant      = i_werks.
    ls_item-stge_loc   = lv_lgort_comp.
    ls_item-material   = ls_data-s_stpo-idnrk.
    ls_item-batch      = ls_data-s_mchb-charg.
    ls_item-entry_qnt  = ls_data-menge.
    ls_item-entry_uom  = ls_data-s_stpo-meins.
    ls_item-costcenter = lv_kostl.
    COLLECT ls_item INTO lt_items.
  ENDLOOP.

  CALL FUNCTION 'ZWM_GOODSMVT_CREATE'
    EXPORTING
      i_code       = '03'
      i_header_txt = lv_exidv
      i_bwart      = lv_bwart_out
      it_items     = lt_items
    IMPORTING
      et_mseg      = lt_mseg
      et_messages  = et_messages
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    PERFORM goodsmvt_cancel USING lt_msegc.
    RAISE error.
  ENDIF.

  APPEND LINES OF lt_mseg TO lt_msegc.

  CLEAR: ls_mseg.
  READ TABLE lt_mseg
        INTO ls_mseg
        INDEX 1.

  CONCATENATE ls_mseg-mblnr ls_mseg-mjahr INTO lv_awkey.
  CONDENSE lv_awkey NO-GAPS.
  DO 1 TIMES.
    SELECT * FROM bkpf
             INTO TABLE lt_bkpf
             WHERE awtyp = 'MKPF' AND
                   awkey = lv_awkey AND
                   awsys = ''.
    CHECK sy-subrc EQ 0.

    DESCRIBE TABLE lt_bkpf LINES lv_lines.
    CHECK lv_lines EQ 1.

    CLEAR: ls_bkpf.
    READ TABLE lt_bkpf
          INTO ls_bkpf
          INDEX 1.

    SELECT dmbtr FROM bseg
                 INTO TABLE lt_ammount
                 WHERE bukrs = lv_bukrs AND
                       belnr = ls_bkpf-belnr AND
                       gjahr = ls_bkpf-gjahr AND
                       buzid = 'M'.

    CHECK sy-subrc EQ 0.
  ENDDO.

  IF lt_ammount IS INITIAL.
**    Não foi possivel determinar custo de Documento Material &/&
    CLEAR: ls_message.
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = 'ZWM001'.
    ls_message-msgnr  = '067'.
    ls_message-msgv1  = ls_mseg-mblnr.
    ls_message-msgv1  = ls_mseg-mjahr.
    APPEND ls_message TO et_messages.
    PERFORM goodsmvt_cancel USING lt_msegc.
    RAISE error.
  ENDIF.

** Entrada de Produto Final e Documentos Contablisticos
**********************************************************************

**  Dados de Documento Contablistico
  CLEAR: ls_documentheader.
  ls_documentheader-username   = sy-uname.
  ls_documentheader-comp_code  = lv_bukrs.
  ls_documentheader-doc_date   = sy-datum.
  ls_documentheader-pstng_date = sy-datum.
  ls_documentheader-fisc_year  = sy-datum(4).
  ls_documentheader-fis_period = sy-datum+4(2).
  ls_documentheader-doc_type   = lv_blart.
  ls_documentheader-header_txt = lv_exidv.

  CLEAR: lt_accountgl,lt_currencyamount, lv_amt_doccur.
  LOOP AT lt_data INTO ls_data  WHERE s_stpo-postp <> 'L'.
    CLEAR: ls_accountgl.
    lv_itemno = lv_itemno + 1.
    ls_accountgl-itemno_acc = lv_itemno.
    ls_accountgl-gl_account = lv_hkont_c.
    ls_accountgl-doc_type   = lv_blart.
    ls_accountgl-pstng_date = sy-datum.
    ls_accountgl-tax_code   = lv_mwskz.
    ls_accountgl-costcenter = lv_kostl.
    ls_accountgl-material   = lv_matnr.
    APPEND ls_accountgl TO lt_accountgl.

    CLEAR: ls_currencyamount.
    ls_currencyamount-itemno_acc = lv_itemno.
    ls_currencyamount-currency   = ls_t001-waers.
    ls_currencyamount-amt_doccur = ls_data-s_stpo-preis.
    lv_amt_doccur = lv_amt_doccur + ls_currencyamount-amt_doccur. "Custo Total de Documento Contablistico
    APPEND ls_currencyamount TO lt_currencyamount.

    CLEAR: ls_accountgl.
    lv_itemno = lv_itemno + 1.
    ls_accountgl-itemno_acc = lv_itemno.
    ls_accountgl-gl_account = lv_hkont_d.
    ls_accountgl-doc_type   = lv_blart.
    ls_accountgl-pstng_date = sy-datum.
    ls_accountgl-tax_code   = lv_mwskz.
    ls_accountgl-material   = lv_matnr.
    APPEND ls_accountgl TO lt_accountgl.

    CLEAR: ls_currencyamount.
    ls_currencyamount-itemno_acc = lv_itemno.
    ls_currencyamount-currency   = ls_t001-waers.
    ls_currencyamount-amt_doccur = ls_data-s_stpo-preis * -1.
    APPEND ls_currencyamount TO lt_currencyamount.
  ENDLOOP.

**  Dados de Documento Material
  CLEAR: ls_item, lt_items.
  ls_item-move_type  = lv_bwart_in.
  ls_item-plant      = i_werks.
  ls_item-stge_loc   = lv_lgort_f_in.
  ls_item-material   = lv_matnr.
  ls_item-batch      = lv_charg.
  ls_item-entry_qnt  = lv_menge_in.
  ls_item-entry_uom  = i_meins.
  ls_item-costcenter = lv_kostl.
  ls_item-amount_lc  = lv_amt_doccur."Custo Total de Documento Contablistico
  LOOP AT lt_ammount INTO lv_ammount.
    ls_item-amount_lc  = ls_item-amount_lc + lv_ammount. "Custo Total de Consumos
  ENDLOOP.
  APPEND ls_item TO lt_items.

**  Lançamento de Documento Material Produto Final
  CALL FUNCTION 'ZWM_GOODSMVT_CREATE'
    EXPORTING
      i_code       = '05'
      i_header_txt = lv_exidv
      i_bwart      = lv_bwart_in
      it_items     = lt_items
    IMPORTING
      e_mblnr      = e_mblnr
      e_mjahr      = e_mjahr
      et_mseg      = lt_mseg
      et_messages  = et_messages
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    PERFORM goodsmvt_cancel USING lt_msegc.
    RAISE error.
  ENDIF.

  APPEND LINES OF lt_mseg TO lt_msegc.

**  Lançamento de Documento Contablistio
  CLEAR: ls_mseg.
  READ TABLE lt_mseg
        INTO ls_mseg
        INDEX 1.

  ls_documentheader-ref_doc_no = ls_mseg-mblnr."Referencia a Documento Material

  CLEAR: lt_return.
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = ls_documentheader
    TABLES
      accountgl      = lt_accountgl
      currencyamount = lt_currencyamount
      return         = lt_return.

  LOOP AT lt_return INTO ls_return WHERE type = 'E'
                                      OR type = 'A'.
    MOVE ls_return-type       TO ls_message-msgtyp.
    MOVE ls_return-id         TO ls_message-msgid.
    MOVE ls_return-number     TO ls_message-msgnr.
    MOVE ls_return-message_v1 TO ls_message-msgv1.
    MOVE ls_return-message_v2 TO ls_message-msgv2.
    MOVE ls_return-message_v3 TO ls_message-msgv3.
    MOVE ls_return-message_v4 TO ls_message-msgv4.
    APPEND ls_message TO et_messages.
    CLEAR  ls_message.
  ENDLOOP.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    PERFORM goodsmvt_cancel USING lt_msegc.
    RAISE error.
  ENDIF.
*  ENDDO.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
ENDFUNCTION.
