FUNCTION z_wmfr_to_consumption.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     VALUE(I_LENUM) TYPE  LENUM
*"  EXPORTING
*"     REFERENCE(ES_DETAILS) TYPE  ZWMMPT004
*"     REFERENCE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
  DATA lv_lgnum         TYPE lgnum.
  DATA lv_return        TYPE zwm_aux-retorno.
  DATA lv_constyp       TYPE char1.
  DATA lv_extwg         TYPE mara-extwg.
  DATA lv_gmcode        TYPE gm_code.
  DATA lv_werks         TYPE werks_d.
  DATA lv_lgort         TYPE lgort_d.
  DATA lv_bwart_cc      TYPE bwart.
  DATA lv_bwart_aufnr   TYPE bwart.
  DATA lv_mblnr         TYPE mblnr.
  DATA lv_mjahr         TYPE mjahr.
  DATA lv_entry_qty     TYPE menge_d.
  DATA lv_conslt        TYPE z02rpconslt-conslt.
  DATA lv_dtaf          TYPE z02rpconslt-divisao.

  DATA ls_xuser       TYPE lrf_wkqu.
  DATA ls_gmcode      TYPE bapi2017_gm_code.
  DATA ls_gmheader    TYPE bapi2017_gm_head_01.
  DATA ls_z02rpconslt TYPE z02rpconslt.

  DATA lt_gmitems   TYPE STANDARD TABLE OF bapi2017_gm_item_create.
  DATA lt_return    TYPE bapiret2_t.
  DATA lt_session   TYPE STANDARD TABLE OF z02rpsessao.

  FIELD-SYMBOLS <fs_zwm001>   LIKE LINE OF gt_zwmmpt001[].
  FIELD-SYMBOLS <fs_gmitem>   LIKE LINE OF lt_gmitems[].
  FIELD-SYMBOLS <fs_return>   LIKE LINE OF lt_return[].
  FIELD-SYMBOLS <fs_session>  LIKE LINE OF lt_session[].
*"----------------------------------------------------------------------

  IF i_lgnum IS NOT SUPPLIED.
    PERFORM f_data_init_get_user_data CHANGING ls_xuser lv_return. " get user RF data for WM
    IF lv_return IS INITIAL.
      lv_lgnum  = ls_xuser-lgnum.
    ENDIF.
  ELSE.
    lv_lgnum  = i_lgnum.
  ENDIF.

  IF gt_zwmmpt001[] IS INITIAL.
    SELECT processo parametro item valor
      FROM zwmmpt001 INTO TABLE gt_zwmmpt001[]
      WHERE armazem EQ lv_lgnum.
  ENDIF.

  CLEAR es_details.

  FREE et_return[].

  SELECT SINGLE *
    FROM zwmmpt004 INTO es_details
    WHERE lgnum EQ lv_lgnum
      AND lenum EQ i_lenum.
  IF sy-subrc NE 0.
    GET TIME.
    es_details-lgnum    = lv_lgnum.
    es_details-lenum    = i_lenum.
    es_details-reg_date = sy-datum.
    es_details-reg_time = sy-uzeit.
    es_details-reg_user = sy-uname.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_prodcons
                   parametro  = c_param_gm_code
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_gmcode = <fs_zwm001>-valor.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_werks
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_werks = <fs_zwm001>-valor.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_lgort
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_lgort = <fs_zwm001>-valor.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_prodcons
                   parametro  = c_param_bwart_mm_cc
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_bwart_cc = <fs_zwm001>-valor.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_prodcons
                   parametro  = c_param_bwart_mm_aufnr
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_bwart_aufnr = <fs_zwm001>-valor.
  ENDIF.

  READ TABLE gt_zwmmpt001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_div_dtaf
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_dtaf = <fs_zwm001>-valor.
  ENDIF.

  es_details-proccess_rounds  = es_details-proccess_rounds + 1.
  CLEAR es_details-error_messsage.

  MODIFY zwmmpt004 FROM es_details.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDIF.

  IF es_details-kostl IS INITIAL AND es_details-aufnr IS INITIAL AND es_details-aufnr2 IS INITIAL.
    es_details-status          = 6.        "Ordem de Produção em Falta
    es_details-error_messsage  = 'Dados relevantes para consumo (Ord.Prd/C.C) em falta.'(001).

    MODIFY zwmmpt004 FROM es_details.
    IF sy-subrc  = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      PERFORM f_send_mail USING es_details.

      RETURN.
    ENDIF.
  ENDIF.

  SELECT SINGLE extwg
    FROM mara INTO lv_extwg
    WHERE matnr EQ es_details-matnr.
  IF lv_extwg EQ c_extwg_hybrid.
    lv_constyp  = c_consumption_type_h.
  ELSE.
    SELECT extwg UP TO 1 ROWS
      FROM zwmmpt008 INTO lv_extwg
      WHERE extwg EQ lv_extwg.
    ENDSELECT.
    IF sy-subrc EQ 0.
      lv_constyp  = c_consumption_type_c.
    ELSE.
      lv_constyp  = c_consumption_type_p.
    ENDIF.
  ENDIF.

  IF lv_constyp EQ c_consumption_type_h.
*  Não faz consumo operação acaba aqui com sucesso
    CLEAR es_details-fail_mm.
    GET TIME.
    es_details-mblnr         = c_consumption_type_h.
    es_details-reg_cons_date = sy-datum.
    es_details-reg_cons_time = sy-uzeit.
    es_details-reg_cons_user = sy-uname.
    es_details-status        = 1.        "Consumo Efectuado com Sucesso.
    MODIFY zwmmpt004 FROM es_details.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ENDIF.
    RETURN.
  ENDIF.

  IF es_details-tanum IS NOT INITIAL AND es_details-mblnr IS INITIAL.
    CLEAR ls_gmcode.
    CLEAR ls_gmheader.

    FREE lt_gmitems[].

    ls_gmcode-gm_code = lv_gmcode.

    ls_gmheader-pstng_date  = sy-datum.

    IF es_details-kostl IS NOT INITIAL.
      APPEND INITIAL LINE TO lt_gmitems[] ASSIGNING <fs_gmitem>.
      <fs_gmitem>-plant         = lv_werks.
      <fs_gmitem>-stge_loc      = lv_lgort.
      <fs_gmitem>-costcenter    = es_details-kostl.
      <fs_gmitem>-move_type     = lv_bwart_cc.
      <fs_gmitem>-batch         = es_details-charg.
      <fs_gmitem>-entry_qnt     = es_details-qtd_consumo.
      <fs_gmitem>-entry_uom     = es_details-meins.
      <fs_gmitem>-entry_uom_iso = es_details-meins.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = es_details-matnr
        IMPORTING
          output = <fs_gmitem>-material.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = ls_gmheader
          goodsmvt_code    = ls_gmcode
        IMPORTING
          materialdocument = lv_mblnr
          matdocumentyear  = lv_mjahr
        TABLES
          goodsmvt_item    = lt_gmitems[]
          return           = lt_return[].
      SORT lt_return[] BY type.
      READ TABLE lt_return[] ASSIGNING <fs_return>
        WITH KEY type = c_msgty_e.
      IF sy-subrc EQ 0 OR lv_mblnr IS INITIAL.
        et_return[] = lt_return[].

        MESSAGE ID <fs_return>-id TYPE c_msgty_e
          NUMBER <fs_return>-number INTO es_details-error_messsage
          WITH <fs_return>-message_v1
               <fs_return>-message_v2
               <fs_return>-message_v3
               <fs_return>-message_v4.

        es_details-status = 4.        "Erro no Consumo
        es_details-fail_mm = abap_true.

        MODIFY zwmmpt004 FROM es_details.
        IF sy-subrc  = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ENDIF.

        PERFORM f_send_mail USING es_details.

        RETURN.
      ELSE.
        GET TIME.

        CLEAR es_details-fail_mm.
        es_details-mblnr         = lv_mblnr.
        es_details-mjahr         = lv_mjahr.
        es_details-reg_cons_date = sy-datum.
        es_details-reg_cons_time = sy-uzeit.
        es_details-reg_cons_user = sy-uname.
        es_details-status        = 1.        "Consumo Efectuado com Sucesso.
        MODIFY zwmmpt004 FROM es_details.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ENDIF.

        RETURN.
      ENDIF.
    ENDIF.

    IF es_details-aufnr IS NOT INITIAL.
      IF lv_constyp EQ c_consumption_type_p.  " validar se ordem tem o login feito
        SELECT *
          FROM z02rpsessao INTO TABLE lt_session[]
          WHERE divisao EQ lv_dtaf
            AND aufnr1 EQ es_details-aufnr.
        IF sy-subrc NE 0.
          CONCATENATE 'Ordem de Produção'(011) es_details-aufnr 'não está aberta na mascara.'(012)
            INTO es_details-error_messsage SEPARATED BY space.
          es_details-fail_wm = abap_true.

          MODIFY zwmmpt004 FROM es_details.
          IF sy-subrc  = 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.
          ENDIF.

          PERFORM f_send_mail USING es_details.

          RETURN.
        ELSE.
          SORT lt_session[] BY data DESCENDING uzeit DESCENDING.
          READ TABLE lt_session[] ASSIGNING <fs_session> INDEX 1.
          IF sy-subrc EQ 0.
            es_details-sessao = <fs_session>-sessao.

            MODIFY zwmmpt004 FROM es_details.
            IF sy-subrc  = 0.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      APPEND INITIAL LINE TO lt_gmitems[] ASSIGNING <fs_gmitem>.
      <fs_gmitem>-plant         = lv_werks.
      <fs_gmitem>-stge_loc      = lv_lgort.
      <fs_gmitem>-move_type     = lv_bwart_aufnr.
      <fs_gmitem>-batch         = es_details-charg.
      <fs_gmitem>-entry_uom     = es_details-meins.
      <fs_gmitem>-entry_uom_iso = es_details-meins.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = es_details-matnr
        IMPORTING
          output = <fs_gmitem>-material.

      IF es_details-aufnr IS NOT INITIAL AND es_details-aufnr2 IS NOT INITIAL.
        lv_entry_qty = es_details-qtd_consumo / 2.

        <fs_gmitem>-orderid     = es_details-aufnr.
        <fs_gmitem>-entry_qnt   = lv_entry_qty.

        APPEND INITIAL LINE TO lt_gmitems[] ASSIGNING <fs_gmitem>.
        <fs_gmitem>-plant         = lv_werks.
        <fs_gmitem>-stge_loc      = lv_lgort.
        <fs_gmitem>-move_type     = lv_bwart_aufnr.
        <fs_gmitem>-batch         = es_details-charg.
        <fs_gmitem>-entry_uom     = es_details-meins.
        <fs_gmitem>-entry_uom_iso = es_details-meins.
        <fs_gmitem>-orderid       = es_details-aufnr2.
        <fs_gmitem>-entry_qnt     = es_details-qtd_consumo - lv_entry_qty.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = es_details-matnr
          IMPORTING
            output = <fs_gmitem>-material.
      ELSE.
        IF es_details-aufnr IS NOT INITIAL.
          <fs_gmitem>-orderid     = es_details-aufnr.
        ELSE.
          <fs_gmitem>-orderid     = es_details-aufnr2.
        ENDIF.

        <fs_gmitem>-entry_qnt     = es_details-qtd_consumo.
      ENDIF.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = ls_gmheader
          goodsmvt_code    = ls_gmcode
        IMPORTING
          materialdocument = lv_mblnr
          matdocumentyear  = lv_mjahr
        TABLES
          goodsmvt_item    = lt_gmitems[]
          return           = lt_return[].
      SORT lt_return[] BY type.
      READ TABLE lt_return[] ASSIGNING <fs_return>
        WITH KEY type = c_msgty_e.
      IF sy-subrc EQ 0 OR lv_mblnr IS INITIAL.
        et_return[] = lt_return[].

        MESSAGE ID <fs_return>-id TYPE c_msgty_e
          NUMBER <fs_return>-number INTO es_details-error_messsage
          WITH <fs_return>-message_v1
               <fs_return>-message_v2
               <fs_return>-message_v3
               <fs_return>-message_v4.

        es_details-status = 4.        "Erro no Consumo
        es_details-fail_mm = abap_true.

        MODIFY zwmmpt004 FROM es_details.
        IF sy-subrc  = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ENDIF.

        PERFORM f_send_mail USING es_details.

        RETURN.
      ELSE.
        GET TIME.

        CLEAR es_details-fail_mm.
        es_details-mblnr         = lv_mblnr.
        es_details-mjahr         = lv_mjahr.
        es_details-reg_cons_date = sy-datum.
        es_details-reg_cons_time = sy-uzeit.
        es_details-reg_cons_user = sy-uname.
        es_details-status        = 1.        "Consumo Efectuado com Sucesso.
        MODIFY zwmmpt004 FROM es_details.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ENDIF.

        CLEAR ls_z02rpconslt.

        CALL FUNCTION 'Z_02RP_NUM_OPERACAO'
          EXPORTING
            divisao            = lv_dtaf
          IMPORTING
            num_operacao       = lv_conslt
          EXCEPTIONS
            erro_int_numeracao = 1
            OTHERS             = 2.
        IF sy-subrc EQ 0.
          GET TIME.

          ls_z02rpconslt-divisao  = lv_dtaf.
          ls_z02rpconslt-conslt   = lv_conslt.
          ls_z02rpconslt-matnr    = es_details-matnr.
          ls_z02rpconslt-charg    = es_details-charg.
          ls_z02rpconslt-meins    = es_details-meins.
          ls_z02rpconslt-lgort    = lv_lgort.
          ls_z02rpconslt-mblnr    = lv_mblnr.
          ls_z02rpconslt-mjahr    = lv_mjahr.
          ls_z02rpconslt-sessao   = es_details-sessao.
          ls_z02rpconslt-datum    = sy-datum.
          ls_z02rpconslt-uname    = sy-uname.
          ls_z02rpconslt-uzeit    = sy-uzeit.

          IF es_details-aufnr IS NOT INITIAL AND es_details-aufnr2 IS NOT INITIAL.
            ls_z02rpconslt-aufnr = es_details-aufnr.
            ls_z02rpconslt-menge = lv_entry_qty.

            INSERT z02rpconslt FROM ls_z02rpconslt.

            ls_z02rpconslt-aufnr = es_details-aufnr2.
            ls_z02rpconslt-menge = es_details-qtd_consumo - lv_entry_qty.

            INSERT z02rpconslt FROM ls_z02rpconslt.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = abap_true.
            ENDIF.
          ELSE.
            IF es_details-aufnr IS NOT INITIAL.
              ls_z02rpconslt-aufnr = es_details-aufnr.
            ELSE.
              es_details-aufnr = es_details-aufnr2.
            ENDIF.

            ls_z02rpconslt-menge = es_details-qtd_consumo.

            INSERT z02rpconslt FROM ls_z02rpconslt.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.

        RETURN.
      ENDIF.
    ENDIF.
  ELSE.
    IF es_details-fail_mm IS NOT INITIAL AND es_details-mblnr IS NOT INITIAL.
      CLEAR es_details-fail_mm.

      MODIFY zwmmpt004 FROM es_details.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.
