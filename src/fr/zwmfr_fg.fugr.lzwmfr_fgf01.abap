*----------------------------------------------------------------------*
***INCLUDE LZWMFR_FGF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_PRODLINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_SUBRC    text
*      <--C_MATNR    text
*----------------------------------------------------------------------*
FORM f_check_prodline CHANGING c_subrc TYPE sysubrc
                               c_matnr TYPE matnr.
  FIELD-SYMBOLS <fs_zwm030> LIKE LINE OF gt_zwm030[].

  SORT gt_zwm030[] BY linha.
  READ TABLE gt_zwm030[] ASSIGNING <fs_zwm030>
    WITH KEY linha = gs_ifprod-linha
    BINARY SEARCH.
  IF sy-subrc NE 0.
    c_subrc = 10. " Linha não existe
    RETURN.
  ENDIF.

  gs_reg-matnr = <fs_zwm030>-matnr.
  gs_reg-aufnr = <fs_zwm030>-aufnr.
  gs_reg-lgnum = <fs_zwm030>-lgnum.
  gs_reg-code  = <fs_zwm030>-code.
  gs_reg-bwart = <fs_zwm030>-bwart.
  gs_reg-werks = <fs_zwm030>-werks.
  gs_reg-charg = <fs_zwm030>-charg.

  PERFORM f_get_descriptions USING gs_reg-matnr.
  PERFORM f_check_paltype USING gs_reg-matnr gs_ifprod-linha CHANGING c_matnr gs_ifprod-cor.

  SORT gt_zwm030[] BY aufnr.
ENDFORM.                    " F_CHECK_PRODLINE
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_SUBRC    text
*      <--C_OUTPROD  text
*----------------------------------------------------------------------*
FORM f_check_code CHANGING c_subrc    TYPE sysubrc
                           c_outprod  TYPE zwm_aux-out_producao.
  DATA lv_tabix TYPE sytabix.
  DATA lv_count TYPE sytabix.
  DATA lv_matnr TYPE mean-matnr.

  DATA lt_mean  TYPE STANDARD TABLE OF ty_ean WITH DEFAULT KEY.

  FIELD-SYMBOLS <fs_mean>   LIKE LINE OF lt_mean[].
  FIELD-SYMBOLS <fs_zwm030> LIKE LINE OF gt_zwm030[].

  CLEAR c_outprod.
  CLEAR gs_reg-matnr.

  SELECT matnr meinh ean11 eantp hpean
    FROM mean INTO TABLE lt_mean[]
    WHERE ean11 EQ gs_ifprod-codigo.
  IF sy-subrc NE 0.
    c_subrc = 20. " Código inválido
    RETURN.
  ENDIF.

  SORT gt_zwm030[] BY matnr.

  lv_count  = 0.

  SORT lt_mean[] BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_mean[] COMPARING matnr.

  LOOP AT lt_mean[] ASSIGNING <fs_mean>.
    READ TABLE gt_zwm030[] TRANSPORTING NO FIELDS
      WITH KEY matnr = <fs_mean>-matnr
      BINARY SEARCH.
    CHECK sy-subrc EQ 0.
    lv_tabix = sy-tabix.

    LOOP AT gt_zwm030[] ASSIGNING <fs_zwm030> FROM lv_tabix.
      IF <fs_zwm030>-matnr NE <fs_mean>-matnr.
        EXIT.
      ENDIF.

      gs_reg-matnr = <fs_zwm030>-matnr.
      gs_reg-aufnr = <fs_zwm030>-aufnr.
      gs_reg-lgnum = <fs_zwm030>-lgnum.
      gs_reg-code  = <fs_zwm030>-code.
      gs_reg-bwart = <fs_zwm030>-bwart.
      gs_reg-werks = <fs_zwm030>-werks.
      gs_reg-charg = <fs_zwm030>-charg.

      lv_count  = lv_count + 1.

      IF c_outprod IS INITIAL.
        c_outprod = <fs_zwm030>-aufnr.
      ELSE.
        CONCATENATE c_outprod ';' <fs_zwm030>-aufnr INTO c_outprod.
      ENDIF.
    ENDLOOP.
    CHECK sy-subrc EQ 0.

    IF lv_matnr NE <fs_mean>-matnr.
      lv_matnr  = <fs_mean>-matnr.
      PERFORM f_get_descriptions USING <fs_mean>-matnr.
    ENDIF.
  ENDLOOP.

  IF gs_reg-matnr IS INITIAL.
    CLEAR c_outprod.
    c_subrc = 21. " Material não existe
  ELSEIF lv_count GT 1.
    CLEAR gs_reg-matnr.
    c_subrc = 22. " Material existe em mais do que uma ordem
  ENDIF.

  SORT gt_zwm030[] BY aufnr.
ENDFORM.                    " F_CHECK_CODE
*&---------------------------------------------------------------------*
*&      Form  F_GET_DESCRIPTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_MATNR    text
*----------------------------------------------------------------------*
FORM f_get_descriptions USING i_matnr TYPE mara-matnr.
  DATA lv_maktx TYPE makt-maktx.

  SELECT SINGLE maktx
    FROM makt INTO lv_maktx
    WHERE matnr EQ i_matnr
      AND spras EQ c_spras_pt.
  IF sy-subrc EQ 0.
    gs_reg-maktx = lv_maktx.
  ENDIF.

  CLEAR lv_maktx.
  SELECT SINGLE maktx
    FROM makt INTO lv_maktx
    WHERE matnr EQ i_matnr
      AND spras EQ c_spras_es.
  IF sy-subrc EQ 0.
    gs_reg-makt2 = lv_maktx.
  ENDIF.

  CLEAR lv_maktx.
  SELECT SINGLE maktx
    FROM makt INTO lv_maktx
    WHERE matnr EQ i_matnr
      AND spras EQ c_spras_fr.
  IF sy-subrc EQ 0.
    gs_reg-makt3 = lv_maktx.
  ENDIF.
ENDFORM.                    " F_GET_DESCRIPTIONS
*&---------------------------------------------------------------------*
*&      Form  f_check_paltype
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_MATNR    text
*      -->I_FEVOR    text
*      <--C_MATNR    text
*      <--C_IFMATNR  text
*----------------------------------------------------------------------*
FORM f_check_paltype USING i_matnr      TYPE mara-matnr
                           i_fevor      TYPE fevor
                     CHANGING c_matnr   TYPE zwm_if_prod-cor
                              c_ifmatnr TYPE zwm_if_prod-cor.
  DATA lv_pal   TYPE ztipopalete-cor.
  DATA lv_matnr TYPE mara-matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_matnr
    IMPORTING
      output = lv_matnr.

  SELECT SINGLE cor
    FROM ztipopalete INTO lv_pal
    WHERE matnr EQ lv_matnr
      AND fevor EQ i_fevor.
  IF sy-subrc EQ 0.
    IF lv_pal NE space.
      c_ifmatnr = lv_pal.
      c_matnr   = lv_pal(6).
    ENDIF.
  ENDIF.
ENDFORM.                    " F_CHECK_PALTYPE
*&---------------------------------------------------------------------*
*&      Form  F_GET_TOTAL_QUANTITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_SUBRC    text
*      <--ENDFORM    text
*----------------------------------------------------------------------*
FORM f_get_total_quantity CHANGING c_subrc  TYPE sysubrc.
  SELECT SINGLE umrez
    FROM marm INTO gs_reg-menge
    WHERE matnr EQ gs_reg-matnr
      AND meinh EQ c_meinh_pal.
  IF sy-subrc NE 0.
    c_subrc = 30. " Unidade PAL não atribuída
    RETURN.
  ENDIF.

  SELECT SINGLE meins
    FROM mara INTO gs_reg-cunit
    WHERE matnr EQ gs_reg-matnr.
ENDFORM.                    " F_GET_TOTAL_QUANTITY
*&---------------------------------------------------------------------*
*&      Form  f_check_complete_pal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_SUBRC    text
*----------------------------------------------------------------------*
FORM f_check_complete_pal CHANGING c_subrc  TYPE sysubrc.
  DATA lv_umrez TYPE marm-umrez.

  SELECT SINGLE umrez
    FROM marm INTO lv_umrez
    WHERE matnr EQ gs_reg-matnr
      AND meinh EQ c_meinh_pal.
  IF sy-subrc NE 0.
    c_subrc = 30. " Unidade PAL não atribuída
    RETURN.
  ENDIF.

  IF gs_reg-menge GT lv_umrez.
    c_subrc = 30. " Quantidade superior a palete completa
    RETURN.
  ENDIF.

  SELECT SINGLE meins
    FROM mara INTO gs_reg-cunit
    WHERE matnr EQ gs_reg-matnr.
ENDFORM.                    "f_check_complete_pal
*&---------------------------------------------------------------------*
*&      Form  f_check_height
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_LETY1    text
*      <--C_SUBRC    text
*      <--C_HEIGHT   text
*----------------------------------------------------------------------*
FORM f_check_height USING i_lety1  TYPE mlgn-lety1
                    CHANGING c_subrc  TYPE sysubrc
                             c_height TYPE hoehe.
  DATA lv_hoehe       TYPE marm-hoehe.
  DATA lv_umrez       TYPE marm-umrez.
  DATA lv_meabm       TYPE marm-meabm.
  DATA lv_qty_aux     TYPE marm-hoehe.
  DATA lv_conv        TYPE vhuadmin-quantity.
  DATA lv_npalls      TYPE i.
  DATA lv_marm_hoehe  TYPE marm-hoehe.
  DATA lv_max_height  TYPE marm-hoehe.
  DATA lv_min_height  TYPE marm-hoehe.

  DATA ls_marm  TYPE marm.

  FIELD-SYMBOLS <fs_zwm001> LIKE LINE OF gt_zwm001[].

  SELECT SINGLE *
    FROM marm INTO ls_marm
    WHERE matnr EQ gs_ifprod-cor
      AND meinh EQ c_meinh_pal.
  IF sy-subrc NE 0.
    c_subrc = 30. " Unidade PAL não atribuída
    RETURN.
  ENDIF.

  lv_hoehe    = ls_marm-hoehe.
  lv_meabm    = ls_marm-meabm.
  lv_qty_aux  = lv_hoehe.
  lv_conv     = lv_qty_aux.

  CALL FUNCTION 'VHUMISC_CONVERT_TO_ALTERN_UNIT'
    EXPORTING
      matnr               = gs_ifprod-cor
      quantity_in_meins   = lv_conv
      meins               = lv_meabm
      unitqty             = 'MM'
    IMPORTING
      quantity_in_unitqty = lv_conv
    EXCEPTIONS
      conversion_failed   = 1
      rounding_error      = 2
      OTHERS              = 3.
  IF sy-subrc NE 0.
    lv_conv = lv_qty_aux.
  ENDIF.

  lv_qty_aux  = lv_conv.
  lv_hoehe    = lv_qty_aux.

  IF i_lety1 EQ c_lety1_p2 OR i_lety1 EQ c_lety1_p5.
    lv_npalls = 2.
    lv_hoehe  = lv_hoehe * 2.
  ELSE.
    lv_npalls = 1.
  ENDIF.

  SELECT SINGLE umrez hoehe
    FROM marm INTO (lv_umrez, lv_marm_hoehe)
    WHERE matnr EQ gs_reg-matnr
      AND meinh EQ c_meinh_pal.
  IF sy-subrc NE 0.
    c_subrc = 30. " Unidade PAL não atribuída
    RETURN.
  ENDIF.

  lv_marm_hoehe = lv_marm_hoehe * lv_npalls.

  c_height = lv_marm_hoehe.

  READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_prodentry
                   parametro  = c_param_max
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_max_height = <fs_zwm001>-valor.
  ENDIF.

  READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_prodentry
                   parametro  = c_param_min
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_min_height = <fs_zwm001>-valor.
  ENDIF.

  lv_min_height = lv_marm_hoehe - lv_min_height.
  lv_max_height = lv_marm_hoehe + lv_max_height.

  IF gs_ifprod-altura GE lv_min_height AND gs_ifprod-altura LE lv_max_height.
    gs_reg-menge = lv_umrez.
  ELSE.
    c_subrc = 31. " Altura inválida
    RETURN.
  ENDIF.

  SELECT SINGLE meins
    FROM mara INTO gs_reg-cunit
    WHERE matnr EQ gs_reg-matnr.
ENDFORM.                    " F_CHECK_HEIGHT
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_QUANTITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_LETY1    text
*      <--C_SUBRC    text
*----------------------------------------------------------------------*
FORM f_check_quantity USING i_lety1  TYPE mlgn-lety1
                      CHANGING c_subrc  TYPE sysubrc.
  DATA ls_marm  TYPE marm.

  SELECT SINGLE *
    FROM marm INTO ls_marm
    WHERE matnr EQ gs_reg-matnr
      AND meinh EQ c_meinh_pal.
  IF sy-subrc NE 0.
    c_subrc = 30. " Unidade PAL não atribuída
    RETURN.
  ENDIF.

* Verifica se para as paletes remontadas as paletes são com quantidades completas
  IF i_lety1 EQ c_lety1_p2 OR i_lety1 EQ c_lety1_p5.
    IF gs_ifprod-quantidade NE ls_marm-umrez.
      c_subrc = 91.
      RETURN.
    ENDIF.
  ENDIF.

  IF gs_ifprod-quantidade GT ls_marm-umrez.
    c_subrc = 40. " Quantidade inválida
    RETURN.
  ELSE.
    gs_reg-menge = gs_ifprod-quantidade.
  ENDIF.

* Ler MARA para extrair unidade base
  SELECT SINGLE meins
    FROM mara INTO gs_reg-cunit
    WHERE matnr EQ gs_reg-matnr.
ENDFORM.                    " F_CHECK_QUANTITY
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_BATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_MATNR    text
*      -->I_LGNUM    text
*      -->I_CHARG    text
*      <--C_SUBRC    text
*----------------------------------------------------------------------*
FORM f_check_batch USING i_matnr  TYPE mara-matnr
                         i_lgnum  TYPE mlgn-lgnum
                         i_charg  TYPE mchb-charg
                   CHANGING c_subrc TYPE sysubrc.
  DATA lt_return TYPE tab_bdcmsgcoll.

  CALL FUNCTION 'ZWM_BATCH_CREATE'
    EXPORTING
      armazem           = i_lgnum
      material          = i_matnr
      lote              = i_charg
    TABLES
      return_msg        = lt_return[]
    EXCEPTIONS
      erro              = 1
      batch_not_created = 2
      OTHERS            = 3.
  IF sy-subrc NE 0.
    c_subrc = 96.
  ENDIF.

  FREE lt_return[].
ENDFORM.                    " F_CHECK_BATCH
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_SSCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_HU       text
*      <--C_SUBRC    text
*----------------------------------------------------------------------*
FORM f_create_sscc CHANGING c_hu    TYPE bapihukey-hu_exid
                            c_subrc TYPE sysubrc.
  DATA lt_items   TYPE STANDARD TABLE OF zwm_items_hu.

  FIELD-SYMBOLS <fs_item> LIKE LINE OF lt_items[].

  APPEND INITIAL LINE TO lt_items[] ASSIGNING <fs_item>.
  <fs_item>-material  = gs_reg-matnr.
  <fs_item>-quantity  = gs_reg-menge.
  <fs_item>-unit      = gs_reg-cunit.
  <fs_item>-batch     = gs_reg-charg.

  CALL FUNCTION 'ZWM_CREATE_HU'
    EXPORTING
      warehouse                  = gs_reg-lgnum
      plant                      = gs_reg-werks
      s_loc                      = c_lgort_cd
      packing_material           = gs_reg-pack
    IMPORTING
      hukey                      = c_hu
    TABLES
      items                      = lt_items[]
    EXCEPTIONS
      empty_table                = 1
      reference_document_differs = 2
      empty_delivery_item        = 3
      item_not_found             = 4
      OTHERS                     = 5.
  c_subrc = sy-subrc.

  IF c_subrc NE 0.
    c_subrc = 90.
  ELSEIF c_hu IS INITIAL.
    c_subrc = 90.
  ENDIF.
ENDFORM.                    " F_CREATE_SSCC
*&---------------------------------------------------------------------*
*&      Form  f_update_zwm013
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_LETY1    text
*----------------------------------------------------------------------*
FORM f_update_zwm013 USING i_lety1 TYPE mlgn-lety1.
  DATA ls_zwm013 TYPE zwm013.

  FIELD-SYMBOLS <fs_zwm001> LIKE LINE OF gt_zwm001[].

  SELECT SINGLE *
    FROM zwm013 INTO ls_zwm013
    WHERE armazem EQ gs_reg-lgnum
      AND sscc EQ gs_reg-hukey1.
  IF sy-subrc EQ 0.
    RETURN.
  ENDIF.

  CLEAR ls_zwm013.

  ls_zwm013-armazem     = gs_reg-lgnum.
  ls_zwm013-sscc        = gs_reg-hukey1.
  ls_zwm013-bloqueado   = abap_true.
  ls_zwm013-tipo_palete = i_lety1.

  READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_movmm_ablad
                   item       = 0.
  IF sy-subrc EQ 0.
    ls_zwm013-destino  = <fs_zwm001>-valor.
  ENDIF.

  INSERT zwm013 FROM ls_zwm013.
ENDFORM.                    " F_UPDATE_ZWM013
*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_ZWM020
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_update_zwm020.
  DATA ls_zwm020 TYPE zwm020.

  SELECT SINGLE *
    FROM zwm020 INTO ls_zwm020
    WHERE armazem EQ gs_reg-lgnum
      AND p1 EQ gs_reg-hukey1
      AND p2 EQ gs_reg-hukey2.
  IF sy-subrc EQ 0.
    RETURN.
  ENDIF.

  CLEAR ls_zwm020.

  ls_zwm020-armazem = gs_reg-lgnum.
  ls_zwm020-p1      = gs_reg-hukey1.
  ls_zwm020-p2      = gs_reg-hukey2.

  INSERT zwm020 FROM ls_zwm020.
ENDFORM.                    " F_UPDATE_ZWM020
*&---------------------------------------------------------------------*
*&      Form  f_do_goodsmvmnt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_TESTRUN  text
*      -->I_LETY1    text
*      <--C_SUBRC    text
*      <--C_MSG      text
*----------------------------------------------------------------------*
FORM f_do_goodsmvmnt USING i_testrun  TYPE testrun
                           i_lety1    TYPE mlgn-lety1
                     CHANGING c_subrc TYPE sysubrc
                              c_msg   TYPE bapi_msg.
  DATA lv_lgort TYPE lgort_d.
  DATA lv_bwart TYPE bwlvs.
  DATA lv_ablad TYPE ablad.
  DATA lv_table TYPE char14.                                "#EC NEEDED
  DATA lv_vemng TYPE vepo-vemng.                            "#EC NEEDED

  DATA lt_return TYPE tab_bdcmsgcoll.
  DATA lt_items  TYPE STANDARD TABLE OF zwm018.

  FIELD-SYMBOLS <fs_zwm001> LIKE LINE OF gt_zwm001[].
  FIELD-SYMBOLS <fs_item>   LIKE LINE OF lt_items[].
  FIELD-SYMBOLS <fs_return> LIKE LINE OF lt_return[].

  CLEAR: gv_ablad.

  APPEND INITIAL LINE TO lt_items[] ASSIGNING <fs_item>.
  <fs_item>-armazem     = gs_reg-lgnum.
  <fs_item>-material    = gs_reg-matnr.
  <fs_item>-uni         = gs_reg-cunit.
  <fs_item>-lote        = gs_reg-charg.

  IF i_lety1 = c_lety1_p2 OR i_lety1 = c_lety1_p5.
    <fs_item>-quantidade = gs_reg-menge * 2.
  ELSE.
    <fs_item>-quantidade = gs_reg-menge.
  ENDIF.

  READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_lgort_ba
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_lgort  = <fs_zwm001>-valor.
  ENDIF.

  READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_movmm_ablad
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_ablad  = <fs_zwm001>-valor+4(10).
  ENDIF.

  lv_bwart  = gs_reg-bwart.
  lv_vemng  = gs_reg-menge.

  CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
    EXPORTING
      lgnum            = gs_reg-lgnum
      aufnr            = gs_reg-aufnr
      code             = gs_reg-code
      mov_mm           = lv_bwart
      porta            = lv_ablad
      testrun          = i_testrun
      plant_o          = gs_reg-werks
      sloc_o           = lv_lgort
    IMPORTING
      materialdocument = gs_reg-mblnr
      matdocumentyear  = gs_reg-mjahr
    TABLES
      return_msg       = lt_return[]
      items            = lt_items[]
    EXCEPTIONS
      error            = 1
      OTHERS           = 2.
  c_subrc = sy-subrc.

  IF lt_return[] IS NOT INITIAL.
    READ TABLE lt_return[] ASSIGNING <fs_return> INDEX 1.
    IF sy-subrc EQ 0.
      MESSAGE ID <fs_return>-msgid TYPE <fs_return>-msgtyp NUMBER <fs_return>-msgnr
        WITH <fs_return>-msgv1 <fs_return>-msgv2 <fs_return>-msgv3 <fs_return>-msgv4
        INTO c_msg.
    ENDIF.
  ENDIF.

  gv_ablad = lv_ablad.
ENDFORM.                    " F_DO_GOODSMVMNT
*&---------------------------------------------------------------------*
*&      Form  F_EAN128_ENCODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_HUKEY       text
*      <--C_SUBRC       text
*      <--C_EAN128_CBA  text
*      <--C_EAN128_CBB  text
*      <--C_EAN128_CBC  text
*      <--C_EAN128_TXA  text
*      <--C_EAN128_TXB  text
*      <--C_EAN128_TXC  text
*----------------------------------------------------------------------*
FORM f_ean128_encode USING i_hukey  TYPE bapihukey-hu_exid
                     CHANGING c_subrc  TYPE sysubrc
                              c_ean128_cba TYPE zwm_aux-out_cb_tx
                              c_ean128_cbb TYPE zwm_aux-out_cb_tx
                              c_ean128_cbc TYPE zwm_aux-out_cb_tx
                              c_ean128_txa TYPE zwm_aux-out_cb_tx
                              c_ean128_txb TYPE zwm_aux-out_cb_tx
                              c_ean128_txc TYPE zwm_aux-out_cb_tx.
  DATA lv_profile     TYPE ean128_profile.
  DATA lv_devicetype  TYPE t313k-devicetype.

  DATA lt_eanbc       TYPE barcode_plus_errors_t.
  DATA lt_eanrd       TYPE barcode_txt_t.

  DATA ls_ean128  TYPE ean128.

  FIELD-SYMBOLS <fs_eanbc> LIKE LINE OF lt_eanbc[].
  FIELD-SYMBOLS <fs_eanrd> LIKE LINE OF lt_eanrd[].

  CLEAR c_ean128_cba.
  CLEAR c_ean128_cbb.
  CLEAR c_ean128_cbc.
  CLEAR c_ean128_txa.
  CLEAR c_ean128_txb.
  CLEAR c_ean128_txc.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_hukey
    IMPORTING
      output = ls_ean128-exidv.

  SELECT SINGLE ean11
    FROM mara INTO ls_ean128-matean
    WHERE matnr EQ gs_reg-matnr.

  ls_ean128-lbltype = 'S'.

  ls_ean128-vhilm = gs_reg-pack.
  ls_ean128-vfdat = gs_reg-vfdat.
  ls_ean128-maktx = gs_reg-maktx.
  ls_ean128-matnr = gs_reg-matnr.
  ls_ean128-charg = gs_reg-charg.
  ls_ean128-lfimg = gs_reg-menge.
  ls_ean128-vrkme = gs_reg-cunit.
  ls_ean128-vemng = gs_reg-menge.
  ls_ean128-vemeh = gs_reg-cunit.

  lv_profile    = '000'.
  lv_devicetype = 'ZLB_ZEB'.

  CALL FUNCTION 'LE_EAN128_ENCODE'
    EXPORTING
      if_encode_profile         = lv_profile
      if_skip_empty_fields      = abap_true
      is_ean128_data            = ls_ean128
      if_devicetype             = lv_devicetype
    IMPORTING
      et_barcode                = lt_eanbc[]
      et_barcode_txt            = lt_eanrd[]
    EXCEPTIONS
      no_barcode_definition     = 1
      error_in_subfunctions     = 2
      profile_not_for_ean128    = 3
      profile_unknown           = 4
      data_error                = 5
      no_ai_relation            = 6
      no_ai_information         = 7
      profile_error             = 8
      general_customizing_error = 9
      OTHERS                    = 10.
  IF sy-subrc NE 0.
    c_subrc = sy-subrc.
    RETURN.
  ENDIF.

  LOOP AT lt_eanbc[] ASSIGNING <fs_eanbc>.
    CASE sy-tabix.
      WHEN 1.
        c_ean128_cba = <fs_eanbc>-barcode.
      WHEN 2.
        c_ean128_cbb = <fs_eanbc>-barcode.
      WHEN 3.
        c_ean128_cbc = <fs_eanbc>-barcode.
      WHEN OTHERS.
        EXIT.
    ENDCASE.
  ENDLOOP.

  LOOP AT lt_eanrd[] ASSIGNING <fs_eanrd>.
    CASE sy-tabix.
      WHEN 1.
        c_ean128_txa = <fs_eanrd>.
      WHEN 2.
        c_ean128_txb = <fs_eanrd>.
      WHEN 3.
        c_ean128_txc = <fs_eanrd>.
      WHEN OTHERS.
        EXIT.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " F_EAN128_ENCODE
*&---------------------------------------------------------------------*
*&      Form  F_USER_COMMAND_EXIT_0010
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_user_command_exit_0010.

  DELETE FROM zwm011
         WHERE armazem     = gs_zwm011-armazem AND
               to_number   = gs_zwm011-to_number AND
               to_item     = gs_zwm011-to_item AND
               user_name   = gs_zwm011-user_name AND
               equipamento = gs_zwm011-equipamento AND
               status      = 'C'.
  COMMIT WORK.


  LEAVE TO SCREEN 0.
ENDFORM.                    " F_USER_COMMAND_EXIT_0010
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_BATCH_V2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_REG_MATNR  text
*      -->P_GS_REG_LGNUM  text
*      -->P_GS_REG_CHARG  text
*      -->P_GS_REG_WERKS  text
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM f_check_batch_v2 USING i_matnr  TYPE mara-matnr
                            i_lgnum  TYPE mlgn-lgnum
                            i_charg  TYPE mchb-charg
                            i_werks  TYPE werks_d
                   CHANGING c_subrc TYPE sysubrc.

  DATA lt_return TYPE tab_bdcmsgcoll.

  CALL FUNCTION 'ZWM_BATCH_CREATE'
    EXPORTING
      armazem           = i_lgnum
      material          = i_matnr
      lote              = i_charg
      centro            = i_werks
    TABLES
      return_msg        = lt_return[]
    EXCEPTIONS
      erro              = 1
      batch_not_created = 2
      OTHERS            = 3.
  IF sy-subrc NE 0.
    c_subrc = 96.
  ENDIF.

  FREE lt_return[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DO_GOODSMVMNT_V2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ABAP_TRUE  text
*      -->P_LV_LETY1  text
*      <--P_LV_SUBRC  text
*      <--P_R_RETURN_MSG  text
*----------------------------------------------------------------------*
FORM f_do_goodsmvmnt_v2  USING i_testrun  TYPE testrun
                               i_lety1    TYPE mlgn-lety1
                         CHANGING c_subrc TYPE sysubrc
                                  c_msg   TYPE bapi_msg.
  DATA lv_lgort TYPE lgort_d.
  DATA lv_bwart TYPE bwlvs.
  DATA lv_ablad TYPE ablad.
  DATA lv_table TYPE char14.                                "#EC NEEDED
  DATA lv_vemng TYPE vepo-vemng.                            "#EC NEEDED

  DATA lt_return TYPE tab_bdcmsgcoll.
  DATA lt_items  TYPE STANDARD TABLE OF zwm018.

  FIELD-SYMBOLS <fs_zwm001> LIKE LINE OF gt_zwm001[].
  FIELD-SYMBOLS <fs_item>   LIKE LINE OF lt_items[].
  FIELD-SYMBOLS <fs_return> LIKE LINE OF lt_return[].

  CLEAR: gv_ablad.

  APPEND INITIAL LINE TO lt_items[] ASSIGNING <fs_item>.
  <fs_item>-armazem     = gs_reg-lgnum.
  <fs_item>-material    = gs_reg-matnr.
  <fs_item>-uni         = gs_reg-cunit.
  <fs_item>-lote        = gs_reg-charg.

  IF i_lety1 = c_lety1_p2 OR i_lety1 = c_lety1_p5.
    <fs_item>-quantidade = gs_reg-menge * 2.
  ELSE.
    <fs_item>-quantidade = gs_reg-menge.
  ENDIF.

  READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_lgort_ba
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_lgort  = <fs_zwm001>-valor.
  ENDIF.

  READ TABLE gt_zwm001[] ASSIGNING <fs_zwm001>
    WITH TABLE KEY processo   = c_process_general
                   parametro  = c_param_movmm_ablad
                   item       = 0.
  IF sy-subrc EQ 0.
    lv_ablad  = <fs_zwm001>-valor+4(10).
  ENDIF.

  lv_bwart  = gs_reg-bwart.
  lv_vemng  = gs_reg-menge.

  CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
    EXPORTING
      lgnum            = gs_reg-lgnum
      aufnr            = gs_reg-aufnr
      code             = gs_reg-code
      mov_mm           = lv_bwart
      porta            = lv_ablad
      testrun          = i_testrun
      plant_o          = gs_reg-werks
      plant_d          = gs_reg-werks
      sloc_o           = lv_lgort
    IMPORTING
      materialdocument = gs_reg-mblnr
      matdocumentyear  = gs_reg-mjahr
    TABLES
      return_msg       = lt_return[]
      items            = lt_items[]
    EXCEPTIONS
      error            = 1
      OTHERS           = 2.
  c_subrc = sy-subrc.

  IF lt_return[] IS NOT INITIAL.
    READ TABLE lt_return[] ASSIGNING <fs_return> INDEX 1.
    IF sy-subrc EQ 0.
      MESSAGE ID <fs_return>-msgid TYPE <fs_return>-msgtyp NUMBER <fs_return>-msgnr
        WITH <fs_return>-msgv1 <fs_return>-msgv2 <fs_return>-msgv3 <fs_return>-msgv4
        INTO c_msg.
    ENDIF.
  ENDIF.

  gv_ablad = lv_ablad.

ENDFORM.
