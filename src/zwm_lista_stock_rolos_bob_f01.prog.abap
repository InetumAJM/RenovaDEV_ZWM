*&---------------------------------------------------------------------*
*&  Include           ZWM_LISTA_STOCK_ROLOS_BOB_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  CONSTANTS: lc_divisao_difa TYPE z02divisao VALUE 'DIFA',
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Inicio
             lc_divisao_dita TYPE z02divisao VALUE 'DITA',
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Fim
* INETUM - NR - 15.05.2024 - RENPRJ00037 - Inicio
             lc_divisao_agv  TYPE z02divisao VALUE 'AGV'.
* INETUM - NR - 15.05.2024 - RENPRJ00037 - Fim

  TYPES: BEGIN OF ty_kunnr,
           kunnr TYPE kna1-kunnr,
         END OF ty_kunnr.

  DATA: lt_kunnr_aux          TYPE STANDARD TABLE OF ty_kunnr,
* INETUM - NR - 02.05.2024 - RENPRJ00037 - Inicio
        lt_z02rpconsprodl_aux TYPE STANDARD TABLE OF z02rpconsprodl.
* INETUM - NR - 02.05.2024 - RENPRJ00037 - Fim

  DATA: ls_kunnr_aux          TYPE ty_kunnr,
        ls_z02rpprolos        TYPE z02rpprolos,
        ls_z02rppbobinak      TYPE z02rppbobinak,
* INETUM - NR - 02.05.2024 - RENPRJ00037 - Inicio
        ls_z02rpconsprodl_aux TYPE z02rpconsprodl,
        ls_lqua               TYPE ty_lqua.
* INETUM - NR - 02.05.2024 - RENPRJ00037 - Fim

* INETUM - NR - 02.05.2024 - RENPRJ00037 - Inicio
  RANGES: lr_lenum FOR lqua-lenum.
* INETUM - NR - 02.05.2024 - RENPRJ00037 - Fim

  SELECT lgnum lqnum matnr werks charg lgpla
         gesme lenum
    FROM lqua
    INTO TABLE gt_lqua
    WHERE lgnum IN so_lgnum
      AND matnr IN so_matnr
      AND werks IN so_werks
      AND charg IN so_charg
      AND lgpla IN so_lgpla
      AND lenum IN so_lenum.

  IF gt_lqua[] IS NOT INITIAL.
    SELECT matnr mtart bismt breit prdha
      FROM mara
      INTO TABLE gt_mara
      FOR ALL ENTRIES IN gt_lqua
      WHERE matnr = gt_lqua-matnr
        AND mtart IN so_mtart
        AND bismt IN so_bismt
        AND prdha IN so_prdha.

    IF gt_mara[] IS NOT INITIAL.
      SELECT *
        FROM makt
        INTO TABLE gt_makt
        FOR ALL ENTRIES IN gt_mara
        WHERE matnr = gt_mara-matnr
          AND spras = sy-langu.
    ENDIF.

    SELECT *
      FROM z02rpprolos
      INTO TABLE gt_z02rpprolos
      FOR ALL ENTRIES IN gt_lqua
      WHERE divisao = lc_divisao_difa
        AND lenum = gt_lqua-lenum
        AND anulado = space
        AND estornado = space.

    LOOP AT gt_z02rpprolos INTO ls_z02rpprolos.
      ls_kunnr_aux-kunnr = ls_z02rpprolos-kunnr.
      APPEND ls_kunnr_aux TO lt_kunnr_aux.

      CLEAR: ls_z02rpprolos, ls_kunnr_aux.
    ENDLOOP.

    SELECT *
      FROM z02rppbobinap
      INTO TABLE gt_z02rppbobinap
      FOR ALL ENTRIES IN gt_lqua
       WHERE matnr = gt_lqua-matnr
         AND lenum = gt_lqua-lenum
         AND anulado = space
         AND estornado = space.

    IF gt_z02rppbobinap[] IS NOT INITIAL.
      SELECT *
        FROM z02rppbobinak
        INTO TABLE gt_z02rppbobinak
        FOR ALL ENTRIES IN gt_z02rppbobinap
        WHERE divisao = gt_z02rppbobinap-divisao
          AND pbobine = gt_z02rppbobinap-pbobine
          AND aufnr = gt_z02rppbobinap-aufnr
          AND anulado = space
          AND estornado = space.

      LOOP AT gt_z02rppbobinak INTO ls_z02rppbobinak.
        ls_kunnr_aux-kunnr = ls_z02rppbobinak-kunnr.
        APPEND ls_kunnr_aux TO lt_kunnr_aux.

        CLEAR: ls_z02rppbobinak, ls_kunnr_aux.
      ENDLOOP.

    ENDIF.

    SORT lt_kunnr_aux BY kunnr.
    DELETE ADJACENT DUPLICATES FROM lt_kunnr_aux COMPARING kunnr.

    IF lt_kunnr_aux[] IS NOT INITIAL.
      SELECT kunnr name1
        FROM kna1
        INTO TABLE gt_kna1
        FOR ALL ENTRIES IN lt_kunnr_aux
        WHERE kunnr = lt_kunnr_aux-kunnr.
    ENDIF.

    SELECT matnr cor
      FROM z02rpmateriais
      INTO TABLE gt_z02rpmateriais
      FOR ALL ENTRIES IN gt_lqua
      WHERE matnr = gt_lqua-matnr
        AND cor IN so_cor.

* INETUM - NR - 21.11.2022 - RENPRJ00037 - Inicio
* INETUM - NR - 02.05.2024 - RENPRJ00037 - Inicio
***    SELECT *
***      FROM z02rpconsprodl
***      INTO TABLE gt_z02rpconsprodl
***      FOR ALL ENTRIES IN gt_lqua
***      WHERE divisao = lc_divisao_dita
***        AND lenum = gt_lqua-lenum
***        AND anulado = space
***        AND estornado = space.
***
***    IF gt_z02rpconsprodl[] IS NOT INITIAL.
***
***      SELECT *
***        FROM z02rpconsprodh
***        INTO TABLE gt_z02rpconsprodh
***        FOR ALL ENTRIES IN gt_z02rpconsprodl
***        WHERE divisao = gt_z02rpconsprodl-divisao
***          AND lenum = gt_z02rpconsprodl-lenum
***          AND flgcp <> space.
***
***      SORT gt_z02rpconsprodh BY consprod DESCENDING.
***
***    ENDIF.

    lr_lenum-sign = 'I'.
    lr_lenum-option = 'EQ'.

    LOOP AT gt_lqua INTO ls_lqua.

      lr_lenum-low = ls_lqua-lenum.

      APPEND lr_lenum.

      CLEAR: ls_lqua.
    ENDLOOP.

    SELECT *
      FROM z02rpconsprodh
      INTO TABLE gt_z02rpconsprodh
      WHERE divisao = lc_divisao_dita
        AND lenum IN lr_lenum
        AND flgcp <> space.

    IF gt_z02rpconsprodh[] IS NOT INITIAL.

      SELECT *
        FROM z02rpconsprodl
        INTO TABLE gt_z02rpconsprodl
        FOR ALL ENTRIES IN gt_z02rpconsprodh
        WHERE divisao = gt_z02rpconsprodh-divisao
          AND consprod = gt_z02rpconsprodh-consprod
          AND lenum = gt_z02rpconsprodh-lenum.

      lt_z02rpconsprodl_aux[] = gt_z02rpconsprodl[].

      DELETE lt_z02rpconsprodl_aux WHERE anulado IS INITIAL AND estornado IS INITIAL.

      DELETE gt_z02rpconsprodl WHERE anulado IS NOT INITIAL OR estornado IS NOT INITIAL.

      LOOP AT lt_z02rpconsprodl_aux INTO ls_z02rpconsprodl_aux.
        "Eliminar entradas anuladas/estornadas
        DELETE gt_z02rpconsprodh WHERE divisao = ls_z02rpconsprodl_aux-divisao AND
                                      consprod = ls_z02rpconsprodl_aux-consprod AND
                                         lenum = ls_z02rpconsprodl_aux-lenum.


        CLEAR: ls_z02rpconsprodl_aux.
      ENDLOOP.

      SORT gt_z02rpconsprodh BY consprod DESCENDING.

    ENDIF.
* INETUM - NR - 02.05.2024 - RENPRJ00037 - Fim
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Fim

* INETUM - NR - 15.05.2024 - RENPRJ00037 - Inicio
    SELECT *
      FROM z02rpebobinak
      INTO TABLE gt_z02rpebobinak
      FOR ALL ENTRIES IN gt_lqua
      WHERE divisao = lc_divisao_agv
        AND lenum = gt_lqua-lenum
        AND ebeln = gt_lqua-charg.
* INETUM - NR - 15.05.2024 - RENPRJ00037 - Fim

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TREAT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM treat_data .

  DATA: ls_lqua           TYPE ty_lqua,
        ls_mara           TYPE ty_mara,
        ls_makt           TYPE makt,
        ls_kna1           TYPE ty_kna1,
        ls_z02rpprolos    TYPE z02rpprolos,
        ls_z02rppbobinap  TYPE z02rppbobinap,
        ls_z02rppbobinak  TYPE z02rppbobinak,
* INETUM - NR - 15.05.2024 - RENPRJ00037 - Inicio
        ls_z02rpebobinak  TYPE z02rpebobinak,
* INETUM - NR - 15.05.2024 - RENPRJ00037 - Fim
        ls_z02rpmateriais TYPE ty_z02rpmateriais,
        ls_alv            TYPE ty_alv.

* INETUM - NR - 15.05.2024 - RENPRJ00037 - Inicio
  CONSTANTS: lc_divisao_agv TYPE z02divisao VALUE 'AGV'.
* INETUM - NR - 15.05.2024 - RENPRJ00037 - Fim

  LOOP AT gt_lqua INTO ls_lqua.

    MOVE-CORRESPONDING ls_lqua TO ls_alv.

    ADD 1 TO ls_alv-numero_total.

    ls_alv-valor_medio = ls_lqua-gesme.

    CLEAR: ls_mara.
    READ TABLE gt_mara INTO ls_mara WITH KEY matnr = ls_lqua-matnr.
    IF sy-subrc EQ 0.
      ls_alv-mtart = ls_mara-mtart.
      ls_alv-prdha = ls_mara-prdha.
      ls_alv-bismt = ls_mara-bismt.
      ls_alv-largura = ls_mara-breit.
    ENDIF.

    CLEAR: ls_makt.
    READ TABLE gt_makt INTO ls_makt WITH KEY matnr = ls_lqua-matnr.
    IF sy-subrc EQ 0.
      ls_alv-maktx = ls_makt-maktx.
    ENDIF.


    "Rolo ou Bobine
* INETUM - NR - 22.04.2024 - RENPRJ00037 - Inicio
    SORT gt_z02rpprolos BY prolo DESCENDING.
* INETUM - NR - 22.04.2024 - RENPRJ00037 - Fim
    CLEAR: ls_z02rpprolos.
    READ TABLE gt_z02rpprolos INTO ls_z02rpprolos WITH KEY lenum = ls_lqua-lenum.
    IF sy-subrc EQ 0.
      "Rolo
      ls_alv-kunnr = ls_z02rpprolos-kunnr.
      ls_alv-vbeln = ls_z02rpprolos-vbeln.
      ls_alv-refcli = ls_z02rpprolos-refcli.
      ls_alv-norma = ls_z02rpprolos-norma.
      ls_alv-dimint = ls_z02rpprolos-dimint.
      ls_alv-dimext = ls_z02rpprolos-dimext.
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Inicio
      "Recalcular o valor do campo Diâmetro externo
      IF ls_z02rpprolos-densidade IS NOT INITIAL.
        PERFORM recalc_dimext_ud USING ls_lqua-lenum
                              CHANGING ls_alv-dimext.
      ENDIF.
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Fim
***      ls_alv-largura = ls_z02rpprolos-largura.
      ls_alv-quebra = ls_z02rpprolos-quebra.
      ls_alv-veloce = ls_z02rpprolos-veloce.
      ls_alv-rol_bob_seq = ls_z02rpprolos-rolosq.
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Inicio
      ls_alv-densidade = ls_z02rpprolos-densidade.
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Fim

      IF ls_z02rpprolos-kunnr IS NOT INITIAL.
        CLEAR: ls_kna1.
        READ TABLE gt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_z02rpprolos-kunnr.
        IF sy-subrc EQ 0.
          ls_alv-name1 = ls_kna1-name1.
        ENDIF.
      ENDIF.

      PERFORM get_packing_desc USING ls_z02rpprolos-tipo_embalag
                            CHANGING ls_alv-embalagem.

    ELSE.
      "Bobine
      CLEAR: ls_z02rppbobinap.
      READ TABLE gt_z02rppbobinap INTO ls_z02rppbobinap WITH KEY lenum = ls_lqua-lenum.
      IF sy-subrc EQ 0.
        ls_alv-quebra = ls_z02rppbobinap-quebra.
        ls_alv-veloce = ls_z02rppbobinap-veloce.
        ls_alv-rol_bob_seq = ls_z02rppbobinap-bobinesq.
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Inicio
        ls_alv-densidade = ls_z02rppbobinap-densidade.
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Fim

        CLEAR ls_z02rppbobinak.
        READ TABLE gt_z02rppbobinak INTO ls_z02rppbobinak WITH KEY divisao = ls_z02rppbobinap-divisao
                                                                   pbobine = ls_z02rppbobinap-pbobine
                                                                     aufnr = ls_z02rppbobinap-aufnr.
        IF sy-subrc EQ 0.
          ls_alv-kunnr = ls_z02rppbobinak-kunnr.
          ls_alv-vbeln = ls_z02rppbobinak-vbeln.
          ls_alv-refcli = ls_z02rppbobinak-refcli.
          ls_alv-norma = ls_z02rppbobinak-norma.
          ls_alv-dimint = ls_z02rppbobinak-dimint.
          ls_alv-dimext = ls_z02rppbobinak-dimext.
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Inicio
          "Recalcular o valor do campo Diâmetro externo
          IF ls_z02rppbobinap-densidade IS NOT INITIAL.
            PERFORM recalc_dimext_ud USING ls_lqua-lenum
                                  CHANGING ls_alv-dimext.
          ENDIF.
* INETUM - NR - 21.11.2022 - RENPRJ00037 - Fim
          ls_alv-largura = ls_z02rppbobinak-breit.

          IF ls_z02rppbobinak-kunnr IS NOT INITIAL.
            CLEAR: ls_kna1.
            READ TABLE gt_kna1 INTO ls_kna1 WITH KEY kunnr = ls_z02rppbobinak-kunnr.
            IF sy-subrc EQ 0.
              ls_alv-name1 = ls_kna1-name1.
            ENDIF.
          ENDIF.

          PERFORM get_packing_desc USING ls_z02rppbobinak-tipo_embalag
                                CHANGING ls_alv-embalagem.

        ENDIF.

* INETUM - NR - 15.05.2024 - RENPRJ00037 - Inicio
      ELSE.
        "Bobinas compradas fora (Entrada de Bobine através da transação Z02RP13)

        CLEAR ls_z02rpebobinak.
        READ TABLE gt_z02rpebobinak INTO ls_z02rpebobinak WITH KEY divisao = lc_divisao_agv
                                                                     lenum = ls_lqua-lenum
                                                                     ebeln = ls_lqua-charg.
        IF sy-subrc EQ 0.
          ls_alv-dimint = ls_z02rpebobinak-dimint.
          ls_alv-dimext = ls_z02rpebobinak-dimext.
          ls_alv-largura = ls_z02rpebobinak-breit.
        ENDIF.
* INETUM - NR - 15.05.2024 - RENPRJ00037 - Fim

      ENDIF.
    ENDIF.

    IF ls_alv-embalagem IS INITIAL.
      ls_alv-embalagem = text-012.
    ENDIF.

    CLEAR: ls_z02rpmateriais.
    READ TABLE gt_z02rpmateriais INTO ls_z02rpmateriais WITH KEY matnr = ls_lqua-matnr.
    IF sy-subrc EQ 0.
      ls_alv-cor = ls_z02rpmateriais-cor.
    ENDIF.

    APPEND ls_alv TO gt_alv.

    CLEAR: ls_lqua, ls_alv.
  ENDLOOP.

  DELETE gt_alv WHERE mtart NOT IN so_mtart.
  DELETE gt_alv WHERE prdha NOT IN so_prdha.
  DELETE gt_alv WHERE kunnr NOT IN so_kunnr.
  DELETE gt_alv WHERE cor NOT IN so_cor.
  DELETE gt_alv WHERE bismt NOT IN so_bismt.

  IF gt_alv[] IS NOT INITIAL.
    PERFORM get_additional_number.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .

  DATA: lo_salv_msg TYPE REF TO cx_salv_msg,
        lv_text     TYPE string.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = go_salv
        CHANGING
          t_table      = gt_alv[].
    CATCH cx_salv_msg INTO lo_salv_msg .
      lv_text = lo_salv_msg->get_text( ).
      MESSAGE lv_text TYPE 'I'.
  ENDTRY.

  PERFORM set_layout.
  PERFORM set_toolbar.
  PERFORM set_columns.
  PERFORM set_aggregations.
  PERFORM set_sorts.

  go_salv->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_toolbar .

  DATA: lo_functions TYPE REF TO cl_salv_functions.

  lo_functions = go_salv->get_functions( ).
  lo_functions->set_all( abap_true ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_COLUMNS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_columns .

  DATA: lo_columns TYPE REF TO cl_salv_columns_table,
        lo_column  TYPE REF TO cl_salv_column_table.

  lo_columns = go_salv->get_columns( ).
  lo_columns->set_optimize( abap_true ).

  TRY.
      lo_column ?= lo_columns->get_column( 'GESME' ).
      lo_column->set_long_text(   text-002 ).
      lo_column->set_medium_text( text-002 ).
      lo_column->set_short_text(  '' ).
    CATCH cx_salv_not_found .
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'NUMERO_TOTAL' ).
      lo_column->set_long_text(   text-003 ).
      lo_column->set_medium_text( text-003 ).
      lo_column->set_short_text(  text-003 ).
    CATCH cx_salv_not_found .
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VALOR_MEDIO' ).
      lo_column->set_long_text(   text-005 ).
      lo_column->set_medium_text( text-005 ).
      lo_column->set_short_text(  '').
    CATCH cx_salv_not_found .
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'ROL_BOB_SEQ' ).
      lo_column->set_long_text(   text-007 ).
      lo_column->set_medium_text( text-007 ).
      lo_column->set_short_text(  '' ).
    CATCH cx_salv_not_found .
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VBELN' ).
      lo_column->set_long_text(   text-009 ).
      lo_column->set_medium_text( text-009 ).
      lo_column->set_short_text(  '' ).
    CATCH cx_salv_not_found .
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'EMBALAGEM' ).
      lo_column->set_long_text(   text-013 ).
      lo_column->set_medium_text( text-013 ).
      lo_column->set_short_text(  text-013 ).
    CATCH cx_salv_not_found .
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'LARGURA' ).
      lo_column->set_long_text(   text-014 ).
      lo_column->set_medium_text( text-014 ).
      lo_column->set_short_text(  text-014 ).
    CATCH cx_salv_not_found .
  ENDTRY.


  TRY.
      lo_column ?= lo_columns->get_column( 'COR' ).
      lo_column->set_long_text(   text-015 ).
      lo_column->set_medium_text( text-015 ).
      lo_column->set_short_text(  text-015 ).
    CATCH cx_salv_not_found .
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_AGGREGATIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_aggregations .

  DATA: lo_aggregations TYPE REF TO cl_salv_aggregations.

*/--------------------------- Add Totals  ----------------------*
*// 1.Get Aggregation object of the ALV
  CALL METHOD go_salv->get_aggregations
    RECEIVING
      value = lo_aggregations.

*// 2.Specify tht column name for totals
  TRY.
      CALL METHOD lo_aggregations->add_aggregation
        EXPORTING
          columnname  = 'GESME'
          aggregation = if_salv_c_aggregation=>total.
    CATCH cx_salv_data_error .
    CATCH cx_salv_not_found .
    CATCH cx_salv_existing .
  ENDTRY.

  TRY.
      CALL METHOD lo_aggregations->add_aggregation
        EXPORTING
          columnname  = 'NUMERO_TOTAL'
          aggregation = if_salv_c_aggregation=>total.
    CATCH cx_salv_data_error .
    CATCH cx_salv_not_found .
    CATCH cx_salv_existing .
  ENDTRY.

  TRY.
      CALL METHOD lo_aggregations->add_aggregation
        EXPORTING
          columnname  = 'VALOR_MEDIO'
          aggregation = if_salv_c_aggregation=>average.
    CATCH cx_salv_data_error .
    CATCH cx_salv_not_found .
    CATCH cx_salv_existing .
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_SORTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_sorts .

  DATA: lo_sorts TYPE REF TO cl_salv_sorts.

*// Sorting
**// 1. Get Sorting Object of the ALV
  CALL METHOD go_salv->get_sorts
    RECEIVING
      value = lo_sorts.

**// 2.Specify the column for sorting
  TRY.
      CALL METHOD lo_sorts->add_sort
        EXPORTING
          columnname = 'LGNUM'
*         position   =
          sequence   = if_salv_c_sort=>sort_up.
***          subtotal   = if_salv_c_bool_sap=>true.  "<<--Subtotals flag for the Airline column
*        group      = IF_SALV_C_SORT=>GROUP_NONE
*        obligatory = IF_SALV_C_BOOL_SAP=>FALSE.
    CATCH cx_salv_not_found .
    CATCH cx_salv_existing .
    CATCH cx_salv_data_error .
  ENDTRY.

  TRY.
      CALL METHOD lo_sorts->add_sort
        EXPORTING
          columnname = 'WERKS'
*         position   =
          sequence   = if_salv_c_sort=>sort_up.
***          subtotal   = if_salv_c_bool_sap=>true.  "<<--Subtotals flag for the Airline column
*        group      = IF_SALV_C_SORT=>GROUP_NONE
*        obligatory = IF_SALV_C_BOOL_SAP=>FALSE.
    CATCH cx_salv_not_found .
    CATCH cx_salv_existing .
    CATCH cx_salv_data_error .
  ENDTRY.

  TRY.
      CALL METHOD lo_sorts->add_sort
        EXPORTING
          columnname = 'LGPLA'
*         position   =
          sequence   = if_salv_c_sort=>sort_up.
***          subtotal   = if_salv_c_bool_sap=>true.  "<<--Subtotals flag for the Airline column
*        group      = IF_SALV_C_SORT=>GROUP_NONE
*        obligatory = IF_SALV_C_BOOL_SAP=>FALSE.
    CATCH cx_salv_not_found .
    CATCH cx_salv_existing .
    CATCH cx_salv_data_error .
  ENDTRY.

  TRY.
      CALL METHOD lo_sorts->add_sort
        EXPORTING
          columnname = 'MATNR'
*         position   =
          sequence   = if_salv_c_sort=>sort_up
          subtotal   = if_salv_c_bool_sap=>true.  "<<--Subtotals flag for the Airline column
*        group      = IF_SALV_C_SORT=>GROUP_NONE
*        obligatory = IF_SALV_C_BOOL_SAP=>FALSE.
    CATCH cx_salv_not_found .
    CATCH cx_salv_existing .
    CATCH cx_salv_data_error .
  ENDTRY.

  TRY.
      CALL METHOD lo_sorts->add_sort
        EXPORTING
          columnname = 'MAKTX'
*         position   =
          sequence   = if_salv_c_sort=>sort_up.
***          subtotal   = if_salv_c_bool_sap=>true.  "<<--Subtotals flag for the Airline column
*        group      = IF_SALV_C_SORT=>GROUP_NONE
*        obligatory = IF_SALV_C_BOOL_SAP=>FALSE.
    CATCH cx_salv_not_found .
    CATCH cx_salv_existing .
    CATCH cx_salv_data_error .
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_PACKING_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_Z02RPPROLOS_TIPO_EMBALAG  text
*      <--P_LS_ALV_EMBALAGEM  text
*----------------------------------------------------------------------*
FORM get_packing_desc  USING    puv_tipo_embalag TYPE z02rp_e_tipos_embalagem
                       CHANGING pcv_embalagem.

  CONSTANTS: lc_nao_embal TYPE z02rp_e_tipos_embalagem VALUE 'NAO_EMBAL'.

  IF puv_tipo_embalag IS INITIAL.
    pcv_embalagem = text-012.
  ELSE.
    IF puv_tipo_embalag EQ lc_nao_embal.
      pcv_embalagem = text-011.
    ELSE.
      pcv_embalagem = text-010.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_ADDITIONAL_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_additional_number .

  TYPES: BEGIN OF ty_ud_additional_number,
           lenum TYPE lqua-lenum,
           lznum TYPE ltak-lznum,
         END OF ty_ud_additional_number.

  DATA: lt_ltap                 TYPE STANDARD TABLE OF ltap,
        lt_ltap_aux             TYPE STANDARD TABLE OF ltap,
        lt_ltak                 TYPE STANDARD TABLE OF ltak,
        lt_ltak_aux             TYPE STANDARD TABLE OF ltak,
        lt_ud_additional_number TYPE STANDARD TABLE OF ty_ud_additional_number,
        lt_alv_aux              TYPE STANDARD TABLE OF ty_alv.

  DATA: ls_ltak_aux             TYPE ltak,
        ls_ud_additional_number TYPE ty_ud_additional_number.

  FIELD-SYMBOLS: <fs_alv> TYPE ty_alv.

  CONSTANTS: lc_vorga_st TYPE ltap-vorga VALUE 'ST',
             lc_vorga_sl TYPE ltap-vorga VALUE 'SL'.


  lt_alv_aux[] = gt_alv[].
  DELETE lt_alv_aux WHERE lenum IS INITIAL.

  IF lt_alv_aux[] IS NOT INITIAL.

    "Nº unidade de depósito de origem (Ud De --->)
    SELECT *
      FROM ltap
      INTO TABLE lt_ltap
      FOR ALL ENTRIES IN lt_alv_aux
      WHERE lgnum = lt_alv_aux-lgnum
        AND pquit = 'X'
        AND ( vorga NE lc_vorga_st AND vorga NE lc_vorga_sl )
        AND vlenr = lt_alv_aux-lenum.

    "Nº unidade de depósito de destino (Ud Para <---)
    SELECT *
      FROM ltap
      APPENDING TABLE lt_ltap
      FOR ALL ENTRIES IN lt_alv_aux
      WHERE lgnum = lt_alv_aux-lgnum
        AND pquit = 'X'
        AND ( vorga NE lc_vorga_st AND vorga NE lc_vorga_sl )
        AND nlenr = lt_alv_aux-lenum.

    SORT lt_ltap BY lgnum tanum tapos.
    DELETE ADJACENT DUPLICATES FROM lt_ltap
    COMPARING lgnum tanum tapos.

    IF lt_ltap[] IS NOT INITIAL.

      SELECT *
        FROM ltak
        INTO TABLE lt_ltak
        FOR ALL ENTRIES IN lt_ltap
        WHERE lgnum = lt_ltap-lgnum
          AND tanum = lt_ltap-tanum
          AND lznum <> ' '.

    ENDIF.

    IF lt_ltak[] IS NOT INITIAL.

      "Obter valor mais recente do campo numero adicional para cada UD
      LOOP AT gt_alv ASSIGNING <fs_alv>.

        lt_ltap_aux[] = lt_ltap[].
        DELETE lt_ltap_aux WHERE vlenr NE <fs_alv>-lenum AND nlenr NE <fs_alv>-lenum.

        lt_ltak_aux[] = lt_ltak[].

        LOOP AT lt_ltak_aux INTO ls_ltak_aux.

          READ TABLE lt_ltap_aux TRANSPORTING NO FIELDS WITH KEY lgnum = ls_ltak_aux-lgnum
                                                                 tanum = ls_ltak_aux-tanum.
          IF sy-subrc NE 0.
            DELETE lt_ltak_aux WHERE lgnum = ls_ltak_aux-lgnum AND tanum = ls_ltak_aux-tanum.
          ENDIF.

          CLEAR: ls_ltak_aux.
        ENDLOOP.

        IF lt_ltak_aux[] IS NOT INITIAL.

          SORT lt_ltak_aux BY bdatu DESCENDING bzeit DESCENDING.

          READ TABLE lt_ltak_aux INTO ls_ltak_aux INDEX 1.
          IF sy-subrc EQ 0.
            "Nº adicional
            <fs_alv>-lznum = ls_ltak_aux-lznum.
          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_layout .

  DATA: lo_layout  TYPE REF TO cl_salv_layout.

  DATA: ls_key TYPE salv_s_layout_key.

  ls_key-report = sy-repid.
  lo_layout = go_salv->get_layout( ).
  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).     "Gravar variantes de layout
  lo_layout->set_default( abap_true ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  RECALC_DIMEXT_UD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_LQUA_LENUM  text
*      <--P_LS_LS_ALV_DIMEXT  text
*----------------------------------------------------------------------*
FORM recalc_dimext_ud  USING    puv_lenum TYPE lqua-lenum
                       CHANGING pcv_dimext TYPE z02rpprolos-dimext.

  "Recalcular o valor do campo Diâmetro externo tendo por base o valor do campo Raio inserido na tle dos consumos da máscara DITA
  "Se a camp dnsidade estiver preenchido quer dizer que o calculo do campo resto da tela dos consumos da máscara DITA,
  "foi calculado automaticamente tendo por base o valor do raio da UD indicado pelos operadores

  "Fórmula de cálculo do diâmetro final:

  " df = 20 * r

  "Onde:  df – diâmetro final (mm)
  "        r – raio atual (cm)


  DATA: ls_z02rpconsprodh TYPE z02rpconsprodh.

  READ TABLE gt_z02rpconsprodh INTO ls_z02rpconsprodh WITH KEY lenum = puv_lenum.
  IF sy-subrc EQ 0.
    pcv_dimext = 20 * ls_z02rpconsprodh-raio.
  ENDIF.

ENDFORM.
