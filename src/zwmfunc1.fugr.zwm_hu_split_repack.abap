FUNCTION zwm_hu_split_repack.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(REMESSA) TYPE  VBELN_VL
*"  TABLES
*"      IT_LTAP STRUCTURE  ZWM_LTAP
*"      IT_SSCC_PICK STRUCTURE  ZWM_LTAP_PICK
*"  EXCEPTIONS
*"      NO_DATA
*"      DELIVERY_NO_GOOD
*"----------------------------------------------------------------------

  DATA: lt_vekp LIKE vekp OCCURS 0 WITH HEADER LINE.
  DATA: lt_vepo LIKE vepo OCCURS 0 WITH HEADER LINE.
  DATA: return  LIKE bapiret2 OCCURS 0  WITH HEADER LINE.
  DATA: lv_new_qtd LIKE vepo-vemng.
  DATA: lv_qtd_aux LIKE ekpo-menge.

  DATA: itemsproposal LIKE bapihuitmproposal OCCURS 0 WITH HEADER LINE.
  DATA: pack_itemproposal TYPE bapihuitmproposal.
  DATA: headerproposal LIKE bapihuhdrproposal.
  DATA: lt_zwm046 LIKE zwm046 OCCURS 0 WITH HEADER LINE.
  DATA: lv_kunnr LIKE kna1-kunnr.
  DATA: lv_line  TYPE i.
  DATA: itemunpack     TYPE bapihuitmunpack .
  DATA: zwm001 TYPE zwm001.
  DATA: pack_mat TYPE vhilm.

  DATA: BEGIN OF lt_hu OCCURS 0,
          exidv TYPE exidv,
        END OF lt_hu.

  DATA: lt_sscc_pick TYPE zwm026 OCCURS 0 WITH HEADER LINE.

  DATA: exidv1 TYPE exidv,
        exidv2 TYPE exidv,
        exidv3 TYPE exidv,
        exidv4 TYPE exidv,
        exidv5 TYPE exidv,
        exidv6 TYPE exidv,
        exidv7 TYPE exidv,
        exidv8 TYPE exidv.

  DATA: ndiv TYPE i.
  DATA: mlgn TYPE mlgn.

  DATA: lv_2step TYPE flag,
        ls_t311  TYPE t311.

*** verifica se remessa em questão deve fazer o processamento
  SELECT SINGLE kunnr INTO lv_kunnr
  FROM vbpa
  WHERE vbeln =  remessa
  AND posnr = 0
  AND parvw = 'W1'.
  IF sy-subrc <> 0.
    RAISE delivery_no_good.
  ENDIF.

  IF it_ltap[] IS INITIAL.
    RAISE no_data.
  ENDIF.

  SELECT *  INTO TABLE lt_vekp FROM vekp
  FOR ALL ENTRIES IN it_ltap WHERE
  exidv = it_ltap-sscc.

  IF sy-subrc = 0.
    SELECT *  INTO TABLE lt_vepo FROM vepo
    FOR ALL ENTRIES IN lt_vekp WHERE
    venum = lt_vekp-venum.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 18.06.2012 17:43:59
*  Motivo: Picking em 2 Passos
*--------------------------------------------------------------------*
    CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum = lt_vekp-lgnum
        i_vbeln = remessa
      IMPORTING
        e_2step = lv_2step
        es_t311 = ls_t311
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.

    IF lv_2step EQ abap_true.
      SELECT * INTO TABLE lt_sscc_pick
        FROM zwm026
        FOR ALL ENTRIES IN lt_vekp
        WHERE armazem = lt_vekp-lgnum AND
              grupo   = ls_t311-refnr AND
              sscc    = lt_vekp-exidv.
    ELSE.
      SELECT * INTO TABLE lt_sscc_pick
        FROM zwm026
        FOR ALL ENTRIES IN lt_vekp
        WHERE armazem = lt_vekp-lgnum AND
              remessa = remessa       AND
              sscc    = lt_vekp-exidv.
    ENDIF.

    SORT lt_sscc_pick BY sscc.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
  ENDIF.

  IF lt_vepo[] IS INITIAL.
    RAISE no_data.
  ENDIF.

  SELECT * INTO TABLE lt_zwm046
  FROM zwm046
  FOR ALL ENTRIES IN lt_vepo
  WHERE kunnr = lv_kunnr AND
        matnr = lt_vepo-matnr.

  IF sy-subrc <> 0.
    SELECT * INTO TABLE lt_zwm046
    FROM zwm046
    FOR ALL ENTRIES IN lt_vepo
    WHERE kunnr = '' AND
          matnr = lt_vepo-matnr.
  ENDIF.


  LOOP AT it_ltap.

** REFRESH
    CALL FUNCTION 'HU_PACKING_REFRESH'.

    READ TABLE lt_vekp WITH KEY exidv = it_ltap-sscc.
    CHECK sy-subrc = 0.

**  Unpack da HU
    LOOP AT lt_vepo WHERE venum = lt_vekp-venum.
*    READ TABLE lt_vepo WITH KEY venum = lt_vekp-venum.
*    CHECK sy-subrc = 0.

      CLEAR: ndiv, pack_mat.

**  Verificar se material é para fazer split.
      READ TABLE lt_zwm046 WITH KEY matnr = lt_vepo-matnr.
      CHECK sy-subrc = 0.

**  Determinar em quantas paletes se vai dividir
      CHECK lt_zwm046-mpalete IS NOT INITIAL OR lt_zwm046-qpalete IS NOT INITIAL.

**  Meia-palete
      CLEAR mlgn.
      SELECT SINGLE *
      FROM mlgn
      WHERE matnr = lt_vepo-matnr
      AND lgnum = lt_vekp-lgnum
      AND ( block = '03' OR block = '04' OR
            block = '05' OR block = '06').
      IF sy-subrc = 0.
        READ TABLE lt_sscc_pick WITH KEY sscc = it_ltap-sscc.
        IF sy-subrc = 0." Palete Picking
          ndiv = 1.
        ELSE.           " Palete completa
          IF mlgn-block = '03' OR mlgn-block = '04'.
            ndiv = 2.
          ELSEIF mlgn-block = '05' OR mlgn-block = '06'.
            ndiv = 4.
          ENDIF.
        ENDIF.

**    Determinar novo packing material Meia Palele
        CLEAR: zwm001, pack_mat.
        SELECT SINGLE *  FROM zwm001
        WHERE armazem   = lt_vekp-lgnum  AND
              processo  = 'MEIA-PALETE' AND
              parametro = lt_vekp-vhilm.

        IF sy-subrc = 0 AND zwm001-valor IS NOT INITIAL.
          pack_mat = zwm001-valor.
        ENDIF.
      ENDIF.

** Quarto-Palete determinar em quantas paletes se vai partir
      CLEAR mlgn.
      SELECT SINGLE *
      FROM mlgn
      WHERE matnr = lt_vepo-matnr
      AND lgnum = lt_vekp-lgnum
      AND ( block = '07' OR block = '08' OR
            block = '09' OR block = '10').
      IF sy-subrc = 0.
        READ TABLE lt_sscc_pick WITH KEY sscc = it_ltap-sscc.
        IF sy-subrc = 0." Palete Picking

          lv_qtd_aux = lt_vepo-vemng.
          CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
            EXPORTING
              i_matnr              = mlgn-matnr
              i_in_me              = lt_vepo-vemeh
              i_out_me             = mlgn-lhme1
              i_menge              = lv_qtd_aux
            IMPORTING
              e_menge              = lv_qtd_aux
            EXCEPTIONS
              error_in_application = 1
              error                = 2
              OTHERS               = 3.

          IF sy-subrc = 0 AND lv_qtd_aux < mlgn-lhmg1.
            IF mlgn-lhmg1 IS NOT INITIAL.
              ndiv = ( lv_qtd_aux * 4 ) / mlgn-lhmg1. "Ndiv = [1-3]
            ENDIF.
          ELSE.
            ndiv = 1.
          ENDIF.

        ELSE.           " Palete Completa
          IF mlgn-block = '07' OR mlgn-block = '08'.
            ndiv = 4.
          ELSEIF mlgn-block = '09' OR mlgn-block = '10'.
            ndiv = 8.
          ENDIF.
        ENDIF.

**    Determinar novo packing material Quarto Palete
        CLEAR: zwm001.
        SELECT SINGLE *  FROM zwm001
        WHERE armazem   = lt_vekp-lgnum    AND
              processo  = 'QUARTO-PALETE'  AND
              parametro = lt_vekp-vhilm.

        IF sy-subrc = 0 AND zwm001-valor IS NOT INITIAL.
          pack_mat = zwm001-valor.
        ENDIF.
      ENDIF.

      CHECK pack_mat IS NOT INITIAL AND ndiv IS NOT INITIAL.

      CLEAR: itemunpack, return.
      REFRESH: return.
      MOVE '1' TO itemunpack-hu_item_type.
      MOVE lt_vepo-matnr TO itemunpack-material.
      MOVE lt_vepo-charg TO itemunpack-batch.
      MOVE lt_vepo-vemng TO itemunpack-pack_qty.
      MOVE lt_vepo-altme TO itemunpack-base_unit_qty_iso.
      MOVE lt_vepo-vemeh TO itemunpack-base_unit_qty.
      MOVE lt_vepo-werks TO itemunpack-plant.
      MOVE lt_vepo-lgort TO itemunpack-stge_loc.

      CALL FUNCTION 'BAPI_HU_UNPACK'
        EXPORTING
          hukey      = it_ltap-sscc
          itemunpack = itemunpack
        TABLES
          return     = return.

      LOOP AT return WHERE  type = 'E' OR
                            type = 'A' .
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      CALL FUNCTION 'HU_PACKING_REFRESH'.

*********************************************
**  Criar Novas HUs

** REFRESH

** Split de quantidade
      lv_new_qtd = lt_vepo-vemng / ndiv.

      CLEAR: headerproposal, itemsproposal,
             exidv1, exidv2.
      REFRESH: itemsproposal.

      itemsproposal-hu_item_type      = '1'. "material
      itemsproposal-base_unit_qty     = lt_vepo-vemeh.
      itemsproposal-base_unit_qty_iso = lt_vepo-altme.
      itemsproposal-pack_qty          = lv_new_qtd.
      itemsproposal-material          = lt_vepo-matnr.
      itemsproposal-plant             = lt_vepo-werks.
      itemsproposal-stge_loc          = lt_vepo-lgort.
      itemsproposal-batch             = lt_vepo-charg.

      APPEND itemsproposal.
      headerproposal-hu_status_init  = 'C'.
      headerproposal-pack_mat = pack_mat.

      CLEAR:   lt_hu.
      REFRESH: lt_hu.

      DO ndiv TIMES.
        CLEAR: hukey, return.
        REFRESH: return.

        CALL FUNCTION 'BAPI_HU_CREATE'
          EXPORTING
            headerproposal = headerproposal
          IMPORTING
*          huheader       = huheader
            hukey          = hukey
          TABLES
            itemsproposal  = itemsproposal
            return         = return.

        IF hukey IS NOT INITIAL.
          CLEAR lt_hu.
          lt_hu-exidv = hukey.
          APPEND lt_hu.
*        IF exidv1 IS INITIAL.
*          MOVE hukey TO exidv1.
*        ELSEIF exidv2 IS INITIAL.
*          MOVE hukey TO exidv2.
*        ELSEIF exidv3 IS INITIAL.
*          MOVE hukey TO exidv3.
*        ELSEIF exidv4 IS INITIAL.
*          MOVE hukey TO exidv4.
*        ENDIF.
        ENDIF.
      ENDDO.

      CLEAR lv_line.
      DESCRIBE TABLE lt_hu LINES lv_line.
      IF lv_line <> ndiv.
        CONTINUE.
      ENDIF.

*    CASE ndiv.
*      WHEN 2.
*        IF exidv1 IS INITIAL OR exidv2 IS INITIAL.
*          CONTINUE.
*        ENDIF.
*      WHEN 4  .
*        IF exidv1 IS INITIAL OR exidv2 IS INITIAL OR
*           exidv3 IS INITIAL OR exidv4 IS INITIAL.
*          CONTINUE.
*        ENDIF.
*    ENDCASE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      CALL FUNCTION 'HU_PACKING_REFRESH'.


**  repack da hu original conteudo duas novas hus.
      DO ndiv TIMES.
        CLEAR: pack_itemproposal, return.
        REFRESH: return.

        pack_itemproposal-hu_item_type = '3'. "HU

        READ TABLE lt_hu INDEX sy-index.
        IF sy-subrc = 0.
          pack_itemproposal-lower_level_exid = lt_hu-exidv.
        ENDIF.

** Meia-Palete
*      IF exidv1 IS NOT INITIAL.
*        pack_itemproposal-lower_level_exid = exidv1.
*        CLEAR exidv1.
*      ELSEIF exidv2 IS NOT INITIAL.
*        pack_itemproposal-lower_level_exid = exidv2.
*        CLEAR exidv2.
*      ELSEIF exidv3 IS NOT INITIAL.
*        pack_itemproposal-lower_level_exid = exidv3.
*        CLEAR exidv3.
*      ELSEIF exidv4 IS NOT INITIAL.
*        pack_itemproposal-lower_level_exid = exidv4.
*        CLEAR exidv4.

        CALL FUNCTION 'BAPI_HU_PACK'
          EXPORTING
            hukey        = lt_vekp-exidv
            itemproposal = pack_itemproposal
          TABLES
            return       = return.

        LOOP AT return WHERE  type = 'E' OR
        type = 'A' .
          EXIT.
        ENDLOOP.

        IF sy-subrc = 0.
          CONTINUE.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          CALL FUNCTION 'HU_PACKING_REFRESH'.
        ENDIF.
      ENDDO.

** Palete Picking
      READ TABLE lt_sscc_pick WITH KEY sscc = it_ltap-sscc.
      IF sy-subrc = 0.

**      Palete com vários materiais de picking
        READ TABLE it_sscc_pick WITH KEY sscc = it_ltap-sscc.
        IF sy-subrc = 0.
          it_sscc_pick-n_qchep = it_sscc_pick-n_qchep + ndiv.
          MODIFY it_sscc_pick INDEX sy-tabix.
        ELSE.
          CLEAR it_sscc_pick.
          it_sscc_pick-sscc    = it_ltap-sscc.
          it_sscc_pick-n_qchep = ndiv.
          APPEND it_sscc_pick.
        ENDIF.

      ENDIF.

    ENDLOOP.
  ENDLOOP.

ENDFUNCTION.
