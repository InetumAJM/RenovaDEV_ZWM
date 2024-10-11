FUNCTION zwm_delivery_batch_split_del.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"     REFERENCE(I_SSCC) TYPE  EXIDV
*"  TABLES
*"      T_RETURN STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: lv_tabix          LIKE sy-tabix.
  DATA: lv_menge          TYPE menge_d.
  DATA: lv_menge_pk       TYPE menge_d.
  DATA: lv_pal            TYPE i.
  DATA: lv_refnr          TYPE lvs_refnr.
  DATA: lv_check          TYPE char1.
  DATA: lv_charg_posnr    TYPE posnr.
  DATA: lv_delivery       TYPE bapiobdlvhdrchg-deliv_numb.

  DATA: ls_likp           TYPE likp.
  DATA: ls_return         TYPE bdcmsgcoll.
  DATA: ls_vbkok_wa       TYPE vbkok.
  DATA: ls_vekp           TYPE vekp.
  DATA: ls_vbuk           TYPE vbuk.
  DATA: ls_header_data    TYPE bapiobdlvhdrchg.
  DATA: ls_header_control TYPE bapiobdlvhdrctrlchg.
  DATA: ls_item_data      TYPE bapiobdlvitemchg.

  DATA: lt_vbfa           TYPE vbfa                 OCCURS 0 WITH HEADER LINE.
  DATA: lt_vepo           TYPE vepo                 OCCURS 0 WITH HEADER LINE.
  DATA: lt_item_data      TYPE bapiobdlvitemchg     OCCURS 0 WITH HEADER LINE.
  DATA: lt_item_control	  TYPE bapiobdlvitemctrlchg OCCURS 0 WITH HEADER LINE.
  DATA: lt_return	        TYPE bapiret2             OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF lt_lips OCCURS 0.
          INCLUDE STRUCTURE lips.
  DATA: pikmg	TYPE rfmng.
  DATA: menge	TYPE rfmng.
  DATA END OF lt_lips.

  DATA: ls_lips     LIKE lt_lips.
  DATA: lt_lips_aux LIKE LINE OF lt_lips OCCURS 0 WITH HEADER LINE.

** Validar Remessa
**********************************************************************
  CHECK i_sscc IS NOT INITIAL.

  SELECT SINGLE *
    FROM likp INTO ls_likp
    WHERE vbeln = i_vbeln.

  SELECT *
    FROM lips INTO TABLE lt_lips
    WHERE vbeln = i_vbeln.

  DELETE lt_lips WHERE lfimg IS INITIAL.

  lt_lips_aux[] = lt_lips[].

  SORT lt_lips BY posnr DESCENDING.

  SORT lt_lips_aux BY uecha charg.

*  " Obter item p/ partição Lote
*  lv_charg_posnr = '900000'.
*
*  READ TABLE lt_lips INDEX 1.
*  IF sy-subrc = 0.
*    IF lt_lips-posnr > lv_charg_posnr.
*      lv_charg_posnr = lt_lips-posnr.
*    ENDIF.
*  ENDIF.

** Validar Picking já efectuado
**********************************************************************
*  IF lt_lips[] IS NOT INITIAL.
*
*    SELECT *
*      FROM vbfa INTO TABLE lt_vbfa
*      FOR ALL ENTRIES IN lt_lips
*      WHERE vbelv = lt_lips-vbeln AND
*            posnv = lt_lips-posnr.
*
*    DELETE lt_vbfa WHERE vbtyp_n <> 'Q'.
*    DELETE lt_vbfa WHERE rfmng IS INITIAL.
*
*    SORT lt_vbfa BY vbelv posnv.
*  ENDIF.

*** Validar quantidade picking
*  LOOP AT lt_lips.
*
*    lv_tabix = sy-tabix.
*
*    lt_lips-menge = lt_lips-lfimg.
*
*    LOOP AT lt_vbfa WHERE vbelv = lt_lips-vbeln AND
*                          posnv = lt_lips-posnr.
*
*      lv_menge = lt_vbfa-rfmng.
*
*      IF lt_lips-vrkme <> lt_vbfa-meins.
*        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*          EXPORTING
*            i_matnr              = lt_lips-matnr
*            i_in_me              = lt_vbfa-meins
*            i_out_me             = lt_lips-vrkme
*            i_menge              = lv_menge
*          IMPORTING
*            e_menge              = lv_menge
*          EXCEPTIONS
*            error_in_application = 1
*            error                = 2
*            OTHERS               = 3.
*      ENDIF.
*
*      lt_lips-pikmg = lt_lips-pikmg + lv_menge. "Qtd com picking efectuado
**      lt_lips-lfimg = lt_lips-lfimg - lv_menge. "Qtd sem picking efectuado
*    ENDLOOP.
*
**    IF lt_lips-lfimg IS INITIAL.
**      DELETE lt_lips INDEX lv_tabix.
**    ELSE.
*    MODIFY lt_lips INDEX lv_tabix.
**    ENDIF.
*  ENDLOOP.

** Actualizar Remessa Com Partição de Lote
**********************************************************************
  DELETE lt_lips WHERE uecha IS INITIAL.

  SORT lt_lips BY matnr charg posnr DESCENDING.

** Header
  ls_header_data-deliv_numb    = ls_likp-vbeln.
  ls_header_control-deliv_numb = ls_likp-vbeln.
  lv_delivery                  = ls_likp-vbeln.

** Obter dados da palete
  SELECT SINGLE *
    FROM vekp INTO ls_vekp
    WHERE exidv = i_sscc.

  SELECT *
    FROM vepo INTO TABLE lt_vepo
    WHERE venum = ls_vekp-venum.

  READ TABLE lt_vepo INDEX 1.
  CHECK sy-subrc = 0.

  CHECK lt_vepo-charg IS NOT INITIAL.

  " Distribuir quantidade pelos items da remessa
  lv_menge = lt_vepo-vemng.

  LOOP AT lt_lips WHERE matnr = lt_vepo-matnr AND
                        charg = lt_vepo-charg.

    IF lt_vepo-vemeh <> lt_lips-vrkme.

      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = lt_vepo-matnr
          i_in_me              = lt_vepo-vemeh
          i_out_me             = lt_lips-vrkme
          i_menge              = lv_menge
        IMPORTING
          e_menge              = lv_menge
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.

      lt_vepo-vemeh = lt_lips-vrkme.
    ENDIF.

    lv_menge_pk = lv_menge - lt_lips-lfimg.

    IF lv_menge_pk < 0.
      lt_lips-lfimg = lt_lips-lfimg - lv_menge.

      lv_menge_pk = lv_menge.
      lv_menge    = 0.

      MODIFY lt_lips INDEX sy-tabix TRANSPORTING lfimg.

    ELSE.
      lv_menge_pk   = lt_lips-lfimg.
      lv_menge      = lv_menge - lt_lips-lfimg.
      lt_lips-lfimg = 0.

      DELETE lt_lips INDEX sy-tabix.
    ENDIF.

    " Atualizar Batch Split
    CLEAR lt_item_data.
    lt_item_data-deliv_numb      = lt_lips-vbeln.
    lt_item_data-deliv_item      = lt_lips-posnr.
    lt_item_data-material        = lt_lips-matnr.
    lt_item_data-batch           = lt_lips-charg.
    lt_item_data-dlv_qty         = lt_lips-lfimg.
    lt_item_data-sales_unit      = lt_lips-vrkme.
    lt_item_data-sales_unit_iso  = lt_item_data-base_uom
                                 = lt_item_data-base_uom_iso
                                 = lt_item_data-sales_unit.
    lt_item_data-fact_unit_nom   = 1.
    lt_item_data-fact_unit_denom = 1.
    APPEND lt_item_data.

    CLEAR lt_item_control.
    lt_item_control-deliv_numb = lt_lips-vbeln.
    lt_item_control-deliv_item = lt_lips-posnr.

    IF lt_lips-lfimg = 0.
      lt_item_control-del_item   = 'X'.
      APPEND lt_item_control.
    ELSE.
      lt_item_control-chg_delqty = 'X'.
      APPEND lt_item_control.
    ENDIF.

    " Actualizar Item Pai
    IF lt_lips-uecha IS NOT INITIAL.

      CLEAR lt_item_data.
      lt_item_data-deliv_numb      = lt_lips-vbeln.
      lt_item_data-deliv_item      = lt_lips-uecha.
      lt_item_data-material        = lt_lips-matnr.
*      lt_item_data-batch           = lt_lips-charg.

      READ TABLE lt_lips_aux INTO ls_lips WITH KEY vbeln = lt_lips-vbeln
                                                   posnr = lt_lips-uecha.

      lt_item_data-dlv_qty         = ls_lips-lfimg + lv_menge_pk.
      lt_item_data-sales_unit      = ls_lips-vrkme.
      lt_item_data-sales_unit_iso  = lt_item_data-base_uom
                                   = lt_item_data-base_uom_iso
                                   = lt_item_data-sales_unit.
      lt_item_data-fact_unit_nom   = 1.
      lt_item_data-fact_unit_denom = 1.
      APPEND lt_item_data.

      CLEAR lt_item_control.
      lt_item_control-deliv_numb = lt_lips-vbeln.
      lt_item_control-deliv_item = lt_lips-uecha.
      lt_item_control-chg_delqty = 'X'.
      APPEND lt_item_control.
    ENDIF.

*      ENDIF.
*    ENDIF.

    IF lv_menge <= 0.
      EXIT.
    ENDIF.

  ENDLOOP.

** Modificação do Lote
**********************************************************************
  CLEAR lv_check.

  IF lt_item_data[] IS NOT INITIAL.

    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
      EXPORTING
        header_data    = ls_header_data
        header_control = ls_header_control
        delivery       = lv_delivery
      TABLES
        item_data      = lt_item_data
        item_control   = lt_item_control
        return         = lt_return.

    LOOP AT lt_return WHERE type = 'E' OR type = 'A'.
      CLEAR ls_return.
      MOVE lt_return-type       TO ls_return-msgtyp.
      MOVE lt_return-id         TO ls_return-msgid.
      MOVE lt_return-number     TO ls_return-msgnr.
      MOVE lt_return-message_v1 TO ls_return-msgv1.
      MOVE lt_return-message_v2 TO ls_return-msgv2.
      MOVE lt_return-message_v3 TO ls_return-msgv3.
      MOVE lt_return-message_v4 TO ls_return-msgv4.
      APPEND ls_return TO t_return.
    ENDLOOP.

    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      RAISE error.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      WAIT UP TO 2 SECONDS.

      DELETE lt_item_data WHERE batch IS INITIAL.

      DO 10 TIMES.

        REFRESH: lt_lips.

        SELECT *
          FROM lips INTO TABLE lt_lips
          WHERE vbeln = i_vbeln.

        DELETE lt_lips WHERE lfimg IS INITIAL.

        CLEAR lv_check.

        LOOP AT lt_item_data.

          READ TABLE lt_lips WITH KEY matnr = lt_item_data-material
                                      charg = lt_item_data-batch.

          IF sy-subrc <> 0.
            ls_item_data = lt_item_data.

            lv_check = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF lv_check IS INITIAL.
          EXIT.
        ENDIF.

      ENDDO.
    ENDIF.
  ENDIF.

** Validar a Partição de lote
*  IF lv_check IS NOT INITIAL.
*
*    " Partição de Lote & não foi possível na Remessa para Material &.
*    CLEAR ls_return.
*    ls_return-msgid  = 'ZWMMSG001'.
*    ls_return-msgtyp = 'E'.
*    ls_return-msgnr  = '328'.
*    ls_return-msgv1  = ls_item_data-batch.
*    ls_return-msgv2  = ls_item_data-material.
*
*    APPEND ls_return TO t_return.
*
*    RAISE error.
*  ENDIF.

ENDFUNCTION.
