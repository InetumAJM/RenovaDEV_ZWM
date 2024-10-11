FUNCTION ZWM_ENTRADAS_MATERIAL_F03.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LGNUM) TYPE  LGNUM
*"     REFERENCE(AUFNR) TYPE  AUFNR OPTIONAL
*"     REFERENCE(EBELN) TYPE  EBELN OPTIONAL
*"     REFERENCE(EBELP) TYPE  EBELP OPTIONAL
*"     REFERENCE(CODE) TYPE  BAPI2017_GM_CODE
*"     REFERENCE(PORTA) TYPE  ABLAD OPTIONAL
*"     REFERENCE(RECEBEDOR) TYPE  WEMPF OPTIONAL
*"     REFERENCE(MOV_MM) TYPE  BWLVS
*"     REFERENCE(TESTRUN) TYPE  BAPI2017_GM_GEN-TESTRUN OPTIONAL
*"     REFERENCE(PLANT_O) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(SLOC_O) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(LINHAS_ZERO) TYPE  XFELD OPTIONAL
*"     REFERENCE(NOTA_REMESSA) TYPE  XABLN OPTIONAL
*"     REFERENCE(I_DATA) TYPE  DATUM OPTIONAL
*"  EXPORTING
*"     REFERENCE(MATERIALDOCUMENT) TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC
*"     REFERENCE(MATDOCUMENTYEAR) TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"      ITEMS STRUCTURE  ZWM018
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
*{   INSERT         DEVK904539                                        1
  DATA: goodsmvt_header LIKE bapi2017_gm_head_01,
        goodsmvt_item
                LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE,
        return LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        return2 LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        mblnr LIKE mkpf-mblnr,
        gjahr LIKE mkpf-mjahr,
        plant TYPE werks_d,
        lgort TYPE lgort_d.


  REFRESH: goodsmvt_item, return, return2.
  CLEAR: materialdocument, matdocumentyear, goodsmvt_header,
         goodsmvt_item, plant, lgort.

* check no items
  IF items[] IS INITIAL.
    RAISE error.
    EXIT.
  ENDIF.

  READ TABLE items INDEX 1.

if lgnum is not initial.
* check material/warehouse
  CALL FUNCTION 'ZWM_GET_MATERIAL_PLANT_SLOC'
    EXPORTING
      warehouse    = lgnum
      material     = items-material
    IMPORTING
      plant        = plant
      s_loc        = lgort
    TABLES
      return_msg   = return_msg
    EXCEPTIONS
      not_found    = 1
      indetermined = 2
      OTHERS       = 3.
  IF sy-subrc <> 0.
    RAISE error.
    EXIT.
  ENDIF.
endif.

  LOOP AT items.

    AT FIRST.
* prepare for posting
* document Date
*      Begin GN(ROFF) 30.10.2012
*      MOVE sy-datum TO goodsmvt_header-doc_date.
      MOVE i_data TO goodsmvt_header-doc_date.
*      End   GN(ROFF) 30.10.2012
* posting date
*      Begin GN(ROFF) 30.10.2012
*      MOVE sy-datum TO goodsmvt_header-pstng_date.
      MOVE i_data TO goodsmvt_header-pstng_date.
*      End   GN(ROFF) 30.10.2012

      IF NOT nota_remessa IS INITIAL.
*        goodsmvt_header-gr_gi_slip_no = nota_remessa.
        goodsmvt_header-ref_doc_no = nota_remessa.
      ENDIF.
    ENDAT.

* se estiverem preenchidos é uma transferencia entre depositos
    IF NOT plant_o IS INITIAL AND NOT sloc_o IS INITIAL.
*      goodsmvt_item-move_plant = goodsmvt_item-plant.
*      goodsmvt_item-move_stloc = goodsmvt_item-stge_loc.
      goodsmvt_item-move_plant = plant.
      goodsmvt_item-move_stloc = lgort.
      goodsmvt_item-plant = plant_o.
      goodsmvt_item-stge_loc = sloc_o.
*      goodsmvt_item-stge_loc = 'B'.
    ELSE.
      goodsmvt_item-plant = plant.
      goodsmvt_item-stge_loc = lgort.
    ENDIF.

    IF NOT recebedor IS INITIAL.
      goodsmvt_item-gr_rcpt = recebedor.
    ENDIF.
* movement type
    MOVE mov_mm TO goodsmvt_item-move_type.
* Entry Unit
    SELECT SINGLE meins
           FROM mara
           INTO (goodsmvt_item-entry_uom)
           WHERE matnr = items-material.
* Iso ENtry Unit
    goodsmvt_item-entry_uom_iso = goodsmvt_item-entry_uom.

    IF NOT items-ebeln IS INITIAL.
      goodsmvt_item-mvt_ind = 'B'.
    ELSE.
      IF NOT aufnr IS INITIAL.
        goodsmvt_item-mvt_ind = 'F'.
      ELSE.
        goodsmvt_item-mvt_ind = ' '.
      ENDIF.
    ENDIF.

    IF NOT porta IS INITIAL.
      goodsmvt_item-unload_pt = porta.
    ENDIF.
* items
    goodsmvt_item-material = items-material.
    goodsmvt_item-batch = items-lote.
    goodsmvt_item-entry_qnt = items-quantidade.
*    goodsmvt_item-UNITTYPE_1 = 'Z01'.
    goodsmvt_item-SPEC_MVMT = 'M'.
*    GOODSMVT_ITEM-EXPIRYDATE = ITEMS-VFDAT.
*    Begin GN(ROFF) 30.10.2012
*    goodsmvt_item-prod_date = sy-datum.
    goodsmvt_item-prod_date = i_data.
*    End   GN(ROFF) 30.10.2012
    IF NOT items-ebeln IS INITIAL.
      goodsmvt_item-po_number = items-ebeln.
      goodsmvt_item-po_item = items-ebelp.
    ENDIF.
    IF NOT aufnr IS INITIAL.
      goodsmvt_item-orderid = aufnr.
    ENDIF.

    IF linhas_zero = 'X'.
      goodsmvt_item-ind_propose_quanx = 'X'.
    ENDIF.

    APPEND goodsmvt_item.

    AT LAST.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = goodsmvt_header
          goodsmvt_code    = code
          testrun          = testrun
        IMPORTING
          materialdocument = materialdocument
          matdocumentyear  = matdocumentyear
        TABLES
          goodsmvt_item    = goodsmvt_item
          return           = return.
*  IF SY-SUBRC <> 0.
*    RAISE ERROR.
*    EXIT.
*  ELSE.
      LOOP AT return.
        MOVE return-type TO return_msg-msgtyp.
        MOVE return-id TO return_msg-msgid.
        MOVE return-number TO return_msg-msgnr.
        MOVE return-message_v1 TO return_msg-msgv1.
        MOVE return-message_v2 TO return_msg-msgv2.
        MOVE return-message_v3 TO return_msg-msgv3.
        MOVE return-message_v4 TO return_msg-msgv4.
        APPEND return_msg.
      ENDLOOP.

      LOOP AT return WHERE type = 'E' OR type = 'A'.
        COMMIT WORK.
        RAISE error.
        EXIT.
      ENDLOOP.

      IF testrun IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          IMPORTING
            return = return2.

* wait for update
        DO 10 TIMES.
          SELECT SINGLE * FROM mkpf
                 BYPASSING BUFFER
                 WHERE mblnr = materialdocument
                   AND mjahr = matdocumentyear.
          IF sy-subrc = 0.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
          ENDIF.
        ENDDO.
      ENDIF.

    ENDAT.
  ENDLOOP.

*}   INSERT
ENDFUNCTION.
