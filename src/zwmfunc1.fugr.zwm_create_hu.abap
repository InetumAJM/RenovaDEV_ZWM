FUNCTION zwm_create_hu.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WAREHOUSE) TYPE  LGNUM
*"     REFERENCE(PLANT) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(S_LOC) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(PACKING_MATERIAL) TYPE  MATNR
*"     REFERENCE(TALAO) TYPE  CHAR5 OPTIONAL
*"     REFERENCE(HU) TYPE  EXIDV OPTIONAL
*"  EXPORTING
*"     REFERENCE(HUKEY) TYPE  BAPIHUKEY-HU_EXID
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL OPTIONAL
*"      ITEMS STRUCTURE  ZWM_ITEMS_HU
*"  EXCEPTIONS
*"      EMPTY_TABLE
*"      REFERENCE_DOCUMENT_DIFFERS
*"      EMPTY_DELIVERY_ITEM
*"      ITEM_NOT_FOUND
*"----------------------------------------------------------------------
  DATA: pack_mat(18).

  IF items[] IS INITIAL.
    RAISE empty_table.
  ENDIF.

***********************************************
* refresh
  CALL FUNCTION 'HU_PACKING_REFRESH'.

***********************************************
  DATA: is_object   LIKE hum_object,
        ls_hum_mm   LIKE hum_plant_stloc,
        lt_messages LIKE huitem_messages OCCURS 0 WITH HEADER LINE,
        w_lips      LIKE lips OCCURS 0 WITH HEADER LINE,
        vbeln       LIKE ltak-vbeln.
* check items
  READ TABLE items INDEX 1.
  vbeln = items-ltak_vbeln.
  LOOP AT items.
    IF items-ltak_vbeln <> vbeln.
      RAISE reference_document_differs.
    ENDIF.
    IF NOT vbeln IS INITIAL AND items-ltap_posnr IS INITIAL.
      RAISE empty_delivery_item.
    ENDIF.
  ENDLOOP.
  IF NOT vbeln IS INITIAL.
    SELECT SINGLE * FROM likp WHERE vbeln = vbeln.
    SELECT * FROM lips INTO TABLE w_lips
           WHERE vbeln = vbeln.
    LOOP AT items.
      READ TABLE w_lips WITH KEY vbeln = items-ltak_vbeln
                                 posnr = items-ltap_posnr.
      IF sy-subrc <> 0.
        RAISE item_not_found.
      ELSEIF w_lips-matnr <> items-material.
        RAISE item_not_found.
      ENDIF.
    ENDLOOP.
  ENDIF.


  IF NOT vbeln IS INITIAL.
*    is_object-object = '01'.
    is_object-objkey = vbeln.
  ELSE.
    is_object-object = '12'.
  ENDIF.

  IF NOT plant IS INITIAL AND NOT s_loc IS INITIAL.
    ls_hum_mm-plant = plant.
    ls_hum_mm-stge_loc = s_loc.
  ENDIF.

  ls_hum_mm-whse_no = warehouse.

* initialize packing
  CALL FUNCTION 'HU_INITIALIZE_PACKING'
    EXPORTING
      is_object      = is_object
      is_plant_stloc = ls_hum_mm
    IMPORTING
      et_messages    = lt_messages[]
    EXCEPTIONS
      not_possible   = 1
      OTHERS         = 99.
  LOOP AT lt_messages.
    MOVE-CORRESPONDING lt_messages TO return_msg.
    MOVE lt_messages-msgno TO return_msg-msgnr.
    APPEND return_msg.
  ENDLOOP.

*********************************************************
  DATA: ls_header_proposal LIKE huhdr_proposal,
        ls_header_add      LIKE huhdr_additional_data,
        lt_item_prop       LIKE huitm_proposal OCCURS 0 WITH HEADER LINE,
        ls_header          LIKE vekpvb,
        lt_items           LIKE vepovb OCCURS 0 WITH HEADER LINE.
*        ls_header_transport LIKE huhdr_transport.

  REFRESH lt_messages.

  ls_header_proposal-exida = 'C'.
  ls_header_proposal-vhilm = packing_material.
  ls_header_proposal-hu_status_init = 'C'.

  IF NOT hu IS INITIAL.
    ls_header_proposal-exida = 'H'.
    ls_header_proposal-exidv = hu.
*    ls_header_proposal-hu_status_init = 'H'.
  ENDIF.

  IF NOT plant IS INITIAL AND NOT s_loc IS INITIAL.
    ls_header_add-werks = plant.
    ls_header_add-lgort = s_loc.
  ENDIF.

  LOOP AT items.
    lt_item_prop-velin = '1'.
    lt_item_prop-quantity = items-quantity.
    lt_item_prop-meins = items-unit.
    lt_item_prop-matnr = items-material.


    lt_item_prop-charg = items-batch.
    IF NOT vbeln IS INITIAL.
      lt_item_prop-belnr = vbeln.
      lt_item_prop-posnr = items-ltap_posnr.
    ENDIF.

    IF items-werks IS INITIAL.
      lt_item_prop-werks = plant.
    ELSE.
      lt_item_prop-werks = items-werks.
    ENDIF.

    IF items-lgort IS INITIAL.
      lt_item_prop-lgort = s_loc.
    ELSE.
      lt_item_prop-lgort = items-lgort.
    ENDIF.


    APPEND lt_item_prop.
  ENDLOOP.

*  IF NOT talao IS INITIAL.
*    move talao to ls_header_transport-namef.
*  condense ls_header_transport-namef no-gaps.
*
*  ENDIF.

* function call
  CALL FUNCTION 'HU_CREATE_ONE_HU'
    EXPORTING
      is_header_proposal = ls_header_proposal
*     IS_HEADER_CAPACITY = LS_HEADER_CAPACITY
      is_header_add      = ls_header_add
*     is_header_transport       = ls_header_transport
      it_items           = lt_item_prop[]
*     IT_SERIAL_NR       = LT_SERIAL
    IMPORTING
      es_header          = ls_header
      et_items           = lt_items[]
      et_messages        = lt_messages[]
    EXCEPTIONS
      input_missing      = 1
      not_possible       = 2
      header_error       = 3
      item_error         = 4
      serial_nr_error    = 5
      OTHERS             = 7.
  LOOP AT lt_messages.
    MOVE-CORRESPONDING lt_messages TO return_msg.
    MOVE lt_messages-msgno TO return_msg-msgnr.
    APPEND return_msg.
  ENDLOOP.

  hukey = ls_header-exidv.

* posting
  IF vbeln IS INITIAL.
    CALL FUNCTION 'HU_POST'
      EXPORTING
        if_synchron = 'X'
        if_commit   = 'X'
        is_object   = is_object
      IMPORTING
        et_messages = lt_messages[].
    LOOP AT lt_messages.
      MOVE-CORRESPONDING lt_messages TO return_msg.
      MOVE lt_messages-msgno TO return_msg-msgnr.
      APPEND return_msg.
    ENDLOOP.
  ELSE.

    DATA: ls_vbkok_wa TYPE vbkok,
          lt_prot     TYPE TABLE OF prott,
          lt_rehang   LIKE hum_rehang_hu OCCURS 0 WITH HEADER LINE,
          save_index  LIKE sy-tabix.
    ls_vbkok_wa-vbeln = vbeln.
    ls_vbkok_wa-wabuc = 'X'.
    ls_vbkok_wa-vbeln_vl = vbeln.
    ls_vbkok_wa-vbtyp_vl = likp-vbtyp.
    LOOP AT lt_items.
      save_index = sy-tabix.
      READ TABLE items WITH KEY material = lt_items-matnr.
      lt_items-vbeln = items-ltak_vbeln.
      lt_items-posnr = items-ltap_posnr.

      lt_rehang-top_hu_internal = lt_items-venum.
      lt_rehang-venum = lt_items-venum.
      lt_rehang-vepos = lt_items-vepos.
      lt_rehang-rfbel = lt_items-vbeln.
      lt_rehang-rfpos = lt_items-posnr.
      APPEND lt_rehang.

      MODIFY lt_items INDEX save_index.
    ENDLOOP.

    CALL FUNCTION 'WS_DELIVERY_UPDATE'
      EXPORTING
        vbkok_wa          = ls_vbkok_wa
        synchron          = 'X'
        commit            = 'X'
        delivery          = ls_vbkok_wa-vbeln
        nicht_sperren     = space
      TABLES
        prot              = lt_prot
        it_handling_units = lt_rehang.


  ENDIF.
*  IF NOT talao IS INITIAL.
*    UPDATE vekp SET namef = talao WHERE exidv = hukey.
*  ENDIF.


* wait for update
  DO 10 TIMES.
    SELECT SINGLE * FROM vekp
           BYPASSING BUFFER
           WHERE exidv = hukey.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

ENDFUNCTION.
