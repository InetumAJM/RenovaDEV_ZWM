FUNCTION zwm_hu_pack_and_unpack.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_EXIDV) TYPE  EXIDV
*"  TABLES
*"      T_ITEMS STRUCTURE  VEPO
*"      T_RETURN STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: ls_item     LIKE huitem_change.
  DATA: ls_req      TYPE packing_item_hu.
  DATA: lt_hus      TYPE hum_exidv_t.
  DATA: ls_hus      TYPE hum_exidv.
  DATA: ls_header   TYPE vekpvb.
  DATA: ls_items    TYPE vepovb.
  DATA: ls_return   TYPE bdcmsgcoll.
  DATA: ls_vepo     LIKE vepovb.
  DATA: ls_messages TYPE huitem_messages.
  DATA: lt_header   TYPE hum_hu_header_t.
  DATA: lt_items    TYPE hum_hu_item_t.
  DATA: lt_messages TYPE huitem_messages_t.

** Dados HU
**********************************************************************
  CHECK i_exidv IS NOT INITIAL AND t_items[] IS NOT INITIAL.

  CLEAR ls_hus.
  ls_hus-exidv = i_exidv.
  APPEND ls_hus TO lt_hus.

  CALL FUNCTION 'HU_GET_HUS'
    EXPORTING
      if_lock_hus = 'X'
      it_hus      = lt_hus
    IMPORTING
      et_header   = lt_header
      et_items    = lt_items
      et_messages = lt_messages
    EXCEPTIONS
      hus_locked  = 1
      no_hu_found = 2
      fatal_error = 3
      OTHERS      = 4.

  IF sy-subrc <> 0.
    CLEAR t_return.
    t_return-msgid  = sy-msgid.
    t_return-msgtyp = sy-msgty.
    t_return-msgnr  = sy-msgno.
    t_return-msgv1  = sy-msgv1.
    t_return-msgv2  = sy-msgv2.
    t_return-msgv3  = sy-msgv3.
    t_return-msgv4  = sy-msgv4.
    APPEND t_return.
    RAISE error.
  ENDIF.

  READ TABLE lt_messages INTO ls_messages WITH KEY msgty = 'E'.
  IF sy-subrc = 0.
    CLEAR t_return.
    t_return-msgid  = ls_messages-msgid.
    t_return-msgtyp = ls_messages-msgty.
    t_return-msgnr  = ls_messages-msgno.
    t_return-msgv1  = ls_messages-msgv1.
    t_return-msgv2  = ls_messages-msgv2.
    t_return-msgv3  = ls_messages-msgv3.
    t_return-msgv4  = ls_messages-msgv4.
    APPEND t_return.
    RAISE error.
  ENDIF.

** Modificar HU
**********************************************************************
  READ TABLE lt_header INTO ls_header WITH KEY exidv = i_exidv.

  LOOP AT t_items.
    CLEAR ls_req.

    MOVE-CORRESPONDING t_items TO ls_req.

    READ TABLE lt_items INTO ls_items WITH KEY venum = t_items-venum
                                               vepos = t_items-vepos.

    CHECK sy-subrc = 0.

    ls_req-quantity  = t_items-vemng - ls_items-vemng.
    ls_req-altme     = ls_items-vemeh.
    ls_req-meins     = ls_items-vemeh.

    CALL FUNCTION 'HU_PACKING_AND_UNPACKING'
      EXPORTING
        is_packing_request = ls_req
      IMPORTING
        es_p_request       = ls_req
        es_item            = ls_vepo
      EXCEPTIONS
        OTHERS             = 99.

    IF sy-subrc <> 0.
      CLEAR t_return.
      t_return-msgid  = sy-msgid.
      t_return-msgtyp = sy-msgty.
      t_return-msgnr  = sy-msgno.
      t_return-msgv1  = sy-msgv1.
      t_return-msgv2  = sy-msgv2.
      t_return-msgv3  = sy-msgv3.
      t_return-msgv4  = sy-msgv4.
      APPEND t_return.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      RAISE error.
    ENDIF.

  ENDLOOP.

  IF sy-subrc = 0.

    CALL FUNCTION 'HU_POST'
      EXPORTING
        if_synchron = 'X'
        if_commit   = 'X'.

    CALL FUNCTION 'HU_ENQUEUE'
      EXPORTING
        if_dequeue   = 'X'
      CHANGING
        cs_header    = ls_header
      EXCEPTIONS
        foreign_lock = 1
        fatal_error  = 2
        le_not_found = 3
        OTHERS       = 4.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.


  ENDIF.

ENDFUNCTION.
