FUNCTION zwm_delivery_update.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LGNUM) TYPE  LGNUM
*"     REFERENCE(REMESSA) TYPE  VBELN
*"  TABLES
*"      IT_SSCC STRUCTURE  ZWM_LTAP
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

** Global data declarations
  DATA: it_hum_rehang_hu LIKE hum_rehang_hu OCCURS 0 WITH HEADER LINE.

  DATA: vbkok_wa     LIKE vbkok,
        et_header    TYPE hum_hu_header_t,
        et_header_wa TYPE vekpvb,
        et_items     TYPE hum_hu_item_t,
        objects      TYPE hum_object_t,
        objects_wa   TYPE hum_object.

  DATA: lt_prot TYPE TABLE OF prott.

  REFRESH: return_msg.
  CLEAR:   return_msg.

  CHECK NOT it_sscc[] IS INITIAL.

** Obter os SSCC já embalados
  REFRESH: et_header, et_items, objects.
  CLEAR:   et_header, et_items, objects, objects_wa.

  objects_wa-objkey = remessa.
  APPEND objects_wa TO objects.

  CALL FUNCTION 'HU_GET_HUS_FROM_VBFA'
    EXPORTING
*     if_object        =
      it_objects       = objects
*     if_change_export =
*     if_lock_hu       =
    IMPORTING
      et_header        = et_header
      et_items         = et_items
    EXCEPTIONS
      hus_locked       = 1
      no_hu_found      = 2
      hu_changed       = 3
      fatal_error      = 4
      OTHERS           = 5.

  LOOP AT et_header INTO et_header_wa.
    DELETE it_sscc WHERE sscc EQ et_header_wa-exidv.
  ENDLOOP.

  CHECK NOT it_sscc[] IS INITIAL.

** Efectua a embalagem dos restantes SSCC
  LOOP AT it_sscc.

    REFRESH: it_hum_rehang_hu, lt_prot.
    CLEAR:   it_hum_rehang_hu, lt_prot, vbkok_wa.

    MOVE it_sscc-sscc TO it_hum_rehang_hu-top_hu_external.
    APPEND it_hum_rehang_hu.
    CLEAR  it_hum_rehang_hu.

    vbkok_wa-vbeln_vl = remessa.
    vbkok_wa-vbtyp_vl = 'J'.
    vbkok_wa-vbeln    = remessa.

** Embalamento
    CALL FUNCTION 'WS_DELIVERY_UPDATE'
      EXPORTING
        vbkok_wa                 = vbkok_wa
        synchron                 = 'X'
        commit                   = 'X'
        delivery                 = remessa
        nicht_sperren            = space
        if_error_messages_send_0 = space
      TABLES
        prot                     = lt_prot
        it_handling_units        = it_hum_rehang_hu
      EXCEPTIONS
        error_message            = 1
        OTHERS                   = 2.

    IF sy-subrc <> 0.
      return_msg-msgid  = sy-msgid.
      return_msg-msgtyp = sy-msgty.
      return_msg-msgnr  = sy-msgno.
      return_msg-msgv1  = sy-msgv1.
      return_msg-msgv2  = sy-msgv2.
      return_msg-msgv3  = sy-msgv3.
      return_msg-msgv4  = sy-msgv4.
      APPEND return_msg.
      CLEAR  return_msg.
    ENDIF.

  ENDLOOP.

  IF NOT return_msg[] IS INITIAL.
    RAISE error.
  ENDIF.

ENDFUNCTION.
