FUNCTION zwm_unpack_handling_unit.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LGNUM) TYPE  LGNUM
*"     REFERENCE(EXIDV) TYPE  EXIDV
*"     REFERENCE(CHARG) TYPE  CHARG_D
*"     REFERENCE(WERKS) TYPE  WERKS_D
*"     REFERENCE(LGORT) TYPE  LGORT_D
*"     REFERENCE(MATNR) TYPE  MATNR
*"     REFERENCE(VEMNG) TYPE  VEMNG
*"     REFERENCE(VEMEH) TYPE  VEMEH
*"  EXCEPTIONS
*"      OTHER
*"      NOT_POSSIBLE
*"      INPUT_MISSING
*"      INPUT_ERROR
*"      NOT_UNPACKED
*"      FATAL_ERROR
*"      DELIVERY_UPDATE_ERROR
*"----------------------------------------------------------------------
  DATA: i_ref_doc LIKE hum_object OCCURS 0 WITH HEADER LINE,
        i_mm_data LIKE hum_plant_stloc OCCURS 0 WITH HEADER LINE,
        ls_hu_internal TYPE hum_venum,
        lt_hu_internal TYPE hum_venum_t,
        ls_hu_external TYPE hum_exidv,
        lt_hu_external TYPE hum_exidv_t,
        i_hu_unpack_items LIKE huitem_unpack OCCURS 0 WITH HEADER LINE,
        i_from_hu_id TYPE huitem_from,
        o_messages TYPE huitem_messages_t.


  CLEAR: i_ref_doc,
         i_mm_data,
         ls_hu_internal,
         lt_hu_internal,
         ls_hu_external,
         lt_hu_external,
         i_hu_unpack_items,
         i_from_hu_id,
         o_messages.

  REFRESH: i_ref_doc,
           i_mm_data,
           i_hu_unpack_items,
           o_messages.

  SELECT SINGLE *
      FROM vekp
          WHERE exidv = exidv.

  SELECT SINGLE *
      FROM vepo
          WHERE venum = vekp-venum AND
                matnr = matnr AND
                charg = charg.

  ls_hu_internal-venum = vepo-venum.
  APPEND ls_hu_internal TO lt_hu_internal.

  ls_hu_external-exidv = vekp-exidv.
  APPEND ls_hu_external TO lt_hu_external.

  i_from_hu_id-venum = vekp-venum.
  i_from_hu_id-exidv = exidv.

  i_hu_unpack_items-velin = 1.
  i_hu_unpack_items-item_number = vepo-vepos.
  i_hu_unpack_items-unpack_venum = vepo-venum.
  i_hu_unpack_items-unpack_exidv = vekp-exidv.
  i_hu_unpack_items-matnr = vepo-matnr.
  i_hu_unpack_items-charg = vepo-charg.
  i_hu_unpack_items-quantity = vemng.
  i_hu_unpack_items-meins = vemeh.
  i_hu_unpack_items-werks = werks.
  i_hu_unpack_items-lgort = lgort.
  APPEND i_hu_unpack_items.


  CALL FUNCTION 'UNPACK_HANDLING_UNIT'
    EXPORTING
      i_ref_doc             = i_ref_doc
      i_mm_data             = i_mm_data
      i_hu_internal         = lt_hu_internal
      i_hu_external         = lt_hu_external
      i_from_hu_id          = i_from_hu_id
      i_hu_unpack_items     = i_hu_unpack_items
    IMPORTING
      o_messages            = o_messages
    EXCEPTIONS
      input_missing         = 1
      input_error           = 2
      not_possible          = 3
      not_unpacked          = 4
      fatal_error           = 5
      delivery_update_error = 6
      OTHERS                = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



ENDFUNCTION.
