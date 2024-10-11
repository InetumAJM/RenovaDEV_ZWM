FUNCTION zwm_hu_change_werks_lgort.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_EXIDV) TYPE  EXIDV
*"     REFERENCE(I_WERKS) TYPE  WERKS_D
*"     REFERENCE(I_LGORT) TYPE  LGORT_D
*"     REFERENCE(I_HEAD_CHANGE) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_ITM_CHANGE) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_COMMIT) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------


  TYPES: BEGIN OF lty_vekp,
             venum TYPE venum,
             werks TYPE werks_d,
             lgort TYPE lgort_d,
             lgnum TYPE lgnum,
            END OF lty_vekp.

  CONSTANTS : lc_deleted TYPE hu_status VALUE '0060'.

  DATA: lt_huitem_messages    TYPE huitem_messages_t,
        lt_changed            TYPE hum_update_header_t,
        it_hu_identifications TYPE TABLE OF hum_exidv,
        it_internal_numbers   TYPE TABLE OF hum_venum,
        lt_vepo               TYPE TABLE OF vepo.

  DATA: ls_message            TYPE bdcmsgcoll,
        ls_huitem_message     TYPE huitem_messages,
        ls_item               TYPE huitem_change,
        ls_header             TYPE vekpvb,
        ls_object             TYPE hum_object,
        ls_hum_mm             TYPE hum_plant_stloc,
        ls_vekp               TYPE lty_vekp,
        ls_vepo               TYPE vepo,
        ls_new_values         TYPE hum_update_header,
        ls_hu_identifications TYPE hum_exidv.

  DATA : lv_exidv TYPE exidv,
         lv_menge TYPE menge_d.

  CHECK NOT i_exidv IS INITIAL.

** Reload de Dados Internos de HU
***********************************************************************
  CALL FUNCTION 'HU_PACKING_REFRESH'.

** Get HU
***********************************************************************
  SELECT SINGLE venum werks lgort lgnum FROM vekp
                                        INTO ls_vekp
                                        WHERE exidv = i_exidv AND
                                              status <> lc_deleted.
  IF sy-subrc <> 0.
**  HU & inválida
    ls_message-msgid  = 'E'.
    ls_message-msgid = 'ZRF'.
    ls_message-msgnr = '018'.
    ls_message-msgv1  = i_exidv.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  CHECK i_head_change EQ abap_true OR
        i_itm_change  EQ abap_true.

***********************************************************************
** HEADER
***********************************************************************
  IF i_head_change EQ abap_true.

    ls_hu_identifications-exidv = i_exidv.
    APPEND ls_hu_identifications TO it_hu_identifications.

    ls_hum_mm-plant      = ls_vekp-werks.
    ls_hum_mm-stge_loc   = ls_vekp-lgort.
    ls_hum_mm-whse_no    = ls_vekp-lgnum.


    CALL FUNCTION 'HU_INITIALIZE_PACKING'
      EXPORTING
        is_object             = ls_object
        is_plant_stloc        = ls_hum_mm
        it_hu_identifications = it_hu_identifications
        it_internal_numbers   = it_internal_numbers
      IMPORTING
        et_messages           = lt_huitem_messages
      EXCEPTIONS
        not_possible          = 1
        OTHERS                = 99.

    IF sy-subrc <> 0.
      LOOP AT lt_huitem_messages INTO ls_huitem_message.
        ls_message-msgtyp = 'E'.
        ls_message-msgspra = sy-langu.
        ls_message-msgid = ls_huitem_message-msgid.
        ls_message-msgnr = ls_huitem_message-msgno.
        ls_message-msgv1 = ls_huitem_message-msgv1.
        ls_message-msgv2 = ls_huitem_message-msgv2.
        ls_message-msgv3 = ls_huitem_message-msgv3.
        ls_message-msgv4 = ls_huitem_message-msgv4.
        APPEND ls_message TO et_messages.
      ENDLOOP.
      RAISE error.
    ENDIF.

    ls_new_values-hdl_unit_itid = ls_vekp-venum.
    ls_new_values-hdl_unit_exid = i_exidv.

    ls_new_values-field_name    = 'WERKS'.
    ls_new_values-field_value   = i_werks.
    APPEND ls_new_values TO lt_changed.

    ls_new_values-field_name    = 'LGORT'.
    ls_new_values-field_value   = i_lgort.
    APPEND ls_new_values TO lt_changed.

** Update de Cabeçalho
***********************************************************************
    CALL FUNCTION 'HU_HEADER_UPDATE'
      EXPORTING
        it_new_values = lt_changed
      IMPORTING
        et_messages   = lt_huitem_messages
      EXCEPTIONS
        not_possible  = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      LOOP AT lt_huitem_messages INTO ls_huitem_message.
        ls_message-msgtyp = 'E'.
        ls_message-msgspra = sy-langu.
        ls_message-msgid = ls_huitem_message-msgid.
        ls_message-msgnr = ls_huitem_message-msgno.
        ls_message-msgv1 = ls_huitem_message-msgv1.
        ls_message-msgv2 = ls_huitem_message-msgv2.
        ls_message-msgv3 = ls_huitem_message-msgv3.
        ls_message-msgv4 = ls_huitem_message-msgv4.
        APPEND ls_message TO et_messages.
      ENDLOOP.
      RAISE error.
    ENDIF.
  ENDIF.

***********************************************************************
** ITEMS
***********************************************************************
  IF i_itm_change EQ abap_true.
    SELECT * FROM vepo
             INTO TABLE lt_vepo
             WHERE venum = ls_vekp-venum.

    LOOP AT lt_vepo INTO ls_vepo.
      CLEAR ls_item.

      ls_item-venum        = ls_vepo-venum.
      ls_item-vepos        = ls_vepo-vepos.
      ls_item-change_werks = abap_true.
      ls_item-change_lgort = abap_true.

      ls_item-werks = i_werks.
      ls_item-lgort = i_lgort.

      CALL FUNCTION 'HU_ITEM_CHANGE'
        EXPORTING
          is_item = ls_item
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.
    ENDLOOP.

    CALL FUNCTION 'HU_POST'
      EXPORTING
        if_synchron   = abap_true
        if_commit     = abap_false
        is_object     = ls_object
      IMPORTING
        et_messages   = lt_huitem_messages
      EXCEPTIONS
        error_message = 99.

    IF sy-subrc <> 0.
      LOOP AT lt_huitem_messages INTO ls_huitem_message.
        ls_message-msgtyp = 'E'.
        ls_message-msgspra = sy-langu.
        ls_message-msgid = ls_huitem_message-msgid.
        ls_message-msgnr = ls_huitem_message-msgno.
        ls_message-msgv1 = ls_huitem_message-msgv1.
        ls_message-msgv2 = ls_huitem_message-msgv2.
        ls_message-msgv3 = ls_huitem_message-msgv3.
        ls_message-msgv4 = ls_huitem_message-msgv4.
        APPEND ls_message TO et_messages.
      ENDLOOP.
      RAISE error.
    ENDIF.
  ENDIF.

  IF i_commit EQ abap_true.
    COMMIT WORK.
    ROLLBACK WORK. "Faz Desbloqueio de HU
  ENDIF.


ENDFUNCTION.
