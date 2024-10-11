FUNCTION zwm_delivery_set_ref_doc_mat.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN_VL
*"     REFERENCE(I_MBLNR) TYPE  MBLNR
*"     REFERENCE(I_MJAHR) TYPE  MJAHR
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: lt_extension1	TYPE TABLE OF bapiextc,
        lt_return	    TYPE TABLE OF bapiret2.

  DATA: lv_text TYPE lifex.

  DATA: ls_message        TYPE bdcmsgcoll,
        ls_header_data    TYPE bapiobdlvhdrchg,
        ls_header_control TYPE bapiobdlvhdrctrlchg,
        ls_zwm_007        TYPE zwm_007,
        ls_extension1	    TYPE bapiextc,
        ls_return	        TYPE bapiret2.

  CLEAR: et_messages.

  ls_header_data-deliv_numb = i_vbeln.
  ls_header_control-deliv_numb = i_vbeln.

  ls_zwm_007-vbeln = i_vbeln.

  CONCATENATE i_mblnr i_mjahr INTO ls_zwm_007-lifex SEPARATED BY '|'.

  ls_extension1-field1 = ls_zwm_007.

  APPEND ls_extension1 TO lt_extension1.


  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
    EXPORTING
      header_data    = ls_header_data
      header_control = ls_header_control
      delivery       = i_vbeln
    TABLES
      extension1     = lt_extension1
      return         = lt_return.


  LOOP AT lt_return INTO ls_return WHERE type = 'E' OR
                                         type = 'A'.
    ls_message-msgtyp  = ls_return-type.
    ls_message-msgspra = sy-langu.
    ls_message-msgid   = ls_return-id.
    ls_message-msgnr   = ls_return-number.
    ls_message-msgv1   = ls_return-message_v1.
    ls_message-msgv2   = ls_return-message_v2.
    ls_message-msgv3   = ls_return-message_v3.
    ls_message-msgv4   = ls_return-message_v3.
    APPEND ls_message TO et_messages.

  ENDLOOP.
  IF sy-subrc EQ 0.
    RAISE error.
  ENDIF.

  CHECK i_commit EQ abap_true.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
ENDFUNCTION.
