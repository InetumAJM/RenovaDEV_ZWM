FUNCTION ZWM_GOODSMVT_CANCEL .
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MBLNR) TYPE  MBLNR
*"     REFERENCE(I_MJAHR) TYPE  MJAHR
*"     REFERENCE(I_DATUM) TYPE  DATUM DEFAULT SY-DATUM
*"  EXPORTING
*"     REFERENCE(E_MBLNR) TYPE  MBLNR
*"     REFERENCE(E_MJAHR) TYPE  MJAHR
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"--------------------------------------------------------------------


  DATA: ls_doc_rev TYPE bapi2017_gm_head_ret,
        lt_return  TYPE TABLE OF bapiret2,
        ls_return  TYPE bapiret2,
        ls_message TYPE bdcmsgcoll.

***********************************************************************
  CLEAR: et_messages, e_mblnr, e_mjahr.

  CHECK NOT i_mblnr IS INITIAL AND
        NOT i_mjahr IS INITIAL.


** Cancela Documento de Material
***********************************************************************
  CALL FUNCTION 'HU_PACKING_REFRESH'. " Limpa Memoria standard


  CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
    EXPORTING
      materialdocument    = i_mblnr
      matdocumentyear     = i_mjahr
      goodsmvt_pstng_date = i_datum
    IMPORTING
      goodsmvt_headret    = ls_doc_rev
    TABLES
      return              = lt_return.

  e_mblnr = ls_doc_rev-mat_doc.
  e_mjahr = ls_doc_rev-doc_year.


  LOOP AT lt_return INTO ls_return WHERE type EQ 'E'
                                      OR type EQ 'A'.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ls_message-msgtyp = ls_return-type.
    ls_message-msgid  = ls_return-id.
    ls_message-msgnr  = ls_return-number.
    ls_message-msgv1  = ls_return-message_v1.
    ls_message-msgv2  = ls_return-message_v2.
    ls_message-msgv3  = ls_return-message_v3.
    ls_message-msgv4  = ls_return-message_v4.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDLOOP.


** Bapi Commit
***********************************************************************
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    IMPORTING
      return = ls_return.

  IF ls_return-type EQ 'E' OR ls_return-type EQ 'A'.
    ls_message-msgtyp = ls_return-type.
    ls_message-msgid  = ls_return-id.
    ls_message-msgnr  = ls_return-number.
    ls_message-msgv1  = ls_return-message_v1.
    ls_message-msgv2  = ls_return-message_v2.
    ls_message-msgv3  = ls_return-message_v3.
    ls_message-msgv4  = ls_return-message_v4.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.




ENDFUNCTION.
