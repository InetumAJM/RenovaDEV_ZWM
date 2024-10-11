FUNCTION zwm_estorna_doc_material.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MBLNR) LIKE  MSEG-MBLNR
*"     REFERENCE(MJAHR) LIKE  MSEG-MJAHR
*"  EXPORTING
*"     REFERENCE(DOCUMENTO) LIKE  BAPI2017_GM_HEAD_RET STRUCTURE
*"        BAPI2017_GM_HEAD_RET
*"  TABLES
*"      RETURN STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      ERRO_COMMIT
*"----------------------------------------------------------------------

  DATA: return1 LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        return2 LIKE bapiret2.

  DATA: l_doc TYPE bapi2017_gm_head_02-mat_doc,
        l_ano TYPE bapi2017_gm_head_02-doc_year.

  FREE: return1, return2.
  CLEAR: return1, return2, l_doc, l_ano.

  l_doc = mblnr.
  l_ano = mjahr.

  CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
    EXPORTING
      materialdocument    = l_doc
      matdocumentyear     = l_ano
*      goodsmvt_pstng_date = sy-datum
    IMPORTING
      goodsmvt_headret    = documento
    TABLES
      return              = return1.

  LOOP AT return1.
    MOVE return1-type TO return-msgtyp.
    MOVE return1-id TO return-msgid.
    MOVE return1-number TO return-msgnr.
    MOVE return1-message_v1 TO return-msgv1.
    MOVE return1-message_v2 TO return-msgv2.
    MOVE return1-message_v3 TO return-msgv3.
    MOVE return1-message_v4 TO return-msgv4.
    APPEND return.
  ENDLOOP.

  CLEAR: return.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    IMPORTING
      return = return2.

  CHECK NOT return2 IS INITIAL.
  RAISE erro_commit.

ENDFUNCTION.
