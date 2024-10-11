FUNCTION zwm_hu_unpack.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_EXIDV) TYPE  EXIDV
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: lt_hunumbers TYPE TABLE OF bapihunumber,
        lt_return    TYPE TABLE OF bapiret2,
        lt_huitem    TYPE TABLE OF bapihuitem.

  DATA: ls_message    TYPE bdcmsgcoll,
        ls_return     TYPE bapiret2,
        ls_huitem     TYPE bapihuitem,
        ls_hunumber   TYPE bapihunumber,
        ls_itemunpack TYPE bapihuitmunpack.



  ls_hunumber-hu_exid = i_exidv.
  APPEND ls_hunumber TO lt_hunumbers.


** Items de HU
***********************************************************************
  CALL FUNCTION 'BAPI_HU_GETLIST'
    EXPORTING
      onlykeys  = abap_false
    TABLES
      hunumbers = lt_hunumbers
      huitem    = lt_huitem
      return    = lt_return.


  LOOP AT lt_return INTO ls_return WHERE type EQ 'E' OR
                                         type EQ 'A'.
    ls_message-msgtyp = ls_return-type.
    ls_message-msgid  = ls_return-id.
    ls_message-msgnr  = ls_return-number.
    ls_message-msgv1  = ls_return-message_v1.
    ls_message-msgv2  = ls_return-message_v2.
    ls_message-msgv3  = ls_return-message_v3.
    ls_message-msgv4  = ls_return-message_v4.
    APPEND ls_message TO et_messages.
  ENDLOOP.

  IF sy-subrc EQ 0.
    RAISE error.
  ENDIF.


** Desembalar todo o Conteudo da HU
***********************************************************************

  LOOP AT lt_huitem INTO ls_huitem.
    CLEAR: ls_itemunpack, lt_return.

    MOVE-CORRESPONDING ls_huitem TO ls_itemunpack.

    CALL FUNCTION 'BAPI_HU_UNPACK'
      EXPORTING
        hukey      = i_exidv
        itemunpack = ls_itemunpack
      TABLES
        return     = lt_return.

    LOOP AT lt_return INTO ls_return WHERE type EQ 'E' OR
                                           type EQ 'A'.
      ls_message-msgtyp = ls_return-type.
      ls_message-msgid  = ls_return-id.
      ls_message-msgnr  = ls_return-number.
      ls_message-msgv1  = ls_return-message_v1.
      ls_message-msgv2  = ls_return-message_v2.
      ls_message-msgv3  = ls_return-message_v3.
      ls_message-msgv4  = ls_return-message_v4.
      APPEND ls_message TO et_messages.
    ENDLOOP.

    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      RAISE error.
    ENDIF.

  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
ENDFUNCTION.
