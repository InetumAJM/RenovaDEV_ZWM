*&---------------------------------------------------------------------*
*&  Include           ZWMREP0128_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_of_selection.
  DATA: lt_messages TYPE tab_bdcmsgcoll.

  DATA: ls_message TYPE bdcmsgcoll.

  CHECK NOT s_vbeln[] IS INITIAL.

  CALL FUNCTION 'Z_WM_SWAP_DELIVERY_FROM_TRANSP'
    EXPORTING
      i_tknum_o   = p_doc_o
      i_tknum_d   = p_doc_d
      ir_vbeln    = s_vbeln[]
    IMPORTING
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    READ TABLE lt_messages INTO ls_message INDEX 1.

    MESSAGE ID ls_message-msgid TYPE 'I' NUMBER ls_message-msgnr
            DISPLAY LIKE ls_message-msgtyp
            WITH ls_message-msgv1 ls_message-msgv2
                 ls_message-msgv3 ls_message-msgv4.
  ENDIF.
ENDFORM.                    " START_OF_SELECTION
