FUNCTION zwm_to_create_dn.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"     REFERENCE(I_SQUIT) TYPE  RL03T-SQUIT DEFAULT 'X'
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_TEILK) TYPE  T340DTEILV OPTIONAL
*"     REFERENCE(IT_DELIT) TYPE  L03B_DELIT_T OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_TANUM) TYPE  TANUM
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------


  DATA: ls_message TYPE bdcmsgcoll.

***********************************************************************
  CLEAR: et_messages, e_tanum.


  CALL FUNCTION 'L_TO_CREATE_DN'
    EXPORTING
      i_lgnum                    = i_lgnum
      i_vbeln                    = i_vbeln
      i_refnr                    = i_refnr
      i_squit                    = i_squit
      i_commit_work              = i_commit
      i_teilk                    = i_teilk
      it_delit                   = it_delit
    IMPORTING
      e_tanum                    = e_tanum
    EXCEPTIONS
      foreign_lock               = 1
      dn_completed               = 2
      partial_delivery_forbidden = 3
      xfeld_wrong                = 4
      ldest_wrong                = 5
      drukz_wrong                = 6
      dn_wrong                   = 7
      squit_forbidden            = 8
      no_to_created              = 9
      teilk_wrong                = 10
      update_without_commit      = 11
      no_authority               = 12
      no_picking_allowed         = 13
      dn_hu_not_choosable        = 14
      input_error                = 15
      error_message              = 16
      OTHERS                     = 17.

  IF sy-subrc <> 0.
    ls_message-msgtyp  = sy-msgty.
    ls_message-msgid   = sy-msgid.
    ls_message-msgnr   = sy-msgno.
    ls_message-msgv1   = sy-msgv1.
    ls_message-msgv2   = sy-msgv2.
    ls_message-msgv3   = sy-msgv3.
    ls_message-msgv4   = sy-msgv4.
    APPEND ls_message TO et_messages.

    ROLLBACK WORK.

    RAISE error.
  ENDIF.

ENDFUNCTION.
