FUNCTION zwm_to_set_let_down_priority.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(I_TANUM) TYPE  TANUM OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  CLEAR et_messages.

  CALL FUNCTION 'ZWM_TAPRI_CHANGE'
    EXPORTING
      i_lgnum     = i_lgnum
      i_tanum     = i_tanum
      i_tapri     = gc_tapri_let_down_pri
      i_commit    = i_commit
    IMPORTING
      et_messages = et_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.


  IF sy-subrc <> 0.
    RAISE error.
  ENDIF.

ENDFUNCTION.
