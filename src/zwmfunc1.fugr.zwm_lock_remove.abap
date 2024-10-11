FUNCTION zwm_lock_remove.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_KEYWORD) TYPE  KEYWORD
*"     REFERENCE(I_SCOPE) TYPE  C DEFAULT '1'
*"----------------------------------------------------------------------
  CHECK NOT is_keyword IS INITIAL.

** Desbloqueia
***********************************************************************
  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword = 'X'
      keyword_     = is_keyword-keyword
      x_datum      = 'X'
      x_zeit       = 'X'
      x_pos        = 'X'
      _scope       = i_scope.


ENDFUNCTION.
