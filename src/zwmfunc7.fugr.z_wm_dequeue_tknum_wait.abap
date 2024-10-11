FUNCTION z_wm_dequeue_tknum_wait.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TKNUM) TYPE  TKNUM
*"     REFERENCE(I_WAIT) TYPE  SYINDEX DEFAULT 20
*"----------------------------------------------------------------------

  DO 20 TIMES.
    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_EVVTTKE'
      EXPORTING
        mode_vttk      = 'E'
        mandt          = sy-mandt
        tknum          = i_tknum
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'DEQUEUE_EVVTTKE'
      EXPORTING
        mode_vttk = 'E'
        mandt     = sy-mandt
        tknum     = i_tknum.

    EXIT.
  ENDDO.



ENDFUNCTION.
