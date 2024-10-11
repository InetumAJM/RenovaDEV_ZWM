FUNCTION ZWM_MODIFY_ZWM016.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) TYPE  LGNUM
*"  TABLES
*"      L_ZWM016 STRUCTURE  ZWM016
*"  EXCEPTIONS
*"      ERROR_UPDATE_TABLE
*"----------------------------------------------------------------------

  IF NOT L_ZWM016[] IS INITIAL.
    MODIFY ZWM016 FROM TABLE L_ZWM016.
    IF SY-SUBRC = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

ENDFUNCTION.
