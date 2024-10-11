FUNCTION z_wmfr_save_zwm028 .
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  TABLES
*"      IT_ZWM028_DEL STRUCTURE  ZWM028 OPTIONAL
*"      IT_ZWM028_INS STRUCTURE  ZWM028 OPTIONAL
*"      IT_ZWM028_UPD STRUCTURE  ZWM028 OPTIONAL
*"----------------------------------------------------------------------

  " delete ZWM028 entries
  IF it_zwm028_del[] IS NOT INITIAL.
    SORT it_zwm028_del.

    DELETE zwm028 FROM TABLE it_zwm028_del.
  ENDIF.

  " insert ZWM028 entries
  IF it_zwm028_ins[] IS NOT INITIAL.
    SORT it_zwm028_ins.

    INSERT zwm028 FROM TABLE it_zwm028_ins ACCEPTING DUPLICATE KEYS.
  ENDIF.

  " update ZWM028 entries
  IF it_zwm028_upd[] IS NOT INITIAL.
    SORT it_zwm028_upd.

    UPDATE zwm028 FROM TABLE it_zwm028_upd.
  ENDIF.
ENDFUNCTION.
