FUNCTION z_wmfr_save_zwm013 .
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  TABLES
*"      IT_ZWM013_DEL STRUCTURE  ZWM013 OPTIONAL
*"      IT_ZWM013_INS STRUCTURE  ZWM013 OPTIONAL
*"      IT_ZWM013_UPD STRUCTURE  ZWM013 OPTIONAL
*"----------------------------------------------------------------------

  " delete ZWM013 entries
  IF it_zwm013_del[] IS NOT INITIAL.
    SORT it_zwm013_del.

    DELETE zwm013 FROM TABLE it_zwm013_del.
  ENDIF.

  " insert ZWM013 entries
  IF it_zwm013_ins[] IS NOT INITIAL.
    SORT it_zwm013_ins.

    INSERT zwm013 FROM TABLE it_zwm013_ins ACCEPTING DUPLICATE KEYS.
  ENDIF.

  " update ZWM011 entries
  IF it_zwm013_upd[] IS NOT INITIAL.
    SORT it_zwm013_upd.

    UPDATE zwm013 FROM TABLE it_zwm013_upd.
  ENDIF.

ENDFUNCTION.
