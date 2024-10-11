FUNCTION z_wmfr_save_zwm020 .
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  TABLES
*"      IT_ZWM020_DEL STRUCTURE  ZWM020 OPTIONAL
*"      IT_ZWM020_INS STRUCTURE  ZWM020 OPTIONAL
*"      IT_ZWM020_UPD STRUCTURE  ZWM020 OPTIONAL
*"----------------------------------------------------------------------

  " delete ZWM020 entries
  IF it_zwm020_del[] IS NOT INITIAL.
    SORT it_zwm020_del.

    DELETE zwm020 FROM TABLE it_zwm020_del.
  ENDIF.

  " insert ZWM020 entries
  IF it_zwm020_ins[] IS NOT INITIAL.
    SORT it_zwm020_ins.

    INSERT zwm020 FROM TABLE it_zwm020_ins ACCEPTING DUPLICATE KEYS.
  ENDIF.

  " update ZWM020 entries
  IF it_zwm020_upd[] IS NOT INITIAL.
    SORT it_zwm020_upd.

    UPDATE zwm020 FROM TABLE it_zwm020_upd.
  ENDIF.
ENDFUNCTION.
