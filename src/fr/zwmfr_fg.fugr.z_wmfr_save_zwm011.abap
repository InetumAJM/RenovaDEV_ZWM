FUNCTION z_wmfr_save_zwm011 .
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  TABLES
*"      IT_ZWM011_DEL STRUCTURE  ZWM011 OPTIONAL
*"      IT_ZWM011_INS STRUCTURE  ZWM011 OPTIONAL
*"      IT_ZWM011_UPD STRUCTURE  ZWM011 OPTIONAL
*"----------------------------------------------------------------------

  " delete ZWM011 entries
  IF it_zwm011_del[] IS NOT INITIAL.
    SORT it_zwm011_del.

    DELETE zwm011 FROM TABLE it_zwm011_del.
  ENDIF.

  " insert ZWM011 entries
  IF it_zwm011_ins[] IS NOT INITIAL.
    SORT it_zwm011_ins.

    INSERT zwm011 FROM TABLE it_zwm011_ins ACCEPTING DUPLICATE KEYS.
  ENDIF.

  " update ZWM011 entries
  IF it_zwm011_upd[] IS NOT INITIAL.
    SORT it_zwm011_upd.

    UPDATE zwm011 FROM TABLE it_zwm011_upd.
  ENDIF.

ENDFUNCTION.
