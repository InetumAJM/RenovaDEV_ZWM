FUNCTION z_wmfr_save_zwmfrt004 .
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  TABLES
*"      IT_ZWMFRT004_DEL STRUCTURE  ZWMFRT004 OPTIONAL
*"      IT_ZWMFRT004_INS STRUCTURE  ZWMFRT004 OPTIONAL
*"      IT_ZWMFRT004_UPD STRUCTURE  ZWMFRT004 OPTIONAL
*"----------------------------------------------------------------------

  " delete ZWMFRT004 entries
  IF it_zwmfrt004_del[] IS NOT INITIAL.
    SORT it_zwmfrt004_del.

    DELETE zwmfrt004 FROM TABLE it_zwmfrt004_del.
  ENDIF.

  " insert ZWMFRT004 entries
  IF it_zwmfrt004_ins[] IS NOT INITIAL.
    SORT it_zwmfrt004_ins.

    INSERT zwmfrt004 FROM TABLE it_zwmfrt004_ins ACCEPTING DUPLICATE KEYS.
  ENDIF.

  " update ZWMFRT004 entries
  IF it_zwmfrt004_upd[] IS NOT INITIAL.
    SORT it_zwmfrt004_upd.

    UPDATE zwmfrt004 FROM TABLE it_zwmfrt004_upd.
  ENDIF.

ENDFUNCTION.
