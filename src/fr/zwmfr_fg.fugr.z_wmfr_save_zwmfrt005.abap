FUNCTION z_wmfr_save_zwmfrt005 .
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  TABLES
*"      IT_ZWMFRT005_DEL STRUCTURE  ZWMFRT005 OPTIONAL
*"      IT_ZWMFRT005_INS STRUCTURE  ZWMFRT005 OPTIONAL
*"      IT_ZWMFRT005_UPD STRUCTURE  ZWMFRT005 OPTIONAL
*"----------------------------------------------------------------------

  " delete ZWMFRT005 entries
  IF it_zwmfrt005_del[] IS NOT INITIAL.
    SORT it_zwmfrt005_del.

    DELETE zwmfrt005 FROM TABLE it_zwmfrt005_del.
  ENDIF.

  " insert ZWMFRT005 entries
  IF it_zwmfrt005_ins[] IS NOT INITIAL.
    SORT it_zwmfrt005_ins.

    INSERT zwmfrt005 FROM TABLE it_zwmfrt005_ins ACCEPTING DUPLICATE KEYS.
  ENDIF.

  " update ZWMFRT005 entries
  IF it_zwmfrt005_upd[] IS NOT INITIAL.
    SORT it_zwmfrt005_upd.

    UPDATE zwmfrt005 FROM TABLE it_zwmfrt005_upd.
  ENDIF.
ENDFUNCTION.
