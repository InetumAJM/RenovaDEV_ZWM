FUNCTION zwm_refresh_buffer.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WHS) LIKE  LTAP-LGNUM
*"  TABLES
*"      TI_ZWM001 STRUCTURE  ZWM001 OPTIONAL
*"----------------------------------------------------------------------


** Parametrizações globais
  IF ti_zwm001[] IS INITIAL.

    SELECT * FROM zwm001 INTO CORRESPONDING FIELDS OF TABLE ti_zwm001
              WHERE armazem = whs.

    SORT ti_zwm001 BY armazem processo parametro.

  ENDIF.

** Parametrizações Equipamentos / Queues
  IF ti_zwm010[] IS INITIAL.

    SELECT * FROM zwm010 INTO CORRESPONDING FIELDS OF TABLE ti_zwm010
              WHERE armazem = whs.

  ENDIF.


ENDFUNCTION.
