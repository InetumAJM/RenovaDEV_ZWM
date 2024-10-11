"Name: \PR:SAPLL03B\FO:DATEN_LTAK_TA_ZUR_LE\SE:END\EI
ENHANCEMENT 0 ZWMFR_002.
  FIELD-SYMBOLS: <lv_tapri> TYPE ANY.

  DO 1 TIMES.
*   Prioridade
    ASSIGN ('I_TAPRI')  TO <lv_tapri>.
    CHECK <lv_tapri> IS ASSIGNED.

    CHECK NOT <lv_tapri> IS INITIAL.

    ltak-tapri = <lv_tapri>.
  ENDDO.

ENDENHANCEMENT.
