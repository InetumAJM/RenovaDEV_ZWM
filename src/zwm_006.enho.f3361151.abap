"Name: \PR:SAPML03T\FO:T340_LESEN\SE:END\EI
ENHANCEMENT 0 ZWM_006.

  FIELD-SYMBOLS: <lv_inventario> TYPE flag.

  DO 1 TIMES.
    ASSIGN ('(SAPLL03B)I_INVENT') TO <lv_inventario>.
    CHECK <lv_inventario> IS ASSIGNED.

    IF <lv_inventario> EQ 'X'.
      t340-vorga = 'IV'.
      vorga = 'IV'.
    ENDIF.

    CLEAR <lv_inventario>.
  ENDDO.

ENDENHANCEMENT.
