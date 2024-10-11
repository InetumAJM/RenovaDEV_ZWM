"Name: \PR:SAPLL03A\FO:QUITTIERUNG_PRUEFEN\SE:BEGIN\EI
ENHANCEMENT 0 ZWM_007.


  FIELD-SYMBOLS: <lv_inventario> TYPE flag.

  DO 1 TIMES.
    ASSIGN ('(SAPLL03B)I_INVENT') TO <lv_inventario>.
    CHECK <lv_inventario> IS ASSIGNED.

    IF <lv_inventario> EQ 'X'.
      CLEAR: I_RL03A-VORGA.
    ENDIF.
  ENDDO.


ENDENHANCEMENT.
