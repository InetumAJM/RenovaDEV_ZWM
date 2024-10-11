*----------------------------------------------------------------------*
***INCLUDE LZWM082I01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  VALIDA_DTINI  INPUT
*&---------------------------------------------------------------------*
MODULE valida_dtini INPUT.

  IF zwm082-dtini IS NOT INITIAL.
    CLEAR zwm082-weekday.
    CLEAR gs_9500-weekday_name.
    IF zwm082-dtfim IS INITIAL.
      zwm082-dtfim = zwm082-dtini.
    ENDIF.
    IF zwm082-hrini EQ space.
      zwm082-hrini = '000000'.
    ENDIF.
    IF zwm082-hrfim EQ space.
      zwm082-hrfim = '235959'. "zwm082-hrini.
    ENDIF.
  ELSE.
    CLEAR zwm082-dtfim.
    CLEAR zwm082-hrfim.
    CLEAR zwm082-hrini.
  ENDIF.

  zwm082-tstamp_ini = zwm082-dtini && zwm082-hrini.
  zwm082-tstamp_fim = zwm082-dtfim && zwm082-hrfim.

  IF zwm082-dtini GT zwm082-dtfim.
    MESSAGE 'Corrigir Datas Inínio e Fim' TYPE 'E'.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDA_DTFIM  INPUT
*&---------------------------------------------------------------------*
MODULE valida_dtfim INPUT.

  IF zwm082-dtfim IS NOT INITIAL.
    CLEAR zwm082-weekday.
    CLEAR gs_9500-weekday_name.
    IF zwm082-dtini IS INITIAL.
      zwm082-dtini = zwm082-dtfim.
    ENDIF.

    IF zwm082-hrfim EQ space.
      zwm082-hrfim = '235959'.                              "'000000'.
    ENDIF.
    IF zwm082-hrini EQ space.
      zwm082-hrini = '000000'. "zwm082-hrfim.
    ENDIF.

  ELSE.
    CLEAR zwm082-dtini.
    CLEAR zwm082-hrfim.
    CLEAR zwm082-hrini.
  ENDIF.

  zwm082-tstamp_ini = zwm082-dtini && zwm082-hrini.
  zwm082-tstamp_fim = zwm082-dtfim && zwm082-hrfim.

  IF zwm082-dtini GT zwm082-dtfim.
    MESSAGE 'Corrigir Datas Inínio e Fim' TYPE 'E'.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDA_WEEKDAY  INPUT
*&---------------------------------------------------------------------*
MODULE valida_weekday INPUT.

  CLEAR gs_9500-weekday_name.

  IF zwm082-weekday IS NOT INITIAL.
    CLEAR zwm082-dtini.
    CLEAR zwm082-dtfim.
    CLEAR zwm082-hrfim.
    CLEAR zwm082-hrini.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_LOG  INPUT
*&---------------------------------------------------------------------*
MODULE set_log INPUT.

  zwm082-erdat = sy-datum.
  zwm082-erzet = sy-uzeit.
  zwm082-uname = sy-uname.

ENDMODULE.
