*&---------------------------------------------------------------------*
*& Report  ZWMREP0090
*& Objetivo : Mapa de Planeamento
*& Transação: ZWM166
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zwmrep0090 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0091_top.
INCLUDE zwmrep0091_top_2.
INCLUDE zwmrep0091_cls_def.
INCLUDE zwmrep0091_cls_imp.
INCLUDE zwmrep0091_f01.
INCLUDE zwmrep0091_o01.
INCLUDE zwmrep0091_i01.

INITIALIZATION.
  lcl_alv=>inicialization( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.
  p_varia = lcl_alv=>get_variante( ).

START-OF-SELECTION.
  IF p_explvl GT 5.
    p_explvl = 5.
  ENDIF.

  lcl_alv=>show_progress( iv_perc = 5 iv_text = text-003 ). "Selecção de Dados...

  lcl_alv=>set_lock(  ).

***  PERFORM get_dados.

  lcl_alv=>get_dados(  ).

  lcl_alv=>remove_lock(  ).

  CALL SCREEN 5000.
