***********************************************************************
* TIPO           : Executável
* CRIADO POR     : ROFF - Diogo Silva
* DATA           : 16/05/2012
* DESCRIÇÃO      : Inventário ADHOC
***********************************************************************
* ALTERAÇÃO      :
* DATA           :
* AUTOR          :
* DESCRIÇÃO      :
* PROCURAR <TAG> :
***********************************************************************

REPORT zwmrep0124 MESSAGE-ID zwm001.

INCLUDE zwmrep0124_d01.
INCLUDE zwmrep0124_f01.
INCLUDE zwmrep0124_i01.
INCLUDE zwmrep0124_o01.

INITIALIZATION.
  PERFORM find_whs.
  PERFORM get_configuration.

START-OF-SELECTION.
  PERFORM call_screen_0001.
