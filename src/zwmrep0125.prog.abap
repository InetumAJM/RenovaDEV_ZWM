***********************************************************************
* TIPO           : Executável
* CRIADO POR     : ROFF - Diogo Silva
* DATA           : 16/05/2012
* DESCRIÇÃO      : Relatorio de Controlo de Paletes
***********************************************************************
* ALTERAÇÃO      :
* DATA           :
* AUTOR          :
* DESCRIÇÃO      :
* PROCURAR <TAG> :
***********************************************************************

REPORT  zwmrep0125 MESSAGE-ID zwm001.

** Includes
***********************************************************************
INCLUDE zwmrep0125_c01.
INCLUDE zwmrep0125_d01.
INCLUDE zwmrep0125_d02.
INCLUDE zwmrep0125_f01.
INCLUDE zwmrep0125_i01.
INCLUDE zwmrep0125_o01.

** F4 HELP
***********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f4_layout CHANGING p_layout.


** Selection
***********************************************************************
START-OF-SELECTION.
  PERFORM call_screen_0001.
