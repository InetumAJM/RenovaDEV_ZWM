***********************************************************************
* Programa       : ZWMREP0151
* Transação      : ZWM169
* CRIADO POR     : Inetum - AndersonJM
* DATA           : 20/06/2024
* DESCRIÇÃO      : Entrada de Mercadoria de Produção Portugal
***********************************************************************
* ALTERAÇÃO      :
* DATA           :
* AUTOR          :
* DESCRIÇÃO      :
* PROCURAR <TAG> :
***********************************************************************
REPORT zwmrep0151 MESSAGE-ID zwmfr001.

INCLUDE zwmrep0151_d01.
INCLUDE zwmrep0151_d02.
INCLUDE zwmrep0151_f01.
INCLUDE zwmrep0151_o01.
INCLUDE zwmrep0151_i01.

INITIALIZATION.
  PERFORM initialization.

** Helps F4
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vhilm.
  PERFORM f4_vhilm CHANGING p_vhilm.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_aufnr.
  PERFORM f4_aufnr CHANGING p_aufnr.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fevor.
  PERFORM f4_fevor CHANGING p_fevor.

** Eventos
***********************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM selection_screen_pbo.

AT SELECTION-SCREEN.
  PERFORM selection_screen_pai.

START-OF-SELECTION.
  PERFORM start_of_selection.
