*&---------------------------------------------------------------------*
*& Report  ZWMREP0046                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZWMREP0046                              .

*-----------------------------------------------Dados Globais
include zdados0046.

*-----------------------------------------------Ecram de Selecção
include zecram0046.

start-of-selection.

perform get_dados.

perform trata_dados.

*---------------------------------------------Rotinas
include zforms0046.
