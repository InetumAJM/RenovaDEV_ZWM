************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0136                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: RF - Armazém Alugado França - Processo Picking           *
* Criado por: Ricardo Sousa                                            *
* Criado em.: 30/04/2019                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0136 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0136_top.
INCLUDE zwmrep0136_o01.
INCLUDE zwmrep0136_i01.
INCLUDE zwmrep0136_f01.

START-OF-SELECTION.
  PERFORM find_whs.

  PERFORM get_customizing.

END-OF-SELECTION.

  PERFORM set_screen.
