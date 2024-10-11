************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0137                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: RF - Armazém Alugado França - Processo Packing           *
* Criado por: Ricardo Sousa                                            *
* Criado em.: 13/05/2019                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0137 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0137_top.
INCLUDE zwmrep0137_o01.
INCLUDE zwmrep0137_i01.
INCLUDE zwmrep0137_f01.

START-OF-SELECTION.
  PERFORM find_whs.

  PERFORM get_customizing.

END-OF-SELECTION.

  SET SCREEN setscreen1.
