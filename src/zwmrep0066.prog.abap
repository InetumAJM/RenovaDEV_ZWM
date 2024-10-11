************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0066                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Consulta de Stocks                                       *
* Criado por: Fernando Lopes                                           *
* Criado em.: 11/01/2006                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0066 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0066_top.

START-OF-SELECTION.
  PERFORM find_whs.

END-OF-SELECTION.

  SET SCREEN setscreen1.

  INCLUDE zwmrep0066_o01.
  INCLUDE zwmrep0066_i01.
  INCLUDE zwmrep0066_f01.
