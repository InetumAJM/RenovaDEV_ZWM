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
* Descrição.: Consulta de conteúdo de palete                           *
* Criado por: Fernando Lopes                                           *
* Criado em.: 13/01/2006                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0070 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0070_top.

START-OF-SELECTION.
  PERFORM find_whs.

END-OF-SELECTION.

  SET SCREEN setscreen1.

  INCLUDE zwmrep0070_o01.
  INCLUDE zwmrep0070_i01.
  INCLUDE zwmrep0070_f01.
