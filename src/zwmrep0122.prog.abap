************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0122                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Devolução Restos                                         *
* Criado por: Sérgio Garcias                                           *
* Criado em.: 27/08/2009                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0122 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0122_top.

START-OF-SELECTION.
  PERFORM find_whs.

  PERFORM get_customizing.

END-OF-SELECTION.

  SET SCREEN setscreen1.

  INCLUDE zwmrep0122_o01.
  INCLUDE zwmrep0122_i01.
  INCLUDE zwmrep0122_f01.
