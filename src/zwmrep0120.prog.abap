************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0120                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Entradas / Saidas Manipulados                            *
* Criado por: Sérgio Garcias                                           *
* Criado em.: 25/08/2009                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0120 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0120_top.

START-OF-SELECTION.
  PERFORM find_whs.

  PERFORM get_customizing.

END-OF-SELECTION.

  SET SCREEN setscreen1.

  INCLUDE zwmrep0120_o01.
  INCLUDE zwmrep0120_i01.
  INCLUDE zwmrep0120_f01.
