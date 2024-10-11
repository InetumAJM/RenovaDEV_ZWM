*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0087                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Transferências Entre Centros/Depósitos                   *
* Criado por: Ricardo Sousa                                            *
* Criado em.: 01/14/2014                                               *
* Tipo PRG..: RF                                                       *
************************************************************************

REPORT zwmrep0087 MESSAGE-ID zwm001.

INCLUDE zwmrep0087_top.
INCLUDE zwmrep0087_o01.
INCLUDE zwmrep0087_i01.
INCLUDE zwmrep0087_f01.

START-OF-SELECTION.
  PERFORM get_whs.
  PERFORM get_parameters.
  PERFORM get_index.

END-OF-SELECTION.

  SET SCREEN setscreen1.
