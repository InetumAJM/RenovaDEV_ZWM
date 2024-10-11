************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0085                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: RF - Transferencia de quantidades parciais               *
* Criado por: Marcelo Rodrigues                                        *
* Criado em.: 16/12/2013                                               *
* Tipo PRG..: RF                                                       *
************************************************************************

REPORT  zwmrep0086.

INCLUDE zwmrep0086_d01.
INCLUDE zwmrep0086_i01.
INCLUDE zwmrep0086_o01.
INCLUDE zwmrep0086_f01.


START-OF-SELECTION.
  PERFORM get_whs.
  PERFORM get_parameters.

END-OF-SELECTION.
  SET SCREEN gv_setscreen1.
