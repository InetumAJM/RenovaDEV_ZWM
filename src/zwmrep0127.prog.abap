*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMMPRF013                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: RF - Transf. SU                                          *
* Criado por: Nuno Sarmento / Diogo Silva                              *
* Criado em.: 10/07/2012                                               *
* Tipo PRG..: RF                                                       *
************************************************************************

REPORT  zwmrep0127 MESSAGE-ID zwmmpmsg.

INCLUDE zwmrep0127_d01.
INCLUDE zwmrep0127_o01.
INCLUDE zwmrep0127_i01.
INCLUDE zwmrep0127_f01.

START-OF-SELECTION.
  PERFORM get_whs.
  PERFORM get_parameters.

END-OF-SELECTION.

  SET SCREEN setscreen1.
