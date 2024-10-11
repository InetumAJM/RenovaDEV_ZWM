************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0068                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Transferência de Paletes                                 *
* Criado por: Fernando Lopes                                           *
* Criado em.: 12/01/2006                                               *
* Tipo PRG..: Executável                                               *
*                                                                      *
************************************************************************
REPORT ZWMREP0068 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0068_top.  " global Data
INCLUDE zwmrep0068_o01.  " PBO-Modules
INCLUDE zwmrep0068_i01.  " PAI-Modules
INCLUDE zwmrep0068_f01.  " FORM-Routines

START-OF-SELECTION.
  PERFORM find_whs.

END-OF-SELECTION.

  PERFORM get_parametrizacao.

  SET SCREEN setscreen1.
