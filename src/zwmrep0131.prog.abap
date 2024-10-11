*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMPREP0131                                              *
* Nm.Cliente: Renova                                                   *
* Descrição.: Acerto PKL                                               *
* Criado por: Sérgio Garcias  s                                        *
* Criado em.: 07/07/2015                                               *
* Tipo PRG..: REP                                                      *
************************************************************************
REPORT  zwmrep0131.

INCLUDE zwmrep0131_d01.
INCLUDE zwmrep0131_f01.

INITIALIZATION.

START-OF-SELECTION.

  PERFORM inv.


END-OF-SELECTION.
