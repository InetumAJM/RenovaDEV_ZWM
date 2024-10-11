*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMPREP0130                                              *
* Nm.Cliente: Renova                                                   *
* Descrição.: Inventário PKL                                           *
* Criado por: Marcelo Rodrigues                                        *
* Criado em.: 25/11/2013                                               *
* Tipo PRG..: REP                                                      *
************************************************************************
REPORT  ZWMREP0130.

INCLUDE ZWMREP0130_d01.
INCLUDE ZWMREP0130_f01.

INITIALIZATION.
PERFORM init.


START-OF-SELECTION.
*  PERFORM get_whs.
PERFORM inv.


END-OF-SELECTION.
