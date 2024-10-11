************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0144                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Entradas no Automático (WCS)                             *
* Criado por: Ricardo Sousa (INETUM)                                   *
* Criado em.: 22/09/2023                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0144 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0144_top.
INCLUDE zwmrep0144_c01.
INCLUDE zwmrep0144_o01.
INCLUDE zwmrep0144_i01.
INCLUDE zwmrep0144_f01.

INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN.
  PERFORM user_command_1000.

START-OF-SELECTION.
  PERFORM enqueue.

  PERFORM get_data.

END-OF-SELECTION.
  PERFORM process_data.
  PERFORM dequeue.
