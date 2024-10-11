************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0148                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Unlock/Lock SU de Automático (WCS)                       *
* Criado por: Ricardo Sousa (INETUM)                                   *
* Criado em.: 13/11/2023                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0148 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0148_top.
INCLUDE zwmrep0148_c01.
INCLUDE zwmrep0148_o01.
INCLUDE zwmrep0148_i01.
INCLUDE zwmrep0148_f01.

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
