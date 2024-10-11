************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0139                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Eliminar SSCC de Pulmão                                  *
* Criado por: Ricardo Sousa                                            *
* Criado em.: 02/10/2019                                               *
* Tipo PRG..: Report                                                   *
************************************************************************
REPORT  zwmrep0139.

INCLUDE zwmrep0139_top.
INCLUDE zwmrep0139_c01.
INCLUDE zwmrep0139_o01.
INCLUDE zwmrep0139_i01.
INCLUDE zwmrep0139_f01.

INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_output.

AT SELECTION-SCREEN.
*  PERFORM user_command_1000.

START-OF-SELECTION.
  PERFORM get_data.

END-OF-SELECTION.

  PERFORM process_data.
