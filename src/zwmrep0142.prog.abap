*&---------------------------------------------------------------------*
*& Report  ZWMREP0142
*&
************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0142                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Acertos de Palete no 916                                 *
* Criado por: Alexandre Morgado                                        *
* Criado em.: 16/05/2022                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT ZWMREP0142 MESSAGE-ID zwmmsg001.

INCLUDE ZWMREP0142_TOP.
INCLUDE ZWMREP0142_SCR.
INCLUDE ZWMREP0142_CLA.

* Secção Principal
START-OF-SELECTION.

  CALL METHOD zcl_acerto_pal_916=>run.
