************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0140                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: RF - Listagem de Material / Posições de picking          *
* Criado por: Ricardo Sousa                                            *
* Criado em.: 18/11/2021                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0140 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0140_top.
INCLUDE zwmrep0140_o01.
INCLUDE zwmrep0140_i01.
INCLUDE zwmrep0140_f01.

START-OF-SELECTION.
  PERFORM find_whs.
  PERFORM get_customizing.

END-OF-SELECTION.

  PERFORM set_screen.
