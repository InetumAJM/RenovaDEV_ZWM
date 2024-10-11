************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0121                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Consumo para a Ordem de Produção                         *
* Criado por: Ricardo Sousa                                            *
* Criado em.: 27/08/2009                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0121 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0121_top.
INCLUDE zwmrep0121_o01.
INCLUDE zwmrep0121_i01.
INCLUDE zwmrep0121_f01.


START-OF-SELECTION.
  PERFORM find_whs.

  PERFORM get_customizing.

END-OF-SELECTION.

  SET SCREEN setscreen1.
