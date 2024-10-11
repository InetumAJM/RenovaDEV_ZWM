************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0132                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Saidas de Materiais para CC/ordem                        *
* Criado por: Sérgio Garcias                                           *
* Criado em.: 09/02/2018                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0132 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0132_top.

START-OF-SELECTION.
  PERFORM find_whs.

  PERFORM get_customizing.

END-OF-SELECTION.

  SET SCREEN setscreen1.

  INCLUDE zwmrep0132_o01.
  INCLUDE zwmrep0132_i01.
  INCLUDE zwmrep0132_f01.
