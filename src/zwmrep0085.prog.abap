*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0085                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: RF - Transferencia de SSCC para Remessa                  *
* Criado por: Marcelo Rodrigues                                        *
* Criado em.: 11/12/2013                                               *
* Tipo PRG..: RF                                                       *
************************************************************************
REPORT  ZWMREP0085.

INCLUDE ZWMREP0085_d01.
INCLUDE ZWMREP0085_i01.
INCLUDE ZWMREP0085_o01.
INCLUDE ZWMREP0085_f01.

START-OF-SELECTION.
  PERFORM get_whs.
  PERFORM get_parameters.
  PERFORM get_lost_data.

END-OF-SELECTION.

  SET SCREEN setscreen1.
