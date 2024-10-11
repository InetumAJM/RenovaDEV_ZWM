************************************************************************
*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0123                                               *
* Nm.Cliente: Renova                                                   *
* Descrição.: Criação e impressão SSCC                                 *
* Criado por: Sérgio Garcias                                           *
* Criado em.: 17/09/2009                                               *
* Tipo PRG..: Executável                                               *
************************************************************************
REPORT zwmrep0123 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0123top.

START-OF-SELECTION.
  IF sy-tcode      = 'ZWM126'. "
    gv_tcode = 'A'.
    n_copias = 1.
  ELSEIF sy-tcode  = 'ZWM126_A'. "Adicionar a tabela ZWM013
    gv_tcode = 'B'.
    n_copias = 3.
  ENDIF.

  PERFORM find_whs.

  PERFORM get_customizing.

END-OF-SELECTION.

  SET SCREEN setscreen1.

  INCLUDE zwmrep0123o01.
  INCLUDE zwmrep0123i01.
  INCLUDE zwmrep0123f01.
