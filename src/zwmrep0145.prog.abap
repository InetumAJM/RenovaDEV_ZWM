
************************************************************************
*      *********************************************************       *
*      *  INETUM - Consultoria em Tecnologia de Informação     *       *
*      *                                                       *       *
*      *                         SAP                           *       *
*      *********************************************************       *
*  Nome ABAP.: ZWMREP0145                                              *
*  Nm.Cliente: Renova                                                  *
*  Descrição.: Cockpit de Paletização Especial                         *
*  Criado Por: Ricardo Sousa                                           *
*  Criado em.: 24/10/2023                                              *
*  Tipo......: Executável                                              *
************************************************************************
REPORT zwmrep0145 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0145_top.
INCLUDE zwmrep0145_c01.

INCLUDE zwmrep0145_f01.
INCLUDE zwmrep0145_o01.
INCLUDE zwmrep0145_i01.

INITIALIZATION.
  PERFORM init.

** Start of Selection
************************************************************************
START-OF-SELECTION.
  PERFORM get_parameters.
  PERFORM get_data.

END-OF-SELECTION.

  PERFORM list_data.
