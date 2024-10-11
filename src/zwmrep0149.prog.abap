************************************************************************
*      *********************************************************       *
*      *  INETUM - Consultoria em Tecnologia de Informação     *       *
*      *                                                       *       *
*      *                         SAP                           *       *
*      *********************************************************       *
*  Nome ABAP.: ZWMREP0149                                              *
*  Nm.Cliente: Renova                                                  *
*  Descrição.: Consulta de Stock no WCS                                *
*  Criado Por: Ricardo Sousa                                           *
*  Criado em.: 20/05/2024                                              *
*  Tipo......: Executável                                              *
************************************************************************
REPORT zwmrep0149 MESSAGE-ID zwmmsg001.

INCLUDE zwmrep0149_top.
INCLUDE zwmrep0149_c01.
INCLUDE zwmrep0149_f01.
INCLUDE zwmrep0149_o01.
INCLUDE zwmrep0149_i01.

INITIALIZATION.

** Start of Selection
************************************************************************
START-OF-SELECTION.
  PERFORM get_data.

END-OF-SELECTION.

  PERFORM list_data.
