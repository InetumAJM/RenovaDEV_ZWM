************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0069                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Consulta de material por posição de picking              *
* Criado por: Fernando Lopes                                           *
* Criado em.: 13/01/2006                                               *
* Tipo PRG..: Executável                                               *
*                                                                      *
************************************************************************

REPORT  zwmrf0006  MESSAGE-ID zwmmsg001.

INCLUDE ZWMREP0069_TOP. " global Data
INCLUDE ZWMREP0069_O01. " PBO-Modules
INCLUDE ZWMREP0069_I01. " PAI-Modules
INCLUDE ZWMREP0069_F01. " FORM-Routines

START-OF-SELECTION.
  PERFORM find_whs.

END-OF-SELECTION.

  PERFORM get_parametrizacao.

  SET SCREEN setscreen1.
