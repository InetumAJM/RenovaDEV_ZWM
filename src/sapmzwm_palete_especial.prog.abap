************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: SAPMZWM_PALETE_ESPECIAL                                  *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Manutenção da Tabela de Paletização Especial             *
* Criado por: Luís Rocha                                               *
* Criado em.: 06/12/2004                                               *
* Tipo PRG..: Pool de Módulos                                          *
************************************************************************

include mzwm_palete_especialtop.   "TOP
* (comuns)
include zwm_include_top.           "TOP Comum
include zwm_include_form.          "Rotinas Comuns
*
include mzwm_palete_especialf01.   "Rotinas I
include mzwm_palete_especialf02.   "Rotinas II
include mzwm_palete_especialo01.   "PBO
include mzwm_palete_especiali01.   "PAI
*
