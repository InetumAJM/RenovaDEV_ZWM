*                                                                      *
*    *************************************************************     *
*    *  ROFF - Consultoria em Tecnologia de Informação           *     *
*    *                                                           *     *
*    *                          SAP                              *     *
*    *************************************************************     *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMPREP0129                                              *
* Nm.Cliente: Renova                                                   *
* Descrição.: Grupo/Remessa - Ajuste de SSCC                           *
* Criado por: Ricardo Sousa                                            *
* Criado em.: 31/10/2013                                               *
* Tipo PRG..: REP                                                      *
************************************************************************
REPORT zwmrep0129 MESSAGE-ID zwm001.

INCLUDE zwmrep0129_c01.
INCLUDE zwmrep0129_top.
INCLUDE zwmrep0129_f01.
INCLUDE zwmrep0129_i01.
INCLUDE zwmrep0129_o01.

START-OF-SELECTION.
  PERFORM get_whs.
*  PERFORM get_parameters.
*  PERFORM get_data.

*  CLEAR gt_alv_tree.
*  gt_alv_tree-lgnum  = '910'.
*  gt_alv_tree-lnumt  = 'C. Dist.  - Torres Novas'.
*  gt_alv_tree-exidv  = '00356010459000500112'.
*  APPEND gt_alv_tree.
*
*  gt_alv_tree-exidv  = '00356010459000500129'.
*  DO 30 TIMES.
*    CLEAR gt_alv_tree.
*    gt_alv_tree-lgnum  = '910'.
*    gt_alv_tree-lnumt  = 'C. Dist.  - Torres Novas'.
*
*    gt_alv_tree-exidv  = sy-index.
*    APPEND gt_alv_tree.
*  ENDDO.

END-OF-SELECTION.

  CALL SCREEN 0001.
