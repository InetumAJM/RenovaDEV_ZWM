*----------------------------------------------------------------------*
* Include: ZWMFR0002_EVT
*----------------------------------------------------------------------*
* Description: RF - Entrada de Produto Acabado/Bobines PT
* RICEFW: WM.02/WM.03
*----------------------------------------------------------------------*
* Author........: [Pedro Silva] [ROFFD] [ROFF(SDF)]
*                 [Tiago Pateiro] [ROFFD] [ROFF(SDF)]
* Creation date:  2015-10-26
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM f_data_free.

START-OF-SELECTION.
  PERFORM f_data_free.
  PERFORM f_data_init.

END-OF-SELECTION.
  IF gv_haserror EQ abap_true.
    LEAVE TO SCREEN 0.
  ELSE.
    SET SCREEN gv_dynnr_1.
  ENDIF.
