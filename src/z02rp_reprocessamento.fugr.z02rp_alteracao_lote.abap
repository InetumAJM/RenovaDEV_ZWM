FUNCTION Z02RP_ALTERACAO_LOTE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  CHANGING
*"     REFERENCE(CT_MODIF_LOTE) TYPE  Z02RP_TT_ALTERACAO_LOTE
*"----------------------------------------------------------------------
  DATA: lt_bdbatch  TYPE TABLE OF BDBATCH.

  DATA: ls_alt_lote   LIKE LINE OF gt_alteracao_lote,
        ls_lote_disp  LIKE LINE OF gt_lotes_disp,
        ls_bdbatch    LIKE LINE OF lt_bdbatch.


  DATA: lv_total_disp TYPE BDCOM-ERFMG.

  gt_alteracao_lote = ct_modif_lote.

  LOOP AT gt_alteracao_lote INTO ls_alt_lote.
*Determinar o lote automáticamente
    CALL FUNCTION 'Z_02RP_DETERMINACAO_LOTE'
      EXPORTING
        matnr         = ls_alt_lote-matnr
        werks         = ls_alt_lote-werks
        lgort         = ls_alt_lote-lgort
        erfmg         = ls_alt_lote-menge_total
        erfme         = ls_alt_lote-meins
        lfdat         = ls_alt_lote-data
      IMPORTING
        total         = lv_total_disp
      TABLES
        e_bdbatch     = lt_bdbatch
      EXCEPTIONS
        erro_detlotes = 1
        OTHERS        = 2.

    LOOP AT lt_bdbatch into ls_bdbatch.
      MOVE-CORRESPONDING ls_bdbatch to ls_lote_disp.
      APPEND ls_lote_disp to gt_lotes_disp.
    ENDLOOP.
  ENDLOOP.

  CALL SCREEN 0100 STARTING AT 20 10
                     ENDING AT 100 20.

  ct_modif_lote = gt_alteracao_lote.

ENDFUNCTION.
