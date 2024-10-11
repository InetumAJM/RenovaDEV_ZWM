"Name: \PR:SAPLV56I_BAPI\FO:FILL_HUN_HDR_DATA_CHANGE\SE:END\EI
ENHANCEMENT 0 ZWM_004.

** ROFF - @ 18/06/2012
** Correcção de Bug SAP

*- loop over the item data for this header
  LOOP AT it_hun_itm INTO ls_hun_itm
    WHERE hdl_unit_into = ls_header_wa-venum.   " Correcção BUG SAP
    lv_idx_itm = sy-tabix.
   READ TABLE it_hun_itm_action INTO ls_hun_itm_action INDEX lv_idx_itm.
    IF sy-subrc NE 0.
      MESSAGE e099(vtbapi) WITH 'HUNITMACT' 'CHG' lv_idx_itm ''
        RAISING programm_error.
    ENDIF.
    cs_hun_wa-content-execute = gc_true.
    CALL FUNCTION 'MAP2I_BAPIDLVHDUNITM_TO_VERPO'
      EXPORTING
        bapidlvhdunitm            = ls_hun_itm
      CHANGING
        verpo                     = ls_item_wa
      EXCEPTIONS
        error_converting_iso_code = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      MESSAGE e104(vtbapi) WITH ls_hun_itm-hdl_unit_exid
        RAISING iso_conversion_failed.
    ENDIF.
    " MATERIAL_EXTERNAL ---> not supported
    " MATERIAL_GUID     ---> not supported
    " MATERIAL_VERSION  ---> not supported
    CASE ls_hun_itm_action-hdl_unit_into.
      WHEN gc_chg_change.
        ls_item_wa_put-venum_ob = ls_item_wa-venum_ob.
      WHEN gc_chg_delete.
        CLEAR ls_item_wa_put-venum_ob.
    ENDCASE.
    CASE ls_hun_itm_action-hdl_unit_no.
      WHEN gc_chg_change.
        ls_item_wa_put-venum = ls_item_wa-venum.
      WHEN gc_chg_delete.
        CLEAR ls_item_wa_put-venum.
    ENDCASE.
    CASE ls_hun_itm_action-hdl_unit_exid_into.
      WHEN gc_chg_change.
        ls_item_wa_put-exidv_ob = ls_item_wa-exidv_ob.
      WHEN gc_chg_delete.
        CLEAR ls_item_wa_put-exidv_ob.
    ENDCASE.
    CASE ls_hun_itm_action-hdl_unit_exid.
      WHEN gc_chg_change.
        ls_item_wa_put-exidv = ls_item_wa-exidv.
      WHEN gc_chg_delete.
        CLEAR ls_item_wa_put-exidv.
    ENDCASE.
    CASE ls_hun_itm_action-deliv_numb.
      WHEN gc_chg_change.
        ls_item_wa_put-vbeln = ls_item_wa-vbeln.
      WHEN gc_chg_delete.
        CLEAR ls_item_wa_put-vbeln.
    ENDCASE.
    CASE ls_hun_itm_action-deliv_item.
      WHEN gc_chg_change.
        ls_item_wa_put-posnr = ls_item_wa-posnr.
      WHEN gc_chg_delete.
        CLEAR ls_item_wa_put-posnr.
    ENDCASE.
    CASE ls_hun_itm_action-pack_qty.
      WHEN gc_chg_change.
        ls_item_wa_put-tmeng = ls_item_wa-tmeng.
      WHEN gc_chg_delete.
        CLEAR ls_item_wa_put-tmeng.
    ENDCASE.
    CASE ls_hun_itm_action-sales_unit.
      WHEN gc_chg_change.
        ls_item_wa_put-vrkme = ls_item_wa-vrkme.
      WHEN gc_chg_delete.
        CLEAR ls_item_wa_put-vrkme.
    ENDCASE.
    CASE ls_hun_itm_action-material.
      WHEN gc_chg_change.
        ls_item_wa_put-matnr = ls_item_wa-matnr.
      WHEN gc_chg_delete.
        CLEAR ls_item_wa_put-matnr.
    ENDCASE.
    CASE ls_hun_itm_action-batch.
      WHEN gc_chg_change.
        ls_item_wa_put-charg = ls_item_wa-charg.
      WHEN gc_chg_delete.
        CLEAR ls_item_wa_put-charg.
    ENDCASE.

    ls_item_wa_put-velin = ls_item_wa-velin. " Faltava o Tipo Conteudo da HU

    APPEND ls_item_wa_put TO cs_hun_wa-content-data.
  ENDLOOP.

ENDENHANCEMENT.
