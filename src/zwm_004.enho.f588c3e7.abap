"Name: \PR:SAPLV56I_BAPI\FO:FILL_HUN_HDR_DATA_ADD\SE:END\EI
ENHANCEMENT 0 ZWM_004.

** ROFF @ 18/06/2012
** Correcção de Bug SAP

*- loop over the item data for this header
  LOOP AT it_hun_itm INTO ls_hun_itm
    WHERE hdl_unit_into = ls_header_wa-venum. " Correcção do ERRO SAP
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
    APPEND ls_item_wa TO cs_hun_wa-content-data.
  ENDLOOP.


ENDENHANCEMENT.
