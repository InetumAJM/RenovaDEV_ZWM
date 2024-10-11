FUNCTION ZWM_LM_FORWARDING_AGENT_ADD.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      CT_POSTAB STRUCTURE  ZWMOV
*"----------------------------------------------------------------------
  TYPES : BEGIN OF ts_select,
            vbeln LIKE vbpa-vbeln,
            kunde LIKE vbpa-kunnr,
            lifnr LIKE vbpa-lifnr,
          END OF ts_select.

  DATA: lt_select TYPE SORTED TABLE OF ts_select
                                    WITH UNIQUE KEY vbeln
                                    INITIAL SIZE 10
                                    WITH HEADER LINE.
  FIELD-SYMBOLS: <ls_postab> TYPE zwmov.

* Selection from partner table
  SELECT vbeln lifnr FROM vbpa
               APPENDING CORRESPONDING FIELDS OF TABLE lt_select
                     FOR ALL ENTRIES IN ct_postab
                     WHERE vbeln EQ ct_postab-vbeln
                     AND   posnr EQ posnr_initial
                     AND   parvw EQ 'SP'.
  IF sy-subrc EQ 0.
*   Add forwarding agent
    LOOP AT ct_postab ASSIGNING <ls_postab>.
      READ TABLE lt_select WITH KEY vbeln = <ls_postab>-vbeln.
      IF sy-subrc EQ 0.
        MOVE lt_select-lifnr TO <ls_postab>-spdnr.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
