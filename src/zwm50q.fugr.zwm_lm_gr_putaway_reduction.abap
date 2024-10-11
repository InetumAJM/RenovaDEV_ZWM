FUNCTION ZWM_LM_GR_PUTAWAY_REDUCTION.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_TVLR STRUCTURE  TVLR OPTIONAL
*"      CT_POSTAB STRUCTURE  ZWMOV
*"----------------------------------------------------------------------

  DATA: lt_tvlr LIKE tvlr OCCURS 0.

  IF NOT it_tvlr IS REQUESTED.
    SELECT * FROM tvlr
             INTO TABLE lt_tvlr.
    SORT lt_tvlr BY lgnum pstyv.
    PERFORM putaway_reduction TABLES ct_postab
                                     lt_tvlr.
  ELSE.
    PERFORM putaway_reduction TABLES ct_postab
                                     it_tvlr.

  ENDIF.

ENDFUNCTION.
