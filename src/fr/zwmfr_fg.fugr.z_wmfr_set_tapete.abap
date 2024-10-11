FUNCTION z_wmfr_set_tapete.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_LGTYP) TYPE  LGTYP
*"     REFERENCE(I_LGPLA) TYPE  LGPLA
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"----------------------------------------------------------------------
  DATA: lt_taps      TYPE TABLE OF lgpla,
        lt_zwmfrt004 TYPE TABLE OF zwmfrt004,
        lt_taps_b    TYPE TABLE OF lgpla.

  DATA: ls_zwm064    TYPE zwm064,
        ls_zwmfrt004 TYPE zwmfrt004,
        ls_ltak      TYPE ltak.

  DATA: lv_activated TYPE flag,
        lv_lines     TYPE sytabix,
        lv_tapn      TYPE n LENGTH 2,
        lv_name      TYPE fieldname,
        lv_tap       TYPE lgpla,
        lv_tabix     TYPE sytabix,
        lv_queue     TYPE lrf_queue,
        lv_queue_sd  TYPE lrf_queue,
        lv_taprep    TYPE lgpla.

  FIELD-SYMBOLS: <lv_tap> TYPE lgpla.

  CHECK NOT i_lgtyp IS INITIAL.

** Valida se Deve Registar Tapete
***********************************************************************
  SELECT SINGLE valor FROM zwm001
                      INTO lv_activated
                      WHERE armazem   = i_lgnum AND
                            processo  = 'LIBERACAO_VIA_IDOC' AND
                            parametro = 'ACTIVAR'.

  CHECK lv_activated EQ abap_true.

  SELECT SINGLE valor FROM zwm001
                      INTO lv_queue
                      WHERE armazem   = i_lgnum AND
                            processo  = 'GESTAO_FILAS' AND
                            parametro = 'FILA_AUT_REP'.

  SELECT SINGLE valor FROM zwm001
                      INTO lv_queue_sd
                      WHERE armazem   = i_lgnum AND
                            processo  = 'GESTAO_FILAS' AND
                            parametro = 'FILA_AUT_SD'.

  SELECT SINGLE valor FROM zwm001
                      INTO lv_taprep
                      WHERE armazem   = i_lgnum AND
                            processo  = 'TAPETE' AND
                            parametro = 'REP'.


** Retorna Entrada de Tabela de Tapetes
***********************************************************************
  SELECT SINGLE * FROM zwm064
                  INTO ls_zwm064
                  WHERE lgnum = i_lgnum AND
                        lgtyp = i_lgtyp AND
                        lgpla = i_lgpla.


  DO.
    lv_tapn = sy-index.

    CONCATENATE 'TAP' lv_tapn INTO lv_name.
    CONDENSE lv_name NO-GAPS.

    UNASSIGN <lv_tap>.
    ASSIGN COMPONENT lv_name OF STRUCTURE ls_zwm064 TO <lv_tap>.
    IF NOT <lv_tap> IS ASSIGNED.
      EXIT.
    ENDIF.
    IF <lv_tap> IS INITIAL.
      EXIT.
    ENDIF.

    APPEND <lv_tap> TO lt_taps.
  ENDDO.

  CHECK NOT lt_taps IS INITIAL.

  DESCRIBE TABLE lt_taps LINES lv_lines.
  IF lv_lines EQ 1.
    READ TABLE lt_taps
          INTO lv_tap
          INDEX 1.
  ELSE.
    SELECT * FROM zwmfrt004
             INTO TABLE lt_zwmfrt004
             FOR ALL ENTRIES IN lt_taps
             WHERE lgnum = i_lgnum AND
                   nlpla = lt_taps-table_line.

    SORT lt_zwmfrt004 BY nlpla.

    lt_taps_b = lt_taps.

    LOOP AT lt_taps INTO lv_tap.
      lv_tabix = sy-tabix.

      CLEAR: ls_zwmfrt004.
      READ TABLE lt_zwmfrt004
            INTO ls_zwmfrt004
            WITH KEY nlpla = lv_tap
            BINARY SEARCH.

      CHECK sy-subrc EQ 0.
      DELETE lt_taps INDEX lv_tabix.
    ENDLOOP.

    IF lt_taps IS INITIAL.
      lt_taps = lt_taps_b.
    ENDIF.

    READ TABLE lt_taps
          INTO lv_tap
          INDEX 1.
  ENDIF.


** Valida se Tem OT's
***********************************************************************
  SELECT SINGLE * FROM ltak
                  INTO ls_ltak
                  WHERE lgnum = i_lgnum AND
                        refnr = i_refnr AND
                        kquit = abap_false AND
                        queue = lv_queue_sd.
  IF sy-subrc EQ 0.
    CLEAR: ls_zwmfrt004.
    ls_zwmfrt004-lgnum    = i_lgnum.
    ls_zwmfrt004-nlpla    = lv_tap.
    ls_zwmfrt004-refnr    = i_refnr.
    ls_zwmfrt004-original = abap_true.
    MODIFY zwmfrt004 FROM ls_zwmfrt004.
  ENDIF.

  SELECT SINGLE * FROM ltak
                  INTO ls_ltak
                  WHERE lgnum = i_lgnum AND
                        benum = i_refnr AND
                        betyp = 'Z' AND
                        kquit = abap_false AND
                        queue = lv_queue.

  IF sy-subrc EQ 0.
    CLEAR: ls_zwmfrt004.
    ls_zwmfrt004-lgnum = i_lgnum.
    ls_zwmfrt004-nlpla = lv_taprep.
    ls_zwmfrt004-refnr = i_refnr.
    ls_zwmfrt004-original = abap_true.
    MODIFY zwmfrt004 FROM ls_zwmfrt004.
  ENDIF.

  CHECK i_commit EQ abap_true.
  COMMIT WORK.
ENDFUNCTION.
