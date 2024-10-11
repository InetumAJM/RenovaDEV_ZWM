FUNCTION zwm_group_check_2step.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"     REFERENCE(I_VBELN) TYPE  VBELN OPTIONAL
*"     REFERENCE(I_TANUM) TYPE  TANUM OPTIONAL
*"     REFERENCE(IS_T311) TYPE  T311 OPTIONAL
*"     REFERENCE(IS_LIKP) TYPE  LIKP OPTIONAL
*"     REFERENCE(IS_VTTK) TYPE  VTTK OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_2STEP) TYPE  FLAG
*"     REFERENCE(E_2SPART) TYPE  FLAG
*"     REFERENCE(E_2CHANG) TYPE  FLAG
*"     REFERENCE(ES_T311) TYPE  T311
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lt_vbeln  TYPE TABLE OF vbeln,
        lt_tknum  TYPE TABLE OF tknum,

        lt_kunnr  TYPE TABLE OF kunnr,
*        lt_zwm039 TYPE TABLE OF zwm039,
        lt_zwm065 TYPE TABLE OF zwm065,
*        lt_zwm073 TYPE TABLE OF zwm073,
        lt_sdabw  TYPE TABLE OF sdabw.

  DATA: lv_komkz   TYPE komkz,
        lv_vkorg   TYPE vkorg,
        lv_vbeln   TYPE vbeln,
        lv_i_vbeln TYPE vbeln,
        lv_sdabw   TYPE sdabw.

  DATA: ls_t311a  TYPE t311a,
*        ls_zwm073 TYPE zwm073,
        ls_ltak   TYPE ltak.

  CLEAR: e_2step, es_t311, e_2spart, e_2chang.

** Processamento
***********************************************************************
  es_t311 = is_t311.
  lv_i_vbeln = i_vbeln.

  IF es_t311 IS INITIAL.

    IF NOT i_lgnum IS INITIAL AND
       NOT i_refnr IS INITIAL.

      SELECT SINGLE * FROM t311
                      INTO es_t311
                      WHERE lgnum = i_lgnum AND
                            refnr = i_refnr.

      IF sy-subrc <> 0.
        RAISE error.
      ENDIF.

    ELSEIF NOT i_lgnum IS INITIAL AND
           NOT lv_i_vbeln IS INITIAL.

      SELECT SINGLE * FROM t311a
                      INTO ls_t311a
                      WHERE lgnum = i_lgnum AND
                            rbtyp = 'L' AND
                            rbnum = lv_i_vbeln.

      IF sy-subrc <> 0.
        RAISE error.
      ENDIF.

      SELECT SINGLE * FROM t311
                      INTO es_t311
                      WHERE lgnum = ls_t311a-lgnum AND
                            refnr = ls_t311a-refnr.

      IF sy-subrc <> 0.
        RAISE error.
      ENDIF.

    ELSEIF NOT i_lgnum IS INITIAL AND
           NOT i_tanum IS INITIAL.

      SELECT SINGLE * FROM ltak
                      INTO ls_ltak
                      WHERE lgnum = i_lgnum AND
                            tanum = i_tanum.

      IF sy-subrc <> 0.
        RAISE error.
      ENDIF.

      IF NOT ls_ltak-vbeln IS INITIAL.
        lv_i_vbeln = ls_ltak-vbeln.
      ELSEIF ls_ltak-betyp EQ 'L'.
        lv_i_vbeln = ls_ltak-benum.
      ENDIF.

      SELECT SINGLE * FROM t311
                      INTO es_t311
                      WHERE lgnum = ls_ltak-lgnum AND
                            refnr = ls_ltak-refnr.

      IF sy-subrc <> 0.
        RAISE error.
      ENDIF.

    ENDIF.
  ENDIF.

  e_2step = abap_true.

  IF es_t311-l2skr <> '2' OR
    es_t311-l2skp <> 'X'.
    e_2step = abap_false.
  ENDIF.

  IF es_t311-l2skm <> '' AND
     es_t311-l2skm <> '2'.
    e_2step = abap_false.
  ENDIF.

** Determina Picking em 2 Passos Total (Servisan) ou Total
***********************************************************************
  DO 1 TIMES.

    IF is_likp IS INITIAL.

      IF ls_t311a IS INITIAL.
        SELECT rbnum FROM t311a
                     INTO TABLE lt_vbeln
                     WHERE lgnum = es_t311-lgnum AND
                           refnr = es_t311-refnr.
        CLEAR: lv_vbeln.
        READ TABLE lt_vbeln
              INTO lv_vbeln
              INDEX 1.
      ELSE.
        lv_vbeln = ls_t311a-rbnum.
      ENDIF.

      SELECT SINGLE vkorg FROM likp
                          INTO lv_vkorg
                          WHERE vbeln = lv_vbeln.

      IF lv_vkorg EQ 'SER1'.
        e_2spart = abap_false.
        EXIT.
      ENDIF.

      IF lt_vbeln IS INITIAL.
        SELECT rbnum FROM t311a
                     INTO TABLE lt_vbeln
                     WHERE lgnum = es_t311-lgnum AND
                           refnr = es_t311-refnr.
      ENDIF.

    ELSE.

      lv_vkorg = is_likp-vkorg.

      IF lv_vkorg = 'SER1'.
        e_2spart = abap_false.
        EXIT.
      ENDIF.

      APPEND is_likp-vbeln TO lt_vbeln.

    ENDIF.

    IF NOT lt_vbeln IS INITIAL.
      SELECT kunnr FROM vbpa
                   INTO TABLE lt_kunnr
                   FOR ALL ENTRIES IN lt_vbeln
                   WHERE vbeln = lt_vbeln-table_line AND
                         parvw = 'W1'.
    ENDIF.

*    IF NOT lt_kunnr IS INITIAL.
*      SELECT * FROM zwm039
*               INTO TABLE lt_zwm039
*               FOR ALL ENTRIES IN lt_kunnr
*               WHERE lgnum = es_t311-lgnum AND
*                     kunnr = lt_kunnr-table_line.
*
*      IF sy-subrc EQ 0.
*        e_2step  = abap_false.
*        e_2spart = abap_false.
*        RETURN.
*      ENDIF.
*    ENDIF.
*
*    SELECT SINGLE * FROM zwm073
*                    INTO ls_zwm073
*                    WHERE lgnum = es_t311-lgnum AND
*                          vkorg = lv_vkorg.
*
*    IF sy-subrc EQ 0.
*      e_2step  = abap_true.
*      e_2spart = abap_true.
*      EXIT.
*    ENDIF.



    CHECK NOT lt_kunnr IS INITIAL.

    SELECT * FROM zwm065
             INTO TABLE lt_zwm065
             FOR ALL ENTRIES IN lt_kunnr
             WHERE lgnum = es_t311-lgnum AND
                   kunnr = lt_kunnr-table_line.

    CHECK sy-subrc EQ 0.

    e_2step  = abap_true.
    e_2spart = abap_true.
  ENDDO.

** Remessas de Grupo com e sem 2 Step
***********************************************************************
  DO 1 TIMES.
    CHECK NOT lv_i_vbeln IS INITIAL.

    SELECT SINGLE komkz FROM lips
                        INTO lv_komkz
                        WHERE vbeln = lv_i_vbeln.
    CHECK sy-subrc EQ 0.

    IF lv_komkz EQ '2'.
      e_2step = abap_true.
    ELSE.
      e_2step = abap_false.
    ENDIF.
  ENDDO.

** Check Por Tipo de Transporte
***********************************************************************
  DO 1 TIMES.
    IF is_vttk IS INITIAL.

      IF lt_vbeln IS INITIAL.
        SELECT rbnum FROM t311a
                     INTO TABLE lt_vbeln
                     WHERE lgnum = es_t311-lgnum AND
                           refnr = es_t311-refnr.
      ENDIF.

      CHECK NOT lt_vbeln IS INITIAL.

      SELECT tknum FROM vttp
                   INTO TABLE lt_tknum
                   FOR ALL ENTRIES IN lt_vbeln
                   WHERE vbeln = lt_vbeln-table_line.

      CHECK sy-subrc EQ 0.

      SELECT sdabw FROM vttk
                   INTO TABLE lt_sdabw
                   FOR ALL ENTRIES IN lt_tknum
                   WHERE tknum = lt_tknum-table_line.

      CHECK sy-subrc EQ 0.

      SORT lt_sdabw.
      DELETE ADJACENT DUPLICATES FROM lt_sdabw.

    ELSE.
      APPEND is_vttk-sdabw TO lt_sdabw.
    ENDIF.

    LOOP AT lt_sdabw INTO lv_sdabw.
      IF lv_sdabw EQ '0011'.
        e_2step  = abap_true.
        e_2spart = abap_true.
        e_2chang = abap_true.
        EXIT.
      ELSEIF lv_vkorg EQ 'SER1' AND lv_sdabw NE '0005'.
        e_2step  = abap_true.
        e_2spart = abap_true.
        e_2chang = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDDO.
ENDFUNCTION.
