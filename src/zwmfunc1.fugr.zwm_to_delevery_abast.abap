FUNCTION zwm_to_delevery_abast.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"     REFERENCE(I_MIN_DATUM) TYPE  DATUM OPTIONAL
*"     REFERENCE(IR_REFNR) TYPE  ZWM01_R_REFNR OPTIONAL
*"     REFERENCE(IR_DATUM) TYPE  ZWM01_R_DATUM OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lt_t311      TYPE TABLE OF t311,
        lt_t311_new  TYPE TABLE OF t311,
        lt_t311a_h   TYPE TABLE OF t311a,
        lt_t311a     TYPE TABLE OF t311a,
        lt_t311l     TYPE TABLE OF t311l,
        lt_vbuk      TYPE HASHED TABLE OF vbuk WITH UNIQUE KEY vbeln,
        lt_messages	 TYPE	tab_bdcmsgcoll,
        lt_tanum     TYPE TABLE OF tanum,
        lv_lines     TYPE sytabix.

  DATA: ls_t311    TYPE t311,
        ls_t311a   TYPE t311a,
        ls_t311a_h TYPE t311a,
        ls_vbuk    TYPE vbuk,
        ls_r_refnr LIKE LINE OF ir_refnr,
        ls_r_datum LIKE LINE OF ir_datum.

  DATA: lv_2step     TYPE flag,
        lv_error     TYPE error,
        lv_tanum     TYPE tanum,
        lv_subrc     TYPE sysubrc,
        lv_min_datum TYPE datum,
        lr_refnr     TYPE zwm01_r_refnr,
        lr_datum     TYPE zwm01_r_datum.

  CLEAR: et_messages.

  lr_refnr = ir_refnr.
  lr_datum = ir_datum.

  IF NOT i_refnr IS INITIAL.
    ls_r_refnr-low = i_refnr.
    ls_r_refnr-sign   = 'I'.
    ls_r_refnr-option = 'EQ'.
    APPEND ls_r_refnr TO lr_refnr.
  ENDIF.

  lv_min_datum = i_min_datum.

  IF NOT lv_min_datum IS INITIAL.
    ls_r_datum-low = i_min_datum.
    ls_r_datum-sign   = 'I'.
    ls_r_datum-option = 'GE'.
    APPEND ls_r_datum TO lr_datum.
  ENDIF.


** Retorna Todos Os grupos
***********************************************************************
  DO 1 TIMES.
    IF NOT lr_refnr IS INITIAL.
      SELECT * FROM t311
         INTO TABLE lt_t311
         WHERE lgnum = i_lgnum AND
               refnr IN lr_refnr AND
               datum IN lr_datum.
    ELSE.
      SELECT * FROM t311
         INTO TABLE lt_t311
         WHERE lgnum = i_lgnum AND
               datum IN lr_datum.
    ENDIF.

    CHECK NOT lt_t311 IS INITIAL.

*--> Valida se É picking em 2 passos
    LOOP AT lt_t311 INTO ls_t311.

      CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
        EXPORTING
          is_t311 = ls_t311
        IMPORTING
          e_2step = lv_2step
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.

      CHECK lv_2step EQ abap_true.

      APPEND ls_t311 TO lt_t311_new.
    ENDLOOP.

    IF lt_t311_new IS INITIAL.
**    Não tem grupos em 2 passos
      RETURN.
    ENDIF.

    lt_t311 = lt_t311_new.

** Update de Grupos
***********************************************************************
    PERFORM update_lx39 USING lt_t311
                              lr_datum
                              i_commit.

*--> Tabela Etapas do Picking em dois passos
    SELECT * FROM t311l
       INTO TABLE lt_t311l
       FOR ALL ENTRIES IN lt_t311
       WHERE lgnum = lt_t311-lgnum AND
             refnr = lt_t311-refnr.

    DELETE lt_t311l WHERE l2ska <> '1'.
    DELETE lt_t311l WHERE kzqui <> 'X'.
  ENDDO.

  CHECK NOT lt_t311l IS INITIAL.

** Retorna Remessas
***********************************************************************
  DO 1 TIMES.
    CHECK NOT lt_t311l IS INITIAL.

    SELECT * FROM t311a
       INTO TABLE lt_t311a
       FOR ALL ENTRIES IN lt_t311l
       WHERE lgnum = lt_t311l-lgnum AND
             refnr = lt_t311l-refnr.

    CHECK sy-subrc EQ 0.
    SORT lt_t311a BY refnr rbnum.

    SELECT * FROM vbuk
       INTO TABLE lt_vbuk
       FOR ALL ENTRIES IN lt_t311a
       WHERE vbeln = lt_t311a-rbnum.

    CHECK sy-subrc EQ 0.
  ENDDO.

** Criação de OT's
***********************************************************************
  lt_t311a_h = lt_t311a.
  DELETE ADJACENT DUPLICATES FROM lt_t311a_h COMPARING refnr.

  LOOP AT lt_t311a_h INTO ls_t311a_h.
    READ TABLE lt_t311a
      WITH KEY refnr = ls_t311a_h-refnr
      TRANSPORTING NO FIELDS
      BINARY SEARCH.

    LOOP AT lt_t311a INTO ls_t311a FROM sy-tabix.
      IF ls_t311a-refnr <> ls_t311a_h-refnr.
        EXIT.
      ENDIF.

      CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
        EXPORTING
          i_refnr = ls_t311a-refnr
          i_vbeln = ls_t311a-rbnum
        IMPORTING
          e_2step = lv_2step
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.

      CHECK lv_2step EQ abap_true.

      CLEAR ls_vbuk.
      READ TABLE lt_vbuk
            INTO ls_vbuk
            WITH TABLE KEY vbeln = ls_t311a-rbnum.

      CHECK ls_vbuk-kostk <> 'C' AND NOT ls_vbuk-kostk IS INITIAL.

      CALL FUNCTION 'ZWM_TO_CREATE_DN'
        EXPORTING
          i_lgnum     = ls_t311a-lgnum
          i_vbeln     = ls_t311a-rbnum
          i_refnr     = ls_t311a-refnr
          i_squit     = abap_true
          i_commit    = i_commit
        IMPORTING
          e_tanum     = lv_tanum
          et_messages = lt_messages
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      APPEND lv_tanum TO lt_tanum.
    ENDLOOP.

    IF NOT lt_messages IS INITIAL.
      APPEND LINES OF lt_messages TO et_messages.
      RAISE error.
    ENDIF.
  ENDLOOP.

  CHECK NOT lt_tanum IS INITIAL.

** Reprecessa LX39
***********************************************************************
  PERFORM update_lx39 USING lt_t311
                            lr_datum
                            i_commit.

** Espera por libertar de bloqueios
***********************************************************************
  LOOP AT lt_t311a INTO ls_t311a.
    CALL FUNCTION 'ZWM_LOCK_WAIT'
      EXPORTING
        i_mandt     = sy-mandt
        i_element1  = ls_t311a-rbnum
        i_gname     = 'LIKP'
        i_wait      = 20
      IMPORTING
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      RAISE error.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
