FUNCTION z_wm_warehouse_complete_info.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_USER) TYPE  UNAME DEFAULT SY-UNAME
*"     REFERENCE(IR_WERKS) TYPE  RANGE_T_WERKS_D OPTIONAL
*"     REFERENCE(IR_LGORT) TYPE  ISI_STORE_LOC_RA OPTIONAL
*"     REFERENCE(I_EXIDV) OPTIONAL
*"     REFERENCE(I_MATNR) TYPE  MATNR OPTIONAL
*"     REFERENCE(I_CHARG) TYPE  CHARG_D OPTIONAL
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"     REFERENCE(I_VBELN) TYPE  VBELN OPTIONAL
*"     REFERENCE(I_RECALL) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_USEWM) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_USERF) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_USEMM) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_USEAUT) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_PARAMET) OPTIONAL
*"     REFERENCE(I_GET_LGNUM) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_GET_WERKS) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_GET_LGORT) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_SEL_WERKS) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_SEL_LGORT) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_FIRST_WERKS) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_FIRST_LGORT) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_LGNUM_DUMMY) TYPE  FLAG
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  CHANGING
*"     REFERENCE(C_LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(C_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(C_LGORT) TYPE  LGORT_D OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"      USER_BACK
*"----------------------------------------------------------------------


  DATA: lt_lgnum   TYPE TABLE OF lgnum,
        lt_werks   TYPE TABLE OF werks_d,
        lt_lgort   TYPE TABLE OF lgort_d,
        lt_werks_b TYPE TABLE OF werks_d,
        lt_lgort_b TYPE TABLE OF lgort_d,
        lt_items   TYPE	zrf01_t_option_select_items,
        lt_t001w   TYPE TABLE OF t001w,
        lt_t001l   TYPE SORTED TABLE OF t001l WITH UNIQUE KEY werks lgort,
        lt_adrc    TYPE TABLE OF adrc,
        lt_vbeln   TYPE TABLE OF vbeln.

  DATA: ls_r_werks TYPE range_s_werks_d,
        ls_r_lgort TYPE isi_store_loc_r,
        ls_message TYPE bdcmsgcoll,
        ls_t001l   TYPE t001l,
        ls_t320    TYPE t320,
        ls_item    TYPE zrf01_option_select_items,
        ls_t001w   TYPE t001w,
        ls_adrc    TYPE adrc,
        ls_zwm040  TYPE zwm040.

  DATA: lr_werks     TYPE RANGE OF werks_d,
        lr_lgort     TYPE RANGE OF lgort_d,
        lr_new_lgort TYPE RANGE OF lgort_d,
        lr_lgtyp     TYPE RANGE OF lgtyp.

  DATA: lv_tabix     TYPE sytabix,
        lv_lines     TYPE sytabix,
        lv_werks     TYPE werks_d,
        lv_lgort     TYPE lgort_d,
        lv_lgnum     TYPE lgnum,
        lv_venum     TYPE venum,
        lv_vbeln     TYPE vbeln,
        lv_refnr     TYPE lvs_refnr,
*        lv_paramet   TYPE z6wm_parameter,

        lv_get_lgnum TYPE  flag,
        lv_get_werks TYPE  flag,
        lv_get_lgort TYPE  flag,
        lv_sel_werks TYPE  flag,
        lv_sel_lgort TYPE  flag,

        ls_r_lgtyp   LIKE LINE OF lr_lgtyp.

  CLEAR: e_lgnum_dummy, et_messages.

  lv_vbeln = i_vbeln.
  lv_refnr = i_refnr.


** Lgnum Trash
***********************************************************************
  ls_r_lgtyp-low = 900.
  ls_r_lgtyp-high = 999.
  ls_r_lgtyp-sign = 'E'.
  ls_r_lgtyp-option = 'BT'.
  APPEND ls_r_lgtyp TO lr_lgtyp.

** Obrigatorios
***********************************************************************
  lv_get_lgnum  = i_get_lgnum.
  lv_get_werks  = i_get_werks.
  lv_get_lgort  = i_get_lgort.
  lv_sel_werks  = i_sel_werks.
  lv_sel_lgort  = i_sel_lgort.

  IF lv_get_lgort EQ abap_true.
    lv_get_werks = abap_true.
  ENDIF.

  IF lv_sel_lgort = abap_true.
    lv_sel_werks = abap_true.
  ENDIF.

** Ranges de Filtro
**********************************************************************
  lr_werks = ir_werks.
  PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.

  lr_lgort = ir_lgort.
  PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.

** Tenta Determinar Armazem
**********************************************************************
  DO 1 TIMES.
    CHECK c_lgnum IS INITIAL.

    IF i_userf EQ abap_true.
      CALL FUNCTION 'Z_WM_GET_WAREHOUSE'
        EXPORTING
          i_uname = i_user
        IMPORTING
          e_lgnum = c_lgnum
          e_werks = lv_werks
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.
    ENDIF.

    CHECK c_lgnum IS INITIAL.

    IF i_usewm EQ abap_true AND
       ( NOT lr_werks IS INITIAL OR
         NOT lr_lgort IS INITIAL ).

      SELECT lgnum FROM t320
                   INTO TABLE lt_lgnum
                   WHERE werks IN lr_werks AND
                         lgort IN lr_lgort.

      IF sy-subrc <> 0.
**        SELECT lgnum FROM z6wmt012
**                     INTO TABLE lt_lgnum
**                     WHERE werks IN lr_werks AND
**                           lgort IN lr_lgort.
      ENDIF.

      SORT lt_lgnum.
      DELETE ADJACENT DUPLICATES FROM lt_lgnum COMPARING ALL FIELDS.
      DESCRIBE TABLE lt_lgnum LINES lv_lines.
      IF lv_lines EQ 1.
        READ TABLE lt_lgnum
              INTO c_lgnum
              INDEX 1.
      ENDIF.
    ENDIF.
  ENDDO.

** Usa Parametros de Filtro
**********************************************************************
**  DO 1 TIMES.
**    CHECK NOT c_lgnum IS INITIAL.
**    CHECK NOT i_paramet IS INITIAL.
**
**    lv_paramet = i_paramet.
**    CALL METHOD z6wm_configuration=>get_range
**      EXPORTING
**        i_master  = c_lgnum
**        i_process = 'GENERAL'
**        i_paramet = lv_paramet
**      IMPORTING
**        er_values = lr_new_lgort
**      EXCEPTIONS
**        error     = 1
**        OTHERS    = 2.
**    CHECK sy-subrc EQ 0.
**
**    APPEND LINES OF lr_new_lgort TO lr_lgort.
**  ENDDO.

** Determinar Centro
**********************************************************************
  DO 1 TIMES.
    CHECK c_werks IS INITIAL.

    IF i_usewm EQ abap_true AND
       NOT c_lgnum IS INITIAL.
      SELECT werks FROM t320
                   APPENDING TABLE lt_werks
                   WHERE lgnum = c_lgnum AND
                         werks IN lr_werks AND
                         lgort IN lr_lgort.

      IF sy-subrc <> 0.
**        SELECT werks FROM z6wmt012
**                     APPENDING TABLE lt_werks
**                     WHERE lgnum = c_lgnum AND
**                           werks IN lr_werks AND
**                           lgort IN lr_lgort.
      ENDIF.

      PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
    ENDIF.

    IF i_usemm EQ abap_true AND
        NOT lr_lgort IS INITIAL.
      SELECT werks FROM t001l
                   APPENDING TABLE lt_werks
                   WHERE werks IN lr_werks AND
                         lgort IN lr_lgort.

      PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
    ENDIF.

**    CHECK c_werks IS INITIAL.
**
**    CALL METHOD z6wm_configuration=>get_value
**      EXPORTING
**        i_master  = c_lgnum
**        i_process = 'GENERAL'
**        i_paramet = 'DEFAULT_PLANT'
**      IMPORTING
**        e_value   = c_werks
**      EXCEPTIONS
**        error     = 1
**        OTHERS    = 2.
**
**    PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
  ENDDO.

** Dados de Centro
***********************************************************************
  PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.

** Determina Deposito
**********************************************************************
  DO 1 TIMES.
    CHECK c_lgort IS INITIAL.

    IF i_usewm EQ abap_true AND
       NOT c_lgnum IS INITIAL.
      SELECT lgort FROM t320
                   APPENDING TABLE lt_lgort
                   WHERE lgnum = c_lgnum AND
                         werks IN lr_werks AND
                         lgort IN lr_lgort.

      IF sy-subrc <> 0.
**        SELECT lgort FROM z6wmt012
**                     APPENDING TABLE lt_lgort
**                     WHERE lgnum = c_lgnum AND
**                           werks IN lr_werks AND
**                           lgort IN lr_lgort.
      ENDIF.

      PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
    ENDIF.

    CHECK c_lgort IS INITIAL.

    IF i_usemm EQ abap_true AND
       NOT lr_werks IS INITIAL.
      SELECT lgort FROM t001l
                   APPENDING TABLE lt_lgort
                   WHERE werks IN lr_werks AND
                         lgort IN lr_lgort.

      PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
    ENDIF.

    CHECK c_lgort IS INITIAL.

**    CALL METHOD z6wm_configuration=>get_value
**      EXPORTING
**        i_master  = c_lgnum
**        i_process = 'GENERAL'
**        i_paramet = 'DEFAULT_SLOC'
**      IMPORTING
**        e_value   = c_lgort
**      EXCEPTIONS
**        error     = 1
**        OTHERS    = 2.
**
**    PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
  ENDDO.

** Dados de Deposito
***********************************************************************
  PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.

** Dummy
**********************************************************************
  IF NOT c_lgnum IS INITIAL.
    SELECT SINGLE * FROM t320
                    INTO ls_t320
                    WHERE lgnum = c_lgnum.
    IF sy-subrc <> 0.
      e_lgnum_dummy = abap_true.
    ENDIF.
  ENDIF.

** Filtra por Palete
***********************************************************************
  DO 1 TIMES.
    CHECK NOT i_exidv IS INITIAL.


    CHECK c_werks IS INITIAL OR
          c_lgort IS INITIAL.

    IF  NOT c_lgnum IS INITIAL.
      IF c_werks IS INITIAL.

        SELECT werks FROM lqua
                     INTO TABLE lt_werks
                     WHERE lgnum = c_lgnum AND
                           lenum = i_exidv AND
                           lgtyp IN lr_lgtyp AND
                           werks IN lr_werks.

        CLEAR c_lgort.

        PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
      ENDIF.

      IF c_lgort IS INITIAL.

        SELECT lgort FROM lqua
                     INTO TABLE lt_lgort
                     WHERE lgnum = c_lgnum AND
                           lenum = i_exidv AND
                           lgtyp IN lr_lgtyp AND
                           werks IN lr_werks AND
                           lgort IN lr_lgort.

        PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
      ENDIF.
    ENDIF.

    CHECK c_werks IS INITIAL OR
          c_lgort IS INITIAL.

    SELECT SINGLE venum FROM vekp
                        INTO lv_venum
                        WHERE exidv = i_exidv.

    CHECK NOT lv_venum IS INITIAL.

    IF c_werks IS INITIAL.

      SELECT werks FROM vepo
                   INTO TABLE lt_werks
                   WHERE venum = lv_venum AND
                         werks IN lr_werks.

      CLEAR c_lgort.

      PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
    ENDIF.

    IF c_lgort IS INITIAL.

      SELECT lgort FROM vepo
                   INTO TABLE lt_lgort
                   WHERE venum = lv_venum AND
                         werks IN lr_werks AND
                         lgort IN lr_lgort.

      PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
    ENDIF.

  ENDDO.

** Filtra Por Agrupador
***********************************************************************
  DO 1 TIMES.
    CHECK NOT lv_vbeln IS INITIAL.

    CHECK NOT c_lgnum IS INITIAL.

    CHECK c_werks IS INITIAL OR
          c_lgort IS INITIAL.

    SELECT SINGLE * FROM zwm040
                    INTO ls_zwm040
                    WHERE lgnum = c_lgnum AND
                          id_servisan = lv_vbeln.

    CHECK sy-subrc EQ 0.

    IF ls_zwm040-remessa EQ gc_vbeln_2step_dummy.
      lv_refnr = ls_zwm040-refnr.
      CLEAR lv_vbeln.
    ELSE.
      lv_refnr = ls_zwm040-refnr.
      lv_vbeln = ls_zwm040-remessa.
    ENDIF.
  ENDDO.


** Filtra Por Grupo
***********************************************************************
  DO 1 TIMES.
    CHECK NOT lv_refnr IS INITIAL.

    CHECK NOT c_lgnum IS INITIAL.

    CHECK c_werks IS INITIAL OR
          c_lgort IS INITIAL.


    SELECT rbnum FROM t311a
                 INTO TABLE lt_vbeln
                 WHERE lgnum = c_lgnum AND
                       refnr = lv_refnr.

    IF NOT lv_vbeln IS INITIAL.
      DELETE lt_vbeln WHERE table_line <> lv_vbeln.
    ENDIF.

    CHECK NOT lt_vbeln IS INITIAL.

    IF c_werks IS INITIAL.
      SELECT werks FROM lips
                        INTO TABLE lt_werks
                        FOR ALL ENTRIES IN lt_vbeln
                        WHERE vbeln = lt_vbeln-table_line AND
                              werks IN lr_werks.

      CLEAR c_lgort.

      PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
    ENDIF.

    IF c_lgort IS INITIAL.
      SELECT lgort FROM lips
                   INTO TABLE lt_lgort
                   FOR ALL ENTRIES IN lt_vbeln
                   WHERE vbeln = lt_vbeln-table_line AND
                         werks IN lr_werks AND
                         lgort IN lr_lgort.

      PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
    ENDIF.


  ENDDO.


** Filtra Por Remessa
***********************************************************************
  DO 1 TIMES.
    CHECK NOT lv_vbeln IS INITIAL.

    CHECK c_werks IS INITIAL OR
          c_lgort IS INITIAL.



    IF c_werks IS INITIAL.
      SELECT werks FROM lips
                          INTO TABLE lt_werks
                          WHERE vbeln = lv_vbeln AND
                                werks IN lr_werks.

      CLEAR c_lgort.

      PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
    ENDIF.

    IF c_lgort IS INITIAL.
      SELECT lgort FROM lips
                   INTO TABLE lt_lgort
                   WHERE vbeln = lv_vbeln AND
                         werks IN lr_werks AND
                         lgort IN lr_lgort.

      PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
    ENDIF.
  ENDDO.

** Filtra Por Material
***********************************************************************
  DO 1 TIMES.
    CHECK NOT i_matnr IS INITIAL.

    CHECK NOT c_lgnum IS INITIAL.

    CHECK c_werks IS INITIAL OR
          c_lgort IS INITIAL.





    IF c_werks IS INITIAL.

      SELECT werks FROM lqua
                   INTO TABLE lt_werks
                   WHERE matnr = i_matnr AND
                         lgnum = c_lgnum AND
                         lgtyp IN lr_lgtyp AND
                         werks IN lr_werks.

      CLEAR c_lgort.

      PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
    ENDIF.

    IF c_lgort IS INITIAL.

      SELECT lgort FROM lqua
                   INTO TABLE lt_lgort
                   WHERE matnr = i_matnr AND
                         lgnum = c_lgnum AND
                         lgtyp IN lr_lgtyp AND
                         werks IN lr_werks AND
                         lgort IN lr_lgort.

      PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
    ENDIF.

    CHECK c_werks IS INITIAL OR
          c_lgort IS INITIAL.


    IF c_werks IS INITIAL.

      SELECT werks FROM ltap
                   INTO TABLE lt_werks
                   WHERE lgnum = c_lgnum AND
                         pquit = abap_false AND
                         matnr = i_matnr AND
                         vltyp IN lr_lgtyp AND
                         nltyp IN lr_lgtyp AND
                         werks IN lr_werks.

      CLEAR c_lgort.

      PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
    ENDIF.

    IF c_lgort IS INITIAL.

      SELECT lgort FROM ltap
                   INTO TABLE lt_lgort
                   WHERE lgnum = c_lgnum AND
                         pquit = abap_false AND
                         matnr = i_matnr AND
                         werks IN lr_werks AND
                         vltyp IN lr_lgtyp AND
                         nltyp IN lr_lgtyp AND
                         lgort IN lr_lgort.

      PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
    ENDIF.

    CHECK c_werks IS INITIAL OR
          c_lgort IS INITIAL.


    IF c_werks IS INITIAL.

      SELECT werks FROM ltap
                   INTO TABLE lt_werks
                   WHERE lgnum = c_lgnum AND
                         pquit = abap_true AND
                         matnr = i_matnr AND
                         vltyp IN lr_lgtyp AND
                         nltyp IN lr_lgtyp AND
                         werks IN lr_werks.

      CLEAR c_lgort.

      PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
    ENDIF.

    IF c_lgort IS INITIAL.

      SELECT lgort FROM ltap
                   INTO TABLE lt_lgort
                   WHERE lgnum = c_lgnum AND
                         pquit = abap_true AND
                         matnr = i_matnr AND
                         werks IN lr_werks AND
                         vltyp IN lr_lgtyp AND
                         nltyp IN lr_lgtyp AND
                         lgort IN lr_lgort.

      PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
    ENDIF.

  ENDDO.

** Filtra Por Lote
***********************************************************************
  DO 1 TIMES.
    CHECK NOT i_charg IS INITIAL.

    CHECK NOT c_lgnum IS INITIAL.

    CHECK c_werks IS INITIAL OR
          c_lgort IS INITIAL.


    IF c_werks IS INITIAL.

      SELECT werks FROM lqua
                   INTO TABLE lt_werks
                   WHERE matnr = i_matnr AND
                         charg = i_charg AND
                         lgnum = c_lgnum AND
                         lgtyp IN lr_lgtyp AND
                         werks IN lr_werks.

      CLEAR c_lgort.

      PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
    ENDIF.

    IF c_lgort IS INITIAL.

      SELECT lgort FROM lqua
                   INTO TABLE lt_lgort
                   WHERE matnr = i_matnr AND
                         charg = i_charg AND
                         lgnum = c_lgnum AND
                         lgtyp IN lr_lgtyp AND
                         werks IN lr_werks AND
                         lgort IN lr_lgort.

      PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
    ENDIF.

    CHECK c_werks IS INITIAL OR
          c_lgort IS INITIAL.


    IF c_werks IS INITIAL.

      SELECT werks FROM ltap
                   INTO TABLE lt_werks
                   WHERE lgnum = c_lgnum AND
                         pquit = abap_false AND
                         matnr = i_matnr AND
                         charg = i_charg AND
                         vltyp IN lr_lgtyp AND
                         nltyp IN lr_lgtyp AND
                         werks IN lr_werks.

      CLEAR c_lgort.

      PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
    ENDIF.

    IF c_lgort IS INITIAL.

      SELECT lgort FROM ltap
                   INTO TABLE lt_lgort
                   WHERE lgnum = c_lgnum    AND
                         pquit = abap_false AND
                         matnr = i_matnr    AND
                         charg = i_charg    AND
                         werks IN lr_werks  AND
                         vltyp IN lr_lgtyp  AND
                         nltyp IN lr_lgtyp  AND
                         lgort IN lr_lgort.

      PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
    ENDIF.

    CHECK c_werks IS INITIAL OR
          c_lgort IS INITIAL.


    IF c_werks IS INITIAL.

      SELECT werks FROM ltap
                   INTO TABLE lt_werks
                   WHERE lgnum = c_lgnum   AND
                         pquit = abap_true AND
                         matnr = i_matnr   AND
                         charg = i_charg   AND
                         vltyp IN lr_lgtyp AND
                         nltyp IN lr_lgtyp AND
                         werks IN lr_werks.
      CLEAR c_lgort.

      PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
    ENDIF.

    IF c_lgort IS INITIAL.

      SELECT lgort FROM ltap
                   INTO TABLE lt_lgort
                   WHERE lgnum = c_lgnum   AND
                         pquit = abap_true AND
                         matnr = i_matnr   AND
                         charg = i_charg   AND
                         werks IN lr_werks AND
                         vltyp IN lr_lgtyp AND
                         nltyp IN lr_lgtyp AND
                         lgort IN lr_lgort.

      PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
    ENDIF.

  ENDDO.


** Aut Object and Option Select Plant
***********************************************************************
  DO 1 TIMES.
    CHECK c_werks IS INITIAL.

    CHECK NOT lt_werks IS INITIAL.

**    IF i_useaut EQ abap_true.
**      lt_werks_b = lt_werks.
**      LOOP AT lt_werks INTO lv_werks.
**        lv_tabix = sy-tabix.
**        AUTHORITY-CHECK OBJECT 'ZWM004'
**                  FOR USER sy-uname
**                  ID 'LGNUM' FIELD c_lgnum
**                  ID 'WERKS' FIELD lv_werks.
**        IF sy-subrc <> 0.
**          DELETE lt_werks INDEX lv_tabix.
**        ENDIF.
**      ENDLOOP.
**
**      IF lt_werks IS INITIAL.
**        lt_werks = lt_werks_b.
**      ENDIF.
**    ENDIF.

    PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
    CHECK c_werks IS INITIAL.

    CHECK lv_sel_werks EQ abap_true.

    SELECT * FROM t001w
             INTO TABLE lt_t001w
             FOR ALL ENTRIES IN lt_werks
             WHERE werks = lt_werks-table_line.
    CHECK sy-subrc EQ 0.
    SORT lt_t001w BY werks.

    SELECT * FROM adrc
             INTO TABLE lt_adrc
             FOR ALL ENTRIES IN lt_t001w
             WHERE addrnumber = lt_t001w-adrnr AND
                   date_from <= sy-datum.
    SORT lt_adrc BY addrnumber.

    CLEAR: lt_items.
    LOOP AT lt_t001w INTO ls_t001w.
      CLEAR: ls_item.

      READ TABLE lt_adrc
            INTO ls_adrc
            WITH KEY addrnumber = ls_t001w-adrnr
            BINARY SEARCH.

      ls_item-op_key  = ls_t001w-werks.

      IF NOT ls_adrc-sort1 IS INITIAL.
        CONCATENATE ls_t001w-werks '-' ls_adrc-sort1 INTO ls_item-op_text SEPARATED BY space.
      ELSE.
        ls_item-op_text = ls_t001w-werks.
      ENDIF.

      APPEND ls_item TO lt_items.
    ENDLOOP.

    CALL FUNCTION 'Z_WM_RF_OPTION_SELECT'
      EXPORTING
        it_items = lt_items
      IMPORTING
        e_key    = c_werks.

    IF c_werks IS INITIAL.
      RAISE user_back.
    ENDIF.
  ENDDO.

  PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.


** Option Select Depósito
***********************************************************************
  DO 1 TIMES.
    CHECK NOT c_werks IS INITIAL AND
              c_lgort IS INITIAL.

    CHECK NOT lt_lgort IS INITIAL.

**    IF i_useaut EQ abap_true.
**      lt_lgort_b = lt_lgort.
**      LOOP AT lt_lgort INTO lv_lgort.
**        lv_tabix = sy-tabix.
**        AUTHORITY-CHECK OBJECT 'ZWM005'
**                  FOR USER sy-uname
**                  ID 'LGNUM' FIELD c_lgnum
**                  ID 'WERKS' FIELD c_werks
**                  ID 'LGORT' FIELD lv_lgort.
**        IF sy-subrc <> 0.
**          DELETE lt_lgort INDEX lv_tabix.
**        ENDIF.
**      ENDLOOP.
**      IF lt_lgort IS INITIAL.
**        lt_lgort = lt_lgort_b.
**      ENDIF.
**    ENDIF.


    PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
    CHECK c_lgort IS INITIAL.

    CHECK  lv_sel_lgort EQ abap_true.

    SELECT * FROM t001l
             INTO TABLE lt_t001l
             FOR ALL ENTRIES IN lt_lgort
             WHERE werks = c_werks AND
                   lgort = lt_lgort-table_line.


    CLEAR: lt_items.
    LOOP AT lt_lgort INTO lv_lgort.
      CLEAR: ls_item.

      CLEAR: ls_t001l.
      READ TABLE lt_t001l
            INTO ls_t001l
            WITH TABLE KEY werks = c_werks
                           lgort = lv_lgort.

      IF NOT ls_t001l-lgobe IS INITIAL.
        ls_item-op_text = ls_t001l-lgobe.
      ELSE.
        ls_item-op_text = lv_lgort.
      ENDIF.


      ls_item-op_key  = lv_lgort.

      APPEND ls_item TO lt_items.
    ENDLOOP.

    CALL FUNCTION 'Z_WM_RF_OPTION_SELECT'
      EXPORTING
        it_items = lt_items
      IMPORTING
        e_key    = c_lgort.

    IF c_lgort IS INITIAL.
      RAISE user_back.
    ENDIF.
  ENDDO.

  PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.

** Tenta Chamar Novamente Para Campos Em Sobra
**********************************************************************
  IF i_recall EQ abap_true.
    CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
      EXPORTING
        i_user      = sy-uname
        ir_werks    = lr_werks
        ir_lgort    = lr_lgort
        i_recall    = abap_false
        i_matnr     = i_matnr
        i_refnr     = lv_refnr
        i_vbeln     = lv_vbeln
        i_usewm     = i_usewm
        i_userf     = i_userf
        i_usemm     = i_usemm
        i_useaut    = i_useaut
        i_get_lgnum = lv_get_lgnum
        i_get_werks = lv_get_werks
        i_get_lgort = lv_get_lgort
        i_paramet   = i_paramet
      CHANGING
        c_lgnum     = c_lgnum
        c_werks     = c_werks
        c_lgort     = c_lgort
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
  ENDIF.

  PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
  PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.

  IF i_first_werks EQ abap_true AND c_werks IS INITIAL.
    READ TABLE lt_werks
          INTO c_werks
          INDEX 1.

    PERFORM filter_whs_data CHANGING lt_werks c_werks lr_werks.
  ENDIF.

  IF i_first_lgort EQ abap_true AND c_lgort IS INITIAL.
    READ TABLE lt_lgort
          INTO c_lgort
          INDEX 1.

    PERFORM filter_whs_data CHANGING lt_lgort c_lgort lr_lgort.
  ENDIF.


** Validações
**********************************************************************
  IF c_lgnum IS INITIAL AND
     lv_get_lgnum EQ abap_true.
**  For User & not possible to determinate &
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = 'Z6WM001'.
    ls_message-msgnr  = '245'.
    ls_message-msgv1  = i_user.
    ls_message-msgv2  = text-001.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  IF c_werks IS INITIAL AND
     lv_get_werks EQ abap_true.

    DESCRIBE TABLE lt_werks LINES lv_lines.

    IF lv_lines > 1.
**    For User & determinated multiple & (&)
      CLEAR: ls_message.
      ls_message-msgtyp = 'E'.
      ls_message-msgid  = 'Z6WM001'.
      ls_message-msgnr  = '245'.
      ls_message-msgv1  = i_user.
      ls_message-msgv2  = text-002.
      LOOP AT lt_werks INTO lv_werks.
        IF ls_message-msgv3 IS INITIAL.
          ls_message-msgv3 = lv_werks.
        ELSE.
          CONCATENATE ls_message-msgv3 lv_werks INTO ls_message-msgv3 SEPARATED BY ','.
        ENDIF.
      ENDLOOP.
      APPEND ls_message TO et_messages.
      RAISE error.
    ELSE.
**    For User & not possible to determinate &
      ls_message-msgtyp = 'E'.
      ls_message-msgid  = 'Z6WM001'.
      ls_message-msgnr  = '245'.
      ls_message-msgv1  = i_user.
      ls_message-msgv2  = text-002.
      APPEND ls_message TO et_messages.
      RAISE error.
    ENDIF.
  ENDIF.

  IF c_lgort IS INITIAL AND
     lv_get_lgort EQ abap_true.

    DESCRIBE TABLE lt_lgort LINES lv_lines.

    IF lv_lines > 1.
*     For User & determinated multiple & (&)
      CLEAR: ls_message.
      ls_message-msgtyp = 'E'.
      ls_message-msgid  = 'Z6WM001'.
      ls_message-msgnr  = '245'.
      ls_message-msgv1  = i_user.
      ls_message-msgv2  = text-003.
      LOOP AT lt_lgort INTO lv_lgort.
        IF ls_message-msgv3 IS INITIAL.
          ls_message-msgv3 = lv_lgort.
        ELSE.
          CONCATENATE ls_message-msgv3 lv_lgort INTO ls_message-msgv3 SEPARATED BY ','.
        ENDIF.
      ENDLOOP.
      APPEND ls_message TO et_messages.
      RAISE error.
    ELSE.
**    For User & not possible to determinate &
      ls_message-msgtyp = 'E'.
      ls_message-msgid  = 'Z6WM001'.
      ls_message-msgnr  = '245'.
      ls_message-msgv1  = i_user.
      ls_message-msgv2  = text-003.
      APPEND ls_message TO et_messages.
      RAISE error.
    ENDIF.
  ENDIF.

** Valida Centro Com Deposito
**********************************************************************
  IF NOT c_werks IS INITIAL AND
     NOT c_lgort IS INITIAL.
    SELECT SINGLE * FROM t001l
                    INTO ls_t001l
                    WHERE werks = c_werks AND
                          lgort = c_lgort.
    IF sy-subrc <> 0.
**    Plant & does not have association with Storage Location &
      ls_message-msgtyp = 'E'.
      ls_message-msgid  = 'Z6WM001'.
      ls_message-msgnr  = '246'.
      ls_message-msgv1  = c_werks.
      ls_message-msgv2  = c_lgort.
      APPEND ls_message TO et_messages.
      RAISE error.
    ENDIF.
  ENDIF.

** Valida Centro Com Deposito com WM (Pode haver ou ser Dummy)
**********************************************************************
  IF NOT c_werks IS INITIAL AND
     NOT c_lgort IS INITIAL AND
     NOT c_lgnum IS INITIAL.

    SELECT SINGLE lgnum FROM t320
                        INTO lv_lgnum
                        WHERE werks = c_werks AND
                              lgort = c_lgort.

    IF sy-subrc <> 0.
**      SELECT SINGLE lgnum FROM z6wmt012
**                          INTO lv_lgnum
**                          WHERE werks = c_werks AND
**                                lgort = c_lgort.
    ENDIF.



    IF lv_lgnum <> c_lgnum.
**    Plant & and Storage Location & not associated with Warehouse &
      ls_message-msgtyp = 'E'.
      ls_message-msgid  = 'Z6WM001'.
      ls_message-msgnr  = '247'.
      ls_message-msgv1  = c_werks.
      ls_message-msgv2  = c_lgort.
      ls_message-msgv2  = c_lgnum.
      APPEND ls_message TO et_messages.
      RAISE error.
    ENDIF.
  ENDIF.

** Zone Code
***********************************************************************
**  IF NOT c_werks IS INITIAL AND
**     NOT c_lgort IS INITIAL.
**    CONCATENATE c_werks c_lgort INTO e_whszn.
**  ENDIF.


ENDFUNCTION.
