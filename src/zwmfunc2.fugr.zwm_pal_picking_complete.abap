FUNCTION zwm_pal_picking_complete.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(I_VBELN_VA) TYPE  VBELN_VA OPTIONAL
*"     REFERENCE(IR_VBELN_VA) TYPE  EHS_RVBELN_T OPTIONAL
*"     REFERENCE(IT_VBELN_VA) TYPE  ANY OPTIONAL
*"     REFERENCE(I_VBELN_VL) TYPE  VBELN_VL OPTIONAL
*"     REFERENCE(IR_VBELN_VL) TYPE  EHS_RVBELN_T OPTIONAL
*"     REFERENCE(IT_VBELN_VL) TYPE  ANY OPTIONAL
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"     REFERENCE(IR_REFNR) TYPE  ZWM_R_REFNR OPTIONAL
*"     REFERENCE(IT_REFNR) TYPE  ANY OPTIONAL
*"     REFERENCE(I_TKNUM) TYPE  TKNUM OPTIONAL
*"     REFERENCE(IR_TKNUM) TYPE  ISI_TKNUM_RA OPTIONAL
*"     REFERENCE(IT_TKNUM) TYPE  ANY OPTIONAL
*"     REFERENCE(I_REMONTADA) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_ACTUALIZA) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_LOAD_CALC) TYPE  FLAG OPTIONAL
*"  TABLES
*"      ZPALETE_PICKING STRUCTURE  ZPALETE_PICKING OPTIONAL
*"  CHANGING
*"     REFERENCE(C_NPAL) TYPE  ZROUTYN_NPAL OPTIONAL
*"     REFERENCE(C_NPAL_REM) TYPE  ZROUTYN_NPAL_REM OPTIONAL
*"     REFERENCE(C_COST_PAL) TYPE  ZROUTYN_COST_PAL OPTIONAL
*"     REFERENCE(C_PAL_SPEC) TYPE  FLAG OPTIONAL
*"----------------------------------------------------------------------

***  CALL FUNCTION 'ZWM_PAL_PICKING'
***    EXPORTING
***      armazem         = i_lgnum
***      actualiza       = i_actualiza
***      remontada       = i_remontada
***    TABLES
***      zpalete_picking = zpalete_picking[].



  TYPES: BEGIN OF lty_vbpa,
           vbeln LIKE vbpa-vbeln,
           adrnr LIKE vbpa-adrnr,
           kunnr LIKE vbpa-kunnr,
           parvw LIKE vbpa-parvw,
         END OF lty_vbpa.

  DATA: lt_pal_picking TYPE TABLE OF zpalete_picking,
        lt_paletes     TYPE TABLE OF zpalete_picking,
        lt_vbeln_va    TYPE TABLE OF vbeln,
        lt_vbeln_vl    TYPE TABLE OF vbeln,
        lt_tknum       TYPE TABLE OF tknum,
        lt_vbfa        TYPE TABLE OF vbfa,
        lt_refnr       TYPE TABLE OF lvs_refnr,
        lt_lips        TYPE TABLE OF lips,
        lt_vbss        TYPE TABLE OF vbss,
        lt_vbpa        TYPE TABLE OF lty_vbpa,
        lt_vbap        TYPE TABLE OF vbap,
        lt_zwm031      TYPE TABLE OF zwm031,
        lt_zroutyn_t23 TYPE TABLE OF zroutyn_t23.

  DATA: ls_lips    TYPE lips,
        ls_vbss    TYPE vbss,
        ls_vbpa    TYPE lty_vbpa,
        ls_vbap    TYPE vbap,
        ls_vbfa    TYPE vbfa,
        ls_zwm031  TYPE zwm031,
        ls_paletes TYPE zpalete_picking.

  DATA: lv_lgnum       TYPE lgnum,
        lv_lgort       TYPE lgort,
        lv_meins       TYPE meins,
        lv_vbeln       TYPE vbeln,
        lv_npal        TYPE zroutyn_npal,
        lv_palete      TYPE i,
        lv_meia_palete TYPE i,
        lv_vkorg       TYPE vkorg,
        lv_werks       TYPE werks_d,
        lv_trfzn       TYPE trfzn,
        lv_aggreg      TYPE zroutyn_aggreg.

  FIELD-SYMBOLS: <ls_pal_picking> TYPE zpalete_picking,
                 <lt_table>       TYPE ANY TABLE,
                 <ls_data>        TYPE any,
                 <lv_data>        TYPE any.


** Valores de entrada
***********************************************************************
  APPEND LINES OF zpalete_picking TO lt_pal_picking.

** Get Delivery
***********************************************************************
  IF NOT it_vbeln_vl IS INITIAL.
    ASSIGN it_vbeln_vl TO <lt_table>.
    LOOP AT <lt_table> ASSIGNING <ls_data>.
      UNASSIGN <lv_data>.
      ASSIGN COMPONENT 'VBELN' OF STRUCTURE <ls_data> TO <lv_data>.
      IF NOT <lv_data> IS ASSIGNED.
        ASSIGN <ls_data> TO <lv_data>.
      ENDIF.
      CHECK NOT <lv_data> IS INITIAL.
      APPEND <lv_data> TO lt_vbeln_vl.
    ENDLOOP.
  ENDIF.


  IF NOT i_vbeln_vl IS INITIAL.
    APPEND i_vbeln_vl TO lt_vbeln_vl.
  ENDIF.

  IF NOT ir_vbeln_vl IS INITIAL.
    SELECT vbeln FROM likp
                 INTO TABLE lt_vbeln_vl
                 WHERE vbeln IN ir_vbeln_vl.
  ENDIF.

** Get OV
***********************************************************************
  IF NOT it_vbeln_va IS INITIAL.
    ASSIGN it_vbeln_va TO <lt_table>.
    LOOP AT <lt_table> ASSIGNING <ls_data>.
      UNASSIGN <lv_data>.
      ASSIGN COMPONENT 'VBELN' OF STRUCTURE <ls_data> TO <lv_data>.
      IF NOT <lv_data> IS ASSIGNED.
        ASSIGN <ls_data> TO <lv_data>.
      ENDIF.
      CHECK <lv_data> IS ASSIGNED.
      CHECK NOT <lv_data> IS INITIAL.
      APPEND <lv_data> TO lt_vbeln_va.
    ENDLOOP.
  ENDIF.

  IF NOT i_vbeln_va IS INITIAL.
    APPEND i_vbeln_va TO lt_vbeln_va.
  ENDIF.

  IF NOT lt_vbeln_va IS INITIAL.
    SELECT * FROM vbap
             INTO TABLE lt_vbap
             FOR ALL ENTRIES IN lt_vbeln_va
             WHERE vbeln = lt_vbeln_va-table_line.

    LOOP AT lt_vbap INTO ls_vbap.
      CLEAR: ls_paletes.
      ls_paletes-vbeln = ls_vbap-vbeln.
      ls_paletes-posnr = ls_vbap-posnr.
      ls_paletes-werks = ls_vbap-werks.
      ls_paletes-lgort = ls_vbap-lgort.

      ls_paletes-matnr = ls_vbap-matnr.
      ls_paletes-lfimg = ls_vbap-klmeng.
      ls_paletes-meins = ls_vbap-meins.


      ls_paletes-vrkme = ls_vbap-vrkme.
      ls_paletes-voleh = ls_vbap-voleh.
      ls_paletes-volum = ls_vbap-volum.

      ls_paletes-nodel = abap_true.

      COLLECT ls_paletes INTO lt_pal_picking.
    ENDLOOP.

    IF lt_vbeln_vl IS INITIAL.
      SELECT vbeln FROM vbfa
                   INTO TABLE lt_vbeln_vl
                   FOR ALL ENTRIES IN lt_vbeln_va
                   WHERE vbelv = lt_vbeln_va-table_line AND
                         vbtyp_n = 'J'.
    ELSE.
      SORT lt_vbeln_vl.

      SELECT * FROM vbfa
                INTO TABLE lt_vbfa
                FOR ALL ENTRIES IN lt_vbeln_va
                WHERE vbelv = lt_vbeln_va-table_line AND
                      vbtyp_n = 'J'.
      SORT lt_vbfa BY vbelv posnv.

      LOOP AT lt_pal_picking ASSIGNING <ls_pal_picking>.

        LOOP AT lt_vbfa INTO ls_vbfa WHERE vbelv = <ls_pal_picking>-vbeln AND
                                           posnv = <ls_pal_picking>-posnr.
**          CLEAR: ls_vbfa.
**          READ TABLE lt_vbfa
**                INTO ls_vbfa
**                WITH KEY vbelv = <ls_pal_picking>-vbeln
**                         posnv = <ls_pal_picking>-posnr
**                BINARY SEARCH.

          READ TABLE lt_vbeln_vl
                TRANSPORTING NO FIELDS
                WITH KEY table_line = ls_vbfa-vbeln.

          CHECK sy-subrc <> 0.

          <ls_pal_picking>-lfimg = <ls_pal_picking>-lfimg - ls_vbfa-rfmng.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ELSEIF NOT lt_vbeln_vl IS INITIAL.


    SELECT * FROM t311a AS a
             INNER JOIN lips AS b
             ON b~vbeln EQ a~rbnum
             APPENDING CORRESPONDING FIELDS OF TABLE lt_pal_picking
             FOR ALL ENTRIES IN lt_vbeln_vl
             WHERE b~vbeln = lt_vbeln_vl-table_line AND
                   b~pstyv <> 'ZPAS' AND
                   b~pstyv <> 'ZPAL' AND
                   b~lgort = 'CD'.

  ENDIF.

** Retora Grupo
***********************************************************************

  IF NOT it_refnr IS INITIAL.
    ASSIGN it_refnr TO <lt_table>.
    LOOP AT <lt_table> ASSIGNING <ls_data>.
      UNASSIGN <lv_data>.
      ASSIGN COMPONENT 'REFNR' OF STRUCTURE <ls_data> TO <lv_data>.
      IF NOT <lv_data> IS ASSIGNED.
        ASSIGN <ls_data> TO <lv_data>.
      ENDIF.
      CHECK <lv_data> IS ASSIGNED.
      CHECK NOT <lv_data> IS INITIAL.
      APPEND <lv_data> TO lt_refnr.
    ENDLOOP.
  ENDIF.


  IF NOT i_refnr IS INITIAL.
    APPEND i_refnr TO lt_refnr.
  ENDIF.

  IF NOT ir_refnr IS INITIAL.
    SELECT refnr FROM t311a
                 INTO TABLE lt_refnr
                 WHERE lgnum = i_lgnum AND
                       refnr IN ir_refnr.
  ENDIF.

  IF NOT lt_refnr IS INITIAL.
    SORT lt_refnr.
    DELETE ADJACENT DUPLICATES FROM lt_refnr COMPARING ALL FIELDS.

    SELECT * FROM t311a AS a
             INNER JOIN lips AS b
             ON  a~rbnum EQ b~vbeln
             APPENDING CORRESPONDING FIELDS OF TABLE lt_pal_picking
             FOR ALL ENTRIES IN lt_refnr
             WHERE a~lgnum = i_lgnum AND
                   a~refnr = lt_refnr-table_line AND
                   b~pstyv <> 'ZPAS' AND
                   b~pstyv <> 'ZPAL' AND
                   b~lgort = 'CD'.
  ENDIF.

** Retorna Documento de Transporte
***********************************************************************
  IF NOT it_tknum IS INITIAL.
    ASSIGN it_tknum TO <lt_table>.
    LOOP AT <lt_table> ASSIGNING <ls_data>.
      UNASSIGN <lv_data>.
      ASSIGN COMPONENT 'TKNUM' OF STRUCTURE <ls_data> TO <lv_data>.
      IF NOT <lv_data> IS ASSIGNED.
        ASSIGN <ls_data> TO <lv_data>.
      ENDIF.
      CHECK <lv_data> IS ASSIGNED.
      CHECK NOT <lv_data> IS INITIAL.
      APPEND <lv_data> TO lt_tknum.
    ENDLOOP.
  ENDIF.


  IF NOT i_tknum IS INITIAL.
    APPEND i_tknum TO lt_tknum.
  ENDIF.

  IF NOT ir_tknum IS INITIAL.
    SELECT tknum FROM vttk
                 INTO TABLE lt_tknum
                 WHERE tknum IN ir_tknum.
  ENDIF.

  IF NOT lt_tknum IS INITIAL.
    SELECT * FROM vttp AS a
             INNER JOIN lips AS b
             ON  a~vbeln EQ b~vbeln
             INNER JOIN t311a AS c
             ON a~vbeln EQ c~rbnum
             APPENDING CORRESPONDING FIELDS OF TABLE lt_pal_picking
             FOR ALL ENTRIES IN lt_tknum
             WHERE a~tknum = lt_tknum-table_line AND
                   b~pstyv <> 'ZPAS' AND
                   b~pstyv <> 'ZPAL' AND
                   b~lgort = 'CD'.
  ENDIF.

** Dados de Cliente
***********************************************************************
  DO 1 TIMES.
    CHECK NOT lt_pal_picking IS INITIAL.


    SELECT *
      FROM vbss INTO TABLE lt_vbss
      FOR ALL ENTRIES IN lt_pal_picking
      WHERE vbeln = lt_pal_picking-vbeln.

    SELECT vbeln adrnr kunnr parvw
      FROM vbpa INTO TABLE lt_vbpa
      FOR ALL ENTRIES IN lt_pal_picking
      WHERE vbeln = lt_pal_picking-vbeln.

    DELETE lt_vbpa WHERE parvw <> 'WE'
                     AND parvw <> 'AG'.

    SORT lt_vbss BY vbeln.
    SORT lt_vbpa BY vbeln parvw.

    lv_lgnum = i_lgnum.

    LOOP AT lt_pal_picking ASSIGNING <ls_pal_picking>.
      <ls_pal_picking>-sammg = <ls_pal_picking>-refnr.


      CLEAR: ls_vbpa.
      READ TABLE lt_vbpa
            INTO ls_vbpa
            WITH KEY vbeln = <ls_pal_picking>-vbeln
                             parvw = 'WE'.

      IF sy-subrc = 0.
        <ls_pal_picking>-kunnr = ls_vbpa-kunnr.
      ENDIF.

      CLEAR: ls_vbpa.
      READ TABLE lt_vbpa
            INTO ls_vbpa
            WITH KEY vbeln = <ls_pal_picking>-vbeln
                             parvw = 'AG'.

      IF sy-subrc = 0.
        <ls_pal_picking>-kunag = ls_vbpa-kunnr.
      ENDIF.

      IF <ls_pal_picking>-lgort IS INITIAL.
        <ls_pal_picking>-lgort = 'CD'.
      ENDIF.

      IF lv_lgnum IS INITIAL AND
         NOT <ls_pal_picking>-werks IS INITIAL.

        SELECT SINGLE lgnum
                 FROM t320
                 INTO lv_lgnum
                 WHERE werks = <ls_pal_picking>-werks AND
                       lgort = <ls_pal_picking>-lgort.

      ENDIF.
    ENDLOOP.
  ENDDO.


***********************************************************************
  CALL FUNCTION 'ZWM_PAL_PICKING'
    EXPORTING
      armazem         = lv_lgnum
      actualiza       = i_actualiza
      remontada       = i_remontada
    TABLES
      zpalete_picking = lt_pal_picking.


  zpalete_picking[] = lt_pal_picking.



** Calculo de Paletes
***********************************************************************
** Paletes Remontadas
  IF lt_pal_picking IS NOT INITIAL AND i_load_calc EQ abap_true.
    SELECT *
      FROM zwm031 INTO TABLE lt_zwm031
      FOR ALL ENTRIES IN lt_pal_picking
      WHERE lgnum = lv_lgnum         AND
            kunnr = lt_pal_picking-kunnr AND
            matnr = lt_pal_picking-matnr.

    SORT lt_zwm031 BY kunnr matnr.
  ENDIF.

** Número de Paletes
  LOOP AT lt_pal_picking INTO ls_paletes WHERE sub_item <> '000099'.

    " Paletização Especial
    CLEAR ls_zwm031.
    READ TABLE lt_zwm031 INTO ls_zwm031 WITH KEY kunnr = ls_paletes-kunnr
                                                 matnr = ls_paletes-matnr.

    IF ls_zwm031-unporpal IS NOT INITIAL AND
       ls_paletes-lfimg > ls_zwm031-unporpal.

      lv_npal = ( ls_paletes-lfimg / ls_zwm031-unporpal ).
      lv_npal =  ceil( lv_npal ).

      IF ls_zwm031-remontada IS NOT INITIAL.
        lv_npal = lv_npal / 2.
      ENDIF.

      c_npal = c_npal + lv_npal.

      lv_palete = lv_palete + lv_npal.

      c_pal_spec = 'X'.

    ELSE.
      c_npal   = c_npal + ls_paletes-pal_completa + ls_paletes-pal_picking.

      lv_palete = lv_palete + ls_paletes-pal_completa.

      IF ls_paletes-pal_picking IS NOT INITIAL.
        IF ls_paletes-pallet_type = 'M'.
          lv_meia_palete = lv_meia_palete + ls_paletes-pal_picking.
        ELSE.
          lv_palete = lv_palete + ls_paletes-pal_picking.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

** Custo à Palete
  lv_aggreg = lv_vkorg.

  " França - Não são enviadas meias paletes
  READ TABLE lt_zroutyn_t23
    TRANSPORTING NO FIELDS
    WITH KEY aggreg = lv_aggreg.
  IF sy-subrc = 0.
    lv_palete = lv_palete + lv_meia_palete.
    lv_meia_palete = 0.
  ENDIF.

  CALL FUNCTION 'ZROUTYN_GET_PAL_COST'
    EXPORTING
      i_palete      = lv_palete
      i_meia_palete = lv_meia_palete
      i_trfzn       = lv_trfzn
      i_werks       = lv_werks
    IMPORTING
      e_cost_pal    = c_cost_pal
    EXCEPTIONS
      error         = 1
      OTHERS        = 2.



***** Paletes Remessa
*************************************************************************
  IF lt_vbeln_vl IS NOT INITIAL.

    SELECT *
      FROM lips INTO TABLE lt_lips
      FOR ALL ENTRIES IN lt_vbeln_vl
      WHERE vbeln = lt_vbeln_vl-table_line.

    DELETE lt_lips WHERE pstyv = 'ZPAS' OR pstyv = 'ZPAL'.
    DELETE lt_lips WHERE lgort <> 'CD'.

    SORT lt_lips BY vbeln posnr.

    IF lt_lips[] IS NOT INITIAL.
      SELECT *
        FROM vbss INTO TABLE lt_vbss
        FOR ALL ENTRIES IN lt_lips
        WHERE vbeln = lt_lips-vbeln.

      SELECT vbeln adrnr kunnr parvw
        FROM vbpa INTO TABLE lt_vbpa
        FOR ALL ENTRIES IN lt_lips
        WHERE vbeln = lt_lips-vbeln.

      DELETE lt_vbpa WHERE parvw <> 'WE'
                       AND parvw <> 'AG'.
    ENDIF.

    SORT lt_vbss BY vbeln.
    SORT lt_vbpa BY vbeln parvw.

    REFRESH: lt_paletes.

    lv_lgort = 'CD'.

    CLEAR: lv_lgnum, lv_meins.

    LOOP AT lt_lips INTO ls_lips.

      IF lv_lgnum IS INITIAL.
        SELECT SINGLE lgnum
          FROM t320 INTO lv_lgnum
          WHERE werks = ls_lips-werks AND
                lgort = ls_lips-lgort.
      ENDIF.

      CLEAR: ls_paletes.
      MOVE-CORRESPONDING ls_lips TO ls_paletes.

      CLEAR: ls_vbss.
      READ TABLE lt_vbss
            INTO ls_vbss
         WITH KEY vbeln = ls_lips-vbeln BINARY SEARCH.
      IF sy-subrc = 0.
        ls_paletes-sammg = ls_vbss-sammg.
        ls_paletes-refnr = ls_vbss-sammg.
      ENDIF.

      CLEAR: ls_vbpa.
      READ TABLE lt_vbpa
            INTO ls_vbpa
            WITH KEY vbeln = ls_lips-vbeln
                     parvw = 'WE'.
      IF sy-subrc = 0.
        ls_paletes-kunnr = ls_vbpa-kunnr.
      ENDIF.

      CLEAR: ls_vbpa.
      READ TABLE lt_vbpa
            INTO ls_vbpa
            WITH KEY vbeln = ls_lips-vbeln
                     parvw = 'AG'.
      IF sy-subrc = 0.
        ls_paletes-kunag = ls_vbpa-kunnr.
      ENDIF.

      APPEND ls_paletes TO lt_paletes.
    ENDLOOP.

    CHECK lt_paletes[] IS NOT INITIAL.

    CALL FUNCTION 'ZWM_PAL_PICKING'
      EXPORTING
        armazem         = lv_lgnum
        actualiza       = ''
        remontada       = 'X'
      TABLES
        zpalete_picking = lt_paletes.


    LOOP AT lt_paletes INTO ls_paletes WHERE sub_item <> '000099'.

      " Paletização Especial
      CLEAR ls_zwm031.
      READ TABLE lt_zwm031
            INTO ls_zwm031
            WITH KEY kunnr = ls_paletes-kunnr
                     matnr = ls_paletes-matnr.

      IF ls_zwm031-unporpal IS NOT INITIAL AND ls_paletes-lfimg > ls_zwm031-unporpal.
        lv_npal = ( ls_paletes-lfimg / ls_zwm031-unporpal ).

        IF ls_zwm031-remontada IS NOT INITIAL.
          lv_npal = lv_npal / 2.
        ENDIF.

        lv_npal = ceil( lv_npal ).

        c_npal_rem = c_npal_rem + lv_npal.

        c_pal_spec = 'X'.

      ELSE.
        c_npal_rem = c_npal_rem + ls_paletes-pal_completa + ls_paletes-pal_picking.
      ENDIF.

    ENDLOOP.
  ENDIF.
ENDFUNCTION.
