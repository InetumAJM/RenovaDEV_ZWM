FUNCTION zwm_exit_sapll2pik_001.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_T311) LIKE  T311 STRUCTURE  T311
*"  TABLES
*"      T_TRITEM STRUCTURE  L2SKTRITEM
*"      T_DELITEM STRUCTURE  L2SKDLITEM
*"----------------------------------------------------------------------
  TYPES: BEGIN OF lty_plant,
           werks TYPE werks_d,
           lgort TYPE lgort_d,
         END OF lty_plant.

  TYPES: BEGIN OF lty_likp_mod,
           vbeln TYPE vbeln,
           vkorg TYPE vkorg,
         END OF lty_likp_mod.

  TYPES: BEGIN OF lty_vbpa_mod,
           vbeln TYPE vbeln,
           kunnr TYPE kunnr,
         END OF lty_vbpa_mod.

  DATA: lt_likp    TYPE HASHED TABLE OF lty_likp_mod WITH UNIQUE KEY vbeln,
        lt_vbpa    TYPE HASHED TABLE OF lty_vbpa_mod WITH UNIQUE KEY vbeln,
        lt_zwm065  TYPE HASHED TABLE OF zwm065       WITH UNIQUE KEY kunnr,
        lt_zwm073  TYPE SORTED TABLE OF zwm073       WITH UNIQUE KEY vkorg,
        lt_delitem TYPE TABLE OF l2skdlitem,
        lt_vkorg   TYPE TABLE OF vkorg,
        lt_plant   TYPE TABLE OF lty_plant.

  DATA: ls_likp    TYPE lty_likp_mod,
        ls_vbpa    TYPE lty_vbpa_mod,
        ls_zwm065  TYPE zwm065,
        ls_zwm073  TYPE zwm073,
        ls_delitem TYPE l2skdlitem,
        ls_plant   TYPE lty_plant.

  DATA: lv_2steppick      TYPE c,
        lv_2steppick_last TYPE c,
        lv_error          TYPE flag,
        lv_posnr          TYPE posnr,
        lv_is_ser1        TYPE flag,
        lv_lines          TYPE sytabix,
        lv_2step          TYPE flag.

  FIELD-SYMBOLS: <ls_delitem> TYPE l2skdlitem.

  CHECK NOT t_delitem IS INITIAL.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

  SELECT vbeln vkorg FROM likp
                     INTO TABLE lt_likp
                     FOR ALL ENTRIES IN t_delitem
                     WHERE vbeln = t_delitem-vbeln.

  lt_delitem = t_delitem[].


  CLEAR: lv_posnr.
  SELECT vbeln kunnr FROM vbpa
                     INTO TABLE lt_vbpa
                     FOR ALL ENTRIES IN t_delitem
                     WHERE vbeln = t_delitem-vbeln AND
                           posnr = lv_posnr AND
                           parvw = 'W1'.

  SELECT * FROM zwm065
           INTO TABLE lt_zwm065
           WHERE lgnum = i_t311-lgnum.

  SELECT * FROM zwm073
           INTO TABLE lt_zwm073
           WHERE lgnum = i_t311-lgnum.

  LOOP AT t_delitem ASSIGNING <ls_delitem>.

    ls_plant-werks = <ls_delitem>-werks.
    ls_plant-lgort = <ls_delitem>-lgort.
    APPEND ls_plant TO lt_plant.

    CLEAR: ls_likp.
    READ TABLE lt_likp
          INTO ls_likp
          WITH TABLE KEY vbeln = <ls_delitem>-vbeln.

    CHECK sy-subrc EQ 0.

    APPEND ls_likp-vkorg TO lt_vkorg.

    IF ls_likp-vkorg EQ 'SER1'.
      <ls_delitem>-l2skr = '2'.
      lv_2steppick = '1'.
      lv_is_ser1 = abap_true.
      CONTINUE.
    ENDIF.

*   Pincking Parcial em 2 Passos
    DO 1 TIMES.
      CLEAR: ls_vbpa.
      READ TABLE lt_vbpa
            INTO ls_vbpa
            WITH TABLE KEY vbeln = <ls_delitem>-vbeln.
      CHECK sy-subrc EQ 0.

      " Ou tem picking por cliente
      CLEAR: ls_zwm065.
      READ TABLE lt_zwm065
            INTO ls_zwm065
            WITH TABLE KEY kunnr = ls_vbpa-kunnr.
      CHECK sy-subrc <> 0.

      " Ou tem picking por OV
      CLEAR: ls_zwm073.
      READ TABLE lt_zwm073
            INTO ls_zwm073
            WITH TABLE KEY vkorg = ls_likp-vkorg.
      CHECK sy-subrc <> 0.
    ENDDO.

    IF sy-subrc EQ 0.
      <ls_delitem>-l2skr = '2'.
      lv_2steppick = '1'.
      CONTINUE.
    ENDIF.

    CLEAR <ls_delitem>-l2skr.
    lv_2steppick = '0'.
  ENDLOOP.

** Valida Centros
***********************************************************************

*  SORT lt_plant.
*  DELETE ADJACENT DUPLICATES FROM lt_plant COMPARING ALL FIELDS.
*  DESCRIBE TABLE lt_plant LINES lv_lines.
*
*  IF lv_lines > 1.
*    lv_error = abap_true.
*  ENDIF.


** Valida SERVISAN
***********************************************************************
  IF lv_is_ser1 EQ abap_true.
    DELETE lt_vkorg WHERE table_line EQ 'SER1'.
    IF NOT lt_vkorg IS INITIAL.
      lv_error = abap_true.
    ENDIF.
  ENDIF.

** Check
***********************************************************************
  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum = i_t311-lgnum
      is_t311 = i_t311
    IMPORTING
      e_2step = lv_2step
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.

  LOOP AT t_delitem ASSIGNING <ls_delitem>.
    IF lv_2step IS INITIAL.
      CLEAR <ls_delitem>-l2skr.
    ELSE.
      <ls_delitem>-l2skr = '2'.
    ENDIF.
  ENDLOOP.


** Valida se todas as Entradas são de 2 Passos
***********************************************************************
  CHECK lv_error EQ abap_true.

  LOOP AT t_delitem ASSIGNING <ls_delitem>.
    CLEAR <ls_delitem>-l2skr.
  ENDLOOP.
ENDFUNCTION.
