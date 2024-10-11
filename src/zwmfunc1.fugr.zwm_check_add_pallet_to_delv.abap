FUNCTION zwm_check_add_pallet_to_delv.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lt_likp   TYPE TABLE OF likp,
        lt_vbpa   TYPE TABLE OF vbpa,
        lt_t311a  TYPE TABLE OF t311a,
        lt_zwm039 TYPE TABLE OF zwm039,
        lt_zwm059 TYPE TABLE OF zwm059.

  DATA: ls_likp TYPE likp.

  DATA: lv_2step TYPE flag,
        lv_lines TYPE sytabix,
        lv_kunnr TYPE kunnr.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum = i_lgnum
      i_refnr = i_refnr
    IMPORTING
      e_2step = lv_2step
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.


** Retorna Todas as Remessas do Grupo
***********************************************************************
  SELECT * FROM t311a
     INTO TABLE lt_t311a
     WHERE lgnum = i_lgnum AND
           refnr = i_refnr.

  CHECK sy-subrc EQ 0.

** Retorna Todas as Remessas
***********************************************************************
  SELECT * FROM likp
     INTO TABLE lt_likp
     FOR ALL ENTRIES IN lt_t311a
     WHERE vbeln = lt_t311a-rbnum.

  CHECK sy-subrc EQ 0.

  LOOP AT lt_likp INTO ls_likp WHERE vkorg = 'SER1'.

    CLEAR lv_kunnr.
    SELECT SINGLE kunnr INTO lv_kunnr FROM tvko WHERE vkorg = ls_likp-vkorg.
    IF lv_kunnr IS NOT INITIAL.
      ls_likp-kunag = lv_kunnr.
      MODIFY lt_likp FROM ls_likp.
    ENDIF.

  ENDLOOP.

  IF lv_2step EQ abap_true.

** Retorna Todos Os Clientes Servisan
***********************************************************************
    SELECT * FROM zwm039
       INTO TABLE lt_zwm039
       FOR ALL ENTRIES IN lt_likp
       WHERE lgnum = i_lgnum AND
             kunnr = lt_likp-kunag AND
             idser = abap_true.

    SORT lt_zwm039 BY kunnr.

** Valida se Todos Os clientes são Servisan
***********************************************************************
    SORT lt_likp BY kunnr.

    LOOP AT lt_likp INTO ls_likp.
      READ TABLE lt_zwm039
        WITH KEY kunnr = ls_likp-kunag
        BINARY SEARCH
        TRANSPORTING NO FIELDS.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDLOOP.

** Valida Recebedor De Mercadoria 2
***********************************************************************
    SELECT * FROM vbpa
       INTO TABLE lt_vbpa
       FOR ALL ENTRIES IN lt_likp
       WHERE vbeln = lt_likp-vbeln AND
             posnr = '' AND
             parvw = 'W1'.

    SORT lt_vbpa BY kunnr.
    DELETE ADJACENT DUPLICATES FROM lt_vbpa COMPARING kunnr.

    DESCRIBE TABLE lt_vbpa LINES lv_lines.

    IF lv_lines EQ 1.
      EXIT.
    ENDIF.

** Não Adicionar na Remessa
***********************************************************************
  RAISE error.


  ELSE.

** retorna todos os clientes que vão para o operador logistico
***********************************************************************
    SELECT * FROM zwm059
       INTO TABLE lt_zwm059
       FOR ALL ENTRIES IN lt_likp
       WHERE lgnum = i_lgnum AND
             kunnr = lt_likp-kunag.

    SORT lt_zwm059 BY kunnr.

    CHECK lt_zwm059[] IS NOT INITIAL.

***********************************************************************
    SORT lt_likp BY kunnr.

    LOOP AT lt_likp INTO ls_likp.
      READ TABLE lt_zwm059
        WITH KEY kunnr = ls_likp-kunag
        BINARY SEARCH
        TRANSPORTING NO FIELDS.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDLOOP.

** Valida Recebedor De Mercadoria 2
***********************************************************************
    SELECT * FROM vbpa
       INTO TABLE lt_vbpa
       FOR ALL ENTRIES IN lt_likp
       WHERE vbeln = lt_likp-vbeln AND
             posnr = '' AND
             parvw = 'WE'.

    SORT lt_vbpa BY kunnr.
    DELETE ADJACENT DUPLICATES FROM lt_vbpa COMPARING kunnr.

    DESCRIBE TABLE lt_vbpa LINES lv_lines.

    IF lv_lines EQ 1.
      EXIT.
    ENDIF.

** Não Adicionar na Remessa
***********************************************************************
  RAISE error.


  ENDIF.


ENDFUNCTION.
