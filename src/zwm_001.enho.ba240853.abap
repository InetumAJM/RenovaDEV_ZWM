"Name: \PR:SAPLV50Q\FO:CHECK_DEL_IN_GROUP\SE:BEGIN\EI
ENHANCEMENT 0 ZWM_001.

  DATA: lt_likp      TYPE TABLE OF likp,
        lt_likp_test TYPE TABLE OF likp,
        lt_zwm039    TYPE TABLE OF zwm039.

  DATA: ls_likp TYPE likp.

  DATA: lv_lines     TYPE sytabix,
        lv_servisan  TYPE flag,
        lv_found     TYPE flag,
        lv_not_found TYPE flag.

  CLEAR lv_servisan.

  DO 1 TIMES.
    CHECK NOT ct_worktab IS INITIAL.

    SELECT * FROM likp
       INTO TABLE lt_likp
       FOR ALL ENTRIES IN ct_worktab
       WHERE vbeln = ct_worktab-vbeln.

    lt_likp_test = lt_likp.

    SORT lt_likp_test BY vkorg.
    DELETE ADJACENT DUPLICATES FROM lt_likp_test COMPARING vkorg.

    READ TABLE lt_likp_test
      WITH KEY vkorg = 'RP06'
      BINARY SEARCH
      TRANSPORTING NO FIELDS.

    CHECK sy-subrc EQ 0.

    DESCRIBE TABLE lt_likp_test LINES lv_lines.

    IF lv_lines <= 1.
      lv_servisan = 'X'.
      EXIT.
    ENDIF.

**  Grupo não pode conter Remessas com Org de Vend RP06 e outras
    MESSAGE s020(zwm001) DISPLAY LIKE 'E'.
    ef_exit_subrc = 1.
    RETURN.
  ENDDO.

** Valida Emissor Servisan
***********************************************************************
  DO 1 TIMES.
    CHECK lv_servisan EQ 'X'.
    CHECK NOT lt_likp IS INITIAL.

    SELECT * FROM zwm039
       INTO TABLE lt_zwm039
       FOR ALL ENTRIES IN lt_likp
       WHERE kunnr = lt_likp-kunag.

    SORT lt_zwm039 BY kunnr.

    LOOP AT lt_likp INTO ls_likp.

      READ TABLE lt_zwm039
        WITH KEY kunnr = ls_likp-kunag
        BINARY SEARCH
        TRANSPORTING NO FIELDS.

      IF sy-subrc EQ 0.
        lv_found = 'X'.
      ELSE.
        lv_not_found = 'X'.
      ENDIF.
    ENDLOOP.


    IF lv_found EQ 'X' AND
       lv_not_found EQ 'X'.
**    Grupo RP06 não pode conter Clientes Servisan com Clientes não Servisan
      MESSAGE s036(zwm001) DISPLAY LIKE 'E'.
      ef_exit_subrc = 1.
      RETURN.
    ENDIF.
  ENDDO.

ENDENHANCEMENT.
