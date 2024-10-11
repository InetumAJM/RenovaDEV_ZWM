FUNCTION zwm_mpul_seq_carga_change.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"     REFERENCE(I_KOBER) TYPE  KOBER OPTIONAL
*"----------------------------------------------------------------------
  DATA: lt_zwm028 TYPE TABLE OF zwm028.

  DATA: ls_zwm028 TYPE zwm028.

  DATA: lv_pos TYPE i.

  FIELD-SYMBOLS: <ls_zwm028> TYPE zwm028.

** Retorna Todas as entradas válidas
***********************************************************************
  SELECT * FROM zwm028
     INTO TABLE lt_zwm028
     WHERE refnr = i_refnr.

  CHECK sy-subrc EQ 0.

** Valida se é a primeira passagem
***********************************************************************
  LOOP AT lt_zwm028 INTO ls_zwm028 WHERE remessa IS INITIAL.
    IF ls_zwm028-zlock <> 1.
      RETURN.
    ENDIF.
  ENDLOOP.

***********************************************************************
  DELETE lt_zwm028 WHERE remessa IS INITIAL.

** Valida se é meio Pulmão
***********************************************************************
  CLEAR ls_zwm028.
  READ TABLE lt_zwm028
        INTO ls_zwm028
        INDEX 1.

  IF NOT i_kober IS INITIAL.
    CHECK NOT i_kober IS INITIAL.
  ELSE.
    CHECK NOT ls_zwm028-kober IS INITIAL.
  ENDIF.

** Inverte Ordem
***********************************************************************
  SORT lt_zwm028 BY ordem DESCENDING.

  lv_pos = 1.
  LOOP AT lt_zwm028 ASSIGNING <ls_zwm028>.
    <ls_zwm028>-ordem = sy-tabix.
    <ls_zwm028>-posicao_ini_pul = lv_pos.

    lv_pos = lv_pos + <ls_zwm028>-total_paletes.
  ENDLOOP.

** Update à Tabela
***********************************************************************
  MODIFY zwm028 FROM TABLE lt_zwm028.
  COMMIT WORK AND WAIT.
ENDFUNCTION.
