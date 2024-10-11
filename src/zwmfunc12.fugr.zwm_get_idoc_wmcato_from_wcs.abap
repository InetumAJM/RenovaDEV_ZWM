FUNCTION zwm_get_idoc_wmcato_from_wcs .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TANUM) TYPE  TANUM
*"  CHANGING
*"     REFERENCE(CT_LTAP_CANCEL) TYPE  ZWM_TT_LTAP_CANCL
*"     REFERENCE(CT_IDOC_DATA) TYPE  EDIDD_TT
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: ls_e1ltcah     TYPE e1ltcah.
  DATA: ls_e1ltcai     TYPE e1ltcai.
  DATA: ls_idoc_data   TYPE edidd.
  DATA: ls_ltak        TYPE ltak.
  DATA: ls_ltap_cancel TYPE ltap_cancl.
  DATA: lt_ltap        TYPE TABLE OF ltap WITH HEADER LINE.

** Estorno de OT de WCS
***********************************************************************
  SELECT SINGLE *
    FROM ltak INTO ls_ltak
    WHERE lgnum = i_lgnum
    AND   tanum = i_tanum.

  IF sy-subrc = 0.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = ls_ltak-lgnum
      AND   tanum = ls_ltak-tanum.
  ENDIF.

  READ TABLE lt_ltap INDEX 1.

  " Palete de Pickind/Pal. Especial de Saida do Automático já com reserva
  IF lt_ltap-vlpla = 'AUT' AND lt_ltap-vlenr IS NOT INITIAL AND ls_ltak-refnr IS NOT INITIAL.

    " Limpar campos para não permitir estorno da OT
    READ TABLE ct_idoc_data INTO ls_idoc_data WITH KEY segnam = 'E1LTCAH'.
    IF sy-subrc = 0.

      MOVE ls_idoc_data-sdata TO ls_e1ltcah.

      CLEAR: ls_e1ltcah-lgnum, ls_e1ltcah-tanum.

      MOVE ls_e1ltcah TO ls_idoc_data-sdata.

      MODIFY ct_idoc_data FROM ls_idoc_data INDEX sy-tabix.
    ENDIF.

    LOOP AT ct_ltap_cancel INTO ls_ltap_cancel.
      CLEAR: ls_ltap_cancel-tanum, ls_ltap_cancel-tapos.

      MODIFY ct_ltap_cancel FROM  ls_ltap_cancel INDEX sy-tabix.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
