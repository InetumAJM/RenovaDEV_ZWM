FUNCTION ZWM_GET_IDOC_WMTORD_FROM_WCS .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"  CHANGING
*"     REFERENCE(C_LZNUM) TYPE  LZNUM
*"     REFERENCE(CS_IDOC_CONTROL) TYPE  EDIDC
*"     REFERENCE(CS_LTAP_CR) TYPE  LTAP_CR
*"     REFERENCE(CT_IDOC_DATA) TYPE  EDIDD_TT
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: ls_e1ltorh   TYPE e1ltorh.
  DATA: ls_e1ltori   TYPE e1ltori.
  DATA: ls_idoc_data TYPE edidd.

** OT Bloqueio/Desbloqueio
***********************************************************************
  IF cs_ltap_cr-vltyp = 'CHD' AND ( cs_ltap_cr-vlpla = 'LOCK' OR cs_ltap_cr-vlpla = 'UNLOCK' ).

    " SSCC passa para LZNUM
    c_lznum = cs_ltap_cr-vlenr.

    READ TABLE ct_idoc_data INTO ls_idoc_data WITH KEY segnam = 'E1LTORH'.
    IF sy-subrc = 0.
      MOVE ls_idoc_data-sdata TO ls_e1ltorh.

      ls_e1ltorh-lznum = c_lznum.

      MOVE ls_e1ltorh TO ls_idoc_data-sdata.

      MODIFY ct_idoc_data FROM ls_idoc_data INDEX sy-tabix.
    ENDIF.

    " Limpar campo de SU na origem
    CLEAR cs_ltap_cr-vlenr.

    LOOP AT ct_idoc_data INTO ls_idoc_data WHERE segnam = 'E1LTORI'.

      MOVE ls_idoc_data-sdata TO ls_e1ltori.

      CHECK ls_e1ltori-vlenr IS NOT INITIAL.

      CLEAR ls_e1ltori-vlenr.

      MOVE ls_e1ltori TO ls_idoc_data-sdata.

      MODIFY ct_idoc_data FROM ls_idoc_data INDEX sy-tabix.
    ENDLOOP.

    WAIT UP TO 1 SECONDS.

  ENDIF.

ENDFUNCTION.
