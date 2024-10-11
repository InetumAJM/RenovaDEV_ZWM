FUNCTION z_wmfr_exit_cancel_idoc_rem.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_LTAK) TYPE  LTAK
*"  CHANGING
*"     REFERENCE(CS_IDOC_CONTROL) TYPE  EDIDC
*"     REFERENCE(CT_LTAP_VB) TYPE  TT_LTAP_VB
*"     REFERENCE(CT_IDOC_DATA) TYPE  EDIDD_TT
*"----------------------------------------------------------------------
  DATA: lt_zwm020  TYPE TABLE OF zwm020,
        lt_ltap_vb TYPE tt_ltap_vb.

  DATA: ls_zwm020 TYPE zwm020.

  lt_ltap_vb = ct_ltap_vb.
  DELETE lt_ltap_vb WHERE nlenr IS INITIAL.

** valida se se trata de uma segunda palete Remontada
***********************************************************************
  CHECK NOT lt_ltap_vb IS INITIAL.

  SELECT * FROM zwm020
           INTO TABLE lt_zwm020
           FOR ALL ENTRIES IN lt_ltap_vb
           WHERE armazem = is_ltak-lgnum AND
                 p2      = lt_ltap_vb-nlenr.
  CHECK sy-subrc EQ 0.

  DELETE lt_zwm020 WHERE p1 IS INITIAL OR
                         p2 IS INITIAL.

  CHECK NOT lt_zwm020 IS INITIAL.

** Aborta envio de IDOC
***********************************************************************
  CLEAR: cs_idoc_control, ct_idoc_data.
ENDFUNCTION.
