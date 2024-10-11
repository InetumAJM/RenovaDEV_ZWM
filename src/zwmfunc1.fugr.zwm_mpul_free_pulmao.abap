FUNCTION zwm_mpul_free_pulmao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"     REFERENCE(IS_ZWM028) TYPE  ZWM028 OPTIONAL
*"  CHANGING
*"     REFERENCE(CS_LAGPV) TYPE  LAGPV
*"----------------------------------------------------------------------
  DATA: lv_2step TYPE flag.

  DATA: ls_zwm028 TYPE zwm028.

  ls_zwm028 = is_zwm028.

  IF ls_zwm028 IS INITIAL.
    SELECT SINGLE * FROM zwm028
                    INTO ls_zwm028
                    WHERE lgnum   = i_lgnum AND
                          refnr   = i_refnr AND
                          remessa = ' '.
  ENDIF.

** Não é Mini Pulmão
***********************************************************************
  IF ls_zwm028-kober IS INITIAL.
    CLEAR: cs_lagpv-kober,
           cs_lagpv-brand.
    EXIT.
  ENDIF.

** Mini Pulmão
***********************************************************************
  REPLACE ls_zwm028-kober
       IN cs_lagpv-kober
       WITH ''.

  IF cs_lagpv-kober IS INITIAL.
    CLEAR cs_lagpv-brand.
  ENDIF.



ENDFUNCTION.
