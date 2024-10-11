FUNCTION zwm_mpul_camiao_seq_carga_chan.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"     REFERENCE(I_VBELN) TYPE  VBELN OPTIONAL
*"     REFERENCE(IS_ZWM028) TYPE  ZWM028 OPTIONAL
*"  CHANGING
*"     REFERENCE(CT_L_LTAP) TYPE  ZWM01_T_L_LTAP
*"----------------------------------------------------------------------
  DATA: ls_zwm028 TYPE zwm028.


  CHECK NOT ct_l_ltap IS INITIAL.

  ls_zwm028 = is_zwm028.

  IF is_zwm028 IS INITIAL.
    SELECT SINGLE * FROM zwm028
                    INTO ls_zwm028
                    WHERE lgnum = i_lgnum AND
                          refnr = i_refnr.
  ENDIF.

** Original
***********************************************************************
  IF ls_zwm028-kober IS INITIAL.
    SORT l_ltap BY pos_remessa second_pulmao pulmao pos_pulmao.
    EXIT.
  ENDIF.

** Meias Paletes
***********************************************************************
  IF ls_zwm028-kober EQ gc_mpul_lado_armazem.
    SORT ct_l_ltap BY "pos_remessa ASCENDING
                      pulmao      ASCENDING
                      pos_pulmao  DESCENDING.
  ELSE.
*    SORT ct_l_ltap BY pos_remessa pulmao pos_pulmao.
    SORT ct_l_ltap BY pulmao pos_pulmao.
  ENDIF.
ENDFUNCTION.
