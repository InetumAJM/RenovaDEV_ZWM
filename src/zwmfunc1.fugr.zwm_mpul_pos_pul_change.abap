FUNCTION zwm_mpul_pos_pul_change.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"     REFERENCE(I_VBELN) TYPE  VBELN OPTIONAL
*"     REFERENCE(IS_ZWM028) TYPE  ZWM028 OPTIONAL
*"  CHANGING
*"     REFERENCE(C_POS_PULMAO)
*"----------------------------------------------------------------------
  DATA: ls_zwm028 TYPE zwm028.

  DATA: lv_mpul_size_portao  TYPE i VALUE 10,
        lv_mpul_size_armazem TYPE i VALUE 7.

  CHECK NOT c_pos_pulmao IS INITIAL.


  PERFORM get_parameter USING i_lgnum
                             'MPUL'
                             'MPUL_SIZE_PORTAO'
                              lv_mpul_size_portao.


  PERFORM get_parameter USING i_lgnum
                             'MPUL'
                             'MPUL_SIZE_ARMAZEM'
                              lv_mpul_size_armazem.

  ls_zwm028 = is_zwm028.

  IF is_zwm028 IS INITIAL.
    SELECT SINGLE * FROM zwm028
                    INTO ls_zwm028
                    WHERE lgnum = i_lgnum AND
                          refnr = i_refnr AND
                          remessa = i_vbeln.
  ENDIF.

  CHECK NOT ls_zwm028-kober IS INITIAL.

  IF ls_zwm028-kober EQ gc_mpul_lado_armazem.
    c_pos_pulmao = c_pos_pulmao + lv_mpul_size_portao.
  ELSEIF ls_zwm028-kober EQ gc_mpul_lado_portao.
    c_pos_pulmao = lv_mpul_size_portao - ( c_pos_pulmao - 1 ).
  ENDIF.
ENDFUNCTION.
