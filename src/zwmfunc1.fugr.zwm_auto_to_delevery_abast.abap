FUNCTION zwm_auto_to_delevery_abast.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"----------------------------------------------------------------------
  DATA: ls_zwm028 TYPE zwm028.

  DATA: lv_step TYPE flag.

  SELECT SINGLE * FROM zwm028
                  INTO ls_zwm028
                  BYPASSING BUFFER
                  WHERE lgnum   = i_lgnum AND
                        refnr   = i_refnr AND
                        remessa = ''.

  CHECK sy-subrc EQ 0.

  CHECK ls_zwm028-total_paletes EQ ls_zwm028-paletes_carro AND
        ls_zwm028-st_dck EQ 'DCK'.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum = i_lgnum
      i_refnr = i_refnr
    IMPORTING
      e_2step = lv_step
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.

  CHECK lv_step EQ abap_true.

  CALL FUNCTION 'ZWM_TO_DELEVERY_ABAST'
    EXPORTING
      i_lgnum = i_lgnum
      i_refnr = i_refnr
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.
ENDFUNCTION.
