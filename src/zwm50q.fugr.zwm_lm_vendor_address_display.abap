FUNCTION ZWM_LM_VENDOR_ADDRESS_DISPLAY.
*"--------------------------------------------------------------------
*"*"Interface local:
*"       IMPORTING
*"             VALUE(IF_VBELN) LIKE  ZWMOV-VBELN
*"             VALUE(IF_LIFNR) LIKE  ZWMOV-LIFNR
*"             VALUE(IF_PARVW) LIKE  ZWMOV-PARVW DEFAULT SPACE
*"       EXCEPTIONS
*"              INITIAL_NUMBER
*"              READ_ERROR
*"--------------------------------------------------------------------
  data: ls_adrs like adrs,
        ls_vbpa like vbpa,
        ls_lfa1 like lfa1,
        ls_vbadr like vbadr.

  if not if_parvw is initial and                            "50A
     not if_vbeln is initial.
    select single * from vbpa
                    into ls_vbpa
                    where vbeln eq if_vbeln
                      and posnr eq posnr_initial
                      and parvw eq if_parvw.
    if sy-subrc eq 0.
      call function 'VIEW_VBADR'
           exporting
                input   = ls_vbpa
           importing
                adresse = ls_vbadr
           exceptions
                error   = 1
                others  = 2.
      if sy-subrc eq 0.
        move-corresponding ls_vbadr to ls_adrs.
      endif.
    endif.
  endif.
  if ls_vbadr is initial or if_parvw is initial.
    if if_lifnr is initial.
      raise initial_number.
    endif.
    call function 'VIEW_LFA1'
         exporting
              lieferant = if_lifnr
         importing
              anschrift = ls_lfa1
         exceptions
              others    = 1.
    if sy-subrc ne 0.
      message e045.
    endif.
    move-corresponding ls_lfa1 to ls_adrs.
  endif.

  call function 'RV_ADDRESS_WINDOW_DISPLAY'
       exporting
            adrswa_in = ls_adrs
            fadrtype  = '2'.

endfunction.
