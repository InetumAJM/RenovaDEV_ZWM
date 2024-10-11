FUNCTION ZWM_LM_CUSTOMER_ADDR_DISPLAY .
*"--------------------------------------------------------------------
*"*"Interface local:
*"       IMPORTING
*"             VALUE(IF_VBELN) LIKE  ZWMOV-VBELN
*"             VALUE(IF_KUNNR) LIKE  ZWMOV-KUNNR
*"             VALUE(IF_PARVW) LIKE  ZWMOV-PARVW DEFAULT SPACE
*"       EXCEPTIONS
*"              INITIAL_NUMBER
*"              READ_ERROR
*"--------------------------------------------------------------------
  data: ls_kna1 like kna1,
        ls_adrs like adrs,
        ls_vbpa like vbpa,
        ls_vbadr like vbadr,
        lf_adrnr like vbpa-adrnr.

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
    if if_kunnr is initial.
      raise initial_number.
    endif.
    call function 'VIEW_KNA1'
         exporting
              kunde     = if_kunnr
         importing
              anschrift = ls_kna1
         exceptions
              no_kna1   = 1
              others    = 2.
    if sy-subrc ne 0.
      message e045.
    endif.
    move-corresponding ls_kna1 to ls_adrs.
  endif.

  call function 'RV_ADDRESS_WINDOW_DISPLAY'
       exporting
            adrswa_in = ls_adrs
            fadrtype  = '2'.

endfunction.
