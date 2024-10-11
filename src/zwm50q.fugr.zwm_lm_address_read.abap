FUNCTION ZWM_LM_ADDRESS_READ.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IF_PARVW) LIKE  VBPA-PARVW DEFAULT SPACE
*"  TABLES
*"      CT_POSTAB STRUCTURE  ZWMOV
*"----------------------------------------------------------------------
  data: begin of lt_addr_pa occurs 10,
          parnr like knvk-parnr,
          name1 like lfa1-name1,
          kunnr like kna1-kunnr,                            "n_563992
          lifnr like lfa1-lifnr,
          ort01 like lfa1-ort01,
        end of lt_addr_pa.
  data: begin of lt_addr_pe occurs 10,
          pernr like vbpa-pernr,
          name1 like lfa1-name1,
          ort01 like lfa1-ort01,
        end of lt_addr_pe.
  data: begin of lt_ku occurs 10,
          kunnr like kna1-kunnr,
        end of lt_ku.
  data: begin of lt_li occurs 10,
          lifnr like lfa1-lifnr,
        end of lt_li.
  data: begin of lt_pa occurs 10,
          parnr like vbpa-parnr,
        end of lt_pa.
  data: begin of lt_pe occurs 10,
          pernr like vbpa-pernr,
        end of lt_pe.
  data: begin of lt_vbpa_select occurs 0,                   "50A
          vbeln like vbpa-vbeln,
        end of lt_vbpa_select,
        begin of lt_vbpa_select2 occurs 0,                  "470
          vbeln like vbpa-vbeln,
        end of lt_vbpa_select2,
        lf_subrc like sy-subrc,
        lt_vbpa  like vbpa  occurs 0 with header line,
        lt_vbadr like vbadr occurs 0 with header line.

  data: lf_ku_lines type i,
        lf_li_lines type i,
        lf_pa_lines type i,
        lf_pe_lines type i,
        lf_kunnr like vbpa-kunnr,
        lf_lifnr like vbpa-lifnr,
        lf_pernr like vbpa-pernr,
        lf_parnr like vbpa-parnr,
        lf_nrart like tpar-nrart,
        ls_hrmr_rep like hrmr_rep,
        lf_no_entry(5) type c value '*****'.

  field-symbols: <ls_postab> type zwmov.

*     Check partner type: search suitable matchcode object
  if not if_parvw is initial.
    perform check_partner_type using    if_parvw
                               changing lf_nrart.
  endif.
* 1. Extract partners from CT_POSTAB
  loop at ct_postab assigning <ls_postab>
                    where posnr eq posnr_initial
                    and   not vbeln is initial.
    lt_vbpa_select-vbeln = <ls_postab>-vbeln.               "50A
    append lt_vbpa_select.
    perform check_for_replenishment using    <ls_postab>-lfart
                                    changing lf_subrc.      "470
    if lf_subrc eq 0.
      lt_vbpa_select2-vbeln = <ls_postab>-vbeln.
      append lt_vbpa_select2.
    endif.
    if not <ls_postab>-kunnr is initial.
      read table gt_kna1 with key kunnr = <ls_postab>-kunnr
                         binary search
                         transporting no fields.
      if sy-subrc ne 0.
        move <ls_postab>-kunnr to lt_ku-kunnr.
        collect lt_ku.
      endif.
    endif.
    if not <ls_postab>-kunag is initial.
      read table gt_kna1 with key kunnr = <ls_postab>-kunag
                         binary search
                         transporting no fields.
      if sy-subrc ne 0.
        move <ls_postab>-kunag to lt_ku-kunnr.
        collect lt_ku.
      endif.
    endif.
    if not <ls_postab>-spdnr is initial.
      read table gt_lfa1 with key lifnr = <ls_postab>-spdnr
                         binary search
                         transporting no fields.
      if sy-subrc ne 0.
        move <ls_postab>-spdnr to lt_li-lifnr.
        collect lt_li.
      endif.
    endif.
    if not <ls_postab>-lifnr is initial.
      read table gt_lfa1 with key lifnr = <ls_postab>-lifnr
                         binary search
                         transporting no fields.
      if sy-subrc ne 0.
        move <ls_postab>-lifnr to lt_li-lifnr.
        collect lt_li.
      endif.
    endif.
    if not <ls_postab>-partn is initial
       and not if_parvw is initial.
      case lf_nrart.
        when 'KU'.
          move <ls_postab>-partn to lt_ku-kunnr.
          read table gt_kna1 with key kunnr = lt_ku-kunnr
                             binary search
                             transporting no fields.
          if sy-subrc ne 0.
            collect lt_ku.
          endif.
        when 'LI'.
          move <ls_postab>-partn to lt_li-lifnr.
          read table gt_lfa1 with key lifnr = lt_li-lifnr
                             binary search
                             transporting no fields.
          if sy-subrc ne 0.
            collect lt_li.
          endif.
        when 'AP'.
          move <ls_postab>-partn to lt_pa-parnr.
          collect lt_pa.
        when 'PE'.
          move <ls_postab>-partn to lt_pe-pernr.
          collect lt_pe.
      endcase.
    endif.
  endloop.

* Read name and assigned customer of the contact person     "v_n_563992
  describe table lt_pa lines lf_pa_lines.
  if lf_pa_lines ne 0.
    select parnr kunnr lifnr name1 from knvk
                 appending corresponding fields of table lt_addr_pa
                 for all entries in lt_pa
                 where parnr eq lt_pa-parnr.
    loop at lt_addr_pa.
      if not lt_addr_pa-kunnr is initial.
        read table gt_kna1 with key kunnr = lt_addr_pa-kunnr
                           binary search
                           transporting no fields.
        if sy-subrc ne 0.
          move lt_addr_pa-kunnr to lt_ku-kunnr.
          collect lt_ku.
        endif.
      elseif not lt_addr_pa-lifnr is initial.
        read table gt_lfa1 with key lifnr = lt_addr_pa-lifnr
                           binary search
                           transporting no fields.
        if sy-subrc ne 0.
          move lt_addr_pa-lifnr to lt_li-lifnr.
          collect lt_li.
        endif.
      endif.
    endloop.
  endif.                                                    "^_n_563992

  describe table lt_ku lines lf_ku_lines.
  describe table lt_li lines lf_li_lines.
  describe table lt_pe lines lf_pe_lines.
* 2. Read name and place from the customer's master data
  if lf_ku_lines ne 0.
    select * from kna1
              appending table gt_kna1
              for all entries in lt_ku
              where kunnr eq lt_ku-kunnr.
    sort gt_kna1 by kunnr.
  endif.
* 3. Read name and place from the vendor's master data
  if lf_li_lines ne 0.
    select * from lfa1
             appending table gt_lfa1
             for all entries in lt_li
             where lifnr eq lt_li-lifnr.
    sort gt_lfa1 by lifnr.
  endif.
* 4. Lines deleted n_563992
* 5. Read name of the employee
  loop at lt_pe.
    call function 'SD_REPRESENTANT_GET_DATA'
         exporting
              fi_pernr          = lt_pe-pernr
         importing
              fe_hrmr_rep       = ls_hrmr_rep
         exceptions
              pernr_not_found   = 1
              no_authorization  = 2
              error_occurs      = 3
              address_not_found = 4
              others            = 5.
*   46A: HR function module deactivated. Instead of private address
*   data, business address data are read now (seems to be more useful)
*   If required, the old function module can be reactivated
*    CALL FUNCTION 'HR_REPRESENTANT_GET_DATA'
*         EXPORTING
*              P_PERNR           = LT_PE-PERNR
*         IMPORTING
*              P_HRMR_REP        = LS_HRMR_REP
*         EXCEPTIONS
*              PERNR_NOT_FOUND   = 1
*              NO_AUTHORIZATION  = 2
*              PAREA_NOT_FOUND   = 3
*              ADDRESS_NOT_FOUND = 4
*              OTHERS            = 5.

    if sy-subrc eq 0.
      move ls_hrmr_rep-namee to lt_addr_pe-name1.
      move ls_hrmr_rep-location_1 to lt_addr_pe-ort01.
    else.
      lt_addr_pe-name1 = lf_no_entry.
      lt_addr_pe-ort01 = lf_no_entry.
    endif.
    lt_addr_pe-pernr = lt_pe-pernr.
    append lt_addr_pe.
  endloop.

  describe table gt_kna1 lines lf_ku_lines.
  describe table gt_lfa1 lines lf_li_lines.
  describe table lt_addr_pa lines lf_pa_lines.
  describe table lt_addr_pe lines lf_pe_lines.
  if lf_li_lines eq 0 and lf_ku_lines eq 0
     and lf_pa_lines eq 0 and lf_pe_lines eq 0.
    exit.
  endif.

* For contact persons: Add place of the assigned customer  "v_n_563992
* or vendor
  loop at lt_addr_pa.
    if not lt_addr_pa-kunnr is initial.
      read table gt_kna1 with key kunnr = lt_addr_pa-kunnr
                         binary search.
      if sy-subrc eq 0.
        lt_addr_pa-ort01 = gt_kna1-ort01.
        modify lt_addr_pa.
      endif.
    elseif not lt_addr_pa-lifnr is initial.
      read table gt_lfa1 with key lifnr = lt_addr_pa-lifnr
                         binary search.
      if sy-subrc eq 0.
        lt_addr_pa-ort01 = gt_lfa1-ort01.
        modify lt_addr_pa.
      endif.
    endif.
  endloop.                                                  "^_n_563992

* 5b. Read partner table to check for manual addresses
  if not lt_vbpa_select[] is initial.                       "50A
    select * from vbpa
             into table lt_vbpa
             for all entries in lt_vbpa_select
             where vbeln eq lt_vbpa_select-vbeln
             and   posnr eq posnr_initial
             and   adrda in ('B', 'C', 'E', 'F').
  endif.
  if not lt_vbpa_select2[] is initial.                      "470
    select * from vbpa
             appending table lt_vbpa
             for all entries in lt_vbpa_select2
             where vbeln eq lt_vbpa_select2-vbeln
             and   posnr eq posnr_initial
             and   adrda in ('A', 'D').
  endif.
  if not lt_vbpa[] is initial.
    sort lt_vbpa by mandt vbeln posnr parvw.
    call function 'SD_V09D_REFRESH'.
    call function 'ADDR_MEMORY_CLEAR'
         exceptions
              unsaved_data_exist = 0
              internal_error     = 0
              others             = 0.
    loop at lt_vbpa.
      call function 'VIEW_VBADR'
           exporting
                input   = lt_vbpa
           importing
                adresse = lt_vbadr
           exceptions
                error   = 1
                others  = 2.
      if sy-subrc ne 0.
        continue.
      endif.
      append lt_vbadr.
    endloop.
    sort lt_vbadr by adrnr.
  endif.

* 6. Add address information to CT_POSTAB
  loop at ct_postab assigning <ls_postab>
                    where not vbeln is initial.
    if not <ls_postab>-kunnr is initial.
      read table lt_vbpa with key vbeln = <ls_postab>-vbeln "50A
                                  posnr = posnr_initial
                                  parvw = 'WE'
                         binary search.
      if sy-subrc eq 0 and
         not lt_vbpa-adrnr is initial.
        read table lt_vbadr with key adrnr = lt_vbpa-adrnr
                            binary search.
        if sy-subrc eq 0.
          <ls_postab>-name_we  = lt_vbadr-name1.
          <ls_postab>-ort01_we = lt_vbadr-ort01.
        else.
          <ls_postab>-name_we  = lf_no_entry.
          <ls_postab>-ort01_we = lf_no_entry.
        endif.
      else.
        read table gt_kna1 with key kunnr = <ls_postab>-kunnr
                           binary search.
        if sy-subrc eq 0.
          <ls_postab>-name_we  = gt_kna1-name1.
          <ls_postab>-ort01_we = gt_kna1-ort01.
        else.
          <ls_postab>-name_we  = lf_no_entry.
          <ls_postab>-ort01_we = lf_no_entry.
        endif.
      endif.
    endif.
    if not <ls_postab>-kunag is initial.
      read table lt_vbpa with key vbeln = <ls_postab>-vbeln "50A
                                  posnr = posnr_initial
                                  parvw = 'AG'
                         binary search.
      if sy-subrc eq 0 and
         not lt_vbpa-adrnr is initial.
        read table lt_vbadr with key adrnr = lt_vbpa-adrnr
                            binary search.
        if sy-subrc eq 0.
          <ls_postab>-name_ag  = lt_vbadr-name1.
          <ls_postab>-ort01_ag = lt_vbadr-ort01.
        else.
          <ls_postab>-name_ag  = lf_no_entry.
          <ls_postab>-ort01_ag = lf_no_entry.
        endif.
      else.
        read table gt_kna1 with key kunnr = <ls_postab>-kunag
                           binary search.
        if sy-subrc eq 0.
          <ls_postab>-name_ag  = gt_kna1-name1.
          <ls_postab>-ort01_ag = gt_kna1-ort01.
        else.
          <ls_postab>-name_ag  = lf_no_entry.
          <ls_postab>-ort01_ag = lf_no_entry.
        endif.
      endif.
    endif.
    if not <ls_postab>-spdnr is initial.
      read table lt_vbpa with key vbeln = <ls_postab>-vbeln "50A
                                  posnr = posnr_initial
                                  parvw = 'SP'
                         binary search.
      if sy-subrc eq 0 and
         not lt_vbpa-adrnr is initial.
        read table lt_vbadr with key adrnr = lt_vbpa-adrnr
                            binary search.
        if sy-subrc eq 0.
          <ls_postab>-name_sp  = lt_vbadr-name1.
          <ls_postab>-ort01_sp = lt_vbadr-ort01.
        else.
          <ls_postab>-name_sp  = lf_no_entry.
          <ls_postab>-ort01_sp = lf_no_entry.
        endif.
      else.
        read table gt_lfa1 with key lifnr = <ls_postab>-spdnr
                           binary search.
        if sy-subrc eq 0.
          <ls_postab>-name_sp  = gt_lfa1-name1.
          <ls_postab>-ort01_sp = gt_lfa1-ort01.
        else.
          <ls_postab>-name_sp  = lf_no_entry.
          <ls_postab>-ort01_sp = lf_no_entry.
        endif.
      endif.
    endif.
    if not <ls_postab>-lifnr is initial.
      read table lt_vbpa with key vbeln = <ls_postab>-vbeln "50A
                                  posnr = posnr_initial
                                  parvw = 'LF'
                         binary search.
      if sy-subrc eq 0 and
         not lt_vbpa-adrnr is initial.
        read table lt_vbadr with key adrnr = lt_vbpa-adrnr
                            binary search.
        if sy-subrc eq 0.
          <ls_postab>-name_li  = lt_vbadr-name1.
          <ls_postab>-ort01_li = lt_vbadr-ort01.
        else.
          <ls_postab>-name_li  = lf_no_entry.
          <ls_postab>-ort01_li = lf_no_entry.
        endif.
      else.
        read table gt_lfa1 with key lifnr = <ls_postab>-lifnr
                           binary search.
        if sy-subrc eq 0.
          <ls_postab>-name_li  = gt_lfa1-name1.
          <ls_postab>-ort01_li = gt_lfa1-ort01.
        else.
          <ls_postab>-name_li  = lf_no_entry.
          <ls_postab>-ort01_li = lf_no_entry.
        endif.
      endif.
    endif.
    if not <ls_postab>-partn is initial.
      read table lt_vbpa with key vbeln = <ls_postab>-vbeln "50A
                                  posnr = posnr_initial
                                  parvw = if_parvw
                         binary search.
      if sy-subrc eq 0 and
         not lt_vbpa-adrnr is initial.
        read table lt_vbadr with key adrnr = lt_vbpa-adrnr
                            binary search.
        if sy-subrc eq 0.
          <ls_postab>-name_pa  = lt_vbadr-name1.
          <ls_postab>-ort01_pa = lt_vbadr-ort01.
        else.
          <ls_postab>-name_pa  = lf_no_entry.
          <ls_postab>-ort01_pa = lf_no_entry.
        endif.
      else.
        case lf_nrart.
          when 'KU'.
            lf_kunnr = <ls_postab>-partn.
            read table gt_kna1 with key kunnr = lf_kunnr
                               binary search.
            if sy-subrc eq 0.
              <ls_postab>-name_pa  = gt_kna1-name1.
              <ls_postab>-ort01_pa = gt_kna1-ort01.
            else.
              <ls_postab>-name_pa  = lf_no_entry.
              <ls_postab>-ort01_pa = lf_no_entry.
            endif.
          when 'LI'.
            lf_lifnr = <ls_postab>-partn.
            read table gt_lfa1 with key lifnr = lf_lifnr
                               binary search.
            if sy-subrc eq 0.
              <ls_postab>-name_pa  = gt_lfa1-name1.
              <ls_postab>-ort01_pa = gt_lfa1-ort01.
            else.
              <ls_postab>-name_pa  = lf_no_entry.
              <ls_postab>-ort01_pa = lf_no_entry.
            endif.
          when 'AP'.
            lf_parnr = <ls_postab>-partn.
            read table lt_addr_pa with key parnr = lf_parnr.
            if sy-subrc eq 0.
              <ls_postab>-name_pa  = lt_addr_pa-name1.
              <ls_postab>-ort01_pa = lt_addr_pa-ort01.
            else.
              <ls_postab>-name_pa  = lf_no_entry.
              <ls_postab>-ort01_pa = lf_no_entry.
            endif.
          when 'PE'.
            lf_pernr = <ls_postab>-partn.
            read table lt_addr_pe with key pernr = lf_pernr.
            if sy-subrc eq 0.
              <ls_postab>-name_pa  = lt_addr_pe-name1.
              <ls_postab>-ort01_pa = lt_addr_pe-ort01.
            else.
              <ls_postab>-name_pa  = lf_no_entry.
              <ls_postab>-ort01_pa = lf_no_entry.
            endif.
        endcase.
      endif.
    endif.
  endloop.

endfunction.
