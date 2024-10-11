FUNCTION ZWM_LM_BACKGROUND_PROCESSING.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IF_PROCTYPE) TYPE  CHAR1
*"     VALUE(IF_KOMIM) LIKE  RL03T-KOMIM
*"     VALUE(IF_EINLM) LIKE  RL03T-EINLM
*"     VALUE(IF_ALAKT) LIKE  RL03T-ALAKT
*"     VALUE(IF_WADAT_IST) LIKE  LIKP-WADAT_IST
*"  TABLES
*"      IT_WORKTAB STRUCTURE  ZWMOV
*"      IT_OUTPUT_V2 STRUCTURE  LIPOV_OUTPUT OPTIONAL
*"      IT_OUTPUT_V4 STRUCTURE  LIPOV_OUTPUT OPTIONAL
*"  EXCEPTIONS
*"      WRONG_PROCTYPE
*"      NO_PERMISSION
*"----------------------------------------------------------------------
  data: lt_success like zwmov occurs 0 with header line,
        lt_error   like zwmov occurs 0 with header line,
        lt_tvst    like tvst occurs 0 with header line,
        lt_vbfs    type standard table of vbfs,
        lt_vbsk    type shp_vbsk_t,
        lf_success_count type i,
        lf_flag_inbound type c,
        lf_error_count type i,
        lf_notrel_count type i,
        lf_lines_v2 type i,
        lf_lines_v4 type i.

* Check correct processing type and set inbound flag
  if       if_proctype eq proctype_pick
        or if_proctype eq proctype_conf
        or if_proctype eq proctype_gdsi
        or if_proctype eq proctype_dist
        or if_proctype eq proctype_unch                     "AIP
        or if_proctype eq proctype_free.
    lf_flag_inbound = space.
  elseif   if_proctype eq proctype_inbpick
        or if_proctype eq proctype_inbconf
        or if_proctype eq proctype_inbdist
        or if_proctype eq proctype_inbgdrc.
    lf_flag_inbound = charx.
  else.
    raise wrong_proctype.
  endif.

  case if_proctype.
    when proctype_unch.                                     "AIP
      write: 'Prüfung ungeprüfter Lieferungen'(069).
      uline.
      call function 'ZWM_LM_CHECK_UNCHECKED_DELIVERY'
           importing
                ef_success_count = lf_success_count
                ef_error_count   = lf_error_count
           tables
                ct_worktab       = it_worktab
                et_success       = lt_success
                et_error         = lt_error
           changing
                ct_vbsk          = lt_vbsk
           exceptions
                no_permission    = 1
                others           = 2.

      case sy-subrc.
        when 1.
          write: 'Fehlende Berechtigung.'(019).
        when others.
          perform display_log tables lt_success
                                     lt_error
                                     lt_vbfs
                              using  lf_flag_inbound
                                     space.
      endcase.

    when proctype_pick or proctype_inbpick.
      write: 'Anlegen von Transportaufträgen'(018).
      uline.
      call function 'ZWM_LM_TRANSFER_ORDER_CREATE'
           exporting
                if_flag_inbound  = lf_flag_inbound
                if_bitype        = bitype_dark
                if_alakt         = if_alakt
                if_komim         = if_komim
                if_einlm         = if_einlm
           importing
                ef_success_count = lf_success_count
                ef_error_count   = lf_error_count
                ef_notrel_count  = lf_notrel_count
           tables
                ct_worktab       = it_worktab
                it_postab        = it_worktab
                et_success       = lt_success
                et_error         = lt_error
           exceptions
                no_permission    = 1
                no_items         = 2
                others           = 3.
      case sy-subrc.
        when 1.
          write: 'Fehlende Berechtigung.'(019).
        when 2.
          write: 'Keine Positionsdaten.'(020).
        when others.
          perform display_log tables lt_success
                                     lt_error
                                     lt_vbfs
                              using  lf_flag_inbound
                                     space.
      endcase.
    when proctype_conf or proctype_inbconf.
      write: 'Quittieren von Kommissionieraufträgen'(021).
      uline.
      call function 'ZWM_LM_TRANSFER_ORDER_CONFIRM'
           exporting
                if_bitype        = bitype_dark
                if_komim         = if_komim
           importing
                ef_success_count = lf_success_count
                ef_error_count   = lf_error_count
           tables
                ct_worktab       = it_worktab
                et_success       = lt_success
                et_error         = lt_error
           exceptions
                no_permission    = 1
                others           = 2.
      case sy-subrc.
        when 1.
          write: 'Fehlende Berechtigung.'(019).
        when others.
          perform display_log tables lt_success
                                     lt_error
                                     lt_vbfs
                              using  lf_flag_inbound
                                     space.
      endcase.
    when proctype_dist or proctype_inbdist.
      write: 'Verteilung von Lieferungen'(070).
      call function 'ZWM_LM_CHANGE_DLV_DISTRIB_STATE'
        importing
          ef_success_count       = lf_success_count
          ef_error_count         = lf_error_count
        tables
          ct_worktab             = it_worktab
          ct_vbfs                = lt_vbfs
          et_success             = lt_success
          et_error               = lt_error
          et_tvst                = lt_tvst
        exceptions
          no_permission          = 1
          others                 = 2.
      case sy-subrc.
        when 1.
          write: /'Keine Berechtigung für Versandstellen:'(032).
          loop at lt_tvst.
            write lt_tvst-vstel.
          endloop.
        when others.
          perform display_log tables lt_success
                                     lt_error
                                     lt_vbfs
                              using  lf_flag_inbound
                                     space.
      endcase.
    when proctype_gdsi or proctype_inbgdrc.
      if if_proctype eq proctype_gdsi.
        write: 'Buchen des Warenausgangs'(022).
      else.
        write: 'Buchen des Wareneingangs'(036).
      endif.
      uline.
      call function 'ZWM_LM_GOODS_MOVEMENT'
           exporting
                if_bitype        = bitype_dark
                if_wadat_ist     = if_wadat_ist
                if_flag_inbound  = lf_flag_inbound
           importing
                ef_success_count = lf_success_count
                ef_error_count   = lf_error_count
           tables
                ct_worktab       = it_worktab
                et_success       = lt_success
                et_error         = lt_error
                et_tvst          = lt_tvst
                et_vbfs          = lt_vbfs
           exceptions
                no_permission    = 1
                others           = 2.
      case sy-subrc.
        when 1.
          write: /'Keine Berechtigung für Versandstellen:'(032).
          loop at lt_tvst.
            write lt_tvst-vstel.
          endloop.
        when others.
          perform display_log tables lt_success
                                     lt_error
                                     lt_vbfs
                              using  lf_flag_inbound
                                     space.
      endcase.
    when proctype_free.
      describe table it_output_v2 lines lf_lines_v2.
      describe table it_output_v4 lines lf_lines_v4.
      if lf_lines_v2 ne 0.
        write: /'Verarbeitung von Liefernachrichten:'(030).
        uline.
        call function 'ZWM_LM_DELIVERY_OUTPUT'
             exporting
                  if_flag_pick       = space
             tables
                  ct_worktab         = it_worktab
                  it_output_v2       = it_output_v2
                  et_tvst            = lt_tvst
             exceptions
                  no_output_selected = 1
                  no_permission      = 2
                  others             = 3.
        case sy-subrc.
          when 1.
            write: / 'Keine Nachrichten ausgewählt.'(061).
          when 2.
            write: / text-032.
            loop at lt_tvst.
              write lt_tvst-vstel.
            endloop.
          when others.
            perform display_log tables it_worktab
                                       lt_error
                                       lt_vbfs
                                using  lf_flag_inbound
                                       charx.
        endcase.
      endif.
      if lf_lines_v4 ne 0.
        write: /'Verarbeitung von Kommissioniernachrichten:'(031).
        uline.
        call function 'ZWM_LM_DELIVERY_OUTPUT'
             exporting
                  if_flag_pick       = charx
             tables
                  ct_worktab         = it_worktab
                  it_output_v4       = it_output_v4
                  et_tvst            = lt_tvst
             exceptions
                  no_output_selected = 1
                  no_permission      = 2
                  others             = 3.
        case sy-subrc.
          when 1.
            write: / 'Keine Nachrichten ausgewählt.'(061).
          when 2.
            write: / text-032.
            loop at lt_tvst.
              write lt_tvst-vstel.
            endloop.
          when others.
            perform display_log tables it_worktab
                                       lt_error
                                       lt_vbfs
                                using  lf_flag_inbound
                                       charx.
        endcase.
      endif.

  endcase.

endfunction.
