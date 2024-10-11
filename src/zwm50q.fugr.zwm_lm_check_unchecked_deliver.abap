FUNCTION ZWM_LM_CHECK_UNCHECKED_DELIVER.
*"--------------------------------------------------------------------
*"*"Interface local:
*"       EXPORTING
*"             REFERENCE(EF_SUCCESS_COUNT) TYPE  I
*"             REFERENCE(EF_ERROR_COUNT) TYPE  I
*"       TABLES
*"              CT_WORKTAB STRUCTURE  ZWMOV
*"              ET_SUCCESS STRUCTURE  ZWMOV
*"              ET_ERROR STRUCTURE  ZWMOV
*"       CHANGING
*"             REFERENCE(CT_VBSK) TYPE  SHP_VBSK_T OPTIONAL
*"       EXCEPTIONS
*"              NO_PERMISSION
*"--------------------------------------------------------------------

  data: lt_package type leshp_delivery_key_t,
        ls_package type leshp_delivery_key,
        lt_vbls    type shp_vbls_t,
        lt_vbfs    type shp_vbfs_t,
        ls_vbsk    type vbsk,
        lf_select_tabix like sy-tabix,
        begin of lt_select occurs 0,
          vstel like likp-vstel,
          vbeln like likp-vbeln,
        end of lt_select,
        begin of lt_shp_point occurs 0,
          vstel like likp-vstel,
        end of lt_shp_point,
        begin of lt_status occurs 0,
          vbeln like vbuk-vbeln,
          bestk like vbuk-bestk,
        end of lt_status,
        lf_flag_group_update type c,
        lf_subrc type sysubrc.

  clear: ef_success_count, ef_error_count.

  loop at ct_worktab.
    lt_select-vstel = ct_worktab-vstel.
    lt_select-vbeln = ct_worktab-vbeln.
    collect lt_select.
    lt_shp_point-vstel = ct_worktab-vstel.
    collect lt_shp_point.
  endloop.

  loop at lt_shp_point.
    authority-check object 'V_LIKP_VST'
             id 'VSTEL' field lt_shp_point-vstel
             id 'ACTVT' field '01'.
    if sy-subrc ne 0.
      loop at ct_worktab where vstel = lt_shp_point-vstel.
        et_error = ct_worktab.
        et_error-color = color_error.
        append et_error.
      endloop.
      message e080(vl) with lt_shp_point-vstel
                       raising no_permission.
    endif.
  endloop.

  sort lt_select by vstel vbeln.

* Check whether selected deliveries have the correct status
  select vbeln bestk from vbuk
                     into table lt_status
                     for all entries in lt_select
                     where vbeln eq lt_select-vbeln
                     and   bestk eq 'A'.
  sort lt_status by vbeln.
  loop at lt_select.
    lf_select_tabix = sy-tabix.
    read table lt_status with key vbeln = lt_select-vbeln
                         binary search
                         transporting no fields.
*   Not relevant for checking means successful processing
    if sy-subrc ne 0.
      add 1 to ef_success_count.
      loop at ct_worktab where vbeln eq lt_select-vbeln.
        et_success = ct_worktab.
        et_success-color = color_success.
        append et_success.
      endloop.
      delete lt_select index lf_select_tabix.
    endif.
  endloop.

  check not lt_select[] is initial.

  loop at lt_shp_point.
    refresh: lt_package, lt_vbls.
    read table lt_select with key vstel = lt_shp_point-vstel
                         binary search
                         transporting no fields.
    if sy-subrc ne 0.
*     No deliveries left for this shipping point
      continue.
    endif.
*   Build package of deliveries for this shipping point
    loop at lt_select from sy-tabix.
      if lt_select-vstel ne lt_shp_point-vstel.
        exit.
      endif.
      ls_package-vbeln = lt_select-vbeln.
      append ls_package to lt_package.
    endloop.
*   Get current group header for this shipping point
    perform get_current_group_header using    lt_select-vstel
                                              'C'
                                              ct_vbsk
                                     changing ls_vbsk
                                              lf_subrc.
*   Perform conversion u2c only if a valid header could be provided
    if lf_subrc eq 0.
      call function 'SHP_CHECK_UNCHECKED_DELIVERIES'
           exporting
                it_deliveries  = lt_package
           importing
                et_vbls        = lt_vbls
                et_vbfs        = lt_vbfs
           changing
                cs_vbsk        = ls_vbsk
           exceptions
                internal_error = 1
                others         = 2.

      if sy-subrc ne 0.
        message id sy-msgid type 'E' number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
      sort lt_vbls by sammg vbeln_lif posnr_lif.
      if ls_vbsk-vbnum gt 0.
        lf_flag_group_update = charx.
      else.
        lf_flag_group_update = space.
      endif.
      perform update_group_table_from_vbsk changing ls_vbsk
                                                    ct_vbsk.
      if lf_flag_group_update = charx.
        call function 'RV_DELIVERY_CC_ERRORLOG_ADD'
             exporting
                  fvbsk = ls_vbsk
             tables
                  fvbfs = lt_vbfs.
      endif.
      commit work.
    endif.
    loop at lt_package into ls_package.
      read table lt_vbls with key vbeln_lif = ls_package-vbeln
                         binary search
                         transporting no fields.
      if sy-subrc eq 0.
        add 1 to ef_success_count.
        loop at ct_worktab where vbeln eq ls_package-vbeln.
          et_success = ct_worktab.
          et_success-color = color_success.
          append et_success.
        endloop.
      else.
        add 1 to ef_error_count.
        loop at ct_worktab where vbeln eq ls_package-vbeln.
          et_error = ct_worktab.
          et_error-color = color_error.
          append et_error.
        endloop.
      endif.
    endloop.
  endloop.

endfunction.
