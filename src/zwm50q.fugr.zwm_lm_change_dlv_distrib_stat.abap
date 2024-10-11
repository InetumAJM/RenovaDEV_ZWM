FUNCTION ZWM_LM_CHANGE_DLV_DISTRIB_STAT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     VALUE(EF_SUCCESS_COUNT) TYPE  I
*"     VALUE(EF_ERROR_COUNT) TYPE  I
*"  TABLES
*"      CT_WORKTAB STRUCTURE  ZWMOV
*"      CT_VBFS STRUCTURE  VBFS
*"      ET_SUCCESS STRUCTURE  ZWMOV
*"      ET_ERROR STRUCTURE  ZWMOV
*"      ET_TVST STRUCTURE  TVST
*"  EXCEPTIONS
*"      NO_PERMISSION
*"----------------------------------------------------------------------
  data: lt_select like gs_select occurs 0 with header line,
        begin of lt_select2 occurs 0,
          vbeln like likp-vbeln,
          vlstk like likp-vlstk,
        end of lt_select2,
        lt_deliveries type leshp_delivery_key_t,
        ls_deliveries type leshp_delivery_key,
        lt_return like bapiret2 occurs 0 with header line,
        lf_select2_tabix like sy-tabix,
        lf_tvst_lines like sy-tabix.

  field-symbols: <ls_worktab> type zwmov,
                 <ls_select2> like line of lt_select2.

  refresh: et_success, et_error, ct_vbfs.
  clear: ef_success_count, ef_error_count.

* Extract shipping points of the selected deliveries
  loop at ct_worktab assigning <ls_worktab>.
    et_tvst-vstel = <ls_worktab>-vstel.
    collect et_tvst.
  endloop.
* Check change authority for the given shipping points
  loop at et_tvst.
    authority-check object 'V_LIKP_VST'
             id 'VSTEL' field et_tvst-vstel
             id 'ACTVT' field '02'.
    if sy-subrc eq 0.
      delete et_tvst.
    endif.
  endloop.
  describe table et_tvst lines lf_tvst_lines.
  if lf_tvst_lines ne 0.
    raise no_permission.
  endif.

* Extract document numbers of the selected deliveries
  loop at ct_worktab assigning <ls_worktab>.
    lt_select-vbeln = <ls_worktab>-vbeln.
    collect lt_select.
  endloop.

  check not lt_select[] is initial.

* Refresh distribution status
  select vbeln vlstk from likp
               into table lt_select2
               for all entries in lt_select
               where vbeln eq lt_select-vbeln.

* Deliveries that are already distributed are considered as successfully
* processed. Deliveries that are not relevant at all are considered as
* incorrect.
  loop at lt_select2 assigning <ls_select2>.
    lf_select2_tabix = sy-tabix.
    if <ls_select2>-vlstk ca 'BC'.                             "n_496089
      add 1 to ef_success_count.
      loop at ct_worktab assigning <ls_worktab>
                         where vbeln eq <ls_select2>-vbeln.
        et_success = <ls_worktab>.
        et_success-color = color_success.
        append et_success.
      endloop.
      delete lt_select2 index lf_select2_tabix.
    elseif <ls_select2>-vlstk eq space.
      add 1 to ef_error_count.
      loop at ct_worktab assigning <ls_worktab>
                         where vbeln eq <ls_select2>-vbeln.
        et_error = <ls_worktab>.
        et_error-color = color_error.
        append et_error.
      endloop.
      delete lt_select2 index lf_select2_tabix.
    endif.
  endloop.

* Process remaining deliveries
  loop at lt_select2 assigning <ls_select2>.
    ls_deliveries-vbeln = <ls_select2>-vbeln.
    append ls_deliveries to lt_deliveries.
  endloop.

  if not lt_deliveries[] is initial.
    call function 'SHP_CHG_DLV_DISTRB_STATE'
      exporting
        it_deliveries = lt_deliveries
        if_synchron   = charx
      tables
        et_return     = lt_return.
*   Take over error log
    loop at lt_return.
      clear ct_vbfs.
      ct_vbfs-vbeln = lt_return-log_no(10).
      ct_vbfs-posnr = lt_return-log_no+11(6).
      ct_vbfs-msgty = lt_return-type.
      ct_vbfs-msgid = lt_return-id.
      ct_vbfs-msgno = lt_return-number.
      ct_vbfs-msgv1 = lt_return-message_v1.
      ct_vbfs-msgv2 = lt_return-message_v2.
      ct_vbfs-msgv3 = lt_return-message_v3.
      ct_vbfs-msgv4 = lt_return-message_v4.
      append ct_vbfs.
    endloop.

* Check success of processing
    refresh lt_select2.
    select vbeln vlstk from likp
                 into table lt_select2
                 for all entries in lt_deliveries
                 where vbeln eq lt_deliveries-vbeln.

    loop at lt_select2 assigning <ls_select2>.
      if <ls_select2>-vlstk ca 'BC'.
        add 1 to ef_success_count.
        loop at ct_worktab assigning <ls_worktab>
                           where vbeln eq <ls_select2>-vbeln.
          et_success = <ls_worktab>.
          et_success-color = color_success.
          append et_success.
        endloop.
      elseif <ls_select2>-vlstk ca 'AD'.                       "n_496089
        add 1 to ef_error_count.
        loop at ct_worktab assigning <ls_worktab>
                           where vbeln eq <ls_select2>-vbeln.
          et_error = <ls_worktab>.
          et_error-color = color_error.
          append et_error.
        endloop.
      endif.
    endloop.

  endif.

endfunction.
