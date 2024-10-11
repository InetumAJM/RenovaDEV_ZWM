FUNCTION ZWM_LM_DATA_SELECTION_VBUP.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_KOSTA STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_LVSTA STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_WBSTA STRUCTURE  RANGE_STAT OPTIONAL
*"      CT_POSTAB STRUCTURE  ZWMOV
*"  EXCEPTIONS
*"      NO_ITEMS_FOUND
*"----------------------------------------------------------------------

  data: lt_vbup like vbup occurs 10 with header line,
        begin of lt_select occurs 10,
          vbeln like vbup-vbeln,
          posnr like vbup-posnr,
        end of lt_select,
        lf_flag_items type c.

  field-symbols: <ls_postab> type zwmov.

* Check whether CT_POSTAB contains item data
  perform item_available_check tables   ct_postab
                               changing lf_flag_items.
  if lf_flag_items eq space.
    raise no_items_found.
  endif.

* Extract all items from CT_POSTAB
  loop at ct_postab assigning <ls_postab>
                    where not posnr is initial
                      and selmeth eq space.                 "470
    move-corresponding <ls_postab> to lt_select.
    append lt_select.
  endloop.

  if not lt_select[] is initial.
    select * from vbup
             appending corresponding fields of table lt_vbup
             for all entries in lt_select
             where vbeln eq lt_select-vbeln
             and   posnr eq lt_select-posnr
             and   lvsta in it_lvsta
             and   kosta in it_kosta
             and   wbsta in it_wbsta.
  endif.

  refresh lt_select.
  loop at ct_postab assigning <ls_postab>                   "470
                    where not posnr is initial
                      and selmeth eq 'L'.
    move-corresponding <ls_postab> to lt_select.
    append lt_select.
  endloop.

* The status of items selected with selection method 'L' (from table
* LEIN) are not checked
  if not lt_select[] is initial.
    select * from vbup
             appending corresponding fields of table lt_vbup
             for all entries in lt_select
             where vbeln eq lt_select-vbeln
             and   posnr eq lt_select-posnr.
  endif.

  if lt_vbup[] is initial.
    refresh ct_postab.
    raise no_items_found.
  endif.
  sort lt_vbup by vbeln posnr.
* Transfer VBUP data to CT_POSTAB
  loop at ct_postab where posnr ne posnr_initial.
    read table lt_vbup with key vbeln = ct_postab-vbeln
                                posnr = ct_postab-posnr
                       binary search.
    if sy-subrc eq 0.
      move-corresponding lt_vbup to ct_postab.
      modify ct_postab.
    else.
      delete ct_postab.
    endif.
  endloop.

* Delete header lines without items
  perform delete_header_wo_item tables ct_postab.
  if ct_postab[] is initial.
    raise no_items_found.
  endif.

endfunction.
