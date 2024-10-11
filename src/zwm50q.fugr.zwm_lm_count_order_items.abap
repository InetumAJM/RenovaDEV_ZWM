FUNCTION ZWM_LM_COUNT_ORDER_ITEMS .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IF_ANZPOS) TYPE  MV50L_ANZPOS DEFAULT SPACE
*"  TABLES
*"      CT_POSTAB STRUCTURE  ZWMOV
*"----------------------------------------------------------------------

  data: lf_count      type i,
        lf_flag_items type c,
        lf_tabix      like sy-tabix.

  field-symbols: <ls_postab> type zwmov,
                 <ls_postab_head> type zwmov,
                 <ls_postab_item> type zwmov.

* Check whether CT_POSTAB contains item data
  perform item_available_check tables   ct_postab
                               changing lf_flag_items.

* Determine number of items only in case it is desired by the caller
  if lf_flag_items eq space.
    if if_anzpos ne space.
      loop at ct_postab assigning <ls_postab>.
        select count(*) from vbap
                        into lf_count
                        where vbeln eq <ls_postab>-vbeln.
        <ls_postab>-anzpos = lf_count.
      endloop.
    endif.
  else.
    loop at ct_postab assigning <ls_postab_head>
                      where posnr eq posnr_initial.
      lf_tabix = sy-tabix + 1.
      loop at ct_postab assigning <ls_postab_item>
                        from lf_tabix.
        if <ls_postab_item>-vbeln ne <ls_postab_head>-vbeln.
          exit.
        endif.
        if <ls_postab_item>-posnr eq posnr_initial.
          continue.
        endif.
        add 1 to <ls_postab_head>-anzpos.
      endloop.
    endloop.
  endif.

endfunction.
