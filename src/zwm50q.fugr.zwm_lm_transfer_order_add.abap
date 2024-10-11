FUNCTION ZWM_LM_TRANSFER_ORDER_ADD.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      CT_POSTAB STRUCTURE  ZWMOV
*"  EXCEPTIONS
*"      NO_ITEMS
*"----------------------------------------------------------------------
  data: begin of lt_wmto occurs 0,
          vbeln like vbfa-vbeln,
          vbelv like vbfa-vbelv,
          posnv like vbfa-posnv,
        end of lt_wmto,
        begin of lt_compress occurs 0,
          vbeln like zwmov-vbeln,
          lgnum like zwmov-lgnum,
          komau like zwmov-komau,
          komauart like zwmov-komauart,
        end of lt_compress,
        lf_flag_items  type c,
        lf_komau_count type i.

* Coding reworked: Performance optimization 50A
  perform item_available_check tables   ct_postab
                               changing lf_flag_items.
  if lf_flag_items eq space.
    raise no_items.
  endif.

* Select transfer orders from the document flow table
  select vbeln vbelv posnv from vbfa
                     appending corresponding fields of table lt_wmto
                     for all entries in ct_postab
                     where vbelv eq ct_postab-vbeln
                     and   posnv eq ct_postab-posnr
                     and vbtyp_n eq 'Q'"WM transfer order
                     and   taqui eq space
                     and   rfmng gt 0.

  sort lt_wmto by vbelv posnv vbeln descending.

* Add TO number to the document table
  loop at ct_postab where posnr ne posnr_initial.
    clear lf_komau_count.
    read table lt_wmto with key vbelv = ct_postab-vbeln
                                posnv = ct_postab-posnr
                       binary search
                       transporting no fields.
    if sy-subrc eq 0.
      loop at lt_wmto from sy-tabix.
        if lt_wmto-vbelv ne ct_postab-vbeln or
           lt_wmto-posnv ne ct_postab-posnr.
          exit.
        endif.
        add 1 to lf_komau_count.
        ct_postab-komau = lt_wmto-vbeln.
        if ct_postab-lvsta ne space.
          ct_postab-komauart = picktype_wm.
        else.
          ct_postab-komauart = picktype_fix.
        endif.
        if lf_komau_count eq 1.
          modify ct_postab from ct_postab
                           transporting komau komauart.
        else.
          insert ct_postab. "more than one transfer order per item
        endif.
        lt_compress-vbeln = ct_postab-vbeln.
        lt_compress-lgnum = ct_postab-lgnum.
        lt_compress-komau = ct_postab-komau.
        lt_compress-komauart = ct_postab-komauart.
        collect lt_compress.
      endloop.
    else.
      delete ct_postab.
    endif.
  endloop.

  perform delete_header_wo_item tables ct_postab.

  sort lt_compress by vbeln komau descending.

* Add compressed information to header
  loop at ct_postab where posnr eq posnr_initial.
    clear lf_komau_count.
    read table lt_compress with key vbeln = ct_postab-vbeln
                           binary search
                           transporting no fields.
    if sy-subrc eq 0.
      loop at lt_compress from sy-tabix.
        if lt_compress-vbeln ne ct_postab-vbeln.
          exit.
        endif.
        add 1 to lf_komau_count.
        ct_postab-lgnum = lt_compress-lgnum.
        ct_postab-komau = lt_compress-komau.
        ct_postab-komauart = lt_compress-komauart.
        if lf_komau_count eq 1.
          modify ct_postab from ct_postab
                           transporting lgnum komau komauart.
        else.
          insert ct_postab.
        endif.
      endloop.
    endif.
  endloop.

endfunction.
