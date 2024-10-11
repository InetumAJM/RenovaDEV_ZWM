FUNCTION ZWM_LM_DAILY_WORK_LIST.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      CT_POSTAB STRUCTURE  ZWMOV
*"      CT_POSTAB_HIERQ STRUCTURE  ZWMOV_HIERQ
*"----------------------------------------------------------------------

  data: begin of lt_select occurs 10,
          vbeln like zwmov-vbeln,
          posnr like zwmov-posnr,
        end of lt_select,
        lt_vbfa         like vbfa   occurs 0 with header line,
        lt_vbfa_work    like vbfavb occurs 0 with header line,
        lt_vbapf        like vbapf occurs 10 with header line,
        lf_sumtab_subrc like sy-subrc,
        lf_sumtab_tabix like sy-tabix,
        lf_expand_save  like zwmov_hierq-expand,
        lf_audat_save   like zwmov_hierq-audat,
        lf_vstel_save   like zwmov_hierq-vstel,
        lf_faktor       like lips-lfimg.

  field-symbols: <ls_postab_hierq> type zwmov_hierq,
                 <ls_postab>       type zwmov.

* Save expansion status of hierarchical list if it is to be refreshed
  loop at ct_postab_hierq assigning <ls_postab_hierq>.
    lf_expand_save = <ls_postab_hierq>-expand.
    lf_audat_save  = <ls_postab_hierq>-audat.
    lf_vstel_save  = <ls_postab_hierq>-vstel.
    clear <ls_postab_hierq>.
    <ls_postab_hierq>-expand = lf_expand_save.
    <ls_postab_hierq>-audat  = lf_audat_save.
    <ls_postab_hierq>-vstel  = lf_vstel_save.
  endloop.

* Prepare mass selection from table VBFA
  loop at ct_postab assigning <ls_postab>
                    where posnr ne posnr_initial
*                    and   kosta eq 'B'
                    and   not audat is initial.
    lt_select-vbeln = <ls_postab>-vbeln.
    lt_select-posnr = <ls_postab>-posnr.
    append lt_select.
  endloop.
  if not lt_select[] is initial.
    select * from vbfa
             appending table lt_vbfa
             for all entries in lt_select
             where vbelv eq lt_select-vbeln
             and   posnv eq lt_select-posnr.
  endif.

  loop at ct_postab where posnr ne posnr_initial.
    clear ct_postab_hierq.
    read table ct_postab_hierq with key vstel = ct_postab-vstel
                                        audat = ct_postab-audat.
    lf_sumtab_subrc = sy-subrc.
    lf_sumtab_tabix = sy-tabix.
*    case ct_postab-kosta.
*      when space.
*        if ct_postab-lvsta eq 'C'.
**         Special logic for inbound deliveries already posted GR
*          ct_postab-pikmg = ct_postab-lfimg.
*          perform quantity_add using    ct_postab-vbeaf
*                                        ct_postab-vbeav
*                                        gc_dec6_high
*                               changing ct_postab-vbeak.
*          perform audat_total using    ct_postab
*                              changing ct_postab_hierq.
*          add 1 to ct_postab_hierq-anzpos.
*        else.
*          delete ct_postab.
*          continue.
*        endif.
*      when 'A'.                        "no picking done until now
*        ct_postab-pikmg = ct_postab-lfimg.
*        perform quantity_add using    ct_postab-vbeaf
*                                      ct_postab-vbeav
*                                      gc_dec6_high
*                             changing ct_postab-vbeak.
*        perform audat_total using    ct_postab
*                            changing ct_postab_hierq.
*        add 1 to ct_postab_hierq-anzpos.
*      when 'B'.                        "partially picked
*        refresh: lt_vbapf, lt_vbfa_work.
*        clear lt_vbapf.
**         Calculate quantity already picked
*        loop at lt_vbfa where vbelv eq ct_postab-vbeln
*                        and   posnv eq ct_postab-posnr.
*          append lt_vbfa to lt_vbfa_work.
*        endloop.
*        sort lt_vbfa_work by mandt vbelv posnv vbeln posnn vbtyp_n.
*        call function 'RV_XVBAPF_CREATE'
*          exporting
*            vbeln   = ct_postab-vbeln
*          tables
*            fxvbapf = lt_vbapf
*            fxvbfa  = lt_vbfa_work.
*        read table lt_vbapf with key vbeln = ct_postab-vbeln
*                                     posnr = ct_postab-posnr.
*        if ct_postab-kzfme ca gc_kzfme_ws.
*          ct_postab-pikmg = lt_vbapf-qmengev.
*        else.
*          ct_postab-pikmg = lt_vbapf-qmenge * ct_postab-umvkn
*                                            / ct_postab-umvkz.
*        endif.
**         Modify weights and volume of the quantity to be picked
*        ct_postab-pikmg = ct_postab-lfimg - ct_postab-pikmg.
*        lf_faktor = ct_postab-pikmg / ct_postab-lfimg.
*        multiply ct_postab-brgew by lf_faktor.
*        multiply ct_postab-ntgew by lf_faktor.
*        multiply ct_postab-volum by lf_faktor.
*        multiply ct_postab-vbeav by lf_faktor.
*        perform quantity_add using    ct_postab-vbeaf
*                                      ct_postab-vbeav
*                                      gc_dec6_high
*                             changing ct_postab-vbeak.
        perform audat_total using    ct_postab
                            changing ct_postab_hierq.
        add 1 to ct_postab_hierq-anzpos.
*      when 'C'.
*        delete ct_postab.
*        continue.
*    endcase.
    if lf_sumtab_subrc eq 0.           "Entry exists already
      modify ct_postab_hierq index lf_sumtab_tabix.
    else. "New picking date or shipping point
      ct_postab_hierq-vstel = ct_postab-vstel.
      ct_postab_hierq-audat = ct_postab-audat.
      ct_postab_hierq-gewei = ct_postab-gewei.
      ct_postab_hierq-voleh = ct_postab-voleh.
      append ct_postab_hierq.
    endif.
    modify ct_postab.
  endloop.
  sort ct_postab_hierq by vstel audat.

  perform delete_header_wo_item tables ct_postab.

* Delete entries of ct_postab_hierq which are no longer necessary
* after refreshing the list
  loop at ct_postab_hierq.
    read table ct_postab with key vstel = ct_postab_hierq-vstel
                                  audat = ct_postab_hierq-audat.
    if sy-subrc ne 0.
      delete ct_postab_hierq.
    endif.
  endloop.

endfunction.
