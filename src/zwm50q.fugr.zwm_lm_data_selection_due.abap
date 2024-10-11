FUNCTION ZWM_LM_DATA_SELECTION_DUE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IF_PROCTYPE) TYPE  CHAR1
*"     VALUE(IF_PARVW) LIKE  VBPA-PARVW DEFAULT SPACE
*"     VALUE(IF_ITEM_ADD) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_LGNUM_HEAD) TYPE  CHAR1 DEFAULT SPACE
*"  TABLES
*"      IT_VSTEL STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_ROUTE STRUCTURE  RANGE_C6 OPTIONAL
*"      IT_AULWE STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_SPDNR STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_KUNAG STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_KUNWE STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_PARTN STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_VKORG STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_KDGRP STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_MATNR STRUCTURE  RANGE_MAT OPTIONAL
*"      IT_WADAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_WADAT_IST STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_LDDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_TDDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_LFDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_SAMMG STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_TKNUM STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_EXIDV STRUCTURE  RANGE_C20 OPTIONAL
*"      IT_LFART STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_AUDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_AUART STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_VBELN STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_LSTEL STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_LGNUM STRUCTURE  RANGE_C3 OPTIONAL
*"      IT_LGORT STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_LGTOR STRUCTURE  RANGE_C3 OPTIONAL
*"      IT_ABLAD STRUCTURE  RANGE_C25 OPTIONAL
*"      IT_VSBED STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_LPRIO STRUCTURE  RANGE_N2 OPTIONAL
*"      IT_KOSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_WBSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_FKSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_KOQUK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_LVSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_TRSTA STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_PKSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_BESTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_ERNAM STRUCTURE  RANGE_C12 OPTIONAL
*"      IT_ERDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_VTWEG STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_SPART STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_CHARG STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_PRVBE STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_AUFNR STRUCTURE  RANGE_C12 OPTIONAL
*"      IT_LGBZO_H STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_XBLNR STRUCTURE  RANGE_C25 OPTIONAL
*"      IT_TRAGR_H STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_ABELN STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_EBELN STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_VGSYS STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_VLSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_BWLVS STRUCTURE  RANGE_N3 OPTIONAL
*"      IT_WERKS STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_LDUHR STRUCTURE  RANGE_TIMS OPTIONAL
*"      IT_WAUHR STRUCTURE  RANGE_TIMS OPTIONAL
*"      IT_LFUHR STRUCTURE  RANGE_TIMS OPTIONAL
*"      ET_POSTAB STRUCTURE  ZWMOV
*"  EXCEPTIONS
*"      NO_DATA_FOUND
*"      WRONG_PROC_TYPE
*"----------------------------------------------------------------------
*       Global data declarations
  data: lf_sammg_lines type i,
        lf_tknum_lines type i,
        lf_kunag_lines type i,
        lf_kunwe_lines type i,
        lf_partn_lines type i,
        lf_spdnr_lines type i,
        lf_matnr_lines type i,
        lf_lgnum_lines type i,
        lf_lgort_lines type i,
        lf_vtweg_lines type i,
        lf_spart_lines type i,
        lf_charg_lines type i,                              "46A BEY
        lf_exidv_lines type i,                              "HUM/46A
        lf_aufnr_lines type i,                              "HUM
        lf_vgsys_lines type i,                              "AIP
        lf_select_lines type i,
        lf_select_vbak_lines type i,
        lf_select_vbrk_lines type i,
        lf_flag_sammg_sel_excl type c value space,
        lf_flag_tknum_sel_excl type c value space,
        lf_flag_partn_sel_excl type c value space,
        lf_flag_spdnr_sel_excl type c value space,
        lf_control_flag type c value space,
        lf_postab_index like sy-tabix,
        lf_tabix like sy-tabix,
        lf_selection_method type c,
        lf_vbeln_subrc like sy-subrc,
*       lt_likp like likp occurs 0 with header line,
        lt_vbak like vbak occurs 0 with header line,
        lt_vbrk like vbrk occurs 0 with header line,
        lt_vbpa like vbpa occurs 0 with header line,
        lt_vbakpuk type ts_ordstat occurs 0 with header line,
*        lt_vbrkpuk type ts_invstat occurs 0 with header line.
        lt_likpuk type ts_delstat occurs 0 with header line.

  data: lt_select like gs_select occurs 0 with header line,
*        begin of lt_groups occurs 0,
*          vbeln like vbss-vbeln,
*          sammg like vbsk-sammg,
*        end of lt_groups,
        lt_index like sy-tabix.

  data: l_no_vbap_lines,
        l_no_vbrp_lines.

  ranges lt_kunwe for zwmov-kunnr.
  ranges lt_kunag for zwmov-kunag.
  ranges lt_vstel for zwmov-vstel.
  ranges lt_route for zwmov-route.
  ranges lt_lddat for zwmov-lddat.
  ranges lt_tddat for zwmov-tddat.
  ranges lt_lfdat for zwmov-lfdat.
  ranges lt_audat for zwmov-audat.
  ranges lt_wadat for zwmov-wadat.
  ranges lt_auart for vbak-auart.
  ranges lt_fkart for vbrk-fkart.
*  ranges lt_audat for vbak-audat.
  ranges lt_fkdat for vbrk-fkdat.
  ranges lt_vbeln for zwmov-vbeln.
  ranges lt_vbelf for zwmov-vbeln.
  ranges lt_kostk for zwmov-kostk.
  ranges lt_lvstk for zwmov-lvstk.
  ranges lt_wbstk for zwmov-wbstk.
  ranges lt_bestk for zwmov-bestk.                          "AIP
  ranges lt_ernam for zwmov-ernam.
  ranges lt_vkorg for zwmov-vkorg.
  ranges lt_vtweg for zwmov-vtweg.
  ranges lt_spart for zwmov-spart.
  ranges lt_lstel for zwmov-lstel.
* ranges lt_lfart for zwmov-lfart.
  ranges lt_lgnum_head for zwmov-lgnum.
  ranges lt_lgnum_item for zwmov-lgnum.
  ranges lt_likp_handle for likp-handle.                    "AIP

  ranges r_vbtyp for vbuk-vbtyp.

  if not ( if_proctype eq proctype_pick
           or if_proctype eq proctype_conf
           or if_proctype eq proctype_load
           or if_proctype eq proctype_tran
           or if_proctype eq proctype_dist
           or if_proctype eq proctype_unch                  "AIP
           or if_proctype eq proctype_gdsi ).
    raise wrong_proc_type.
  endif.
* Warehouse number check on header or on items?
  if if_lgnum_head ne space.
    lt_lgnum_head[] = it_lgnum[].
    refresh lt_lgnum_item.
  else.
    lt_lgnum_item[] = it_lgnum[].
    refresh lt_lgnum_head.
  endif.

  describe table it_sammg lines lf_sammg_lines.
  describe table it_tknum lines lf_tknum_lines.
  describe table it_kunag lines lf_kunag_lines.
  describe table it_kunwe lines lf_kunwe_lines.
  describe table it_partn lines lf_partn_lines.
  describe table it_spdnr lines lf_spdnr_lines.
  describe table it_matnr lines lf_matnr_lines.
  describe table it_lgort lines lf_lgort_lines.
  describe table it_vtweg lines lf_vtweg_lines.
  describe table it_spart lines lf_spart_lines.
  describe table it_exidv lines lf_exidv_lines.             "HUM/46A
  describe table it_charg lines lf_charg_lines.             "46A BEY
  describe table it_aufnr lines lf_aufnr_lines.             "HUM/50A
  describe table it_vgsys lines lf_vgsys_lines.             "AIP
  describe table lt_lgnum_item lines lf_lgnum_lines.

* Local copies of the ranges table for selection (to be deleted when
* selection according to this criteria took place)
  lt_kunwe[] = it_kunwe[].
  lt_kunag[] = it_kunag[].
  lt_vstel[] = it_vstel[].
  lt_route[] = it_route[].
  lt_lddat[] = it_lddat[].
  lt_tddat[] = it_tddat[].
  lt_lfdat[] = it_lfdat[].
  lt_audat[] = it_audat[].
  lt_wadat[] = it_wadat[].
  lt_audat[] = it_audat[].
*  lt_fkdat[] = it_fkdat[].
  lt_auart[] = it_auart[].
*  lt_fkart[] = it_fkart[].
  lt_vbeln[] = it_vbeln[].
*  lt_vbelf[] = it_vbelf[].
  lt_kostk[] = it_kostk[].
  lt_lvstk[] = it_lvstk[].
  lt_wbstk[] = it_wbstk[].
  lt_ernam[] = it_ernam[].
  lt_vkorg[] = it_vkorg[].
  lt_vtweg[] = it_vtweg[].
  lt_spart[] = it_spart[].
  lt_lstel[] = it_lstel[].
* lt_lfart[] = it_lfart[].

* Check: Only excluding selection criteria for group, transport, partner
*  if lf_sammg_lines ne 0.
*    perform excluding_check tables   it_sammg
*                            changing lf_flag_sammg_sel_excl.
*  endif.
*  if lf_tknum_lines ne 0.
*    perform excluding_check tables   it_tknum
*                            changing lf_flag_tknum_sel_excl.
*  endif.
*  if lf_spdnr_lines ne 0.
*    perform excluding_check tables   it_spdnr
*                            changing lf_flag_spdnr_sel_excl.
*  endif.
*  if lf_partn_lines ne 0.
*    perform excluding_check tables   it_partn
*                            changing lf_flag_partn_sel_excl.
*  endif.

*  refresh: et_postab, lt_likpuk.
*  clear:   et_postab, lt_likpuk.
  refresh: et_postab, lt_vbakpuk.
  clear:   et_postab, lt_vbakpuk.

********************************
  refresh: lt_wbstk.
********************************

* 2. Selection from table VBSS if group number is restricted and
* transportation number is not.
*  if tknum_lines eq 0 and lf_sammg_lines gt 0
*     and lf_flag_sammg_sel_excl eq space.
*    select vbeln from vbss
*                 appending corresponding fields of table et_postab
*                 where sammg in it_sammg
*                 and   vbeln in lt_vbeln.
*    refresh: lt_vbeln.
*  endif.
*
** 3. Selection from table VTTP if transportation number is restricted
** and group number is not
*  if lf_tknum_lines gt 0 and lf_sammg_lines eq 0
*     and lf_flag_tknum_sel_excl eq space.
*    select vbeln from vttp
*                 appending corresponding fields of table et_postab
*                 where tknum in it_tknum
*                 and   vbeln in lt_vbeln.
*    refresh: lt_vbeln.
*  endif.
*
* 4. Selection if both transportation and group number are specified
*  if lf_tknum_lines gt 0 and lf_sammg_lines gt 0
*     and lf_flag_tknum_sel_excl eq space
*     and lf_flag_sammg_sel_excl eq space.
*    select s~vbeln into corresponding fields of table et_postab
*                   from vbss as s inner join vttp as t
*                   on s~vbeln = t~vbeln
*          where s~sammg in it_sammg
*          and   s~vbeln in lt_vbeln
*          and   t~tknum in it_tknum.
*    refresh: lt_vbeln.
*  endif.
*
** Exit if no data found until now
*  describe table et_postab lines lf_select_lines.
*  if lf_select_lines eq 0.
*    raise no_data_found.
*  endif.
*
* Delete possible duplicate entries
*  sort et_postab by vbeln.
*  delete adjacent duplicates from et_postab comparing vbeln.

* Reduce selected deliveries by the group number if excluding criterion
*  if lf_flag_sammg_sel_excl eq charx.
*    perform sammg_reduction tables et_postab
*                                   it_sammg.
*    describe table et_postab lines lf_select_lines.
*    if lf_select_lines eq 0.
*      raise no_data_found.
*    endif.
*  endif.
*
** Reduce selected deliveries by the transportation number (excluding)
*  if lf_flag_tknum_sel_excl eq charx.
*    perform tknum_reduction tables et_postab
*                                   it_tknum.
*    describe table et_postab lines lf_select_lines.
*    if lf_select_lines eq 0.
*      raise no_data_found.
*    endif.
*  endif.
*
** Reduce selected deliveries by means of the partner index if ship-to
** party is restricted and material is not
*  describe table lt_kunwe lines lf_kunwe_lines.             "470
*  if lf_kunwe_lines ne 0 and lf_matnr_lines eq 0.
*    perform kunwe_reduction tables    et_postab
*                                      lt_kunwe
*                                      lt_lfdat
*                                      lt_vkorg
*                                      lt_lfart
*                                      lt_ernam
*                                      lt_vstel.
*    describe table et_postab lines lf_select_lines.
*  endif.
*
** Reduce selected deliveries by means of the partner index if sold-to
** party is restricted and material is not
*  if lf_kunag_lines ne 0 and lf_matnr_lines eq 0.           "50A
*    perform kunag_reduction tables    et_postab
*                                      lt_kunag
*                                      lt_lfdat
*                                      lt_vkorg
*                                      lt_lfart
*                                      lt_ernam
*                                      lt_vstel.
*    describe table et_postab lines lf_select_lines.
*  endif.
*
** Reduce selected deliveries by means of the material index if
** material and ship-to party or only the material or the customer
** is restricted
** Items with appropriate material numbers are added to et_postab
*  if lf_select_lines ne 0 and lf_matnr_lines ne 0.          "50A
*    perform material_reduction tables et_postab
*                                      it_matnr
*                                      lt_lfdat
*                                      lt_vkorg
*                                      lt_lfart
*                                      lt_kunwe
*                                      lt_kunag
*                                      lt_ernam
*                                      lt_vstel
*                               using  lf_matnr_lines.
*    describe table et_postab lines lf_select_lines.
*  endif.
*
** Reduce selected deliveries by means of the forwarding agent
*  if lf_select_lines ne 0 and lf_spdnr_lines ne 0.
*    if lf_flag_spdnr_sel_excl eq space.
*      perform forward_ag_reduction tables et_postab
*                                          it_spdnr
*                                          lt_lfdat
*                                          lt_vkorg
*                                          lt_lfart
*                                          lt_ernam
*                                          lt_vstel.
*    else.
*      perform forward_ag_reduction_excl tables et_postab
*                                               it_spdnr.
*    endif.
*    describe table et_postab lines lf_select_lines.
*  endif.
*
** Reduce selected deliveries by means of the partner
*  if lf_select_lines ne 0 and lf_partn_lines ne 0.
*    if lf_flag_partn_sel_excl eq space.
*      perform partn_reduction tables et_postab
*                                     it_partn
*                                     lt_lfdat
*                                     lt_vkorg
*                                     lt_lfart
*                                     lt_ernam
*                                     lt_vstel
*                               using if_parvw.
*    else.
*      perform partn_reduction_excl tables et_postab
*                                          it_partn
*                                    using if_parvw.
*    endif.
*    describe table et_postab lines lf_select_lines.
*  endif.
*
** Exit if no selected deliveries left
*  if lf_select_lines eq 0.
*    raise no_data_found.
*  endif.
*
** Reduce selected delveries by means of the HU identification
*  if lf_select_lines ne 0 and lf_exidv_lines ne 0.
*    perform exidv_reduction tables et_postab
*                                   it_exidv.
*    describe table et_postab lines lf_select_lines.
*  endif.
*
** Only for unchecked deliveries: Allow selection for preceding document
** number
*  if if_proctype eq proctype_unch.                          "AIP
*    perform vgbel_reduction tables lt_likp_handle
*                                   it_abeln
*                                   it_ebeln.
*  endif.
*
** Check whether selection can be accelerated by two-step selection
  perform determine_selection_method_out tables   lt_wbstk
                                                  lt_lvstk
                                                  lt_kostk
                                                  it_fkstk
                                         changing lf_selection_method.
*
** Transfer the first bunch of document numbers to the ranges table
** Maximum number of documents per select can be controlled by the
** constant GC_SELECT_ROWS in LV50QTOP
*  lf_postab_index = 1.
*  perform prepare_selection_table tables   et_postab
*                                           lt_vbeln
*                                  changing lf_postab_index
*                                           lf_vbeln_subrc.
*
  r_vbtyp-sign   = 'I'.
  r_vbtyp-option = 'EQ'.
  r_vbtyp-low    = 'C'. append r_vbtyp.
*  r_vbtyp-low    = 'M'. append r_vbtyp.
*  r_vbtyp-low    = 'N'. append r_vbtyp.
*  r_vbtyp-low    = 'O'. append r_vbtyp.
*  r_vbtyp-low    = 'P'. append r_vbtyp.
*  r_vbtyp-low    = 'S'. append r_vbtyp.

* VBAK

    case lf_selection_method.
      when gc_selmeth_two_step.

*     First selection step on table VBUK
*        while lf_vbeln_subrc eq 0.
*          if if_proctype ne proctype_conf.
*            select * from vbuk
*                     appending corresponding fields of table lt_vbakpuk
*                     where vbeln in lt_vbeln
*                     and   vbtyp in r_vbtyp
*                     and   koquk in it_koquk
*                     and   kostk in lt_kostk
*                     and   lvstk in lt_lvstk
*                     and   trsta in it_trsta
*                     and   pkstk in it_pkstk
*                     and   fkstk in it_fkstk
*                     and   bestk in lt_bestk                  "AIP
*                     and   wbstk in lt_wbstk.
*          else.
*            select * from vbuk
*                     appending corresponding fields of table lt_vbakpuk
*                     where vbeln in lt_vbeln
*                     and   vbtyp in r_vbtyp
*                     and   kostk in lt_kostk
*                     and   trsta in it_trsta
*                     and   pkstk in it_pkstk
*                     and   fkstk in it_fkstk
*                     and   bestk in lt_bestk                  "AIP
*                     and ( koquk in it_koquk
*                        or lvstk in lt_lvstk )
*                     and   wbstk in lt_wbstk.
*          endif.
*          refresh lt_vbeln.
*          perform prepare_selection_table tables   et_postab
*                                                   lt_vbeln
*                                          changing lf_postab_index
*                                                   lf_vbeln_subrc.
*        endwhile.
*        sort lt_vbakpuk by mandt vbeln.
*
**     Reduce number of valid documents by taking into account selection
**     results on table VBUK
*        loop at et_postab.
*          read table lt_vbakpuk with key vbeln = et_postab-vbeln
*                               binary search.
*          if sy-subrc eq 0.
*            move-corresponding lt_vbakpuk to et_postab.
*            modify et_postab.
*          else.
*            delete et_postab.
*          endif.
*        endloop.

*      if et_postab[] is initial.
*        raise no_data_found.
*      endif.

*     Prepare second selection step for table VBAK and VBRK
        refresh lt_vbakpuk.
        refresh lt_vbeln.
        lf_postab_index = 1.
        perform prepare_selection_table tables   et_postab
                                                 lt_vbeln
                                        changing lf_postab_index
                                                 lf_vbeln_subrc.

        while lf_vbeln_subrc eq 0.
          select * from vbak
                   appending corresponding fields of table lt_vbak
                   where vbeln in lt_vbeln
                   and   vbtyp in r_vbtyp
                   and   audat in lt_audat
                   and   auart in lt_auart
                   and   ernam in lt_ernam
                   and   erdat in it_erdat
                   and   kunnr in lt_kunwe
                   and   vkorg in lt_vkorg
                   and   vtweg in lt_vtweg
                   and   spart in lt_spart
                   and   vsbed in it_vsbed
                   and   handle in lt_likp_handle             "AIP
                   and   xblnr in it_xblnr  .                  "46C

          refresh lt_vbeln.
          perform prepare_selection_table tables   et_postab
                                                   lt_vbeln
                                          changing lf_postab_index
                                                   lf_vbeln_subrc.
        endwhile.

*       Atenção! Este bloco deve ser optimizado >>>>>
        if not it_kdgrp[] is initial.
          loop at lt_vbakpuk.
            lt_index = sy-tabix.
            select single * from  knvv
                   where  kunnr  = lt_vbakpuk-kunnr
                   and    vkorg  = lt_vbakpuk-vkorg
                   and    vtweg  = lt_vbakpuk-vtweg
                   and    spart  = lt_vbakpuk-spart.
            if sy-subrc ne 0.
              delete lt_vbakpuk index lt_index.
            else.
              if not knvv-kdgrp in it_kdgrp.
                delete lt_vbakpuk index lt_index.
              endif.
            endif.
          endloop.
        endif.
*       Atenção! Este bloco deve ser optimizado <<<<<

      when gc_selmeth_inner_join.
        while lf_vbeln_subrc eq 0.
*       Start main selection via inner join of VBAK and VBUK
*       KOQUK and LVSTK are checked as AND conditions when
*       not in confirming mode
          if   if_proctype ne proctype_conf.
*            select * appending corresponding fields of table lt_vbakpuk
*                     from vbak as l inner join vbuk as v
*                     on l~vbeln = v~vbeln
*                     where l~vbeln in lt_vbeln
*                     and   l~vbtyp in r_vbtyp
*                     and   l~ernam in lt_ernam
*                     and   l~erdat in it_erdat
*                     and   l~audat in it_audat
*                     and   l~auart in lt_auart
*                     and   l~kunnr in lt_kunag
*                     and   l~vkorg in lt_vkorg
*                     and   l~vsbed in it_vsbed
**                    and   l~handle in lt_likp_handle         "AIP
*                     and   l~xblnr in it_xblnr                "46C
*                     and   v~koquk in it_koquk
*                     and   v~kostk in lt_kostk
*                     and   v~lvstk in lt_lvstk
*                     and   v~trsta in it_trsta
*                     and   v~pkstk in it_pkstk
*                     and   v~fkstk in it_fkstk
*                     and   v~bestk in lt_bestk                "AIP
*                     and   v~wbstk in lt_wbstk.
            select * from vbak
                     appending corresponding fields of table lt_vbakpuk
                       where vbeln in lt_vbeln
                       and   vbtyp in r_vbtyp
                       and   ernam in lt_ernam
                       and   erdat in it_erdat
                       and   audat in it_audat
                       and   auart in lt_auart
                       and   kunnr in lt_kunag
                       and   vkorg in lt_vkorg
                       and   vtweg in lt_vtweg
                       and   spart in lt_spart
                       and   vsbed in it_vsbed
*                      and   handle in lt_likp_handle         "AIP
                       and   xblnr in it_xblnr.                    "46C

            else.
            select * from vbak
                     appending corresponding fields of table lt_vbakpuk
                       where vbeln in lt_vbeln
                       and   vbtyp in r_vbtyp
                       and   ernam in lt_ernam
                       and   erdat in it_erdat
                       and   audat in it_audat
                       and   auart in lt_auart
                       and   kunnr in lt_kunag
                       and   vkorg in lt_vkorg
                       and   vtweg in lt_vtweg
                       and   spart in lt_spart
                       and   vsbed in it_vsbed
*                      and   handle in lt_likp_handle         "AIP
                       and   xblnr in it_xblnr.                    "46C
            endif.
            refresh lt_vbeln.
*        perform prepare_selection_table tables   et_postab
*                                                 lt_vbeln
*                                        changing lf_postab_index
*                                                 lf_vbeln_subrc.

*       Atenção! Este bloco deve ser optimizado >>>>>
            if not it_kdgrp[] is initial.
              loop at lt_vbakpuk.
                lt_index = sy-tabix.
                select single * from  knvv
                       where  kunnr  = lt_vbakpuk-kunnr
                       and    vkorg  = lt_vbakpuk-vkorg
                       and    vtweg  = lt_vbakpuk-vtweg
                       and    spart  = lt_vbakpuk-spart.
                if sy-subrc ne 0.
                  delete lt_vbakpuk index lt_index.
                else.
                  if not knvv-kdgrp in it_kdgrp.
                    delete lt_vbakpuk index lt_index.
                  endif.
                endif.
              endloop.
            endif.
*       Atenção! Este bloco deve ser optimizado <<<<<

            clear lf_vbeln_subrc.
            loop at lt_vbakpuk.
              lf_tabix = sy-tabix.
              select * from vbpa into table lt_vbpa
                     where  vbeln  = lt_vbakpuk-vbeln.
              clear: vbpa, lt_vbpa.
              loop at lt_vbpa.
                check lt_vbpa-parvw = 'WE'.
                vbpa = lt_vbpa.
                exit.
              endloop.
              check vbpa-kunnr in lt_kunwe.
              lt_vbakpuk-kunnr = vbpa-kunnr.
*            select single * from kna1 where kunnr = vbpa-kunnr.
*            if sy-subrc = 0.
*              lt_likpuk-name1_we = kna1-name1.
*            endif.
              modify lt_vbakpuk index lf_tabix.
              clear et_postab.
              move-corresponding lt_vbakpuk to et_postab.
              et_postab-auart = lt_vbakpuk-auart.
              append et_postab.
            endloop.
            exit.
          endwhile.
      endcase.

      describe table lt_vbakpuk lines lf_select_vbak_lines.

** Fill output table with information from likpuk, delete superfluos
** entries (if rejected in the main selection via likpuk)
*    case lf_selection_method.
*      when gc_selmeth_two_step.
*        describe table lt_vbak lines lf_select_vbak_lines.
*        if lf_select_vbak_lines eq 0.
*          refresh et_postab.
**          raise no_data_found.
*        endif.
*        sort lt_vbak by mandt vbeln.
*        loop at et_postab.
*          read table lt_vbak with key vbeln = et_postab-vbeln
*                                  binary search.
*          if sy-subrc eq 0.
*            perform move_vbak_to_postab using    lt_vbak
*                                        changing et_postab. "n_605460
**         Lines deleted n_573946/n_605460
*            modify et_postab.
*          else.
*            delete et_postab.
*          endif.
*        endloop.
*      when gc_selmeth_inner_join.
*        describe table lt_vbakpuk lines lf_select_vbak_lines.
*        if lf_select_vbak_lines eq 0.
*          refresh et_postab.
**          raise no_data_found.
*        endif.
*        sort lt_vbakpuk by mandt vbeln.
*        loop at et_postab.
*          read table lt_vbakpuk with key vbeln = et_postab-vbeln
*                               binary search.
*          if sy-subrc eq 0.
*            perform move_vbakpuk_to_postab using    lt_vbakpuk
*                                          changing et_postab."n_605460
**         Lines deleted n_573946/n_605460
*            modify et_postab.
*          else.
*            delete et_postab.
*          endif.
*        endloop.
*    endcase.
*

* Só podem ser criados grupos para documentos com condição de expedição
*    delete et_postab where vsbed is initial.

    if lf_select_vbak_lines is initial.
      refresh et_postab.
      raise no_data_found.
    endif.

* Só podem ser criados grupos para documentos com condição de expedição
*    delete et_postab where vsbed is initial.

    sort et_postab by vbeln posnr.

* Read additional item information and check item selection criteria
    lf_control_flag = space.
*  if lf_matnr_lines ne 0.
*    lf_control_flag = 'B'.  "material already checked, items available
*  elseif    lf_lgnum_lines ne 0        "check LIPS selection criteria
*          or lf_vtweg_lines ne 0
*          or lf_spart_lines ne 0
*          or lf_charg_lines ne 0                            "46A BEY
*          or lf_lgort_lines ne 0
*          or lf_aufnr_lines ne 0                            "HUM
*          or lf_vgsys_lines ne 0                            "AIP
*          or not it_bwlvs[] is initial                      "v_n_605460
*          or not it_werks[] is initial                      "^_n_605460
*          or if_item_add eq charx.
    if if_item_add eq charx.           "add all matching items
      lf_control_flag = 'A'.
    else.                              "checking only
*      lf_control_flag = 'C'.
    endif.
*  endif.

    clear: l_no_vbap_lines.

    if lf_control_flag ne space.
        call function 'ZWM_LM_DATA_SELECTION_VBAP'
          exporting
            if_control_flag = lf_control_flag
          tables
            ct_postab       = et_postab
            it_matnr        = it_matnr
            it_charg        = it_charg                      "46A BEY
            it_spart        = it_spart
            it_aufnr        = it_aufnr                    "HUM
            it_werks        = it_werks                      "^_n_605460
          exceptions
            no_items_found  = 1
            others          = 2.
        if sy-subrc ne 0.
          l_no_vbap_lines = 'X'.
        endif.

      if l_no_vbap_lines = 'X'.
        raise no_data_found.
      endif.

      perform delete_header_wo_item tables et_postab.
      describe table et_postab lines lf_select_lines.
      if lf_select_lines eq 0.
        raise no_data_found.
      endif.
    endif.

* Atribui gupos já assignados
*    refresh lt_select.
*    loop at et_postab.
*      lt_select-vbeln = et_postab-vbeln.
*      collect lt_select.
*    endloop.
*
*    select s~vbeln k~sammg
*           into corresponding fields of table lt_groups
*                from vbsk as k inner join vbss as s
*                     on k~sammg = s~sammg
*                       for all entries in lt_select
*                         where s~vbeln eq lt_select-vbeln
*                           and k~smart eq 'M'.
*    if sy-subrc ne 0.
*      exit.
*    endif.
*
*    refresh lt_select.
*    sort lt_groups by vbeln.
*
*    loop at et_postab.
*      lt_index = sy-tabix.
*      read table lt_groups with key vbeln = et_postab-vbeln
*                                    binary search.
*      check sy-subrc = 0.
*      et_postab-sammg = lt_groups-sammg.
*      modify et_postab index lt_index.
*    endloop.
*
*    refresh lt_groups.
*
*    describe table it_sammg lines lt_index.
*    if not lt_index is initial.
*      delete et_postab where not sammg in it_sammg.
*    endif.
*
** Reduce selected deliveries by the group number if excluding criterion
*    if lf_flag_sammg_sel_excl eq charx.
*      perform sammg_reduction tables et_postab
*                                     it_sammg.
*    endif.

    describe table et_postab lines lf_select_lines.
    if lf_select_lines eq 0.
      raise no_data_found.
    endif.
  endfunction.
