function zwm_order_monitor.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IF_C_PROCTYPE) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_C_PARVW) LIKE  VBPA-PARVW DEFAULT SPACE
*"     VALUE(IF_C_ITEM_ADD) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_C_ANZPOS) TYPE  MV50L_ANZPOS DEFAULT SPACE
*"     VALUE(IF_C_SPDNR_ADD) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_C_FLAG_WM) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_C_FLAG_FIX) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_C_REFNUM_ADD) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_C_LGNUM_HEAD) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_C_REF_GEWEI) LIKE  LIPOV-GEWEI DEFAULT SPACE
*"     VALUE(IF_C_REF_VOLEH) LIKE  LIPOV-VOLEH DEFAULT SPACE
*"     VALUE(IF_C_ALAKT) LIKE  RL03T-ALAKT DEFAULT 'X'
*"     VALUE(IF_C_KOMIM) LIKE  RL03T-KOMIM DEFAULT SPACE
*"     VALUE(IF_C_EINLM) LIKE  RL03T-EINLM DEFAULT SPACE
*"     VALUE(IF_C_UNCHA) LIKE  MV50L-UNCHA DEFAULT SPACE
*"  TABLES
*"      IT_C_VSTEL STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_C_ROUTE STRUCTURE  RANGE_C6 OPTIONAL
*"      IT_C_AULWE STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_SPDNR STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_KUNAG STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_KUNWE STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_PARTN STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_VKORG STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_C_KDGRP STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_C_MATNR STRUCTURE  RANGE_MAT OPTIONAL
*"      IT_C_WADAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_C_WADAT_IST STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_C_LDDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_C_TDDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_C_LFDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_C_audat STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_C_SAMMG STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_TKNUM STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_LFART STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_C_AUDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_C_AUART STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_C_VBELN STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_LSTEL STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_C_LGNUM STRUCTURE  RANGE_C3 OPTIONAL
*"      IT_C_LGORT STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_C_LGTOR STRUCTURE  RANGE_C3 OPTIONAL
*"      IT_C_ABLAD STRUCTURE  RANGE_C25 OPTIONAL
*"      IT_C_VSBED STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_C_LPRIO STRUCTURE  RANGE_N2 OPTIONAL
*"      IT_C_KOSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_C_WBSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_C_FKSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_C_KOQUK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_C_LVSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_C_TRSTA STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_C_PKSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_C_BESTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_C_ERNAM STRUCTURE  RANGE_C12 OPTIONAL
*"      IT_C_ERDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_C_VTWEG STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_C_SPART STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_C_EBELN STRUCTURE  RANGE_EBELN OPTIONAL
*"      IT_C_EBELP STRUCTURE  RANGE_C5 OPTIONAL
*"      IT_C_TRAID STRUCTURE  RANGE_C20 OPTIONAL
*"      IT_C_EXIDV STRUCTURE  RANGE_C20 OPTIONAL
*"      IT_C_LIFEX STRUCTURE  RANGE_C35 OPTIONAL
*"      IT_C_LIFNR STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_KDMAT STRUCTURE  RANGE_C35 OPTIONAL
*"      IT_C_EAN11 STRUCTURE  RANGE_C18 OPTIONAL
*"      IT_C_CHARG STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_PRVBE STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_AUFNR STRUCTURE  RANGE_C12 OPTIONAL
*"      IT_C_LGBZO_H STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_XBLNR STRUCTURE  RANGE_C25 OPTIONAL
*"      IT_C_TRAGR_H STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_C_ABELN STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_VGSYS STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_C_VLSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_C_E1_OUTP_SEL STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_C_V2_OUTP_SEL STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_C_V4_OUTP_SEL STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_C_V5_OUTP_SEL STRUCTURE  RANGE_C4 OPTIONAL
*"  EXCEPTIONS
*"      NO_DATA_FOUND
*"      WRONG_PROC_TYPE
*"----------------------------------------------------------------------

*       Global data declarations
  data: lt_postab like zwmov occurs 10 with header line,
        lt_postab_hierq like zwmov_hierq occurs 10 with header line,
        lt_output_v2 like lipov_output occurs 0 with header line,
        lt_output_v4 like lipov_output occurs 0 with header line,
        lt_output_v5 like lipov_output occurs 0 with header line,
        lt_output_e1 like lipov_output occurs 0 with header line,
        lf_wadat_ist like likp-wadat_ist,
        lf_cattactive type c,
        lf_cattneu type c,
        lf_flag_inbound type c,
        lf_debug_flag type c.

*  data: lt_select like gs_select occurs 0 with header line,
*        begin of lt_groups occurs 0,
*          vbeln like vbss-vbeln,
*          sammg like vbsk-sammg,
*        end of lt_groups,
*        lt_index like sy-tabix.

*  ranges: lt_wadat_ist for likp-wadat_ist.                  "45B/46A

* Import CATT flags from memory
  import cattaktiv to lf_cattactive from memory id 'CATT'.  "50A
  import cattneu to lf_cattneu from memory id 'CATTNEU'.

* If debug flag is set stop here
  get parameter id 'LE_SHP_DEL_MON_DEBUG' field lf_debug_flag.
  if lf_debug_flag eq charx.
    break-point.                                           "#EC NOBREAK
  endif.

  if   if_c_proctype eq proctype_pick
    or if_c_proctype eq proctype_conf
    or if_c_proctype eq proctype_load
    or if_c_proctype eq proctype_tran
    or if_c_proctype eq proctype_gdsi
    or if_c_proctype eq proctype_unch                       "AIP
    or if_c_proctype eq proctype_dist
    or if_c_proctype eq proctype_free.
    lf_flag_inbound = space.
  else.
    lf_flag_inbound = charx.
  endif.

** If in background mode for goods issue/receipt then selection for
** WADAT_IST does not make sense.
*  if     ( sy-batch ne space and sy-binpt eq space )        "45B/46A
*     and ( if_c_proctype eq proctype_gdsi
*       or  if_c_proctype eq proctype_inbgdrc ).
*    refresh lt_wadat_ist.
*  else.
*    lt_wadat_ist[] = it_c_wadat_ist[].
*  endif.

  call function 'ZWM_LM_DATA_SELECTION_OUTPUT'
    exporting
      if_flag_inbound = lf_flag_inbound
    tables
      it_v2_outp_sel  = it_c_v2_outp_sel
      it_v4_outp_sel  = it_c_v4_outp_sel
      it_v5_outp_sel  = it_c_v5_outp_sel
      it_e1_outp_sel  = it_c_e1_outp_sel
      et_output_v2    = lt_output_v2
      et_output_v4    = lt_output_v4
      et_output_v5    = lt_output_v5
      et_output_e1    = lt_output_e1.

  call function 'ZWM_LM_DATA_SELECTION'
    exporting
      if_proctype     = if_c_proctype
      if_flag_inbound = lf_flag_inbound
      if_parvw        = if_c_parvw
      if_item_add     = if_c_item_add
      if_anzpos       = if_c_anzpos
      if_spdnr_add    = if_c_spdnr_add
      if_flag_wm      = if_c_flag_wm
      if_flag_fix     = if_c_flag_fix
      if_refnum_add   = if_c_refnum_add
      if_lgnum_head   = if_c_lgnum_head
      if_ref_gewei    = if_c_ref_gewei
      if_ref_voleh    = if_c_ref_voleh
      if_uncha        = if_c_uncha                    "AIP
*      if_order        = if_c_order
*      if_billd        = if_c_billd
    tables
      it_kunag        = it_c_kunag
      it_kunwe        = it_c_kunwe
      it_vkorg        = it_c_vkorg
      it_kdgrp        = it_c_kdgrp
      it_matnr        = it_c_matnr
      it_sammg        = it_c_sammg
      it_audat        = it_c_audat
*      it_fkdat        = it_c_fkdat
      it_auart        = it_c_auart
*      it_fkart        = it_c_fkart
      it_vbeln        = it_c_vbeln
*      it_vbelf        = it_c_vbelf
      it_vsbed        = it_c_vsbed
      it_kostk        = it_c_kostk
      it_wbstk        = it_c_wbstk
      it_fkstk        = it_c_fkstk
      it_koquk        = it_c_koquk
      it_lvstk        = it_c_lvstk
      it_trsta        = it_c_trsta
      it_pkstk        = it_c_pkstk
      it_ernam        = it_c_ernam
      it_erdat        = it_c_erdat
      it_vtweg        = it_c_vtweg
      it_spart        = it_c_spart
      it_ebeln        = it_c_ebeln
      it_ebelp        = it_c_ebelp
      it_traid        = it_c_traid
      it_exidv        = it_c_exidv
      it_lifex        = it_c_lifex
      it_lifnr        = it_c_lifnr
      it_kdmat        = it_c_kdmat
      it_ean11        = it_c_ean11
      it_charg        = it_c_charg                    "46A
      it_prvbe        = it_c_prvbe                    "HUM
      it_aufnr        = it_c_aufnr                    "HUM
      it_lgbzo_h      = it_c_lgbzo_h                  "46C
      it_xblnr        = it_c_xblnr                    "46C
      it_tragr_h      = it_c_tragr_h                  "46C
      it_abeln        = it_c_abeln                    "AIP
      it_vgsys        = it_c_vgsys                    "AIP
      it_vlstk        = it_c_vlstk
      ct_postab       = lt_postab
      ct_postab_hierq = lt_postab_hierq
    exceptions
      no_data_found   = 1
      wrong_proc_type = 2
      others          = 3.
* Write a message to the spool if no data found in background processing
  if     sy-subrc ne 0
     and sy-batch ne space
     and sy-binpt eq space.
    write: / 'Keine Lieferungen selektiert.'(062).
  endif.
  case sy-subrc.
    when 0.
    when 1.
      raise no_data_found.
    when 2.
      raise wrong_proc_type.
    when others.
      raise no_data_found.
  endcase.


** Atribui gupos já assignados
*  loop at lt_postab.
*    lt_select-vbeln = lt_postab-vbeln.
*    collect lt_select.
*  endloop.
*
*  select s~vbeln k~sammg
*         into corresponding fields of table lt_groups
*              from vbsk as k inner join vbss as s
*                   on k~sammg = s~sammg
*                     for all entries in lt_select
*                       where s~vbeln eq lt_select-vbeln
*                         and k~smart eq 'M'.
*  if sy-subrc ne 0.
*    exit.
*  endif.
*
*  refresh lt_select.
*  sort lt_groups by vbeln.
*
*  loop at lt_postab.
*    lt_index = sy-tabix.
*    read table lt_groups with key vbeln = lt_postab-vbeln
*                                  binary search.
*    check sy-subrc = 0.
*    lt_postab-sammg = lt_groups-sammg.
*    modify lt_postab index lt_index.
*  endloop.
*
*  refresh lt_groups.

  if     ( sy-batch ne space and sy-binpt eq space          "50A
           and lf_cattactive eq space
           and lf_cattneu eq space )
     and ( if_c_proctype eq proctype_gdsi
       or  if_c_proctype eq proctype_inbgdrc
       or  if_c_proctype eq proctype_pick
       or  if_c_proctype eq proctype_inbpick
       or  if_c_proctype eq proctype_conf
       or  if_c_proctype eq proctype_unch                   "AIP
       or  if_c_proctype eq proctype_inbconf
       or  if_c_proctype eq proctype_dist
       or  if_c_proctype eq proctype_inbdist
       or  if_c_proctype eq proctype_free ).

*    lf_wadat_ist = it_c_wadat_ist-low.

    call function 'ZWM_LM_BACKGROUND_PROCESSING'
      exporting
        if_proctype    = if_c_proctype
        if_komim       = if_c_komim
        if_einlm       = if_c_einlm
        if_alakt       = if_c_alakt
        if_wadat_ist   = lf_wadat_ist
      tables
        it_worktab     = lt_postab
        it_output_v2   = lt_output_v2
        it_output_v4   = lt_output_v4
      exceptions
        wrong_proctype = 0
        no_permission  = 0
        others         = 0.

  else.
*   !!!!!!!!!!!!!!!!!!!
    clear if_c_item_add.

    call function 'ZWM_LM_LIST_DISPLAY'
      exporting
        if_d_proctype     = if_c_proctype
        if_d_inbound      = lf_flag_inbound
        if_d_item_add     = if_c_item_add
        if_d_anzpos       = if_c_anzpos
        if_d_parvw        = if_c_parvw
        if_d_spdnr_add    = if_c_spdnr_add
        if_d_flag_wm      = if_c_flag_wm
        if_d_flag_fix     = if_c_flag_fix
        if_d_refnum_add   = if_c_refnum_add
        if_d_lgnum_head   = if_c_lgnum_head
        if_d_ref_gewei    = if_c_ref_gewei
        if_d_ref_voleh    = if_c_ref_voleh
        if_d_uncha        = if_c_uncha                "AIP
      tables
        it_d_postab       = lt_postab
        it_d_postab_hierq = lt_postab_hierq
        it_d_output_e1    = lt_output_e1
        it_d_output_v2    = lt_output_v2
        it_d_output_v4    = lt_output_v4
        it_d_output_v5    = lt_output_v5
        it_d_kunag        = it_c_kunag
        it_d_kunwe        = it_c_kunwe
        it_d_vkorg        = it_c_vkorg
        it_d_kdgrp        = it_c_kdgrp
        it_d_matnr        = it_c_matnr
        it_d_sammg        = it_c_sammg
        it_d_audat        = it_c_audat
*        it_d_fkdat        = it_c_fkdat
        it_d_auart        = it_c_auart
*        it_d_fkart        = it_c_fkart
        it_d_vbeln        = it_c_vbeln
*        it_d_vbelf        = it_c_vbelf
        it_d_vsbed        = it_c_vsbed
        it_d_kostk        = it_c_kostk
        it_d_wbstk        = it_c_wbstk
        it_d_fkstk        = it_c_fkstk
        it_d_koquk        = it_c_koquk
        it_d_lvstk        = it_c_lvstk
        it_d_trsta        = it_c_trsta
        it_d_pkstk        = it_c_pkstk
        it_d_ebeln        = it_c_ebeln
        it_d_ebelp        = it_c_ebelp
        it_d_ernam        = it_c_ernam
        it_d_erdat        = it_c_erdat
        it_d_vtweg        = it_c_vtweg
        it_d_spart        = it_c_spart
        it_d_traid        = it_c_traid
        it_d_lifex        = it_c_lifex
        it_d_lifnr        = it_c_lifnr
        it_d_kdmat        = it_c_kdmat
        it_d_vlstk        = it_c_vlstk
        it_d_exidv        = it_c_exidv                "45B
        it_d_charg        = it_c_charg                "46A
        it_d_prvbe        = it_c_prvbe                "HUM
        it_d_aufnr        = it_c_aufnr                "HUM
        it_d_lgbzo_h      = it_c_lgbzo_h
        it_d_xblnr        = it_c_xblnr                "46C
        it_d_tragr_h      = it_c_tragr_h              "46C
        it_d_abeln        = it_c_abeln                "AIP
        it_d_vgsys        = it_c_vgsys                "AIP
        it_d_ean11        = it_c_ean11.

  endif.

endfunction.
