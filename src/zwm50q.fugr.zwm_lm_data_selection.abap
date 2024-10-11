FUNCTION ZWM_LM_DATA_SELECTION.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IF_PROCTYPE) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_FLAG_INBOUND) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_PARVW) LIKE  VBPA-PARVW DEFAULT SPACE
*"     VALUE(IF_ITEM_ADD) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_ANZPOS) TYPE  MV50L_ANZPOS DEFAULT SPACE
*"     VALUE(IF_SPDNR_ADD) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_FLAG_WM) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_FLAG_FIX) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_REFNUM_ADD) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_LGNUM_HEAD) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_REF_GEWEI) LIKE  LIPOV-GEWEI DEFAULT SPACE
*"     VALUE(IF_REF_VOLEH) LIKE  LIPOV-VOLEH DEFAULT SPACE
*"     VALUE(IF_UNCHA) LIKE  MV50L-UNCHA DEFAULT SPACE
*"     VALUE(IF_NO_ADDITIONAL_DATA) TYPE  XFELD DEFAULT SPACE
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
*"      IT_audat STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_SAMMG STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_TKNUM STRUCTURE  RANGE_C10 OPTIONAL
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
*"      IT_EBELN STRUCTURE  RANGE_EBELN OPTIONAL
*"      IT_EBELP STRUCTURE  RANGE_C5 OPTIONAL
*"      IT_TRAID STRUCTURE  RANGE_C20 OPTIONAL
*"      IT_EXIDV STRUCTURE  RANGE_C20 OPTIONAL
*"      IT_LIFEX STRUCTURE  RANGE_C35 OPTIONAL
*"      IT_LIFNR STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_KDMAT STRUCTURE  RANGE_C35 OPTIONAL
*"      IT_EAN11 STRUCTURE  RANGE_C18 OPTIONAL
*"      IT_CHARG STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_PRVBE STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_AUFNR STRUCTURE  RANGE_C12 OPTIONAL
*"      IT_LGBZO_H STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_XBLNR STRUCTURE  RANGE_C25 OPTIONAL
*"      IT_TRAGR_H STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_ABELN STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_VGSYS STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_VLSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_BWLVS STRUCTURE  RANGE_N3 OPTIONAL
*"      IT_WERKS STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_IMWRK STRUCTURE  RANGE_C1 OPTIONAL
*"      IT_LDUHR STRUCTURE  RANGE_TIMS OPTIONAL
*"      IT_WAUHR STRUCTURE  RANGE_TIMS OPTIONAL
*"      IT_LFUHR STRUCTURE  RANGE_TIMS OPTIONAL
*"      CT_POSTAB STRUCTURE  ZWMOV
*"      CT_POSTAB_HIERQ STRUCTURE  ZWMOV_HIERQ OPTIONAL
*"  EXCEPTIONS
*"      NO_DATA_FOUND
*"      WRONG_PROC_TYPE
*"----------------------------------------------------------------------

*       Global data declarations
  data: lf_item_add type c,
        lf_flag_seq_grpa type c,
        lt_tvlr like tvlr occurs 0 with header line,
        lt_postab like zwmov occurs 0 with header line,     "AIP
        lf_first_tabix like sy-tabix,
        lf_last_tabix  like sy-tabix,
        lf_selection_subrc like sy-subrc,
        lf_debug_flag type c.

  ranges: lt_kosta for vbup-kosta,
          lt_lvsta for vbup-lvsta,
          lt_wbsta for vbup-wbsta,
          lt_vbeln for vbak-vbeln,
          lt_vbeln_part for likp-vbeln,
          lt_vstel_checked for likp-vstel,
          lt_kostk for vbuk-kostk,                          "n_556119
          lt_imwrk for likp-imwrk,                          "n_605460
          lt_bestk for vbuk-bestk.                          "AIP

* If debug flag is set stop here
  get parameter id 'LE_SHP_DEL_MON_DEBUG' field lf_debug_flag.
  if lf_debug_flag eq charx.
    break-point.                                           "#EC NOBREAK
  endif.

* Inbound deliveries: Check whether sequence of goods receipt and
* putaway can be changed
  if if_flag_inbound eq charx.
    perform check_for_gr_pa_sequence tables   it_lgnum
                                              lt_tvlr
                                     changing lf_flag_seq_grpa.
  endif.

* Adjust selection criteria for shipping point/receiving point according
* to the authorization of the user
  perform vstel_authorization_check tables it_vstel         "50A
                                           lt_vstel_checked
                                    using  if_flag_inbound.

* Since IMWRK is a new field with no initialization, selection criteria
* have to be transformed in order to ensure a correct selection result
  perform transform_imwrk_criteria tables it_imwrk
                                          lt_imwrk.         "n_605460

* Show progress when called by delivery monitor
  if sy-tcode(4) eq 'VL06'.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        percentage = 0
        text       = 'Lieferungsdaten werden gelesen...'(064).
  endif.

* For some processing types item data are required in every case
  if (    if_proctype eq proctype_pick
       or if_proctype eq proctype_inbpick
       or if_proctype eq proctype_conf
       or if_proctype eq proctype_inbconf
       or if_proctype eq proctype_inbgdrc ) and
     if_no_additional_data eq space.                        "n_605460
    lf_item_add = charx.
  else.
    lf_item_add = if_item_add.
  endif.

* Call function for main data selection
  case if_proctype.
*   when proctype_free.
    when proctype_pick or proctype_conf or
         proctype_load or proctype_gdsi or
         proctype_tran or proctype_unch or
         proctype_dist.
      call function 'ZWM_LM_DATA_SELECTION_DUE'
        exporting
          if_proctype     = if_proctype
          if_parvw        = if_parvw
          if_item_add     = lf_item_add
          if_lgnum_head   = if_lgnum_head
*          if_order        = if_order
*          if_billd        = if_billd
        tables
          it_vstel        = lt_vstel_checked          "50A
          it_route        = it_route
          it_aulwe        = it_aulwe
          it_spdnr        = it_spdnr
          it_kunag        = it_kunag
          it_kunwe        = it_kunwe
          it_partn        = it_partn
          it_vkorg        = it_vkorg
          it_kdgrp        = it_kdgrp
          it_matnr        = it_matnr
          it_wadat        = it_wadat
          it_wadat_ist    = it_wadat_ist
          it_lddat        = it_lddat
          it_tddat        = it_tddat
          it_lfdat        = it_lfdat
          it_audat        = it_audat
*          it_audat        = it_audat
*          it_fkdat        = it_fkdat
          it_auart        = it_auart
*          it_fkart        = it_fkart
          it_sammg        = it_sammg
          it_tknum        = it_tknum
          it_exidv        = it_exidv                        "HUM/99A
          it_lfart        = it_lfart
          it_vbeln        = it_vbeln
*          it_vbelf        = it_vbelf
          it_lstel        = it_lstel
          it_lgnum        = it_lgnum
          it_lgort        = it_lgort
          it_lgtor        = it_lgtor
          it_ablad        = it_ablad
          it_vsbed        = it_vsbed
          it_lprio        = it_lprio
          it_kostk        = it_kostk
          it_wbstk        = it_wbstk
          it_fkstk        = it_fkstk
          it_koquk        = it_koquk
          it_lvstk        = it_lvstk
          it_trsta        = it_trsta
          it_pkstk        = it_pkstk
          it_bestk        = it_bestk                  "AIP
          it_ernam        = it_ernam
          it_erdat        = it_erdat
          it_vtweg        = it_vtweg
          it_spart        = it_spart
          it_charg        = it_charg                  "46A
          it_prvbe        = it_prvbe                  "HUM
          it_aufnr        = it_aufnr                  "HUM
          it_lgbzo_h      = it_lgbzo_h                "46C
          it_xblnr        = it_xblnr                  "46C
          it_tragr_h      = it_tragr_h                "46C
          it_abeln        = it_abeln                  "AIP
          it_ebeln        = it_ebeln                  "AIP
          it_vgsys        = it_vgsys                  "AIP
          it_vlstk        = it_vlstk
          it_bwlvs        = it_bwlvs                        "v_n_605460
          it_lduhr        = it_lduhr
          it_wauhr        = it_wauhr
          it_lfuhr        = it_lfuhr
          it_werks        = it_werks                        "^_n_605460
          et_postab       = ct_postab
        exceptions
          no_data_found   = 1
          wrong_proc_type = 2
          others          = 3.
      lf_selection_subrc = sy-subrc.
*    when proctype_inbpick or proctype_inbgdrc or
*         proctype_inbconf or proctype_inbtran or
*         proctype_inbdist.
    when others.
      refresh ct_postab_hierq.
      raise wrong_proc_type.
  endcase.
  case lf_selection_subrc.
    when 0.
    when 1.
      refresh ct_postab_hierq.
      raise no_data_found.
    when 2.
      refresh ct_postab_hierq.
      raise wrong_proc_type.
    when others.
      refresh ct_postab_hierq.
      raise no_data_found.
  endcase.

* Show progress when called by delivery monitor
  if sy-tcode(4) eq 'VL06'.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        percentage = 0
        text       = 'Zusatzdaten werden ermittelt...'(065).
  endif.

  if if_spdnr_add ne space.
    call function 'ZWM_LM_FORWARDING_AGENT_ADD'
      tables
        ct_postab = ct_postab.
  endif.

  if if_refnum_add ne space.
    call function 'ZWM_LM_DESELECT_REFNUM'
      tables
        it_lgnum  = it_lgnum
        ct_postab = ct_postab.
  endif.

  if if_no_additional_data eq space.                        "n_605460
    call function 'ZWM_LM_ADDRESS_READ'
      exporting
        if_parvw  = if_parvw
      tables
        ct_postab = ct_postab.
  endif.                                                    "n_605460

*  if lf_item_add ne space and
*    if_no_additional_data eq space.                         "n_605460
*    call function 'ZWM_LM_DATA_SELECTION_VBUP'
*      tables
*        it_kosta       = lt_kosta
*        it_lvsta       = lt_lvsta
*        it_wbsta       = lt_wbsta
*        ct_postab      = ct_postab
*      exceptions
*        no_items_found = 1
*        others         = 2.
*    if sy-subrc ne 0.
*      refresh ct_postab_hierq.
** Atenção !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**      raise no_data_found.
*    endif.
*  endif.

* In case of a changed sequence of goods receipt and putaway the data
* have to be reduced based on the customizing settings in TVLR
  if if_proctype eq proctype_inbgdrc and
     lf_flag_seq_grpa eq charx.
    call function 'ZWM_LM_GR_PUTAWAY_REDUCTION'
      tables
        it_tvlr   = lt_tvlr
        ct_postab = ct_postab.
    if ct_postab[] is initial.
      raise no_data_found.
    endif.
  endif.

  call customer-function '001'
       exporting
            if_flag_inbound = if_flag_inbound
            if_proctype     = if_proctype
       tables
            ct_postab       = ct_postab.

  if ( if_proctype eq proctype_conf or
       if_proctype eq proctype_inbconf ) and
     if_no_additional_data eq space.                        "n_605460
    call function 'ZWM_LM_TRANSFER_ORDER_ADD'
      tables
        ct_postab = ct_postab.
  endif.

* Show progress when called by delivery monitor
  if sy-tcode(4) eq 'VL06'.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        percentage = 0
        text       = 'Liste wird aufbereitet...'(066).
  endif.

  if if_no_additional_data eq space.                        "n_605460
    call function 'ZWM_LM_UOM_CONVERSION'
      exporting
        if_ref_gewei = if_ref_gewei
        if_ref_voleh = if_ref_voleh
      tables
        ct_postab    = ct_postab.
  endif.                                                    "n_605460


  if ( if_proctype eq proctype_pick or
       if_proctype eq proctype_inbpick ) and
     if_no_additional_data eq space.                        "n_605460
    call function 'ZWM_LM_DAILY_WORK_LIST'
      tables
        ct_postab       = ct_postab
        ct_postab_hierq = ct_postab_hierq.
  endif.


  if ct_postab[] is initial.                                "50A
    raise no_data_found.
  endif.

  call function 'ZWM_LM_COUNT_ORDER_ITEMS'
    exporting
      if_anzpos = if_anzpos
    tables
      ct_postab = ct_postab.

endfunction.
