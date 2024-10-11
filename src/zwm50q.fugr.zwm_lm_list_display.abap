FUNCTION ZWM_LM_LIST_DISPLAY.
*"----------------------------------------------------------------------
*"*"Interface global:
*"  IMPORTING
*"     VALUE(IF_D_PROCTYPE) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_D_INBOUND) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_D_EXTERNAL_CALL) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_D_ITEM_ADD) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_D_ANZPOS) TYPE  MV50L_ANZPOS DEFAULT SPACE
*"     VALUE(IF_D_PARVW) LIKE  VBPA-PARVW DEFAULT SPACE
*"     VALUE(IF_D_SPDNR_ADD) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_D_FLAG_WM) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_D_FLAG_FIX) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_D_REFNUM_ADD) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_D_LGNUM_HEAD) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_D_REF_GEWEI) LIKE  LIPOV-GEWEI DEFAULT SPACE
*"     VALUE(IF_D_REF_VOLEH) LIKE  LIPOV-VOLEH DEFAULT SPACE
*"     VALUE(IF_D_ALAKT) LIKE  RL03T-ALAKT DEFAULT SPACE
*"     VALUE(IF_D_EINLM) LIKE  RL03T-EINLM DEFAULT SPACE
*"     VALUE(IF_D_UNCHA) LIKE  MV50L-UNCHA DEFAULT SPACE
*"  TABLES
*"      IT_D_POSTAB STRUCTURE  ZWMOV
*"      IT_D_POSTAB_HIERQ STRUCTURE  ZWMOV_HIERQ OPTIONAL
*"      IT_D_OUTPUT_E1 STRUCTURE  LIPOV_OUTPUT OPTIONAL
*"      IT_D_OUTPUT_V2 STRUCTURE  LIPOV_OUTPUT OPTIONAL
*"      IT_D_OUTPUT_V4 STRUCTURE  LIPOV_OUTPUT OPTIONAL
*"      IT_D_OUTPUT_V5 STRUCTURE  LIPOV_OUTPUT OPTIONAL
*"      IT_D_VSTEL STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_D_ROUTE STRUCTURE  RANGE_C6 OPTIONAL
*"      IT_D_AULWE STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_SPDNR STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_KUNAG STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_KUNWE STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_PARTN STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_VKORG STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_D_KDGRP STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_D_MATNR STRUCTURE  RANGE_MAT OPTIONAL
*"      IT_D_WADAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_D_WADAT_IST STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_D_LDDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_D_TDDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_D_LFDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_D_audat STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_D_SAMMG STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_TKNUM STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_LFART STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_D_AUDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_D_AUART STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_D_VBELN STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_LSTEL STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_D_LGNUM STRUCTURE  RANGE_C3 OPTIONAL
*"      IT_D_LGORT STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_D_LGTOR STRUCTURE  RANGE_C3 OPTIONAL
*"      IT_D_ABLAD STRUCTURE  RANGE_C25 OPTIONAL
*"      IT_D_VSBED STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_D_LPRIO STRUCTURE  RANGE_N2 OPTIONAL
*"      IT_D_KOSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_D_WBSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_D_FKSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_D_KOQUK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_D_LVSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_D_TRSTA STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_D_PKSTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_D_BESTK STRUCTURE  RANGE_STAT OPTIONAL
*"      IT_D_EBELN STRUCTURE  RANGE_EBELN OPTIONAL
*"      IT_D_EBELP STRUCTURE  RANGE_C5 OPTIONAL
*"      IT_D_ERNAM STRUCTURE  RANGE_C12 OPTIONAL
*"      IT_D_ERDAT STRUCTURE  RANGE_DATE OPTIONAL
*"      IT_D_VTWEG STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_D_SPART STRUCTURE  RANGE_C2 OPTIONAL
*"      IT_D_TRAID STRUCTURE  RANGE_C20 OPTIONAL
*"      IT_D_LIFEX STRUCTURE  RANGE_C35 OPTIONAL
*"      IT_D_LIFNR STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_KDMAT STRUCTURE  RANGE_C35 OPTIONAL
*"      IT_D_EAN11 STRUCTURE  RANGE_C18 OPTIONAL
*"      IT_D_EXIDV STRUCTURE  RANGE_C20 OPTIONAL
*"      IT_D_CHARG STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_PRVBE STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_AUFNR STRUCTURE  RANGE_C12 OPTIONAL
*"      IT_D_LGBZO_H STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_XBLNR STRUCTURE  RANGE_C25 OPTIONAL
*"      IT_D_TRAGR_H STRUCTURE  RANGE_C4 OPTIONAL
*"      IT_D_ABELN STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_VGSYS STRUCTURE  RANGE_C10 OPTIONAL
*"      IT_D_VLSTK STRUCTURE  RANGE_STAT OPTIONAL
*"----------------------------------------------------------------------

*       Global data declarations

  field-symbols: <ls_d_postab> type zwmov.
* General settings
  perform alv_initialisation.

* Refresh internal memory
  perform refresh_memory.                                   "46A

* Flag 'item_add' can be changed on list -> work with copy
  gf_item_add = if_d_item_add.
* Default transaction type at batch input
  if if_d_proctype eq proctype_gdsi or
     if_d_proctype eq proctype_inbgdrc.
    gf_bitype = bitype_dark.
  else.
    gf_bitype = bitype_bright_on_error.
  endif.

* Prepare output list
  if gf_item_add ne space.
    loop at it_d_postab assigning <ls_d_postab>
                        where posnr ne posnr_initial.
      <ls_d_postab>-counter = 1.                            "n_555998
      append <ls_d_postab> to gt_outtab.
    endloop.
  else.
    loop at it_d_postab assigning <ls_d_postab>
                        where posnr eq posnr_initial.
      <ls_d_postab>-counter = 1.                            "n_555998
      append <ls_d_postab> to gt_outtab.
    endloop.
  endif.

* Default color grouping on item lists
  get parameter id 'YLO_LM_CG' field gf_fl_color_group.      "46A
  perform set_list_colors tables gt_outtab.                 "AIP
  perform set_list_colors tables gt_outtab_s2.


  sort it_d_postab_hierq by vstel audat.
  clear gf_list_rebuild.

* Load manual marker settings from memory
  import gt_marktab from memory id 'LM_MT'.
* Prepare manual marker configuration
  perform marker_init tables gt_outtab.

  do.

*   Evaluate list type (simple or hierarchical) depending on the
*   processing mode and the user settings
    if if_d_proctype eq proctype_pick.
      if gf_list_category(1) eq 'N'.
        gf_list_type = type_basic_list.
      else.
        gf_list_type = type_picking_overview.
      endif.
    else.
      gf_list_type = type_basic_list.
    endif.

*   Define layout and field catalog for first call
    perform define_layout using gf_list_type.
    perform define_fieldcat tables gt_fieldcat
                            using  gf_list_type.
    perform define_variant using if_d_proctype
                                 gf_list_type
                                 gf_item_add.

*   Start list processing
    if gf_list_type eq type_picking_overview.
      perform list_display_picking_overview.
    else.
      perform list_display_basic_list.
    endif.
    if gf_list_rebuild eq space.
      exit.
    endif.
    gf_list_rebuild = space.
  enddo.

* Save manual marker settings for next call in same transaction  "46A
  export gt_marktab to memory id 'LM_MT'.

endfunction.
