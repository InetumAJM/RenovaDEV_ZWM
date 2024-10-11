*----------------------------------------------------------------------*
***INCLUDE RV50QAF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECTION_ORDER
*&---------------------------------------------------------------------*
form selection_order using proctype.

* select data from db
  call function 'ZWM_ORDER_MONITOR'
    exporting
      if_c_proctype    = proctype
*      if_c_parvw       = if_parvw
      if_c_item_add    = 'X'  "if_item
*      if_c_anzpos      = if_anzpo
*      if_c_spdnr_add   = if_spd_a
*      if_c_ref_gewei   = if_refgw
*      if_c_ref_voleh   = if_refvo
*      if_c_alakt       = if_alakt
*      if_c_komim       = if_komim
      if_c_uncha       = if_uncha                     "AIP
*      if_c_order       = if_order
*      if_c_billd       = if_billd
    tables
      it_c_kunag       = it_kunag
      it_c_kunwe       = it_kunwe
      it_c_vkorg       = it_vkorg
      it_c_kdgrp       = it_kdgrp
      it_c_matnr       = it_matnr
      it_c_sammg       = it_sammg
      it_c_audat       = it_audat
*      it_c_fkdat       = it_fkdat
      it_c_auart       = it_auart
*      it_c_fkart       = it_fkart
      it_c_vbeln       = it_vbeln
*      it_c_vbelf       = it_vbelf
      it_c_vsbed       = it_vsbed
      it_c_charg       = it_charg                     "46A
*      it_c_kostk       = it_kostk
*      it_c_wbstk       = it_wbstk
*      it_c_fkstk       = it_fkstk
*      it_c_koquk       = it_koquk
*      it_c_lvstk       = it_lvstk
*      it_c_trsta       = it_trsta
*      it_c_pkstk       = it_pkstk
      it_c_ernam       = it_ernam
      it_c_erdat       = it_erdat
      it_c_vtweg       = it_vtweg
      it_c_spart       = it_spart
      it_c_vlstk       = it_vlstk
      it_c_abeln       = it_abeln                     "AIP
      it_c_ebeln       = it_ebeln                     "AIP
      it_c_vgsys       = it_vgsys                     "AIP
*      it_c_v2_outp_sel = i_kschln
*      it_c_v4_outp_sel = i_kschl
*      it_c_v5_outp_sel = i_kschl5
    exceptions
      no_data_found    = 1
      wrong_proc_type  = 2.

  if sy-subrc = 1.
*   message s008.
*   Não existem entradas para a selecção efectuada
    message s130(ZWMMSG001).
  elseif sy-subrc = 2.
    message s009.
  endif.

endform.                               " SELECTION_DELIVERY
*&---------------------------------------------------------------------*
*&      Form  PREPARE_OUTPUT
*&---------------------------------------------------------------------*
form prepare_output.

  ranges lt_kosta   for vbup-kosta.
  ranges lt_lvsta   for vbup-lvsta.
* time span of selection in days
  data: lc_delta type dats value 8.
* number of selection criteria
  data: lf_num_lines type n.
* last day of intervall
  data: lf_ldays type sy-datlo.
* selection type
  data: lf_proctype type c.
* perform default value setting only the first time
  statics: lf_first_run type c.
* select intervall 1 week
  lf_ldays = sy-datlo + lc_delta.

* Route plan is now available in standard system, system switch
* is no longer relevant (do not delete this lines, relevant in 4.0B!)
*  LOOP AT SCREEN.
*    IF SCREEN-GROUP1 = 'AUL' AND GS_SYSDEF-SYSDEF = '00'.
**     standard system, hide select option route plan
*      SCREEN-ACTIVE = 0.
*    ENDIF.
*    MODIFY SCREEN.
*  ENDLOOP.

* set default selection values

  check sy-repid = gc_progname_free or
        sy-repid = gc_progname_pick or
        sy-repid = gc_progname_load or
        sy-repid = gc_progname_conf or
        sy-repid = gc_progname_gdsi or
        sy-repid = gc_progname_dist or
        sy-repid = gc_progname_unch or                      "AIP
        sy-repid = gc_progname_tran.

  if vari-variant cs gc_var_standard
     and lf_first_run eq space.
    lf_first_run = 'X'.
* set dynamic values for select options
*    case sy-repid.
*      when gc_progname_unch.                                "AIP
*        describe table it_wadat lines lf_num_lines.
*        if lf_num_lines eq 0.
*          it_wadat-sign    = 'I'.
*          it_wadat-option  = 'BT'.
*          it_wadat-low     = sy-datlo.
*          it_wadat-high    = lf_ldays.
*          append it_wadat.
*        endif.
*      when gc_progname_pick.
*        describe table it_kodat lines lf_num_lines.
*        if lf_num_lines eq 0.
*          it_kodat-sign    = 'I'.
*          it_kodat-option  = 'BT'.
*          it_kodat-low     = sy-datlo.
*          it_kodat-high    = lf_ldays.
*          append it_kodat.
*        endif.
*      when gc_progname_dist.
*        describe table it_kodat lines lf_num_lines.
*        if lf_num_lines eq 0.
*          it_kodat-sign    = 'I'.
*          it_kodat-option  = 'BT'.
*          it_kodat-low     = sy-datlo.
*          it_kodat-high    = lf_ldays.
*          append it_kodat.
*        endif.
*      when gc_progname_load.
*        describe table it_lddat lines lf_num_lines.
*        if lf_num_lines eq 0.
*          it_lddat-sign    = 'I'.
*          it_lddat-option  = 'BT'.
*          it_lddat-low     = sy-datlo.
*          it_lddat-high    = lf_ldays.
*          append it_lddat.
*        endif.
*      when gc_progname_conf.
*        describe table it_wadat lines lf_num_lines.
*        if lf_num_lines eq 0.
*          it_wadat-sign    = 'I'.
*          it_wadat-option  = 'BT'.
*          it_wadat-low     = sy-datlo.
*          it_wadat-high    = lf_ldays.
*          append it_wadat.
*        endif.
*      when gc_progname_gdsi.
*        describe table it_wadat lines lf_num_lines.
*        if lf_num_lines eq 0.
*          it_wadat-sign    = 'I'.
*          it_wadat-option  = 'BT'.
*          it_wadat-low     = sy-datlo.
*          it_wadat-high    = lf_ldays.
*          append it_wadat.
*        endif.
*      when gc_progname_tran.
*        describe table it_tddat lines lf_num_lines.
*        if lf_num_lines eq 0.
*          it_tddat-sign    = 'I'.
*          it_tddat-option  = 'BT'.
*          it_tddat-low     = sy-datlo.
*          it_tddat-high    = lf_ldays.
*          append it_tddat.
*        endif.
*      when gc_progname_free.
*        describe table it_wadat lines lf_num_lines.
*        if lf_num_lines eq 0.
*          it_wadat-sign    = 'I'.
*          it_wadat-option  = 'BT'.
*          it_wadat-low     = sy-datlo.
*          it_wadat-high    = lf_ldays.
*          append it_wadat.
*        endif.
*    endcase.
  endif.

  case sy-repid.
    when gc_progname_unch.                                  "AIP
      lf_proctype = gc_proctype_unch.
    when gc_progname_pick.
      lf_proctype = gc_proctype_pick.
    when gc_progname_dist.
      lf_proctype = gc_proctype_dist.
    when gc_progname_load.
      lf_proctype = gc_proctype_load.
    when gc_progname_conf.
      lf_proctype = gc_proctype_conf.
    when gc_progname_gdsi.
      lf_proctype = gc_proctype_gdsi.
    when gc_progname_tran.
      lf_proctype = gc_proctype_tran.
  endcase.

* Do not display some parameters in confirmation mode (may cause
* misunderstandings since some parameters are linked with OR)
  if lf_proctype eq gc_proctype_conf.
    loop at screen.
      if screen-name eq 'IF_FL_FI'
      or screen-name eq 'IF_FL_WM'
      or screen-name cs 'IT_KOSTK'
      or screen-name cs 'IT_KOQUK'
      or screen-name cs 'IT_WBSTK'
      or screen-name cs 'IT_LVSTK'.
        screen-active = 0.
        modify screen.
      endif.
    endloop.
  endif.
* Do not display options for choosing WM relevance in distribution mode
  if lf_proctype eq gc_proctype_dist.                       "v_n_600491
    loop at screen.
      if screen-name eq 'IF_FL_FI'
      or screen-name eq 'IF_FL_WM'
      or screen-name eq 'IF_FL_BE'.
        screen-active = 0.
        modify screen.
      endif.
    endloop.
  endif.                                                    "^_n_600491

*  if not lf_proctype is initial.
*    call function 'WS_LM_STATUS_SET'
*      exporting
*        if_proctype = lf_proctype
**        if_flag_wm  = if_fl_wm
**        if_flag_fix = if_fl_fi
*      tables
*        ct_kostk    = it_kostk
*        ct_kosta    = lt_kosta
*        ct_lvstk    = it_lvstk
*        ct_lvsta    = lt_lvsta
*        ct_wbstk    = it_wbstk
*        ct_bestk    = it_bestk                        "AIP
*        ct_vlstk    = it_vlstk
*        ct_koquk    = it_koquk.
*  endif.

endform.                               " PREPARE_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_STATUS_AND_TITLE
*&---------------------------------------------------------------------*
form set_status_and_title.

  set pf-status 'SEL'.
  set titlebar 'TITLE'.

endform.                               " SET_STATUS_AND_TITLE

*&--------------------------------------------------------------------*
*&      Form  selection_order_display
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
form selection_order_display.

  perform selection_order using gc_proctype_pick.

endform.                    "selection_order_display

*&---------------------------------------------------------------------*
*&      Form  CHECK_STANDARD_OR_RETAIL
*&---------------------------------------------------------------------*
form check_standard_or_retail.

* display route plan only in RETAIL system
  call function 'SYSTEM_SWITCH_GET'
    importing
      sysdef = gs_sysdef
    exceptions
      error  = 1
      others = 2.

*  Im Fehlerfall wird Standard angenommen
  if sy-subrc <> 0.
    gs_sysdef-sysdef = '00'.
  endif.

endform.                               " CHECK_STANDARD_OR_RETAIL
*&---------------------------------------------------------------------*
*&      Form  ANALYSE_ADD_PARTNER
*&---------------------------------------------------------------------*
form analyse_add_partner.

  data    lf_lines type p.
  ranges  lf_pernr for vbpa-pernr.
  ranges  lf_parnr for vbpa-parnr.

*  describe table  it_partn lines lf_lines.
*  if ( lf_lines = 0 )  and  ( not if_parvw is initial ).
*    message e531(vr).
*  endif.

*  case if_parvw.
**   Partnerrolle zur Partnernummer vorhanden ?
*    when '  '.
*      if  lf_lines > 0.
*        message e532(vr).
*      endif.
**   Sonderbehandlung für Partner im Index
*    when 'AG'.
*      perform check_standard_partner tables it_kunag.
*    when 'WE'.
*      perform check_standard_partner tables it_kunwe.
**    when 'SP'.
**      perform check_standard_partner tables it_spdnr.
*    when others.
*      select single * from  tpar
*                      where parvw = if_parvw.
*      case tpar-nrart.
*        when 'KU'.
*        when 'LI'.
**        when 'PE'.
**          refresh lf_pernr.
**          loop at it_partn.
**            move-corresponding it_partn to lf_pernr.
**            append lf_pernr.
**          endloop.
*        when 'AP'.
**          refresh lf_parnr.
**          loop at it_partn .
**            move-corresponding it_partn to lf_parnr.
**            append lf_parnr.
**          endloop.
*        when others.
*          message e540(vr) with tpar-nrart if_parvw.
*      endcase.
*  endcase.

endform.                               " ANALYSE_ADD_PARTNER

*&---------------------------------------------------------------------*
*&      Form  CHECK_STANDARD_PARTNER
*&---------------------------------------------------------------------*
form check_standard_partner tables ls_it_kunwe structure it_kunwe.

*  data lf_lines type p.
*  describe table ls_it_kunwe lines lf_lines.
*  if lf_lines = 0.
**    loop at it_partn.
**      ls_it_kunwe = it_partn.
**      append ls_it_kunwe.
**    endloop.
*    message i534(vr) with if_parvw.
*  else.
*    message w533(vr) with if_parvw.
*  endif.
*  clear   if_parvw.
**  clear   it_partn.
**  refresh it_partn.

endform.                               " CHECK_STANDARD_PARTNER
*&---------------------------------------------------------------------*
*&      Form  PROCESS_INPUT
*&---------------------------------------------------------------------*
form process_input.

  gf_okcode = sscrfields-ucomm.

endform.                               " PROCESS_INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_REPORT_PARAMETER
*&---------------------------------------------------------------------*
form get_report_parameter.

  if it_vkorg-low is initial.
    get parameter id 'VKO' field it_vkorg-low.
    if not it_vkorg-low is initial.
      it_vkorg-sign    = 'I'.
      it_vkorg-option  = 'EQ'.
      append it_vkorg.
    endif.
  endif.

  if it_vtweg-low is initial.
    get parameter id 'VTW' field it_vtweg-low.
    if not it_vtweg-low is initial.
      it_vtweg-sign    = 'I'.
      it_vtweg-option  = 'EQ'.
      append it_vtweg.
    endif.
  endif.

  if it_spart-low is initial.
    get parameter id 'SPA' field it_spart-low.
    if not it_spart-low is initial.
      it_spart-sign    = 'I'.
      it_spart-option  = 'EQ'.
      append it_spart.
    endif.
  endif.

*  if if_vstel-low is initial.
*    get parameter id 'VST' field if_vstel-low.
*    if not if_vstel-low is initial.
*      if_vstel-sign    = 'I'.
*      if_vstel-option  = 'EQ'.
*      append if_vstel.
*    endif.
*  endif.
endform.                               " GET_REPORT_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  F4_HELP_PARTNER
*&---------------------------------------------------------------------*
*       F4-help for the additional partner must be dependend on the
*       partner role
*----------------------------------------------------------------------*
form f4_help_partner changing cf_partner like vepvg-kunwe.

  constants: lc_fieldname type dynfnam value 'IF_PARVW'.
  data: lf_parvw like vbpa-parvw,
        lt_dynpro like dynpread occurs 0 with header line,
        lf_programname like d020s-prog,
        lf_dynpronumber like d020s-dnum,
        lf_selfieldname like dd33s-fieldname,
        lf_nrart like tpar-nrart,
        lf_matchcode like help_info-mcobj,
        lt_returnvalues like ddshretval occurs 0 with header line.

* Read value for partner role from the dynpro
  lt_dynpro-fieldname = lc_fieldname.
  append lt_dynpro.

  lf_programname = sy-repid.
  lf_dynpronumber = sy-dynnr.

  call function 'DYNP_VALUES_READ'
    exporting
      dyname               = lf_programname
      dynumb               = lf_dynpronumber
      translate_to_upper   = 'X'
    tables
      dynpfields           = lt_dynpro
    exceptions
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      others               = 10.
  if sy-subrc ne 0.
    message s539(vr).
    exit.
  endif.

  read table lt_dynpro with key fieldname = lc_fieldname.
  if sy-subrc eq 0.
    lf_parvw = lt_dynpro-fieldvalue.
  endif.

* Exit if partner role is not yet given
  if lf_parvw is initial.
    message s532(vr).
    exit.
  endif.

* Read partner type for the given partner role and assign search value
  select single nrart from tpar
                      into (lf_nrart)
                      where parvw eq lf_parvw.
  case lf_nrart.
    when 'KU'.
      lf_selfieldname = 'KUNNR'.
      lf_matchcode = 'DEBI'.
    when 'LI'.
      lf_selfieldname = 'LIFNR'.
      lf_matchcode = 'KREDI'.
    when 'AP'.
      lf_selfieldname = 'PARNR'.
      lf_matchcode = 'VKNK'.
    when 'PE'.
      lf_selfieldname = 'PERNR'.
      lf_matchcode = 'PREM'.
    when others.
      message i535(vr) with lf_parvw.
      exit.
  endcase.

  refresh lt_returnvalues.
* Search help dialog returning a single selected value
  call function 'F4IF_FIELD_VALUE_REQUEST'
       exporting
            tabname           = 'VBPA'
            fieldname         = lf_selfieldname
            searchhelp        = lf_matchcode
*           SHLPPARAM         = ' '
            dynpprog          = lf_programname
            dynpnr            = lf_dynpronumber
*           DYNPROFIELD       = ' '
*           STEPL             = 0
*           VALUE             = ' '
*           MULTIPLE_CHOICE   = ' '
*           DISPLAY           = ' '
*           CALLBACK_PROGRAM  = ' '
*           CALLBACK_FORM     = ' '
       tables
            return_tab        = lt_returnvalues
       exceptions
            field_not_found   = 1
            no_help_for_field = 2
            inconsistent_help = 3
            no_values_found   = 4
            others            = 5.
  if sy-subrc ne 0.
    exit.
  endif.
  loop at lt_returnvalues where fieldname eq lf_selfieldname.
    cf_partner = lt_returnvalues-fieldval.
  endloop.

endform.                               " F4_HELP_PARTNER
*&---------------------------------------------------------------------*
*&      Form  SELECT_OPTIONS_RESTRICT  (HUM/99A)
*&---------------------------------------------------------------------*
*       Restricts the input possibilities for several select-options
*       (forbidds excluding criteria)
*----------------------------------------------------------------------*
form select_options_restrict.

  data: lx_restrict type sscr_restrict,
        lt_ass      type sscr_ass,
        lt_opt_list type sscr_opt_list.

  clear lt_opt_list.
  lt_opt_list-name       = 'IT_EXIDV'.
  lt_opt_list-options-bt = 'X'.
  lt_opt_list-options-eq = 'X'.
  append lt_opt_list to lx_restrict-opt_list_tab.

  clear lt_ass.
  lt_ass-kind    = 'S'.
  lt_ass-name    = 'IT_EXIDV'.
  lt_ass-sg_main = 'I'.
  lt_ass-sg_addy = space.
  lt_ass-op_main = 'IT_EXIDV'.
  append lt_ass to lx_restrict-ass_tab.

  call function 'SELECT_OPTIONS_RESTRICT'
    exporting
      restriction            = lx_restrict
    exceptions
      too_late               = 0
      repeated               = 0
      selopt_without_options = 0
      selopt_without_signs   = 0
      invalid_sign           = 0
      empty_option_list      = 0
      invalid_kind           = 0
      repeated_kind_a        = 0
      others                 = 0.

endform.                               " SELECT_OPTIONS_RESTRICT
*&---------------------------------------------------------------------*
*&      Form  determine_default_variant (50A)
*&---------------------------------------------------------------------*
*       Gets default selection variant from the user's parameter
*----------------------------------------------------------------------*
form determine_default_variant using    value(if_progname) like sy-repid
                               changing cf_variant like sy-slset.

  data: lf_param(20) type c,
        lf_variant like sy-slset.

* Continue only if standard variant is active
  check sy-slset eq 'SAP&STANDARD'.

* Determine parameter id
* MLR: 2004.05.07 Ini >>>>>
*  case if_progname.
*    when 'WS_MONITOR_OUTB_DEL_PICK'.
*      lf_param = 'VA1'.
*    when 'WS_MONITOR_OUTB_DEL_CONF'.
*      lf_param = 'VA3'.
*    when 'WS_MONITOR_OUTB_DEL_LOAD'.
  lf_param = 'VA2'.
*    when 'WS_MONITOR_OUTB_DEL_TRAN'.
*      lf_param = 'VA5'.
*    when 'WS_MONITOR_OUTB_DEL_GDSI'.
*      lf_param = 'VA4'.
*    when 'WS_MONITOR_OUTB_DEL_FREE'.
*      lf_param = 'VA6'.
*    when 'WS_MONITOR_OUTB_DEL_UNCH'.
*      lf_param = 'V12'.
*    when 'WS_MONITOR_OUTB_DEL_DIST'.
*      lf_param = 'V13'.
*  endcase.
* MLR: 2004.05.07 Fim <<<<<

  get parameter id lf_param field lf_variant.

  if not lf_variant eq 'SAP&STANDARD'.
    cf_variant = lf_variant.
  else.
    clear cf_variant.
  endif.

endform.                               " determine_default_variant
*&---------------------------------------------------------------------*
*&      Form  set_default_variant (50A)
*&---------------------------------------------------------------------*
*       Sets default selection variant if deviating from standard
*----------------------------------------------------------------------*
form set_default_variant using    value(if_progname) like sy-repid
                         changing cf_variant like sy-slset.

  check not cf_variant is initial.

  call function 'RS_SUPPORT_SELECTIONS'
    exporting
      report               = if_progname
      variant              = cf_variant
    exceptions
      variant_not_existent = 1
      variant_obsolete     = 2
      others               = 3.

  if sy-subrc ne 0.
    call function 'RS_SUPPORT_SELECTIONS'
      exporting
        report               = if_progname
        variant              = 'SAP&STANDARD'
      exceptions
        variant_not_existent = 0
        variant_obsolete     = 0
        others               = 0.
  endif.

* Clear variant variable to ensure that the default variant is set only
* once on start of the program
  clear cf_variant.

endform.                               " set_default_variant
