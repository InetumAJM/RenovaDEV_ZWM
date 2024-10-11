*&---------------------------------------------------------------------*
*& Include RV50QTOP                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*

* Type pool for search helps and select_options_restrict
type-pools: shlp, sscr.

* declaration of VL tables (needed in selections screen definition)
tables:  likp, lips, vepvg, vbsk, vbuk, mv50l, vttk, tvst, tpar, rl03t.
tables:  lipspo, vekp, t006, zwmov, ekko, vbak, vbap, vbrk, vbrp, knvv.

* return value of selection screen
tables:  sscrfields.

data:
* text for button show hide variants
         txt_varz type smp_dyntxt,
* show_sel_variants
         gf_showvar type c,
* fill selection criteria
         gs_rspar like rsparams occurs 10 with header line,
* retail or standard system
         gs_sysdef like tsysdef,
* variant for initial report
         gf_variant_main_prog like rsvar-variant,
* show and hide parameter
         gc_show  type n value 0,
         gc_hide  type n value 1,
* okcode
         gf_okcode       type sscrfields-ucomm,
* modus of selection
         gf_mode         type sscrfields-ucomm,
* selection variant
         gf_var like rsvar-variant,
* report name
         gf_progname like sy-repid,
* Default selection variant
         gf_default_variant like sy-slset,                  "50A

* program type
         gf_proctype type c,
* general help string
         char(1)     type c,           " Help string
         gf_help_field(10),
         gf_help_value(40).

* Excluding-Fcodes für Status
data: begin of excl_fcodes occurs 0,
        fcode like rseu1-func.
data: end   of excl_fcodes.

* return structur from selection function
data: et_postab like lipov occurs 0 with header line.

* boolean type
types: boolean    type c.

constants:
*     Schalter
      true              type boolean      value 'X',
      false             type boolean      value ' ',
* selection variant pattern
      gc_pattern_variant(10) type c       value 'LF_SVAR',
* processing types
      gc_proctype_pick     type c value 'K',  " picking
      gc_proctype_conf     type c value 'Q',  " picking confirmation
      gc_proctype_load     type c value 'L',  " loading
      gc_proctype_tran     type c value 'T',  " transportation planning
      gc_proctype_gdsi     type c value 'W',  " goods issue
      gc_proctype_free     type c value 'F',  " free selection
      gc_proctype_unch     type c value 'U',  " unchecked deliveries"AIP
      gc_proctype_dist     type c value 'D',  " distribution WMS
      gc_proctype_inb_dist type c value 'E',  " distribution WMS inbound
      gc_proctype_inb_tran type c value 'S',  " tran. plan. inbound
      gc_proctype_inb_pick type c value 'M',  " pick selection inbound
      gc_proctype_inb_conf type c value 'R',  " conf selection inbound
      gc_proctype_inb_gdrc type c value 'N',  " selection goods receipt
      gc_proctype_inb_free type c value 'P',  " free selection inbound
* selection reports
   gc_progname      type sy-repid     value 'WS_DELIVERY_MONITOR',
   gc_progname_unch type sy-repid     value 'WS_MONITOR_OUTB_DEL_UNCH',
   gc_progname_pick type sy-repid     value 'WS_MONITOR_OUTB_DEL_PICK',
   gc_progname_conf type sy-repid     value 'WS_MONITOR_OUTB_DEL_CONF',
   gc_progname_load type sy-repid     value 'WS_MONITOR_OUTB_DEL_LOAD',
   gc_progname_tran type sy-repid     value 'WS_MONITOR_OUTB_DEL_TRAN',
   gc_progname_gdsi type sy-repid     value 'WS_MONITOR_OUTB_DEL_GDSI',
   gc_progname_free type sy-repid     value 'WS_MONITOR_OUTB_DEL_FREE',
   gc_progname_dist type sy-repid     value 'WS_MONITOR_OUTB_DEL_DIST',
   gc_progname_inb_dist type sy-repid value 'WS_MONITOR_INB_DEL_DIST',
   gc_progname_inb_tran type sy-repid value 'WS_MONITOR_INB_DEL_TRAN',
   gc_progname_inb_pick type sy-repid value 'WS_MONITOR_INB_DEL_PICK',
   gc_progname_inb_conf type sy-repid value 'WS_MONITOR_INB_DEL_CONF',
   gc_progname_inb_gdrc type sy-repid value 'WS_MONITOR_INB_DEL_GDRC',
   gc_progname_inb_free type sy-repid value 'WS_MONITOR_INB_DEL_FREE',
* name of standard variants
   gc_var_standard   type rsvar-variant value 'SAP&STANDARD',
   gc_var_inbound    type rsvar-variant value 'SAP&INB_DELMON',
   gc_var_outbound   type rsvar-variant value 'SAP&OUB_DELMON'.
