*----------------------------------------------------------------------*
* Include: ZWMFR0002_TOP
*----------------------------------------------------------------------*
* Description: RF - Entrada de Produto Acabado/Bobines PT
* RICEFW: WM.02/WM.03
*----------------------------------------------------------------------*
* Author........: [Pedro Silva] [ROFFD] [ROFF(SDF)]
*                 [Tiago Pateiro] [ROFFD] [ROFF(SDF)]
* Creation date:  2015-10-26
*----------------------------------------------------------------------*
*** Tables

*** Classes Definitions

*** Type Pools
TYPE-POOLS abap.

*** Constants
CONSTANTS c_pfstatus_scr001       TYPE sypfkey                VALUE 'ZWMFR0002_GUI_SCR1'.
CONSTANTS c_pfstatus_scr002       TYPE sypfkey                VALUE 'ZWMFR0002_GUI_SCR2'.
CONSTANTS c_syrepid               TYPE zdev_inc_meth          VALUE sy-repid.
CONSTANTS c_tcode_zwmfr0002a      TYPE sytcode                VALUE 'ZWMFR0002A'.
CONSTANTS c_tcode_zwmfr0002b      TYPE sytcode                VALUE 'ZWMFR0002B'.
CONSTANTS c_tcode_zwmfr0002c      TYPE sytcode                VALUE 'ZWMFR0002C'.
CONSTANTS c_tcode_zwmfr0002d      TYPE sytcode                VALUE 'ZWMFR0002D'.
CONSTANTS c_msgid_zwmfr001        TYPE bdcmsgcoll-msgid       VALUE 'ZWMFR001'.
CONSTANTS c_msgty_s               TYPE bdcmsgcoll-msgtyp      VALUE 'S'.
CONSTANTS c_msgty_e               TYPE bdcmsgcoll-msgtyp      VALUE 'E'.
CONSTANTS c_msgty_w               TYPE bdcmsgcoll-msgtyp      VALUE 'W'.
CONSTANTS c_ropt_eq               TYPE tvarv_opti             VALUE 'EQ'.
CONSTANTS c_rsig_i                TYPE tvarv_sign             VALUE 'I'.
CONSTANTS c_devty_16x20           TYPE lvs_devtyp             VALUE '16X20'.
CONSTANTS c_devty_16x20its        TYPE lvs_devtyp             VALUE '16X20ITS'.
CONSTANTS c_devty_8x40            TYPE lvs_devtyp             VALUE '8X40'.
CONSTANTS c_process_general       TYPE zwmmpt001-processo     VALUE 'GERAL'.
CONSTANTS c_process_tp_entries    TYPE zwmmpt001-processo     VALUE 'ENTRADA_TERCEIROS'.
CONSTANTS c_param_gmcode          TYPE zwmmpt001-parametro    VALUE 'GM_CODE'.
CONSTANTS c_param_bwart           TYPE zwmmpt001-parametro    VALUE 'MOV'.
CONSTANTS c_param_bwart_mm        TYPE zwmmpt001-parametro    VALUE 'MOV_MM'.
CONSTANTS c_param_bwart_wm        TYPE zwmmpt001-parametro    VALUE 'MOV_WM'.
CONSTANTS c_param_bwart_wm_pk     TYPE zwmmpt001-parametro    VALUE 'MOV_WM_PICK'.
CONSTANTS c_param_lgnum_pt        TYPE zwmmpt001-parametro    VALUE 'LGNUM_PT'.
CONSTANTS c_param_lgnum_pa_pt     TYPE zwmmpt001-parametro    VALUE 'LGNUM_PA_PT'.
CONSTANTS c_param_letyp           TYPE zwmmpt001-parametro    VALUE 'MOV_WM_TUD'.
CONSTANTS c_ucomm_back            TYPE syucomm                VALUE 'BACK'.
CONSTANTS c_ucomm_clear           TYPE syucomm                VALUE 'CLR'.
CONSTANTS c_ucomm_next            TYPE syucomm                VALUE 'NEXT'.
CONSTANTS c_ucomm_save            TYPE syucomm                VALUE 'SAVE'.
CONSTANTS c_ucomm_pdown           TYPE syucomm                VALUE 'PGD'.
CONSTANTS c_ucomm_pup             TYPE syucomm                VALUE 'PGU'.
CONSTANTS c_ucomm_retcode_yes     TYPE flag                   VALUE 'O'.
CONSTANTS c_ucomm_retcode_no      TYPE flag                   VALUE 'C'.
CONSTANTS c_cursor_ebeln          TYPE fieldname              VALUE 'GS_SCR001-EBELN'.
CONSTANTS c_cursor_licha          TYPE fieldname              VALUE 'GS_SCR001-LICHA'.
CONSTANTS c_cursor_xblnr          TYPE fieldname              VALUE 'GS_SCR001-XBLNR'.
CONSTANTS c_cursor_npall          TYPE fieldname              VALUE 'GS_SCR001-NPALL'.
CONSTANTS c_cursor_pnext          TYPE fieldname              VALUE 'RLMOB-PNEXT'.
CONSTANTS c_cursor_psave          TYPE fieldname              VALUE 'RLMOB-PSAVE'.
CONSTANTS c_cursor_lenum          TYPE fieldname              VALUE 'GS_SCR002-LENUM'.
CONSTANTS c_cursor_zeugn          TYPE fieldname              VALUE 'GS_SCR002-ZEUGN'.
CONSTANTS c_cursor_matnr          TYPE fieldname              VALUE 'GS_SCR002-MATNR'.
CONSTANTS c_cursor_charg          TYPE fieldname              VALUE 'GS_SCR002-CHARG'.
CONSTANTS c_cursor_vsolm          TYPE fieldname              VALUE 'GS_SCR002-VSOLM'.
CONSTANTS c_status_0020           TYPE vekp-status            VALUE '0020'.
CONSTANTS c_vpobj_12              TYPE vekp-vpobj             VALUE '12'.

*** Types and Table Types
TYPES: BEGIN OF ty_zwmmpt001,
        processo  TYPE zwmmpt001-processo,
        parametro TYPE zwmmpt001-parametro,
        item      TYPE zwmmpt001-item,
        valor     TYPE zwmmpt001-valor,
       END OF ty_zwmmpt001.
TYPES: ty_st_zwmmpt001  TYPE SORTED TABLE OF ty_zwmmpt001 WITH UNIQUE KEY processo parametro item.

TYPES: BEGIN OF ty_texts,
        scr001_title  TYPE text20,
        scr001_tpall  TYPE text10,
        scr002_ud     TYPE text20,
       END OF ty_texts.

TYPES: BEGIN OF ty_scr001,
        ebeln TYPE ekko-ebeln,
        xblnr TYPE likp-vbeln,
        versi TYPE int4,
        licha TYPE mcha-licha,
        npall TYPE int1,
        cpall TYPE int1,
        date  type datum,
       END OF ty_scr001.

TYPES: BEGIN OF ty_scr002,
        lenum   TYPE lqua-lenum,
        vepos   TYPE vepo-vepos,
        zeugn   TYPE ltap-zeugn,
        matnr   TYPE ltap-matnr,
        maktx   TYPE makt-maktx,
        maktx_1 TYPE text20,
        maktx_2 TYPE text20,
        charg   TYPE ltap-charg,
        vsolm   TYPE ltap-vsolm,
        meins   TYPE ltap-meins,
        letyp   TYPE ltap-letyp,
        lgnum   TYPE ltap-lgnum,
        tanum   TYPE ltap-tanum,
        tapos   TYPE ltap-tapos,
        nlpla   TYPE ltap-nlpla,
        werks   TYPE ltap-werks,
        lgort   TYPE ltap-lgort,
        ebelp   TYPE ekpo-ebelp,
        type    TYPE c LENGTH 2,
        remon   TYPE lenum,"palete remontada
       END OF ty_scr002.
TYPES: ty_t_scr002  TYPE STANDARD TABLE OF ty_scr002 WITH KEY lenum.
TYPES: ty_ht_scr002 TYPE HASHED TABLE OF ty_scr002 WITH UNIQUE KEY lenum zeugn.

TYPES: BEGIN OF ty_ekpo,
        ebeln TYPE ekpo-ebeln,
        ebelp TYPE ekpo-ebelp,
        loekz TYPE ekpo-loekz,
        matnr TYPE ekpo-matnr,
        werks TYPE ekpo-werks,
        lgort TYPE ekpo-lgort,
        meins TYPE ekpo-meins,
        elikz TYPE ekpo-elikz,
       END OF ty_ekpo.
TYPES: ty_t_ekpo  TYPE STANDARD TABLE OF ty_ekpo WITH KEY ebeln ebelp.
TYPES: ty_ht_ekpo TYPE HASHED TABLE OF ty_ekpo WITH UNIQUE KEY ebeln ebelp.
TYPES: ty_st_ekpo TYPE SORTED TABLE OF ty_ekpo WITH NON-UNIQUE KEY matnr.

TYPES: BEGIN OF ty_mlgn,
        matnr TYPE mlgn-matnr,
        lgnum TYPE mlgn-lgnum,
        lvorm TYPE mlgn-lvorm,
       END OF ty_mlgn.
TYPES: ty_ht_mlgn TYPE HASHED TABLE OF ty_mlgn WITH UNIQUE KEY matnr lgnum.

TYPES: BEGIN OF ty_ltap,
        lgnum TYPE ltap-lgnum,
        tanum TYPE ltap-tanum,
        tapos TYPE ltap-tapos,
        matnr TYPE ltap-matnr,
        charg TYPE ltap-charg,
        bestq TYPE ltap-bestq,
        meins TYPE ltap-meins,
        letyp TYPE ltap-letyp,
        pquit TYPE ltap-pquit,
        vsolm TYPE ltap-vsolm,
        nlpla TYPE ltap-nlpla,
        vlenr TYPE ltap-vlenr,
       END OF ty_ltap.
TYPES: ty_ht_ltap TYPE HASHED TABLE OF ty_ltap WITH UNIQUE KEY lgnum tanum tapos.

TYPES: BEGIN OF ty_makt,
        matnr TYPE makt-matnr,
        maktx TYPE makt-maktx,
       END OF ty_makt.
TYPES: ty_ht_makt TYPE HASHED TABLE OF ty_makt WITH UNIQUE KEY matnr.

TYPES: BEGIN OF ty_goodsmvt_itm,
        ebeln TYPE ekpo-ebeln,
        ebelp TYPE ekpo-ebelp,
        matnr TYPE ekpo-matnr,
        werks TYPE ekpo-werks,
        charg TYPE ltap-charg,
        meins TYPE ltap-meins,
        vsolm TYPE ltap-vsolm,
        type  TYPE char2,
       END OF ty_goodsmvt_itm.
TYPES: ty_st_goodsmvt_itm TYPE SORTED TABLE OF ty_goodsmvt_itm WITH UNIQUE KEY ebeln ebelp matnr werks charg meins type.

TYPES: BEGIN OF ty_mcha,
        matnr TYPE mcha-matnr,
        werks TYPE mcha-werks,
        charg TYPE mcha-charg,
       END OF ty_mcha.
TYPES: ty_ht_mcha TYPE HASHED TABLE OF ty_mcha WITH UNIQUE KEY matnr werks charg.

*** Internal Tables
DATA gt_scr002          TYPE ty_t_scr002.                     "#EC NEEDED
DATA gt_scr002_pck      TYPE ty_t_scr002.                     "#EC NEEDED
DATA gt_scr002_check    TYPE ty_ht_scr002.                    "#EC NEEDED
DATA gt_ekpo            TYPE ty_st_ekpo.                      "#EC NEEDED
DATA gt_mlgn            TYPE ty_ht_mlgn.                      "#EC NEEDED
DATA gt_ltap            TYPE ty_ht_ltap.                      "#EC NEEDED
DATA gt_makt            TYPE ty_ht_makt.                      "#EC NEEDED
DATA gt_hardcodes       TYPE SORTED TABLE OF zhardcode_table WITH UNIQUE KEY occurrence counter. "#EC NEEDED
DATA gt_zwmmpt001       TYPE ty_st_zwmmpt001.                 "#EC NEEDED
DATA gt_zwmfrt001       TYPE SORTED TABLE OF zwmfrt001 WITH UNIQUE KEY lgnum xblnr ebeln versi ebelp lenum vepos zeugn. "#EC NEEDED
DATA gt_zwmfrt001_conf  TYPE SORTED TABLE OF zwmfrt001 WITH UNIQUE KEY lgnum xblnr ebeln versi ebelp lenum vepos zeugn. "#EC NEEDED
DATA gt_zwm020          TYPE TABLE OF zwm020.

*** Ranges

*** Work Areas
DATA gs_xuser   TYPE lrf_wkqu.                              "#EC NEEDED
DATA gs_texts   TYPE ty_texts.                              "#EC NEEDED
DATA gs_scr001  TYPE ty_scr001.                             "#EC NEEDED
DATA gs_scr002  TYPE ty_scr002.                             "#EC NEEDED

*** Field Symbols

*** Variables
DATA gv_okcode    TYPE syucomm.                             "#EC NEEDED
DATA gv_dynnr_1   TYPE sydynnr.                             "#EC NEEDED
DATA gv_dynnr_2   TYPE sydynnr.                             "#EC NEEDED
DATA gv_haserror  TYPE abap_bool.                           "#EC NEEDED
DATA gv_cursor    TYPE fieldname.                           "#EC NEEDED

*** Object Instances

*** OLE Objects
