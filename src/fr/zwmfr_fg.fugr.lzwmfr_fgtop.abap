FUNCTION-POOL zwmfr_fg MESSAGE-ID zwmfr001.
**********************************************************************
* Common
**********************************************************************
*** Tables

*** Classes Definitions

*** Type Pools
TYPE-POOLS abap.

*** Constants
CONSTANTS c_balobj_zwm_fr         TYPE balobj_d         VALUE 'ZWM_FR'.
CONSTANTS c_balsubobj_interface   TYPE balsubobj        VALUE 'INTERFACE'.
CONSTANTS c_msgty_e               TYPE symsgty          VALUE 'E'.
CONSTANTS c_msgid_zwmfr001        TYPE bdcmsgcoll-msgid VALUE 'ZWMFR001'.
CONSTANTS c_msgid_zwmmsg001       TYPE bdcmsgcoll-msgid VALUE 'ZWMMSG001'.

*** Types and Table Types

*** Internal Tables

*** Ranges

*** Work Areas

*** Field Symbols

*** Variables
DATA: gv_key_exit_lock TYPE keywords.

*** Object Instances

*** OLE Objects

**********************************************************************
* Z_WMFR_RFC_PRODUCTION_ENTRY
**********************************************************************
*** Tables

*** Classes Definitions

*** Type Pools

*** Constants
CONSTANTS c_ropt_eq               TYPE tvarv_opti             VALUE 'EQ'.
CONSTANTS c_ropt_bt               TYPE tvarv_opti             VALUE 'BT'.
CONSTANTS c_rsig_i                TYPE tvarv_sign             VALUE 'I'.
CONSTANTS c_label_default         TYPE zwm_aux-out_cb_tx      VALUE 'RENOVA'.
CONSTANTS c_spras_fr              TYPE spras                  VALUE 'F'.
CONSTANTS c_spras_pt              TYPE spras                  VALUE 'P'.
CONSTANTS c_spras_es              TYPE spras                  VALUE 'S'.
CONSTANTS c_eantp_ic              TYPE mean-eantp             VALUE 'IC'.
CONSTANTS c_eantp_he              TYPE mean-eantp             VALUE 'HE'.
CONSTANTS c_meinh_pal             TYPE mean-meinh             VALUE 'PAL'.
CONSTANTS c_meinh_pal_alt         TYPE mean-meinh             VALUE 'pal'.
CONSTANTS c_lety1_p2              TYPE mlgn-lety1             VALUE 'P2'.
CONSTANTS c_lety1_p5              TYPE mlgn-lety1             VALUE 'P5'.
CONSTANTS c_process_prodentry     TYPE zwm001-processo        VALUE 'ENTRADA_PRODUCAO'.
CONSTANTS c_process_general       TYPE zwm001-processo        VALUE 'GERAL'.
CONSTANTS c_param_max             TYPE zwm001-parametro       VALUE 'MAX'.
CONSTANTS c_param_min             TYPE zwm001-parametro       VALUE 'MIN'.
CONSTANTS c_param_lgort_ba        TYPE zwm001-parametro       VALUE 'LGORT_BA'.
CONSTANTS c_param_movmm_ablad     TYPE zwm001-parametro       VALUE 'MOVMM_ABLAD'.
CONSTANTS c_lgort_cd              TYPE lgort_d                VALUE 'CD'.

*** Types and Table Types
TYPES: BEGIN OF ty_aufk,
        aufnr TYPE aufk-aufnr,
        werks TYPE aufk-werks,
        objnr TYPE aufk-objnr,
       END OF ty_aufk.
TYPES: ty_ht_aufk TYPE HASHED TABLE OF ty_aufk WITH UNIQUE KEY aufnr.

TYPES: BEGIN OF ty_jest,
        objnr TYPE jest-objnr,
        stat  TYPE jest-stat,
        inact TYPE jest-inact,
       END OF ty_jest.
TYPES: ty_st_jest TYPE SORTED TABLE OF ty_jest WITH NON-UNIQUE KEY objnr.

TYPES: BEGIN OF ty_ean,
        matnr TYPE mean-matnr,
        meinh TYPE mean-meinh,
        ean11 TYPE mean-ean11,
        eantp TYPE mean-eantp,
        hpean TYPE mean-hpean,
       END OF ty_ean.

TYPES: BEGIN OF ty_mseg,
        mblnr TYPE mseg-mblnr,
        mjahr TYPE mseg-mjahr,
        zeile TYPE mseg-zeile,
        matnr TYPE mseg-matnr,
        werks TYPE mseg-werks,
        lgort TYPE mseg-lgort,
        charg TYPE mseg-charg,
       END OF ty_mseg.

TYPES: BEGIN OF ty_reg,
        matnr   TYPE mara-matnr,
        aufnr   TYPE afko-aufnr,
        charg   TYPE afpo-charg,
        lgnum   TYPE mseg-lgnum,
        bwart   TYPE mseg-bwart,
        mblnr   TYPE mkpf-mblnr,
        mjahr   TYPE mkpf-mjahr,
        werks   TYPE mseg-werks,
        lgort   TYPE mseg-lgort,
        pack    TYPE mara-matnr,
        code    TYPE bapi2017_gm_code,
        hukey1  TYPE bapihukey-hu_exid,
        hukey2  TYPE bapihukey-hu_exid,
        menge   TYPE zwm_if_prod-quantidade,
        cunit   TYPE mara-meins,
        maktx   TYPE makt-maktx,
        makt2   TYPE makt-maktx,
        makt3   TYPE makt-maktx,
        vfdat   TYPE mch1-vfdat,
        meins   TYPE mara-meins,
        end_str TYPE char1,
       END OF ty_reg.

*** Internal Tables
DATA gt_zwm030 TYPE STANDARD TABLE OF zwm030 WITH KEY aufnr.
DATA gt_zwm001 TYPE SORTED TABLE OF zwm001 WITH UNIQUE KEY processo parametro item.

*** Ranges

*** Work Areas
DATA gs_ifprod  TYPE zwm_if_prod.
DATA gs_reg     TYPE ty_reg.
DATA gs_log     TYPE zwm_log_efacec.

*** Field Symbols

*** Variables

*** Object Instances

*** OLE Objects

**********************************************************************
* Z_WMFR_WS_OT_CREATE
**********************************************************************
*** Tables

*** Classes Definitions

*** Type Pools

*** Constants
CONSTANTS c_return_no_lgnum       TYPE sysubrc                VALUE 10.
CONSTANTS c_return_no_hardcodes   TYPE sysubrc                VALUE 20.
CONSTANTS c_return_pending_to     TYPE sysubrc                VALUE 30.
CONSTANTS c_return_no_su_to_move  TYPE sysubrc                VALUE 40.
CONSTANTS c_return_no_wm_movemnt  TYPE sysubrc                VALUE 50.
CONSTANTS c_return_no_to_created  TYPE sysubrc                VALUE 60.
CONSTANTS c_return_no_lock        TYPE sysubrc                VALUE 70.
CONSTANTS c_return_invalid_to     TYPE sysubrc                VALUE 80.
CONSTANTS c_process_interface_abs TYPE zwmmpt001-processo     VALUE 'ARRUMACAO_AUTOMATICO'.
CONSTANTS c_param_mov_wm_interf   TYPE zwmmpt001-parametro    VALUE 'MOV_WM'.
CONSTANTS c_param_mov_aut_queues  TYPE zwmmpt001-parametro    VALUE 'FILA_IN_AUT'.
CONSTANTS c_block_03              TYPE mlgn-block             VALUE '03'.
CONSTANTS c_block_04              TYPE mlgn-block             VALUE '04'.
CONSTANTS c_block_05              TYPE mlgn-block             VALUE '05'.
CONSTANTS c_block_06              TYPE mlgn-block             VALUE '06'.
CONSTANTS c_block_07              TYPE mlgn-block             VALUE '07'.
CONSTANTS c_block_08              TYPE mlgn-block             VALUE '08'.
CONSTANTS c_block_09              TYPE mlgn-block             VALUE '09'.
CONSTANTS c_block_10              TYPE mlgn-block             VALUE '10'.

*** Types and Table Types
TYPES: BEGIN OF ty_zwmmpt001,
        processo  TYPE zwmmpt001-processo,
        parametro TYPE zwmmpt001-parametro,
        item      TYPE zwmmpt001-item,
        valor     TYPE zwmmpt001-valor,
       END OF ty_zwmmpt001.
TYPES: ty_st_zwmmpt001  TYPE SORTED TABLE OF ty_zwmmpt001 WITH UNIQUE KEY processo parametro item.

*** Internal Tables
DATA gt_zwmmpt001     TYPE ty_st_zwmmpt001.

*** Ranges

*** Work Areas

*** Field Symbols

*** Variables

*** Object Instances

*** OLE Objects

**********************************************************************
* Z_WMFR_WS_ASYNCHRONOUS_LITE
**********************************************************************
*** Tables

*** Classes Definitions

*** Type Pools

*** Constants
CONSTANTS c_edi_partyp_ls         TYPE edi4sndprt             VALUE 'LS'.
CONSTANTS c_process_edi_wm_auto   TYPE zwmmpt001-processo     VALUE 'AUTOMATICO_EDI'.
CONSTANTS c_param_edi_wm_sndpor   TYPE zwmmpt001-parametro    VALUE 'LOG_SYS'.
CONSTANTS c_param_edi_wm_rcvpor   TYPE zwmmpt001-parametro    VALUE 'ENVIRONMENT'.

*** Types and Table Types

*** Internal Tables

*** Ranges

*** Work Areas

*** Field Symbols

*** Variables

*** Object Instances

*** OLE Objects

**********************************************************************
* Z_WMFR_RFC_GET_TO
**********************************************************************
*** Tables

*** Classes Definitions

*** Type Pools

*** Constants
CONSTANTS c_process_halfpall    TYPE zwm001-processo  VALUE 'MEIA-PALETE'.
CONSTANTS c_process_quarterpall TYPE zwm001-processo  VALUE 'QUARTO-PALETE'.
CONSTANTS c_process_wmentry     TYPE zwm001-processo  VALUE 'ENTRADA_ARMAZEM'.
CONSTANTS c_param_lgtyp         TYPE zwm001-parametro VALUE 'ST_PKB'.
CONSTANTS c_param_bwart_wm      TYPE zwm001-parametro VALUE 'MOV_WM'.
CONSTANTS c_param_bwart_pal     TYPE zwm001-parametro VALUE 'MOV_PAL'.
CONSTANTS c_param_bwart_t       TYPE zwm001-parametro VALUE 'MOV_T'.
CONSTANTS c_param_gmcode        TYPE zwm001-parametro VALUE 'CODE'.
CONSTANTS c_param_plant         TYPE zwm001-parametro VALUE 'PLANT'.
CONSTANTS c_nltyp_cma           TYPE ltap-nltyp       VALUE 'CMA'.

*** Types and Table Types
TYPES ty_t_zwm_sscc TYPE STANDARD TABLE OF zwm_sscc.

TYPES: BEGIN OF ty_mara,
        matnr TYPE mara-matnr,
        extwg TYPE mara-extwg,
       END OF ty_mara.
TYPES: ty_ht_mara TYPE HASHED TABLE OF ty_mara WITH UNIQUE KEY matnr.

TYPES: BEGIN OF ty_kostl,
        extwg TYPE zwmmpt008-extwg,
        kostl TYPE zwmmpt008-kostl,
        fevor TYPE zwmmpt008-fevor,
       END OF ty_kostl.
TYPES: ty_ht_kostl  TYPE HASHED TABLE OF ty_kostl WITH UNIQUE KEY extwg fevor.

*** Internal Tables

*** Ranges

*** Work Areas

*** Field Symbols

*** Variables

*** Object Instances

*** OLE Objects

**********************************************************************
* Z_WMFR_TO_ZXLTOU02_INSREM_QTY
**********************************************************************
*** Tables

*** Classes Definitions

*** Type Pools

*** Constants
CONSTANTS c_vorga_st              TYPE ltap-vorga             VALUE 'ST'.
CONSTANTS c_vorga_sl              TYPE ltap-vorga             VALUE 'SL'.
CONSTANTS c_process_prodcons      TYPE zwm001-processo        VALUE 'CONSUMOS_PRODUCAO'.
CONSTANTS c_param_nltyp_cc        TYPE zwm001-parametro       VALUE 'LINHA_KOSTL_LGTYP'.
CONSTANTS c_param_nltyp_aufnr     TYPE zwm001-parametro       VALUE 'ORDPROD_LGTYP'.
CONSTANTS c_param_werks           TYPE zwm001-parametro       VALUE 'CENTRO'.
CONSTANTS c_param_lgort           TYPE zwm001-parametro       VALUE 'DEPOSITO'.
CONSTANTS c_param_gm_code         TYPE zwm001-parametro       VALUE 'GM_CODE'.
CONSTANTS c_param_bwart_mm_cc     TYPE zwm001-parametro       VALUE 'MOV_MM_CC'.
CONSTANTS c_param_bwart_mm_aufnr  TYPE zwm001-parametro       VALUE 'MOV_MM'.
CONSTANTS c_param_div_dtaf        TYPE zwm001-parametro       VALUE 'DIVISAO'.
CONSTANTS c_extwg_hybrid          TYPE mara-extwg             VALUE 'HIBRIDO'.
CONSTANTS c_consumption_type_c    TYPE char1                  VALUE 'C'.
CONSTANTS c_consumption_type_p    TYPE char1                  VALUE 'P'.
CONSTANTS c_consumption_type_h    TYPE char1                  VALUE 'H'.
CONSTANTS c_cons_errors_mail      TYPE so_recname             VALUE 'Z_ERR_CON450'.

*** Types and Table Types

*** Internal Tables

*** Ranges

*** Work Areas

*** Field Symbols

*** Variables

*** Object Instances

*** OLE Objects

**********************************************************************
* Z_WMFR_RFC_PRODUCTION_ENTRY_V2
**********************************************************************
*** Tables

*** Classes Definitions

*** Type Pools

*** Constants

*** Types and Table Types

*** Internal Tables

*** Ranges

*** Work Areas

*** Field Symbols

*** Variables
DATA gv_ablad TYPE ablad.

*** Object Instances

*** OLE Objects



**********************************************************************
* Z_WMFR_CHANGE_HU
**********************************************************************
*** Tables

*** Classes Definitions

*** Type Pools

*** Constants
CONSTANTS c_deleted TYPE hu_status VALUE '0060'.

*** Types and Table Types

*** Internal Tables

*** Ranges

*** Work Areas

*** Field Symbols

*** Variables

*** Object Instances

*** OLE Objects

**********************************************************************
* Z_WMFR_RF_PICKING_AUT
**********************************************************************
*** Tables

*** Classes Definitions

*** Type Pools

*** Types and Table Types
TYPES ty_st_zwmfrt005 TYPE SORTED TABLE OF zwmfrt005 WITH UNIQUE KEY nlpla refnr lenum.

TYPES: BEGIN OF ty_scr001,
        nlpla       TYPE char14,
        vlenr       type lqua-lenum,
        lenum       TYPE lqua-lenum,
        matnr       TYPE ltap-matnr,
        maktx_1     TYPE text20,
        maktx_2     TYPE text20,
        lgpla       TYPE char14,
        kober       TYPE ltap-kober,
        npall       TYPE numc2,
        lgpla_conf  TYPE char14,
        lgpla_pul1  TYPE numc2,
        lgpla_pul2  TYPE numc2,
       END OF ty_scr001.

TYPES: BEGIN OF ty_texts,
        scr001_title          TYPE text20,
        scr001_nlpla          TYPE text20,
        scr001_npall_conf     TYPE text20,
        scr001_lgpla          TYPE text20,
        scr001_lgpla_conf     TYPE text20,
        scr001_npall_load     TYPE char14,
       END OF ty_texts.

*** Constants
CONSTANTS c_process_change_order      TYPE zwmmpt001-processo     VALUE 'CHANGE_ORDEM'.
CONSTANTS c_process_lung              TYPE zwmmpt001-processo     VALUE 'PULMAO'.
CONSTANTS c_process_queues_maint      TYPE zwmmpt001-processo     VALUE 'GESTAO_FILAS'.
CONSTANTS c_process_treadmill         TYPE zwmmpt001-processo     VALUE 'TAPETE'.
CONSTANTS c_process_warehouse_ent     TYPE zwmmpt001-processo     VALUE 'ENTRADA_ARMAZEM'.
CONSTANTS c_param_bin_type            TYPE zwmmpt001-parametro    VALUE 'NLTYP_OK'.
CONSTANTS c_param_queue_rep           TYPE zwmmpt001-parametro    VALUE 'REP'.
CONSTANTS c_param_queue_aut_rep       TYPE zwmmpt001-parametro    VALUE 'FILA_AUT_REP'.
CONSTANTS c_param_queue_aut_sd        TYPE zwmmpt001-parametro    VALUE 'FILA_AUT_SD'.
CONSTANTS c_param_queue_aut_sd_pul    TYPE zwmmpt001-parametro    VALUE 'ST_PUL'.
CONSTANTS c_param_queue_aut_sd_dck    TYPE zwmmpt001-parametro    VALUE 'ST_DCK'.
CONSTANTS c_param_queue_aut_prm       TYPE zwmmpt001-parametro    VALUE 'FILA_AUT_PRM'.
CONSTANTS c_param_max_pal             TYPE zwmmpt001-parametro    VALUE 'MAX_PAL'.
CONSTANTS c_devty_16x20               TYPE lvs_devtyp             VALUE '16X20'.
CONSTANTS c_devty_16x20its            TYPE lvs_devtyp             VALUE '16X20ITS'.
CONSTANTS c_devty_8x40                TYPE lvs_devtyp             VALUE '8X40'.
CONSTANTS c_pfstatus_scr001           TYPE sypfkey                VALUE 'ZWMFR_GUI_SCR001'.
CONSTANTS c_ucomm_back                TYPE syucomm                VALUE 'BACK'.
CONSTANTS c_ucomm_next                TYPE syucomm                VALUE 'NEXT'.
CONSTANTS c_scr001_lenum              TYPE fieldname              VALUE 'GS_SCR001-LENUM'.
CONSTANTS c_scr001_lgpla_conf         TYPE fieldname              VALUE 'GS_SCR001-LGPLA_CONF'.
CONSTANTS c_scr001_lgpla_pul2         TYPE fieldname              VALUE 'GS_SCR001-LGPLA_PUL2'.

*** Internal Tables

*** Ranges

*** Work Areas
DATA gs_ltap      TYPE ltap.                                "#EC NEEDED
DATA gs_ltak      TYPE ltak.                                "#EC NEEDED
DATA gs_makt      TYPE makt.                                "#EC NEEDED
DATA gs_scr001    TYPE ty_scr001.                           "#EC NEEDED
DATA gs_texts     TYPE ty_texts.                            "#EC NEEDED
DATA gs_xuser     TYPE lrf_wkqu.                            "#EC NEEDED
DATA gs_zwm011    TYPE zwm011.                              "#EC NEEDED
DATA gs_zwm028    TYPE zwm028.                              "#EC NEEDED
DATA gs_zwmfrt005 TYPE zwmfrt005.                           "#EC NEEDED

*** Field Symbols

*** Variables
DATA gv_dynnr     TYPE sydynnr.                             "#EC NEEDED
DATA gv_cursor    TYPE fieldname.                           "#EC NEEDED
DATA gv_equipment TYPE char20.                              "#EC NEEDED
DATA gv_okcode    TYPE syucomm.                             "#EC NEEDED
DATA gv_process   TYPE zwmmp_parametro.                     "#EC NEEDED

*** Object Instances

*** OLE Objects

**********************************************************************
* Z_WMFR_ZXLIDU05_SET_NLPLA
**********************************************************************
*** Tables

*** Classes Definitions

*** Type Pools

*** Constants
CONSTANTS c_process_exit_idoc     TYPE zwmmpt001-processo     VALUE 'EXIT_IDOC'.
CONSTANTS c_param_nltyp_ok        TYPE zwmmpt001-parametro    VALUE 'NLTYP_OK'.
CONSTANTS c_param_vltyp_ok        TYPE zwmmpt001-parametro    VALUE 'VLTYP_OK'.

*** Types and Table Types

*** Internal Tables
DATA gt_zwmfrt004 TYPE STANDARD TABLE OF zwmfrt004.
DATA gt_zwmfrt005 TYPE STANDARD TABLE OF zwmfrt005.

*** Ranges

*** Work Areas

*** Field Symbols

*** Variables

*** Object Instances

*** OLE Objects
