*----------------------------------------------------------------------*
* Include: ZWMFR0001_TOP
*----------------------------------------------------------------------*
* Description: Monitor de planeamento do abastecimento à produção
*----------------------------------------------------------------------*
* Author........: [Tiago Pateiro] [ROFFD] [ROFF(SDF)]
* Creation date: 2015-10-23
*----------------------------------------------------------------------*
*** Tables
TABLES resb.
TABLES aufk.
TABLES afko.

*** Classes Definitions

*** Type Pools
TYPE-POOLS sscr.
TYPE-POOLS abap.

*** Constants
CONSTANTS c_pfstatus              TYPE sypfkey                VALUE 'ZWMFR0001_GUI'.
CONSTANTS c_pfstatus_popup        TYPE sypfkey                VALUE 'ZWMFR0001_POPUP_GUI'.
CONSTANTS c_pfstatus_popup2       TYPE sypfkey                VALUE 'ZWMFR0001_POPUP2_GUI'.
CONSTANTS c_pfstatus_popup3       TYPE sypfkey                VALUE 'ZWMFR0001_POPUP3_GUI'.
CONSTANTS c_pfstatus_topopup      TYPE sypfkey                VALUE 'ZWMFR0001_TO_GUI'.
CONSTANTS c_ucomm_tocancel        TYPE syucomm                VALUE '&TO_CANCEL'.
CONSTANTS c_ucomm_tocreate        TYPE syucomm                VALUE '&TO_CREATE'.
CONSTANTS c_ucomm_toview          TYPE syucomm                VALUE '&TO_VIEW'.
CONSTANTS c_ucomm_reqdev          TYPE syucomm                VALUE '&REQ_VEV'.
CONSTANTS c_ucomm_dupview         TYPE syucomm                VALUE '&DUPL_VIEW'.
CONSTANTS c_ucomm_refresh         TYPE syucomm                VALUE '&REFRESH'.
CONSTANTS c_ucomm_accept          TYPE syucomm                VALUE '&ACCEPT'.
CONSTANTS c_ucomm_cancel          TYPE syucomm                VALUE '&CANCEL'.
CONSTANTS c_ucomm_mansel          TYPE syucomm                VALUE '&MANSEL'.
CONSTANTS c_syrepid               TYPE zdev_inc_meth          VALUE sy-repid.
CONSTANTS c_occur_lgtyp           TYPE zdev_occur             VALUE '003'.
CONSTANTS c_occur_lgtyp_cfo       TYPE zdev_occur             VALUE '004'.
CONSTANTS c_occur_mtart           TYPE zdev_occur             VALUE '002'.
CONSTANTS c_occur_stat            TYPE zdev_occur             VALUE '001'.
CONSTANTS c_count_stat            TYPE zdev_count             VALUE '01'.
CONSTANTS c_ropt_eq               TYPE tvarv_opti             VALUE 'EQ'.
CONSTANTS c_ropt_bt               TYPE tvarv_opti             VALUE 'BT'.
CONSTANTS c_rsig_i                TYPE tvarv_sign             VALUE 'I'.
CONSTANTS c_scr_kind              TYPE rsscr-kind             VALUE 'S'.
CONSTANTS c_scrname_werks         TYPE rsrestrict-objectname  VALUE 'S_WERKS'.
CONSTANTS c_msgid_zwmfr001        TYPE symsgid                VALUE 'ZWMFR001'.
CONSTANTS c_msgty_e               TYPE symsgty                VALUE 'E'.
CONSTANTS c_msgty_s               TYPE symsgty                VALUE 'S'.
CONSTANTS c_msgty_w               TYPE symsgty                VALUE 'W'.
CONSTANTS c_fname_gmein           TYPE fieldname              VALUE 'GMEIN'.
CONSTANTS c_fname_meins           TYPE fieldname              VALUE 'MEINS'.
CONSTANTS c_fname_fevor           TYPE fieldname              VALUE 'FEVOR'.
CONSTANTS c_fname_gstrp           TYPE fieldname              VALUE 'GSTRP'.
CONSTANTS c_fname_aufnr           TYPE fieldname              VALUE 'AUFNR'.
CONSTANTS c_fname_kostl           TYPE fieldname              VALUE 'KOSTL'.
CONSTANTS c_fname_plnbez          TYPE fieldname              VALUE 'PLNBEZ'.
CONSTANTS c_fname_matnr           TYPE fieldname              VALUE 'MATNR'.
CONSTANTS c_fname_tfevo           TYPE fieldname              VALUE 'TFEVO'.
CONSTANTS c_fname_tplnbe          TYPE fieldname              VALUE 'TPLNBE'.
CONSTANTS c_fname_tmatn           TYPE fieldname              VALUE 'TMATN'.
CONSTANTS c_fname_nodek           TYPE fieldname              VALUE 'NODEK'.
CONSTANTS c_fname_nodty           TYPE fieldname              VALUE 'NODTY'.
CONSTANTS c_fname_gamng           TYPE fieldname              VALUE 'GAMNG'.
CONSTANTS c_fname_igmng           TYPE fieldname              VALUE 'IGMNG'.
CONSTANTS c_fname_dimng           TYPE fieldname              VALUE 'DIMNG'.
CONSTANTS c_fname_bdmng           TYPE fieldname              VALUE 'BDMNG'.
CONSTANTS c_fname_efmng           TYPE fieldname              VALUE 'EFMNG'.
CONSTANTS c_fname_remng           TYPE fieldname              VALUE 'REMNG'.
CONSTANTS c_fname_check           TYPE lvc_fname              VALUE 'CHCKD'.
CONSTANTS c_icon_fevor            TYPE salv_de_tree_image     VALUE '@ND@'.
CONSTANTS c_icon_gstrp            TYPE salv_de_tree_image     VALUE '@1U@'.
CONSTANTS c_icon_aufnr            TYPE salv_de_tree_image     VALUE '@9Y@'.
CONSTANTS c_icon_matnr            TYPE salv_de_tree_image     VALUE '@AQ@'.
CONSTANTS c_icon_kostl            TYPE salv_de_tree_image     VALUE '@FD@'.
CONSTANTS c_icon_red              TYPE salv_de_tree_image     VALUE '@0A@'.
CONSTANTS c_icon_yellow           TYPE salv_de_tree_image     VALUE '@09@'.
CONSTANTS c_icon_green            TYPE salv_de_tree_image     VALUE '@08@'.
CONSTANTS c_nodty_fevor           TYPE int1                   VALUE 1.
CONSTANTS c_nodty_gstrp           TYPE int1                   VALUE 2.
CONSTANTS c_nodty_aufnr           TYPE int1                   VALUE 3.
CONSTANTS c_nodty_kostl           TYPE int1                   VALUE 4.
CONSTANTS c_nodty_matnr           TYPE int1                   VALUE 6.
CONSTANTS c_process_prod_cons     TYPE zwmmpt001-processo     VALUE 'CONSUMOS_PRODUCAO'.
CONSTANTS c_process_interface_abs TYPE zwmmpt001-processo     VALUE 'INTERFACE_ABS'. "#EC NEEDED
CONSTANTS c_process_general       TYPE zwmmpt001-processo     VALUE 'GERAL'.
CONSTANTS c_process_system_guide  TYPE zwmmpt001-processo     VALUE 'SYSTEM_GUIDE'.
CONSTANTS c_param_lgtyp_bulk      TYPE zwmmpt001-parametro    VALUE 'ARM_LGTYP_BULK'.
CONSTANTS c_param_lgtyp_rack      TYPE zwmmpt001-parametro    VALUE 'ARM_LGTYP_RACK'.
CONSTANTS c_param_lgtyp_locker    TYPE zwmmpt001-parametro    VALUE 'ARM_LGTYP_CACIFO'.
CONSTANTS c_param_queue_lin       TYPE zwmmpt001-parametro    VALUE 'QUEUE_LIN'.
CONSTANTS c_param_mov_wm_op_aufnr TYPE zwmmpt001-parametro    VALUE 'MOV_WM_OP'.
CONSTANTS c_param_mov_wm_op_cc    TYPE zwmmpt001-parametro    VALUE 'MOV_WM_CC'.
CONSTANTS c_param_lgort           TYPE zwmmpt001-parametro    VALUE 'DEPOSITO'.
CONSTANTS c_param_mov_wm_interf   TYPE zwmmpt001-parametro    VALUE 'MOV_WM_INTERF'. "#EC NEEDED
CONSTANTS c_param_mov_wm_locker   TYPE zwmmpt001-parametro    VALUE 'MOV_WM_CACIF'. "#EC NEEDED
CONSTANTS c_extwg_hybrid          TYPE mara-extwg             VALUE 'HIBRIDO'.
CONSTANTS c_totype_cc             TYPE char2                  VALUE 'CC'.
CONSTANTS c_totype_po             TYPE char2                  VALUE 'PO'.

*** Types and Table Types
TYPES: BEGIN OF ty_zwmmpt001,
        processo  TYPE zwmmpt001-processo,
        parametro TYPE zwmmpt001-parametro,
        item      TYPE zwmmpt001-item,
        valor     TYPE zwmmpt001-valor,
       END OF ty_zwmmpt001.
TYPES: ty_st_zwmmpt001  TYPE SORTED TABLE OF ty_zwmmpt001 WITH UNIQUE KEY processo parametro item.

TYPES: BEGIN OF ty_aufk,
        aufnr TYPE aufk-aufnr,
        auart TYPE aufk-auart,
        autyp TYPE aufk-autyp,
        werks TYPE aufk-werks,
        objnr TYPE aufk-objnr,
       END OF ty_aufk.
TYPES: ty_ht_aufk TYPE HASHED TABLE OF ty_aufk WITH UNIQUE KEY aufnr.

TYPES: BEGIN OF ty_afko,
        aufnr   TYPE afko-aufnr,
        gstrp   TYPE afko-gstrp,
        rsnum   TYPE afko-rsnum,
        gamng   TYPE afko-gamng,
        gmein   TYPE afko-gmein,
        plnbez  TYPE afko-plnbez,
        fevor   TYPE afko-fevor,
        igmng   TYPE afko-igmng,
       END OF ty_afko.
TYPES: ty_ht_afko TYPE HASHED TABLE OF ty_afko WITH UNIQUE KEY aufnr.

TYPES: BEGIN OF ty_afpo,
        aufnr   TYPE afpo-aufnr,
        posnr   TYPE afpo-posnr,
        wemng   TYPE afpo-wemng,
       END OF ty_afpo.
TYPES: ty_ht_afpo TYPE HASHED TABLE OF ty_afpo WITH UNIQUE KEY aufnr.

TYPES: BEGIN OF ty_jest,
        objnr TYPE jest-objnr,
        stat  TYPE jest-stat,
        inact TYPE jest-inact,
       END OF ty_jest.
TYPES: ty_ht_jest TYPE HASHED TABLE OF ty_jest WITH UNIQUE KEY objnr stat.

TYPES: BEGIN OF ty_resb,
        rsnum TYPE resb-rsnum,
        rspos TYPE resb-rspos,
        rsart TYPE resb-rsart,
        xloek TYPE resb-xloek,
        xwaok TYPE resb-xwaok,
        kzear TYPE resb-kzear,
        matnr TYPE resb-matnr,
        werks TYPE resb-werks,
        bdmng TYPE resb-bdmng,
        enmng TYPE resb-enmng,
        meins TYPE resb-meins,
        aufnr TYPE resb-aufnr,
       END OF ty_resb.
TYPES: ty_st_resb TYPE SORTED TABLE OF ty_resb WITH UNIQUE KEY rsnum rspos rsart.
TYPES: ty_st_resb_matnr TYPE SORTED TABLE OF ty_resb WITH NON-UNIQUE KEY matnr.

TYPES: BEGIN OF ty_matnr,
        matnr TYPE mara-matnr,
        mtart TYPE mara-mtart,
        extwg TYPE mara-extwg,
       END OF ty_matnr.
TYPES: ty_ht_mara TYPE HASHED TABLE OF ty_matnr WITH UNIQUE KEY matnr.

TYPES: BEGIN OF ty_makt,
        matnr TYPE makt-matnr,
        maktx TYPE makt-maktx,
       END OF ty_makt.
TYPES: ty_ht_makt TYPE HASHED TABLE OF ty_makt WITH UNIQUE KEY matnr.

TYPES: BEGIN OF ty_t024f,
        fevor TYPE t024f-fevor,
        txt   TYPE t024f-txt,
       END OF ty_t024f.
TYPES: ty_ht_t024f TYPE HASHED TABLE OF ty_t024f WITH UNIQUE KEY fevor.

TYPES: BEGIN OF ty_kostl,
        extwg TYPE zwmmpt008-extwg,
        kostl TYPE zwmmpt008-kostl,
        fevor TYPE zwmmpt008-fevor,
       END OF ty_kostl.
TYPES: ty_ht_kostl  TYPE HASHED TABLE OF ty_kostl WITH UNIQUE KEY extwg fevor.

TYPES: BEGIN OF ty_ltak,
        lgnum TYPE ltak-lgnum,
        tanum TYPE ltak-tanum,
        kquit TYPE ltak-kquit,
        lznum TYPE ltak-lznum,
        queue TYPE ltak-queue,
       END OF ty_ltak.
TYPES: ty_ht_ltak TYPE HASHED TABLE OF ty_ltak WITH UNIQUE KEY lgnum tanum.

TYPES: BEGIN OF ty_ltap,
        lgnum TYPE ltap-lgnum,
        tanum TYPE ltap-tanum,
        tapos TYPE ltap-tapos,
        matnr TYPE ltap-matnr,
        meins TYPE ltap-meins,
        altme TYPE ltap-altme,
        pquit TYPE ltap-pquit,
        qname TYPE ltap-qname,
        vltyp TYPE ltap-vltyp,
        vlpla TYPE ltap-vlpla,
        vsolm TYPE ltap-vsolm,
        vsola TYPE ltap-vsola,
        pvqui TYPE ltap-pvqui,
       END OF ty_ltap.
TYPES: ty_st_ltap       TYPE SORTED TABLE OF ty_ltap WITH UNIQUE KEY lgnum tanum tapos.
TYPES: ty_st_ltap_matnr TYPE SORTED TABLE OF ty_ltap WITH NON-UNIQUE KEY matnr.

TYPES: BEGIN OF ty_popup,
        fevor       TYPE afko-fevor,
        aufnr       TYPE afko-aufnr,
        kostl       TYPE zwmmpt008-kostl,
        matnr       TYPE mara-matnr,
        maktx       TYPE makt-maktx,
        verme       TYPE lqua-verme,
        meins       TYPE lqua-meins,
        tpall_wm    TYPE text50,
        npall_wm    TYPE i,
        tpall_scr   TYPE text50,
        npall_scr   TYPE i,
        verme_scr   TYPE lqua-verme,
        meins_scr   TYPE lqua-meins,
        verme_final TYPE lqua-verme,
        title001    TYPE text50,
        title002    TYPE text50,
       END OF ty_popup.

TYPES: BEGIN OF ty_lqua,
        lgnum TYPE lqua-lgnum,
        lqnum TYPE lqua-lqnum,
        matnr TYPE lqua-matnr,
        werks TYPE lqua-werks,
        charg TYPE lqua-charg,
        bestq TYPE lqua-bestq,
        sobkz TYPE lqua-sobkz,
        sonum TYPE lqua-sonum,
        lgtyp TYPE lqua-lgtyp,
        lgpla TYPE lqua-lgpla,
        skzue TYPE lqua-skzue,
        skzua TYPE lqua-skzua,
        skzsi TYPE lqua-skzsi,
        wdatu TYPE lqua-wdatu,
        meins TYPE lqua-meins,
        gesme TYPE lqua-gesme,
        verme TYPE lqua-verme,
        lenum TYPE lqua-lenum,
        lgort TYPE lqua-lgort,
        chckd TYPE flag,
       END OF ty_lqua.
TYPES: ty_t_lqua  TYPE STANDARD TABLE OF ty_lqua.

TYPES: BEGIN OF ty_lqua_collect,
        matnr TYPE lqua-matnr,
        verme TYPE lqua-verme,
       END OF ty_lqua_collect.
TYPES: ty_t_lqua_collect  TYPE HASHED TABLE OF ty_lqua_collect WITH UNIQUE KEY matnr.

TYPES: BEGIN OF ty_mchb,
        matnr TYPE mchb-matnr,
        werks TYPE mchb-werks,
        lgort TYPE mchb-lgort,
        charg TYPE mchb-charg,
        lvorm TYPE mchb-lvorm,
        ersda TYPE mchb-ersda,
       END OF ty_mchb.
TYPES: ty_ht_mchb TYPE HASHED TABLE OF ty_mchb WITH UNIQUE KEY matnr werks lgort charg.

TYPES: BEGIN OF ty_popup_sel,
        chckd TYPE abap_bool,
        fevor TYPE afko-fevor,
        kostl TYPE zwmfr_sprodplan_output-kostl,
        aufnr TYPE afko-aufnr,
        matnr TYPE resb-matnr,
        lqnum TYPE lqua-lqnum,
        maktx TYPE makt-maktx,
        verme TYPE lqua-verme,
        meins TYPE lqua-meins,
        lgtyp TYPE lqua-lgtyp,
        lgpla TYPE lqua-lgpla,
        lenum TYPE lqua-lenum,
        charg TYPE lqua-charg,
        werks TYPE lqua-werks,
        lgort TYPE lqua-lgort,
        wdatu TYPE lqua-wdatu,
        color TYPE lvc_t_scol,
       END OF ty_popup_sel.

TYPES: BEGIN OF ty_duplicates,
        matnr TYPE zwmfr_sprodplan_output-matnr,
        maktx TYPE makt-maktx,
        bdmng TYPE bdmng,
        enmng TYPE enmng,
        meins TYPE meins,
        dbcnt TYPE sydbcnt,
       END OF ty_duplicates.
TYPES: ty_st_duplicates TYPE SORTED TABLE OF ty_duplicates WITH UNIQUE KEY matnr.

*** Internal Tables
DATA gt_hardcodes   TYPE SORTED TABLE OF zhardcode_table WITH UNIQUE KEY occurrence counter. "#EC NEEDED
DATA gt_aufk        TYPE ty_ht_aufk.                        "#EC NEEDED
DATA gt_afko        TYPE ty_ht_afko.                        "#EC NEEDED
DATA gt_afpo        TYPE ty_ht_afpo.                        "#EC NEEDED
DATA gt_resb        TYPE ty_st_resb.                        "#EC NEEDED
DATA gt_mara        TYPE ty_ht_mara.                        "#EC NEEDED
DATA gt_makt        TYPE ty_ht_makt.                        "#EC NEEDED
DATA gt_t024f       TYPE ty_ht_t024f.                       "#EC NEEDED
DATA gt_kostl       TYPE ty_ht_kostl.                       "#EC NEEDED
DATA gt_output      TYPE HASHED TABLE OF zwmfr_sprodplan_output WITH UNIQUE KEY nodek. "#EC NEEDED
DATA gt_output_tmp  TYPE SORTED TABLE OF zwmfr_sprodplan_output WITH UNIQUE KEY fevor kostl gstrp aufnr plnbez matnr. "#EC NEEDED
DATA gt_zwmmpt001   TYPE ty_st_zwmmpt001.                   "#EC NEEDED
DATA gt_toview      TYPE STANDARD TABLE OF zwmfr_sprodplan_output_toview. "#EC NEEDED
DATA gt_lqua        TYPE ty_t_lqua.                         "#EC NEEDED
DATA gt_lqua_coll   TYPE ty_t_lqua_collect.                 "#EC NEEDED
DATA gt_popup_lqua  TYPE STANDARD TABLE OF ty_popup_sel.    "#EC NEEDED
DATA gt_dupview     TYPE ty_st_duplicates.                  "#EC NEEDED

*** Ranges
DATA gr_lgnum TYPE RANGE OF t320-lgnum.                     "#EC NEEDED

*** Work Areas
DATA gs_xuser   TYPE lrf_wkqu.                              "#EC NEEDED
DATA gs_topopup TYPE ty_popup.                              "#EC NEEDED

*** Field Symbols

*** Variables
DATA gv_okcode       TYPE syucomm.                          "#EC NEEDED
DATA gv_okcode_popup TYPE syucomm.                          "#EC NEEDED
DATA gv_okcode_bckp  TYPE syucomm.                          "#EC NEEDED

DATA: gv_queue_dev TYPE lrf_queue.
DATA: gv_dummy_dev TYPE bwlvs.
DATA: gv_dummy_mat TYPE matnr.

*** Object Instances
DATA go_tree      TYPE REF TO cl_salv_tree.                 "#EC NEEDED
DATA go_popup     TYPE REF TO cl_salv_table.                "#EC NEEDED
DATA go_popup_to  TYPE REF TO cl_salv_table.                "#EC NEEDED

*** OLE Objects
