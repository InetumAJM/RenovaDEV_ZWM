TYPE-POOL zrf01.



*--> Items de FM Z_WM_RF_OPTION_SELECT
TYPES: BEGIN OF zrf01_option_select_items,
         op_key  TYPE char20,
         op_text TYPE char40,
         op_pick TYPE flag,
       END OF zrf01_option_select_items.

TYPES: zrf01_t_option_select_items TYPE TABLE OF zrf01_option_select_items.


*--> Items de FM Z_WM_RF_OPTION_PICKER
TYPES: BEGIN OF zrf01_option_picker_items,
         op_key  TYPE char20,
         op_text TYPE char40,
         op_pick TYPE flag,
       END OF zrf01_option_picker_items.

TYPES: zrf01_t_option_picker_items TYPE TABLE OF zrf01_option_picker_items.

*--> Items ZWMMP_QM_NOTE_CREATE
TYPES: BEGIN OF zrf01_note_create,
         code_group	     TYPE qmgrp,
         code            TYPE qmcod,
         short_text	     TYPE qmtxt,
         material_plant  TYPE werks,
         material        TYPE matnr,
         batch           TYPE charg_d,
         stor_loc_batch  TYPE lgort_d,
         po_number       TYPE ebeln,
         po_item         TYPE ebelp,
         doc_year        TYPE mjahr,
         mat_doc         TYPE mblnr,
         mat_doc_item    TYPE mblpo,
         vend_no         TYPE lifnr,
         manufacturer    TYPE lifnr,
         quant_complaint TYPE erfmg,
         qty_unit        TYPE erfme,

         menge_sscc      TYPE menge_d,

         ref_mjahr       TYPE mjahr,
         ref_mblnr       TYPE mblnr,
         ref_mblpo       TYPE mblpo,
       END OF zrf01_note_create.

TYPES: zrf01_t_note_create TYPE TABLE OF zrf01_note_create.


***********************************************************************
** ZWM_HU_TRANSFER
***********************************************************************
TYPES: BEGIN OF zrf01_hu_transfer,
         vepos_o    TYPE vepos,
         matnr      TYPE matnr,
         charg      TYPE charg_d,
         werks      TYPE werks_d,
         lgort      TYPE lgort_d,
         venum_d    TYPE venum,
         vepos_d    TYPE vepos,
         exidv_d    TYPE exidv,
         vhilm      TYPE vhilm,
         vemng      TYPE vemng,
         vemeh      TYPE vemeh,
         setqt      TYPE flag,
       END OF zrf01_hu_transfer.

TYPES: zrf01_t_hu_transfer TYPE TABLE OF zrf01_hu_transfer.

TYPES: zrf01_tt_exidv TYPE TABLE OF exidv.

TYPES: zrf01_t_bapihuitmproposal TYPE TABLE OF bapihuitmproposal.
