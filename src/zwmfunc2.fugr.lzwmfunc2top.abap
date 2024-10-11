FUNCTION-POOL zwmfunc2 MESSAGE-ID zwmmsg001.

** Types
***********************************************************************
TYPE-POOLS: zrf01.
TYPE-POOLS: abap.
TYPE-POOLS: truxs.

TABLES: t100.
DATA: msg1 LIKE sprot-line,
      msg2 LIKE sprot-line,
      msg3 LIKE sprot-line,
      msg4 LIKE sprot-line,
      msg5 LIKE sprot-line,
      msg6 LIKE sprot-line,
      msg7 LIKE sprot-line.

DATA: line_out(255).
FIELD-SYMBOLS: <mess_field>.

DATA: field_name      LIKE  dd03d-fieldname,
      message_length  TYPE i,
*        con_x              TYPE c   VALUE 'X',
      line_count      TYPE i VALUE 1,
      line_count_c(2) TYPE c,
      mess_pos        TYPE i VALUE 0,
      line_pos        TYPE i,
      from_pos        TYPE i,
      length          TYPE i,
      test_char(1)    TYPE c,
      i_line_size     TYPE i VALUE 0,
      i_lines         TYPE i VALUE 0,
      line_offset     TYPE i.

DATA: return_value.

TABLES: zwm001.

DATA : BEGIN OF ti_zwm001 OCCURS 0.
         INCLUDE STRUCTURE zwm001.
       DATA: END OF ti_zwm001.
       INCLUDE rlmobinc.

       DATA: matnr_pal  TYPE matnr,
       matnr_mpal TYPE matnr.

DATA: ls_mara_mpal TYPE mara,
      ls_mara_pal  TYPE mara.
DATA: gt_zwm071 TYPE STANDARD TABLE OF zwm071.

** Variáveis
DATA : retcode,
       ok_code1 LIKE sy-ucomm,
       ok_code2 LIKE sy-ucomm,
       ok_code3 LIKE sy-ucomm,
       ok_code4 LIKE sy-ucomm.


** OPTION_SELECT
***********************************************************************
TYPES:BEGIN OF gty_scr0005,
        text1  TYPE char20,
        text2  TYPE char20,
        text3  TYPE char20,
        text4  TYPE char20,
        text5  TYPE char20,
        text6  TYPE char20,
        text7  TYPE char20,
        nu1    TYPE char20,
        te11   TYPE char20,
        te12   TYPE char20,
        vl1    TYPE char20,
        pk1    TYPE char1,
        nu2    TYPE char20,
        te21   TYPE char20,
        te22   TYPE char20,
        vl2    TYPE char20,
        pk2    TYPE char1,
        nu3    TYPE char20,
        te31   TYPE char20,
        te32   TYPE char20,
        vl3    TYPE char20,
        pk3    TYPE char1,
        nu4    TYPE char20,
        te41   TYPE char20,
        te42   TYPE char20,
        vl4    TYPE char20,
        pk4    TYPE char1,
        nu5    TYPE char20,
        te51   TYPE char20,
        te52   TYPE char20,
        vl5    TYPE char20,
        pk5    TYPE char1,
        nu6    TYPE char20,
        te61   TYPE char20,
        te62   TYPE char20,
        vl6    TYPE char20,
        pk6    TYPE char1,
        nu7    TYPE char20,
        te71   TYPE char20,
        te72   TYPE char20,
        vl7    TYPE char20,
        pk7    TYPE char1,
        nu8    TYPE char20,
        te81   TYPE char20,
        te82   TYPE char20,
        vl8    TYPE char20,
        pk8    TYPE char1,
        nu9    TYPE char20,
        te91   TYPE char20,
        te92   TYPE char20,
        vl9    TYPE char20,
        pk9    TYPE char1,
        nu10   TYPE char20,
        te101  TYPE char20,
        te102  TYPE char20,
        vl10   TYPE char20,
        pk10   TYPE char1,
        okcode TYPE syucomm,
        page_a TYPE char2,
        page_t TYPE char2,
        opc    TYPE char20,
*        text1  TYPE char20,
*        text2  TYPE char20,
*        text3  TYPE char20,
*        text4  TYPE char20,
*        text5  TYPE char20,
*        text6  TYPE char20,
*        text7  TYPE char20,
*        nu1    TYPE char20,
*        te11   TYPE char20,
*        te12   TYPE char20,
*        vl1    TYPE char20,
*        nu2    TYPE char20,
*        te21   TYPE char20,
*        te22   TYPE char20,
*        vl2    TYPE char20,
*        nu3    TYPE char20,
*        te31   TYPE char20,
*        te32   TYPE char20,
*        vl3    TYPE char20,
*        nu4    TYPE char20,
*        te41   TYPE char20,
*        te42   TYPE char20,
*        vl4    TYPE char20,
*        nu5    TYPE char20,
*        te51   TYPE char20,
*        te52   TYPE char20,
*        vl5    TYPE char20,
*        okcode TYPE syucomm,
*        page_a TYPE char2,
*        page_t TYPE char2,
*        opc    TYPE char20,
      END OF gty_scr0005.

DATA: scr0005 TYPE gty_scr0005.

CONSTANTS: gc_0005_total_items TYPE i VALUE 10.

DATA: gt_0005_items          TYPE zrf01_t_option_select_items.

DATA: gv_0005_text1(20)   TYPE c,
      gv_0005_text2(20)   TYPE c,
      gv_0005_text3(20)   TYPE c,
      gv_0005_text4(20)   TYPE c,
      gv_0005_text5(20)   TYPE c,
      gv_0005_text6(20)   TYPE c,
      gv_0005_text7(20)   TYPE c,
      gv_0005_reseted     TYPE flag,
      gv_0005_right_align TYPE flag,
      gv_0005_required    TYPE flag,
      gv_total_items      TYPE i,
      gv_0005_text_length TYPE i VALUE 20.

FIELD-SYMBOLS: <gv_0005_selected_key>  TYPE any,
               <gv_0005_selected_text> TYPE any,
               <gv_0005_selected_line> TYPE i,
               <gs_0005_item>          TYPE zrf01_option_select_items.



** OPTION_PICKER
***********************************************************************
*TYPES:BEGIN OF gty_scr0010,
*        text1  TYPE char20,
*        text2  TYPE char20,
*        text3  TYPE char20,
*        text4  TYPE char20,
*        text5  TYPE char20,
*        text6  TYPE char20,
*        text7  TYPE char20,
*        nu1    TYPE char20,
*        te11   TYPE char20,
*        te12   TYPE char20,
*        vl1    TYPE char20,
*        pk1    TYPE char1,
*        nu2    TYPE char20,
*        te21   TYPE char20,
*        te22   TYPE char20,
*        vl2    TYPE char20,
*        pk2    TYPE char1,
*        nu3    TYPE char20,
*        te31   TYPE char20,
*        te32   TYPE char20,
*        vl3    TYPE char20,
*        pk3    TYPE char1,
*        nu4    TYPE char20,
*        te41   TYPE char20,
*        te42   TYPE char20,
*        vl4    TYPE char20,
*        pk4    TYPE char1,
*        nu5    TYPE char20,
*        te51   TYPE char20,
*        te52   TYPE char20,
*        vl5    TYPE char20,
*        pk5    TYPE char1,
*        nu6    TYPE char20,
*        te61   TYPE char20,
*        te62   TYPE char20,
*        vl6    TYPE char20,
*        pk6    TYPE char1,
*        nu7    TYPE char20,
*        te71   TYPE char20,
*        te72   TYPE char20,
*        vl7    TYPE char20,
*        pk7    TYPE char1,
*        nu8    TYPE char20,
*        te81   TYPE char20,
*        te82   TYPE char20,
*        vl8    TYPE char20,
*        pk8    TYPE char1,
*        nu9    TYPE char20,
*        te91   TYPE char20,
*        te92   TYPE char20,
*        vl9    TYPE char20,
*        pk9    TYPE char1,
*        nu10   TYPE char20,
*        te101  TYPE char20,
*        te102  TYPE char20,
*        vl10   TYPE char20,
*        pk10   TYPE char1,
*        okcode TYPE syucomm,
*        page_a TYPE char2,
*        page_t TYPE char2,
*        opc    TYPE char20,
*      END OF gty_scr0010.
*
*DATA: scr0010 TYPE gty_scr0010.
*
*CONSTANTS: gc_0010_total_items TYPE i VALUE 10.
*
*DATA: gt_0010_items  TYPE zrf01_option_picker_items.
*
*DATA: gv_00_text1(20)   TYPE c,
*      gv_0005_text2(20)   TYPE c,
*      gv_0005_text3(20)   TYPE c,
*      gv_0005_text4(20)   TYPE c,
*      gv_0005_text5(20)   TYPE c,
*      gv_0005_text6(20)   TYPE c,
*      gv_0005_text7(20)   TYPE c,
*      gv_0005_reseted     TYPE flag,
*      gv_0005_right_align TYPE flag,
*      gv_0005_required    TYPE flag,
*      gv_total_items      TYPE i,
*      gv_0005_text_length TYPE i VALUE 20.
