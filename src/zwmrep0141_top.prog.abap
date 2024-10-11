*&---------------------------------------------------------------------*
*&  Include           ZWMREP0140_TOP
*&---------------------------------------------------------------------*

DATA: BEGIN OF scr0001,
        pos         TYPE char14,
        lgtyp       TYPE lgtyp,
        lgpla       TYPE lgpla,
        lenvw	      TYPE t331_lenvw,
        lenum       TYPE lenum,
        ean11       TYPE mean-ean11,
        matnr       TYPE lqua-matnr,
        maktx_a(20) TYPE c,
        maktx_b(20) TYPE c,
        menge       TYPE menge_d,
        uni         TYPE meins,
        npos        TYPE i,
        dest        TYPE char14,
      END OF scr0001.

DATA: BEGIN OF gs_user OCCURS 0.
        INCLUDE STRUCTURE lrf_wkqu.
      DATA: END OF gs_user.

DATA: gv_lgnum        LIKE lqua-lgnum,
      gv_bwlvs        TYPE bwlvs,
      gv_text1        LIKE bdcmsgcoll-msgv1,
      gv_text2        LIKE bdcmsgcoll-msgv2,
      gv_text3        LIKE bdcmsgcoll-msgv2,
      gv_text4        LIKE bdcmsgcoll-msgv2,
      gv_setscreen1   TYPE char4,
      cursorfield(20).

DATA: gr_st_pick TYPE RANGE OF lgtyp WITH HEADER LINE.
