FUNCTION zwm_check_remontadas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_MATNR) TYPE  MATNR OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_UNPAIRED_STOCK) TYPE  LQUA_T
*"     REFERENCE(ET_UNPAIRED) TYPE  Z6WM_TT_ZWM020
*"----------------------------------------------------------------------

  TYPES: BEGIN OF lty_stock,
           lgtyp TYPE lgtyp,
           lgpla TYPE lgpla,
           lenum TYPE lenum,
           matnr TYPE matnr,
         END OF lty_stock.

  DATA: lt_stock  TYPE TABLE OF lty_stock,
        lt_zwm020 TYPE TABLE OF zwm020.

  DATA: ls_stock_p1 TYPE lty_stock,
        ls_stock_p2 TYPE lty_stock,
        ls_zwm020   TYPE zwm020.

  DATA: lr_matnr TYPE RANGE OF matnr.

  DATA: lv_tabix TYPE sytabix.

  DATA: ls_r_matnr LIKE LINE OF lr_matnr.

  IF NOT i_matnr IS INITIAL.
    ls_r_matnr-low = i_matnr.
    ls_r_matnr-sign = 'I'.
    ls_r_matnr-option = 'EQ'.
    APPEND ls_r_matnr TO lr_matnr.
  ENDIF.

  data(lr_letyp_remontada) =  z_wm_cl_management=>r_letyp_remontada( i_lgnum ).

  SELECT a~lgtyp a~lgpla a~lenum
         a~matnr                 FROM lqua AS a
                                 INNER JOIN mlgn AS b ON a~matnr = b~matnr AND
                                                         a~lgnum = b~lgnum
                                 INNER JOIN lagp AS c ON a~lgnum = c~lgnum AND
                                                         a~lgtyp = c~lgtyp AND
                                                         a~lgpla = c~lgpla
                                 INTO TABLE lt_stock
                                 WHERE a~lgnum = i_lgnum AND
                                       a~lgtyp IN ( 'DRI', 'BLK', 'AUT', 'TRI' ) AND
                                       a~matnr IN lr_matnr AND
                                       a~gesme > 0 AND
                                       a~verme > 0 AND
                                       b~lety1 in lr_letyp_remontada AND
                                       c~skzua = abap_false.

  CHECK NOT lt_stock IS INITIAL.
  SORT lt_stock BY lenum.

  LOOP AT lt_stock INTO ls_stock_p1.
    IF ls_stock_p1-lenum EQ ls_stock_p2-lenum.
      BREAK-POINT.
    ENDIF.

    ls_stock_p2 = ls_stock_p1.

  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM lt_stock COMPARING lenum.

  SELECT * FROM zwm020
           INTO TABLE lt_zwm020
           FOR ALL ENTRIES IN lt_stock
           WHERE armazem = i_lgnum AND
                 p1 = lt_stock-lenum.

  LOOP AT lt_zwm020 INTO ls_zwm020.
    lv_tabix = sy-tabix.

    READ TABLE lt_stock
          INTO ls_stock_p1
          WITH KEY lenum = ls_zwm020-p1
          BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    READ TABLE lt_stock
          INTO ls_stock_p2
          WITH KEY lenum = ls_zwm020-p2
          BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    CHECK ls_stock_p1-lgtyp EQ ls_stock_p2-lgtyp AND
          ls_stock_p1-lgpla EQ ls_stock_p2-lgpla.

    DELETE lt_stock WHERE lenum = ls_zwm020-p1.
    DELETE lt_stock WHERE lenum = ls_zwm020-p2.
    DELETE lt_zwm020 INDEX lv_tabix.
  ENDLOOP.

  IF NOT lt_stock IS INITIAL.
    SELECT * FROM lqua
             INTO TABLE et_unpaired_stock
             FOR ALL ENTRIES IN lt_stock
             WHERE lgnum = i_lgnum AND
                   lenum = lt_stock-lenum.
  ENDIF.

  et_unpaired = lt_zwm020.




ENDFUNCTION.
