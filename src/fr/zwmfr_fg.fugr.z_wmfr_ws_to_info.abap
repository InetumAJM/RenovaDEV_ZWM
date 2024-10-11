FUNCTION z_wmfr_ws_to_info.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LGNUM
*"     VALUE(I_NLPLA) TYPE  LTAP_NLPLA
*"     VALUE(I_REFNR) TYPE  LVS_REFNR
*"     VALUE(I_LENUM) TYPE  LENUM
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  SYSUBRC
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
  DATA: lt_zwmfrt005 TYPE TABLE OF zwmfrt005.

  DATA: ls_zwmfrt004 TYPE zwmfrt004,
        ls_zwmfrt005 TYPE zwmfrt005.

  DATA: lv_posnr TYPE posnr.

  IF i_lgnum IS INITIAL OR
     i_nlpla IS INITIAL OR
     i_refnr IS INITIAL OR
     i_lenum IS INITIAL.
    e_return = 4.
    EXIT.
  ENDIF.


  SELECT SINGLE * FROM zwmfrt004
                  INTO ls_zwmfrt004
                  WHERE lgnum = i_lgnum AND
                        nlpla = i_nlpla AND
                        refnr = i_refnr.

  IF sy-subrc <> 0.
    ls_zwmfrt004-lgnum = i_lgnum.
    ls_zwmfrt004-nlpla = i_nlpla.
    ls_zwmfrt004-refnr = i_refnr.
    ls_zwmfrt004-original = abap_false.
    INSERT zwmfrt004 FROM ls_zwmfrt004.
  ENDIF.

  SELECT * FROM zwmfrt005
           INTO TABLE lt_zwmfrt005
           WHERE lgnum = i_lgnum AND
                 nlpla = i_nlpla.

  SORT lt_zwmfrt005 BY posnr DESCENDING.
  READ TABLE lt_zwmfrt005
        INTO ls_zwmfrt005
        INDEX 1.

  lv_posnr = ls_zwmfrt005-posnr + 1.

  CLEAR: ls_zwmfrt005.
  ls_zwmfrt005-lgnum = i_lgnum.
  ls_zwmfrt005-nlpla = i_nlpla.
  ls_zwmfrt005-refnr = i_refnr.
  ls_zwmfrt005-lenum = i_lenum.
  ls_zwmfrt005-posnr = lv_posnr.

  INSERT zwmfrt005 FROM ls_zwmfrt005.
  COMMIT WORK.

ENDFUNCTION.
