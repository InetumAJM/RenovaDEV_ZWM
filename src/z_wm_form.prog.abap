*&---------------------------------------------------------------------*
*& Report  Z_WM_FORM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_wm_form.

*&---------------------------------------------------------------------*
*&      Form  get_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IN_PAR     text
*      -->OUT_PAR    text
*----------------------------------------------------------------------*
FORM get_vendor TABLES in_par STRUCTURE itcsy
out_par STRUCTURE itcsy.

  DATA: mblnr TYPE ltak-mblnr,
        wdatu TYPE ltap-wdatu,
        wa_mkpf TYPE mkpf,
        wa_mseg TYPE mseg,
        wa_lfa1 TYPE lfa1.

*read Number of Material Document
  READ TABLE in_par WITH KEY 'MBLNR'.
  CHECK sy-subrc = 0.
  mblnr = in_par-value.

*read Date of Goods Receipt
  READ TABLE in_par WITH KEY 'WDATU'.
  CHECK sy-subrc = 0.
  wdatu = in_par-value.

*get vendor number and item text
  SELECT SINGLE * FROM mseg INTO wa_mseg
          WHERE mblnr = mblnr AND
                mjahr = wdatu+0(4).

*get Document Header Text
  SELECT SINGLE * FROM mkpf INTO wa_mkpf
            WHERE mblnr = mblnr AND
                  mjahr = wdatu+0(4).

*get vendor name
  SELECT SINGLE * FROM lfa1 INTO wa_lfa1
          WHERE lifnr  = wa_mseg-lifnr.


  READ TABLE out_par WITH KEY 'NUMBER'.
  CHECK sy-subrc = 0.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = wa_mseg-lifnr
    IMPORTING
      output = wa_mseg-lifnr.

  out_par-value = wa_mseg-lifnr.
  MODIFY out_par INDEX sy-tabix.


  READ TABLE out_par WITH KEY 'NAME'.
  CHECK sy-subrc = 0.

  out_par-value = wa_lfa1-name1.
  MODIFY out_par INDEX sy-tabix.

*set Item Text
  READ TABLE out_par WITH KEY 'SGTXT'.
  CHECK sy-subrc = 0.

  out_par-value = wa_mseg-sgtxt.
  CONDENSE out_par-value.
  MODIFY out_par INDEX sy-tabix.

*set Document Header Text
  READ TABLE out_par WITH KEY 'BKTXT'.
  CHECK sy-subrc = 0.

  out_par-value = wa_mkpf-bktxt.
  CONDENSE out_par-value.
  MODIFY out_par INDEX sy-tabix.

*set Delivery number
  READ TABLE out_par WITH KEY 'VBELN'.
  CHECK sy-subrc = 0.

  out_par-value = wa_mseg-xblnr_mkpf.
  CONDENSE out_par-value.
  MODIFY out_par INDEX sy-tabix.

  READ TABLE out_par WITH KEY 'BUDAT'.
  CHECK sy-subrc = 0.

  out_par-value = wa_mkpf-budat.
  CONDENSE out_par-value.
  MODIFY out_par INDEX sy-tabix.

ENDFORM.                    "get_vendor



*&---------------------------------------------------------------------*
*&      Form  get_destination
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IN_PAR     text
*      -->OUT_PAR    text
*----------------------------------------------------------------------*
FORM get_destination TABLES in_par STRUCTURE itcsy
out_par STRUCTURE itcsy.

  DATA: tanum   TYPE ltap-tanum,
        lgnum   TYPE ltap-lgnum,
        tapos   TYPE ltap-tapos,
        wa_ltap TYPE ltap.

*read Transfer Order Number
  READ TABLE in_par WITH KEY 'TANUM'.
  CHECK sy-subrc = 0.
  tanum = in_par-value.

  READ TABLE in_par WITH KEY 'TAPOS'.
  IF sy-subrc EQ 0.
    tapos = in_par-value.
  ENDIF.

*read Whare
  READ TABLE in_par WITH KEY 'LGNUM'.
  IF sy-subrc EQ 0.
    lgnum = in_par-value.
  ELSE.
    lgnum = '200'. "Matener Programas Antigos
  ENDIF.

*get Destination storage unit number
  IF tapos IS INITIAL.
    SELECT SINGLE * FROM ltap INTO wa_ltap
            WHERE lgnum = lgnum AND
                  tanum = tanum.
  ELSE.
    SELECT SINGLE * FROM ltap INTO wa_ltap
            WHERE lgnum = lgnum AND
                  tanum = tanum AND
                  tapos = tapos.
  ENDIF.


  READ TABLE out_par WITH KEY 'NLENR'.
  CHECK sy-subrc = 0.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = wa_ltap-nlenr
    IMPORTING
      output = wa_ltap-nlenr.

  out_par-value = wa_ltap-nlenr.
  MODIFY out_par INDEX sy-tabix.
  " - paulo sousa - 2016.05.27 -
  DATA: nlenr2 LIKE wa_ltap-nlenr.
  nlenr2 = wa_ltap-nlenr.
  READ TABLE out_par WITH KEY 'NLENR2'.
  IF sy-subrc EQ 0.
    out_par-value = wa_ltap-nlenr.
    MODIFY out_par INDEX sy-tabix.
  ENDIF.

  READ TABLE out_par WITH KEY 'ZEUGN'.
  IF sy-subrc EQ 0.
    out_par-value = wa_ltap-zeugn.
    MODIFY out_par INDEX sy-tabix.
  ENDIF.





ENDFORM.                    "get_destination



*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IN_PAR     text
*      -->OUT_PAR    text
*----------------------------------------------------------------------*
FORM remove_leading_zeros TABLES in_par STRUCTURE itcsy
out_par STRUCTURE itcsy.

  DATA: tanum TYPE ltap-tanum,
        wa_ltap TYPE ltap.

  READ TABLE in_par WITH KEY 'NLENR'.
  CHECK sy-subrc = 0.


  READ TABLE out_par WITH KEY 'NLENR'.
  CHECK sy-subrc = 0.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = in_par-value
    IMPORTING
      output = out_par-value.
  MODIFY out_par INDEX sy-tabix.

ENDFORM.                    "CONVERSION_EXIT_ALPHA_OUTPUT
