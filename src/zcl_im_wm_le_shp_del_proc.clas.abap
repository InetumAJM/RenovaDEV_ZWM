class ZCL_IM_WM_LE_SHP_DEL_PROC definition
  public
  final
  create public .

*"* public components of class ZCL_IM_WM_LE_SHP_DEL_PROC
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_DELIVERY_PROC .
protected section.
*"* protected components of class ZCL_IM_WM_LE_SHP_DEL_PROC
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_WM_LE_SHP_DEL_PROC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_WM_LE_SHP_DEL_PROC IMPLEMENTATION.


method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_HEADER.
  DATA: zs_ylikp TYPE likp,
        zs_xlips type lips.

  READ TABLE it_ylikp INTO zs_ylikp INDEX 1.

  READ TABLE it_xlips with key updkz = 'I' into zs_xlips.

 if cs_likp-lfdat is initial.
    cs_likp-lfdat = zs_ylikp-lfdat.
 else.
    if not zs_xlips is initial.
      IF NOT zs_ylikp-lfdat IS INITIAL.
       cs_likp-lfdat = zs_ylikp-lfdat.
      endif.
    endif.
 endif.

 if cs_likp-lfuhr is initial.
    cs_likp-lfuhr = zs_ylikp-lfuhr.
 else.
    if not zs_xlips is initial.
      IF NOT zs_ylikp-lfuhr IS INITIAL.
       cs_likp-lfuhr = zs_ylikp-lfuhr.
      endif.
    endif.
 endif.

endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_ITEM.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FCODE_ATTRIBUTES.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FIELD_ATTRIBUTES.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~CHECK_ITEM_DELETION.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_DELETION.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_FINAL_CHECK.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~DOCUMENT_NUMBER_PUBLISH.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_HEADER.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_ITEM.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~INITIALIZE_DELIVERY.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~ITEM_DELETION.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~PUBLISH_DELIVERY_ITEM.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~READ_DELIVERY.
endmethod.


METHOD if_ex_le_shp_delivery_proc~save_and_publish_before_output.

ENDMETHOD.


method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_DOCUMENT.
endmethod.


METHOD if_ex_le_shp_delivery_proc~save_document_prepare.

  DATA: zwa_xlikp TYPE likp.

  FIELD-SYMBOLS <wa_xlikp> TYPE likpvb.

  READ TABLE ct_xlikp ASSIGNING <wa_xlikp> INDEX 1.

  IF <wa_xlikp>-lfdat IS INITIAL.
    IF sy-batch NE 'X'.
      MESSAGE e000(zwm001) WITH 'Data Descarga Incorreta'.
    ELSE.
      <wa_xlikp>-lfdat = sy-datum.
    ENDIF.
  ENDIF.

  IF <wa_xlikp>-lfuhr IS INITIAL.
    IF sy-batch NE 'X'.
      MESSAGE e000(zwm001) WITH 'Hora Descarga Incorreta'.
    ELSE.
      <wa_xlikp>-lfuhr = sy-uzeit.
    ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.
