class ZCL_IM_WM_SHP_DEL_PROC definition
  public
  final
  create public .

*"* public components of class ZCL_IM_WM_SHP_DEL_PROC
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_DELIVERY_PROC .
protected section.
*"* protected components of class ZCL_IM_WM_SHP_DEL_PROC
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_WM_SHP_DEL_PROC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_WM_SHP_DEL_PROC IMPLEMENTATION.


method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_HEADER.
endmethod.


METHOD if_ex_le_shp_delivery_proc~change_delivery_item.


  DATA: lt_messages TYPE tab_bdcmsgcoll.

  DATA: ls_message  TYPE bdcmsgcoll,
        ls_log      TYPE shp_badi_error_log,
        ls_lips     TYPE lips,
        ls_vbup     TYPE vbup,
        ls_xvbup    TYPE vbupvb.

  FIELD-SYMBOLS: <lv_fcode> TYPE c.

** Valida Se é Fornecimento
***********************************************************************
  DO 1 TIMES.
    ASSIGN ('(SAPFV50P)OLD_FCODE') TO <lv_fcode>.
    CHECK <lv_fcode> IS ASSIGNED.


  ENDDO.

** Valida Remessa
***********************************************************************
  CALL FUNCTION 'ZWM_VALIDATE_DELIVERY_DELETE'
    EXPORTING
      i_vbeln     = cs_lips-vbeln
    IMPORTING
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.



    READ TABLE lt_messages INTO ls_message INDEX 1.

    IF ls_message-msgtyp EQ 'E'.
      cf_flag_delete_item = 'X'.
    ENDIF.


    MESSAGE ID ls_message-msgid TYPE 'I' NUMBER ls_message-msgnr
                                DISPLAY LIKE ls_message-msgtyp
          WITH ls_message-msgv1 ls_message-msgv2
               ls_message-msgv3 ls_message-msgv4.

    ls_log-vbeln = cs_lips-vbeln.
    ls_log-posnr = cs_lips-posnr.
    ls_log-msgty = ls_message-msgtyp.
    ls_log-msgid = ls_message-msgid.
    ls_log-msgno = ls_message-msgnr.
    ls_log-msgv1 = ls_message-msgv1.
    ls_log-msgv2 = ls_message-msgv2.
    ls_log-msgv3 = ls_message-msgv3.
    ls_log-msgv4 = ls_message-msgv4.
    APPEND ls_log TO ct_log.
    CLEAR  ls_log.
  ENDIF.

ENDMETHOD.


METHOD if_ex_le_shp_delivery_proc~change_fcode_attributes.
  DATA: ls_cua_exclude TYPE shp_cua_exclude.

  CALL FUNCTION 'ZWM_VALIDATE_DELIVERY_DELETE'
    EXPORTING
      i_vbeln = is_likp-vbeln
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.

  CHECK sy-subrc <> 0.

** Exclui a Opção de Eliminação de Remessa
  ls_cua_exclude-function = 'LOES_T'.
  APPEND ls_cua_exclude TO ct_cua_exclude.


ENDMETHOD.


METHOD if_ex_le_shp_delivery_proc~change_field_attributes.

  DATA: ls_xvbuk TYPE vbukvb.

  DATA: ls_field_attributes TYPE shp_screen_attributes.

  DO 1 TIMES.
    CHECK is_likp-vkorg EQ 'RP12'.

    READ TABLE it_xvbuk INTO ls_xvbuk WITH KEY vbeln = is_likp-vbeln.
    CHECK sy-subrc EQ 0.
    CHECK ls_xvbuk-wbstk EQ 'A'.

    CLEAR: ls_field_attributes.
    ls_field_attributes-name = 'LIPSD-G_LFIMG'.
    ls_field_attributes-active = 1.
    ls_field_attributes-input = 1.
    APPEND ls_field_attributes TO ct_field_attributes.

    CLEAR: ls_field_attributes.
    ls_field_attributes-name = 'LIPS-LFIMG'.
    ls_field_attributes-active = 1.
    ls_field_attributes-input = 1.
    APPEND ls_field_attributes TO ct_field_attributes.
  ENDDO.

ENDMETHOD.


METHOD if_ex_le_shp_delivery_proc~check_item_deletion.


  DATA: lt_messages TYPE tab_bdcmsgcoll.

  DATA: ls_message  TYPE bdcmsgcoll,
        ls_log      TYPE shp_badi_error_log,
        ls_lips     TYPE lips,
        ls_vbup     TYPE vbup,
        ls_xvbup    TYPE vbupvb.

  CALL FUNCTION 'ZWM_VALIDATE_DELIVERY_DELETE'
    EXPORTING
      i_vbeln     = is_likp-vbeln
    IMPORTING
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.

    READ TABLE lt_messages INTO ls_message INDEX 1.

    IF ls_message-msgtyp EQ 'E'.
      cf_item_not_deletable = 'X'.
    ENDIF.


    MESSAGE ID ls_message-msgid TYPE 'I' NUMBER ls_message-msgnr
                                DISPLAY LIKE ls_message-msgtyp
          WITH ls_message-msgv1 ls_message-msgv2
               ls_message-msgv3 ls_message-msgv4.

    ls_log-vbeln = is_xlips-vbeln.
    ls_log-posnr = is_xlips-posnr.
    ls_log-msgty = ls_message-msgtyp.
    ls_log-msgid = ls_message-msgid.
    ls_log-msgno = ls_message-msgnr.
    ls_log-msgv1 = ls_message-msgv1.
    ls_log-msgv2 = ls_message-msgv2.
    ls_log-msgv3 = ls_message-msgv3.
    ls_log-msgv4 = ls_message-msgv4.
    APPEND ls_log TO ct_log.
    CLEAR  ls_log.
  ENDIF.



ENDMETHOD.


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


method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_BEFORE_OUTPUT.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_DOCUMENT.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_DOCUMENT_PREPARE.
endmethod.
ENDCLASS.
