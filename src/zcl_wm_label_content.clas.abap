class ZCL_WM_LABEL_CONTENT definition
  public
  final
  create public .

public section.

  types:
    ty_itcsy TYPE STANDARD TABLE OF itcsy .

*  types:
*    BEGIN OF ty_linha,
*        maktx  TYPE c LENGTH 40,  "char40
*        vemng  TYPE c LENGTH 04,  "char4
*        vemeh  TYPE c LENGTH 03,  "char03
*        charg  TYPE c LENGTH 10,  "char10
*        vbeln  TYPE c LENGTH 10,  "char10
*        pedido TYPE c LENGTH 10,  "char10
*      END OF ty_linha .
*  types:
**    TYPES:
**      ty_linhas TYPE STANDARD TABLE OF ty_linha WITH EMPTY KEY .
*    BEGIN OF ty_head,
*        we_name2          TYPE adrc-name2,
*        we_street         TYPE adrc-street,
*        we_post_code1     TYPE adrc-post_code1,
*        we_city1          TYPE adrc-city1,
*        we_numguia        TYPE likp-vbeln,
*        delivery_date(10) TYPE c,
*        we_numorder       TYPE vbak-bstnk,
*        ag_name1          TYPE adrc-name1,
*        notas(80)         TYPE c,
*      END OF ty_head .
*  types:
*    BEGIN OF ty_label_content,
*             header TYPE ty_head,
*             linhas TYPE STANDARD TABLE OF ty_linha WITH EMPTY KEY,
*           END OF ty_label_content .
  class-methods GET_PREPARE_LABEL_CONTENT
    importing
      !IV_SSCC type EXIDV
      !GET_ENTREGA_PICKING type BKK_YESNO default 'X'
      !GET_CONTEUDO type BKK_YESNO default 'X'
    exporting
      !ET_ITCSY type TY_ITCSY
    returning
      value(RS_LABEL_CONTENT) type ZWM_S008 .
  class-methods WS_UPD_LABEL_DISPLAY
    importing
      !SSCC type EXIDV .
protected section.
private section.

  class-methods CONVERSION_EXIT_ALPHA_INPUT
    importing
      !IV_SSCC type EXIDV
    returning
      value(RV_SSCC) type EXIDV .
  class-methods GET_LABEL_CONTENT
    importing
      !IV_SSCC type EXIDV
      !GET_ENTREGA_PICKING type BKK_YESNO default 'X'
      !GET_CONTEUDO type BKK_YESNO default 'X'
    exporting
      !RT_VALUES type TY_ITCSY .
  class-methods SEND_WS_MESSAGE
    importing
      !MENSAGEM type ANY .
ENDCLASS.



CLASS ZCL_WM_LABEL_CONTENT IMPLEMENTATION.


METHOD conversion_exit_alpha_input.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = iv_sscc
    IMPORTING
      output = rv_sscc.

ENDMETHOD.


METHOD get_label_content.

  CHECK iv_sscc IS NOT INITIAL.

  DATA(lv_exidv) = conversion_exit_alpha_input( iv_sscc ).

*FORM get_conteudo TABLES in_par STRUCTURE itcsy
*                         out_par STRUCTURE itcsy.
  DATA: lt_itcsy_in  TYPE ty_itcsy,
        lt_itcsy_out TYPE ty_itcsy.
*NAME 1 Tipo  TDTPRGNAME  CHAR  130 0 Nome de um símbolo de programa (sem &)
*VALUE  1 Tipo  TDSYMVALUE  CHAR  255 0 Valor de um símbolo de texto

  APPEND VALUE #( name = 'EAN128-EXIDV' value = lv_exidv ) TO lt_itcsy_in.

  PERFORM clear IN PROGRAM zwmrep0029.

  IF get_conteudo EQ abap_true.
    lt_itcsy_out = VALUE #(
      "get_conteudo
                                ( name = 'LINHA1' )
                                ( name = 'LINHA2' )
                                ( name = 'LINHA3' )
                                ( name = 'LINHA4' )
                                ( name = 'LINHA5' )
                                ( name = 'LINHA6' )
                                ( name = 'LINHA7' )
                                ( name = 'LINHA8' )
                                ( name = 'LINHA9' )
                                ( name = 'LINHA10' )
                                ( name = 'LINHA11' )
                                ( name = 'LINHA12' )
                                ( name = 'LINHA13' )
                                ( name = 'LINHA14' )
                                ( name = 'LINHA15' )
                                ( name = 'LINHA16' )
                                ( name = 'LINHA17' )
                                ( name = 'LINHA18' )
                                ( name = 'LINHA19' )
                                ( name = 'LINHA20' )
                                ( name = 'LINHA21' )
                                ( name = 'LINHA22' )
                                ( name = 'LINHA23' )
                                ( name = 'TIPOPALETE' )
                                ( name = 'ALLOTIE' )
                                ( name = 'TOTALBOXES' )
                                ).
    "get_entrega_picking
*                                ( name = 'WE_NAME2' )
*                                ( name = 'WE_STREET' )
*                                ( name = 'WE_POST_CODE1' )
*                                ( name = 'WE_CITY1' )
*                                ( name = 'WE_NUMGUIA' )
*                                ( name = 'DELIVERY_DATE' )
*                                ( name = 'WE_NUMORDER' )
*                                ( name = 'AG_NAME1' )
*                                ( name = 'NOTAS' )
*                                ).

    PERFORM get_conteudo IN PROGRAM zwmrep0029 TABLES lt_itcsy_in
                                                      lt_itcsy_out.
  ENDIF.

  IF get_entrega_picking EQ abap_true.
    lt_itcsy_out = VALUE #( BASE lt_itcsy_out
*    get_entrega_picking
                                ( name = 'WE_NAME2' )
                                ( name = 'WE_STREET' )
                                ( name = 'WE_POST_CODE1' )
                                ( name = 'WE_CITY1' )
                                ( name = 'WE_NUMGUIA' )
                                ( name = 'DELIVERY_DATE' )
                                ( name = 'WE_NUMORDER' )
                                ( name = 'AG_NAME1' )
                                ( name = 'NOTAS' )
                                ).

    PERFORM get_entrega_picking IN PROGRAM zwmrep0029 TABLES lt_itcsy_in
                                                             lt_itcsy_out.
  ENDIF.

  rt_values = lt_itcsy_out.

ENDMETHOD.


METHOD get_prepare_label_content.

  CHECK iv_sscc IS NOT INITIAL.

  DATA(lv_exidv) = conversion_exit_alpha_input( iv_sscc ).

  get_label_content(
    EXPORTING
      iv_sscc             = lv_exidv     " Identificador externo da unidade comercial
      get_conteudo        = get_conteudo
      get_entrega_picking = get_entrega_picking
  IMPORTING
    rt_values = et_itcsy ).

  DATA lv_cont TYPE numc2.

  LOOP AT et_itcsy INTO DATA(ls_itcsy).
    IF ls_itcsy-name(5) = 'LINHA'.
      ADD 1 TO lv_cont.
      IF ls_itcsy-value IS NOT INITIAL.
        APPEND ls_itcsy-value TO rs_label_content-linhas
          ASSIGNING FIELD-SYMBOL(<lfs_linha>).
        <lfs_linha>-cont = lv_cont.
        <lfs_linha>-sscc = lv_exidv.
      ENDIF.
    ELSE.
      ASSIGN COMPONENT ls_itcsy-name
        OF STRUCTURE rs_label_content
          TO FIELD-SYMBOL(<lfv_value>).
      IF sy-subrc IS NOT INITIAL OR <lfv_value> IS NOT ASSIGNED.
        ASSIGN COMPONENT ls_itcsy-name
          OF STRUCTURE rs_label_content-header
            TO <lfv_value>.
      ENDIF.
      IF sy-subrc IS INITIAL AND <lfv_value> IS ASSIGNED.
        <lfv_value> = ls_itcsy-value.
      ENDIF.
      UNASSIGN <lfv_value>.
    ENDIF.
  ENDLOOP.

  IF rs_label_content IS NOT INITIAL.
    rs_label_content-sscc = lv_exidv.
  ENDIF.

ENDMETHOD.


METHOD send_ws_message.

  zcl_wm_gestao_cais_de_carga=>set_enh_pretty_name_active( abap_true ).
  DATA(lv_json) = /ui2/cl_json=>serialize(
    EXPORTING
      data             = mensagem
      compress         = abap_true
      numc_as_string   = abap_true
      pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      conversion_exits = abap_true ).
  zcl_wm_gestao_cais_de_carga=>set_enh_pretty_name_active( abap_false ).

  REPLACE ALL OCCURRENCES OF '":[' IN lv_json WITH '":{"results":['.
  REPLACE ALL OCCURRENCES OF ']' IN lv_json WITH ']}'.

*  DATA(lv_dest_fiori) = |FIDCLNT030_RFC|.
*  DATA(lv_dest_fiori) = |FIDCLNT030_WS|.
  DATA(lv_dest_fiori) = zcl_wm_gestao_cais_de_carga=>get_dest_name( ).
  DATA lv_msg TYPE string.
  CALL FUNCTION 'ZWM_WS_UPD_LABEL_CONTENT'
    DESTINATION lv_dest_fiori
    EXPORTING
      iv_msg                = lv_json
    EXCEPTIONS
      communication_failure = 90 "MESSAGE lv_msg
      system_failure        = 92 "MESSAGE lv_msg
      OTHERS                = 99.

ENDMETHOD.


METHOD ws_upd_label_display.

  get_prepare_label_content(
    EXPORTING
      iv_sscc             = sscc
*    get_entrega_picking = 'X'    " Campo Sim/Não
*    get_conteudo        = 'X'    " Campo Sim/Não
*  IMPORTING
*    et_itcsy            =
    RECEIVING
      rs_label_content    = DATA(ls_label_content)
  ).

  send_ws_message( ls_label_content ).

ENDMETHOD.
ENDCLASS.
