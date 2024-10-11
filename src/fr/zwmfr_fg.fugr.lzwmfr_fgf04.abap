*----------------------------------------------------------------------*
***INCLUDE LZWMFR_FGF04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IS_DETAILS text
*----------------------------------------------------------------------*
FORM f_send_mail USING is_details TYPE zwmmpt004.
  DATA lv_uni_str(3).
  DATA lv_qtd_str(8).
  DATA lv_matnr_str(10).
  DATA lv_data_frm(10).
  DATA lv_hora_frm(10).
  DATA lv_title TYPE so_obj_des.

  DATA ls_docdata TYPE sodocchgi1.

  DATA lt_messages_sap  TYPE STANDARD TABLE OF solisti1.
  DATA lt_receiver      TYPE STANDARD TABLE OF somlreci1.

  FIELD-SYMBOLS <fs_messages_sap> LIKE LINE OF lt_messages_sap[].
  FIELD-SYMBOLS <fs_receiver>     LIKE LINE OF lt_receiver[].

  APPEND INITIAL LINE TO lt_messages_sap[] ASSIGNING <fs_messages_sap>.
  CONCATENATE '<B>Erro Consumo para SSCC: </b> '(003) is_details-lenum INTO <fs_messages_sap>-line SEPARATED BY space.

  APPEND INITIAL LINE TO lt_messages_sap[] ASSIGNING <fs_messages_sap>.
  CONCATENATE '<BR>' '<br>' INTO <fs_messages_sap>-line.

  APPEND INITIAL LINE TO lt_messages_sap[] ASSIGNING <fs_messages_sap>.
  CONCATENATE '<B>Linha: </b> '(004) is_details-fevor INTO <fs_messages_sap>-line  SEPARATED BY space.

  WRITE is_details-matnr TO lv_matnr_str.

  APPEND INITIAL LINE TO lt_messages_sap[] ASSIGNING <fs_messages_sap>.
  CONCATENATE '<br><B>Material: </b> '(005) lv_matnr_str is_details-maktx INTO <fs_messages_sap>-line SEPARATED BY space.

  WRITE is_details-qtd_consumo TO lv_qtd_str UNIT is_details-meins.
  WRITE is_details-meins TO lv_uni_str.

  APPEND INITIAL LINE TO lt_messages_sap[] ASSIGNING <fs_messages_sap>.
  CONCATENATE '<br><B>Quantidade: </b> '(006) lv_qtd_str lv_uni_str '<br> <br>' INTO <fs_messages_sap>-line SEPARATED BY space.

  APPEND INITIAL LINE TO lt_messages_sap[] ASSIGNING <fs_messages_sap>.
  MOVE is_details-error_messsage TO <fs_messages_sap>-line.

  WRITE sy-datum TO lv_data_frm.
  WRITE sy-uzeit TO lv_hora_frm.

  APPEND INITIAL LINE TO lt_messages_sap[] ASSIGNING <fs_messages_sap>.
  CONCATENATE '<BR><BR>Data: '(007) lv_data_frm '<BR>Hora: '(008) lv_hora_frm INTO <fs_messages_sap>-line SEPARATED BY space.

  APPEND INITIAL LINE TO lt_messages_sap[] ASSIGNING <fs_messages_sap>.
  CONCATENATE '<BR>Utilizador: '(009) sy-uname INTO <fs_messages_sap>-line SEPARATED BY space.


**    Descrição do Processo
  CONCATENATE 'Erro consumo de matéria prima.'(002) '' INTO lv_title SEPARATED BY space.
  APPEND INITIAL LINE TO lt_receiver[] ASSIGNING <fs_receiver>.
  <fs_receiver>-receiver  = c_cons_errors_mail.
  <fs_receiver>-rec_type  = 'C'.
  <fs_receiver>-express   = abap_true.

** Cabeçalho
************************************************************************
  ls_docdata-obj_langu    =  'P'.
  ls_docdata-obj_prio     =  '1'.
  ls_docdata-priority     =  '1'.
  ls_docdata-obj_name     = 'Cabeçalho'(010).
  ls_docdata-obj_descr    = lv_title.
  ls_docdata-sensitivty   = 'P'.

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = ls_docdata
      document_type              = 'HTM'
      commit_work                = abap_true
    TABLES
      object_content             = lt_messages_sap[]
      receivers                  = lt_receiver[]
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.
ENDFORM.                    " F_SEND_MAIL
