FUNCTION zwm_pal_transfer_email.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TITLE) TYPE  STRING
*"     REFERENCE(I_EXIDV) TYPE  EXIDV OPTIONAL
*"     REFERENCE(I_LGORT_DEST) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(IS_MSEG) TYPE  MSEG OPTIONAL
*"     REFERENCE(IS_LTAK) TYPE  LTAK OPTIONAL
*"     REFERENCE(I_TARGET) TYPE  SO_RECNAME OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"----------------------------------------------------------------------

  DATA: ls_docdata      TYPE sodocchgi1,
        ls_messages_sap TYPE solisti1,
        ls_receiver     TYPE somlreci1,
        ls_p            TYPE solisti1,
        ls_lb           TYPE solisti1,
        ls_mseg         TYPE mseg.

  DATA: lt_messages_sap TYPE TABLE OF solisti1,
        lt_receiver     TYPE TABLE OF somlreci1.

  DATA: lv_maktx    TYPE maktx,
        lv_name     TYPE name1,
        lv_title    TYPE so_obj_des,
        lv_title_o  TYPE string,
        lv_text     TYPE c LENGTH 100.

  CHECK NOT i_target IS INITIAL.

  ls_mseg = is_mseg.

** Linha Em Branco
***********************************************************************
  CONCATENATE '<br />' '<br />' '<br />'
  INTO ls_p-line.

  CONCATENATE '<br />' ''
  INTO ls_lb-line.

** Tipo de Letra
***********************************************************************
  ls_messages_sap-line = '<div style="font-family:Calibri;">'.
  APPEND ls_messages_sap TO lt_messages_sap.
  CLEAR  ls_messages_sap.

** Titulo
***********************************************************************
  CONCATENATE i_title i_exidv 'para' i_lgort_dest
         INTO lv_title_o SEPARATED BY space.

  lv_title = lv_title_o.

  CONCATENATE '<b>'
              lv_title_o
              '</b>'
         INTO lv_title_o.


** Header
***********************************************************************
  ls_messages_sap-line = lv_title_o.
  APPEND ls_messages_sap TO lt_messages_sap.
  CLEAR  ls_messages_sap.

  APPEND ls_p TO lt_messages_sap.

** Outros Dados
***********************************************************************

*--> Documento Material
  CONCATENATE '(' ls_mseg-bwart '):</b>'
         INTO lv_text.

  CONCATENATE '<b>Documento Material'
               lv_text
              ls_mseg-mblnr
        INTO  ls_messages_sap-line
        SEPARATED BY space.
  APPEND ls_messages_sap TO lt_messages_sap.
  APPEND ls_lb TO lt_messages_sap.
  CLEAR  ls_messages_sap.

  CONCATENATE '<b>Ano Documento Material:</b>'
              ls_mseg-mjahr
        INTO  ls_messages_sap-line
        SEPARATED BY space.
  APPEND ls_messages_sap TO lt_messages_sap.
  APPEND ls_lb TO lt_messages_sap.
  CLEAR  ls_messages_sap.

  CONCATENATE '<b>OT:</b>'
              is_ltak-tanum
        INTO  ls_messages_sap-line
        SEPARATED BY space.
  APPEND ls_messages_sap TO lt_messages_sap.
  APPEND ls_lb TO lt_messages_sap.
  CLEAR  ls_messages_sap.

*--> Material
  SELECT SINGLE maktx
           FROM makt
           INTO lv_maktx
           WHERE matnr = ls_mseg-matnr AND
                 spras = 'PT'.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    EXPORTING
      input  = ls_mseg-matnr
    IMPORTING
      output = ls_mseg-matnr.


  CONCATENATE '<b>Material:</b>'
              ls_mseg-matnr
              lv_maktx
        INTO  ls_messages_sap-line
        SEPARATED BY space.
  APPEND ls_messages_sap TO lt_messages_sap.
  APPEND ls_lb TO lt_messages_sap.
  CLEAR  ls_messages_sap.

  CONCATENATE ls_messages_sap-line
              lv_maktx
       INTO ls_messages_sap-line
       SEPARATED BY ' - '.

*--> Lote
  CONCATENATE '<b>Lote:</b>'
              ls_mseg-charg
        INTO  ls_messages_sap-line
        SEPARATED BY space.
  APPEND ls_messages_sap TO lt_messages_sap.
  APPEND ls_lb TO lt_messages_sap.
  CLEAR  ls_messages_sap.

*--> Quantidade
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = ls_mseg-meins
      language       = sy-langu
    IMPORTING
      output         = ls_mseg-meins
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.


  lv_text = ls_mseg-menge.
  CONDENSE lv_text.
  CONCATENATE '<b>Quantidade:</b>'
              lv_text
              ls_mseg-meins
        INTO  ls_messages_sap-line
        SEPARATED BY space.
  APPEND ls_messages_sap TO lt_messages_sap.
  APPEND ls_lb TO lt_messages_sap.
  CLEAR  ls_messages_sap.

** Processado Por:
***********************************************************************
  APPEND ls_p TO lt_messages_sap.

  CONCATENATE '<b>User:</b>'
              sy-uname
        INTO  ls_messages_sap-line
        SEPARATED BY space.
  APPEND ls_messages_sap TO lt_messages_sap.
  APPEND ls_lb TO lt_messages_sap.
  CLEAR  ls_messages_sap.

  WRITE sy-datum TO lv_text.
  CONDENSE lv_text.
  CONCATENATE '<b>Data:</b>'
              lv_text
        INTO  ls_messages_sap-line
        SEPARATED BY space.
  APPEND ls_messages_sap TO lt_messages_sap.
  APPEND ls_lb TO lt_messages_sap.
  CLEAR  ls_messages_sap.

  WRITE sy-uzeit TO lv_text.
  CONDENSE lv_text.
  CONCATENATE '<b>Hora:</b>'
              lv_text
        INTO  ls_messages_sap-line
        SEPARATED BY space.
  APPEND ls_messages_sap TO lt_messages_sap.
  APPEND ls_lb TO lt_messages_sap.
  CLEAR  ls_messages_sap.

** Fim de Tipo de Letra
***********************************************************************
  ls_messages_sap-line = '</div>'.
  APPEND ls_messages_sap TO lt_messages_sap.
  CLEAR  ls_messages_sap.

** Titulo
***********************************************************************
  ls_receiver-receiver  = i_target.
  ls_receiver-rec_type  = 'C'.
  ls_receiver-express   = 'X'.
  APPEND ls_receiver TO lt_receiver.

  ls_docdata-obj_langu   =  'P'.
  ls_docdata-obj_prio    =  '1'.
  ls_docdata-priority    =  '1'.

** Cabeçalho
************************************************************************
  ls_docdata-obj_name   = 'Cabeçalho'.
  ls_docdata-obj_descr  = lv_title.
  ls_docdata-sensitivty = 'P'.

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = ls_docdata
      document_type              = 'HTM'
      commit_work                = i_commit
    TABLES
      object_content             = lt_messages_sap
      receivers                  = lt_receiver
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
ENDFUNCTION.
