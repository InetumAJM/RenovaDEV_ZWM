FUNCTION zwm_pallet_transfer_email.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TITLE) TYPE  STRING
*"     REFERENCE(IS_MSEG) TYPE  MSEG OPTIONAL
*"     REFERENCE(IS_MKPF_ORIG) TYPE  MKPF OPTIONAL
*"     REFERENCE(IS_LIKP) TYPE  LIKP OPTIONAL
*"     REFERENCE(IT_MESSAGES) TYPE  TAB_BDCMSGCOLL OPTIONAL
*"     REFERENCE(I_TARGET) TYPE  SO_RECNAME
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"----------------------------------------------------------------------
  DATA: ls_docdata      TYPE sodocchgi1,
        ls_messages_sap TYPE solisti1,
        ls_receiver     TYPE somlreci1,
        ls_p            TYPE solisti1,
        ls_lb           TYPE solisti1,
        ls_message      TYPE bdcmsgcoll,
        ls_mseg         TYPE mseg.

  DATA: lt_messages_sap TYPE TABLE OF solisti1,
        lt_receiver     TYPE TABLE OF somlreci1.

  DATA: lv_maktx    TYPE maktx,
        lv_name     TYPE name1,
        lv_title    TYPE so_obj_des,
        lv_title_o  TYPE string,
        lv_text     TYPE c LENGTH 100,
        lv_message  TYPE c LENGTH 200.

  CHECK NOT i_target IS INITIAL.

** Retorna Documento Material Criado
***********************************************************************
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
  lv_title_o = i_title.
  lv_title   = i_title.

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

** Informação
***********************************************************************
  ls_messages_sap-line = '<u><b>Erros na Transferencia de Paletes</b></u>'.
  APPEND ls_messages_sap TO lt_messages_sap.
  CLEAR  ls_messages_sap.

  APPEND ls_p TO lt_messages_sap.
** Mensagem
***********************************************************************
  IF NOT it_messages IS INITIAL.

    ls_messages_sap-line = '<b>Erro:</b>'.
    APPEND ls_messages_sap TO lt_messages_sap.
    CLEAR ls_messages_sap-line.

    ls_messages_sap-line = '<ul>'.
    APPEND ls_messages_sap TO lt_messages_sap.
    CLEAR ls_messages_sap-line.

    LOOP AT it_messages INTO ls_message.
      CLEAR ls_messages_sap.

      MESSAGE ID ls_message-msgid TYPE ls_message-msgtyp
          NUMBER ls_message-msgnr
            WITH ls_message-msgv1 ls_message-msgv2
                 ls_message-msgv3 ls_message-msgv4
            INTO lv_message.

      CONDENSE lv_message.

      CONCATENATE '<li>'
                  lv_message
                  '</li>'
             INTO ls_messages_sap-line.

      APPEND ls_messages_sap TO lt_messages_sap.

      CLEAR ls_messages_sap.
    ENDLOOP.

    ls_messages_sap-line = '</ul>'.
    APPEND ls_messages_sap TO lt_messages_sap.

    APPEND ls_p TO lt_messages_sap.
  ENDIF.

** Outros Dados
***********************************************************************

*--> Documento Material
  CONCATENATE '<b>Documento Material:</b>'
              is_mkpf_orig-mblnr
        INTO  ls_messages_sap-line
        SEPARATED BY space.
  APPEND ls_messages_sap TO lt_messages_sap.
  APPEND ls_lb TO lt_messages_sap.
  CLEAR  ls_messages_sap.

  CONCATENATE '<b>Ano Documento Material:</b>'
              is_mkpf_orig-mjahr
        INTO  ls_messages_sap-line
        SEPARATED BY space.
  APPEND ls_messages_sap TO lt_messages_sap.
  APPEND ls_lb TO lt_messages_sap.
  CLEAR  ls_messages_sap.

*--> Documento Material de Gerado
  IF NOT ls_mseg IS INITIAL.
    CONCATENATE '(' ls_mseg-bwart '):</b>'
           INTO lv_text.

    CONCATENATE '<b>Documento Material Gerado'
                 lv_text
                ls_mseg-mblnr
          INTO  ls_messages_sap-line
          SEPARATED BY space.
    APPEND ls_messages_sap TO lt_messages_sap.
    APPEND ls_lb TO lt_messages_sap.
    CLEAR  ls_messages_sap.

    CONCATENATE '<b>Ano Documento Material Gerado:</b>'
                ls_mseg-mjahr
          INTO  ls_messages_sap-line
          SEPARATED BY space.
    APPEND ls_messages_sap TO lt_messages_sap.
    APPEND ls_lb TO lt_messages_sap.
    CLEAR  ls_messages_sap.
  ENDIF.

*--> Remessa
  CONCATENATE '<b>Remessa:</b>'
              is_likp-vbeln
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
