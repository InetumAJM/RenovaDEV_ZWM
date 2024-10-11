FUNCTION zwm_email_ent_pal_producao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_LGORT) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(I_MBLNR) TYPE  MBLNR OPTIONAL
*"     REFERENCE(I_MJAHR) TYPE  MJAHR OPTIONAL
*"     REFERENCE(IT_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"     REFERENCE(IT_ITEMS) TYPE  TAB_BAPI_GOODSMVT_ITEM
*"     REFERENCE(I_TARGET) TYPE  SO_RECNAME
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"----------------------------------------------------------------------
  DATA: ls_docdata      TYPE sodocchgi1,
        ls_messages_sap TYPE solisti1,
        ls_receiver     TYPE somlreci1,
        ls_item         TYPE bapi2017_gm_item_create,
        ls_p            TYPE solisti1,
        ls_lb           TYPE solisti1,
        ls_message      TYPE bdcmsgcoll.

  DATA: lt_messages_sap TYPE TABLE OF solisti1,
        lt_receiver     TYPE TABLE OF somlreci1,
        lt_messages     TYPE tab_bdcmsgcoll.

  DATA: lv_title    TYPE so_obj_des,
        lv_title_o  TYPE string,
        lv_text     TYPE c LENGTH 100.

  lt_messages = it_messages.

  LOOP AT lt_messages INTO ls_message WHERE msgtyp <> 'E' AND
                                            msgtyp <> 'A'.
    DELETE lt_messages INDEX sy-tabix.
  ENDLOOP.

** Titulo
***********************************************************************
  IF lt_messages IS INITIAL.
    lv_title = 'Entrada de Paletes de Produção'.
  ELSE.
    lv_title = 'Erro na Entrada de Paletes de Produção'.
  ENDIF.


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
  CONCATENATE '<b>'
              lv_title
              '</b>'
         INTO lv_title_o.


** Header
***********************************************************************
  ls_messages_sap-line = lv_title_o.
  APPEND ls_messages_sap TO lt_messages_sap.
  CLEAR  ls_messages_sap.

** Mensagem
***********************************************************************
  IF NOT lt_messages IS INITIAL.
    APPEND ls_p TO lt_messages_sap.

    ls_messages_sap-line = '<b>Erro:</b>'.
    APPEND ls_messages_sap TO lt_messages_sap.
    CLEAR ls_messages_sap-line.

    ls_messages_sap-line = '<ul>'.
    APPEND ls_messages_sap TO lt_messages_sap.
    CLEAR ls_messages_sap-line.

    LOOP AT lt_messages INTO ls_message.
      CLEAR ls_messages_sap.

      MESSAGE ID ls_message-msgid
            TYPE ls_message-msgtyp
            NUMBER ls_message-msgnr
            WITH ls_message-msgv1
                 ls_message-msgv2
                 ls_message-msgv3
                 ls_message-msgv4
            INTO lv_text.

      CONCATENATE '<li>'
                  lv_text
                  '</li>'
             INTO ls_messages_sap-line.

      APPEND ls_messages_sap TO lt_messages_sap.

      CLEAR ls_messages_sap.
      CLEAR lv_text.
    ENDLOOP.

    ls_messages_sap-line = '</ul>'.
    APPEND ls_messages_sap TO lt_messages_sap.
  ENDIF.

** Dados
***********************************************************************
  APPEND ls_p TO lt_messages_sap.

  IF NOT i_mblnr IS INITIAL.
    CONCATENATE '<b>Doc. Material:</b>'
                i_mblnr
          INTO  ls_messages_sap-line
          SEPARATED BY space.
    APPEND ls_messages_sap TO lt_messages_sap.
    APPEND ls_lb TO lt_messages_sap.
    CLEAR  ls_messages_sap.

    CONCATENATE '<b>Ano Doc. Material:</b>'
                i_mjahr
          INTO  ls_messages_sap-line
          SEPARATED BY space.
    APPEND ls_messages_sap TO lt_messages_sap.
    APPEND ls_lb TO lt_messages_sap.
    CLEAR  ls_messages_sap.
  ENDIF.

  IF NOT i_werks IS INITIAL.
    CONCATENATE '<b>Centro:</b>'
                i_werks
          INTO  ls_messages_sap-line
          SEPARATED BY space.
    APPEND ls_messages_sap TO lt_messages_sap.
    APPEND ls_lb TO lt_messages_sap.
    CLEAR  ls_messages_sap.
  ENDIF.

  IF NOT i_lgort IS INITIAL.
    CONCATENATE '<b>Depósito:</b>'
                i_lgort
          INTO  ls_messages_sap-line
          SEPARATED BY space.
    APPEND ls_messages_sap TO lt_messages_sap.
    APPEND ls_lb TO lt_messages_sap.
    CLEAR  ls_messages_sap.
  ENDIF.

** Dados Extras
***********************************************************************
  APPEND ls_p TO lt_messages_sap.
  ls_messages_sap-line = '<b>Dados:</b>'.
  APPEND ls_messages_sap TO lt_messages_sap.
  APPEND ls_lb TO lt_messages_sap.
  CLEAR  ls_messages_sap.


  LOOP AT it_items INTO ls_item.
    ls_messages_sap-line = '<ul>'.

    CONCATENATE '<li><b>Material:</b>'
                ls_item-material
                '</li>'
          INTO  ls_messages_sap-line
          SEPARATED BY space.

    APPEND ls_messages_sap TO lt_messages_sap.
    CLEAR  ls_messages_sap.

    WRITE ls_item-entry_qnt TO lv_text UNIT ls_item-entry_uom.

    CONCATENATE '<li><b>Quantidade:</b>'
                lv_text
                ls_item-entry_uom
                '</li>'
          INTO  ls_messages_sap-line
          SEPARATED BY space.

    APPEND ls_messages_sap TO lt_messages_sap.
    CLEAR  ls_messages_sap.

    WRITE ls_item-amount_lc TO lv_text.
    CONCATENATE '<li><b>Preço:</b>'
                lv_text
                '</li>'
          INTO  ls_messages_sap-line
          SEPARATED BY space.

    APPEND ls_messages_sap TO lt_messages_sap.
    CLEAR  ls_messages_sap.

    ls_messages_sap-line = '</ul>'.
  ENDLOOP.

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
