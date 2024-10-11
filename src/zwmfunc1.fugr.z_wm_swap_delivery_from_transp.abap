FUNCTION z_wm_swap_delivery_from_transp.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TKNUM_O) TYPE  TKNUM
*"     REFERENCE(I_TKNUM_D) TYPE  TKNUM
*"     REFERENCE(I_VBELN) TYPE  VBELN OPTIONAL
*"     REFERENCE(IR_VBELN) TYPE  ZWM01_R_VBELN OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: lr_vbeln TYPE zwm01_r_vbeln.

  DATA: lt_vttp_o         TYPE TABLE OF vttp,
        lt_vekp           TYPE TABLE OF vekp,
        lt_vepo           TYPE TABLE OF vepo,
        lt_vepo_move      TYPE TABLE OF vepo,
        lt_return         TYPE TABLE OF bapiret2,
        lt_itemdata	      TYPE TABLE OF bapishipmentitem,
        lt_itemdataaction	TYPE TABLE OF bapishipmentitemaction.

  DATA: ls_r_vbeln          LIKE LINE OF lr_vbeln,
        ls_message          TYPE bdcmsgcoll,
        ls_vttk_o           TYPE vttk,
        ls_vttp_o           TYPE vttp,
        ls_vttk_d           TYPE vttk,
        ls_vekp             TYPE vekp,
        ls_vepo             TYPE vepo,
        ls_return           TYPE bapiret2,
        ls_headerdata	      TYPE bapishipmentheader,
        ls_headerdataaction	TYPE bapishipmentheaderaction,
        ls_itemdata	        TYPE bapishipmentitem,
        ls_itemdataaction	  TYPE bapishipmentitemaction.

  DATA: lv_lines TYPE sytabix,
        lv_tabix TYPE sytabix,
        lv_pkstk TYPE pkstk_vttk.

  CLEAR: et_messages.

  lr_vbeln = ir_vbeln.

  IF NOT i_vbeln IS INITIAL.
    ls_r_vbeln-low    = i_vbeln.
    ls_r_vbeln-sign   = 'I'.
    ls_r_vbeln-option = 'EQ'.
    APPEND ls_r_vbeln TO lr_vbeln.
  ENDIF.

  CHECK NOT lr_vbeln IS INITIAL.

** Retorna Documento de Transporte de Origem
***********************************************************************
  DO 1 TIMES.
    SELECT SINGLE * FROM vttk
                    INTO ls_vttk_o
                    WHERE tknum = i_tknum_o.

    CHECK sy-subrc EQ 0.

    SELECT * FROM vttp
       INTO TABLE lt_vttp_o
       WHERE tknum = i_tknum_o.

    CHECK sy-subrc EQ 0.
  ENDDO.

  IF sy-subrc <> 0.
** 	Documento de Transporte & inválido
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = 'ZWM001'.
    ls_message-msgnr = '037'.
    ls_message-msgv1 = i_tknum_o.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

** HU's
***********************************************************************
  DO 1 TIMES.
    SELECT * FROM vekp
       INTO TABLE lt_vekp
       WHERE vpobj = '04' AND
             vpobjkey = i_tknum_o.

    CHECK sy-subrc EQ 0.
    SORT lt_vekp BY venum.

    SELECT * FROM vepo
       INTO TABLE lt_vepo
       FOR ALL ENTRIES IN lt_vekp
       WHERE venum = lt_vekp-venum.
  ENDDO.

** Retorna Documento de Transporte de Destino
***********************************************************************
  SELECT SINGLE * FROM vttk
                  INTO ls_vttk_d
                  WHERE tknum = i_tknum_d.

  IF sy-subrc <> 0.
** 	Documento de Transporte & inválido
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = 'ZWM001'.
    ls_message-msgnr = '037'.
    ls_message-msgv1 = i_tknum_d.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

** Retorna Lista de Remessas
***********************************************************************
  LOOP AT lt_vepo INTO ls_vepo WHERE vbeln IN lr_vbeln.
    LOOP AT lt_vepo INTO ls_vepo WHERE venum = ls_vepo-venum AND
                                       vbeln NOT IN lr_vbeln.

      CLEAR: ls_vekp.
      READ TABLE lt_vekp
            INTO ls_vekp
            WITH KEY venum = ls_vepo-venum
            BINARY SEARCH.

** 	  Palete & contem remessas não indicadas
      ls_message-msgtyp = 'E'.
      ls_message-msgspra = sy-langu.
      ls_message-msgid = 'ZWM001'.
      ls_message-msgnr = '038'.
      ls_message-msgv1 = ls_vekp-exidv.
      APPEND ls_message TO et_messages.
      RAISE error.
    ENDLOOP.

    APPEND ls_vepo TO lt_vepo_move.
  ENDLOOP.



  SORT lt_itemdata BY delivery.
  DELETE ADJACENT DUPLICATES FROM lt_itemdata COMPARING delivery.

  SORT lt_vepo_move BY venum.
  DELETE ADJACENT DUPLICATES FROM lt_vepo_move COMPARING venum.

** remessas a Serem Movidas
***********************************************************************
  LOOP AT lt_vttp_o INTO ls_vttp_o WHERE vbeln IN lr_vbeln.
    ls_itemdata-delivery = ls_vttp_o-vbeln.
    APPEND ls_itemdata TO lt_itemdata.
  ENDLOOP.

** Modifica Status Documentos de Transporte
***********************************************************************
  UPDATE vttk SET stdis = '' WHERE tknum = i_tknum_o.
  UPDATE vttk SET stdis = '' WHERE tknum = i_tknum_d.
  COMMIT WORK.

** Altera HU
***********************************************************************
  LOOP AT lt_vepo_move INTO ls_vepo.

    CLEAR ls_vekp.
    READ TABLE lt_vekp
          INTO ls_vekp
          WITH KEY venum = ls_vepo-venum
          BINARY SEARCH.

    CHECK ls_vekp-vpobj EQ '04'.

    UPDATE vekp SET vpobj = '00' vpobjkey = i_tknum_d WHERE venum = ls_vepo-venum.
  ENDLOOP.

** Apaga Remessas do Documento de Transporte de Origem
***********************************************************************
  ls_headerdata-shipment_num = i_tknum_o.

  DESCRIBE TABLE lt_itemdata LINES lv_lines.

  CLEAR lt_itemdataaction.
  DO lv_lines TIMES.
    ls_itemdataaction-delivery = 'D'.
    ls_itemdataaction-itenerary = 'D'.
    APPEND ls_itemdataaction TO lt_itemdataaction.
  ENDDO.

  CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
    EXPORTING
      headerdata       = ls_headerdata
      headerdataaction = ls_headerdataaction
    TABLES
      itemdata         = lt_itemdata
      itemdataaction   = lt_itemdataaction
      return           = lt_return.

  LOOP AT lt_return INTO ls_return WHERE type EQ 'E' OR
                                         type EQ 'A'.
    CLEAR: ls_message.
    ls_message-msgtyp = ls_return-type.
    ls_message-msgid  = ls_return-id.
    ls_message-msgnr  = ls_return-number.
    ls_message-msgv1  = ls_return-message_v1.
    ls_message-msgv2  = ls_return-message_v2.
    ls_message-msgv3  = ls_return-message_v3.
    ls_message-msgv4  = ls_return-message_v4.
    APPEND ls_message TO et_messages.
    CLEAR ls_message.
  ENDLOOP.
  IF sy-subrc EQ 0.
    RAISE error.
  ENDIF.

** Adiciona Remessas do Documento de Transporte de Destino
***********************************************************************
  ls_headerdata-shipment_num = i_tknum_d.

  DESCRIBE TABLE lt_itemdata LINES lv_lines.

  CLEAR lt_itemdataaction.
  DO lv_lines TIMES.
    ls_itemdataaction-delivery = 'A'.
    ls_itemdataaction-itenerary = 'A'.
    APPEND ls_itemdataaction TO lt_itemdataaction.
  ENDDO.

  CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
    EXPORTING
      headerdata       = ls_headerdata
      headerdataaction = ls_headerdataaction
    TABLES
      itemdata         = lt_itemdata
      itemdataaction   = lt_itemdataaction
      return           = lt_return.

  LOOP AT lt_return INTO ls_return WHERE type EQ 'E' OR
                                         type EQ 'A'.
    CLEAR: ls_message.
    ls_message-msgtyp = ls_return-type.
    ls_message-msgid  = ls_return-id.
    ls_message-msgnr  = ls_return-number.
    ls_message-msgv1  = ls_return-message_v1.
    ls_message-msgv2  = ls_return-message_v2.
    ls_message-msgv3  = ls_return-message_v3.
    ls_message-msgv4  = ls_return-message_v4.
    APPEND ls_message TO et_messages.
    CLEAR ls_message.
  ENDLOOP.
  IF sy-subrc EQ 0.
    RAISE error.
  ENDIF.

** Altera Status de HU's
***********************************************************************
  LOOP AT lt_vepo_move INTO ls_vepo.

    CLEAR ls_vekp.
    READ TABLE lt_vekp
          INTO ls_vekp
          WITH KEY venum = ls_vepo-venum
          BINARY SEARCH.

    lv_tabix = sy-tabix.

    CHECK ls_vekp-vpobj EQ '04'.

    UPDATE vekp SET vpobj = '04' vpobjkey = i_tknum_d WHERE venum = ls_vepo-venum.
    CHECK sy-subrc EQ 0.

    DELETE lt_vekp INDEX lv_tabix.
  ENDLOOP.

** Altera Status de Transportes
***********************************************************************
  IF lt_vekp IS INITIAL.
    lv_pkstk = abap_false.
  ELSE.
    lv_pkstk = ls_vttk_o-pkstk.
  ENDIF.

  UPDATE vttk SET stdis = ls_vttk_o-stdis
                  pkstk = lv_pkstk
              WHERE tknum = i_tknum_o.

  UPDATE vttk SET stdis = ls_vttk_d-stdis WHERE tknum = i_tknum_d.
  COMMIT WORK.

ENDFUNCTION.
