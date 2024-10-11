FUNCTION zwm_pack_transportation.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(I_TKNUM) TYPE  TKNUM OPTIONAL
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"     REFERENCE(IT_ITEMS) TYPE  ZWM_T_PACK_IN_TRANSP
*"     REFERENCE(I_PACK) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  CONSTANTS: lc_action_create TYPE c VALUE 'A',
             lc_action_modify TYPE c VALUE 'C'.

  DATA: lv_tabix    TYPE sytabix,
        lv_vbeln    TYPE vbeln,
        lv_tknum    TYPE tknum,
        lv_subrc    TYPE sysubrc,
        lv_action   TYPE bapishipmentchangeaction,
        lv_garg     TYPE eqegraarg,
        lv_commit   TYPE flag,
        lv_count    TYPE i,
        lv_lines_in TYPE i.

  DATA: lt_return           TYPE TABLE OF bapiret2,
        lt_vekp             TYPE TABLE OF vekp,
        lt_vttp             TYPE TABLE OF vttp,
        lt_vepo             TYPE TABLE OF vepo,
        lt_hus              TYPE hum_exidv_t,
        lt_hu_header        TYPE hum_hu_header_t,
        lt_messages_hu      TYPE huitem_messages_t,
        lt_hdunheader	      TYPE TABLE OF bapishipmenthdunheader,
        lt_hdunheaderaction	TYPE TABLE OF bapishipmenthdunheaderaction,
        lt_hdunitem	        TYPE TABLE OF bapishipmenthdunitem,
        lt_hdunitemaction	  TYPE TABLE OF bapishipmenthdunitemaction.

  DATA: ls_item             TYPE zwm01_pack_in_transp,
        ls_vekp             TYPE vekp,
        ls_vttp             TYPE vttp,
        ls_vekp_test        TYPE vekp,
        ls_vepo             TYPE vepo,
        ls_hu               TYPE hum_exidv,
        ls_hu_header        TYPE vekpvb,
        ls_huchanged        TYPE bapihuheader,
        ls_message          TYPE bdcmsgcoll,
        ls_return           TYPE bapiret2,
        ls_headerdata	      TYPE bapishipmentheader,
        ls_headerdataaction	TYPE bapishipmentheaderaction,
        ls_hdunheader	      TYPE bapishipmenthdunheader,
        ls_hdunheaderaction	TYPE bapishipmenthdunheaderaction,
        ls_hdunitem	        TYPE bapishipmenthdunitem,
        ls_hdunitemaction	  TYPE bapishipmenthdunitemaction.

  FIELD-SYMBOLS: <ls_hdunitem> TYPE bapishipmenthdunitem.

  CLEAR et_messages.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

  IF it_items IS INITIAL.
**  Nenhuma Palete informada para embalar Doc de Transporte &
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = zwm001.
    ls_message-msgnr = '035'.
    ls_message-msgv1 = i_tknum.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  lv_action = lc_action_modify.

  lv_tknum = i_tknum.

** Retorna Doc de transporte de Grupo
***********************************************************************
  DO 1 TIMES.
    CHECK lv_tknum IS INITIAL.
    CHECK NOT i_refnr IS INITIAL.

    SELECT SINGLE rbnum FROM t311a
                    INTO lv_vbeln
                    WHERE lgnum = i_lgnum AND
                          refnr = i_refnr.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE tknum FROM vttp
                        INTO lv_tknum
                         WHERE vbeln = lv_vbeln.
  ENDDO.

  IF lv_tknum IS INITIAL.
    RAISE error.
  ENDIF.

  DO 1 TIMES.
** Retorna Cabeçalhos
***********************************************************************
    SELECT * FROM vekp
       INTO TABLE lt_vekp
       FOR ALL ENTRIES IN it_items
       WHERE exidv = it_items-exidv AND
             status <> '0060'.

    CHECK sy-subrc EQ 0.

    SORT lt_vekp BY exidv.

** Retorna Items Actuais
***********************************************************************
    SELECT * FROM vepo
       INTO TABLE lt_vepo
       FOR ALL ENTRIES IN lt_vekp
       WHERE venum = lt_vekp-venum.
  ENDDO.

** Associa a Documento de Transporte
***********************************************************************
  LOOP AT lt_vekp INTO ls_vekp.
    lv_tabix = sy-tabix.

    CLEAR: lt_hus, ls_hu.
    ls_hu-exidv = ls_vekp-exidv.
    APPEND ls_hu TO lt_hus.

    IF ls_vekp-vpobj EQ '01'.
*--> Já está associado à remessa
      DELETE lt_vekp INDEX lv_tabix.
      CONTINUE.
    ELSEIF ls_vekp-vpobj <> '04' OR
           ls_vekp-vpobjkey <> lv_tknum.
*--> Desembala HU se não atribuida ao transporte

      CALL FUNCTION 'ZWM_HU_UNPACK'
        EXPORTING
          i_exidv     = ls_vekp-exidv
        IMPORTING
          et_messages = et_messages
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      IF sy-subrc <> 0.
        RAISE error.
      ENDIF.

*      CALL FUNCTION 'HU_PACKING_REFRESH'.


*--> Associa HU ao Transporte
      ls_huchanged-hu_exid = ls_vekp-exidv.
      ls_huchanged-pack_mat_object = '04'.
      ls_huchanged-pack_mat_obj_key = lv_tknum.

      CLEAR: lt_return.
      CALL FUNCTION 'BAPI_HU_CHANGE_HEADER'
        EXPORTING
          hukey     = ls_vekp-exidv
          huchanged = ls_huchanged
        TABLES
          return    = lt_return.

      LOOP AT lt_return INTO ls_return WHERE type EQ 'E' OR
                                             type EQ 'A'.
        ls_message-msgtyp = ls_return-type.
        ls_message-msgid  = ls_return-id.
        ls_message-msgnr  = ls_return-number.
        ls_message-msgv1  = ls_return-message_v1.
        ls_message-msgv2  = ls_return-message_v2.
        ls_message-msgv3  = ls_return-message_v3.
        ls_message-msgv4  = ls_return-message_v4.
        APPEND ls_message TO et_messages.
      ENDLOOP.

      IF sy-subrc EQ 0.
        RAISE error.
      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      CONCATENATE sy-mandt ls_vekp-venum INTO lv_garg.
      CONDENSE lv_garg NO-GAPS.

      CALL FUNCTION 'Z_WM_DEQUEUE_WAIT'
        EXPORTING
          i_garg  = lv_garg
          i_gname = 'VEKP'
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.

      IF sy-subrc <> 0.
        RAISE error.
      ENDIF.

      DO 20 TIMES.
*--> Refresh de HU's
        CLEAR lv_subrc.
        CALL FUNCTION 'HU_PACKING_REFRESH'.

        CALL FUNCTION 'HU_GET_HUS'
          EXPORTING
            it_hus      = lt_hus
          IMPORTING
            et_header   = lt_hu_header
          EXCEPTIONS
            hus_locked  = 1
            no_hu_found = 2
            fatal_error = 3
            OTHERS      = 4.

        CLEAR ls_hu_header.
        READ TABLE lt_hu_header
              INTO ls_hu_header
              INDEX 1.

        IF ls_hu_header-vpobj EQ '04' AND
           ls_hu_header-vpobjkey EQ lv_tknum.
          EXIT.
        ENDIF.


*        CLEAR: ls_vekp_test.
*        SELECT SINGLE * FROM vekp
*                        BYPASSING BUFFER
*                        INTO ls_vekp_test
*                        WHERE exidv = ls_vekp-exidv.
*
*        IF ls_vekp_test-vpobj EQ '04' AND
*           ls_vekp_test-vpobjkey EQ lv_tknum.
*          EXIT.
*        ENDIF.

        WAIT UP TO 1 SECONDS.
      ENDDO.

      IF ls_hu_header-vpobj <> '04' OR
         ls_hu_header-vpobjkey <> lv_tknum.

**      Impossivel de associar HU & a Doc de Transporte &
        ls_message-msgtyp = 'E'.
        ls_message-msgspra = sy-langu.
        ls_message-msgid = 'ZWMSG001'.
        ls_message-msgnr = '004'.
        ls_message-msgv1 = ls_vekp-exidv.
        ls_message-msgv2 = lv_tknum.
        APPEND ls_message TO et_messages.
        RAISE error.
      ENDIF.
    ENDIF.
  ENDLOOP.


  CHECK i_pack EQ abap_true.


***********************************************************************
***********************************************************************

** Embalamento
***********************************************************************
  LOOP AT lt_vekp INTO ls_vekp.
    CLEAR: lt_hus, ls_hu.
    ls_hu-exidv = ls_vekp-exidv.
    APPEND ls_hu TO lt_hus.

    CLEAR: lt_hdunheader,
           lt_hdunheaderaction,
           lt_hdunitem,
           lt_hdunitemaction,
           ls_headerdata,
           ls_headerdataaction.

    CLEAR ls_hdunheader.
    ls_hdunheader-hdl_unit      = ls_vekp-venum.
    ls_hdunheader-hdl_unit_exid = ls_vekp-exidv.
    APPEND ls_hdunheader TO lt_hdunheader.

    CLEAR ls_hdunheaderaction.
    ls_hdunheaderaction-hdl_unit_exid = lv_action.
    APPEND ls_hdunheaderaction TO lt_hdunheaderaction.

*--> Novos Items
    LOOP AT it_items INTO ls_item WHERE exidv = ls_vekp-exidv.

      CLEAR ls_vekp.
      READ TABLE lt_vekp
            INTO ls_vekp
            WITH KEY exidv = ls_item-exidv
            BINARY SEARCH.

      CLEAR ls_hdunitem.
      ls_hdunitem-hdl_unit_into      = ls_vekp-venum.
      ls_hdunitem-hdl_unit_no        = ls_vekp-venum.
      ls_hdunitem-hdl_unit_exid_into = ls_item-exidv.
      ls_hdunitem-hdl_unit_exid      = ls_item-exidv.
      ls_hdunitem-deliv_numb         = ls_item-vbeln.
      ls_hdunitem-deliv_item         = ls_item-posnr.
      ls_hdunitem-pack_qty           = ls_item-vemng.
      ls_hdunitem-sales_unit         = ls_item-vrkme.
      ls_hdunitem-sales_unit_iso     = ls_item-vrkme.
      ls_hdunitem-hu_item_type       = '1'.
      APPEND ls_hdunitem TO lt_hdunitem.

      CLEAR ls_hdunitemaction.
      ls_hdunitemaction-hdl_unit_exid_into = lv_action.
      ls_hdunitemaction-hdl_unit_exid      = lv_action.
      ls_hdunitemaction-deliv_numb         = lv_action.
      ls_hdunitemaction-deliv_item         = lv_action.
      ls_hdunitemaction-pack_qty           = lv_action.
      ls_hdunitemaction-sales_unit         = lv_action.
      ls_hdunitemaction-sales_unit_iso     = lv_action.
      APPEND ls_hdunitemaction TO lt_hdunitemaction.
    ENDLOOP.

*--> Refresh
    DO 1 TIMES.
      CLEAR: lv_subrc, lt_return.

      IF sy-index > 1.
        WAIT UP TO 1 SECONDS.
      ENDIF.


      CLEAR lv_subrc.
      CALL FUNCTION 'HU_PACKING_REFRESH'.

      CALL FUNCTION 'HU_GET_HUS'
        EXPORTING
          it_hus      = lt_hus
        EXCEPTIONS
          hus_locked  = 1
          no_hu_found = 2
          fatal_error = 3
          OTHERS      = 4.

*--> Embala HU's
      ls_headerdata-shipment_num = lv_tknum.

      CLEAR lt_return.
      CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
        EXPORTING
          headerdata       = ls_headerdata
          headerdataaction = ls_headerdataaction
        TABLES
          hdunheader       = lt_hdunheader
          hdunheaderaction = lt_hdunheaderaction
          hdunitem         = lt_hdunitem
          hdunitemaction   = lt_hdunitemaction
          return           = lt_return.

      lv_subrc = sy-subrc.

**  Erros
*      LOOP AT lt_return INTO ls_return WHERE type EQ 'E' OR
*                                             type EQ 'A'.
*        CLEAR: ls_message.
*        ls_message-msgtyp = ls_return-type.
*        ls_message-msgid  = ls_return-id.
*        ls_message-msgnr  = ls_return-number.
*        ls_message-msgv1  = ls_return-message_v1.
*        ls_message-msgv2  = ls_return-message_v2.
*        ls_message-msgv3  = ls_return-message_v3.
*        ls_message-msgv4  = ls_return-message_v4.
*        APPEND ls_message TO et_messages.
*        CLEAR ls_message.
*      ENDLOOP.
*      IF sy-subrc EQ 0.
*        RAISE error.
*      ENDIF.

*      IF lv_subrc <> 0 AND NOT sy-msgid IS INITIAL.
*        CLEAR: ls_message.
*        ls_message-msgtyp = sy-msgty.
*        ls_message-msgid  = sy-msgid.
*        ls_message-msgnr  = sy-msgno.
*        ls_message-msgv1  = sy-msgv1.
*        ls_message-msgv2  = sy-msgv2.
*        ls_message-msgv3  = sy-msgv3.
*        ls_message-msgv4  = sy-msgv4.
*        APPEND ls_message TO et_messages.
*        CLEAR  ls_message.
*        RAISE error.
*      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

**    Remove Bloqueios
      PERFORM dequeue_tknum USING lv_tknum.

      DESCRIBE TABLE lt_hdunitem LINES lv_lines_in.

      CLEAR lv_count.
      SELECT COUNT(*) FROM vepo
                      INTO lv_count
                      BYPASSING BUFFER
                      WHERE venum = ls_vekp-venum.

      IF lv_count EQ lv_lines_in.
        EXIT.
      ENDIF.

    ENDDO.
  ENDLOOP.
  CHECK sy-subrc EQ 0.

** Valida Embalamento
***********************************************************************
  DO 10 TIMES.
    CLEAR et_messages.

    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    SELECT * FROM vttp
       INTO TABLE lt_vttp
       BYPASSING BUFFER
       WHERE tknum = lv_tknum.

    LOOP AT it_items INTO ls_item.

      READ TABLE lt_vekp
        WITH KEY exidv = ls_item-exidv
        BINARY SEARCH
        TRANSPORTING NO FIELDS.

      CHECK sy-subrc EQ 0.

      LOOP AT lt_vttp INTO ls_vttp WHERE vbeln = ls_item-vbeln AND
                                         pksta <> abap_true.

**    Erro a embalar Doc Transporte &, Remessa & na HU &
        ls_message-msgtyp = 'E'.
        ls_message-msgspra = sy-langu.
        ls_message-msgid = 'ZWMSG001'.
        ls_message-msgnr = '005'.
        ls_message-msgv1 = ls_vttp-tknum.
        ls_message-msgv2 = ls_vttp-vbeln.
        ls_message-msgv3 = ls_item-exidv.
        ls_message-msgv4 = sy-msgv4.
        APPEND ls_message TO et_messages.
      ENDLOOP.
    ENDLOOP.

    IF et_messages IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.

  IF NOT et_messages IS INITIAL.
    RAISE error.
  ENDIF.
ENDFUNCTION.
