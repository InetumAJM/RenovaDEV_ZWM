FUNCTION zwm_goodsmvt_create .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CODE) TYPE  GM_CODE
*"     REFERENCE(I_BWART) TYPE  BWART OPTIONAL
*"     REFERENCE(I_BSSKZ) TYPE  LVS_BSSKZ OPTIONAL
*"     REFERENCE(I_ABLAD) TYPE  ABLAD OPTIONAL
*"     REFERENCE(I_AUFNR) TYPE  AUFNR OPTIONAL
*"     REFERENCE(I_XBLNR) OPTIONAL
*"     REFERENCE(I_XABLN) OPTIONAL
*"     REFERENCE(I_SGTXT) TYPE  SGTXT OPTIONAL
*"     REFERENCE(I_GRUND) TYPE  MB_GRBEW OPTIONAL
*"     REFERENCE(I_WAIT) TYPE  NUMC2 DEFAULT '20'
*"     REFERENCE(I_REVERSE) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_FRBNR) TYPE  FRBNR OPTIONAL
*"     REFERENCE(I_LIFNR) TYPE  LIFNR OPTIONAL
*"     REFERENCE(I_DATUM) TYPE  DATUM DEFAULT SYST-DATUM
*"     REFERENCE(I_HEADER_TXT) OPTIONAL
*"     REFERENCE(I_TESTRUN) TYPE  BAPI2017_GM_GEN-TESTRUN OPTIONAL
*"     REFERENCE(IT_ITEMS) TYPE  TAB_BAPI_GOODSMVT_ITEM
*"     REFERENCE(IT_EXTENSIONIN) TYPE  TT_BAPIPAREX OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(E_MBLNR) TYPE  MBLNR
*"     REFERENCE(E_MJAHR) TYPE  MJAHR
*"     REFERENCE(ET_MSEG) TYPE  TY_MSEG
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------



** Códigos Possíveis
***********************************************************************
*01     MB01  - Entrada para Pedido
*02     MB31  - Entrada para Ordem
*03     MB1A  - Saídas
*04     MB1B  - Transferências
*05     MB1C  - Outras Entradas
*06     MB11  - Movimento Mercadorias (Genérico)
*07     MB04  - Subcontratação
***********************************************************************

** Campos Items
***********************************************************************
*  plant     = werks.
*  stge_loc  = lgort
*  material  = matnr
*  batch     = charg.
*  entry_qnt = menge.
*  entry_uom = meins.
*  po_number = ebeln.
*  po_item   = ebelp.


***********************************************************************

  DATA: lv_flag_bd         TYPE flag,
        lv_bwart           TYPE bwart,
        lv_code            TYPE bapi2017_gm_code,
        ls_goodsmvt_header TYPE bapi2017_gm_head_01,
        ls_message         TYPE bdcmsgcoll,
        ls_goodsmvt_item   TYPE bapi2017_gm_item_create,
        ls_return          TYPE bapiret2,
        lv_times           TYPE i,
        lt_goodsmvt_item   TYPE TABLE OF bapi2017_gm_item_create,
        lt_return          TYPE TABLE OF bapiret2.


***********************************************************************
  CLEAR: et_messages, e_mblnr, e_mjahr, et_mseg.

  IF it_items IS INITIAL.
**  Sem dados a processar
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = 'YMM001'.
    ls_message-msgnr  = '004'.
    APPEND ls_message TO et_messages.
    CLEAR  ls_message.
    RAISE error.
  ENDIF.


** Movimento Directo ou Estorno
***********************************************************************
  IF i_reverse IS INITIAL.
**  Movimento Directo
    lv_bwart = i_bwart.
  ELSE.
**  Determina Movimento de Estorno
    SELECT SINGLE bwart_next FROM t156n INTO lv_bwart
            WHERE fcode EQ 'ST'
              AND bwart EQ i_bwart.
    IF sy-subrc <> 0.
**    Sem Movimento de Estorno definido para Movimento &
      ls_message-msgtyp = 'E'.
      ls_message-msgid  = 'YMM001'.
      ls_message-msgnr  = '005'.
      ls_message-msgv1  = i_bwart.
      APPEND ls_message TO et_messages.
      CLEAR  ls_message.
      RAISE error.
    ENDIF.
  ENDIF.


** Cabeçalho
***********************************************************************
  GET TIME.
  lv_code-gm_code = i_code.

  MOVE: i_datum      TO ls_goodsmvt_header-doc_date,
        i_datum      TO ls_goodsmvt_header-pstng_date,
        i_xblnr      TO ls_goodsmvt_header-ref_doc_no,
        i_xabln      TO ls_goodsmvt_header-gr_gi_slip_no,
        i_frbnr      TO ls_goodsmvt_header-bill_of_lading,
        i_header_txt TO ls_goodsmvt_header-header_txt.


** Items
***********************************************************************
  LOOP AT it_items INTO ls_goodsmvt_item.
**  MOVE_* => Destino

**  Dados Gerais
    IF ls_goodsmvt_item-move_type IS INITIAL.
      ls_goodsmvt_item-move_type = lv_bwart.
    ENDIF.

    IF ls_goodsmvt_item-item_text IS INITIAL.
      ls_goodsmvt_item-item_text = i_sgtxt.
    ENDIF.

    IF ls_goodsmvt_item-orderid IS INITIAL.
      ls_goodsmvt_item-orderid = i_aufnr.
    ENDIF.

    IF ls_goodsmvt_item-unload_pt IS INITIAL.
      ls_goodsmvt_item-unload_pt = i_ablad.
    ENDIF.

    IF ls_goodsmvt_item-move_reas IS INITIAL.
      ls_goodsmvt_item-move_reas = i_grund.
    ENDIF.

    IF ls_goodsmvt_item-vendor IS INITIAL.
      ls_goodsmvt_item-vendor = i_lifnr.
    ENDIF.

**  Código de Movimento
    IF NOT ls_goodsmvt_item-po_number IS INITIAL.
      ls_goodsmvt_item-mvt_ind = 'B'.  " Pedido
    ELSEIF NOT ls_goodsmvt_item-deliv_numb IS INITIAL.
      ls_goodsmvt_item-mvt_ind = 'L'.  " Remessa
    ELSEIF NOT ls_goodsmvt_item-orderid IS INITIAL.
      ls_goodsmvt_item-mvt_ind = 'F'.  " Ordem Produção
    ELSE.
      ls_goodsmvt_item-mvt_ind = ' '.
    ENDIF.

    IF NOT i_bsskz IS INITIAL.
      ls_goodsmvt_item-spec_mvmt = i_bsskz.
    ENDIF.

    ls_goodsmvt_item-entry_uom_iso = ls_goodsmvt_item-entry_uom.

    COLLECT ls_goodsmvt_item INTO lt_goodsmvt_item.
    CLEAR   ls_goodsmvt_item.
  ENDLOOP.


** Executa Movimento
***********************************************************************
  CALL FUNCTION 'HU_PACKING_REFRESH'.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = ls_goodsmvt_header
      goodsmvt_code    = lv_code
      testrun          = i_testrun
    IMPORTING
      materialdocument = e_mblnr
      matdocumentyear  = e_mjahr
    TABLES
      goodsmvt_item    = lt_goodsmvt_item
      return           = lt_return
      extensionin      = it_extensionin.

** Pesquisa de Erros
***********************************************************************
  LOOP AT lt_return INTO ls_return WHERE type = 'E'
                                      OR type = 'A'.
    MOVE ls_return-type       TO ls_message-msgtyp.
    MOVE ls_return-id         TO ls_message-msgid.
    MOVE ls_return-number     TO ls_message-msgnr.
    MOVE ls_return-message_v1 TO ls_message-msgv1.
    MOVE ls_return-message_v2 TO ls_message-msgv2.
    MOVE ls_return-message_v3 TO ls_message-msgv3.
    MOVE ls_return-message_v4 TO ls_message-msgv4.
    APPEND ls_message TO et_messages.
    CLEAR  ls_message.
  ENDLOOP.
  IF sy-subrc EQ 0.
    IF i_commit EQ abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.
    RAISE error.
  ENDIF.


** Finaliza
***********************************************************************
  IF i_testrun IS INITIAL AND i_commit EQ abap_true.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    EXIT.
  ENDIF.

** Aguarda por Actualização da Base de Dados
***********************************************************************
  CHECK NOT i_wait IS INITIAL.

  lv_times = i_wait + 1.

  DO lv_times TIMES.
    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.
    SELECT * FROM mseg INTO TABLE et_mseg
     WHERE mblnr EQ e_mblnr
       AND mjahr EQ e_mjahr.

    CHECK sy-subrc = 0.
    lv_flag_bd = 'X'.
    EXIT.
  ENDDO.

  IF lv_flag_bd IS INITIAL.
**  Documento & (&) não encontrado na BD
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = 'YMM001'.
    ls_message-msgnr  = '006'.
    ls_message-msgv1  = e_mblnr.
    ls_message-msgv2  = e_mjahr.
    APPEND ls_message TO et_messages.
    CLEAR  ls_message.
    RAISE error.
  ENDIF.
ENDFUNCTION.
