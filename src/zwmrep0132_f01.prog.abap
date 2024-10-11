*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0132_F01                                             *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  clear
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear.

  CLEAR: cursorfield,
         ok_code_0001.

  PERFORM clear_ecran.

ENDFORM.                    " clear

*&---------------------------------------------------------------------*
*&      Form  clear_ecran
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_ecran.

  CLEAR:scr.

ENDFORM.                    " clear_ecran

*&---------------------------------------------------------------------*
*&      Form  FIND_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_whs.

  setscreen1 = '0001'.
*
*  CLEAR whs.
*  CALL FUNCTION 'L_USER_DATA_GET'
*    EXPORTING
*      i_uname        = sy-uname
*    TABLES
*      t_xuser        = l_user
*    EXCEPTIONS
*      no_entry_found = 1
*      OTHERS         = 2.
*  IF sy-subrc <> 0.
***   raise no_warehouse_found.
*  ELSE.
*    READ TABLE l_user WITH KEY statu = gc_true.  " con_x.
*    IF sy-subrc <> 0.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWMMSG001'
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = '003'
*        IMPORTING
*          ret_code       = resposta.
*      IF resposta = 'O'.
*        LEAVE TO SCREEN '0001'.
*      ENDIF.
*    ELSE.
*      whs = l_user-lgnum.
*      setscreen1 = '0001'.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " FIND_WHS
*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save.


* INETUM - JM - 04.11.2021 - RENPRJ00019 - Inicio
****** COMENTED ******
*  DATA: goodsmvt_header   LIKE bapi2017_gm_head_01,
*        goodsmvt_item
*                         LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE,
*        return            LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
*        return2           LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
*        mblnr             LIKE mkpf-mblnr,
*        gjahr             LIKE mkpf-mjahr,
*        materialdocument1 TYPE  bapi2017_gm_head_ret-mat_doc,
*        matdocumentyear1  TYPE  bapi2017_gm_head_ret-doc_year,
*        materialdocument2 TYPE  bapi2017_gm_head_ret-mat_doc,
*        matdocumentyear2  TYPE  bapi2017_gm_head_ret-doc_year,
*        materialdocument3 TYPE  bapi2017_gm_head_ret-mat_doc,
*        matdocumentyear3  TYPE  bapi2017_gm_head_ret-doc_year,
*        return_msg        LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*
*  DATA: lv_objectkey       TYPE bapi1003_key-object.
*  DATA: lt_allocvaluesnum  TYPE bapi1003_alloc_values_num OCCURS 0 WITH HEADER LINE,
*        lt_allocvalueschar TYPE bapi1003_alloc_values_char OCCURS 0 WITH HEADER LINE,
*        lt_allocvaluescurr TYPE bapi1003_alloc_values_curr OCCURS 0,
*        lt_return          TYPE bapiret2 OCCURS 0,
*        lv_output          TYPE cha_class_view-sollwert.
*
*  code = '03'.
*  IF gv_mnv IS NOT INITIAL.
*** Mov 201 para CC
*    REFRESH: goodsmvt_item, return, return2.
*    CLEAR: materialdocument1, matdocumentyear1, goodsmvt_header,
*           goodsmvt_item.
*
*    MOVE sy-datum TO goodsmvt_header-doc_date.
*    MOVE sy-datum TO goodsmvt_header-pstng_date.
*    MOVE scr-pernr TO goodsmvt_header-header_txt.
*
*    goodsmvt_item-plant      = ls_mchb-werks.
*    goodsmvt_item-stge_loc   = ls_mchb-lgort.
** INETUM - NR - 26.07.2021 - RENPRJ00019 - Inicio
****    MOVE '201' TO goodsmvt_item-move_type.
*    MOVE gs_hardcode-bwart_cc_1 TO goodsmvt_item-move_type.
** INETUM - NR - 26.07.2021 - RENPRJ00019 - Fim
*    goodsmvt_item-entry_uom = scr-meins.
*    goodsmvt_item-entry_uom_iso = goodsmvt_item-entry_uom.
*    goodsmvt_item-material      = scr-matnr.
*    CONCATENATE 'MNV' scr-charg+3(7) INTO goodsmvt_item-batch.
** INETUM - NR - 26.07.2021 - RENPRJ00019 - Inicio
****    goodsmvt_item-entry_qnt     = ls_mchb-clabs.
*    goodsmvt_item-entry_qnt     = scr-menge.
** INETUM - NR - 26.07.2021 - RENPRJ00019 - Fim
*    goodsmvt_item-costcenter   = 'RENIDVGCOM'.
*    APPEND goodsmvt_item.
*
*    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*      EXPORTING
*        goodsmvt_header  = goodsmvt_header
*        goodsmvt_code    = code
*      IMPORTING
*        materialdocument = materialdocument1
*        matdocumentyear  = matdocumentyear1
*      TABLES
*        goodsmvt_item    = goodsmvt_item
*        return           = return.
*
*    CLEAR return_msg.
*    REFRESH return_msg.
*    LOOP AT return WHERE type = 'E' OR type = 'A'.
*      MOVE return-type       TO return_msg-msgtyp.
*      MOVE return-id         TO return_msg-msgid.
*      MOVE return-number     TO return_msg-msgnr.
*      MOVE return-message_v1 TO return_msg-msgv1.
*      MOVE return-message_v2 TO return_msg-msgv2.
*      MOVE return-message_v3 TO return_msg-msgv3.
*      MOVE return-message_v4 TO return_msg-msgv4.
*      APPEND return_msg.
*    ENDLOOP.
*
*    READ TABLE return_msg INDEX 1.
*    IF sy-subrc = 0.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = return_msg-msgid
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = return_msg-msgnr
*          message_var1   = return_msg-msgv1
*          message_var2   = return_msg-msgv2
*          message_var3   = return_msg-msgv3
*          message_var4   = return_msg-msgv4
*        IMPORTING
*          ret_code       = resposta.
*
*      IF resposta = 'O'.
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*        LEAVE TO SCREEN setscreen1.
*      ENDIF.
*    ENDIF.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*
*** Mov 202 para CC
*    REFRESH: goodsmvt_item, return, return2.
*    CLEAR: materialdocument2, matdocumentyear2, goodsmvt_header,
*           goodsmvt_item.
*
*    MOVE sy-datum TO goodsmvt_header-doc_date.
*    MOVE sy-datum TO goodsmvt_header-pstng_date.
*    MOVE scr-pernr TO goodsmvt_header-header_txt.
*
*    goodsmvt_item-plant      = ls_mchb-werks.
*    goodsmvt_item-stge_loc   = ls_mchb-lgort.
** INETUM - NR - 26.07.2021 - RENPRJ00019 - Inicio
****    MOVE '202' TO goodsmvt_item-move_type.
*    MOVE gs_hardcode-bwart_cc_2 TO goodsmvt_item-move_type.
** INETUM - NR - 26.07.2021 - RENPRJ00019 - Fim
*    goodsmvt_item-entry_uom = scr-meins.
*    goodsmvt_item-entry_uom_iso = goodsmvt_item-entry_uom.
*    goodsmvt_item-material      = scr-matnr.
*
*** Obter o lote original
*
*    CONCATENATE 'MNV' scr-charg+3(7) INTO mnv_charg.
*
*    CONCATENATE  scr-matnr ls_mchb-werks mnv_charg INTO lv_objectkey.
*
*    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
*      EXPORTING
*        objectkey       = lv_objectkey
*        objecttable     = 'MCHA'
*        classnum        = 'CLASSIF_LOTES'
*        classtype       = '022'
*      TABLES
*        allocvaluesnum  = lt_allocvaluesnum
*        allocvalueschar = lt_allocvalueschar
*        allocvaluescurr = lt_allocvaluescurr
*        return          = lt_return.
*
*    READ TABLE lt_allocvalueschar WITH KEY charact = 'Z_LOTE_ANTIGO '.
*    IF sy-subrc IS INITIAL AND lt_allocvalueschar-value_char IS NOT INITIAL.
*      goodsmvt_item-batch  = lt_allocvalueschar-value_char.
*    ENDIF.
*
***
*
** INETUM - NR - 26.07.2021 - RENPRJ00019 - Inicio
****    goodsmvt_item-entry_qnt     = ls_mchb-clabs.
*    goodsmvt_item-entry_qnt     = scr-menge.
** INETUM - NR - 26.07.2021 - RENPRJ00019 - Fim
*    goodsmvt_item-costcenter   = 'RENIDVGCOM'.
*    APPEND goodsmvt_item.
*
*    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*      EXPORTING
*        goodsmvt_header  = goodsmvt_header
*        goodsmvt_code    = code
*      IMPORTING
*        materialdocument = materialdocument2
*        matdocumentyear  = matdocumentyear2
*      TABLES
*        goodsmvt_item    = goodsmvt_item
*        return           = return.
*
*    CLEAR return_msg.
*    REFRESH return_msg.
*    LOOP AT return WHERE type = 'E' OR type = 'A'.
*      MOVE return-type       TO return_msg-msgtyp.
*      MOVE return-id         TO return_msg-msgid.
*      MOVE return-number     TO return_msg-msgnr.
*      MOVE return-message_v1 TO return_msg-msgv1.
*      MOVE return-message_v2 TO return_msg-msgv2.
*      MOVE return-message_v3 TO return_msg-msgv3.
*      MOVE return-message_v4 TO return_msg-msgv4.
*      APPEND return_msg.
*    ENDLOOP.
*
*    READ TABLE return_msg INDEX 1.
*    IF sy-subrc = 0.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = return_msg-msgid
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = return_msg-msgnr
*          message_var1   = return_msg-msgv1
*          message_var2   = return_msg-msgv2
*          message_var3   = return_msg-msgv3
*          message_var4   = return_msg-msgv4
*        IMPORTING
*          ret_code       = resposta.
*
*      IF resposta = 'O'.
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*        LEAVE TO SCREEN setscreen1.
*
*        CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
*          EXPORTING
*            materialdocument = materialdocument1
*            matdocumentyear  = matdocumentyear1
*          TABLES
*            return           = return.
*
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*
*        EXIT.
*      ENDIF.
*    ENDIF.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*
*  ENDIF.
*
*** Mov 201 para CC ou 261 para ordem
*  REFRESH: goodsmvt_item, return, return2.
*  CLEAR: materialdocument3, matdocumentyear3, goodsmvt_header,
*         goodsmvt_item.
*
*  MOVE sy-datum TO goodsmvt_header-doc_date.
*  MOVE sy-datum TO goodsmvt_header-pstng_date.
*  MOVE scr-pernr TO goodsmvt_header-header_txt.
*
*  goodsmvt_item-plant      = ls_mchb-werks.
*  goodsmvt_item-stge_loc   = ls_mchb-lgort.
*
*  IF gv_ordem IS NOT INITIAL.
** INETUM - NR - 26.07.2021 - RENPRJ00019 - Inicio
****    MOVE '261' TO goodsmvt_item-move_type.
*    MOVE gs_hardcode-bwart_ord_cc_1 TO goodsmvt_item-move_type.
** INETUM - NR - 26.07.2021 - RENPRJ00019 - Fim
*  ELSEIF gv_ccusto IS NOT INITIAL.
** INETUM - NR - 26.07.2021 - RENPRJ00019 - Inicio
****    MOVE '201' TO goodsmvt_item-move_type.
*    MOVE gs_hardcode-bwart_ord_cc_2 TO goodsmvt_item-move_type.
** INETUM - NR - 26.07.2021 - RENPRJ00019 - Fim
*  ENDIF.
*
*  goodsmvt_item-entry_uom = scr-meins.
*  goodsmvt_item-entry_uom_iso = goodsmvt_item-entry_uom.
*  goodsmvt_item-material      = scr-matnr.
*  goodsmvt_item-batch         = scr-charg.
*  goodsmvt_item-entry_qnt     = scr-menge.
*  IF gv_ordem IS NOT INITIAL.
*    goodsmvt_item-orderid   = scr-cc_ordem.
*  ELSEIF gv_ccusto IS NOT INITIAL.
*    goodsmvt_item-costcenter   = scr-cc_ordem.
*  ENDIF.
*  APPEND goodsmvt_item.
*
*  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*    EXPORTING
*      goodsmvt_header  = goodsmvt_header
*      goodsmvt_code    = code
*    IMPORTING
*      materialdocument = materialdocument3
*      matdocumentyear  = matdocumentyear3
*    TABLES
*      goodsmvt_item    = goodsmvt_item
*      return           = return.
*
*  CLEAR return_msg.
*  REFRESH return_msg.
*  LOOP AT return WHERE type = 'E' OR type = 'A'.
*    MOVE return-type       TO return_msg-msgtyp.
*    MOVE return-id         TO return_msg-msgid.
*    MOVE return-number     TO return_msg-msgnr.
*    MOVE return-message_v1 TO return_msg-msgv1.
*    MOVE return-message_v2 TO return_msg-msgv2.
*    MOVE return-message_v3 TO return_msg-msgv3.
*    MOVE return-message_v4 TO return_msg-msgv4.
*    APPEND return_msg.
*  ENDLOOP.
*
*  READ TABLE return_msg INDEX 1.
*  IF sy-subrc = 0.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = return_msg-msgid
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = return_msg-msgnr
*        message_var1   = return_msg-msgv1
*        message_var2   = return_msg-msgv2
*        message_var3   = return_msg-msgv3
*        message_var4   = return_msg-msgv4
*      IMPORTING
*        ret_code       = resposta.
*
*    IF resposta = 'O'.
*      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*      LEAVE TO SCREEN setscreen1.
*
*      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
*        EXPORTING
*          materialdocument = materialdocument1
*          matdocumentyear  = matdocumentyear1
*        TABLES
*          return           = return.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.
*
*      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
*        EXPORTING
*          materialdocument = materialdocument2
*          matdocumentyear  = matdocumentyear2
*        TABLES
*          return           = return.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.
*
*      EXIT.
*    ENDIF.
*  ENDIF.
*
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*    EXPORTING
*      wait = 'X'.
****** COMENTED ******

  DATA: ls_goitem                 TYPE goitem,
        lv_leave_to_screen_1_flag TYPE flag,
        ls_mnv_docs               TYPE zmigo_badi_mnv_documents,
        lv_order_flag             TYPE flag.

  IF gv_ordem IS NOT INITIAL.
    lv_order_flag = abap_true.
  ENDIF.

  ls_goitem-meins = scr-meins.
  ls_goitem-matnr = scr-matnr.
  ls_goitem-menge = scr-menge.
  ls_goitem-charg = scr-charg.

  CALL FUNCTION 'ZMM_MIGO_MNV'
    EXPORTING
      is_goitem                 = ls_goitem
      iv_migo_flag              = abap_false
      iv_header_txt             = scr-pernr
      iv_order_flag             = lv_order_flag
      iv_ccorder_no             = scr-cc_ordem
    IMPORTING
      ev_leave_to_screen_1_flag = lv_leave_to_screen_1_flag
    CHANGING
      es_mnv_documents          = ls_mnv_docs.

  IF lv_leave_to_screen_1_flag EQ abap_true.
    LEAVE TO SCREEN setscreen1.
  ENDIF.
* INETUM - JM - 04.11.2021 - RENPRJ00019 - Fim

* INETUM - NR - 10.12.2021 - RENPRJ00019 - Inicio
***  PERFORM clear.
  CLEAR: cursorfield, ok_code_0001, scr-matnr, scr-maktx1, scr-maktx2, scr-charg, scr-menge, scr-meins.
* INETUM - NR - 10.12.2021 - RENPRJ00019 - Fim

ENDFORM.                    " SAVE
*&---------------------------------------------------------------------*
*&      Form  GET_CUSTOMIZING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_customizing .

*  CLEAR: plant, lgort_o, lgort_d, code, mov.
*
*  PERFORM get_parameter USING whs
*                             'GERAL'
*                             'PLANT'
*                              valor.
*
*  WRITE valor TO plant LEFT-JUSTIFIED.
*
*  IF sy-tcode = 'ZWM120'.
*    PERFORM get_parameter USING whs
*                               'GERAL'
*                               'LGORTBA'
*                                valor.
*
*    WRITE valor TO lgort_o LEFT-JUSTIFIED.
*
*    PERFORM get_parameter USING whs
*                               'GERAL'
*                               'LGORT'
*                                valor.
*
*    WRITE valor TO lgort_d LEFT-JUSTIFIED.
*
*  ELSEIF sy-tcode = 'ZWM121'.
*
*    PERFORM get_parameter USING whs
*                               'GERAL'
*                               'LGORTRETR'
*                                valor.
*
*    WRITE valor TO lgort_o LEFT-JUSTIFIED.
*
*    PERFORM get_parameter USING whs
*                           'GERAL'
*                           'LGORT'
*                            valor.
*
*    WRITE valor TO lgort_d LEFT-JUSTIFIED.
*
*  ELSEIF sy-tcode = 'ZWM122'.
*
*    PERFORM get_parameter USING whs
*                               'GERAL'
*                               'LGORTBA'
*                                valor.
*
*    WRITE valor TO lgort_d LEFT-JUSTIFIED.
*
*    PERFORM get_parameter USING whs
*                           'GERAL'
*                           'LGORT'
*                            valor.
*
*    WRITE valor TO lgort_o LEFT-JUSTIFIED.
*
*  ELSEIF sy-tcode = 'ZWM123'.
*
*    PERFORM get_parameter USING whs
*                               'GERAL'
*                               'LGORTRETR'
*                                valor.
*
*    WRITE valor TO lgort_d LEFT-JUSTIFIED.
*
*    PERFORM get_parameter USING whs
*                           'GERAL'
*                           'LGORT'
*                            valor.
*
*    WRITE valor TO lgort_o LEFT-JUSTIFIED.
*  ENDIF.
*
*  PERFORM get_parameter USING whs
*                           'MAN_TRANSF'
*                           'COD'
*                            valor.
*
*  WRITE valor TO code LEFT-JUSTIFIED.
*
*  PERFORM get_parameter USING whs
*                       'MAN_TRANSF'
*                       'MOV'
*                        valor.
*
*  WRITE valor TO mov LEFT-JUSTIFIED.

* INETUM - NR - 26.07.2021 - RENPRJ00019 - Inicio
  PERFORM get_hardcodes.
* INETUM - NR - 26.07.2021 - RENPRJ00019 - Fim

ENDFORM.                    " GET_CUSTOMIZING

*&---------------------------------------------------------------------*
*&      Form  get_parameter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->WHS        text
*      -->MODULE     text
*      -->PARAM      text
*      -->VALOR      text
*----------------------------------------------------------------------*
FORM get_parameter  USING    whs
                             module
                             param
                             valor.

*  IF ti_zwm001[] IS INITIAL.
*    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
*      EXPORTING
*        whs       = whs
*      TABLES
*        ti_zwm001 = ti_zwm001.
*  ENDIF.
*
*  CLEAR zwm001.
*  READ TABLE ti_zwm001 WITH KEY      armazem   = whs
*                                     processo  = module
*                                     parametro = param
*                                     BINARY SEARCH.
*  IF sy-subrc = 0.
*    MOVE ti_zwm001 TO zwm001.
*  ENDIF.
*  MOVE zwm001-valor TO valor.

ENDFORM.                    " GET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  GET_HARDCODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_hardcodes .

  DATA: lt_zhardcode_table TYPE STANDARD TABLE OF zhardcode_table.

  DATA: ls_zhardcode_table TYPE zhardcode_table.

  CONSTANTS: lc_inc_meth     TYPE zhardcode_table-inc_meth VALUE 'ZWMREP0132',
             lc_occurrence_1 TYPE zhardcode_table-occurrence VALUE '001',
             lc_occurrence_2 TYPE zhardcode_table-occurrence VALUE '002',
             lc_counter_1    TYPE zhardcode_table-counter VALUE '01',
             lc_counter_2    TYPE zhardcode_table-counter VALUE '02'.

  SELECT *
    FROM zhardcode_table
    INTO TABLE lt_zhardcode_table
    WHERE inc_meth = lc_inc_meth.


  READ TABLE lt_zhardcode_table INTO ls_zhardcode_table WITH KEY occurrence = lc_occurrence_1
                                                                    counter = lc_counter_1.
  IF sy-subrc EQ 0.
    gs_hardcode-bwart_cc_1 = ls_zhardcode_table-ue_low.
  ENDIF.

  CLEAR: ls_zhardcode_table.
  READ TABLE lt_zhardcode_table INTO ls_zhardcode_table WITH KEY occurrence = lc_occurrence_1
                                                                    counter = lc_counter_2.
  IF sy-subrc EQ 0.
    gs_hardcode-bwart_cc_2 = ls_zhardcode_table-ue_low.
  ENDIF.

  CLEAR: ls_zhardcode_table.
  READ TABLE lt_zhardcode_table INTO ls_zhardcode_table WITH KEY occurrence = lc_occurrence_2
                                                                  counter = lc_counter_1.
  IF sy-subrc EQ 0.
    gs_hardcode-bwart_ord_cc_1 = ls_zhardcode_table-ue_low.
  ENDIF.

  CLEAR: ls_zhardcode_table.
  READ TABLE lt_zhardcode_table INTO ls_zhardcode_table WITH KEY occurrence = lc_occurrence_2
                                                                  counter = lc_counter_2.
  IF sy-subrc EQ 0.
    gs_hardcode-bwart_ord_cc_2 = ls_zhardcode_table-ue_low.
  ENDIF.

ENDFORM.
