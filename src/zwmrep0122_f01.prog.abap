*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0122_F01                                             *
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

  CLEAR: aufnr_in,
         ean_in,
         matnr_in,
         menge_in,
         data_in,
         cursorfield,
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

  CLEAR: maktx_out1,
         maktx_out2,
         menge_out,
         meins_out,
         charg_out,
         data_in,
         sscc_out.

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

  CLEAR whs.
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = l_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
**   raise no_warehouse_found.
  ELSE.
    READ TABLE l_user WITH KEY statu = gc_true.  " con_x.
    IF sy-subrc <> 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '003'
        IMPORTING
          ret_code       = resposta.
      IF resposta = 'O'.
        LEAVE TO SCREEN '0001'.
      ENDIF.
    ELSE.
      whs = l_user-lgnum.
      setscreen1 = '0001'.
    ENDIF.
  ENDIF.

ENDFORM.                    " FIND_WHS
*&---------------------------------------------------------------------*
*&      Form  SAVE_MM_WM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_mm_wm .

  DATA: goodsmvt_header LIKE bapi2017_gm_head_01,
        goodsmvt_item
                         LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE,
        return           LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        return2          LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        mblnr            LIKE mkpf-mblnr,
        gjahr            LIKE mkpf-mjahr,
        materialdocument TYPE  bapi2017_gm_head_ret-mat_doc,
        matdocumentyear  TYPE  bapi2017_gm_head_ret-doc_year,
        return_msg       LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

  DATA lt_trite TYPE l03b_trite_t.
  DATA wa_trite LIKE LINE OF lt_trite.

  DATA lt_ltap_create LIKE  ltap_creat OCCURS 0 WITH HEADER LINE.

  DATA: lv_tbnum      LIKE mseg-tbnum,
        lv_tbpos      LIKE mseg-tbpos,
        lv_lhmg1      LIKE mlgn-lhmg1,
        lv_lhme1      LIKE mlgn-lhme1,
        lv_lety1      LIKE mlgn-lety1,
        lv_tanum      LIKE ltak-tanum,
        lv_lznum      LIKE  ltak-lznum,
        lv_benum      LIKE  ltak-benum.

  TYPES: vsep_t_h          TYPE vekp OCCURS 0,
         vsep_t_i          TYPE vepo OCCURS 0,
         vsep_t_his        TYPE vevw OCCURS 0.

  DATA:it_dummy_hdr_insr  TYPE vsep_t_h,
        it_dummy_hdr_del  TYPE vsep_t_h,
        it_dummy_itm_insr TYPE vsep_t_i,
        it_itm_update     TYPE  vsep_t_i,
        it_dummy_itm_del  TYPE vsep_t_i,
        it_dummy_his_insr TYPE vsep_t_his,
        it_dummy_his_upd  TYPE vsep_t_his,
        it_dummy_his_del  TYPE vsep_t_his.

  DATA it_upd_vekp TYPE vsep_t_h.
  DATA wa_upd_vekp LIKE LINE OF it_upd_vekp.

  REFRESH: goodsmvt_item, return, return2, lt_ltap_create.
  CLEAR: materialdocument, matdocumentyear, goodsmvt_header,
         goodsmvt_item, lt_ltap_create, lv_lznum, lv_benum.

  MOVE data_in TO goodsmvt_header-doc_date.
  MOVE data_in TO goodsmvt_header-pstng_date.
  MOVE sscc_out TO goodsmvt_header-header_txt.

  goodsmvt_item-plant      = plant.
  goodsmvt_item-stge_loc   = lgort.
  goodsmvt_item-orderid    = aufnr_in.

  MOVE mov TO goodsmvt_item-move_type.

  SELECT SINGLE meins
        FROM mara
        INTO (goodsmvt_item-entry_uom)
        WHERE matnr = matnr_in.

  goodsmvt_item-entry_uom_iso = goodsmvt_item-entry_uom.
  goodsmvt_item-mvt_ind       = ' '.
  goodsmvt_item-material      = matnr_in.
  goodsmvt_item-batch         = charg_out.
  goodsmvt_item-entry_qnt     = menge_in.

  APPEND goodsmvt_item.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = goodsmvt_header
      goodsmvt_code    = code
    IMPORTING
      materialdocument = materialdocument
      matdocumentyear  = matdocumentyear
    TABLES
      goodsmvt_item    = goodsmvt_item
      return           = return.

  CLEAR return_msg.
  REFRESH return_msg.
  LOOP AT return WHERE type = 'E' OR type = 'A'.
    MOVE return-type       TO return_msg-msgtyp.
    MOVE return-id         TO return_msg-msgid.
    MOVE return-number     TO return_msg-msgnr.
    MOVE return-message_v1 TO return_msg-msgv1.
    MOVE return-message_v2 TO return_msg-msgv2.
    MOVE return-message_v3 TO return_msg-msgv3.
    MOVE return-message_v4 TO return_msg-msgv4.
    APPEND return_msg.
  ENDLOOP.

  READ TABLE return_msg INDEX 1.
  IF sy-subrc = 0.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = return_msg-msgid
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = return_msg-msgnr
        message_var1   = return_msg-msgv1
        message_var2   = return_msg-msgv2
        message_var3   = return_msg-msgv3
        message_var4   = return_msg-msgv4
      IMPORTING
        ret_code       = resposta.

    IF resposta = 'O'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
        IMPORTING
          return = return.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

  CLEAR vekp.
  SELECT SINGLE * FROM vekp WHERE exidv = sscc_out.

  IF NOT vekp-status IS INITIAL AND
     NOT vekp-hu_lgort IS INITIAL.

    CLEAR wa_upd_vekp.
    CLEAR: vekp-status, vekp-hu_lgort.
    MOVE-CORRESPONDING vekp TO wa_upd_vekp.
    APPEND wa_upd_vekp TO it_upd_vekp .


    CALL FUNCTION 'V51S_HU_UPDATE_DB'
      EXPORTING
        it_hdr_insert = it_dummy_hdr_insr
        it_hdr_update = it_upd_vekp
        it_hdr_delete = it_dummy_hdr_del
        it_itm_insert = it_dummy_itm_insr
        it_itm_update = it_itm_update
        it_itm_delete = it_dummy_itm_del
        it_his_insert = it_dummy_his_insr
        it_his_update = it_dummy_his_upd
        it_his_delete = it_dummy_his_del.
    COMMIT WORK.
  ENDIF.

  CLEAR lt_ltap_create.

  MOVE plant TO lt_ltap_create-werks.
  MOVE lgort TO lt_ltap_create-lgort.

  SELECT SINGLE lhmg1 lhme1 lety1
    INTO (lv_lhmg1, lv_lhme1, lv_lety1)
        FROM mlgn
            WHERE lgnum = whs
              AND matnr = matnr_in.

  MOVE lv_lety1 TO lt_ltap_create-letyp.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = sscc_out
    IMPORTING
      output = lt_ltap_create-nlenr.

  MOVE matnr_in  TO  lt_ltap_create-matnr.
  MOVE charg_out TO  lt_ltap_create-charg.
  MOVE menge_in  TO  lt_ltap_create-anfme.
  MOVE meins_out TO  lt_ltap_create-altme.

  APPEND lt_ltap_create.

  MOVE aufnr_in TO lv_lznum.
  MOVE lv_lznum(10) TO lv_benum.

  CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
    EXPORTING
      i_lgnum       = whs
      i_bwlvs       = '262'
      i_betyp       = 'F'
      i_benum       = lv_benum
      i_lznum       = lv_lznum
      i_commit_work = ' '
    IMPORTING
      e_tanum       = lv_tanum
    TABLES
      t_ltap_creat  = lt_ltap_create
    EXCEPTIONS
      error_message = 99.

  IF sy-subrc <> 0 OR lv_tanum IS INITIAL.

    MOVE sy-msgty TO return_msg-msgtyp.
    MOVE sy-msgid TO return_msg-msgid.
    MOVE sy-msgno TO return_msg-msgnr.
    MOVE sy-langu TO return_msg-msgspra.
    MOVE sy-msgv1 TO return_msg-msgv1.
    MOVE sy-msgv2 TO return_msg-msgv2.
    MOVE sy-msgv3 TO return_msg-msgv3.
    MOVE sy-msgv4 TO return_msg-msgv4.
    APPEND return_msg.

    READ TABLE return_msg INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = return_msg-msgid
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = return_msg-msgnr
          message_var1   = return_msg-msgv1
          message_var2   = return_msg-msgv2
          message_var3   = return_msg-msgv3
          message_var4   = return_msg-msgv4
        IMPORTING
          ret_code       = resposta.

      IF resposta = 'O'.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
          IMPORTING
            return = return.

        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    PERFORM clear.
  ENDIF.
ENDFORM.                    " SAVE_MM_WM
*&---------------------------------------------------------------------*
*&      Form  GET_CUSTOMIZING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_customizing .

  CLEAR: plant, lgort, code, mov.

  PERFORM get_parameter USING whs
                             'GERAL'
                             'PLANT'
                              valor.

  WRITE valor TO plant LEFT-JUSTIFIED.

  PERFORM get_parameter USING whs
                             'GERAL'
                             'LGORT'
                              valor.

  WRITE valor TO lgort LEFT-JUSTIFIED.


  PERFORM get_parameter USING whs
                           'MAN_RETORNOS'
                           'COD'
                            valor.

  WRITE valor TO code LEFT-JUSTIFIED.

  PERFORM get_parameter USING whs
                       'MAN_RETORNOS'
                       'MOV'
                        valor.

  WRITE valor TO mov LEFT-JUSTIFIED.
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

  IF ti_zwm001[] IS INITIAL.
    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
      EXPORTING
        whs       = whs
      TABLES
        ti_zwm001 = ti_zwm001.
  ENDIF.

  CLEAR zwm001.
  READ TABLE ti_zwm001 WITH KEY      armazem   = whs
                                     processo  = module
                                     parametro = param
                                     BINARY SEARCH.
  IF sy-subrc = 0.
    MOVE ti_zwm001 TO zwm001.
  ENDIF.
  MOVE zwm001-valor TO valor.

ENDFORM.                    " GET_PARAMETER
