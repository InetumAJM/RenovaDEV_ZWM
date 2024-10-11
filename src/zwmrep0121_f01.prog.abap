*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0121_F01                                             *
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
         sscc_in,
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

  CLEAR:
        matnr_out ,
        maktx_out1,
        maktx_out2,
        menge_out,
        meins_out,
        charg_out,
        data_in.

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
      setscreen2 = '0002'.
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

  DATA: goodsmvt_header  LIKE bapi2017_gm_head_01,
        mblnr            LIKE mkpf-mblnr,
        gjahr            LIKE mkpf-mjahr,
        materialdocument TYPE  bapi2017_gm_head_ret-mat_doc,
        matdocumentyear  TYPE  bapi2017_gm_head_ret-doc_year.


  DATA: goodsmvt_item LIKE bapi2017_gm_item_create
                                            OCCURS 0 WITH HEADER LINE,

        return        LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        return2       LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        return_msg    LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

  DATA lt_trite       TYPE l03b_trite_t.
  DATA wa_trite       LIKE LINE OF lt_trite.
  DATA lt_ltap_create LIKE  ltap_creat OCCURS 0 WITH HEADER LINE.


  DATA: lv_tbnum      LIKE mseg-tbnum,
        lv_tbpos      LIKE mseg-tbpos,
        lv_lhmg1      LIKE mlgn-lhmg1,
        lv_lhme1      LIKE mlgn-lhme1,
        lv_lety1      LIKE mlgn-lety1,
        lv_tanum      LIKE ltak-tanum,
        lv_lznum      LIKE  ltak-lznum,
        lv_benum      LIKE  ltak-benum.


  REFRESH: goodsmvt_item, return, return2.
  CLEAR: materialdocument, matdocumentyear, goodsmvt_header,
         goodsmvt_item.

**********************************************************************
** MM - Movimento Material
**********************************************************************

** Cabeçalho
  MOVE data_in TO goodsmvt_header-doc_date.
  MOVE data_in TO goodsmvt_header-pstng_date.
  MOVE sscc_in TO goodsmvt_header-header_txt.
** Item
  goodsmvt_item-plant      = plant.
  goodsmvt_item-stge_loc   = lgort.
  goodsmvt_item-move_type  = mov.
  goodsmvt_item-orderid    = aufnr_in.

  SELECT SINGLE meins
        FROM mara
        INTO (goodsmvt_item-entry_uom)
        WHERE matnr = matnr_out.

  goodsmvt_item-entry_uom_iso = goodsmvt_item-entry_uom.
  goodsmvt_item-mvt_ind       = ' '.
  goodsmvt_item-material      = matnr_out.
  goodsmvt_item-batch         = charg_out.
  goodsmvt_item-entry_qnt     = menge_out.

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

***********************************************************************
*** WM - Criação TO
***********************************************************************

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
*
*  CLEAR: lv_tbnum, lv_tbpos, lv_lhmg1, lv_lety1,
*         lt_trite, wa_trite.
*
*  REFRESH: lt_trite.
*
*  DO 20 TIMES.
*    SELECT SINGLE tbnum tbpos
*        INTO (lv_tbnum, lv_tbpos)
*            FROM mseg
*                WHERE mblnr = materialdocument
*                  AND mjahr = matdocumentyear
*                  AND tbnum <> ''.
*
*    IF NOT lv_tbnum IS INITIAL AND NOT lv_tbpos IS INITIAL.
*      EXIT.
*    ELSE.
*      WAIT UP TO 1 SECONDS.
*    ENDIF.
*  ENDDO.
*
*
*  wa_trite-tbpos = lv_tbpos.
*  wa_trite-anfme = menge_out.
*  wa_trite-charg = charg_out.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = sscc_in
*    IMPORTING
*      output = wa_trite-vlenr.
*
*  SELECT SINGLE lhmg1 lhme1 lety1
*      INTO (lv_lhmg1, lv_lhme1, lv_lety1)
*          FROM mlgn
*              WHERE lgnum = whs
*                AND matnr = matnr_out.
*
*  wa_trite-altme = meins_out.
*  wa_trite-letyp = lv_lety1.
*
*  APPEND wa_trite TO lt_trite.
*  CALL FUNCTION 'L_TO_CREATE_TR'
*    EXPORTING
*      i_lgnum       = whs
*      i_tbnum       = lv_tbnum
*      it_trite      = lt_trite
*    IMPORTING
*      e_tanum       = lv_tanum
*    EXCEPTIONS
*      error_message = 99.


  CLEAR lt_ltap_create.

  MOVE plant TO lt_ltap_create-werks.
  MOVE lgort TO lt_ltap_create-lgort.

  SELECT SINGLE lhmg1 lhme1 lety1
    INTO (lv_lhmg1, lv_lhme1, lv_lety1)
        FROM mlgn
            WHERE lgnum = whs
              AND matnr = matnr_out.


  MOVE lv_lety1 TO lt_ltap_create-letyp.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = sscc_in
    IMPORTING
      output = lt_ltap_create-vlenr.

  MOVE matnr_out TO  lt_ltap_create-matnr.
  MOVE charg_out TO  lt_ltap_create-charg.
  MOVE menge_out TO  lt_ltap_create-anfme.
  MOVE meins_out TO  lt_ltap_create-altme.

  APPEND lt_ltap_create.

  MOVE aufnr_in TO lv_lznum.
  MOVE lv_lznum(10) TO lv_benum.

  CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
    EXPORTING
      i_lgnum       = whs
      i_bwlvs       = '261'
      i_betyp       = 'F'
      i_benum       = lv_benum
      i_lznum       = lv_lznum
*      i_commit_work = ' '
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

** ESTORNA DOC MATERIAL
      IF resposta = 'O'.
        CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
          EXPORTING
            materialdocument = materialdocument
            matdocumentyear  = matdocumentyear
          TABLES
            return           = return.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*          IMPORTING
*            return = return.


        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ENDIF.
  ELSE.

*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.

    CLEAR:  sscc_in, cursorfield, ok_code_0001.
    PERFORM clear_ecran.
    MOVE 'SSCC_IN' TO cursorfield.
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

** Centro
  PERFORM get_parameter USING whs
                             'GERAL'
                             'PLANT'
                              valor.

  WRITE valor TO plant LEFT-JUSTIFIED.

** Depósito
  PERFORM get_parameter USING whs
                             'GERAL'
                             'LGORT'
                              valor.

  WRITE valor TO lgort LEFT-JUSTIFIED.

** Código BAPI
  PERFORM get_parameter USING whs
                           'MAN_CONSUMOS'
                           'COD'
                            valor.

  WRITE valor TO code LEFT-JUSTIFIED.

** Tipo movimento BAPI
  PERFORM get_parameter USING whs
                       'MAN_CONSUMOS'
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
*&---------------------------------------------------------------------*
*&      Form  CLEAR_FIELDS2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_fields2 .

  CLEAR: ok_code_0002,
         cursorfield2,
         data_in,
         sscc2_out,
         matnr2_in,
         maktx2_out1,
         maktx2_out2,
         meins2_out,
         charg2_out,
         menge2_out,
         current_item,
         total_items.

  CLEAR gt_lqua. REFRESH gt_lqua.

ENDFORM.                    " CLEAR_FIELDS2
*&---------------------------------------------------------------------*
*&      Form  CARREGA_SSCC_INCOMPLETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_sscc_incompletos.

  CLEAR gt_lqua.
  REFRESH gt_lqua.

  SELECT *
    FROM lqua INTO TABLE gt_lqua
    WHERE matnr = matnr2_in AND
          lgnum = whs AND
          lgtyp = 'MAN'.

  SELECT SINGLE *
    FROM mlgn
    WHERE matnr = matnr2_in AND
          lgnum = whs.

  CHECK sy-subrc = 0.

  DELETE gt_lqua WHERE verme >= mlgn-lhmg1.

  IF gt_lqua[] IS NOT INITIAL.
    current_item = 1.
    DESCRIBE TABLE gt_lqua LINES total_items.
  ENDIF.

ENDFORM.                    " CARREGA_SSCC_INCOMPLETOS
*&---------------------------------------------------------------------*
*&      Form  CARREGA_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_item .

  CHECK gt_lqua[] IS NOT INITIAL AND current_item IS NOT INITIAL.

  READ TABLE gt_lqua INDEX current_item.
  IF sy-subrc = 0.
    MOVE gt_lqua-matnr TO matnr2_in.
    MOVE gt_lqua-lenum TO sscc2_out.

    SELECT SINGLE * FROM makt
      WHERE matnr = matnr2_in
        AND spras = sy-langu.

    maktx2_out1 = makt-maktx(20).
    maktx2_out2 = makt-maktx+20(20).

    MOVE gt_lqua-charg TO charg2_out.
    MOVE gt_lqua-verme TO menge2_out.
    MOVE gt_lqua-meins TO meins2_out.
  ENDIF.


ENDFORM.                    " CARREGA_ITEM
