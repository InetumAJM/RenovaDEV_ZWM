*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0136_F01                                             *
*----------------------------------------------------------------------*
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
    LEAVE TO SCREEN 0.
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
        LEAVE TO SCREEN 0.
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

*  DATA: goodsmvt_header  LIKE bapi2017_gm_head_01,
*        mblnr            LIKE mkpf-mblnr,
*        gjahr            LIKE mkpf-mjahr,
*        materialdocument TYPE  bapi2017_gm_head_ret-mat_doc,
*        matdocumentyear  TYPE  bapi2017_gm_head_ret-doc_year.
*
*
*  DATA: goodsmvt_item LIKE bapi2017_gm_item_create
*                                            OCCURS 0 WITH HEADER LINE,
*
*        return        LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
*        return2       LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
*        return_msg    LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*
*  DATA lt_trite       TYPE l03b_trite_t.
*  DATA wa_trite       LIKE LINE OF lt_trite.
*  DATA lt_ltap_create LIKE  ltap_creat OCCURS 0 WITH HEADER LINE.
*
*
*  DATA: lv_tbnum      LIKE mseg-tbnum,
*        lv_tbpos      LIKE mseg-tbpos,
*        lv_lhmg1      LIKE mlgn-lhmg1,
*        lv_lhme1      LIKE mlgn-lhme1,
*        lv_lety1      LIKE mlgn-lety1,
*        lv_tanum      LIKE ltak-tanum,
*        lv_lznum      LIKE  ltak-lznum,
*        lv_benum      LIKE  ltak-benum.
*
*
*  REFRESH: goodsmvt_item, return, return2.
*  CLEAR: materialdocument, matdocumentyear, goodsmvt_header,
*         goodsmvt_item.
*
***********************************************************************
*** MM - Movimento Material
***********************************************************************
*
*** Cabeçalho
*  MOVE data_in TO goodsmvt_header-doc_date.
*  MOVE data_in TO goodsmvt_header-pstng_date.
*  MOVE sscc_in TO goodsmvt_header-header_txt.
*** Item
*  goodsmvt_item-plant      = plant.
*  goodsmvt_item-stge_loc   = lgort.
*  goodsmvt_item-move_type  = mov.
*  goodsmvt_item-orderid    = aufnr_in.
*
*  SELECT SINGLE meins
*        FROM mara
*        INTO (goodsmvt_item-entry_uom)
*        WHERE matnr = matnr_out.
*
*  goodsmvt_item-entry_uom_iso = goodsmvt_item-entry_uom.
*  goodsmvt_item-mvt_ind       = ' '.
*  goodsmvt_item-material      = matnr_out.
*  goodsmvt_item-batch         = charg_out.
*  goodsmvt_item-entry_qnt     = menge_out.
*
*  APPEND goodsmvt_item.
*
*  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*    EXPORTING
*      goodsmvt_header  = goodsmvt_header
*      goodsmvt_code    = code
*    IMPORTING
*      materialdocument = materialdocument
*      matdocumentyear  = matdocumentyear
*    TABLES
*      goodsmvt_item    = goodsmvt_item
*      return           = return.
*
*  CLEAR return_msg.
*  REFRESH return_msg.
*
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
*      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*        IMPORTING
*          return = return.
*
*      LEAVE TO SCREEN setscreen1.
*    ENDIF.
*  ENDIF.
*
************************************************************************
**** WM - Criação TO
************************************************************************
*
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*    EXPORTING
*      wait = 'X'.
**
**  CLEAR: lv_tbnum, lv_tbpos, lv_lhmg1, lv_lety1,
**         lt_trite, wa_trite.
**
**  REFRESH: lt_trite.
**
**  DO 20 TIMES.
**    SELECT SINGLE tbnum tbpos
**        INTO (lv_tbnum, lv_tbpos)
**            FROM mseg
**                WHERE mblnr = materialdocument
**                  AND mjahr = matdocumentyear
**                  AND tbnum <> ''.
**
**    IF NOT lv_tbnum IS INITIAL AND NOT lv_tbpos IS INITIAL.
**      EXIT.
**    ELSE.
**      WAIT UP TO 1 SECONDS.
**    ENDIF.
**  ENDDO.
**
**
**  wa_trite-tbpos = lv_tbpos.
**  wa_trite-anfme = menge_out.
**  wa_trite-charg = charg_out.
**
**  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
**    EXPORTING
**      input  = sscc_in
**    IMPORTING
**      output = wa_trite-vlenr.
**
**  SELECT SINGLE lhmg1 lhme1 lety1
**      INTO (lv_lhmg1, lv_lhme1, lv_lety1)
**          FROM mlgn
**              WHERE lgnum = whs
**                AND matnr = matnr_out.
**
**  wa_trite-altme = meins_out.
**  wa_trite-letyp = lv_lety1.
**
**  APPEND wa_trite TO lt_trite.
**  CALL FUNCTION 'L_TO_CREATE_TR'
**    EXPORTING
**      i_lgnum       = whs
**      i_tbnum       = lv_tbnum
**      it_trite      = lt_trite
**    IMPORTING
**      e_tanum       = lv_tanum
**    EXCEPTIONS
**      error_message = 99.
*
*
*  CLEAR lt_ltap_create.
*
*  MOVE plant TO lt_ltap_create-werks.
*  MOVE lgort TO lt_ltap_create-lgort.
*
*  SELECT SINGLE lhmg1 lhme1 lety1
*    INTO (lv_lhmg1, lv_lhme1, lv_lety1)
*        FROM mlgn
*            WHERE lgnum = whs
*              AND matnr = matnr_out.
*
*
*  MOVE lv_lety1 TO lt_ltap_create-letyp.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = sscc_in
*    IMPORTING
*      output = lt_ltap_create-vlenr.
*
*  MOVE matnr_out TO  lt_ltap_create-matnr.
*  MOVE charg_out TO  lt_ltap_create-charg.
*  MOVE menge_out TO  lt_ltap_create-anfme.
*  MOVE meins_out TO  lt_ltap_create-altme.
*
*  APPEND lt_ltap_create.
*
*  MOVE aufnr_in TO lv_lznum.
*  MOVE lv_lznum(10) TO lv_benum.
*
*  CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
*    EXPORTING
*      i_lgnum       = whs
*      i_bwlvs       = '261'
*      i_betyp       = 'F'
*      i_benum       = lv_benum
*      i_lznum       = lv_lznum
**      i_commit_work = ' '
*    IMPORTING
*      e_tanum       = lv_tanum
*    TABLES
*      t_ltap_creat  = lt_ltap_create
*    EXCEPTIONS
*      error_message = 99.
*
*  IF sy-subrc <> 0 OR lv_tanum IS INITIAL.
*
*    MOVE sy-msgty TO return_msg-msgtyp.
*    MOVE sy-msgid TO return_msg-msgid.
*    MOVE sy-msgno TO return_msg-msgnr.
*    MOVE sy-langu TO return_msg-msgspra.
*    MOVE sy-msgv1 TO return_msg-msgv1.
*    MOVE sy-msgv2 TO return_msg-msgv2.
*    MOVE sy-msgv3 TO return_msg-msgv3.
*    MOVE sy-msgv4 TO return_msg-msgv4.
*    APPEND return_msg.
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
*** ESTORNA DOC MATERIAL
*      IF resposta = 'O'.
*        CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
*          EXPORTING
*            materialdocument = materialdocument
*            matdocumentyear  = matdocumentyear
*          TABLES
*            return           = return.
*
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*
**        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
**          IMPORTING
**            return = return.
*
*
*        LEAVE TO SCREEN setscreen1.
*      ENDIF.
*    ENDIF.
*  ELSE.
*
**    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
**      EXPORTING
**        wait = 'X'.
*
*    CLEAR:  sscc_in, cursorfield, ok_code_0001.
*    PERFORM clear_ecran.
*    MOVE 'SSCC_IN' TO cursorfield.
*  ENDIF.

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

*  CLEAR: plant, lgort, code, mov.
*
*** Centro
*  PERFORM get_parameter USING whs
*                             'GERAL'
*                             'PLANT'
*                              valor.
*
*  WRITE valor TO plant LEFT-JUSTIFIED.
*
*** Depósito
*  PERFORM get_parameter USING whs
*                             'GERAL'
*                             'LGORT'
*                              valor.
*
*  WRITE valor TO lgort LEFT-JUSTIFIED.
*
*** Código BAPI
*  PERFORM get_parameter USING whs
*                           'MAN_CONSUMOS'
*                           'COD'
*                            valor.
*
*  WRITE valor TO code LEFT-JUSTIFIED.
*
*** Tipo movimento BAPI
*  PERFORM get_parameter USING whs
*                       'MAN_CONSUMOS'
*                       'MOV'
*                        valor.
*
*  WRITE valor TO mov LEFT-JUSTIFIED.
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
*&      Form  CHECK_REFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_refnr.

  DATA: ls_vbsk TYPE vbsk.

** Validar Grupo
**********************************************************************
  CHECK scr1-refnr IS NOT INITIAL.

  SELECT SINGLE *
    FROM vbsk INTO ls_vbsk
    WHERE sammg = scr1-refnr.

  IF sy-subrc <> 0.
    text1 = scr1-refnr.

    " Grupo & inválido!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '308'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.

    CLEAR scr1.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

ENDFORM.                    " CHECK_REFNR
*&---------------------------------------------------------------------*
*&      Form  CHECK_VBELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vbeln.

  DATA: lv_subrc TYPE sy-subrc.

  DATA: ls_vbss  TYPE vbss.
  DATA: ls_likp  TYPE likp.
  DATA: ls_vbuk  TYPE vbuk.

  DATA: lt_lips  TYPE STANDARD TABLE OF lips WITH HEADER LINE.

** Validar Remessa
**********************************************************************
  CHECK scr1-vbeln IS NOT INITIAL.

  SELECT SINGLE *
    FROM vbss INTO ls_vbss
    WHERE sammg = scr1-refnr AND
          vbeln = scr1-vbeln.

  IF sy-subrc <> 0.
    text1 = scr1-vbeln.
    text2 = scr1-refnr.

    " Remessa & não pertence ao grupo &!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '309'
        message_var1   = text1
        message_var2   = text2
      IMPORTING
        ret_code       = resposta.

    CLEAR scr1-vbeln.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Obter dados da remessa
  SELECT SINGLE *
    FROM likp INTO ls_likp
    WHERE vbeln = scr1-vbeln.

  IF sy-subrc <> 0.
    text1 = scr1-vbeln.

    " Erro! Remessa & não existe!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '061'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.

    CLEAR scr1-vbeln.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Validar status Picking
  SELECT SINGLE *
    FROM vbuk INTO ls_vbuk
    WHERE vbeln = scr1-vbeln.

  IF ls_vbuk-kostk = 'C'.
    text1 = scr1-vbeln.

    " Remessa & com picking completo.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '310'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.

*    CLEAR scr1-vbeln.
*    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Bloqueio da remessa
**********************************************************************
  CONCATENATE 'ZWMREP0136' scr1-vbeln INTO gv_key SEPARATED BY '_'.

  PERFORM enqueue USING gv_key CHANGING lv_subrc.
  IF lv_subrc <> 0.

    text1 = sy-msgv1.
    text2 = scr1-vbeln.

    " Utilizador & está a processar remessa &.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '316'
        message_var1   = text1
        message_var2   = text2
      IMPORTING
        ret_code       = resposta.

    CLEAR scr1-vbeln.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

ENDFORM.                    " CHECK_VBELN
*&---------------------------------------------------------------------*
*&      Form  CHECK_SSCC_IN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_sscc_in.

  DATA: lv_tabix     TYPE sy-tabix.
  DATA: lv_lines     TYPE i.
  DATA: lv_menge     TYPE menge_d.
  DATA: lv_menge_tot TYPE menge_d.
  DATA: lv_menge_pal TYPE menge_d.
  DATA: lv_menge_rem TYPE menge_d.
  DATA: lv_sscc      TYPE exidv.

  DATA: ls_lein      TYPE lein.
  DATA: ls_vekp      TYPE vekp.
  DATA: ls_vekp_069  TYPE vekp.
  DATA: ls_zwm069    TYPE zwm069.
  DATA: ls_zwm020    TYPE zwm020.

  DATA: lt_vekp_069  TYPE vekp   OCCURS 0 WITH HEADER LINE.
  DATA: lt_vepo      TYPE vepo   OCCURS 0 WITH HEADER LINE.
  DATA: lt_vepo_069  TYPE vepo   OCCURS 0 WITH HEADER LINE.
  DATA: lt_vbfa      TYPE vbfa   OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm069    TYPE zwm069 OCCURS 0 WITH HEADER LINE.
  DATA: lt_paletes   TYPE ltap   OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF lt_lips OCCURS 0.
          INCLUDE STRUCTURE lips.
  DATA: pikmg	TYPE rfmng.
  DATA END OF lt_lips.

** Validar SSCC
**********************************************************************
  CHECK NOT scr1-sscc_in IS INITIAL.

** Validar se Palete é remontada
  SELECT SINGLE *
    FROM zwm020 INTO ls_zwm020
    WHERE p1 = scr1-sscc_in OR
          p2 = scr1-sscc_in.

  IF sy-subrc = 0.

    lt_paletes-vlenr = ls_zwm020-p1.
    APPEND lt_paletes.

    lt_paletes-vlenr = ls_zwm020-p2.
    APPEND lt_paletes.

    "Palete remontada. Deseja só efetuar o picking de uma palete?
    CLEAR resposta.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'W'
        message_number = '329'
      IMPORTING
        ret_code       = resposta.

    IF resposta = 'O'.
      READ TABLE lt_paletes INDEX 1.
      IF sy-subrc = 0.
        lv_sscc = lt_paletes-vlenr.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = lv_sscc
          IMPORTING
            output = lv_sscc.

        WRITE lv_sscc TO text1 LEFT-JUSTIFIED.
      ENDIF.

      READ TABLE lt_paletes INDEX 2.
      IF sy-subrc = 0.
        lv_sscc = lt_paletes-vlenr.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = lv_sscc
          IMPORTING
            output = lv_sscc.

        WRITE lv_sscc TO text2 LEFT-JUSTIFIED.
      ENDIF.

      CLEAR resposta.

      " Escolher (SIM) 1- SSCC & / (NAO) 2- SSCC &.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'W'
          message_number = '333'
          message_var1   = text1
          message_var2   = text2
        IMPORTING
          ret_code       = resposta.

      IF resposta = 'O'.
        READ TABLE lt_paletes INDEX 1.
        IF sy-subrc = 0.
          DELETE lt_paletes INDEX 2. " Eliminar palete de cima.
        ENDIF.

      ELSE.
        READ TABLE lt_paletes INDEX 2.
        IF sy-subrc = 0.
          DELETE lt_paletes INDEX 1. " Eliminar palete de baixo
        ENDIF.
      ENDIF.
    ENDIF.

*      IF ls_zwm020-p1 = scr1-sscc_in.
*
*        lt_paletes-vlenr = ls_zwm020-p1.
*        APPEND lt_paletes.
*
*        lt_paletes-vlenr = ls_zwm020-p2.
*        APPEND lt_paletes.
*
*      ELSEIF ls_zwm020-p2 = scr1-sscc_in.
*        text1 = ls_zwm020-p1.
*
*        " Palete remontada. Picar SSCC & de palete de baixo.
*        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*          EXPORTING
*            message_id     = 'ZWMMSG001'
*            message_lang   = sy-langu
*            message_type   = 'E'
*            message_number = '320'
*            message_var1   = text1
*          IMPORTING
*            ret_code       = resposta.
*
*        CLEAR scr1-sscc_in.
*        LEAVE TO SCREEN setscreen1.
*      ENDIF.

  ELSE.
    lt_paletes-vlenr = scr1-sscc_in.
    APPEND lt_paletes.
  ENDIF.

** Obter dados da palete
  LOOP AT lt_paletes.
    lv_tabix = sy-tabix.

    SELECT SINGLE *
      FROM vekp INTO ls_vekp
      WHERE exidv = lt_paletes-vlenr.

    IF ls_vekp-vpobj = '01'.
      text1 = lt_paletes-vlenr.
      text2 = ls_vekp-vpobjkey.

      " Palete & já se foi embalada na remessa &!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '332'
          message_var1   = text1
          message_var2   = text2
        IMPORTING
          ret_code       = resposta.

      CLEAR scr1-sscc_in.
      LEAVE TO SCREEN setscreen1.
    ENDIF.

    SELECT *
      FROM vepo INTO TABLE lt_vepo
      WHERE venum = ls_vekp-venum.

    READ TABLE lt_vepo INDEX 1.
    IF sy-subrc <> 0.
      text1 = lt_paletes-vlenr.

      " SSCC & inválido!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '113'
          message_var1   = text1
        IMPORTING
          ret_code       = resposta.

      CLEAR scr1-sscc_in.
      LEAVE TO SCREEN setscreen1.
    ENDIF.

** Validar se existe em armazém
    SELECT SINGLE *
      FROM lein INTO ls_lein
      WHERE lenum = lt_paletes-vlenr.

    IF sy-subrc = 0.
      text1 = ls_lein-lenum.
      text2 = ls_lein-lgnum.

      " 311- Palete & pertence ao armazém &.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '311'
          message_var1   = text1
          message_var2   = text2
        IMPORTING
          ret_code       = resposta.

      CLEAR scr1-sscc_in.
      LEAVE TO SCREEN setscreen1.
    ENDIF.

** Validar se palete foi contabilizada
    READ TABLE gt_zwm069 WITH KEY sscc = lt_paletes-vlenr.
    IF sy-subrc = 0.
      text1 = lt_paletes-vlenr.

      " Palete & já foi contabilizada!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '313'
          message_var1   = text1
        IMPORTING
          ret_code       = resposta.

      CLEAR scr1-sscc_in.
      LEAVE TO SCREEN setscreen1.
    ENDIF.

    SELECT SINGLE *
      FROM zwm069 INTO ls_zwm069
      WHERE sscc = lt_paletes-vlenr.

    IF sy-subrc = 0.
      text1 = lt_paletes-vlenr.
      text2 = ls_zwm069-vbeln.

      " Palete & já foi contabilizada na remessa &.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '314'
          message_var1   = text1
          message_var2   = text2
        IMPORTING
          ret_code       = resposta.

      CLEAR scr1-sscc_in.
      LEAVE TO SCREEN setscreen1.
    ENDIF.

    " Quantidade total das paletes
    lv_menge_tot = lv_menge_tot + lt_vepo-vemng.

    lt_paletes-vsolm = lt_vepo-vemng.

    MODIFY lt_paletes INDEX lv_tabix.
  ENDLOOP.

** Quantidade de Paletes já registadas para picking
**********************************************************************
  lt_zwm069[] = gt_zwm069[].

  DELETE lt_zwm069 WHERE picking IS NOT INITIAL.

  IF lt_zwm069[] IS NOT INITIAL.
    SELECT *
      FROM vekp INTO TABLE lt_vekp_069
      FOR ALL ENTRIES IN lt_zwm069
      WHERE exidv = lt_zwm069-sscc.
  ENDIF.

  IF lt_vekp_069[] IS NOT INITIAL.
    SELECT *
      FROM vepo INTO TABLE lt_vepo_069
      FOR ALL ENTRIES IN lt_vekp_069
      WHERE venum = lt_vekp_069-venum.
  ENDIF.

  DELETE lt_vepo_069 WHERE matnr <> lt_vepo-matnr.

  CLEAR lv_menge_pal.

** Quantidade registada
  LOOP AT lt_vepo_069.

    lv_menge = lt_vepo_069-vemng.

    IF lt_vepo_069-vemeh <> lt_vepo-vemeh.

      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = lt_vepo_069-matnr
          i_in_me              = lt_vepo_069-vemeh
          i_out_me             = lt_vepo-vemeh
          i_menge              = lv_menge
        IMPORTING
          e_menge              = lv_menge
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.

    ENDIF.

    lv_menge_pal = lv_menge_pal + lv_menge.
  ENDLOOP.

** Quantidade de Picking da remessa
**********************************************************************
  SELECT *
    FROM lips INTO TABLE lt_lips
    WHERE vbeln = scr1-vbeln.

  DELETE lt_lips WHERE matnr <> lt_vepo-matnr.

  IF lt_lips[] IS INITIAL.

    text1 = lt_vepo-matnr.
    text2 = scr1-vbeln.

    " Material & da palete não existe na remessa &.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '279'
        message_var1   = text1
        message_var2   = text2
      IMPORTING
        ret_code       = resposta.

    CLEAR scr1-sscc_in.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Picking já efectuado
  IF lt_lips[] IS NOT INITIAL.
    SELECT *
      FROM vbfa INTO TABLE lt_vbfa
      FOR ALL ENTRIES IN lt_lips
      WHERE vbelv = lt_lips-vbeln AND
            posnv = lt_lips-posnr.

    DELETE lt_vbfa WHERE vbtyp_n <> 'Q'.
    DELETE lt_vbfa WHERE rfmng IS INITIAL.

    SORT lt_vbfa BY vbelv posnv.
  ENDIF.

** Validar quantidade picking
  LOOP AT lt_lips.

    lv_tabix = sy-tabix.

    LOOP AT lt_vbfa WHERE vbelv = lt_lips-vbeln AND
                          posnv = lt_lips-posnr.

      lv_menge = lt_vbfa-rfmng.

      IF lt_lips-vrkme <> lt_vbfa-meins.

        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = lt_lips-matnr
            i_in_me              = lt_vbfa-meins
            i_out_me             = lt_lips-vrkme
            i_menge              = lv_menge
          IMPORTING
            e_menge              = lv_menge
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
      ENDIF.

      lt_lips-pikmg = lt_lips-pikmg + lv_menge. " Qtd com picking efectuado
      lt_lips-lfimg = lt_lips-lfimg - lv_menge. " Qtd sem picking efectuado
    ENDLOOP.

    IF lt_lips-lfimg IS INITIAL.
      DELETE lt_lips INDEX lv_tabix.
    ELSE.
      MODIFY lt_lips INDEX lv_tabix TRANSPORTING lfimg pikmg.
    ENDIF.

  ENDLOOP.

** Quantidade da remessa
  CLEAR lv_menge_rem.

  LOOP AT lt_lips.

    lv_menge = lt_lips-lfimg.

    IF lt_lips-vrkme <> lt_vepo-vemeh.

      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = lt_lips-matnr
          i_in_me              = lt_lips-vrkme
          i_out_me             = lt_vepo-vemeh
          i_menge              = lv_menge
        IMPORTING
          e_menge              = lv_menge
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.

    ENDIF.

    lv_menge_rem = lv_menge_rem + lv_menge.
  ENDLOOP.

** Quantidade disponivel para picking
**********************************************************************
  lv_menge = lv_menge_rem - lv_menge_pal.

  IF lv_menge_tot > lv_menge.

    WRITE lv_menge_tot TO text1 LEFT-JUSTIFIED.
    WRITE lv_menge     TO text2 LEFT-JUSTIFIED.

    " 312 - Quantidade & da palete é superior à quantidade & disponivel da remessa.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '312'
        message_var1   = text1
        message_var2   = text2
      IMPORTING
        ret_code       = resposta.

    DESCRIBE TABLE lt_paletes LINES lv_lines.

    " Palete Normal
    IF lv_lines = 1.
      CLEAR scr1-sscc_in.
      LEAVE TO SCREEN setscreen1.

      " Palete Remontada
    ELSE.

      READ TABLE lt_paletes INDEX 1.

      IF lt_paletes-vsolm <= lv_menge.

        "Palete remontada. Deseja só efetuar o picking de uma palete?
        CLEAR resposta.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'W'
            message_number = '329'
          IMPORTING
            ret_code       = resposta.

        IF resposta = 'O'.

          READ TABLE lt_paletes INDEX 1.
          IF sy-subrc = 0.
            lv_sscc = lt_paletes-vlenr.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = lv_sscc
              IMPORTING
                output = lv_sscc.

            WRITE lv_sscc TO text1 LEFT-JUSTIFIED.
          ENDIF.

          READ TABLE lt_paletes INDEX 2.
          IF sy-subrc = 0.
            lv_sscc = lt_paletes-vlenr.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = lv_sscc
              IMPORTING
                output = lv_sscc.

            WRITE lv_sscc TO text2 LEFT-JUSTIFIED.
          ENDIF.

          CLEAR resposta.

          " Escolher (SIM) 1- SSCC & / (NAO) 2- SSCC &.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'W'
              message_number = '333'
              message_var1   = text1
              message_var2   = text2
            IMPORTING
              ret_code       = resposta.

          IF resposta = 'O'.
            READ TABLE lt_paletes INDEX 1.
            IF sy-subrc = 0.
              lv_menge_tot = lv_menge_tot - lt_paletes-vsolm.

              DELETE lt_paletes INDEX 2. " Eliminar palete de cima.
            ENDIF.

          ELSE.
            READ TABLE lt_paletes INDEX 2.
            IF sy-subrc = 0.
              lv_menge_tot = lv_menge_tot - lt_paletes-vsolm.

              DELETE lt_paletes INDEX 1. " Eliminar palete de baixo.
            ENDIF.
          ENDIF.

        ELSE.
          CLEAR scr1-sscc_in.
          LEAVE TO SCREEN setscreen1.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

** Adicionar Palete
**********************************************************************
  LOOP AT lt_paletes.

    GET TIME.
    CLEAR ls_zwm069.
    ls_zwm069-refnr   = scr1-refnr.
    ls_zwm069-vbeln   = scr1-vbeln.
    ls_zwm069-sscc    = lt_paletes-vlenr.
    ls_zwm069-re_user = sy-uname.
    ls_zwm069-re_time = sy-uzeit.
    ls_zwm069-re_date = sy-datum.

    MODIFY zwm069 FROM ls_zwm069.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.

      scr1-idx = scr1-idx + 1.
    ENDIF.
  ENDLOOP.

** Validar se já atingiu a quantidade de picking
**********************************************************************
  lv_menge = lv_menge - lv_menge_tot.

  IF lv_menge = 0.
    WRITE scr1-vbeln    TO text1 LEFT-JUSTIFIED.
    WRITE lt_vepo-matnr TO text2 LEFT-JUSTIFIED.

    " Obtida quantidade total de picking da remessa & para o material &.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '319'
        message_var1   = text1
        message_var2   = text2
      IMPORTING
        ret_code       = resposta.
  ENDIF.

  CLEAR scr1-sscc_in.

ENDFORM.                    " CHECK_SSCC_IN
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_SCREEN_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_screen_0001.

  DATA: lv_lines TYPE i.
  DATA: ls_vekp  TYPE vekp.
  DATA: ls_makt  TYPE makt.
  DATA: lt_vepo  TYPE vepo OCCURS 0 WITH HEADER LINE.

** Obter dados
**********************************************************************
  REFRESH gt_zwm069.

  SELECT *
    FROM zwm069 INTO TABLE gt_zwm069
    WHERE refnr = scr1-refnr AND
          vbeln = scr1-vbeln.

  SORT gt_zwm069 BY re_date re_time.

  DESCRIBE TABLE gt_zwm069 LINES lv_lines.

  IF scr1-tot IS INITIAL.
    scr1-idx = lv_lines.
  ENDIF.

  scr1-tot = lv_lines.

  CLEAR: scr1-sscc,  scr1-matnr, scr1-maktx1, scr1-maktx2,
         scr1-charg, scr1-menge, scr1-meins.

  READ TABLE gt_zwm069 INDEX scr1-idx.
  CHECK sy-subrc = 0.

** Dados SSCC
  SELECT SINGLE *
    FROM vekp INTO ls_vekp
    WHERE exidv = gt_zwm069-sscc.

  SELECT *
    FROM vepo INTO TABLE lt_vepo
    WHERE venum = ls_vekp-venum.

  scr1-sscc = gt_zwm069-sscc.

  READ TABLE lt_vepo INDEX 1.
  IF sy-subrc = 0.

    SELECT SINGLE *
      FROM makt INTO ls_makt
      WHERE matnr = lt_vepo-matnr AND
            spras = sy-langu.

    scr1-matnr  = lt_vepo-matnr.
    scr1-maktx1 = ls_makt-maktx(20).
    scr1-maktx2 = ls_makt-maktx+20(20).
    scr1-charg  = lt_vepo-charg.
    scr1-menge  = lt_vepo-vemng.
    scr1-meins  = lt_vepo-vemeh.
  ENDIF.

ENDFORM.                    " GET_DATA_SCREEN_0001
*&---------------------------------------------------------------------*
*&      Form  EXIT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_0001.

  DATA: lv_ret_code TYPE char1.

  CASE sy-ucomm.
    WHEN 'BACK'.

      IF scr1-vbeln IS NOT INITIAL.

        WRITE scr1-vbeln TO text1 LEFT-JUSTIFIED.

        " Deseja sair da remessa & ?
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'W'
            message_number = '315'
            message_var1   = text1
          IMPORTING
            ret_code       = lv_ret_code.

        CHECK lv_ret_code = 'O'.

        PERFORM dequeue USING gv_key.
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
  CLEAR sy-ucomm.

ENDFORM.                    " EXIT_0001
*&---------------------------------------------------------------------*
*&      Form  ENQUEUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_KEY  text
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM enqueue USING    pv_key TYPE keywords
             CHANGING pv_subrc.

  DATA: lv_rc       TYPE c.
  DATA: lv_ok       TYPE c.
  DATA: lv_uname    TYPE uname.

** Criar bloqueio
**********************************************************************
  pv_subrc = 4.

  DO 2 TIMES.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        keyword_       = pv_key
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc = 0.
      CLEAR pv_subrc.
      EXIT.
    ENDIF.

    WAIT UP TO 1 SECONDS.
  ENDDO.

ENDFORM.                    "enqueue

*&---------------------------------------------------------------------*
*&      Form  DEQUEUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_KEY  text
*----------------------------------------------------------------------*
FORM dequeue USING pv_key TYPE keywords.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      keyword_ = pv_key.

ENDFORM.                    "dequeue
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0001.

  CASE sy-ucomm.
    WHEN 'CLR'.
      PERFORM clear_0001.
    WHEN 'NEXT'.
      PERFORM delete.
    WHEN 'SAVE'.
      PERFORM save.
    WHEN 'PGDN'.
      PERFORM page_down.
    WHEN 'PGUP'.
      PERFORM page_up.
    WHEN OTHERS.
  ENDCASE.
  CLEAR sy-ucomm.

ENDFORM.                    " USER_COMMAND_0001
*&---------------------------------------------------------------------*
*&      Form  CLEAR_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_0001.

  DATA: lv_ret_code TYPE char1.
  DATA: lv_refnr    TYPE t311-refnr.

** Input
**********************************************************************
  IF scr1-idx = scr1-tot.

    IF scr1-vbeln IS NOT INITIAL.

      IF scr1-tot IS NOT INITIAL.

        text1 = scr1-vbeln.

        " Deseja sair da remessa & ?
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'W'
            message_number = '315'
            message_var1   = text1
          IMPORTING
            ret_code       = lv_ret_code.

        CHECK lv_ret_code = 'O'.
      ENDIF.

      PERFORM dequeue USING gv_key.

      lv_refnr = scr1-refnr.

      CLEAR: scr1.

      scr1-refnr = lv_refnr.

    ELSEIF scr1-refnr IS NOT INITIAL.
      CLEAR scr1.
    ENDIF.

  ENDIF.

ENDFORM.                    " CLEAR_0001
*&---------------------------------------------------------------------*
*&      Form  DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete.

  CONSTANTS: lc_refresh TYPE flag VALUE 'X'.

  DATA: lv_index   TYPE i.
  DATA: ls_return  TYPE bdcmsgcoll.
  DATA: ls_zwm020  TYPE zwm020.
  DATA: ls_zwm069  TYPE zwm069.
  DATA: lt_paletes TYPE ltap       OCCURS 0 WITH HEADER LINE.
  DATA: lt_return	 TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

** Delete
**********************************************************************
*  CHECK scr1-idx < scr1-tot.

  READ TABLE gt_zwm069 INDEX scr1-idx.
  CHECK sy-subrc = 0.

** Validar Packing
**********************************************************************
  IF gt_zwm069-packing IS NOT INITIAL.

    text1 = gt_zwm069-sscc.
    text2 = gt_zwm069-vbeln.

    " 317 - Palete & com packing efetuado para remessa &.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '317'
        message_var1   = text1
        message_var2   = text2
      IMPORTING
        ret_code       = resposta.

    RETURN.
  ENDIF.

** Validar Picking
**********************************************************************
*  SELECT SINGLE *
*    FROM zwm020 INTO ls_zwm020
*    WHERE p1 = gt_zwm069-sscc OR
*          p2 = gt_zwm069-sscc.
*
*** Validar se Palete é remontada
*  IF sy-subrc = 0.
*    IF ls_zwm020-p1 = gt_zwm069-sscc.
*
*      lt_paletes-vlenr = ls_zwm020-p1.
*      APPEND lt_paletes.
*
*      lt_paletes-vlenr = ls_zwm020-p2.
*      APPEND lt_paletes.
*
*    ELSEIF ls_zwm020-p2 = gt_zwm069-sscc.
*      text1 = ls_zwm020-p1.
*
*      " Palete remontada. Picar SSCC & de palete de baixo.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWMMSG001'
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = '320'
*          message_var1   = text1
*        IMPORTING
*          ret_code       = resposta.
*
*      LEAVE TO SCREEN setscreen1.
*    ENDIF.
*
*  ELSE.
  lt_paletes-vlenr = gt_zwm069-sscc.
  APPEND lt_paletes.
*  ENDIF.

** Retirar picking
  IF gt_zwm069-picking IS NOT INITIAL.

    LOOP AT lt_paletes.

      CLEAR ls_zwm069.
      READ TABLE gt_zwm069 INTO ls_zwm069 WITH KEY refnr = gt_zwm069-refnr
                                                   vbeln = gt_zwm069-vbeln
                                                   sscc  = lt_paletes-vlenr.

      CHECK ls_zwm069-picking IS NOT INITIAL.

      REFRESH: lt_return.

      CALL FUNCTION 'ZWM_DELIVERY_PICKING_SSCC_DEL'
        EXPORTING
          i_vbeln  = scr1-vbeln
          i_sscc   = lt_paletes-vlenr
        TABLES
          t_return = lt_return
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF sy-subrc <> 0.

        READ TABLE lt_return INDEX 1 INTO ls_return.
        IF sy-subrc = 0.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = ls_return-msgid
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = ls_return-msgnr
              message_var1   = ls_return-msgv1
              message_var2   = ls_return-msgv2
              message_var3   = ls_return-msgv3
              message_var4   = ls_return-msgv4
            IMPORTING
              ret_code       = resposta.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          LEAVE TO SCREEN setscreen1.
        ENDIF.
      ENDIF.

** Remover Batch Split
      REFRESH: lt_return.

      CALL FUNCTION 'ZWM_DELIVERY_BATCH_SPLIT_DEL'
        EXPORTING
          i_vbeln  = scr1-vbeln
          i_sscc   = lt_paletes-vlenr
        TABLES
          t_return = lt_return
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF sy-subrc <> 0.

        READ TABLE lt_return INDEX 1 INTO ls_return.
        IF sy-subrc = 0.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = ls_return-msgid
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = ls_return-msgnr
              message_var1   = ls_return-msgv1
              message_var2   = ls_return-msgv2
              message_var3   = ls_return-msgv3
              message_var4   = ls_return-msgv4
            IMPORTING
              ret_code       = resposta.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          LEAVE TO SCREEN setscreen1.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

** Remover palete
**********************************************************************
  lv_index = scr1-idx.

  LOOP AT lt_paletes.

    DELETE FROM zwm069 WHERE refnr = gt_zwm069-refnr AND
                             vbeln = gt_zwm069-vbeln AND
                             sscc  = lt_paletes-vlenr.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.

      DELETE gt_zwm069 WHERE refnr = gt_zwm069-refnr AND
                             vbeln = gt_zwm069-vbeln AND
                             sscc  = lt_paletes-vlenr.

      DESCRIBE TABLE gt_zwm069 LINES scr1-tot.

      IF lv_index > scr1-tot.
        scr1-idx = scr1-tot.
      ELSE.
        scr1-idx = lv_index.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " DELETE
*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save.

  DATA: lv_tabix       LIKE sy-tabix.
  DATA: lv_menge       TYPE menge_d.
  DATA: lv_menge_pk    TYPE menge_d.
  DATA: lv_pal         TYPE i.
  DATA: lv_refnr       TYPE lvs_refnr.
  DATA: lv_check       TYPE char1.
  DATA: lv_charg_posnr TYPE posnr VALUE '900000'.
  DATA: lv_sscc        TYPE lenum.

  DATA: ls_likp        TYPE likp.
  DATA: ls_return      TYPE bdcmsgcoll.
  DATA: ls_vbkok_wa    TYPE vbkok.
  DATA: ls_vekp        TYPE vekp.
  DATA: ls_vbuk        TYPE vbuk.
  DATA: ls_zwm069      TYPE zwm069.

  DATA: lt_vbfa        TYPE vbfa          OCCURS 0 WITH HEADER LINE.
  DATA: lt_vbpok       TYPE vbpok         OCCURS 0 WITH HEADER LINE.
  DATA: lt_prot        TYPE prott         OCCURS 0 WITH HEADER LINE.
  DATA: lt_hu          TYPE hum_rehang_hu OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm069      TYPE zwm069        OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm020      TYPE zwm020        OCCURS 0 WITH HEADER LINE.
  DATA: lt_vepo        TYPE vepo          OCCURS 0 WITH HEADER LINE.
  DATA: lt_matnr_charg TYPE lips          OCCURS 0 WITH HEADER LINE.

  DATA: lv_delivery       TYPE bapiobdlvhdrchg-deliv_numb.
  DATA: ls_header_data    TYPE bapiobdlvhdrchg.
  DATA: ls_header_control TYPE bapiobdlvhdrctrlchg.
  DATA: lt_item_data      TYPE bapiobdlvitemchg     OCCURS 0 WITH HEADER LINE.
  DATA: lt_item_control	  TYPE bapiobdlvitemctrlchg OCCURS 0 WITH HEADER LINE.
  DATA: lt_return	        TYPE bdcmsgcoll           OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF lt_lips OCCURS 0.
          INCLUDE STRUCTURE lips.
  DATA: pikmg	TYPE rfmng.
  DATA: menge	TYPE rfmng.
  DATA END OF lt_lips.

  DATA: ls_lips     LIKE lt_lips.
  DATA: lt_lips_aux LIKE lt_lips OCCURS 0 WITH HEADER LINE.

** Validar Remessa
**********************************************************************
  CHECK gt_zwm069[] IS NOT INITIAL.

** Validar status Picking
  SELECT SINGLE *
    FROM vbuk INTO ls_vbuk
    WHERE vbeln = scr1-vbeln.

  IF ls_vbuk-kostk = 'C'.
    text1 = scr1-vbeln.

    " Remessa & com picking completo.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '310'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.

    LEAVE TO SCREEN setscreen1.
  ENDIF.

  lt_zwm069[] = gt_zwm069[].

  DELETE lt_zwm069 WHERE picking IS NOT INITIAL.

  CHECK lt_zwm069[] IS NOT INITIAL.

  LOOP AT lt_zwm069.

    CALL FUNCTION 'ZWM_DELIVERY_BATCH_SPLIT'
      EXPORTING
        i_vbeln  = scr1-vbeln
        i_sscc   = lt_zwm069-sscc
      TABLES
        t_return = lt_return
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF sy-subrc <> 0.

      READ TABLE lt_return INDEX 1 INTO ls_return.
      IF sy-subrc = 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = ls_return-msgid
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = ls_return-msgnr
            message_var1   = ls_return-msgv1
            message_var2   = ls_return-msgv2
            message_var3   = ls_return-msgv3
            message_var4   = ls_return-msgv4
          IMPORTING
            ret_code       = resposta.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        LEAVE TO SCREEN setscreen1.
      ENDIF.

    ENDIF.

    REFRESH: lt_return.

    CALL FUNCTION 'ZWM_DELIVERY_PICKING_SSCC'
      EXPORTING
        i_vbeln  = scr1-vbeln
        i_sscc   = lt_zwm069-sscc
      TABLES
        t_return = lt_return
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF sy-subrc <> 0.

      READ TABLE lt_return INDEX 1 INTO ls_return.
      IF sy-subrc = 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = ls_return-msgid
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = ls_return-msgnr
            message_var1   = ls_return-msgv1
            message_var2   = ls_return-msgv2
            message_var3   = ls_return-msgv3
            message_var4   = ls_return-msgv4
          IMPORTING
            ret_code       = resposta.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        LEAVE TO SCREEN setscreen1.
      ENDIF.

    ENDIF.

** Guardar Dados
    GET TIME.
    lt_zwm069-picking = 'X'.
    lt_zwm069-pi_user = sy-uname.
    lt_zwm069-pi_time = sy-uzeit.
    lt_zwm069-pi_date = sy-datum.

    MODIFY zwm069 FROM lt_zwm069.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.

    lv_pal = lv_pal + 1.
  ENDLOOP.

** Validar Palete remontada
**********************************************************************
  IF lt_zwm069[] IS NOT INITIAL.
    SELECT *
      FROM zwm020 INTO TABLE lt_zwm020
      FOR ALL ENTRIES IN lt_zwm069
      WHERE p1 = lt_zwm069-sscc OR
            p2 = lt_zwm069-sscc.
  ENDIF.

** Validar se só foi efetuado o picking de uma palete das remontadas
  LOOP AT lt_zwm069.

    CLEAR: lt_zwm020, lv_sscc.

    READ TABLE lt_zwm020 WITH KEY p1 = lt_zwm069-sscc.
    IF sy-subrc = 0.
      lv_sscc = lt_zwm020-p2.
    ELSE.
      READ TABLE lt_zwm020 WITH KEY p2 = lt_zwm069-sscc.
      IF sy-subrc = 0.
        lv_sscc = lt_zwm020-p1.
      ENDIF.
    ENDIF.

** Palete remontada
    CHECK lv_sscc IS NOT INITIAL.

    READ TABLE gt_zwm069 INTO ls_zwm069 WITH KEY sscc = lv_sscc.
    CHECK sy-subrc <> 0.

    " Remover palete da tabela das remontada
    DELETE FROM zwm020 WHERE armazem = lt_zwm020-armazem AND
                             p1      = lt_zwm020-p1      AND
                             p2      = lt_zwm020-p2.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDLOOP.

** Mensagem de sucesso
**********************************************************************
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  WRITE lv_pal TO text1 LEFT-JUSTIFIED.

  " Picking de & paletes efetuado com sucesso!
  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWMMSG001'
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = '318'
      message_var1   = text1
    IMPORTING
      ret_code       = resposta.

** Validar Picking completo
  SELECT SINGLE *
    FROM vbuk INTO ls_vbuk
    WHERE vbeln = scr1-vbeln.

  IF ls_vbuk-kostk = 'C'.
    text1 = scr1-vbeln.

    " Remessa & com picking completo.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '310'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.

    PERFORM dequeue USING gv_key.

    lv_refnr = scr1-refnr.

    CLEAR: scr1.

    scr1-refnr = lv_refnr.
  ENDIF.

*  SELECT SINGLE *
*    FROM likp INTO ls_likp
*    WHERE vbeln = scr1-vbeln.
*
*  SELECT *
*    FROM lips INTO TABLE lt_lips
*    WHERE vbeln = scr1-vbeln.
*
*  DELETE lt_lips WHERE lfimg IS INITIAL.
*
*  SORT lt_lips BY posnr DESCENDING.
*
*  " Obter item p/ partição Lote
*  READ TABLE lt_lips INDEX 1.
*  IF sy-subrc = 0.
*    IF lt_lips-posnr > lv_charg_posnr.
*      lv_charg_posnr = lt_lips-posnr.
*    ENDIF.
*  ENDIF.
*
*  SORT lt_lips BY matnr posnr.
*
*  lt_lips_aux[] = lt_lips[].
*
*** Validar Picking já efectuado
***********************************************************************
*  IF lt_lips[] IS NOT INITIAL.
*
*    SELECT *
*      FROM vbfa INTO TABLE lt_vbfa
*      FOR ALL ENTRIES IN lt_lips
*      WHERE vbelv = lt_lips-vbeln AND
*            posnv = lt_lips-posnr.
*
*    DELETE lt_vbfa WHERE vbtyp_n <> 'Q'.
*    DELETE lt_vbfa WHERE rfmng IS INITIAL.
*
*    SORT lt_vbfa BY vbelv posnv.
*  ENDIF.
*
*** Validar quantidade picking
*  LOOP AT lt_lips.
*    lv_tabix = sy-tabix.
*
*    lt_lips-menge = lt_lips-lfimg.
*
*    LOOP AT lt_vbfa WHERE vbelv = lt_lips-vbeln AND
*                          posnv = lt_lips-posnr.
*
*      lv_menge = lt_vbfa-rfmng.
*
*      IF lt_lips-vrkme <> lt_vbfa-meins.
*        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*          EXPORTING
*            i_matnr              = lt_lips-matnr
*            i_in_me              = lt_vbfa-meins
*            i_out_me             = lt_lips-vrkme
*            i_menge              = lv_menge
*          IMPORTING
*            e_menge              = lv_menge
*          EXCEPTIONS
*            error_in_application = 1
*            error                = 2
*            OTHERS               = 3.
*      ENDIF.
*
*      lt_lips-pikmg = lt_lips-pikmg + lv_menge. "Qtd com picking efectuado
*      lt_lips-lfimg = lt_lips-lfimg - lv_menge. "Qtd sem picking efectuado
*    ENDLOOP.
*
*    IF lt_lips-lfimg IS INITIAL.
*      DELETE lt_lips INDEX lv_tabix.
*    ELSE.
*      MODIFY lt_lips INDEX lv_tabix.
*    ENDIF.
*  ENDLOOP.
*
*** Actualizar Remessa Com Partição de Lote
***********************************************************************
*
*** Header
*  ls_header_data-deliv_numb    = ls_likp-vbeln.
*  ls_header_control-deliv_numb = ls_likp-vbeln.
*  lv_delivery                  = ls_likp-vbeln.
*
*** Items
*  LOOP AT lt_zwm069.
*
*    " Obter dados da palete
*    SELECT SINGLE *
*      FROM vekp INTO ls_vekp
*      WHERE exidv = lt_zwm069-sscc.
*
*    SELECT *
*      FROM vepo INTO TABLE lt_vepo
*      WHERE venum = ls_vekp-venum.
*
*    READ TABLE lt_vepo INDEX 1.
*    CHECK sy-subrc = 0.
*
*    CHECK lt_vepo-charg IS NOT INITIAL.
*
*    " Distribuir quantidade pelos items da remessa
*    lv_menge = lt_vepo-vemng.
*
*    LOOP AT lt_lips WHERE matnr = lt_vepo-matnr.
*
*      IF lt_vepo-vemeh <> lt_lips-vrkme.
*
*        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*          EXPORTING
*            i_matnr              = lt_vepo-matnr
*            i_in_me              = lt_vepo-vemeh
*            i_out_me             = lt_lips-vrkme
*            i_menge              = lv_menge
*          IMPORTING
*            e_menge              = lv_menge
*          EXCEPTIONS
*            error_in_application = 1
*            error                = 2
*            OTHERS               = 3.
*
*        lt_vepo-vemeh = lt_lips-vrkme.
*      ENDIF.
*
*      lv_menge_pk = lv_menge - lt_lips-lfimg.
*
*      IF lv_menge_pk < 0.
*        lt_lips-lfimg = lt_lips-lfimg - lv_menge.
*
*        lv_menge_pk = lv_menge.
*        lv_menge    = 0.
*
*        MODIFY lt_lips INDEX sy-tabix TRANSPORTING lfimg.
*      ELSE.
*        lv_menge_pk = lt_lips-lfimg.
*        lv_menge    = lv_menge - lt_lips-lfimg.
*
*        DELETE lt_lips INDEX sy-tabix.
*      ENDIF.
*
*      " Primeiro lote no item
*      IF lt_lips-charg IS INITIAL.
*
*        CLEAR lt_item_data.
*        lt_item_data-deliv_numb      = lt_lips-vbeln.
*        lt_item_data-deliv_item      = lt_lips-posnr.
**        lt_item_data-hieraritem      = lt_lips-posnr.
**        lt_item_data-usehieritm      = '1'.
*        lt_item_data-material        = lt_lips-matnr.
*        lt_item_data-batch           = lt_vepo-charg.
*        lt_item_data-dlv_qty         = lt_lips-menge.
*        lt_item_data-sales_unit      = lt_lips-vrkme.
*        lt_item_data-sales_unit_iso  = lt_item_data-base_uom
*                                     = lt_item_data-base_uom_iso
*                                     = lt_item_data-sales_unit.
*        lt_item_data-fact_unit_nom   = 1.
*        lt_item_data-fact_unit_denom = 1.
*        APPEND lt_item_data.
*
*        CLEAR lt_item_control.
*        lt_item_control-deliv_numb   = lt_lips-vbeln.
*        lt_item_control-deliv_item   = lt_lips-posnr.
*        lt_item_control-chg_delqty   = 'X'.
*        APPEND lt_item_control.
*
*        " Segundo lote no item - Batch Split
*      ELSE.
*
*        IF lt_lips-charg <> lt_vepo-charg.
*
*          CLEAR lt_item_data.
*          lt_item_data-deliv_numb      = lt_lips-vbeln.
*          lt_item_data-deliv_item      = lt_lips-posnr.
*          lt_item_data-dlv_qty         = lt_lips-menge - lv_menge_pk.
*          lt_item_data-sales_unit      = lt_lips-vrkme.
*          lt_item_data-sales_unit_iso  = lt_item_data-base_uom
*                                       = lt_item_data-base_uom_iso
*                                       = lt_item_data-sales_unit.
*          lt_item_data-fact_unit_nom   = 1.
*          lt_item_data-fact_unit_denom = 1.
*          APPEND lt_item_data.
*
*          CLEAR lt_item_control.
*          lt_item_control-deliv_numb = lt_lips-vbeln.
*          lt_item_control-deliv_item = lt_lips-posnr.
*          lt_item_control-chg_delqty = 'X'.
*          APPEND lt_item_control.
*
*          " New batch item
*          READ TABLE lt_lips INTO ls_lips WITH KEY uecha = lt_lips-posnr
*                                                   charg = lt_vepo-charg.
*          IF sy-subrc = 0.
*            CLEAR lt_item_data.
*            lt_item_data-deliv_numb      = ls_lips-vbeln.
*            lt_item_data-deliv_item      = ls_lips-posnr.
*            lt_item_data-dlv_qty         = ls_lips-menge + lv_menge_pk.
*            lt_item_data-sales_unit      = ls_lips-vrkme.
*            lt_item_data-sales_unit_iso  = lt_item_data-base_uom
*                                         = lt_item_data-base_uom_iso
*                                         = lt_item_data-sales_unit.
*            lt_item_data-fact_unit_nom   = 1.
*            lt_item_data-fact_unit_denom = 1.
*            APPEND lt_item_data.
*
*
*          ELSE.
*            lv_charg_posnr = lv_charg_posnr + 1.
*
*            CLEAR lt_item_data.
*            lt_item_data-deliv_numb      = lt_lips-vbeln.
*            lt_item_data-deliv_item      = lv_charg_posnr.
*            lt_item_data-hieraritem      = lt_lips-posnr.
*            lt_item_data-usehieritm      = '1'.
*            lt_item_data-material        = lt_lips-matnr.
*            lt_item_data-batch           = lt_vepo-charg.
*            lt_item_data-dlv_qty         = lv_menge_pk.
*            lt_item_data-sales_unit      = lt_lips-vrkme.
*            lt_item_data-sales_unit_iso  = lt_item_data-base_uom
*                                         = lt_item_data-base_uom_iso
*                                         = lt_item_data-sales_unit.
*            lt_item_data-fact_unit_nom   = 1.
*            lt_item_data-fact_unit_denom = 1.
*            APPEND lt_item_data.
*          ENDIF.
*
*          CLEAR lt_item_control.
*          lt_item_control-deliv_numb = lt_lips-vbeln.
*          lt_item_control-deliv_item = lv_charg_posnr.
*          lt_item_control-chg_delqty = 'X'.
*          APPEND lt_item_control.
*        ENDIF.
*      ENDIF.
*
*      IF lv_menge <= 0.
*        EXIT.
*      ENDIF.
*
*    ENDLOOP.
*
*  ENDLOOP.
*
*** Modificação do Lote
*  IF lt_item_data[] IS NOT INITIAL.
*
*    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
*      EXPORTING
*        header_data    = ls_header_data
*        header_control = ls_header_control
*        delivery       = lv_delivery
*      TABLES
*        item_data      = lt_item_data
*        item_control   = lt_item_control
*        return         = lt_return.
*
*    LOOP AT lt_return WHERE type = 'E' OR type = 'A'.
*      CLEAR ls_return.
*      MOVE lt_return-type       TO ls_return-msgtyp.
*      MOVE lt_return-id         TO ls_return-msgid.
*      MOVE lt_return-number     TO ls_return-msgnr.
*      MOVE lt_return-message_v1 TO ls_return-msgv1.
*      MOVE lt_return-message_v2 TO ls_return-msgv2.
*      MOVE lt_return-message_v3 TO ls_return-msgv3.
*      MOVE lt_return-message_v4 TO ls_return-msgv4.
*      EXIT.
*    ENDLOOP.
*
*    IF sy-subrc EQ 0.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = ls_return-msgid
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = ls_return-msgnr
*          message_var1   = ls_return-msgv1
*          message_var2   = ls_return-msgv2
*          message_var3   = ls_return-msgv3
*          message_var4   = ls_return-msgv4
*        IMPORTING
*          ret_code       = resposta.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
*      LEAVE TO SCREEN setscreen1.
*
*    ELSE.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*      WAIT UP TO 2 SECONDS.
*
*      DO 10 TIMES.
*
*        REFRESH: lt_lips.
*
*        SELECT *
*          FROM lips INTO TABLE lt_lips
*          WHERE vbeln = scr1-vbeln.
*
*        DELETE lt_lips WHERE lfimg IS INITIAL.
*
*        CLEAR lv_check.
*
*        LOOP AT lt_item_data.
*
*          READ TABLE lt_lips WITH KEY matnr = lt_item_data-material
*                                      charg = lt_item_data-batch.
*
*          IF sy-subrc <> 0.
*            lv_check = 'X'.
*          ENDIF.
*        ENDLOOP.
*
*        IF lv_check IS INITIAL.
*          EXIT.
*        ENDIF.
*
*      ENDDO.
*    ENDIF.
*  ENDIF.
*
*** Validar Picking já efectuado
***********************************************************************
*  IF lt_lips[] IS NOT INITIAL.
*    SELECT *
*      FROM vbfa INTO TABLE lt_vbfa
*      FOR ALL ENTRIES IN lt_lips
*      WHERE vbelv = lt_lips-vbeln AND
*            posnv = lt_lips-posnr.
*
*    DELETE lt_vbfa WHERE vbtyp_n <> 'Q'.
*    DELETE lt_vbfa WHERE rfmng IS INITIAL.
*
*    SORT lt_vbfa BY vbelv posnv.
*  ENDIF.
*
*** Validar quantidade picking
*  LOOP AT lt_lips.
*
*    lv_tabix = sy-tabix.
*
*    lt_lips-menge = lt_lips-lfimg.
*
*    LOOP AT lt_vbfa WHERE vbelv = lt_lips-vbeln AND
*                          posnv = lt_lips-posnr.
*
*      lv_menge = lt_vbfa-rfmng.
*
*      IF lt_lips-vrkme <> lt_vbfa-meins.
*        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*          EXPORTING
*            i_matnr              = lt_lips-matnr
*            i_in_me              = lt_vbfa-meins
*            i_out_me             = lt_lips-vrkme
*            i_menge              = lv_menge
*          IMPORTING
*            e_menge              = lv_menge
*          EXCEPTIONS
*            error_in_application = 1
*            error                = 2
*            OTHERS               = 3.
*      ENDIF.
*
*      lt_lips-pikmg = lt_lips-pikmg + lv_menge. "Qtd com picking efectuado
*      lt_lips-lfimg = lt_lips-lfimg - lv_menge. "Qtd sem picking efectuado
*    ENDLOOP.
*
*    IF lt_lips-lfimg IS INITIAL.
*      DELETE lt_lips INDEX lv_tabix.
*    ELSE.
*      MODIFY lt_lips INDEX lv_tabix.
*    ENDIF.
*  ENDLOOP.
*
*** Efetuar Picking
***********************************************************************
*  CLEAR ls_vbkok_wa.
*  ls_vbkok_wa-vbeln_vl = ls_likp-vbeln.
*  ls_vbkok_wa-vbtyp_vl = ls_likp-vbtyp.
*  ls_vbkok_wa-vbeln    = ls_likp-vbeln.
**  ls_vbkok_wa-komue    = 'X'.
*
*  LOOP AT lt_zwm069.
*
*    " Obter dados da palete
*    SELECT SINGLE *
*      FROM vekp INTO ls_vekp
*      WHERE exidv = lt_zwm069-sscc.
*
*    SELECT *
*      FROM vepo INTO TABLE lt_vepo
*      WHERE venum = ls_vekp-venum.
*
*    READ TABLE lt_vepo INDEX 1.
*    CHECK sy-subrc = 0.
*
*    " Distribuir quantidade pelos items da remessa
*    lv_menge = lt_vepo-vemng.
*
*    LOOP AT lt_lips WHERE matnr = lt_vepo-matnr AND
*                          charg = lt_vepo-charg.
*
*      IF lt_vepo-vemeh <> lt_lips-vrkme.
*
*        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*          EXPORTING
*            i_matnr              = lt_vepo-matnr
*            i_in_me              = lt_vepo-vemeh
*            i_out_me             = lt_lips-vrkme
*            i_menge              = lv_menge
*          IMPORTING
*            e_menge              = lv_menge
*          EXCEPTIONS
*            error_in_application = 1
*            error                = 2
*            OTHERS               = 3.
*
*        lt_vepo-vemeh = lt_lips-vrkme.
*      ENDIF.
*
*      lv_menge_pk = lv_menge - lt_lips-lfimg.
*
*      IF lv_menge_pk < 0.
*        lt_lips-lfimg = lt_lips-lfimg - lv_menge.
*        lt_lips-pikmg = lt_lips-pikmg + lv_menge.
*        lv_menge      = 0.
*
*        MODIFY lt_lips INDEX sy-tabix TRANSPORTING lfimg pikmg.
*      ELSE.
*        lt_lips-pikmg = lt_lips-pikmg + lt_lips-lfimg.
*        lv_menge      = lv_menge - lt_lips-lfimg.
*
*        DELETE lt_lips INDEX sy-tabix.
*      ENDIF.
*
*      CLEAR lt_vbpok.
*      lt_vbpok-vbeln_vl = lt_lips-vbeln.
*      lt_vbpok-posnr_vl = lt_lips-posnr.
*      lt_vbpok-vbeln    = lt_lips-vbeln.
*      lt_vbpok-posnn    = lt_lips-posnr.
*      lt_vbpok-matnr    = lt_lips-matnr.
*      lt_vbpok-charg    = lt_vepo-charg. "lt_lips-charg.
*      lt_vbpok-vrkme    = lt_lips-vrkme.
*
*      lt_vbpok-pikmg    = lt_lips-pikmg.
*
*      APPEND lt_vbpok.
*
*      IF lv_menge <= 0.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.
*
*  IF lt_vbpok[] IS INITIAL.
*    " Sem items da remessa para efetuar o picking.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '321'
*      IMPORTING
*        ret_code       = resposta.
*
*    LEAVE TO SCREEN setscreen1.
*  ENDIF.
*
*** Update
*  CALL FUNCTION 'WS_DELIVERY_UPDATE'
*    EXPORTING
*      vbkok_wa                 = ls_vbkok_wa
*      synchron                 = 'X'
*      commit                   = 'X'
*      delivery                 = scr1-vbeln
*      update_picking           = 'X'
**      if_error_messages_send_0 = ''
*    TABLES
*      vbpok_tab                = lt_vbpok
*      prot                     = lt_prot
*      it_handling_units        = lt_hu
*    EXCEPTIONS
*        error_message          = 99.
*
*  IF sy-subrc <> 0.
*    CLEAR ls_return.
*
*    ls_return-msgid = sy-msgid.
*    ls_return-msgnr = sy-msgno.
*    ls_return-msgv1 = sy-msgv1.
*    ls_return-msgv2 = sy-msgv2.
*    ls_return-msgv3 = sy-msgv3.
*    ls_return-msgv4 = sy-msgv4.
*
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = ls_return-msgid
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = ls_return-msgnr
*        message_var1   = ls_return-msgv1
*        message_var2   = ls_return-msgv2
*        message_var3   = ls_return-msgv3
*        message_var4   = ls_return-msgv4
*      IMPORTING
*        ret_code       = resposta.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
*    LEAVE TO SCREEN setscreen1.
*  ENDIF.
*
*  DELETE lt_prot WHERE msgty <> 'E'.
*
*  READ TABLE lt_prot INDEX 1.
*  IF sy-subrc = 0.
*
*    CLEAR ls_return.
*    ls_return-msgid = lt_prot-msgid.
*    ls_return-msgnr = lt_prot-msgno.
*    ls_return-msgv1 = lt_prot-msgv1.
*    ls_return-msgv2 = lt_prot-msgv2.
*    ls_return-msgv3 = lt_prot-msgv3.
*    ls_return-msgv4 = lt_prot-msgv4.
*
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = ls_return-msgid
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = ls_return-msgnr
*        message_var1   = ls_return-msgv1
*        message_var2   = ls_return-msgv2
*        message_var3   = ls_return-msgv3
*        message_var4   = ls_return-msgv4
*      IMPORTING
*        ret_code       = resposta.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*
*    LEAVE TO SCREEN setscreen1.
*  ENDIF.
*
*** Guardar dados
***********************************************************************
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*    EXPORTING
*      wait = 'X'.
*
*  LOOP AT lt_zwm069.
*
*    GET TIME.
*    lt_zwm069-picking = 'X'.
*    lt_zwm069-pi_user = sy-uname.
*    lt_zwm069-pi_time = sy-uzeit.
*    lt_zwm069-pi_date = sy-datum.
*
*    MODIFY zwm069 FROM lt_zwm069.
*    IF sy-subrc = 0.
*      COMMIT WORK AND WAIT.
*    ENDIF.
*
*    lv_pal = lv_pal + 1.
*
*  ENDLOOP.
*
*  WAIT UP TO 1 SECONDS.
*
*** Mensagem de sucesso
***********************************************************************
*  WRITE lv_pal TO text1 LEFT-JUSTIFIED.
*
*  " Picking de & paletes efetuado com sucesso!
*  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*    EXPORTING
*      message_id     = 'ZWMMSG001'
*      message_lang   = sy-langu
*      message_type   = 'E'
*      message_number = '318'
*      message_var1   = text1
*    IMPORTING
*      ret_code       = resposta.
*
*** Validar Picking completo
*  SELECT SINGLE *
*    FROM vbuk INTO ls_vbuk
*    WHERE vbeln = scr1-vbeln.
*
*  IF ls_vbuk-kostk = 'C'.
*    text1 = scr1-vbeln.
*
*    " Remessa & com picking completo.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '310'
*        message_var1   = text1
*      IMPORTING
*        ret_code       = resposta.
*
*    PERFORM dequeue USING gv_key.
*
*    lv_refnr = scr1-refnr.
*
*    CLEAR: scr1.
*
*    scr1-refnr = lv_refnr.
*  ENDIF.

ENDFORM.                    " SAVE
*&---------------------------------------------------------------------*
*&      Form  PAGE_DOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM page_down.

  CHECK scr1-idx > 1.

  scr1-idx = scr1-idx - 1.

ENDFORM.                    " PAGE_DOWN
*&---------------------------------------------------------------------*
*&      Form  PAGE_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM page_up.

  CHECK scr1-idx < scr1-tot.

  scr1-idx = scr1-idx + 1.

ENDFORM.                    " PAGE_UP
*&---------------------------------------------------------------------*
*&      Form  CHECK_SSCC_IN_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_sscc_in_2.

  DATA: lv_tabix     TYPE sy-tabix.
  DATA: lv_lines     TYPE i.
  DATA: lv_menge     TYPE menge_d.
  DATA: lv_menge_tot TYPE menge_d.
  DATA: lv_menge_pal TYPE menge_d.
  DATA: lv_menge_rem TYPE menge_d.

  DATA: ls_lein      TYPE lein.
  DATA: ls_vekp      TYPE vekp.
  DATA: ls_makt      TYPE makt.
  DATA: ls_vekp_069  TYPE vekp.
  DATA: ls_zwm069    TYPE zwm069.
  DATA: ls_zwm020    TYPE zwm020.

  DATA: lt_vekp_069  TYPE vekp   OCCURS 0 WITH HEADER LINE.
  DATA: lt_vepo      TYPE vepo   OCCURS 0 WITH HEADER LINE.
  DATA: lt_vepo_069  TYPE vepo   OCCURS 0 WITH HEADER LINE.
  DATA: lt_vbfa      TYPE vbfa   OCCURS 0 WITH HEADER LINE.
  DATA: lt_zwm069    TYPE zwm069 OCCURS 0 WITH HEADER LINE.
  DATA: lt_paletes   TYPE ltap   OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF lt_lips OCCURS 0.
          INCLUDE STRUCTURE lips.
  DATA: pikmg	TYPE rfmng.
  DATA END OF lt_lips.

** Validar SSCC
**********************************************************************
  CHECK NOT scr2-sscc_in IS INITIAL.

** Validar se Palete é remontada
  SELECT SINGLE *
    FROM zwm020 INTO ls_zwm020
    WHERE p1 = scr1-sscc_in OR
          p2 = scr1-sscc_in.

  IF sy-subrc = 0.
    IF ls_zwm020-p1 = scr2-sscc_in.

      lt_paletes-vlenr = ls_zwm020-p1.
      APPEND lt_paletes.

      lt_paletes-vlenr = ls_zwm020-p2.
      APPEND lt_paletes.

    ELSEIF ls_zwm020-p2 = scr2-sscc_in.
      text1 = ls_zwm020-p1.

      " Palete remontada. Picar SSCC & de palete de baixo.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '320'
          message_var1   = text1
        IMPORTING
          ret_code       = resposta.

      CLEAR scr2-sscc_in.
      LEAVE TO SCREEN setscreen2.
    ENDIF.

  ELSE.
    lt_paletes-vlenr = scr2-sscc_in.
    APPEND lt_paletes.
  ENDIF.

** Obter dados da palete
  LOOP AT lt_paletes.
    lv_tabix = sy-tabix.

    SELECT SINGLE *
      FROM vekp INTO ls_vekp
      WHERE exidv = lt_paletes-vlenr.

    SELECT *
      FROM vepo INTO TABLE lt_vepo
      WHERE venum = ls_vekp-venum.

    READ TABLE lt_vepo INDEX 1.
    IF sy-subrc <> 0.
      text1 = lt_paletes-vlenr.

      " SSCC & inválido!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '113'
          message_var1   = text1
        IMPORTING
          ret_code       = resposta.

      CLEAR scr2-sscc_in.
      LEAVE TO SCREEN setscreen2.
    ENDIF.

** Validar se existe em armazém
    SELECT SINGLE *
      FROM lein INTO ls_lein
      WHERE lenum = lt_paletes-vlenr.

    IF sy-subrc = 0.
      text1 = ls_lein-lenum.
      text2 = ls_lein-lgnum.

      " 311- Palete & pertence ao armazém &.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '311'
          message_var1   = text1
          message_var2   = text2
        IMPORTING
          ret_code       = resposta.

      CLEAR scr2-sscc_in.
      LEAVE TO SCREEN setscreen2.
    ENDIF.

** Validar se palete foi contabilizada
    SELECT SINGLE *
      FROM zwm069 INTO ls_zwm069
      WHERE sscc = lt_paletes-vlenr.

    IF sy-subrc <> 0.
      " Palete & não foi registada para eliminar!
      text1 = lt_paletes-vlenr.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '331'
          message_var1   = text1
        IMPORTING
          ret_code       = resposta.

      CLEAR scr2-sscc_in.
      LEAVE TO SCREEN setscreen2.
    ENDIF.

  ENDLOOP.

** Obter dados SSCC
**********************************************************************
  CLEAR scr2-sscc_in.

  scr2-sscc = ls_zwm069-sscc.

  READ TABLE lt_vepo INDEX 1.
  IF sy-subrc = 0.

    SELECT SINGLE *
      FROM makt INTO ls_makt
      WHERE matnr = lt_vepo-matnr AND
            spras = sy-langu.

    scr2-matnr  = lt_vepo-matnr.
    scr2-maktx1 = ls_makt-maktx(20).
    scr2-maktx2 = ls_makt-maktx+20(20).
    scr2-charg  = lt_vepo-charg.
    scr2-menge  = lt_vepo-vemng.
    scr2-meins  = lt_vepo-vemeh.
  ENDIF.

ENDFORM.                    " CHECK_SSCC_IN_2
*&---------------------------------------------------------------------*
*&      Form  EXIT_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_0002.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
  CLEAR sy-ucomm.

ENDFORM.                    " EXIT_0002
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0002.

  CASE sy-ucomm.
    WHEN 'CLR'.
      PERFORM clear_0002.
    WHEN 'NEXT'.
      PERFORM delete_0002.
    WHEN OTHERS.
  ENDCASE.
  CLEAR sy-ucomm.

ENDFORM.                    " USER_COMMAND_0002
*&---------------------------------------------------------------------*
*&      Form  CLEAR_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_0002.
  CLEAR scr2.
ENDFORM.                    " CLEAR_0002
*&---------------------------------------------------------------------*
*&      Form  DELETE_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_0002.

  CONSTANTS: lc_refresh TYPE flag VALUE 'X'.

  DATA: lv_index    TYPE i.
  DATA: ls_return   TYPE bdcmsgcoll.
  DATA: ls_zwm020   TYPE zwm020.
  DATA: ls_zwm069   TYPE zwm069.
  DATA: ls_zwm069_2 TYPE zwm069.
  DATA: lt_paletes  TYPE ltap       OCCURS 0 WITH HEADER LINE.
  DATA: lt_return	  TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

** Delete
**********************************************************************
*  CHECK scr1-idx < scr1-tot.
  CHECK scr2-sscc IS NOT INITIAL.

  SELECT SINGLE *
    FROM zwm069 INTO ls_zwm069
    WHERE sscc = scr2-sscc.

  CHECK sy-subrc = 0.

** Validar Packing
**********************************************************************
  IF ls_zwm069-packing IS NOT INITIAL.

    text1 = ls_zwm069-sscc.
    text2 = ls_zwm069-vbeln.

    " 317 - Palete & com packing efetuado para remessa &.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '317'
        message_var1   = text1
        message_var2   = text2
      IMPORTING
        ret_code       = resposta.

    RETURN.
  ENDIF.

** Validar Picking
**********************************************************************
*  SELECT SINGLE *
*    FROM zwm020 INTO ls_zwm020
*    WHERE p1 = ls_zwm069-sscc OR
*          p2 = ls_zwm069-sscc.
*
*** Validar se Palete é remontada
*  IF sy-subrc = 0.
*    IF ls_zwm020-p1 = ls_zwm069-sscc.
*
*      lt_paletes-vlenr = ls_zwm020-p1.
*      APPEND lt_paletes.
*
*      lt_paletes-vlenr = ls_zwm020-p2.
*      APPEND lt_paletes.
*
*    ELSEIF ls_zwm020-p2 = ls_zwm069-sscc.
*      text1 = ls_zwm020-p1.
*
*      " Palete remontada. Picar SSCC & de palete de baixo.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWMMSG001'
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = '320'
*          message_var1   = text1
*        IMPORTING
*          ret_code       = resposta.
*
*      LEAVE TO SCREEN setscreen2.
*    ENDIF.
*
*  ELSE.
  lt_paletes-vlenr = ls_zwm069-sscc.
  APPEND lt_paletes.
*  ENDIF.

** Retirar picking
  IF ls_zwm069-picking IS NOT INITIAL.

    LOOP AT lt_paletes.

      SELECT SINGLE *
       FROM zwm069 INTO ls_zwm069_2
       WHERE refnr = ls_zwm069-refnr
        AND  vbeln = ls_zwm069-vbeln
        AND  sscc  = lt_paletes-vlenr.

      CHECK ls_zwm069_2-picking IS NOT INITIAL.

      REFRESH: lt_return.

      CALL FUNCTION 'ZWM_DELIVERY_PICKING_SSCC_DEL'
        EXPORTING
          i_vbeln  = ls_zwm069-vbeln
          i_sscc   = lt_paletes-vlenr
        TABLES
          t_return = lt_return
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF sy-subrc <> 0.

        READ TABLE lt_return INDEX 1 INTO ls_return.
        IF sy-subrc = 0.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = ls_return-msgid
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = ls_return-msgnr
              message_var1   = ls_return-msgv1
              message_var2   = ls_return-msgv2
              message_var3   = ls_return-msgv3
              message_var4   = ls_return-msgv4
            IMPORTING
              ret_code       = resposta.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          LEAVE TO SCREEN setscreen2.
        ENDIF.
      ENDIF.

** Remover Batch Split
      REFRESH: lt_return.

      CALL FUNCTION 'ZWM_DELIVERY_BATCH_SPLIT_DEL'
        EXPORTING
          i_vbeln  = ls_zwm069-vbeln
          i_sscc   = lt_paletes-vlenr
        TABLES
          t_return = lt_return
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF sy-subrc <> 0.

        READ TABLE lt_return INDEX 1 INTO ls_return.
        IF sy-subrc = 0.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = ls_return-msgid
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = ls_return-msgnr
              message_var1   = ls_return-msgv1
              message_var2   = ls_return-msgv2
              message_var3   = ls_return-msgv3
              message_var4   = ls_return-msgv4
            IMPORTING
              ret_code       = resposta.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          LEAVE TO SCREEN setscreen2.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

** Remover palete
**********************************************************************
  LOOP AT lt_paletes.

    DELETE FROM zwm069 WHERE refnr = ls_zwm069-refnr AND
                             vbeln = ls_zwm069-vbeln AND
                             sscc  = lt_paletes-vlenr.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDLOOP.

  CLEAR scr2.

ENDFORM.                    " DELETE_0002
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen.

  IF sy-tcode = 'ZWM147D'.
    SET SCREEN setscreen2.
  ELSE.
    SET SCREEN setscreen1.
  ENDIF.

ENDFORM.                    " SET_SCREEN
