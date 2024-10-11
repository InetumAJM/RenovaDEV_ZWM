*&---------------------------------------------------------------------*
*&  Include           ZWMREP0086_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_whs .
  CLEAR gv_whs.
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = gs_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*   ERRO: Utilizador não tem armazem atribuído!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '001'.

  ELSE.
    READ TABLE gs_user WITH KEY statu = 'X'. "Util. Atrib. Arm.
    IF sy-subrc <> 0.
      WRITE gs_user-lgnum TO gv_text1 LEFT-JUSTIFIED.
*     ERRO: Utilizador não está atribuído ao armazém &!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMPMSG'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '002'
          message_var1   = gv_text1.

    ELSE.
      gv_whs = gs_user-lgnum.
      IF gs_user-devty(5) = '16X20'.
        gv_setscreen1 = '0001'.
      ELSE.
        gv_setscreen1 = '0011'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_WHS
*&---------------------------------------------------------------------*
*&      Form  GET_PARAMETERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_parameters .
  DATA: lv_valor       LIKE zwm001-valor.
  DATA: lv_dep(20)     TYPE c.
**********************************************************************
  DATA: lt_messages TYPE tab_bdcmsgcoll,
        ls_message  TYPE bdcmsgcoll.
**********************************************************************
  DO 1 TIMES.
** Movimento MM
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_whs
        i_processo  = 'TRANSF.QTD PARCIAIS'
        i_parametro = 'TP_MOV_MM'
      IMPORTING
        e_valor     = gv_mm_mov
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

*    gv_mm_mov = '311'.
** Movimento WM
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_whs
        i_processo  = 'TRANSF.QTD PARCIAIS'
        i_parametro = 'TP_MOV_WM'
      IMPORTING
        e_valor     = gv_wm_mov
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.


** Tipo deposito destino
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_whs
        i_processo  = 'TRANSF.QTD PARCIAIS'
        i_parametro = 'TP_DEP_DEST'
      IMPORTING
        e_valor     = gv_tp_dep_dest
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.


** Posição de destino
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_whs
        i_processo  = 'TRANSF.QTD PARCIAIS'
        i_parametro = 'POS_DEP_DEST'
      IMPORTING
        e_valor     = gv_pos_dep_dest
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

** Obter tipos de deposito.

    SELECT valor FROM zwm001 INTO TABLE gt_td
     WHERE armazem   = gv_whs
       AND processo  = 'TRANSF.QTD PARCIAIS'
       AND parametro LIKE 'TP%'.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    SELECT * FROM t331 INTO
      CORRESPONDING FIELDS OF TABLE gt_td_su
      FOR ALL ENTRIES IN gt_td
      WHERE lgnum = gv_whs
        AND lgtyp = gt_td-lgtyp.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.




  ENDDO.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    LEAVE TO SCREEN 0.
  ENDIF.




ENDFORM.                    " GET_PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  CHK_P_ORI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  chk_p_ori .
  DATA: lt_lagp  TYPE TABLE OF lagp,
        ls_lagp  TYPE lagp,
        lv_lgtyp TYPE lgtyp,
        lv_lgpla TYPE lgpla.
**********************************************************************

  CHECK scr0001-p_ori IS NOT INITIAL.

*  PERFORM clear_ecran.

  MOVE scr0001-p_ori(3)    TO lv_lgtyp.
  MOVE scr0001-p_ori+4(10) TO lv_lgpla.

** Verificar se a posição origem existe no armazém
  SELECT SINGLE * FROM lagp INTO ls_lagp
          WHERE lgnum = gv_whs
            AND lgtyp = lv_lgtyp
            AND lgpla = lv_lgpla.

  IF sy-subrc <> 0.
** Posição inválida
    WRITE scr0001-p_ori TO gv_text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '034'
        message_var1   = gv_text1.

    CLEAR scr0001.

    MOVE 'SCR0001-P_ORI' TO cursorfield.

    LEAVE TO SCREEN gv_setscreen1.
  ENDIF.

  READ TABLE gt_td_su WITH KEY lgtyp = lv_lgtyp TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
** Posição inválida
    WRITE scr0001-p_ori TO gv_text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '034'
        message_var1   = gv_text1.

    CLEAR scr0001.

    MOVE 'SCR0001-P_ORI' TO cursorfield.

    LEAVE TO SCREEN gv_setscreen1.
  ENDIF.
*  SELECT SINGLE * from lqua INTO ls_lqua
*   WHERE LGNUM = gv_whs
*     and LGTYP = lv_lgtyp
*     and LGPLA = lv_lgpla.

  scr0001-lgtyp = lv_lgtyp.
  scr0001-lgpla = lv_lgpla.

ENDFORM.                    " CHK_P_ORI
*&---------------------------------------------------------------------*
*&      Form  GET_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_range .

*  SELECT valor FROM zwm001 INTO TABLE gt_td
*   WHERE armazem   = gv_whs
*     and processo  = 'TRANSF.QTD PARCIAIS'
*     and PARAMETRO like 'TP%'.

ENDFORM.                    " GET_RANGE
*&---------------------------------------------------------------------*
*&      Form  CHK_SSCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_lenum .
  DATA: wa_lqua TYPE lqua.
  DATA: wa_makt TYPE makt.
  DATA: lv_lenum LIKE lein-lenum.
  DATA: lv_maktx LIKE makt-maktx.
  DATA: lv_subrc LIKE sy-subrc.
  DATA: lv_user  LIKE sy-uname.
  DATA: ls_lein  TYPE lein.
  DATA: ls_ltak  TYPE ltak.
  DATA: lt_ltap  TYPE ltap OCCURS 0 WITH HEADER LINE.
  DATA: ls_lqua  TYPE lqua.
  DATA: ls_makt  TYPE makt.

**********************************************************************
** Validar SU
**********************************************************************
  CHECK scr0001-lenum IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = scr0001-lenum
    IMPORTING
      output = lv_lenum.

** Verificar se SU existe em armazém.
  SELECT SINGLE *
    FROM lein INTO ls_lein
    WHERE lenum = lv_lenum.

  IF sy-subrc <> 0.
    MOVE lv_lenum TO gv_text1.
    MOVE gv_whs   TO gv_text2.

*   ERRO: Uni. depósito & não existe no armazém &!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '011'
        message_var1   = gv_text1
        message_var2   = gv_text2.

    CLEAR scr0001-lenum.
    LEAVE TO SCREEN gv_setscreen1.
  ENDIF.

** Verificar se existe OT pendente para SU
  SELECT *
    FROM ltap INTO TABLE lt_ltap
    WHERE lgnum = gv_whs           AND
          pquit <> abap_true          AND
          nltyp = ls_lein-lgtyp AND
          nlpla = ls_lein-lgpla.

  DELETE lt_ltap WHERE vorga = 'ST' OR vorga = 'SL'. "Eliminarextornadas
  DELETE lt_ltap WHERE nlenr <> lv_lenum.

  IF lt_ltap[] IS NOT INITIAL.
    MOVE lv_lenum TO gv_text1.
*     ERRO: Unid. Depósito já esta a ser arrumada pelo utilizador &!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '053'
        message_var1   = gv_text1.

    CLEAR scr0001-lenum.
    LEAVE TO SCREEN gv_setscreen1.
  ENDIF.

  SELECT SINGLE * FROM lqua INTO ls_lqua
   WHERE lgnum = gv_whs
     AND lgtyp = scr0001-lgtyp
     AND lgpla = scr0001-lgpla
     AND lenum = lv_lenum.

  IF sy-subrc <> 0.
**SSCC & invalido !
    WRITE lv_lenum TO gv_text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '215'
        message_var1   = gv_text1.

    CLEAR scr0001-lenum.

    MOVE 'SCR0001-LENUM' TO cursorfield.

    LEAVE TO SCREEN gv_setscreen1.
  ENDIF.

*  scr0001-gesme     = ls_lqua-verme.
  scr0001-vemeh     = ls_lqua-meins.
  scr0001-meins     = ls_lqua-meins.
  scr0001-charg     = ls_lqua-charg.
  scr0001-werks     = ls_lqua-werks.
  scr0001-gesme_max = ls_lqua-verme.
  scr0001-lgort     = ls_lqua-lgort.


  MOVE 'SCR0001-GESME' TO cursorfield.

  LEAVE TO SCREEN gv_setscreen1.
*  CHECK sy-subrc = 0.
*  scr0001-matnr = ls_lqua-matnr.
*
*  SELECT SINGLE * FROM makt INTO ls_makt
*      WHERE matnr = ls_lqua-matnr AND
*            spras = sy-langu.
*  IF sy-subrc = 0.
*    scr0001-maktx_a = ls_makt-maktx(20).
*    scr0001-maktx_b = ls_makt-maktx+20(20).
*  ENDIF.
*
*  scr0001-charg     = ls_lqua-charg.
*  scr0001-gesme_max = ls_lqua-verme.
*  scr0001-vemeh     = ls_lqua-meins.
*  scr0001-meins     = ls_lqua-meins.
*  scr0001-werks     = ls_lqua-werks.
*  scr0001-lgort     = ls_lqua-lgort.
ENDFORM.                    " CHK_SSCC
*&---------------------------------------------------------------------*
*&      Form  CHK_EAN11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_ean11 .
  DATA: ls_marm TYPE marm.
  DATA: ls_lqua TYPE lqua.
  DATA: ls_makt TYPE makt.
**********************************************************************
  CHECK scr0001-ean11 IS NOT INITIAL.

  SELECT SINGLE * FROM lqua INTO ls_lqua
 WHERE lgnum = gv_whs
   AND lgtyp = scr0001-lgtyp
   AND lgpla = scr0001-lgpla.

  IF sy-subrc <> 0.
    MOVE scr0001-lgpla TO gv_text1.
**  Sem quantos na posição &
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '288'
        message_var1   = gv_text1.
    CLEAR scr0001.
    EXIT.
  ENDIF.

** Verifica se exite codigo de ean11 para o material e U.M.
*  SELECT SINGLE * FROM marm INTO ls_marm
*    WHERE matnr = ls_lqua-matnr AND
*          meinh = ls_lqua-meins.
  SELECT SINGLE * FROM marm INTO ls_marm
    WHERE matnr = ls_lqua-matnr AND
          ean11 = scr0001-ean11.

  IF sy-subrc <> 0.
    MOVE scr0001-ean11 TO gv_text1.
** Código EAN & inválido
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '284'
        message_var1   = gv_text1.

    CLEAR : scr0001-ean11.
    EXIT.
  ENDIF.
  scr0001-matnr = ls_lqua-matnr.
  SELECT SINGLE * FROM makt INTO ls_makt
      WHERE matnr = ls_lqua-matnr AND
            spras = sy-langu.
  IF sy-subrc = 0.
    scr0001-maktx_a = ls_makt-maktx(20).
    scr0001-maktx_b = ls_makt-maktx+20(20).
  ENDIF.

  scr0001-gesme_max = ( ls_lqua-verme * ls_marm-umren ) / ls_marm-umrez.
  " se unidade diferente da umb verifica quantidade maxima (pkl)
  IF ls_marm-meinh <> ls_lqua-meins.

    CALL FUNCTION 'ROUND'
      EXPORTING
        decimals      = 0
        input         = scr0001-gesme_max
        sign          = 'X'
      IMPORTING
        output        = scr0001-gesme_max
      EXCEPTIONS
        input_invalid = 1
        overflow      = 2
        type_invalid  = 3
        OTHERS        = 4.
  ENDIF.
*  scr0001-gesme = scr0001-gesme_max.
  scr0001-qtd   = ls_lqua-verme.
  scr0001-charg = ls_lqua-charg.
  scr0001-umren = ls_marm-umren.
  scr0001-umrez = ls_marm-umrez.
  scr0001-vemeh = ls_marm-meinh.
  scr0001-meins = ls_lqua-meins.
  scr0001-werks = ls_lqua-werks.
  scr0001-lgort = ls_lqua-lgort.

  MOVE 'SCR0001-GESME' TO cursorfield.

  LEAVE TO SCREEN gv_setscreen1.
ENDFORM.                    " CHK_EAN11
*&---------------------------------------------------------------------*
*&      Form  STATUS_001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM status_001 .

  DATA: lt_td_su TYPE gty_td_su.
  SET PF-STATUS 'GUI_0001'.
*  SET TITLEBAR 'xxx'.
  SET CURSOR FIELD cursorfield.

  LOOP AT SCREEN.
    CASE screen-name.
*      WHEN 'SCR0001-P_ORI'.
*        IF scr0001-p_ori IS INITIAL.
*          screen-input = '1'.
*        ELSE.
*          screen-input = '0'.
*        ENDIF.
      WHEN 'SCR0001-MATNR'.
        IF scr0001-matnr IS INITIAL.
          screen-input = '1'.
        ELSE.
          screen-input = '0'.
        ENDIF.
      WHEN 'SCR0001-LGTYP'.
        IF scr0001-lgtyp IS INITIAL AND
           scr0001-matnr IS NOT INITIAL.
          screen-input = '1'.
        ELSE.
          screen-input = '0'.
        ENDIF.
*      WHEN 'SCR0001-LGPLA'.
*        IF scr0001-lgtyp  = 'PRB' OR scr0001-lgtyp  = 'PKR'.
*          screen-input = '1'.
*        ELSE.
*          screen-input = '0'.
*        ENDIF.

      WHEN 'SCR0001-EAN11'.
        READ TABLE gt_td_su WITH KEY lgtyp = scr0001-lgtyp
        INTO lt_td_su.
        IF lt_td_su-lenvw IS INITIAL
        AND scr0001-ean11 IS INITIAL
        AND scr0001-lgtyp IS NOT INITIAL
        AND scr0001-matnr IS NOT INITIAL
        AND scr0001-lgtyp <> 'PCK'.
          screen-input = '1'.
        ELSE.
          screen-input = '0'.
        ENDIF.
      WHEN 'SCR0001-LENUM'.
        READ TABLE gt_td_su WITH KEY lgtyp = scr0001-lgtyp
        INTO lt_td_su.
        IF lt_td_su-lenvw IS NOT INITIAL
        AND scr0001-lenum IS INITIAL
        AND scr0001-lgtyp IS NOT INITIAL
        AND scr0001-matnr IS NOT INITIAL.
          screen-input = '1'.
        ELSE.
          screen-input = '0'.
        ENDIF.
*      WHEN 'SCR0001-GESME'.
*        IF ( scr0001-lenum IS NOT INITIAL
*        OR scr0001-ean11 IS NOT INITIAL )
*        AND scr0001-gesme IS INITIAL.
*          screen-input = '1'.
*        ELSE.
*          screen-input = '0'.
*        ENDIF.

*      WHEN 'SCR0001-BKTXT'.
*        IF scr0001-gesme IS NOT INITIAL AND scr0001-bktxt IS INITIAL.
*          screen-input = '1'.
*        ELSE.
*          screen-input = '0'.
*        ENDIF.
*      WHEN 'SCR0001-LGORT_D'.
*        IF scr0001-bktxt IS NOT INITIAL
*       AND scr0001-lgort_d IS INITIAL .
*          screen-input = '1'.
*        ELSE.
*          screen-input = '0'.
*        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " STATUS_001
*&---------------------------------------------------------------------*
*&      Form  CHK_GESME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_gesme .

  DATA: lt_t301 LIKE t301 OCCURS 0 WITH HEADER LINE.
  DATA: lt_vbbe TYPE TABLE OF vbbe.
  DATA: lt_vbb2 TYPE TABLE OF vbbe.
  DATA: wa_vbbe LIKE LINE OF lt_vbbe.
  DATA: wa_vbbe2 LIKE LINE OF lt_vbbe.
  DATA: lt_lqua_tot LIKE lqua OCCURS 0 WITH HEADER LINE.
  DATA: lt_lqua  LIKE lqua OCCURS 0 WITH HEADER LINE.
  DATA: lv_gesme TYPE lqua-gesme.
  DATA: lv_gesme_aux TYPE lqua-gesme.

  CHECK scr0001-gesme IS NOT INITIAL.

  IF scr0001-gesme > scr0001-gesme_max.
**    Quantidade não está disponivel
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '289'.

    CLEAR : scr0001-gesme.
    MOVE 'SCR0001-GESME' TO cursorfield.
    EXIT.
  ENDIF.


  SELECT * INTO TABLE lt_vbbe FROM vbbe
  WHERE matnr = scr0001-matnr AND
        werks = scr0001-werks AND
        lgort = scr0001-lgort .
  IF sy-subrc = 0.
    LOOP AT lt_vbbe INTO wa_vbbe.
      MOVE wa_vbbe-matnr TO wa_vbbe2-matnr.
      MOVE wa_vbbe-omeng TO wa_vbbe2-omeng.
      MOVE wa_vbbe-meins TO wa_vbbe2-meins.
      COLLECT wa_vbbe2 INTO lt_vbb2.
    ENDLOOP.


    SELECT * INTO TABLE lt_lqua FROM lqua
    WHERE lgnum = gv_whs         AND
*          lgtyp <> 'INC'         AND
          matnr = scr0001-matnr  AND
          bestq = '' AND
          skzue = '' AND
          skzua = '' AND
          skzse = '' AND
          skzsa = '' AND
          skzsi = ''.

    LOOP AT lt_lqua.
      CLEAR: lt_lqua_tot.
      MOVE lt_lqua-matnr TO lt_lqua_tot-matnr.
      MOVE lt_lqua-gesme TO lt_lqua_tot-gesme.
      MOVE lt_lqua-meins TO lt_lqua_tot-meins.
      COLLECT lt_lqua_tot.
    ENDLOOP.

  ENDIF.

**      verificar quanto é preciso
  READ TABLE lt_vbb2 INTO wa_vbbe WITH KEY matnr = scr0001-matnr.
  IF sy-subrc = 0.

    IF scr0001-vemeh <> lt_lqua_tot-meins.
      lv_gesme_aux = ( scr0001-gesme * scr0001-umrez ) / scr0001-umren.
    ELSE.
      lv_gesme_aux = scr0001-gesme.
    ENDIF.

    lv_gesme = wa_vbbe-omeng + lv_gesme_aux.
*        verificar quanto está disponivel sem ser no INC
    READ TABLE lt_lqua_tot WITH KEY matnr = scr0001-matnr.
    IF sy-subrc = 0 AND lv_gesme > lt_lqua_tot-gesme.
**    Quantidade não está disponivel
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '289'.

      CLEAR : scr0001-gesme.
      MOVE 'SCR0001-GESME' TO cursorfield.
      EXIT.
    ENDIF.
  ENDIF.

  MOVE 'SCR0001-BKTXT' TO cursorfield.

ENDFORM.                    " CHK_GESME
*&---------------------------------------------------------------------*
*&      Form  TRANSF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transf.

  DATA: lv_tanum           TYPE tanum.
  DATA: lv_lgort           TYPE lgort_d.
  DATA: lv_goodsmvt_code   LIKE bapi2017_gm_code.
  DATA: lv_matdocument     TYPE bapi2017_gm_head_ret-mat_doc.
  DATA: lv_matdocyear      TYPE bapi2017_gm_head_ret-doc_year.
  DATA: lv_kunwe           TYPE kunwe.
  DATA: lv_vbeln           TYPE vbeln_vl.
  DATA: ls_t001w           TYPE t001w.
  DATA: ls_msg             TYPE bdcmsgcoll.
  DATA: ls_trite           TYPE l03b_trite.
  DATA: ls_goodsmvt_header LIKE bapi2017_gm_head_01.
  DATA: lt_trite           TYPE l03b_trite_t.
  DATA: lt_goodsmvt_item   LIKE bapi2017_gm_item_create OCCURS 0 WITH
HEADER LINE.
  DATA: lt_return          LIKE bapiret2                OCCURS 0 WITH
HEADER LINE.
  DATA: lt_return_msg      LIKE bdcmsgcoll              OCCURS 0 WITH
HEADER LINE.
  DATA: lt_ltbk            LIKE ltbk                    OCCURS 0 WITH
HEADER LINE.
  DATA: lt_ltbp            LIKE ltbp                    OCCURS 0 WITH
HEADER LINE.
  DATA: lt_dlv_items       TYPE lips                    OCCURS 0 WITH
HEADER LINE.
  DATA: lv_tbnum      TYPE mseg-tbnum.
  DATA: lv_tbpos      TYPE mseg-tbpos.
  DATA: lv_aux        TYPE lqua-gesme,
        lv_qtd        TYPE lqua-gesme.
*  DATA: lv_tanum      TYPE ltap-tanum.
*** Movimento de MM
***********************************************************************
*  CHECK scr0001 IS INITIAL AND gt_items[] IS NOT INITIAL.
  CHECK scr0001-lgort_d IS NOT INITIAL.
  CLEAR: lv_goodsmvt_code, ls_goodsmvt_header, lt_goodsmvt_item.
  REFRESH: lt_goodsmvt_item.
*
** Dados Cabeçalho
*  lv_goodsmvt_code              = gv_gm_code. "Código da Bapi
  lv_goodsmvt_code              = '04'. "Código da Bapi
  ls_goodsmvt_header-doc_date   = sy-datum.   "Data de lançamento
  ls_goodsmvt_header-pstng_date = sy-datum.   "Data de documento
  ls_goodsmvt_header-header_txt = scr0001-bktxt.
*  ls_goodsmvt_header-ref_doc_no = scr_vbeln.

*** Items
  CLEAR lt_goodsmvt_item.
  lt_goodsmvt_item-move_type     = gv_mm_mov.
  lt_goodsmvt_item-plant         = scr0001-werks.
  lt_goodsmvt_item-stge_loc      = scr0001-lgort.
  lt_goodsmvt_item-move_plant    = scr0001-werks.
  lt_goodsmvt_item-move_stloc    = scr0001-lgort_d.

  lt_goodsmvt_item-material      = scr0001-matnr.
  lt_goodsmvt_item-batch         = scr0001-charg.

  IF scr0001-lenum IS INITIAL AND scr0001-lgtyp <> 'PCK'.
    lv_aux = ( scr0001-gesme * scr0001-umrez ) / scr0001-umren.
** se quantidade arrendondada maior que valor na pos criar doc com
** valor de pos
    IF lv_aux > scr0001-qtd.
      lv_aux = scr0001-qtd.
    ENDIF.
  ELSE.
    lv_aux = scr0001-gesme.
  ENDIF.

*  lt_goodsmvt_item-entry_qnt     = scr0001-gesme.
  lt_goodsmvt_item-entry_qnt     = lv_aux.
  lt_goodsmvt_item-entry_uom     = scr0001-meins.
  lt_goodsmvt_item-entry_uom_iso = scr0001-meins.
*  lt_goodsmvt_item-entry_uom     = scr0001-vemeh.
*  lt_goodsmvt_item-entry_uom_iso = scr0001-vemeh.
  COLLECT lt_goodsmvt_item.


** Movimento MM
  CLEAR: lv_matdocument, lv_matdocyear.
  CLEAR: lt_return.
  REFRESH lt_return.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = ls_goodsmvt_header
      goodsmvt_code    = lv_goodsmvt_code
    IMPORTING
      materialdocument = lv_matdocument
      matdocumentyear  = lv_matdocyear
    TABLES
      goodsmvt_item    = lt_goodsmvt_item
      return           = lt_return.

  READ TABLE lt_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    CLEAR ls_msg.
    ls_msg-msgid = lt_return-id.
    ls_msg-msgnr = lt_return-number.
    ls_msg-msgv1 = lt_return-message_v1.
    ls_msg-msgv2 = lt_return-message_v2.
    ls_msg-msgv3 = lt_return-message_v3.
    ls_msg-msgv4 = lt_return-message_v4.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = ls_msg-msgid
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = ls_msg-msgnr
        message_var1   = ls_msg-msgv1
        message_var2   = ls_msg-msgv2
        message_var3   = ls_msg-msgv3
        message_var4   = ls_msg-msgv4.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    CLEAR scr0001.
    CLEAR cursorfield.
    EXIT.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

** Movimento de WM
**********************************************************************
*  IF scr0001-meins <> scr0001-vemeh.
*    lv_aux = ( scr0001-gesme * scr0001-umrez ) / scr0001-umren.
*    lv_qtd = ( lv_aux * scr0001-umren ) / scr0001-umrez.
*
*    IF scr0001-gesme > lv_qtd.
*       scr0001-gesme = lv_aux + '0.001'.
*    else.
*      scr0001-gesme = lv_aux.
*    ENDIF.
*  ENDIF.

  CALL FUNCTION 'L_TO_CREATE_SINGLE'
    EXPORTING
      i_lgnum       = gv_whs
      i_bwlvs       = gv_wm_mov
      i_matnr       = scr0001-matnr
      i_werks       = scr0001-werks
      i_charg       = scr0001-charg
      i_anfme       = lv_aux
*      i_anfme       = scr0001-gesme
      i_altme       = scr0001-meins
      i_squit       = 'X'
      i_vltyp       = scr0001-lgtyp
      i_vlpla       = scr0001-lgpla
      i_vlenr       = scr0001-lenum
      i_nltyp       = gv_tp_dep_dest
      i_nlpla       = gv_pos_dep_dest
    IMPORTING
      e_tanum       = lv_tanum
    EXCEPTIONS
      no_to_created = 1
      OTHERS        = 34.

  IF sy-subrc <> 0.
    CLEAR ls_msg.
    ls_msg-msgid = sy-msgid.
    ls_msg-msgnr = sy-msgno.
    ls_msg-msgv1 = sy-msgv1.
    ls_msg-msgv2 = sy-msgv2.
    ls_msg-msgv3 = sy-msgv3.
    ls_msg-msgv4 = sy-msgv4.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = ls_msg-msgid
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = ls_msg-msgnr
        message_var1   = ls_msg-msgv1
        message_var2   = ls_msg-msgv2
        message_var3   = ls_msg-msgv3
        message_var4   = ls_msg-msgv4.

    CLEAR scr0001.
    CLEAR cursorfield.
    EXIT.
  ENDIF.

** Sucesso na tranf com Doc Mat & e OT &
  MOVE lv_matdocument TO gv_text1.
  MOVE lv_tanum       TO gv_text2.
  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWMMSG001'
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = '291'
      message_var1   = gv_text1
      message_var2   = gv_text2.
  CLEAR scr0001.
  CLEAR cursorfield.

ENDFORM.                    " TRANSF
*&---------------------------------------------------------------------*
*&      Form  CHK_LGORT_D
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_lgort_d .

  DATA: lt_t001l TYPE TABLE OF t001l.

**********************************************************************
  CHECK scr0001-lgort_d IS NOT INITIAL.

  SELECT * FROM t001l INTO TABLE lt_t001l
    WHERE werks = scr0001-werks
      AND lgort = scr0001-lgort_d.

  IF sy-subrc <> 0.
**  Depósito & inválido
    MOVE scr0001-lgort_d TO gv_text1.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '292'
        message_var1   = gv_text1.
    CLEAR scr0001-lgort_d.
  ENDIF.

ENDFORM.                    " CHK_LGORT_D
*&---------------------------------------------------------------------*
*&      Form  CHK_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_matnr .
  DATA: lt_mara TYPE TABLE OF mara.
  DATA: ls_makt TYPE makt.
**********************************************************************
  CHECK scr0001-matnr IS NOT INITIAL.

  SELECT * FROM mara INTO TABLE lt_mara
   WHERE matnr = scr0001-matnr.

  IF sy-subrc <> 0.
** Erro! Codigo de material & invalido!
    MOVE scr0001-matnr TO gv_text1.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '70'.
    CLEAR scr0001-matnr.
  ENDIF.



  SELECT SINGLE * FROM makt INTO ls_makt
      WHERE matnr = scr0001-matnr AND
            spras = sy-langu.
  IF sy-subrc = 0.
    scr0001-maktx_a = ls_makt-maktx(20).
    scr0001-maktx_b = ls_makt-maktx+20(20).
  ENDIF.
*
*  scr0001-charg     = ls_lqua-charg.
*  scr0001-gesme_max = ls_lqua-verme.
*  scr0001-vemeh     = ls_lqua-meins.
*  scr0001-meins     = ls_lqua-meins.
*  scr0001-werks     = ls_lqua-werks.
*  scr0001-lgort     = ls_lqua-lgort.
ENDFORM.                    " CHK_MATNR
*&---------------------------------------------------------------------*
*&      Form  CHK_LGTYP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_lgtyp .
  DATA: lt_lagp  TYPE TABLE OF lagp,
        ls_lagp  TYPE lagp,
        ls_lqua  TYPE lqua,
        lt_lqua  TYPE TABLE OF lqua.
  DATA: lt_td_su TYPE gty_td_su.
**********************************************************************

  CHECK scr0001-lgtyp IS NOT INITIAL.


** Verificar se a posição origem existe no armazém
  SELECT SINGLE * FROM lagp INTO ls_lagp
          WHERE lgnum = gv_whs
            AND lgtyp = scr0001-lgtyp.

  IF sy-subrc <> 0.
**  Tipo de deposito de origem & incorrecto!
    WRITE scr0001-lgtyp TO gv_text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '261'
        message_var1   = gv_text1.

    CLEAR scr0001-lgtyp.
    MOVE 'SCR0001-LGTYP' TO cursorfield.

    LEAVE TO SCREEN gv_setscreen1.
  ENDIF.

  READ TABLE gt_td_su WITH KEY lgtyp = scr0001-lgtyp
                                                TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
**  Tipo de deposito de origem & incorrecto!
    WRITE scr0001-lgtyp TO gv_text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '261'
        message_var1   = gv_text1.

    CLEAR scr0001-lgtyp.

    MOVE 'SCR0001-LGTYP' TO cursorfield.

    LEAVE TO SCREEN gv_setscreen1.
  ENDIF.


  SELECT * FROM lqua INTO TABLE lt_lqua
    WHERE lgnum = gv_whs           AND
          matnr = scr0001-matnr AND
          lgtyp = scr0001-lgtyp AND
          verme > 0.



  IF  sy-subrc <> 0 .
**Não existe stock para o material & no deposito &! 232
    WRITE scr0001-matnr TO gv_text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '293'
        message_var1   = gv_text1.

    CLEAR scr0001-lgtyp.

    MOVE 'SCR0001-LGTYP' TO cursorfield.

    LEAVE TO SCREEN gv_setscreen1.
  ENDIF.


  CLEAR ls_lqua.
  READ TABLE lt_lqua INTO ls_lqua INDEX 1.
  scr0001-lgpla = ls_lqua-lgpla.
*
*  SORT lt_lqua by lgpla.
**  DELETE ADJACENT DUPLICATES FROM lt_lqua COMPARING lgpla.
*  DELETE lt_lqua WHERE lgpla <>  ls_lqua-lgpla.
*  IF  LINES( lt_lqua ) = 1.
*    scr0001-lenum = ls_lqua-lenum.
*  ENDIF.

  CHECK scr0001-lgtyp = 'PCK'.

*  scr0001-gesme     = ls_lqua-verme.
  scr0001-vemeh     = ls_lqua-meins.
  scr0001-meins     = ls_lqua-meins.
  scr0001-charg     = ls_lqua-charg.
  scr0001-werks     = ls_lqua-werks.
  scr0001-gesme_max = ls_lqua-verme.
  scr0001-lgort     = ls_lqua-lgort.

  MOVE 'SCR0001-EAN11' TO cursorfield.

  LEAVE TO SCREEN gv_setscreen1.
ENDFORM.                    " CHK_LGTYP
*&---------------------------------------------------------------------*
*&      Form  CHK_BKTXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_bktxt .
  IF scr0001-bktxt IS NOT INITIAL.
    MOVE 'SCR0001-LGORT_D' TO cursorfield.
  ENDIF.
ENDFORM.                    " CHK_BKTXT
*&---------------------------------------------------------------------*
*&      Form  CHK_LGPLA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_lgpla .
  DATA: lt_lqua TYPE TABLE OF lqua.
  DATA: ls_lqua TYPE lqua.

  CHECK scr0001-lgpla IS NOT INITIAL.

  SELECT * FROM lqua INTO TABLE lt_lqua
      WHERE lgnum = gv_whs           AND
            matnr = scr0001-matnr AND
            lgtyp = scr0001-lgtyp AND
            lgpla = scr0001-lgpla.

  IF  sy-subrc <> 0 .
**Não existe stock para o material & no deposito & e posição &! 232
    WRITE scr0001-matnr TO gv_text1 LEFT-JUSTIFIED.
    WRITE scr0001-lgtyp TO gv_text2 LEFT-JUSTIFIED.
    WRITE scr0001-lgpla TO gv_text3 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '294'
        message_var1   = gv_text1
        message_var2   = gv_text2
        message_var3   = gv_text3.

    CLEAR:scr0001-lgpla,
          scr0001-ean11,
          scr0001-lenum,
          scr0001-vemeh,
          scr0001-meins,
          scr0001-charg,
          scr0001-werks,
          scr0001-gesme,
          scr0001-gesme_max,
          scr0001-lgort.
    MOVE 'SCR0001-LGPLA' TO cursorfield.

    LEAVE TO SCREEN gv_setscreen1.
  ENDIF.

  CLEAR ls_lqua.

  SORT lt_lqua BY lgpla.
*  DELETE ADJACENT DUPLICATES FROM lt_lqua COMPARING lgpla.
  READ TABLE lt_lqua INTO ls_lqua INDEX 1.
  DELETE lt_lqua WHERE lgpla <>  ls_lqua-lgpla.

  IF  LINES( lt_lqua ) = 1 and scr0001-lgtyp <> 'PKL'.
    scr0001-lenum     = ls_lqua-lenum.
    scr0001-vemeh     = ls_lqua-meins.
    scr0001-meins     = ls_lqua-meins.
    scr0001-charg     = ls_lqua-charg.
    scr0001-werks     = ls_lqua-werks.
    scr0001-gesme_max = ls_lqua-verme.
    scr0001-lgort     = ls_lqua-lgort.
  ENDIF.

  scr0001-lgpla = ls_lqua-lgpla.


  IF scr0001-lgtyp = 'PKL'.
    MOVE 'SCR0001-EAN11' TO cursorfield.
  ELSEif SCR0001-LENUM IS INITIAL .
    MOVE 'SCR0001-LENUM' TO cursorfield.
  ELSE.
    MOVE 'SCR0001-GESME' TO cursorfield.
  ENDIF.


  LEAVE TO SCREEN gv_setscreen1.

ENDFORM.                    " CHK_LGPLA
