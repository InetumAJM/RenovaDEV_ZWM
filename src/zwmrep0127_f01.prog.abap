*&---------------------------------------------------------------------*
*&  Include           ZWMMPRF013_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_whs.

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
*   ERRO: Utilizador não tem armazem atribuído!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '001'.

  ELSE.
    READ TABLE l_user WITH KEY statu = abap_true. "Util. Atrib. Arm.
    IF sy-subrc <> 0.
      WRITE l_user-lgnum TO text1 LEFT-JUSTIFIED.
*     ERRO: Utilizador não está atribuído ao armazém &!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMPMSG'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '002'
          message_var1   = text1.

    ELSE.
      whs = l_user-lgnum.
      IF l_user-devty(5) = '16X20'.
        setscreen1 = '0001'.
      ELSE.
        setscreen1 = '0011'.
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
FORM get_parameters.
  DATA: lt_messages TYPE tab_bdcmsgcoll.

  DO 1 TIMES.

** Parâmetro Centro
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = whs
        i_processo  = 'TRANSFER_DEPOSITO'
        i_parametro = 'CENTRO'
      IMPORTING
        e_valor     = gv_werks
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

** Parâmetro Depósito
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = whs
        i_processo  = 'TRANSFER_DEPOSITO'
        i_parametro = 'DEPOSITO'
      IMPORTING
        e_valor     = gv_lgort
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

** Parâmetro Depósito destino
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = whs
        i_processo  = 'TRANSFER_DEPOSITO'
        i_parametro = 'DEP_DESTINO'
      IMPORTING
        e_valor     = gv_lgort_dest
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    gv_lgort_dest_cons  = gv_lgort_dest.


** Parâmetro movimento WM_1 - Ordem Produção
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = whs
        i_processo  = 'TRANSFER_DEPOSITO'
        i_parametro = 'MOV_WM'
      IMPORTING
        e_valor     = gv_to_mov
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.


** Parâmetro Movimento MM - Devolução Ordem Produção
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = whs
        i_processo  = 'TRANSFER_DEPOSITO'
        i_parametro = 'MOV_MM'
      IMPORTING
        e_valor     = gv_gm_mov
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

*--> Mov de Desbloqueio
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = whs
        i_processo  = 'TRANSFER_DEPOSITO'
        i_parametro = 'MOV_MM_DESBLOQUEIO'
      IMPORTING
        e_valor     = gv_gm_mov_unblock
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

*--> Mov Bloqueio
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = whs
        i_processo  = 'TRANSFER_DEPOSITO'
        i_parametro = 'MOV_MM_S'
      IMPORTING
        e_valor     = gv_gm_mov_block
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

** Parâmetro Código da Bapi_Goodsmovment
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = whs
        i_processo  = 'TRANSFER_DEPOSITO'
        i_parametro = 'GM_CODE'
      IMPORTING
        e_valor     = gv_gm_code
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    IF sy-subrc <> 0.
      CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
        EXPORTING
          it_messages = lt_messages.
      LEAVE TO SCREEN 0.
    ENDIF.

** Parâmetro Email de Sucesso
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = whs
        i_processo  = 'TRANSFER_DEPOSITO'
        i_parametro = 'LISTA_DISTR_EMAIL'
      IMPORTING
        e_valor     = gv_email_target
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

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

**********************************************************************
**                          TELA 0001
**********************************************************************

*&---------------------------------------------------------------------*
*&      Form  CLEAR_FIELDS_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_fields_0001.

  CLEAR: scr_su, scr_matnr, scr_desc1, scr_desc2, scr_charg, scr_qtd,
         scr_uni, scr_pos_dest, scr_pos_conf, scr_pos_ori_typ,
         scr_pos_dest_lgtyp.
  CLEAR: gt_ltap.
  REFRESH: gt_ltap.
ENDFORM.                    " CLEAR_FIELDS_0001
*&---------------------------------------------------------------------*
*&      Form  CHECK_SCR_SU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_scr_su.
  DATA: wa_lqua TYPE lqua.
  DATA: wa_makt TYPE makt.
  DATA: lv_lenum LIKE lein-lenum.
  DATA: lv_maktx LIKE makt-maktx.
  DATA: lv_subrc LIKE sy-subrc.
  DATA: lv_user  LIKE sy-uname.
  DATA: ls_lein  TYPE lein.
  DATA: ls_ltak  TYPE ltak.
  DATA: lt_ltap  TYPE ltap OCCURS 0 WITH HEADER LINE.

**********************************************************************
** Validar SU
**********************************************************************
  CHECK scr_su IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = scr_su
    IMPORTING
      output = lv_lenum.

** Verificar se SU existe em armazém.
  SELECT SINGLE *
    FROM lein INTO ls_lein
    WHERE lenum = lv_lenum.

  IF sy-subrc <> 0.
    MOVE lv_lenum TO text1.
    MOVE whs      TO text2.

*   ERRO: Uni. depósito & não existe no armazém &!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '011'
        message_var1   = text1
        message_var2   = text2.

    PERFORM clear_fields_0001.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

** Verificar se existe OT pendente para SU
  SELECT *
    FROM ltap INTO TABLE lt_ltap
    WHERE lgnum = whs           AND
          pquit <> abap_true          AND
          nltyp = ls_lein-lgtyp AND
          nlpla = ls_lein-lgpla.

  DELETE lt_ltap WHERE vorga = 'ST' OR vorga = 'SL'. "Eliminar extornadas
  DELETE lt_ltap WHERE nlenr <> lv_lenum.

  IF lt_ltap[] IS NOT INITIAL.
    MOVE lv_lenum TO text1.
*     ERRO: Unid. Depósito já esta a ser arrumada pelo utilizador &!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '053'
        message_var1   = text1.

    PERFORM clear_fields_0001.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

**********************************************************************
** Obter dados da Su
**********************************************************************
  CLEAR: gt_ltap, scr_matnr, scr_desc1, scr_desc2, scr_charg, scr_pos_ori_typ,
         scr_qtd, scr_uni, scr_pos_dest, scr_pos_conf, scr_pos_dest_lgtyp.

  SELECT SINGLE * INTO wa_lqua FROM lqua WHERE lgnum = whs AND
                                               lenum = lv_lenum.
  IF sy-subrc = 0.
    MOVE wa_lqua-matnr TO scr_matnr.
    MOVE wa_lqua-charg TO scr_charg.
    MOVE wa_lqua-gesme TO scr_qtd.
    MOVE wa_lqua-meins TO scr_uni.
    MOVE wa_lqua-lgpla TO scr_pos_dest.
    MOVE wa_lqua-lgtyp TO scr_pos_dest_lgtyp.

    IF wa_lqua-plpos IS NOT INITIAL.
      CONCATENATE scr_pos_dest '-' wa_lqua-plpos INTO scr_pos_dest.
    ENDIF.

    MOVE wa_lqua-lgtyp TO scr_pos_ori_typ.
    SELECT SINGLE * INTO wa_makt FROM makt WHERE matnr = wa_lqua-matnr AND
                                                 spras = sy-langu.
    MOVE wa_makt-maktx(20) TO scr_desc1.
    MOVE wa_makt-maktx+20(20) TO scr_desc2.

    IF wa_lqua-bestq IS NOT INITIAL.
      MOVE abap_true TO gv_su_block.
    ELSE.
      CLEAR: gv_su_block.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_SCR_SU
*&---------------------------------------------------------------------*
*&      Form  CHECK_SCR_POS_CONF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_scr_pos_conf.

*  SELECT SINGLE * INTO wa_lagp FROM lagp WHERE lgnum = whs AND
*                                               lgpla = scr_pos_conf.
*  IF sy-subrc <> 0.
**   Pode ser de RACK entao temos de retirar os últimos 2 caractares.
*    lv_lgpla_aux = scr_pos_conf(8).
*    lv_sub_pos = scr_pos_conf+9(1).
*    SELECT SINGLE * INTO wa_lagp FROM lagp WHERE lgnum = whs AND
*                                                 lgpla = lv_lgpla_aux.
*    IF sy-subrc <> 0.
*      MOVE scr_pos_conf TO text1.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWMMPMSG'
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = '053'
*          message_var1   = text1.
*      CLEAR scr_pos_conf.
*      EXIT.
*    ELSE.
*      MOVE lv_lgpla_aux TO scr_pos_conf.
*    ENDIF.
*  ENDIF.
*
*  IF scr_pos_ori_typ = gv_interf_typ AND
*     wa_lagp-lgtyp <> scr_pos_ori_typ.
*
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMPMSG'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '054'.
*
*    CLEAR: scr_pos_conf.
*    EXIT.
*  ENDIF.
*
*  IF scr_pos_ori_typ IN r_arm_typ AND
*     wa_lagp-lgtyp NOT IN r_arm_typ.
*
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMPMSG'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '055'.
*
*    CLEAR: scr_pos_conf.
*    EXIT.
*  ENDIF.

ENDFORM.                    " CHECK_SCR_POS_CONF
*&---------------------------------------------------------------------*
*&      Form  SAVE_OT_DO_MM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_ot_do_mm USING VALUE(uv_su)       TYPE lenum
                         uv_teste_remontada TYPE flag.

  DATA: lt_return     LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
  DATA: return_msg    TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: ls_msg        TYPE bdcmsgcoll.
  DATA: ls_mseg       TYPE mseg.
  DATA: ls_ltak       TYPE ltak.
  DATA: wa_lagp       TYPE lagp.
  DATA: lv_tanum      TYPE tanum.
  DATA: lv_lgpla_aux  TYPE lgpla.
  DATA: lv_sub_pos    LIKE ltap-nppos.
  DATA: lt_ltap_vb    LIKE  ltap_vb OCCURS 0 WITH HEADER LINE.

  DATA: ls_goodsmvt_header LIKE bapi2017_gm_head_01.
  DATA: lv_goodsmvt_code   LIKE bapi2017_gm_code.
  DATA: lt_goodsmvt_item   LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE.
  DATA: lt_goodsmvt_item_unblock LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE.
  DATA: lv_matdocument     TYPE bapi2017_gm_head_ret-mat_doc.
  DATA: lv_matdocyear      TYPE bapi2017_gm_head_ret-doc_year.

  DATA: ls_mara_pal TYPE mara,
        ls_vekp     TYPE vekp,
        ls_zwm020   TYPE zwm020.

***********************************************************************
** Valida Paletes Remontadas
***********************************************************************
  DO 1 TIMES.
    CHECK uv_teste_remontada EQ abap_true.

    SELECT SINGLE * FROM zwm020
                    INTO ls_zwm020
                    WHERE armazem = whs AND
                          ( p1 = uv_su OR
                            p2 = uv_su ).

    CHECK sy-subrc EQ 0.

    IF uv_su EQ ls_zwm020-p1.
      PERFORM save_ot_do_mm USING ls_zwm020-p2
                                  abap_false.
    ELSEIF uv_su EQ ls_zwm020-p2.
      PERFORM save_ot_do_mm USING ls_zwm020-p1
                                   abap_false.
    ENDIF.
  ENDDO.


**********************************************************************
** CRIAR OT COM CONFIRMACAO AUTOMATICA
**********************************************************************
  CALL FUNCTION 'L_TO_CREATE_MOVE_SU'
    EXPORTING
      i_lenum       = uv_su
      i_bwlvs       = gv_to_mov
      i_squit       = abap_true
    IMPORTING
      e_tanum       = lv_tanum
    TABLES
      t_ltap_vb     = lt_ltap_vb
    EXCEPTIONS
      error_message = 1
      OTHERS        = 2.

  IF sy-subrc <> 0 OR lv_tanum IS INITIAL.
    CLEAR return_msg.
    MOVE sy-msgid TO return_msg-msgid.
    MOVE sy-msgty TO return_msg-msgtyp.
    MOVE sy-msgno TO return_msg-msgnr.
    MOVE sy-msgv1 TO return_msg-msgv1.
    MOVE sy-msgv2 TO return_msg-msgv2.
    MOVE sy-msgv3 TO return_msg-msgv3.
    MOVE sy-msgv4 TO return_msg-msgv4.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = return_msg-msgid
        message_lang   = sy-langu
        message_type   = return_msg-msgtyp
        message_number = return_msg-msgnr
        message_var1   = return_msg-msgv1
        message_var2   = return_msg-msgv2
        message_var3   = return_msg-msgv3
        message_var4   = return_msg-msgv4.
    EXIT.
  ENDIF.

**********************************************************************
** Movimento MM - Para Consumos
**********************************************************************
  CLEAR: lv_goodsmvt_code, ls_goodsmvt_header, lt_goodsmvt_item.
  REFRESH: lt_goodsmvt_item.

**  Dados Cabeçalho
  lv_goodsmvt_code              = gv_gm_code.
  ls_goodsmvt_header-pstng_date = sy-datum.
  ls_goodsmvt_header-header_txt = gv_bktxt.

**  Dados Items
  LOOP AT lt_ltap_vb.
*    lt_goodsmvt_item-move_type     = gv_gm_mov.
    IF gv_su_block IS INITIAL.
      lt_goodsmvt_item-move_type     = gv_gm_mov.
    ELSE.
      lt_goodsmvt_item-move_type     = gv_gm_mov_block.
    ENDIF.

    lt_goodsmvt_item-plant         = gv_werks.
    lt_goodsmvt_item-stge_loc      = gv_lgort.
    lt_goodsmvt_item-move_plant    = gv_werks.
    lt_goodsmvt_item-move_stloc    = gv_lgort_dest.

*   Material
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = lt_ltap_vb-matnr
      IMPORTING
        output = lt_goodsmvt_item-material.

    lt_goodsmvt_item-batch         = lt_ltap_vb-charg.
    lt_goodsmvt_item-entry_qnt     = lt_ltap_vb-nsola.
    lt_goodsmvt_item-entry_uom     = lt_ltap_vb-meins.
    lt_goodsmvt_item-entry_uom_iso = lt_goodsmvt_item-entry_uom.
    lt_goodsmvt_item-spec_mvmt     = 'I'.
    APPEND lt_goodsmvt_item.
    MOVE-CORRESPONDING lt_goodsmvt_item TO lt_goodsmvt_item_unblock.
    lt_goodsmvt_item_unblock-move_type     = gv_gm_mov_unblock.
    lt_goodsmvt_item_unblock-stge_loc      = gv_lgort_dest.
    CLEAR:lt_goodsmvt_item_unblock-move_stloc  .
    APPEND lt_goodsmvt_item_unblock.
  ENDLOOP.

**  Movimento de MM
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

    REFRESH: lt_return.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = lt_return.
    EXIT.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.


    IF gv_su_block IS NOT INITIAL.

      CLEAR: lv_matdocument, lv_matdocyear, lt_return.
      REFRESH: lt_return.
      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = ls_goodsmvt_header
          goodsmvt_code    = lv_goodsmvt_code
        IMPORTING
          materialdocument = lv_matdocument
          matdocumentyear  = lv_matdocyear
        TABLES
          goodsmvt_item    = lt_goodsmvt_item_unblock
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

        REFRESH: lt_return.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
          IMPORTING
            return = lt_return.
        EXIT.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ENDIF.
    ENDIF.
  ENDIF.

** Moviemnto de Paletes
***********************************************************************
  CLEAR: lt_goodsmvt_item.
  REFRESH lt_goodsmvt_item.

  DO 1 TIMES.
    SELECT SINGLE * FROM vekp
                    INTO ls_vekp
                    WHERE exidv = uv_su.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE * FROM mara
                    INTO ls_mara_pal
                    WHERE matnr = ls_vekp-vhilm.

    CHECK sy-subrc EQ 0.

    lt_goodsmvt_item-move_type     = gv_gm_mov.
    lt_goodsmvt_item-plant         = gv_werks.
    lt_goodsmvt_item-stge_loc      = gv_lgort.
    lt_goodsmvt_item-move_plant    = gv_werks.
    lt_goodsmvt_item-move_stloc    = gv_lgort_dest.
    lt_goodsmvt_item-material      = ls_vekp-vhilm.
    lt_goodsmvt_item-entry_qnt     = 1.
    lt_goodsmvt_item-entry_uom     = ls_mara_pal-meins.
    lt_goodsmvt_item-entry_uom_iso = lt_goodsmvt_item-entry_uom.
    lt_goodsmvt_item-spec_mvmt     = 'P'.
    APPEND lt_goodsmvt_item.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header = ls_goodsmvt_header
        goodsmvt_code   = lv_goodsmvt_code
      TABLES
        goodsmvt_item   = lt_goodsmvt_item
        return          = lt_return.


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

      REFRESH: lt_return.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
        IMPORTING
          return = lt_return.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ENDIF.

  ENDDO.


** Retorna Documento Material
***********************************************************************
  DO 20 TIMES.
    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    IF ls_mseg IS INITIAL.
      SELECT SINGLE * FROM mseg
                      INTO ls_mseg
                      WHERE mblnr = lv_matdocument AND
                            mjahr = lv_matdocyear.

      CHECK sy-subrc EQ 0.
    ENDIF.

    SELECT SINGLE * FROM ltak
                    INTO ls_ltak
                    WHERE lgnum = whs AND
                          tanum = lv_tanum.

    IF sy-subrc EQ 0.
      EXIT.
    ENDIF.
  ENDDO.

** Movimento com ot & com doc. material & efectuados com sucesso!
  MOVE lv_tanum       TO text1.
  MOVE lv_matdocument TO text2.

  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWMMPMSG'
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = '089'
      message_var1   = text1
      message_var2   = text2.

** Email
***********************************************************************
  CALL FUNCTION 'ZWM_PAL_TRANSFER_EMAIL'
    EXPORTING
      i_title      = 'Transf de Palete'(001)
      i_exidv      = uv_su
      i_lgort_dest = gv_lgort_dest
      is_mseg      = ls_mseg
      is_ltak      = ls_ltak
      i_target     = gv_email_target
      i_commit     = abap_true.

** Reset
***********************************************************************
  PERFORM clear_fields_0001.
  EXIT.

ENDFORM.                    " SAVE_OT_DO_MM
*&---------------------------------------------------------------------*
*&      Form  CHECK_LGORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_lgort .

  DATA: lv_back_lgort LIKE t001l-lgort.
  DATA: wa_t001l TYPE t001l.

  SELECT SINGLE * INTO wa_t001l
  FROM t001l WHERE werks = gv_werks AND
                   lgort = gv_lgort_dest.
  IF sy-subrc <> 0.
    MOVE gv_lgort_dest TO text1.
    MOVE gv_werks TO text2.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '080'
        message_var1   = text1
        message_var2   = text2.
    MOVE gv_lgort_dest_cons TO gv_lgort_dest.
  ENDIF.

ENDFORM.                    " CHECK_LGORT
