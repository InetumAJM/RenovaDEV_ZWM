*&---------------------------------------------------------------------*
*&  Include           ZWMREP0140_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FIND_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_whs.

  CLEAR gv_lgnum.
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

    LEAVE TO SCREEN 0.

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

      LEAVE TO SCREEN 0.

    ELSE.
      gv_lgnum = gs_user-lgnum.

      IF gs_user-devty(5) = '16X20'.
        gv_setscreen1 = '0001'.
      ELSE.
        gv_setscreen1 = '0011'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_CUSTOMIZING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_customizing.

  DATA: BEGIN OF it_001 OCCURS 0,
          processo  LIKE zwm001-processo,
          parametro LIKE zwm001-parametro,
          valor     LIKE zwm001-valor,
        END OF it_001.

  DATA: lt_zwm001 TYPE TABLE OF zwm001 WITH HEADER LINE.

** Obter parametros
**********************************************************************
  REFRESH: lt_zwm001.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = gv_lgnum
      i_processo  = 'PICKING'
      i_parametro = 'ST_TYPE'
    TABLES
      t_zwm001    = lt_zwm001
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  LOOP AT lt_zwm001.
    CLEAR gr_st_pick.
    gr_st_pick-sign   = 'I'.
    gr_st_pick-option = 'EQ'.
    gr_st_pick-low    = lt_zwm001-valor.
    APPEND gr_st_pick.
  ENDLOOP.

** Tipo Movimento
  SELECT processo parametro valor
          FROM  zwm001
          INTO CORRESPONDING FIELDS OF TABLE it_001
         WHERE armazem = gv_lgnum.

  SORT it_001 BY processo parametro.

  CLEAR it_001.
  READ TABLE it_001 WITH KEY processo  = 'TRANSFERENCIA'
                             parametro = 'MOV'.
  IF sy-subrc = 0.
    gv_bwlvs = it_001-valor.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen.

  SET SCREEN gv_setscreen1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXIT_COMMAND_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_command_0001.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR scr0001.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR sy-ucomm.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHK_POS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_pos.

  DATA: lt_messages TYPE tab_bdcmsgcoll,
        lt_lqua     TYPE TABLE OF lqua.

  DATA: ls_lagp TYPE lagp,
        ls_lqua TYPE lqua,
        ls_t331 TYPE t331,
        ls_makt TYPE makt.

  DATA: lv_lgpla_bc TYPE barcode,
        lv_lgpla_14 TYPE char14,
        lv_error    TYPE flag,
        lv_subrc    TYPE sysubrc,
        lv_menge    TYPE menge_d.

** Validar Posição
**********************************************************************
  CHECK scr0001-pos IS NOT INITIAL.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = gv_lgnum
      iv_bin_code = scr0001-pos
    IMPORTING
      ev_bin      = scr0001-pos
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    READ TABLE lt_messages INTO DATA(ls_messages) INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_messages-msgid
          message_lang   = sy-langu
          message_type   = ls_messages-msgtyp
          message_number = ls_messages-msgnr
          message_var1   = ls_messages-msgv1
          message_var2   = ls_messages-msgv2
          message_var3   = ls_messages-msgv3
          message_var4   = ls_messages-msgv4.
      CLEAR scr0001-pos.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.

  lv_lgpla_bc = scr0001-pos.

  CALL FUNCTION 'ZWM_BIN_CHECK'
    EXPORTING
      i_lgnum     = gv_lgnum
      i_lock_in   = '0'
      i_lock_out  = '0'
      i_volume    = abap_false
      i_dynamic   = abap_false
    IMPORTING
      es_lagp     = ls_lagp
      et_lqua     = lt_lqua
      et_messages = lt_messages
    CHANGING
      c_lgpla     = lv_lgpla_bc
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.

    CLEAR scr0001-pos.
    EXIT.
  ENDIF.

** Valida Stock a Entrar e A sair
  LOOP AT lt_lqua INTO ls_lqua WHERE einme > 0.

    MOVE scr0001-pos TO gv_text1.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'YWM001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '043'
        message_var1   = gv_text1.

    CLEAR scr0001-pos.
    EXIT.
  ENDLOOP.

  IF sy-subrc EQ 0.
    EXIT.
  ENDIF.

** Validar Stock
  IF lt_lqua[] IS INITIAL.
    MOVE scr0001-pos TO gv_text1.

    " Não existe stock para posição &!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '340'
        message_var1   = gv_text1.

    CLEAR: scr0001-pos.
    EXIT.
  ENDIF.

** Obter dados
**********************************************************************
  scr0001-lgtyp = ls_lagp-lgtyp.
  scr0001-lgpla = ls_lagp-lgpla.

  READ TABLE lt_lqua INDEX 1 INTO ls_lqua.
  IF sy-subrc = 0.
    scr0001-matnr = ls_lqua-matnr.
  ENDIF.

  SELECT SINGLE *
    FROM makt INTO ls_makt
    WHERE matnr = scr0001-matnr
    AND   spras = sy-langu.

  scr0001-maktx_a = ls_makt-maktx(20).
  scr0001-maktx_b = ls_makt-maktx+20(20).
  scr0001-uni     = ls_lqua-meins.

  LOOP AT lt_lqua INTO ls_lqua.
    lv_menge = lv_menge + ls_lqua-verme.
  ENDLOOP.

  scr0001-menge = lv_menge.

** Tipo depósito gerido por SU
  SELECT SINGLE *
    FROM t331 INTO ls_t331
    WHERE lgnum = gv_lgnum
    AND   lgtyp = scr0001-lgtyp.

  IF sy-subrc = 0.
    scr0001-lenvw = ls_t331-lenvw.
  ENDIF.

** Stock noutras posições
  REFRESH: lt_lqua.

  SELECT *
    FROM lqua INTO TABLE lt_lqua
    WHERE matnr = scr0001-matnr
    AND   lgnum = gv_lgnum
    AND   lgtyp = scr0001-lgtyp.

  DELETE lt_lqua WHERE verme <= 0.

  SORT lt_lqua BY lgpla.
  DELETE ADJACENT DUPLICATES FROM lt_lqua COMPARING lgpla.

  DESCRIBE TABLE lt_lqua LINES scr0001-npos.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHK_LENUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_lenum.

  DATA: ls_lein TYPE lein.
  DATA: ls_lqua TYPE lqua.

** Valida Palete
**********************************************************************
  CHECK scr0001-lenum IS NOT INITIAL.

** Valida a existênca SSCC
  SELECT SINGLE *
    FROM lein INTO ls_lein
    WHERE lenum = scr0001-lenum.

  IF sy-subrc <> 0.

    MOVE scr0001-lenum TO gv_text1.
    CONDENSE gv_text1.

    " Erro! SSCC & invalido!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '113'
        message_var1   = gv_text1.

    CLEAR scr0001-lenum.
    EXIT.
  ENDIF.

** Validar posição
  SELECT SINGLE *
    FROM lqua INTO ls_lqua
    WHERE lgnum = gv_lgnum
    AND   lgtyp = scr0001-lgtyp
    AND   lgpla = scr0001-lgpla
    AND   lenum = scr0001-lenum.

  IF sy-subrc <> 0.
    MOVE scr0001-lenum TO gv_text1.
    CONDENSE gv_text1.

    " Erro! O SSCC & não pertence a esta posição !
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '119'
        message_var1   = gv_text1.

    CLEAR scr0001-lenum.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHK_DEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_dest.

  DATA: lt_messages TYPE tab_bdcmsgcoll,
        lt_lqua     TYPE TABLE OF lqua.

  DATA: ls_lagp TYPE lagp,
        ls_lqua TYPE lqua,
        ls_t331 TYPE t331,
        ls_makt TYPE makt.

  DATA: lv_lgpla_bc TYPE barcode,
        lv_lgpla_14 TYPE char14,
        lv_error    TYPE flag,
        lv_subrc    TYPE sysubrc,
        lv_menge    TYPE menge_d.

** validar posição
**********************************************************************
  CHECK scr0001-dest IS NOT INITIAL.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = gv_lgnum
      iv_bin_code = scr0001-dest
    IMPORTING
      ev_bin      = scr0001-dest
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    READ TABLE lt_messages INTO DATA(ls_messages) INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_messages-msgid
          message_lang   = sy-langu
          message_type   = ls_messages-msgtyp
          message_number = ls_messages-msgnr
          message_var1   = ls_messages-msgv1
          message_var2   = ls_messages-msgv2
          message_var3   = ls_messages-msgv3
          message_var4   = ls_messages-msgv4.
      CLEAR scr0001-dest.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.

** Validar se posição é a mesma
  IF scr0001-pos EQ scr0001-dest.

    MOVE scr0001-dest TO gv_text1.

    " Erro! Posição não pode ser a mesma!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_type   = 'E'
        message_number = '229'
        message_lang   = sy-langu
        message_var1   = gv_text1.

    CLEAR scr0001-dest.
    EXIT.
  ENDIF.

** Validar Posição
  lv_lgpla_bc = scr0001-dest.

  CALL FUNCTION 'ZWM_BIN_CHECK'
    EXPORTING
      i_lgnum     = gv_lgnum
      i_lock_in   = '0'
      i_lock_out  = '0'
      i_volume    = abap_false
      i_dynamic   = abap_false
    IMPORTING
      es_lagp     = ls_lagp
      et_lqua     = lt_lqua
      et_messages = lt_messages
    CHANGING
      c_lgpla     = lv_lgpla_bc
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.

    CLEAR scr0001-dest.
    EXIT.
  ENDIF.

** Validar tipo depósito
  IF ls_lagp-lgtyp <> scr0001-lgtyp.

    " Erro! Tipo depósito destino & não permitido!
    WRITE  ls_lagp-lgtyp TO gv_text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_type   = 'E'
        message_number = '233'
        message_lang   = sy-langu
        message_var1   = gv_text1.

    CLEAR scr0001-dest.
    EXIT.
  ENDIF.

** Valida Stock a Entrar e A sair
  LOOP AT lt_lqua INTO ls_lqua WHERE einme > 0.

    MOVE scr0001-dest TO gv_text1.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'YWM001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '043'
        message_var1   = gv_text1.

    CLEAR scr0001-dest.
    EXIT.
  ENDLOOP.

  IF sy-subrc EQ 0.
    EXIT.
  ENDIF.

** Validar Stock
  IF lt_lqua[] IS INITIAL.
    MOVE scr0001-dest TO gv_text1.

    " Não existe stock para posição &!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '340'
        message_var1   = gv_text1.

    CLEAR: scr0001-dest.
    EXIT.
  ENDIF.

** Validar Material
  READ TABLE lt_lqua INDEX 1 INTO ls_lqua.
  IF sy-subrc = 0.
    IF ls_lqua-matnr <> scr0001-matnr.

      MOVE scr0001-dest  TO gv_text1.
      MOVE ls_lqua-matnr TO gv_text2.

      " Posição & com material & diferente!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '341'
          message_var1   = gv_text1
          message_var2   = gv_text2.

      CLEAR: scr0001-dest.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.
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
      PERFORM clear_fields.
    WHEN 'LIST'.
      PERFORM list.
    WHEN 'SAVE'.
      PERFORM save.
    WHEN OTHERS.

  ENDCASE.
  CLEAR sy-ucomm.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CLEAR_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_fields.

  IF scr0001-lenum IS NOT INITIAL.
    CLEAR: scr0001-lenum.

  ELSEIF scr0001-dest IS NOT INITIAL.
    CLEAR: scr0001-dest.

  ELSE.
    CLEAR: scr0001.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list.

  DATA: lv_lines   TYPE i.
  DATA: lv_field   TYPE char20.
  DATA: lv_wide    TYPE char1.
  DATA: lv_matnr   TYPE matnr.
  DATA: lv_meins   TYPE meins.
  DATA: lv_lgpla   TYPE lgpla.
  DATA: lv_qtd     TYPE char15.
  DATA: lv_titulo  TYPE char20.
  DATA: lt_lqua    TYPE TABLE OF lqua      WITH HEADER LINE.
  DATA: lt_rflista TYPE TABLE OF zwmrfs003 WITH HEADER LINE.

** Listar stock material
**********************************************************************
  CHECK scr0001-matnr IS NOT INITIAL AND scr0001-npos > 1.

*  IF scr0001-lgtyp = 'PKL'.
*    SELECT SINGLE meinh
*      FROM mean INTO lv_meins
*      WHERE matnr = scr0001-matnr
*      AND   ean11 = scr0001-ean11.
*
*  ELSE.
  SELECT SINGLE meins
    FROM mara INTO lv_meins
    WHERE matnr = scr0001-matnr.
*  ENDIF.

  SELECT *
    FROM lqua INTO TABLE lt_lqua
    WHERE matnr = scr0001-matnr
    AND   lgnum = gv_lgnum
    AND   lgtyp = scr0001-lgtyp.

  DELETE lt_lqua WHERE verme <= 0.
  DELETE lt_lqua WHERE lgpla = scr0001-lgpla.

  SORT lt_lqua BY lgpla.

** Listar posições
  LOOP AT lt_lqua.

    IF lt_lqua-meins <> lv_meins.
      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = lt_lqua-matnr
          i_in_me              = lt_lqua-meins
          i_out_me             = lv_meins
          i_menge              = lt_lqua-verme
        IMPORTING
          e_menge              = lt_lqua-verme
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.
    ENDIF.

    WRITE lt_lqua-verme TO lv_qtd LEFT-JUSTIFIED.
    CONDENSE lv_qtd.

    CLEAR lt_rflista.
    lt_rflista-rflabel = lt_lqua-lgpla.
    lt_rflista-rfvalor = lt_lqua-lgpla.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = lv_meins
      IMPORTING
        output         = lv_meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    CONCATENATE lv_qtd lv_meins INTO lt_rflista-rflabel2 SEPARATED BY space.

    APPEND lt_rflista.
  ENDLOOP.

  " Listagem Stock
  lv_matnr = scr0001-matnr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    EXPORTING
      input  = lv_matnr
    IMPORTING
      output = lv_matnr.

  CONCATENATE 'Material:' lv_matnr INTO lv_titulo SEPARATED BY space.

** Obter Posição
  CLEAR lv_lgpla.

  CALL FUNCTION 'ZWMMP_SCREEN_LIST'
    EXPORTING
      titulo              = lv_titulo
      no_sel              = ''
      wide_screen         = lv_wide
    IMPORTING
      valor_out           = lv_lgpla
    TABLES
      it_rflista          = lt_rflista
    EXCEPTIONS
      sem_entradas        = 1
      index_errado        = 2
      demasiadas_entradas = 3
      OTHERS              = 4.

  CHECK lv_lgpla IS NOT INITIAL.

  CONCATENATE scr0001-lgtyp lv_lgpla INTO scr0001-dest SEPARATED BY space.

  PERFORM chk_dest.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save.

  DATA: lv_lgpla_bc TYPE barcode.
  DATA: lv_werks    TYPE werks_d.
  DATA: lv_lgort    TYPE lgort_d.
  DATA: lv_tanum    TYPE tanum.
  DATA: ls_lagp     TYPE lagp.
  DATA: lt_lqua     TYPE TABLE OF lqua     WITH HEADER LINE.
  DATA: lt_sscc     TYPE TABLE OF zwm_sscc WITH HEADER LINE.

  DATA: BEGIN OF lt_return_msg OCCURS 0.
          INCLUDE STRUCTURE bdcmsgcoll.
        DATA: END OF lt_return_msg.

** Tranferencia de Material
**********************************************************************
  CHECK scr0001-dest IS NOT INITIAL AND scr0001-matnr IS NOT INITIAL.

  IF scr0001-lenvw = 'X'.
    CHECK scr0001-lenum IS NOT INITIAL.
  ENDIF.

** Posição destino
  lv_lgpla_bc = scr0001-dest.

  CALL FUNCTION 'ZWM_BIN_CHECK'
    EXPORTING
      i_lgnum    = gv_lgnum
      i_lock_in  = '0'
      i_lock_out = '0'
      i_volume   = abap_false
      i_dynamic  = abap_false
    IMPORTING
      es_lagp    = ls_lagp
    CHANGING
      c_lgpla    = lv_lgpla_bc
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.

** Stock
  SELECT *
    FROM lqua INTO TABLE lt_lqua
    WHERE matnr = scr0001-matnr
    AND   lgnum = gv_lgnum
    AND   lgtyp = scr0001-lgtyp
    AND   lgpla = scr0001-lgpla.

  IF scr0001-lenum IS NOT INITIAL.
    DELETE lt_lqua WHERE lenum <> scr0001-lenum.
  ENDIF.

  READ TABLE lt_lqua INDEX 1.
  IF sy-subrc = 0.
    lv_werks = lt_lqua-werks.
    lv_lgort = lt_lqua-lgort.
  ENDIF.

  REFRESH: lt_return_msg.
  CLEAR:   lt_return_msg.

  LOOP AT lt_lqua.
    CLEAR lt_sscc.
    lt_sscc-sscc          = lt_lqua-lenum.
    lt_sscc-tipo_su       = lt_lqua-letyp.
    lt_sscc-material      = lt_lqua-matnr.
    lt_sscc-quantidade    = lt_lqua-verme.
    lt_sscc-uni           = lt_lqua-meins.
    lt_sscc-lote_producao = lt_lqua-charg.
    APPEND lt_sscc.
  ENDLOOP.

  CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
    EXPORTING
      warehouse   = gv_lgnum
      mov_type    = gv_bwlvs
      st_type_o   = scr0001-lgtyp
      bin_origem  = scr0001-lgpla
      st_type_d   = ls_lagp-lgtyp
      bin_destino = ls_lagp-lgpla
*     STOCK_CAT   =
      plant       = lv_werks
      s_loc       = lv_lgort
*     CERTIFICADO =
      origem      = 'X'
*     REQ_NUMBER  =
*     REQ_TYPE    =
*     SSCC_ADICIONAL       =
    IMPORTING
      to          = lv_tanum
    TABLES
      return_msg  = lt_return_msg
      sscc        = lt_sscc
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0 OR lv_tanum IS INITIAL.
    READ TABLE lt_return_msg INDEX 1.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = lt_return_msg-msgid
        message_lang   = sy-langu
        message_type   = lt_return_msg-msgtyp
        message_number = lt_return_msg-msgnr
        message_var1   = lt_return_msg-msgv1
        message_var2   = lt_return_msg-msgv2
        message_var3   = lt_return_msg-msgv3
        message_var4   = lt_return_msg-msgv4.

    EXIT.
  ENDIF.

** Tranferência efetuada com sucesso
**********************************************************************
  MOVE lv_tanum TO gv_text1.

  " Tranferência & efetuada com sucesso!
  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWMMSG001'
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = '342'
      message_var1   = gv_text1.

  CLEAR scr0001.

ENDFORM.
