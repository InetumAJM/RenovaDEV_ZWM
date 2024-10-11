*&---------------------------------------------------------------------*
*&  Include           ZWMREP0124_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_screen_0001.
  PERFORM reset_0001.

  CHECK NOT gv_scr0001 IS INITIAL.
  CALL SCREEN gv_scr0001.
ENDFORM.                    " CALL_SCREEN_0001
*&---------------------------------------------------------------------*
*&      Form  RESET_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001.
  PERFORM reset_0001_lgpla_bc.
ENDFORM.                    " RESET_0001
*&---------------------------------------------------------------------*
*&      Form  FIND_WHZ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_whs.
  DATA: lt_user TYPE TABLE OF lrf_wkqu.

  DATA: ls_user TYPE lrf_wkqu.

  DATA: lv_resposta TYPE c.

  CLEAR gv_lgnum.

  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = lt_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  DO 1 TIMES.
    CHECK NOT lt_user IS INITIAL.

    READ TABLE lt_user
          INTO ls_user
           WITH KEY statu = abap_true.
    CHECK sy-subrc EQ 0.

    gv_lgnum = ls_user-lgnum.

    IF ls_user-devty(5) = '16X20'.
      gv_scr0001 = '0001'.
      gv_scr0002 = '0002'.
    ELSE.
      gv_scr0001 = '0010'.
      gv_scr0002 = '0020'.
    ENDIF.
  ENDDO.



  CHECK gv_lgnum IS INITIAL.

  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWMMSG001'
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = '003'.

  LEAVE TO SCREEN 0.
ENDFORM.                    " FIND_WHS
*&---------------------------------------------------------------------*
*&      Form  CONFIGURE_0001_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM configure_0001_fields.
  DATA: lv_subrc TYPE sysubrc.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'SCR0001-LGPLA_BC'.
        IF NOT scr0001-lgpla_bc IS INITIAL.
          screen-input = 0.
        ENDIF.
      WHEN 'SCR0001-MATNR'.
        IF scr0001-lgpla IS INITIAL OR
           NOT scr0001-matnr IS INITIAL.
          screen-input = 0.
        ENDIF.
      WHEN 'SCR0001-CHARG'.
        IF scr0001-matnr IS INITIAL OR
           NOT scr0001-charg IS INITIAL.
          screen-input = 0.
        ENDIF.

        IF scr0001-xchpf EQ abap_false.
          screen-input = 0.
        ENDIF.
      WHEN 'SCR0001-MENGE_C'.
        IF ( scr0001-charg IS INITIAL AND
             scr0001-xchpf EQ abap_true ) OR
          ( scr0001-matnr IS INITIAL AND
             scr0001-xchpf EQ abap_false ) OR
           NOT scr0001-menge_c IS INITIAL.
          screen-input = 0.
        ENDIF.
      WHEN 'SCR0001-BTN_ADIAR'.
        PERFORM check_execute_0001_adiar USING abap_false
                                         CHANGING lv_subrc.
        IF lv_subrc <> 0.
          screen-invisible = 1.
        ENDIF.

    ENDCASE.

    CASE screen-group1.
      WHEN 'LT'.
        IF NOT gt_lqua IS INITIAL OR
           scr0001-lgpla_bc IS INITIAL.
          screen-invisible = 1.
        ENDIF.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " CONFIGURE_0001_FIELDS
*&---------------------------------------------------------------------*
*&      Form  RESET_0001_LGPLA_BC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001_lgpla_bc.
  PERFORM bin_unlock.

  PERFORM reset_0001_struc.

  CLEAR: scr0001-lgpla_bc,
         scr0001-lgtyp,
         scr0001-lgpla,
         gt_lqua,
         gt_scr0001,
         scr0001_new,
         scr0001-pag,
         scr0001-pag_t.

  PERFORM reset_0001_matnr.
ENDFORM.                    " RESET_0001_LGPLA_BC
*&---------------------------------------------------------------------*
*&      Form  RESET_0001_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001_matnr.
  CLEAR: scr0001-matnr,
         scr0001-maktx,
         scr0001-maktxa,
         scr0001-maktxb,
         scr0001-xchpf,
         scr0001-meins.
  PERFORM reset_0001_charg.
ENDFORM.                    " RESET_0001_MATNR
*&---------------------------------------------------------------------*
*&      Form  RESET_0001_CHARG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001_charg.
  CLEAR: scr0001-charg,
         scr0001-menge_disp,
         scr0001-meins_disp.

  PERFORM reset_0001_menge_c.
ENDFORM.                    " RESET_0001_CHARG
*&---------------------------------------------------------------------*
*&      Form  RESET_0001_MENGE_C
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001_menge_c.
  CLEAR: scr0001-menge_c,
         scr0001-menge.
ENDFORM.                    " RESET_0001_MENGE_C
*&---------------------------------------------------------------------*
*&      Form  CHECK_0001_MENGE_C
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_0001_menge_c.
  DATA: lv_menge_c TYPE c LENGTH 18,
        lv_subrc   TYPE sysubrc.

  lv_menge_c = scr0001-menge_c.
  PERFORM reset_0001_menge_c.
  scr0001-menge_c = lv_menge_c.

** Valor
***********************************************************************
  CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
    EXPORTING
      input      = scr0001-menge_c
      internal   = abap_true
    IMPORTING
      output     = scr0001-menge_c
    EXCEPTIONS
      no_numeric = 1
      OTHERS     = 2.

  IF sy-subrc <> 0.
    PERFORM reset_0001_menge_c.
    EXIT.
  ENDIF.

  TRY.
      scr0001-menge  = scr0001-menge_c.
    CATCH cx_sy_conversion_overflow.
      PERFORM reset_0001_menge_c.
      EXIT.
    CATCH cx_sy_arithmetic_overflow.
      PERFORM reset_0001_menge_c.
      EXIT.
    CATCH cx_sy_conversion_no_number.
      PERFORM reset_0001_menge_c.
      EXIT.
  ENDTRY.


  WRITE scr0001-menge TO scr0001-menge_c UNIT scr0001-meins.

  IF scr0001-menge < 0.
    PERFORM reset_0001_menge_c.
    EXIT.
  ENDIF.

  DO 1 TIMES.
    CHECK scr0001-menge EQ 0.

    IF gt_lqua IS INITIAL.
      EXIT.
    ENDIF.


    IF scr0001-xchpf EQ abap_false.
**    Deseja limpar totalmente o Mat & da Posição &?
      PERFORM ask_decision
                  USING
                     'ZWM001'
                     '027'
                     scr0001-matnr
                     scr0001-lgpla
                     '' ''
                  CHANGING
                     lv_subrc.
    ELSE.
**    Deseja limpar totalmente o Mat & Lote & da Posição &?
      PERFORM ask_decision
                  USING
                     'ZWM001'
                     '002'
                     scr0001-matnr
                     scr0001-charg
                     scr0001-lgpla
                     ''
                  CHANGING
                     lv_subrc.

    ENDIF.

    IF lv_subrc <> 0.
      PERFORM reset_0001_menge_c.
      EXIT.
    ENDIF.
  ENDDO.

  IF scr0001-appended EQ abap_true.
    MODIFY gt_scr0001 FROM scr0001 INDEX scr0001-pag.
    PERFORM execute_0001_move USING 1.
  ELSE.
    PERFORM append_0001_line.
  ENDIF.
ENDFORM.                    " CHECK_0001_MENGE_C

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM user_command_0001.
  CASE scr0001-okcode.
    WHEN 'CLR'.
      PERFORM undo_0001.
    WHEN 'NEXT'.
      PERFORM execute_0001_new_pos.
    WHEN 'SAVE'.
      PERFORM execute_0001_save.
    WHEN 'UP'.
      PERFORM execute_0001_move USING 1.
    WHEN 'DN'.
      PERFORM execute_0001_move USING -1.
    WHEN 'ADIAR'.
      PERFORM execute_0001_adiar.
  ENDCASE.

  CLEAR scr0001-okcode.
ENDFORM.                    " USER_COMMAND_0001
*&---------------------------------------------------------------------*
*&      Form  UNDO_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM undo_0001.
  IF NOT scr0001-menge_c IS INITIAL.
    PERFORM reset_0001_menge_c.
  ELSEIF NOT scr0001-matnr IS INITIAL.
    PERFORM undo_0001_material.
  ENDIF.
ENDFORM.                                                    " UNDO_0001
*&---------------------------------------------------------------------*
*&      Form  CHECK_0001_LGPLA_BC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_0001_lgpla_bc.
  DATA: lt_messages TYPE tab_bdcmsgcoll,
        lt_lqua     TYPE TABLE OF lqua.

  DATA: ls_lagp TYPE lagp,
        ls_lqua TYPE lqua,
        ls_t331 TYPE t331.

  DATA: lv_lgpla_bc TYPE barcode,
        lv_lgpla_14 TYPE char14,
        lv_error    TYPE flag,
        lv_subrc    TYPE sysubrc.

  lv_lgpla_bc = scr0001-lgpla_bc.
  PERFORM reset_0001_lgpla_bc.
  scr0001-lgpla_bc = lv_lgpla_bc.

** Valida Posição
***********************************************************************
  CALL FUNCTION 'ZWM_BIN_CHECK'
    EXPORTING
      i_lgnum     = gv_lgnum
      i_lock_in   = '0'
      i_lock_out  = '0'
      i_volume    = abap_false
      i_dynamic   = abap_false
    IMPORTING
      es_lagp     = ls_lagp
      et_lqua     = gt_lqua
      et_messages = lt_messages
    CHANGING
      c_lgpla     = scr0001-lgpla_bc
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    PERFORM reset_0001_lgpla_bc.
    EXIT.
  ENDIF.

** Valida Stock a Entrar e A sair
***********************************************************************
  LOOP AT gt_lqua INTO ls_lqua WHERE einme > 0.

    PERFORM show_rf_message USING 'YWM001'
                                  'E' '043'
                                  scr0001-lgpla_bc
                                  '' '' ''.

    PERFORM reset_0001_lgpla_bc.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    EXIT.
  ENDIF.


** Tipo de Deposito e Posição
***********************************************************************
  scr0001-lgtyp = ls_lagp-lgtyp.
  scr0001-lgpla = ls_lagp-lgpla.

** Valida Tipo de Deposito
***********************************************************************
  SELECT SINGLE * FROM t331
                  INTO ls_t331
                  WHERE lgnum = gv_lgnum AND
                        lgtyp = scr0001-lgtyp.

  "Ini-31/05/24-AJM-Remover bloqueio ZWM131
**  IF ls_t331-lenvw EQ abap_true.
****  Posição & gerida por SU
**    PERFORM show_rf_message USING
**               'ZWM001'
**               'E'
**               '028'
**               scr0001-lgpla_bc
**               '' '' ''.
**    PERFORM reset_0001_lgpla_bc.
**    EXIT.
**  ENDIF.
  "Fim-31/05/24-AJM-Remover bloqueio ZWM131

  CALL FUNCTION 'ZWM_CONCATENATE_BIN'
    EXPORTING
      lgtyp = scr0001-lgtyp
      lgpla = scr0001-lgpla
    IMPORTING
      bin   = lv_lgpla_14.

  scr0001-lgpla_bc = lv_lgpla_14.

***********************************************************************
  lv_lgpla_14 = scr0001-lgpla_bc.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = gv_lgnum
      iv_bin_code = lv_lgpla_14
    IMPORTING
      ev_bin      = lv_lgpla_14
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
      PERFORM reset_0001_lgpla_bc.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.

** Ordena Tabela Para Binary Search
***********************************************************************
  SORT gt_lqua BY matnr ASCENDING
                  charg ASCENDING.
  lt_lqua = gt_lqua.

** Uma entrada por material
***********************************************************************
  DELETE ADJACENT DUPLICATES FROM lt_lqua COMPARING matnr.

** Guarda Nova Linha
***********************************************************************
  scr0001_new = scr0001.

** Bloqueia Posição
***********************************************************************
  PERFORM bin_lock CHANGING lv_subrc.

** Carrega Tela
***********************************************************************
  LOOP AT lt_lqua INTO ls_lqua.
    DO 1 TIMES.
      scr0001-matnr = ls_lqua-matnr.
      PERFORM check_0001_matnr.

      IF scr0001-matnr IS INITIAL.
        lv_error = abap_true.
        EXIT.
      ENDIF.

      IF NOT scr0001-xchpf IS INITIAL.
**       Pedido para remover validação de lote
*        scr0001-charg = ls_lqua-charg.
*        PERFORM check_0001_charg.
*
*        IF scr0001-charg IS INITIAL.
*          lv_error = abap_true.
*          EXIT.
*        ENDIF.
      ENDIF.
    ENDDO.

    IF NOT lv_error IS INITIAL.
      PERFORM reset_0001_lgpla_bc.
      EXIT.
    ENDIF.

    scr0001-appended = abap_true.

    APPEND scr0001 TO gt_scr0001.
  ENDLOOP.
  IF sy-subrc <> 0.
    SELECT SINGLE matnr FROM mlgt
                        INTO scr0001-matnr
                        WHERE lgnum = gv_lgnum AND
                              lgtyp = scr0001-lgtyp AND
                              lgpla = scr0001-lgpla.
    IF NOT scr0001-matnr IS INITIAL.
      PERFORM check_0001_matnr.
    ENDIF.

    EXIT.
  ENDIF.

  PERFORM load_scr0001 USING '1'.
ENDFORM.                    " CHECK_0001_LGPLA_bc
*&---------------------------------------------------------------------*
*&      Form  CHECK_0001_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_0001_matnr.
  DATA: lt_messages TYPE tab_bdcmsgcoll,
        lt_mcha     TYPE TABLE OF mcha,
        lt_mean     TYPE TABLE OF mean WITH HEADER LINE.

  DATA: ls_mcha TYPE mcha.

  DATA: lv_matnr TYPE matnr,
        lv_subrc TYPE sysubrc,
        lv_lines TYPE i.

  lv_matnr = scr0001-matnr.
  PERFORM reset_0001_matnr.
  scr0001-matnr = lv_matnr.


** Centro e Deposito
***********************************************************************
  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user      = sy-uname
      i_matnr     = scr0001-matnr
      i_recall    = 'X'
      i_usewm     = 'X'
      i_userf     = 'X'
      i_usemm     = 'X'
      i_useaut    = 'X'
      i_get_lgnum = 'X'
      i_get_werks = 'X'
      i_get_lgort = 'X'
      i_sel_werks = 'X'
      i_sel_lgort = 'X'
** IMPORTING
**   ET_MESSAGES         =
    CHANGING
      c_lgnum     = gv_lgnum
      c_werks     = scr0001-werks
      c_lgort     = scr0001-lgort
    EXCEPTIONS
      error       = 1
      user_back   = 2
      OTHERS      = 3.

** Valida Material
***********************************************************************
  CALL FUNCTION 'ZWM_MATERIAL_CHECK'
    EXPORTING
      i_werks     = scr0001-werks
      i_value     = scr0001-matnr
*     i_matnr_val =
    IMPORTING
*     E_MATNR     =
*     E_MTART     =
      e_xchpf     = scr0001-xchpf
      e_maktx     = scr0001-maktx
      e_maktx_a   = scr0001-maktxa
      e_maktx_b   = scr0001-maktxb
*     E_MHDHB     =
*     E_MHDRZ     =
      e_meins     = scr0001-meins
*     E_INSMK     =
*     ES_EAN128   =
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    PERFORM reset_0001_matnr.
    EXIT.
  ENDIF.

** Valida Unidade de embalagem
**********************************************************************
  IF scr0001-lgtyp = 'PKL'.
    SELECT *
      FROM mean INTO TABLE lt_mean
      WHERE matnr = scr0001-matnr.

    DELETE lt_mean WHERE eantp <> 'HE'.

    DESCRIBE TABLE lt_mean LINES lv_lines.

    IF lv_lines = 1.
      READ TABLE lt_mean INDEX 1.
      IF sy-subrc = 0.
        scr0001-meins = lt_mean-meinh.
      ENDIF.

    ELSEIF lv_lines > 1.
      PERFORM call_screen_0002.

      IF scr0002-ean IS INITIAL.
        PERFORM reset_0001_matnr.
        EXIT.
      ELSE.
        READ TABLE lt_mean WITH KEY ean11 = scr0002-ean.
        IF sy-subrc = 0.
          scr0001-meins = lt_mean-meinh.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

** Retorna Lote Mais Recente Para posição vazia
***********************************************************************
  IF gt_lqua IS INITIAL.
    SELECT * FROM mcha
       INTO TABLE lt_mcha
            WHERE matnr EQ scr0001-matnr AND
                  werks EQ scr0001-werks.

    SORT lt_mcha BY ersda DESCENDING.

    READ TABLE lt_mcha
          INTO ls_mcha
          INDEX 1.

    scr0001-charg = ls_mcha-charg.
  ENDIF.


** Valida Dados
***********************************************************************
  PERFORM validate_material_position CHANGING lv_subrc.
ENDFORM.                    " CHECK_0001_MATNR
*&---------------------------------------------------------------------*
*&      Form  GET_CONFIGURATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_configuration.
  DATA: lt_messages TYPE tab_bdcmsgcoll.

  DO 1 TIMES.

***--> Centro
**    CALL FUNCTION 'ZWM_GET_PARAMETER'
**      EXPORTING
**        i_lgnum     = gv_lgnum
**        i_processo  = 'INVENTARIO_ADHOC'
**        i_parametro = 'CENTRO'
**      IMPORTING
**        e_valor     = scr0001-werks
**        et_messages = lt_messages
**      EXCEPTIONS
**        error       = 1
**        OTHERS      = 2.
**    CHECK sy-subrc EQ 0.
**
***--> Depósito
**    CALL FUNCTION 'ZWM_GET_PARAMETER'
**      EXPORTING
**        i_lgnum     = gv_lgnum
**        i_processo  = 'INVENTARIO_ADHOC'
**        i_parametro = 'DEPOSITO'
**      IMPORTING
**        e_valor     = scr0001-lgort
**        et_messages = lt_messages
**      EXCEPTIONS
**        error       = 1
**        OTHERS      = 2.
**    CHECK sy-subrc EQ 0.

*--> Código MM para Entarda
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'INVENTARIO_ADHOC'
        i_parametro = 'CODE_MOV_MM_ENTRADA'
      IMPORTING
        e_valor     = gv_mm_code_ent
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

*--> Código MM para Saida
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'INVENTARIO_ADHOC'
        i_parametro = 'CODE_MOV_MM_SAIDA'
      IMPORTING
        e_valor     = gv_mm_code_sai
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

*--> Código MM para Transferencias
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'INVENTARIO_ADHOC'
        i_parametro = 'CODE_MOV_MM_TRANSF'
      IMPORTING
        e_valor     = gv_mm_code_transf
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

*--> Deposito para Saidas
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'INVENTARIO_ADHOC'
        i_parametro = 'DEPOSITO_DIFERENCAS'
      IMPORTING
        e_valor     = gv_lgort_dif
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

*--> Mov MM para Entrada
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'INVENTARIO_ADHOC'
        i_parametro = 'TIP_MOV_MM_ENTRADA'
      IMPORTING
        e_valor     = gv_bwart_ent
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

*--> Mov MM para Transferencias
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'INVENTARIO_ADHOC'
        i_parametro = 'TIP_MOV_MM_TRANSF'
      IMPORTING
        e_valor     = gv_bwart_transf
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.


*--> Mov MM para Saida
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'INVENTARIO_ADHOC'
        i_parametro = 'TIP_MOV_MM_SAIDA'
      IMPORTING
        e_valor     = gv_bwart_sai
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

*--> Mov WM para Entrada
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'INVENTARIO_ADHOC'
        i_parametro = 'TIP_MOV_WM_ENTRADA'
      IMPORTING
        e_valor     = gv_bwlvs_ent
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

*--> Mov WM para Saida
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'INVENTARIO_ADHOC'
        i_parametro = 'TIP_MOV_WM_SAIDA'
      IMPORTING
        e_valor     = gv_bwlvs_sai
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

*--> Codigo Expecial de Movimento
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'INVENTARIO_ADHOC'
        i_parametro = 'COD_ESPECIAL_MOV'
      IMPORTING
        e_valor     = gv_bsskz_mov
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

*--> Centro de Custo Entrada
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'INVENTARIO_ADHOC'
        i_parametro = 'CENTRO_CUSTO_ENTRADA'
      IMPORTING
        e_valor     = gv_kostl_ent
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.

*--> Centro de Custo Saída
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'INVENTARIO_ADHOC'
        i_parametro = 'CENTRO_CUSTO_SAIDA'
      IMPORTING
        e_valor     = gv_kostl_sai
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.
  ENDDO.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.                    " GET_CONFIGURATION
*&---------------------------------------------------------------------*
*&      Form  CHECK_0001_CHARG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_0001_charg.
  DATA: lt_messages TYPE tab_bdcmsgcoll.

  DATA: lv_charg TYPE charg_d,
        lv_subrc TYPE sysubrc.

  CHECK scr0001-xchpf EQ abap_true.

  lv_charg = scr0001-charg.
  PERFORM reset_0001_charg.
  scr0001-charg = lv_charg.

** Valida Lote
***********************************************************************
  CALL FUNCTION 'ZWM_BATCH_CHECK'
    EXPORTING
      i_matnr     = scr0001-matnr
      i_werks     = scr0001-werks
    IMPORTING
*     ES_EAN128   =
*     E_VFDAT     =
*     E_HSDAT     =
      et_messages = lt_messages
    CHANGING
      c_charg     = scr0001-charg
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    PERFORM reset_0001_charg.
    EXIT.
  ENDIF.

  PERFORM validate_material_position CHANGING lv_subrc.
ENDFORM.                    " CHECK_0001_CHARG
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_0001_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_0001_save.
  TYPES: BEGIN OF lty_movement,
           code        TYPE gm_code,
           bwlvs       TYPE bwlvs,
           werks       TYPE werks_d,
           lgort       TYPE lgort_d,
           bwart       TYPE bwart,
           nltyp       TYPE lgtyp,
           vltyp       TYPE lgtyp,
           nlpla       TYPE lgpla,
           vlpla       TYPE lgpla,
           lety1       TYPE lvs_letyp1,
           s_item      TYPE bapi2017_gm_item_create,
           type        TYPE c,
           lenum_nlenr TYPE lqua-lenum,
           lenum_vlenr TYPE lqua-lenum,
         END OF lty_movement.

  TYPES: BEGIN OF lty_dif_stock,
           matnr TYPE matnr,
           charg TYPE charg_d,
           menge TYPE menge_d,
         END OF lty_dif_stock.

  DATA: lt_messages    TYPE tab_bdcmsgcoll,
        lt_scr0001     TYPE TABLE OF gty_scr0001,
        lt_lqua        TYPE TABLE OF lqua,
        lt_mlgn        TYPE TABLE OF mlgn,
        lt_mchb        TYPE TABLE OF mchb,
        lt_mard        TYPE TABLE OF mard,
        lt_mcha        TYPE TABLE OF mcha,
        lt_zwm029      TYPE TABLE OF zwm029,
        lt_items       TYPE tab_bapi_goodsmvt_item,
        lt_movements   TYPE TABLE OF lty_movement,
        lt_movements_h TYPE TABLE OF lty_movement,
        lt_mm_mov      TYPE gty_t_mm_mov,
        lt_tanum       TYPE gty_t_tanum,
        lt_dif_stock   TYPE TABLE OF lty_dif_stock,
        lt_mkpf        TYPE HASHED TABLE OF mkpf WITH UNIQUE KEY mblnr mjahr.

  DATA: ls_lqua       TYPE lqua,
        ls_mlgn       TYPE mlgn,
        ls_mcha       TYPE mcha,
        ls_item       TYPE bapi2017_gm_item_create,
        ls_movement_h TYPE lty_movement,
        ls_movement   TYPE lty_movement,
        ls_mm_mov     TYPE gty_mm_mov,
        ls_mchb       TYPE mchb,
        ls_mard       TYPE mard,
        ls_zwm029     TYPE zwm029,
        ls_dif_stock  TYPE lty_dif_stock.

  DATA: lv_subrc      TYPE sysubrc,
        lv_error      TYPE flag,
        lv_menge_tot  TYPE menge_d,
        lv_menge      TYPE menge_d,
        lv_menge_save TYPE menge_d,
        lv_tanum      TYPE tanum,
        lv_lines_lqua TYPE sytabix,
        lv_index_lqua TYPE sytabix,
        lv_charg      TYPE charg_d,
        lv_meins      TYPE meins.

  FIELD-SYMBOLS: <ls_lqua>    TYPE lqua,
                 <ls_scr0001> TYPE gty_scr0001.

  CHECK NOT gt_scr0001 IS INITIAL.

  PERFORM check_execute_0001_save CHANGING lv_subrc.
  CHECK lv_subrc EQ 0.

  lt_scr0001 = gt_scr0001.

** Retorna Tipo de UD
***********************************************************************
*  SELECT * FROM mlgn
*     INTO TABLE lt_mlgn
*     FOR ALL ENTRIES IN gt_scr0001
*     WHERE matnr = gt_scr0001-matnr AND
*           lgnum = gv_lgnum.

** Retorna dados de Lotes
***********************************************************************
  IF NOT lt_scr0001 IS INITIAL.
    SELECT * FROM mcha
       INTO TABLE lt_mcha
       FOR ALL ENTRIES IN lt_scr0001
       WHERE matnr = lt_scr0001-matnr AND
             werks = lt_scr0001-werks.

    SORT lt_mcha BY matnr ASCENDING
                    werks ASCENDING
                    ersda DESCENDING.

    DELETE ADJACENT DUPLICATES FROM lt_mcha COMPARING matnr werks.
  ENDIF.
** Stock MM
***********************************************************************
  SELECT * FROM mchb
     INTO TABLE lt_mchb
     FOR ALL ENTRIES IN gt_scr0001
     WHERE matnr = gt_scr0001-matnr AND
           werks = gt_scr0001-werks AND
           lgort = gv_lgort_dif.

  SORT lt_mchb BY matnr ASCENDING
                  charg DESCENDING.

  SELECT * FROM mard
     INTO TABLE lt_mard
     FOR ALL ENTRIES IN gt_scr0001
     WHERE matnr = gt_scr0001-matnr AND
           werks = gt_scr0001-werks AND
           lgort = gv_lgort_dif.

  SORT lt_mard BY matnr ASCENDING.

** Documentos
***********************************************************************
  LOOP AT lt_scr0001 ASSIGNING <ls_scr0001>.
    CLEAR: lv_menge_tot.

    lt_lqua = gt_lqua.
    SORT lt_lqua BY matnr ASCENDING
                    charg ASCENDING.

    DELETE lt_lqua WHERE matnr <> <ls_scr0001>-matnr.

    " Unidade medida Base
    CALL FUNCTION 'ZWM_MATERIAL_CHECK'
      EXPORTING
        i_werks = <ls_scr0001>-werks
        i_value = <ls_scr0001>-matnr
      IMPORTING
        e_meins = lv_meins
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.

    IF lv_meins <> <ls_scr0001>-meins.
      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = <ls_scr0001>-matnr
          i_in_me              = <ls_scr0001>-meins
          i_out_me             = lv_meins
          i_menge              = <ls_scr0001>-menge
        IMPORTING
          e_menge              = <ls_scr0001>-menge
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.

      <ls_scr0001>-meins = lv_meins.
    ENDIF.

*--> Calcula a Quantidade Total deste Material
    LOOP AT lt_lqua INTO ls_lqua.
      lv_menge_tot = lv_menge_tot + ls_lqua-gesme.
    ENDLOOP.

    IF lv_menge_tot > <ls_scr0001>-menge.
**    Saida

      lv_menge_tot = lv_menge_tot - <ls_scr0001>-menge.

      LOOP AT lt_lqua ASSIGNING <ls_lqua>.
        CLEAR: ls_movement.

        IF lv_menge_tot > <ls_lqua>-verme.
          lv_menge = <ls_lqua>-verme.
        ELSEIF lv_menge_tot <= <ls_lqua>-verme.
          lv_menge = lv_menge_tot.
        ENDIF.

        lv_menge_tot = lv_menge_tot - lv_menge.
        <ls_lqua>-verme = <ls_lqua>-verme - lv_menge.

        ls_movement-code  = gv_mm_code_sai.
        ls_movement-bwlvs = gv_bwlvs_sai.

        ls_item-stge_loc = <ls_scr0001>-lgort.
        ls_item-move_stloc = gv_lgort_dif.
        ls_item-costcenter = gv_kostl_sai.

        ls_movement-bwart = gv_bwart_sai.
        ls_movement-vltyp = <ls_scr0001>-lgtyp.
        ls_movement-vlpla = <ls_scr0001>-lgpla.
        ls_movement-type  = 'S'.


        ls_item-material   = <ls_scr0001>-matnr.
        ls_item-plant      = <ls_scr0001>-werks.
        ls_item-stge_loc   = <ls_scr0001>-lgort.
        ls_item-batch      = <ls_lqua>-charg.
        ls_item-entry_qnt  = lv_menge.
        ls_item-entry_uom  = <ls_scr0001>-meins.

        ls_movement-werks  = <ls_scr0001>-werks.
        ls_movement-lgort  = <ls_scr0001>-lgort.
        ls_movement-lety1  = ls_mlgn-lety1.
        ls_movement-s_item = ls_item.
        ls_movement-lenum_vlenr  = <ls_lqua>-lenum. "31/05/24-AJM-Incluir posição destino

        CHECK ls_item-entry_qnt > 0.
        APPEND ls_movement TO lt_movements.
      ENDLOOP.

      IF lv_menge_tot > 0.
**      Quantidade de diferença súperior à Quantidade disponivel na Pos &
        PERFORM show_rf_message USING 'ZWM001' 'E' '029'
                                      <ls_scr0001>-lgpla_bc '' '' ''.
        RETURN.
      ENDIF.

    ELSEIF lv_menge_tot < <ls_scr0001>-menge.
      CLEAR: lt_dif_stock.

      lv_menge_tot = <ls_scr0001>-menge - lv_menge_tot.

      IF scr0001-xchpf_real EQ abap_true.
        LOOP AT lt_mchb INTO ls_mchb WHERE matnr = <ls_scr0001>-matnr.
          "Ini-18/06/24-AJM-Remover fração
          TRY.
              DATA(lv_int) = ls_mchb-clabs DIV 1.
*              IF ls_mchb-clabs MOD 1 NE 0.
              IF lv_int LE 0.
                CONTINUE.
              ELSE.
                ls_mchb-clabs = lv_int.
              ENDIF.
            CATCH cx_root.
          ENDTRY.
          "Ini-18/06/24-AJM-Remover fração
          CLEAR ls_dif_stock.
          ls_dif_stock-matnr = ls_mchb-matnr.
          ls_dif_stock-charg = ls_mchb-charg.
          ls_dif_stock-menge = ls_mchb-clabs.
          APPEND ls_dif_stock TO lt_dif_stock.
        ENDLOOP.
      ELSE.
        CLEAR ls_mard.
        READ TABLE lt_mard
              INTO ls_mard
              WITH KEY matnr = <ls_scr0001>-matnr
              BINARY SEARCH.

        IF sy-subrc EQ 0.
          CLEAR ls_dif_stock.
          ls_dif_stock-matnr = ls_mard-matnr.
          ls_dif_stock-menge = ls_mard-labst.
          APPEND ls_dif_stock TO lt_dif_stock.
        ENDIF.
      ENDIF.

      DELETE lt_dif_stock WHERE menge <= 0.

**    Entrada de Dif
      LOOP AT lt_dif_stock INTO ls_dif_stock.
        CLEAR: ls_movement.

        IF lv_menge_tot <= 0.
          EXIT.
        ENDIF.

        ls_movement-code  = gv_mm_code_transf.
        ls_movement-bwlvs = gv_bwlvs_ent.
        ls_movement-bwart = gv_bwart_transf.

        ls_item-stge_loc   = gv_lgort_dif.
        ls_item-move_stloc = <ls_scr0001>-lgort.
*        ls_item-costcenter = gv_kostl_ent.

        ls_movement-nltyp = <ls_scr0001>-lgtyp.
        ls_movement-nlpla = <ls_scr0001>-lgpla.
        ls_movement-type  = 'T'.

        ls_item-material   = ls_dif_stock-matnr.
        ls_item-plant      = <ls_scr0001>-werks.

        ls_item-batch      = ls_dif_stock-charg.

        IF ls_dif_stock-menge > lv_menge_tot.
          ls_item-entry_qnt = lv_menge_tot.
        ELSE.
          ls_item-entry_qnt = ls_dif_stock-menge.
        ENDIF.

        lv_menge_tot = lv_menge_tot - ls_item-entry_qnt.

        ls_item-entry_uom  = <ls_scr0001>-meins.

        ls_movement-werks  = <ls_scr0001>-werks.
        ls_movement-lgort  = <ls_scr0001>-lgort.
        ls_movement-lety1  = ls_mlgn-lety1.
        ls_movement-s_item = ls_item.

        CHECK ls_item-entry_qnt > 0.
        APPEND ls_movement TO lt_movements.
      ENDLOOP.

      CHECK lv_menge_tot > 0.

**    Entrada

      CLEAR lv_charg.
      IF lt_lqua IS INITIAL.
        CLEAR: ls_mcha.
        READ TABLE lt_mcha
              INTO ls_mcha
              WITH KEY matnr = <ls_scr0001>-matnr
                       werks = <ls_scr0001>-werks
              BINARY SEARCH.

        lv_charg = ls_mcha-charg.
      ELSE.
        DESCRIBE TABLE lt_lqua LINES lv_lines_lqua.
        CLEAR ls_lqua.
        READ TABLE lt_lqua
              INTO ls_lqua
              INDEX lv_lines_lqua.

        lv_charg = ls_lqua-charg.
      ENDIF.

      CLEAR: ls_movement.
      ls_movement-lenum_nlenr  = ls_lqua-lenum. "31/05/24-AJM-Incluir posição destino
      ls_movement-code  = gv_mm_code_ent.
      ls_movement-bwlvs = gv_bwlvs_ent.
      ls_movement-bwart = gv_bwart_ent.

      ls_item-stge_loc = <ls_scr0001>-lgort.
      ls_item-move_stloc = gv_lgort_dif.
      ls_item-costcenter = gv_kostl_ent.

      ls_movement-nltyp = <ls_scr0001>-lgtyp.
      ls_movement-nlpla = <ls_scr0001>-lgpla.
      ls_movement-type  = 'E'.

      ls_item-material   = <ls_scr0001>-matnr.
      ls_item-plant      = <ls_scr0001>-werks.
      ls_item-stge_loc   = <ls_scr0001>-lgort.
      ls_item-batch      = lv_charg.
      ls_item-entry_qnt  = lv_menge_tot.
      ls_item-entry_uom  = <ls_scr0001>-meins.

      ls_movement-werks  = <ls_scr0001>-werks.
      ls_movement-lgort  = <ls_scr0001>-lgort.
      ls_movement-lety1  = ls_mlgn-lety1.
      ls_movement-s_item = ls_item.

      CHECK ls_item-entry_qnt > 0.
      APPEND ls_movement TO lt_movements.
    ELSE.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  lt_movements_h = lt_movements.
  SORT lt_movements_h BY type.
  DELETE ADJACENT DUPLICATES FROM lt_movements_h COMPARING type.

** Desbloqueia Posição
***********************************************************************
  PERFORM bin_unlock.

** Inventaria Posição
***********************************************************************
  CLEAR lt_messages.

  LOOP AT lt_movements_h INTO ls_movement_h.
    CLEAR: lt_mm_mov,
           lt_tanum.

    IF NOT lt_messages IS INITIAL.
      EXIT.
    ENDIF.

*--> Cria OT
    LOOP AT lt_movements INTO ls_movement WHERE type = ls_movement_h-type.
      CALL FUNCTION 'ZWM_TO_CREATE_SINGLE'
        EXPORTING
          i_lgnum     = gv_lgnum
          i_bwlvs     = ls_movement-bwlvs
          i_werks     = ls_movement-werks
          i_lgort     = ls_movement-lgort
          i_matnr     = ls_movement-s_item-material
          i_charg     = ls_movement-s_item-batch
          i_anfme     = ls_movement-s_item-entry_qnt
          i_altme     = ls_movement-s_item-entry_uom
          i_letyp     = ls_movement-lety1
          i_vltyp     = ls_movement-vltyp
          i_vlpla     = ls_movement-vlpla
          i_nltyp     = ls_movement-nltyp
          i_nlpla     = ls_movement-nlpla
          i_invent    = abap_true
          i_squit     = abap_false
          i_commit    = abap_true
          i_nlenr     = ls_movement-lenum_nlenr "31/05/24-AJM-Incluir posição destino
          i_vlenr     = ls_movement-lenum_vlenr "31/05/24-AJM-Incluir posição destino
        IMPORTING
          e_tanum     = lv_tanum
          et_messages = lt_messages
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      IF sy-subrc <> 0.
        PERFORM cancel_ot USING lt_tanum.
        EXIT.
      ENDIF.

      APPEND lv_tanum TO lt_tanum.
    ENDLOOP.

    CHECK lt_messages IS INITIAL.

*--> Cria Documento Material
    CLEAR lt_items.
    LOOP AT lt_movements INTO ls_movement WHERE type = ls_movement_h-type.
      APPEND ls_movement-s_item TO lt_items.
    ENDLOOP.

    CHECK NOT lt_items IS INITIAL.

    CALL FUNCTION 'ZWM_GOODSMVT_CREATE'
      EXPORTING
        i_code      = ls_movement_h-code
        i_bwart     = ls_movement_h-bwart
        i_bsskz     = gv_bsskz_mov
        it_items    = lt_items
        i_commit    = abap_true
      IMPORTING
        e_mblnr     = ls_mm_mov-mblnr
        e_mjahr     = ls_mm_mov-mjahr
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      PERFORM cancel_ot USING lt_tanum.
      EXIT.
    ENDIF.

    APPEND ls_mm_mov TO lt_mm_mov.

*--> Espera Por Commit de Documento Material
    DO 20 TIMES.
      CLEAR lv_error.

      IF sy-index > 1.
        WAIT UP TO 1 SECONDS.
      ENDIF.

      CLEAR lt_mkpf.
      SELECT * FROM mkpf
         INTO TABLE lt_mkpf
         BYPASSING BUFFER
         FOR ALL ENTRIES IN lt_mm_mov
         WHERE mblnr = lt_mm_mov-mblnr AND
               mjahr = lt_mm_mov-mjahr.

      LOOP AT lt_mm_mov INTO ls_mm_mov.
        READ TABLE lt_mkpf
          WITH TABLE KEY mblnr = ls_mm_mov-mblnr
                         mjahr = ls_mm_mov-mjahr
          TRANSPORTING NO FIELDS.

        IF sy-subrc <> 0.
          lv_error = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_error IS INITIAL.
        PERFORM confirm_ot USING lt_tanum.
        EXIT.
      ENDIF.
    ENDDO.
  ENDLOOP.

** Bloqueio de Posição
***********************************************************************
  PERFORM bin_lock CHANGING lv_subrc.

** Erros
***********************************************************************
  IF NOT lt_messages IS INITIAL.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    PERFORM reset_0001_next_read.
    EXIT.
  ENDIF.

** Regista Erros de Entradas
***********************************************************************
*  PERFORM update_resgistry_error_table USING lt_zwm029.

** Apaga Posições Adiadas
***********************************************************************
  PERFORM clear_pos_adiada.

** Limpa Tela
***********************************************************************
  PERFORM reset_0001_next_read.
ENDFORM.                    " EXECUTE_0001_SAVE
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXECUTE_0001_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM check_execute_0001_save  CHANGING cv_subrc TYPE sysubrc.
  DATA: ls_scr0001 TYPE gty_scr0001.

  CLEAR cv_subrc.

  IF scr0001-menge_c IS INITIAL AND
     NOT scr0001-matnr IS INITIAL.
    cv_subrc = 4.
    EXIT.
  ENDIF.

  LOOP AT gt_scr0001 INTO ls_scr0001.
    IF ls_scr0001-menge_c IS INITIAL.

**    Existem Quantos não lidos
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWM001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '016'.

      cv_subrc = 4.
      RETURN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_EXECUTE_0001_SAVE
*&---------------------------------------------------------------------*
*&      Form  ASK_DECISION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0754   text
*      -->P_SCR0001_MATNR  text
*      -->P_SCR0001_LGPLA  text
*      -->P_0757   text
*      -->P_0758   text
*----------------------------------------------------------------------*
FORM ask_decision  USING uv_message_id
                         uv_message_number
                         uv_v1 uv_v2 uv_v3 uv_v4
                   CHANGING cv_subrc TYPE sysubrc.

  DATA: lv_message_id     TYPE bdc_mid,
        lv_message_number TYPE bdc_mnr,
        lv_var1           TYPE bdc_vtext1,
        lv_var2           TYPE bdc_vtext1,
        lv_var3           TYPE bdc_vtext1,
        lv_var4           TYPE bdc_vtext1.

  DATA: lv_resposta.

  CLEAR cv_subrc.

  lv_message_id = uv_message_id.
  lv_message_number = uv_message_number.
  lv_var1 = uv_v1.
  lv_var2 = uv_v2.
  lv_var3 = uv_v3.
  lv_var4 = uv_v4.

  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = lv_message_id
      message_lang   = sy-langu
      message_type   = 'W'
      message_number = lv_message_number
      message_var1   = lv_var1
      message_var2   = lv_var2
      message_var3   = lv_var3
      message_var4   = lv_var4
    IMPORTING
      ret_code       = lv_resposta.

  IF lv_resposta <> 'O'.
    cv_subrc = 4.
  ELSE.
    CLEAR cv_subrc.
  ENDIF.

ENDFORM.                    " ASK_DECISION
*&---------------------------------------------------------------------*
*&      Form  RESET_0001_NEXT_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001_next_read.
  PERFORM reset_0001_lgpla_bc.
ENDFORM.                    " RESET_0001_NEXT_READ
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_MATERIAL_POSITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_material_position CHANGING cv_subrc TYPE sysubrc.
  DATA: lt_lqua TYPE TABLE OF lqua.

  DATA: ls_lqua TYPE lqua.

  DATA: lv_var1    TYPE char50,
        lv_var2    TYPE char50,
        lv_var3    TYPE char50,
        lv_var4    TYPE char50,
        lv_msg_n   TYPE bdc_mnr,
        lv_lines   TYPE sytabix,
        lv_matnr_o TYPE matnr.

  CLEAR: cv_subrc.

  CHECK NOT scr0001-matnr IS INITIAL.

** Somente valida lote em posições vazias
***********************************************************************
  scr0001-xchpf_real = scr0001-xchpf.

  IF NOT gt_lqua IS INITIAL.
    CLEAR scr0001-xchpf.
  ENDIF.

  IF scr0001-xchpf EQ abap_true.
    CHECK NOT scr0001-charg IS INITIAL.
  ENDIF.

** Converte Para Mensagem
***********************************************************************
  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    EXPORTING
      input  = scr0001-matnr
    IMPORTING
      output = lv_matnr_o.


** Tabela interna
***********************************************************************
  lt_lqua = gt_lqua.
  DELETE ADJACENT DUPLICATES FROM lt_lqua COMPARING matnr.

** Valida se Quanto Existe
***********************************************************************
  DO 1 TIMES.
    CHECK '1' EQ '0'. "Validação desligada a pedido

    IF scr0001-xchpf EQ abap_true.
      DELETE lt_lqua WHERE matnr <> scr0001-matnr OR
                           charg <> scr0001-charg.

*     Mat. & Lt. & não presente na posição &. Deseja Continuar?
      lv_msg_n = '004'.
      lv_var1  = lv_matnr_o.
      lv_var2  = scr0001-charg.
      lv_var3  = scr0001-lgpla.
    ELSE.
      DELETE lt_lqua WHERE matnr <> scr0001-matnr.

*     Mat. & não presente na posição &. Deseja Continuar?
      lv_msg_n = '003'.
      lv_var1  = lv_matnr_o.
      lv_var2  = scr0001-lgpla.
      lv_var3  = ''.
    ENDIF.

    DESCRIBE TABLE lt_lqua LINES lv_lines.
    CHECK lv_lines EQ 0.

    PERFORM ask_decision
                USING
                   'ZWM001'
                   lv_msg_n
                   lv_var1
                   lv_var2
                   lv_var3
                   ''
                CHANGING
                   cv_subrc.
  ENDDO.

** Valida Se tem Mais que um Quanto
***********************************************************************
  DO 1 TIMES.
    CHECK cv_subrc IS INITIAL.

    IF scr0001-xchpf EQ abap_true.
*     Mat. & Lt. & contem mais que um Quanto na Posição &
      lv_msg_n = '007'.
      lv_var1  = lv_matnr_o.
      lv_var2  = scr0001-charg.
      lv_var3  = scr0001-lgpla.
    ELSE.
*     Mat. & contem mais que um Quanto na Posição &
      lv_msg_n = '006'.
      lv_var1  = lv_matnr_o.
      lv_var2  = scr0001-lgpla.
      lv_var3  = ''.
    ENDIF.

    CHECK lv_lines > 1.

    PERFORM show_rf_message
                USING
                   'ZWM001'
                   'E'
                   lv_msg_n
                   lv_var1
                   lv_var2
                   lv_var3
                   ''.

    cv_subrc = 4.
  ENDDO.

** Valida se Quanto Está Bloqueado
***********************************************************************
  DO 1 TIMES.
    CHECK cv_subrc IS INITIAL.

    IF scr0001-xchpf EQ abap_true.
      READ TABLE gt_lqua
            INTO ls_lqua
             WITH KEY matnr = scr0001-matnr
                      charg = scr0001-charg
              BINARY SEARCH.

*     Mat. & Lt. & bloqueado na Posição & (&)
      lv_msg_n = '009'.
      lv_var1  = lv_matnr_o.
      lv_var2  = scr0001-charg.
      lv_var3  = scr0001-lgpla.
      lv_var4 = ls_lqua-bestq.
    ELSE.
      READ TABLE gt_lqua
            INTO ls_lqua
             WITH KEY matnr = scr0001-matnr
              BINARY SEARCH.

*     Mat. & bloqueado na Posição & (&)
      lv_msg_n = '008'.
      lv_var1  = lv_matnr_o.
      lv_var2  = scr0001-lgpla.
      lv_var3 = ls_lqua-bestq.
      lv_var4 = ''.
    ENDIF.

    CHECK NOT ls_lqua-bestq IS INITIAL.

    PERFORM show_rf_message
                USING
                   'ZWM001'
                   'E'
                   lv_msg_n
                   lv_var1
                   lv_var2
                   lv_var3
                   ''.

    cv_subrc = 4.
  ENDDO.

** Retorna Quantidade Disponivel
***********************************************************************
  scr0001-meins_disp = scr0001-meins.

  LOOP AT gt_lqua INTO ls_lqua WHERE matnr = scr0001-matnr.

    IF ls_lqua-meins <> scr0001-meins.
      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = ls_lqua-matnr
          i_in_me              = ls_lqua-meins
          i_out_me             = scr0001-meins
          i_menge              = ls_lqua-gesme
        IMPORTING
          e_menge              = ls_lqua-gesme
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.
    ENDIF.

    scr0001-menge_disp = scr0001-menge_disp + ls_lqua-gesme.
  ENDLOOP.

** Valida se Já tem uma leitura
***********************************************************************
  DO 1 TIMES.
    CHECK NOT gt_scr0001 IS INITIAL.

    CHECK cv_subrc IS INITIAL.

    READ TABLE gt_scr0001
      WITH KEY matnr = scr0001-matnr
               charg = scr0001-charg
      TRANSPORTING NO FIELDS.

    CHECK sy-subrc EQ 0.
    cv_subrc = 4.

    IF scr0001-xchpf EQ abap_true.
*     Material & Lote & já lido na posição &
      lv_msg_n = '017'.
      lv_var1  = lv_matnr_o.
      lv_var2  = scr0001-charg.
      lv_var3  = sy-tabix.
      lv_var4 = ''.
    ELSE.
*    	Material & já lido na posição &
      lv_msg_n = '018'.
      lv_var1  = lv_matnr_o.
      lv_var2  = sy-tabix.
      lv_var3 = ''.
      lv_var4 = ''.
    ENDIF.

    PERFORM show_rf_message
                USING
                   'ZWM001'
                   'E'
                   lv_msg_n
                   lv_var1
                   lv_var2
                   lv_var3
                   ''.
  ENDDO.

** Erros
***********************************************************************
  CHECK cv_subrc <> 0.
  PERFORM reset_0001_matnr.
ENDFORM.                    " VALIDATE_MATERIAL_POSITION
*&---------------------------------------------------------------------*
*&      Form  SHOW_RF_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0086   text
*      -->P_0087   text
*      -->P_0088   text
*      -->P_SCR0001_LGTYP  text
*      -->P_0090   text
*      -->P_0091   text
*      -->P_0092   text
*----------------------------------------------------------------------*
FORM show_rf_message  USING uv_message_id
                            uv_message_type
                            uv_message_number
                            uv_message_var1
                            uv_message_var2
                            uv_message_var3
                            uv_message_var4.

  DATA: lv_message_id     TYPE  bdcmsgcoll-msgid,
        lv_message_type   TYPE  bdcmsgcoll-msgtyp,
        lv_message_number TYPE  bdcmsgcoll-msgnr,
        lv_message_var1   TYPE  bdcmsgcoll-msgv1,
        lv_message_var2   TYPE  bdcmsgcoll-msgv2,
        lv_message_var3   TYPE  bdcmsgcoll-msgv3,
        lv_message_var4   TYPE  bdcmsgcoll-msgv4.

  lv_message_id     = uv_message_id.
  lv_message_type   = uv_message_type.
  lv_message_number = uv_message_number.
  lv_message_var1   = uv_message_var1.
  lv_message_var2   = uv_message_var2.
  lv_message_var3   = uv_message_var3.
  lv_message_var4   = uv_message_var4.

  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = lv_message_id
      message_lang   = sy-langu
      message_type   = lv_message_type
      message_number = lv_message_number
      message_var1   = lv_message_var1
      message_var2   = lv_message_var2
      message_var3   = lv_message_var3
      message_var4   = lv_message_var4.

ENDFORM.                    " SHOW_RF_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  LOAD_SCR0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*----------------------------------------------------------------------*
FORM load_scr0001 USING VALUE(uv_page).
  PERFORM reset_0001_struc.

  READ TABLE gt_scr0001
        INTO scr0001
        INDEX uv_page.

  IF sy-subrc <> 0 AND NOT scr0001_new IS INITIAL.
    scr0001 = scr0001_new.
  ENDIF.

  MOVE uv_page TO scr0001-pag.

  DESCRIBE TABLE gt_scr0001 LINES scr0001-pag_t.
  scr0001-pag_t = scr0001-pag_t + 1.
ENDFORM.                    " LOAD_SCR0001
*&---------------------------------------------------------------------*
*&      Form  RESET_0001_STRUC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001_struc .
  DATA: ls_scr0001_back TYPE gty_scr0001.

  ls_scr0001_back = scr0001.
  CLEAR: scr0001.
  scr0001-werks = ls_scr0001_back-werks.
  scr0001-lgort = ls_scr0001_back-lgort.
ENDFORM.                    " RESET_0001_STRUC
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_0001_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P__1  text
*----------------------------------------------------------------------*
FORM execute_0001_move  USING uv_move TYPE i.
  DATA: lv_pag   TYPE i,
        lv_pag_t TYPE i.

** Deixa de Fazer sentido sem gestão de Lote
***********************************************************************
  EXIT.

  lv_pag   = scr0001-pag.
  lv_pag_t = scr0001-pag_t.

  lv_pag = lv_pag + uv_move.

  IF lv_pag <= 0.
    EXIT.
  ENDIF.

  IF lv_pag > lv_pag_t.
    EXIT.
  ENDIF.

  PERFORM load_scr0001 USING lv_pag.
ENDFORM.                    " EXECUTE_0001_MOVE
*&---------------------------------------------------------------------*
*&      Form  APPEND_0001_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_0001_line.
  DATA: lv_tabix TYPE sytabix.

  scr0001-new      = abap_true.
  scr0001-appended = abap_true.

  APPEND scr0001 TO gt_scr0001.
  lv_tabix = sy-tabix.

  PERFORM load_scr0001 USING lv_tabix.
ENDFORM.                    " APPEND_0001_LINE
*&---------------------------------------------------------------------*
*&      Form  UNDO_0001_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM undo_0001_material.
  DATA: lv_subrc TYPE sysubrc.

  IF scr0001-appended EQ abap_true AND
     scr0001-new EQ abap_true.
** 	Apagar leitura &?
*    PERFORM ask_decision
*                USING
*                   'ZWM001'
*                   '015'
*                   scr0001-pag
*                   ''
*                   ''
*                   ''
*                CHANGING
*                   lv_subrc.
*    CHECK lv_subrc EQ 0.
*
*    PERFORM delete_0001_line USING scr0001-pag.
*
*    PERFORM reset_0001_matnr.
*
*    EXIT.
  ELSEIF scr0001-appended EQ abap_true.
*    EXIT.
  ENDIF.

  IF NOT scr0001-charg IS INITIAL.
    PERFORM reset_0001_charg.
  ELSEIF NOT scr0001-matnr IS INITIAL.
    PERFORM reset_0001.
*    PERFORM reset_0001_matnr.
  ENDIF.
ENDFORM.                    " UNDO_0001_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  DELETE_0001_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SCR0001_PAG  text
*----------------------------------------------------------------------*
FORM delete_0001_line USING VALUE(uv_page).
  DELETE gt_scr0001 INDEX uv_page.

  PERFORM load_scr0001 USING uv_page.
ENDFORM.                    " DELETE_0001_LINE
*&---------------------------------------------------------------------*
*&      Form  CANCEL_MM_MOV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MM_MOV  text
*----------------------------------------------------------------------*
FORM cancel_mm_mov USING ut_mm_mov TYPE gty_t_mm_mov.
  DATA: ls_mm_mov TYPE gty_mm_mov.

  LOOP AT ut_mm_mov INTO ls_mm_mov.
    CALL FUNCTION 'ZWM_GOODSMVT_CANCEL'
      EXPORTING
        i_mblnr = ls_mm_mov-mblnr
        i_mjahr = ls_mm_mov-mjahr
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.
  ENDLOOP.
ENDFORM.                    " CANCEL_MM_MOV
*&---------------------------------------------------------------------*
*&      Form  CANCEL_OT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_TANUM  text
*----------------------------------------------------------------------*
FORM cancel_ot USING ut_tanum TYPE gty_t_tanum.
  DATA: lv_tanum TYPE tanum.

  LOOP AT ut_tanum INTO lv_tanum.
    CALL FUNCTION 'ZWM_TO_CANCEL'
      EXPORTING
        i_lgnum = gv_lgnum
        i_tanum = lv_tanum
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.
  ENDLOOP.

ENDFORM.                    " CANCEL_OT

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_0001_NEW_POS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_0001_new_pos.
  DATA: lv_subrc TYPE sysubrc.

  PERFORM check_0001_exit CHANGING lv_subrc.
  CHECK lv_subrc EQ 0.

  PERFORM reset_0001_lgpla_bc.
ENDFORM.                    " EXECUTE_0001_NEW_POS
*&---------------------------------------------------------------------*
*&      Form  CHECK_0001_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM check_0001_exit CHANGING cv_subrc TYPE sysubrc.
  CLEAR cv_subrc.

  CHECK NOT scr0001-lgpla_bc IS INITIAL.

** Esta ação irá apagar dados não gravados. Continuar?
  PERFORM ask_decision
              USING
                 'ZWM001'
                 '019'
                 '' ''
                 '' ''
              CHANGING
                 cv_subrc.
ENDFORM.                    " CHECK_0001_EXIT
*&---------------------------------------------------------------------*
*&      Form  EXIT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_0001.
  DATA: lv_subrc TYPE sysubrc.

  PERFORM check_0001_exit CHANGING lv_subrc.
  CHECK lv_subrc EQ 0.

  PERFORM reset_0001.

  LEAVE TO SCREEN 0.
ENDFORM.                                                    " EXIT_0001

*&---------------------------------------------------------------------*
*&      Form  bin_lock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bin_lock CHANGING cv_subrc TYPE sysubrc.
  DATA: lt_messages TYPE tab_bdcmsgcoll.

  CLEAR: cv_subrc.

  CHECK NOT scr0001-lgpla IS INITIAL AND
        NOT scr0001-lgtyp IS INITIAL.

  CALL FUNCTION 'ZWM_BIN_LOCK_UNLOCK'
    EXPORTING
      i_lgnum     = gv_lgnum
      i_lgtyp     = scr0001-lgtyp
      i_lgpla     = scr0001-lgpla
      i_skzue     = abap_true
      i_skzua     = abap_true
    IMPORTING
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    cv_subrc = 4.
    EXIT.
  ENDIF.
ENDFORM.                    "bin_lock

*&---------------------------------------------------------------------*
*&      Form  bin_unlock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bin_unlock.
  DATA: lt_messages TYPE tab_bdcmsgcoll.

  CHECK NOT scr0001-lgpla IS INITIAL AND
        NOT scr0001-lgtyp IS INITIAL.

  CALL FUNCTION 'ZWM_BIN_LOCK_UNLOCK'
    EXPORTING
      i_lgnum     = gv_lgnum
      i_lgtyp     = scr0001-lgtyp
      i_lgpla     = scr0001-lgpla
      i_skzue     = abap_false
      i_skzua     = abap_false
    IMPORTING
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    EXIT.
  ENDIF.
ENDFORM.                    "bin_unlock
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_OT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_TANUM  text
*----------------------------------------------------------------------*
FORM confirm_ot USING ut_tanum TYPE gty_t_tanum.
  DATA: lt_messages	    TYPE tab_bdcmsgcoll,
        lt_messages_app	TYPE tab_bdcmsgcoll.

  DATA: lv_tanum TYPE tanum.

  LOOP AT ut_tanum INTO lv_tanum.
    CALL FUNCTION 'ZWM_TO_CONFIRM'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_tanum     = lv_tanum
        i_quknz     = '4'
      IMPORTING
        et_messages = lt_messages_app
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    APPEND LINES OF lt_messages_app TO lt_messages.
  ENDLOOP.

  CHECK NOT lt_messages IS INITIAL.

  CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
    EXPORTING
      it_messages = lt_messages.

ENDFORM.                    " CONFIRM_OT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_RESGISTRY_ERROR_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZWM029  text
*----------------------------------------------------------------------*
FORM update_resgistry_error_table USING ut_zwm029 TYPE gty_t_zwm029.

  DATA: lt_zwm029     TYPE TABLE OF zwm029,
        lt_zwm029_new TYPE TABLE OF zwm029.

  DATA: ls_zwm029 TYPE zwm029.

  FIELD-SYMBOLS: <ls_zwm029_new> TYPE zwm029.


  CHECK NOT ut_zwm029 IS INITIAL.

** Entradas Actuais
***********************************************************************
  lt_zwm029_new = ut_zwm029.

  SELECT * FROM zwm029
     INTO TABLE lt_zwm029
     FOR ALL ENTRIES IN lt_zwm029_new
     WHERE lgtyp = lt_zwm029_new-lgtyp AND
           lgpla = lt_zwm029_new-lgpla AND
           matnr = lt_zwm029_new-matnr AND
           charg = lt_zwm029_new-charg.


  SORT lt_zwm029 BY lgtyp lgpla matnr charg.

** Inset and Update
***********************************************************************
  GET TIME.
  LOOP AT lt_zwm029_new ASSIGNING <ls_zwm029_new>.
    IF <ls_zwm029_new>-menge <= 0.
      DELETE lt_zwm029_new INDEX sy-tabix.
      CONTINUE.
    ENDIF.

*--> Entradas Antigas
    CLEAR ls_zwm029.
    READ TABLE lt_zwm029
          INTO ls_zwm029
          WITH KEY lgtyp = <ls_zwm029_new>-lgtyp
                   lgpla = <ls_zwm029_new>-lgpla
                   matnr = <ls_zwm029_new>-matnr
                   charg = <ls_zwm029_new>-charg
          BINARY SEARCH.

    <ls_zwm029_new>-menge = <ls_zwm029_new>-menge + ls_zwm029-menge.
    <ls_zwm029_new>-ernam = sy-uname.
    <ls_zwm029_new>-erdat = sy-datum.
    <ls_zwm029_new>-erzet = sy-uzeit.
  ENDLOOP.

** Modifica Tabela
***********************************************************************
  MODIFY zwm029 FROM TABLE lt_zwm029_new.
  COMMIT WORK.
ENDFORM.                    " UPDATE_RESGISTRY_ERROR_TABLE
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_0001_ADIAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_0001_adiar.
  DATA: ls_zwm050 TYPE zwm050.

  DATA: lv_subrc TYPE sysubrc.

  PERFORM check_execute_0001_adiar USING abap_false
                                   CHANGING lv_subrc.
  CHECK lv_subrc EQ 0.

  GET TIME.
  ls_zwm050-lgnum  = gv_lgnum.
  ls_zwm050-lgtyp  = scr0001-lgtyp.
  ls_zwm050-lgpla  = scr0001-lgpla.
  ls_zwm050-matnr  = scr0001-matnr.
  ls_zwm050-maktx  = scr0001-maktx.
  ls_zwm050-stkdif = scr0001-menge - scr0001-menge_disp.
  ls_zwm050-meins  = scr0001-meins.
  ls_zwm050-ernam  = sy-uname.
  ls_zwm050-erdat  = sy-datum.
  ls_zwm050-erzet  = sy-uzeit.

  MODIFY zwm050 FROM ls_zwm050.
  COMMIT WORK.

  PERFORM reset_0001_lgpla_bc.
ENDFORM.                    " EXECUTE_0001_ADIAR
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXECUTE_0001_ADIAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM check_execute_0001_adiar USING    uv_message TYPE flag
                              CHANGING cv_subrc TYPE sysubrc.

  IF scr0001-lgpla IS INITIAL OR
     scr0001-matnr IS INITIAL OR
     ( scr0001-charg IS INITIAL AND scr0001-xchpf EQ abap_true ).
    cv_subrc = 4.
    EXIT.
  ENDIF.

  IF scr0001-menge_c IS INITIAL.
    IF uv_message EQ abap_true.
**    Indicar Quantidade da Posição & para Material &
      PERFORM show_rf_message USING 'ZWM001' 'E' '039'
                                     scr0001-lgpla_bc scr0001-matnr
                                     '' ''.
    ENDIF.
    cv_subrc = 4.
    EXIT.
  ENDIF.

  IF scr0001-menge EQ scr0001-menge_disp.
    cv_subrc = 4.
    EXIT.
  ENDIF.

ENDFORM.                    " CHECK_EXECUTE_0001_ADIAR
*&---------------------------------------------------------------------*
*&      Form  CLEAR_POS_ADIADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_pos_adiada.

  DELETE FROM zwm050 WHERE lgnum = gv_lgnum AND
                           lgtyp = scr0001-lgtyp AND
                           lgpla = scr0001-lgpla.
  COMMIT WORK.

ENDFORM.                    " CLEAR_POS_ADIADA
*&---------------------------------------------------------------------*
*&      Form  EXIT_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_0002.

  CLEAR scr0002.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_0002_EAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_0002_ean.

  DATA: lt_mean TYPE TABLE OF mean WITH HEADER LINE.

** Validar EAN
**********************************************************************
  CHECK scr0002-ean IS NOT INITIAL.

  SELECT *
    FROM mean INTO TABLE lt_mean
    WHERE matnr = scr0001-matnr.

  READ TABLE lt_mean WITH KEY ean11 = scr0002-ean.
  IF sy-subrc <> 0.
    " EAN & não está definido para o material &
    PERFORM show_rf_message USING 'ZWM001' 'E' '076' scr0002-ean scr0001-matnr '' ''.

    CLEAR scr0002-ean.
    EXIT.
  ENDIF.

  IF lt_mean-eantp <> 'HE'.
    " EAN & não é do tipo HE definido para embalagem
    PERFORM show_rf_message USING 'ZWM001' 'E' '077' scr0002-ean '' '' ''.

    CLEAR scr0002-ean.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0002.

  CASE scr0002-okcode.

    WHEN 'NEXT'.
      PERFORM check_ean.
  ENDCASE.

  CLEAR scr0002-okcode.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_EAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ean.

  CHECK scr0002-ean IS NOT INITIAL.

  CLEAR scr0002-okcode.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_screen_0002.

  CLEAR scr0002.
  CALL SCREEN gv_scr0002.

ENDFORM.
