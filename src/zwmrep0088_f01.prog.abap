*&---------------------------------------------------------------------*
*&  Include           ZWMREP0088_F01
*&---------------------------------------------------------------------*

FORM initialization.
  DATA: lt_messages TYPE tab_bdcmsgcoll.

  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_userf     = abap_true
      i_get_werks = abap_false
      i_get_lgort = abap_false
    IMPORTING
      et_messages = lt_messages
    CHANGING
      c_lgnum     = gv_lgnum
    EXCEPTIONS
      error       = 1
      user_back   = 2
      OTHERS      = 3.
  IF sy-subrc <> 0.
    PERFORM show_message USING lt_messages.
    LEAVE TO SCREEN 0.
  ENDIF.


ENDFORM.                    " INITIALIZATION
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
  PERFORM call_screen USING '0001'.
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
  CLEAR: scr0001.
ENDFORM.                    " RESET_0001
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

** Screens
***********************************************************************
  LOOP AT SCREEN.

    CASE screen-name.
      WHEN 'RLMOB-PSAVE'.
        PERFORM check_save_0001 CHANGING lv_subrc.
        IF lv_subrc <> 0.
          screen-invisible = 1.
        ENDIF.
      WHEN 'RLMOB-PBACK'.
        PERFORM check_exit_0001 USING abap_false CHANGING lv_subrc.
        IF lv_subrc <> 0.
          screen-invisible = 1.
        ENDIF.
      WHEN 'SCR0001-EAN11'.
        IF NOT scr0001-ean11 IS INITIAL.
          screen-input = 0.
        ENDIF.
      WHEN 'SCR0001-EXIDV'.
        IF scr0001-ean11 IS INITIAL OR
           NOT scr0001-exidv IS  INITIAL.
          screen-input = 0.
        ENDIF.
      WHEN 'SCR0001-EXIDV2'.
        IF scr0001-exidv IS  INITIAL OR
           NOT scr0001-exidv2 IS  INITIAL.
          screen-input = 0.
        ENDIF.

        IF scr0001-remontada IS INITIAL.
          screen-input = 0.
        ENDIF.
      WHEN 'SCR0001-VHILM'.
        IF scr0001-exidv IS INITIAL OR
           ( scr0001-exidv2 IS INITIAL AND scr0001-remontada EQ abap_true ) OR
           NOT scr0001-vhilm IS INITIAL.
          screen-input = 0.
        ENDIF.
      WHEN 'SCR0001-MENGE'.
        IF scr0001-vhilm IS INITIAL OR
           NOT scr0001-menge_conf IS INITIAL.
          screen-input = 0.
        ENDIF.

        IF scr0001-remontada EQ abap_true.
          screen-input = 0.
        ENDIF.
      WHEN 'SCR0001-NLPLA_CONF'.
        IF scr0001-saved IS INITIAL OR
           scr0001-nlpla IS  INITIAL OR
           NOT scr0001-nlpla_conf IS  INITIAL.
          screen-input = 0.
        ENDIF.
    ENDCASE.

    CASE screen-group1.
      WHEN 'SV'.
        IF scr0001-saved <> abap_true.
          screen-input = 0.
          screen-invisible = 1.
          screen-output  = 0.
        ENDIF.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " CONFIGURE_0001_FIELDS

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0001.
  CASE scr0001-okcode.
    WHEN 'SAVE'.
      PERFORM save_0001.
    WHEN 'CLR'.
      PERFORM undo_0001.
  ENDCASE.

  CLEAR: scr0001-okcode.
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
  IF scr0001-saved EQ abap_true.
    PERFORM reset_nlpla_conf_0001.
  ELSEIF NOT scr0001-menge_conf IS INITIAL AND
     NOT scr0001-menge IS INITIAL AND
     NOT scr0001-vhilm IS INITIAL.
    PERFORM undo_menge_0001.
    IF scr0001-remontada EQ abap_true.
      IF NOT scr0001-vhilm IS INITIAL.
        PERFORM reset_vhilm_0001.
      ENDIF.
    ENDIF.
  ELSEIF NOT scr0001-vhilm IS INITIAL.
    PERFORM reset_vhilm_0001.
  ELSEIF NOT scr0001-exidv2 IS INITIAL.
    PERFORM reset_exidv2_0001.
  ELSEIF NOT scr0001-exidv IS INITIAL.
    PERFORM reset_exidv_0001.
  ELSEIF NOT scr0001-ean11 IS INITIAL.
    PERFORM reset_ean11_0001.
  ELSE.
    PERFORM reset_0001.
  ENDIF.
ENDFORM.                    " UNDO_0001
*&---------------------------------------------------------------------*
*&      Form  NEXT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM next_0001.
  DATA: lv_field TYPE c LENGTH 50,
        lv_subrc TYPE sysubrc.

  GET CURSOR FIELD lv_field.

  CASE lv_field.
    WHEN ''.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " NEXT_0001
*&---------------------------------------------------------------------*
*&      Form  call_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_USER_SCR01  text
*----------------------------------------------------------------------*
FORM call_screen  USING uv_dynnr TYPE sydynnr.
  CHECK sy-dynnr <> uv_dynnr.
  CALL SCREEN uv_dynnr.
ENDFORM.                    " call_screen
*&---------------------------------------------------------------------*
*&      Form  RELOAD_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reload_0001.

ENDFORM.                    " RELOAD_0001
*&---------------------------------------------------------------------*
*&      Form  EXIT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_0001 .
  DATA: lv_subrc TYPE sysubrc.

  PERFORM check_exit_0001 USING abap_true CHANGING lv_subrc.
  CHECK lv_subrc EQ 0.

  PERFORM reset_0001.
  LEAVE TO SCREEN 0.
ENDFORM.                    " EXIT_0001
*&---------------------------------------------------------------------*
*&      Form  SAVE_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_0001.
  DATA: lt_messages   TYPE tab_bdcmsgcoll.

  DATA: lv_subrc  TYPE sysubrc,
        lv_exidv  TYPE exidv,
        lv_exidv2 TYPE exidv.

  PERFORM check_save_0001 CHANGING lv_subrc.
  CHECK lv_subrc EQ 0.

  lv_exidv  = scr0001-exidv.
  lv_exidv2 = scr0001-exidv2.

  IF lv_exidv EQ 'X'. "Hack Generate SSCC
    CLEAR: lv_exidv.
  ENDIF.

  IF lv_exidv2 EQ 'X'. "Hack Generate SSCC
    CLEAR: lv_exidv2.
  ENDIF.

  CALL METHOD z_wm_cl_management=>copacking_production_entry
    EXPORTING
      i_lgnum     = gv_lgnum
      i_ean11     = scr0001-ean11
      i_menge     = scr0001-menge
      i_vhilm     = scr0001-vhilm
    IMPORTING
      et_messages = lt_messages
    CHANGING
      c_exidv     = lv_exidv
      c_exidv2    = lv_exidv2
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    PERFORM show_message USING lt_messages.
    EXIT.
  ENDIF.

  PERFORM set_nlpla_0001.

  scr0001-saved = abap_true.
ENDFORM.                    " SAVE_0001
*&---------------------------------------------------------------------*
*&      Form  CHECK_SAVE_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM check_save_0001 CHANGING cv_subrc TYPE sysubrc.
  CLEAR: cv_subrc.

  IF scr0001-menge IS INITIAL.
    cv_subrc = 4.
    EXIT.
  ENDIF.

  IF scr0001-vhilm IS INITIAL.
    cv_subrc = 4.
    EXIT.
  ENDIF.

  IF scr0001-menge_conf IS INITIAL.
    cv_subrc = 4.
    EXIT.
  ENDIF.

  IF scr0001-saved EQ abap_true.
    cv_subrc = 4.
    EXIT.
  ENDIF.
ENDFORM.                    " CHECK_SAVE_0001

*&---------------------------------------------------------------------*
*&      Form  RESET_0001_VBELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0001_vbeln.
  PERFORM reset_0001.
ENDFORM.                    " RESET_0001_VBELN
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXIDV_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_exidv_0001 .
  DATA: lv_exidv TYPE exidv.

  lv_exidv = scr0001-exidv.
  PERFORM reset_exidv_0001.
  scr0001-exidv = lv_exidv.

  CHECK NOT scr0001-exidv IS INITIAL.

  CHECK scr0001-exidv <> 'X'. "Hack Generate SSCC

  PERFORM validade_sscc CHANGING scr0001-exidv.
  IF scr0001-exidv IS INITIAL.
    PERFORM reset_exidv_0001.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXIDV2_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_exidv2_0001.
  DATA: lv_exidv TYPE exidv.

  lv_exidv = scr0001-exidv2.
  PERFORM reset_exidv2_0001.
  scr0001-exidv2 = lv_exidv.

  CHECK NOT scr0001-exidv2 IS INITIAL.

  CHECK scr0001-exidv2 <> 'X'. "Hack Generate SSCC

  IF scr0001-exidv2 EQ scr0001-exidv.
**  SSCC igual, leia uma etiqueta diferente
    CALL FUNCTION 'ZWM_RF_MESSAGE'
      EXPORTING
        i_message_id     = 'ZWM001'
        i_message_type   = 'E'
        i_message_number = '123'.
    PERFORM reset_exidv2_0001.
    EXIT.
  ENDIF.

  PERFORM validade_sscc CHANGING scr0001-exidv2.
  IF scr0001-exidv2 IS INITIAL.
    PERFORM reset_exidv2_0001.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_EAN11_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ean11_0001 .
  DATA: lt_marm   TYPE TABLE OF marm,
        lt_zwm067 TYPE TABLE OF zwm067.

  DATA: ls_marm   TYPE marm,
        ls_zwm067 TYPE zwm067.

  DATA: lv_ean11 TYPE ean11,
        lv_matnr TYPE matnr,
        lv_lines TYPE sytabix.

  lv_ean11 = scr0001-ean11.
  PERFORM reset_ean11_0001.
  scr0001-ean11 = lv_ean11.

  CHECK NOT scr0001-ean11 IS INITIAL.

  DO 1 TIMES.
    SELECT * FROM marm
             INTO TABLE lt_marm
             WHERE ean11 = scr0001-ean11.
    CHECK sy-subrc <> 0.
    lv_matnr = scr0001-ean11.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = lv_matnr
      IMPORTING
        output       = lv_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    CHECK sy-subrc EQ 0.


    SELECT * FROM marm
             INTO TABLE lt_marm
             WHERE matnr = lv_matnr AND
                   numtp = 'IC'.
  ENDDO.

  IF lt_marm IS INITIAL.
**  Material & inválido
    CALL FUNCTION 'ZWM_RF_MESSAGE'
      EXPORTING
        i_message_id     = 'ZWM001'
        i_message_type   = 'E'
        i_message_number = '118'
        i_message_var1   = scr0001-ean11.
    PERFORM reset_ean11_0001.
    EXIT.
  ENDIF.

  SORT lt_marm BY matnr.

  SELECT * FROM zwm067
           INTO TABLE lt_zwm067
           FOR ALL ENTRIES IN lt_marm
           WHERE lgnum = gv_lgnum AND
                 matnr = lt_marm-matnr AND
                 datbe => sy-datum AND
                 datuv <= sy-datum AND
                 inactive = abap_false AND
                 deleted = abap_false.

  IF sy-subrc <> 0.
**  Material & não é válido para manipulação
    CALL FUNCTION 'ZWM_RF_MESSAGE'
      EXPORTING
        i_message_id     = 'ZWM001'
        i_message_type   = 'E'
        i_message_number = '119'
        i_message_var1   = scr0001-ean11.
    PERFORM reset_ean11_0001.
    EXIT.
  ENDIF.

  DESCRIBE TABLE lt_zwm067 LINES lv_lines.
  IF lv_lines > 1.
**  Mais que uma linha activa para Material &
    CALL FUNCTION 'ZWM_RF_MESSAGE'
      EXPORTING
        i_message_id     = 'ZWM001'
        i_message_type   = 'E'
        i_message_number = '122'
        i_message_var1   = scr0001-ean11.
    PERFORM reset_ean11_0001.
    EXIT.
  ENDIF.

  CLEAR ls_zwm067.
  READ TABLE lt_zwm067
        INTO ls_zwm067
        INDEX 1.

  CLEAR: ls_marm.
  READ TABLE lt_marm
        INTO ls_marm
        WITH KEY matnr = ls_zwm067-matnr
        BINARY SEARCH.

  scr0001-ean11 = ls_marm-ean11.
  scr0001-matnr = ls_marm-matnr.
  scr0001-meinh = ls_marm-meinh.

  SELECT SINGLE maktx FROM makt
                      INTO scr0001-maktx_a
                      WHERE matnr = scr0001-matnr AND
                            spras = sy-langu.

  scr0001-maktx_b = scr0001-maktx_a+20(20).
  scr0001-maktx_a = scr0001-maktx_a(20).

  scr0001-remontada = z_wm_cl_management=>is_remontada( i_lgnum = gv_lgnum is_data = scr0001 ).

  z_wm_cl_management=>get_qtd_pal_material( EXPORTING i_lgnum = gv_lgnum i_matnr = scr0001-matnr i_remontadas_double = abap_false IMPORTING e_menge = scr0001-menge_pal ).

  PERFORM reset_exidv_0001.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  RESET_EAN11_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_ean11_0001.
  PERFORM reset_0001.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  RESET_VHILM_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_vhilm_0001 .
  CLEAR: scr0001-vhilm.
  PERFORM reset_menge_0001.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  RESET_MENGE_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_menge_0001 .
  CLEAR: scr0001-menge, scr0001-menge_conf.
  scr0001-menge = scr0001-menge_pal.
  IF scr0001-remontada EQ abap_true.
    scr0001-menge_conf = abap_true.
  ENDIF.
  PERFORM reset_nlpla_conf_0001.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHOW_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MESSAGES  text
*----------------------------------------------------------------------*
FORM show_message USING ut_messages TYPE tab_bdcmsgcoll.
  DATA: ls_message TYPE bdcmsgcoll.

  READ TABLE ut_messages
        INTO ls_message
        INDEX 1.

  CALL FUNCTION 'ZWM_RF_MESSAGE'
    EXPORTING
      i_message_id     = ls_message-msgid
      i_message_type   = ls_message-msgtyp
      i_message_number = ls_message-msgnr
      i_message_var1   = ls_message-msgv1
      i_message_var2   = ls_message-msgv2
      i_message_var3   = ls_message-msgv3
      i_message_var4   = ls_message-msgv4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  RESET_EXIDV2_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_exidv2_0001 .
  CLEAR: scr0001-exidv2.
  PERFORM reset_vhilm_0001.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  RESET_EXIDV_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_exidv_0001 .
  CLEAR: scr0001-exidv.
  PERFORM reset_exidv2_0001.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALIDADE_SSCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SCR0001_EXIDV  text
*----------------------------------------------------------------------*
FORM validade_sscc  CHANGING cv_exidv.
  DATA: ls_vekp TYPE vekp,
        ls_ltap TYPE ltap.

  IF z_wm_cl_management=>is_sscc( cv_exidv ) <> abap_true.
**  SSCC & inválido
    CALL FUNCTION 'ZWM_RF_MESSAGE'
      EXPORTING
        i_message_id     = 'ZWM001'
        i_message_type   = 'E'
        i_message_number = '120'
        i_message_var1   = cv_exidv.
    CLEAR: cv_exidv.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM ltap
                  INTO ls_ltap
                  WHERE lgnum = gv_lgnum AND
                        nlenr = cv_exidv.

  IF sy-subrc EQ 0.
**  SSCC & já existe
    CALL FUNCTION 'ZWM_RF_MESSAGE'
      EXPORTING
        i_message_id     = 'ZWM001'
        i_message_type   = 'E'
        i_message_number = '117'
        i_message_var1   = cv_exidv.
    CLEAR: cv_exidv.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_VHILM_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vhilm_0001.
  DATA: ls_mara TYPE mara.

  DATA: lv_vhilm TYPE vhilm.

  lv_vhilm = scr0001-vhilm.
  PERFORM reset_vhilm_0001.
  scr0001-vhilm = lv_vhilm.

  CHECK NOT scr0001-vhilm IS INITIAL.

  SELECT SINGLE * FROM mara
                  INTO ls_mara
                  WHERE matnr = scr0001-vhilm AND
                        mtart = 'PALT'.

  IF sy-subrc <> 0.
**  Material de Embalagem & inválido
    CALL FUNCTION 'ZWM_RF_MESSAGE'
      EXPORTING
        i_message_id     = 'ZWM001'
        i_message_type   = 'E'
        i_message_number = '121'
        i_message_var1   = scr0001-vhilm.
    PERFORM reset_vhilm_0001.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_MENGE_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  check_menge_0001.
  DATA: lv_menge     TYPE menge_d,
        lv_menge_pal TYPE menge_d.

  lv_menge = scr0001-menge.
  PERFORM reset_menge_0001.
  scr0001-menge = lv_menge.

  CHECK scr0001-menge > 0.

  IF scr0001-remontada EQ abap_true.
    IF scr0001-menge <> scr0001-menge_pal.
      CALL FUNCTION 'ZWM_RF_MESSAGE'
        EXPORTING
          i_message_id     = 'ZWM001'
          i_message_type   = 'E'
          i_message_number = '112'.
      PERFORM reset_menge_0001.
      EXIT.
    ENDIF.
  ENDIF.

  scr0001-menge_conf = abap_true.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_NLPLA_CONF_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_nlpla_conf_0001 .
  DATA: lv_nlpla TYPE ltap_nlpla.

  lv_nlpla = scr0001-nlpla_conf.
  PERFORM reset_nlpla_conf_0001.
  scr0001-nlpla_conf = lv_nlpla.

  IF scr0001-nlpla_conf <> scr0001-nlpla.
    PERFORM reset_nlpla_conf_0001.
    EXIT.
  ENDIF.

  PERFORM reset_0001.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  RESET_NLPLA_CONF_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_nlpla_conf_0001 .
  CLEAR: scr0001-nlpla_conf.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_NLPLA_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_nlpla_0001.
  scr0001-nlpla = 'MAN_ENT1'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UNDO_MENGE_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM undo_menge_0001.
  IF scr0001-remontada EQ abap_true.
    scr0001-menge = scr0001-menge_pal.
  ELSE.
    PERFORM reset_menge_0001.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXIT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM check_exit_0001 USING uv_confirm TYPE flag
                     CHANGING cv_subrc TYPE sysubrc.
  DATA: lv_retcode TYPE c.

  CLEAR: cv_subrc.

  IF scr0001-okcode EQ 'CANCEL'.
    EXIT. "GUI Exit
  ENDIF.

  IF scr0001-saved EQ abap_true.
    cv_subrc = 4.
    EXIT.
  ENDIF.

  IF NOT scr0001-ean11 IS INITIAL AND
     scr0001-saved EQ abap_false AND
     uv_confirm eq abap_true.
**  Sair e perder todos os dados lidos?
    CALL FUNCTION 'ZWM_RF_MESSAGE'
      EXPORTING
        i_message_id     = 'ZWM001'
        i_message_type   = 'W'
        i_message_number = '049'
      IMPORTING
        e_ret_code       = lv_retcode.

    IF lv_retcode <> 'O'.
      cv_subrc = 4.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.
