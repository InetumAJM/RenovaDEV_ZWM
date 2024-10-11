*&---------------------------------------------------------------------*
*&  Include           ZWMFR0004_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN_PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection_screen_pbo.
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'P_FEVOR'.
        screen-input = 1.
      WHEN 'P_AUFNR'.
        screen-input = 1.
      WHEN 'P_MENGE'.
        screen-input = 1.
      WHEN 'P_VHILM'.
        screen-input = 1.
      WHEN 'P_PRINT'.
        screen-input = 1.
      WHEN 'P_COPIES'.
        screen-input = 1.
      WHEN 'P_NRPAL'.
        screen-input = 1.
      WHEN OTHERS.
        screen-input = 0.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

  IF p_fevor IS INITIAL.
    SET CURSOR FIELD 'P_FEVOR'.
  ELSEIF p_aufnr IS INITIAL.
    SET CURSOR FIELD 'P_AUFNR'.
  ELSEIF p_menge IS INITIAL.
    SET CURSOR FIELD 'P_MENGE'.
  ELSEIF p_vhilm IS INITIAL.
    SET CURSOR FIELD 'P_VHILM'.
  ENDIF.
ENDFORM.                    " SELECTION_SCREEN_PBO
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN_PAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection_screen_pai .
  DATA: ls_afko TYPE afko,
        ls_aufk TYPE aufk,
        ls_mlgn TYPE mlgn,
        ls_mara TYPE mara.

  IF gv_old_aufnr NE p_aufnr.
    gv_old_aufnr = p_aufnr.
    p_copies = 3.
    p_nrpal  = 1.
    CLEAR: "p_fevor, "16/09/24
           p_matnr,
           p_ean11,
           p_gmein,
*           p_meinh,
*           p_hoehe,
*           p_meabm,
           p_menge,
           p_vhilm,
           p_print.
  ENDIF.

** Dados de Centro e Depósito
***********************************************************************
  IF NOT p_werks IS INITIAL.
    SELECT SINGLE name1 FROM t001w
                        INTO sc_werks
                        WHERE werks = p_werks.
  ELSE.
    CLEAR sc_werks.
  ENDIF.

  IF NOT p_lgnum IS INITIAL.
    SELECT SINGLE lnumt FROM t300t
                        INTO sc_lgnum
                        WHERE spras = sy-langu AND
                              lgnum = p_lgnum.
  ELSE.
    CLEAR sc_lgnum.
  ENDIF.

** Dados Ordem
***********************************************************************
  DATA lt_orders TYPE  ztporders.
  DO 1 TIMES.
    CHECK NOT p_fevor IS INITIAL.
    CALL FUNCTION 'ZWS_GET_PROD_ORDER'
      EXPORTING
        linha  = p_fevor
*       DATA   =
      IMPORTING
        orders = lt_orders.

  ENDDO.

  DO 1 TIMES.
    CHECK NOT p_aufnr IS INITIAL.
    CHECK NOT p_fevor IS INITIAL.

    READ TABLE lt_orders TRANSPORTING NO FIELDS
      WITH KEY aufnr = p_aufnr
               linha = p_fevor.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE s000 WITH |Ordem não disponível na linha { p_fevor }|
        DISPLAY LIKE 'E'.
      STOP.
    ELSE.
      SELECT SINGLE * FROM aufk
                      INTO ls_aufk
                      WHERE aufnr = p_aufnr AND
                            werks = p_werks.
      CHECK sy-subrc EQ 0.

      SELECT SINGLE * FROM afko
                      INTO ls_afko
                      WHERE aufnr = p_aufnr.
      CHECK sy-subrc EQ 0.

      "p_fevor = ls_afko-fevor. "16/09/24
      p_matnr = ls_afko-plnbez.
      p_gmein = ls_afko-gmein.
    ENDIF.
  ENDDO.

  IF sy-subrc <> 0.
    CLEAR: p_aufnr,
           "p_fevor, 16/09/24
           p_matnr, p_gmein,
           p_ean11,p_menge,p_vhilm.
  ENDIF.

*  IF p_matnr IS NOT INITIAL.
*    SELECT SINGLE meinh hoehe meabm
*      FROM marm
*      INTO (p_meinh, p_hoehe, p_meabm)
*      WHERE matnr EQ p_matnr
*        AND meinh EQ 'PAL'.
*  ENDIF.

** EAN
***********************************************************************
  DO 1 TIMES.
    CHECK NOT p_matnr IS INITIAL AND
          NOT p_gmein IS INITIAL.

    SELECT SINGLE ean11 FROM mean
                        INTO p_ean11
                        WHERE matnr = p_matnr AND
                              meinh = p_gmein AND
                              lfnum IS NOT NULL.
  ENDDO.

** Quantidade
***********************************************************************
  DO 1 TIMES.
    CHECK p_menge IS INITIAL.

    CHECK NOT p_matnr IS INITIAL AND
          NOT p_lgnum IS INITIAL.

    SELECT SINGLE * FROM mlgn
                    INTO ls_mlgn
                    WHERE matnr = p_matnr AND
                          lgnum = p_lgnum.
    CHECK sy-subrc EQ 0.

    p_menge = ls_mlgn-lhmg1.
  ENDDO.

** Material Palete
***********************************************************************
  DO 1 TIMES.
    CHECK NOT p_vhilm IS INITIAL.

    SELECT SINGLE * FROM mara
                    INTO ls_mara
                    WHERE matnr = p_vhilm.
    IF sy-subrc <> 0.

      CLEAR: p_vhilm.
      CONTINUE.
    ENDIF.

    IF ls_mara-mtart <> 'PALT'.
      CLEAR: p_vhilm.
      CONTINUE.
    ENDIF.
  ENDDO.

  IF NOT p_matnr IS INITIAL.
    SELECT SINGLE maktx FROM makt
                        INTO sc_matnr
                        WHERE matnr = p_matnr AND
                              spras = sy-langu.
  ELSE.
    CLEAR: sc_matnr.
  ENDIF.

  IF NOT p_vhilm IS INITIAL.
    SELECT SINGLE maktx FROM makt
                        INTO sc_vhilm
                        WHERE matnr = p_vhilm AND
                              spras = sy-langu.
  ELSE.
    CLEAR sc_vhilm.
  ENDIF.
ENDFORM.                    " SELECTION_SCREEN_PAI
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
  p_werks = 'RENV'. "'RFRA'.
  SET PARAMETER ID 'WRK' FIELD p_werks.

  p_lgnum = '100'. "'150'.
  PERFORM selection_screen_pai.

*  meinhtx = 'Unidad.med.alt.'.
*  hoehetx = 'Altura'.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_of_selection.
  DATA: lv_answer.
*        lv_ean128_bc1 TYPE zwm_aux-out_cb_tx,
*        lv_ean128_bc2 TYPE zwm_aux-out_cb_tx,
*        lv_ean128_bc3 TYPE zwm_aux-out_cb_tx,
*        lv_ean128_bc4 TYPE zwm_aux-out_cb_tx,
*        lv_ean128_tx1 TYPE zwm_aux-out_cb_tx,
*        lv_ean128_tx2 TYPE zwm_aux-out_cb_tx,
*        lv_ean128_tx3 TYPE zwm_aux-out_cb_tx,
*        lv_ean128_tx4 TYPE zwm_aux-out_cb_tx,
*        lv_return     TYPE zwm_aux-retorno,
*        lv_return_msg TYPE bapi_msg,
*        lv_outstring  TYPE zwm_aux-out_string,
  DATA: lv_exidv1 TYPE exidv,
        lv_exidv2 TYPE exidv.

  DATA lv_string TYPE zwm_aux-in_string.
  DATA:
    lv_outstring    LIKE  zwm_aux-out_string,
    lv_out_producao LIKE  zwm_aux-out_producao,
    lv_retorno      LIKE  zwm_aux-retorno,
    lv_altura       LIKE  zwm_log_efacec-altura,
    lv_cor          LIKE  zwm_log_efacec-cor,
    lv_ean11        LIKE  mean-ean11,
    lv_ean128_cb1   LIKE  zwm_aux-out_cb_tx,
    lv_ean128_cb2   LIKE  zwm_aux-out_cb_tx,
    lv_ean128_cb3   LIKE  zwm_aux-out_cb_tx,
    lv_ean128_cb4   LIKE  zwm_aux-out_cb_tx,
    lv_ean128_tx1   LIKE  zwm_aux-out_cb_tx,
    lv_ean128_tx2   LIKE  zwm_aux-out_cb_tx,
    lv_ean128_tx3   LIKE  zwm_aux-out_cb_tx,
    lv_ean128_tx4   LIKE  zwm_aux-out_cb_tx,
    lv_lay_etiqueta LIKE  zwm_aux-out_cb_tx,
    lv_e_sscc       LIKE  vekp-exidv,
    lv_e_sscc2      LIKE  vekp-exidv,
    lt_et_messages  TYPE  tab_bdcmsgcoll.
  DATA: BEGIN OF ls_string,
          codigo       TYPE c LENGTH 18,   " *  MOVE in_string(18)    TO gt_registo-codigo.
          linha        TYPE c LENGTH 03,   " *  MOVE in_string+18(3)  TO gt_registo-linha.
          altura       TYPE c LENGTH 04,   " *  MOVE in_string+21(4)  TO gt_registo-altura.
          cor          TYPE c LENGTH 06,   " *  MOVE in_string+25(6)  TO gt_registo-cor.
          quantidade   TYPE c LENGTH 04,   " *  MOVE in_string+31(4)  TO gt_registo-quantidade.
          aufnr        TYPE c LENGTH 12,   " *  MOVE in_string+35(12) TO gt_registo-aufnr.
          pal_completa TYPE c LENGTH 01,   " *  MOVE in_string+47(1)  TO gt_registo-pal_completa.
        END OF ls_string.

  DATA: lr_exidv TYPE RANGE OF exidv.

  DATA: lr_s_exidv LIKE LINE OF lr_exidv.

  DATA: ls_alv_display_0001 TYPE gty_alv_display_0001.

  IF p_ean11 IS INITIAL OR
     p_fevor IS INITIAL OR
     p_vhilm IS INITIAL OR
     p_aufnr IS INITIAL OR
     p_menge IS INITIAL OR
     p_nrpal IS INITIAL.
    MESSAGE s065 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF p_nrpal GT 10.
    MESSAGE s000 WITH 'Indicar valor para Nº Paletes entre 1 e 10' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = 'Lançar entrada de Mercadoria?'(003)
      text_button_1         = 'Sim'(001)
      text_button_2         = 'Não'(002)
      display_cancel_button = abap_false
    IMPORTING
      answer                = lv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK lv_answer EQ '1'.

  DO p_nrpal TIMES.

    ls_string-codigo       = p_ean11.
    ls_string-linha        = p_fevor.
*    ls_string-altura       = |{ p_hoehe NUMBER = RAW DECIMALS = 0 }|.
    ls_string-cor          = p_vhilm.
    ls_string-quantidade   = |{ p_menge NUMBER = RAW DECIMALS = 0 }|.
    ls_string-aufnr        = p_aufnr.
    ls_string-pal_completa = ''.

    lv_string = ls_string.

    CLEAR: lv_outstring,
           lv_out_producao,
           lv_retorno,
           lv_altura,
           lv_cor,
           lv_ean11,
           lv_ean128_cb1,
           lv_ean128_cb2,
           lv_ean128_cb3,
           lv_ean128_cb4,
           lv_ean128_tx1,
           lv_ean128_tx2,
           lv_ean128_tx3,
           lv_ean128_tx4,
           lv_lay_etiqueta,
           lv_e_sscc,
           lv_e_sscc2,
           lt_et_messages.

    CALL FUNCTION 'ZWM_RFC_ENTRADA_PRODUCAO'
      EXPORTING
        in_string     = lv_string
*       SIMULACAO     =
*       POSTO         =
*       NAO_IMPRIME   =
*       SSCC          =
*       SSCC2         =
      IMPORTING
        out_string    = lv_outstring
        out_producao  = lv_out_producao
        retorno       = lv_retorno
        altura        = lv_altura
        cor           = lv_cor
        ean11         = lv_ean11
        ean128_cb1    = lv_ean128_cb1
        ean128_cb2    = lv_ean128_cb2
        ean128_cb3    = lv_ean128_cb3
        ean128_cb4    = lv_ean128_cb4
        ean128_tx1    = lv_ean128_tx1
        ean128_tx2    = lv_ean128_tx2
        ean128_tx3    = lv_ean128_tx3
        ean128_tx4    = lv_ean128_tx4
        lay_etiqueta  = lv_lay_etiqueta
        e_sscc        = lv_e_sscc
        e_sscc2       = lv_e_sscc2
        et_messages   = lt_et_messages
      EXCEPTIONS
        exception     = 1
        error_message = 99
        OTHERS        = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      EXIT.
    ELSE.
      LOOP AT lt_et_messages INTO DATA(ls_msg)
        WHERE msgtyp CA 'EAX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc IS INITIAL.
        EXIT.
      ENDIF.
    ENDIF.

** Impressão
***********************************************************************
    DO 1 TIMES.
      CHECK NOT p_print IS INITIAL AND
            NOT p_copies IS INITIAL.

      lv_exidv1 = lv_outstring(20).
      lv_exidv2 = lv_outstring+20(20).

      IF NOT lv_exidv1 IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_exidv1
          IMPORTING
            output = lv_exidv1.
        lr_s_exidv-low = lv_exidv1.
        lr_s_exidv-option = 'EQ'.
        lr_s_exidv-sign = 'I'.
        APPEND lr_s_exidv TO lr_exidv.
      ENDIF.

      IF NOT lv_exidv2 IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_exidv2
          IMPORTING
            output = lv_exidv2.
        lr_s_exidv-low = lv_exidv2.
        lr_s_exidv-option = 'EQ'.
        lr_s_exidv-sign = 'I'.
        APPEND lr_s_exidv TO lr_exidv.
      ENDIF.

      CHECK NOT lr_exidv IS INITIAL.

      SUBMIT zwmrep0041 WITH p_print  = p_print
                        WITH p_copies = p_copies
                        WITH s_sscc IN lr_exidv
                        AND RETURN.
    ENDDO.

** ALV
***********************************************************************
    ls_alv_display_0001-message = '>>>>>>> BC >>>>>>'.
    APPEND ls_alv_display_0001 TO gt_alv_display_0001.
    ls_alv_display_0001-message = lv_ean128_cb1.
    APPEND ls_alv_display_0001 TO gt_alv_display_0001.
    ls_alv_display_0001-message = lv_ean128_cb2.
    APPEND ls_alv_display_0001 TO gt_alv_display_0001.
    ls_alv_display_0001-message = lv_ean128_cb3.
    APPEND ls_alv_display_0001 TO gt_alv_display_0001.
    ls_alv_display_0001-message = lv_ean128_cb4.
    APPEND ls_alv_display_0001 TO gt_alv_display_0001.

    ls_alv_display_0001-message = '>>>>>>> BC TX >>>>>>'.
    APPEND ls_alv_display_0001 TO gt_alv_display_0001.
    ls_alv_display_0001-message = lv_ean128_tx1.
    APPEND ls_alv_display_0001 TO gt_alv_display_0001.
    ls_alv_display_0001-message = lv_ean128_tx2.
    APPEND ls_alv_display_0001 TO gt_alv_display_0001.
    ls_alv_display_0001-message = lv_ean128_tx3.
    APPEND ls_alv_display_0001 TO gt_alv_display_0001.
    ls_alv_display_0001-message = lv_ean128_tx4.
    APPEND ls_alv_display_0001 TO gt_alv_display_0001.

    ls_alv_display_0001-message = '>>>>>>> Mensagens >>>>>>'.
    APPEND ls_alv_display_0001 TO gt_alv_display_0001.
    ls_alv_display_0001-message = lv_retorno.
    APPEND ls_alv_display_0001 TO gt_alv_display_0001.

    LOOP AT lt_et_messages INTO ls_msg. " DATA(ls_msg).
      TRY.
          MESSAGE ID ls_msg-msgid TYPE ls_msg-msgtyp NUMBER ls_msg-msgnr
            WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4
              INTO DATA(lv_return_msg).
          ls_alv_display_0001-message = lv_return_msg.
          APPEND ls_alv_display_0001 TO gt_alv_display_0001.
        CATCH cx_root.
      ENDTRY.
    ENDLOOP.
  ENDDO.

  PERFORM call_screen_0001.
ENDFORM.                    " START_OF_SELECTION


*&---------------------------------------------------------------------*
*&      Form  call_screen_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_screen_0001.
  PERFORM display_alv_0001.
  PERFORM call_screen USING '0001'.
ENDFORM.                    " CALL_SCREEN_0001

*&---------------------------------------------------------------------*
*&      Form  call_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UV_DYNNR   text
*----------------------------------------------------------------------*
FORM call_screen USING uv_dynnr TYPE sydynnr.

  CHECK sy-dynnr <> uv_dynnr.
  CALL SCREEN uv_dynnr.
ENDFORM.                    " CALL_SCREEN

*&---------------------------------------------------------------------*
*&      Form  display_alv_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_alv_0001 .
  IF gref_alv_container_0001 IS BOUND.
    gref_alv_display_0001->refresh( ).
    EXIT.
  ENDIF.

** Container
***********************************************************************
  IF cl_salv_table=>is_offline( ) EQ if_salv_c_bool_sap=>false.
    CREATE OBJECT gref_alv_container_0001
      EXPORTING
        container_name = 'SCR0001-CONTAINER01'.
  ENDIF.

** Cria Output
***********************************************************************
  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          r_container    = gref_alv_container_0001
          container_name = 'SCR0001-CONTAINER01'
        IMPORTING
          r_salv_table   = gref_alv_display_0001
        CHANGING
          t_table        = gt_alv_display_0001.

    CATCH cx_salv_msg.
      EXIT.
  ENDTRY.

** Configuração
***********************************************************************
  PERFORM set_output_config_0001.

** Output
***********************************************************************
  gref_alv_display_0001->display( ).

ENDFORM.                    " DISPLAY_ALV_0001

*&---------------------------------------------------------------------*
*&      Form  reset_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM reset_0001 .
  CLEAR: scr0001.
  CLEAR: gt_alv_display_0001.
ENDFORM.                    " RESET_0001

*&---------------------------------------------------------------------*
*&      Form  set_output_config_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_output_config_0001 .
  DATA: lref_functions  TYPE REF TO cl_salv_functions,
        lref_columns    TYPE REF TO cl_salv_columns_table,
        lref_column     TYPE REF TO cl_salv_column_table,
        lref_layout     TYPE REF TO cl_salv_layout,
        lref_selections TYPE REF TO cl_salv_selections,
        lref_display    TYPE REF TO cl_salv_display_settings.

  DATA: ls_key TYPE salv_s_layout_key.

  DATA: lv_stext TYPE scrtext_s,
        lv_mtext TYPE scrtext_m,
        lv_ltext TYPE scrtext_l.

  lref_columns = gref_alv_display_0001->get_columns( ).
  lref_columns->set_optimize( abap_true ).

** Zebra
***********************************************************************
  lref_display = gref_alv_display_0001->get_display_settings( ). "General Display Settings  "get them all first
  lref_display->set_striped_pattern( cl_salv_display_settings=>true ). "zebra stripes

** Selecções
***********************************************************************
  lref_selections = gref_alv_display_0001->get_selections( ).
  lref_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

** Cell Type
***********************************************************************
*  lref_columns->set_cell_type_column( 'T_CELLTYPE' ).

** Campos Tecnicos
***********************************************************************
**  PERFORM set_technical USING: lref_columns 'WERKS',
**                               lref_columns 'LGORT',
**                               lref_columns 'DATUM',
**                               lref_columns 'UZEIT',
**                               lref_columns 'TYPE',
**                               lref_columns 'CWM'.

  TRY.
**      lref_column ?= lref_columns->get_column( 'WARN' ).
**      MOVE text-001 TO lv_stext.
**      MOVE text-001 TO lv_mtext.
**      MOVE text-001 TO lv_ltext.
**      lref_column->set_short_text( lv_stext ).
**      lref_column->set_medium_text( lv_mtext ).
**      lref_column->set_long_text( lv_ltext ).
**      lref_column->set_alignment( if_salv_c_alignment=>centered ).
    CATCH: cx_salv_not_found, cx_salv_data_error.
  ENDTRY.

ENDFORM.                    " SET_OUTPUT_CONFIG_0001

*&---------------------------------------------------------------------*
*&      Form  set_technical
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UREF_COLUMNS  text
*      -->UV_FNAME      text
*----------------------------------------------------------------------*
FORM set_technical USING uref_columns TYPE REF TO cl_salv_columns_table
                         uv_fname     TYPE lvc_fname.

  DATA: lref_column TYPE REF TO cl_salv_column_table.

  TRY.
      lref_column ?= uref_columns->get_column( uv_fname ).
      lref_column->set_technical( abap_true ).
    CATCH: cx_salv_not_found, cx_salv_data_error.
  ENDTRY.
ENDFORM.                    " SET_TECHNICAL

*&---------------------------------------------------------------------*
*&      Form  exit_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM exit_0001.
*  LEAVE TO SCREEN 0.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.                    " EXIT_0001

*&---------------------------------------------------------------------*
*&      Form  status_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM status_0001.
  SET PF-STATUS 'Z001'.
  SET TITLEBAR 'Z001'.
ENDFORM.                    " STATUS_0001
*&---------------------------------------------------------------------*
*&      Form  F4_VHILM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_VHILM  text
*----------------------------------------------------------------------*
FORM f4_vhilm CHANGING cv_vhilm TYPE vhilm.
  DATA: lt_return_tab	TYPE TABLE OF ddshretval.

  DATA: ls_return_tab	TYPE ddshretval.

  DATA: lv_mtart TYPE mtart.

  lv_mtart = 'PALT'.

  SET PARAMETER ID 'MTA' FIELD lv_mtart.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'MARA'
      fieldname  = 'MATNR'
      searchhelp = 'Z_MATPAL_SE'
      shlpparam  = 'MATNR'
    TABLES
      return_tab = lt_return_tab
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CLEAR: lv_mtart.
  SET PARAMETER ID 'MTA' FIELD lv_mtart.

  CLEAR: ls_return_tab.
  READ TABLE lt_return_tab
        INTO ls_return_tab
        INDEX 1.

  CHECK sy-subrc EQ 0.

  cv_vhilm = ls_return_tab-fieldval.
ENDFORM.                    " F4_VHILM


*&---------------------------------------------------------------------*
*&      Form  F4_AUFNR
*&---------------------------------------------------------------------*
FORM f4_aufnr CHANGING cv_aufnr TYPE aufk-aufnr.

  DATA: lt_return_tab TYPE hrreturn_tab. "ddshretval.

  IF p_fevor IS INITIAL.
    MESSAGE s000 WITH 'Informar Linha' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DATA lt_ztporders TYPE ztporders.
  CALL FUNCTION 'ZWS_GET_PROD_ORDER'
    EXPORTING
      linha  = p_fevor "lv_linha "'H09'
*     DATA   =
    IMPORTING
      orders = lt_ztporders.

  DATA lt_field_tab TYPE dfies_tab.
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = 'ZPORDERS'
    TABLES
      dfies_tab      = lt_field_tab
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = ' '
      retfield        = 'AUFNR'
*     PVALKEY         = ' '
*     DYNPPROG        = ' '
*     DYNPNR          = ' '
*     DYNPROFIELD     = ' '
*     STEPL           = 0
*     WINDOW_TITLE    =
*     VALUE           = ' '
      value_org       = 'S'
*     MULTIPLE_CHOICE = ' '
*     DISPLAY         = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM   = ' '
*     CALLBACK_METHOD =
*     MARK_TAB        =
* IMPORTING
*     USER_RESET      =
    TABLES
      value_tab       = lt_ztporders
      field_tab       = lt_field_tab
      return_tab      = lt_return_tab
*     DYNPFLD_MAPPING =
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  READ TABLE lt_return_tab INTO DATA(ls_return_tab) INDEX 1.

  cv_aufnr = ls_return_tab-fieldval.
ENDFORM.                    " F4_VHILM
*&---------------------------------------------------------------------*
*&      Form  F4_FEVOR
*&---------------------------------------------------------------------*
FORM f4_fevor CHANGING cv_fevor TYPE afko-fevor.

  DATA: lt_return_tab TYPE hrreturn_tab. "ddshretval.

*  DATA lt_ztporders TYPE ztporders.
*  CALL FUNCTION 'ZWS_GET_PROD_ORDER'
*    EXPORTING
*      linha  = p_fevor "lv_linha "'H09'
**     DATA   =
*    IMPORTING
*      orders = lt_ztporders.

  DATA lt_field_tab TYPE dfies_tab.
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = 'ZWM_S007' "'Z02RPLINHAS'
    TABLES
      dfies_tab      = lt_field_tab
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  DATA lr_fields_exc TYPE RANGE OF fieldname.
  lr_fields_exc = VALUE #( sign = 'I' option = 'EQ'
   ( low = 'MANDT' )
   ( low = 'WERKS' )
*   ( low = 'ACTIVO' )
   ).
  DELETE lt_field_tab WHERE fieldname IN lr_fields_exc.
*  DATA lv_pos TYPE i VALUE 1.
*  LOOP AT lt_field_tab ASSIGNING FIELD-SYMBOL(<lfs_field>).
**    <lfs_field>-position = lv_pos.
**    ADD 1 TO lv_pos.
*    <lfs_field>-tabname = 'LT_LINHAS'.
*  ENDLOOP.

  SELECT "*
      cod_linha,
      fevor,
      descricao,
      activo
    FROM z02rplinhas
    INTO TABLE @DATA(lt_linhas)
    WHERE werks EQ @p_werks.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE  = 'Z02RPLINHAS'
      retfield        = 'FEVOR'
*     PVALKEY         = ' '
*     DYNPPROG        = sy-repid
*     DYNPNR          = ' '
*     DYNPROFIELD     = ' '
*     STEPL           = 0
*     WINDOW_TITLE    =
*     VALUE           = ' '
      value_org       = 'S'
*     MULTIPLE_CHOICE = ' '
*     DISPLAY         = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM   = ' '
*     CALLBACK_METHOD =
*     MARK_TAB        =
* IMPORTING
*     USER_RESET      =
    TABLES
      value_tab       = lt_linhas
      field_tab       = lt_field_tab
      return_tab      = lt_return_tab
*     DYNPFLD_MAPPING =
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  READ TABLE lt_return_tab INTO DATA(ls_return_tab) INDEX 1.

  cv_fevor = ls_return_tab-fieldval.

ENDFORM.                    " F4_FEVOR
