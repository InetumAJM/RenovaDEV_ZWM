*&---------------------------------------------------------------------*
*&  Include           ZWMREP0125_F01
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
** Retorna Dados
************************************************************************
  PERFORM refresh_0001.
  CHECK NOT gt_alv_display IS INITIAL.
ENDFORM.                    " CALL_SCREEN_0001
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: lt_lqua        TYPE TABLE OF lqua,
        lt_mlgn        TYPE HASHED TABLE OF mlgn WITH UNIQUE KEY matnr,
        lt_makt        TYPE HASHED TABLE OF makt WITH UNIQUE KEY matnr,
        lt_vekp        TYPE TABLE OF vekp,
        lt_zwm001      TYPE TABLE OF zwm001.

  DATA: ls_lqua        TYPE lqua,
        ls_mlgn        TYPE mlgn,
        ls_makt        TYPE makt,
        ls_vekp        TYPE vekp,
        ls_zwm001      TYPE zwm001,
        ls_alv_display TYPE gty_alv_display.

** Retorna Todas as SU's
***********************************************************************
  SELECT * FROM lqua
     INTO TABLE lt_lqua
     WHERE lgnum = p_lgnum AND
           lgtyp IN s_lgtyp AND
           lgpla IN s_lgpla AND
           lenum IN s_lenum AND
           matnr IN s_matnr.

  DELETE lt_lqua WHERE lenum IS INITIAL OR
                       gesme <= 0.

  CHECK NOT lt_lqua IS INITIAL.

** Retorna Todas as Paletes
***********************************************************************
  DO 1 TIMES.
    CHECK NOT lt_lqua IS INITIAL.

*--> Dados de Material
    SELECT * FROM mlgn
       INTO TABLE lt_mlgn
       FOR ALL ENTRIES IN lt_lqua
       WHERE matnr = lt_lqua-matnr AND
            lgnum  = lt_lqua-lgnum.

    CHECK NOT lt_mlgn IS INITIAL.

*--> HU
    SELECT * FROM vekp
       INTO TABLE lt_vekp
       FOR ALL ENTRIES IN lt_lqua
       WHERE exidv = lt_lqua-lenum.

    CHECK NOT lt_vekp IS INITIAL.

    SORT lt_vekp BY exidv.
    DELETE ADJACENT DUPLICATES FROM lt_vekp COMPARING exidv.

*--> Parametros
    SELECT * FROM zwm001
       INTO TABLE lt_zwm001
       WHERE armazem = p_lgnum AND
             processo IN ('MEIA-PALETE', 'QUARTO-PALETE').

    SORT lt_zwm001 BY processo parametro.
  ENDDO.

** Descrição
***********************************************************************
  DO 1 TIMES.
    CHECK NOT lt_lqua IS INITIAL.

    SELECT * FROM makt
       INTO TABLE lt_makt
       FOR ALL ENTRIES IN lt_lqua
       WHERE matnr = lt_lqua-matnr AND
             spras = sy-langu.

  ENDDO.

** Agrupa Informação
***********************************************************************
  LOOP AT lt_lqua INTO ls_lqua.
    CLEAR ls_alv_display.

    CLEAR ls_vekp.
    READ TABLE lt_vekp
          INTO ls_vekp
          WITH KEY exidv = ls_lqua-lenum
          BINARY SEARCH.

    CHECK sy-subrc EQ 0.

    CLEAR ls_mlgn.
    READ TABLE lt_mlgn
          INTO ls_mlgn
          WITH TABLE KEY matnr = ls_lqua-matnr.

    CHECK sy-subrc EQ 0.

    CLEAR ls_makt.
    READ TABLE lt_makt
          INTO ls_makt
          WITH TABLE KEY matnr = ls_lqua-matnr.

    CHECK sy-subrc EQ 0.


*--> Dados gerais
    ls_alv_display-lgtyp = ls_lqua-lgtyp.
    ls_alv_display-lgpla = ls_lqua-lgpla.
    ls_alv_display-exidv = ls_vekp-exidv.
    ls_alv_display-matnr = ls_lqua-matnr.
    ls_alv_display-maktx = ls_makt-maktx.
    ls_alv_display-vhilm = ls_vekp-vhilm.
    ls_alv_display-quant = 1.

*--> Tipo de Palete
    CASE ls_mlgn-block.
      WHEN '03' OR '04' OR '05' OR '06'.

        CLEAR ls_zwm001.
        READ TABLE lt_zwm001
              INTO ls_zwm001
              WITH KEY processo  = 'MEIA-PALETE'
                       parametro = ls_alv_display-vhilm.

        ls_alv_display-vhilm_m = ls_zwm001-valor.
        ls_alv_display-quant_m = 2.
      WHEN '07' OR '08' OR '09' OR '10'.

        CLEAR ls_zwm001.
        READ TABLE lt_zwm001
              INTO ls_zwm001
              WITH KEY processo  = 'QUARTO-PALETE'
                       parametro = ls_alv_display-vhilm.

        ls_alv_display-vhilm_q = ls_zwm001-valor.
        ls_alv_display-quant_q = 4.
    ENDCASE.

    IF NOT ls_alv_display-vhilm   IN s_vhilm AND
       NOT ls_alv_display-vhilm_m IN s_vhilm AND
       NOT ls_alv_display-vhilm_q IN s_vhilm.
      CONTINUE.
    ENDIF.

    APPEND ls_alv_display TO gt_alv_display .
  ENDLOOP.

  SORT gt_alv_display BY lgtyp lgpla exidv.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  REFRESH_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_0001.

** Retorna Dados
***********************************************************************
  PERFORM get_data.

  IF gt_alv_display IS INITIAL.
**  Sem dados para os parametros informados
    MESSAGE s010 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

** Display
***********************************************************************
  PERFORM display_alv.

ENDFORM.                    " REFRESH_0001
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
** Cria Output
***********************************************************************
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = gref_alv_display
        CHANGING
          t_table      = gt_alv_display.

    CATCH cx_salv_msg.
      EXIT.
  ENDTRY.

** Configuração
***********************************************************************
  PERFORM set_output_config.

** Eventos
***********************************************************************
  PERFORM register_events.

** Output
***********************************************************************
  gref_alv_display->display( ).
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT_CONFIG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output_config .
  DATA: lref_functions TYPE REF TO cl_salv_functions,
        lref_columns   TYPE REF TO cl_salv_columns_table,
        lref_column    TYPE REF TO cl_salv_column_table,
        lref_layout    TYPE REF TO cl_salv_layout.

  DATA: ls_key TYPE salv_s_layout_key.

  DATA: lv_stext TYPE scrtext_s,
        lv_mtext TYPE scrtext_m,
        lv_ltext TYPE scrtext_l.

***********************************************************************
  CHECK gref_alv_display IS BOUND.

** Layout (Variantes)
***********************************************************************
  lref_layout = gref_alv_display->get_layout( ).

  ls_key-report = sy-repid.
  lref_layout->set_key( ls_key ).
  lref_layout->set_default( abap_true ). " Permitir Gravação
  lref_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

  IF NOT p_layout IS INITIAL.
    lref_layout->set_initial_layout( p_layout ).
  ENDIF.

** Funções
***********************************************************************
  lref_functions = gref_alv_display->get_functions( ).
  lref_functions->set_all( if_salv_c_bool_sap=>true ).

** Colunas
***********************************************************************
  lref_columns = gref_alv_display->get_columns( ).
  lref_columns->set_optimize( abap_true ).

** Colunas Tecnicas
***********************************************************************
*  PERFORM set_technical USING: lref_columns 'MANDT',
*                               lref_columns 'ORDEM',
*                               lref_columns 'ERZET'.

  TRY.
      lref_column ?= lref_columns->get_column( 'LGPLA' ).
      lref_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH: cx_salv_not_found, cx_salv_data_error.
  ENDTRY.


*
*  TRY.
*      lref_column ?= lref_columns->get_column( 'N_SU_ORIG' ).
*      lref_column->set_output_length( 8 ).
*      lref_column->set_alignment( 2 ).
*      lref_column->set_zero( abap_true ).
*      MOVE text-001 TO lv_stext.
*      MOVE text-001 TO lv_mtext.
*      MOVE text-001 TO lv_ltext.
*      lref_column->set_short_text( lv_stext ).
*      lref_column->set_medium_text( lv_mtext ).
*      lref_column->set_long_text( lv_ltext ).
*    CATCH: cx_salv_not_found, cx_salv_data_error.
*  ENDTRY.
ENDFORM.                    " SET_OUTPUT_CONFIG
*&---------------------------------------------------------------------*
*&      Form  SET_TECHNICAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LREF_COLUMNS  text
*      -->P_0139   text
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
*&      Form  REGISTER_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_events .
*  DATA: lr_events TYPE REF TO cl_salv_events_table.
*
*  CREATE OBJECT gref_events.
*
*  lr_events = gref_alv_display->get_event( ).
*  SET HANDLER gref_events->on_link_click FOR lr_events.
ENDFORM.                    " REGISTER_EVENTS
*&---------------------------------------------------------------------*
*&      Form  ON_LINK_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_INDEX  text
*      -->P_COLUMN  text
*----------------------------------------------------------------------*
FORM on_link_click USING uv_index  TYPE i
                         uv_column TYPE salv_de_column.

  DATA: ls_alv_display TYPE gty_alv_display.

  READ TABLE gt_alv_display
        INTO ls_alv_display
        INDEX uv_index.


  CASE uv_column.
    WHEN 'LGPLA'.
      PERFORM call_lx02 USING ls_alv_display.
  ENDCASE.

ENDFORM.                    " ON_LINK_CLICK
*&---------------------------------------------------------------------*
*&      Form  F4_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_LAYOUT  text
*----------------------------------------------------------------------*
FORM f4_layout CHANGING pc_layout TYPE slis_vari.

  DATA: ls_layout TYPE salv_s_layout_info,
        ls_key    TYPE salv_s_layout_key.

***********************************************************************
  ls_key-report = sy-repid.

  ls_layout = cl_salv_layout_service=>f4_layouts(
                         s_key    = ls_key
                         restrict = if_salv_c_layout=>restrict_none ).

  pc_layout = ls_layout-layout.
ENDFORM.                                                    " F4_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  CALL_LX02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ALV_DISPLAY  text
*----------------------------------------------------------------------*
FORM call_lx02 USING us_alv_display TYPE gty_alv_display.

  DATA: lr_lgtyp TYPE RANGE OF lgtyp,
        lr_lgpla TYPE RANGE OF lgpla.

  DATA: lr_s_lgtyp LIKE LINE OF lr_lgtyp,
        lr_s_lgpla LIKE LINE OF lr_lgpla.

  lr_s_lgtyp-sign = 'I'.
  lr_s_lgtyp-option = 'EQ'.
  lr_s_lgtyp-low = us_alv_display-lgtyp.
  APPEND lr_s_lgtyp TO lr_lgtyp.

  lr_s_lgpla-sign = 'I'.
  lr_s_lgpla-option = 'EQ'.
  lr_s_lgpla-low = us_alv_display-lgpla.
  APPEND lr_s_lgpla TO lr_lgpla.

  SUBMIT rls10030 WITH s1_lgnum EQ p_lgnum
                  WITH s1_lgtyp IN lr_lgtyp
                  WITH s1_lgpla IN lr_lgpla
                  WITH pmitb EQ abap_true
                  AND RETURN.
ENDFORM.                                                    " CALL_LX02
