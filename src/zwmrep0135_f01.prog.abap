*&---------------------------------------------------------------------*
*&  Include           ZWMREP0135_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_of_selection.
  PERFORM get_dados.


** Prepara Listagem
  PERFORM ajusta_propriedades.

** Colunas Listagem
  PERFORM catalogo.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat              = fieldcat_tab
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      it_events                = eventos
      is_layout                = layout
      i_grid_title             = grid_title
      i_callback_pf_status_set = 'PFSTATUS'
      is_variant               = is_variant
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_program       = programa
      i_save                   = 'X'
      i_background_id          = 'ALV_BACKGROUND'
    TABLES
      t_outtab                 = gt_itab
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  CHECK sy-ucomm = '&F03'.

ENDFORM.                    " START_OF_SELECTION
*&---------------------------------------------------------------------*
*&      Form  GET_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dados.
  DATA: lt_mseg TYPE TABLE OF mseg,
        lt_mkpf TYPE TABLE OF mkpf.

  DATA: ls_mseg TYPE mseg,
        ls_mkpf TYPE mkpf,
        ls_itab TYPE gty_itab.

  DATA: lv_bwart_in   TYPE bwart,
        lv_bwart_out  TYPE bwart,
        lv_lgort_f_in TYPE lgort_d.

  FIELD-SYMBOLS: <ls_itab> TYPE gty_itab.

  CLEAR: gt_itab.


  SELECT SINGLE lgnum lgort FROM t320
                            INTO (gv_lgnum,gv_lgort)
                            WHERE werks = p_werks.

** Parametros
**********************************************************************
  DO 1 TIMES.
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'MOV_CODE_IN'
      IMPORTING
        e_valor     = lv_bwart_in
*        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = gv_lgnum
        i_processo  = 'CONSOLIDATION'
        i_parametro = 'STG_LOC_FINAL'
      IMPORTING
        e_valor     = lv_lgort_f_in
*        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
    CHECK sy-subrc EQ 0.
  ENDDO.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.


  SELECT SINGLE bwart_next FROM t156n
                           INTO lv_bwart_out
                           WHERE fcode = 'SS' AND
                                 bwart = lv_bwart_in.


*  SD - 03/12/2018 - retirado por excesso de tempo na execução
*
*  SELECT * FROM mkpf
*           INTO TABLE lt_mkpf
*           WHERE budat IN s_data.
*  CHECK sy-subrc EQ 0.
*
*  SELECT * FROM mseg
*           INTO TABLE lt_mseg
*           FOR ALL ENTRIES IN lt_mkpf
*           WHERE mblnr = lt_mkpf-mblnr AND
*                 mjahr = lt_mkpf-mjahr.
*
*  DELETE lt_mseg WHERE NOT matnr IN s_matnr       OR
*                           werks <> p_werks       OR
*                           lgort <> lv_lgort_f_in OR
*                           ( bwart <> lv_bwart_in AND bwart <> lv_bwart_out ).

* nova leitura das tabelas
  select p~mjahr p~mblnr p~matnr p~meins p~bwart
         p~werks p~charg p~menge p~budat_mkpf

    into CORRESPONDING FIELDS OF TABLE lt_mseg
    from mkpf as k
    INNER JOIN mseg as p on p~mjahr = k~mjahr and
                            p~mblnr = k~mblnr
    where k~budat in s_data and
          p~matnr in s_matnr and
          p~werks = p_werks and
          p~lgort = lv_lgort_f_in and
          ( bwart = lv_bwart_in or bwart = lv_bwart_out ).

*
*  SD - fim

  LOOP AT lt_mseg INTO ls_mseg.
    CLEAR: ls_itab.

    MOVE-CORRESPONDING ls_mseg TO ls_itab.



    SELECT SINGLE maktx FROM makt
    INTO ls_itab-maktx
    WHERE matnr EQ ls_itab-matnr
      AND spras EQ sy-langu.

    SELECT * FROM mean
    WHERE matnr EQ ls_itab-matnr
      AND meinh EQ ls_itab-meins
      AND hpean EQ 'X'.
      ls_itab-ean11 = mean-ean11.
      EXIT.
    ENDSELECT.

    CASE ls_mseg-bwart.
      WHEN lv_bwart_in.
        ls_itab-menge = ls_mseg-menge.
      WHEN lv_bwart_out.
        ls_itab-menge = ls_mseg-menge * -1.
      WHEN OTHERS.
        ls_itab-menge = ls_mseg-menge.
    ENDCASE.

    UNASSIGN <ls_itab>.
    COLLECT ls_itab INTO gt_itab.
  ENDLOOP.

  DELETE gt_itab WHERE menge <= 0.

  SORT lt_mseg BY matnr ASCENDING charg ASCENDING  budat_mkpf DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_mseg COMPARING matnr charg.

  LOOP AT gt_itab ASSIGNING <ls_itab>.
    READ TABLE lt_mseg
          INTO ls_mseg
          WITH KEY matnr = <ls_itab>-matnr
                   charg = <ls_itab>-charg
          BINARY SEARCH.

    <ls_itab>-data = ls_mseg-budat_mkpf.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = <ls_itab>-matnr
      IMPORTING
        output = <ls_itab>-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = <ls_itab>-meins
        language       = sy-langu
      IMPORTING
        output         = <ls_itab>-meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

  ENDLOOP.

  SORT gt_itab BY matnr charg.
ENDFORM.                    " GET_DADOS


*&---------------------------------------------------------------------*
*&      Form  AJUSTA_PROPRIEDADES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ajusta_propriedades.

  layout-colwidth_optimize = p_cwidth.
  layout-window_titlebar = sy-title.
  layout-zebra = 'X'.
  layout-no_vline = p_vrtgln.
  layout-no_vline = p_vrtgln.
  layout-def_status = 'A'.

  programa = sy-repid.

** Eventos a capturar
  REFRESH eventos.
  wa_eventos-name = 'USER_COMMAND'.
  wa_eventos-form = 'HOTSPOT_ACTIVO'.
  APPEND wa_eventos TO eventos.

ENDFORM.                    " AJUSTA_PROPRIEDADES

*&---------------------------------------------------------------------*
*&      Form   CATALOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  catalogo.
  FREE fieldcat_tab.
  CLEAR fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'MATNR'.
  fieldcat_linha-reptext_ddic = 'MATERIAL'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'EAN11'.
  fieldcat_linha-reptext_ddic = 'EAN/DUN/ITF'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'MAKTX'.
  fieldcat_linha-reptext_ddic = 'DESCRIÇÃO'.
  fieldcat_linha-just = 'L'.
*  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'CHARG'.
  fieldcat_linha-reptext_ddic = 'LOTE'.
  fieldcat_linha-just = 'L'.
*  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'MENGE'.
  fieldcat_linha-reptext_ddic = 'QUANT.'.
  fieldcat_linha-just = 'R'.
  fieldcat_linha-datatype = 'QUAN'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'MEINS'.
  fieldcat_linha-reptext_ddic = 'UM'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'DATA'.
  fieldcat_linha-reptext_ddic = 'ULT.ENTRADA'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-datatype = 'DATS'.
  APPEND fieldcat_linha TO fieldcat_tab.
ENDFORM.                    "catalogo

************************************************************************
*   Form PFSTATUS
************************************************************************
FORM pfstatus USING lt_extab.

  SET PF-STATUS 'STANDARD'.

ENDFORM.                    "pfstatus

*** PROCESSO DE EVENTOS
*&---------------------------------------------------------------------*
*&      Form TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.

  STATICS: lt_list_commentary TYPE slis_t_listheader,
           ls_list_commentary TYPE slis_listheader.

  DATA: l_datum(10),
        l_linhas LIKE sy-tabix,
        l_uzeit(8).

  FREE: lt_list_commentary.
  CLEAR: lt_list_commentary.

  DESCRIBE TABLE gt_itab LINES l_linhas.

  CASE usr01-datfm.
    WHEN '1' OR '2' OR '3'.
      WRITE sy-datum TO l_datum USING EDIT MASK '__.__.____'.
    WHEN OTHERS.
      WRITE sy-datum TO l_datum USING EDIT MASK '____.__.__'.
  ENDCASE.

  WRITE sy-uzeit TO l_uzeit USING EDIT MASK '__:__:__'.

* Título
  ls_list_commentary-typ  = 'H'.
  ls_list_commentary-info = text-002.
  APPEND ls_list_commentary TO lt_list_commentary.

* Informações adicionais
  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-004.
  ls_list_commentary-info = sy-uname.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-005.
  ls_list_commentary-info = l_datum.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-006.
  ls_list_commentary-info = l_uzeit.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-007.
  ls_list_commentary-info = l_linhas.
  APPEND ls_list_commentary TO lt_list_commentary.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_list_commentary
      i_logo             = 'RENOVA_LOGO'.

ENDFORM.                    " TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  check_autorizacoes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_autorizacoes .

  DATA: p_msg(40),
        l_field  LIKE ust12-field,
        l_value  LIKE ust12-von.

  l_field = 'WERKS'.
  l_value = p_werks.

  CALL FUNCTION 'AUTHORITY_CHECK'
    EXPORTING
      user                = sy-uname
      object              = 'ZREPROCESS'
      field1              = l_field
      value1              = l_value
    EXCEPTIONS
      user_dont_exist     = 1
      user_is_authorized  = 2
      user_not_authorized = 3
      user_is_locked      = 4
      OTHERS              = 5.
  CASE sy-subrc.
    WHEN 1.
      l_aut = 'X'.
      CONCATENATE 'User' sy-uname 'não existe.'
                  INTO p_msg SEPARATED BY space.
    WHEN 2.
      CLEAR: l_aut.
    WHEN 3.
      l_aut = 'X'.
      p_msg = 'Sem autorização para execução'.
    WHEN 4.
      l_aut = 'X'.
      CONCATENATE 'User' sy-uname 'está bloqueado.'
                  INTO p_msg SEPARATED BY space.
    WHEN OTHERS.
      l_aut = 'X'.
      p_msg = 'Sem autorização para execução'.
  ENDCASE.

  CHECK NOT p_msg IS INITIAL.
  MESSAGE i000 WITH p_msg.

ENDFORM.                    " check_autorizacoes
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
  CONCATENATE sy-datum(4) sy-datum+4(2) '01' INTO s_data-low.
  s_data-sign = 'I'.
  s_data-option = 'BT'.

  s_data-high = s_data-low + 31.
  CONCATENATE s_data-high(4) s_data-high+4(2) '01' INTO s_data-high.
  s_data-high = s_data-high - 1.
  APPEND s_data.
ENDFORM.                    " INITIALIZATION

************************************************************************
*   Form HOTSPOT_ACTIVO
************************************************************************
FORM hotspot_activo USING r_ucomm
                 CHANGING ls_selfield TYPE slis_selfield.

  DATA: l_idx   LIKE sy-tabix,
        l_tabix LIKE sy-tabix.

  CASE r_ucomm.
    WHEN 'REFR'.
      CLEAR: gt_itab.

      PERFORM get_dados.
      ls_selfield-refresh = 'X'.
  ENDCASE.

ENDFORM.                    "hotspot_activo
