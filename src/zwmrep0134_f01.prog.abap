*&---------------------------------------------------------------------*
*&  Include           ZWMREP0134_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .

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

  PERFORM call_screen_0001.

ENDFORM.                    " START_OF_SELECTION
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_screen_0001.

  PERFORM get_data.

  CALL SCREEN 0001.

ENDFORM.                    " CALL_SCREEN_0001
*&---------------------------------------------------------------------*
*&      Form  EXIT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_0001.
  LEAVE TO SCREEN 0.
ENDFORM.                    " EXIT_0001
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
    WHEN 'CRIARNT'.
      PERFORM create_nt_0001.
    WHEN OTHERS.
  ENDCASE.

  CLEAR: scr0001-okcode.
ENDFORM.                    " USER_COMMAND_0001
*&---------------------------------------------------------------------*
*&      Form  SAVE_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_0001.



ENDFORM.                    " SAVE_0001
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: lt_makt TYPE SORTED TABLE OF makt WITH UNIQUE KEY matnr.

  DATA: ls_makt TYPE makt.

  FIELD-SYMBOLS: <ls_data> TYPE gty_data.

  SELECT * FROM zwm067
           INTO TABLE gt_data
           WHERE lgnum = gv_lgnum AND
                 deleted = abap_false.

  SORT gt_data BY datbe matnr.

  LOOP AT gt_data ASSIGNING <ls_data>.
    PERFORM get_line_info CHANGING <ls_data>.
  ENDLOOP.

  DO 20 TIMES.
    CLEAR: gs_data.
    INSERT gs_data INTO TABLE gt_data.
  ENDDO.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_LINE_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<LS_DATA>  text
*----------------------------------------------------------------------*
FORM get_line_info CHANGING cs_data TYPE gty_data.
  DATA: ls_makt TYPE makt.

  cs_data-lgnum = gv_lgnum.

  DO 1 TIMES.
    CHECK NOT cs_data-matnr IS INITIAL.

    SELECT SINGLE * FROM makt
                    INTO ls_makt
                    WHERE matnr = cs_data-matnr AND
                          spras = sy-langu.


    cs_data-maktx = ls_makt-maktx.
  ENDDO.


ENDFORM.                    " GET_LINE_INFO
*&---------------------------------------------------------------------*
*&      Form  SAVE_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_line USING uv_line
               CHANGING cs_data TYPE gty_data.
  DATA: ls_zwm067 TYPE zwm067,
        ls_data   TYPE gty_data.

  CHECK NOT cs_data-matnr IS INITIAL AND
        NOT cs_data-datbe IS INITIAL AND
        NOT cs_data-datuv IS INITIAL.

  IF cs_data-stlnr IS INITIAL OR
     cs_data-stlal IS INITIAL.
**  Indicar uma Lista Técnica para o Material &
    MESSAGE s063 DISPLAY LIKE 'E' WITH cs_data-matnr.
    EXIT.
  ENDIF.

  IF cs_data-datuv IS INITIAL.
    cs_data-datuv = sy-datum.
  ENDIF.

  IF cs_data-datbe IS INITIAL.
    cs_data-datbe = '99991231'.
  ENDIF.

** Valida Overlap
***********************************************************************
  LOOP AT gt_data INTO ls_data WHERE matnr = cs_data-matnr AND
                                     datuv <= cs_data-datuv AND
                                     datbe >= cs_data-datbe AND
                                     inactive EQ abap_false AND
                                     deleted EQ abap_false.
    CHECK uv_line <> sy-tabix.

**  Meterial & com sobreposição de datas
    MESSAGE s073 DISPLAY LIKE 'E' WITH cs_data-matnr.
    RETURN.
  ENDLOOP.

  LOOP AT gt_data INTO ls_data WHERE matnr = cs_data-matnr AND
                                     datbe >= cs_data-datuv AND
                                     inactive EQ abap_false AND
                                     deleted EQ abap_false.
    CHECK uv_line <> sy-tabix.

**  Meterial & com sobreposição de datas
    MESSAGE s073 DISPLAY LIKE 'E' WITH cs_data-matnr.
    RETURN.
  ENDLOOP.

** Insere Nova Linha Se Não Existir
**********************************************************************
  ls_zwm067-lgnum = gv_lgnum.
  ls_zwm067-matnr = cs_data-matnr.
  ls_zwm067-datbe = cs_data-datbe.
  INSERT zwm067 FROM ls_zwm067.

** Update de Campos (NUNCA ALTERA LOTE)
**********************************************************************
  UPDATE zwm067 SET stlnr = cs_data-stlnr
                    stlal = cs_data-stlal
                    datuv = cs_data-datuv
                    begti = cs_data-begti
                    datbe = cs_data-datbe
                    endeuz = cs_data-endeuz
                    inactive = cs_data-inactive
                WHERE lgnum = gv_lgnum AND
                      matnr = cs_data-matnr AND
                      datbe = cs_data-datbe.

  COMMIT WORK.
ENDFORM.                    " SAVE_LINE
*&---------------------------------------------------------------------*
*&      Form  STLNR_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  stlnr_value_request
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM stlnr_value_request.
  TYPES: BEGIN OF lty_table,
           stlnr TYPE stnum,
         END OF lty_table.
  DATA: lt_mast    TYPE TABLE OF mast,
        lt_stko    TYPE TABLE OF stko,
        lt_table   TYPE TABLE OF lty_table,
        lt_return  TYPE TABLE OF ddshretval,
        lt_mapping TYPE TABLE OF dselc.

  DATA: ls_mast    TYPE mast,
        ls_stko    TYPE stko,
        ls_table   TYPE lty_table,
        ls_return  TYPE ddshretval,
        ls_mapping TYPE dselc.

  DATA: lv_linno TYPE sytabix.

  FIELD-SYMBOLS: <ls_data> TYPE gty_data.

  GET CURSOR LINE lv_linno.
  lv_linno = lv_linno + ztc0001-top_line - 1 .
  READ TABLE gt_data
   ASSIGNING <ls_data>
   INDEX lv_linno.

  CHECK <ls_data> IS ASSIGNED.
  CHECK NOT <ls_data>-matnr IS INITIAL.



  DO 1 TIMES.
    SELECT * FROM mast
             INTO TABLE lt_mast
             WHERE matnr = <ls_data>-matnr AND
                   werks =  gv_werks AND
                   stlan = '5'.
    CHECK sy-subrc EQ 0.

    CHECK NOT lt_mast IS INITIAL.

    SELECT * FROM stko
             INTO TABLE lt_stko
             FOR ALL ENTRIES IN lt_mast
             WHERE stlnr = lt_mast-stlnr AND
                   datuv <= sy-datum.

    CHECK sy-subrc EQ 0.

    LOOP AT lt_mast INTO ls_mast.
      LOOP AT lt_stko INTO ls_stko WHERE stlnr = ls_mast-stlnr.
        CHECK ls_stko-bmeng > 0.

        ls_table-stlnr = ls_stko-stlnr.

        APPEND ls_table TO lt_table.
      ENDLOOP.
    ENDLOOP.
  ENDDO.

  IF lt_table IS INITIAL.
    MESSAGE s056 DISPLAY LIKE 'E' WITH <ls_data>-matnr.
    EXIT.
  ENDIF.

  SORT lt_table.
  DELETE ADJACENT DUPLICATES FROM lt_table.

  CLEAR ls_mapping.
  ls_mapping-fldname     = 'F0001'.
  ls_mapping-dyfldname   = 'GV_STLNR'.
  APPEND ls_mapping TO lt_mapping.

  CLEAR ls_mapping.
  ls_mapping-fldname     = 'F0002'.
  ls_mapping-dyfldname   = 'GV_STLAL'.
  APPEND ls_mapping TO lt_mapping.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'STLNR'
      value_org       = 'S'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
    TABLES
      value_tab       = lt_table
      return_tab      = lt_return
      dynpfld_mapping = lt_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CLEAR: ls_return.
  READ TABLE lt_return
        INTO ls_return
        WITH KEY fieldname = 'F0001'.

  CHECK NOT ls_return-fieldval IS INITIAL.
  <ls_data>-stlnr = ls_return-fieldval.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = <ls_data>-stlnr
    IMPORTING
      output = <ls_data>-stlnr.


  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = 'NEXT'
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

  PERFORM save_line USING lv_linno
                    CHANGING <ls_data>.
ENDFORM.                    " STLNR_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  STLAL_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM stlal_value_request .
  TYPES: BEGIN OF lty_table,
           stlnr TYPE stnum,
           stlal TYPE stlal,
         END OF lty_table.
  DATA: lt_mast    TYPE TABLE OF mast,
        lt_stko    TYPE TABLE OF stko,
        lt_table   TYPE TABLE OF lty_table,
        lt_return  TYPE TABLE OF ddshretval,
        lt_mapping TYPE TABLE OF dselc.

  DATA: ls_mast    TYPE mast,
        ls_stko    TYPE stko,
        ls_table   TYPE lty_table,
        ls_return  TYPE ddshretval,
        ls_mapping TYPE dselc.

  DATA: lv_linno TYPE sytabix.

  FIELD-SYMBOLS: <ls_data> TYPE gty_data.

  GET CURSOR LINE lv_linno.
  lv_linno = lv_linno + ztc0001-top_line - 1 .
  READ TABLE gt_data
   ASSIGNING <ls_data>
   INDEX lv_linno.

  CHECK <ls_data> IS ASSIGNED.
  CHECK NOT <ls_data>-matnr IS INITIAL AND
        NOT <ls_data>-stlnr IS INITIAL.


  DO 1 TIMES.
    SELECT * FROM mast
             INTO TABLE lt_mast
             WHERE matnr = <ls_data>-matnr AND
                   werks =  gv_werks AND
                   stlan = '5'.
    CHECK sy-subrc EQ 0.

    DELETE lt_mast WHERE stlnr <> <ls_data>-stlnr.

    CHECK NOT lt_mast IS INITIAL.

    SELECT * FROM stko
             INTO TABLE lt_stko
             FOR ALL ENTRIES IN lt_mast
             WHERE stlnr = lt_mast-stlnr AND
                   datuv <= sy-datum.

    CHECK sy-subrc EQ 0.

    LOOP AT lt_mast INTO ls_mast.
      LOOP AT lt_stko INTO ls_stko WHERE stlnr = ls_mast-stlnr.
        CHECK ls_stko-bmeng > 0.

        ls_table-stlnr = ls_stko-stlnr.
        ls_table-stlal = ls_stko-stlal.

        APPEND ls_table TO lt_table.
      ENDLOOP.
    ENDLOOP.
  ENDDO.

  DELETE lt_table WHERE stlnr <> <ls_data>-stlnr.

  IF lt_table IS INITIAL.
    MESSAGE s056 DISPLAY LIKE 'E' WITH <ls_data>-matnr.
    EXIT.
  ENDIF.

  SORT lt_table.
  DELETE ADJACENT DUPLICATES FROM lt_table.

  CLEAR ls_mapping.
  ls_mapping-fldname     = 'F0001'.
  ls_mapping-dyfldname   = 'GV_STLNR'.
  APPEND ls_mapping TO lt_mapping.

  CLEAR ls_mapping.
  ls_mapping-fldname     = 'F0002'.
  ls_mapping-dyfldname   = 'GV_STLAL'.
  APPEND ls_mapping TO lt_mapping.



  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'STLNR'
      value_org       = 'S'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
    TABLES
      value_tab       = lt_table
      return_tab      = lt_return
      dynpfld_mapping = lt_mapping
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  CLEAR: ls_return.
  READ TABLE lt_return
        INTO ls_return
        WITH KEY fieldname = 'F0001'.

  CHECK NOT ls_return-fieldval IS INITIAL.
  <ls_data>-stlnr = ls_return-fieldval.

  CLEAR: ls_return.
  READ TABLE lt_return
        INTO ls_return
        WITH KEY fieldname = 'F0002'.

  CHECK NOT ls_return-fieldval IS INITIAL.
  <ls_data>-stlal = ls_return-fieldval.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = <ls_data>-stlal
    IMPORTING
      output = <ls_data>-stlal.


  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = 'NEXT'
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

  PERFORM save_line USING lv_linno
                    CHANGING <ls_data>.
ENDFORM.                    " STLAL_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  STATUS_ZTC0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM status_ztc0001.
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'GS_DATA-MATNR'.
        IF NOT gs_data-matnr IS INITIAL.
          screen-input = 0.
        ENDIF.
      WHEN 'GS_DATA-STLNR'.
        IF gs_data-matnr IS INITIAL.
          screen-input = 0.
        ENDIF.
      WHEN 'GS_DATA-STLAL'.
        IF gs_data-matnr IS INITIAL OR
           gs_data-stlnr IS INITIAL.
          screen-input = 0.
        ENDIF.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " STATUS_ZTC0001

FORM get_selected_lines CHANGING lt_data TYPE gty_t_data.
  DATA: ls_data TYPE gty_data.

  CLEAR: lt_data.

  LOOP AT gt_data INTO ls_data WHERE check = abap_true.
    APPEND ls_data TO lt_data.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_NT_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_nt_0001.
  DATA: lt_data       TYPE gty_t_data,
        lt_ltba       TYPE zwm_tt_ltba,
        lt_messages   TYPE tab_bdcmsgcoll,
        lt_components TYPE z_wm_cl_management=>t_matnr_prd_components,
        lt_fields	    TYPE TABLE OF sval,
        lt_ltba_out   TYPE zwm_tt_ltba.

  DATA: ls_ltba      TYPE ltba,
        ls_data      TYPE gty_data,
        ls_field     TYPE sval,
        ls_mara      TYPE mara,
        ls_component TYPE z_wm_cl_management=>matnr_prd_components.

  DATA: lv_lines      TYPE sytabix,
        lv_menge      TYPE menge_d,
        lv_menge_f    TYPE f,
        lv_returncode TYPE c,
        lv_tbnum      TYPE tbnum.

  PERFORM get_selected_lines CHANGING lt_data.
  CHECK NOT lt_data IS INITIAL.

  DESCRIBE TABLE lt_data LINES lv_lines.
  IF lv_lines > 1.
    MESSAGE s090 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: ls_data.
  READ TABLE lt_data
        INTO ls_data
        INDEX 1.

  IF z_wm_cl_management=>is_tr_matnr_created( i_lgnum = ls_data-lgnum i_matnr = ls_data-matnr is_data = ls_data ).
**  Necessidade para Material & já existe
    MESSAGE s091 DISPLAY LIKE 'E' WITH ls_data-matnr.
    RETURN.
  ENDIF.

  SELECT SINGLE * FROM mara
                  INTO ls_mara
                  WHERE matnr = ls_data-matnr.

  ls_field-fieldtext = ls_data-maktx.
  ls_field-tabname   = 'LTBP'.
  ls_field-fieldname = 'MENGE'.
  ls_field-field_obl = abap_true.
  APPEND ls_field TO lt_fields.

  CLEAR: ls_field.
  ls_field-value     = ls_mara-meins.
  ls_field-tabname   = 'LTBP'.
  ls_field-fieldname = 'MEINS'.
  ls_field-field_attr = '02'.
  APPEND ls_field TO lt_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      no_value_check  = ' '
      popup_title     = text-001
      start_column    = '5'
      start_row       = '5'
    IMPORTING
      returncode      = lv_returncode
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  READ TABLE lt_fields
        INTO ls_field
        INDEX 1.

  lv_menge = ls_field-value.
  CHECK lv_menge >= 1.

  CALL FUNCTION 'ROUND'
    EXPORTING
      input         = lv_menge
    IMPORTING
      output        = lv_menge
    EXCEPTIONS
      input_invalid = 1
      overflow      = 2
      type_invalid  = 3
      OTHERS        = 4.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CALL METHOD z_wm_cl_management=>get_componets
    EXPORTING
      i_lgnum       = gv_lgnum
      i_werks       = gv_werks
      i_matnr       = ls_data-matnr
      i_stlnr       = ls_data-stlnr
      i_stlal       = ls_data-stlal
      i_menge_prod  = lv_menge
    IMPORTING
      et_messages   = lt_messages
      et_components = lt_components
    EXCEPTIONS
      error         = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
    PERFORM show_message USING lt_messages.
    EXIT.
  ENDIF.

  LOOP AT lt_components INTO ls_component WHERE pal_menge IS NOT INITIAL.
*    ls_ltba-tabix = sy-tabix.
*    ls_ltba-tbpos = sy-tabix.
    ls_ltba-lgnum = ls_component-lgnum.
    ls_ltba-tbktx = lv_menge.
    ls_ltba-matnr = ls_component-matnr.
    ls_ltba-werks = ls_component-werks.
    ls_ltba-lgort = ls_component-lgort.
    ls_ltba-menga = ls_component-pal_menge.
    ls_ltba-altme = ls_component-meins.
    ls_ltba-bwlvs = gv_bwlvs_nt.
    ls_ltba-betyp = 'M'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_data-matnr
      IMPORTING
        output = ls_ltba-benum.

    APPEND ls_ltba TO lt_ltba.
  ENDLOOP.

  IF NOT lt_ltba IS  INITIAL.

    CALL FUNCTION 'ZWM_TR_CREATE'
      EXPORTING
        i_update_task = abap_true
        i_commit_work = abap_true
        it_ltba       = lt_ltba
      IMPORTING
        e_tbnum       = lv_tbnum
        et_ltba       = lt_ltba_out
        et_messages   = lt_messages
      EXCEPTIONS
        error         = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      PERFORM show_message USING lt_messages.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.

FORM show_message USING ut_messages TYPE tab_bdcmsgcoll.

  READ TABLE ut_messages
        INTO DATA(ls_message)
        INDEX 1.


  MESSAGE ID ls_message-msgid TYPE 'I'
                              NUMBER ls_message-msgnr
                              DISPLAY LIKE ls_message-msgtyp
                              WITH ls_message-msgv1 ls_message-msgv2
                                   ls_message-msgv3 ls_message-msgv4.

ENDFORM.
