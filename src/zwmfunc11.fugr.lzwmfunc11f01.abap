*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC11F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FORMATA_ENDERECO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ADRC  text
*----------------------------------------------------------------------*
FORM formata_endereco USING tab TYPE adrc.
  PERFORM formata USING tab-name1.
  PERFORM formata USING tab-name2.
  PERFORM formata USING tab-name3.
  PERFORM formata USING tab-name4.
  PERFORM formata USING tab-city1.
  PERFORM formata USING tab-city2.
  PERFORM formata USING tab-street.
ENDFORM.                    " FORMATA_ENDERECO
*&---------------------------------------------------------------------*
*&      Form  FORMATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TAB_NAME1  text
*----------------------------------------------------------------------*
FORM formata  USING inout_string.

  CALL FUNCTION 'ISP_CONVERT_FIRSTCHARS_TOUPPER'
    EXPORTING
      input_string        = inout_string
*   SEPARATORS          = ' -.,;:'
   IMPORTING
     output_string       = inout_string.
ENDFORM.                    " FORMATA
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BDC_TAB  text
*      -->P_0008   text
*      -->P_0009   text
*      -->P_0010   text
*----------------------------------------------------------------------*
FORM dynpro TABLES bdc_tab STRUCTURE bdcdata
            USING value(dynbegin) value(name) value(value).

  CLEAR bdc_tab.
  IF dynbegin = 'X'.
    bdc_tab-program  = name.
    bdc_tab-dynpro   = value.
    bdc_tab-dynbegin = 'X'.
  ELSE .
    bdc_tab-fnam = name.
    bdc_tab-fval = value.
  ENDIF.
  APPEND bdc_tab.

ENDFORM.                               " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  UPDATE_0001_QUANTITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DATA_READS  text
*      <--P_LT_DATA  text
*      <--P_LS_DATA  text
*----------------------------------------------------------------------*
FORM update_0001_quantity USING ut_data_reads TYPE gty_t_data_consolidation "Tabela das Leituras
                          CHANGING ct_data    TYPE gty_t_data_consolidation "Tabela de Header
                                   cs_data    TYPE gty_data_consolidation.  "Struc de Tela
  DATA: ls_data_reads TYPE gty_data_consolidation,
        ls_data       TYPE gty_data_consolidation.

  FIELD-SYMBOLS: <ls_data> TYPE gty_data_consolidation.

  CLEAR: cs_data-menge_l, cs_data-menge_f.

  LOOP AT ct_data ASSIGNING <ls_data>.
    CLEAR: <ls_data>-menge_l, <ls_data>-menge_f.

    LOOP AT ut_data_reads INTO ls_data_reads WHERE s_stpo-idnrk = <ls_data>-s_stpo-idnrk.
      <ls_data>-menge_l = <ls_data>-menge_l + ls_data_reads-menge.
    ENDLOOP.

    <ls_data>-menge_f = <ls_data>-s_stpo-menge - <ls_data>-menge_l.
    IF <ls_data>-menge_f < 0.
      <ls_data>-menge_f = 0.
    ENDIF.
  ENDLOOP.

  CLEAR: ls_data.
  READ TABLE ct_data
        INTO ls_data
        INDEX cs_data-index.

  CHECK sy-subrc EQ 0.

  cs_data-menge_l = ls_data-menge_l.
  cs_data-menge_f = ls_data-menge_f.

ENDFORM.                    " UPDATE_0001_QUANTITY
*&---------------------------------------------------------------------*
*&      Form  GOODSMVT_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MSEGC  text
*----------------------------------------------------------------------*
FORM goodsmvt_cancel USING ut_mseg TYPE gty_t_mseg.
  DATA: lt_messages TYPE tab_bdcmsgcoll,
        lt_mseg     TYPE gty_t_mseg.

  DATA: ls_mseg TYPE mseg.

  lt_mseg = ut_mseg.
  SORT lt_mseg BY mblnr mjahr.
  DELETE ADJACENT DUPLICATES FROM lt_mseg COMPARING mblnr mjahr.

  LOOP AT lt_mseg INTO ls_mseg.
    CALL FUNCTION 'ZWM_GOODSMVT_CANCEL'
      EXPORTING
        i_mblnr     = ls_mseg-mblnr
        i_mjahr     = ls_mseg-mjahr
      IMPORTING
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
  ENDLOOP.
ENDFORM.                    " GOODSMVT_CANCEL
*&---------------------------------------------------------------------*
*&      Form  FILTER_WHS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_WERKS  text
*      <--P_C_WERKS  text
*      <--P_LR_WERKS  text
*----------------------------------------------------------------------*
FORM filter_whs_data  CHANGING ct_table TYPE STANDARD TABLE
                               c_field
                               cr_range TYPE STANDARD TABLE.

  DATA: lv_lines TYPE sytabix.

  DATA: lo_data TYPE REF TO data.

  FIELD-SYMBOLS: <ls_r_range> TYPE ANY,
                 <lv_field>   TYPE ANY,
                 <lv_table>   TYPE ANY.

** Create Data
***********************************************************************
  CREATE DATA lo_data LIKE LINE OF cr_range.
  CHECK lo_data IS BOUND.
  ASSIGN lo_data->* TO <ls_r_range>.
  CHECK <ls_r_range> IS ASSIGNED.

** Returns The Only Entry
***********************************************************************
  IF NOT c_field IS INITIAL.
    CLEAR ct_table.
    APPEND c_field TO ct_table.
  ENDIF.

  SORT ct_table.
  DELETE ADJACENT DUPLICATES FROM ct_table COMPARING ALL FIELDS.
  DESCRIBE TABLE ct_table LINES lv_lines.
  IF lv_lines EQ 1.
    READ TABLE ct_table
          INTO c_field
          INDEX 1.
  ENDIF.

  IF NOT c_field IS INITIAL.
    CLEAR: cr_range, <ls_r_range>.

    UNASSIGN <lv_field>.
    ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_r_range> TO <lv_field>.
    CHECK <lv_field> IS ASSIGNED.
    <lv_field> = 'I'.

    UNASSIGN <lv_field>.
    ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_r_range> TO <lv_field>.
    CHECK <lv_field> IS ASSIGNED.
    <lv_field> = 'EQ'.

    UNASSIGN <lv_field>.
    ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_r_range> TO <lv_field>.
    CHECK <lv_field> IS ASSIGNED.
    <lv_field> = c_field.

    APPEND <ls_r_range> TO cr_range.

** Returns Multiple Entries
***********************************************************************
  ELSEIF NOT ct_table IS INITIAL.
    CLEAR: cr_range.
    LOOP AT ct_table ASSIGNING <lv_table>.
      CLEAR: <ls_r_range>.

      UNASSIGN <lv_field>.
      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <ls_r_range> TO <lv_field>.
      CHECK <lv_field> IS ASSIGNED.
      <lv_field> = 'I'.

      UNASSIGN <lv_field>.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_r_range> TO <lv_field>.
      CHECK <lv_field> IS ASSIGNED.
      <lv_field> = 'EQ'.

      UNASSIGN <lv_field>.
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_r_range> TO <lv_field>.
      CHECK <lv_field> IS ASSIGNED.
      <lv_field> = <lv_table>.

      APPEND <ls_r_range> TO cr_range.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " FILTER_WHS_DATA
