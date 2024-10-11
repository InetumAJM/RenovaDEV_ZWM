*&---------------------------------------------------------------------*
*&  Include           ZWMREP0144_C01
*&---------------------------------------------------------------------*
** Classes
***********************************************************************
CLASS lcl_event_receiver DEFINITION DEFERRED.

*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_toolbar       FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_menu_button   FOR EVENT menu_button OF cl_gui_alv_grid
        IMPORTING e_object e_ucomm,

      handle_user_command  FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "lcl_event_receiver DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_toolbar.
    DATA: ls_toolbar  TYPE stb_button.

    IF gv_mod IS NOT INITIAL.
      " Separator
      CLEAR ls_toolbar.
      ls_toolbar-butn_type = 3.
      APPEND ls_toolbar TO e_object->mt_toolbar.

      " Button
      CLEAR ls_toolbar.
      ls_toolbar-function  = 'PROCESS'.
      ls_toolbar-icon      = icon_activity.
      ls_toolbar-quickinfo = 'Processar'.
      ls_toolbar-text      = 'Processar'.
      ls_toolbar-disabled  = ' '.
      APPEND ls_toolbar TO e_object->mt_toolbar.

*      " Separator
*      CLEAR ls_toolbar.
*      ls_toolbar-butn_type = 3.
*      APPEND ls_toolbar TO e_object->mt_toolbar.

*      " Button
*      CLEAR ls_toolbar.
*      ls_toolbar-function  = 'DELETE'.
*      ls_toolbar-icon      = icon_delete_row.
*      ls_toolbar-quickinfo = 'Eliminar Registo'.
*      ls_toolbar-text      = 'Eliminar'.
*      ls_toolbar-disabled  = ' '.
*      APPEND ls_toolbar TO e_object->mt_toolbar.
*
*      " Button
*      CLEAR ls_toolbar.
*      ls_toolbar-function  = 'ADD'.
*      ls_toolbar-icon      = icon_insert_row.
*      ls_toolbar-quickinfo = 'Adicionar Registo'.
*      ls_toolbar-text      = 'Adicionar'.
*      ls_toolbar-disabled  = ' '.
*      APPEND ls_toolbar TO e_object->mt_toolbar.
    ENDIF.

    " Menu Button.
*    CLEAR ls_toolbar.
*    ls_toolbar-butn_type = 2.
*    ls_toolbar-function  = 'DETAIL_MENU'.
*    ls_toolbar-icon      = icon_detail.
*    ls_toolbar-quickinfo = 'Mudar Menu'.
*    ls_toolbar-text      = 'Mudar Menu'.
*    ls_toolbar-disabled  = ' '.
*    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.                    "handle_toolbar

  METHOD handle_menu_button.
    IF e_ucomm = 'DETAIL_MENU'.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'DETAIL_MENU1'
          text  = 'Menu1'.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'DETAIL_MENU2'
          text  = 'Menu2'.
    ENDIF.

  ENDMETHOD.                    "handle_menu_button

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'PROCESS'.
        PERFORM process_selection.
      WHEN 'DELETE'.
*        PERFORM delete_selection.
      WHEN 'ADD'.
*        PERFORM add_row.
    ENDCASE.

    CALL METHOD cl_gui_cfw=>flush.
  ENDMETHOD.                           "handle_user_command

  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no.
  ENDMETHOD.                           "handle_hotspot_click
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATIO.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM handle_hotspot_click  USING pu_row_id    TYPE lvc_s_row
                                 pu_column_id TYPE lvc_s_col
                                 pu_row_no    TYPE lvc_s_roid.

  DATA: lv_col_nm TYPE string.
  DATA: lv_kunnr  TYPE kunnr.

  DATA: lr_tanum TYPE RANGE OF ltak-tanum,
        ls_tanum LIKE LINE OF lr_tanum.
*
  FIELD-SYMBOLS: <ls_data>    TYPE any,
                 <lv_value_1> TYPE any,
                 <lv_value_2> TYPE any,
                 <lv_value_3> TYPE any.

***********************************************************************
  CHECK gt_alv_table[] IS NOT INITIAL.

** Working Area
  READ TABLE gt_alv_table INDEX pu_row_id-index ASSIGNING <ls_data>.
  CHECK sy-subrc EQ 0.

** Valor
  CONCATENATE '<LS_DATA>-' pu_column_id-fieldname INTO lv_col_nm.
  CONDENSE lv_col_nm NO-GAPS.

  ASSIGN (lv_col_nm) TO <lv_value_1>.
  CHECK sy-subrc EQ 0.

  CHECK NOT <lv_value_1> IS INITIAL.

  CASE pu_column_id-fieldname.

** Ordem
*    WHEN 'S_ORDER'.
*      PERFORM check_order USING <lv_value_1> pu_row_id-index.
*
*
*** Cliente
*    WHEN 'CONTRACT_ID'.
*      lv_kunnr = <lv_value_1>.
*
*      PERFORM routynff_tela_clientes USING lv_kunnr.

*      SET PARAMETER ID 'KUN'  FIELD <lv_value_1>.
*      CALL TRANSACTION 'XD03' AND SKIP FIRST SCREEN.

**** Ordem de venda
**    WHEN 'VBELN_VA'.
**      SET PARAMETER ID 'AUN'  FIELD <lv_value_1>.
**      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
**
****  Remessa
**    WHEN 'VBELN'.
**      SET PARAMETER ID 'VL'  FIELD <lv_value_1>.
**      CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
**
****  Documento MM
*************************************************************************
**    WHEN 'MBLNR'.
**      SET PARAMETER ID 'MBN'  FIELD <lv_value_1>.
**      ASSIGN ('<LS_DATA>-MJAHR') TO <lv_value_2>.
**      IF sy-subrc EQ 0.
**        SET PARAMETER ID 'MJA' FIELD <lv_value_2>.
**      ENDIF.
**      CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.
**
****  Posição
*************************************************************************
**    WHEN 'LGPLA' OR 'NLPLA' OR 'VLPLA'.
****    Armazem
**      ASSIGN ('<LS_DATA>-LGNUM') TO <lv_value_2>.
**      CHECK sy-subrc EQ 0.
**      CHECK NOT <lv_value_2> IS INITIAL.
**
****    Tipo de Depósito
**      CLEAR lv_col_nm.
**      CONCATENATE '<LS_DATA>-' pu_column_id-fieldname(2) 'PLA'
**             INTO lv_col_nm.
**      ASSIGN (lv_col_nm) TO <lv_value_3>.
**      CHECK sy-subrc EQ 0.
**      CHECK NOT <lv_value_3> IS INITIAL.
**
**      SET PARAMETER ID 'LGN'  FIELD <lv_value_2>.
**      SET PARAMETER ID 'LGT'  FIELD <lv_value_3>.
**      SET PARAMETER ID 'LGP'  FIELD <lv_value_1>.
**      CALL TRANSACTION 'LS03N' AND SKIP FIRST SCREEN.
**
****  Tarefa
*************************************************************************
    WHEN 'TANUM_A'.
**    Armazem
*      ASSIGN <lv_value_2> to p_lgnum.
*      CHECK NOT <lv_value_2> IS INITIAL.

      ls_tanum-sign   = 'I'.
      ls_tanum-option = 'EQ'.
      ls_tanum-low    = <lv_value_1>.
      COLLECT ls_tanum INTO lr_tanum.
      CLEAR   ls_tanum.

      SUBMIT rllt2300 WITH t1_lgnum = p_lgnum
                      WITH t1_tanum IN lr_tanum
                      WITH t1_offta = ' '
                      WITH t1_quita = ' '
                      WITH t1_allta = 'X' " Confirmadas e por Confirmar
                      AND RETURN.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK
