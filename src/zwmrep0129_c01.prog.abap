*&---------------------------------------------------------------------*
*&  Include           ZWMREP0129_C01
*&---------------------------------------------------------------------*
**********************************************************************
**  Classes
**********************************************************************
CLASS lcl_tree_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS handle_item_double_click
      FOR EVENT item_double_click OF cl_gui_alv_tree
      IMPORTING node_key
                fieldname.

    METHODS handle_button_click
      FOR EVENT button_click OF cl_gui_alv_tree
      IMPORTING node_key
                fieldname.

    METHODS handle_link_click
      FOR EVENT link_click OF cl_gui_alv_tree
      IMPORTING node_key
                fieldname.

    METHODS handle_header_click
      FOR EVENT header_click OF cl_gui_alv_tree
      IMPORTING fieldname.

ENDCLASS.                    "lcl_tree_event_receiver DEFINITION
**********************************************************************
CLASS lcl_tree_event_receiver IMPLEMENTATION.
  METHOD handle_item_double_click.
    PERFORM event_double_click USING node_key fieldname.
  ENDMETHOD.                    "handle_item_double_click

  METHOD handle_button_click.
  ENDMETHOD.                    "handle_button_click

  METHOD handle_link_click.
  ENDMETHOD.                    "handle_link_click

  METHOD handle_header_click.
  ENDMETHOD.                    "handle_header_click

ENDCLASS.                    "lcl_tree_event_receiver IMPLEMENTATION
**********************************************************************
CLASS lcl_toolbar_event_receiver DEFINITION.

  PUBLIC SECTION.

** MÉTODO REGISTADO NA CRIAÇÃO DO ALV-TREE - PERFORM register_events.
    METHODS: on_button_clicked
              FOR EVENT function_selected OF cl_gui_toolbar
                  IMPORTING fcode.


ENDCLASS.                    "lcl_toolbar_event_receiver DEFINITION
**********************************************************************
CLASS lcl_toolbar_event_receiver IMPLEMENTATION.

** ATENÇÃO É NECESSÁRIO QUANDO FOR PARA ADICIONAR BOTOES AO TOOLBAR
  METHOD on_button_clicked.
    CASE fcode.
      WHEN 'DEL'.
        PERFORM delete_sscc.
      WHEN 'EDIT'.
        PERFORM edit_sscc.
      WHEN 'SAVE'.
        PERFORM save_sscc.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "on_dropdown_clicked

ENDCLASS.                    "lcl_toolbar_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_grid_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_grid_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS:
    handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object e_interactive,

    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm,

    handle_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
            IMPORTING er_data_changed.

ENDCLASS.                    "lcl_grid_event_receiver DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_grid_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_grid_event_receiver IMPLEMENTATION.

  METHOD handle_toolbar.
*    DATA: ls_toolbar  TYPE stb_button.
*
** append a separator to normal toolbar
*    CLEAR ls_toolbar.
*    ls_toolbar-butn_type = 3.
*    APPEND ls_toolbar TO e_object->mt_toolbar.
*
** append an icon
*    CLEAR ls_toolbar.
*    ls_toolbar-function  = 'REM'.
*    ls_toolbar-icon      = '@18@'.
*    ls_toolbar-quickinfo = 'Remover Item'.
*    ls_toolbar-text      = ''.
*    ls_toolbar-disabled  = ''.
*    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.                    "handle_toolbar
*-------------------------------------------------------------------
  METHOD handle_user_command.

*    CASE e_ucomm.
*      WHEN 'REM'.
*        PERFORM remove_item.
*    ENDCASE.

  ENDMETHOD.                           "handle_user_command
*-----------------------------------------------------------------
  METHOD handle_data_changed.
    PERFORM data_changed USING er_data_changed.
  ENDMETHOD.                    "handle_data_changed

ENDCLASS.                    " lcl_grid_event_receiver IMPLEMENTATION


CLASS cl_gui_column_tree DEFINITION LOAD.
CLASS cl_gui_cfw         DEFINITION LOAD.
