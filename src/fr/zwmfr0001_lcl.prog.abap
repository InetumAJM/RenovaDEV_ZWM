*----------------------------------------------------------------------*
* Include: ZWMFR0001_LCL
*----------------------------------------------------------------------*
* Description: Monitor de planeamento do abastecimento à produção
*----------------------------------------------------------------------*
* Author........: [Tiago Pateiro] [ROFFD] [ROFF(SDF)]
* Creation date: 2015-10-23
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_mc_events DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_mc_events DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS
      hndl_user_command FOR EVENT if_salv_events_functions~added_function OF cl_salv_events_tree
        IMPORTING !e_salv_function.
    CLASS-METHODS
      hndl_popup_user_command FOR EVENT if_salv_events_functions~added_function OF cl_salv_events_table
        IMPORTING !e_salv_function.
    CLASS-METHODS
      hndl_link_click FOR EVENT if_salv_events_actions_table~link_click OF cl_salv_events_table
        IMPORTING !row
                  !column.
ENDCLASS.                    "lcl_mc_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_mc_events IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_mc_events IMPLEMENTATION.
  METHOD hndl_user_command.
    CASE e_salv_function.
      WHEN c_ucomm_refresh.
        PERFORM f_data_ucomm_refresh.
      WHEN c_ucomm_tocreate.
        PERFORM f_data_ucomm_tocreate.
      WHEN c_ucomm_dupview.
        PERFORM f_data_ucomm_dupview.
      WHEN c_ucomm_toview.
        PERFORM f_data_ucomm_toview USING abap_false.
      WHEN c_ucomm_reqdev.
        PERFORM f_create_ot_dev.
      WHEN OTHERS.
        cl_gui_cfw=>dispatch( ).
    ENDCASE.

    cl_gui_cfw=>flush( ).
  ENDMETHOD.                    "hndl_user_command
  METHOD hndl_popup_user_command.
    CASE e_salv_function.
      WHEN c_ucomm_accept OR c_ucomm_cancel.
        gv_okcode_popup = e_salv_function.

        go_popup_to->close_screen( ).
      WHEN c_ucomm_tocancel.
        PERFORM f_data_ucomm_tocancel.
      WHEN OTHERS.
        cl_gui_cfw=>dispatch( ).
    ENDCASE.

    cl_gui_cfw=>flush( ).
  ENDMETHOD.                    "hndl_popup_user_command
  METHOD hndl_link_click.
    CASE column.
      WHEN c_fname_check.
        PERFORM f_data_ucomm_tocreate_setchckd USING row.
      WHEN OTHERS.
        cl_gui_cfw=>dispatch( ).
    ENDCASE.

    cl_gui_cfw=>flush( ).
  ENDMETHOD.                    "hndl_link_click
ENDCLASS.                    "lcl_mc_events IMPLEMENTATION
