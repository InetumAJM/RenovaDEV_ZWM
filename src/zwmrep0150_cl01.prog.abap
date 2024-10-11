*&---------------------------------------------------------------------*
*&  Include           ZWMREP0150_CL01
*&---------------------------------------------------------------------*

CLASS lcl_handle_events  DEFINITION.

  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function sender.

ENDCLASS.

CLASS lcl_handle_events  IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM on_user_command USING e_salv_function.
*    DATA: lr_selections TYPE REF TO cl_salv_selections.
*    DATA: lt_rows TYPE salv_t_row.
*    DATA: ls_rows TYPE i.
*    DATA: message TYPE string.
*    CASE e_salv_function.
*      WHEN 'MYFUNCTION'.
*        MESSAGE : 'You pushed the button!' TYPE 'I'.
*    ENDCASE.
  ENDMETHOD.

ENDCLASS.
