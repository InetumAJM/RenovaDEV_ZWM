*&---------------------------------------------------------------------*
*&  Include           ZWMREP0147_C01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.
ENDCLASS. "gcl_handle_events DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_handle_events IMPLEMENTATION.

  METHOD on_link_click.
    PERFORM on_link_click_0001 USING row column.
  ENDMETHOD.                   "on_link_click

ENDCLASS. "gcl_handle_events IMPLEMENTATION
