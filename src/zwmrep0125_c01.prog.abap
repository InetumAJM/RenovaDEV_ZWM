*&---------------------------------------------------------------------*
*&  Include           ZWMREP0125_C01
*&---------------------------------------------------------------------*

CLASS gcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_link_click
        FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.
ENDCLASS.                    "gcl_handle_events_dt DEFINITION

*----------------------------------------------------------------------*
*       CLASS gcl_handle_events_dt IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_handle_events IMPLEMENTATION.
  METHOD on_link_click.

    DATA: lv_index TYPE i.

    MOVE row TO lv_index.
    PERFORM on_link_click USING lv_index
                                column.
  ENDMETHOD.                    "on_single_click
ENDCLASS.
