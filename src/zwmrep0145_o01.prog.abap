*&---------------------------------------------------------------------*
*&  Include           ZWMREP0145_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'GUI_0100'.
  SET TITLEBAR 'GUI1'.

  PERFORM init_container.

  IF tree IS INITIAL.
    PERFORM init_tree.
  ENDIF.

  IF gcl_alv_grid IS INITIAL.
    PERFORM init_alv.
  ENDIF.

  IF flag_tree IS INITIAL.
    PERFORM creat_hierarchy.
    flag_tree = 'X'.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.


ENDMODULE.                 " STATUS_0100  OUTPUT
