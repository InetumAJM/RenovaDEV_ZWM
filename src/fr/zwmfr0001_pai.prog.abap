*----------------------------------------------------------------------*
* Include: ZWMFR0001_PAI
*----------------------------------------------------------------------*
* Description: Monitor de planeamento do abastecimento à produção
*----------------------------------------------------------------------*
* Author........: [Tiago Pateiro] [ROFFD] [ROFF(SDF)]
* Creation date: 2015-10-23
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_0100 INPUT.
  gv_okcode_bckp  = gv_okcode.

  CLEAR gv_okcode.

  CASE gv_okcode_bckp.
    WHEN c_ucomm_accept.
      IF gs_topopup-verme_final LE gs_topopup-verme AND gs_topopup-verme_final NE 0.
        PERFORM f_data_ucomm_tocreate_save.
      ENDIF.

      FREE gt_lqua[].

      CLEAR gs_topopup.
      CLEAR gv_okcode.
      CLEAR gv_okcode_bckp.

      LEAVE TO SCREEN 0.
    WHEN c_ucomm_mansel.
      PERFORM f_data_ucomm_tocreate_select.

      IF gv_okcode_popup EQ c_ucomm_accept.
        IF gs_topopup-verme_final LE gs_topopup-verme AND gs_topopup-verme_final NE 0.
          PERFORM f_data_ucomm_tocreate_save.
        ENDIF.

        FREE gt_lqua[].

        CLEAR gs_topopup.
        CLEAR gv_okcode.
        CLEAR gv_okcode_bckp.
        CLEAR gv_okcode_popup.

        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN OTHERS.
      cl_gui_cfw=>dispatch( ).
  ENDCASE.

  cl_gui_cfw=>flush( ).
ENDMODULE.                 " M_USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_SCRDATA_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_scrdata_0100 INPUT.
  IF gs_topopup-verme_scr GT gs_topopup-verme.
    gs_topopup-verme_scr = gs_topopup-verme.
  ENDIF.

  PERFORM f_data_ucomm_tocreate_set_qty.
ENDMODULE.                 " M_CHECK_SCRDATA_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_EXIT_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_exit_0100 INPUT.
  gv_okcode_bckp  = gv_okcode.

  CLEAR gv_okcode.

  CASE gv_okcode_bckp.
    WHEN c_ucomm_cancel.
      FREE gt_lqua[].

      CLEAR gs_topopup.
      CLEAR gv_okcode.
      CLEAR gv_okcode_bckp.

      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      cl_gui_cfw=>dispatch( ).
  ENDCASE.

  cl_gui_cfw=>flush( ).
ENDMODULE.                 " M_USER_COMMAND_EXIT_0100  INPUT
