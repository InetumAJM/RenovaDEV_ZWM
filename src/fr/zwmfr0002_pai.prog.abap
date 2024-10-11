*----------------------------------------------------------------------*
* Include: ZWMFR0002_PAI
*----------------------------------------------------------------------*
* Description: RF - Entrada de Produto Acabado/Bobines PT
* RICEFW: WM.02/WM.03
*----------------------------------------------------------------------*
* Author........: [Pedro Silva] [ROFFD] [ROFF(SDF)]
*                 [Tiago Pateiro] [ROFFD] [ROFF(SDF)]
* Creation date:  2015-10-26
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_EXIT_0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_exit_0010 INPUT.
  CASE gv_okcode.
    WHEN c_ucomm_back.
      PERFORM f_data_free.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      cl_gui_cfw=>dispatch( ).
  ENDCASE.

  CLEAR gv_okcode.

  cl_gui_cfw=>flush( ).
ENDMODULE.                 " M_USER_COMMAND_EXIT_0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_0010 INPUT.
  CASE gv_okcode.
    WHEN c_ucomm_clear.
      PERFORM f_data_scr0010_ucomm_clear.
    WHEN c_ucomm_next.
      IF gs_scr001 IS NOT INITIAL AND gv_cursor EQ c_cursor_pnext.
        PERFORM f_data_scr0010_ucomm_next.
      ENDIF.
    WHEN OTHERS.
      cl_gui_cfw=>dispatch( ).
  ENDCASE.

  CLEAR gv_okcode.

  cl_gui_cfw=>flush( ).
ENDMODULE.                 " M_USER_COMMAND_0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_EBELN_0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_ebeln_0010 INPUT.
  PERFORM f_data_scr0010_check_ebeln CHANGING gv_okcode gv_cursor.
ENDMODULE.                 " M_CHECK_EBELN_0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_XBLNR_0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_xblnr_0010 INPUT.
  PERFORM f_data_scr0010_check_xblnr CHANGING gv_okcode gv_cursor.
ENDMODULE.                 " M_CHECK_XBLNR_0010  INPUT
*----------------------------------------------------------------------*
*  MODULE m_check_licha_0010 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE m_check_licha_0010 INPUT.
  PERFORM f_data_scr0010_check_licha CHANGING gv_okcode gv_cursor.
ENDMODULE.                    "m_check_licha_0010 INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_NPALL_0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_npall_0010 INPUT.
  PERFORM f_data_scr0010_check_npall CHANGING gv_okcode gv_cursor.
ENDMODULE.                 " M_CHECK_NPALL_0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_EXIT_0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_exit_0020 INPUT.
  CASE gv_okcode.
    WHEN c_ucomm_back.
      FREE gt_scr002[].
      FREE gt_scr002_check[].
      FREE gt_zwmfrt001[].

      CLEAR gs_scr002.

      PERFORM f_data_scr0010_ucomm_clear.

      LEAVE TO SCREEN gv_dynnr_1.
    WHEN OTHERS.
      cl_gui_cfw=>dispatch( ).
  ENDCASE.

  CLEAR gv_okcode.

  cl_gui_cfw=>flush( ).
ENDMODULE.                 " M_USER_COMMAND_EXIT_0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_0020 INPUT.
  CASE gv_okcode.
    WHEN c_ucomm_clear.
      PERFORM f_data_scr0020_ucomm_clear USING c_cursor_lenum.
    WHEN c_ucomm_next.
      IF gv_cursor EQ c_cursor_pnext AND gs_scr002 IS NOT INITIAL.
        PERFORM f_data_scr0020_ucomm_next.
      ENDIF.
    WHEN c_ucomm_save.
      PERFORM f_data_scr0020_ucomm_save.

      FREE gt_scr002[].
      FREE gt_scr002_check[].
      FREE gt_zwmfrt001[].

      CLEAR gs_scr002.

      PERFORM f_data_scr0010_ucomm_clear.

      LEAVE TO SCREEN gv_dynnr_1.
    WHEN c_ucomm_pdown.
      PERFORM f_data_scr0020_ucomm_pdown.
    WHEN c_ucomm_pup.
      PERFORM f_data_scr0020_ucomm_pup.
    WHEN OTHERS.
      cl_gui_cfw=>dispatch( ).
  ENDCASE.

  CLEAR gv_okcode.

  cl_gui_cfw=>flush( ).
ENDMODULE.                 " M_USER_COMMAND_0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_LENUM_0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_lenum_0020 INPUT.
  PERFORM f_data_scr0020_check_lenum USING abap_false CHANGING gv_okcode.
ENDMODULE.                 " M_CHECK_LENUM_0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_EXIT_0030  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_exit_0030 INPUT.
  CASE gv_okcode.
    WHEN c_ucomm_back.
      FREE gt_scr002[].
      FREE gt_scr002_check[].
      FREE gt_zwmfrt001[].

      CLEAR gs_scr002.

      PERFORM f_data_scr0010_ucomm_clear.

      LEAVE TO SCREEN gv_dynnr_1.
    WHEN OTHERS.
      cl_gui_cfw=>dispatch( ).
  ENDCASE.

  CLEAR gv_okcode.

  cl_gui_cfw=>flush( ).
ENDMODULE.                 " M_USER_COMMAND_EXIT_0030  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_USER_COMMAND_0030  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_user_command_0030 INPUT.
  CASE gv_okcode.
    WHEN c_ucomm_clear.
      PERFORM f_data_scr0020_ucomm_clear USING c_cursor_zeugn.
    WHEN c_ucomm_next.
      IF gv_cursor EQ c_cursor_pnext.
        IF gs_scr002-zeugn IS NOT INITIAL AND gs_scr002-matnr IS NOT INITIAL AND
           gs_scr002-charg IS NOT INITIAL AND gs_scr002-vsolm IS NOT INITIAL.
          PERFORM f_data_scr0020_ucomm_next.
        ENDIF.
      ENDIF.
    WHEN c_ucomm_save.
      PERFORM f_data_scr0020_ucomm_save.

      FREE gt_scr002[].
      FREE gt_scr002_check[].
      FREE gt_zwmfrt001[].

      CLEAR gs_scr002.

      PERFORM f_data_scr0010_ucomm_clear.

      LEAVE TO SCREEN gv_dynnr_1.
    WHEN c_ucomm_pdown.
      PERFORM f_data_scr0020_ucomm_pdown.
    WHEN c_ucomm_pup.
      PERFORM f_data_scr0020_ucomm_pup.
    WHEN OTHERS.
      cl_gui_cfw=>dispatch( ).
  ENDCASE.

  CLEAR gv_okcode.

  cl_gui_cfw=>flush( ).
ENDMODULE.                 " M_USER_COMMAND_0030  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_ZEUGN_0030  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_zeugn_0030 INPUT.
  PERFORM f_data_scr0030_check_zeugn CHANGING gv_okcode gv_cursor.
ENDMODULE.                 " M_CHECK_ZEUGN_0030  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_MATNR_0030  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_matnr_0030 INPUT.
  PERFORM f_data_scr0030_check_matnr CHANGING gv_okcode gv_cursor.
ENDMODULE.                 " M_CHECK_MATNR_0030  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_CHARG_0030  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_charg_0030 INPUT.
  PERFORM f_data_scr0030_check_charg CHANGING gv_okcode gv_cursor.
ENDMODULE.                 " M_CHECK_CHARG_0030  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_VSOLM_0030  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_vsolm_0030 INPUT.
  PERFORM f_data_scr0030_check_vsolm CHANGING gv_okcode gv_cursor.
ENDMODULE.                 " M_CHECK_VSOLM_0030  INPUT
