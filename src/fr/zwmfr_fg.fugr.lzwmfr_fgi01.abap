*----------------------------------------------------------------------*
***INCLUDE LZWMFR_FGI01 .
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  MODULE m_user_command_exit_0010 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE m_user_command_exit_0010 INPUT.
  CASE gv_okcode.
    WHEN c_ucomm_back.
      PERFORM f_user_command_exit_0010.
    WHEN OTHERS.
      cl_gui_cfw=>dispatch( ).
  ENDCASE.

  CLEAR gv_okcode.

  cl_gui_cfw=>flush( ).
ENDMODULE.                    "m_user_command_exit_0010 INPUT
*----------------------------------------------------------------------*
*  MODULE m_user_command_0010 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE m_user_command_0010 INPUT.
  CASE gv_okcode.
    WHEN c_ucomm_next.
      PERFORM f_data_scr0010_ucomm_next.
    WHEN OTHERS.
      cl_gui_cfw=>dispatch( ).
  ENDCASE.

  CLEAR gv_okcode.

  cl_gui_cfw=>flush( ).
ENDMODULE.                    "m_user_command_0010 INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_LENUM_0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_lenum_0010 INPUT.
  PERFORM f_data_scr0010_check_lenum
    USING gs_ltak
          gv_equipment
          gs_zwmfrt005-lenum
          gv_process
    CHANGING gs_ltap
             gs_zwm011
             gs_scr001-lenum
             gv_okcode.
ENDMODULE.                 " M_CHECK_LENUM_0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_LGPLA_CONF_0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_lgpla_conf_0010 INPUT.
  PERFORM f_data_scr0010_check_lgpla_c
    USING gs_ltak
          gs_scr001-lgpla
          gv_process
    CHANGING gs_scr001-lgpla_conf
             gs_scr001-lgpla_pul1
             gv_okcode.

  PERFORM f_posicao_pulmao_skip USING gs_ltak-lgnum gs_scr001-lgpla_pul1
                              CHANGING gs_scr001-lgpla_pul2.
ENDMODULE.                 " M_CHECK_LGPLA_CONF_0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  M_CHECK_LGPLA_PUL2_0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_check_lgpla_pul2_0010 INPUT.
  PERFORM f_data_scr0010_check_pul
    USING gs_scr001-lgpla_conf
          gs_xuser-lgnum
          gv_process
          gs_scr001-lgpla_pul1
    CHANGING gs_scr001-lgpla_pul2
             gv_okcode.
ENDMODULE.                 " M_CHECK_LGPLA_PUL2_0010  INPUT
