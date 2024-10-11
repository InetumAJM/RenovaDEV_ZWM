*----------------------------------------------------------------------*
***INCLUDE LZWMFR_FGO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  M_STATUS_0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE m_status_0010 OUTPUT.
  SET PF-STATUS c_pfstatus_scr001.
ENDMODULE.                 " M_STATUS_0010  OUTPUT
*----------------------------------------------------------------------*
*  MODULE m_set_fields_0010 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE m_set_fields_0010 OUTPUT.
  PERFORM f_set_fields_0010.

  " set cursor
  IF gs_scr001-lenum IS INITIAL.
    gv_cursor = c_scr001_lenum.
  ELSE.
    IF gs_scr001-lgpla_conf IS INITIAL.
      gv_cursor = c_scr001_lgpla_conf.
    ELSE.
      IF gv_process EQ c_param_queue_aut_sd_pul.
        gv_cursor = c_scr001_lgpla_pul2.
      ENDIF.
    ENDIF.
  ENDIF.

  SET CURSOR FIELD gv_cursor.

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'PAL'.
        IF gv_process EQ c_param_queue_aut_sd_pul OR
           gv_process EQ c_param_queue_aut_sd_dck.
          screen-invisible = 0.
        ELSE.
          screen-invisible = 1.
        ENDIF.
      WHEN 'PUL'.
        IF gv_process EQ c_param_queue_aut_sd_pul.
          screen-invisible = 0.
        ELSE.
          screen-invisible = 1.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    CASE screen-name.
      WHEN c_scr001_lenum.
        IF gv_cursor NE c_scr001_lenum.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN c_scr001_lgpla_conf.
        IF gv_cursor NE c_scr001_lgpla_conf.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN c_scr001_lgpla_pul2.
        IF gv_cursor NE c_scr001_lgpla_pul2.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " M_SET_FIELDS_0010  OUTPUT
