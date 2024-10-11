*&---------------------------------------------------------------------*
*&  Include           ZWMREP0134_I02
*&---------------------------------------------------------------------*

*&SPWIZARD: INPUT MODULE FOR TC 'ZTC0001'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE ztc0001_modify INPUT.
  PERFORM get_line_info CHANGING gs_data.

  MODIFY gt_data
    FROM gs_data
    INDEX ztc0001-current_line.

  PERFORM save_line USING ztc0001-current_line
                    CHANGING gs_data.
ENDMODULE.                    "ZTC0001_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'ZTC0001'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE ztc0001_mark INPUT.
  DATA: g_ztc0001_wa2 LIKE LINE OF gt_data.
  IF ztc0001-line_sel_mode = 1
  AND gs_data-check = 'X'.
    LOOP AT gt_data INTO g_ztc0001_wa2
      WHERE check = 'X'.
      g_ztc0001_wa2-check = ''.
      MODIFY gt_data
        FROM g_ztc0001_wa2
        TRANSPORTING check.
    ENDLOOP.
  ENDIF.
  MODIFY gt_data
    FROM gs_data
    INDEX ztc0001-current_line
    TRANSPORTING check.
ENDMODULE.                    "ZTC0001_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'ZTC0001'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE ztc0001_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'ZTC0001'
                              'GT_DATA'
                              'CHECK'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "ZTC0001_USER_COMMAND INPUT
