*&---------------------------------------------------------------------*
*&  Include           Z10MM05I_V1                                      *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  LE_PICKING  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE le_picking INPUT.
  PERFORM lock_carga.
ENDMODULE.                             " LE_PICKING  INPUT

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CLEAR: vttk-daten, vttk-uaten, save_okcode, okcode, text, vbss-sammg.
  CASE sy-ucomm.
    WHEN 'BACK'.

      IF sy-dynnr = '0100'.

        CALL FUNCTION 'DEQUEUE_EVVBSK'
          EXPORTING
            sammg          = vbss-sammg
          EXCEPTIONS
            system_failure = 8.

        CLEAR vbss-sammg.
        SET SCREEN 0.
        LEAVE SCREEN.

      ELSE.
        LEAVE TO SCREEN 100.
      ENDIF.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " exit  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: lv_subrc TYPE sysubrc.

  CLEAR save_okcode.
  save_okcode = okcode.
  CLEAR okcode.

  CASE save_okcode.
    WHEN 'NEXT' OR 'ENTR'.
      CHECK NOT vbss-sammg IS INITIAL.

      PERFORM delevery_abast CHANGING lv_subrc.
      CHECK lv_subrc EQ 0.

      PERFORM le_picking.
      PERFORM proc_bonus.
      PERFORM save.

    WHEN 'BACK'.
      CALL FUNCTION 'DEQUEUE_EVVBSK'
        EXPORTING
          sammg          = vbss-sammg
        EXCEPTIONS
          system_failure = 8.

      CLEAR vbss-sammg.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " user_command_0100  INPUT
