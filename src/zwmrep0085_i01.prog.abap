*&---------------------------------------------------------------------*
*&  Include           ZWMREP0085_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHK_LENUM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE chk_exidv INPUT.
  PERFORM chk_exidv.
ENDMODULE.                 " CHK_LENUM  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.
  CASE scr0001-ok_code.

    WHEN 'CLR'.
      PERFORM clear_all.

      LEAVE TO SCREEN setscreen1.

    WHEN 'BACK'.

    WHEN 'NEXT'.
      PERFORM trf_forn.
    WHEN 'SAVE'.


    WHEN 'PU'.
      IF scr0001-actual < scr0001-total.
        ADD 1 TO scr0001-actual.
        PERFORM list_sscc.
      ENDIF.
    WHEN 'PD'.
      IF scr0001-actual > 1 .
        SUBTRACT 1 FROM scr0001-actual.
        PERFORM list_sscc.
      ENDIF.
    WHEN OTHERS.

  ENDCASE.

  CLEAR scr0001-ok_code.
ENDMODULE.                 " USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_COMMAND_0001 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
*      PERFORM clear_all .
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR sy-ucomm.
ENDMODULE.                 " EXIT_COMMAND_0001  INPUT
