*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0066_O01                                             *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  status_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'STATUS_GUI'.
*  SET CURSOR FIELD 'PDT_ORIGEM'.

    LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'PDT_ORIGEM'.
        IF pdt_origem(3) = 'PKL'.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN 'PDT_EAN11'.
        IF pdt_origem(3) = 'PKL' and PDT_EAN11 IS INITIAL.
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " status_0001  OUTPUT
