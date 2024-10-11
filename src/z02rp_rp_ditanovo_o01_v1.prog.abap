*----------------------------------------------------------------------*
*   INCLUDE Z02RP_RP_REPDITA_O01                                       *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ST100'.
  SET TITLEBAR 'T01'.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = '01' AND Z02RPCONSPROD-AUFNR IS INITIAL.
      SCREEN-ACTIVE = '0'.
    ENDIF.
    IF SCREEN-GROUP1 = '02' AND *Z02RPCONSPROD-AUFNR IS INITIAL.
      SCREEN-ACTIVE = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " STATUS_0100  OUTPUT
