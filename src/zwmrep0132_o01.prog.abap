*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0132_O01                                             *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  status_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'STATUS_GUI'.
  IF scr-cc_ordem IS INITIAL.
    SET CURSOR FIELD 'SCR-CC_ORDEM'.
  ELSEIF  scr-pernr IS INITIAL.
    SET CURSOR FIELD 'SCR-PERNR'.
  ELSEIF  scr-matnr IS INITIAL.
    SET CURSOR FIELD 'SCR-MATNR'.
  ELSEIF  scr-charg IS INITIAL.
    SET CURSOR FIELD 'SCR-CHARG'.
  ELSEIF  scr-menge IS INITIAL.
    SET CURSOR FIELD 'SCR-MENGE'.
  ENDIF.

ENDMODULE.                 " status_0001  OUTPUT
