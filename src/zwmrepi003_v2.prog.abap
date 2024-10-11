*&---------------------------------------------------------------------*
*&  Include           ZWMREPI003_V2                                    *
*&---------------------------------------------------------------------*

*&spwizard: input modul for tc 'TCFILA1'. do not change this line!
*&spwizard: mark table
MODULE tcfila1_mark INPUT.
  DATA: g_tcfila1_wa2 LIKE LINE OF fila_porta.
  IF tcfila1-line_sel_mode = 1.
    LOOP AT fila_porta INTO g_tcfila1_wa2
      WHERE index = 'X'.
      g_tcfila1_wa2-index = ''.
      MODIFY fila_porta
        FROM g_tcfila1_wa2
        TRANSPORTING index.
    ENDLOOP.
  ENDIF.

  MODIFY fila_porta
    INDEX tcfila1-current_line
    TRANSPORTING index.

  MOVE tcfila1-current_line TO indice_fila.

ENDMODULE.                    "TCFILA1_mark INPUT
