*&---------------------------------------------------------------------*
*&  Include           ZWMREP0003O01                                    *
*&---------------------------------------------------------------------*

*&spwizard: output module for tc 'FILA'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
module FILA_change_tc_attr output.
  describe table IZWM003 lines FILA-lines.
endmodule.

*&spwizard: output module for tc 'FILA'. do not change this line!
*&spwizard: get lines of tablecontrol
module FILA_get_lines output.
  g_FILA_lines = sy-loopc.
endmodule.



*&spwizard: output module for tc 'PORT'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
module PORT_change_tc_attr output.
  describe table TAB_PORTAS1 lines PORT-lines.
endmodule.

*&spwizard: output module for tc 'PORT'. do not change this line!
*&spwizard: get lines of tablecontrol
module PORT_get_lines output.
  g_PORT_lines = sy-loopc.
endmodule.

*&spwizard: output module for tc 'TCFILA'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
module TCFILA_change_tc_attr output.
*  describe table FILA_PORTA lines TCFILA-lines.
  describe table FILA_PORTA lines TCFILA1-lines.

endmodule.

*&spwizard: output module for tc 'TCFILA'. do not change this line!
*&spwizard: get lines of tablecontrol
module TCFILA_get_lines output.
  g_TCFILA_lines = sy-loopc.
endmodule.

*&spwizard: output module for tc 'CARGAA'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
module CARGAA_change_tc_attr output.
  describe table CARGA1 lines CARGAA-lines.
endmodule.

*&spwizard: output module for tc 'CARGAA'. do not change this line!
*&spwizard: get lines of tablecontrol
module CARGAA_get_lines output.
  g_CARGAA_lines = sy-loopc.
endmodule.

*&spwizard: output module for tc 'DESCARGA'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
module DESCARGA_change_tc_attr output.
  describe table IZWM005 lines DESCARGA-lines.
endmodule.

*&spwizard: output module for tc 'DESCARGA'. do not change this line!
*&spwizard: get lines of tablecontrol
module DESCARGA_get_lines output.
  g_DESCARGA_lines = sy-loopc.
endmodule.
