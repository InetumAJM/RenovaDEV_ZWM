*&---------------------------------------------------------------------*
*&  Include           ZWMREP0003I01                                    *
*&---------------------------------------------------------------------*

*&spwizard: input module for tc 'FILA'. do not change this line!
*&spwizard: process user command
MODULE fila_user_command INPUT.
  ok_code_0002 = sy-ucomm.
  PERFORM user_ok_tc USING    'FILA'
                              'IZWM003'
                              ' '
                     CHANGING ok_code_0002.
  sy-ucomm = ok_code_0002.
ENDMODULE.                    "FILA_user_command INPUT


*&spwizard: input module for tc 'PORT'. do not change this line!
*&spwizard: modify table
MODULE port_modify INPUT.
  MODIFY tab_portas1
    INDEX port-current_line.
ENDMODULE.                    "PORT_modify INPUT

*&spwizard: input modul for tc 'PORT'. do not change this line!
*&spwizard: mark table
MODULE port_mark INPUT.

  DATA: g_port_wa2 LIKE LINE OF tab_portas1.
  IF port-line_sel_mode = 1.
    LOOP AT tab_portas1 INTO g_port_wa2
      WHERE index = 'X'.
      g_port_wa2-index = ''.
      MODIFY tab_portas1
        FROM g_port_wa2
        TRANSPORTING index.
    ENDLOOP.
  ENDIF.

  MODIFY tab_portas1
    INDEX port-current_line
    TRANSPORTING index.
  MOVE port-current_line TO indice_porta.

ENDMODULE.                    "PORT_mark INPUT

*&spwizard: input module for tc 'PORT'. do not change this line!
*&spwizard: process user command
MODULE port_user_command INPUT.
  ok_code_0005 = sy-ucomm.
  PERFORM user_ok_tc USING    'PORT'
                              'TAB_PORTAS1'
                              'INDEX'
                     CHANGING ok_code_0005.
  sy-ucomm = ok_code_0005.
ENDMODULE.                    "PORT_user_command INPUT

*&spwizard: input modul for tc 'TCFILA'. do not change this line!
*&spwizard: mark table
MODULE tcfila_mark INPUT.
  DATA: g_tcfila_wa2 LIKE LINE OF fila_porta.
  IF tcfila-line_sel_mode = 1.
    LOOP AT fila_porta INTO g_tcfila_wa2
      WHERE index = 'X'.
      g_tcfila_wa2-index = ''.
      MODIFY fila_porta
        FROM g_tcfila_wa2
        TRANSPORTING index.
    ENDLOOP.
  ENDIF.

  MODIFY fila_porta
    INDEX tcfila-current_line
    TRANSPORTING index.

  MOVE tcfila-current_line TO indice_fila.

ENDMODULE.                    "TCFILA_mark INPUT

**&spwizard: input module for tc 'TCFILA'. do not change this line!
**&spwizard: process user command
*MODULE tcfila_user_command INPUT.
*  ok_code_0005 = sy-ucomm.
*  PERFORM user_ok_tc USING    'TCFILA'
*                              'FILA_PORTA'
*                              'INDEX'
*                     CHANGING ok_code_0005.
*  sy-ucomm = ok_code_0005.
*ENDMODULE.                    "TCFILA_user_command INPUT
*
**&spwizard: input module for tc 'CARGAA'. do not change this line!
**&spwizard: process user command
*MODULE cargaa_user_command INPUT.
*  ok_code_0001 = sy-ucomm.
*  PERFORM user_ok_tc USING    'CARGAA'
*                              'CARGA1'
*                              ' '
*                     CHANGING ok_code_0001.
*  sy-ucomm = ok_code_0001.
*ENDMODULE.                    "CARGAA_user_command INPUT
*
**&spwizard: input module for tc 'DESCARGA'. do not change this line!
**&spwizard: process user command
*MODULE descarga_user_command INPUT.
*  ok_code_0001 = sy-ucomm.
*  PERFORM user_ok_tc USING    'DESCARGA'
*                              'IZWM005'
*                              ' '
*                     CHANGING ok_code_0001.
*  sy-ucomm = ok_code_0001.
*ENDMODULE.                    "DESCARGA_user_command INPUT

**&---------------------------------------------------------------------
**
**&      Module  CHECK_EXISTE_DOC_CARGA  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE check_existe_doc_carga INPUT.
*
*  IF doc_carga IS INITIAL.
*    MOVE 'DOC_CARGA' TO cursorfield.
*    MESSAGE i126.
**   Não existem transportes selecionados
*
*  ENDIF.
*
*ENDMODULE.                 " CHECK_EXISTE_DOC_CARGA  INPUT

*&---------------------------------------------------------------------*
*&      Module  del_all  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE del_all INPUT.

  IF NOT g_del_all IS INITIAL.
    PERFORM inicializa.
    CLEAR g_del_all.
  ENDIF.

  IF transportador IS INITIAL.
    CLEAR nome_transp.
  ENDIF.

ENDMODULE.                 " del_all  INPUT

*&---------------------------------------------------------------------*
*&      Module  MOD_all  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE mod_all INPUT.

  IF NOT new_doc_carga IS INITIAL.
    doc_carga     = new_doc_carga.
  ENDIF.

  IF NOT new_matricula IS INITIAL.
    matricula     = new_matricula.
  ENDIF.

  IF NOT new_observacao IS INITIAL.
    observacao    = new_observacao.
  ENDIF.

  IF NOT new_tipo_camiao IS INITIAL.
    tipo_camiao   = new_tipo_camiao.
  ENDIF.

  IF NOT new_transportador IS INITIAL.
    transportador = new_transportador.
  ENDIF.

  IF NOT new_nome_transp IS INITIAL.
    nome_transp   = new_nome_transp.
  ENDIF.
  CLEAR : new_doc_carga, new_matricula, new_observacao, new_tipo_camiao,
          new_transportador, new_nome_transp.

ENDMODULE.                 " MOD_all  INPUT

*&---------------------------------------------------------------------*
*&      Module  teste  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE teste INPUT.
  describe table fila_porta lines sy-tfill.

  MESSAGE i000 WITH sy-tfill.
*   & & & &

ENDMODULE.                 " teste  INPUT
