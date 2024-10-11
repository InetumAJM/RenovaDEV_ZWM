*&---------------------------------------------------------------------*
*&  Include           ZWMREP003O01                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'ZWMGESTAO'.
  SET TITLEBAR 'GERAL'.
  CLEAR gv_scr0004_mode.

*dados para ALV ESTADO PORTAS
  PERFORM select_data.
  SORT tab_f BY porta ASCENDING.
  PERFORM tab_final.
  IF tree IS INITIAL.
    PERFORM init_tree.
  ENDIF.

  CALL METHOD tree->refresh_table_display.
  CALL METHOD tree->expand_tree
    EXPORTING
      i_level = 1.


*dados para Tabela de Descargas
  PERFORM select_descargas.
*dados para Table Cargas
  PERFORM select_cargas.
**  PERFORM SELECT_CARGAS_1.

*-------------------------------------------------------INS Mai2005
  IF NOT carga1[] IS INITIAL.

    SORT carga1 BY porta ASCENDING.

    IF treec IS INITIAL.

      PERFORM init_tree_cargas.
*-----------------------------------------------------Ins Mai2005 - 23

***      CALL METHOD treec->refresh_table_display.
*---------------------------------------------------------------------
    ENDIF.
*-----------------------------------------------------inserir nós
    IF flag_tree IS INITIAL.
      PERFORM cria_hierarquia_c.
      flag_tree = 'X'.
    ENDIF.

    CALL METHOD cl_gui_cfw=>flush.

  ELSE.
    IF treec IS INITIAL.

      PERFORM init_tree_cargas.

    ENDIF.

    PERFORM no_de_topo USING 'C'.
    CALL METHOD cl_gui_cfw=>flush.

  ENDIF.

*------------------------------------------------------------------
  IF NOT izwm005[] IS INITIAL.

    SORT izwm005 BY porta ASCENDING.

    IF treed IS INITIAL.

      PERFORM init_tree_descargas.

***      CALL METHOD treed->refresh_table_display.

    ENDIF.

    IF flag_tree_d IS INITIAL.
      PERFORM cria_hierarquia_d.
      flag_tree_d = 'X'.
    ENDIF.

  ELSE.
    IF treed IS INITIAL.

      PERFORM init_tree_descargas.

***      CALL METHOD treed->refresh_table_display.

    ENDIF.

    PERFORM no_de_topo USING 'D'.
    CALL METHOD cl_gui_cfw=>flush.


  ENDIF.
*------------------------------------------------------------------
  CLEAR : ok_code_0001.
ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0002 OUTPUT.
  SET PF-STATUS 'ZWMFILA'.
  SET TITLEBAR 'LISTA'.

ENDMODULE.                 " STATUS_0002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0003 OUTPUT.

  SET PF-STATUS 'ZWMDESCARGA1'.
  SET TITLEBAR 'DESCARGA'.
  SET CURSOR FIELD cursorfield.

** Limpar todos os campos do ecrã de inserção
** de mais ordens de compra

  CLEAR : doc_compra1,
          doc_compra2,
          doc_compra3,
          doc_compra4,
          doc_compra5,
          doc_compra6,
          doc_compra7,
          doc_compra8,
          doc_compra9,
          doc_compra10.

ENDMODULE.                 " STATUS_0003  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0004 OUTPUT.

  SET PF-STATUS 'ZWMDESCARGA1'.
  SET TITLEBAR 'CARGA'.
  SET CURSOR FIELD cursorfield.

  CLEAR: lfa1, vttk, flag_tree, flag_treec, flag_tree_d.

  IF gv_scr0004_mode EQ gc_scr0004-modificar.
    LOOP AT SCREEN.
      IF screen-name EQ 'DOC_CARGA'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_0004  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0005 OUTPUT.

  SET PF-STATUS 'ZWMPORTA'.
  SET TITLEBAR 'PORTA'.

  PERFORM fill_tables.

  CLEAR: indice_fila,indice_porta, flag_tree, flag_treec, flag_tree_d.

ENDMODULE.                 " STATUS_0005  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0009  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0009 OUTPUT.

  SET PF-STATUS 'ZWMDESCARGA1'.
  SET TITLEBAR 'Z01'.

  SET CURSOR FIELD cursorfield.

ENDMODULE.                 " STATUS_0009  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0010 OUTPUT.

  SET PF-STATUS 'ZWMDESCARGA1'.
  SET TITLEBAR 'Z02'.

ENDMODULE.                 " STATUS_0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  teste  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE teste OUTPUT.

  DESCRIBE TABLE fila_porta LINES sy-tfill.
  MESSAGE i000 WITH sy-tfill.

ENDMODULE.                 " teste  OUTPUT
