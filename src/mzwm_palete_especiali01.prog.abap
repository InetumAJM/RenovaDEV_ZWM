************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: MZWM_PALETE_ESPECIALI01                                  *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Manutenção da Tabela de Paletização Especial (PAI)       *
* Criado por: Luís Rocha                                               *
* Criado em.: 06/12/2004                                               *
* Tipo PRG..: Include                                                  *
************************************************************************
*---------------------------------------------------------------------*
*       MODULE USER_COMMAND_0050 INPUT                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE user_command_0050 INPUT.

  w_ucomm = sy-ucomm.
  CLEAR sy-ucomm.
  CASE w_ucomm.
    WHEN 'CANC'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
*    WHEN 'MOD'.
*      PERFORM enqueue_table USING zwm_scr-lgnum.
*      CLEAR scroll_0100.
*      SET SCREEN 100.
*      LEAVE SCREEN.
    WHEN 'MODI'.
      IF zwm_scr-lgnum IS INITIAL OR
         zwm_scr-kunnr IS INITIAL.
        MESSAGE e000(zwmmsg001) WITH 'Preencher a UD e o Cliente'.
      ELSE.

        SELECT SINGLE * INTO wa_zwm049
        FROM zwm049 WHERE lgnum = zwm_scr-lgnum AND
                          kunnr = zwm_scr-kunnr.
        IF sy-subrc = 0.
          MOVE wa_zwm049-paltxt  TO scr200_paltxt.
          MOVE wa_zwm049-rf_show TO scr200_rfshow.
        ELSE.
          CLEAR: scr200_paltxt, scr200_rfshow.
        ENDIF.

        PERFORM enqueue_table USING zwm_scr-lgnum
                                    zwm_scr-kunnr.
        CLEAR scroll_0200.
        REFRESH CONTROL 'ctrl_tab' FROM SCREEN 200.
        SET SCREEN 200.
        LEAVE SCREEN.
      ENDIF.
  ENDCASE.

ENDMODULE.                    "user_command_0050 INPUT

*---------------------------------------------------------------------*
*       MODULE USER_COMMAND_0100 INPUT                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  w_ucomm = sy-ucomm.
  CLEAR sy-ucomm.
  CASE w_ucomm.
    WHEN 'CANC'.
      PERFORM ok_code_cancel USING scroll_0100-changes ' ' '0050'.
    WHEN 'EXIT'.
      PERFORM ok_code_exit   USING scroll_0100-changes ' ' '0050'.
    WHEN 'BACK'.
      PERFORM ok_code_back   USING scroll_0100-changes ' ' '0050'.
    WHEN 'REFR'.
      PERFORM refresh USING scroll_0100-changes
                            scroll_0100-initial.
    WHEN 'SAVE'.
      PERFORM altera_dados USING scroll_0100-changes
                                 scroll_0100-initial.
    WHEN 'DELE'.
      PERFORM apaga_entradas USING 'X'
                                   scroll_0100-initial.
    WHEN 'MKAL'.
      PERFORM marca_linhas.
    WHEN 'MKLO'.
      PERFORM desmarca_linhas.

    WHEN 'PRN'.
      PERFORM imprime_dados.
    WHEN 'P--'.
      PERFORM scrolling_in_work_table USING scroll_0100-firstline
                                            scroll_0100-start
                                            scroll_0100-maxlines
                                            'X'
                                            scroll_0100-loops
                                            'P--'
                                            'X'
                                   CHANGING scroll_0100-entries_sum
                                            scroll_0100-page_sum.
    WHEN 'P-'.
      PERFORM scrolling_in_work_table USING scroll_0100-firstline
                                            scroll_0100-start
                                            scroll_0100-maxlines
                                            'X'
                                            scroll_0100-loops
                                            'P-'
                                            'X'
                                   CHANGING scroll_0100-entries_sum
                                            scroll_0100-page_sum.
    WHEN 'P+'.
      PERFORM scrolling_in_work_table USING scroll_0100-firstline
                                            scroll_0100-start
                                            scroll_0100-maxlines
                                            ' '
                                            scroll_0100-loops
                                            'P+'
                                            'X'
                                   CHANGING scroll_0100-entries_sum
                                            scroll_0100-page_sum.
    WHEN 'P++'.
      PERFORM scrolling_in_work_table USING scroll_0100-firstline
                                            scroll_0100-start
                                            scroll_0100-maxlines
                                            ' '
                                            scroll_0100-loops
                                            'P++'
                                            'X'
                                   CHANGING scroll_0100-entries_sum
                                            scroll_0100-page_sum.
  ENDCASE.
ENDMODULE.                    "user_command_0100 INPUT

*---------------------------------------------------------------------*
*       MODULE USER_COMMAND_0200 INPUT                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  w_ucomm = sy-ucomm.
  CLEAR sy-ucomm.
  CASE w_ucomm.
*    WHEN 'MODI'.
*      CLEAR: scroll_0200-initial.
    WHEN 'CANC'.
      PERFORM ok_code_cancel USING scroll_0200-changes ' ' '0050'.
    WHEN 'EXIT'.
      PERFORM ok_code_exit   USING scroll_0200-changes ' ' '0050'.
    WHEN 'BACK'.
      PERFORM ok_code_back   USING scroll_0200-changes ' ' '0050'.
*    WHEN 'REFR'.
*      PERFORM refresh USING scroll_0200-changes
*                            scroll_0200-initial.
    WHEN 'SAVE'.
      PERFORM altera_dados USING scroll_0200-changes
                                 scroll_0200-initial.

      SELECT SINGLE * INTO wa_zwm049
      FROM zwm049 WHERE lgnum = zwm_scr-lgnum AND
                        kunnr = zwm_scr-kunnr.
      IF sy-subrc = 0.
        UPDATE zwm049 SET paltxt  = scr200_paltxt
                          rf_show = scr200_rfshow
                      WHERE lgnum = zwm_scr-lgnum AND
                            kunnr = zwm_scr-kunnr.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
      ELSE.
        zwm049-paltxt  = scr200_paltxt.
        zwm049-rf_show = scr200_rfshow.
        zwm049-lgnum = zwm_scr-lgnum.
        zwm049-kunnr = zwm_scr-kunnr.
        INSERT zwm049.
        IF sy-subrc = 0.
          COMMIT WORK.
        ENDIF.
      ENDIF.

    WHEN 'DELE'.
      PERFORM apaga_entradas USING 'X'
                                   scroll_0200-initial.
    WHEN 'MKAL'.
      PERFORM marca_linhas.
    WHEN 'MKLO'.
      PERFORM desmarca_linhas.
    WHEN 'POSI'.
      CALL SCREEN 250 STARTING AT  5  5
                        ENDING AT 40  8.
    WHEN 'PRN'.
      PERFORM imprime_dados.
*    when 'NEWL'.
*      perform novas_entradas.
    WHEN 'P--'.
      scroll_0200-firstline = ctrl_tab-top_line.
      PERFORM scrolling_in_work_table USING scroll_0200-firstline
                                            scroll_0200-start
                                            scroll_0200-maxlines
                                            'X'
                                            scroll_0200-loops
                                            'P--'
                                            'X'
                                   CHANGING scroll_0200-entries_sum
                                            scroll_0200-page_sum.
      ctrl_tab-top_line = scroll_0200-firstline.
    WHEN 'P-'.
      scroll_0200-firstline = ctrl_tab-top_line.
      PERFORM scrolling_in_work_table USING scroll_0200-firstline
                                            scroll_0200-start
                                            scroll_0200-maxlines
                                            'X'
                                            scroll_0200-loops
                                            'P-'
                                            'X'
                                   CHANGING scroll_0200-entries_sum
                                            scroll_0200-page_sum.
      ctrl_tab-top_line = scroll_0200-firstline.
    WHEN 'P+'.
      scroll_0200-firstline = ctrl_tab-top_line.
      PERFORM scrolling_in_work_table USING scroll_0200-firstline
                                            scroll_0200-start
                                            scroll_0200-maxlines
                                            ' '
                                            scroll_0200-loops
                                            'P+'
                                            'X'
                                   CHANGING scroll_0200-entries_sum
                                            scroll_0200-page_sum.
      ctrl_tab-top_line = scroll_0200-firstline.
    WHEN 'P++'.
      scroll_0200-firstline = ctrl_tab-top_line.
      PERFORM scrolling_in_work_table USING scroll_0200-firstline
                                            scroll_0200-start
                                            scroll_0200-maxlines
                                            ' '
                                            scroll_0200-loops
                                            'P++'
                                            'X'
                                   CHANGING scroll_0200-entries_sum
                                            scroll_0200-page_sum.
      ctrl_tab-top_line = scroll_0200-firstline.
  ENDCASE.

ENDMODULE.                    "user_command_0200 INPUT

*---------------------------------------------------------------------*
*       MODULE USER_COMMAND_0250 INPUT                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE user_command_0250 INPUT.
  w_ucomm = sy-ucomm.
  CLEAR sy-ucomm.
  CASE w_ucomm.
    WHEN 'CANC'.
      CLEAR *zwm031-matnr.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CONF'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.                    "user_command_0250 INPUT

*---------------------------------------------------------------------*
*       MODULE EXIT_0050                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE exit_0050.
  w_ucomm = sy-ucomm.
  CLEAR sy-ucomm.
  CASE w_ucomm.
    WHEN 'CANC'.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'BACK'.
      SET SCREEN 0. LEAVE SCREEN.
  ENDCASE.
ENDMODULE.                                                  "exit_0050

*---------------------------------------------------------------------*
*       MODULE EXIT_0100                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE exit_0100.
  w_ucomm = sy-ucomm.
  CLEAR sy-ucomm.
  CASE w_ucomm.
    WHEN 'CANC'.
      PERFORM ok_code_cancel USING scroll_0100-changes ' ' '0050'.
    WHEN 'EXIT'.
      PERFORM ok_code_exit   USING scroll_0100-changes ' ' '0050'.
    WHEN 'BACK'.
      PERFORM ok_code_back   USING scroll_0100-changes ' ' '0050'.
  ENDCASE.
ENDMODULE.                                                  "exit_0100

*---------------------------------------------------------------------*
*       MODULE EXIT_0200                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE exit_0200.
  w_ucomm = sy-ucomm.

  CLEAR sy-ucomm.
  CASE w_ucomm.
    WHEN 'CANC'.
      PERFORM ok_code_cancel USING scroll_0200-changes ' ' '0050'.
    WHEN 'EXIT'.
      PERFORM ok_code_exit   USING scroll_0200-changes ' ' '0050'.
    WHEN 'BACK'.
      PERFORM ok_code_back   USING scroll_0200-changes ' ' '0050'.
  ENDCASE.
ENDMODULE.                                                  "exit_0200

*---------------------------------------------------------------------*
*       MODULE VERIFY_0050                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE verify_0050.

* Valida se o depósito existe
  SELECT SINGLE * FROM t300
                    WHERE lgnum = zwm_scr-lgnum.
  IF sy-subrc <> 0.
*   ERRO: Nº Depósito Inválido !
    MESSAGE e146(zwmmsg001).
  ENDIF.

  PERFORM ler_t300t USING zwm_scr-lgnum.

ENDMODULE.                    "verify_0050

*---------------------------------------------------------------------*
*       MODULE MODIFY_TAB_0100                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE modify_tab_0100.
  CHECK NOT ( zwm031-matnr IS INITIAL ).
  MOVE-CORRESPONDING zwm031 TO w_key.
  READ TABLE w_tab_0100 WITH KEY w_key BINARY SEARCH.
  w_indice = sy-tabix.
  CASE sy-subrc.
    WHEN 0.
      PERFORM modifica_registo_0100.
      MODIFY w_tab_0100 INDEX w_indice.
    WHEN 4.
      PERFORM novo_registo_0100.
      INSERT w_tab_0100 INDEX w_indice.
    WHEN 8.
      PERFORM novo_registo_0100.
      APPEND w_tab_0100.
  ENDCASE.
ENDMODULE.                    "modify_tab_0100

*---------------------------------------------------------------------*
*       MODULE MODIFY_TAB_0200                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE modify_tab_0200.

  CHECK NOT zwm031-matnr IS INITIAL.

  MOVE-CORRESPONDING zwm031 TO w_key.

  SORT w_tab_0200.

  READ TABLE w_tab_0200 WITH KEY kunnr = w_key-kunnr
                                 matnr = w_key-matnr
                                 BINARY SEARCH.
  w_indice = sy-tabix.

  CASE sy-subrc.
    WHEN 0.
      CHECK w_tab_0200-flg_exist NE 'X'.
      PERFORM modifica_registo_0200.
      MODIFY w_tab_0200 INDEX w_indice.
    WHEN 4.

      READ TABLE w_tab_0200 INDEX ctrl_tab-current_line.
      IF sy-subrc EQ 0.
        IF w_key-matnr NE w_tab_0200-matnr.
          CHECK w_tab_0200-flg_exist NE 'X'.
          PERFORM modifica_registo_0200.
          MODIFY w_tab_0200 INDEX ctrl_tab-current_line.
        ENDIF.
      ELSE.
        PERFORM novo_registo_0200.
        INSERT w_tab_0200 INDEX w_indice.
      ENDIF.
    WHEN 8.
      PERFORM novo_registo_0200.
      APPEND w_tab_0200.
  ENDCASE.

ENDMODULE.                    "modify_tab_0200

*&---------------------------------------------------------------------*
*&      Module  verify_0050_kunnr  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verify_0050_kunnr INPUT.

  PERFORM check_kunnr.

ENDMODULE.                    "verify_0050_kunnr INPUT
" verify_0050_kunnr  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_matnr  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr INPUT.

  DATA: l_index LIKE sy-tabix.

  SORT w_tab_0200.

  READ TABLE w_tab_0200 WITH KEY kunnr = zwm031-kunnr
                                 matnr = zwm031-matnr
                                 BINARY SEARCH.

  l_index = sy-tabix.

  IF sy-subrc EQ 0.

    READ TABLE w_tab_0200 INDEX l_index.
    IF sy-subrc EQ 0.
      MESSAGE w000(zwmmsg001) WITH 'O material' w_tab_0200-matnr
                                   'já existe'.
    ELSE.
      MESSAGE w000(zwmmsg001) WITH 'O material' w_key-matnr
                                   'já existe'.
    ENDIF.
    w_tab_0200-flg_exist = 'X'.
    MODIFY w_tab_0200 INDEX sy-tabix.
  ENDIF.

ENDMODULE.                 " check_matnr  INPUT
