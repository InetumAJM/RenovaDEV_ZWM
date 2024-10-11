************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: MZWM_PALETE_ESPECIALO01                                  *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Manutenção da Tabela de Paletização Especial (PBO)       *
* Criado por: Luís Rocha                                               *
* Criado em.: 06/12/2004                                               *
* Tipo PRG..: Include                                                  *
************************************************************************

*---------------------------------------------------------------------*
*       MODULE set_pf_status_0050 OUTPUT                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE set_pf_status_0050 OUTPUT.

  SET PF-STATUS '0050'.
  SET TITLEBAR '001'.

ENDMODULE.                    "set_pf_status_0050 OUTPUT

*---------------------------------------------------------------------*
*  MODULE init_values_0050 OUTPUT
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
MODULE init_values_0050 OUTPUT.

  IF NOT ( w_zwm031_lock IS INITIAL ).
    PERFORM dequeue_table USING zwm_scr-lgnum
                                zwm_scr-kunnr.
    CLEAR w_zwm031_lock.
  ENDIF.

  CHECK zwm_scr-lgnum IS INITIAL.

  DATA: xuser LIKE lrf_wkqu OCCURS 0 WITH HEADER LINE.
* Read the user data from table lrf_wkqu ( for all the warehouses)
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = xuser
    EXCEPTIONS
      no_entry_found = 01.
  IF sy-subrc = 0.
    READ TABLE xuser INDEX 1.
    MOVE xuser-lgnum TO zwm_scr-lgnum.
  ENDIF.

  PERFORM ler_t300t USING zwm_scr-lgnum.

ENDMODULE.                    "init_values OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_PF_STATUS_0100 OUTPUT                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE set_pf_status_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR '002'.

ENDMODULE.                    "set_pf_status_0100 OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_PF_STATUS_0200 OUTPUT                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE set_pf_status_0200 OUTPUT.

  SET PF-STATUS '0200'.
  SET TITLEBAR '002'.

ENDMODULE.                    "set_pf_status_0200 OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_PF_STATUS_0250 OUTPUT                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE set_pf_status_0250 OUTPUT.

  SET PF-STATUS '0250'.
  SET TITLEBAR '003'.

ENDMODULE.                    "set_pf_status_0250 OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_TAB_0100 OUTPUT                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE set_tab_0100 OUTPUT.

  IF scroll_0100-initial IS INITIAL.
    REFRESH: w_tab_0100.
    PERFORM ler_tabelas_0100.
    scroll_0100-firstline = 1.
    scroll_0100-start = 1.
    scroll_0100-initial = k_um.
    CLEAR scroll_0100-changes.
    scroll_0100-entries_sum = scroll_0100-maxlines.
  ENDIF.

  CLEAR w_tab_0100.
  IF NOT ( *zwm031-kunnr IS INITIAL ) OR
     NOT ( *zwm031-matnr IS INITIAL ).
    LOOP AT w_tab_0100 WHERE kunnr GE *zwm031-kunnr
                         AND matnr GE *zwm031-matnr.
*      ctrl_tab-top_line = sy-tabix.
      scroll_0100-firstline = sy-tabix.
      EXIT.
    ENDLOOP.
    CLEAR: *zwm031-kunnr, *zwm031-matnr.
  ENDIF.

* as linhas seguintes servem para controlo em MODIFY_SCREEN
  ctrl_maxlines  = scroll_0100-maxlines  + 1.
  ctrl_firstline = scroll_0100-firstline - 1.

ENDMODULE.                    "set_tab_0100 OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_TAB_0200 OUTPUT                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE set_tab_0200 OUTPUT.

*  break roffd.

  IF scroll_0200-initial IS INITIAL.
    FREE: w_tab_0200.

    PERFORM ler_tabelas_0200.

    scroll_0200-firstline = 0.
    scroll_0200-start = 1.
    scroll_0200-initial = k_um.

    CLEAR scroll_0200-changes.

    scroll_0200-entries_sum = scroll_0200-maxlines.
    ctrl_tab-lines = scroll_0200-entries_sum.
  ENDIF.

  CLEAR w_tab_0200.
  IF NOT ( *zwm031-kunnr IS INITIAL ) OR
     NOT ( *zwm031-matnr IS INITIAL ).
    LOOP AT w_tab_0200 WHERE kunnr GE *zwm031-kunnr
                         AND matnr GE *zwm031-matnr.
      ctrl_tab-top_line = sy-tabix.
      EXIT.
    ENDLOOP.
    CLEAR: *zwm031-kunnr, *zwm031-matnr.
  ENDIF.

*
*    select single * into wa_ZWM049
*    from ZWM049 where lgnum = ZWM_SCR-LGNUM and
*                      kunnr = ZWM_SCR-KUNNR.
*    if sy-subrc = 0.
*        move wa_ZWM049-PALTXT to SCR200_PALTXT.
*    else.
*      clear: scr200_paltxt.
*    endif.


ENDMODULE.                    "set_tab_0200 OUTPUT

*---------------------------------------------------------------------*
*       MODULE MODIFY_SCREEN_0100 OUTPUT                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE modify_screen_0100 OUTPUT.

  scroll_0100-loops = sy-loopc.
  ctrl_line = ctrl_firstline + sy-stepl.
  READ TABLE w_tab_0100 INDEX ctrl_line.
  IF sy-subrc EQ 0.
    IF NOT ( w_tab_0100-kunnr IS INITIAL ).
      MOVE-CORRESPONDING w_tab_0100 TO zwm031.
      MOVE w_tab_0100-name1 TO kna1-name1.
      MOVE w_tab_0100-maktx TO makt-maktx.
      MOVE w_tab_0100-flg_del TO w_flag_del.
    ENDIF.
  ENDIF.

  CHECK ctrl_line GT ctrl_maxlines.

  LOOP AT SCREEN.
    screen-input = k_zero.
    screen-invisible = k_um.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                    "modify_screen_0100 OUTPUT

*---------------------------------------------------------------------*
*       MODULE MODIFY_SCREEN_0200 OUTPUT                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE modify_screen_0200 OUTPUT.

  scroll_0200-loops = sy-loopc.
  ctrl_line = ctrl_tab-current_line.

  READ TABLE w_tab_0200 INDEX ctrl_line.

  IF sy-subrc EQ 0.
*    IF NOT ( w_tab_0200-kunnr IS INITIAL ).
    MOVE-CORRESPONDING w_tab_0200 TO zwm031.
*    MOVE w_tab_0200-name1 TO kna1-name1.
    MOVE w_tab_0200-maktx TO makt-maktx.
    MOVE w_tab_0200-flg_del TO w_flag_del.
*    ENDIF.
  ENDIF.

ENDMODULE.                    "modify_screen_0200 OUTPUT

*---------------------------------------------------------------------*
*       MODULE CURSOR_POSITION_0100 OUTPUT                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE cursor_position_0100 OUTPUT.

  IF NOT ( scroll_0100-curow IS INITIAL ).
    SET CURSOR FIELD 'ZWM031-MATNR' LINE scroll_0100-curow.
  ENDIF.

ENDMODULE.                             " POSICAO_CURSOR  OUTPUT

*---------------------------------------------------------------------*
*       MODULE CURSOR_POSITION_0200 OUTPUT                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE cursor_position_0200 OUTPUT.

  IF NOT ( scroll_0200-curow IS INITIAL ).
    SET CURSOR FIELD 'ZWM031-MATNR' LINE scroll_0200-curow.
  ENDIF.

ENDMODULE.                             " POSICAO_CURSOR  OUTPUT

*
