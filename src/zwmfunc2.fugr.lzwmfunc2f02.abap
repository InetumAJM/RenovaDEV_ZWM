*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC2F02 .
*----------------------------------------------------------------------*

*{   INSERT         DEVK907424                                        1
*&---------------------------------------------------------------------*
*&      Form  STATUS_0005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0      text
*----------------------------------------------------------------------*
FORM status_0005 USING uv_lv_page TYPE i.

  STATICS: lsv_page TYPE i VALUE 1.

  TYPES: BEGIN OF lty_botoes_activos,
           nome(132) TYPE c,
           spacer    TYPE flag,
         END OF lty_botoes_activos.

  DATA: lt_botoes_activos TYPE TABLE OF lty_botoes_activos.

  DATA: lv_name_sec(2) TYPE c,
        lv_index       TYPE i,
        lv_index_table TYPE i,
        lv_nome(30)    TYPE c,
        lv_c_index(2)  TYPE c,
        lv_tot_bot     TYPE i,
        lv_index_txt   TYPE c,
        lv_tot_pag_d   TYPE p DECIMALS 1,
        lv_tot_pag_i   TYPE i,
        lv_subrc       TYPE sysubrc,
        lref_text      TYPE REF TO data.

  DATA: ls_0005_item      TYPE zrf01_option_select_items,
        ls_botoes_activos TYPE lty_botoes_activos.

  FIELD-SYMBOLS: <lfs_rb_tela_0005_texto1> LIKE scr0005-te11,
                 <lfs_rb_tela_0005_texto2> LIKE scr0005-te12,
                 <lfs_rb_tela_0005_value>  LIKE scr0005-vl1,
                 <lfs_rb_tela_0005_number> LIKE scr0005-nu1,
                 <lfs_rb_tela_0005_picker> LIKE scr0005-pk1,
                 <lv_text>                 TYPE ANY.

  CLEAR: scr0005.

  CREATE DATA lref_text TYPE c LENGTH gv_0005_text_length.
  ASSIGN lref_text->* TO <lv_text>.

  <lv_text> = gv_0005_text1.

** Textos de Cabeçalho
***********************************************************************
  scr0005-text1 = gv_0005_text1.
  scr0005-text2 = gv_0005_text2.
  scr0005-text3 = gv_0005_text3.
  scr0005-text4 = gv_0005_text4.
  scr0005-text5 = gv_0005_text5.
  scr0005-text6 = gv_0005_text6.
  scr0005-text7 = gv_0005_text7.

** Iniciador
***********************************************************************
  IF gv_0005_reseted EQ abap_false.
    lsv_page = 1.
    gv_0005_reseted = abap_true.
  ENDIF.

  lsv_page = lsv_page + uv_lv_page.

  MOVE lsv_page TO scr0005-page_a.
  SHIFT scr0005-page_a RIGHT DELETING TRAILING space.

  DESCRIBE TABLE gt_0005_items LINES lv_tot_bot.

  lv_tot_pag_d = lv_tot_bot / gv_total_items.

  lv_tot_pag_i  = CEIL( lv_tot_pag_d ).

  MOVE lv_tot_pag_i TO scr0005-page_t.
  SHIFT scr0005-page_t RIGHT DELETING TRAILING space.

** Escreve dados para tela
***********************************************************************
  DO gv_total_items TIMES.

    CLEAR: ls_botoes_activos.

*--> Index actual
    lv_index = lv_index + 1.
    lv_index_table = lv_index + ( gv_total_items * ( lsv_page - 1 ) ).

*--> Retorna dados da linha actual
    READ TABLE gt_0005_items INTO ls_0005_item INDEX lv_index_table.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    IF ls_0005_item IS INITIAL.
      lv_index_txt = '-'.
    ELSE.
      CLEAR lv_index_txt.
    ENDIF.

    MOVE  lv_index TO lv_c_index.

*--> Guarda Valor de linha
    CONCATENATE 'SCR0005-VL' lv_c_index INTO lv_nome.
    ls_botoes_activos-nome = lv_nome.
    ASSIGN (lv_nome) TO <lfs_rb_tela_0005_value>.
    CHECK <lfs_rb_tela_0005_value> IS ASSIGNED.

    ls_botoes_activos-nome = lv_nome.
    APPEND ls_botoes_activos TO lt_botoes_activos.

    <lfs_rb_tela_0005_value> = ls_0005_item-op_key.

*--> Escreve textos de linha
    CONCATENATE 'SCR0005-TE' lv_c_index '1' INTO lv_nome.
    ASSIGN (lv_nome) TO <lfs_rb_tela_0005_texto1>.
    CHECK <lfs_rb_tela_0005_texto1> IS ASSIGNED.

    ls_botoes_activos-nome = lv_nome.

    IF NOT lv_index_txt IS INITIAL.
      ls_botoes_activos-spacer = abap_true.
    ENDIF.

    APPEND ls_botoes_activos TO lt_botoes_activos.

    CONCATENATE 'SCR0005-TE' lv_c_index '2' INTO lv_nome.
    ASSIGN (lv_nome) TO <lfs_rb_tela_0005_texto2>.
    CHECK <lfs_rb_tela_0005_texto1> IS ASSIGNED.

    ls_botoes_activos-nome = lv_nome.
    APPEND ls_botoes_activos TO lt_botoes_activos.

    <lfs_rb_tela_0005_texto1> = ls_0005_item-op_text(20).
    <lfs_rb_tela_0005_texto2> = ls_0005_item-op_text+20(20).

    IF gv_0005_right_align EQ abap_true.
      <lv_text> = <lfs_rb_tela_0005_texto1>.
      SHIFT <lv_text> RIGHT DELETING TRAILING space.
      <lfs_rb_tela_0005_texto1> =  <lv_text>.
      CLEAR <lfs_rb_tela_0005_texto2>.
    ENDIF.

*--> Picker
    CONCATENATE 'SCR0005-PK' lv_c_index INTO lv_nome.
    ASSIGN (lv_nome) TO <lfs_rb_tela_0005_picker>.
    CHECK <lfs_rb_tela_0005_picker> IS ASSIGNED.

    IF NOT ls_0005_item-op_pick IS INITIAL.
      <lfs_rb_tela_0005_picker> = abap_true.
    ENDIF.

    ls_botoes_activos-nome = lv_nome.
    APPEND ls_botoes_activos TO lt_botoes_activos.

*--> Escreve Numero de Linha
    CONCATENATE 'SCR0005-NU' lv_c_index INTO lv_nome.
    ls_botoes_activos-nome = lv_nome.

    ASSIGN (lv_nome) TO <lfs_rb_tela_0005_number>.
    CHECK <lfs_rb_tela_0005_number> IS ASSIGNED.

    IF lv_index_txt IS INITIAL.
      <lfs_rb_tela_0005_number> = lv_c_index.
      SHIFT ls_0005_item-op_key LEFT DELETING LEADING space.
      CONDENSE ls_0005_item-op_key.
      <lfs_rb_tela_0005_number> = ls_0005_item-op_key.
    ELSE.
      <lfs_rb_tela_0005_number> = lv_index_txt.
      ls_botoes_activos-spacer = abap_true.
    ENDIF.

    APPEND ls_botoes_activos TO lt_botoes_activos.

    UNASSIGN <lfs_rb_tela_0005_texto1>.
    UNASSIGN <lfs_rb_tela_0005_texto2>.
  ENDDO.

  CLEAR: ls_botoes_activos.

  LOOP AT SCREEN.

    IF screen-group1 EQ 'COR'.
      screen-intensified = 1.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group4 EQ 'MOV' AND
       scr0005-page_t <= 1.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.

    CASE screen-name.
      WHEN 'SCR0005-BT_DN'.
        PERFORM check_btn_down_0005 USING lv_subrc.
        MODIFY SCREEN.
        CONTINUE.
      WHEN 'SCR0005-BT_UP'.
        PERFORM check_btn_up_0005 USING lv_subrc.
        MODIFY SCREEN.
        CONTINUE.
      WHEN 'RLMOB-PSAVE'.
        CONTINUE.
      WHEN 'RLMOB-PBACK'.
        IF gv_0005_required EQ abap_false.
          screen-invisible = 0.
        ELSE.
          screen-invisible = 1.
        ENDIF.

        MODIFY SCREEN.
        CONTINUE.
      WHEN 'SCR0005-TEXT1' OR 'SCR0005-TEXT2' OR 'SCR0005-TEXT3' OR
           'SCR0005-TEXT4' OR 'SCR0005-TEXT5' OR 'SCR0005-TEXT6' OR
           'SCR0005-TEXT7'.
        CONTINUE.
      WHEN 'SCR0005-PAGE_A'.
        CONTINUE.
      WHEN 'SCR0005-PAGE_T'.
        CONTINUE.
      WHEN 'SCR0005-OPC'.
        CONTINUE.
    ENDCASE.

    CLEAR ls_botoes_activos.
    READ TABLE lt_botoes_activos
          INTO ls_botoes_activos
          WITH KEY nome = screen-name.

    lv_name_sec = screen-name+8(2).

    IF ( lv_name_sec EQ 'NU' OR
         lv_name_sec EQ 'VL' OR
         lv_name_sec EQ 'PK'
       ) AND ( NOT ls_botoes_activos IS INITIAL ).
      CONTINUE.
    ENDIF.

    IF NOT ls_botoes_activos IS INITIAL AND
       ls_botoes_activos-spacer EQ abap_true AND
       lv_name_sec EQ 'TE'.
      screen-input = 0.
      screen-output = 0.
      screen-invisible = 1.
    ELSEIF NOT ls_botoes_activos IS INITIAL.
      screen-input = 1.
      screen-output = 1.
      screen-invisible = 0.
    ELSE.
      screen-input = 0.
      screen-output = 0.

      IF lv_name_sec = 'TE' OR lv_name_sec = 'NU' OR
         lv_name_sec = 'VL'.
        screen-invisible = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " STATUS_0005

*}   INSERT
