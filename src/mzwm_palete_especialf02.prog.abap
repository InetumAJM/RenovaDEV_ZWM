************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: MZWM_PALETE_ESPECIALF02                                  *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Manutenção da Tabela de Paletização Especial (Forms 2)   *
* Criado por: Luís Rocha                                               *
* Criado em.: 06/12/2004                                               *
* Tipo PRG..: Include                                                  *
************************************************************************

*---------------------------------------------------------------------*
*       FORM MARCA_LINHAS                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM marca_linhas.
  CASE sy-dynnr.
    WHEN '0100'.
      LOOP AT w_tab_0100.
        w_tab_0100-flg_del = 'X'.
        MODIFY w_tab_0100 INDEX sy-tabix.
      ENDLOOP.
    WHEN '0200'.
      LOOP AT w_tab_0200.
        w_tab_0200-flg_del = 'X'.
        MODIFY w_tab_0200 INDEX sy-tabix.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    "marca_linhas

*---------------------------------------------------------------------*
*       FORM DESMARCA_LINHAS                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM desmarca_linhas.
  CASE sy-dynnr.
    WHEN '0100'.
      LOOP AT w_tab_0100.
        CLEAR w_tab_0100-flg_del.
        MODIFY w_tab_0100 INDEX sy-tabix.
      ENDLOOP.
    WHEN '0200'.
      LOOP AT w_tab_0200.
        CLEAR w_tab_0200-flg_del.
        MODIFY w_tab_0200 INDEX sy-tabix.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    "desmarca_linhas

*---------------------------------------------------------------------*
*       FORM NOVAS_ENTRADAS                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM novas_entradas.
  ctrl_tab-lines =  ctrl_tab-lines + 10.
  scroll_0200-entries_sum = ctrl_tab-lines.
ENDFORM.                    "novas_entradas

*---------------------------------------------------------------------*
*       FORM MODIFICA_REGISTO_0100                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM modifica_registo_0100.
* perform verifica_cliente  using zwm031-kunnr.
* perform verifica_material using zwm031-matnr.
  MOVE-CORRESPONDING zwm031 TO w_tab_0100.
  PERFORM ler_kna1 USING zwm031-kunnr
                CHANGING w_tab_0100-maktx.
  PERFORM ler_makt USING zwm031-matnr
                CHANGING w_tab_0100-maktx.
  MOVE w_flag_del TO w_tab_0100-flg_del.
  scroll_0100-curow = sy-stepl.
  w_tab_0100-flg_mod = k_on.
  scroll_0100-changes = k_um.
ENDFORM.                    "modifica_registo_0100

*---------------------------------------------------------------------*
*       FORM NOVO_REGISTO_0100                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM novo_registo_0100.

  PERFORM verifica_cliente  USING zwm_scr-kunnr.
  PERFORM verifica_material USING zwm031-matnr.
  MOVE-CORRESPONDING zwm031 TO w_tab_0100.
  PERFORM ler_kna1 USING zwm031-kunnr
                CHANGING w_tab_0100-maktx.
  PERFORM ler_makt USING zwm031-matnr
                CHANGING w_tab_0100-maktx.
  MOVE w_flag_del TO w_tab_0100-flg_del.
  scroll_0100-curow = sy-stepl + 1.
  w_tab_0100-flg_ins = k_on.
  scroll_0100-maxlines = scroll_0100-maxlines + 1.
  scroll_0100-changes = k_um.
  IF scroll_0100-curow > scroll_0100-loops.
    PERFORM scrolling_in_work_table USING scroll_0100-firstline
                                          scroll_0100-start
                                          scroll_0100-maxlines
                                          'X'
                                          scroll_0100-loops
                                          'P+'
                                          'X'
                                 CHANGING scroll_0100-entries_sum
                                          scroll_0100-page_sum.
    scroll_0100-curow = 2.
  ENDIF.
ENDFORM.                    "novo_registo_0100

*---------------------------------------------------------------------*
*       FORM MODIFICA_REGISTO_0200                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM modifica_registo_0200.

  MOVE-CORRESPONDING zwm031 TO w_tab_0200.

*  perform ler_kna1 using zwm031-kunnr
*                changing w_tab_0200-maktx.

  PERFORM ler_makt USING zwm031-matnr
                CHANGING w_tab_0200-maktx.

  MOVE w_flag_del TO w_tab_0200-flg_del.
  scroll_0200-curow = sy-stepl.
  w_tab_0200-flg_mod = k_on.
  scroll_0200-changes = k_um.

ENDFORM.                    "modifica_registo_0200

*---------------------------------------------------------------------*
*       FORM NOVO_REGISTO_0200                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM novo_registo_0200.

*  PERFORM verifica_cliente  USING zwm_scr-kunnr.
  PERFORM verifica_material USING zwm031-matnr.
  MOVE-CORRESPONDING zwm031 TO w_tab_0200.
*  PERFORM ler_kna1 USING zwm031-kunnr
*                CHANGING w_tab_0200-maktx.
  PERFORM ler_makt USING zwm031-matnr
                CHANGING w_tab_0200-maktx.
  MOVE w_flag_del TO w_tab_0200-flg_del.
  scroll_0200-curow = sy-stepl + 1.
  w_tab_0200-flg_ins = k_on.
  scroll_0200-maxlines = scroll_0200-maxlines + 1.
  scroll_0200-changes = k_um.
  IF scroll_0200-curow > scroll_0200-loops.
    scroll_0200-firstline = ctrl_tab-top_line.
    PERFORM scrolling_in_work_table USING scroll_0200-firstline
                                          scroll_0200-start
                                          scroll_0200-maxlines
                                          'X'
                                          scroll_0200-loops
                                          'P+'
                                          'X'
                                 CHANGING scroll_0200-entries_sum
                                          scroll_0200-page_sum.
    ctrl_tab-top_line = scroll_0200-firstline.
    scroll_0200-curow = 2.
  ENDIF.
ENDFORM.                    "novo_registo_0200

*---------------------------------------------------------------------*
*       FORM verifica_material                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_MATNR                                                       *
*---------------------------------------------------------------------*
FORM verifica_material USING f_matnr.
* Valida se o material existe
  SELECT SINGLE * FROM mara WHERE matnr = f_matnr.
  IF sy-subrc NE 0.
    MESSAGE e238(m3) WITH f_matnr.
*   Material & não existe
  ENDIF.
ENDFORM.                    "verifica_material

*&--------------------------------------------------------------------*
*&      Form  verifica_cliente
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_KUNNR    text
*---------------------------------------------------------------------*
FORM verifica_cliente USING f_kunnr.

* Valida se o cliente existe
  SELECT SINGLE * FROM kna1 WHERE kunnr = f_kunnr.
  IF sy-subrc NE 0.
    MESSAGE e153(f2) WITH f_kunnr.
*   Cliente & não foi criado
  ENDIF.
ENDFORM.                    "verifica_cliente

*---------------------------------------------------------------------*
*       FORM ler_makt                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_MATNR                                                       *
*  -->  F_MAKTX                                                       *
*---------------------------------------------------------------------*
FORM ler_makt USING f_matnr
           CHANGING f_maktx.
  CLEAR makt.
  SELECT SINGLE * FROM makt WHERE spras = sy-langu
                              AND matnr = f_matnr.
  MOVE makt-maktx TO f_maktx.
ENDFORM.                    "ler_makt

*&--------------------------------------------------------------------*
*&      Form  ler_kna1
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_KUNNR    text
*      -->F_NAME1    text
*---------------------------------------------------------------------*
FORM ler_kna1 USING f_kunnr
           CHANGING f_name1.
  CLEAR kna1.
  SELECT SINGLE * FROM kna1 WHERE kunnr = f_kunnr.
  MOVE kna1-name1 TO f_name1.
ENDFORM.                                                    "ler_kna1

*&--------------------------------------------------------------------*
*&      Form  ler_t300t
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_LGNUM    text
*---------------------------------------------------------------------*
FORM ler_t300t USING f_lgnum.

  CLEAR t300t.
  SELECT SINGLE * FROM t300t WHERE spras = sy-langu
                               AND lgnum = f_lgnum.
ENDFORM.                                                    "ler_t300t

*---------------------------------------------------------------------*
*       FORM POSICIONAMENTO                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM posicionamento.

  CALL SCREEN 250 STARTING AT 1  15
                    ENDING AT 50 23.

ENDFORM.                    "posicionamento


*---------------------------------------------------------------------*
*       FORM IMPRIME_DADOS                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM imprime_dados .

*  leave to list-processing .
*  set pf-status 'PRINT'.
*  clear t001w .
*  select single * from t001w where werks = zbc_aux-werks .
*
*  loop at w_tab_0200 .
*    write:/13 w_tab_0200-matnr intensified off ,
*           25 w_tab_0200-maktx intensified off .
*  endloop .
*

ENDFORM .                    "imprime_dados

*----------------------------------------------------------------------*
*  bloco de cabecalho                                                  *
*----------------------------------------------------------------------*

*TOP-OF-PAGE .
*
*  ADD -1 TO sy-linsz.
*
*  MOVE: '0'  TO bhdgd-inifl ,
*    'PT01'   TO bhdgd-bukrs ,
*    sy-linsz TO bhdgd-lines ,
*    sy-uname TO bhdgd-uname ,
*    sy-repid TO bhdgd-repid ,
*    sy-title TO bhdgd-line1 ,
*    '   '    TO bhdgd-line2 ,
*    '   '    TO bhdgd-miffl ,
*    ' '      TO bhdgd-separ ,
*    'BURKS'  TO bhdgd-domai.
*
*  SUMMARY.
*  PERFORM batch-heading(rsbtchh0) .
*  ADD 1 TO sy-linsz.
*  ULINE .
