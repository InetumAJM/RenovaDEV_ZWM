************************************************************************
*                                                                      *
* Criação                                                              *
*  Autor: Ricardo Lopes - ROFF                                         *
*  Data:  09.05.2005                                                   *
*  Descrição: Mapa de controle da tabela ZWM028                        *
*
* Modificações
*  Autor:
*  Data:
*  Descrição:
*  Pesquisar alterações por:
*
************************************************************************

REPORT zwmrep0056 MESSAGE-ID zwmmsg001 .

************************************************************************
** Tabelas DD
************************************************************************
TABLES: zwm028_mod, zwm028, usr01.

************************************************************************
** Types e Includes
************************************************************************
TYPE-POOLS: slis.

************************************************************************
** Tabelas Internas
************************************************************************
DATA: itab       LIKE zwm028     OCCURS 0 WITH HEADER LINE,
      itab_mod   LIKE zwm028_mod OCCURS 0 WITH HEADER LINE.

************************************************************************
*   Dados p/ALV
************************************************************************
DATA: fieldcat_linha TYPE slis_fieldcat_alv,
      fieldcat_tab TYPE slis_t_fieldcat_alv,
      wa_eventos TYPE slis_alv_event,
      eventos TYPE slis_t_event,
      layout TYPE slis_layout_alv,
      is_variant TYPE disvariant,
      reprepid TYPE slis_reprep_id,
      it_sort TYPE slis_t_sortinfo_alv,
      is_sort TYPE slis_sortinfo_alv,
      grid_title TYPE lvc_title.

************************************************************************
** Variáveis
************************************************************************
DATA: idx       LIKE sy-tabix,
      programa  LIKE sy-repid,
      ok_code   LIKE sy-ucomm,
      pal_tot   LIKE zwm028-total_paletes,
      pal_pul   LIKE zwm028-paletes_pulmao,
      pal_car   LIKE zwm028-paletes_carro,
      st_pul    LIKE zwm028-st_pul,
      st_ppk    LIKE zwm028-st_pul,
      st_dck    LIKE zwm028-st_pul,
      pul1      LIKE zwm028-pulmao1,
      pul2      LIKE zwm028-pulmao1,
      pick      LIKE zwm028-pulmao1,
      porta     LIKE zwm028-pulmao1,
      motivo    LIKE zwm028_mod-motivo.

************************************************************************
** Constantes
************************************************************************
*constants: .

************************************************************************
** Parâmetros de entrada
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-000.
PARAMETERS: p_lgnum LIKE t300-lgnum DEFAULT '100' OBLIGATORY,
            p_refnr LIKE zwm028-refnr OBLIGATORY,
            p_vbeln LIKE likp-vbeln.
**
SELECTION-SCREEN SKIP 1.
PARAMETERS p_varia LIKE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK blk1.
**
PARAMETERS p_cwidth DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_hrzgln DEFAULT 'X' NO-DISPLAY.
PARAMETERS p_vrtgln DEFAULT 'X' NO-DISPLAY.

************************************************************************
** At Selection-screen.
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  is_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = is_variant
    IMPORTING
      es_variant    = is_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc = 0.
    p_varia = is_variant-variant.
  ELSE.
    MESSAGE i000 WITH 'Ainda não existem variantes de exibição'.
  ENDIF.

************************************************************************
** Start-of-Selection
************************************************************************
START-OF-SELECTION.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = text-001.

  CLEAR: usr01.

  SELECT SINGLE * FROM usr01
  WHERE bname = sy-uname.

** Obtem dados a listar
  PERFORM get_dados.

  IF itab[] IS INITIAL.
    MESSAGE i000 WITH
    'Não existem dados para as opções indicadas'.
    EXIT.
  ENDIF.

************************************************************************
** End-of-Selection
************************************************************************
END-OF-SELECTION.

** Prepara Listagem
  PERFORM ajusta_propriedades.

** Colunas Listagem
  PERFORM catalogo.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat              = fieldcat_tab
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      it_events                = eventos
      is_layout                = layout
      it_sort                  = it_sort
      i_grid_title             = grid_title
      i_callback_pf_status_set = 'PFSTATUS'
      is_variant               = is_variant
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_program       = programa
      i_save                   = 'X'
      i_background_id          = 'ALV_BACKGROUND'
    TABLES
      t_outtab                 = itab
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  CHECK sy-ucomm = '&F03'.

*&---------------------------------------------------------------------*
*&      Form  get_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dados .

  DATA: l_pal(5).

  FREE: itab, itab_mod.
  CLEAR: itab, itab_mod.

  SELECT * FROM zwm028
  INTO CORRESPONDING FIELDS OF TABLE itab
  WHERE lgnum   EQ p_lgnum
    AND refnr   EQ p_refnr
    AND remessa EQ p_vbeln.
*    AND remessa EQ ' '.

  CHECK sy-subrc EQ 0.

  READ TABLE itab INDEX 1.

  l_pal = itab-paletes_carro.
  CONDENSE l_pal.
  itab_mod-pal_car_out = l_pal.

  itab_mod-lgnum       = p_lgnum.
  itab_mod-refnr       = p_refnr.
  itab_mod-user_sap    = sy-uname.
  itab_mod-pal_tot_out = itab-total_paletes.
  itab_mod-pal_pul_out = itab-paletes_pulmao.
  APPEND itab_mod.

ENDFORM.                    " get_dados

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_PROPRIEDADES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ajusta_propriedades.

  layout-colwidth_optimize = p_cwidth.
  layout-window_titlebar = sy-title.
  layout-zebra = 'X'.
  layout-no_vline = p_vrtgln.
  layout-no_vline = p_vrtgln.
  layout-def_status = 'A'.

  programa = sy-repid.

** Eventos a capturar
  REFRESH eventos.
  wa_eventos-name = 'USER_COMMAND'.
  wa_eventos-form = 'HOTSPOT_ACTIVO'.
  APPEND wa_eventos TO eventos.

ENDFORM.                    " AJUSTA_PROPRIEDADES

*&---------------------------------------------------------------------*
*&      Form   CATALOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  catalogo.

  FREE fieldcat_tab.
  CLEAR fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'LGNUM'.
  fieldcat_linha-reptext_ddic = 'Sist.Dép.'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'REFNR'.
  fieldcat_linha-reptext_ddic = 'Grupo'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'TOTAL_PALETES'.
  fieldcat_linha-reptext_ddic = 'Total Paletes'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-datatype = 'QUANT'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'PALETES_PULMAO'.
  fieldcat_linha-reptext_ddic = 'Paletes Pulmão'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-datatype = 'QUANT'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'PALETES_CARRO'.
  fieldcat_linha-reptext_ddic = 'Paletes Carro'.
  fieldcat_linha-just = 'C'.
  fieldcat_linha-datatype = 'QUANT'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'ST_PUL'.
  fieldcat_linha-reptext_ddic = 'Tipo Dép.'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'PULMAO1'.
  fieldcat_linha-reptext_ddic = 'Pulmão 1'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'PULMAO2'.
  fieldcat_linha-reptext_ddic = 'Pulmão 2.'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'ST_PPK'.
  fieldcat_linha-reptext_ddic = 'Tipo Dép.'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'PRE_PICK'.
  fieldcat_linha-reptext_ddic = 'Posição'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'ST_DCK'.
  fieldcat_linha-reptext_ddic = 'Tipo Dép.'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'PORTA'.
  fieldcat_linha-reptext_ddic = 'Porta'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'TRANSPORTE'.
  fieldcat_linha-reptext_ddic = 'Nº Transporte'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'ZLOCK'.
  fieldcat_linha-reptext_ddic = 'Status'.
  fieldcat_linha-just = 'C'.
  APPEND fieldcat_linha TO fieldcat_tab.

ENDFORM.                    "catalogo

************************************************************************
*   Form PFSTATUS
************************************************************************
FORM pfstatus USING lt_extab.

  SET PF-STATUS 'STANDARD'.

ENDFORM.                    "pfstatus

*** PROCESSO DE EVENTOS
*&---------------------------------------------------------------------*
*&      Form TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.

  STATICS: lt_list_commentary TYPE slis_t_listheader,
           ls_list_commentary TYPE slis_listheader.

  DATA: l_datum(10),
        l_linhas LIKE sy-tabix,
        l_uzeit(8).

  FREE: lt_list_commentary.
  CLEAR: lt_list_commentary.

  DESCRIBE TABLE itab LINES l_linhas.

  CASE usr01-datfm.
    WHEN '1' OR '2' OR '3'.
      WRITE sy-datum TO l_datum USING EDIT MASK '__.__.____'.
    WHEN OTHERS.
      WRITE sy-datum TO l_datum USING EDIT MASK '____.__.__'.
  ENDCASE.

  WRITE sy-uzeit TO l_uzeit USING EDIT MASK '__:__:__'.

* Título
  ls_list_commentary-typ  = 'H'.
  ls_list_commentary-info = text-002.
  APPEND ls_list_commentary TO lt_list_commentary.

* Informações adicionais
  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-004.
  ls_list_commentary-info = sy-uname.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-005.
  ls_list_commentary-info = l_datum.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-006.
  ls_list_commentary-info = l_uzeit.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = text-007.
  ls_list_commentary-info = l_linhas.
  APPEND ls_list_commentary TO lt_list_commentary.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_list_commentary
      i_logo             = 'RENOVA_LOGO'.

ENDFORM.                    " TOP_OF_PAGE
************************************************************************
*   Form HOTSPOT_ACTIVO
************************************************************************
FORM hotspot_activo USING r_ucomm
                 CHANGING ls_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN 'REFR'.
      PERFORM get_dados.

    WHEN 'PALETES'.
      CALL SCREEN '0001' STARTING AT 10 1 ENDING AT 63 10.
  ENDCASE.

  ls_selfield-refresh = 'X'.

ENDFORM.                    "hotspot_activo
*&---------------------------------------------------------------------*
*&      Module  status_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'MAIN'.
  SET TITLEBAR 'TMAIN'.

  CHECK pal_tot IS INITIAL.

  READ TABLE itab INDEX 1.

  pal_tot = itab-total_paletes.
  pal_pul = itab-paletes_pulmao.
  pal_car = itab-paletes_carro.
  st_pul  = itab-st_pul.
  pul1    = itab-pulmao1.
  pul2    = itab-pulmao2.
  st_ppk  = itab-st_ppk.
  pick    = itab-pre_pick.
  st_dck  = itab-st_dck.
  porta   = itab-porta.

ENDMODULE.                 " status_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CLEAR: motivo, pal_tot, pal_pul, pal_car, ok_code, st_pul, pul1, pul2,
         st_ppk, pick, st_dck, porta.

  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  DATA: l_aux     LIKE zwm028_mod-pal_car_in,
        l_lock,
        hlp_uname LIKE sy-uname,
        l_tot     LIKE zwm028-total_paletes,
        l_pul     LIKE zwm028-paletes_pulmao,
        l_car     LIKE zwm028-paletes_carro,
        l_pulmao  LIKE zwm028-st_pul,
        l_pul1    LIKE zwm028-pulmao1,
        l_pul2    LIKE zwm028-pulmao1,
        l_ppk     LIKE zwm028-st_pul,
        l_ppk1    LIKE zwm028-pulmao1,
        l_dck     LIKE zwm028-st_pul,
        l_dck1    LIKE zwm028-pulmao1.

  l_aux = pal_car.

  CASE ok_code.
    WHEN 'SAV'.
      READ TABLE itab INDEX 1.
      IF pal_tot = itab-total_paletes  AND
         pal_pul = itab-paletes_pulmao AND
         pal_car = itab-paletes_carro  AND
         st_pul  = itab-st_pul         AND
         pul1    = itab-pulmao1        AND
         pul2    = itab-pulmao2        AND
         st_ppk  = itab-st_ppk         AND
         pick    = itab-pre_pick       AND
         st_dck  = itab-st_dck         AND
         porta   = itab-porta.
        MESSAGE i000 WITH 'Não existem alterações'.
      ELSE.
        l_tot    = itab-total_paletes.
        l_pul    = itab-paletes_pulmao.
        l_car    = itab-paletes_carro.
        l_pulmao = itab-st_pul.
        l_pul1   = itab-pulmao1.
        l_pul2   = itab-pulmao2.
        l_ppk    = itab-st_ppk.
        l_ppk1   = itab-pre_pick.
        l_dck    = itab-st_dck.
        l_dck1   = itab-porta.

        itab-total_paletes  = pal_tot.
        itab-paletes_pulmao = pal_pul.
        itab-paletes_carro  = pal_car.
        itab-st_pul         = st_pul.
        itab-pulmao1        = pul1.
        itab-pulmao2        = pul2.
        itab-st_ppk         = st_ppk.
        itab-pre_pick       = pick.
        itab-st_dck         = st_dck.
        itab-porta          = porta.

        MODIFY itab INDEX 1.

** CRIAR BLOQUEIO
        CLEAR: hlp_uname, l_lock.
        CALL FUNCTION 'ENQUEUE_EZ_ZWM028'
          EXPORTING
            mode_zwm028    = 'E'
            mandt          = sy-mandt
            lgnum          = itab-lgnum
            refnr          = itab-refnr
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        hlp_uname = sy-msgv1.

        CASE sy-subrc.
          WHEN 0.
            l_lock = 'X'.
          WHEN OTHERS.
            MESSAGE e000 WITH 'Ordem bloqueada por' hlp_uname.
        ENDCASE.

        MODIFY zwm028 FROM TABLE itab.
        COMMIT WORK AND WAIT.

** desbloquer a proxima caso exista
        IF NOT p_vbeln IS INITIAL.
          DATA l_ordem LIKE zwm028-ordem.

          READ TABLE itab INDEX 1.
          IF itab-tipo_lock = 'R'.
            CLEAR l_ordem.
            l_ordem = itab-ordem + 1.
            SELECT SINGLE *
                FROM zwm028
                    WHERE lgnum = itab-lgnum
                      AND refnr = itab-refnr
                      AND ordem = l_ordem.
            IF sy-subrc = 0.
              UPDATE zwm028 SET zlock = itab-zlock
                            WHERE lgnum = itab-lgnum
                            AND refnr = itab-refnr
                            AND ordem = l_ordem.
              COMMIT WORK AND WAIT.
            ENDIF.
          ENDIF.
        ENDIF.

        CALL FUNCTION 'DEQUEUE_EZ_ZWM028'
          EXPORTING
            mode_zwm028 = 'E'
            mandt       = sy-mandt
            lgnum       = itab-lgnum
            refnr       = itab-refnr.
        IF sy-subrc EQ 0.
          CLEAR: l_lock.
        ENDIF.

*        WAIT UP TO 1 SECONDS.

        GET TIME.

        READ TABLE itab_mod INDEX 1.
        CLEAR: itab_mod-hora.
        itab_mod-data         = sy-datum.
        itab_mod-hora         = sy-uzeit.
** Dados Novos
        itab_mod-pal_tot_in   = pal_tot.
        itab_mod-pal_pul_in   = pal_pul.
        itab_mod-pal_car_in   = l_aux.
        itab_mod-st_pul_in    = l_pulmao.
        itab_mod-pulmao1_in   = l_pul1.
        itab_mod-pulmao2_in   = l_pul2.
        itab_mod-st_ppk_in    = l_ppk.
        itab_mod-pre_pick_in  = l_ppk1.
        itab_mod-st_dck_in    = l_dck.
        itab_mod-porta_in     = l_dck1.
** Dados Antigos
        itab_mod-pal_tot_out  = l_tot.
        itab_mod-pal_pul_out  = l_pul.
        itab_mod-pal_car_out  = l_car.
        itab_mod-st_pul_out   = st_pul.
        itab_mod-pulmao1_out  = pul1.
        itab_mod-pulmao2_out  = pul2.
        itab_mod-st_ppk_out   = st_ppk.
        itab_mod-pre_pick_out = pick.
        itab_mod-st_dck_out   = st_dck.
        itab_mod-porta_out    = porta.
        itab_mod-motivo       = motivo.

        MODIFY itab_mod INDEX 1.

        MODIFY zwm028_mod FROM TABLE itab_mod.
        COMMIT WORK AND WAIT.

      ENDIF.

      CLEAR: motivo, pal_tot, pal_pul, pal_car, st_pul, pul1, pul2,
             st_ppk, pick, st_dck, porta, ok_code, itab_mod, itab.

      SET SCREEN '0000'.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                 " user_command_0001  INPUT
