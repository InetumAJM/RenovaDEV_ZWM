*&---------------------------------------------------------------------*
*&  Include           Z10MM05I_V1                                      *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  LE_PICKING  INPUT
*&---------------------------------------------------------------------*

MODULE le_picking INPUT.
  PERFORM lock_carga.
ENDMODULE.                             " LE_PICKING  INPUT
*&---------------------------------------------------------------------*
*&      Module  MOD_ITM_GUIA  INPUT
*&---------------------------------------------------------------------*
MODULE mod_itm_guia INPUT.
  read_index = page_begin + sy-stepl.
  READ TABLE tabi_guias_por_item-linha INTO *z1com_s INDEX read_index.
* valida se quantidade inserida > quantidade encomendada
  IF NOT z1com_s-vbeln IS INITIAL.
* só para guias criadas com base em encomendas
    IF z1com_s-total_cf > *z1com_s-total_cfi.
      MESSAGE e017(z1).
    ENDIF.
  ENDIF.
   *z1com_s-total_cf = z1com_s-total_cf.
  MODIFY tabi_guias_por_item-linha FROM *z1com_s INDEX read_index.
  CLEAR tabi_guias_por_item-oferta_processada.
  MODIFY tabi_guias_por_item INDEX tabix.
  quant_alterada = 'X'.
ENDMODULE.                             " MOD_ITM_GUIA  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  save_okcode = okcode.
  CLEAR okcode.
  CASE save_okcode.
    WHEN 'PGUP'.
      page_begin = page_begin - loop_lines.
      IF page_begin < 0.
        page_begin = 0.
      ENDIF.
    WHEN 'PGDN'.
      page_begin = page_begin + loop_lines.
      IF page_begin >= tab_lines.
        page_begin = tab_lines - 1.
      ENDIF.
    WHEN 'HOME'.
      page_begin = 0.
    WHEN 'END'.
      page_begin = tab_lines - loop_lines.
      IF page_begin < 0.
        page_begin = 0.
      ENDIF.
    WHEN 'SAVE'.
      PERFORM save.
      PERFORM carrega_guia_ecra.
    WHEN 'OFME'.
* recalcula oferta de mercadoria
      PERFORM rec_of_mercadoria.
    WHEN 'GS'.
* guia seguinte
      PERFORM guia_seguinte.
    WHEN 'GA'.
* guia anterior
      PERFORM guia_anterior.
    WHEN 'GP'.
      PERFORM guias_picking.
    WHEN 'EXG'.
      PERFORM exibir_guia.
    WHEN 'BL'.
* bloquear guia (excluir da confirmação)
      PERFORM bloquear_guia.
    WHEN 'DB'.
* desbloqueai guia (incluir na confirmação do picking)
      PERFORM desbloquear_guia.
  ENDCASE.
ENDMODULE.                             " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  SAIR_VOLTAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE sair_voltar INPUT.
  PERFORM sair_voltar.
  PERFORM carrega_guia_ecra.
ENDMODULE.                             " SAIR_VOLTAR  INPUT
*&---------------------------------------------------------------------*
*&      Module  SAIR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE sair INPUT.
  LEAVE PROGRAM.
ENDMODULE.                             " SAIR  INPUT
*&---------------------------------------------------------------------*
*&      Form  BLOQUEAR_GUIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bloquear_guia.
  tabi_guias_por_item-guia_bloqueada = 'X'.
  MODIFY tabi_guias_por_item INDEX tabix.
  MESSAGE s027(z1).
ENDFORM.                    " BLOQUEAR_GUIA
*&---------------------------------------------------------------------*
*&      Form  DESBLOQUEAR_GUIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM desbloquear_guia.
  tabi_guias_por_item-guia_bloqueada = ' '.
  MODIFY tabi_guias_por_item INDEX tabix.
  MESSAGE s028(z1).
ENDFORM.                    " DESBLOQUEAR_GUIA
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CLEAR: vttk-daten, vttk-uaten, save_okcode, okcode, text, vbss-sammg.
  CASE sy-ucomm.
    WHEN 'BACK'.

      IF sy-dynnr = '0100'.
        CLEAR vbss-sammg.
        SET SCREEN 0.
        LEAVE SCREEN.

        CALL FUNCTION 'DEQUEUE_EVVBSK'
          EXPORTING
            sammg          = vbss-sammg
          EXCEPTIONS
            system_failure = 8.
        CASE sy-subrc.
*        WHEN 8. MESSAGE a808 WITH vbss-sammg INTO g_dummy.
        ENDCASE.

      ELSE.
***        LEAVE TO SCREEN 100.
        SET SCREEN 0.
        LEAVE SCREEN.

      ENDIF.

*    WHEN 'EXIT' OR 'CANC'.
*
*      SET SCREEN 0.
*      LEAVE SCREEN.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0150  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0150 INPUT.

  CLEAR save_okcode.
  save_okcode = okcode.
  CLEAR okcode.

  CASE save_okcode.
    WHEN 'ENTR'.
    WHEN 'NEXT'.

      CHECK NOT vttk-daten IS INITIAL AND NOT vbss-sammg IS INITIAL.

      PERFORM valida_grupo_existe.

      PERFORM valida_transporte.

      PERFORM valida_remessas.

      CHECK g_rc IS INITIAL.
      PERFORM saida_mercadoria.

      CHECK g_rc IS INITIAL.
      PERFORM free_door.

      CHECK g_rc IS INITIAL.
      PERFORM ajustes_stock.

      CHECK g_rc IS INITIAL.
      PERFORM fecho_transporte.
    WHEN OTHERS.

  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0150  INPUT
*&---------------------------------------------------------------------*
*&      Module  data  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE data INPUT.

  DATA: in_data LIKE sy-datum, out_data LIKE sy-datum,
        l_uzeit LIKE sy-uzeit VALUE '120000',
        it_factory_calendars LIKE vtbfcal OCCURS 0 WITH HEADER LINE.

  CLEAR: in_data, out_data, it_factory_calendars, it_factory_calendars[],
         g_rc.

  IF sy-uzeit LT l_uzeit.

*-------------------------------------Seleccionar o 1º dia util do mes
    it_factory_calendars-ident = 'PT'.
    APPEND it_factory_calendars.

    in_data = sy-datum.
    in_data+6(2) = '01'.

    CALL FUNCTION 'DATE_CONVERT_TO_WORKINGDAY'
      EXPORTING
        date              = in_data
        direction         = '+'
      IMPORTING
        workingday        = out_data
      TABLES
        factory_calendars = it_factory_calendars.

  ENDIF.

*  IF out_data = sy-datum.
**----Mensagem de erro sempre que for o 1º dia util do mes e hora < 12h
*    CLEAR text.
*    CLEAR: g_dummy, g_rc.
*    text = 'Operação temporáriamente indisponivel, tente mais tarde'.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '000'
*        message_var1   = text.
*    MESSAGE e000(zwmmsg001) WITH text INTO g_dummy.
*    sy-msgv1 = g_dummy.
**   & & & &
*
*    g_rc = '4'.
*
*    CLEAR l_zztpdoc.
*    l_zztpdoc = 'TP_DATAINI'.
*
*    PERFORM log USING t311a-rbnum ' '.
*
*    setcursor = 'VBSS-SAMMG'.
*    CLEAR: vttk-daten, vttk-uaten, vbss-sammg.
*
*    CLEAR vttk-daten. g_rc = 4.
*    LEAVE TO SCREEN 150.
*
*
*  ENDIF.

*--Mensagem de erro sempre que data do transporte menor que data siatema

  CHECK g_rc IS INITIAL.

*  IF NOT vttk-daten IS INITIAL.
*    IF vttk-daten LT sy-datum.
*
*      CLEAR text.
*      CLEAR: g_dummy, g_rc.
*      text = 'Data do Transporte no passado'.
*
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWMMSG001'
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = '000'
*          message_var1   = text.
*      MESSAGE e000(zwmmsg001) WITH text INTO g_dummy.
*      sy-msgv1 = g_dummy.
**   & & & &
*
*      CLEAR l_zztpdoc.
*      l_zztpdoc = 'TP_DATAANT'.
*
*      PERFORM log USING t311a-rbnum ' '.
*
*      CLEAR vttk-daten. g_rc = 4.
*      LEAVE TO SCREEN 150.
*
*
*    ENDIF.
*  ENDIF.
  IF g_rc IS INITIAL.
    setcursor = 'VTTK-UATEN'.
  ENDIF.

ENDMODULE.                 " data  INPUT
*&---------------------------------------------------------------------*
*&      Module  hora  INPUT
*&---------------------------------------------------------------------*
MODULE hora INPUT.


*-----------Se hora preenchida menor que hora actual msg erro
*  IF vttk-daten EQ sy-datum AND vttk-uaten LT sy-uzeit
*     AND NOT vttk-uaten IS INITIAL.
*    .
*    CLEAR text.
*    CLEAR : g_dummy, g_rc.
*    CONCATENATE 'Hora do Transporte no passado:'
*                vttk-uaten '<' sy-uzeit INTO text SEPARATED BY space.
*
**    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
**      EXPORTING
**        message_id     = 'ZWMMSG001'
**        message_lang   = sy-langu
**        message_type   = 'E'
**        message_number = '000'
**        message_var1   = text.
*
*    MESSAGE e000(zwmmsg001) WITH text INTO g_dummy.
*    sy-msgv1 = g_dummy.
**   & & & &
*
*    CLEAR l_zztpdoc.
*    l_zztpdoc = 'TP_HORA'.
*
*    PERFORM log USING t311a-rbnum ' '.
*
*    CLEAR vttk-uaten. g_rc = 4.
*
*    LEAVE TO SCREEN 150.
*
*
*  ENDIF.

ENDMODULE.                 " hora  INPUT
*&---------------------------------------------------------------------*
*&      Module  FINAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE final INPUT.
*------------------------Se preenche hora e não preenche data msg erro

  IF vbss-sammg IS INITIAL.

    setcursor = 'VBSS-SAMMG'.

  ELSEIF vttk-daten IS INITIAL.

    setcursor = 'VTTK-DATEN'.

  ELSEIF vttk-uaten IS INITIAL.

    setcursor = 'VTTK-UATEN'.

  ENDIF.

ENDMODULE.                 " FINAL  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CLEAR save_okcode.
  save_okcode = okcode.
  CLEAR okcode.

  CASE save_okcode.
    WHEN 'NEXT' OR 'ENTR'.
      CHECK NOT vbss-sammg IS INITIAL.
      PERFORM le_picking.
*-----------------------------------------------ins
      PERFORM proc_bonus.
      PERFORM save.
    WHEN 'BACK'.

      CALL FUNCTION 'DEQUEUE_EVVBSK'
        EXPORTING
          sammg          = vbss-sammg
        EXCEPTIONS
          system_failure = 8.
      CASE sy-subrc.
*        WHEN 8. MESSAGE a808 WITH vbss-sammg INTO g_dummy.
      ENDCASE.

      CLEAR vbss-sammg.

    WHEN OTHERS.

  ENDCASE.


ENDMODULE.                 " user_command_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  VALIDA_GRUPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_grupo INPUT.

**********************************************************************

  PERFORM get_tipo_grupo.


  CLEAR t311a.   CLEAR : it_vttp, it_vttp[].

  SELECT * FROM  t311a
*         WHERE  lgnum  = '100' " << DEL ROFF(SDF):TMGP:06.01.2016 17:01:26
         WHERE  lgnum  = gv_lgnum " << INS ROFF(SDF):TMGP:06.01.2016 17:01:29
         AND    refnr  = vbss-sammg.

    PERFORM verifica_servisan_tx USING t311a-rbnum g_rc .

    SELECT SINGLE * FROM  vbuk
           WHERE  vbeln  = t311a-rbnum.


    IF g_rc IS INITIAL AND ( vbuk-pkstk NE '' AND vbuk-pkstk  NE 'C' AND vbuk-pkstk  NE 'B' ).
*------------------------------------------Verificar status Embalamento

      PERFORM valida_embalamento.

    ENDIF.

    IF vbuk-kostk NE z1om_kostk AND vbuk-lvstk NE z1om_lvstk.
*-------------------------- Status global do picking / Status global WM

      PERFORM valida_status_picking.

    ENDIF.

*    IF vbuk-wbstk = 'C'.
**--------------------------------------------Status Saida de Mercadoria
*
*      PERFORM valida_saida_mercadoria.
*    ENDIF.


    PERFORM valida_transporte_1.

    MOVE-CORRESPONDING t311a TO it_t311a.
    APPEND it_t311a. CLEAR it_t311a.

  ENDSELECT.

**--------------------------------------------Status Saida de Mercadoria
  PERFORM valida_todas_saidas_merc.


*------------------VALIDAR TRANSPORTES DIFERENTES NO MESMO GRUPO
  PERFORM valida_transportes_2.

ENDMODULE.                 " VALIDA_GRUPO  INPUT
