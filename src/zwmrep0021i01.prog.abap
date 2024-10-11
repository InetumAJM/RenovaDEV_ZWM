*&---------------------------------------------------------------------*
*&  Include           ZWMREP0021I01                                    *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  PERFORM clear_field1.
  CLEAR: sscc,cursorfield,ok_code_0001.

  SET SCREEN '0000'.
  LEAVE SCREEN.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_sscc  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc INPUT.

  DATA: ls_zwm028     TYPE zwm028,
        ls_zwm060     TYPE zwm060,
        lv_message_v1 TYPE bdc_vtext1,
        lv_remessa    TYPE vbeln,
        lv_subrc      TYPE sy-subrc,
        ls_lqua       TYPE lqua.

  CLEAR: i_zwm026, plant, s_loc.
  REFRESH i_zwm026.

  CHECK NOT sscc IS INITIAL.

  SELECT SINGLE * FROM vekp WHERE lgnum = lgnum AND exidv = sscc.
  IF sy-subrc = 0.

    " Validar se palete de picking foi Rejeitada - Dar nova entrada
    SELECT SINGLE *
      FROM lqua INTO ls_lqua
      WHERE lgnum = lgnum
      AND   lgtyp = 'REJ'
      AND   lgpla = 'EXP2_REJ'
      AND   lenum = sscc.

    IF sy-subrc <> 0.
      CLEAR text1.
      WRITE sscc TO text1 LEFT-JUSTIFIED.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '115'
          message_var1   = text1.

      PERFORM clear_field1.
      EXIT.

    ELSE.
      SELECT * INTO TABLE i_zwm026
        FROM zwm026
        WHERE armazem = lgnum
        AND   sscc    = sscc.

      READ TABLE i_zwm026 INDEX 1.
      IF sy-subrc <> 0.
        CLEAR text1.

        WRITE sscc TO text1 LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '111'
            message_var1   = text1.

        PERFORM clear_field1.
        EXIT.
      ENDIF.
    ENDIF.

  ELSE.
    SELECT * INTO TABLE i_zwm026
        FROM zwm026
            WHERE armazem = lgnum AND sscc = sscc.

    IF i_zwm026[] IS INITIAL.
      CLEAR text1.
      WRITE sscc TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '111'
          message_var1   = text1.

      PERFORM clear_field1.
      EXIT.

    ELSE.
      LOOP AT i_zwm026.
        IF i_zwm026-estado <> 'T'.
          CLEAR text1.
          WRITE sscc TO text1 LEFT-JUSTIFIED.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '112'
              message_var1   = text1.

          PERFORM clear_field1.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

** Validar Bloqueio de utilizador
  PERFORM enqueue CHANGING lv_subrc.
  IF lv_subrc <> 0.

    " O utilizador & já está a processar &
    WRITE sy-msgv1 TO text1 LEFT-JUSTIFIED.
    WRITE sscc     TO text2 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '366'
        message_var1   = text1
        message_var2   = text2.

    PERFORM clear_field1.
    EXIT.
  ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Pedro F. Lopes <<ROFF>>
*  Data: 24.02.2015 11:38:27
*  Motivo: Regras por Cliente
*--------------------------------------------------------------------*
  CLEAR lv_remessa.
  SELECT SINGLE id_servisan FROM zwm040
    INTO lv_remessa
    WHERE lgnum   = i_zwm026-armazem
      AND refnr   = i_zwm026-grupo
      AND remessa = i_zwm026-remessa.

  IF lv_remessa IS INITIAL.
    lv_remessa = i_zwm026-remessa.
  ENDIF.

  CLEAR: ls_zwm028, ls_zwm060.

  SELECT SINGLE * FROM zwm028
    INTO ls_zwm028
    WHERE lgnum   = i_zwm026-armazem
      AND refnr   = i_zwm026-grupo
      AND remessa = lv_remessa.

  IF ls_zwm028 IS NOT INITIAL.
    SELECT SINGLE * FROM zwm060
      INTO ls_zwm060
      WHERE kunnr = ls_zwm028-emissor.

    IF ls_zwm060 IS NOT INITIAL.

      CLEAR lv_message_v1.
      lv_message_v1 = ls_zwm060-message.

      " Mensagem Parametrizada por Cliente
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWM001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '000'
          message_var1   = lv_message_v1
        IMPORTING
          ret_code       = retcode.
    ENDIF.
  ENDIF.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Pedro F. Lopes <<ROFF>>
*--------------------------------------------------------------------*


  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user      = sy-uname
      i_vbeln     = lv_remessa
      i_recall    = 'X'
      i_usewm     = 'X'
      i_userf     = 'X'
      i_usemm     = 'X'
      i_useaut    = 'X'
      i_get_lgnum = 'X'
      i_get_werks = 'X'
      i_get_lgort = 'X'
    CHANGING
      c_lgnum     = lgnum
      c_werks     = plant
      c_lgort     = s_loc
    EXCEPTIONS
      error       = 1
      user_back   = 2
      OTHERS      = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

** Dados de Carga
  CONCATENATE 'Grupo:' ls_zwm028-refnr INTO carga_txt1 SEPARATED BY space.

  IF ls_zwm028-st_pul = 'AUT'.
    carga_txt2 = 'Carga - Automático'.
  ELSE.
    carga_txt2 = 'Carga - Pulmão'.
  ENDIF.

ENDMODULE.                 " check_sscc  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  DATA: ls_ltap TYPE ltap.

  DATA: lv_kunnr     TYPE kunnr,
        lv_sscc      TYPE bapihukey-hu_exid,
        lv_exidv     TYPE exidv,
        lv_param     TYPE zwm_parametro,
        lv_lgpla     TYPE lgpla,
        lv_werks     TYPE werks_d,
        lv_lgort     TYPE lgort_d,
        n_copies     TYPE itcpo-tdcopies,
*        ls_zwm060    type zwm060,
        ls_zwm061    TYPE zwm061,
        lt_mono_item TYPE zwm_items_hu OCCURS 0 WITH HEADER LINE,
        lt_sscc      TYPE zwm_ean128   OCCURS 0 WITH HEADER LINE.

**********************************************************************
  CHECK NOT sscc IS INITIAL.

  IF gv_aut_wcs = 'X'.
    CHECK lgpla_in IS NOT INITIAL.
  ENDIF.

  CLEAR: items, return_msg.
  REFRESH: items, return_msg.
*  Criar uma HU com os materiais da palete.

  LOOP AT i_zwm026.
    items-material = i_zwm026-material.
*    items-quantity = i_zwm026-quantidade.

    SELECT SINGLE * FROM ltap INTO ls_ltap
     WHERE lgnum = i_zwm026-armazem   AND
           tanum = i_zwm026-to_number.

    IF  sy-subrc = 0.
      CALL FUNCTION 'ROUND'
        EXPORTING
          decimals      = 0
          input         = ls_ltap-vsola
          sign          = 'X'
        IMPORTING
          output        = items-quantity
        EXCEPTIONS
          input_invalid = 1
          overflow      = 2
          type_invalid  = 3
          OTHERS        = 4.
      items-unit = ls_ltap-altme.
*    items-unit = i_zwm026-unidade.
    ENDIF.

    items-werks = ls_ltap-werks.
    items-lgort = ls_ltap-lgort.
    items-batch = i_zwm026-lote.
    APPEND items.
    CLEAR items.
  ENDLOOP.

  CLEAR: imp.
**  CLEAR: plant, s_loc, imp.
**
**  SELECT SINGLE *
**      FROM zwm001
**          WHERE armazem = lgnum AND
**                processo = 'GERAL' AND
**                parametro = 'PLANT'.
**
**  MOVE zwm001-valor TO plant.
**
**  CLEAR valor.
**  SELECT SINGLE *
**      FROM zwm001
**          WHERE armazem = lgnum AND
**                processo = 'GERAL' AND
**                parametro = 'LGORT'.
**
**  MOVE zwm001-valor TO s_loc.
  CLEAR vekp.
  SELECT SINGLE * FROM vekp WHERE lgnum = lgnum AND exidv = sscc.
  IF sy-subrc <> 0.

    CALL FUNCTION 'ZWM_CREATE_HU'
      EXPORTING
        warehouse                  = lgnum
        plant                      = plant
        s_loc                      = s_loc
        packing_material           = i_zwm026-pack_material
        hu                         = sscc
      TABLES
        return_msg                 = return_msg
        items                      = items
      EXCEPTIONS
        empty_table                = 1
        reference_document_differs = 2
        empty_delivery_item        = 3
        item_not_found             = 4
        OTHERS                     = 5.

    IF sy-subrc <> 0.
      READ TABLE return_msg INDEX 1.
      IF sy-subrc = 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = return_msg-msgid
            message_lang   = sy-langu
            message_type   = return_msg-msgtyp
            message_number = return_msg-msgnr
            message_var1   = return_msg-msgv1.
      ENDIF.

      PERFORM clear_field1.
      EXIT.
    ENDIF.
  ENDIF.

* Impressao Automatica da etiqueta de envio e lista de conteudo
* Impressao da Lista de Conteudo - modificado para ser tudo so na
* etiqueta
*    SUBMIT zwmrep0022 WITH p_lgnum = lgnum
*                      WITH p_sscc = sscc
*                      AND RETURN.

  CLEAR t_sscc.
  REFRESH t_sscc.

  t_sscc-sscc = sscc.
  APPEND t_sscc.

** -> Pedro F. Lopes - 24.02.2015 15:56:02
  ls_zwm061-lgnum = lgnum.
  ls_zwm061-sscc_master = sscc.
** <- Pedro F. Lopes - 24.02.2015 15:56:02

  lv_param = sy-uname.

  CLEAR valor.
  SELECT SINGLE *
      FROM zwm001
          WHERE armazem = lgnum AND
                processo = 'EAN128' AND
                parametro = lv_param.

  IF sy-subrc = 0.
    MOVE zwm001-valor TO imp.

  ELSE.
    CLEAR valor.
    SELECT SINGLE *
        FROM zwm001
            WHERE armazem = lgnum AND
                  processo = 'EAN128' AND
                  parametro = 'PICKING'.

    MOVE zwm001-valor TO imp.
  ENDIF.

*    call function 'ZWM_IMPRIME_EAN128'
*      exporting
*        printer                  = imp
*      tables
*        sscc                     = t_sscc
*      exceptions
*        impressora_nao_existe    = 1
*        sscc_nao_existe          = 2
*        sscc_com_impressao_grupo = 3
*        others                   = 4.
*    if sy-subrc <> 0.
**      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    endif.

*    clear: cursorfield, sscc.
*    set screen '0002'.leave screen.


*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Pedro F. Lopes <<ROFF>>
*  Data: 24.02.2015 12:01:38
*  Motivo: Etiqueta Mono-Referência por Cliente
*--------------------------------------------------------------------*
  SELECT SINGLE emissor FROM zwm028
    INTO lv_kunnr
    WHERE lgnum   = i_zwm026-armazem
      AND refnr   = i_zwm026-grupo
      AND remessa = i_zwm026-remessa.

  IF sy-subrc IS INITIAL AND vekp-exidv IS INITIAL.

    SELECT SINGLE * FROM zwm060
      INTO ls_zwm060
      WHERE kunnr = lv_kunnr.

    IF sy-subrc IS INITIAL AND
      ls_zwm060-etiq_mono IS NOT INITIAL.

      REFRESH lt_sscc.
      CLEAR lt_sscc.

      LOOP AT items.

        CLEAR: return_msg, lt_mono_item, lv_sscc.
        REFRESH: return_msg, lt_mono_item.

        lt_mono_item = items.

        IF items-werks IS NOT INITIAL.
          lv_werks = items-werks.
        ELSE.
          lv_werks = plant.
        ENDIF.

        IF items-lgort IS NOT INITIAL.
          lv_lgort = items-lgort.
        ELSE.
          lv_lgort = s_loc.
        ENDIF.

        APPEND lt_mono_item.

        " Criar HU por item
        CALL FUNCTION 'ZWM_CREATE_HU'
          EXPORTING
            warehouse                  = lgnum
            plant                      = lv_werks "plant
            s_loc                      = lv_lgort "s_loc
            packing_material           = i_zwm026-pack_material
          IMPORTING
            hukey                      = lv_sscc
          TABLES
            return_msg                 = return_msg
            items                      = lt_mono_item
          EXCEPTIONS
            empty_table                = 1
            reference_document_differs = 2
            empty_delivery_item        = 3
            item_not_found             = 4
            OTHERS                     = 5.

        IF sy-subrc <> 0.
          READ TABLE return_msg INDEX 1.
          IF sy-subrc = 0.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = return_msg-msgid
                message_lang   = sy-langu
                message_type   = return_msg-msgtyp
                message_number = return_msg-msgnr
                message_var1   = return_msg-msgv1.

            PERFORM clear_field1.
          ENDIF.

          EXIT.

        ENDIF.

        lt_sscc-sscc = lv_sscc.

        CLEAR ls_zwm061-sscc.
        ls_zwm061-sscc = lv_sscc.
*          lt_sscc-sscc_master = sscc.
        APPEND lt_sscc.

        MODIFY zwm061 FROM ls_zwm061.

        COMMIT WORK.

      ENDLOOP.

    ENDIF.

  ENDIF.

** Imprissão Etiqueta Picking - Envolvedora Manual
**********************************************************************
  IF gv_aut_wcs IS INITIAL OR ( gv_aut_wcs = 'X' AND lgpla_in = lgpla_man ).

    " Imprime Etiqueta Multi-Referência
    CALL FUNCTION 'ZWM_IMPRIME_EAN128'
      EXPORTING
        printer                  = imp
      TABLES
        sscc                     = t_sscc
      EXCEPTIONS
        impressora_nao_existe    = 1
        sscc_nao_existe          = 2
        sscc_com_impressao_grupo = 3
        OTHERS                   = 4.

    IF sy-subrc IS INITIAL AND ls_zwm060-etiq_mono IS NOT INITIAL.

      n_copies = 1.

      " Impressão Etiquetas Mono-Referência
      CALL FUNCTION 'ZWM_IMPRIME_EAN128'
        EXPORTING
          printer                  = imp
          copies                   = n_copies
        TABLES
          sscc                     = lt_sscc
        EXCEPTIONS
          impressora_nao_existe    = 1
          sscc_nao_existe          = 2
          sscc_com_impressao_grupo = 3
          OTHERS                   = 4.
      IF sy-subrc <> 0.
      ENDIF.

    ENDIF.

    sscc_ant = sscc.

    CLEAR: cursorfield, sscc, lgpla_in.

    SET SCREEN '0002'.
    LEAVE SCREEN.

** Mesa entrada no automático (WCS)
**********************************************************************
  ELSEIF gv_aut_wcs = 'X' AND lgpla_in = lgpla_aut.

    PERFORM create_ot_exp_picking.

  ENDIF.

ENDMODULE.                 " USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_sscc2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc2 INPUT.

  CHECK NOT sscc IS INITIAL.

  IF sscc <> sscc_ant.
    CLEAR text1.
    WRITE sscc TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '117'
        message_var1   = text1.
    CLEAR: ok_code_0002, sscc, lgpla_in, cursorfield.
  ELSE.
    SELECT SINGLE * FROM vekp WHERE exidv = sscc.

    IF sy-subrc <> 0.
      CLEAR text1.
      WRITE sscc TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '113'
          message_var1   = text1.

      CLEAR: ok_code_0002, sscc, lgpla_in, cursorfield.
    ENDIF.
  ENDIF.
ENDMODULE.                 " check_sscc2  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit2 INPUT.
*  CLEAR: sscc, cursorfield, ok_code_0002.
*  SET SCREEN '0000'.
*  LEAVE SCREEN.
ENDMODULE.                 " exit2  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0002 INPUT.

  PERFORM create_ot_exp_picking.

ENDMODULE.                 " USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_LGPLA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_lgpla INPUT.

** Validar posição
**********************************************************************
  CHECK lgpla_in IS NOT INITIAL.

  CLEAR: lgpla_aut, lgpla_man.

  CALL FUNCTION 'ZWM_GET_MESA_WCS'
    EXPORTING
      i_lgnum = lgnum
      i_lgtyp = '932'
      i_type  = 'E'
    IMPORTING
      e_mesa  = lgpla_aut(10)
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.

  IF sscc_ant IS INITIAL.
    lgpla_man = 'EXP2_ETIQ'.
  ELSE.
    lgpla_man = 'EXP2_MAN'.
  ENDIF.

  IF lgpla_in <> lgpla_aut AND lgpla_in <> lgpla_man.
    CLEAR text1.
    WRITE lgpla_in TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '034'
        message_var1   = text1.

    CLEAR: ok_code_0001, lgpla_in.
  ENDIF.

ENDMODULE.
