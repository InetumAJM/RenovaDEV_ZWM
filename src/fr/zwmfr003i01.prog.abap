*&---------------------------------------------------------------------*
*&  Include           ZWMREP0021I01                                    *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
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

  DATA: ls_zwm028 TYPE zwm028,
        ls_zwm060 TYPE zwm060,
        lv_message_v1 TYPE bdc_vtext1,
        lv_remessa TYPE vbeln.

  CLEAR i_zwm026.
  REFRESH i_zwm026.

  CHECK NOT sscc IS INITIAL.

  SELECT SINGLE * FROM vekp WHERE lgnum = lgnum AND exidv = sscc.
  IF sy-subrc = 0.
    CLEAR text1.
    WRITE sscc TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '115'
        message_var1   = text1.
    CLEAR: ok_code_0001, sscc, cursorfield.

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
      CLEAR: ok_code_0001, sscc, cursorfield.

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
          CLEAR: ok_code_0001, sscc, cursorfield.
          EXIT.
        ENDIF.
      ENDLOOP.
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
        n_copies     TYPE itcpo-tdcopies,
*        ls_zwm060    type zwm060,
        ls_zwm061    TYPE zwm061,
        lt_mono_item TYPE zwm_items_hu OCCURS 0 WITH HEADER LINE,
        lt_sscc      TYPE zwm_ean128   OCCURS 0 WITH HEADER LINE.

**********************************************************************

  CHECK NOT sscc IS INITIAL.

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

    items-batch = i_zwm026-lote.
    APPEND items.
    CLEAR items.
  ENDLOOP.

  CLEAR: plant, s_loc, imp.
*
*  SELECT SINGLE *
*      FROM zwm001
*          WHERE armazem = lgnum AND
*                processo = 'GERAL' AND
*                parametro = 'PLANT'.
*
*  MOVE zwm001-valor TO plant.
*
*  CLEAR valor.
*  SELECT SINGLE *
*      FROM zwm001
*          WHERE armazem = lgnum AND
*                processo = 'GERAL' AND
*                parametro = 'LGORT'.
*
*  MOVE zwm001-valor TO s_loc.


  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_matnr   = ls_ltap-matnr
      i_vbeln   = ls_ltap-vbeln
    CHANGING
      c_lgnum   = lgnum
      c_werks   = plant
      c_lgort   = s_loc
    EXCEPTIONS
      error     = 1
      user_back = 2
      OTHERS    = 3.


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
      CLEAR: ok_code_0001, sscc, cursorfield.
    ENDIF.


  ELSE.
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

    CLEAR valor.
    SELECT SINGLE *
        FROM zwm001
            WHERE armazem = lgnum AND
                  processo = 'EAN128' AND
                  parametro = 'PICKING'.

    MOVE zwm001-valor TO imp.

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

    sscc_ant = sscc.
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

    IF sy-subrc IS INITIAL.

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
          APPEND lt_mono_item.

          " Criar HU por item
          CALL FUNCTION 'ZWM_CREATE_HU'
            EXPORTING
              warehouse                  = lgnum
              plant                      = plant
              s_loc                      = s_loc
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
              CLEAR: ok_code_0001, sscc, cursorfield.
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
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Pedro F. Lopes <<ROFF>>
*--------------------------------------------------------------------*

    CLEAR: cursorfield, sscc.
    SET SCREEN '0002'.
    LEAVE SCREEN.

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
    CLEAR: ok_code_0002, sscc, cursorfield.
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

      CLEAR: ok_code_0002, sscc, cursorfield.
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

  DATA i_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.
  DATA l_lagp LIKE lagp OCCURS 0 WITH HEADER LINE.

  DATA: ls_zwm011	TYPE zwm011.

  DATA: lv_tanum TYPE tanum.

  DATA: lt_zwm010	TYPE TABLE OF zwm010,
        lt_return_msg	TYPE TABLE OF bdcmsgcoll,
        ls_ltap_c    TYPE ltap,
        ls_ltak_c    TYPE ltak,
        ls_zwm028_c  TYPE zwm028,
        ls_zwm028_c1 TYPE zwm028,
        ls_zwm026_c  TYPE zwm026,
        ls_zwm001_c  TYPE zwm001.

  CHECK NOT sscc IS INITIAL.

  CLEAR i_sscc.
  REFRESH i_sscc.

  LOOP AT i_zwm026.
    i_sscc-material = i_zwm026-material.
    i_sscc-quantidade = i_zwm026-quantidade.
    i_sscc-uni = i_zwm026-unidade.
    i_sscc-lote_producao = i_zwm026-lote.
    APPEND i_sscc.
    CLEAR i_sscc.
  ENDLOOP.

  CLEAR valor.
  CLEAR: mov, plant, s_loc, lagp.
  CLEAR l_lagp.
  REFRESH l_lagp.


  CLEAR zwm026.
  SELECT SINGLE *
      FROM zwm026
          WHERE armazem = lgnum AND
                sscc = sscc.

  CLEAR zwm028.
  SELECT SINGLE *
      FROM zwm028
          WHERE lgnum = lgnum AND
                refnr = zwm026-grupo AND
                remessa = '          '.

  IF NOT zwm028-st_ppk IS INITIAL.
** Movimento de WM
    SELECT SINGLE valor INTO valor
        FROM zwm001
            WHERE armazem = lgnum AND
                  processo = 'SAIDA_ARMAZEM_PPK' AND
                  parametro = 'MOV_WM'.

*
*    SELECT SINGLE *
*          FROM lagp
*              WHERE lgnum = lgnum AND
*                    lgtyp = 'PPK' AND
*                    skzue <> 'X' AND
*                    skzsi <> 'X' AND
*                    anzqu = '0'.
*    IF sy-subrc <> 0.

    SELECT * INTO TABLE l_lagp
              FROM lagp
                  WHERE lgnum = lgnum AND
                        lgtyp = 'PPK' AND
                        skzue <> 'X' AND
                        skzsi <> 'X' AND
                        anzqu = '0'.
    IF l_lagp[] IS INITIAL.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '207'
          message_var1   = text1.
      CLEAR: ok_code_0001, sscc, cursorfield.
      EXIT.
    ELSE.
      SORT l_lagp BY sorlp.
      READ TABLE l_lagp INDEX 1.
    ENDIF.
  ELSE.
    SELECT SINGLE valor INTO valor
          FROM zwm001
              WHERE armazem = lgnum AND
                    processo = 'SAIDA_ARMAZEM_PCK' AND
                    parametro = 'MOV_WM'.
  ENDIF.

  MOVE valor TO mov.
  CLEAR valor.

  SELECT SINGLE valor INTO valor
     FROM zwm001
         WHERE armazem = lgnum AND
               processo = 'GERAL' AND
               parametro = 'PLANT'.
  MOVE valor TO plant.
  CLEAR valor.

  SELECT SINGLE valor INTO valor
   FROM zwm001
       WHERE armazem = lgnum AND
             processo = 'GERAL' AND
             parametro = 'LGORT'.
  MOVE valor TO s_loc.
  CLEAR valor.

  IF zwm028-st_ppk IS INITIAL.
    CLEAR l_lagp-lgpla.
  ENDIF.

  CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
    EXPORTING
      warehouse      = lgnum
      mov_type       = mov
      plant          = plant
      s_loc          = s_loc
      bin_destino    = l_lagp-lgpla
      certificado    = 'PKF 00-001'
      sscc_adicional = sscc
    IMPORTING
      to             = lv_tanum
    TABLES
      return_msg     = return_msg
      sscc           = i_sscc
    EXCEPTIONS
      error          = 1
      OTHERS         = 2.
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

      CLEAR: ok_code_0002, sscc, cursorfield.
    ENDIF.
    RETURN.
  ENDIF.


** Confirmação de TO
***********************************************************************
  DO 1 TIMES.
    SELECT SINGLE * FROM ltak
                  INTO ls_ltak_c
                  WHERE lgnum = lgnum AND
                        tanum = lv_tanum.
    CHECK sy-subrc EQ 0.

    SELECT SINGLE * FROM ltap
                    INTO ls_ltap_c
                    WHERE lgnum = lgnum AND
                          tanum = lv_tanum AND
                          pquit = '' AND
                          pvqui = ''.
    CHECK sy-subrc EQ 0.

    SELECT SINGLE * FROM zwm026
                    INTO ls_zwm026_c
                    WHERE armazem = ls_ltak_c-lgnum AND
                          sscc = ls_ltak_c-lznum.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE * FROM zwm028
                    INTO ls_zwm028_c1
                    WHERE lgnum   = ls_ltak_c-lgnum AND
                          refnr   = ls_zwm026_c-grupo AND
                          remessa = ls_zwm026_c-remessa.
    CHECK sy-subrc EQ 0.

    SELECT SINGLE * FROM zwm028
                    INTO ls_zwm028_c
                    WHERE lgnum   = ls_ltak_c-lgnum AND
                          refnr   = ls_zwm026_c-grupo AND
                          remessa = ''.
    CHECK sy-subrc EQ 0.

    CHECK ls_zwm028_c1-zlock EQ ls_zwm028_c-zlock.

    ls_zwm011-armazem         = ls_ltak_c-lgnum.
    ls_zwm011-to_number       = ls_ltak_c-tanum.
    ls_zwm011-to_item         = ls_ltap_c-tapos.
    ls_zwm011-user_name       = sy-uname.
    ls_zwm011-equipamento     = 'PORTAPALETES'.
    ls_zwm011-status          = 'C'.
    ls_zwm011-queue           = ls_ltak_c-queue.


    IF NOT ls_zwm028_c-st_ppk IS INITIAL.
      ls_zwm011-ultimo_tipo_dep = ls_zwm028_c-st_ppk.
      ls_zwm011-ultimo_bin      = ls_zwm028_c-pre_pick.
    ELSEIF NOT ls_zwm028_c-st_dck IS INITIAL.
      ls_zwm011-ultimo_tipo_dep = ls_zwm028_c-st_dck .
      ls_zwm011-ultimo_bin      = ls_zwm028_c-porta.
    ELSEIF NOT ls_zwm028_c-st_pul IS INITIAL.
      ls_zwm011-ultimo_tipo_dep = ls_zwm028_c-st_pul.

      SELECT SINGLE * FROM zwm001
                      INTO ls_zwm001_c
                      WHERE armazem = ls_ltak_c-lgnum AND
                            processo = 'PULMAO' AND
                            parametro = 'MAX_PAL'.

      IF ls_zwm028_c-paletes_pulmao <= ls_zwm001_c-valor.
        ls_zwm011-ultimo_bin = ls_zwm028_c-pulmao1.
      ELSE.
        ls_zwm011-ultimo_bin = ls_zwm028_c-pulmao2.
      ENDIF.
    ENDIF.

    MODIFY zwm011 FROM ls_zwm011.
    COMMIT WORK AND WAIT.

    CALL FUNCTION 'ZWM_CALL_TASK_RET'
      EXPORTING
        armazem    = lgnum
        tab_zwm011 = ls_zwm011
        primeira   = 'X'
        onlyone    = 'X'.
  ENDDO.


**********************************************************************
  CLEAR: cursorfield, sscc.
  LEAVE TO SCREEN '0001'.
ENDMODULE.                 " USER_COMMAND_0002  INPUT
