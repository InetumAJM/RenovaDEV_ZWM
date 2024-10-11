*&---------------------------------------------------------------------*
*&      Module  EXIT7  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit7 INPUT.

  CLEAR: ok_code_0007,cursorfield.
  CLEAR : numero_transporte.
  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT7  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0007  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0007 INPUT.

  CASE ok_code_0007.
    WHEN 'CONFIRMAR'.
      CLEAR: ok_code_0007.
      MOVE vttk-tknum TO numero_transporte.
      SET PARAMETER ID 'TNR' FIELD numero_transporte.
      CALL TRANSACTION 'VT02N' AND SKIP FIRST SCREEN.
      CLEAR numero_transporte.
      SET SCREEN '0000'.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0007  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT8  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit8 INPUT.
  CLEAR: doc_compra,matricula,tipo_camiao,transportador,
          observacao,ok_code_0003,cursorfield,nome_transp.

  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT8  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_TALAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_talao INPUT.

  CHECK NOT talao IS INITIAL.

  SELECT SINGLE * FROM zwm005
    WHERE num_entrada = talao AND finalizada = ' ' .
  IF sy-subrc <> 0.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '056' WITH talao.
    CLEAR talao.
    MOVE 'TALAO' TO cursorfield.
    EXIT.
  ELSE.
    SELECT SINGLE *
      FROM zwm002
        WHERE armazem = zwm005-armazem AND
              porta = zwm005-porta.
    IF zwm002-estado <> 'O'.
      MESSAGE ID 'ZWMMSG001' TYPE 'I'
               NUMBER '084' WITH talao.
      CLEAR talao.
      MOVE 'TALAO' TO cursorfield.
      EXIT.

    ELSE.
      SELECT SINGLE * FROM zwm017 WHERE flag_mm_wm = ' ' .
      IF sy-subrc = 0.
        IF zwm017-num_entrada <> talao AND
           NOT zwm017-num_entrada IS INITIAL.
          MESSAGE ID 'ZWMMSG001' TYPE 'I'
                 NUMBER '057' WITH zwm017-num_entrada.
          CLEAR talao.
          MOVE 'TALAO' TO cursorfield.
          EXIT.
        ENDIF.
      ELSE.
        SELECT SINGLE * FROM zwm017
            WHERE num_entrada = talao AND flag_mm_wm = 'T' .
        IF sy-subrc = 0.
          MESSAGE ID 'ZWMMSG001' TYPE 'I'
                   NUMBER '129' WITH talao.
          CLEAR talao.
          MOVE 'TALAO' TO cursorfield.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  MOVE 'CLIENTE' TO cursorfield.

ENDMODULE.                 " CHECK_TALAO  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_EBELN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ebeln INPUT.

  CHECK NOT pedido IS INITIAL.

  SELECT SINGLE *
      FROM ekko
          WHERE ebeln = pedido AND
                loekz <> 'X'.   " nao eliminado
  IF sy-subrc <> 0.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '049' WITH pedido.
    CLEAR: pedido, lote, material, item, descricao, uni, quantidade.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM zwm005
      WHERE num_entrada = talao AND
            ord_compra = pedido AND
            finalizada = ' '.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '052' WITH pedido.
    CLEAR: pedido, lote, material, item, descricao, uni, quantidade.
    EXIT.
  ELSE.
    lote = pedido.
  ENDIF.

ENDMODULE.                 " CHECK_EBELN  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0008  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0008 INPUT.

  DATA: items LIKE zwm018 OCCURS 0 WITH HEADER LINE,
        wa_zwm017 LIKE zwm017 OCCURS 0 WITH HEADER LINE,
        i_zwm017 LIKE zwm017 OCCURS 0 WITH HEADER LINE,
        i_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

  DATA: mblnr LIKE mkpf-mblnr,
        gjahr LIKE mkpf-mjahr,
        code LIKE bapi2017_gm_code,
        mov_mm TYPE bwlvs,
        mov_wm TYPE bwlvs,
        porta_desc TYPE ablad,
        to TYPE tanum,
        st_type_o TYPE lgtyp,
        st_type_d TYPE lgtyp,
        bin_destino LIKE ltap-nlpla,
        bin_origem LIKE ltap-vlpla,
        range LIKE inri-nrrangenr,
        plant TYPE werks_d,
        s_loc TYPE lgort_d,
        valor(20),
        text1(40).

  REFRESH: items, wa_zwm017, i_zwm017, i_sscc.

  PERFORM user_own_data.

  CASE ok_code_0008.
    WHEN 'SAVE'.
      CHECK NOT talao IS INITIAL AND
            NOT pedido IS INITIAL AND
            NOT material IS INITIAL AND
            NOT quantidade IS INITIAL AND
            NOT lote IS INITIAL.

      items-ebeln = pedido.
      items-ebelp = item.
      items-material = material.
      items-quantidade = quantidade.
      items-uni = uni.

      CALL FUNCTION 'ZWM_BATCH_CREATE'
        EXPORTING
          armazem           = xuser-lgnum
          material          = material
          lote              = lote
        TABLES
          return_msg        = return_msg
        EXCEPTIONS
          erro              = 1
          batch_not_created = 2
          OTHERS            = 3.
      IF sy-subrc <> 0.

        IF sy-subrc = 1.
          MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '083' WITH lote.
          CLEAR lote.
          EXIT.
        ELSEIF sy-subrc = 2.
          READ TABLE return_msg INDEX 1.
          MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER return_msg-msgnr
           WITH return_msg-msgv1.
          EXIT.

        ENDIF.
      ELSE.
        items-lote = lote.

      ENDIF.

      APPEND items.

      CLEAR valor.
      PERFORM get_parameter USING xuser-lgnum
                              'ENTRADA_TERCEIROS'
                              'CODE'
                              valor.
      MOVE valor TO code.
      CLEAR valor.
      PERFORM get_parameter USING xuser-lgnum
                              'ENTRADA_TERCEIROS'
                              'MOV'
                              valor.

      MOVE valor TO mov_mm.
      CLEAR valor.

      CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
        EXPORTING
          lgnum            = xuser-lgnum
*         PORTA            = PORTA_DESC
          code             = code
          mov_mm           = mov_mm
          testrun          = 'X'
        IMPORTING
          materialdocument = mblnr
          matdocumentyear  = gjahr
        TABLES
          return_msg       = return_msg
          items            = items
        EXCEPTIONS
          error            = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        LOOP AT return_msg WHERE msgtyp = 'E'.
          MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
                  NUMBER return_msg-msgnr
                  WITH return_msg-msgv1 return_msg-msgv2.
        ENDLOOP.
      ELSE.
*      inserir na tabela zwm017
        wa_zwm017-armazem = xuser-lgnum.
        wa_zwm017-num_entrada = talao.
        wa_zwm017-ebeln = pedido.
        wa_zwm017-ebelp = item.
        wa_zwm017-material = material.
        wa_zwm017-quantidade = quantidade.
        wa_zwm017-uni = uni.
        wa_zwm017-lote = lote.
        wa_zwm017-flag_mm_wm = 'M'.
        APPEND wa_zwm017.
        INSERT INTO zwm017 VALUES wa_zwm017.
        IF sy-subrc <> 0.
         MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '055' WITH pedido talao.
          CLEAR: pedido, talao.
          EXIT.
        ELSE.
          COMMIT WORK.
        ENDIF.
      ENDIF.
      REFRESH return_msg.
      CLEAR: pedido,item, material, descricao, quantidade, uni,
             ok_code_0008, lote.
      SET SCREEN '0008'.
      LEAVE SCREEN.

    WHEN 'CREATE_OT'.
      CLEAR: code, mov_mm, mov_wm.
      IF talao IS INITIAL.
        MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '059' WITH talao.
      ELSE.
        SELECT * APPENDING CORRESPONDING FIELDS OF TABLE i_zwm017
          FROM zwm017 WHERE num_entrada = talao.

        IF i_zwm017[] IS INITIAL.
          MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '053'.
        ELSE.
          CALL SCREEN '0012' STARTING AT  30 10 ENDING AT 105 20.

          REFRESH i_zwm017.

          MOVE 'PORTA' TO i_sscc-material.
          MOVE '1' TO i_sscc-quantidade.
          MOVE 'UN' TO i_sscc-uni.
*         MOVE '1990026116' TO i_sscc-lote_producao.
          i_sscc-tipo_su = 'P6'."por defeito pq nao interessa o tipo
          APPEND i_sscc.

          READ TABLE i_sscc INDEX 1.
          CALL FUNCTION 'ZWM_GET_MATERIAL_PLANT_SLOC'
            EXPORTING
              warehouse    = xuser-lgnum
              material     = i_sscc-material
            IMPORTING
              plant        = plant
              s_loc        = s_loc
            TABLES
              return_msg   = return_msg
            EXCEPTIONS
              not_found    = 1
              indetermined = 2
              OTHERS       = 3.
          IF sy-subrc <> 0.
            LOOP AT return_msg WHERE msgtyp = 'E'.
              MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
                      NUMBER return_msg-msgnr
                      WITH return_msg-msgv1 return_msg-msgv2.
            ENDLOOP.
          ENDIF.

          REFRESH return_msg.

          CLEAR valor.
          PERFORM get_parameter USING xuser-lgnum
                      'ENTRADA_TERCEIROS'
                      'MOV_WM'
                      mov_wm.

          MOVE valor TO mov_mm.
          CLEAR valor.

          SELECT SINGLE porta INTO porta_desc
                FROM zwm005
                    WHERE armazem = xuser-lgnum AND
                          num_entrada = talao AND
                          finalizada = ' '.
*         MOVE PORTA_DESC+8(2) TO PORTA_AUX.
          CONCATENATE '000-000-' porta_desc+1(2) INTO bin_origem.
          CONCATENATE '000-000-' porta_desc+1(2) INTO bin_destino.

          SELECT SINGLE lzone INTO certificado
              FROM lagp WHERE lgnum = xuser-lgnum AND
                              lgtyp = 'DCK' AND
                              lgpla = bin_origem.

          CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
            EXPORTING
              warehouse   = xuser-lgnum
              mov_type    = mov_wm
*             ST_TYPE_O   = ST_TYPE_O
              bin_origem  = bin_origem
*             ST_TYPE_D   = ST_TYPE_D
              bin_destino = bin_destino
              plant       = plant
              s_loc       = s_loc
              certificado = certificado
            IMPORTING
              to          = to
            TABLES
              return_msg  = return_msg
              sscc        = i_sscc
            EXCEPTIONS
              error       = 1
              OTHERS      = 2.
          IF sy-subrc <> 0.
            LOOP AT return_msg WHERE msgtyp = 'E'.
              MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
                      NUMBER return_msg-msgnr
                      WITH return_msg-msgv1 return_msg-msgv2.
            ENDLOOP.
          ELSE.
            UPDATE zwm017 SET flag_mm_wm = 'T'
                WHERE num_entrada = talao.
            COMMIT WORK.

            SUBMIT zwmrep0009 WITH p_armaz = xuser-lgnum
                              WITH p_talao = talao
                              AND RETURN.
            CLEAR talao.
          ENDIF.
        ENDIF.
*      ENDIF.
      ENDIF.
      CLEAR ok_code_0008.
      SET SCREEN '0008'.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0008  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_talao  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_talao INPUT.
  DATA zzwm005 LIKE zwm005 OCCURS 0 WITH HEADER LINE.
  DATA index2 TYPE sy-tabix.

* CHECK NOT TALAO IS INITIAL.
  REFRESH zzwm005.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE zzwm005
      FROM zwm005 WHERE finalizada = ' ' AND porta <> '   ' AND
                        ord_compra <> '          '.

  LOOP AT zzwm005.
    SELECT SINGLE *
      FROM zwm002
        WHERE armazem = zzwm005-armazem AND porta = zzwm005-porta.
    IF zwm002-estado <> 'O'.
      DELETE zzwm005 INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
  CLEAR index2.

  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
    EXPORTING
*     CUCOL                               = 20
      curow                               = 10
*     DISPLAY                             = ' '
      selectfield                         = 'ARMAZEM'
      tablename                           = 'ZWM005'
      given_value                         = ' '
*     SHOW_ALL_VALUES_AT_FIRST_TIME       = ' '
      titel                               = 'Dados de Descarga'
   IMPORTING
      ind                                 = index2
    TABLES
      full_table                          = zzwm005
   EXCEPTIONS
     no_tablefields_in_dictionary        = 1
     no_tablestructure_given             = 2
     more_then_one_selectfield           = 3
     no_selectfield                      = 4
     OTHERS                              = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE zzwm005 INDEX index2.
    IF sy-subrc = 0.
      talao = zzwm005-num_entrada.
    ENDIF.
  ENDIF.
ENDMODULE.                 " HELP_talao  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_MATNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr INPUT.

  DATA zzwm019 LIKE zwm019  OCCURS 0 WITH HEADER LINE.
  DATA index1 TYPE sy-tabix.

  CHECK NOT material IS INITIAL.
  REFRESH zzwm019.

  SELECT *
      FROM ekpo
          WHERE ebeln = pedido AND
                matnr = material AND
                loekz <> 'X' AND " nao eliminado
                elikz <> 'X'.     " codigo de remessa final
    IF sy-subrc = 0.
      MOVE ekpo-ebeln TO zzwm019-ebeln.
      MOVE ekpo-ebelp TO zzwm019-ebelp.
      MOVE ekpo-matnr TO zzwm019-matnr.
      MOVE ekpo-meins TO zzwm019-uni.
      APPEND zzwm019.
    ENDIF.
  ENDSELECT.
  IF zzwm019[] IS INITIAL.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '085' WITH material pedido.
    CLEAR: material, item, descricao, uni.
    EXIT.
  ENDIF.

  READ TABLE zzwm019 INDEX index1.
  IF sy-subrc = 0.
    SELECT SINGLE maktx INTO descricao
        FROM makt WHERE matnr = material.
    item = zzwm019-ebelp.
    uni = zzwm019-uni.
  ENDIF.

ENDMODULE.                 " CHECK_MATNR  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT9  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit9 INPUT.

  IF doc_compra1 IS INITIAL.

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

  ENDIF.

  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT9  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0009  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0009 INPUT.

  CASE ok_code_0009.

    WHEN 'CONFIRMAR'.
** Actualização da tabela global ORDENS com todas as ordens de compra
** que estão associadas à chegada do camião de descarga
      PERFORM actualiza_ordens.

      MOVE 'MATRICULA' TO cursorfield.

      SET SCREEN '0000'.
      LEAVE SCREEN.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0009  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT11  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit11 INPUT.

  CLEAR: doc_compra,matricula,tipo_camiao,transportador,
          observacao,ok_code_0003,cursorfield,nome_transp,
          doc_renova, cliente.

  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT11  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_MATNR_REMESSA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr_remessa INPUT.

  PERFORM check_material_remessa.

ENDMODULE.                 " CHECK_MATNR_REMESSA  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0011  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0011 INPUT.

  CLEAR: mblnr, gjahr, code, mov_mm, mov_wm, porta_desc, to,
         st_type_o, st_type_d, bin_destino, bin_origem, range,
         plant, s_loc.

  CLEAR: items, wa_zwm017, i_zwm017, i_sscc, zwm025.
  REFRESH: items, wa_zwm017, i_zwm017, i_sscc.

  PERFORM user_own_data.

  CASE ok_code_0011.

** Registo das entradas de devolução
    WHEN 'SAVE'.
      CHECK NOT cliente IS INITIAL AND
            NOT material IS INITIAL AND
            NOT quantidade IS INITIAL AND
            NOT uni IS INITIAL AND
            NOT lote IS INITIAL.

** Actualização da tabela zwm025 que contém as entradas teóricas
** de uma devolução
      zwm025-armazem = xuser-lgnum.
      zwm025-cliente = cliente.
      zwm025-material = material.
      zwm025-data = sy-datum.
      GET TIME.
      zwm025-hora = sy-uzeit.
      zwm025-quantidade = quantidade.
      zwm025-unidade = uni.
      zwm025-lote = lote.
*      IF NOT factura_renova IS INITIAL.
*        zwm025-factura_renova = factura_renova.
*      ENDIF.
      IF NOT doc_renova IS INITIAL.
        PERFORM converte_formato_interno USING doc_renova
                               CHANGING l_vbeln.

        zwm025-factura_renova = l_vbeln.
      ENDIF.
      CLEAR zwm025-finalizada.

      zwm025-cod_dev = cod_dev.
      zwm025-cod_motivo = cod_mot.

      MODIFY zwm025.
      COMMIT WORK.

      CLEAR : material, quantidade, uni, lote,
              doc_renova, descricao, cliente, descricao_cliente,
              cod_dev, l_text, cod_mot, motivo,
              cursorfield, ok_code_0011.

      MOVE 'CLIENTE' TO cursorfield.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0011  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0012  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0012 INPUT.
  DATA wazwm017 LIKE zwm017 OCCURS 0 WITH HEADER LINE.

  CASE ok_code_0012.
    WHEN 'OK'.
*     inserir na tabela zwm017
      IF NOT material1 IS INITIAL.

        PERFORM get_parameter USING xuser-lgnum
                          'ENTRADA_TERCEIROS'
                          material1
                          valor.

        wazwm017-armazem = xuser-lgnum.
        MOVE valor TO wazwm017-ebeln.
        MOVE valor+5(5) TO wazwm017-ebelp.
        wazwm017-num_entrada = talao.
        wazwm017-material = material1.
        wazwm017-quantidade = qtd1.
        wazwm017-uni = uni1.
        CLEAR wazwm017-lote.
        wazwm017-flag_mm_wm = 'T'.
        APPEND wazwm017.
        CLEAR wazwm017.
      ENDIF.

      IF NOT material2 IS INITIAL.

        PERFORM get_parameter USING xuser-lgnum
                          'ENTRADA_TERCEIROS'
                          material2
                          valor.

        wazwm017-armazem = xuser-lgnum.
        MOVE valor TO wazwm017-ebeln.
        MOVE valor+5(5) TO wazwm017-ebelp.
        wazwm017-num_entrada = talao.
        wazwm017-material = material2.
        wazwm017-quantidade = qtd2.
        wazwm017-uni = uni2.
        CLEAR wazwm017-lote.
        wazwm017-flag_mm_wm = 'T'.
        APPEND wazwm017.
        CLEAR wazwm017.
      ENDIF.

      IF NOT material3 IS INITIAL.

        PERFORM get_parameter USING xuser-lgnum
                          'ENTRADA_TERCEIROS'
                          material3
                          valor.

        wazwm017-armazem = xuser-lgnum.
        MOVE valor TO wazwm017-ebeln.
        MOVE valor+5(5) TO wazwm017-ebelp.
        wazwm017-num_entrada = talao.
        wazwm017-material = material3.
        wazwm017-quantidade = qtd3.
        wazwm017-uni = uni3.
        CLEAR wazwm017-lote.
        wazwm017-flag_mm_wm = 'T'.
        APPEND wazwm017.
        CLEAR wazwm017.
      ENDIF.

      MODIFY zwm017 FROM TABLE wazwm017.
      COMMIT WORK.
      SET SCREEN '0000'.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0013  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_MATNR1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr1 INPUT.

  CHECK NOT material1 IS INITIAL.

  SELECT SINGLE *
    FROM mara
        WHERE matnr = material1 AND mtart = 'PALT'.
  IF sy-subrc <> 0.
    MESSAGE i077 WITH material1.
  ELSE.
    SELECT SINGLE maktx INTO des1 FROM makt WHERE matnr = material1.
    uni1 = mara-meins.
  ENDIF.
ENDMODULE.                 " CHECK_MATNR1  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_MATNR2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr2 INPUT.

  CHECK NOT material2 IS INITIAL.

  SELECT SINGLE *
    FROM mara
        WHERE matnr = material2 AND mtart = 'PALT'.
  IF sy-subrc <> 0.
    MESSAGE i077 WITH material2.
  ELSE.
    SELECT SINGLE maktx INTO des2 FROM makt WHERE matnr = material2.
    uni2 = mara-meins.
  ENDIF.
ENDMODULE.                 " CHECK_MATNR2  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_MATNR1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr3 INPUT.

  CHECK NOT material3 IS INITIAL.

  SELECT SINGLE *
    FROM mara
        WHERE matnr = material3 AND mtart = 'PALT'.
  IF sy-subrc <> 0.
    MESSAGE i077 WITH material3.
  ELSE.
    SELECT SINGLE maktx INTO des3 FROM makt WHERE matnr = material3.
    uni3 = mara-meins.
  ENDIF.
ENDMODULE.                 " CHECK_MATNR3  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIt12  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit12 INPUT.
  SET SCREEN '0000'.
  LEAVE SCREEN.
ENDMODULE.                 " EXIt12  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_cliente  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_cliente INPUT.

  PERFORM check_cliente.

ENDMODULE.                 " check_cliente  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_quantidade  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_quantidade INPUT.

  CHECK NOT quantidade IS INITIAL.

  MOVE 'COD_DEV' TO cursorfield.

ENDMODULE.                 " check_quantidade  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_doc_renova  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_doc_renova INPUT.

  PERFORM check_remessa.

ENDMODULE.                 " check_factura_renova  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_lote_dev  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_lote_dev INPUT.

  PERFORM check_lote_dev.

ENDMODULE.                 " check_lote_dev  INPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT13  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit13 INPUT.

  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT13  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_talao  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_talao_fab1 INPUT.

* CHECK NOT TALAO IS INITIAL.
  REFRESH zzwm005.
  CLEAR index2.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE zzwm005
      FROM zwm005 WHERE finalizada = ' ' AND porta <> '   ' AND
                        ord_compra = '          '.

  LOOP AT zzwm005.
    SELECT SINGLE *
      FROM zwm002
        WHERE armazem = zzwm005-armazem AND porta = zzwm005-porta.
    IF zwm002-estado <> 'O'.
      DELETE zzwm005 INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
  CLEAR index2.

  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
    EXPORTING
*     CUCOL                               = 20
      curow                               = 10
*     DISPLAY                             = ' '
      selectfield                         = 'ARMAZEM'
      tablename                           = 'ZWM005'
      given_value                         = ' '
*     SHOW_ALL_VALUES_AT_FIRST_TIME       = ' '
     titel                               = 'Dados de Descarga Fabrica 1'
   IMPORTING
     ind                                 = index2
    TABLES
      full_table                          = zzwm005
   EXCEPTIONS
     no_tablefields_in_dictionary        = 1
     no_tablestructure_given             = 2
     more_then_one_selectfield           = 3
     no_selectfield                      = 4
     OTHERS                              = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE zzwm005 INDEX index2.
    IF sy-subrc = 0.
      talao = zzwm005-num_entrada.
    ENDIF.
  ENDIF.
ENDMODULE.                 " HELP_talao_fab1  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_TALAO_FAB1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_talao_fab1 INPUT.

  CHECK NOT talao IS INITIAL.

  SELECT SINGLE * FROM zwm005
    WHERE num_entrada = talao AND finalizada = ' '
           AND porta <> '   '
           AND ord_compra = '          '.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '056' WITH talao.
    CLEAR: talao, deposito.
    MOVE 'TALAO' TO cursorfield.
    EXIT.
  ELSE.
    SELECT SINGLE *
      FROM zwm002
        WHERE armazem = zwm005-armazem AND
              porta = zwm005-porta.
    IF zwm002-estado <> 'O'.
      MESSAGE ID 'ZWMMSG001' TYPE 'I'
               NUMBER '084' WITH talao.
      CLEAR: talao, deposito.
      MOVE 'TALAO' TO cursorfield.
      EXIT.

    ENDIF.
  ENDIF.
  MOVE 'CLIENTE' TO cursorfield.

ENDMODULE.                 " CHECK_TALAO_fab1  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_deposito  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_deposito INPUT.

  DATA t_zwm032 LIKE zwm032.

  CHECK NOT deposito IS INITIAL.

  SELECT SINGLE *
      FROM t001l
          WHERE werks = 'RENV' AND
                lgort = deposito.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZWMMSG001' TYPE 'I'
        NUMBER '157' WITH deposito 'RENV'.
    CLEAR deposito.
    MOVE 'DEPOSITO' TO cursorfield.
    EXIT.
  ENDIF.

ENDMODULE.                 " check_deposito  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0013  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0013 INPUT.

  CHECK NOT talao IS INITIAL AND
        NOT deposito IS INITIAL.

  REFRESH: items, wa_zwm017, i_zwm017, i_sscc.

  CLEAR:mov_mm,
        mov_wm,
        to,
        st_type_o,
        st_type_d,
        bin_destino,
        bin_origem,
        plant,
        s_loc,
        valor,
        text1.

  CASE ok_code_0013.

    WHEN 'CREATE_OT'.
      CLEAR: code, mov_mm, mov_wm.
      IF talao IS INITIAL.
        MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '059' WITH talao.
      ELSE.

        MOVE 'PORTA' TO i_sscc-material.
        MOVE '1' TO i_sscc-quantidade.
        MOVE 'UN' TO i_sscc-uni.
*       MOVE '1990026116' TO i_sscc-lote_producao.
        i_sscc-tipo_su = 'P6'."por defeito pq nao interessa o tipo
        APPEND i_sscc.

        READ TABLE i_sscc INDEX 1.
        CALL FUNCTION 'ZWM_GET_MATERIAL_PLANT_SLOC'
          EXPORTING
            warehouse    = xuser-lgnum
            material     = i_sscc-material
          IMPORTING
            plant        = plant
            s_loc        = s_loc
          TABLES
            return_msg   = return_msg
          EXCEPTIONS
            not_found    = 1
            indetermined = 2
            OTHERS       = 3.
        IF sy-subrc <> 0.
          LOOP AT return_msg WHERE msgtyp = 'E'.
            MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
                    NUMBER return_msg-msgnr
                    WITH return_msg-msgv1 return_msg-msgv2.
          ENDLOOP.
        ENDIF.

        REFRESH return_msg.

        CLEAR valor.
        PERFORM get_parameter USING xuser-lgnum
                    'ENTRADA_FABRICA1'
                    'MOV_WM'
                    mov_wm.

        MOVE valor TO mov_mm.
        CLEAR valor.

        SELECT SINGLE porta INTO porta_desc
              FROM zwm005
                  WHERE armazem = xuser-lgnum AND
                        num_entrada = talao AND
                        finalizada = ' '.
*       MOVE PORTA_DESC+8(2) TO PORTA_AUX.
        CONCATENATE '000-000-' porta_desc+1(2) INTO bin_origem.
        CONCATENATE '000-000-' porta_desc+1(2) INTO bin_destino.

        SELECT SINGLE *
            FROM ltap
                WHERE lgnum = xuser-lgnum AND
                      vltyp = 'DCK' AND
                      nlpla = bin_origem AND
                      nltyp = 'DCK' AND
                      vlpla = bin_destino AND
                      pquit = ' '.

        IF sy-subrc = 0.
          MESSAGE ID 'ZWMMSG001' TYPE 'I'
           NUMBER '092' WITH porta_desc.
          CLEAR: talao, deposito.
          MOVE 'TALAO' TO cursorfield.
          EXIT.
        ENDIF.

        SELECT SINGLE lzone INTO certificado
            FROM lagp WHERE lgnum = xuser-lgnum AND
                            lgtyp = 'DCK' AND
                            lgpla = bin_origem.

        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
          EXPORTING
            warehouse   = xuser-lgnum
            mov_type    = mov_wm
            bin_origem  = bin_origem
            bin_destino = bin_destino
            plant       = plant
            s_loc       = s_loc
            certificado = certificado
          IMPORTING
            to          = to
          TABLES
            return_msg  = return_msg
            sscc        = i_sscc
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.
        IF sy-subrc <> 0.
          LOOP AT return_msg WHERE msgtyp = 'E'.
            MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
                    NUMBER return_msg-msgnr
                    WITH return_msg-msgv1 return_msg-msgv2.
          ENDLOOP.
        ELSE.
          t_zwm032-armazem = xuser-lgnum.
          t_zwm032-talao = talao.
          t_zwm032-deposito = deposito.
          t_zwm032-ot = to.
          INSERT INTO zwm032 VALUES t_zwm032.
          IF sy-subrc = 0.
            COMMIT WORK.
          ELSE.
            ROLLBACK WORK.
          ENDIF.
        ENDIF.

      ENDIF.
      CLEAR: ok_code_0013, talao, deposito.
      SET SCREEN '0013'.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0013  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_cod_devol  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_cod_devol INPUT.

  PERFORM check_cod_dev.

ENDMODULE.                 " check_cod_devol  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_cod_mot  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_cod_mot INPUT.

  PERFORM check_cod_mot.

ENDMODULE.                 " check_cod_mot  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_material INPUT.

  REFRESH zzwm019.

  SELECT *
      FROM ekpo
          WHERE ebeln = pedido AND
*               matnr = material AND
                loekz <> 'X' AND " nao eliminado
                elikz <> 'X'.     " codigo de remessa final
    IF sy-subrc = 0.
      MOVE ekpo-ebeln TO zzwm019-ebeln.
      MOVE ekpo-ebelp TO zzwm019-ebelp.
      MOVE ekpo-matnr TO zzwm019-matnr.
      MOVE ekpo-meins TO zzwm019-uni.
      APPEND zzwm019.
    ENDIF.
  ENDSELECT.

  CLEAR index1.

  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
    EXPORTING
      curow                        = 10
      selectfield                  = 'MATNR'
      tablename                    = 'ZWM019'
      given_value                  = ' '
      titel                        = 'Dados do Pedido'
    IMPORTING
      ind                          = index1
    TABLES
      full_table                   = zzwm019
    EXCEPTIONS
      no_tablefields_in_dictionary = 1
      no_tablestructure_given      = 2
      more_then_one_selectfield    = 3
      no_selectfield               = 4
      OTHERS                       = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE zzwm019 INDEX index1.
    IF sy-subrc = 0.
      SELECT SINGLE maktx INTO descricao
          FROM makt WHERE matnr = zzwm019-matnr.
      item = zzwm019-ebelp.
      uni = zzwm019-uni.
      material = zzwm019-matnr.
    ENDIF.
  ENDIF.

  SELECT SINGLE * FROM zwm017
    WHERE num_entrada = talao AND
          ebeln = pedido AND
          ebelp = item.
  IF sy-subrc = 0.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '058' WITH pedido item talao.
    CLEAR: pedido, item, descricao, uni, material.
    EXIT.
  ENDIF.

ENDMODULE.                 " HELP_MATERIAL  INPUT

*&---------------------------------------------------------------------*
*&      Module  f4_material  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_material INPUT.

  PERFORM get_f4_material.

ENDMODULE.                 " f4_material  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_dados_globais  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_dados_globais INPUT.

  PERFORM check_material_lote_remessa.

ENDMODULE.                 " check_dados_globais  INPUT
