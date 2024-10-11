*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1F05 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save .


*      confirma a to.
  CLEAR return_msg.
  REFRESH return_msg.
  CALL FUNCTION 'ZWM_CONFIRM_ALL_ITEMS_TO'
    EXPORTING
      armazem              = xuser-lgnum
      confirm_type         = 'T'
    TABLES
      return_msg           = return_msg
    CHANGING
      to                   = to_terceiros
    EXCEPTIONS
      confirm_type_error   = 1
      to_not_found         = 2
      to_already_confirmed = 3
      to_already_picked    = 4
      to_not_picked        = 5
      wrong_su             = 6
      missing_su           = 7
      error                = 8
      OTHERS               = 9.
  IF sy-subrc <> 0.
    READ TABLE return_msg INDEX 1.
    IF sy-subrc = 0 AND return_msg-msgtyp <> 'W'.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = return_msg-msgid
          message_lang   = sy-langu
          message_type   = return_msg-msgtyp
          message_number = return_msg-msgnr
          message_var1   = return_msg-msgv1
          message_var2   = return_msg-msgv2
          message_var3   = return_msg-msgv3.

      CLEAR ok_code_0013.
      SET SCREEN '0000'.LEAVE SCREEN.
    ENDIF.

  ELSE.
*PROCESSAMENTO OK
** Actualizar a descarga finalizada
    UPDATE zwm016
               SET finalizada = 'X'
                 WHERE armazem = xuser-lgnum
                   AND user_name = sy-uname
                   AND porta = porta1.
*                       pulmao = pulmao2.

*actualizar tabela ZWM011 com STATUS T pq é TO
    CALL FUNCTION 'ZWM_MODIFY_ZWM011'
      EXPORTING
        armazem            = xuser-lgnum
        to_number          = to_terceiros
        to_item            = '0001'
        status             = 'T'
      EXCEPTIONS
        error_update_table = 1
        OTHERS             = 2.

** CRIAR UMA TO da tabela zwm013
    PERFORM cria_to.

  ENDIF.


** Retirar da tabela ZWM002 o user_name do operador
** para poder controlar mais paletes em outra porta
** caso existam
  CLEAR: l_porta.
  l_porta = porta1+12(2).

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_porta
    IMPORTING
      output = l_porta.

  UPDATE zwm002 SET user_name = ' '
          WHERE armazem = xuser-lgnum AND
                porta   = l_porta.
*                pulmao_1 = pulmao2+4(10).


* chamar uma nova tarefa
  MOVE 'SU' TO cursorfield.


  CLEAR: pack_material, quantidade, unidade, material,
         lote,plant, lgort, tipo_palete, su, paletes_dif,
         pulmao2,pedido,item,ped_compra,posicao_pulmao,kober.
  CLEAR : tab_zwm011, return_msg.

  REFRESH : return_msg.
** Não sei se será preciso fazer isto de NOVO ????
  SELECT * FROM zwm010 INTO TABLE tab_zwm010
  WHERE armazem = xuser-lgnum AND
        equipamento = equipamento_.

  CALL FUNCTION 'ZWM_GET_TO_RET'
    EXPORTING
      armazem           = xuser-lgnum
      tamanho           = lrf_wkqu-devty(5)
    IMPORTING
      nova_to           = tab_zwm011
      tipo_queue        = tipo
    TABLES
      l_zwm010          = tab_zwm010
      return_msg        = return_msg
    EXCEPTIONS
      no_equipment      = 1
      no_work_available = 2
      OTHERS            = 3.
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
      CLEAR equipamento_.
** Para saltar para o ecrã correcto
      f3_activo = 'X'.
      IF lrf_wkqu-devty(5) = '16X20'.
        SET SCREEN '0001'.LEAVE SCREEN.
      ELSE.
        SET SCREEN '0002'.LEAVE SCREEN.
      ENDIF.
    ENDIF.

  ELSE.
** Atribuição de nova tarefa
    CALL FUNCTION 'ZWM_CALL_TASK_RET'
      EXPORTING
        armazem     = xuser-lgnum
        ecran       = ecran
        tab_zwm011  = tab_zwm011
        tipo_queue  = tipo
        equipamento = equipamento_.
  ENDIF.

ENDFORM.                    " SAVE


*&---------------------------------------------------------------------*
*&      Form  cria_doc_material_paletes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_doc_material_paletes .

  DATA: l_ebeln LIKE ekpo-ebeln,
        l_ebelp LIKE ekpo-ebelp,
        l_msg   LIKE bdcmsgcoll-msgv1,
        l_matnr LIKE mara-matnr,
        l_meins LIKE mara-meins,
        l_code  TYPE bapi2017_gm_code,
        l_mov   TYPE bwlvs.

** Tabelas auxiliares
  DATA : BEGIN OF l_zwm023 OCCURS 0.
           INCLUDE STRUCTURE zwm023.
         DATA : END OF l_zwm023.

  DATA: itab_items LIKE zwm018 OCCURS 0 WITH HEADER LINE.

** Carregar a informação da tabela zwm023 por armazem/user
  SELECT * FROM zwm023 INTO TABLE l_zwm023
           WHERE armazem = xuser-lgnum AND
                 user_name = sy-uname.
  IF sy-subrc = 0.
** Obter o CODE para dar entrada
    PERFORM get_parameter USING xuser-lgnum
                             'ENTRADA_TERCEIROS'
                             'CODE'
                             l_code.

** Obter o MOVIMENTO para dar entrada
    PERFORM get_parameter USING xuser-lgnum
                             'ENTRADA_TERCEIROS'
                             'MOV'
                             l_mov.

    LOOP AT l_zwm023.

** Inicia variaveis/tabelas internas
      FREE: itab_items.
      CLEAR: ekpo, ekko, l_ebeln, l_ebelp, itab_items,
             l_matnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = l_zwm023-matnr
        IMPORTING
          output = l_matnr.

** Obter o MOVIMENTO para dar entrada
      IF l_matnr = 'PEURO'.
        PERFORM get_parameter USING xuser-lgnum
                                 'ENTRADA_TERCEIROS'
                                 'MOV_EM'
                                 l_mov.

      ELSE.
** Obter o pedido mais antigo
        SELECT * FROM ekpo
        WHERE matnr EQ l_matnr
          AND elikz NE 'X'
          AND loekz EQ ' '.
          l_ebeln = ekpo-ebeln.
          l_ebelp = ekpo-ebelp.
          EXIT.
        ENDSELECT.
      ENDIF.

      IF l_ebeln IS INITIAL AND
         l_matnr NE 'PEURO'.
*      IF sy-subrc NE 0.
        l_msg = l_zwm023-matnr.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '169'
            message_var1   = l_msg.
        sy-subrc = 4.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = l_zwm023-ebeln
          IMPORTING
            output = l_zwm023-ebeln.

** Obter a UMB
        CLEAR: mara.
        SELECT SINGLE meins FROM mara
        INTO l_meins
        WHERE matnr EQ l_matnr.

** Criar tabela de Items
        itab_items-armazem    = l_zwm023-armazem.
        itab_items-material   = l_zwm023-matnr.
        itab_items-quantidade = l_zwm023-quantidade.
        itab_items-uni        = l_meins.
        itab_items-ebeln      = l_ebeln.
        itab_items-ebelp      = l_ebelp.
        APPEND itab_items.

** Dar entrada Palete
        CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
          EXPORTING
            lgnum        = xuser-lgnum
            code         = l_code
            mov_mm       = l_mov
            linhas_zero  = 'X'
            nota_remessa = l_ebeln
          TABLES
            return_msg   = return_msg
            items        = itab_items
          EXCEPTIONS
            error        = 1
            OTHERS       = 2.

        IF sy-subrc <> 0.
          READ TABLE return_msg INDEX 1.

          CLEAR: wa_log.

          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = return_msg-msgid
              msgnr               = return_msg-msgnr
              msgv1               = return_msg-msgv1
              msgv2               = return_msg-msgv2
              msgv3               = return_msg-msgv3
              msgv4               = return_msg-msgv4
            IMPORTING
              message_text_output = wa_log-msg.

          GET TIME.
          wa_log-data     = sy-datum.
          wa_log-processo = 'ENTRADA_TERCEIROS'.
          wa_log-hora     = sy-uzeit.
          wa_log-retorno  = '98'.
          MODIFY zwm_log_efacec FROM wa_log.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ELSE.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = 'Sem pedido na tabela ZWM023'.
    sy-subrc = 4.
  ENDIF.

ENDFORM.                    " cria_doc_material_paletes


*&---------------------------------------------------------------------*
*&      Form  cria_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_to .

  break roffd.

  DATA : req_type TYPE lvs_betyp,
         l_pulmao LIKE zwm013-destino.
  DATA : dest_bin LIKE ltap-nlpla.
  DATA x_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

  CLEAR : req_type, l_pulmao, s_type, parametro.

* Fazer lock antes de actualizar a tabela das mensulas
  DO.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'ZWM014'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

** Posição preenchida quando existe uma incidência
** de palete estragada

  CLEAR dest_bin.

** Obter o Canal do pulmão, o primeiro a "arrumar" é o canal 1
  IF pulmao2+12(2) = '02'.
    CONCATENATE pulmao2(12) '01' INTO l_pulmao.
  ELSE.
    l_pulmao = pulmao2.
  ENDIF.

** Seleccionar dados da HU
  SELECT * FROM zwm013
                WHERE armazem = xuser-lgnum AND
                      destino = l_pulmao AND
*                      destino = pulmao2 AND
                      bloqueado = 'X' AND
                      trans_terc = 'X'
                      ORDER BY posicao_pulmao DESCENDING.
    IF sy-subrc = 0.
      EXIT.
    ENDIF.
  ENDSELECT.
  IF sy-subrc NE 0.
** Sem mais registos na posição+canal 1, lêr canal 2
** Seleccionar dados da HU

    CONCATENATE pulmao2(12) '02' INTO l_pulmao.

*    l_pulmao = pulmao2.
    CLEAR zwm013.
    SELECT * FROM zwm013
                  WHERE armazem = xuser-lgnum AND
                        destino = l_pulmao AND
*                      destino = pulmao2 AND
                        bloqueado = 'X' AND
                        trans_terc = 'X'
                        ORDER BY posicao_pulmao DESCENDING.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDSELECT.


  ENDIF.

** Só tenta criar a TO caso existam entradas para executar na
** zwm013
  IF sy-subrc = 0.

** Verificar se é uma palete remontada.

    CLEAR x_sscc.
    REFRESH x_sscc.

    CLEAR zwm020.
    SELECT SINGLE *
        FROM zwm020
            WHERE armazem = zwm013-armazem AND
                 ( p1 = zwm013-sscc OR
                   p2 = zwm013-sscc ).
    IF sy-subrc = 0.
      CLEAR x_sscc.
      REFRESH x_sscc.
      CLEAR: vekp, vepo.
      SELECT SINGLE p~vemng p~vemeh p~matnr
                    p~werks p~lgort p~charg
          INTO (x_sscc-quantidade, x_sscc-uni, x_sscc-material,
                plant, lgort, x_sscc-lote_producao)
             FROM vekp AS k INNER JOIN vepo AS p
                  ON  k~venum = p~venum
                      WHERE  p~vepos = '000001' AND
                             k~exidv = zwm020-p1.

      x_sscc-sscc = zwm020-p1.
      SELECT SINGLE tipo_palete INTO x_sscc-tipo_su
          FROM zwm013
            WHERE armazem = xuser-lgnum AND
                  sscc = zwm020-p1.
      APPEND x_sscc.
      CLEAR x_sscc.

      SELECT SINGLE p~vemng p~vemeh p~matnr
                    p~werks p~lgort p~charg
            INTO (x_sscc-quantidade, x_sscc-uni, x_sscc-material,
                  plant, lgort,x_sscc-lote_producao)
               FROM vekp AS k INNER JOIN vepo AS p
                    ON  k~venum = p~venum
                        WHERE  p~vepos = '000001' AND
                               k~exidv = zwm020-p2.

      x_sscc-sscc = zwm020-p2.
      SELECT SINGLE tipo_palete INTO x_sscc-tipo_su
          FROM zwm013
              WHERE armazem = xuser-lgnum AND
                    sscc = zwm020-p1.
      APPEND x_sscc.
      CLEAR x_sscc.


    ELSE.
** Carregar dados da VEKP e VEPO
      CLEAR mat_struct.
      REFRESH mat_struct.
      CLEAR: vekp, vepo.
      SELECT SINGLE p~vemng p~vemeh p~matnr
                       p~werks p~lgort p~charg
        INTO (mat_struct-menge, mat_struct-meins, mat_struct-material,
                   plant, lgort, mat_struct-charg)
                FROM vekp AS k INNER JOIN vepo AS p
                     ON  k~venum = p~venum
                         WHERE  p~vepos = '000001' AND
                                k~exidv = zwm013-sscc.
      IF sy-subrc = 0.
        APPEND mat_struct.
        CLEAR mat_struct.
      ENDIF.
    ENDIF.

**  Criar OT - Palete Simples
    CLEAR: marm, marc, parametro.
    IF NOT mat_struct[] IS INITIAL.
      READ TABLE mat_struct INDEX 1.
      SELECT SINGLE *
          FROM marc
              WHERE matnr = mat_struct-material AND
                    werks = plant.
      IF marc-maabc IS INITIAL.

      ELSE.
        SELECT SINGLE *
              FROM marm
                  WHERE matnr = mat_struct-material AND
                        meinh = 'PAL'.

        IF mat_struct-menge < marm-umrez.
          MOVE 'Z' TO parametro.
        ENDIF.

        CLEAR: valor, s_type.
        PERFORM get_parameter USING xuser-lgnum
                       'ENTRADA_TERCEIROS'
                       parametro
                       valor.
        MOVE valor TO s_type.
      ENDIF.
    ELSE.
** Palete Remontada
      READ TABLE x_sscc INDEX 1.
      SELECT SINGLE *
          FROM marc
              WHERE matnr = x_sscc-material AND
                    werks = plant.
      IF marc-maabc IS INITIAL.

      ELSE.
        CLEAR marm.
        SELECT SINGLE *
              FROM marm
                  WHERE matnr = x_sscc-material AND
                        meinh = 'PAL'.

        IF x_sscc-quantidade <> marm-umrez.
          MOVE 'Z' TO parametro.
        ENDIF.

        CLEAR: valor, s_type.
        PERFORM get_parameter USING xuser-lgnum
                       'ENTRADA_TERCEIROS'
                       parametro
                       valor.
        MOVE valor TO s_type.
      ENDIF.

    ENDIF.
** Verificar se é uma entrada de terceiros ... ou uma
** entrada vinda da fábrica 1 - movimentos de WM distintos
    IF zwm013-fabrica_1 = ' '.
      CLEAR: valor, req_type.
      PERFORM get_parameter USING xuser-lgnum
                         'ENTRADA_TERCEIROS'
                         'MOV_WM1'
                         valor.
      MOVE valor TO mov_wm.
      req_type = 'E'.
    ELSEIF zwm013-fabrica_1 = 'X'.
      CLEAR: valor, req_type.
      PERFORM get_parameter USING xuser-lgnum
                         'ENTRADA_FABRICA1'
                         'MOV_WM1'
                         valor.
      MOVE valor TO mov_wm.
      req_type = 'C'.
    ENDIF.


** Incidência da palete estragada
    IF zwm013-incidencia = '6'.
      dest_bin = '001-001-00'.
      s_type = 'INC'.
    ELSE.
      CLEAR dest_bin.
    ENDIF.

    DATA req_number TYPE lvs_benum.
    CLEAR req_number.
*    MOVE l_pulmao+4(10) TO req_number.
    MOVE zwm013-destino+4(10) TO req_number.

    CLEAR: lagp, certificado.
    SELECT SINGLE lzone INTO certificado
    FROM lagp WHERE lgnum = xuser-lgnum AND
                    lgtyp = 'PUL' AND
                    lgpla = zwm013-destino+4(10).

** Palete Simples
    IF NOT mat_struct[] IS INITIAL.

      CLEAR e_to.
** Cross Docking
*      IF s_type IS INITIAL.
*
*        DATA:  qtd LIKE ltap-vsola,
*               bin LIKE lagp-lgpla.
*
*        CLEAR: qtd, bin, e_to.
*
*        MOVE mat_struct-menge TO qtd.
*        MOVE req_number TO bin.
*
*        READ TABLE mat_struct INDEX 1.
*
*        CALL FUNCTION 'ZWM_CROSS_DOCKING'
*          EXPORTING
*            lgnum       = xuser-lgnum
*            material    = mat_struct-material
*            quantidade  = mat_struct-menge
*            uni         = mat_struct-meins
*            tipo_palete = zwm013-tipo_palete
*            su          = zwm013-sscc
*            st_origem   = '902'
*            bin_origem  = bin
*          IMPORTING
*            to          = e_to
*          EXCEPTIONS
*            error       = 1
*            OTHERS      = 2.
*        IF sy-subrc = 0.
*          UPDATE zwm013 SET bloqueado = ' '
*              WHERE destino = l_pulmao AND
*                    armazem = xuser-lgnum AND
*                    sscc = zwm013-sscc.
*          COMMIT WORK.
*        ENDIF.
*
*      ENDIF.

      IF e_to IS INITIAL.

        CALL FUNCTION 'ZWM_CREATE_STORAGE_UNIT'
          EXPORTING
            warehouse   = xuser-lgnum
            mov_type    = mov_wm
            st_type     = s_type
            req_type    = req_type
            req_number  = req_number
            plant       = plant
            s_loc       = lgort
            su_type     = zwm013-tipo_palete
            dest_bin    = dest_bin
            certificado = certificado
            mat_struct  = mat_struct
          IMPORTING
            to          = e_to
          TABLES
            result_msg  = return_msg
          CHANGING
            su_number   = zwm013-sscc
          EXCEPTIONS
            OTHERS      = 1.

        IF sy-subrc <> 0.

          CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
            EXPORTING
              mode_keyword   = 'X'
              keyword_       = 'ZWM014'
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          LOOP AT return_msg WHERE msgtyp = 'E'.
            MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
                    NUMBER return_msg-msgnr
                    WITH return_msg-msgv1 return_msg-msgv2.
          ENDLOOP.

        ELSE.

          UPDATE zwm013 SET bloqueado = ' '
              WHERE destino = l_pulmao AND
                    armazem = xuser-lgnum AND
                    sscc = zwm013-sscc.
          COMMIT WORK.

        ENDIF.
      ENDIF.
** Palete Remontada
    ELSE.

      CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
        EXPORTING
          warehouse   = xuser-lgnum
          mov_type    = mov_wm
          st_type_o   = s_type
          bin_destino = dest_bin
          plant       = plant
          s_loc       = lgort
          certificado = certificado
          req_number  = req_number
          req_type    = req_type
        IMPORTING
          to          = e_to
        TABLES
          return_msg  = return_msg
          sscc        = x_sscc
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.
      IF sy-subrc <> 0.

        CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
          EXPORTING
            mode_keyword   = 'X'
            keyword_       = 'ZWM014'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        LOOP AT return_msg WHERE msgtyp = 'E'.
          MESSAGE ID return_msg-msgid TYPE return_msg-msgtyp
                  NUMBER return_msg-msgnr
                  WITH return_msg-msgv1 return_msg-msgv2.
        ENDLOOP.

      ELSE.

        UPDATE zwm013 SET bloqueado = ' '
            WHERE destino = l_pulmao AND
                  armazem = xuser-lgnum AND
                  sscc = zwm020-p1.
        COMMIT WORK.

        UPDATE zwm013 SET bloqueado = ' '
            WHERE destino = l_pulmao AND
                  armazem = xuser-lgnum AND
                  sscc = zwm020-p2.
        COMMIT WORK.

      ENDIF.

    ENDIF.
  ENDIF.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = 'ZWM014'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  CLEAR : mat_struct,zwm013,zwm013-sscc,zwm013-incidencia,
          dest_bin,s_type,req_type, posicao_pulmao, x_sscc,
          marm, marc,kober.
  REFRESH : mat_struct, x_sscc.

ENDFORM.                    " cria_to
*&---------------------------------------------------------------------*
*&      Form  cria_doc_material_paletes_tr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_doc_material_paletes_tr .

  break roffd.

  DATA: pul1(14), pul2(14).
  DATA t_zwm013 LIKE zwm013 OCCURS 0 WITH HEADER LINE.

  DATA : BEGIN OF tab_pal OCCURS 0,
           vhilm LIKE vekp-vhilm,
           matnr LIKE vepo-matnr,
           qtd   TYPE i,
         END OF tab_pal.

  CLEAR: pul1, pul2, t_zwm013.
  REFRESH t_zwm013.

  DATA : aux_porta(3).
  CLEAR aux_porta.

  CONCATENATE '0' porta1+12(2) INTO aux_porta.

  SELECT SINGLE *
      FROM zwm002
          WHERE armazem = xuser-lgnum AND porta = aux_porta.

  CALL FUNCTION 'ZWM_CONCATENATE_BIN'
    EXPORTING
      lgtyp = 'PUL'
      lgpla = zwm002-pulmao_1
    IMPORTING
      bin   = pul1.

  CALL FUNCTION 'ZWM_CONCATENATE_BIN'
    EXPORTING
      lgtyp = 'PUL'
      lgpla = zwm002-pulmao_2
    IMPORTING
      bin   = pul2.

  SELECT * INTO TABLE t_zwm013
      FROM zwm013
      WHERE armazem = xuser-lgnum AND
           ( destino = pul1 OR
             destino = pul2 ).

  LOOP AT t_zwm013.

    SELECT SINGLE * FROM vekp WHERE exidv = t_zwm013-sscc.

    MOVE vekp-vhilm TO tab_pal-vhilm.
    MOVE 1 TO tab_pal-qtd.

    SELECT SINGLE * FROM vepo WHERE venum = vekp-venum.
    MOVE vepo-matnr TO tab_pal-matnr.
    COLLECT tab_pal.

  ENDLOOP.

  CLEAR items.
  REFRESH items.

  LOOP AT tab_pal.
    items-material = tab_pal-vhilm.
    items-quantidade = tab_pal-qtd.
    items-uni = 'PAL'.
    APPEND items.
    CLEAR items.

** INI - Entrada de Meia Palete.
    SELECT SINGLE *
        FROM mlgn
            WHERE matnr = tab_pal-matnr
              AND lgnum = xuser-lgnum
              AND ( block = '03' OR block = '04' ).
    IF sy-subrc = 0.
      SELECT SINGLE *
          FROM zwm001
              WHERE armazem = xuser-lgnum AND
                    processo = 'MEIA-PALETE' AND
                    parametro = tab_pal-vhilm.
      IF sy-subrc = 0.
        items-material = zwm001-valor.
        items-quantidade = tab_pal-qtd * 2.
        items-uni = 'PAL'.
        APPEND items.
      ENDIF.
    ENDIF.
** FIM - Entrada de Meia Palete.

** INI - Entrada de Quarto Palete.
    SELECT SINGLE *
        FROM mlgn
            WHERE matnr = tab_pal-matnr
              AND lgnum = xuser-lgnum
              AND ( block = '07' OR block = '08' ).
    IF sy-subrc = 0.
      SELECT SINGLE *
          FROM zwm001
              WHERE armazem = xuser-lgnum AND
                    processo = 'QUARTO-PALETE' AND
                    parametro = tab_pal-vhilm.
      IF sy-subrc = 0.
        items-material = zwm001-valor.
        items-quantidade = tab_pal-qtd * 4.
        items-uni = 'PAL'.
        APPEND items.
      ENDIF.
    ENDIF.
** FIM - Entrada de Quarto Palete.
  ENDLOOP.

  CLEAR valor.
  PERFORM get_parameter USING xuser-lgnum
                          'GERAL'
                          'PLANT'
                          valor.

  MOVE valor TO plant_o.
** Seleccionar o deposito de origem da transferencia
  SELECT SINGLE deposito INTO sloc_o
      FROM zwm032
          WHERE armazem = xuser-lgnum AND
                ot = to_ret.

  code = '04'.

  CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
    EXPORTING
      lgnum            = xuser-lgnum
*     AUFNR            =
*     EBELN            =
*     EBELP            =
      code             = code
*     PORTA            =
*     RECEBEDOR        =
      mov_mm           = '311'
*     TESTRUN          =
      plant_o          = plant_o
      sloc_o           = sloc_o
*     LINHAS_ZERO      =
*     NOTA_REMESSA     =
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
    READ TABLE return_msg INDEX 1.

    CLEAR: wa_log.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = return_msg-msgid
        msgnr               = return_msg-msgnr
        msgv1               = return_msg-msgv1
        msgv2               = return_msg-msgv2
        msgv3               = return_msg-msgv3
        msgv4               = return_msg-msgv4
      IMPORTING
        message_text_output = wa_log-msg.

    GET TIME.
    wa_log-data     = sy-datum.
    wa_log-processo = 'ENTRADA_FABRICA'.
    wa_log-hora     = sy-uzeit.
    wa_log-retorno  = '98'.
    MODIFY zwm_log_efacec FROM wa_log.

  ENDIF.

ENDFORM.                    " cria_doc_material_paletes_tr
*&---------------------------------------------------------------------*
*&      Form  F_IS_NEW_CALC
*&---------------------------------------------------------------------*
FORM f_is_new_calc USING i_lgnum TYPE lgnum
                   CHANGING c_new_calc TYPE abap_bool.

  SELECT mandt UP TO 1 ROWS
    FROM zwm001 INTO sy-mandt
    WHERE armazem EQ i_lgnum
      AND processo EQ 'TROCA_ORDEM_PULMAO'
      AND parametro EQ 'ACTIVA'
      AND valor EQ 'X'.
  ENDSELECT.
  IF sy-subrc EQ 0.
    c_new_calc = abap_true.
  ELSE.
    c_new_calc = abap_false.
  ENDIF.
ENDFORM.                    " F_IS_NEW_CALC
*&---------------------------------------------------------------------*
*&      Form  F_GET_LUNG_POSITION
*&---------------------------------------------------------------------*
FORM f_get_lung_position  USING i_lgnum TYPE lgnum
                                i_refnr TYPE lvs_refnr
                            CHANGING c_pos TYPE char2.

  DATA lv_max_pal TYPE numc2.
  DATA lv_pos     TYPE numc2.
  DATA lv_pul_pal TYPE numc2.
  DATA lv_pul_pos TYPE numc2.
  DATA lv_valor   TYPE zwm_valor.

  SELECT valor UP TO 1 ROWS
    FROM zwm001
    INTO lv_valor
    WHERE armazem EQ i_lgnum
      AND processo EQ 'PULMAO'
      AND parametro EQ 'MAX_PAL'.
  ENDSELECT.

  IF lv_valor IS NOT INITIAL.
    IF lv_valor(2) CO '1234567890'.
      lv_max_pal = lv_valor.

      SELECT SINGLE paletes_pulmao
        FROM zwm028
        INTO lv_pul_pal
        WHERE lgnum EQ i_lgnum
          AND refnr EQ i_refnr
          AND remessa EQ space.

      WHILE lv_pul_pal GE lv_max_pal.
        lv_pul_pal = lv_pul_pal - lv_max_pal.
      ENDWHILE.

      lv_pul_pos = lv_max_pal - lv_pul_pal.

    ENDIF.
  ELSE.
    lv_max_pal = 0.
  ENDIF.

  c_pos = lv_pul_pos.
ENDFORM.                    " F_GET_LUNG_POSITION


*&---------------------------------------------------------------------*
*&      Form  f_posicao_pulmao_skip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_LGNUM    text
*      -->I_POS      text
*      <--C_POS      text
*----------------------------------------------------------------------*
FORM f_posicao_pulmao_skip USING    i_lgnum TYPE lgnum
                                    i_pos   TYPE char2
                           CHANGING c_pos TYPE char2.

  c_pos = i_pos.

*** França
************************************************************************
*  IF i_lgnum EQ '150'.
*    c_pos = i_pos.
*    RETURN.
*  ENDIF.
*
** Desbloqueio Por Ordem de Venda e Tipo de Carro
************************************************************************
*  DO  1 TIMES.
*    CALL FUNCTION 'Z_WM_IS_SPECIAL_PICK_TRANS_OV'
*      EXPORTING
*        i_lgnum = xuser-lgnum
*        i_refnr = zwm028-refnr
*        i_tknum = zwm028-transporte
*      EXCEPTIONS
*        error   = 1
*        OTHERS  = 2.
*
*    CHECK sy-subrc EQ 0.
*
*    c_pos = i_pos.
*  ENDDO.

ENDFORM.                    "f_posicao_pulmao_skip
