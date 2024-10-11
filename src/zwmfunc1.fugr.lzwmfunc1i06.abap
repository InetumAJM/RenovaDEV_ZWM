*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I06 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_SSCC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc INPUT.

  IF NOT su IS INITIAL.

    SELECT SINGLE * FROM lein
        WHERE lenum = su AND lgnum = xuser-lgnum.
    IF sy-subrc = 0.
      MOVE su TO text1.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '071'
          message_var1   = text1.
      CLEAR: su, material, quantidade, unidade,
             tipo_palete, pack_material.
      MOVE 'SU' TO cursorfield.
    ELSE.
      CLEAR: pack_material, quantidade, unidade, material,
              plant, lgort, lote.
      SELECT SINGLE k~vhilm p~vemng p~vemeh p~matnr
                    p~werks p~lgort p~charg
        INTO (pack_material, quantidade, unidade, material,
              plant, lgort, lote)
           FROM vekp AS k INNER JOIN vepo AS p
                ON  k~venum = p~venum
                    WHERE  p~vepos = '000001' AND
                           k~exidv = su.

      IF sy-subrc <> 0.
        MOVE su TO text1.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '071'
            message_var1   = text1.
        CLEAR: su, material,quantidade, unidade, tipo_palete,
               pack_material.

        MOVE 'SU' TO cursorfield.
      ELSE.
        SELECT SINGLE *
            FROM zwm013
                WHERE armazem = xuser-lgnum AND
                      sscc = su.
        IF sy-subrc = 0.
          MOVE su TO text1.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '071'
              message_var1   = text1.
          CLEAR: su, material,quantidade, unidade, tipo_palete,
                 pack_material.

          MOVE 'SU' TO cursorfield.
        ELSE.
          SELECT SINGLE lety1 INTO tipo_palete
            FROM mlgn WHERE matnr = material AND
                 lgnum = xuser-lgnum.

          MOVE 'POSICAO_PULMAO' TO cursorfield.

        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '075'.

    CLEAR: su, material, quantidade, unidade, tipo_palete,
           pack_material.
    MOVE 'SU' TO cursorfield.
  ENDIF.

ENDMODULE.                 " CHECK_SSCC  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0013  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0013 INPUT.

  DATA: items LIKE zwm018 OCCURS 0 WITH HEADER LINE,
        mat_struct LIKE zwm_material OCCURS 0 WITH HEADER LINE,
        x_zwm013 LIKE zwm013 OCCURS 0 WITH HEADER LINE,
        x_zwm020 LIKE zwm020 OCCURS 0 WITH HEADER LINE.

  DATA:
*  valor(20),
        mblnr LIKE mkpf-mblnr,
        gjahr LIKE mkpf-mjahr,
        code LIKE bapi2017_gm_code,
        mov_mm TYPE bwlvs,
        mov_wm TYPE bwlvs,
        e_to TYPE tanum,
        s_type LIKE ltap-vltyp,
        certificado LIKE ltap-zeugn,
        parametro(20).

  REFRESH: items, mat_struct, x_zwm013, return_msg.
  CLEAR: items, mat_struct, x_zwm013, return_msg.

  CASE ok_code_0013.

    WHEN 'PRINT'.
*   FIM VERIFICAR SE CONFERIU TODAS AS PALETES
      SELECT SINGLE *
          FROM zwm016
              WHERE armazem = xuser-lgnum
                AND user_name = sy-uname
                AND porta = porta1.

      IF zwm016-num_paletes = zwm016-palete_conf.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '079'.

        CLEAR: pack_material, quantidade, unidade, material,
               plant, lgort, tipo_palete,su.

      ELSE.
        CLEAR: pack_material, quantidade, unidade, material,
               plant, lgort, tipo_palete,posicao_pulmao,kober.

        ecra_chamador = '0015'.
        IF lrf_wkqu-devty(5) = '16X20'.
          CALL SCREEN '0015'.
        ELSE.
          CALL SCREEN '0016'.
        ENDIF.
      ENDIF.

    WHEN 'INCID'.
*   registar incidencias
*      CHECK NOT su IS INITIAL.

** ecra que chama o ecra de incidencias
      ecra_chamador = '0013'.
      IF lrf_wkqu-devty(5) = '16X20'.
        CALL SCREEN '0010'.
      ELSE.
        CALL SCREEN '0009'.
      ENDIF.

    WHEN 'CONF'.
*   entrada de MM
      CHECK NOT su IS INITIAL AND NOT posicao_pulmao IS INITIAL.
*   verificar se esta a conferir paletes a mais do que o
*   que foi registado na descarga fisica
      SELECT SINGLE *
          FROM zwm016
              WHERE armazem = xuser-lgnum
                AND user_name = sy-uname
                AND porta = porta1.

      IF zwm016-num_paletes = zwm016-palete_conf.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '079'.

        CLEAR: pack_material, quantidade, unidade, material,
               lote, plant, lgort, tipo_palete,su.
      ELSE.

** Criação do LOTE
        CLEAR return_msg.
        REFRESH return_msg.
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
            CLEAR text1.
            WRITE lote TO text1 LEFT-JUSTIFIED.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '083'
                message_var1   = text1.
          ELSEIF sy-subrc = 2.

            READ TABLE return_msg INDEX 1.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = return_msg-msgnr
                message_var1   = return_msg-msgv1.
          ENDIF.

        ENDIF.

** Criação do documento de material
        CLEAR items.
        REFRESH items.
        items-ebeln = pedido.
        items-ebelp = item.
        items-material = material.
        items-quantidade = quantidade.
        items-uni = unidade.
        items-lote = lote.
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

        DATA pulmao TYPE ablad.
        MOVE pulmao2+4(10) TO pulmao.

        CLEAR return_msg.
        REFRESH return_msg.
        CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
          EXPORTING
            lgnum            = xuser-lgnum
            code             = code
            mov_mm           = mov_mm
            porta            = pulmao
            testrun          = ' '
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
        ENDIF.
**  Verificar se ja existe uma palete na mesma posicao no pulmao e se é
**  remontada inserir na tabela zwm020

        IF z_wm_cl_management=>is_remontada( i_lgnum = xuser-lgnum i_letyp = tipo_palete ) eq abap_true.
          SELECT SINGLE *
              FROM zwm013
                  WHERE armazem = xuser-lgnum AND
                  destino = pulmao2 AND
                  posicao_pulmao = posicao_pulmao.
          IF sy-subrc = 0.
            CLEAR x_zwm020.
            REFRESH x_zwm020.
            x_zwm020-armazem = xuser-lgnum.
            x_zwm020-p1 = zwm013-sscc.
            x_zwm020-p2 = su.
            APPEND x_zwm020.
            INSERT zwm020 FROM x_zwm020.
            COMMIT WORK.
          ENDIF.
        ENDIF.

*        inserir na tabela para desbloquear no fim
        x_zwm013-armazem = xuser-lgnum.
        x_zwm013-sscc = su.
*            X_ZWM013-TO_NUMBER = E_TO.
        x_zwm013-destino = pulmao2.
        x_zwm013-bloqueado = 'X'.
        x_zwm013-tipo_palete = tipo_palete.
        x_zwm013-incidencia = incidencia_terceiros.
        x_zwm013-posicao_pulmao = posicao_pulmao.
        CLEAR x_zwm013-fabrica_1.
        x_zwm013-trans_terc = 'X'.
        APPEND x_zwm013.
        INSERT zwm013 FROM x_zwm013.
        COMMIT WORK.
**      actualizar tabela 16 incrementar o num paletes

        UPDATE zwm016 SET palete_conf = palete_conf + 1
            WHERE armazem   = xuser-lgnum AND
                  user_name = sy-uname AND
                  porta     = porta1.
*                  pulmao = pulmao2.
        COMMIT WORK.

*      registar na tabela de paletes para no fim da confirmação
*      dar entrada em MM (pchep...)
        CLEAR zwm023.
        SELECT SINGLE *
            FROM zwm023
                WHERE armazem = xuser-lgnum AND
                      user_name = sy-uname AND
                      ebeln = pedido AND
                      ebelp = item AND
                      matnr = pack_material.
        IF sy-subrc <> 0.
          zwm023-armazem = xuser-lgnum.
          zwm023-user_name = sy-uname.
          zwm023-ebeln = pedido.
          zwm023-ebelp = item.
          zwm023-matnr = pack_material.
          zwm023-quantidade = 1.
        ELSE.
          zwm023-quantidade = zwm023-quantidade + 1.
        ENDIF.

        MODIFY zwm023.
        COMMIT WORK.

** INI - Entrada de Meia Palete.
        SELECT SINGLE *
            FROM mlgn
                WHERE matnr = material
                  AND lgnum = xuser-lgnum
                  AND ( block = '03' OR block = '04' ).
        IF sy-subrc = 0.
          CLEAR zwm001.
          SELECT SINGLE *
              FROM zwm001
                  WHERE armazem = xuser-lgnum AND
                        processo = 'MEIA-PALETE' AND
                        parametro = pack_material.
          IF sy-subrc = 0.
            CLEAR zwm023.
            SELECT SINGLE *
                FROM zwm023
                    WHERE armazem = xuser-lgnum AND
                          user_name = sy-uname AND
                          ebeln = pedido AND
                          ebelp = item AND
                          matnr = zwm001-valor.
            IF sy-subrc <> 0.
              zwm023-armazem = xuser-lgnum.
              zwm023-user_name = sy-uname.
              zwm023-ebeln = pedido.
              zwm023-ebelp = item.
              zwm023-matnr = zwm001-valor.
              zwm023-quantidade = 2.
            ELSE.
              zwm023-quantidade = zwm023-quantidade + 2.
            ENDIF.

            MODIFY zwm023.
            COMMIT WORK.
          ENDIF.
        ENDIF.
** FIM - Entrada de Meia Palete.


** INI - Entrada de Quarto Palete.
        SELECT SINGLE *
            FROM mlgn
                WHERE matnr = material
                  AND lgnum = xuser-lgnum
                  AND ( block = '07' OR block = '08' ).
        IF sy-subrc = 0.
          CLEAR zwm001.
          SELECT SINGLE *
              FROM zwm001
                  WHERE armazem = xuser-lgnum AND
                        processo = 'QUARTO-PALETE' AND
                        parametro = pack_material.
          IF sy-subrc = 0.
            CLEAR zwm023.
            SELECT SINGLE *
                FROM zwm023
                    WHERE armazem = xuser-lgnum AND
                          user_name = sy-uname AND
                          ebeln = pedido AND
                          ebelp = item AND
                          matnr = zwm001-valor.
            IF sy-subrc <> 0.
              zwm023-armazem = xuser-lgnum.
              zwm023-user_name = sy-uname.
              zwm023-ebeln = pedido.
              zwm023-ebelp = item.
              zwm023-matnr = zwm001-valor.
              zwm023-quantidade = 4.
            ELSE.
              zwm023-quantidade = zwm023-quantidade + 4.
            ENDIF.

            MODIFY zwm023.
            COMMIT WORK.
          ENDIF.
        ENDIF.
** FIM - Entrada de Meia Palete.

        CLEAR: pack_material, quantidade, unidade, material,
               plant, lgort, tipo_palete,su,incidencia_terceiros,
               posicao_pulmao,kober.

        MOVE 'SU' TO cursorfield.
      ENDIF.

    WHEN 'SAVE'.

*   fim verificar se conferiu todas as paletes
      SELECT SINGLE *
          FROM zwm016
              WHERE armazem = xuser-lgnum
                AND user_name = sy-uname
                AND porta = porta1.
*                    pulmao = pulmao2.

** Primeiro verificar se o user já foi ao PC fechar a conferência
      IF zwm016-finalizada IS INITIAL.

        CLEAR : resposta.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '081'
          IMPORTING
            ret_code       = resposta.

        IF resposta = 'O'.

          IF lrf_wkqu-devty(5) = '16X20'.
            SET SCREEN '0013'.
            LEAVE SCREEN.
          ELSE.
            SET SCREEN '0014'.
            LEAVE SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.

      IF zwm016-palete_conf IS INITIAL.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '093'.

      ELSEIF zwm016-num_paletes < zwm016-palete_conf.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'W'
            message_number = '080'
          IMPORTING
            ret_code       = resposta.
** Avançar
        IF resposta = 'O'.

** 04.03.2005 - ROFFD ** DEL
** Registar incidência e prosseguir
*          CALL FUNCTION 'ZWM_INSERT_ERROR'
*            EXPORTING
*              armazem    = xuser-lgnum
*              incidencia = '7'
*            EXCEPTIONS
*              no_commit  = 1
*              OTHERS     = 2.
** 04.03.2005 - ROFFD ** DEL

          PERFORM cria_doc_material_paletes.
          PERFORM save.

        ELSEIF resposta = 'C'.
          UPDATE zwm016
            SET finalizada = ' '
              WHERE armazem = xuser-lgnum AND
                    user_name = sy-uname AND
                    pulmao = pulmao2.
          COMMIT WORK.

          CLEAR: pack_material, quantidade, unidade, material,
                 plant, lgort, tipo_palete, su.
        ENDIF.

      ELSEIF zwm016-num_paletes > zwm016-palete_conf.

        paletes_dif = zwm016-num_paletes - zwm016-palete_conf.
        MOVE paletes_dif TO text1.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'W'
            message_number = '073'
            message_var1   = text1
          IMPORTING
            ret_code       = resposta.
** Avançar
        IF resposta = 'O'.

** 04.03.2005 - ROFFD ** DEL
** Registar incidência e prosseguir
*          CALL FUNCTION 'ZWM_INSERT_ERROR'
*            EXPORTING
*              armazem    = xuser-lgnum
*              incidencia = '7'
*            EXCEPTIONS
*              no_commit  = 1
*              OTHERS     = 2.
** 04.03.2005 - ROFFD ** DEL

          PERFORM cria_doc_material_paletes.
          PERFORM save.

** Cancelar
        ELSEIF resposta = 'C'.
          UPDATE zwm016
            SET finalizada = ' '
              WHERE armazem = xuser-lgnum AND
                    user_name = sy-uname AND
                    pulmao = pulmao2.
          CLEAR: pack_material, quantidade, unidade, material,
                 plant, lgort, tipo_palete, su, paletes_dif.
        ENDIF.

      ELSE.
* criar material document com informacao da tabela zwm023 e apagar as
* entradas depois de criar o informaçao de recepcoes
* falta saber qual o movimento e onde se poe o fornecedor

        PERFORM cria_doc_material_paletes.
*        CHECK sy-subrc IS INITIAL.
        PERFORM save.

      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0013  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT13  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit13 INPUT.

  CASE ok_code_0013.

    WHEN 'BACK'.
      CLEAR: ped_compra, pedido, item, pack_material,
             quantidade, unidade, material, lote, plant,
             lgort, tipo_palete,su,incidencia_terceiros,
             posicao_pulmao,kober.
      SET SCREEN '0000'.
      LEAVE SCREEN.

*    WHEN 'PRINT'.
**   FIM VERIFICAR SE CONFERIU TODAS AS PALETES
*      SELECT SINGLE *
*          FROM ZWM016
*              WHERE ARMAZEM = XUSER-LGNUM AND
*                    USER_NAME = SY-UNAME.
*
*      IF ZWM016-NUM_PALETES = ZWM016-PALETE_CONF.
*
*        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*          EXPORTING
*            MESSAGE_ID     = 'ZWMMSG001'
*            MESSAGE_LANG   = SY-LANGU
*            MESSAGE_TYPE   = 'E'
*            MESSAGE_NUMBER = '079'.
*
*        CLEAR: PACK_MATERIAL, QUANTIDADE, UNIDADE, MATERIAL,
*               PLANT, LGORT, TIPO_PALETE,SU.
*
*      ELSE.
*        CLEAR: PACK_MATERIAL, QUANTIDADE, UNIDADE, MATERIAL,
*               PLANT, LGORT, TIPO_PALETE.
*
*        ECRA_CHAMADOR = '0015'.
*        IF LRF_WKQU-DEVTY(5) = '16X20'.
*          CALL SCREEN '0015'.
*        ELSE.
*          CALL SCREEN '0016'.
*        ENDIF.
*      ENDIF.
  ENDCASE.
ENDMODULE.                 " EXIT13  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_tipo_palete  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_tipo_palete INPUT.

  CHECK NOT tipo_palete IS INITIAL.
  SELECT SINGLE *
      FROM t307
          WHERE lgnum = xuser-lgnum AND
                letyp = tipo_palete.
  IF sy-subrc <> 0.
    MOVE tipo_palete TO text1.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '076'
        message_var1   = text1.
    CLEAR tipo_palete.
    MOVE 'TIPO_PALETE' TO cursorfield.
  ELSE.
    SELECT SINGLE *
        FROM marm
            WHERE matnr = material AND
                  meinh = 'PAL' .
*                  meinh = tipo_palete.
    IF sy-subrc <> 0.
      MOVE tipo_palete TO text1.
      MOVE material TO text2.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '087'
          message_var1   = text1
          message_var2   = text2.
      CLEAR tipo_palete.
      MOVE 'TIPO_PALETE' TO cursorfield.
    ELSE.
** Novo campo para a posição da palete no pulmão
      MOVE 'POSICAO_PULMAO' TO cursorfield.
    ENDIF.
  ENDIF.


ENDMODULE.                 " CHECK_tipo_palete  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_posicao_pulmao  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_posicao_pulmao INPUT.

  DATA: l_pulmao(14),
        l_flag.

  CLEAR: text1,
         text2,
         l_flag.

  IF posicao_pulmao = '00' OR
     posicao_pulmao GT '17' OR
     posicao_pulmao IS INITIAL.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '145'
        message_var1   = text1.
    CLEAR: posicao_pulmao,kober.
    MOVE 'POSICAO_PULMAO' TO cursorfield.
  ENDIF.

** verificar se já existe algum SSCC dentro do pulmao
** que tenha a mesma posição
** se for palete remontada pode colocar outra palete
  DATA: num_paletes TYPE i,
        num_total   TYPE i,
        num_total_rem TYPE i,
        num_total_sim TYPE i.
  CLEAR num_paletes.

** Validar se o canal do pulmão está cheio, se assim for passa
** para o canal seguinte

  data(lr_remontadas_letyp) = z_wm_cl_management=>r_letyp_remontada( xuser-lgnum ).

  CLEAR zwm013.
  SELECT COUNT(*) INTO num_total_rem
      FROM zwm013
          WHERE armazem EQ xuser-lgnum
            AND destino EQ pulmao2 AND
                ( tipo_palete in lr_remontadas_letyp ).

  IF sy-subrc = 0.
    num_total_rem = num_total_rem DIV 2.
  ENDIF.

  CLEAR zwm013.
  SELECT COUNT(*) INTO num_total_sim
    FROM zwm013
        WHERE armazem EQ xuser-lgnum
          AND destino EQ pulmao2 AND
              tipo_palete not in lr_remontadas_letyp.

  num_total = num_total_rem + num_total_sim.

  IF num_total = 17.
    l_flag = 'X'.
  ENDIF.


  IF l_flag = 'X'.

    CLEAR: zwm002, l_porta, l_lgpla.
    l_porta = porta1+12(2).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = l_porta
      IMPORTING
        output = l_porta.

    SELECT SINGLE * FROM zwm002
    WHERE armazem EQ xuser-lgnum
      AND porta   EQ l_porta.

    IF sy-subrc EQ 0.
      l_lgpla = zwm002-pulmao_2.
      CLEAR l_pulmao.

      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = 'PUL'
          lgpla = l_lgpla
        IMPORTING
          bin   = l_pulmao.

      pulmao2 = l_pulmao.
    ENDIF.
  ENDIF.

  CLEAR: zwm013, num_paletes.
  SELECT COUNT(*) tipo_palete INTO (num_paletes, zwm013-tipo_palete)
      FROM zwm013
          WHERE armazem = xuser-lgnum AND
                destino = pulmao2 AND
                posicao_pulmao = posicao_pulmao
                GROUP BY tipo_palete.
  ENDSELECT.

  IF sy-subrc = 0.
    IF z_wm_cl_management=>is_remontada( i_lgnum = xuser-lgnum i_letyp = tipo_palete ) eq abap_true.
      IF num_paletes = 1.
        IF tipo_palete <> zwm013-tipo_palete.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '194'.
          CLEAR: posicao_pulmao,kober.
          MOVE 'POSICAO_PULMAO' TO cursorfield.
        ENDIF.
      ELSEIF num_paletes > 1.
        WRITE posicao_pulmao TO text1 LEFT-JUSTIFIED.
        WRITE l_pulmao TO text2 LEFT-JUSTIFIED..
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '094'
            message_var1   = text1
            message_var2   = text2.
        CLEAR: posicao_pulmao,kober.
        MOVE 'POSICAO_PULMAO' TO cursorfield.
      ENDIF.
    ELSE.
      WRITE posicao_pulmao TO text1 LEFT-JUSTIFIED.
      WRITE l_pulmao TO text2 LEFT-JUSTIFIED.
*      WRITE pulmao2 TO text2 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '094'
          message_var1   = text1
          message_var2   = text2.
      CLEAR: posicao_pulmao,kober.
      MOVE 'POSICAO_PULMAO' TO cursorfield.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_posicao_pulmao  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_posicao_pulmao2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_posicao_pulmao2 INPUT.

  CLEAR: text1,
         text2.

  CHECK bin_output_d(3) = 'PUL'.

  IF posicao_pulmao2 = '00' OR
     posicao_pulmao2 IS INITIAL.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '071'
        message_var1   = text1.
    CLEAR: posicao_pulmao2.
    MOVE 'POSICAO_PULMAO2' TO cursorfield.

  ELSEIF posicao_pulmao1 <> posicao_pulmao2.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '145'
        message_var1   = text1.
    CLEAR: posicao_pulmao2.
    MOVE 'POSICAO_PULMAO2' TO cursorfield.
  ELSE.

** Verificar se o SSCC já está picado para uma remessa.
    IF bin_output_o(3) = 'DRI' OR bin_output_o(3) = 'BLK'.
      SELECT SINGLE *
          FROM ltap
              WHERE lgnum = xuser-lgnum AND
                    tanum = to_ret AND
                    vlenr = su AND
                    vorga <> 'ST'.
      IF sy-subrc <> 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '213'
            message_var1   = text1.
        CLEAR: posicao_pulmao2.
        MOVE 'POSICAO_PULMAO2' TO cursorfield.
        EXIT.
      ENDIF.
    ENDIF.

    SELECT SINGLE *
        FROM zwm013
            WHERE armazem = xuser-lgnum AND
                  destino = bin_output_d AND
                  posicao_pulmao = posicao_pulmao2.
    IF sy-subrc = 0.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '175'
          message_var1   = text1.

      CLEAR:posicao_pulmao1, posicao_pulmao2.
      MOVE 'POSICAO_PULMAO2' TO cursorfield.

    ENDIF.
  ENDIF.

ENDMODULE.                 " check_posicao_pulmao2  INPUT
