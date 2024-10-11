*&---------------------------------------------------------------------*
*& Report  ZWMREP0031                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zwmrep0031 MESSAGE-ID zwmmsg001.

TABLES: zwm001, zwm013, zwm026, zwm028, zwm031, zwm034, zwm035.

INCLUDE: rlmobinc.

DATA : BEGIN OF ti_zwm001 OCCURS 0.
        INCLUDE STRUCTURE zwm001.
DATA: END OF ti_zwm001.

DATA : BEGIN OF t_remessa OCCURS 0.
        INCLUDE STRUCTURE lips.
DATA: kunnr LIKE likp-kunnr.
DATA: pal_especial(1).
DATA: qtd_especial LIKE vepo-vemng.
DATA: uni_especial LIKE vepo-vemeh.
DATA: END OF t_remessa.

DATA : BEGIN OF t_vepo OCCURS 0.
        INCLUDE STRUCTURE vepo.
DATA: exidv LIKE vekp-exidv.
DATA: END OF t_vepo.

DATA: BEGIN OF t_pal_especial OCCURS 0.
DATA: vlenr LIKE ltap-vlenr.
DATA: matnr LIKE ltap-matnr.
DATA: vbeln LIKE ltap-vbeln.
DATA: pulmao LIKE zwm013-destino.
DATA: pos_pulmao LIKE zwm013-posicao_pulmao.
DATA: vsolm LIKE ltap-vsolm.
DATA: meins LIKE ltap-meins.
DATA: charg LIKE ltap-charg.
DATA: qtd_especial LIKE vepo-vemng.
DATA: uni_especial LIKE vepo-vemeh.
DATA: pal_pick(1).
DATA: END OF t_pal_especial.

DATA w_pal_especial LIKE t_pal_especial.

DATA items LIKE zwm_items_hu OCCURS 0 WITH HEADER LINE.

DATA t_zwm013 LIKE zwm013 OCCURS 0 WITH HEADER LINE.
DATA t_zwm020 LIKE zwm020 OCCURS 0 WITH HEADER LINE.
DATA t_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE.
DATA t_ltak LIKE ltak OCCURS 0 WITH HEADER LINE.
DATA t_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.
DATA t_zwm034 LIKE zwm034 OCCURS 0 WITH HEADER LINE.
DATA ti_zwm034 LIKE zwm034 OCCURS 0 WITH HEADER LINE.
DATA wa_zwm034 LIKE zwm034.
DATA: aux_zwm034 LIKE zwm034 OCCURS 0 WITH HEADER LINE.
DATA t_zwm035 LIKE zwm035 OCCURS 0 WITH HEADER LINE.
DATA t_sscc LIKE zwm_ean128 OCCURS 0 WITH HEADER LINE.

DATA gv_werks TYPE werks_d.
DATA gv_lgort TYPE lgort_d.

*  Dados gerais
DATA: ok_code_0001 LIKE sy-ucomm,
      ok_code_0003 LIKE sy-ucomm,
      ok_code_0005 LIKE sy-ucomm,
      grupo LIKE t311-refnr,
      pulmao1(14),
      pulmao2(14),
      pulmao3(14),
      pulmao4(14),
      text TYPE  bdcmsgcoll-msgv1,
      text1 TYPE  bdcmsgcoll-msgv1,
      save_index LIKE sy-tabix,
      pos_pulmao(2),
      sscc1 LIKE vekp-exidv,
      sscc2 LIKE vekp-exidv,
      material LIKE ltap-matnr,
      desc2 LIKE makt-maktx,
      qtd LIKE vepo-vemng,
      uni LIKE vepo-vemeh,
      new_sscc LIKE vekp-exidv,
      pulmao(14),
      pospulmao(2),
      pack_material LIKE mara-matnr,
      count TYPE i.


DATA: cursorfield(20),
      return_msg TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

START-OF-SELECTION.
  message_lang = sy-langu.
  PERFORM user_own_data.

  IF lrf_wkqu-devty(5) = '16X20'.
    CALL SCREEN '0001'.
  ELSE.
    CALL SCREEN '0002'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'ZRF1'.
  SET CURSOR FIELD cursorfield.

ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CLEAR: grupo,pulmao1,pulmao2,pulmao3,pulmao4,ok_code_0001.
  SET SCREEN '0000'.
  LEAVE SCREEN.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_grupo  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_grupo INPUT.

  CHECK NOT grupo IS INITIAL.

  CLEAR: t_zwm028, t_remessa, save_index, gv_werks, gv_lgort.
  REFRESH: t_zwm028, t_remessa.

  SELECT SINGLE *
      FROM zwm028
          WHERE lgnum = xuser-lgnum AND
                refnr = grupo AND
                remessa = ' '.

  IF sy-subrc = 0.
    IF zwm028-total_paletes <> zwm028-paletes_pulmao.
**  A carga do Grupo & ainda não esta toda preparada no pulmão !
      CLEAR text.
      WRITE grupo TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '159'
          message_var1   = text.
      MOVE 'GRUPO' TO cursorfield.
      CLEAR grupo.
      EXIT.
    ELSE.
      SELECT * INTO TABLE t_zwm028
          FROM zwm028
              WHERE lgnum = xuser-lgnum AND
                    refnr = grupo AND
                    remessa <> ' '.

      LOOP AT t_zwm028.
        SELECT * APPENDING CORRESPONDING FIELDS OF TABLE t_remessa
            FROM likp AS a INNER JOIN lips AS b
                ON a~vbeln = b~vbeln
                    WHERE a~vbeln = t_zwm028-remessa.
      ENDLOOP.

      LOOP AT t_remessa.
        save_index = sy-tabix.
        CLEAR zwm031.
        SELECT SINGLE * FROM zwm031
              WHERE lgnum  = xuser-lgnum
                AND kunnr  = t_remessa-kunnr
                AND matnr  = t_remessa-matnr.
        IF sy-subrc = 0.
          t_remessa-pal_especial = 'X'.
          t_remessa-qtd_especial = zwm031-unporpal.
          t_remessa-uni_especial = t_remessa-meins.
          MODIFY t_remessa INDEX save_index.
        ENDIF.
      ENDLOOP.

      READ TABLE t_remessa WITH KEY pal_especial = 'X'.
      IF sy-subrc <> 0.
**  O Grupo & não necessita de Paletização Especial.
        CLEAR text.
        WRITE grupo TO text LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '161'
            message_var1   = text.
        MOVE 'GRUPO' TO cursorfield.
        CLEAR grupo.
        EXIT.
      ELSE.

        CALL FUNCTION 'ZWM_CONCATENATE_BIN'
          EXPORTING
            lgtyp = zwm028-st_pul
            lgpla = zwm028-pulmao1
          IMPORTING
            bin   = pulmao1.

        IF NOT zwm028-pulmao2 IS INITIAL.
          CALL FUNCTION 'ZWM_CONCATENATE_BIN'
            EXPORTING
              lgtyp = zwm028-st_pul
              lgpla = zwm028-pulmao2
            IMPORTING
              bin   = pulmao2.

        ENDIF.

        MOVE 'PULMAO3' TO cursorfield.
      ENDIF.
    ENDIF.
  ELSE.
**  Carga & invalida !
    CLEAR text.
    WRITE grupo TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '160'
        message_var1   = text.
    MOVE 'GRUPO' TO cursorfield.
    CLEAR grupo.
    EXIT.
  ENDIF.

** Dados de Centro
***********************************************************************
  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
   EXPORTING
     i_refnr             = grupo
     i_recall            = 'X'
     i_usewm             = 'X'
     i_userf             = 'X'
     i_usemm             = 'X'
     i_useaut            = 'X'
     i_get_lgnum         = 'X'
     i_get_werks         = 'X'
     i_get_lgort         = 'X'
* IMPORTING
*   ET_MESSAGES         =
   CHANGING
     c_lgnum             = xuser-lgnum
     c_werks             = gv_werks
     c_lgort             = gv_lgort
   EXCEPTIONS
     error               = 1
     user_back           = 2
     OTHERS              = 3.


ENDMODULE.                 " CHECK_grupo  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_pulmao3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pulmao3 INPUT.

  CHECK NOT pulmao3 IS INITIAL.

  IF pulmao1 <> pulmao3.
** Erro! Pulmao & incorrecto - diferente de Pulmao & !

    CLEAR: text, text1.
    WRITE pulmao1 TO text LEFT-JUSTIFIED.
    WRITE pulmao3 TO text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '036'
        message_var1   = text.
    CLEAR pulmao3.
    MOVE 'PULMAO3' TO cursorfield.
    EXIT.
  ELSE.
    IF NOT pulmao2 IS INITIAL.
      MOVE 'PULMAO4' TO cursorfield.
      EXIT.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_pulmao3  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_pulmao4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pulmao4 INPUT.

  CHECK NOT pulmao2 IS INITIAL.

  CHECK NOT pulmao4 IS INITIAL.

  IF pulmao2 <> pulmao4.
** Erro! Pulmao & incorrecto - diferente de Pulmao & !

    CLEAR: text, text1.
    WRITE pulmao1 TO text LEFT-JUSTIFIED.
    WRITE pulmao3 TO text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '036'
        message_var1   = text.
    CLEAR pulmao4.
    MOVE 'PULMAO4' TO cursorfield.
    EXIT.
  ENDIF.
ENDMODULE.                 " check_pulmao4  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CHECK NOT grupo IS INITIAL AND
        NOT pulmao3 IS INITIAL.

  IF NOT pulmao2 IS INITIAL.
    CHECK NOT pulmao4 IS INITIAL.
  ENDIF.

  DELETE t_remessa WHERE pal_especial = ' '.

  CLEAR: t_zwm013, t_ltak, t_ltap, t_pal_especial, t_vepo.
  REFRESH: t_zwm013, t_ltak, t_ltap, t_pal_especial, t_vepo.

  SELECT * INTO TABLE t_zwm013
      FROM zwm013
          WHERE armazem = xuser-lgnum AND
                ( destino = pulmao1 OR
                  destino = pulmao2 ) AND
                  destino <> ' '.

  SELECT * INTO TABLE t_ltak
      FROM ltak
          WHERE lgnum = xuser-lgnum AND
                refnr = grupo.

  DELETE t_ltak WHERE kquit = 'X'.

  LOOP AT t_ltak.

    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE t_ltap
        FROM ltap
            WHERE lgnum = t_ltak-lgnum AND
                  tanum = t_ltak-tanum AND
                  pquit <> 'X'.

  ENDLOOP.

  LOOP AT t_zwm013.

    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE t_vepo
        FROM vekp AS k INNER JOIN vepo AS p
             ON  k~venum = p~venum
                 WHERE k~exidv = t_zwm013-sscc.

  ENDLOOP.
**  apartir desta informacao criar uma tabela que so contenha as paletes
**  que necessitam de ser repaletizadas e chamar o novo ecran para ir
**  processando os unpacks

  LOOP AT t_remessa.

    LOOP AT t_ltap WHERE vbeln = t_remessa-vbeln AND
                         matnr = t_remessa-matnr.
** Paletes Completas
      IF NOT t_ltap-vlenr IS INITIAL.

        READ TABLE t_vepo WITH KEY exidv = t_ltap-vlenr.

        CHECK t_vepo-vemng > t_remessa-qtd_especial.

        t_pal_especial-vlenr = t_ltap-vlenr.
        t_pal_especial-matnr = t_ltap-matnr.
        t_pal_especial-vbeln = t_ltap-vbeln.
        t_pal_especial-vsolm = t_ltap-vsolm.
        t_pal_especial-meins = t_ltap-meins.
        t_pal_especial-charg = t_ltap-charg.
        t_pal_especial-qtd_especial = t_remessa-qtd_especial.
        t_pal_especial-uni_especial = t_remessa-meins.

        READ TABLE t_zwm013 WITH KEY armazem = t_ltap-lgnum
                                     sscc = t_ltap-vlenr.

        t_pal_especial-pulmao = t_zwm013-destino.
        t_pal_especial-pos_pulmao = t_zwm013-posicao_pulmao.

        APPEND t_pal_especial.
        CLEAR t_pal_especial.
** Paletes Incompletas - Testar
      ELSE.

        SELECT SINGLE *
            FROM zwm026
                WHERE armazem = t_ltap-lgnum AND
                      to_number = t_ltap-tanum.

        READ TABLE t_vepo WITH KEY exidv = zwm026-sscc.

*        CHECK t_vepo-vemng > t_remessa-qtd_especial.

        t_pal_especial-vlenr = zwm026-sscc.
        t_pal_especial-matnr = t_remessa-matnr.
        t_pal_especial-vbeln = t_ltap-vbeln.
        t_pal_especial-vsolm = t_ltap-vsolm.
        t_pal_especial-meins = t_ltap-meins.
        t_pal_especial-charg = t_ltap-charg.
        t_pal_especial-qtd_especial = t_remessa-qtd_especial.
        t_pal_especial-uni_especial = t_remessa-meins.
        t_pal_especial-pal_pick = 'X'.

        READ TABLE t_zwm013 WITH KEY armazem = t_ltap-lgnum
                                            sscc = zwm026-sscc.

        t_pal_especial-pulmao = t_zwm013-destino.
        t_pal_especial-pos_pulmao = t_zwm013-posicao_pulmao.

        APPEND t_pal_especial.
        CLEAR t_pal_especial.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF NOT t_pal_especial[] IS INITIAL.
    IF lrf_wkqu-devty(5) = '16X20'.
      CALL SCREEN '0003'.
    ELSE.
      CALL SCREEN '0004'.
    ENDIF.
  ELSE.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '170'
        message_var1   = text.

    CLEAR: grupo,pulmao1,pulmao2,pulmao3,pulmao4,ok_code_0001.
    SET SCREEN '0000'.
    LEAVE SCREEN.

  ENDIF.
ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  status_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0003 OUTPUT.

  SET PF-STATUS 'ZRF1'.
  SET CURSOR FIELD cursorfield.

  IF NOT t_pal_especial[] IS INITIAL.
    SORT t_pal_especial BY vbeln matnr pulmao pos_pulmao.

    CLEAR: pulmao1, pos_pulmao, sscc1, uni, material.
    READ TABLE t_pal_especial INDEX 1.
    pulmao1 = t_pal_especial-pulmao.
    WRITE t_pal_especial-pos_pulmao TO pos_pulmao LEFT-JUSTIFIED.
    sscc1 = t_pal_especial-vlenr.
    uni = t_pal_especial-meins.
    material = t_pal_especial-matnr.
  ELSE.

** finalizado o processo imprimir todos os sscc´s.
** verificar se falta criar alguma palete

    CLEAR t_zwm034.
    REFRESH t_zwm034.
    SELECT * INTO TABLE t_zwm034
        FROM zwm034
            WHERE armazem = xuser-lgnum AND
                  remessa = t_pal_especial-vbeln AND
                  material = t_pal_especial-matnr.

    SELECT SINGLE maktx INTO desc2
        FROM makt
            WHERE matnr = t_pal_especial-matnr AND
                  spras = sy-langu.

    IF NOT t_zwm034[] IS INITIAL.
      CLEAR new_sscc.
      IF lrf_wkqu-devty(5) = '16X20'.
        CALL SCREEN '0005'.
      ELSE.
        CALL SCREEN '0006'.
      ENDIF.
    ENDIF.
*****************************************

    CLEAR t_sscc.
    REFRESH t_sscc.

    SELECT *
      FROM zwm035
          WHERE armazem = xuser-lgnum AND
                grupo = grupo.
      MOVE zwm035-sscc TO t_sscc-sscc.
      APPEND t_sscc.
      CLEAR t_sscc.
    ENDSELECT.

    DATA: l_printer LIKE nast-ldest,
          valor LIKE zwm001-valor.

    PERFORM get_parameter USING xuser-lgnum
                                  'EAN128'
                                  pulmao1
                                  valor.

    MOVE valor TO l_printer.

    break roff.
    CALL FUNCTION 'ZWM_IMPRIME_EAN128'
      EXPORTING
        printer                  = l_printer
      TABLES
        sscc                     = t_sscc
      EXCEPTIONS
        impressora_nao_existe    = 1
        sscc_nao_existe          = 2
        sscc_com_impressao_grupo = 3
        OTHERS                   = 4.
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT t_sscc.
      DELETE FROM zwm035
          WHERE armazem = xuser-lgnum AND
                          grupo = grupo AND
                          sscc = t_sscc-sscc.
      COMMIT WORK.
    ENDLOOP.

    CLEAR: grupo,pulmao1,pulmao2,pulmao3,pulmao4,ok_code_0001.

    SET SCREEN '0000'.
    LEAVE SCREEN.
  ENDIF.
ENDMODULE.                 " status_0003  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit3 INPUT.

  CLEAR: grupo,
         pulmao1,pulmao2,
         pulmao3,pulmao4,
         ok_code_0001,
         ok_code_0003,
         cursorfield,
         pos_pulmao,
         sscc1,
         sscc2,
         material,
         qtd,
         uni.

  SET SCREEN '0000'.
  LEAVE SCREEN.
ENDMODULE.                 " exit3  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_sscc  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc INPUT.

  CHECK NOT sscc2 IS INITIAL.

  IF sscc1 <> sscc2.
    CLEAR: text.
    WRITE sscc2 TO text LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '033'
        message_var1   = text.
    CLEAR sscc2.
    MOVE 'SSCC2' TO cursorfield.
    EXIT.
  ELSE.
    MOVE 'QTD' TO cursorfield.
  ENDIF.

ENDMODULE.                 " check_sscc  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_qtd  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_qtd INPUT.

  CHECK NOT qtd IS INITIAL.

  DATA qtd_resto LIKE vepo-vemng.
  qtd_resto = t_pal_especial-vsolm - qtd.

  IF t_pal_especial-pal_pick <> 'X'.
    IF qtd_resto <> t_pal_especial-qtd_especial.

      CLEAR: text.
      WRITE qtd TO text LEFT-JUSTIFIED.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '165'
          message_var1   = text.

      CLEAR qtd.
      MOVE 'QTD' TO cursorfield.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_qtd  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0003 INPUT.


  CASE ok_code_0003.
** Fazer o Unpack
*      break roffd.
    WHEN 'CONF'.

      CHECK NOT sscc2 IS INITIAL AND
            NOT qtd IS INITIAL.

      IF t_pal_especial-pal_pick <> 'X'.
        CHECK qtd_resto = t_pal_especial-qtd_especial.
      ENDIF.

      CLEAR new_sscc.
      CALL FUNCTION 'ZWM_UNPACK_HANDLING_UNIT'
        EXPORTING
          lgnum                 = xuser-lgnum
          exidv                 = sscc2
          charg                 = t_pal_especial-charg
          werks                 = gv_werks
          lgort                 = gv_lgort
          matnr                 = material
          vemng                 = qtd
          vemeh                 = uni
        EXCEPTIONS
          other                 = 1
          not_possible          = 2
          input_missing         = 3
          input_error           = 4
          not_unpacked          = 5
          fatal_error           = 6
          delivery_update_error = 7
          OTHERS                = 8.
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

          SET SCREEN '0000'.LEAVE SCREEN.
        ENDIF.

      ELSE.
** Actualizar a Tabela com as quantidades retiradas da palete.

        DATA flag(1).
        CLEAR flag.
        WHILE flag IS INITIAL.
          CLEAR: t_zwm034, wa_zwm034.
          REFRESH t_zwm034.
          SELECT SINGLE *
              FROM zwm034
                  WHERE armazem = xuser-lgnum AND
                        remessa = t_pal_especial-vbeln AND
                        material = t_pal_especial-matnr AND
                        lote = t_pal_especial-charg.

          IF sy-subrc = 0.

            MOVE-CORRESPONDING zwm034 TO t_zwm034.
            t_zwm034-qtd = t_zwm034-qtd + qtd.
            APPEND t_zwm034.
            CLEAR t_zwm034.
            MODIFY zwm034 FROM TABLE t_zwm034.
            COMMIT WORK.
            CLEAR qtd.
          ELSE.
            t_zwm034-armazem = xuser-lgnum.
            t_zwm034-remessa = t_pal_especial-vbeln.
            t_zwm034-material = t_pal_especial-matnr.
            t_zwm034-lote = t_pal_especial-charg.
            t_zwm034-qtd = qtd.
            t_zwm034-uni = uni.
            APPEND t_zwm034.
            INSERT zwm034 FROM TABLE t_zwm034.
            COMMIT WORK.
          ENDIF.


**   Verificar existe quantidade suficiente desembalada para fazer
**   uma nova etiqueta
          CLEAR t_zwm034.
          REFRESH t_zwm034.

          SELECT *
              FROM zwm034
                  WHERE armazem = xuser-lgnum AND
                        remessa = t_pal_especial-vbeln AND
                        material = t_pal_especial-matnr.

            MOVE-CORRESPONDING zwm034 TO t_zwm034.
            APPEND t_zwm034.
            CLEAR t_zwm034.
          ENDSELECT.

          DATA: qtd_total LIKE zwm034-qtd,
                qtd_parcial LIKE zwm034-qtd.
          CLEAR: qtd_total, qtd_parcial.

          SORT t_zwm034 BY armazem remessa material.

          LOOP AT t_zwm034.
            AT NEW material.
              SUM.
              MOVE t_zwm034-qtd TO qtd_total.
            ENDAT.
          ENDLOOP.

** Chamar ecra para pedir informacao para a criação da nova etiqueta
          IF qtd_total >= t_pal_especial-qtd_especial.

            IF qtd_total > t_pal_especial-qtd_especial.

              CLEAR: ti_zwm034, wa_zwm034.
              REFRESH ti_zwm034.

              LOOP AT t_zwm034.
                qtd_parcial = qtd_parcial + t_zwm034-qtd.
                IF qtd_parcial <= t_pal_especial-qtd_especial.
                  MOVE-CORRESPONDING t_zwm034 TO ti_zwm034.
                  APPEND ti_zwm034.
                  CLEAR ti_zwm034.
                ELSE.
                  MOVE-CORRESPONDING t_zwm034 TO ti_zwm034.
                  ti_zwm034-qtd = t_pal_especial-qtd_especial.
                  APPEND ti_zwm034.
                  CLEAR ti_zwm034.

                  t_zwm034-qtd =
                    qtd_parcial - t_pal_especial-qtd_especial.

                  MOVE-CORRESPONDING t_zwm034 TO wa_zwm034.
                  EXIT.
                ENDIF.
              ENDLOOP.

              CLEAR t_zwm034.
              REFRESH t_zwm034.

              t_zwm034[] = ti_zwm034[].
*              MODIFY zwm034 FROM TABLE t_zwm034.
*              COMMIT WORK.

            ENDIF.

            SELECT SINGLE maktx INTO desc2
                FROM makt
                    WHERE matnr = t_pal_especial-matnr AND
                          spras = sy-langu.
            CLEAR new_sscc.
            IF lrf_wkqu-devty(5) = '16X20'.
              CALL SCREEN '0005'.
            ELSE.
              CALL SCREEN '0006'.
            ENDIF.
          ENDIF.

          IF NOT wa_zwm034 IS INITIAL.
            MODIFY zwm034 FROM wa_zwm034.
          ENDIF.

          IF wa_zwm034-qtd >= t_pal_especial-qtd_especial.
            CLEAR flag.
          ELSE.
            flag = 'X'.
          ENDIF.

** Inserir na Tabela zwm035 para imprimir tudo no fim

          IF NOT new_sscc IS INITIAL.
            CLEAR t_zwm035.
            REFRESH t_zwm035.
            t_zwm035-armazem = xuser-lgnum.
            t_zwm035-grupo = grupo.
            t_zwm035-sscc = new_sscc.
            APPEND t_zwm035.
            CLEAR t_zwm035.
            INSERT zwm035 FROM TABLE t_zwm035.
            COMMIT WORK.
            CLEAR new_sscc.
          ENDIF.

        ENDWHILE.


        CLEAR t_zwm035.
        REFRESH t_zwm035.
        t_zwm035-armazem = xuser-lgnum.
        t_zwm035-grupo = grupo.
        t_zwm035-sscc = sscc2.
        APPEND t_zwm035.
        CLEAR t_zwm035.

        INSERT zwm035 FROM TABLE t_zwm035.
        COMMIT WORK.
        CLEAR t_zwm035.
        REFRESH t_zwm035.

**   Eliminar da tabela interna o SSCC que ja foi desembalado
        DELETE t_pal_especial WHERE vlenr = sscc2.

        MOVE-CORRESPONDING t_pal_especial TO w_pal_especial.

        READ TABLE t_pal_especial WITH KEY vbeln = w_pal_especial-vbeln
                                           matnr = w_pal_especial-matnr.
        IF sy-subrc <> 0.
*        IF t_pal_especial[] IS INITIAL.

          CLEAR t_zwm034.
          REFRESH t_zwm034.

          SELECT * INTO TABLE t_zwm034
              FROM zwm034
                  WHERE armazem = xuser-lgnum AND
                        remessa = w_pal_especial-vbeln AND
                        material = w_pal_especial-matnr.

          IF NOT t_zwm034[] IS INITIAL.
            CLEAR new_sscc.
            IF lrf_wkqu-devty(5) = '16X20'.
              CALL SCREEN '0005'.
            ELSE.
              CALL SCREEN '0006'.
            ENDIF.

            IF NOT new_sscc IS INITIAL.
              CLEAR t_zwm035.
              REFRESH t_zwm035.
              t_zwm035-armazem = xuser-lgnum.
              t_zwm035-grupo = grupo.
              t_zwm035-sscc = new_sscc.
              APPEND t_zwm035.
              CLEAR t_zwm035.
            ENDIF.

            INSERT zwm035 FROM TABLE t_zwm035.
            COMMIT WORK.

          ENDIF.
        ENDIF.

        CLEAR: t_pal_especial,
               w_pal_especial,
               ok_code_0003,
               cursorfield,
               pos_pulmao,
               sscc1,
               sscc2,
               material,
               qtd,
               uni,
               new_sscc.
        EXIT.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " user_command_0003  INPUT

*&---------------------------------------------------------------------*
*&      Module  exit5  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit5 INPUT.

  CLEAR:  pulmao,
          ok_code_0005,
          cursorfield,
          pospulmao,
          pack_material.

  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " exit5  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_0005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0005 INPUT.

  CHECK NOT pulmao IS INITIAL AND
        NOT pospulmao IS INITIAL AND
        NOT pack_material IS INITIAL.


** Criar novo SSCC
** Criar OT
** Actualzar ZWM013
** Eliminar ZWM034

  CLEAR items.
  REFRESH items.

  LOOP AT t_zwm034.
    items-material = t_zwm034-material.
    items-quantity = t_zwm034-qtd.
    items-unit = t_zwm034-uni.
    items-batch = t_zwm034-lote.
    COLLECT items.
    CLEAR items.
  ENDLOOP.


  CALL FUNCTION 'ZWM_CREATE_HU'
    EXPORTING
      warehouse                  = xuser-lgnum
      plant                      = gv_werks
      s_loc                      = gv_lgort
      packing_material           = pack_material
    IMPORTING
      hukey                      = new_sscc
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
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.

** Apagar as qtds que ja tinham sido desembaladas.

    DELETE zwm034 FROM TABLE t_zwm034.

    CLEAR t_zwm013.
    REFRESH t_zwm013.

    t_zwm013-armazem = xuser-lgnum.
    t_zwm013-sscc = new_sscc.
    t_zwm013-destino = pulmao.
    t_zwm013-bloqueado = 'X'.
    CLEAR t_zwm013-tipo_palete.
    t_zwm013-tipo_palete = 'P6'.
    CLEAR t_zwm013-incidencia.
    t_zwm013-posicao_pulmao = pospulmao.
    CLEAR t_zwm013-fabrica_1.
    APPEND t_zwm013.
    CLEAR t_zwm013.

    INSERT zwm013 FROM TABLE t_zwm013.
    COMMIT WORK.

** Criar a OT
    DATA ti_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.
    DATA to LIKE ltak-tanum.
    CLEAR to.
    CLEAR: return_msg, ti_sscc.
    REFRESH: return_msg, ti_sscc.

    READ TABLE items INDEX 1.

    LOOP AT items.
      ti_sscc-material = items-material.
      ti_sscc-quantidade = items-quantity.
      ti_sscc-uni = items-unit.
      ti_sscc-lote_producao = items-batch.
      APPEND ti_sscc.
      CLEAR ti_sscc.
    ENDLOOP.

    READ TABLE t_zwm034 INDEX 1.

    CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse      = xuser-lgnum
        mov_type       = '991'
        bin_origem     = t_zwm034-remessa
        bin_destino    = t_zwm034-remessa
        plant          = gv_werks
        s_loc          = gv_lgort
        req_number     = grupo
        req_type       = 'H'
        sscc_adicional = new_sscc
      IMPORTING
        to             = to
      TABLES
        return_msg     = return_msg
        sscc           = ti_sscc
      EXCEPTIONS
        error          = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CLEAR: pulmao,
         ok_code_0005,
         cursorfield,
         pospulmao,
         pack_material.

    SET SCREEN '0000'.
    LEAVE SCREEN.

  ENDIF.

ENDMODULE.                 " user_command_0005  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_pulmao  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pulmao INPUT.

  CHECK NOT pulmao IS INITIAL.

  IF pulmao <> pulmao1 AND
     pulmao <> pulmao2.

    CLEAR: text.
    WRITE pulmao TO text LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '168'
        message_var1   = text.

    CLEAR pulmao.
    MOVE 'PULMAO' TO cursorfield.
  ELSE.
    MOVE 'POSPULMAO' TO cursorfield.
  ENDIF.

ENDMODULE.                 " check_pulmao  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_pospulmao  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pospulmao INPUT.

  CHECK NOT pospulmao IS INITIAL.

  SELECT COUNT(*) INTO count
      FROM zwm013
          WHERE armazem = xuser-lgnum AND
                destino = pulmao AND
                posicao_pulmao = pospulmao.

  IF count > 2.
** Erro

  ELSE.

    MOVE 'PACK_MATERIAL' TO cursorfield.

  ENDIF.

ENDMODULE.                 " check_pospulmao  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_pack_material  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pack_material INPUT.

  CHECK NOT pack_material IS INITIAL.

  SELECT SINGLE * FROM mara
                   WHERE matnr = pack_material.
  IF sy-subrc <> 0.

    WRITE pack_material TO text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '070'
        message_var1   = text1.

    CLEAR : pack_material.
    MOVE 'PACK_MATERIAL' TO cursorfield.

  ELSE.

    IF mara-mtart <> 'PALT'.

      WRITE pack_material TO text1 LEFT-JUSTIFIED.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '077'
          message_var1   = text1.

      CLEAR : pack_material.
      MOVE 'PACK_MATERIAL' TO cursorfield.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_pack_material  INPUT

*****************************************************
*
*  FORM GET_PARAMETER
*
*****************************************************
FORM get_parameter  USING    whs
                             module
                             param
                             valor.

  IF ti_zwm001[] IS INITIAL.
    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
      EXPORTING
        whs       = xuser-lgnum
      TABLES
        ti_zwm001 = ti_zwm001.
  ENDIF.

  CLEAR zwm001.
  READ TABLE ti_zwm001 WITH KEY      armazem   = whs
                                     processo  = module
                                     parametro = param
                                     BINARY SEARCH.
  IF sy-subrc = 0.
    MOVE ti_zwm001 TO zwm001.
  ENDIF.
  MOVE zwm001-valor TO valor.

ENDFORM.                    " GET_PARAMETER
*&---------------------------------------------------------------------*
*&      Module  status_0005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0005 OUTPUT.

  SET PF-STATUS 'ZRF2'.
  SET CURSOR FIELD cursorfield.

ENDMODULE.                 " status_0005  OUTPUT
