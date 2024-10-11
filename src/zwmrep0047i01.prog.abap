*&---------------------------------------------------------------------*
*&  Include           ZWMREP0047I01                                    *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CLEAR: bin_origem,cursorfield,ok_code_0001.
  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_origem  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_origem INPUT.
  PERFORM check_origem USING abap_true.
ENDMODULE.                 " check_origem  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  DATA: lt_messages TYPE TAB_BDCMSGCOLL.

  CHECK NOT bin_origem IS INITIAL.

  CLEAR: cursorfield, ok_code_0001, ok_code_0002.

*** Descodifica Bin
*  CALL FUNCTION 'ZWM_DECODE_BIN'
*    EXPORTING
*      iv_lgnum    = lgnum
*      iv_bin_code = bin_origem
*    IMPORTING
*      ev_bin      = bin_origem
*      et_messages = lt_messages
*    EXCEPTIONS
*      error       = 1
*      OTHERS      = 2.
*  IF sy-subrc <> 0.
*    READ TABLE lt_messages INTO DATA(ls_messages) INDEX 1.
*    IF sy-subrc = 0.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = ls_messages-msgid
*          message_lang   = sy-langu
*          message_type   = ls_messages-msgtyp
*          message_number = ls_messages-msgnr
*          message_var1   = ls_messages-msgv1
*          message_var2   = ls_messages-msgv2
*          message_var3   = ls_messages-msgv3
*          message_var4   = ls_messages-msgv4.
*      CLEAR bin_origem.
*      REFRESH lt_messages.
*      RETURN.
*    ENDIF.
*  ENDIF.

  CALL FUNCTION 'ZWM_SPLIT_BIN'
    EXPORTING
      bin   = gv_bind
    IMPORTING
      lgtyp = lgtyp
      lgpla = lgpla.

  l_printer = 'ZCD1'.
  l_copies = 3.
  SET SCREEN '0002'.
  LEAVE SCREEN.

ENDMODULE.                 " USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit2 INPUT.

  CLEAR : ean11, material, quantidade, unidade,
          cursorfield, lote, ok_code_0001, ok_code_0002, bin_origem.
  CLEAR t_items.
  REFRESH t_items.
  SET SCREEN '0001'.
  LEAVE SCREEN.

ENDMODULE.                 " exit2  INPUT

*&---------------------------------------------------------------------*
*&      Module  exit3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit3 INPUT.

  CLEAR : pack_material, cursorfield, ok_code_0003.

  SET SCREEN '0003'.
  LEAVE SCREEN.

ENDMODULE.                 " exit3  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_ean11  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ean11 INPUT.

  CLEAR: t_marm, linhas, material, gv_werks_t.
  REFRESH t_marm.
  CHECK NOT ean11 IS INITIAL.

** FL -> 17/01/2006
  SELECT * INTO TABLE t_marm
      FROM marm
          WHERE ean11 = ean11.

  DELETE t_marm WHERE umrez NE '1'
                   OR umren NE '1'.
** FL <- 17/01/2006

  IF t_marm[] IS INITIAL.

    WRITE ean11 TO text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '135'
        message_var1   = text1.

    CLEAR ean11.
    MOVE 'EAN11' TO cursorfield.
  ELSE.

    DESCRIBE TABLE t_marm LINES linhas.
    IF linhas = 1.
      READ TABLE t_marm INDEX 1.
      material = t_marm-matnr.
      SELECT SINGLE meins INTO unidade
          FROM mara WHERE matnr = material.

********************************************
      CHECK NOT material IS INITIAL AND
            NOT ean11 IS INITIAL.

** Verificar se o material existe e não está marcado para eliminação
      SELECT SINGLE * FROM mara
                      WHERE matnr = material AND
                            lvorm = ' '.
      IF sy-subrc <> 0.

        WRITE material TO text1 LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '070'
            message_var1   = text1.

        CLEAR : ean11, material.
        MOVE 'MATERIAL' TO cursorfield.
        EXIT.
      ENDIF.

      CLEAR : plant,s_loc,return_msg.
      REFRESH : return_msg.

** Verificar se o material está aberto no centro depósito
      CALL FUNCTION 'ZWM_GET_MATERIAL_PLANT_SLOC'
        EXPORTING
          warehouse    = lgnum
          material     = material
          werks        = gv_werks
          lgort        = gv_lgort_d
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
        READ TABLE return_msg INDEX 1.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = return_msg-msgid
            message_lang   = sy-langu
            message_type   = return_msg-msgtyp
            message_number = return_msg-msgnr
            message_var1   = return_msg-msgv1.

        CLEAR : material.
        MOVE 'MATERIAL' TO cursorfield.
        EXIT.
      ENDIF.

*** Validar a classificação do material.
*      SELECT SINGLE *
*          FROM marc
*              WHERE matnr = material AND
*                    werks = plant.
*      IF marc-maabc IS INITIAL.
*
*        WRITE material TO text1 LEFT-JUSTIFIED.
*        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*          EXPORTING
*            message_id     = 'ZWMMSG001'
*            message_lang   = sy-langu
*            message_type   = 'E'
*            message_number = '219'
*            message_var1   = text1.
*
*        CLEAR : ean11, material.
*        MOVE 'MATERIAL' TO cursorfield.
*        EXIT.
*      ENDIF.

** Verifica se existe material na posicao
      IF lgtyp = 'INC'.
        SELECT SINGLE *
            FROM lqua
                WHERE matnr = material AND
                      lgnum = lgnum    AND
                      lgtyp = lgtyp    AND
                      lgpla = lgpla.

        IF sy-subrc <> 0.
          WRITE material TO text1 LEFT-JUSTIFIED.
          WRITE bin_origem TO text2 LEFT-JUSTIFIED.

          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '198'
              message_var1   = text1
              message_var2   = text2.

          CLEAR : material, ean11, text1, text2.
          MOVE 'EAN11' TO cursorfield.
          EXIT.
        ENDIF.
      ENDIF.

      IF NOT t_items[] IS INITIAL.
        CLEAR linhas.
        DESCRIBE TABLE t_items LINES linhas.
        READ TABLE t_items INDEX 1.
        IF t_items-meins = 'PAL'.
          IF linhas = 2.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '204'.

            CLEAR : material, ean11, text1, text2.
            MOVE 'EAN11' TO cursorfield.
            EXIT.
          ENDIF.
        ENDIF.

**   Verificar se o material já foi introduzido para não permitir
**   repeticoes de material. Excepto no caso de paletes remontadas
        READ TABLE t_items WITH KEY matnr = material.
        IF sy-subrc = 0.
          IF t_items-meins = 'PAL'.
            IF z_wm_cl_management=>is_remontada( is_data = t_items ) <> abap_true.
              WRITE material TO text1 LEFT-JUSTIFIED.
              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
                EXPORTING
                  message_id     = 'ZWMMSG001'
                  message_lang   = sy-langu
                  message_type   = 'E'
                  message_number = '201'
                  message_var1   = text1
                  message_var2   = text2.

              CLEAR : material, ean11, text1, text2.
              MOVE 'EAN11' TO cursorfield.
              EXIT.
            ENDIF.
          ELSE.
            WRITE material TO text1 LEFT-JUSTIFIED.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '201'
                message_var1   = text1
                message_var2   = text2.

            CLEAR : material, ean11, text1, text2.
            MOVE 'EAN11' TO cursorfield.
            EXIT.
          ENDIF.
        ENDIF.

**    Verificar se já tem uma Palete Completa, caso se verifique nao
**    permite adicionar mais nada, excepto nas remontadas
        READ TABLE t_items WITH KEY meins = 'PAL'.
        IF sy-subrc = 0.
          IF z_wm_cl_management=>is_remontada( is_data = t_items ) <> abap_true.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '202'
                message_var1   = text1
                message_var2   = text2.

            CLEAR : material, ean11, text1, text2.
            MOVE 'EAN11' TO cursorfield.
            EXIT.
          ENDIF.
        ENDIF.

** Se já tiver materiais não permitir juntar uma pal. completa
        SELECT SINGLE *
           FROM mlgn
             WHERE matnr = material AND
                   lgnum = lgnum.
        IF quantidade = mlgn-lhmg1.
          IF z_wm_cl_management=>is_remontada( is_data = t_items ) <> abap_true.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '203'
                message_var1   = text1
                message_var2   = text2.

            CLEAR : material, ean11, text1, text2.
            MOVE 'EAN11' TO cursorfield.
            EXIT.
          ENDIF.
        ENDIF.

**    Verifica se já foi introduzido algum material com picking
**    variavel, caso se verifique nao permite juntar mais
        READ TABLE t_items INDEX 1.
        IF t_items-meins <> 'PAL'.
          CLEAR mlgt.
          SELECT SINGLE *
              FROM mlgt
                  WHERE matnr = t_items-matnr AND
                        lgnum = lgnum AND
                        lgtyp = 'PCK'.

          IF mlgt-lgpla IS INITIAL.
            CLEAR mlgn.
            SELECT SINGLE *
                FROM mlgn
                    WHERE matnr = t_items-matnr
                      AND lgnum = lgnum.
            IF mlgn-plkpt = 'PKB'.
              WRITE material TO text1 LEFT-JUSTIFIED.

              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
                EXPORTING
                  message_id     = 'ZWMMSG001'
                  message_lang   = sy-langu
                  message_type   = 'E'
                  message_number = '199'
                  message_var1   = text1.

              CLEAR : material, ean11, text1, text2.
              MOVE 'EAN11' TO cursorfield.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
** Verifica se o material que se esta a introduzir é de picking variavel
** Caso se verifique nao permite juntar este material com o(s) que já
** estão
        IF t_items-meins <> 'PAL'.
          CLEAR mlgt.
          SELECT SINGLE *
              FROM mlgt
                  WHERE matnr = material AND
                        lgnum = lgnum AND
                        lgtyp = 'PCK'.

          IF mlgt-lgpla IS INITIAL.
            CLEAR mlgn.
            SELECT SINGLE *
                FROM mlgn
                    WHERE matnr = material
                      AND lgnum = lgnum.
            IF mlgn-plkpt = 'PKB'.

              CALL FUNCTION 'YWM_MESSAGE_SCREEN'
                EXPORTING
                  message_id     = 'ZWMMSG001'
                  message_lang   = sy-langu
                  message_type   = 'E'
                  message_number = '200'
                  message_var1   = text1.

              CLEAR : material, ean11, text1, text2.
              MOVE 'EAN11' TO cursorfield.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      MOVE mara-meins TO unidade.
      MOVE 'QUANTIDADE' TO cursorfield.


*********************************************
      MOVE 'QUANTIDADE' TO cursorfield.
    ELSE.
      SORT t_marm BY matnr.
      MOVE 'MATERIAL' TO cursorfield.
    ENDIF.
  ENDIF.


ENDMODULE.                 " check_ean11  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_material  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_material INPUT.

  CLEAR : text1,
          text2.

  CHECK NOT material IS INITIAL AND
        NOT ean11 IS INITIAL.

** Valida o material com o EAN11 Introduzido
  READ TABLE t_marm WITH KEY matnr = material.
  IF sy-subrc <> 0.
    WRITE: material TO text1 LEFT-JUSTIFIED,
           ean11    TO text2 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '250'
        message_var1   = text1
        message_var2   = text2.

    CLEAR : ean11, material.
    MOVE 'EAN11' TO cursorfield.
    EXIT.
  ENDIF.

** Verificar se o material existe e não está marcado para eliminação
  SELECT SINGLE * FROM mara
                  WHERE matnr = material AND
                        lvorm = ' '.
  IF sy-subrc <> 0.

    WRITE material TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '070'
        message_var1   = text1.

    CLEAR : ean11, material.
    MOVE 'EAN11' TO cursorfield.
    EXIT.
  ENDIF.

  CLEAR : plant,s_loc,return_msg.
  REFRESH : return_msg.

** Verificar se o material está aberto no centro depósito
  CALL FUNCTION 'ZWM_GET_MATERIAL_PLANT_SLOC'
    EXPORTING
      warehouse    = lgnum
      werks        = gv_werks
      lgort        = gv_lgort_d
      material     = material
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
    READ TABLE return_msg INDEX 1.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = return_msg-msgid
        message_lang   = sy-langu
        message_type   = return_msg-msgtyp
        message_number = return_msg-msgnr
        message_var1   = return_msg-msgv1.

    CLEAR : material.
    MOVE 'MATERIAL' TO cursorfield.
    EXIT.
  ENDIF.

*** Validar a classificação do material.
*  SELECT SINGLE *
*      FROM marc
*          WHERE matnr = material AND
*                werks = plant.
*  IF marc-maabc IS INITIAL.
*
*    WRITE material TO text1 LEFT-JUSTIFIED.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '219'
*        message_var1   = text1.
*
*    CLEAR : ean11, material.
*    MOVE 'EAN11' TO cursorfield.
*    EXIT.
*  ENDIF.

** Verifica se existe material na posicao
  IF lgtyp = 'INC'.
    SELECT SINGLE *
        FROM lqua
            WHERE matnr = material AND
                  lgnum = lgnum AND
                  lgtyp = lgtyp AND
                  lgpla = lgpla.
    IF sy-subrc <> 0.
      WRITE material TO text1 LEFT-JUSTIFIED.
      WRITE bin_origem TO text2 LEFT-JUSTIFIED.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '198'
          message_var1   = text1
          message_var2   = text2.

      CLEAR : material, ean11, text1, text2.
      MOVE 'EAN11' TO cursorfield.
      EXIT.
    ENDIF.
  ENDIF.


  IF NOT t_items[] IS INITIAL.

    CLEAR linhas.
    DESCRIBE TABLE t_items LINES linhas.
    READ TABLE t_items INDEX 1.
    IF t_items-meins = 'PAL'.
      IF linhas = 2.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '204'.

        CLEAR : material, ean11, text1, text2.
        MOVE 'EAN11' TO cursorfield.
        EXIT.
      ENDIF.
    ENDIF.

**   Verificar se o material já foi introduzido para não permitir
**   repeticoes de material. Excepto no caso de paletes remontadas
    READ TABLE t_items WITH KEY matnr = material.
    IF sy-subrc = 0.
      IF t_items-meins = 'PAL'.
        IF z_wm_cl_management=>is_remontada( is_data = t_items ) <> abap_true.
          WRITE material TO text1 LEFT-JUSTIFIED.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '201'
              message_var1   = text1
              message_var2   = text2.

          CLEAR : material, ean11, text1, text2.
          MOVE 'EAN11' TO cursorfield.
          EXIT.
        ENDIF.
      ELSE.
        WRITE material TO text1 LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '201'
            message_var1   = text1
            message_var2   = text2.

        CLEAR : material, ean11, text1, text2.
        MOVE 'EAN11' TO cursorfield.
        EXIT.
      ENDIF.
    ENDIF.

**    Verificar se já tem uma Palete Completa, caso se verifique nao
**    permite adicionar mais nada, excepto nas remontadas
    READ TABLE t_items WITH KEY meins = 'PAL'.
    IF sy-subrc = 0.
      IF z_wm_cl_management=>is_remontada( is_data = t_items ) <> abap_true.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '202'
            message_var1   = text1
            message_var2   = text2.

        CLEAR : material, ean11, text1, text2.
        MOVE 'EAN11' TO cursorfield.
        EXIT.
      ENDIF.
    ENDIF.

** Se já tiver materiais não permitir juntar uma pal. completa
    SELECT SINGLE *
       FROM mlgn
         WHERE matnr = material AND
               lgnum = lgnum.
    IF quantidade = mlgn-lhmg1.
      IF z_wm_cl_management=>is_remontada( is_data = t_items ) <> abap_true.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '203'
            message_var1   = text1
            message_var2   = text2.

        CLEAR : material, ean11, text1, text2.
        MOVE 'EAN11' TO cursorfield.
        EXIT.
      ENDIF.
    ENDIF.

**    Verifica se já foi introduzido algum material com picking
**    variavel, caso se verifique nao permite juntar mais
    READ TABLE t_items INDEX 1.
    IF t_items-meins <> 'PAL'.

      CLEAR mlgt.
      SELECT SINGLE *
          FROM mlgt
              WHERE matnr = t_items-matnr AND
                    lgnum = lgnum AND
                    lgtyp = 'PCK'.

      IF mlgt-lgpla IS INITIAL.
        CLEAR mlgn.
        SELECT SINGLE *
            FROM mlgn
                WHERE matnr = t_items-matnr
                  AND lgnum = lgnum.
        IF mlgn-plkpt = 'PKB'.
          WRITE material TO text1 LEFT-JUSTIFIED.

          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '199'
              message_var1   = text1.

          CLEAR : material, ean11, text1, text2.
          MOVE 'EAN11' TO cursorfield.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
** Verifica se o material que se esta a introduzir é de picking variavel
** Caso se verifique nao permite juntar este material com o(s) que já
** estão
    IF t_items-meins <> 'PAL'.
      CLEAR mlgt.
      SELECT SINGLE *
          FROM mlgt
              WHERE matnr = material AND
                    lgnum = lgnum AND
                    lgtyp = 'PCK'.

      IF mlgt-lgpla IS INITIAL.
        CLEAR mlgn.
        SELECT SINGLE *
            FROM mlgn
                WHERE matnr = material
                  AND lgnum = lgnum.
        IF mlgn-plkpt = 'PKB'.

          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '200'
              message_var1   = text1.

          CLEAR : material, ean11, text1, text2.
          MOVE 'EAN11' TO cursorfield.
          EXIT.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.
  MOVE mara-meins TO unidade.
  MOVE 'QUANTIDADE' TO cursorfield.


ENDMODULE.                 " check_material  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_quantidade  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_quantidade INPUT.
  DATA gv_werks_d TYPE werks_d. " << INS ROFF(SDF):TMGP:22.01.2016 16:44:08

  gv_werks_d = gv_werks.

*& Begin of Modification by Tiago Pateiro - ROFF @ 20.01.2016 11:20:25
**  SELECT werks UP TO 1 ROWS
**    FROM t320 INTO gv_werks_d
**    WHERE lgort EQ 'CD'
**      AND lgnum EQ lgnum.
**  ENDSELECT.
**  IF sy-subrc NE 0.
**    gv_werks_d = 'RENV'.
**  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 20.01.2016 11:20:26

  CHECK NOT material IS INITIAL AND
        NOT ean11 IS INITIAL AND
        NOT quantidade IS INITIAL.

  SELECT SINGLE *
    FROM mlgn
      WHERE matnr = material
        AND lgnum = lgnum.

  IF quantidade > mlgn-lhmg1.
    WRITE quantidade TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '143'
        message_var1   = text1.

    CLEAR : quantidade.
    MOVE 'QUANTIDADE' TO cursorfield.
    EXIT.
  ENDIF.


  IF NOT t_items[] IS INITIAL.
    READ TABLE t_items INDEX 1.
    IF t_items-meins = 'PAL'.

      SELECT SINGLE * FROM mara WHERE matnr = t_items-matnr.
      CLEAR qtd_out.
      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
        EXPORTING
          input                = t_items-verme
          kzmeinh              = 'X'
          matnr                = t_items-matnr
          meinh                = t_items-meins
          meins                = mara-meins
        IMPORTING
          output               = qtd_out
        EXCEPTIONS
          conversion_not_found = 1
          input_invalid        = 2
          material_not_found   = 3
          meinh_not_found      = 4
          meins_missing        = 5
          no_meinh             = 6
          output_invalid       = 7
          overflow             = 8
          OTHERS               = 9.
      IF sy-subrc <> 0.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF qtd_out <> mlgn-lhmg1.
        WRITE quantidade TO text1 LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '143'
            message_var1   = text1.
        CLEAR : quantidade.
        MOVE 'QUANTIDADE' TO cursorfield.
        EXIT.
      ENDIF.

      IF quantidade <> mlgn-lhmg1.
        WRITE quantidade TO text1 LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '143'
            message_var1   = text1.
        CLEAR : quantidade.
        MOVE 'QUANTIDADE' TO cursorfield.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  DATA qtd_aux LIKE lqua-verme.
  CLEAR qtd_aux.
**  Validar qtd em WM
  IF lgtyp = 'INC'.
    CLEAR: t_lqua, qtd_total.
    REFRESH t_lqua.

    READ TABLE t_items WITH KEY matnr = material.
    IF sy-subrc = 0.

      SELECT SINGLE * FROM mara WHERE matnr = t_items-matnr.
      CLEAR qtd_out.
      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
        EXPORTING
          input                = t_items-verme
          kzmeinh              = 'X'
          matnr                = t_items-matnr
          meinh                = t_items-meins
          meins                = mara-meins
        IMPORTING
          output               = qtd_out
        EXCEPTIONS
          conversion_not_found = 1
          input_invalid        = 2
          material_not_found   = 3
          meinh_not_found      = 4
          meins_missing        = 5
          no_meinh             = 6
          output_invalid       = 7
          overflow             = 8
          OTHERS               = 9.
      IF sy-subrc <> 0.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      qtd_aux = quantidade + qtd_out.
    ELSE.
      qtd_aux = quantidade.
    ENDIF.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_lqua
        FROM lqua
            WHERE matnr = material AND
                  lgnum = lgnum AND
                  lgtyp = lgtyp AND
                  lgpla = lgpla.

*    SORT t_lqua BY verme DESCENDING.
    LOOP AT t_lqua.
      IF t_lqua-verme >= qtd_aux.
        qtd_total = t_lqua-verme.
        lote = t_lqua-charg.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF qtd_total < qtd_aux.
      WRITE quantidade TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '143'
          message_var1   = text1.
      CLEAR : quantidade.
      MOVE 'QUANTIDADE' TO cursorfield.
    ELSE.
*      IF t_items-charg <> lote.
      t_items-charg = lote.
*        MODIFY t_items INDEX 1.
*      ENDIF.
      MOVE 'QUANTIDADE' TO cursorfield.
    ENDIF.
  ELSEIF lgtyp = 'DEV'.

    READ TABLE t_items WITH KEY matnr = material.
    IF sy-subrc = 0.

      SELECT SINGLE * FROM mara WHERE matnr = t_items-matnr.
      CLEAR: qtd_out, qtd_aux.
      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
        EXPORTING
          input                = t_items-verme
          kzmeinh              = 'X'
          matnr                = t_items-matnr
          meinh                = t_items-meins
          meins                = mara-meins
        IMPORTING
          output               = qtd_out
        EXCEPTIONS
          conversion_not_found = 1
          input_invalid        = 2
          material_not_found   = 3
          meinh_not_found      = 4
          meins_missing        = 5
          no_meinh             = 6
          output_invalid       = 7
          overflow             = 8
          OTHERS               = 9.
      IF sy-subrc <> 0.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      qtd_aux = quantidade + qtd_out.
    ELSE.
      qtd_aux = quantidade.
    ENDIF.

**  Validar quantidade em MM
    SELECT SINGLE *
        FROM mchb
            WHERE matnr = material AND
*                  werks = 'RENV' AND " << DEL ROFF(SDF):TMGP:22.01.2016 16:43:37
                  werks = gv_werks_d AND " << INS ROFF(SDF):TMGP:22.01.2016 16:43:40
                  lgort = gv_lgort_o AND
                  clabs >= qtd_aux.
    IF sy-subrc <> 0.
      WRITE quantidade TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '143'
          message_var1   = text1.
      CLEAR : quantidade.
      MOVE 'QUANTIDADE' TO cursorfield.
    ELSE.
      lote = mchb-charg.
      MOVE 'QUANTIDADE' TO cursorfield.
    ENDIF.
  ENDIF.

**  se a quantidade for completa
  IF quantidade = mlgn-lhmg1.
    quantidade = 1.
    unidade    = 'PAL'.
  ENDIF.
*  ENDIF.
ENDMODULE.                 " check_quantidade  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_pack_material  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pack_material INPUT.

  CHECK NOT pack_material IS INITIAL.

** verificar se o tipo do packing material é = 'PALT' e se existe no SAP
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
    MOVE 'PACK_MATERIAL' TO cursorfield.
  ENDIF.


ENDMODULE.                 " check_pack_material  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0002 INPUT.
  PERFORM user_command_0002.


ENDMODULE.                 " user_command_0002  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0003 INPUT.

  CHECK NOT pack_material IS INITIAL.

  LEAVE TO SCREEN '0000'.

ENDMODULE.                 " user_command_0003  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_SSCC_ORIGEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc_origem INPUT.
  PERFORM check_sscc_origem.
ENDMODULE.                 " CHECK_SSCC_ORIGEM  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0004 INPUT.
  PERFORM user_command_0004.
ENDMODULE.                 " USER_COMMAND_0004  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_lote INPUT.
  PERFORM check_lote.
ENDMODULE.                 " CHECK_LOTE  INPUT
