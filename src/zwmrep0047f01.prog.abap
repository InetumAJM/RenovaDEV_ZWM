*&---------------------------------------------------------------------*
*&  Include           ZWMREP0047F01                                    *
*&---------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Form  CHECK_SSCC_ORIGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_sscc_origem.
  DATA: lt_vepo TYPE TABLE OF vepo,
        lt_mlgn TYPE TABLE OF mlgn.

  DATA: ls_vepo TYPE vepo,
        ls_vekp TYPE vekp,
        ls_mlgn TYPE mlgn,
        ls_lein TYPE lein.

  DATA: lv_count TYPE sytabix,
        lv_lines TYPE sytabix.

  CLEAR: gv_werks_t.

  DATA: lr_werks TYPE RANGE OF werks_d.

  DATA: ls_r_werks LIKE LINE OF lr_werks.


** Dados de Armzem
***********************************************************************
  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user      = sy-uname
      i_exidv     = sscc_origem
      i_matnr     = ls_vepo-matnr
      i_recall    = 'X'
      i_usewm     = 'X'
      i_usemm     = 'X'
      i_useaut    = 'X'
      i_get_lgnum = 'X'
      i_get_werks = 'X'
      i_get_lgort = ''
    CHANGING
      c_lgnum     = lgnum
      c_werks     = gv_werks
    EXCEPTIONS
      error       = 1
      user_back   = 2
      OTHERS      = 3.


  PERFORM check_origem USING abap_false.
**********************************************************************


  DO 1 TIMES.
    SELECT SINGLE * FROM vekp
                    INTO ls_vekp
                    WHERE exidv = sscc_origem.
    CHECK sy-subrc EQ 0.

    SELECT * FROM vepo
             INTO TABLE lt_vepo
             WHERE venum = ls_vekp-venum.

    pack_material = ls_vekp-vhilm.
  ENDDO.

  IF sy-subrc <> 0.
**  SSCC & inválido
    CALL FUNCTION 'ZWM_RF_MESSAGE'
      EXPORTING
        i_message_id     = 'ZWM001'
        i_message_type   = 'E'
        i_message_number = '068'
        i_message_var1   = sscc_origem.
    CLEAR: sscc_origem.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM lein
                  INTO ls_lein
                  WHERE lenum = sscc_origem.

  IF sy-subrc EQ 0.
**  SSCC & inválido, já presente em Armazem
    CALL FUNCTION 'ZWM_RF_MESSAGE'
      EXPORTING
        i_message_id     = 'ZWM001'
        i_message_type   = 'E'
        i_message_number = '069'
        i_message_var1   = sscc_origem.
    CLEAR: sscc_origem.
    EXIT.
  ENDIF.

  CLEAR: t_items.
  READ TABLE t_items
    WITH KEY sscc = sscc_origem.
  IF sy-subrc EQ 0.
**  SSCC & já lido
    CALL FUNCTION 'ZWM_RF_MESSAGE'
      EXPORTING
        i_message_id     = 'ZWM001'
        i_message_type   = 'E'
        i_message_number = '070'
        i_message_var1   = sscc_origem.
    CLEAR: sscc_origem.
    EXIT.
  ENDIF.

  SELECT * FROM mlgn
           INTO TABLE lt_mlgn
           FOR ALL ENTRIES IN lt_vepo
           WHERE matnr = lt_vepo-matnr AND
                 lgnum = lgnum.
  SORT lt_mlgn BY matnr.

  CLEAR: ls_vepo.
  READ TABLE lt_vepo
        INTO ls_vepo
        INDEX 1.


  CLEAR: ls_mlgn.
  READ TABLE lt_mlgn
        INTO ls_mlgn
        WITH KEY matnr = ls_vepo-matnr
        BINARY SEARCH.

  IF NOT t_items-matnr IS INITIAL AND
      ls_vepo-matnr <> t_items-matnr.
**  Material & diferente do da Palete &
    CALL FUNCTION 'ZWM_RF_MESSAGE'
      EXPORTING
        i_message_id     = 'ZWM001'
        i_message_type   = 'E'
        i_message_number = '071'
        i_message_var1   = ls_vepo-matnr
        i_message_var2   = t_items-sscc.
    CLEAR: sscc_origem.
    EXIT.
  ENDIF.


  CLEAR: t_items.
  t_items-matnr = ls_vepo-matnr.
  t_items-charg = ls_vepo-charg.
  t_items-verme = ls_vepo-vemng.
  t_items-meins = ls_vepo-vemeh.
  t_items-letyp = ls_mlgn-lety1.
  t_items-sscc  = sscc_origem.

  DELETE FROM zwm020 WHERE armazem = lgnum AND
                           ( p1 = sscc_origem OR p2 = sscc_origem ).
  COMMIT WORK.

  APPEND t_items.


  IF z_wm_cl_management=>is_remontada( is_data = t_items ) EQ abap_true.
    lv_count = 2.
  ELSE.
    lv_count = 1.
  ENDIF.

  DESCRIBE TABLE t_items LINES lv_lines.
  IF lv_lines < lv_count.
    CLEAR: sscc_origem.
    EXIT.
  ENDIF.


  PERFORM save_0002.
ENDFORM.                    " CHECK_SSCC_ORIGEM

*}   INSERT
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0002 .
  CASE ok_code_0002.

    WHEN 'CONF'.
      CHECK NOT material IS INITIAL AND
            NOT ean11 IS INITIAL AND
            NOT quantidade IS INITIAL AND
            NOT unidade IS INITIAL.

      IF gv_get_batch EQ 'X' AND
         lote IS INITIAL.
        EXIT.
      ENDIF.

      IF lgtyp = 'DEV'.
        CLEAR items.
        REFRESH items.

        items-material = material.
        items-quantidade = quantidade.
        items-uni = unidade.
        items-lote = lote.
        APPEND items.
        CLEAR zwm001.
        SELECT SINGLE *
            FROM zwm001
                WHERE armazem = lgnum AND
                      processo = 'ENTRADA_DEVOLUCAO'  AND
                      parametro = 'CODE'.

        MOVE zwm001-valor TO code.
        CLEAR zwm001.
        SELECT SINGLE *
            FROM zwm001
                WHERE armazem = lgnum AND
                      processo = 'ENTRADA_DEVOLUCAO' AND
                      parametro = 'MOV_MM'.

        MOVE zwm001-valor TO mov_mm.

        CLEAR return_msg.
        REFRESH return_msg.

** RL -> INS 02.05.2005
        FREE: itab_aux.
        CLEAR: itab_aux.

        itab_aux[] = items[].

        LOOP AT itab_aux.
          l_index = sy-tabix.

          CLEAR: l_meins.
          SELECT SINGLE meins FROM mara
          INTO l_meins
          WHERE matnr EQ itab_aux-material.

          CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
            EXPORTING
              input                = itab_aux-quantidade
              kzmeinh              = 'X'
              matnr                = itab_aux-material
              meinh                = itab_aux-uni
              meins                = l_meins
            IMPORTING
              output               = itab_aux-quantidade
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

          itab_aux-uni = l_meins.
          MODIFY itab_aux INDEX l_index.
        ENDLOOP.
** RL <- INS 02.05.2005

        CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
          EXPORTING
            lgnum            = lgnum
            code             = code
            mov_mm           = mov_mm
            testrun          = 'T'
            plant_o          = gv_werks " << INS ROFF(SDF):TMGP:22.01.2016 16:39:53
            plant_d          = gv_werks
            sloc_o           = gv_lgort_o
            sloc_d           = gv_lgort_d
          IMPORTING
            materialdocument = mblnr
            matdocumentyear  = gjahr
          TABLES
            return_msg       = return_msg
** RL -> MOD 02.05.2005
*           items            = items
            items            = itab_aux
** RL <- MOD 02.05.2005
          EXCEPTIONS
            error            = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          READ TABLE return_msg INDEX 1.
          IF sy-subrc = 0.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = return_msg-msgid
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = return_msg-msgnr
                message_var1   = return_msg-msgv1
                message_var2   = return_msg-msgv2
                message_var3   = return_msg-msgv3
                message_var4   = return_msg-msgv4
              IMPORTING
                ret_code       = retcode.

            IF retcode = 'O'.
              FREE: items, itab_aux.
              CLEAR: items, itab_aux.

              CLEAR : ok_code_0002.
              SET SCREEN '0002'.
              LEAVE SCREEN.
            ENDIF.
          ENDIF.
** RL -> INS 11.05.2005
** Fazer simulação da transferência da Palete
*        ELSE.
*          FREE: itab_aux, return_msg, itab_sum.
*          CLEAR: itab_aux, return_msg, g_mblnr, g_gjahr, l_mov_mm,
*                 l_linha, itab_sum.
*
*          CLEAR zwm001.
*          SELECT SINGLE *
*              FROM zwm001
*                  WHERE armazem = lgnum AND
*                        processo = 'ENTRADA_PRODUCAO' AND
*                        parametro = 'MOV_PAL'.
*
*          MOVE zwm001-valor TO l_mov_mm.
*
*          DESCRIBE TABLE t_items LINES l_linha.
*
*          IF l_linha = 1.
*            READ TABLE t_items INDEX 1.
*
*            CLEAR: marm, l_umrez.
*            SELECT SINGLE umrez FROM marm
*            INTO l_umrez
*            WHERE matnr EQ t_items-matnr
*              AND meinh EQ 'PAL'.
*
*            IF z_wm_cl_management=>is_remontada( is_data = t_items ) eq abap_true.
*              l_umrez = l_umrez * 2.
*
*              IF l_umrez EQ t_items-verme.
*                itab_aux-quantidade = 2.
*              ELSE.
*                itab_aux-quantidade = 1.
*              ENDIF.
*            ELSE.
*              itab_aux-quantidade = 1.
*            ENDIF.
*
*            itab_aux-material = pack_material.
*            itab_aux-uni = 'PAL'.
*            APPEND itab_aux.
*          ELSE.
*            SORT t_items BY matnr.
*
*            LOOP AT t_items.
*              MOVE-CORRESPONDING t_items TO itab_sum.
*              COLLECT itab_sum.
*              CLEAR: itab_sum.
*              SORT itab_sum BY matnr.
*            ENDLOOP.
*
*            LOOP AT itab_sum WHERE letyp in in z_wm_cl_management=>r_letyp( lgnum ).
*
*              IF itab_sum-meins = 'PAL'.
*                itab_aux-material = pack_material.
*                itab_aux-quantidade = itab_sum-verme.
*                itab_aux-uni = 'PAL'.
*                APPEND itab_aux.
*              ELSE.
*                CLEAR: marm, l_umrez.
*                SELECT SINGLE umrez FROM marm
*                INTO l_umrez
*                WHERE matnr EQ itab_sum-matnr
*                  AND meinh EQ 'PAL'.
*
*                l_umrez = l_umrez * 2.
*
*                IF l_umrez EQ itab_sum-verme.
*                  itab_aux-material = pack_material.
*                  itab_aux-quantidade = 2.
*                  itab_aux-uni = 'PAL'.
*                  APPEND itab_aux.
*                ENDIF.
*              ENDIF.
*            ENDLOOP.
*            IF itab_aux[] IS INITIAL.
*              itab_aux-material = pack_material.
*              itab_aux-quantidade = 1.
*              itab_aux-uni = 'PAL'.
*              APPEND itab_aux.
*            ENDIF.
*          ENDIF.
*
*          IF NOT itab_aux[] IS INITIAL.
*            CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
*              EXPORTING
*                lgnum            = lgnum
*                code             = code
*                mov_mm           = l_mov_mm
*                testrun          = 'T'
*                plant_o          = 'RENV'
*                sloc_o           = 'DEV'
*              IMPORTING
*                materialdocument = g_mblnr
*                matdocumentyear  = g_gjahr
*              TABLES
*                return_msg       = return_msg
*                items            = itab_aux
*              EXCEPTIONS
*                error            = 1
*                OTHERS           = 2.
*
*            IF sy-subrc <> 0.
*              READ TABLE return_msg INDEX 1.
*              IF sy-subrc = 0.
*                CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*                  EXPORTING
*                    message_id     = return_msg-msgid
*                    message_lang   = sy-langu
*                    message_type   = 'E'
*                    message_number = return_msg-msgnr
*                    message_var1   = return_msg-msgv1
*                    message_var2   = return_msg-msgv2
*                    message_var3   = return_msg-msgv3
*                    message_var4   = return_msg-msgv4
*                  IMPORTING
*                    ret_code       = retcode.
*
*                IF retcode = 'O'.
*                  FREE: t_items.
*                  CLEAR: t_items.
*
*                  CLEAR : ok_code_0002.
*                  SET SCREEN '0002'.
*                  LEAVE SCREEN.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*** RL <- INS 11.05.2005
        ENDIF.
      ENDIF. "DEV

      MOVE material TO t_items-matnr.
      MOVE lote TO t_items-charg.
      MOVE quantidade TO t_items-verme.
      MOVE unidade TO t_items-meins.

      CLEAR mlgn.
      SELECT SINGLE *
       FROM mlgn
         WHERE matnr = material AND
               lgnum = lgnum.

      t_items-letyp = mlgn-lety1.
      APPEND t_items.
      CLEAR t_items.

      CLEAR : ean11, material, quantidade, unidade,
              cursorfield, lote, ok_code_0002.

      SET SCREEN '0002'.
      LEAVE SCREEN.


    WHEN 'SAVE'.
      PERFORM save_0002.
  ENDCASE.
ENDFORM.                    " USER_COMMAND_0002
*&---------------------------------------------------------------------*
*&      Form  SAVE_0002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_0002.
  DATA: gv_lgort_in TYPE lgort_d VALUE 'DEV'.
  DATA: gv_lgort_out TYPE lgort_d VALUE 'CD'.
  DATA: lv_werks_d TYPE werks_d.

  CHECK NOT t_items[] IS INITIAL.



** Chamar ecra para pedir material de embalagem
  IF gv_mode <> gc_mode_hu.
    CLEAR pack_material.
    CALL SCREEN '0003'.
  ENDIF.

  IF gv_lgort_mode EQ 'X'.
    gv_lgort_in = bin_origem.
    lv_werks_d  = gv_werks.
  ELSE.
    gv_lgort_in  = gv_lgort_o.
    gv_lgort_out = gv_lgort_d.
    lv_werks_d  = gv_werks.
  ENDIF.

** RL -> INS 02.05.2005
** Para os Dois Casos é necessário criar HU
  CLEAR: return_msg, items_hu, itab_sscc.
  REFRESH: return_msg, items_hu, itab_sscc.

  READ TABLE t_items INDEX 1.

  IF gv_mode <> gc_mode_hu.
    PERFORM create_hu.
    CHECK NOT hukey IS INITIAL.
  ENDIF.


  IF lgtyp = 'DEV' OR gv_lgort_mode EQ 'X'.
** Transferencia de Material das Devoluçoes
    CLEAR items.
    REFRESH items.
    LOOP AT t_items.
      items-material = t_items-matnr.
      items-quantidade = t_items-verme.
      items-uni = t_items-meins.
      items-lote = t_items-charg.
      APPEND items.
    ENDLOOP.

    CLEAR zwm001.
    SELECT SINGLE *
      FROM zwm001
          WHERE armazem = lgnum AND
                processo = 'ENTRADA_DEVOLUCAO'  AND
                parametro = 'CODE'.

    MOVE zwm001-valor TO code.
    CLEAR zwm001.
    SELECT SINGLE *
        FROM zwm001
            WHERE armazem = lgnum AND
                  processo = 'ENTRADA_DEVOLUCAO' AND
                  parametro = 'MOV_MM'.

    MOVE zwm001-valor TO mov_mm.

    CLEAR return_msg.
    REFRESH return_msg.

** RL -> INS 02.04.2005
    FREE: itab_aux.
    CLEAR: itab_aux.

    itab_aux[] = items[].

    LOOP AT itab_aux.
      l_index = sy-tabix.

      CLEAR: l_meins.
      SELECT SINGLE meins FROM mara
      INTO l_meins
      WHERE matnr EQ itab_aux-material.

      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
        EXPORTING
          input                = itab_aux-quantidade
          kzmeinh              = 'X'
          matnr                = itab_aux-material
          meinh                = itab_aux-uni
          meins                = l_meins
        IMPORTING
          output               = itab_aux-quantidade
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

      itab_aux-uni = l_meins.
      MODIFY itab_aux INDEX l_index.
    ENDLOOP.
** RL <- INS 02.05.2005

    CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
      EXPORTING
        lgnum            = lgnum
        code             = code
        mov_mm           = mov_mm
        testrun          = ' '
        plant_o          = gv_werks " << INS ROFF(SDF):TMGP:22.01.2016 16:40:41
        plant_d          = lv_werks_d
        sloc_o           = gv_lgort_in
        sloc_d           = gv_lgort_out
      IMPORTING
        materialdocument = mblnr
        matdocumentyear  = gjahr
      TABLES
        return_msg       = return_msg
** RL -> MOD 02.05.2005
*       items            = items
        items            = itab_aux
** RL <- MOD 02.05.2005
      EXCEPTIONS
        error            = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      READ TABLE return_msg INDEX 1.
      IF sy-subrc = 0.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = return_msg-msgid
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = return_msg-msgnr
            message_var1   = return_msg-msgv1
            message_var2   = return_msg-msgv2
            message_var3   = return_msg-msgv3
            message_var4   = return_msg-msgv4
          IMPORTING
            ret_code       = retcode.

        IF retcode = 'O'.
          FREE: t_items.
          CLEAR: t_items.

          CASE gv_mode.
            WHEN gc_mode_hu.
              CLEAR : ok_code_0004.
              SET SCREEN '0004'.
            WHEN OTHERS.
              CLEAR : ok_code_0002.
              SET SCREEN '0002'.
          ENDCASE.

          LEAVE SCREEN.
        ENDIF.
      ENDIF.
** RL -> INS 11.05.2005
** Fazer a transferência da Palete
    ELSE.
      FREE: itab_aux, return_msg, itab_sum.
      CLEAR: itab_aux, return_msg, g_mblnr, g_gjahr, l_mov_mm,
             l_linha, itab_sum.

      CLEAR zwm001.
      SELECT SINGLE *
          FROM zwm001
              WHERE armazem = lgnum AND
                    processo = 'ENTRADA_PRODUCAO' AND
                    parametro = 'MOV_PAL'.

      MOVE zwm001-valor TO l_mov_mm.

      DESCRIBE TABLE t_items LINES l_linha.

      IF l_linha = 1.
        READ TABLE t_items INDEX 1.

        CLEAR: marm, l_umrez.
        SELECT SINGLE umrez FROM marm
        INTO l_umrez
        WHERE matnr EQ t_items-matnr
          AND meinh EQ 'PAL'.

        IF z_wm_cl_management=>is_remontada( is_data = t_items ) EQ abap_true.
          l_umrez = l_umrez * 2.

          IF l_umrez EQ t_items-verme.
            itab_aux-quantidade = 2.
          ELSE.
            itab_aux-quantidade = 1.
          ENDIF.
        ELSE.
          itab_aux-quantidade = 1.
        ENDIF.

        itab_aux-material = pack_material.
        itab_aux-uni = 'PAL'.
        APPEND itab_aux.

** INI - Entrada de Meia Palete.
        SELECT SINGLE *
           FROM mlgn
               WHERE matnr = t_items-matnr
                 AND lgnum = lgnum
                 AND ( block = '03' OR block = '04' ).
        IF sy-subrc = 0.
          SELECT SINGLE *
              FROM zwm001
                  WHERE armazem = lgnum AND
                        processo = 'MEIA-PALETE' AND
                        parametro = pack_material.
          IF sy-subrc = 0.
            itab_aux-material = zwm001-valor.
            itab_aux-quantidade = 2.
            itab_aux-uni = 'PAL'.
            APPEND itab_aux.
          ENDIF.
        ENDIF.
** FIM - Entrada de Meia Palete.

** INI - Entrada de Quarto Palete.
        SELECT SINGLE *
           FROM mlgn
               WHERE matnr = t_items-matnr
                 AND lgnum = lgnum
                 AND ( block = '07' OR block = '08' ).
        IF sy-subrc = 0.
          SELECT SINGLE *
              FROM zwm001
                  WHERE armazem = lgnum            AND
                        processo = 'QUARTO-PALETE' AND
                        parametro = pack_material.
          IF sy-subrc = 0.
            itab_aux-material = zwm001-valor.
            itab_aux-quantidade = 4.
            itab_aux-uni = 'PAL'.
            APPEND itab_aux.
          ENDIF.
        ENDIF.
** FIM - Entrada de Quarto Palete.

      ELSE.
        SORT t_items BY matnr.

        LOOP AT t_items.
          MOVE-CORRESPONDING t_items TO itab_sum.
          COLLECT itab_sum.
          CLEAR: itab_sum.
          SORT itab_sum BY matnr.
        ENDLOOP.

        LOOP AT itab_sum WHERE letyp IN z_wm_cl_management=>r_letyp_remontada( lgnum ).

          IF itab_sum-meins = 'PAL'.
            itab_aux-material = pack_material.
            itab_aux-quantidade = itab_sum-verme.
            itab_aux-uni = 'PAL'.
            APPEND itab_aux.

** INI - Entrada de Meia Palete.
            SELECT SINGLE *
                FROM mlgn
                    WHERE matnr = itab_sum-matnr
                      AND lgnum = lgnum
                      AND ( block = '03' OR block = '04' ).
            IF sy-subrc = 0.
              SELECT SINGLE *
                  FROM zwm001
                      WHERE armazem = lgnum AND
                            processo = 'MEIA-PALETE' AND
                            parametro = pack_material.
              IF sy-subrc = 0.
                itab_aux-material = zwm001-valor.
                itab_aux-quantidade = 4.
                itab_aux-uni = 'PAL'.
                APPEND itab_aux.
              ENDIF.
            ENDIF.
** FIM - Entrada de Meia Palete.

** INI - Entrada de Quarto Palete.
            SELECT SINGLE *
               FROM mlgn
                   WHERE matnr = t_items-matnr
                     AND lgnum = lgnum
                     AND ( block = '07' OR block = '08' ).
            IF sy-subrc = 0.
              SELECT SINGLE *
                  FROM zwm001
                      WHERE armazem = lgnum            AND
                            processo = 'QUARTO-PALETE' AND
                            parametro = pack_material.
              IF sy-subrc = 0.
                itab_aux-material = zwm001-valor.
                itab_aux-quantidade = 8.
                itab_aux-uni = 'PAL'.
                APPEND itab_aux.
              ENDIF.
            ENDIF.
** FIM - Entrada de Quarto Palete.

          ELSE.
            CLEAR: marm, l_umrez.
            SELECT SINGLE umrez FROM marm
            INTO l_umrez
            WHERE matnr EQ itab_sum-matnr
              AND meinh EQ 'PAL'.

            l_umrez = l_umrez * 2.

            IF l_umrez EQ itab_sum-verme.
              itab_aux-material = pack_material.
              itab_aux-quantidade = 2.
              itab_aux-uni = 'PAL'.
              APPEND itab_aux.
** INI - Entrada de Meia Palete.
              SELECT SINGLE *
                  FROM mlgn
                      WHERE matnr = itab_sum-matnr
                        AND lgnum = lgnum
                        AND ( block = '03' OR block = '04' ).
              IF sy-subrc = 0.
                SELECT SINGLE *
                    FROM zwm001
                        WHERE armazem = lgnum AND
                              processo = 'MEIA-PALETE' AND
                              parametro = pack_material.
                IF sy-subrc = 0.
                  itab_aux-material = zwm001-valor.
                  itab_aux-quantidade = 4.
                  itab_aux-uni = 'PAL'.
                  APPEND itab_aux.
                ENDIF.
              ENDIF.
** FIM - Entrada de Meia Palete.

** INI - Entrada de Quarto Palete.
              SELECT SINGLE *
                 FROM mlgn
                     WHERE matnr = t_items-matnr
                       AND lgnum = lgnum
                       AND ( block = '07' OR block = '08' ).
              IF sy-subrc = 0.
                SELECT SINGLE *
                    FROM zwm001
                        WHERE armazem = lgnum            AND
                              processo = 'QUARTO-PALETE' AND
                              parametro = pack_material.
                IF sy-subrc = 0.
                  itab_aux-material = zwm001-valor.
                  itab_aux-quantidade = 8.
                  itab_aux-uni = 'PAL'.
                  APPEND itab_aux.
                ENDIF.
              ENDIF.
** FIM - Entrada de Quarto Palete.

            ENDIF.
          ENDIF.
        ENDLOOP.
        IF itab_aux[] IS INITIAL.
          itab_aux-material = pack_material.
          itab_aux-quantidade = 1.
          itab_aux-uni = 'PAL'.
          APPEND itab_aux.
        ENDIF.
      ENDIF.

      IF NOT itab_aux[] IS INITIAL.
        CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
          EXPORTING
            lgnum            = lgnum
            code             = code
            mov_mm           = l_mov_mm
            testrun          = ' '
            plant_o          = gv_werks " << INS ROFF(SDF):TMGP:22.01.2016 16:41:57
            plant_d          = lv_werks_d
            sloc_o           = gv_lgort_in
            sloc_d           = gv_lgort_out
          IMPORTING
            materialdocument = g_mblnr
            matdocumentyear  = g_gjahr
          TABLES
            return_msg       = return_msg
            items            = itab_aux
          EXCEPTIONS
            error            = 1
            OTHERS           = 2.

        IF sy-subrc <> 0.
          READ TABLE return_msg INDEX 1.
          IF sy-subrc = 0.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = return_msg-msgid
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = return_msg-msgnr
                message_var1   = return_msg-msgv1
                message_var2   = return_msg-msgv2
                message_var3   = return_msg-msgv3
                message_var4   = return_msg-msgv4
              IMPORTING
                ret_code       = retcode.

*                IF retcode = 'O'.
*                  FREE: t_items.
*                  CLEAR: t_items.

*                  CLEAR : ok_code_0002.
*                  SET SCREEN '0002'.
*                  LEAVE SCREEN.
*                ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
** RL <- INS 11.05.2005
    ENDIF.



    IF gv_lgort_d IS INITIAL.
      SELECT SINGLE umlgo FROM mseg INTO gv_lgort_d
                                    WHERE mblnr = mblnr AND
                                          mjahr = gjahr AND
                                          shkzg = 'H'.
    ENDIF.

  ENDIF.

*** RL -> INS 02.05.2005
*      WAIT UP TO 5 SECONDS.
*** RL <- INS 02.05.2005
*
*** Para os Dois Casos é necessário criar HU
*
*      CLEAR: return_msg, items_hu, itab_sscc.
*      REFRESH: return_msg, items_hu, itab_sscc.
*
*      READ TABLE t_items INDEX 1.
*      IF z_wm_cl_management=>is_remontada( is_data = t_items ) eq abap_true.
*
*        LOOP AT t_items.
*          CLEAR items_hu.
*          REFRESH items_hu.
*          items_hu-material = t_items-matnr.
*          items_hu-quantity = t_items-verme.
*          items_hu-unit = t_items-meins.
*          items_hu-batch = t_items-charg.
*          APPEND items_hu.
*          CLEAR items_hu.
*
*          CALL FUNCTION 'ZWM_CREATE_HU'
*            EXPORTING
*              warehouse                  = lgnum
*              plant                      = 'RENV'
*              s_loc                      = 'CD'
*              packing_material           = pack_material
*            IMPORTING
*              hukey                      = hukey
*            TABLES
*              return_msg                 = return_msg
*              items                      = items_hu
*            EXCEPTIONS
*              empty_table                = 1
*              reference_document_differs = 2
*              empty_delivery_item        = 3
*              item_not_found             = 4
*              OTHERS                     = 5.
*          IF sy-subrc <> 0.
*
*            READ TABLE return_msg INDEX 1.
*            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*              EXPORTING
*                message_id     = return_msg-msgid
*                message_lang   = sy-langu
*                message_type   = return_msg-msgtyp
*                message_number = return_msg-msgnr
*                message_var1   = return_msg-msgv1.
*
*            CLEAR : ok_code_0002.
*            SET SCREEN '0002'.
*            LEAVE SCREEN.
*
*          ELSE.
*            CLEAR itab_sscc.
*            itab_sscc-sscc = hukey.
*            APPEND itab_sscc.
*          ENDIF.
*
*        ENDLOOP.
*      ELSE.
*
*        LOOP AT t_items.
*          items_hu-material = t_items-matnr.
*          items_hu-quantity = t_items-verme.
*          items_hu-unit = t_items-meins.
*          items_hu-batch = t_items-charg.
*          APPEND items_hu.
*          CLEAR items_hu.
*        ENDLOOP.
*
*        CALL FUNCTION 'ZWM_CREATE_HU'
*          EXPORTING
*            warehouse                  = lgnum
*            plant                      = 'RENV'
*            s_loc                      = 'CD'
*            packing_material           = pack_material
*          IMPORTING
*            hukey                      = hukey
*          TABLES
*            return_msg                 = return_msg
*            items                      = items_hu
*          EXCEPTIONS
*            empty_table                = 1
*            reference_document_differs = 2
*            empty_delivery_item        = 3
*            item_not_found             = 4
*            OTHERS                     = 5.
*        IF sy-subrc <> 0.
*
*          READ TABLE return_msg INDEX 1.
*          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*            EXPORTING
*              message_id     = return_msg-msgid
*              message_lang   = sy-langu
*              message_type   = return_msg-msgtyp
*              message_number = return_msg-msgnr
*              message_var1   = return_msg-msgv1.
*
*          CLEAR : ok_code_0002.
*          SET SCREEN '0002'.
*          LEAVE SCREEN.
*
*        ELSE.
*          CLEAR itab_sscc.
*          itab_sscc-sscc = hukey.
*          APPEND itab_sscc.
*        ENDIF.
*      ENDIF.
** Criar a OT
***************************************

  CLEAR zwm001.
  IF lgtyp = 'DEV' OR gv_lgort_mode EQ 'X'.
    SELECT SINGLE *
        FROM zwm001
            WHERE armazem = lgnum AND
                  processo = 'ENTRADA_DEVOLUCAO' AND
                  parametro = 'MOV_WM'.

  ELSEIF lgtyp = 'INC'.
    SELECT SINGLE *
        FROM zwm001
            WHERE armazem = lgnum AND
                  processo = 'ENTRADA_INCIDENCIA' AND
                  parametro = 'MOV_WM'.
  ENDIF.
  MOVE zwm001-valor TO mov_wm.

  CLEAR: lagp, certificado.
  SELECT SINGLE lzone INTO certificado
      FROM lagp
          WHERE lgnum = lgnum AND
                lgtyp = lgtyp AND
                lgpla = lgpla.

  CLEAR: s_type, linhas.
  DESCRIBE TABLE t_items LINES linhas.

** Se for paletes completas actualiza o sscc
  READ TABLE t_items WITH KEY meins = 'PAL'.
  IF sy-subrc = 0.
    LOOP AT itab_sscc.
      t_items-sscc = itab_sscc-sscc.
      MODIFY t_items INDEX sy-tabix.
    ENDLOOP.

** Se for só uma pal remontada o s_type é o PRM
    IF z_wm_cl_management=>is_remontada( is_data = t_items ) EQ abap_true.
      IF linhas = 1.
        s_type = 'PRM'.
      ENDIF.
    ENDIF.
** Paletes incompletas.
  ELSE.
** Se nao for picking variavel
    SELECT SINGLE *
        FROM mlgn
            WHERE matnr = t_items-matnr
              AND lgnum = lgnum.
    IF mlgn-plkpt = 'PKB'.
      LOOP AT itab_sscc.
        t_items-sscc = itab_sscc-sscc.
        MODIFY t_items INDEX sy-tabix.
      ENDLOOP.
      CLEAR s_type.
    ELSE.
      s_type = 'PCK'.
    ENDIF.
  ENDIF.

  IF gv_lgort_mode EQ 'X'.
    CLEAR s_type.
  ENDIF.

  IF s_type IS INITIAL.
    SELECT SINGLE *
        FROM marc
            WHERE matnr = t_items-matnr AND
                  werks = gv_werks.
    IF marc-maabc IS INITIAL.

    ELSE.

      SELECT SINGLE *
          FROM mara
              WHERE matnr = t_items-matnr.

      SELECT SINGLE *
            FROM marm
                WHERE matnr = t_items-matnr AND
                      meinh = 'PAL'.

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

      CLEAR: parametro.
      IF qtd_out < marm-umrez.
        MOVE 'Z' TO parametro.
        SELECT SINGLE *
            FROM zwm001
                WHERE armazem = lgnum AND
                      processo = 'ENTRADA_DEVOLUCAO' AND
                      parametro = parametro.

        MOVE zwm001-valor TO s_type.
      ENDIF.
    ENDIF.
  ENDIF.


  CLEAR x_sscc.
  REFRESH x_sscc.

  LOOP AT t_items.

    SELECT SINGLE *
       FROM mara
           WHERE matnr = t_items-matnr.

    MOVE t_items-sscc  TO x_sscc-sscc.
    MOVE t_items-matnr TO x_sscc-material.
    MOVE t_items-verme TO x_sscc-quantidade.
    MOVE t_items-meins TO x_sscc-uni.
    MOVE t_items-letyp TO x_sscc-tipo_su.
    MOVE t_items-charg TO x_sscc-lote_producao.

    CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
      EXPORTING
        input                = t_items-verme
        kzmeinh              = 'X'
        matnr                = t_items-matnr
        meinh                = t_items-meins
        meins                = mara-meins
      IMPORTING
        output               = x_sscc-quantidade
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

    x_sscc-uni = mara-meins.

    APPEND x_sscc.
    CLEAR x_sscc.
  ENDLOOP.

  IF s_type = 'PCK'.
    CLEAR adicional.
    adicional = hukey.
  ENDIF.

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

  DATA t_zwm020 LIKE zwm020 OCCURS 0 WITH HEADER LINE.

  CLEAR t_zwm020.
  REFRESH t_zwm020.
  READ TABLE t_items INDEX 1.
  IF t_items-sscc <> ' '.
    IF z_wm_cl_management=>is_remontada( is_data = t_items ) EQ abap_true.
      t_zwm020-armazem = lgnum.
      t_zwm020-p1 = t_items-sscc.
      READ TABLE t_items INDEX 2.
      IF t_items-sscc <> ' '.
        t_zwm020-p2 = t_items-sscc.
        APPEND t_zwm020.
        INSERT zwm020 FROM TABLE t_zwm020.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
    EXPORTING
      warehouse      = lgnum
      mov_type       = mov_wm
      st_type_d      = s_type
*     plant          = 'RENV' " << DEL ROFF(SDF):TMGP:20.01.2016 11:22:11
      plant          = gv_werks " << INS ROFF(SDF):TMGP:20.01.2016 11:22:13
      s_loc          = gv_lgort_d
      sscc_adicional = adicional
      certificado    = certificado
    IMPORTING
      to             = e_to
    TABLES
      return_msg     = return_msg
      sscc           = x_sscc
    EXCEPTIONS
      error          = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.

    CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'ZWM014'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    READ TABLE return_msg INDEX 1.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = return_msg-msgid
        message_lang   = sy-langu
        message_type   = return_msg-msgtyp
        message_number = return_msg-msgnr
        message_var1   = return_msg-msgv1.

** Estornar o Documento de Material **
    IF NOT mblnr IS INITIAL.
** RL -> INS 02.05.2005
      CLEAR: documento.
** RL <- INS 02.05.2005
      CALL FUNCTION 'ZWM_ESTORNA_DOC_MATERIAL'
        EXPORTING
          mblnr     = mblnr
          mjahr     = gjahr
        IMPORTING
** RL -> MOD 02.05.2005
*         documento = o_mblnr
          documento = documento
** RL <- MOD 02.05.2005
        TABLES
          return    = return_msg.
** RL -> INS 02.05.2005
      o_mblnr = documento-mat_doc.
      o_gjahr = documento-doc_year.
** RL <- INS 02.05.2005
    ENDIF.

** RL -> INS 11.05.2005
** Estornar o Documento de Material para a Palete**
    IF NOT g_mblnr IS INITIAL.
      CLEAR: documento, return_msg.
      FREE: return_msg.

      CALL FUNCTION 'ZWM_ESTORNA_DOC_MATERIAL'
        EXPORTING
          mblnr     = g_mblnr
          mjahr     = g_gjahr
        IMPORTING
          documento = documento
        TABLES
          return    = return_msg.

      o_mblnr = documento-mat_doc.
      o_gjahr = documento-doc_year.
    ENDIF.
** RL <- INS 11.05.2005

    CLEAR t_items.
    REFRESH t_items.
    CASE gv_mode.
      WHEN gc_mode_hu.
        CLEAR : ok_code_0004.
        SET SCREEN '0004'.
      WHEN OTHERS.
        CLEAR : ok_code_0002.
        SET SCREEN '0002'.
    ENDCASE.
    LEAVE SCREEN.
  ELSE.

*        CLEAR t_items.
*        REFRESH t_items.
*        CLEAR : ok_code_0002.
*        SET SCREEN '0002'.
*        LEAVE SCREEN.

  ENDIF.


  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = 'ZWM014'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF l_printer IS INITIAL.
** Imprimir EAN128
    CLEAR zwm001.
    SELECT SINGLE *
        FROM zwm001
            WHERE armazem = lgnum AND
                  processo = 'EAN128'  AND
                  parametro = 'DEV_INC'.

    MOVE zwm001-valor TO l_printer.
  ENDIF.

  IF gv_mode <> gc_mode_hu.

    CALL FUNCTION 'ZWM_IMPRIME_EAN128'
      EXPORTING
        printer                  = l_printer
        copies                   = l_copies
      TABLES
        sscc                     = itab_sscc
      EXCEPTIONS
        impressora_nao_existe    = 1
        sscc_nao_existe          = 2
        sscc_com_impressao_grupo = 3
        OTHERS                   = 4.
    IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  CLEAR : ean11, material, quantidade, unidade,
          cursorfield, lote, ok_code_0002, sscc_origem.
  CLEAR t_items.
  REFRESH t_items.

  CASE gv_mode.
    WHEN gc_mode_hu.
      CLEAR : ok_code_0004.
      SET SCREEN '0004'.
    WHEN OTHERS.
      CLEAR : ok_code_0002.
      SET SCREEN '0002'.
  ENDCASE.
ENDFORM.                    " SAVE_0002
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
  CHECK lgnum IS INITIAL OR
        gv_mode IS INITIAL OR
        gv_werks IS INITIAL.


  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = l_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*      raise no_warehouse_found.
  ELSE.
    READ TABLE l_user WITH KEY statu = 'X'.
    IF sy-subrc <> 0.
      WRITE sy-uname TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG000'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '003'
          message_msg1   = text1
        IMPORTING
          ret_code       = resposta.
      IF resposta = 'O'.
        LEAVE TO SCREEN '0001'.
      ENDIF.

    ELSE.
      lgnum = l_user-lgnum.
    ENDIF.
  ENDIF.


  CASE sy-tcode.
    WHEN 'ZWM068B'.
      gv_mode = gc_mode_hu.
    WHEN OTHERS.
      gv_mode = gc_mode_normal.
  ENDCASE.

**  SELECT werks UP TO 1 ROWS
**    FROM t320 INTO gv_werks
**    WHERE lgort EQ 'CD'
**      AND lgnum EQ lgnum.
**  ENDSELECT.
**  IF sy-subrc NE 0.
**    gv_werks = 'RENV'.
**  ENDIF.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  CREATE_HU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ITEMS[]  text
*----------------------------------------------------------------------*
FORM create_hu.

  IF z_wm_cl_management=>is_remontada( is_data = t_items ) EQ abap_true.

    LOOP AT t_items.
      CLEAR items_hu.
      REFRESH items_hu.
      items_hu-material = t_items-matnr.
      items_hu-quantity = t_items-verme.
      items_hu-unit = t_items-meins.
      items_hu-batch = t_items-charg.
      APPEND items_hu.
      CLEAR items_hu.

      CALL FUNCTION 'ZWM_CREATE_HU'
        EXPORTING
          warehouse                  = lgnum
*         plant                      = 'RENV' " << DEL ROFF(SDF):TMGP:20.01.2016 11:21:24
          plant                      = gv_werks " << INS ROFF(SDF):TMGP:20.01.2016 11:21:25
          s_loc                      = gv_lgort_d
          packing_material           = pack_material
        IMPORTING
          hukey                      = hukey
        TABLES
          return_msg                 = return_msg
          items                      = items_hu
        EXCEPTIONS
          empty_table                = 1
          reference_document_differs = 2
          empty_delivery_item        = 3
          item_not_found             = 4
          OTHERS                     = 5.
      IF sy-subrc <> 0.

        READ TABLE return_msg INDEX 1.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = return_msg-msgid
            message_lang   = sy-langu
            message_type   = return_msg-msgtyp
            message_number = return_msg-msgnr
            message_var1   = return_msg-msgv1.

        FREE: t_items.
        CLEAR: t_items.

        CLEAR : ok_code_0002.
        SET SCREEN '0002'.
        LEAVE SCREEN.

      ELSE.
        CLEAR itab_sscc.
        itab_sscc-sscc = hukey.
        APPEND itab_sscc.
      ENDIF.

    ENDLOOP.
  ELSE.

    LOOP AT t_items.
      items_hu-material = t_items-matnr.
      items_hu-quantity = t_items-verme.
      items_hu-unit = t_items-meins.
      items_hu-batch = t_items-charg.
      APPEND items_hu.
      CLEAR items_hu.
    ENDLOOP.

    CALL FUNCTION 'ZWM_CREATE_HU'
      EXPORTING
        warehouse                  = lgnum
*       plant                      = 'RENV' " << DEL ROFF(SDF):TMGP:20.01.2016 11:21:38
        plant                      = gv_werks " << INS ROFF(SDF):TMGP:20.01.2016 11:21:39
        s_loc                      = 'CD'
        packing_material           = pack_material
      IMPORTING
        hukey                      = hukey
      TABLES
        return_msg                 = return_msg
        items                      = items_hu
      EXCEPTIONS
        empty_table                = 1
        reference_document_differs = 2
        empty_delivery_item        = 3
        item_not_found             = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.

      READ TABLE return_msg INDEX 1.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = return_msg-msgid
          message_lang   = sy-langu
          message_type   = return_msg-msgtyp
          message_number = return_msg-msgnr
          message_var1   = return_msg-msgv1.

      FREE: t_items.
      CLEAR: t_items.

      CLEAR : ok_code_0002.
      SET SCREEN '0002'.
      LEAVE SCREEN.

    ELSE.
      CLEAR itab_sscc.
      itab_sscc-sscc = hukey.
      APPEND itab_sscc.
    ENDIF.
  ENDIF.

  WAIT UP TO 3 SECONDS.



ENDFORM.                    " CREATE_HU
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0004 .
  CHECK NOT bin_origem IS INITIAL.

  CLEAR: cursorfield, ok_code_0001, ok_code_0002, ok_code_0004.

  CALL FUNCTION 'ZWM_SPLIT_BIN'
    EXPORTING
      bin   = gv_bind
    IMPORTING
      lgtyp = lgtyp
      lgpla = lgpla.
ENDFORM.                    " USER_COMMAND_0004
*&---------------------------------------------------------------------*
*&      Form  STATUS_0004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM status_0004.
  DATA: lv_lines   TYPE sytabix,
        lv_lines_c TYPE c,
        lv_count   TYPE sytabix.

  IF bin_origem IS INITIAL.
    cursorfield = 'BIN_ORIGEM'.
  ELSEIF sscc_origem IS  INITIAL.
    cursorfield = 'SSCC_ORIGEM'.
  ENDIF.

  SET CURSOR FIELD cursorfield.

  DESCRIBE TABLE t_items LINES lv_lines.
  lv_lines = lv_lines + 1.
  lv_lines_c = lv_lines.
  CONDENSE lv_lines_c NO-GAPS.

  CONCATENATE 'SSCC' lv_lines_c INTO sscc_label SEPARATED BY space.
  CONDENSE sscc_label.

  CHECK NOT t_items[] IS INITIAL.

  DESCRIBE TABLE t_items LINES lv_lines.

  CLEAR: t_items.
  READ TABLE t_items
       INDEX 1.

  IF z_wm_cl_management=>is_remontada( is_data = t_items ) EQ abap_true.
    lv_count = 2.
  ELSE.
    lv_count = 1.
  ENDIF.


  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'SSCC_ORIGEM'.
        IF lv_lines >= lv_count.
          screen-input = 0.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " STATUS_0004
*&---------------------------------------------------------------------*
*&      Form  CHECK_ORIGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_origem USING uv_clear TYPE flag.
  DATA lt_t001l TYPE TABLE OF t001l.
  DATA: ls_t001l TYPE t001l.
  CLEAR: ls_t001l.

  DATA: lt_messages TYPE tab_bdcmsgcoll.

  DATA: ls_zwm070 TYPE zwm070.

  DATA: lr_werks TYPE RANGE OF werks_d.
  DATA: lr_s_werks LIKE LINE OF lr_werks.

  CLEAR: gv_werks_t, gv_lgort_o, gv_lgort_d, gv_bind.

  CHECK NOT bin_origem IS INITIAL.

**  CHECK NOT gv_werks IS INITIAL.

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

  IF uv_clear EQ abap_true.
    CLEAR: t_items.
    REFRESH: t_items.
    CLEAR: gv_lgort_mode.
  ENDIF.

  CASE bin_origem(3).
    WHEN 'INC'.
      CLEAR zwm001.
      SELECT SINGLE *
          FROM zwm001
              WHERE armazem = lgnum AND
                    processo = 'PICK_INVERSO' AND
                    parametro = 'INCIDENCIA'.

      SELECT SINGLE * FROM zwm070
                      INTO ls_zwm070
                      WHERE lgnum = lgnum AND
                            bino  = bin_origem.

      IF sy-subrc EQ 0.
        gv_werks   = ls_zwm070-werks.
        gv_lgort_o = ls_zwm070-lgort_o.
        gv_lgort_d = ls_zwm070-lgort_d.
        gv_bind    = ls_zwm070-bind.
      ENDIF.


**    WHEN 'DEV'.
**      CLEAR zwm001.
**      SELECT SINGLE *
**          FROM zwm001
**              WHERE armazem = lgnum AND
**                    processo = 'PICK_INVERSO' AND
**                    parametro = 'DEVOLUCAO'.
    WHEN OTHERS.
      SELECT SINGLE * FROM zwm070
                      INTO ls_zwm070
                      WHERE lgnum = lgnum AND
                            bino  = bin_origem.

      IF sy-subrc EQ 0.
        gv_werks   = ls_zwm070-werks.
        gv_lgort_o = ls_zwm070-lgort_o.
        gv_lgort_d = ls_zwm070-lgort_d.
        gv_bind    = ls_zwm070-bind.
        gv_get_batch = ls_zwm070-getbatch.
      ELSE.
        SELECT SINGLE * FROM t001l
                        INTO ls_t001l
                        WHERE werks = gv_werks AND
                              lgort = bin_origem.

        IF sy-subrc EQ 0.
          gv_lgort_mode = 'X'.
        ENDIF.
      ENDIF.
  ENDCASE.

**  IF bin_origem <> zwm001-valor AND gv_lgort_mode EQ ''.
**    CLEAR text1.
**    WRITE bin_origem TO text1 LEFT-JUSTIFIED.
**    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
**      EXPORTING
**        message_id     = 'ZWMMSG001'
**        message_lang   = sy-langu
**        message_type   = 'E'
**        message_number = '197'
**        message_var1   = text1.
**
**    CLEAR: ok_code_0001, bin_origem,
**           cursorfield, text1.
**  ENDIF.
ENDFORM.                    " CHECK_ORIGEM
*&---------------------------------------------------------------------*
*&      Form  CHECK_LOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_lote.
  DATA: ls_mchb TYPE mchb.

  SELECT SINGLE * FROM mchb
                  INTO ls_mchb
                  WHERE matnr = material AND
                        charg = lote.

  IF sy-subrc <> 0.
    CLEAR: lote.
    EXIT.
  ENDIF.

ENDFORM.                    " CHECK_LOTE
