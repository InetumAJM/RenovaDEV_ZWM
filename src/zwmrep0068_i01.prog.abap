*&---------------------------------------------------------------------*
*&  Include           ZWMREP0068_I01                                   *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  exit_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_0001 INPUT.

  CASE ok_code_0001.

    WHEN 'BACK'.
      PERFORM clear_fields.
      LEAVE TO SCREEN '0000'.

  ENDCASE.

ENDMODULE.                 " exit_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_pdt_sscc  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pdt_sscc INPUT.

  DATA: lenum    LIKE lein-lenum,
        verme    LIKE lqua-verme,
        stein    LIKE t331-stein,
        aofta    LIKE lquab-aofta,
        n_linhas TYPE i,
        flag_ot.

  DATA: ls_zwm013 TYPE zwm013.
  DATA: ls_vekp   TYPE vekp.
  DATA: ls_vepo   TYPE vepo.
  DATA: ls_makt   TYPE makt.
  DATA: ls_lagp   TYPE lagp.

  DATA: it_020    TYPE TABLE OF zwm020 WITH HEADER LINE.
  DATA: lt_ltak   TYPE TABLE OF ltak   WITH HEADER LINE.
  DATA: lt_ltap   TYPE TABLE OF ltap   WITH HEADER LINE.
  DATA: lt_zwm078 TYPE TABLE OF zwm078 WITH HEADER LINE.

  CHECK NOT pdt_sscc IS INITIAL.

  PERFORM clear.

  CALL FUNCTION 'CONVERSION_EXIT_LENUM_INPUT'
    EXPORTING
      input           = pdt_sscc
    IMPORTING
      output          = lenum
    EXCEPTIONS
      check_failed    = 1
      not_numeric     = 2
      t344_get_failed = 3
      wrong_length    = 4
      OTHERS          = 5.

  IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

** Centro e Deposito
***********************************************************************
  CLEAR: werks, lgort.

  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user      = sy-uname
      i_exidv     = lenum
      i_recall    = 'X'
      i_usewm     = 'X'
      i_userf     = 'X'
      i_usemm     = 'X'
      i_useaut    = 'X'
**   I_PARAMET           =
      i_get_lgnum = 'X'
      i_get_werks = 'X'
      i_get_lgort = 'X'
**   I_SEL_WERKS         =
**   I_SEL_LGORT         =
** IMPORTING
**   ET_MESSAGES         =
    CHANGING
      c_lgnum     = lgnum
      c_werks     = werks
      c_lgort     = lgort
    EXCEPTIONS
      error       = 1
      user_back   = 2
      OTHERS      = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


** Valida se é Palete de Copacking Rejeitada do WCS
**********************************************************************
  DO 1 TIMES.

    SELECT SINGLE * FROM lein
      WHERE lenum = lenum.

    CHECK sy-subrc <> 0.

    SELECT SINGLE *
      FROM zwm013 INTO ls_zwm013
      WHERE armazem = lgnum
      AND   sscc    = lenum.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      FROM lagp INTO ls_lagp
      WHERE lgnum = lgnum
      AND   lgtyp = 'REJ'
      AND   lgpla = ls_zwm013-pos_rej.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      FROM vekp INTO ls_vekp
      WHERE exidv = lenum.

    CHECK sy-subrc = 0.
    SELECT SINGLE *
      FROM vepo INTO ls_vepo
      WHERE venum = ls_vekp-venum.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      FROM makt INTO ls_makt
      WHERE matnr = ls_vepo-matnr
      AND   spras = sy-langu.

    pdt_matnr  = ls_makt-matnr.
    pdt_charg  = ls_vepo-charg.
    pdt_gesme  = ls_vepo-vemng.
    pdt_meins  = ls_vepo-vemeh.
    pdt_maktx1 = ls_makt-maktx(20).
    pdt_maktx2 = ls_makt-maktx+20(20).
    lgtyp      = ls_lagp-lgtyp.
    lgpla      = ls_lagp-lgpla.

    CONCATENATE ls_lagp-lgtyp ls_lagp-lgpla INTO pdt_lgpla SEPARATED BY space.

    CLEAR it_sscc.
    it_sscc-sscc       = lenum.
    it_sscc-material   = pdt_matnr.
    it_sscc-quantidade = pdt_gesme.
    it_sscc-uni        = pdt_meins.
    APPEND it_sscc.

    MOVE 'PDT_DEST' TO cursorfield.

    RETURN.
  ENDDO.

** Validar se é Palete de expedição rejeitada de WCS
**********************************************************************
  DO 1 TIMES.
    REFRESH lt_ltap.

    SELECT *
     FROM ltap INTO TABLE lt_ltap
     WHERE lgnum = lgnum
     AND   vlenr = lenum.

    IF lt_ltap[] IS NOT INITIAL.
      SELECT *
        FROM zwm078 INTO TABLE lt_zwm078
        FOR ALL ENTRIES IN lt_ltap
        WHERE lgnum = lt_ltap-lgnum
        AND   tanum = lt_ltap-tanum.
    ENDIF.

    CLEAR lt_zwm078.
    READ TABLE lt_zwm078 INDEX 1.

    CHECK lt_zwm078-rlpla IS NOT INITIAL.

    SELECT SINGLE *
      FROM lagp INTO ls_lagp
      WHERE lgnum = lgnum
      AND   lgtyp = 'REJ'
      AND   lgpla = lt_zwm078-rlpla.

    CHECK sy-subrc = 0.

    READ TABLE lt_ltap WITH KEY tanum = lt_zwm078-tanum.
    CHECK sy-subrc = 0.

    SELECT SINGLE *
      FROM makt INTO ls_makt
      WHERE matnr = lt_ltap-matnr
      AND   spras = sy-langu.

    pdt_matnr  = ls_makt-matnr.
    pdt_charg  = lt_ltap-charg.
    pdt_gesme  = lt_ltap-vsolm.
    pdt_meins  = lt_ltap-meins.
    pdt_maktx1 = ls_makt-maktx(20).
    pdt_maktx2 = ls_makt-maktx+20(20).
    pdt_refnr  = lt_zwm078-refnr.
    lgtyp      = ls_lagp-lgtyp.
    lgpla      = ls_lagp-lgpla.

    CONCATENATE ls_lagp-lgtyp ls_lagp-lgpla INTO pdt_lgpla SEPARATED BY space.

    CLEAR it_sscc.
    it_sscc-sscc       = lenum.
    it_sscc-material   = pdt_matnr.
    it_sscc-quantidade = pdt_gesme.
    it_sscc-uni        = pdt_meins.
    APPEND it_sscc.

    MOVE 'PDT_DEST' TO cursorfield.

    RETURN.
  ENDDO.


** Valida Palete de Armazém
**********************************************************************
  SELECT SINGLE * FROM lein
          WHERE lenum = lenum.

  IF sy-subrc <> 0.
    " Erro! SSCC & invalido!
    WRITE pdt_sscc TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_type   = 'E'
        message_number = '113'
        message_lang   = sy-langu
        message_var1   = text1
      IMPORTING
        ret_code       = ret_code.
    IF ret_code = 'O'.
      PERFORM clear_fields.
      LEAVE TO SCREEN setscreen1.
    ENDIF.

  ELSE.
    " SSCC existe
    it_sscc-sscc = lenum.
    APPEND it_sscc.
  ENDIF.

** Verifica as paletes remontadas
  IF z_wm_cl_management=>is_remontada( is_data = lein ) EQ abap_true.

    SELECT * FROM zwm020
             INTO TABLE it_020
            WHERE armazem EQ lgnum
              AND ( p1 EQ lenum OR
                    p2 EQ lenum ).

    IF sy-subrc = 0.
      DESCRIBE TABLE it_020 LINES n_linhas.
      IF n_linhas <> 1.
** Erro! Erro na palete remontada!
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_type   = 'E'
            message_number = '236'
            message_lang   = sy-langu
          IMPORTING
            ret_code       = ret_code.
        IF ret_code = 'O'.
          PERFORM clear_fields.
          LEAVE TO SCREEN setscreen1.
        ENDIF.
      ENDIF.

      READ TABLE it_020 INDEX 1.
      IF it_020-p1 = lenum.
        it_sscc-sscc = it_020-p2.
      ELSE.
        it_sscc-sscc = it_020-p1.
      ENDIF.
      APPEND it_sscc.
    ENDIF.
  ENDIF.

  LOOP AT it_sscc.
    indice = sy-tabix.

    CLEAR lqua.
    SELECT SINGLE * FROM lqua
            WHERE lenum = it_sscc-sscc.

** Verificação se quanto bloqueado
    IF NOT  lqua-skzua IS INITIAL.
** Erro! Quanto & bloqueado!
      WRITE lqua-lqnum TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_type   = 'E'
          message_number = '226'
          message_lang   = sy-langu
          message_var1   = text1
        IMPORTING
          ret_code       = ret_code.
      IF ret_code = 'O'.
        PERFORM clear_fields.
        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ENDIF.

    IF indice EQ 1.

** Verificação de posição bloqueada
      CLEAR: anzqu, skzue.
      SELECT SINGLE anzqu skzua FROM lagp
               INTO (anzqu, skzua)
              WHERE lgnum EQ lgnum
                AND lgtyp EQ lqua-lgtyp
                AND lgpla EQ lqua-lgpla.

      IF sy-subrc NE 0.
** Erro! Posição & invalida!
        CONCATENATE lqua-lgtyp lqua-lgpla
               INTO text1 SEPARATED BY space.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_type   = 'E'
            message_number = '034'
            message_lang   = sy-langu
            message_var1   = text1
          IMPORTING
            ret_code       = ret_code.
        IF ret_code = 'O'.
          PERFORM clear_fields.
          LEAVE TO SCREEN setscreen1.
        ENDIF.

      ELSE.

        IF NOT skzua IS INITIAL.
** Erro! Posição & bloqueada!
          CONCATENATE lqua-lgtyp lqua-lgpla
                 INTO text1 SEPARATED BY space.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_type   = 'E'
              message_number = '227'
              message_lang   = sy-langu
              message_var1   = text1
            IMPORTING
              ret_code       = ret_code.
          IF ret_code = 'O'.
            PERFORM clear_fields.
            LEAVE TO SCREEN setscreen1.
          ENDIF.
        ENDIF.

      ENDIF.

      CLEAR pdt_sscc2.
      pdt_matnr = lqua-matnr.
      pdt_charg = lqua-charg.
      pdt_gesme = lqua-gesme.
      pdt_meins = lqua-meins.
      lgtyp     = lqua-lgtyp.
      lgpla     = lqua-lgpla.
      bestq     = lqua-bestq.

      CONCATENATE lqua-lgtyp lqua-lgpla
             INTO pdt_lgpla SEPARATED BY space.

*        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*          EXPORTING
*            input                = lqua-meins
*            language             = sy-langu
*          IMPORTING
**           LONG_TEXT            =
*            output               = pdt_meins
**           SHORT_TEXT           =
*          EXCEPTIONS
*            unit_not_found       = 1
*            OTHERS               = 2.

    ELSE.
      pdt_sscc2 = it_sscc-sscc.
    ENDIF.

    it_sscc-material   = lqua-matnr.
    it_sscc-quantidade = lqua-gesme.
    it_sscc-uni        = pdt_meins.
    it_sscc-tipo_su    = lein-letyp.

** Valida se existe alguma OT pendente
    CLEAR flag_ot.
    IF NOT ( lqua-einme EQ 0 AND lqua-ausme EQ 0 ).
      flag_ot = 'X'.
    ELSE.

      CLEAR stein.
      SELECT SINGLE stein FROM t331 INTO stein
              WHERE lgnum EQ lgnum
                AND lgtyp EQ lgtyp.
      IF sy-subrc NE 0.
        flag_ot = 'X'.
      ENDIF.

      IF stein EQ 'B'.
        CLEAR aofta.
        SELECT aofta UP TO 1 ROWS FROM lquab
                INTO aofta
               WHERE lgnum EQ lgnum
                 AND matnr EQ pdt_matnr
                 AND werks EQ werks
*                  AND charg EQ pdt_charg
                 AND lgtyp EQ lgtyp
                 AND lgpla EQ lgpla
                 AND lgort EQ lgort.
        ENDSELECT.
        IF sy-subrc NE 0 OR aofta NE 0.
          flag_ot = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT flag_ot IS INITIAL.
** Erro! Já existem Ot's Criadas!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_type   = 'E'
          message_number = '237'
          message_lang   = sy-langu
        IMPORTING
          ret_code       = ret_code.
      IF ret_code = 'O'.
        PERFORM clear_fields.
        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ENDIF.

    MODIFY it_sscc INDEX indice.

  ENDLOOP.

  IF lgtyp = 'DRI' OR lgtyp = 'BLK'.
    DATA lt_lquab LIKE lquab OCCURS 0 WITH HEADER LINE.

    DATA : BEGIN OF l_lagp OCCURS 0.
    DATA: lgpla LIKE lagp-lgpla.
    DATA: prpro LIKE rl01s-prpro.
    DATA: END OF l_lagp.

    CLEAR: lt_lquab, l_lagp.
    REFRESH: lt_lquab, l_lagp.
    SELECT * INTO TABLE lt_lquab
        FROM lquab
            WHERE lgnum = lgnum
              AND lgtyp = lgtyp
              AND matnr = pdt_matnr
              AND charg = pdt_charg
              AND aofta = 0.

    DELETE lt_lquab WHERE lgpla = lgpla.
    IF NOT lt_lquab[] IS INITIAL.
      LOOP AT lt_lquab.
        CLEAR  l_lagp.
        CALL FUNCTION 'L_LAGP_CALC_USED_CAPACITY'
          EXPORTING
            i_lgnum = lt_lquab-lgnum
            i_lgtyp = lt_lquab-lgtyp
            i_lgpla = lt_lquab-lgpla
          IMPORTING
            e_prpro = l_lagp-prpro.

        l_lagp-lgpla = lt_lquab-lgpla.
        APPEND l_lagp.
      ENDLOOP.

      DELETE l_lagp WHERE prpro = 100.
      SORT l_lagp BY prpro DESCENDING.
      IF NOT l_lagp[] IS INITIAL.
        READ TABLE l_lagp INDEX 1.
        CONCATENATE lgtyp l_lagp-lgpla INTO dest_dri SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF l_lagp[] IS INITIAL.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_type   = 'E'
          message_number = '267'
          message_lang   = sy-langu
        IMPORTING
          ret_code       = ret_code.
    ENDIF.
  ENDIF.

** Descrição Material
  CLEAR maktx.
  SELECT SINGLE maktx FROM makt INTO maktx
          WHERE matnr = pdt_matnr
            AND spras = sy-langu.

  pdt_maktx1 = maktx(20).
  pdt_maktx2 = maktx+20(20).

  MOVE 'PDT_DEST' TO cursorfield.

** Tipo depósito de Rejeição
  IF pdt_lgpla(3) = gc_lgtyp_rej.

    REFRESH: lt_ltak, lt_ltap.

    SELECT *
      FROM ltap INTO TABLE lt_ltap
      WHERE lgnum = lgnum
      AND   vlenr = lenum.

    DELETE lt_ltap WHERE vltyp <> 'EAU'.

    IF lt_ltap[] IS NOT INITIAL.
      SELECT *
        FROM ltak INTO TABLE lt_ltak
        FOR ALL ENTRIES IN lt_ltap
        WHERE lgnum = lt_ltap-lgnum
        AND   tanum = lt_ltap-tanum.

      DELETE lt_ltak WHERE refnr IS INITIAL.

      " Obter Grupo
      READ TABLE lt_ltak INDEX 1.
      IF sy-subrc = 0.
        pdt_refnr = lt_ltak-refnr.
      ENDIF.
    ENDIF.
  ENDIF.

  SET CURSOR FIELD cursorfield.

ENDMODULE.                 " check_pdt_sscc  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_pdt_lagp  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pdt_dest INPUT.

  DATA: d_lgpla LIKE lqua-lgpla,
        d_lgtyp LIKE lqua-lgtyp.

  DATA: lt_messages TYPE tab_bdcmsgcoll.

  CHECK NOT pdt_dest IS INITIAL.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = lgnum
      iv_bin_code = pdt_dest
    IMPORTING
      ev_bin      = pdt_dest
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.
  IF sy-subrc <> 0.
    READ TABLE lt_messages INTO DATA(ls_messages) INDEX 1.
    IF sy-subrc = 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_messages-msgid
          message_lang   = sy-langu
          message_type   = ls_messages-msgtyp
          message_number = ls_messages-msgnr
          message_var1   = ls_messages-msgv1
          message_var2   = ls_messages-msgv2
          message_var3   = ls_messages-msgv3
          message_var4   = ls_messages-msgv4.
      PERFORM clear_fields.
      REFRESH lt_messages.
      LEAVE TO SCREEN setscreen1.
      RETURN.
    ENDIF.
  ENDIF.

  IF pdt_lgpla EQ pdt_dest.
** Erro! Posição não pode ser a mesma!
    WRITE pdt_lgpla TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_type   = 'E'
        message_number = '229'
        message_lang   = sy-langu
        message_var1   = text1
      IMPORTING
        ret_code       = ret_code.
    IF ret_code = 'O'.
      PERFORM clear_fields.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

  IF NOT pdt_pck IS INITIAL AND pdt_dest NE pdt_pck.
**  Erro! Posição não é a de picking do material &!
    WRITE pdt_matnr TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_type   = 'E'
        message_number = '251'
        message_lang   = sy-langu
        message_var1   = text1
      IMPORTING
        ret_code       = ret_code.
    IF ret_code = 'O'.
*     PERFORM clear_fields.
      CLEAR: pdt_dest.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

** Posição Destino
  d_lgtyp = pdt_dest(3).
  d_lgpla = pdt_dest+4(10).

** Tipos de depósito permitidos
  CHECK d_lgtyp NE 'BLK'.

  " Rejeição
  IF pdt_lgpla(3) = gc_lgtyp_rej.

    IF d_lgtyp <> gc_lgtyp_wcs AND d_lgtyp <> 'PUA'.

      " Erro! Tipo depósito destino & não permitido!
      WRITE d_lgtyp TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_type   = 'E'
          message_number = '233'
          message_lang   = sy-langu
          message_var1   = text1
        IMPORTING
          ret_code       = ret_code.

      IF ret_code = 'O'.
        PERFORM clear_fields.
        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ENDIF.

    IF pdt_lgpla+4(10) = gc_lgpla_rej.

      IF d_lgpla <> gc_lgpla_wcs.

        " ERRO: Posição de destino & não permitida!
        WRITE d_lgpla TO text1 LEFT-JUSTIFIED.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_type   = 'E'
            message_number = '373'
            message_lang   = sy-langu
            message_var1   = text1
          IMPORTING
            ret_code       = ret_code.

        IF ret_code = 'O'.
          PERFORM clear_fields.
          LEAVE TO SCREEN setscreen1.
        ENDIF.
      ENDIF.
    ENDIF.

  ELSE.

    READ TABLE it_lgtyp WITH KEY valor = d_lgtyp.
    IF sy-subrc NE 0.

      " Erro! Tipo depósito destino & não permitido!
      WRITE d_lgtyp TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_type   = 'E'
          message_number = '233'
          message_lang   = sy-langu
          message_var1   = text1
        IMPORTING
          ret_code       = ret_code.
      IF ret_code = 'O'.
        PERFORM clear_fields.
        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ENDIF.
  ENDIF.

** Tipo de depósito PCK
  IF d_lgtyp EQ lgtyp_pck.

    CLEAR mlgt.
    SELECT SINGLE lgpla FROM mlgt
             INTO mlgt-lgpla
            WHERE matnr EQ pdt_matnr
              AND lgnum EQ lgnum
              AND lgtyp EQ lgtyp_pck.

    IF mlgt-lgpla IS INITIAL.
** Erro! Material & não tem posição de picking atribuida!
      WRITE pdt_matnr TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_type   = 'E'
          message_number = '234'
          message_lang   = sy-langu
          message_var1   = text1
        IMPORTING
          ret_code       = ret_code.
      IF ret_code = 'O'.
        PERFORM clear_fields.
        LEAVE TO SCREEN setscreen1.
      ENDIF.

    ELSE.

      IF mlgt-lgpla <> d_lgpla.
** Erro! Posição & não é posição de picking do material & !
        WRITE d_lgpla   TO text1 LEFT-JUSTIFIED.
        WRITE pdt_matnr TO text2 LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_type   = 'E'
            message_number = '235'
            message_lang   = sy-langu
            message_var1   = text1
            message_var2   = text2
          IMPORTING
            ret_code       = ret_code.
        IF ret_code = 'O'.
          PERFORM clear_fields.
          LEAVE TO SCREEN setscreen1.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

** Valida existência localização destino
  CLEAR: anzqu, skzue.
  SELECT SINGLE anzqu skzue FROM lagp
           INTO (anzqu, skzue)
          WHERE lgnum EQ lgnum
            AND lgtyp EQ d_lgtyp
            AND lgpla EQ d_lgpla.

  IF sy-subrc NE 0.
** Erro! Posição & inexistente!
    WRITE pdt_dest TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_type   = 'E'
        message_number = '240'
        message_lang   = sy-langu
        message_var1   = text1
      IMPORTING
        ret_code       = ret_code.
    IF ret_code = 'O'.
      PERFORM clear_fields.
      LEAVE TO SCREEN setscreen1.
    ENDIF.

  ELSE.

    IF NOT skzue IS INITIAL.
** Erro! Posição & bloqueada!
      WRITE d_lgpla TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_type   = 'E'
          message_number = '227'
          message_lang   = sy-langu
          message_var1   = text1
        IMPORTING
          ret_code       = ret_code.
      IF ret_code = 'O'.
        PERFORM clear_fields.
        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ENDIF.

    CHECK d_lgtyp NE 'INC'.

    CHECK d_lgtyp NE 'PCK'.

    CHECK d_lgtyp NE 'EAU'.

    IF anzqu NE 0 AND d_lgtyp NE lgtyp_dri.
** erro! condições materiais incompativeis!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_type   = 'E'
          message_number = '230'
          message_lang   = sy-langu
        IMPORTING
          ret_code       = ret_code.
      IF ret_code = 'O'.
        PERFORM clear_fields.
        LEAVE TO SCREEN setscreen1.
      ENDIF.

    ELSEIF anzqu NE 0.

** Valida se tem as mesmas condições
      SELECT * UP TO 1 ROWS FROM lqua
           WHERE lgnum EQ lgnum
             AND matnr EQ pdt_matnr
             AND charg EQ pdt_charg
             AND bestq EQ bestq
             AND lgtyp EQ d_lgtyp
             AND lgpla EQ d_lgpla.
      ENDSELECT.

      IF sy-subrc NE 0.
** erro! condições materiais incompativeis!
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_type   = 'E'
            message_number = '230'
            message_lang   = sy-langu
          IMPORTING
            ret_code       = ret_code.
        IF ret_code = 'O'.
          PERFORM clear_fields.
          LEAVE TO SCREEN setscreen1.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.

ENDMODULE.                 " check_pdt_dest  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  DATA: pck_lgpla LIKE mlgt-lgpla.

  DATA: lv_tanum TYPE tanum.
  DATA: ls_msg   TYPE bdcmsgcoll.

*  DATA: lt_ltak       TYPE TABLE OF ltak      WITH HEADER LINE.
  DATA: lt_ltap_conf  TYPE TABLE OF ltap_conf WITH HEADER LINE.

  CASE ok_code_0001.

    WHEN 'PCK'.
**    Obtem posição de Picking para o material
      CLEAR: pdt_pck, dest_dri.
      SELECT SINGLE lgpla FROM mlgt INTO pck_lgpla
             WHERE matnr EQ pdt_matnr
               AND lgnum EQ lgnum
               AND lgtyp EQ lgtyp_pck.
      IF sy-subrc EQ 0 AND NOT pck_lgpla IS INITIAL.
        CONCATENATE lgtyp_pck pck_lgpla
               INTO pdt_pck SEPARATED BY space.
      ELSE.
**      Erro! Material & sem dados de picking!
        WRITE pdt_matnr TO text1 LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_type   = 'E'
            message_number = '252'
            message_lang   = sy-langu
            message_var1   = text1
          IMPORTING
            ret_code       = ret_code.
*        IF ret_code = 'O'.
*          PERFORM clear_fields.
*          LEAVE TO SCREEN setscreen1.
*        ENDIF.

      ENDIF.

    WHEN 'CLR'.
      PERFORM clear_fields.

    WHEN 'NEXT'.
      IF NOT pdt_sscc IS INITIAL.
        cursorfield = 'PDT_DEST'.
        SET CURSOR FIELD cursorfield.
      ENDIF.

    WHEN 'SAVE'.
      CHECK NOT pdt_lgpla EQ pdt_dest
        AND NOT lgnum     IS INITIAL
        AND NOT bwlvs     IS INITIAL
        AND NOT pdt_matnr IS INITIAL
        AND NOT pdt_gesme IS INITIAL
        AND NOT pdt_meins IS INITIAL
        AND NOT werks     IS INITIAL
        AND NOT lgort     IS INITIAL
        AND NOT lgtyp     IS INITIAL
        AND NOT lgpla     IS INITIAL
        AND NOT pdt_charg IS INITIAL
        AND NOT pdt_dest  IS INITIAL
        AND NOT pdt_sscc  IS INITIAL.

      REFRESH: return_msg.
      CLEAR:   return_msg.

      " WCS - Mesa de Entrada no Automático
      IF pdt_dest(3) = gc_lgtyp_wcs OR pdt_dest(3) = 'PUA'.

        CALL FUNCTION 'ZWM_CREATE_TO_REJ_WCS_IN'
          EXPORTING
            i_lgnum   = lgnum
            i_lenum   = pdt_sscc
            i_lgpla_o = lgpla
            i_lgpla_d = d_lgpla
          IMPORTING
            e_tanum   = lv_tanum
          TABLES
            t_return  = return_msg
          EXCEPTIONS
            error     = 1
            OTHERS    = 2.

      ELSE.
        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
          EXPORTING
            warehouse   = lgnum
            mov_type    = bwlvs
            st_type_o   = lgtyp
            bin_origem  = lgpla
            st_type_d   = d_lgtyp
            bin_destino = d_lgpla
*           STOCK_CAT   =
            plant       = werks
            s_loc       = lgort
*           CERTIFICADO =
            origem      = 'X'
*           REQ_NUMBER  =
*           REQ_TYPE    =
*           SSCC_ADICIONAL       =
          IMPORTING
            to          = lv_tanum
          TABLES
            return_msg  = return_msg
            sscc        = it_sscc
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.

*      CALL FUNCTION 'ZWM_CREATE_TO'
*        EXPORTING
*          warehouse        = lgnum
*          mov_type         = bwlvs
*          material         = pdt_matnr
*          quantity         = pdt_gesme
*          unit             = pdt_meins
*          plant            = werks
*          s_loc            = lgort
*          lote             = pdt_charg
*          source_sty       = lgtyp
*          source_bin       = lgpla
*          dest_sty         = d_lgtyp
*          dest_bin         = d_lgpla
**         req_type         =
**         req_number       =
*          su               = pdt_sscc
*        IMPORTING
*          to               = e_to
*          to_item          = e_to_item
*        TABLES
*          return_msg       = return_msg
*        EXCEPTIONS
*          error            = 1
*          OTHERS           = 2.
      ENDIF.

      IF sy-subrc <> 0.
        READ TABLE return_msg INDEX 1.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = return_msg-msgid
            message_lang   = sy-langu
            message_type   = return_msg-msgtyp
            message_number = return_msg-msgnr
            message_var1   = return_msg-msgv1
            message_var2   = return_msg-msgv2
            message_var3   = return_msg-msgv3
            message_var4   = return_msg-msgv4.

        CLEAR: ok_code_0001.
        PERFORM clear_fields.
        SET SCREEN setscreen1.
        LEAVE SCREEN.
      ENDIF.

      PERFORM clear_fields.
      cursorfield = 'PDT_SSCC'.
      SET CURSOR FIELD cursorfield.
  ENDCASE.

  CLEAR ok_code_0001.

ENDMODULE.                 " user_command_0001  INPUT
