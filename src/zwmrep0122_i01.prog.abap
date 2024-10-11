*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0122_I01                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  exit_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_0001 INPUT.

  CASE ok_code_0001.

    WHEN 'BACK'.
      PERFORM clear.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.                 " exit_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code_0001.

    WHEN 'CLR'.
      PERFORM clear.
      LEAVE TO SCREEN setscreen1.

    WHEN 'BACK'.

    WHEN 'NEXT'.

    WHEN 'SAVE'.

      CHECK sscc_out IS NOT INITIAL AND
            data_in  IS NOT INITIAL.

      PERFORM save_mm_wm.

    WHEN OTHERS.

      CHECK NOT ok_code_0001 IS INITIAL.
*   ERRO: Comando Inválido!
      WRITE 'Comando Inválido!' TO text1 LEFT-JUSTIFIED.
      WRITE ok_code_0001        TO text2 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '000'
          message_var1   = text1
          message_var2   = text2
        IMPORTING
          ret_code       = resposta.

      IF resposta = 'O'.
        LEAVE TO SCREEN setscreen1.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " user_command_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_origem  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_origem INPUT.

*  CHECK NOT pdt_origem IS INITIAL.
*
*  CLEAR: aux_lgtyp, aux_lgpla, lagp.
*
*  PERFORM clear_ecran.
*
*  MOVE pdt_origem(3)    TO aux_lgtyp.
*  MOVE pdt_origem+4(10) TO aux_lgpla.
*
*** Verificar se a posição origem existe no armazém
*  SELECT SINGLE * FROM lagp
*          WHERE lgnum = whs
*            AND lgtyp = aux_lgtyp
*            AND lgpla = aux_lgpla.
*
*  IF sy-subrc <> 0.
*** Posição inválida
*    WRITE pdt_origem TO text1 LEFT-JUSTIFIED.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '034'
*        message_var1   = text1
*      IMPORTING
*        ret_code       = resposta.
*    IF resposta = 'O'.
*      PERFORM clear.
*      MOVE 'PDT_ORIGEM' TO cursorfield.
*      LEAVE TO SCREEN setscreen1.
*    ENDIF.
*
*  ELSE.
*
**  VERIFICAR KZLER
*    IF lagp-kzler <> ' '.
*      WRITE pdt_origem TO text1 LEFT-JUSTIFIED.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWMMSG001'
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = '224'
*          message_var1   = text1
*        IMPORTING
*          ret_code       = resposta.
*      IF resposta = 'O'.
*        PERFORM clear.
*        MOVE 'PDT_ORIGEM' TO cursorfield.
*        LEAVE TO SCREEN setscreen1.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  CLEAR:   it_lqua, it_mat, it_makt, lincount, pdt_total, pdt_pag,
*           matnr.
*  REFRESH: it_lqua, it_mat, it_makt.
*
*  SELECT matnr charg meins gesme verme einme ausme
*         INTO CORRESPONDING FIELDS OF TABLE it_lqua
*         FROM  lqua
*         WHERE lgnum = whs
*         AND   lgtyp = aux_lgtyp
*         AND   lgpla = aux_lgpla.
*
*  IF NOT it_lqua[] IS INITIAL.
*    SELECT matnr maktx
*           FROM  makt
*           INTO CORRESPONDING FIELDS OF TABLE it_makt
*           FOR ALL ENTRIES IN it_lqua
*           WHERE matnr = it_lqua-matnr
*           AND   spras = sy-langu.
*
*    SORT it_makt BY matnr.
*  ENDIF.
*
*  LOOP AT it_lqua.
*    IF matnr IS INITIAL.
*      matnr = it_lqua-matnr.
*    ELSE.
*      IF it_lqua-matnr <> matnr.
*** Mais do que 1 material
*        WRITE pdt_origem TO text1 LEFT-JUSTIFIED.
*        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*          EXPORTING
*            message_id     = 'ZWMMSG001'
*            message_lang   = sy-langu
*            message_type   = 'E'
*            message_number = '225'
*            message_var1   = text1
*          IMPORTING
*            ret_code       = resposta.
*        IF resposta = 'O'.
*          PERFORM clear.
*          MOVE 'PDT_ORIGEM' TO cursorfield.
*          LEAVE TO SCREEN setscreen1.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    MOVE-CORRESPONDING it_lqua TO it_mat.
*
*    CLEAR it_makt.
*    READ TABLE it_makt WITH KEY matnr = it_lqua-matnr BINARY SEARCH.
*    it_mat-maktx = it_makt-maktx.
*
*    COLLECT it_mat.
*    CLEAR   it_mat.
*  ENDLOOP.
*
*  SORT it_mat BY matnr charg.
*
*  DESCRIBE TABLE it_mat LINES lincount.
*  totalpage = lincount.
*  pdt_total = totalpage.
*  currpage  = 1.
*  pdt_pag   = currpage.
*  READ TABLE it_mat INDEX 1.
*
*  pdt_matnr    = it_mat-matnr.
*  pdt_maktx_a  = it_mat-maktx(20).
*  pdt_maktx_b  = it_mat-maktx+20(20).
*  pdt_gesme_1  = it_mat-gesme.
*  pdt_verme_1  = it_mat-verme.
*  pdt_einme_1  = it_mat-einme.
*  pdt_ausme_1  = it_mat-ausme.
*  pdt_meins_1  = it_mat-meins.
*  pdt_charg_1  = it_mat-charg.

ENDMODULE.                 " check_origem  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_SSCC_IN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc_in INPUT.

*  CHECK NOT sscc_in IS INITIAL.
*
*  SELECT SINGLE * FROM vekp WHERE exidv = sscc_in.
*
*  IF sy-subrc <> 0.
*    WRITE sscc_in TO text1 LEFT-JUSTIFIED.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '033'
*        message_var1   = text1
*      IMPORTING
*        ret_code       = resposta.
*    IF resposta = 'O'.
*      PERFORM clear.
*      LEAVE TO SCREEN setscreen1.
*    ENDIF.
*  ENDIF.
*
*  SELECT SINGLE *
*      FROM vepo
*          WHERE venum = vekp-venum.
*
*  IF sy-subrc <> 0.
*    WRITE sscc_in TO text1 LEFT-JUSTIFIED.
*    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*      EXPORTING
*        message_id     = 'ZWMMSG001'
*        message_lang   = sy-langu
*        message_type   = 'E'
*        message_number = '033'
*        message_var1   = text1
*      IMPORTING
*        ret_code       = resposta.
*    IF resposta = 'O'.
*      PERFORM clear.
*      LEAVE TO SCREEN setscreen1.
*    ENDIF.
*  ENDIF.
*
*  matnr_out = vepo-matnr.
*  menge_out = vepo-vemng.
*  meins_out = vepo-vemeh.
*  charg_out = vepo-charg.
*
*  SELECT SINGLE * FROM makt
*      WHERE matnr = matnr_out
*        AND spras = sy-langu.
*
*  maktx_out1 = makt-maktx(20).
*  maktx_out2 = makt-maktx+20(20).

ENDMODULE.                 " CHECK_SSCC_IN  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_EAN_IN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ean_in INPUT.

  DATA: linhas TYPE i.

  CLEAR:  t_marm, linhas, matnr_in.
  REFRESH t_marm.

  CHECK NOT ean_in IS INITIAL.

  SELECT * INTO TABLE t_marm
      FROM marm
          WHERE ean11 = ean_in AND
                umrez = '1' AND
                umren = '1'.

  IF t_marm[] IS INITIAL.

    WRITE ean_in TO text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '135'
        message_var1   = text1.

    CLEAR ean_in.
    MOVE 'EAN_IN' TO cursorfield.
  ELSE.

    DESCRIBE TABLE t_marm LINES linhas.
    IF linhas = 1.
      READ TABLE t_marm INDEX 1.
      matnr_in = t_marm-matnr.
      SELECT SINGLE meins INTO meins_out
          FROM mara WHERE matnr = matnr_in.

      SELECT SINGLE * FROM makt
          WHERE matnr = matnr_in
              AND spras = sy-langu.

      maktx_out1 = makt-maktx(20).
      maktx_out2 = makt-maktx+20(20).
      MOVE 'MENGE_IN' TO cursorfield.
    ELSE.
      SORT t_marm BY matnr.
      MOVE 'MATNR_IN' TO cursorfield.
    ENDIF.
  ENDIF.
ENDMODULE.                 " CHECK_EAN_IN  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_MATNR_IN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr_in INPUT.

  CHECK NOT matnr_in IS INITIAL AND
        NOT ean_in    IS INITIAL.

  READ TABLE t_marm WITH KEY matnr = matnr_in.
  IF sy-subrc <> 0.
    WRITE: matnr_in TO text1 LEFT-JUSTIFIED,
           ean_in    TO text2 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '250'
        message_var1   = text1
        message_var2   = text2.

    CLEAR : ean_in, matnr_in.
    MOVE 'EAN_IN' TO cursorfield.
    EXIT.
  ENDIF.

** Verificar se o material existe e não está marcado para eliminação
  SELECT SINGLE * FROM mara
                  WHERE matnr = matnr_in AND
                        lvorm = ' '.
  IF sy-subrc <> 0.

    WRITE matnr_in TO text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '070'
        message_var1   = text1.

    CLEAR : matnr_in.
    MOVE 'MATNR_IN' TO cursorfield.
  ELSE.
    SELECT SINGLE * FROM makt
    WHERE matnr = matnr_in
        AND spras = sy-langu.

    maktx_out1 = makt-maktx(20).
    maktx_out2 = makt-maktx+20(20).

    MOVE mara-meins TO meins_out.
    MOVE 'MENGE_IN' TO cursorfield.
  ENDIF.

ENDMODULE.                 " CHECK_MATNR_IN  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_MENGE_IN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_menge_in INPUT.

  CHECK NOT matnr_in IS INITIAL AND
        NOT ean_in IS INITIAL AND
        NOT menge_in IS INITIAL.

  SELECT SINGLE *
    FROM mlgn
      WHERE matnr = matnr_in AND lgnum = whs.

  IF menge_in > mlgn-lhmg1.
    WRITE menge_in TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '143'
        message_var1   = text1.

    CLEAR : menge_in, text1.
    MOVE 'MENGE_IN' TO cursorfield.
    EXIT.
  ENDIF.

** Seleccionar o ultimo SSCC para retornar com o resto

  DATA: lt_ltak     LIKE ltak OCCURS 0 WITH HEADER LINE,
        lt_ltap_261 LIKE ltap OCCURS 0 WITH HEADER LINE,
        lt_ltap_262 LIKE ltap OCCURS 0 WITH HEADER LINE,
        lt_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.

  DATA: save_index LIKE sy-tabix,
        save_sscc  LIKE ltap-vlenr,
        save_charg LIKE ltap-charg.

  CLEAR:   lt_ltak, lt_ltap_261, lt_ltap_262, lt_ltap,
           save_index, save_sscc, save_charg.
  REFRESH: lt_ltak, lt_ltap_261, lt_ltap_262, lt_ltap.

  SELECT * INTO TABLE lt_ltak
      FROM ltak
          WHERE lgnum = whs
            AND lznum = aufnr_in.

  DELETE lt_ltak WHERE bwlvs <> '261' AND bwlvs <> '262'.

  IF NOT lt_ltak[] IS INITIAL.

    SELECT * INTO TABLE lt_ltap
         FROM ltap
             FOR ALL ENTRIES IN lt_ltak
                 WHERE lgnum = lt_ltak-lgnum
                   AND tanum = lt_ltak-tanum.

    DELETE lt_ltap WHERE matnr <> matnr_in.

    SORT lt_ltak BY bdatu DESCENDING bzeit DESCENDING.
    SORT lt_ltap.
    LOOP AT lt_ltak WHERE lgnum = whs AND bwlvs = '261'.

      CLEAR: save_sscc, save_charg.
      READ TABLE lt_ltap WITH KEY lgnum = lt_ltak-lgnum
                                  tanum = lt_ltak-tanum.
      IF sy-subrc = 0.
        save_sscc =  lt_ltap-vlenr.
        save_charg = lt_ltap-charg.
        CLEAR lt_ltap.

        LOOP AT lt_ltap WHERE lgnum = whs
                          AND nlenr = save_sscc.
        ENDLOOP.
        IF sy-subrc <> 0.
          EXIT.
        ELSE.
          DELETE lt_ltap WHERE lgnum = whs
                           AND tanum = lt_ltap-tanum
                           AND tapos = lt_ltap-tapos.
          CLEAR: save_sscc, save_charg.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  sscc_out  = save_sscc.
  charg_out = save_charg.

  IF sscc_out IS INITIAL.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '271'.
    CLEAR menge_in.
    EXIT.
  ENDIF.

  data_in = sy-datum.

ENDMODULE.                 " CHECK_MENGE_IN  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_AUFNR_IN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_aufnr_in INPUT.

  CHECK NOT aufnr_in IS INITIAL.

  SELECT SINGLE *
      FROM afko
      WHERE aufnr = aufnr_in.

  IF sy-subrc <> 0.
    WRITE aufnr_in TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '268'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.
    IF resposta = 'O'.
      CLEAR: aufnr_in, text1.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

  MOVE 'EAN_IN' TO cursorfield.

ENDMODULE.                 " CHECK_AUFNR_IN  INPUT
