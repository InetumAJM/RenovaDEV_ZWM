*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0120_I01                                             *
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

      CHECK sscc_in IS NOT INITIAL.

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

  CHECK NOT sscc_in IS INITIAL.

  IF sy-tcode = 'ZWM120' OR sy-tcode = 'ZWM121'.
    SELECT SINGLE *
        FROM lein
            WHERE lenum = sscc_in
              AND lgnum = whs.
    IF sy-subrc = 0.
      WRITE sscc_in TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '253'
          message_var1   = text1
        IMPORTING
          ret_code       = resposta.
      IF resposta = 'O'.
        PERFORM clear.
        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM vekp WHERE exidv = sscc_in.

    IF sy-subrc <> 0.
      WRITE sscc_in TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '033'
          message_var1   = text1
        IMPORTING
          ret_code       = resposta.
      IF resposta = 'O'.
        PERFORM clear.
        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ENDIF.

    SELECT SINGLE *
        FROM vepo
            WHERE venum = vekp-venum.

    IF sy-subrc <> 0.
      WRITE sscc_in TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '033'
          message_var1   = text1
        IMPORTING
          ret_code       = resposta.
      IF resposta = 'O'.
        PERFORM clear.
        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ENDIF.

    matnr_out = vepo-matnr.
    menge_out = vepo-vemng.
    meins_out = vepo-vemeh.
    charg_out = vepo-charg.

  ELSEIF sy-tcode = 'ZWM122' OR sy-tcode = 'ZWM123'.

    SELECT SINGLE matnr verme meins charg
        INTO (matnr_out, menge_out, meins_out, charg_out)
            FROM lqua
                WHERE lgnum = whs AND lenum = sscc_in.

  ENDIF.

  SELECT SINGLE * FROM makt
      WHERE matnr = matnr_out
        AND spras = sy-langu.

  maktx_out1 = makt-maktx(20).
  maktx_out2 = makt-maktx+20(20).

ENDMODULE.                 " CHECK_SSCC_IN  INPUT
