*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0066_I01                                             *
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
      MOVE 'PDT_ORIGEM' TO cursorfield.
      LEAVE TO SCREEN setscreen1.

    WHEN 'BACK'.

    WHEN 'NEXT'.

    WHEN 'SAVE'.

    WHEN 'UP'.
      IF NOT lincount IS INITIAL AND
         NOT currpage IS INITIAL.

        IF totalpage > currpage.
          PERFORM clear_ecran.

          currpage = currpage + 1.

          READ TABLE it_mat INDEX currpage.
          pdt_matnr    = it_mat-matnr.
          pdt_maktx_a  = it_mat-maktx(20).
          pdt_maktx_b  = it_mat-maktx+20(20).
          pdt_gesme_1  = it_mat-gesme.
          pdt_verme_1  = it_mat-verme.
          pdt_einme_1  = it_mat-einme.
          pdt_ausme_1  = it_mat-ausme.
          pdt_meins_1  = it_mat-meins.
          pdt_charg_1  = it_mat-charg.

          pdt_pag = currpage.

        ENDIF.
      ENDIF.

      CLEAR: ok_code_0001.

    WHEN 'DN'.
      IF NOT lincount IS INITIAL AND
         NOT currpage IS INITIAL.

        IF currpage > 1.
          PERFORM clear_ecran.

          currpage = currpage - 1.

          READ TABLE it_mat INDEX currpage.
          pdt_matnr    = it_mat-matnr.
          pdt_maktx_a  = it_mat-maktx(20).
          pdt_maktx_b  = it_mat-maktx+20(20).
          pdt_gesme_1  = it_mat-gesme.
          pdt_verme_1  = it_mat-verme.
          pdt_einme_1  = it_mat-einme.
          pdt_ausme_1  = it_mat-ausme.
          pdt_meins_1  = it_mat-meins.
          pdt_charg_1  = it_mat-charg.

          pdt_pag = currpage.

        ENDIF.
      ENDIF.

      CLEAR: ok_code_0001.

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

  DATA: lt_messages TYPE TAB_BDCMSGCOLL.

  CHECK NOT pdt_origem IS INITIAL.

  CLEAR: aux_lgtyp, aux_lgpla, lagp, gs_lqua, gs_marm.

  PERFORM clear_ecran.

** Descodifica Bin
  CALL FUNCTION 'ZWM_DECODE_BIN'
    EXPORTING
      iv_lgnum    = whs
      iv_bin_code = pdt_origem
    IMPORTING
      ev_bin      = pdt_origem
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
      CLEAR pdt_origem.
      MOVE 'PDT_ORIGEM' TO cursorfield.
      REFRESH lt_messages.
      RETURN.
    ENDIF.
  ENDIF.


  MOVE pdt_origem(3)    TO aux_lgtyp.
  MOVE pdt_origem+4(10) TO aux_lgpla.

** Verificar se a posição origem existe no armazém
  SELECT SINGLE * FROM lagp
          WHERE lgnum = whs
            AND lgtyp = aux_lgtyp
            AND lgpla = aux_lgpla.

  IF sy-subrc <> 0.
** Posição inválida
    WRITE pdt_origem TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '034'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.
    IF resposta = 'O'.
      PERFORM clear.
      MOVE 'PDT_ORIGEM' TO cursorfield.
      LEAVE TO SCREEN setscreen1.
    ENDIF.

  ELSE.

*  VERIFICAR KZLER
    IF lagp-kzler <> ' '.
      WRITE pdt_origem TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '224'
          message_var1   = text1
        IMPORTING
          ret_code       = resposta.
      IF resposta = 'O'.
        PERFORM clear.
        MOVE 'PDT_ORIGEM' TO cursorfield.
        LEAVE TO SCREEN setscreen1.
      ENDIF.
    ENDIF.
  ENDIF.

  IF pdt_origem(3) <> 'PKL'.
    PERFORM get_dados.
  ELSE.
    MOVE 'PDT_EAN11' TO cursorfield.
    LEAVE TO SCREEN setscreen1.
  ENDIF.

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
*&      Module  CHECK_EAN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ean INPUT.
  PERFORM check_ean.
ENDMODULE.                 " CHECK_EAN  INPUT
