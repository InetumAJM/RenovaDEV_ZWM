*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0070_I01                                             *
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
      CLEAR: pdt_sscc, pdt_total, pdt_pag.
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
      CLEAR: pdt_sscc, pdt_total, pdt_pag.
      LEAVE TO SCREEN setscreen1.

    WHEN 'BACK'.

    WHEN 'NEXT'.

    WHEN 'SAVE'.

    WHEN 'UP'.
      IF NOT linhas   IS INITIAL AND
         NOT currpage IS INITIAL.

        IF totalpage >  currpage.
          CLEAR: auxindex.
          PERFORM clear_ecran.

          ADD 1 TO currpage.
          auxindex = currpage * 2 - 1.

          READ TABLE it_mat INDEX auxindex .
          pdt_matnr1  = it_mat-matnr.
          pdt_maktx1a = it_mat-maktxa.
          pdt_maktx1b = it_mat-maktxb.
          pdt_vemng1  = it_mat-vemng.
          pdt_altme1  = it_mat-altme.
          pdt_charg1  = it_mat-charg.

          IF linhas > auxindex.
            ADD 1 TO auxindex.
            READ TABLE it_mat INDEX auxindex .
            pdt_matnr2  = it_mat-matnr.
            pdt_maktx2a = it_mat-maktxa.
            pdt_maktx2b = it_mat-maktxb.
            pdt_vemng2  = it_mat-vemng.
            pdt_altme2  = it_mat-altme.
            pdt_charg2  = it_mat-charg.
          ENDIF.

          pdt_pag = currpage.

        ENDIF.
      ENDIF.

    WHEN 'DN'.
      IF NOT linhas   IS INITIAL AND
         NOT currpage IS INITIAL.

        IF currpage > 1.
          CLEAR: auxindex.
          PERFORM clear_ecran.

          currpage = currpage - 1.
          auxindex = currpage * 2 - 1.

          READ TABLE it_mat INDEX auxindex .
          pdt_matnr1  = it_mat-matnr.
          pdt_maktx1a = it_mat-maktxa.
          pdt_maktx1b = it_mat-maktxb.
          pdt_vemng1  = it_mat-vemng.
          pdt_altme1  = it_mat-altme.
          pdt_charg1  = it_mat-charg.

          IF linhas > auxindex.
            ADD 1 TO auxindex .
            READ TABLE it_mat INDEX auxindex.
            pdt_matnr2  = it_mat-matnr.
            pdt_maktx2a = it_mat-maktxa.
            pdt_maktx2b = it_mat-maktxb.
            pdt_vemng2  = it_mat-vemng.
            pdt_altme2  = it_mat-altme.
            pdt_charg2  = it_mat-charg.
          ENDIF.

          pdt_pag = currpage.

        ENDIF.
      ENDIF.

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

  CLEAR ok_code_0001.

ENDMODULE.                 " user_command_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_sscc  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc INPUT.

  CHECK NOT pdt_sscc IS INITIAL.

  PERFORM clear.
  CLEAR:  pdt_total, pdt_pag.

  SELECT venum erdat
          FROM vekp
          INTO CORRESPONDING FIELDS OF TABLE it_vekp
         WHERE exidv EQ pdt_sscc.

  IF sy-subrc <> 0.
** Erro! SSCC & invalido!
    WRITE pdt_sscc TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '113'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.
    IF resposta = 'O'.
      PERFORM clear.
      CLEAR: pdt_sscc, pdt_total, pdt_pag.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

** Obtem o mais recente
  SORT it_vekp BY erdat DESCENDING.

  READ TABLE it_vekp INDEX 1.

  SELECT matnr charg vemng vemeh altme
          FROM vepo
          INTO CORRESPONDING FIELDS OF TABLE it_vepo
         WHERE venum EQ it_vekp-venum.

  IF sy-subrc <> 0.
** Erro! SSCC & sem conteudo!
    WRITE pdt_sscc TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '241'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.
    IF resposta = 'O'.
      PERFORM clear.
      CLEAR: pdt_sscc, pdt_total, pdt_pag.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

** Descrição materiais
  SELECT matnr maktx
          FROM  makt
          INTO CORRESPONDING FIELDS OF TABLE it_makt
           FOR ALL ENTRIES IN it_vepo
         WHERE matnr = it_vepo-matnr
          AND   spras = sy-langu.

  SORT it_makt BY matnr.

  LOOP AT it_vepo.
    MOVE-CORRESPONDING it_vepo TO it_mat.

    IF it_vepo-vemeh <> it_vepo-altme.
** Converte para a unidade embalada
      qtd = it_mat-vemng.
      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = it_vepo-matnr
          i_in_me              = it_vepo-vemeh
          i_out_me             = it_mat-altme
          i_menge              = qtd
        IMPORTING
          e_menge              = qtd
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.
      it_mat-vemng = qtd.
    ENDIF.

** Descrição material
    CLEAR it_makt.
    READ TABLE it_makt WITH KEY matnr = it_vepo-matnr BINARY SEARCH.
    it_mat-maktxa = it_makt-maktx(20).
    it_mat-maktxb = it_makt-maktx+20(20).

    APPEND it_mat.
    CLEAR  it_mat.
  ENDLOOP.

** Processamento páginas
  DESCRIBE TABLE it_mat LINES linhas.
  totalpage = CEIL( linhas / 2 ).
  pdt_total = totalpage.
  currpage  = 1.
  pdt_pag   = currpage.

  READ TABLE it_mat INDEX 1.
  pdt_matnr1  = it_mat-matnr.
  pdt_maktx1a = it_mat-maktxa.
  pdt_maktx1b = it_mat-maktxb.
  pdt_vemng1  = it_mat-vemng.
  pdt_altme1  = it_mat-altme.
  pdt_charg1  = it_mat-charg.

  IF linhas > 1.
    READ TABLE it_mat INDEX 2.
    pdt_matnr2  = it_mat-matnr.
    pdt_maktx2a = it_mat-maktxa.
    pdt_maktx2b = it_mat-maktxb.
    pdt_vemng2  = it_mat-vemng.
    pdt_altme2  = it_mat-altme.
    pdt_charg2  = it_mat-charg.
  ENDIF.

ENDMODULE.                 " check_sscc  INPUT
