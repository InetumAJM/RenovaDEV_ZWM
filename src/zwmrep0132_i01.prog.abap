*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0132_I01                                             *
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

      CHECK scr-cc_ordem IS NOT INITIAL
        AND scr-pernr    IS NOT INITIAL
* INETUM - NR - 15.12.2021 - RENPRJ00019 - Inicio
        AND scr-requisitante IS NOT INITIAL
* INETUM - NR - 15.12.2021 - RENPRJ00019 - Fim
        AND scr-matnr    IS NOT INITIAL
        AND scr-charg    IS NOT INITIAL
        AND scr-menge    IS NOT INITIAL.

      PERFORM save.

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
*  IF sy-tcode = 'ZWM120' OR sy-tcode = 'ZWM121'.
*    SELECT SINGLE *
*        FROM lein
*            WHERE lenum = sscc_in
*              AND lgnum = whs.
*    IF sy-subrc = 0.
*      WRITE sscc_in TO text1 LEFT-JUSTIFIED.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWMMSG001'
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = '253'
*          message_var1   = text1
*        IMPORTING
*          ret_code       = resposta.
*      IF resposta = 'O'.
*        PERFORM clear.
*        LEAVE TO SCREEN setscreen1.
*      ENDIF.
*    ENDIF.
*
*    SELECT SINGLE * FROM vekp WHERE exidv = sscc_in.
*
*    IF sy-subrc <> 0.
*      WRITE sscc_in TO text1 LEFT-JUSTIFIED.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWMMSG001'
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = '033'
*          message_var1   = text1
*        IMPORTING
*          ret_code       = resposta.
*      IF resposta = 'O'.
*        PERFORM clear.
*        LEAVE TO SCREEN setscreen1.
*      ENDIF.
*    ENDIF.
*
*    SELECT SINGLE *
*        FROM vepo
*            WHERE venum = vekp-venum.
*
*    IF sy-subrc <> 0.
*      WRITE sscc_in TO text1 LEFT-JUSTIFIED.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWMMSG001'
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = '033'
*          message_var1   = text1
*        IMPORTING
*          ret_code       = resposta.
*      IF resposta = 'O'.
*        PERFORM clear.
*        LEAVE TO SCREEN setscreen1.
*      ENDIF.
*    ENDIF.
*
*    matnr_out = vepo-matnr.
*    menge_out = vepo-vemng.
*    meins_out = vepo-vemeh.
*    charg_out = vepo-charg.
*
*  ELSEIF sy-tcode = 'ZWM122' OR sy-tcode = 'ZWM123'.
*
*    SELECT SINGLE matnr verme meins charg
*        INTO (matnr_out, menge_out, meins_out, charg_out)
*            FROM lqua
*                WHERE lgnum = whs AND lenum = sscc_in.
*
*  ENDIF.
*
*  SELECT SINGLE * FROM makt
*      WHERE matnr = matnr_out
*        AND spras = sy-langu.
*
*  maktx_out1 = makt-maktx(20).
*  maktx_out2 = makt-maktx+20(20).

ENDMODULE.                 " CHECK_SSCC_IN  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CC_ORDEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_cc_ordem INPUT.
* INETUM - JM - 05.11.2021 - RENPRJ00019 - Inicio
  DATA:
    lv_aufnr TYPE aufnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = scr-cc_ordem
    IMPORTING
      output = lv_aufnr.
* INETUM - JM - 05.11.2021 - RENPRJ00019 - Fim

  CLEAR: gv_ccusto, gv_ordem.
  SELECT SINGLE *
      FROM afko
*          WHERE aufnr = scr-cc_ordem. " INETUM - JM - 05.11.2021 - RENPRJ00019
          WHERE aufnr = lv_aufnr. " INETUM - JM - 05.11.2021 - RENPRJ00019
  IF sy-subrc <> 0.
    SELECT SINGLE *
        FROM csks
            WHERE kostl = scr-cc_ordem.
    IF sy-subrc <> 0.
      WRITE scr-cc_ordem TO text1 LEFT-JUSTIFIED.

*   Não existe nenhuma ordem/Centro de custo & !
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '302'
          message_var1   = text1.
*      CLEAR scr. " INETUM - JM - 05.11.2021 - RENPRJ00019
      CLEAR scr-cc_ordem. " INETUM - JM - 05.11.2021 - RENPRJ00019
      EXIT.
    ELSE.
      gv_ccusto = 'X'.
    ENDIF.
  ELSE.
    gv_ordem = 'X'.
  ENDIF.

ENDMODULE.                 " CHECK_CC_ORDEM  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PERNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pernr INPUT.

  DATA:
    ls_pa0000 TYPE pa0000.

* INETUM - NR - 27.07.2021 - RENPRJ00019 - Inicio

  CONSTANTS: lc_subty_0001 TYPE pa0105-subty VALUE '0001',
             lc_usrty_0001 TYPE pa0105-usrty VALUE '0001',
             lc_stat2_3    TYPE stat2 VALUE '3'. " INETUM - JM - 05.11.2021 - RENPRJ00019

***  SELECT SINGLE * FROM pa0001 WHERE pernr = scr-pernr.
  SELECT SINGLE *
    FROM pa0105
    WHERE pernr = scr-pernr
      AND subty = lc_subty_0001
      AND endda >= sy-datum
      AND begda <= sy-datum
      AND usrty = lc_usrty_0001.
*      AND usrid = sy-uname.
* INETUM - NR - 27.07.2021 - RENPRJ00019 - Fim
  IF sy-subrc <> 0.

* INETUM - JM - 05.11.2021 - RENPRJ00019 - Inicio
    SELECT SINGLE *
      FROM pa0000
      INTO ls_pa0000
      WHERE pernr = scr-pernr
*      AND subty = lc_subty_0001
      AND endda >= sy-datum
      AND begda <= sy-datum
      AND stat2 = lc_stat2_3.
    IF sy-subrc <> 0.
* INETUM - JM - 05.11.2021 - RENPRJ00019 - Fim

      WRITE scr-pernr TO text1 LEFT-JUSTIFIED.
* INETUM - NR - 29.07.2021 - RENPRJ00019 - Inicio
      WRITE sy-uname TO text2 LEFT-JUSTIFIED.
* INETUM - NR - 29.07.2021 - RENPRJ00019 - Fim
*   Numero de empregado & inválido !
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
* INETUM - NR - 29.07.2021 - RENPRJ00019 - Inicio
***        message_number = '303'
          message_number = '337'
* INETUM - NR - 29.07.2021 - RENPRJ00019 - Fim
          message_var1   = text1
* INETUM - NR - 29.07.2021 - RENPRJ00019 - Inicio
          message_var2   = text2.
* INETUM - NR - 29.07.2021 - RENPRJ00019 - Fim
      CLEAR scr-pernr.
      EXIT.
    ENDIF. " INETUM - JM - 05.11.2021 - RENPRJ00019

  ENDIF.

ENDMODULE.                 " CHECK_PERNR  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_MATNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr INPUT.

  SELECT SINGLE * FROM mara WHERE matnr = scr-matnr.
  IF sy-subrc <> 0.
    WRITE scr-matnr TO text1 LEFT-JUSTIFIED.
*   Código de Material & inválido !
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '304'
        message_var1   = text1.
    CLEAR scr-matnr.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM makt WHERE matnr = scr-matnr.

  scr-maktx1 = makt-maktx(20).
  scr-maktx2 = makt-maktx+20(20).
  scr-meins = mara-meins.

ENDMODULE.                 " CHECK_MATNR  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CHARG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_charg INPUT.

  DATA mnv_charg LIKE mchb-charg.

* INETUM - NR - 15.12.2021 - RENPRJ00019 - Inicio
  "Não é permitido a utilização de lotes não valorizados (MNV)
  IF scr-charg(3) EQ 'MNV'.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '343'.

    CLEAR scr-charg.
    EXIT.
  ENDIF.
* INETUM - NR - 15.12.2021 - RENPRJ00019 - Fim

  CLEAR gv_mnv.
  SELECT SINGLE * INTO ls_mchb
      FROM mchb
          WHERE matnr = scr-matnr
            AND charg = scr-charg
* INETUM - NR - 10.12.2021 - RENPRJ00019 - Inicio
***            AND clabs > 0.
            AND clabs >= scr-menge.
* INETUM - NR - 10.12.2021 - RENPRJ00019 - Fim
  IF sy-subrc <> 0.
    CLEAR mnv_charg.
    CONCATENATE 'MNV' scr-charg+3(7) INTO mnv_charg.
    SELECT SINGLE * INTO ls_mchb
         FROM mchb
             WHERE matnr = scr-matnr
               AND charg = mnv_charg.

    IF sy-subrc <> 0.
      WRITE scr-charg TO text1 LEFT-JUSTIFIED.
      WRITE scr-matnr TO text2 LEFT-JUSTIFIED.
*   Lote & não existe para o material &!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '305'
          message_var1   = text1
          message_var2   = text2.
      CLEAR scr-charg.
      EXIT.
    ELSE.
      gv_mnv = 'X'.
    ENDIF.
  ENDIF.

  CLEAR t320.
  SELECT SINGLE * FROM t320
      WHERE werks = ls_mchb-werks
        AND lgort = ls_mchb-lgort.

  IF t320-lgnum IS NOT INITIAL.
    WRITE ls_mchb-werks TO text1 LEFT-JUSTIFIED.
    WRITE ls_mchb-lgort TO text2 LEFT-JUSTIFIED.
*   Centro &, Deposito & geridos por WM. Mov Impossivel!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '307'
        message_var1   = text1
        message_var2   = text2.
    CLEAR scr-charg.
    EXIT.
  ENDIF.

ENDMODULE.                 " CHECK_CHARG  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_MENGE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_menge INPUT.

* INETUM - NR - 10.12.2021 - RENPRJ00019 - Inicio
  DATA: lv_clabs_mval  TYPE mchb-clabs,
        lv_stock_total TYPE mchb-clabs.

***  IF scr-menge > ls_mchb-clabs.
***    WRITE scr-menge TO text1 LEFT-JUSTIFIED.
****   Quantidade & inferior ao stock disponivel!
***    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
***      EXPORTING
***        message_id     = 'ZWMMSG001'
***        message_lang   = sy-langu
***        message_type   = 'E'
***        message_number = '306'
***        message_var1   = text1
***        message_var2   = text2.
***    CLEAR scr-menge.
***    EXIT.
***  ENDIF.


  IF gv_mnv IS NOT INITIAL.
    "Calcular stock total disponivel (Stock lote Valorizado + Não Vlorizado livre + Não Valorizado bolqueado)
    "Verificar se o stocl total é suficiente

    "Obter stock valorizado
    SELECT SINGLE clabs INTO lv_clabs_mval
     FROM mchb
     WHERE matnr = scr-matnr
       AND charg = scr-charg
       AND clabs > 0.

    lv_stock_total = lv_clabs_mval + ls_mchb-clabs + ls_mchb-cspem.

    IF scr-menge > lv_stock_total.
      WRITE scr-menge TO text1 LEFT-JUSTIFIED.
*   Quantidade & inferior ao stock disponivel!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '306'
          message_var1   = text1
          message_var2   = text2.
      CLEAR scr-menge.
      EXIT.
    ENDIF.

  ENDIF.
* INETUM - NR - 10.12.2021 - RENPRJ00019 - Fim

ENDMODULE.                 " CHECK_MENGE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INITIAL_PERNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_initial_pernr INPUT.

  IF scr-pernr IS INITIAL.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '335'.

    EXIT.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_INITIAL_REQUISITANTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_initial_requisitante INPUT.

  IF scr-requisitante IS INITIAL.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '336'.

    EXIT.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_REQUISITANTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_requisitante INPUT.

  CONSTANTS: lc_subty_req_0001 TYPE pa0105-subty VALUE '0001',
             lc_usrty_req_0001 TYPE pa0105-usrty VALUE '0001',
             lc_stat2_req_3    TYPE stat2 VALUE '3'. " INETUM - JM - 05.11.2021 - RENPRJ00019

  SELECT SINGLE *
    FROM pa0105
    WHERE pernr = scr-requisitante
      AND subty = lc_subty_req_0001
      AND endda >= sy-datum
      AND begda <= sy-datum
      AND usrty = lc_usrty_req_0001.
  IF sy-subrc <> 0.

* INETUM - JM - 05.11.2021 - RENPRJ00019 - Inicio
    SELECT SINGLE *
      FROM pa0000
      INTO ls_pa0000
      WHERE pernr = scr-requisitante
*      AND subty = lc_subty_0001
      AND endda >= sy-datum
      AND begda <= sy-datum
      AND stat2 = lc_stat2_req_3.
    IF sy-subrc <> 0.
* INETUM - JM - 05.11.2021 - RENPRJ00019 - Fim

      WRITE scr-requisitante TO text1 LEFT-JUSTIFIED.
*   Numero de requisitante & inválido !
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '334'
          message_var1   = text1.
      CLEAR scr-requisitante.
      EXIT.

    ENDIF. " INETUM - JM - 05.11.2021 - RENPRJ00019
  ENDIF.

ENDMODULE.
