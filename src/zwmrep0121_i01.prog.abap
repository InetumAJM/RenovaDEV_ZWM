*----------------------------------------------------------------------*
*   INCLUDE ZWMREP0121_I01                                            *
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

    WHEN 'RESTOS'.

      CHECK gt_resb[] IS NOT INITIAL.

      CLEAR sscc_in.
      PERFORM clear_ecran.
      LEAVE TO SCREEN setscreen2.

    WHEN 'SAVE'.

      CHECK aufnr_in IS NOT INITIAL AND
            sscc_in  IS NOT INITIAL AND
            data_in  IS NOT INITIAL.

      PERFORM save_mm_wm.

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.                 " user_command_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_AUFNR_IN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_aufnr_in INPUT.

  CHECK aufnr_in IS NOT INITIAL.

*  CLEAR gv_matnr_ordem.

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
      PERFORM clear.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

  CLEAR gt_resb.
  REFRESH gt_resb.

  SELECT * INTO TABLE gt_resb
      FROM resb
          WHERE rsnum = afko-rsnum.

  IF sy-subrc <> 0.

    WRITE aufnr_in TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '269'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.
    IF resposta = 'O'.
      PERFORM clear.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

  MOVE 'SSCC_IN' TO cursorfield.

ENDMODULE.                 " CHECK_AUFNR_IN  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_SSCC_IN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc_in INPUT.

  CHECK NOT sscc_in IS INITIAL.

** Validar SSCC
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
*
*    IF resposta = 'O'.
*      CLEAR: sscc_in, cursorfield, ok_code_0001.
*      PERFORM clear_ecran.
*      MOVE 'SSCC_IN' TO cursorfield.
*      LEAVE TO SCREEN setscreen1.
*    ENDIF.
*  ENDIF.

** Leitura dos dados relativos ao SSCC
  SELECT SINGLE *
      FROM lqua
          WHERE lgnum = whs AND
                lenum = sscc_in.


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
      CLEAR: sscc_in, cursorfield, ok_code_0001.
      PERFORM clear_ecran.
      MOVE 'SSCC_IN' TO cursorfield.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

** Verificar se material é igual ao material da ordem pordução
  SORT gt_resb.

  READ TABLE gt_resb WITH KEY matnr = lqua-matnr.
  IF sy-subrc <> 0.
*  IF lqua-matnr <> gv_matnr_ordem.
    WRITE lqua-matnr TO text1 LEFT-JUSTIFIED.
*    WRITE gv_matnr_ordem TO text2 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '270'
        message_var1   = text1
        message_var2   = text2
      IMPORTING
        ret_code       = resposta.

    IF resposta = 'O'.
      CLEAR: sscc_in, cursorfield, ok_code_0001.
      PERFORM clear_ecran.
      MOVE 'SSCC_IN' TO cursorfield.
      LEAVE TO SCREEN setscreen1.
    ENDIF.
  ENDIF.

** Cópia dos dados para as variáveis do ecrã
  matnr_out =  lqua-matnr.

  SELECT SINGLE * FROM makt
      WHERE matnr = matnr_out
        AND spras = sy-langu.

  maktx_out1 = makt-maktx(20).
  maktx_out2 = makt-maktx+20(20).

  charg_out =  lqua-charg.
  menge_out =  lqua-verme.
  meins_out =  lqua-meins.

  data_in = sy-datum.

ENDMODULE.                 " CHECK_SSCC_IN  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_0002 INPUT.

  CASE ok_code_0002.

    WHEN 'BACK'.
      CLEAR ean11_in.
      PERFORM clear_fields2.
      LEAVE TO SCREEN setscreen1.
  ENDCASE.

ENDMODULE.                 " USER_EXIT_0002  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0002 INPUT.

  CASE ok_code_0002.

    WHEN 'CLR'.
      CLEAR ean11_in.
      PERFORM clear_fields2.
      LEAVE TO SCREEN setscreen2.

    WHEN 'MARCAR'.
      CHECK sscc2_out IS NOT INITIAL AND matnr2_in IS NOT INITIAL.

      MOVE sscc2_out   TO sscc_in.
      MOVE matnr2_in   TO matnr_out.
      MOVE maktx2_out1 TO maktx_out1.
      MOVE maktx2_out2 TO maktx_out2.
      MOVE meins2_out  TO meins_out.
      MOVE charg2_out  TO charg_out.
      MOVE menge2_out  TO menge_out.

      CLEAR ean11_in.
      PERFORM clear_fields2.
      LEAVE TO SCREEN setscreen1.

    WHEN 'PGDN'.
      IF current_item > 1.
        current_item = current_item - 1.
      ENDIF.

    WHEN 'PGUP'.
      IF current_item < total_items.
        current_item = current_item + 1.
      ENDIF.

    WHEN OTHERS.

  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_EAN11_IN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ean11_in INPUT.

  CHECK ean11_in IS NOT INITIAL.

  CLEAR flg_matnr_input.

  CLEAR: gt_marm, gt_lqua.
  REFRESH: gt_marm, gt_lqua.

  DATA: lv_lines TYPE i.

  SELECT *
    FROM marm INTO TABLE gt_marm
    WHERE ean11 = ean11_in.

  IF sy-subrc <> 0.
    WRITE ean11_in TO text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '135'
        message_var1   = text1
      IMPORTING
        ret_code       = resposta.

    IF resposta = 'O'.
      CLEAR ean11_in.
      PERFORM clear_fields2.
      MOVE 'EAN11_IN' TO cursorfield.
      LEAVE TO SCREEN setscreen2.
    ENDIF.
  ENDIF.


  DESCRIBE TABLE gt_marm LINES lv_lines.

  IF lv_lines = 1.
    READ TABLE gt_marm INDEX 1.
    matnr2_in = gt_marm-matnr.


    READ TABLE gt_resb WITH KEY matnr = matnr2_in.
    IF sy-subrc <> 0.
*    IF matnr2_in <> gv_matnr_ordem.
      WRITE matnr2_in TO text1 LEFT-JUSTIFIED.
*      WRITE gv_matnr_ordem TO text2 LEFT-JUSTIFIED.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '270'
          message_var1   = text1
          message_var2   = text2
        IMPORTING
          ret_code       = resposta.

      IF resposta = 'O'.
        CLEAR ean11_in.
        PERFORM clear_fields2.
        MOVE 'EAN11_IN' TO cursorfield.
        LEAVE TO SCREEN setscreen2.
      ENDIF.
    ENDIF.

    PERFORM carrega_sscc_incompletos.


  ELSEIF lv_lines > 1.

    SORT gt_marm BY matnr.
    flg_matnr_input = 'X'.
    MOVE 'MATNR_IN' TO cursorfield.
  ENDIF.

ENDMODULE.                 " CHECK_EAN11_IN  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_MATNR2_IN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr2_in INPUT.

  CHECK matnr2_in IS NOT INITIAL AND ean11_in IS NOT INITIAL.

  CLEAR gt_lqua.
  REFRESH gt_lqua.

  READ TABLE gt_marm WITH KEY matnr = matnr2_in.

  IF sy-subrc <> 0.
    WRITE: matnr2_in TO text1 LEFT-JUSTIFIED,
           ean11_in   TO text2 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '250'
        message_var1   = text1
        message_var2   = text2.

    CLEAR : matnr2_in.
    MOVE 'MATNR2_IN' TO cursorfield.
    PERFORM clear_fields2.
    LEAVE TO SCREEN setscreen2.
  ENDIF.

** Verificar se o material existe e não está marcado para eliminação
  SELECT SINGLE * FROM mara
                  WHERE matnr = matnr2_in AND
                        lvorm = ' '.
  IF sy-subrc <> 0.

    WRITE matnr2_in TO text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '070'
        message_var1   = text1.

    CLEAR : matnr2_in.
    MOVE 'MATNR2_IN' TO cursorfield.
    PERFORM clear_fields2.
    LEAVE TO SCREEN setscreen2.
  ENDIF.

  READ TABLE gt_resb WITH KEY matnr = matnr2_in.
  IF sy-subrc <> 0.
*  IF matnr2_in <> gv_matnr_ordem.

    WRITE matnr2_in TO text1 LEFT-JUSTIFIED.
*    WRITE gv_matnr_ordem TO text2 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '270'
        message_var1   = text1
        message_var2   = text2
      IMPORTING
        ret_code       = resposta.

    IF resposta = 'O'.
      PERFORM clear_fields2.
      MOVE 'MATNR2_IN' TO cursorfield.
      LEAVE TO SCREEN setscreen2.
    ENDIF.
  ENDIF.

  PERFORM carrega_sscc_incompletos.



**  Carregamento dos dados.

ENDMODULE.                 " CHECK_MATNR2_IN  INPUT
