*&---------------------------------------------------------------------*
*&  Include           ZWMREP0045I01                                    *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  FREE: itab_dados.
  CLEAR: sscc, cursorfield, ok_code_0001, posicao, posicao_pulmao,
         l_pulmao, mat1, mat2, mat3, mat4, mat5, mat6, mat7, desc1,
         desc2, desc3, desc4, desc5, desc6, desc7, l_index,
         ok_code_0002, itab_dados, qtd1, qtd2, qtd3, qtd4, qtd5, qtd6,
         qtd7, umb1, umb2, umb3, umb4, umb5, umb6, umb7, grupo.

  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_sscc  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc INPUT.

  CHECK NOT sscc IS INITIAL.

  FREE: itab_dados.
  CLEAR: posicao, posicao_pulmao, grupo,
  l_pulmao, mat1, mat2, mat3, mat4, mat5, mat6, mat7, desc1,
  desc2, desc3, desc4, desc5, desc6, desc7, l_index,
  ok_code_0002, itab_dados, qtd1, qtd2, qtd3, qtd4, qtd5, qtd6,
  qtd7, umb1, umb2, umb3, umb4, umb5, umb6, umb7, grupo.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = sscc
    IMPORTING
      output = sscc.

  CLEAR: vekp.
  SELECT SINGLE * FROM vekp
  WHERE lgnum EQ lgnum
    AND exidv EQ sscc.

  IF sy-subrc NE 0.
    CLEAR text1.
    WRITE sscc TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '113'
        message_var1   = text1.

    FREE: itab_dados.
    CLEAR: ok_code_0001, sscc, cursorfield, posicao, posicao_pulmao,
           l_pulmao, mat1, mat2, mat3, mat4, mat5, mat6, mat7, desc1,
           desc2, desc3, desc4, desc5, desc6, desc7, l_index,
           ok_code_0002, itab_dados, qtd1, qtd2, qtd3, qtd4, qtd5,
           qtd6, qtd7, umb1, umb2, umb3, umb4, umb5, umb6, umb7, grupo.
  ENDIF.

ENDMODULE.                 " check_sscc  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  FREE: itab_dados.
  CLEAR: itab_dados.

  CHECK NOT sscc IS INITIAL.

** Verifica se é de uma remessa
  CLEAR: vepo.
  SELECT * FROM vepo
  INTO CORRESPONDING FIELDS OF TABLE itab_dados
  WHERE venum EQ vekp-venum.

  IF sy-subrc EQ 0.
    IF posicao IS INITIAL.
      READ TABLE itab_dados INDEX 1.
      IF sy-subrc EQ 0.
        posicao = itab_dados-vbeln.

        SELECT SINGLE * FROM likp WHERE vbeln = itab_dados-vbeln.
        IF sy-subrc = 0.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = posicao
            IMPORTING
              output = posicao.

          SELECT SINGLE refnr INTO grupo
              FROM t311a
                  WHERE lgnum = lgnum
                    AND rbnum = itab_dados-vbeln.

        ELSE.
          DATA: lt_ltap LIKE ltap OCCURS 0 WITH HEADER LINE,
                lt_ltak LIKE ltak OCCURS 0 WITH HEADER LINE.

          CLEAR: lt_ltak, lt_ltap.
          REFRESH: lt_ltak, lt_ltap.
          SELECT * INTO TABLE lt_ltap
              FROM ltap
                  WHERE lgnum = lgnum
                    AND vlenr = sscc.

          IF sy-subrc EQ 0.

            DELETE lt_ltap WHERE vorga = 'ST'.
            IF NOT lt_ltap[] IS INITIAL.
              SELECT * INTO TABLE lt_ltak
                  FROM ltak
                      FOR ALL ENTRIES IN lt_ltap
                      WHERE lgnum = lt_ltap-lgnum
                        AND tanum = lt_ltap-tanum.

              SORT lt_ltak BY bdatu DESCENDING bzeit DESCENDING.

              READ TABLE lt_ltak INDEX 1.
              posicao = lt_ltak-vbeln.
              grupo   = lt_ltak-refnr.
            ENDIF.

          ELSE.
            CLEAR: posicao, grupo.
          ENDIF.

          IF grupo IS INITIAL.

            SELECT SINGLE grupo FROM zwm026
                                INTO grupo
                                WHERE armazem = lgnum AND
                                      sscc    =   sscc.

          ENDIF.

          SELECT SINGLE kober FROM zwm028
                              INTO kober
                              WHERE lgnum = lgnum AND
                                    refnr = grupo.


        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CASE ok_code_0001.
    WHEN 'BACK'.

      FREE: itab_dados.
      CLEAR: ok_code_0001, sscc, cursorfield, posicao, posicao_pulmao,
            l_pulmao, mat1, mat2, mat3, mat4, mat5, mat6, mat7, desc1,
             desc2, desc3, desc4, desc5, desc6, desc7, l_index,
             ok_code_0002, itab_dados, qtd1, qtd2, qtd3, qtd4, qtd5,
             qtd6, qtd7, umb1, umb2, umb3, umb4, umb5, umb6, umb7, grupo.

      SET SCREEN '0000'.
      LEAVE SCREEN.

    WHEN 'NEXT'.

      IF itab_dados[] IS INITIAL.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '179'
            message_var1   = text1.

        CLEAR: ok_code_0001, ok_code_0002.
      ELSE.
        IF l_user-devty(5) = '16X20'.
          SET SCREEN '0003'.
          LEAVE SCREEN.
        ELSE.
          SET SCREEN '0004'.
          LEAVE SCREEN.
        ENDIF.
      ENDIF.

    WHEN OTHERS.

      PERFORM get_posicao.

  ENDCASE.

  CLEAR: ok_code_0001.

ENDMODULE.                 " USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0002 INPUT.

  IF ok_code_0002 = 'BACK'.
    FREE: itab_dados.
    CLEAR: ok_code_0001, sscc, cursorfield, posicao, posicao_pulmao,
           l_pulmao, mat1, mat2, mat3, mat4, mat5, mat6, mat7, desc1,
           desc2, desc3, desc4, desc5, desc6, desc7, l_index,
           ok_code_0002, itab_dados, qtd1, qtd2, qtd3, qtd4, qtd5, qtd6,
           qtd7, umb1, umb2, umb3, umb4, umb5, umb6, umb7, grupo.

    IF l_user-devty(5) = '16X20'.
      SET SCREEN '0001'.
      LEAVE SCREEN.
    ELSE.
      SET SCREEN '0002'.
      LEAVE SCREEN.
    ENDIF.

  ENDIF.

ENDMODULE.                 " user_command_0002  INPUT
