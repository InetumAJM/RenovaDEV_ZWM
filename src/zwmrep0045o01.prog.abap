*&---------------------------------------------------------------------*
*&  Include           ZWMREP0045O01                                    *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

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

  IF l_pulmao NE 'X'.
    LOOP AT SCREEN.
      CHECK screen-name = 'POSICAO_PULMAO' OR
            screen-name = 'KOBER' OR
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  status_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0003 OUTPUT.

  IF l_pulmao NE 'X'.
    LOOP AT SCREEN.
      CHECK screen-name = 'POSICAO_PULMAO'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

*  DESCRIBE TABLE itab_matnr LINES l_index.

*  CASE l_index.
*    WHEN 1.
*      LOOP AT SCREEN.
*        CHECK screen-name NE 'LBL_1' OR
*              screen-name NE 'MAT1'  OR
*              screen-name NE 'DESC1'.
*        screen-invisible = '1'.
*        MODIFY SCREEN.
*      ENDLOOP.
*    WHEN 2.
*      LOOP AT SCREEN.
*        CHECK screen-name = 'LBL_3' OR
*              screen-name = 'MAT3'  OR
*              screen-name = 'DESC3' OR
*              screen-name = 'LBL_4' OR
*              screen-name = 'MAT4'  OR
*              screen-name = 'DESC4' OR
*              screen-name = 'LBL_5' OR
*              screen-name = 'MAT5'  OR
*              screen-name = 'DESC5' OR
*              screen-name = 'LBL_6' OR
*              screen-name = 'MAT6'  OR
*              screen-name = 'DESC6' OR
*              screen-name = 'LBL_7' OR
*              screen-name = 'MAT7'  OR
*              screen-name = 'DESC7'.
*        screen-invisible = '1'.
*        MODIFY SCREEN.
*      ENDLOOP.
*    WHEN 3.
*      LOOP AT SCREEN.
*        CHECK screen-name = 'LBL_4' OR
*              screen-name = 'MAT4'  OR
*              screen-name = 'DESC4' OR
*              screen-name = 'LBL_5' OR
*              screen-name = 'MAT5'  OR
*              screen-name = 'DESC5' OR
*              screen-name = 'LBL_6' OR
*              screen-name = 'MAT6'  OR
*              screen-name = 'DESC6' OR
*              screen-name = 'LBL_7' OR
*              screen-name = 'MAT7'  OR
*              screen-name = 'DESC7'.
*        screen-invisible = '1'.
*        MODIFY SCREEN.
*      ENDLOOP.
*    WHEN 4.
*      LOOP AT SCREEN.
*        CHECK screen-name = 'LBL_5' OR
*              screen-name = 'MAT5'  OR
*              screen-name = 'DESC5' OR
*              screen-name = 'LBL_6' OR
*              screen-name = 'MAT6'  OR
*              screen-name = 'DESC6' OR
*              screen-name = 'LBL_7' OR
*              screen-name = 'MAT7'  OR
*              screen-name = 'DESC7'.
*        screen-invisible = '1'.
*        MODIFY SCREEN.
*      ENDLOOP.
*    WHEN 5.
*      LOOP AT SCREEN.
*        CHECK screen-name = 'LBL_6' OR
*              screen-name = 'MAT6'  OR
*              screen-name = 'DESC6' OR
*              screen-name = 'LBL_7' OR
*              screen-name = 'MAT7'  OR
*              screen-name = 'DESC7'.
*        screen-invisible = '1'.
*        MODIFY SCREEN.
*      ENDLOOP.
*    WHEN 6.
*      LOOP AT SCREEN.
*        CHECK screen-name = 'LBL_7' OR
*              screen-name = 'MAT7'  OR
*              screen-name = 'DESC7'.
*        screen-invisible = '1'.
*        MODIFY SCREEN.
*      ENDLOOP.
*  ENDCASE.

  CLEAR: l_index.

  LOOP AT itab_dados.
    l_index = sy-tabix.

    CLEAR: makt, l_quant.
    SELECT SINGLE * FROM makt
    WHERE matnr EQ itab_dados-matnr
      AND spras EQ sy-langu.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = itab_dados-matnr
      IMPORTING
        output = itab_dados-matnr.

    l_quant = TRUNC( itab_dados-vemng ).
    CONDENSE l_quant.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input                = itab_dados-vemeh
*   LANGUAGE             = SY-LANGU
     IMPORTING
*   LONG_TEXT            =
       output               = itab_dados-vemeh
*   SHORT_TEXT           =
     EXCEPTIONS
       unit_not_found       = 1
       OTHERS               = 2
              .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CASE l_index.
      WHEN 1.
        mat1 = itab_dados-matnr.
        qtd1 = l_quant.
        umb1 = itab_dados-vemeh.
        desc1 = makt-maktx.
      WHEN 2.
        mat2 = itab_dados-matnr.
        qtd2 = l_quant.
        umb2 = itab_dados-vemeh.
        desc2 = makt-maktx.
      WHEN 3.
        mat3 = itab_dados-matnr.
        qtd3 = l_quant.
        umb3 = itab_dados-vemeh.
        desc3 = makt-maktx.
      WHEN 4.
        mat4 = itab_dados-matnr.
        qtd4 = l_quant.
        umb4 = itab_dados-vemeh.
        desc4 = makt-maktx.
      WHEN 5.
        mat5 = itab_dados-matnr.
        qtd5 = l_quant.
        umb5 = itab_dados-vemeh.
        desc5 = makt-maktx.
      WHEN 6.
        mat6 = itab_dados-matnr.
        qtd6 = l_quant.
        umb6 = itab_dados-vemeh.
        desc6 = makt-maktx.
      WHEN 7.
        mat7 = itab_dados-matnr.
        qtd7 = l_quant.
        umb7 = itab_dados-vemeh.
        desc7 = makt-maktx.
    ENDCASE.
  ENDLOOP.

  SET PF-STATUS 'ZRF'.
  SET CURSOR FIELD cursorfield.

ENDMODULE.                 " status_0003  OUTPUT
