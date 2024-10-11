*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I08 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_PED_COMPRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ped_compra INPUT.
  CLEAR : text1,
          text2.


  IF NOT ped_compra IS INITIAL.

** Item e pedido actual a processar
    MOVE ped_compra(10) TO pedido.
    MOVE ped_compra+10(5) TO item.

    SELECT SINGLE * FROM ekko
    WHERE ebeln = pedido.

    IF sy-subrc <> 0.

      WRITE pedido TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '007'
          message_var1   = text1.
      MOVE 'PED_COMPRA' TO cursorfield.
      CLEAR : ped_compra, pedido, item.

      IF lrf_wkqu-devty(5) = '16X20'.
        LEAVE TO SCREEN '0007'.
      ELSE.
        LEAVE TO SCREEN '0008'.
      ENDIF.

    ELSEIF ekko-autlf = 'X'.

      WRITE pedido TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '009'
          message_var1   = text1.
      MOVE 'PED_COMPRA' TO cursorfield.
      CLEAR : ped_compra, pedido, item.

      IF lrf_wkqu-devty(5) = '16X20'.
        LEAVE TO SCREEN '0007'.
      ELSE.
        LEAVE TO SCREEN '0008'.
      ENDIF.

    ENDIF.

** Verificar se o pedido/item foi um daqueles que deu entrada na gestão
** de portaria

    SELECT SINGLE * FROM zwm017
                    WHERE armazem = xuser-lgnum AND
                          ebeln = pedido AND
                          ebelp = item.
    IF sy-subrc <> 0.
      WRITE pedido TO text1 LEFT-JUSTIFIED.
      WRITE item TO text2 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '074'
          message_var1   = text1
          message_var2   = text2.
      MOVE 'PED_COMPRA' TO cursorfield.
      CLEAR : ped_compra, pedido, item.

      IF lrf_wkqu-devty(5) = '16X20'.
        LEAVE TO SCREEN '0007'.
      ELSE.
        LEAVE TO SCREEN '0008'.
      ENDIF.

** Verificar se o pedido entrado é válido para conferência
** no pulmão em questão
    ELSE.

      SELECT SINGLE porta FROM zwm005 INTO zwm005-porta
                          WHERE armazem = xuser-lgnum AND
                                num_entrada = zwm017-num_entrada.
      IF sy-subrc = 0.

        SELECT SINGLE * FROM zwm002
                        WHERE armazem = xuser-lgnum AND
                              porta = zwm005-porta AND
                              user_name = sy-uname.
        IF sy-subrc = 0.

** Verificar se o pulmão é igual ao que o operador
** está a confirmar ( no ecra )

          IF zwm002-pulmao_1 <> pulmao2+4(10).

            WRITE pedido TO text1 LEFT-JUSTIFIED.
            WRITE pulmao2 TO text2 LEFT-JUSTIFIED.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '086'
                message_var1   = text1
                message_var2   = text2.
            MOVE 'PED_COMPRA' TO cursorfield.
            CLEAR : ped_compra, pedido, item.

            IF lrf_wkqu-devty(5) = '16X20'.
              LEAVE TO SCREEN '0007'.
            ELSE.
              LEAVE TO SCREEN '0008'.
            ENDIF.
          ENDIF.

** Não acha entrada na tabela zwm002 ... logo é pedido
** que não pertence a esta descarga
        ELSE.

          WRITE pedido TO text1 LEFT-JUSTIFIED.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '086'
              message_var1   = text1.
          MOVE 'PED_COMPRA' TO cursorfield.
          CLEAR : ped_compra, pedido, item.

          IF lrf_wkqu-devty(5) = '16X20'.
            LEAVE TO SCREEN '0007'.
          ELSE.
            LEAVE TO SCREEN '0008'.
          ENDIF.


        ENDIF.

      ENDIF.

    ENDIF.


  ENDIF.

ENDMODULE.                 " CHECK_PED_COMPRA  INPUT
