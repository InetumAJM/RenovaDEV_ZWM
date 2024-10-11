*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I11 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT19  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module EXIT19 input.

CLEAR: ok_code_0019,
         pulmao1,
         pulmao2,
         cursorfield,
         to_ret,
         item_ret,
         tab_zwm011.


** apaga entrada do dicionario de dados para o user
  DELETE FROM zwm011
  WHERE user_name = sy-uname AND
        status <> 'P'.

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  f3_activo = 'X'.

  SET SCREEN '0001'.
  LEAVE SCREEN.

endmodule.                 " EXIT19  INPUT
