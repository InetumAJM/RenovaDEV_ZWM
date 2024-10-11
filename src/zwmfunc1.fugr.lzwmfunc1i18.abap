*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I18 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  exit29  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit29 INPUT.
  CLEAR: ok_code_0029.
  CLEAR: bin_origem_o, sscc1, sscc2, desc, qtd, uni,
         bin_dest1, bin_dest2, cursorfield.

*apaga entrada do dicionario de dados para o user
  DELETE FROM zwm011
  WHERE user_name = sy-uname AND
        status <> 'P'.

  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  f3_activo = 'X'.

  SET SCREEN '0001'. LEAVE SCREEN.
ENDMODULE.                 " exit29  INPUT
