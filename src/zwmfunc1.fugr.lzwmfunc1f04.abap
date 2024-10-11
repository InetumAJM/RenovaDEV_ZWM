*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1F04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CONFIRMA_TO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONFIRMA_TO .

  CLEAR : TO_TERCEIROS.

  SELECT SINGLE * FROM ZWM011
                  WHERE ARMAZEM = XUSER-LGNUM AND
                        USER_NAME = SY-UNAME AND
                        STATUS = 'P'.
  IF SY-SUBRC = 0.
** Confirma a TO que estava a ser processada e coloca uma entrada na
** tabela ZWM002 a indicar q o user está a processar naquele pulmão

    CLEAR : RETURN_MSG.
    REFRESH : RETURN_MSG.
** A To de terceiros que necessitava de confirmação
    MOVE ZWM011-TO_NUMBER TO TO_TERCEIROS.


    DATA : PORTA_AUX LIKE ZWM002-PORTA.
    CLEAR PORTA_AUX.
** Descobrir qual foi o pulmao atribuido à porta
    MOVE ZWM011-ULTIMO_BIN+8(2) TO PORTA_AUX.
    UNPACK PORTA_AUX TO PORTA_AUX.
    SELECT SINGLE * FROM ZWM002
                    WHERE ARMAZEM = XUSER-LGNUM AND
                          PORTA = PORTA_AUX.
    IF SY-SUBRC = 0.
      ZWM002-USER_NAME = SY-UNAME.
      MODIFY ZWM002.
      COMMIT WORK.
    ENDIF.

  ENDIF.

ENDFORM.                    " CONFIRMA_TO
