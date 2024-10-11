************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0013                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Actualização de posições do depósito                     *
* Criado por: Bruno Simões                                             *
* Criado em.: 22/12/2003                                               *
* Tipo PRG..: Report                                                   *
************************************************************************

REPORT  ZWMREP0013.

tables : lagp.


************************************************************************
* Tabelas internas
************************************************************************
DATA: BEGIN OF l_lagp OCCURS 0.
        include structure lagp.
data : end of l_lagp.



************************************************************************
* Parâmetros de selecção
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS : S_ARM FOR lagp-lgnum
                 NO INTERVALS NO-EXTENSION DEFAULT '100'.
SELECTION-SCREEN END OF BLOCK B0.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS : S_st FOR lagp-LGTYP no-extension no intervals
                 obligatory,
                 s_pos for lagp-lgpla no-extension obligatory.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS : s_zona for lagp-lzone no-extension no intervals
                 obligatory.
SELECTION-SCREEN END OF BLOCK B1.




START-OF-SELECTION.

  perform actualiza_posicoes.

END-OF-SELECTION.




*&---------------------------------------------------------------------*
*&      Form  actualiza_posicoes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_posicoes .

  select * from lagp into table l_lagp
           where lgnum in s_arm and
                 lgtyp in s_st and
                 lgpla in s_pos.

  if sy-subrc = 0.

    loop at l_lagp.

      CALL FUNCTION 'LESI_BIN_COORD_UPDATE'
        EXPORTING
          IV_LGNUM = l_lagp-lgnum
          IV_LGTYP = l_lagp-lgtyp
          IV_LGPLA = l_lagp-lgpla
          IV_XCORD = ' '
          IV_YCORD = ' '
          IV_ZCORD = ' '
          IV_LZONE = s_zona-low.

      commit work.

    endloop.

  else.
   MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '000' WITH
   'Não existem dados selecionáveis'.
    exit.
  endif.

ENDFORM.                    " actualiza_posicoes
