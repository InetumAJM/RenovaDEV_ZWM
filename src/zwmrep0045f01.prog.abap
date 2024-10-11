*----------------------------------------------------------------------*
***INCLUDE ZWMREP0045F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_posicao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_posicao .

  DATA: l_mensula LIKE zwm014-mensula.

  CLEAR: l_pulmao, kober.

** Verifica de está na mensula
  CLEAR: zwm014.
  SELECT * FROM zwm014
  WHERE su EQ sscc.
    l_mensula = zwm014-mensula.
    EXIT.
  ENDSELECT.

  IF sy-subrc NE 0.
** Verifica onde realmente está
    CLEAR: lqua.
    SELECT * FROM lqua
    WHERE lgnum EQ lgnum
      AND lenum EQ sscc.
      EXIT.
    ENDSELECT.

    IF sy-subrc NE 0.
** Verifica se está no Pulmão
      CLEAR: zwm013.
      SELECT SINGLE * FROM zwm013
      WHERE armazem EQ lgnum
        AND sscc    EQ sscc.

      IF sy-subrc NE 0.
        IF posicao IS INITIAL.
          SELECT SINGLE *
              FROM zwm026
                  WHERE armazem = lgnum
                    AND sscc = sscc.
          IF sy-subrc = 0.
            CLEAR posicao.
            grupo = zwm026-grupo.
          ELSE.
            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
              EXPORTING
                message_id     = 'ZWMMSG001'
                message_lang   = sy-langu
                message_type   = 'E'
                message_number = '180'
                message_var1   = text1.

            CLEAR: ok_code_0001, sscc, cursorfield, posicao,
                   posicao_pulmao, grupo, ok_code_0002.
          ENDIF.
        ENDIF.
      ELSE.
        posicao = zwm013-destino.
*        CLEAR grupo.
        posicao_pulmao = zwm013-posicao_pulmao.
        l_pulmao = 'X'.

        SELECT SINGLE kober
                 INTO kober
                 FROM zwm028
                 WHERE lgnum = lgnum AND
                       refnr = grupo.

      ENDIF.
    ELSE.
      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = lqua-lgtyp
          lgpla = lqua-lgpla
        IMPORTING
          bin   = posicao.

      CLEAR grupo.
    ENDIF.
  ELSE.
    CALL FUNCTION 'ZWM_CONCATENATE_BIN'
      EXPORTING
        lgtyp = 'MEN'
        lgpla = l_mensula
      IMPORTING
        bin   = posicao.
    CLEAR grupo.
  ENDIF.

ENDFORM.                    " get_posicao
