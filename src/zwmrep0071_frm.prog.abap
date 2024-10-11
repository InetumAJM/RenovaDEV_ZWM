*&---------------------------------------------------------------------*
*&  Include           ZWMREP0071_FRM                                   *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  find_whs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_whs .

  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = l_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*    raise no_warehouse_found.
  ELSE.
    READ TABLE l_user WITH KEY statu = con_x.
    IF sy-subrc <> 0.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '003'
        IMPORTING
          ret_code       = ret_code.
      IF ret_code = 'O'.
        LEAVE TO SCREEN '0001'.
      ENDIF.
    ELSE.
      whs = l_user-lgnum.
    ENDIF.
  ENDIF.

ENDFORM.                    " find_whs

*&---------------------------------------------------------------------*
*&      Form  regista_contagem_monitor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM regista_contagem_monitor .

  zwm038-lgnum = whs.
  zwm038-lgtyp = posicao(3).
  zwm038-lgpla = posicao+4(10).
  zwm038-qtd_contada = qtd.
  zwm038-uni = uni.

  GET TIME.
  zwm038-data_contagem = sy-datum.
  zwm038-qtd_existente = qtd_existente.

  MODIFY zwm038.
  COMMIT WORK.

  CLEAR: cursorfield, posicao, uni, qtd_existente, qtd,
         ok_code_0001.

  LEAVE TO SCREEN '0001'.

ENDFORM.                    " regista_contagem_monitor

*&---------------------------------------------------------------------*
*&      Form  find_uni
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_UNI  text
*----------------------------------------------------------------------*
FORM find_uni  CHANGING p_uni.

  CLEAR : text1, text2.

** posição inserida
  SELECT SINGLE lgtyp lgpla anzqu
                  FROM lagp INTO (lagp-lgtyp,lagp-lgpla,qtd_existente)
                  WHERE lgnum = whs AND
                        lgtyp = posicao(3) AND
                        lgpla = posicao+4(10).
  IF sy-subrc = 0.
** Unidade de pré-contagem "PAL"
    READ TABLE itab_zwm001 WITH KEY armazem   = whs
                                    processo  = 'INVENTARIO'
                                    parametro = 'UNIDADE_CONTAGEM'
                           BINARY SEARCH.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      unidade_pre_contagem = itab_zwm001-valor.
      WRITE unidade_pre_contagem TO uni LEFT-JUSTIFIED.
    ENDIF.

** Verificar se existe algum doc. de inventário activo
** para a posição em questão
    SELECT SINGLE k~ivnum INTO link-ivnum
                       FROM link AS k INNER JOIN linp AS p
                       ON  k~lgnum = p~lgnum AND
                           k~ivnum = p~ivnum
                       WHERE k~lgnum = whs AND
                             k~lgtyp = lagp-lgtyp AND
                             p~lgpla = lagp-lgpla AND
                             ( p~istat = 'A' OR
                               p~istat = 'Z' OR
                               p~istat = 'N' ).
    IF sy-subrc = 0.
** Já existe doc. inventário
      WRITE lagp-lgtyp TO text1 LEFT-JUSTIFIED.
      WRITE lagp-lgpla TO text2 LEFT-JUSTIFIED.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '185'
          message_var1   = text1
          message_var2   = text2
        IMPORTING
          ret_code       = ret_code.
      IF ret_code = 'O'.
        CLEAR : cursorfield,posicao,uni,qtd_existente,qtd, ok_code_0001.
        LEAVE TO SCREEN '0001'.
      ENDIF.
    ENDIF.
  ELSE.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '191'
        message_var1   = l_pos.

    CLEAR : cursorfield,posicao,uni,qtd_existente,qtd, ok_code_0001.

***** sscc inserido *******
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = posicao
*      IMPORTING
*        output = posicao.
*
*    SELECT SINGLE lgtyp lgpla
*                  FROM lqua INTO (lqua-lgtyp,lqua-lgpla)
*                  WHERE lgnum = whs AND
*                        lenum = posicao.
*    IF sy-subrc = 0.
*** Unidade de pré-contagem "PAL"
*      READ TABLE itab_zwm001 WITH KEY armazem   = whs
*                                      processo  = 'INVENTARIO'
*                                      parametro = 'UNIDADE_CONTAGEM'
*                             BINARY SEARCH.
*      IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ELSE.
*        WRITE unidade_pre_contagem TO uni LEFT-JUSTIFIED.
*      ENDIF.
*
*      SELECT SINGLE anzqu FROM lagp INTO qtd_existente
*                          WHERE lgnum = whs AND
*                                lgtyp = lqua-lgtyp AND
*                                lgpla = lqua-lgpla.
*** Verificar se existe algum doc. de inventário activo
*** para a posição em questão
*      SELECT SINGLE k~ivnum INTO link-ivnum
*                         FROM link AS k INNER JOIN linp AS p
*                         ON  k~lgnum = p~lgnum AND
*                             k~ivnum = p~ivnum
*                         WHERE k~lgnum = whs AND
*                               k~lgtyp = lqua-lgtyp AND
*                               p~lgpla = lqua-lgpla AND
*                               ( p~istat = 'A' OR
*                                 p~istat = 'Z' OR
*                                 p~istat = 'N' ).
*      IF sy-subrc = 0.
*** Já existe doc. inventário
*        WRITE lqua-lgtyp TO text1 LEFT-JUSTIFIED.
*        WRITE lqua-lgpla TO text2 LEFT-JUSTIFIED.
*        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*          EXPORTING
*            message_id     = 'ZWMMSG001'
*            message_lang   = sy-langu
*            message_type   = 'E'
*            message_number = '185'
*            message_var1   = text1
*            message_var2   = text2
*          IMPORTING
*            ret_code       = ret_code.
*        IF ret_code = 'O'.
*          CLEAR : cursorfield,posicao,uni,qtd_existente,qtd.
*          LEAVE TO SCREEN '0001'.
*        ENDIF.
*      ENDIF.
*    ENDIF.
  ENDIF.

  CLEAR: lqua, lagp.

ENDFORM.                    " find_uni
*&---------------------------------------------------------------------*
*&      Form  check_posicao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_posicao USING inventario.

  DATA : tipo_dep      LIKE lagp-lgtyp,
         posicao_dep   LIKE lagp-lgpla,
         sscc_aux      LIKE lein-lenum,
         sscc_erro(20).

*  break roffd.

  MOVE posicao(3) TO tipo_dep.
  MOVE posicao+4(10) TO posicao_dep.

** Contagem de inventario na posição
  SELECT SINGLE * FROM lagp
  WHERE lgnum = whs
    AND lgtyp = tipo_dep
    AND lgpla = posicao_dep.

  IF sy-subrc = 0.
** Verificar se a posição está bloqueada para inventário ... ou para
** entradas/ saídas
    IF NOT lagp-skzue IS INITIAL.
** bloqueado para entrada
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '188'
          message_var1   = l_pos.

      CLEAR: posicao, qtd, quantidade, uni, cursorfield, ok_code_0001.

    ELSEIF NOT lagp-skzua IS INITIAL.
** bloqueada para saida
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '189'
          message_var1   = l_pos.

      CLEAR : posicao, qtd, quantidade, uni, cursorfield, ok_code_0001.

    ELSEIF NOT lagp-skzsi IS INITIAL.
** bloqueada para inventário
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '190'
          message_var1   = l_pos.

      CLEAR : posicao, qtd, quantidade, uni, cursorfield, ok_code_0001.

    ENDIF.
  ELSE.

*    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '191'.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '191'
        message_var1   = l_pos.

    CLEAR : posicao, qtd, uni, cursorfield, ok_code_0001.

*** Não é uma posição ... verificar se é um SSCC
*    MOVE posicao TO sscc_aux.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = sscc_aux
*      IMPORTING
*        output = sscc_aux.
*
*    SELECT SINGLE * FROM lein
*    WHERE lenum = sscc_aux
*      AND lgnum = whs.
*
*    IF sy-subrc <> 0.
*      sscc_erro = sscc_aux.
*      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*        EXPORTING
*          message_id     = 'ZWMMSG001'
*          message_lang   = sy-langu
*          message_type   = 'E'
*          message_number = '113'
*          message_var1   = sscc_erro.
*
*      CLEAR: posicao, qtd, quantidade, uni, cursorfield, ok_code_0001.
*
*    ELSE.
*** Verificar se o SSCC existe na posição de origem
*      IF inventario IS INITIAL.
*        SELECT SINGLE * FROM lqua
*        WHERE lgnum = whs
*          AND lenum = sscc_aux
*          AND lgtyp = tipo_dep
*          AND lgpla = posicao_dep.
*        IF sy-subrc <> 0.
*          sscc_erro = sscc_aux.
*          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*            EXPORTING
*              message_id     = 'ZWMMSG001'
*              message_lang   = sy-langu
*              message_type   = 'E'
*              message_number = '119'
*              message_var1   = sscc_erro.
*
*          CLEAR: posicao, qtd, quantidade, uni, cursorfield,
*                 ok_code_0001.
*        ENDIF.
*      ELSE.
*** Validações à posição de inventário
*** Verificar se a posição inserida é mesmo uma posição
*        SELECT SINGLE * FROM lagp
*        WHERE lgnum = whs
*          AND lgtyp = tipo_dep
*          AND lgpla = posicao_dep.
*        IF sy-subrc = 0.
*** Verificar se a posição está bloqueada para inventário ... ou para
*** entradas/ saídas
*          IF NOT lagp-skzue IS INITIAL.
*** bloqueado para entrada
*
*            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*              EXPORTING
*                message_id     = 'ZWMMSG001'
*                message_lang   = sy-langu
*                message_type   = 'E'
*                message_number = '188'
*                message_var1   = posicao.
*
*            CLEAR: posicao, qtd, quantidade, uni, cursorfield,
*                   ok_code_0001.
*
*          ELSEIF NOT lagp-skzua IS INITIAL.
*** bloqueada para saida
*            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*              EXPORTING
*                message_id     = 'ZWMMSG001'
*                message_lang   = sy-langu
*                message_type   = 'E'
*                message_number = '189'
*                message_var1   = posicao.
*
*            CLEAR : posicao, qtd, quantidade, uni, cursorfield,
*                    ok_code_0001.
*
*          ELSEIF NOT lagp-skzsi IS INITIAL.
*** bloqueada para inventário
*            CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*              EXPORTING
*                message_id     = 'ZWMMSG001'
*                message_lang   = sy-langu
*                message_type   = 'E'
*                message_number = '190'
*                message_var1   = posicao.
*
*            CLEAR : posicao, qtd, quantidade, uni, cursorfield,
*                    ok_code_0001.
*
*          ENDIF.
*        ELSE.
*** Verificar se o sscc introduzido está numa posição bloqueada
*          CLEAR: lqua.
*          SELECT SINGLE * FROM lqua
*          WHERE lgnum = whs
*            AND lenum = sscc_aux.
*          IF sy-subrc = 0.
*            SELECT SINGLE skzue skzua skzsi
*                   FROM lagp INTO (lqua-skzue,lqua-skzua,lqua-skzsi)
*                   WHERE lgnum = lqua-lgnum
*                     AND lgtyp = lqua-lgtyp
*                     AND lgpla = lqua-lgpla.
*            IF sy-subrc = 0.
*** Verificar se a posição está bloqueada para inventário ... ou para
*** entradas/ saídas
*              IF NOT lagp-skzue IS INITIAL.
*** bloqueado para entrada
*                CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*                  EXPORTING
*                    message_id     = 'ZWMMSG001'
*                    message_lang   = sy-langu
*                    message_type   = 'E'
*                    message_number = '188'
*                    message_var1   = posicao.
*
*                CLEAR: posicao, qtd, quantidade, uni, cursorfield,
*                        ok_code_0001.
*
*              ELSEIF NOT lagp-skzua IS INITIAL.
*** bloqueada para saida
*                CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*                  EXPORTING
*                    message_id     = 'ZWMMSG001'
*                    message_lang   = sy-langu
*                    message_type   = 'E'
*                    message_number = '189'
*                    message_var1   = posicao.
*
*                CLEAR : posicao, qtd, quantidade, uni, cursorfield,
*                        ok_code_0001.
*
*              ELSEIF NOT lagp-skzsi IS INITIAL.
*** bloqueada para inventário
*                CALL FUNCTION 'YWM_MESSAGE_SCREEN'
*                  EXPORTING
*                    message_id     = 'ZWMMSG001'
*                    message_lang   = sy-langu
*                    message_type   = 'E'
*                    message_number = '190'
*                    message_var1   = posicao.
*
*                CLEAR: posicao, qtd, quantidade, uni, cursorfield,
*                       ok_code_0001.
*              ENDIF.
*            ENDIF.
*          ENDIF. "SSCC em posição Bloqueada
*        ENDIF. "Status das Posições
*      ENDIF. "Processo Inventário
*    ENDIF. "SSCC Não existe na LEIN
  ENDIF. "Verifica se é 1 um SSCC ou Posição

ENDFORM.                    " check_posicao
