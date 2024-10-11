*&---------------------------------------------------------------------*
*&  Include           ZWMREP0032F01                                    *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  sai_ecra
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sai_ecra .

  CLEAR: matnr, maktx, aufnr, charg, lhmg1, meins, matnr_pal,
         maktx_pal, erro, cursorfield, ok_code, centro, desc1,
         area, linha, ean11.

  SET SCREEN 0.
  LEAVE SCREEN.

ENDFORM.                    " sai_ecra

*&---------------------------------------------------------------------*
*&      Form  get_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dados .

  FREE: itab_arln, itab_tj02t.
  CLEAR: itab_arln, itab_tj02t.

  SELECT * FROM z02rparln
  INTO CORRESPONDING FIELDS OF TABLE itab_arln.

  SORT itab_arln.

  SELECT * FROM tj02t
  INTO CORRESPONDING FIELDS OF TABLE itab_tj02t
  WHERE spras EQ sy-langu
    AND txt04 IN r_txt04.

  LOOP AT itab_tj02t.
    r_stat-low = itab_tj02t-istat.
    r_stat-option = 'EQ'.
    r_stat-sign = 'I'.
    APPEND r_stat.
    CLEAR: r_stat.
  ENDLOOP.

ENDFORM.                    " get_dados

*&---------------------------------------------------------------------*
*&      Form  check_quant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_quant .

  CLEAR: lhmg1.

  CHECK NOT aufnr IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = matnr
    IMPORTING
      output = l_matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lgnum
    IMPORTING
      output = l_lgnum.

  CLEAR: lhmg1, mlgn, lhme1.
  SELECT SINGLE lhmg1 lhme1 FROM mlgn
  INTO (lhmg1, lhme1)
  WHERE matnr EQ l_matnr
    AND lgnum EQ l_lgnum.

  IF sy-subrc NE 0.
**      cursorfield = 'QUANT'.
    MESSAGE e000 WITH 'A quantidade standard é zero. É impossível'
                      'criar a palete'.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = lhme1
        language       = sy-langu
      IMPORTING
        output         = meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

**      cursorfield = 'QUANT'.
***      cursorfield = 'MATNR_PAL'.
  ENDIF.

ENDFORM.                    " check_quant
*&---------------------------------------------------------------------*
*&      Form  check_okcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_okcode .

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT'.
      PERFORM verifica_saida.
    WHEN 'SSCC'.
      PERFORM check_tipo_palete.               "AMALCATA 20071128.
      PERFORM cria_sscc.
  ENDCASE.

ENDFORM.                    " check_okcode
*&---------------------------------------------------------------------*
*&      Form  cria_f4_aufnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_f4_aufnr .

  FREE: itab_dynp, itab_return.
  CLEAR: itab_dynp, itab_return.

  DATA: itab_jest LIKE jest OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF itab_f4 OCCURS 0,
          aufnr   LIKE afpo-aufnr,
        END OF itab_f4.

  DATA: itab_dados LIKE itab_f4 OCCURS 0 WITH HEADER LINE.

  DATA: l_tit(40),
        p_aufnr LIKE afpo-aufnr.

  FREE: itab_dynp, itab_return, itab_f4, itab_dados.
  CLEAR: itab_dynp, itab_return, itab_f4, itab_dados.

  CHECK NOT matnr IS INITIAL.

  itab_dynp-fieldname = 'AUFNR'.
  APPEND itab_dynp.

** Obtém valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: itab_dynp.
  READ TABLE itab_dynp INDEX 1.
  l_matnr = itab_dynp-fieldvalue.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = matnr
    IMPORTING
      output = l_matnr.

  CONCATENATE linha+2(1) '%' INTO p_aufnr.

  CLEAR: afpo.
  SELECT * FROM afpo
  INTO CORRESPONDING FIELDS OF TABLE itab_dados
  WHERE matnr EQ l_matnr
    AND aufnr LIKE p_aufnr.

  LOOP AT itab_dados.
    CLEAR: afko.
** Selecionar apenas ordens por datas
    SELECT SINGLE * FROM afko
    WHERE aufnr EQ itab_dados-aufnr
      AND gstrs IN r_gstrs.

    CHECK sy-subrc EQ 0.

    CLEAR: aufk.
    SELECT SINGLE * FROM aufk
    WHERE aufnr EQ itab_dados-aufnr.

    CLEAR: jest.
** Selecionar ordens que estejam com status liberado e não fechado
** técnicamente
    SELECT * FROM jest
    INTO CORRESPONDING FIELDS OF TABLE itab_jest
    WHERE objnr EQ aufk-objnr
      AND inact EQ ' '
      AND stat  IN r_stat.

    CHECK sy-subrc EQ 0.
    SORT itab_tj02t.
    SORT itab_jest.

** Fechado Técnicamente
    READ TABLE itab_tj02t WITH KEY txt04 = 'ENTE'
                          BINARY SEARCH.

    CHECK sy-subrc EQ 0.

    READ TABLE itab_jest WITH KEY objnr = aufk-objnr
                                  stat  = itab_tj02t-istat
                         BINARY SEARCH.

    CHECK sy-subrc NE 0.

** Liberado
    READ TABLE itab_tj02t WITH KEY txt04 = 'LIB'
                          BINARY SEARCH.
    CHECK sy-subrc EQ 0.
    READ TABLE itab_jest WITH KEY objnr = aufk-objnr
                                  stat  = itab_tj02t-istat
                         BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    MOVE-CORRESPONDING itab_dados TO itab_f4.
    APPEND itab_f4.
    CLEAR: itab_f4.
  ENDLOOP.

  CONCATENATE 'Ordens com o material' matnr
        INTO l_tit SEPARATED BY space.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'AUFNR'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'AUFNR'
      value_org       = 'S'
      window_title    = l_tit
    TABLES
      value_tab       = itab_f4
      return_tab      = itab_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE itab_return INDEX 1.
    READ TABLE itab_dynp INDEX 1.
    itab_dynp-fieldvalue = itab_return-fieldval.
    MODIFY itab_dynp INDEX 1.
  ENDIF.

** Passar valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " cria_f4_aufnr

*&---------------------------------------------------------------------*
*&      Form  check_campos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_campos .

  CLEAR: ok_code.

  IF matnr IS INITIAL.
** Material Vazio
    LOOP AT SCREEN.
      CHECK screen-group1 EQ 'EXI'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDLOOP.
*  ELSEIF NOT matnr IS INITIAL AND
*            ean11  IS INITIAL.
*** Material Preenchido
*    LOOP AT SCREEN.
*      CHECK screen-group2 EQ 'EAN'.
*      screen-input = '0'.
*      MODIFY SCREEN.
*    ENDLOOP.
  ELSEIF NOT matnr IS INITIAL AND
             divisao IS INITIAL.
** Divisão Vazia
    LOOP AT SCREEN.
      CHECK screen-group2 EQ 'DIV' OR
            screen-group2 EQ 'EXI' OR
            screen-group2 EQ 'VIS'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ELSEIF NOT matnr   IS INITIAL AND
         NOT divisao IS INITIAL AND
             area    IS INITIAL.
** Àrea Vazia
    LOOP AT SCREEN.
      CHECK screen-group3 EQ 'DIV' OR
            screen-group2 EQ 'EXI' OR
            screen-group2 EQ 'VIS'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ELSEIF NOT matnr   IS INITIAL AND
         NOT divisao IS INITIAL AND
         NOT area    IS INITIAL AND
             linha   IS INITIAL.
** Linha Vazia
    LOOP AT SCREEN.
      CHECK screen-group2 EQ 'EXI' OR
            screen-group2 EQ 'VIS'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ELSEIF NOT matnr IS INITIAL AND
             aufnr IS INITIAL.
** Ordem de Produção Vazia
    LOOP AT SCREEN.
      CHECK screen-group2 EQ 'EXI'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ELSEIF NOT matnr IS INITIAL AND
         NOT aufnr IS INITIAL.
** Outras opções
    LOOP AT SCREEN.
      CHECK screen-group3 EQ 'EXI'.
      screen-input = '1'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  CHECK NOT aufnr IS INITIAL.

  IF quant = 'X'.

    PERFORM check_quant.

    LOOP AT SCREEN.
      CHECK screen-group4 = 'EXI'.
      screen-input = '0'.
      screen-required = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " check_campos
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr .

  CHECK NOT matnr   IS INITIAL.

  CLEAR: maktx.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = matnr
    IMPORTING
      output = l_matnr.

  CLEAR: mara.
  SELECT SINGLE * FROM mara
  WHERE matnr EQ l_matnr.

  IF sy-subrc NE 0.
    cursorfield = 'MATNR'.
    MESSAGE e000 WITH 'O material' matnr 'não existe'.
  ELSE.
    cursorfield = 'EAN11'.
    PERFORM get_descricao_matnr USING matnr
                                CHANGING maktx.
  ENDIF.

ENDFORM.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  ler_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ler_status .

  SET PF-STATUS 'STATUS_100'.
  SET TITLEBAR 'TIT_100'.

  GET PARAMETER ID 'WRK' FIELD centro.
  GET PARAMETER ID 'ZDIV' FIELD divisao.
  GET PARAMETER ID 'DGR' FIELD area.
  GET PARAMETER ID 'CFV' FIELD linha.
  GET PARAMETER ID 'ANR' FIELD aufnr.
  GET PARAMETER ID 'ZSESSAO' FIELD z02rpsessao.
*  GET PARAMETER ID 'ZOPER' FIELD operador.

ENDFORM.                    " ler_status
*&---------------------------------------------------------------------*
*&      Form  check_lgnum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_lgnum .

  CHECK NOT lgnum IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lgnum
    IMPORTING
      output = l_lgnum.

  CLEAR: t300t, lnumt.
  SELECT SINGLE lnumt FROM t300t
  INTO lnumt
  WHERE lgnum EQ l_lgnum
    AND spras EQ sy-langu.

  IF sy-subrc NE 0.
    CLEAR: lnumt.
    cursorfield = 'LGNUM'.
    MESSAGE e000 WITH 'O Depósito' lgnum 'não existe'.
  ELSE.
    cursorfield = 'DIVISAO'.
  ENDIF.

ENDFORM.                    " check_lgnum
*&---------------------------------------------------------------------*
*&      Form  check_aufnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_aufnr .

  CHECK NOT aufnr IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = aufnr
    IMPORTING
      output = l_aufnr.

  CLEAR: afko.
  SELECT SINGLE * FROM afko
  WHERE aufnr EQ l_aufnr
    AND gstrs IN r_gstrs.

  IF sy-subrc NE 0.
    cursorfield = 'AUFNR'.
    MESSAGE e000 WITH 'A Ordem de Produção' aufnr 'não existe'.
  ELSE.
    CLEAR: afpo.
    SELECT * FROM afpo
    WHERE aufnr EQ l_aufnr.
      charg = afpo-bwtar.
      EXIT.
    ENDSELECT.

    IF charg IS INITIAL.
      CONCATENATE l_aufnr(5) l_aufnr+7(5) INTO charg.
    ENDIF.

    cursorfield = 'QUANT'.
  ENDIF.

ENDFORM.                    " check_aufnr
*&---------------------------------------------------------------------*
*&      Form  check_lety1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr_pal .

  CHECK NOT lgnum     IS INITIAL AND
        NOT aufnr     IS INITIAL AND
        NOT matnr_pal IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = matnr_pal
    IMPORTING
      output = l_matnr.

  SELECT SINGLE * FROM mara WHERE matnr EQ l_matnr.

  IF sy-subrc NE 0.
    cursorfield = 'MATNR_PAL'.
    MESSAGE e000 WITH 'O tipo de palete' matnr_pal 'não existe'.
  ELSE.

    PERFORM get_descricao_matnr USING l_matnr
                                CHANGING maktx_pal.
    cursorfield = 'NUM_PAL'.
  ENDIF.

ENDFORM.                    " check_MATNR_PAL
*&---------------------------------------------------------------------*
*&      Form  cria_f4_matnr_pal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_f4_matnr_pal .

  FREE: itab_dynp, itab_return.
  CLEAR: itab_dynp, itab_return.

  DATA: BEGIN OF itab_f4 OCCURS 0,
          matnr LIKE mara-matnr,
        END OF itab_f4.

  DATA: l_tit(40).

  FREE: itab_dynp, itab_return, itab_f4.
  CLEAR: itab_dynp, itab_return, itab_f4.

  itab_dynp-fieldname = 'MATNR_PAL'.
  APPEND itab_dynp.

** Obtém valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: mara.
  SELECT * FROM mara
  INTO CORRESPONDING FIELDS OF TABLE itab_f4
  WHERE mtart EQ 'PALT'.

  CONCATENATE 'Tipo palete' 'PALT' INTO l_tit SEPARATED BY space.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'MATNR_PAL'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'MATNR_PAL'
      value_org       = 'S'
      window_title    = l_tit
    TABLES
      value_tab       = itab_f4
      return_tab      = itab_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE itab_return INDEX 1.
    READ TABLE itab_dynp INDEX 1.
    itab_dynp-fieldvalue = itab_return-fieldval.
    MODIFY itab_dynp INDEX 1.
  ENDIF.

** Passar valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " cria_f4_matnr_pal
*&---------------------------------------------------------------------*
*&      Form  cria_sscc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_sscc .

  DATA: retorno   LIKE zwm_aux-retorno,
        wa_cons   LIKE zwm030,
        l_valor   LIKE mlgn-lhmg1,
        l_quant   LIKE mlgn-lhmg1,
        l_aux(18),
        l_cons    TYPE z02rpprodp,
        l_ean11   LIKE mean-ean11,
        l_altura(4),
        l_quant1(4),
        l_cor(6),
        quant_aux TYPE i,
        l_aufnr1  LIKE afko-aufnr,
        l_in      LIKE zwm_aux-in_string,
        l_out     LIKE zwm_aux-out_string,
        l_retorno LIKE zwm_aux-retorno.

  FREE: itab_sscc.
  CLEAR: ctrl, l_msg, l_sscc1, l_sscc2, itab_sscc, l_quant.

  cursorfield = 'AUFNR'.

  CHECK erro IS INITIAL.

  IF matnr     IS INITIAL OR
     ean11     IS INITIAL OR
     lgnum     IS INITIAL OR
     aufnr     IS INITIAL OR
     lhmg1     IS INITIAL OR
     matnr_pal IS INITIAL.
    MESSAGE i000 WITH 'Para poder dar entrada produção, todos os campos'
      'devem estar preenchidos'.
  ELSE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = matnr
      IMPORTING
        output = l_matnr.

    ctrl = 'X'.

*** ACTUALIZA CAMPOS NA TABELA ZWM030
    CLEAR: wa_cons, zwm030, l_aufnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = aufnr
      IMPORTING
        output = l_aufnr.

    SELECT SINGLE * FROM zwm030
    WHERE aufnr EQ l_aufnr.

    IF sy-subrc NE 0.
      MESSAGE e000 WITH 'Ainda não foi aberta a produção'
            'para a ordem' aufnr.
    ENDIF.

*** EAN
*    CLEAR: l_caracteres, l_filler.
*    l_ean11 = ean11.
*    l_caracteres = STRLEN( l_ean11 ).
*    l_filler = 18 - l_caracteres.
*    DO l_filler TIMES.
*      l_acerto = l_acerto + 1.
*      CONCATENATE l_ean11 c_x INTO l_ean11.
*    ENDDO.
*
*** ALTURA
*    DO 4 TIMES.
*      CONCATENATE l_altura c_x INTO l_altura.
*    ENDDO.
*
*** CÔR
*    CLEAR: l_caracteres, l_filler.
*    l_cor = matnr_pal.
*    l_caracteres = STRLEN( l_cor ).
*    l_filler = 6 - l_caracteres.
*    DO l_filler TIMES.
*      l_acerto = l_acerto + 1.
*      CONCATENATE l_cor c_x INTO l_cor.
*    ENDDO.
*
*** ORDEM PRODUÇÃO
*    CLEAR: l_caracteres, l_filler.
*    l_aufnr1 = aufnr.
*    l_caracteres = STRLEN( l_aufnr1 ).
*    l_filler = 12 - l_caracteres.
*    DO l_filler TIMES.
*      l_acerto = l_acerto + 1.
*      CONCATENATE l_aufnr1 c_x INTO l_aufnr1.
*    ENDDO.
*
*** QUANTIDADE
*    IF quant = 'X'.
*      DO 4 TIMES.
*        CONCATENATE l_quant1 c_x INTO l_quant1.
*      ENDDO.
*
*** CONSTROI LINHA
*      CONCATENATE l_ean11 linha l_altura l_cor l_quant1 l_aufnr1 'X'
*                  INTO l_in.
*    ELSE.
*      CLEAR: l_caracteres, l_filler.
*      quant_aux = TRUNC( lhmg1 ).
*      l_quant1 = quant_aux.
*      CONDENSE l_quant1.
*      l_caracteres = STRLEN( l_quant1 ).
*      l_filler = 4 - l_caracteres.
*      DO l_filler TIMES.
*        l_acerto = l_acerto + 1.
*        CONCATENATE c_x l_quant1 INTO l_quant1.
*      ENDDO.
*
*** CONSTROI LINHA
*      CONCATENATE l_ean11 linha l_altura l_cor l_quant1 l_aufnr1 c_x
*                  INTO l_in.
*    ENDIF.
*
*    DO.
*      REPLACE '£' WITH ' ' INTO l_in.
*      IF sy-subrc NE 0.
*        EXIT.
*      ENDIF.
*    ENDDO.

** Entrada de Produção (MOV 101) e impressão de SSCC
    DO num_pal TIMES.

      CLEAR: l_sscc1, l_sscc2, l_msg.

      CALL FUNCTION 'ZWM_ENTRADA_PRODUCAO'
        EXPORTING
          matnr     = l_matnr
          lgnum     = lgnum
          aufnr     = l_aufnr
          cor       = matnr_pal
          charg     = charg
          lhmg1     = lhmg1
          meins     = meins
          cria_sscc = imprimir
        IMPORTING
          msg       = l_msg
          sscc1     = l_sscc1
          sscc2     = l_sscc2
        EXCEPTIONS
          exception = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.


*      CALL FUNCTION 'ZWM_RFC_ENTRADA_PRODUCAO'
*        EXPORTING
*          in_string  = l_in
*        IMPORTING
*          out_string = l_out
*          retorno    = l_retorno
*        EXCEPTIONS
*          exception  = 1
*          OTHERS     = 2.

      IF sy-subrc <> 0.
        l_msg = 'Ocorreram erros ao dar entrada de produção'.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        EXIT.
      ELSE.

        IF l_retorno IS INITIAL.
          IF imprimir = 'X'.
            WAIT UP TO 1 SECONDS.

            IF NOT l_sscc1 IS INITIAL.
              itab_sscc-sscc1 = l_sscc1.
              SUBMIT zwmrep0041 WITH s_sscc  = l_sscc1
                                WITH p_print = printer
                                WITH p_msg   = 'X'
                          AND RETURN.
            ENDIF.

            WAIT UP TO 1 SECONDS.

            IF NOT l_sscc2 IS INITIAL.
              itab_sscc-sscc2 = l_sscc2.
              SUBMIT zwmrep0041 WITH s_sscc  = l_sscc2
                                WITH p_print = printer
                                WITH p_msg   = 'X'
                          AND RETURN.
            ENDIF.

            itab_sscc-log = 'Entrada com sucesso'.
*            APPEND itab_sscc.
          ELSE.
            l_msg = 'Entradas feitas com sucesso'.
          ENDIF.
        ELSE.
*** ERRO **
*          CASE l_retorno.
*            WHEN '10'.
*              l_msg = text-e01.
*            WHEN '20'.
*              l_msg = text-e02.
*            WHEN '21'.
*              l_msg = text-e03.
*            WHEN '22'.
*              l_msg = text-e04.
*            WHEN '30'.
*              l_msg = text-e05.
*            WHEN '31'.
*              l_msg = text-e06.
*            WHEN '40'.
*              l_msg = text-e07.
*            WHEN '50'.
*              l_msg = text-e08.
*            WHEN '51'.
*              l_msg = text-e09.
*            WHEN '60'.
*              l_msg = text-e10.
*            WHEN '61'.
*              l_msg = text-e11.
*            WHEN '70'.
*              l_msg = text-e12.
*            WHEN '71'.
*              l_msg = text-e13.
*            WHEN '80'.
*              l_msg = text-e14.
*            WHEN '90'.
*              l_msg = text-e15.
*            WHEN '91'.
*              l_msg = text-e16.
*          ENDCASE.
          itab_sscc-log = l_msg.
        ENDIF.
      ENDIF.

      itab_sscc-sscc1 = l_sscc1.
      itab_sscc-sscc2 = l_sscc2.

      APPEND itab_sscc.
      CLEAR: itab_sscc, l_msg.

    ENDDO.

    IF NOT l_msg IS INITIAL.
      MESSAGE i000 WITH l_msg.
    ELSE.

*** LISTA LOG
      PERFORM lista_log_processamento.

      CLEAR: matnr, maktx, aufnr, charg, lhmg1, meins,
*             maktx_pal, matnr_pal,
             erro, l_msg, l_sscc1, l_sscc2, ok_code, itab_sscc,
             cursorfield, area, linha, desc1, l_cons,
             l_valor, l_aux, l_quant, ean11, quant.

      num_pal = 1.
    ENDIF.
  ENDIF.

  cursorfield = 'MATNR'.

ENDFORM.                    " cria_sscc
*&---------------------------------------------------------------------*
*&      Form  verifica_saida
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_saida .

  DATA: l_resp.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = text-001
      text_button_1  = 'Sim'
      icon_button_1  = ' '
      text_button_2  = 'Não'
      icon_button_2  = ' '
    IMPORTING
      answer         = l_resp
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  CHECK l_resp = '1'. "Sim
  PERFORM sai_ecra.

ENDFORM.                    " verifica_saida
*&---------------------------------------------------------------------*
*&      Form  get_descricao_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MATNR  text
*      <--P_MAKTX  text
*----------------------------------------------------------------------*
FORM get_descricao_matnr  USING    p_matnr
                          CHANGING p_maktx.

  CHECK NOT p_matnr IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_matnr
    IMPORTING
      output = l_matnr.

  CLEAR: makt, p_maktx.
  SELECT SINGLE maktx FROM makt
  INTO p_maktx
  WHERE spras EQ sy-langu
    AND matnr EQ l_matnr.

ENDFORM.                    " get_descricao_matnr

*&---------------------------------------------------------------------*
*&      Form  check_dados_globais
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_dados_globais .

  CLEAR: erro.

  CHECK NOT matnr IS INITIAL AND
        NOT aufnr IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = matnr
    IMPORTING
      output = l_matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = aufnr
    IMPORTING
      output = l_aufnr.

  CLEAR: afpo.
  SELECT * FROM afpo
  WHERE aufnr EQ l_aufnr
    AND matnr EQ l_matnr.
  ENDSELECT.

  IF sy-subrc NE 0.
    MESSAGE i000 WITH 'A ordem de produção' aufnr
      'não é válida para o material' matnr.
    erro = 'X'.
    cursorfield = 'AUFNR'.
  ELSEIF charg IS INITIAL.
    cursorfield = 'AUFNR'.
    MESSAGE e000 WITH 'Não existe lote para a'
                      'Ordem de Produção' aufnr.
  ELSE.
    CLEAR: afko.
    SELECT SINGLE * FROM afko
    WHERE aufnr EQ l_aufnr
      AND gstrs IN r_gstrs.

    IF sy-subrc NE 0.
      MESSAGE i000 WITH 'A ordem de produção' aufnr
        'não é válida para o material' matnr.
      erro = 'X'.
      cursorfield = 'AUFNR'.
    ELSEIF charg IS INITIAL.
      cursorfield = 'AUFNR'.
      MESSAGE e000 WITH 'Não existe lote para a'
                        'Ordem de Produção' aufnr.
    ELSE.
      cursorfield = 'QUANT'.
    ENDIF.
  ENDIF.

ENDFORM.                    " check_dados_globais
*&---------------------------------------------------------------------*
*&      Form  cria_range_datas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_range_datas .

** Cria validação de datas para ordens de produção
  FREE: r_gstrs.
  CLEAR: r_gstrs.

  r_gstrs-low = sy-datum - 35.
  r_gstrs-high = sy-datum.
  r_gstrs-option = 'BT'.
  r_gstrs-sign = 'I'.
  APPEND r_gstrs.

** Cria validação de datas para dados de sessão
  FREE: r_data.
  CLEAR: r_data.

  r_data-low = sy-datum - 3.
  r_data-high = sy-datum.
  r_data-option = 'BT'.
  r_data-sign = 'I'.
  APPEND r_data.

ENDFORM.                    " cria_range_datas
*&---------------------------------------------------------------------*
*&      Form  check_lhmg1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_lhmg1 .

  DATA: l_aux(18),
        l_lhmg1 LIKE mlgn-lhmg1.

  CHECK NOT lhmg1 IS INITIAL.

  CLEAR: marm, l_umrez.
  SELECT SINGLE umrez FROM marm
  INTO l_umrez
  WHERE matnr EQ l_matnr
    AND meinh EQ 'PAL'.

  l_aux = l_umrez.
  l_lhmg1 = l_aux.

  IF lhmg1 GT l_lhmg1.
    MESSAGE e000 WITH 'A quantidade não pode ser maior do que'
                      l_umrez '. É impossível criar a palete'.
    cursorfield = 'LHMG1'.
  ELSE.
    cursorfield = 'MATNR_PAL'.
  ENDIF.

ENDFORM.                    " check_lhmg1
*&---------------------------------------------------------------------*
*&      Form  lista_log_processamento
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lista_log_processamento .

  CHECK NOT itab_sscc[] IS INITIAL.

** Prepara Listagem
  PERFORM ajusta_propriedades.

** Colunas Listagem
  PERFORM catalogo.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat              = fieldcat_tab
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      it_events                = eventos
      is_layout                = layout
      i_grid_title             = grid_title
      i_callback_pf_status_set = 'PFSTATUS'
      is_variant               = is_variant
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_program       = sy-repid
      i_save                   = 'X'
    TABLES
      t_outtab                 = itab_sscc
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.                    " lista_log_processamento

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_PROPRIEDADES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ajusta_propriedades.

  layout-colwidth_optimize = p_cwidth.
  layout-window_titlebar = sy-title.
  layout-zebra = 'X'.
  layout-no_vline = p_vrtgln.
  layout-no_vline = p_vrtgln.
  layout-def_status = 'A'.

** Eventos a capturar
  REFRESH eventos.
  wa_eventos-name = 'USER_COMMAND'.
  wa_eventos-form = 'HOTSPOT_ACTIVO'.
  APPEND wa_eventos TO eventos.

ENDFORM.                    " AJUSTA_PROPRIEDADES

*&---------------------------------------------------------------------*
*&      Form   CATALOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  catalogo.

  FREE fieldcat_tab.
  CLEAR fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'SSCC1'.
  fieldcat_linha-reptext_ddic = 'SSCC1'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'SSCC2'.
  fieldcat_linha-reptext_ddic = 'SSCC2'.
  fieldcat_linha-just = 'L'.
  fieldcat_linha-key = 'X'.
  APPEND fieldcat_linha TO fieldcat_tab.

  CLEAR fieldcat_linha.
  fieldcat_linha-fieldname = 'LOG'.
  fieldcat_linha-reptext_ddic = 'LOG.'.
  fieldcat_linha-just = 'L'.
  APPEND fieldcat_linha TO fieldcat_tab.

ENDFORM.                    "catalogo

************************************************************************
*   Form PFSTATUS
************************************************************************
FORM pfstatus USING lt_extab.

  SET PF-STATUS 'STANDARD'.

ENDFORM.                    "pfstatus

*&---------------------------------------------------------------------*
*&      Form TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.

  STATICS: lt_list_commentary TYPE slis_t_listheader,
           ls_list_commentary TYPE slis_listheader.

  DATA: l_datum(10),
        l_uzeit(8).

  FREE: lt_list_commentary.
  CLEAR: lt_list_commentary.

  WRITE sy-datum TO l_datum USING EDIT MASK '__.__.____'.
  WRITE sy-uzeit TO l_uzeit USING EDIT MASK '__:__:__'.

** Título
  ls_list_commentary-typ  = 'H'.
  ls_list_commentary-info = 'Log. entradas manuais'.
  APPEND ls_list_commentary TO lt_list_commentary.

** Informações adicionais
  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = 'Usuário:'.
  ls_list_commentary-info = sy-uname.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = 'Data:'.
  ls_list_commentary-info = l_datum.
  APPEND ls_list_commentary TO lt_list_commentary.

  ls_list_commentary-typ  = 'S'.
  ls_list_commentary-key  = 'Hora:'.
  ls_list_commentary-info = l_uzeit.
  APPEND ls_list_commentary TO lt_list_commentary.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_list_commentary.

ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  valida_quant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_quant .

  CHECK NOT aufnr IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = matnr
    IMPORTING
      output = l_matnr.

  CLEAR: mara, meins.
  SELECT SINGLE meins FROM mara
  INTO meins
  WHERE matnr EQ l_matnr.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input          = meins
      language       = sy-langu
    IMPORTING
      output         = meins
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.

  cursorfield = 'LHMG1'.

ENDFORM.                    " valida_quant
*&---------------------------------------------------------------------*
*&      Form  f4_area
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_area .

  FREE: itab_dynp, itab_return.
  CLEAR: itab_dynp, itab_return.

  DATA: BEGIN OF itab_f4 OCCURS 0,
          area LIKE z02rparln-dispo,
        END OF itab_f4.

  DATA: l_tit(40).

  FREE: itab_dynp, itab_return, itab_f4.
  CLEAR: itab_dynp, itab_return, itab_f4.

  CHECK NOT divisao IS INITIAL.

  itab_dynp-fieldname = 'AREA'.
  APPEND itab_dynp.

** Obtém valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: itab_dynp.
  READ TABLE itab_dynp INDEX 1.

  LOOP AT itab_arln WHERE divisao EQ divisao
                      AND werks   EQ centro.

    itab_f4-area = itab_arln-dispo.
    COLLECT itab_f4.
    CLEAR: itab_f4.
  ENDLOOP.

  CONCATENATE 'Áreas para a divisão' divisao
        INTO l_tit SEPARATED BY space.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'AREA'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'AREA'
      value_org       = 'S'
      window_title    = l_tit
    TABLES
      value_tab       = itab_f4
      return_tab      = itab_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE itab_return INDEX 1.
    READ TABLE itab_dynp INDEX 1.
    itab_dynp-fieldvalue = itab_return-fieldval.
    MODIFY itab_dynp INDEX 1.
  ENDIF.

** Passar valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                                                    " f4_area
*&---------------------------------------------------------------------*
*&      Form  check_centro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_centro .

  CHECK NOT centro IS INITIAL AND
        NOT ean11  IS INITIAL.

  CLEAR: t001w.
  SELECT SINGLE name1 FROM t001w
  INTO desc1
  WHERE werks EQ centro.

  IF sy-subrc NE 0.
    CLEAR: desc1.
    cursorfield = 'CENTRO'.
    MESSAGE e000 WITH 'O Centro' centro 'não existe'.
  ELSE.
    cursorfield = 'LGNUM'.
  ENDIF.

ENDFORM.                    " check_centro
*&---------------------------------------------------------------------*
*&      Form  check_divisao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_divisao .

  CHECK NOT divisao IS INITIAL.

  READ TABLE itab_arln WITH KEY divisao = divisao
                                werks   = centro
                       BINARY SEARCH.
  IF sy-subrc EQ 0.
    cursorfield = 'AREA'.
  ELSE.
    cursorfield = 'DIVISAO'.
    MESSAGE e000 WITH 'A Divisão' divisao 'não existe'.
  ENDIF.

ENDFORM.                    " check_divisao
*&---------------------------------------------------------------------*
*&      Form  check_area
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_area .

  CHECK NOT area IS INITIAL.

  READ TABLE itab_arln WITH KEY divisao = divisao
                                werks   = centro
                                dispo   = area
                       BINARY SEARCH.
  IF sy-subrc EQ 0.
    cursorfield = 'LINHA'.
  ELSE.
    cursorfield = 'AREA'.
    MESSAGE e000 WITH 'A Área' area 'não existe'.
  ENDIF.

ENDFORM.                    " check_area

*&---------------------------------------------------------------------*
*&      Form  check_linha
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_linha .

  CHECK NOT linha IS INITIAL.

  READ TABLE itab_arln WITH KEY divisao = divisao
                                werks   = centro
                                dispo   = area
                                fevor   = linha
                       BINARY SEARCH.
  IF sy-subrc EQ 0.
    cursorfield = 'AUFNR'.
  ELSE.
    cursorfield = 'LINHA'.
    MESSAGE e000 WITH 'A Área' area 'não existe'.
  ENDIF.

ENDFORM.                    " check_linha
*&---------------------------------------------------------------------*
*&      Form  f4_linha
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_linha .

  FREE: itab_dynp, itab_return.
  CLEAR: itab_dynp, itab_return.

  DATA: BEGIN OF itab_f4 OCCURS 0,
          linha LIKE z02rparln-fevor,
        END OF itab_f4.

  DATA: l_tit(40).

  FREE: itab_dynp, itab_return, itab_f4.
  CLEAR: itab_dynp, itab_return, itab_f4.

  CHECK NOT area IS INITIAL.

  itab_dynp-fieldname = 'LINHA'.
  APPEND itab_dynp.

** Obtém valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: itab_dynp.
  READ TABLE itab_dynp INDEX 1.

  LOOP AT itab_arln WHERE divisao EQ divisao
                      AND werks   EQ centro
                      AND dispo   EQ area.

    itab_f4-linha = itab_arln-fevor.
    COLLECT itab_f4.
    CLEAR: itab_f4.
  ENDLOOP.

  CONCATENATE 'Linhas para a divisão' divisao
        INTO l_tit SEPARATED BY space.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LINHA'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'LINHA'
      value_org       = 'S'
      window_title    = l_tit
    TABLES
      value_tab       = itab_f4
      return_tab      = itab_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE itab_return INDEX 1.
    READ TABLE itab_dynp INDEX 1.
    itab_dynp-fieldvalue = itab_return-fieldval.
    MODIFY itab_dynp INDEX 1.
  ENDIF.

** Passar valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                                                    " f4_linha
*&---------------------------------------------------------------------*
*&      Form  cria_ranges_status_ordem
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_ranges_status_ordem .

  FREE: r_txt04.
  CLEAR: r_txt04.

  r_txt04-low = 'LIB'.
  r_txt04-option = 'EQ'.
  r_txt04-sign = 'I'.
  APPEND r_txt04.

  r_txt04-low = 'ENTE'.
  r_txt04-option = 'EQ'.
  r_txt04-sign = 'I'.
  APPEND r_txt04.

ENDFORM.                    " cria_ranges_status_ordem
*&---------------------------------------------------------------------*
*&      Form  check_registo_zwm030
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_registo_zwm030 .

  CHECK NOT aufnr IS INITIAL.

  CLEAR: zwm030.
  SELECT SINGLE * FROM zwm030
  WHERE aufnr EQ aufnr.

  IF sy-subrc EQ 0.
    MESSAGE e000 WITH 'Impossível criar SSCC.'
                 'Ainda existe uma sessão em aberto para a ordem' aufnr.
  ENDIF.

ENDFORM.                    " check_registo_zwm030
*&---------------------------------------------------------------------*
*&      Form  get_dados_sessao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dados_sessao .

  CHECK NOT divisao IS INITIAL AND
        NOT area    IS INITIAL AND
        NOT linha   IS INITIAL.

  FREE: itab_sessao.
  CLEAR: itab_sessao.

  CLEAR: z02rpsessao.
  SELECT * FROM z02rpsessao
  INTO CORRESPONDING FIELDS OF TABLE itab_sessao
  WHERE divisao EQ divisao
    AND werks   EQ centro
    AND area    EQ area
    AND linha   EQ linha
    AND data    IN r_data.

ENDFORM.                    " get_dados_sessao
*&---------------------------------------------------------------------*
*&      Form  check_ean
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ean .

  CHECK NOT matnr IS INITIAL AND
        NOT ean11 IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = matnr
    IMPORTING
      output = l_matnr.

  CLEAR: mara.
  SELECT SINGLE * FROM mara
  WHERE matnr EQ l_matnr
    AND ean11 EQ ean11.

  IF sy-subrc NE 0.
    cursorfield = 'MATNR'.
    MESSAGE e000 WITH 'O EAN/DUN' ean11 'não pertence ao material'
                      matnr.
  ENDIF.

ENDFORM.                    " check_ean
*&---------------------------------------------------------------------*
*&      Form  cria_f4_ean11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_f4_ean11 .

  FREE: itab_dynp, itab_return.
  CLEAR: itab_dynp, itab_return.

  DATA: BEGIN OF itab_f4 OCCURS 0,
          ean11 LIKE mean-ean11,
          eantp LIKE mean-eantp,
        END OF itab_f4.

  DATA: l_tit(40).

  FREE: itab_dynp, itab_return, itab_f4.
  CLEAR: itab_dynp, itab_return, itab_f4.

  itab_dynp-fieldname = 'EAN11'.
  APPEND itab_dynp.

** Obtém valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = matnr
    IMPORTING
      output = l_matnr.

  CLEAR: mean.
  SELECT * FROM mean
  INTO CORRESPONDING FIELDS OF TABLE itab_f4
  WHERE matnr EQ l_matnr.

  CONCATENATE 'EAN/DUN para o material' matnr
  INTO l_tit SEPARATED BY space.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'EAN11'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'EAN11'
      value_org       = 'S'
      window_title    = l_tit
    TABLES
      value_tab       = itab_f4
      return_tab      = itab_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE itab_return INDEX 1.
    READ TABLE itab_dynp INDEX 1.
    itab_dynp-fieldvalue = itab_return-fieldval.
    MODIFY itab_dynp INDEX 1.
  ENDIF.

** Passar valores para o search-help
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = itab_dynp
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " cria_f4_ean11
*&---------------------------------------------------------------------*
*&      Form  check_ean11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ean11 .

  CHECK NOT ean11 IS INITIAL.

  CLEAR: mean.
  SELECT * FROM mean
  WHERE ean11 EQ ean11.
    EXIT.
  ENDSELECT.
  IF sy-subrc NE 0.
    cursorfield = 'EAN11'.
    MESSAGE e000 WITH 'O EAN/DUN' ean11 'não existe'.
  ELSE.
    cursorfield = 'CENTRO'.
  ENDIF.

ENDFORM.                    " check_ean11
*&---------------------------------------------------------------------*
*&      Form  check_tipo_palete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_tipo_palete .

  DATA: lv_cor LIKE ztipopalete-cor.

* Caso exista uma entrada na tabela de parametrizacao ZTIPOPALETE
* substitui o tipo de palete pela indicada na tabela
  SELECT SINGLE cor INTO lv_cor
             FROM ztipopalete
             WHERE matnr = l_matnr
               AND fevor = linha.
  IF sy-subrc = 0 AND lv_cor <> ''.
    matnr_pal = lv_cor.
  ENDIF.

ENDFORM.                    " check_tipo_palete
