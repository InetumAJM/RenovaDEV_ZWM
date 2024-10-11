************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWMREP0010                                               *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Impressão de informação de recepções - Terceiros         *
* Criado por: Bruno Simões                                             *
* Criado em.: 03/12/2003                                               *
* Tipo PRG..: Report                                                   *
************************************************************************
REPORT  zwmrep0010 MESSAGE-ID zwmmsg001.

TABLES : zwm023,
         zwm016,
         zwm017,
         zwm005,
         zwm013,
         marm,
         makt,
         mara,
         vekp,
         vepo,
         ekko,
         ekpo.

** Variáveis Globais
DATA : porta LIKE zwm016-porta,
       save_index LIKE sy-tabix,
*       palete1 LIKE zwm017-material,
*       palete2 LIKE zwm017-material,
*       palete3 LIKE zwm017-material,
*       palete4 LIKE zwm017-material,
*       quantidade1 LIKE zwm017-quantidade,
*       quantidade2 LIKE zwm017-quantidade,
*       quantidade3 LIKE zwm017-quantidade,
*       quantidade4 LIKE zwm017-quantidade,
*       quantidade1_aux(17),
*       quantidade2_aux(17),
*       quantidade3_aux(17),
*       quantidade4_aux(17),
*       quantidade_teo1 LIKE zwm017-quantidade,
*       quantidade_teo2 LIKE zwm017-quantidade,
*       quantidade_teo3 LIKE zwm017-quantidade,
*       quantidade_teo4 LIKE zwm017-quantidade,
*       quantidade_teo1a(17),
*       quantidade_teo2a(17),
*       quantidade_teo3a(17),
*       quantidade_teo4a(17),
       matricula LIKE zwm005-matricula,
*       desc_pal1 LIKE makt-maktx,
*       desc_pal2 LIKE makt-maktx,
*       desc_pal3 LIKE makt-maktx,
*       desc_pal4 LIKE makt-maktx,
*       uni_pal1  LIKE mara-meins,
*       uni_pal2  LIKE mara-meins,
*       uni_pal3  LIKE mara-meins,
*       uni_pal4  LIKE mara-meins,
       total_pal_teorica LIKE zwm017-quantidade,
       total_pal_teo_aux(17),
       total_pal_real LIKE zwm017-quantidade,
       total_pal_real_aux(17),
       parte_inteira(3),
       parte_decimal(3),
       desc_transportador LIKE zwm005-desc_transp,
       fornecedor LIKE ekko-lifnr.

DATA : BEGIN OF l_recepcao OCCURS 0,
          material        LIKE zwm017-material,
          matnr           LIKE zwm023-matnr,
          quantidade      LIKE zwm023-quantidade,    "qtd real das paletes
          ebeln           LIKE zwm023-ebeln,
          ebelp           LIKE zwm023-ebelp,
          maktx           LIKE makt-maktx,
          quantidade_real LIKE zwm017-quantidade,
          quantidade_teo  LIKE zwm017-quantidade,
          num_entrada     LIKE zwm017-num_entrada,
          uni             LIKE zwm017-uni.
DATA : END OF l_recepcao.

DATA : BEGIN OF l_zwm013 OCCURS 0.
        INCLUDE STRUCTURE zwm013.
DATA : END OF l_zwm013.

DATA : lvs_itcpo LIKE itcpo.

RANGES: r_ebeln FOR ekko-ebeln.

** Parametros de selecção
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-h01.
PARAMETERS: p_armaz  LIKE lagp-lgnum DEFAULT '100' OBLIGATORY,
            p_user   LIKE usr02-bname DEFAULT sy-uname OBLIGATORY,
            p_pulmao LIKE zwm013-destino OBLIGATORY,
            p_talao  LIKE zwm006-num_entrada MATCHCODE OBJECT zwm_num_talao OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  CLEAR: l_recepcao.
  FREE: l_recepcao.

** Obter dados
  PERFORM carrega_info.

  IF l_recepcao[] IS INITIAL.
    MESSAGE i000 WITH 'Não existem dados para as opções indicadas'.
    EXIT.
  ELSE.
** Para os materiais paletes
    PERFORM carrega_info_real.

** Imprime talão
    PERFORM imprime_talao.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  CARREGA_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_info .

  DATA: itab_zwm017 LIKE zwm017 OCCURS 0 WITH HEADER LINE,
        l_quant LIKE vhuadmin-quantity,
        l_real  LIKE vhuadmin-quantity,
        l_valor LIKE vhuadmin-quantity,
        l_aux(20),
        l_talao LIKE zwm017-num_entrada.

  CLEAR : save_index, porta.

  PERFORM cria_ranges_paletes.

  PERFORM converte_formato_interno USING p_talao
                                   CHANGING l_talao.

** Obter dados para quantidades teóricas
  SELECT * FROM zwm017
  INTO CORRESPONDING FIELDS OF TABLE itab_zwm017
  WHERE armazem     EQ p_armaz
    AND num_entrada EQ p_talao.

  CHECK sy-subrc EQ 0.

** Carregamentos relativos aos packing materials
  SELECT * FROM zwm023
  INTO CORRESPONDING FIELDS OF TABLE l_recepcao
  FOR ALL ENTRIES IN itab_zwm017
  WHERE armazem   EQ p_armaz
    AND user_name EQ p_user
    AND ebeln     EQ itab_zwm017-ebeln.

  CHECK sy-subrc EQ 0.

** Porta associada à descarga
  SELECT SINGLE porta FROM zwm016
  INTO porta
  WHERE armazem   EQ p_armaz
    AND pulmao    EQ p_pulmao
    AND user_name EQ p_user.

** Matricula do camião
  SELECT SINGLE matricula FROM zwm005
  INTO matricula
  WHERE armazem     EQ p_armaz
    AND num_entrada EQ p_talao.

** Transportador
  SELECT SINGLE desc_transp
  INTO desc_transportador
  FROM zwm005
  WHERE armazem     EQ p_armaz
    AND num_entrada EQ p_talao.

  SORT: itab_zwm017, l_recepcao.

** Obter as Quantidades Teóricas/ Reais
  LOOP AT l_recepcao.

    save_index = sy-tabix.

** Obter as quantidades teóricas - Para items que não paletes
    READ TABLE itab_zwm017 WITH KEY armazem     = p_armaz
                                    ebeln       = l_recepcao-ebeln
                                    ebelp       = l_recepcao-ebelp
                                    num_entrada = p_talao
                           BINARY SEARCH.

    IF sy-subrc = 0.
      CLEAR: zwm017.
      zwm017 = itab_zwm017.
      l_recepcao-num_entrada    = zwm017-num_entrada.
      l_recepcao-material       = zwm017-material.
      l_recepcao-quantidade_teo = zwm017-quantidade.
      l_recepcao-uni            = zwm017-uni.

** Descrição do material
      SELECT SINGLE maktx FROM makt
      INTO l_recepcao-maktx
      WHERE matnr = l_recepcao-material
        AND spras = sy-langu.

*** Obter quantidades reais
*      CLEAR: ekpo.
*      SELECT SINGLE * FROM ekpo
*      WHERE ebeln EQ l_recepcao-ebeln
*        AND ebelp EQ l_recepcao-ebelp.
*
*** Obter as quantidades Reais
*      CLEAR: l_quant, l_real, l_valor, l_aux.
*      l_aux = l_recepcao-quantidade.
*      l_quant = l_aux.
*
*      CALL FUNCTION 'VHUMISC_CONVERT_TO_BASEUNIT'
*        EXPORTING
*          matnr             = ekpo-matnr
*          quantity          = l_quant
*          unitqty           = 'PAL'
*        IMPORTING
*          basequantity      = l_real
*        CHANGING
*          baseunit          = l_recepcao-uni
*        EXCEPTIONS
*          conversion_failed = 1
*          rounding_error    = 2
*          OTHERS            = 3.
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ELSE.
*        CLEAR: l_aux.
*        l_aux = l_real.
*        l_recepcao-quantidade_real = l_aux.
*      ENDIF.
    ENDIF.

    MODIFY l_recepcao INDEX save_index.
    CLEAR: l_recepcao.
  ENDLOOP.

  SORT l_recepcao.

** Material PALETE
  LOOP AT itab_zwm017 WHERE ebeln IN r_ebeln.
    CLEAR: zwm017.
    zwm017 = itab_zwm017.
    l_recepcao-num_entrada    = zwm017-num_entrada.
    l_recepcao-material       = zwm017-material.
    l_recepcao-quantidade_teo = zwm017-quantidade.
    l_recepcao-uni            = zwm017-uni.

** Descrição do material
    SELECT SINGLE maktx FROM makt
    INTO l_recepcao-maktx
    WHERE matnr = l_recepcao-material
      AND spras = sy-langu.

    APPEND l_recepcao.
    CLEAR: l_recepcao.
  ENDLOOP.

ENDFORM.                    " CARREGA_INFO

*&---------------------------------------------------------------------*
*&      Form  CARREGA_INFO_REAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_info_real .

  DATA: itab_zwm013 LIKE zwm013 OCCURS 0 WITH HEADER LINE,
        itab_vekp   LIKE vekp   OCCURS 0 WITH HEADER LINE,
        p_dest      LIKE zwm013-destino,
        l_index     LIKE sy-tabix.

  FREE: itab_zwm013.
  CLEAR: itab_zwm013, p_dest.

  SORT l_recepcao BY matnr.

  CONCATENATE p_pulmao(12) '%' INTO p_dest.

** Obter dados das HU
  CLEAR: zwm013.
  SELECT * FROM zwm013
  INTO CORRESPONDING FIELDS OF TABLE itab_zwm013
  WHERE armazem EQ   p_armaz
    AND destino LIKE p_dest.

  CHECK sy-subrc EQ 0.

  FREE: itab_vekp.
  CLEAR: itab_vekp.

  SELECT * FROM vekp
  INTO CORRESPONDING FIELDS OF TABLE itab_vekp
  FOR ALL ENTRIES IN itab_zwm013
  WHERE exidv EQ itab_zwm013-sscc.

  SORT itab_vekp BY exidv.

  LOOP AT itab_zwm013.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = itab_zwm013-sscc
      IMPORTING
        output = itab_zwm013-sscc.

    LOOP AT itab_vekp WHERE exidv = itab_zwm013-sscc.
      CLEAR: l_index, l_recepcao.

      SELECT SINGLE * FROM vepo WHERE venum = itab_vekp-venum.

      READ TABLE l_recepcao WITH KEY material = vepo-matnr.
      l_index = sy-tabix.
      IF sy-subrc EQ 0.
        l_recepcao-quantidade_real =
               l_recepcao-quantidade_real + vepo-vemng.

        MODIFY l_recepcao INDEX l_index.
        CLEAR: l_recepcao.
      ENDIF.

** Material Palete
      READ TABLE l_recepcao WITH KEY material = itab_vekp-vhilm.
      l_index = sy-tabix.
      IF sy-subrc EQ 0.
        l_recepcao-quantidade_real =
               l_recepcao-quantidade_real + 1.

        MODIFY l_recepcao INDEX l_index.
        CLEAR: l_recepcao.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " CARREGA_INFO_REAL


*&---------------------------------------------------------------------*
*&      Form  IMPRIME_TALAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_talao.

*** Actualização dos parâmetros da impressão
  lvs_itcpo-tdimmed = 'X'.
  lvs_itcpo-tdnewid = 'X'.

** Criação do formulário
  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device                      = 'PRINTER'
      dialog                      = 'X'
      form                        = 'ZWMFORM003'
      language                    = sy-langu
      options                     = lvs_itcpo
    EXCEPTIONS
      canceled                    = 1
      device                      = 2
      form                        = 3
      options                     = 4
      unclosed                    = 5
      mail_options                = 6
      archive_error               = 7
      invalid_fax_number          = 8
      more_params_needed_in_batch = 9
      spool_error                 = 10
      OTHERS                      = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  CALL FUNCTION 'START_FORM'
    EXPORTING
      form        = 'ZWMFORM003'
      language    = sy-langu
      program     = 'ZWMREP0010'
    EXCEPTIONS
      form        = 1
      format      = 2
      unended     = 3
      unopened    = 4
      unused      = 5
      spool_error = 6
      OTHERS      = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA: l_total_qtd LIKE l_recepcao-quantidade_teo.

  SORT l_recepcao BY material.

** Somar por tipo de palete
  LOOP AT l_recepcao.
    AT NEW material.
      SUM.
      MOVE l_recepcao-quantidade_teo TO l_total_qtd.
    ENDAT.

    MOVE l_total_qtd TO l_recepcao-quantidade_teo.
    MODIFY l_recepcao.

    CLEAR l_recepcao.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM l_recepcao COMPARING material.
*
  LOOP AT l_recepcao.

** Impressão do número do talão
    AT NEW ebeln.

      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          window                   = 'TALAO'
        EXCEPTIONS
          element                  = 1
          function                 = 2
          type                     = 3
          unopened                 = 4
          unstarted                = 5
          window                   = 6
          bad_pageformat_for_print = 7
          spool_error              = 8
          OTHERS                   = 9.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          window                   = 'CODBARRA'
        EXCEPTIONS
          element                  = 1
          function                 = 2
          type                     = 3
          unopened                 = 4
          unstarted                = 5
          window                   = 6
          bad_pageformat_for_print = 7
          spool_error              = 8
          OTHERS                   = 9.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          window                   = 'HEADER'
        EXCEPTIONS
          element                  = 1
          function                 = 2
          type                     = 3
          unopened                 = 4
          unstarted                = 5
          window                   = 6
          bad_pageformat_for_print = 7
          spool_error              = 8
          OTHERS                   = 9.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDAT.

** Impressão dos items
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element                  = 'ITEMS'
        window                   = 'MAIN'
      EXCEPTIONS
        element                  = 1
        function                 = 2
        type                     = 3
        unopened                 = 4
        unstarted                = 5
        window                   = 6
        bad_pageformat_for_print = 7
        spool_error              = 8
        OTHERS                   = 9.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*    AT LAST.
*      DATA :  l_total_palete LIKE l_recepcao_aux-quantidade.
*      SORT l_recepcao_aux BY matnr.

** Somar por tipo de palete
*      LOOP AT l_recepcao_aux.
*        AT NEW matnr.
*          SUM.
*          MOVE l_recepcao_aux-quantidade TO l_total_palete.
*        ENDAT.
*
*        MOVE l_total_palete TO l_recepcao_aux-quantidade.
*        MODIFY l_recepcao_aux.
*
*        CLEAR l_recepcao_aux.
*      ENDLOOP.

*      DELETE ADJACENT DUPLICATES FROM l_recepcao_aux COMPARING matnr.

*
*      CLEAR : total_pal_teorica, total_pal_real.
*
*      CLEAR : desc_pal1,palete1,uni_pal1,quantidade_teo1,
*              parte_inteira,parte_decimal.
*      READ TABLE l_recepcao_aux INDEX 1.
*      palete1 = l_recepcao_aux-matnr.
**      quantidade1 = l_recepcao_aux-quantidade.
*      WRITE quantidade1 TO quantidade1_aux LEFT-JUSTIFIED.
*      SPLIT quantidade1_aux AT ',' INTO parte_inteira parte_decimal.
*      CLEAR quantidade1_aux.
*      MOVE parte_inteira TO quantidade1_aux.
*      IF quantidade1_aux = '0'.
*        CLEAR quantidade1_aux.
*        MOVE '-' TO quantidade1_aux+1(1).
*      ENDIF.
*
*
*      CLEAR : parte_inteira, parte_decimal.
*      quantidade_teo1 = l_recepcao_aux-quantidade_teo_pal.
*      WRITE quantidade_teo1 TO quantidade_teo1a LEFT-JUSTIFIED.
*      SPLIT quantidade_teo1a AT ',' INTO parte_inteira parte_decimal.
*      CLEAR quantidade_teo1a.
*      MOVE parte_inteira TO quantidade_teo1a.
*      IF quantidade_teo1a = '0'.
*        CLEAR quantidade_teo1a.
*        MOVE '-' TO quantidade_teo1a+1(1).
*      ENDIF.
*
*
*** Descrição da palete 1
*      SELECT SINGLE maktx FROM makt INTO desc_pal1
*                          WHERE matnr = palete1 AND
*                                spras = sy-langu.
*** Unidade de medida da palete 1
*      SELECT SINGLE meins FROM mara INTO uni_pal1
*                          WHERE matnr = palete1.
*
*
*
*      CLEAR : desc_pal2,palete2,uni_pal2,quantidade_teo2.
*      READ TABLE l_recepcao_aux INDEX 2.
*      IF sy-subrc = 0.
*        palete2 = l_recepcao_aux-matnr.
*        quantidade2 = l_recepcao_aux-quantidade.
*        WRITE quantidade2 TO quantidade2_aux LEFT-JUSTIFIED.
*        SPLIT quantidade2_aux AT ',' INTO parte_inteira parte_decimal.
*        CLEAR quantidade2_aux.
*        MOVE parte_inteira TO quantidade2_aux.
*        IF quantidade2_aux = '0'.
*          CLEAR quantidade2_aux.
*          MOVE '-' TO quantidade2_aux+1(1).
*        ENDIF.
*
*        quantidade_teo2 = l_recepcao_aux-quantidade_teo_pal.
*        WRITE quantidade_teo2 TO quantidade_teo2a LEFT-JUSTIFIED.
*        SPLIT quantidade_teo2a AT ',' INTO parte_inteira parte_decimal.
*        CLEAR quantidade_teo2a.
*        MOVE parte_inteira TO quantidade_teo2a.
*        IF quantidade_teo2a = '0'.
*          CLEAR quantidade_teo2a.
*          MOVE '-' TO quantidade_teo2a+1(1).
*        ENDIF.
*
*** Descrição da palete 2
*        SELECT SINGLE maktx FROM makt INTO desc_pal2
*                            WHERE matnr = palete2 AND
*                                  spras = sy-langu.
*** Unidade de medida da palete 2
*        SELECT SINGLE meins FROM mara INTO uni_pal2
*                            WHERE matnr = palete2.
*      ENDIF.
*
**** Palete 3
*      CLEAR : desc_pal3, palete3, uni_pal3, quantidade_teo3.
*      READ TABLE l_recepcao_aux INDEX 3.
*      IF sy-subrc = 0.
*        palete3 = l_recepcao_aux-matnr.
*        quantidade3 = l_recepcao_aux-quantidade.
*        WRITE quantidade3 TO quantidade3_aux LEFT-JUSTIFIED.
*        SPLIT quantidade3_aux AT ',' INTO parte_inteira parte_decimal.
*        CLEAR quantidade3_aux.
*        MOVE parte_inteira TO quantidade3_aux.
*        IF quantidade3_aux = '0'.
*          CLEAR quantidade3_aux.
*          MOVE '-' TO quantidade3_aux+1(1).
*        ENDIF.
*
*        quantidade_teo3 = l_recepcao_aux-quantidade_teo_pal.
*        WRITE quantidade_teo3 TO quantidade_teo3a LEFT-JUSTIFIED.
*        SPLIT quantidade_teo3a AT ',' INTO parte_inteira parte_decimal.
*        CLEAR quantidade_teo3a.
*        MOVE parte_inteira TO quantidade_teo3a.
*        IF quantidade_teo3a = '0'.
*          CLEAR quantidade_teo3a.
*          MOVE '-' TO quantidade_teo3a+1(1).
*        ENDIF.
*
*** Descrição da palete 3
*        SELECT SINGLE maktx FROM makt INTO desc_pal3
*                            WHERE matnr = palete3 AND
*                                  spras = sy-langu.
*** Unidade de medida da palete 3
*        SELECT SINGLE meins FROM mara INTO uni_pal3
*                            WHERE matnr = palete3.
*      ENDIF.
*
**** Palete 3
*      CLEAR : desc_pal4, palete4, uni_pal4, quantidade_teo4.
*      READ TABLE l_recepcao_aux INDEX 4.
*      IF sy-subrc = 0.
*        palete4 = l_recepcao_aux-matnr.
*        quantidade4 = l_recepcao_aux-quantidade.
*        WRITE quantidade4 TO quantidade4_aux LEFT-JUSTIFIED.
*        SPLIT quantidade4_aux AT ',' INTO parte_inteira parte_decimal.
*        CLEAR quantidade4_aux.
*        MOVE parte_inteira TO quantidade4_aux.
*        IF quantidade4_aux = '0'.
*          CLEAR quantidade4_aux.
*          MOVE '-' TO quantidade4_aux+1(1).
*        ENDIF.
*
*        quantidade_teo4 = l_recepcao_aux-quantidade_teo_pal.
*        WRITE quantidade_teo4 TO quantidade_teo4a LEFT-JUSTIFIED.
*        SPLIT quantidade_teo4a AT ',' INTO parte_inteira parte_decimal.
*        CLEAR quantidade_teo4a.
*        MOVE parte_inteira TO quantidade_teo4a.
*        IF quantidade_teo4a = '0'.
*          CLEAR quantidade_teo4a.
*          MOVE '-' TO quantidade_teo4a+1(1).
*        ENDIF.
*
*** Descrição da palete 4
*        SELECT SINGLE maktx FROM makt INTO desc_pal4
*                            WHERE matnr = palete4 AND
*                                  spras = sy-langu.
*** Unidade de medida da palete 3
*        SELECT SINGLE meins FROM mara INTO uni_pal4
*                            WHERE matnr = palete4.
*
*      ENDIF.
*
*** Cálculo das quantidades totais das paletes - reais e teóricas
*      CLEAR : parte_inteira, parte_decimal.
*      total_pal_real = quantidade1 + quantidade2 + quantidade3 + quantidade4.
*      WRITE total_pal_real TO total_pal_real_aux LEFT-JUSTIFIED.
*      SPLIT total_pal_real_aux AT ',' INTO parte_inteira parte_decimal.
*      CLEAR total_pal_real_aux.
*      MOVE parte_inteira TO total_pal_real_aux.
*      IF total_pal_real_aux = '0'.
*        CLEAR : total_pal_real_aux.
*        MOVE '-' TO total_pal_real_aux+1(1).
*      ENDIF.
*
*
*      CLEAR : parte_inteira, parte_decimal.
*      total_pal_teorica = quantidade_teo1 + quantidade_teo2 + quantidade_teo3 +
*                          quantidade_teo4.
*      WRITE total_pal_teorica TO total_pal_teo_aux LEFT-JUSTIFIED.
*      SPLIT total_pal_teo_aux AT ',' INTO parte_inteira parte_decimal.
*      CLEAR total_pal_teo_aux.
*      MOVE parte_inteira TO total_pal_teo_aux.
*      IF total_pal_teo_aux = '0'.
*        CLEAR : total_pal_teo_aux.
*        MOVE '-' TO total_pal_teo_aux+1(1).
*      ENDIF.
*
*      CALL FUNCTION 'WRITE_FORM'
*        EXPORTING
*          element                  = 'PALETES'
*          window                   = 'MAIN'
*        EXCEPTIONS
*          element                  = 1
*          function                 = 2
*          type                     = 3
*          unopened                 = 4
*          unstarted                = 5
*          window                   = 6
*          bad_pageformat_for_print = 7
*          spool_error              = 8
*          OTHERS                   = 9.
*
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*** Janela para totais de paletes
*      CALL FUNCTION 'WRITE_FORM'
*        EXPORTING
*          window                   = 'TOTALPAL'
*        EXCEPTIONS
*          element                  = 1
*          function                 = 2
*          type                     = 3
*          unopened                 = 4
*          unstarted                = 5
*          window                   = 6
*          bad_pageformat_for_print = 7
*          spool_error              = 8
*          OTHERS                   = 9.
*
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*    ENDAT.

  ENDLOOP.

  CALL FUNCTION 'END_FORM'
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      spool_error              = 3
      OTHERS                   = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

** fecho do formulario
  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      send_error               = 3
      spool_error              = 4
      OTHERS                   = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " IMPRIME_TALAO
*&---------------------------------------------------------------------*
*&      Form  converte_formato_interno
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->F_CAMPO  text
*      <--F_VAL  text
*----------------------------------------------------------------------*
FORM converte_formato_interno  USING    f_campo
                               CHANGING f_val.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = f_campo
    IMPORTING
      output = f_val.

ENDFORM.                    " converte_formato_interno
*&---------------------------------------------------------------------*
*&      Form  cria_ranges_paletes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_ranges_paletes .

  r_ebeln-low = '9999900010'.
  r_ebeln-high = '9999900090'.
  r_ebeln-option = 'BT'.
  r_ebeln-sign = 'I'.
  APPEND r_ebeln.

ENDFORM.                    " cria_ranges_paletes
