*&---------------------------------------------------------------------*
*&  Include           LZWMFUNC4F01                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  verifica_linha
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->F_SUBRC  text
*----------------------------------------------------------------------*
FORM  verifica_linha  USING    f_subrc
                      CHANGING f_cor.                       "AM20071207

  READ TABLE gt_zwm030 WITH KEY linha = gt_registo-linha.
  IF sy-subrc NE 0.
*   Linha não existe
    f_subrc = 10.
    EXIT.
  ENDIF.
  gs_reg-matnr = gt_zwm030-matnr.
  gs_reg-aufnr = gt_zwm030-aufnr.

* Ler descrição do material
  PERFORM ler_descricao USING gs_reg-matnr.

* Substitui tipo de palete se existir na tabela ZTIPOPALETE
  PERFORM check_tipo_palete CHANGING f_cor.       "AMALCATA 20071128.

ENDFORM.                    "verifica_linha

*&---------------------------------------------------------------------*
*&      Form  verifica_codigo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->F_SUBRC  text
*----------------------------------------------------------------------*
FORM verifica_codigo  USING    f_subrc
                      CHANGING p_out_producao.

  DATA: BEGIN OF itab_ean OCCURS 0,
          matnr LIKE mean-matnr,
        END OF itab_ean.

  CLEAR: mean, p_out_producao, gs_reg-matnr.
*  SELECT SINGLE * FROM mean WHERE ean11 = gt_registo-codigo.
  SELECT * FROM mean
  INTO CORRESPONDING FIELDS OF TABLE itab_ean
  WHERE ean11 = gt_registo-codigo.

  IF sy-subrc NE 0.
*   Código inválido
    f_subrc = 20.
    EXIT.
  ENDIF.

** Para cada Material que possua este EAN, deve listar as Ordens
** de produção abertas
  CLEAR w_num.
  LOOP AT itab_ean.
    LOOP AT gt_zwm030 WHERE matnr = itab_ean-matnr.

** Guarda Ordem para utilizar no caso de só existir uma para o material
      gs_reg-matnr = gt_zwm030-matnr.
      gs_reg-aufnr = gt_zwm030-aufnr.

      PERFORM ler_descricao USING itab_ean-matnr.

      w_num = w_num + 1.

** Constroi variavel com as ordens de produção
      IF p_out_producao IS INITIAL.
        p_out_producao = gt_zwm030-aufnr.
      ELSE.
        CONCATENATE p_out_producao ';' gt_zwm030-aufnr
        INTO p_out_producao.
      ENDIF.

    ENDLOOP.
  ENDLOOP.

  IF gs_reg-matnr IS INITIAL.
    CLEAR: p_out_producao.
*     Material não existe
    f_subrc = 21.
  ELSEIF w_num > 1.
    CLEAR: gs_reg-matnr.
*     Material existe em mais do que uma ordem
    f_subrc = 22.
  ENDIF.

**  LOOP AT itab_ean.
**    CHECK p_out_producao IS INITIAL.
***    CLEAR: gs_reg-matnr.
***    gs_reg-matnr = itab_ean-matnr.
*** Ler descrição do material
**    PERFORM ler_descricao USING itab_ean-matnr.
*** Verifica se o material existe em várias ordens
**    CLEAR w_num.
**    LOOP AT gt_zwm030 WHERE matnr = itab_ean-matnr.
**      CLEAR: gs_reg-matnr.
**      gs_reg-matnr = gt_zwm030-matnr.
***   Guarda Ordem para utilizar no caso de só existir uma para o
*material
**      gs_reg-aufnr = gt_zwm030-aufnr.
**      w_num = w_num + 1.
**
**** Constroi variavel com as ordens de produção
**      IF p_out_producao IS INITIAL.
**        p_out_producao = gt_zwm030-aufnr.
**      ELSE.
**     CONCATENATE p_out_producao ';' gt_zwm030-aufnr INTO
*p_out_producao.
**      ENDIF.
**    ENDLOOP.
**  ENDLOOP.
*
**  IF w_num <> 1.
*  IF gs_reg-matnr IS INITIAL.
**    IF w_num = 0.
*    CLEAR: p_out_producao.
**     Material não existe
*    f_subrc = 21.
*  ELSEIF w_num > 1.
**     Material existe em mais do que uma ordem
*    f_subrc = 22.
**    ENDIF.
*  ENDIF.

ENDFORM.                    "verifica_codigo

*&--------------------------------------------------------------------*
*&      Form  ler_descricao
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_MATNR    text
*---------------------------------------------------------------------*
FORM ler_descricao USING f_matnr.

  CLEAR: makt, gs_reg-maktx, gs_reg-makt2, gs_reg-makt3.

  SELECT SINGLE * FROM makt WHERE spras = 'PT'
                              AND matnr = f_matnr.
  IF sy-subrc = 0.
    gs_reg-maktx = makt-maktx.
  ENDIF.

  SELECT SINGLE * FROM makt WHERE spras = 'S'
                              AND matnr = f_matnr.
  IF sy-subrc = 0.
    gs_reg-makt2 = makt-maktx.
  ENDIF.

  SELECT SINGLE * FROM makt WHERE spras = 'F'
                            AND matnr = f_matnr.
  IF sy-subrc = 0.
    gs_reg-makt3 = makt-maktx.
  ENDIF.

ENDFORM.                    "ler_descricao

*&--------------------------------------------------------------------*
*&      Form  verifica_altura
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_SUBRC    text
*---------------------------------------------------------------------*
FORM verifica_altura USING f_subrc
                     CHANGING f_altura.

  DATA: l_altura_min  LIKE marm-hoehe,
        l_altura_max  LIKE marm-hoehe,
        altura_min    LIKE marm-hoehe,
        altura_max    LIKE marm-hoehe,
        num_paletes   TYPE i,
        l_hoehe       LIKE marm-hoehe,
        l_meabm       LIKE marm-meabm,
        l_qtd_aux(15) TYPE c,
        l_conv        LIKE  vhuadmin-quantity,
        valor         LIKE zwm001-valor.

  CLEAR: mara, marm, mlgn, num_paletes, l_hoehe, l_meabm, l_qtd_aux,
         l_conv.

  CLEAR: marm.
  SELECT SINGLE * FROM marm WHERE matnr = gt_registo-cor
                              AND meinh = 'PAL'.
  IF sy-subrc NE 0.
*   Unidade PAL não atribuída à palete
    f_subrc = 30.
    EXIT.
  ELSE.
    l_hoehe = marm-hoehe.
    l_meabm = marm-meabm.

    l_qtd_aux = l_hoehe.
    l_conv = l_qtd_aux.

** Converter para MM
    CALL FUNCTION 'VHUMISC_CONVERT_TO_ALTERN_UNIT'
      EXPORTING
        matnr               = gt_registo-cor
        quantity_in_meins   = l_conv
        meins               = l_meabm
        unitqty             = 'MM'
      IMPORTING
        quantity_in_unitqty = l_conv
      EXCEPTIONS
        conversion_failed   = 1
        rounding_error      = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CLEAR: l_qtd_aux, l_hoehe.
    l_qtd_aux = l_conv.
    l_hoehe = l_qtd_aux.

  ENDIF.

** Paletes Remontadas
  SELECT SINGLE * FROM mlgn WHERE matnr = gs_reg-matnr
                              AND lgnum = gs_reg-lgnum.

  IF z_wm_cl_management=>is_remontada( is_data = mlgn ) EQ abap_true.
    num_paletes = 2.
    l_hoehe = l_hoehe * 2.
  ELSE.
    num_paletes = 1.
  ENDIF.

  CLEAR: marm.
  SELECT SINGLE * FROM marm WHERE matnr = gs_reg-matnr
                              AND meinh = 'PAL'.
  IF sy-subrc NE 0.
*   Unidade PAL não atribuída
    f_subrc = 30.
    EXIT.
  ENDIF.

* A altura é enviada em MM e inclui a altura da palete
** Alteração - No mestre de materiais a altura já inclui a da palete.
** Logo não é necessário subtrair - 30-03-2004

*  gt_registo-altura = gt_registo-altura - l_hoehe.

  marm-hoehe = marm-hoehe * num_paletes.

  CLEAR: f_altura.
  f_altura = marm-hoehe.

  PERFORM get_zwm001 TABLES itab_zwm001
                       USING gs_reg-lgnum
                             'ENTRADA_PRODUCAO'
                             'MAX'
                       CHANGING valor.
  MOVE valor TO altura_max.

  PERFORM get_zwm001 TABLES itab_zwm001
                     USING gs_reg-lgnum
                           'ENTRADA_PRODUCAO'
                           'MIN'
                     CHANGING valor.
  MOVE valor TO altura_min.

*  l_altura_min = marm-hoehe - 100.
*  l_altura_max = marm-hoehe + 50.
** Pedido de Alteração 30-03-2005 - Rui Marques
  l_altura_min = marm-hoehe - altura_min.
  l_altura_max = marm-hoehe + altura_max.
**

  IF gt_registo-altura GE l_altura_min AND
     gt_registo-altura LE l_altura_max.
    gs_reg-quantidade = marm-umrez.
  ELSE.
*   Altura inválida
    f_subrc = 31.
    EXIT.
  ENDIF.

* Ler MARA para extrair unidade base
  SELECT SINGLE * FROM mara WHERE matnr = gs_reg-matnr.
  CHECK sy-subrc = 0.
  gs_reg-unidade = mara-meins.

ENDFORM.                    "verifica_altura

*&--------------------------------------------------------------------*
*&      Form  verifica_quantidade
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_SUBRC    text
*---------------------------------------------------------------------*
FORM verifica_quantidade USING f_lgnum
                               f_lety1
                               f_subrc.

  CLEAR: mara, marm.
  SELECT SINGLE * FROM marm WHERE matnr = gs_reg-matnr
                              AND meinh = 'PAL'.
  IF sy-subrc NE 0.
*   Unidade PAL não atribuída
    f_subrc = 30.
    EXIT.
  ELSE.

** Verifica se para as paletes remontadas as paletes são com quantidades
** completas
    IF z_wm_cl_management=>is_remontada( i_lgnum = f_lgnum i_letyp = f_lety1 ) EQ abap_true.
      IF gt_registo-quantidade NE marm-umrez.
        f_subrc = 91.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gt_registo-quantidade > marm-umrez.
*   Quantidade inválida
    f_subrc = 40.
  ELSE.
    gs_reg-quantidade = gt_registo-quantidade.
  ENDIF.
* Ler MARA para extrair unidade base
  SELECT SINGLE * FROM mara WHERE matnr = gs_reg-matnr.
  CHECK sy-subrc = 0.
  gs_reg-unidade = mara-meins.
ENDFORM.                    "verifica_quantidade

*&--------------------------------------------------------------------*
*&      Form  verifica_cor
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_SUBRC    text
*---------------------------------------------------------------------*
FORM verifica_cor USING f_subrc.

  IF gt_registo-cor IS INITIAL.
*   Cor não especificada
    f_subrc = 50.
    EXIT.
  ENDIF.

  CLEAR mara.
  SELECT SINGLE * FROM mara WHERE matnr = gt_registo-cor.
  IF sy-subrc NE 0.
*   Côr inválida
    f_subrc = 51.
  ELSE.
    gs_reg-pack = mara-matnr.
  ENDIF.

ENDFORM.                    "verifica_cor

*&---------------------------------------------------------------------*
*&      Form  get_quantidade_completa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_quantidade_completa USING f_subrc.

  CLEAR: mara, marm.
  SELECT SINGLE * FROM marm WHERE matnr = gs_reg-matnr
                              AND meinh = 'PAL'.
  IF sy-subrc NE 0.
*   Unidade PAL não atribuída
    f_subrc = 30.
    EXIT.
  ENDIF.

  gs_reg-quantidade = marm-umrez.

* Ler MARA para extrair unidade base
  SELECT SINGLE * FROM mara WHERE matnr = gs_reg-matnr.
  CHECK sy-subrc = 0.
  gs_reg-unidade = mara-meins.

ENDFORM.                    " get_quantidade_completa

*&--------------------------------------------------------------------*
*&      Form  actualiza_zwm013
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM actualiza_zwm013.

  DATA: ls_zwm013 LIKE zwm013.

  SELECT SINGLE * FROM zwm013 WHERE armazem = gs_reg-lgnum
                                AND sscc    = gs_reg-hukey1.
  CHECK sy-subrc NE 0.

  CLEAR ls_zwm013.

  ls_zwm013-armazem     = gs_reg-lgnum.
  ls_zwm013-sscc        = gs_reg-hukey1.
  ls_zwm013-bloqueado   = 'X'.
  ls_zwm013-tipo_palete = mlgn-lety1.

  INSERT zwm013 FROM ls_zwm013.

ENDFORM.                    "actualiza_zwm013

*&--------------------------------------------------------------------*
*&      Form  actualiza_zwm020
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM actualiza_zwm020.

  DATA: ls_zwm020 LIKE zwm020.

  SELECT SINGLE * FROM zwm020 WHERE armazem = gs_reg-lgnum
                                AND p1      = gs_reg-hukey1
                                AND p2      = gs_reg-hukey2.
  CHECK sy-subrc NE 0.

  CLEAR ls_zwm020.

  ls_zwm020-armazem = gs_reg-lgnum.
  ls_zwm020-p1      = gs_reg-hukey1.
  ls_zwm020-p2      = gs_reg-hukey2.

  INSERT zwm020 FROM ls_zwm020.

ENDFORM.                    "actualiza_zwm020

*&--------------------------------------------------------------------*
*&      Form  regista_entrada
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_MODO     text
*      -->F_SUBRC    text
*---------------------------------------------------------------------*
FORM regista_entrada USING f_modo f_subrc.

  DATA: lt_items    LIKE zwm018   OCCURS 0 WITH HEADER LINE.

  DATA: l_mov_mm    TYPE bwlvs.

  DATA: l_quant LIKE vepo-vemng.

  DATA: mesa(14).

  lt_items-armazem    = gs_reg-lgnum.
  lt_items-material   = gs_reg-matnr.
  lt_items-quantidade = gs_reg-quantidade.
  lt_items-uni        = gs_reg-unidade.
  lt_items-lote       = gs_reg-charg.
*  lt_items-ebeln      =
*  lt_items-ebelp      =
*  lt_items-cliente    =
  APPEND lt_items.

  l_mov_mm = gs_reg-bwart.

  REFRESH return_msg.

  l_quant = gs_reg-quantidade.

  CALL FUNCTION 'ZWM_GET_MESA'
    EXPORTING
      matnr      = gs_reg-matnr
      quantidade = l_quant
*     quantidade = gs_reg-quantidade
    IMPORTING
      mesa       = mesa.

  CLEAR: l_porta.
  l_porta = mesa+4(10).

  CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
    EXPORTING
      lgnum            = gs_reg-lgnum
      aufnr            = gs_reg-aufnr
      code             = gs_reg-code
      porta            = l_porta
      mov_mm           = l_mov_mm
      testrun          = f_modo
    IMPORTING
      materialdocument = gs_reg-mblnr
      matdocumentyear  = gs_reg-mjahr
    TABLES
      return_msg       = return_msg
      items            = lt_items
    EXCEPTIONS
      error            = 1
      OTHERS           = 2.

  f_subrc = sy-subrc.

ENDFORM.                    "regista_entrada

*&--------------------------------------------------------------------*
*&      Form  cria_sscc
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->F_SUBRC    text
*---------------------------------------------------------------------*
FORM cria_sscc USING f_subrc sscc.

  DATA lt_items LIKE zwm_items_hu OCCURS 0 WITH HEADER LINE.

  DATA: ls_vekp TYPE vekp.

  CLEAR g_hukey.

  IF NOT sscc IS INITIAL.
    SELECT SINGLE * FROM vekp
                    INTO ls_vekp
                    WHERE exidv = sscc.
    IF sy-subrc EQ 0.
      g_hukey = sscc.
      EXIT.
    ENDIF.
  ENDIF.

  CLEAR lt_items.
*  lt_items-ltak_vbeln  =
*  lt_items-ltap_posnr  =
  lt_items-material    = gs_reg-matnr.
  lt_items-quantity    = gs_reg-quantidade.
  lt_items-unit        = gs_reg-unidade.
  lt_items-batch       = gs_reg-charg.
  COLLECT lt_items.

  REFRESH return_msg.

  CALL FUNCTION 'ZWM_CREATE_HU'
    EXPORTING
      warehouse                  = gs_reg-lgnum
      plant                      = gs_reg-werks
      s_loc                      = 'CD'
      packing_material           = gs_reg-pack
      hu                         = sscc
    IMPORTING
      hukey                      = g_hukey
    TABLES
      return_msg                 = return_msg
      items                      = lt_items
    EXCEPTIONS
      empty_table                = 1
      reference_document_differs = 2
      empty_delivery_item        = 3
      item_not_found             = 4
      OTHERS                     = 5.
  f_subrc = sy-subrc.

  IF f_subrc <> 0.
    f_subrc = 90.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSEIF g_hukey IS INITIAL.
    f_subrc = 90.
  ENDIF.

ENDFORM.                    "cria_sscc
*&---------------------------------------------------------------------*
*&      Form  regista_entrada_armazem
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->F_MODO   text
*      -->F_SUBRC  text
*----------------------------------------------------------------------*
FORM regista_entrada_armazem  USING    f_modo
                                       f_subrc
                                       f_lety
                              CHANGING f_msg.

  DATA: lt_items LIKE zwm018 OCCURS 0 WITH HEADER LINE.

  DATA: l_mov_mm TYPE bwlvs,
        valor    LIKE zwm001-valor,
        sto_loc  TYPE lgort_d.

  DATA: l_quant LIKE vepo-vemng.

  DATA: mesa(14).

  FREE return_msg.
  CLEAR: lt_items, return_msg, f_msg.

  lt_items-armazem    = gs_reg-lgnum.
  lt_items-material   = gs_reg-matnr.

  IF z_wm_cl_management=>is_remontada( i_lgnum = gs_reg-lgnum i_letyp = f_lety ) EQ abap_true.
    lt_items-quantidade = gs_reg-quantidade * 2.
  ELSE.
    lt_items-quantidade = gs_reg-quantidade.
  ENDIF.
  lt_items-uni        = gs_reg-unidade.
  lt_items-lote       = gs_reg-charg.
  APPEND lt_items.

  l_mov_mm = gs_reg-bwart.

  l_quant = gs_reg-quantidade.

  CALL FUNCTION 'ZWM_GET_MESA'
    EXPORTING
      matnr      = gs_reg-matnr
      quantidade = l_quant
    IMPORTING
      mesa       = mesa.

  CLEAR: l_porta.
  l_porta = mesa+4(10).

** Obter centro
  PERFORM get_zwm001 TABLES itab_zwm001
                     USING '100'
                           'GERAL'
                           'LGORT_BA'
                     CHANGING valor.
  MOVE valor TO sto_loc.

  CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
    EXPORTING
      lgnum            = gs_reg-lgnum
      aufnr            = gs_reg-aufnr
      code             = gs_reg-code
      porta            = l_porta
      mov_mm           = l_mov_mm
      testrun          = f_modo
      plant_o          = 'RENV'
      sloc_o           = sto_loc
    IMPORTING
      materialdocument = gs_reg-mblnr
      matdocumentyear  = gs_reg-mjahr
    TABLES
      return_msg       = return_msg
      items            = lt_items
    EXCEPTIONS
      error            = 1
      OTHERS           = 2.

  f_subrc = sy-subrc.

  IF NOT return_msg[] IS INITIAL.
    READ TABLE return_msg INDEX 1.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = return_msg-msgid
        msgnr               = return_msg-msgnr
        msgv1               = return_msg-msgv1
        msgv2               = return_msg-msgv2
        msgv3               = return_msg-msgv3
        msgv4               = return_msg-msgv4
      IMPORTING
        message_text_output = f_msg.
  ENDIF.

ENDFORM.                    " regista_entrada_armazem

*&---------------------------------------------------------------------*
*&      Form  get_zwm001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROCESSO  text
*      -->P_PARAMETRO text
*      <--P_VALOR     text
*----------------------------------------------------------------------*
FORM get_zwm001  TABLES   itab_zwm001 STRUCTURE zwm001
                 USING    p_lgnum
                          p_processo
                          p_parametro
                 CHANGING p_valor.

  CLEAR: p_valor.

  READ TABLE itab_zwm001 WITH KEY armazem   = p_lgnum
                                  processo  = p_processo
                                  parametro = p_parametro
                         BINARY SEARCH.

  CHECK sy-subrc EQ 0.
  p_valor = itab_zwm001-valor.

ENDFORM.                    " get_zwm001

*&---------------------------------------------------------------------*
*&      Form  transferencia_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ITAB_SSCC    text
*      -->ITAB_ZWM001  text
*      -->F_LGNUM      text
*      -->F_TIPO       text
*----------------------------------------------------------------------*
FORM transferencia_stock  TABLES itab_sscc STRUCTURE zwm_sscc
                            itab_zwm001 STRUCTURE zwm001
                     USING  f_lgnum
                            f_mesa
                            f_linha
                            f_tipo
                            f_estorno
                     CHANGING f_subrc
                              f_msg
                              l_mblnr
                              l_mjahr.

  WAIT UP TO 3 SECONDS.

  DATA: items LIKE zwm018 OCCURS 0 WITH HEADER LINE.
*
  DATA: l_mov_mm   TYPE bwlvs,
        l_code     TYPE bapi2017_gm_code,
        l_lgort_ba TYPE lgort_d,
        l_plant    TYPE werks_d,
*        l_mblnr    LIKE mseg-mblnr,
*        l_mjahr    LIKE mseg-mjahr,
        l_mesa     TYPE ablad,
        valor      LIKE zwm001-valor.

  FREE: items, return_msg.
  CLEAR: f_subrc, f_msg, items, return_msg.

  l_mesa = f_mesa.

** Movimento de material
  IF f_tipo = 'X'.
    PERFORM get_zwm001 TABLES itab_zwm001
                       USING f_lgnum
                             'ENTRADA_PRODUCAO'
                             'MOV_PAL'
                       CHANGING valor.
  ELSE.
*    IF f_estorno = 'X'.
*
*      PERFORM get_zwm001 TABLES itab_zwm001
*                         USING f_lgnum
*                               'ENTRADA_PRODUCAO'
*                               'MOV_T_EST'
*                         CHANGING valor.
*    ELSE.
    PERFORM get_zwm001 TABLES itab_zwm001
                       USING f_lgnum
                             'ENTRADA_PRODUCAO'
                             'MOV_T'
                       CHANGING valor.
*    ENDIF.
  ENDIF.

  l_mov_mm = valor.

** CODE
  PERFORM get_zwm001 TABLES itab_zwm001
                     USING f_lgnum
                           'ENTRADA_PRODUCAO'
                           'CODE'
                     CHANGING valor.
  l_code = valor.

** ARMAZÉM
  PERFORM get_zwm001 TABLES itab_zwm001
                     USING f_lgnum
                           'GERAL'
                           'PLANT'
                     CHANGING valor.
  l_plant = valor.

** DEPÓSITO
  IF f_linha = 'GMA'. " Copacking
    PERFORM get_zwm001 TABLES itab_zwm001
                      USING f_lgnum
                            'GERAL'
                            'LGORTRETR'
                      CHANGING valor.
    l_lgort_ba = valor.

  ELSE.

    PERFORM get_zwm001 TABLES itab_zwm001
                       USING f_lgnum
                             'GERAL'
                             'LGORT_BA'
                       CHANGING valor.
    l_lgort_ba = valor.

    IF sy-tcode = 'ZWM088_A'.
      l_lgort_ba = 'A'.
    ENDIF.
  ENDIF.


** Criação da transferencia de material
** do armazem(BA) para o armazem(CD)

  LOOP AT itab_sscc.
    IF f_tipo = 'X'.
      items-material = itab_sscc-vhilm.
*    ADD 1 TO items-quantidade.
      items-quantidade = 1.
      items-uni = 'PAL'.
      COLLECT items.
      CLEAR items.
** Verificar se o material é gerido á semi-palete e fazer transferencia
** das semi-paletes.
** INI - Entrada de Meia Palete.
      SELECT SINGLE *
          FROM mlgn
              WHERE matnr = itab_sscc-material
                AND lgnum = f_lgnum
                AND ( block = '03' OR block = '04' OR
                      block = '05' OR block = '06').
      IF sy-subrc = 0.
        SELECT SINGLE *
            FROM zwm001
                WHERE armazem = f_lgnum AND
                      processo = 'MEIA-PALETE' AND
                      parametro = itab_sscc-vhilm.
        IF sy-subrc = 0.
          items-material = zwm001-valor.
          IF mlgn-block = '03' OR  mlgn-block = '04'.
            items-quantidade = 2.
          ELSEIF mlgn-block = '05' OR  mlgn-block = '06'.
            items-quantidade = 4.
          ENDIF.
          items-uni = 'PAL'.
          COLLECT items.
          CLEAR items.
        ENDIF.
      ENDIF.
** FIM - Entrada de Meia Palete.

** Verificar se o material é gerido à quarto-palete e fazer transferencia
** das semi-paletes.
** INI - Entrada de Quarto Palete.
      SELECT SINGLE *
               FROM mlgn
                   WHERE matnr = itab_sscc-material
                     AND lgnum = f_lgnum
                     AND ( block = '07' OR block = '08' ).
*                      or  block = '09' OR block = '10').
      IF sy-subrc = 0.
        SELECT SINGLE *
            FROM zwm001
                WHERE armazem = f_lgnum AND
                      processo = 'QUARTO-PALETE' AND
                      parametro = itab_sscc-vhilm.
        IF sy-subrc = 0.
          items-material = zwm001-valor.
          IF mlgn-block = '07' OR  mlgn-block = '08'.
            items-quantidade = 4.
*          ELSEIF mlgn-block = '09' OR  mlgn-block = '10'.
*            items-quantidade = 8.
          ENDIF.
          items-uni = 'PAL'.
          COLLECT items.
          CLEAR items.
        ENDIF.
      ENDIF.
** FIM - Entrada de Quarto Palete.

    ELSE.
      items-material = itab_sscc-material.
      items-quantidade = itab_sscc-quantidade.
      items-uni = itab_sscc-uni.
      items-lote = itab_sscc-lote_producao.
      COLLECT items.
      CLEAR items.
    ENDIF.

*    IF f_remontada = 'X'.
*      items-quantidade = items-quantidade * 2.
*    ENDIF.

  ENDLOOP.



  IF f_tipo EQ 'X'.
    CALL FUNCTION 'ZWM_ENTRADAS_PALETES_PRODUCAO'
      EXPORTING
        i_lgnum  = f_lgnum
        i_werks  = l_plant
        i_lgort  = l_lgort_ba
        it_items = items[].
  ENDIF.

  CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
    EXPORTING
      lgnum            = f_lgnum
      code             = l_code
      porta            = l_mesa
      mov_mm           = l_mov_mm
      testrun          = ' '
      plant_d          = l_plant
      plant_o          = l_plant
      sloc_o           = l_lgort_ba
    IMPORTING
      materialdocument = l_mblnr
      matdocumentyear  = l_mjahr
    TABLES
      return_msg       = return_msg
      items            = items
    EXCEPTIONS
      error            = 1
      OTHERS           = 2.

  IF sy-subrc NE 0.

    READ TABLE return_msg INDEX 1.

    CLEAR: f_msg.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = return_msg-msgid
        msgnr               = return_msg-msgnr
        msgv1               = return_msg-msgv1
        msgv2               = return_msg-msgv2
        msgv3               = return_msg-msgv3
        msgv4               = return_msg-msgv4
      IMPORTING
        message_text_output = f_msg.

*    IF f_estorno = 'X'.
*      f_subrc = 97.
*    ELSE.
    IF f_tipo = 'X'.
      f_subrc = 98.
    ELSE.
      f_subrc = 99.
    ENDIF.
  ENDIF.
*  ENDIF.

ENDFORM.                    " transferencia_stock
*&---------------------------------------------------------------------*
*&      Form  estorno_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->F_MBLNR  text
*      -->F_MJAHR  text
*      <--F_SUBRC  text
*      <--F_MSG  text
*----------------------------------------------------------------------*
FORM estorno_stock  USING    f_mblnr
                             f_mjahr
                    CHANGING f_subrc
                             f_msg
                             x_mblnr
                             x_mjahr.

  DATA: l_doc LIKE bapi2017_gm_head_ret.

  WAIT UP TO 3 SECONDS.

  FREE: return_msg.
  CLEAR: f_subrc, f_msg, return_msg.

  CALL FUNCTION 'ZWM_ESTORNA_DOC_MATERIAL'
    EXPORTING
      mblnr       = f_mblnr
      mjahr       = f_mjahr
    IMPORTING
      documento   = l_doc
    TABLES
      return      = return_msg
    EXCEPTIONS
      erro_commit = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    f_subrc = '97'.
  ELSE.
    x_mblnr = l_doc-mat_doc.
    x_mjahr = l_doc-doc_year.
  ENDIF.

ENDFORM.                    " estorno_stock
*&---------------------------------------------------------------------*
*&      Form  check_lote
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->F_SUBRC  text
*----------------------------------------------------------------------*
FORM check_lote  USING    f_matnr
                          f_lgnum
                          f_charg
                          f_subrc
                          f_msg.

  FREE: return_msg.
  CLEAR: return_msg, f_msg.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = f_charg
    IMPORTING
      output = f_charg.

  CALL FUNCTION 'ZWM_BATCH_CREATE'
    EXPORTING
      armazem           = f_lgnum
      material          = f_matnr
      lote              = f_charg
      centro            = 'RENV'
    TABLES
      return_msg        = return_msg
    EXCEPTIONS
      erro              = 1
      batch_not_created = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    f_subrc = '96'.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF NOT return_msg[] IS INITIAL.
    READ TABLE return_msg INDEX 1.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = return_msg-msgid
        msgnr               = return_msg-msgnr
        msgv1               = return_msg-msgv1
        msgv2               = return_msg-msgv2
        msgv3               = return_msg-msgv3
        msgv4               = return_msg-msgv4
      IMPORTING
        message_text_output = f_msg.
  ENDIF.

ENDFORM.                    " check_lote

*&---------------------------------------------------------------------*
*&      Form  EAN128_ENCODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ean128_encode USING    p_hukey
                   CHANGING ean128_ca
                            ean128_cb
                            ean128_cc
                            ean128_ta
                            ean128_tb
                            ean128_tc.

  DATA: wa_values            TYPE ean128,
        ean128               TYPE ean128,
        l_profile            TYPE ean128_profile,
        l_devicetype         TYPE t313k-devicetype,
        l_linhas             LIKE sy-tabix,
        barcode              TYPE barcode_flat,
        cs_ean128_data-eanbc TYPE barcode_plus_errors_t,
        cs_ean128_data-eanrd TYPE barcode_txt_t,
        wa_eanrd             TYPE barcode_txt,
        wa_eanbc             TYPE barcode_plus_errors.

  CLEAR: wa_values, ean128, cs_ean128_data-eanbc, cs_ean128_data-eanrd,
         ean128_ca, ean128_cb, ean128_cc,
         ean128_ta, ean128_tb, ean128_tc.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_hukey
    IMPORTING
      output = wa_values-exidv.

** 2015.03.04 - paulo Sousa - Ignorar DUN PALETE nas etiquetas
** Obter o Ean
*  SELECT SINGLE ean11 FROM marm
*           INTO wa_values-matean
*          WHERE matnr = gs_reg-matnr
*            AND meinh = 'PAL'.

*  IF wa_values-matean IS INITIAL.
  SELECT SINGLE ean11 FROM mara
           INTO wa_values-matean
          WHERE matnr EQ gs_reg-matnr.
*  ENDIF.
** 2015.03.04 - paulo Sousa - Ignorar DUN PALETE nas etiquetas - FIM

** Tipo de etiqueta
  wa_values-lbltype = 'S'.

  wa_values-vhilm = gs_reg-pack.
  wa_values-vfdat = gs_reg-vfdat.
  wa_values-maktx = gs_reg-maktx.
  wa_values-matnr = gs_reg-matnr.
  wa_values-charg = gs_reg-charg.
  wa_values-lfimg = gs_reg-quantidade.
  wa_values-vrkme = gs_reg-unidade.
  wa_values-vemng = gs_reg-quantidade.
  wa_values-vemeh = gs_reg-unidade.

  MOVE wa_values TO ean128.

** Tipo de Perfil
  l_profile = '000'.
** Device Type
  l_devicetype = 'ZLB_ZEB'.

  CALL FUNCTION 'LE_EAN128_ENCODE'
    EXPORTING
      if_encode_profile         = l_profile
      if_skip_empty_fields      = 'X'
      is_ean128_data            = ean128
      if_devicetype             = l_devicetype
    IMPORTING
      et_barcode                = cs_ean128_data-eanbc
      et_barcode_txt            = cs_ean128_data-eanrd
    EXCEPTIONS
      no_barcode_definition     = 1
      error_in_subfunctions     = 2
      profile_not_for_ean128    = 3
      profile_unknown           = 4
      data_error                = 5
      no_ai_relation            = 6
      no_ai_information         = 7
      profile_error             = 8
      general_customizing_error = 9
      OTHERS                    = 10.

  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT cs_ean128_data-eanbc INTO wa_eanbc.
    CASE sy-tabix.
      WHEN 1.
        ean128_ca = wa_eanbc-barcode.
      WHEN 2.
        ean128_cb = wa_eanbc-barcode.
      WHEN 3.
        ean128_cc = wa_eanbc-barcode.
      WHEN OTHERS.
        EXIT.
    ENDCASE.
  ENDLOOP.

  LOOP AT cs_ean128_data-eanrd INTO wa_eanrd.
    CASE sy-tabix.
      WHEN 1.
        ean128_ta = wa_eanrd.
      WHEN 2.
        ean128_tb = wa_eanrd.
      WHEN 3.
        ean128_tc = wa_eanrd.
      WHEN OTHERS.
        EXIT.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " EAN128_ENCODE

*&---------------------------------------------------------------------*
*&      Form  msg_log_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM msg_log_display USING p_fase
                            p_msgtyp
                            p_msgno
                            p_msgv1
                            p_msgv2
                            p_msgv3
                            p_msgv4 .

  IF p_fase EQ 'I'.
** Inicializa mensagem de Log
    CALL FUNCTION 'ZWM_LOG_INICIALIZACAO'
      EXPORTING
        object        = 'ZWM001'
        subobject     = 'ZWM001'
        inicializacao = 'X'
        cabecalho     = 'X'
*     TABLES
*       return_msg    =
      EXCEPTIONS
        error         = 1
        OTHERS        = 2.
  ENDIF.

  IF p_fase EQ 'M'.
** Escreve a mensagem de Log
    CALL FUNCTION 'ZWM_LOG_MENSAGEM'
      EXPORTING
        object    = 'ZWM001'
        subobject = 'ZWM001'
        msgtyp    = p_msgtyp
        msgid     = 'ZWMMSG001'
        msgno     = p_msgno
        msgv1     = p_msgv1
        msgv2     = p_msgv2
        msgv3     = p_msgv3
        msgv4     = p_msgv4
*       MSGDT     = 1
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.
  ENDIF.

  IF p_fase EQ 'F'.
** Finaliza mensagem de Log
    CALL FUNCTION 'ZWM_LOG_FINALIZACAO'
      EXPORTING
        object    = 'ZWM001'
        subobject = 'ZWM001'
*     TABLES
*       return_msg =
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.
  ENDIF.

ENDFORM.                    " msg_log_display
*&---------------------------------------------------------------------*
*&      Form  check_tipo_palete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_tipo_palete CHANGING p_cor.

  DATA: lv_cor   LIKE ztipopalete-cor,
        lv_matnr LIKE mara-matnr.


* Limita utilização a paetizador central
* Paulo Sousa 2018.04.24
  IF wa_log-posto = 'AUTO_CENTRAL' OR
    wa_log-posto = 'MAN1_PROD' OR
    wa_log-posto = 'MAN2_PROD'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gt_zwm030-matnr
      IMPORTING
        output = lv_matnr.

* Caso exista uma entrada na tabela de parametrizacao ZTIPOPALETE
* substitui o tipo de palete pela indicada na tabela
    SELECT SINGLE cor INTO lv_cor
               FROM ztipopalete
               WHERE matnr = lv_matnr
                 AND fevor = gt_registo-linha.
    IF sy-subrc = 0 AND lv_cor <> ''.
      gt_registo-cor = lv_cor.
      wa_log-cor = lv_cor.
* Altera a string de entrada para exportação
      p_cor = lv_cor+0(6).
    ENDIF.
  ENDIF.
ENDFORM.                    " check_tipo_palete
*&---------------------------------------------------------------------*
*&      Form  CHECK_MANIPULADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_REG_MATNR  text
*      -->P_GT_REGISTO_CODIGO  text
*      -->P_0303   text
*      <--P_MAT_MANIPULADO  text
*----------------------------------------------------------------------*
FORM check_manipulado  USING    p_matnr TYPE matnr
                                p_ean
                                p_lgnum
                       CHANGING p_mat_manipulado.
  DATA: lt_matnr  TYPE TABLE OF matnr,
        lt_zwm067 TYPE TABLE OF zwm067,
        wa_zwm067 TYPE zwm067,
        wa_matnr  TYPE matnr.

  " verifica se existe manipulado para codigo ou ean
  REFRESH lt_zwm067.
  CLEAR: lt_zwm067, p_mat_manipulado, wa_zwm067.

  IF p_matnr IS NOT INITIAL.
    SELECT SINGLE matnr INTO wa_zwm067-matnr FROM zwm067 WHERE matnr = p_matnr
                                                   AND ( datuv <= sy-datum
                                                   AND datbe >= sy-datum ).


  ELSEIF p_ean IS NOT INITIAL.
    SELECT matnr FROM mean INTO TABLE lt_matnr WHERE ean11 = p_ean.

    LOOP AT lt_matnr INTO wa_matnr.
      SELECT * FROM zwm067 APPENDING TABLE lt_zwm067 WHERE lgnum = p_lgnum
                         AND matnr = wa_matnr
         AND ( datuv <= sy-datum AND datbe >= sy-datum )
        AND inactive <> 'X'
        AND deleted <> 'X'.
    ENDLOOP.

    READ TABLE lt_zwm067 INTO wa_zwm067 INDEX 1.
  ENDIF.
  p_mat_manipulado = wa_zwm067-matnr.
ENDFORM.                    " CHECK_MANIPULADO
*&---------------------------------------------------------------------*
*&      Form  GET_EAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_REG_MATNR  text
*      -->P_GT_REGISTO_CODIGO  text
*      <--P_EAN  text
*----------------------------------------------------------------------*
FORM get_ean  USING    p_matnr
                       p_codigo
              CHANGING p_ean11.

  DATA: BEGIN OF it_mean OCCURS 0,
          matnr LIKE mean-matnr,
          eantp LIKE mean-eantp,
          meinh LIKE mean-meinh,
          hpean LIKE mean-hpean,
          ean11 LIKE mean-ean11,
        END OF it_mean.

  REFRESH: it_mean.
  CLEAR:   it_mean, p_ean11.

  SELECT matnr eantp meinh hpean ean11 FROM mean
         INTO CORRESPONDING FIELDS OF TABLE it_mean
          WHERE matnr EQ p_matnr.

  SORT it_mean BY matnr eantp meinh hpean ean11.

  READ TABLE it_mean WITH KEY matnr = p_matnr
                              eantp = 'IC'
                              meinh = 'PAL'
                              hpean = 'X'.
** 2015.03.04 - paulo sousa********
  IF sy-subrc <> 0.
    READ TABLE it_mean WITH KEY matnr = p_matnr
                                eantp = 'IC'
                                meinh = 'pal'
                                hpean = 'X'.
  ENDIF.
** 2015.03.04 - paulo sousa******** FIM
  IF sy-subrc EQ 0.
    p_ean11 = it_mean-ean11.
  ELSE.
    READ TABLE it_mean WITH KEY matnr = gs_reg-matnr
                                eantp = 'IC'
                                hpean = 'X'
                                ean11 = p_codigo.
    IF sy-subrc EQ 0.
      p_ean11 = it_mean-ean11.
    ELSE.
      READ TABLE it_mean WITH KEY matnr = p_matnr
                                  eantp = 'IC'
                                  hpean = 'X'.
      IF sy-subrc EQ 0.
        p_ean11 = it_mean-ean11.
      ELSE.
        READ TABLE it_mean WITH KEY matnr = p_matnr
                                    eantp = 'HE'
                                    hpean = 'X'.
        IF sy-subrc EQ 0.
          p_ean11 = it_mean-ean11.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.                    " GET_EAN
