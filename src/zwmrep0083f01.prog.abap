*&---------------------------------------------------------------------*
*&  Include           ZWMREP0083F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_P_LGNUM
*&---------------------------------------------------------------------*
FORM check_p_lgnum .

  CHECK NOT p_lgnum IS INITIAL.

** Valida Armazém
  SELECT SINGLE * FROM t300
          WHERE lgnum EQ p_lgnum.

  IF sy-subrc <> 0.
**  217	Armazem & inválido!
    MESSAGE s217 DISPLAY LIKE 'E' WITH p_lgnum.
    gv_error = 'X'.
  ENDIF.
ENDFORM.                    " CHECK_P_LGNUM

*&---------------------------------------------------------------------*
*&      Form  INICIALIZACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inicializacao .
  GET PARAMETER ID 'LGN' FIELD p_lgnum.
ENDFORM.                    " INICIALIZACAO

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  IF s_refnr[] IS NOT INITIAL OR ( s_vbeln[] IS NOT INITIAL AND s_vfeln[] IS INITIAL ).
    PERFORM get_data_from_delivery.
  ELSEIF s_vbeln[] IS NOT INITIAL AND s_vfeln[] IS NOT INITIAL.
    PERFORM get_data_from_invoice.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_from_delivery.

  DATA: lt_t311a   LIKE t311a OCCURS 0 WITH HEADER LINE,
        lt_lips    LIKE lips  OCCURS 0 WITH HEADER LINE,
        lt_mara    LIKE mara  OCCURS 0 WITH HEADER LINE,
        lt_marm    LIKE marm  OCCURS 0 WITH HEADER LINE,
        lt_kna1    LIKE kna1  OCCURS 0 WITH HEADER LINE,
        lt_vttp    LIKE vttp  OCCURS 0 WITH HEADER LINE,
        lt_kna1_s  LIKE kna1 OCCURS 0 WITH HEADER LINE,
        lt_adrc    LIKE adrc OCCURS 0 WITH HEADER LINE,
        lt_adrc_s  LIKE adrc OCCURS 0 WITH HEADER LINE,
        lt_vbrk    LIKE vbrk OCCURS 0 WITH HEADER LINE,
        temp_dados LIKE gt_dados OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF lt_likp OCCURS 0,
          vbeln LIKE likp-vbeln,
          kunnr LIKE likp-kunnr,
          kunag LIKE likp-kunag,

* SD 27/02/2015
          vstel like likp-vstel,
          vkorg LIKE likp-VKORG,
* fim SD
        END OF lt_likp.

  DATA: BEGIN OF lt_vbfa OCCURS 0,
         vbelv TYPE vbeln_von,
         vbeln TYPE vbeln_nach,
       END OF lt_vbfa.

  DATA: BEGIN OF lt_marc OCCURS 0,
        matnr TYPE matnr,
        werks TYPE werks_d,
        stawn TYPE stawn,
      END OF lt_marc.

  DATA: lv_indice TYPE sytabix,
        wa_dados LIKE gt_dados.

  DATA: unidades type MARM-UMREN.

  CLEAR: lt_marc, lt_vbfa, lt_likp, lt_kna1_s, lt_kna1,
         lt_marm, lt_mara, lt_lips, lt_t311a, gt_dados,
         gt_dados_genericos, lt_vttp, lt_adrc, lt_adrc_s,
         lt_vbrk .

  REFRESH: lt_marc, lt_vbfa, lt_likp, lt_kna1_s, lt_kna1,
       lt_marm, lt_mara, lt_lips, lt_t311a, gt_dados, lt_vttp,
       lt_adrc, lt_adrc_s, lt_vbrk .

  IF s_refnr[] IS NOT INITIAL.
    SELECT * INTO TABLE lt_t311a FROM t311a
          WHERE lgnum EQ p_lgnum
            AND refnr IN s_refnr.

    SORT lt_t311a BY rbnum ASCENDING.

    CHECK NOT lt_t311a[] IS INITIAL.

    SELECT vbeln kunnr kunag vstel vkorg INTO TABLE lt_likp
           FROM likp
           FOR ALL ENTRIES IN lt_t311a
           WHERE vbeln EQ lt_t311a-rbnum.

  ELSEIF s_vbeln IS NOT INITIAL.

    SELECT vbeln kunnr kunag vstel vkorg INTO TABLE lt_likp
           FROM likp
           WHERE vbeln IN s_vbeln.
  ENDIF.

  CHECK NOT lt_likp[] IS INITIAL.

  SELECT vbelv vbeln INTO TABLE lt_vbfa
         FROM vbfa
         FOR ALL ENTRIES IN lt_likp
         WHERE vbelv EQ lt_likp-vbeln
           AND vbtyp_n EQ 'M'.

  SORT lt_likp.

  data: sytabix like sy-tabix.

  LOOP AT lt_likp.
    sytabix = sy-tabix.
    IF lt_likp-VKORG = 'RP12'.

      SELECT SINGLE adrnr INTO lt_kna1-adrnr  FROM vbpa WHERE vbeln = lt_likp-vbeln
      AND parvw = 'WE'
      AND posnr = '000000'.

    ELSE.

      SELECT SINGLE adrnr INTO lt_kna1-adrnr  FROM vbpa WHERE vbeln = lt_likp-vbeln
      AND parvw = 'W1'
      AND posnr = '000000'.

     ENDIF.
      sy-tabix = sytabix.

      MODIFY lt_likp INDEX sy-tabix.
    ENDLOOP.

*  SELECT * INTO TABLE lt_kna1
*         FROM kna1
*         FOR ALL ENTRIES IN lt_likp
*         WHERE kunnr EQ lt_likp-kunnr.
*
** Dados do Cliente
  READ TABLE lt_kna1 INDEX 1.

*
  SELECT * INTO TABLE lt_adrc FROM adrc
           WHERE addrnumber EQ lt_kna1-adrnr.
*
*

    IF sy-subrc EQ 0.
      READ TABLE lt_adrc INDEX 1.
      gt_dados_genericos-c_nome1 = lt_adrc-name1.
      gt_dados_genericos-c_nome2 = lt_adrc-name2.
      gt_dados_genericos-c_nome3 = lt_adrc-name3.
      gt_dados_genericos-c_nome4 = lt_adrc-name4.
      gt_dados_genericos-c_stras = lt_adrc-street.
      gt_dados_genericos-c_pstlz = lt_adrc-post_code1.
      gt_dados_genericos-c_ort01 = lt_adrc-city1.
    ELSE.
      gt_dados_genericos-c_nome1 = lt_kna1-name1.
      gt_dados_genericos-c_nome2 = lt_kna1-name2.
      gt_dados_genericos-c_nome3 = lt_kna1-name3.
      gt_dados_genericos-c_nome4 = lt_kna1-name4.
      gt_dados_genericos-c_stras = lt_kna1-stras.
      gt_dados_genericos-c_pstlz = lt_kna1-pstlz.
      gt_dados_genericos-c_ort01 = lt_kna1-ort01.
    ENDIF.

    SELECT SINGLE landx INTO gt_dados_genericos-c_landx
           FROM t005t
           WHERE spras EQ lt_kna1-spras
             AND land1 EQ lt_kna1-land1.

** SD 27/02/2015
*    ler o endereço do local de expedição que será o local
*     de saida da mercadoria

    data: xcountry like adrc-country,
          zz_adrnr like adrc-addrnumber.
    clear: xcountry, zz_adrnr.

    READ TABLE lt_likp INDEX 1.

    select single adrnr into zz_adrnr
                  from tvst where vstel = lt_likp-vstel.

    SELECT single name1 name2 street post_code1 city1 country
             into (gt_dados_genericos-s_nome1, gt_dados_genericos-s_nome2,
                    gt_dados_genericos-s_stras, gt_dados_genericos-s_pstlz,
                    gt_dados_genericos-s_ort01, xcountry )
             FROM adrc
             WHERE addrnumber EQ zz_adrnr.

    SELECT * INTO TABLE lt_lips FROM lips
             FOR ALL ENTRIES IN lt_likp
             WHERE vbeln EQ lt_likp-vbeln.

    CLEAR: lt_lips.
    READ TABLE lt_lips INDEX 1.

*** Codigo excluido
*  SELECT SINGLE kunnr INTO t001w-kunnr
*         FROM t001w
*         WHERE werks EQ lt_lips-werks.
*
*  SELECT * INTO TABLE lt_kna1_s
*         FROM kna1
*         WHERE kunnr EQ t001w-kunnr.
*
** Dados Proprios
*  READ TABLE lt_kna1_s INDEX 1.
*  SELECT * INTO TABLE lt_adrc_s FROM adrc
*         WHERE addrnumber EQ lt_kna1_s-adrnr.
*  IF sy-subrc EQ 0.
*    READ TABLE lt_adrc_s INDEX 1.
*    gt_dados_genericos-s_nome1 = lt_adrc_s-name1.
*    gt_dados_genericos-s_nome2 = lt_adrc_s-name2.
*    gt_dados_genericos-s_stras = lt_adrc_s-street.
*    gt_dados_genericos-s_pstlz = lt_adrc_s-post_code1.
*    gt_dados_genericos-s_ort01 = lt_adrc_s-city1.
*  ELSE.
*    gt_dados_genericos-s_nome1 = lt_kna1_s-name1.
*    gt_dados_genericos-s_nome2 = lt_kna1_s-name2.
*    gt_dados_genericos-s_stras = lt_kna1_s-stras.
*    gt_dados_genericos-s_pstlz = lt_kna1_s-pstlz.
*    gt_dados_genericos-s_ort01 = lt_kna1_s-ort01.
*  ENDIF.


    SELECT SINGLE landx INTO gt_dados_genericos-s_landx
           FROM t005t
           WHERE spras EQ sy-langu
*           AND land1 EQ lt_kna1_s-land1.
             AND land1 EQ xcountry.

*** fim SD


* Transporte
    SELECT * INTO TABLE lt_vttp FROM vttp
             FOR ALL ENTRIES IN lt_likp
             WHERE vbeln EQ lt_likp-vbeln.
    IF sy-subrc EQ 0.
      READ TABLE lt_vttp INDEX 1.
      CLEAR vttk.
      SELECT SINGLE * FROM vttk
             WHERE tknum EQ lt_vttp-tknum.
      IF sy-subrc EQ 0.
        MOVE vttk-exti1 TO gt_dados_genericos-container.
      ENDIF.
    ENDIF.

* Dados da Factura
*  READ TABLE lt_vbfa INDEX 1.
*  SELECT SINGLE * FROM vbrk
*         WHERE vbeln EQ lt_vbfa-vbeln.

    SELECT * INTO TABLE lt_vbrk
      FROM vbrk
          FOR ALL ENTRIES IN lt_vbfa
              WHERE vbeln EQ lt_vbfa-vbeln
                AND fksto EQ ' '.
    READ TABLE lt_vbrk INDEX 1.

    gt_dados_genericos-factura = lt_vbrk-vbeln.
    gt_dados_genericos-data_factura = lt_vbrk-fkdat.
*  gt_dados_genericos-container = '.'.

    IF gt_dados_genericos-factura IS INITIAL.
      gt_dados_genericos-factura = '-----'.
    ENDIF.
    IF gt_dados_genericos-data_factura IS INITIAL.
      gt_dados_genericos-data_factura = '-----'.
    ENDIF.
    IF gt_dados_genericos-container IS INITIAL.
      gt_dados_genericos-container = '-----'.
    ENDIF.

    SORT lt_lips BY matnr ASCENDING werks ASCENDING.

    SELECT matnr werks stawn INTO TABLE lt_marc
           FROM marc
           FOR ALL ENTRIES IN lt_lips
           WHERE matnr EQ lt_lips-matnr
             AND werks EQ lt_lips-werks.

    SORT lt_lips BY matnr ASCENDING vrkme ASCENDING.

    SELECT * INTO TABLE lt_marm FROM marm
             FOR ALL ENTRIES IN lt_lips
             WHERE matnr EQ lt_lips-matnr
               AND meinh EQ lt_lips-vrkme.

    SELECT * INTO TABLE lt_mara FROM mara
             FOR ALL ENTRIES IN lt_lips
             WHERE matnr EQ lt_lips-matnr.

    SORT lt_mara BY matnr ASCENDING.

    DELETE lt_mara WHERE mtart EQ 'PALT'.


    LOOP AT lt_lips.
      CLEAR gt_dados.
      CLEAR: Unidades.
*########Adicionar total de unidades de consumo##################
      PERFORM get_total_unidades USING lt_lips-matnr Unidades.
      IF Unidades EQ 0.
        Unidades = 1.
      ENDIF.
      gt_dados-totunidades = lt_lips-lfimg * Unidades.
*###############################################################
      gt_dados-matnr = lt_lips-matnr.
      gt_dados-maktx = lt_lips-arktx.
      gt_dados-lfimg = lt_lips-lfimg.
      gt_dados-vrkme = lt_lips-vrkme.
      IF lt_lips-brgew IS INITIAL.
*      gt_dados-brgew = lt_lips-kcbrgew.
      ELSE.
        gt_dados-brgew = lt_lips-brgew.
      ENDIF.
      IF lt_lips-ntgew IS INITIAL.
*      gt_dados-ntgew = lt_lips-kcntgew.
      ELSE.
        gt_dados-ntgew = lt_lips-ntgew.
      ENDIF.
      IF lt_lips-volum IS INITIAL.
*      gt_dados-volum = lt_lips-kcvolum.
      ELSE.
        gt_dados-volum = lt_lips-volum.
      ENDIF.
      COLLECT gt_dados.
    ENDLOOP.


    CLEAR: wa_dados, temp_dados.
    REFRESH: temp_dados.


    LOOP AT gt_dados.
      lv_indice = sy-tabix.


      READ TABLE lt_mara WITH KEY matnr = gt_dados-matnr.
      IF sy-subrc NE 0.
        DELETE gt_dados INDEX lv_indice.
        CONTINUE.
      ENDIF.

      READ TABLE lt_marm WITH KEY matnr = gt_dados-matnr
                                  meinh = gt_dados-vrkme.
      IF sy-subrc EQ 0.
        gt_dados-laeng = lt_marm-laeng.
        gt_dados-breit = lt_marm-breit.
        gt_dados-hoehe = lt_marm-hoehe.
      ENDIF.

      SELECT SINGLE * FROM mara
             WHERE matnr EQ gt_dados-matnr.
      IF sy-subrc EQ 0 AND
         mara-mtart EQ 'SRVÇ'.
        CLEAR: gt_dados-stawn, gt_dados-ntgew,
               gt_dados-brgew, gt_dados-volum, wa_dados.

*     Garantir o Shipping & Handling no fim
        wa_dados = gt_dados.
        APPEND wa_dados TO temp_dados.
        DELETE gt_dados INDEX lv_indice.
        CONTINUE.
      ELSE.
        READ TABLE lt_marc WITH KEY matnr = gt_dados-matnr.
        IF sy-subrc EQ 0.
          gt_dados-stawn = lt_marc-stawn.
        ENDIF.

        "Total de unidade medida
        gt_dados_genericos-total_qtd_un = gt_dados_genericos-total_qtd_un + gt_dados-totunidades.

        gt_dados_genericos-total_qtd =
         gt_dados_genericos-total_qtd + gt_dados-lfimg.
      ENDIF.

      gt_dados_genericos-total_peso_net =
             gt_dados_genericos-total_peso_net + gt_dados-ntgew.
      gt_dados_genericos-total_peso_gross =
             gt_dados_genericos-total_peso_gross + gt_dados-brgew.
      gt_dados_genericos-total_volume =
             gt_dados_genericos-total_volume + gt_dados-volum.


      MODIFY gt_dados INDEX lv_indice.
    ENDLOOP.

* Garantir o Shipping & Handling no fim
    APPEND LINES OF temp_dados TO gt_dados.

  ENDFORM.                    " GET_DATA_FROM_DELIVERY
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_INVOICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_from_invoice.

  DATA: ls_ekko    TYPE ekko,
        lt_mara    LIKE mara     OCCURS 0 WITH HEADER LINE,
        lt_marm    LIKE marm     OCCURS 0 WITH HEADER LINE,
        lt_kna1    LIKE kna1     OCCURS 0 WITH HEADER LINE,
        lt_kna1_s  LIKE kna1     OCCURS 0 WITH HEADER LINE,
        lt_likp    LIKE likp     OCCURS 0 WITH HEADER LINE,
        lt_vttp    LIKE vttp     OCCURS 0 WITH HEADER LINE,
        lt_adrc    LIKE adrc     OCCURS 0 WITH HEADER LINE,
        lt_adrc_s  LIKE adrc     OCCURS 0 WITH HEADER LINE,
        lt_vbrk    LIKE vbrk     OCCURS 0 WITH HEADER LINE,
        lt_vbrp    LIKE vbrp     OCCURS 0 WITH HEADER LINE,
        lt_vbfa    LIKE vbfa     OCCURS 0 WITH HEADER LINE,
        temp_dados LIKE gt_dados OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF lt_marc OCCURS 0,
        matnr TYPE matnr,
        werks TYPE werks_d,
        stawn TYPE stawn,
      END OF lt_marc.

  DATA: lv_indice TYPE sytabix,
        wa_dados LIKE gt_dados.

  DATA: unidades type MARM-UMREN.

  CLEAR: lt_marc, lt_vbfa, lt_kna1, lt_kna1_s,
         lt_marm, lt_mara, gt_dados, gt_dados_genericos,
         lt_vttp, lt_adrc, lt_adrc_s, lt_vbrk, lt_vbrp.

  REFRESH: lt_marc, lt_vbfa, lt_kna1, lt_kna1_s,
           lt_marm, lt_mara,  gt_dados, lt_vttp,
           lt_adrc, lt_adrc_s, lt_vbrk.

** Obter dados da Factura
**********************************************************************
  IF s_vfeln[] IS NOT INITIAL.
    SELECT *
      FROM vbrk INTO TABLE lt_vbrk
      WHERE vbeln IN s_vfeln.
  ENDIF.

  CHECK NOT lt_vbrk[] IS INITIAL.

** Items
  SELECT *
    FROM vbrp INTO TABLE lt_vbrp
    FOR ALL ENTRIES IN lt_vbrk
    WHERE vbeln = lt_vbrk-vbeln.

** Dados do Cliente
  SELECT *
    FROM kna1 INTO TABLE lt_kna1
    FOR ALL ENTRIES IN lt_vbrk
    WHERE kunnr EQ lt_vbrk-kunrg.

  READ TABLE lt_kna1 INDEX 1.

** Morada
  SELECT * INTO TABLE lt_adrc FROM adrc
           WHERE addrnumber EQ lt_kna1-adrnr.
  IF sy-subrc EQ 0.
    READ TABLE lt_adrc INDEX 1.
    gt_dados_genericos-c_nome1 = lt_adrc-name1.
    gt_dados_genericos-c_nome2 = lt_adrc-name2.
    gt_dados_genericos-c_nome3 = lt_adrc-name3.
    gt_dados_genericos-c_nome4 = lt_adrc-name4.
    gt_dados_genericos-c_stras = lt_adrc-street.
    gt_dados_genericos-c_pstlz = lt_adrc-post_code1.
    gt_dados_genericos-c_ort01 = lt_adrc-city1.
  ELSE.
    gt_dados_genericos-c_nome1 = lt_kna1-name1.
    gt_dados_genericos-c_nome2 = lt_kna1-name2.
    gt_dados_genericos-c_nome3 = lt_kna1-name3.
    gt_dados_genericos-c_nome4 = lt_kna1-name4.
    gt_dados_genericos-c_stras = lt_kna1-stras.
    gt_dados_genericos-c_pstlz = lt_kna1-pstlz.
    gt_dados_genericos-c_ort01 = lt_kna1-ort01.
  ENDIF.

  SELECT SINGLE landx INTO gt_dados_genericos-c_landx
         FROM t005t
         WHERE spras EQ lt_kna1-spras
           AND land1 EQ lt_kna1-land1.

** Dados Origem
  SELECT *
    FROM likp INTO TABLE lt_likp
    WHERE vbeln IN s_vbeln.

  IF lt_likp[] IS NOT INITIAL.
    SELECT * INTO TABLE lt_kna1_s
           FROM kna1
           FOR ALL ENTRIES IN lt_likp
           WHERE kunnr EQ lt_likp-kunnr.
  ENDIF.

  READ TABLE lt_kna1_s INDEX 1.

  SELECT * INTO TABLE lt_adrc_s FROM adrc
         WHERE addrnumber EQ lt_kna1_s-adrnr.
  IF sy-subrc EQ 0.
    READ TABLE lt_adrc_s INDEX 1.
    gt_dados_genericos-s_nome1 = lt_adrc_s-name1.
    gt_dados_genericos-s_nome2 = lt_adrc_s-name2.
    gt_dados_genericos-s_stras = lt_adrc_s-street.
    gt_dados_genericos-s_pstlz = lt_adrc_s-post_code1.
    gt_dados_genericos-s_ort01 = lt_adrc_s-city1.
  ELSE.
    gt_dados_genericos-s_nome1 = lt_kna1_s-name1.
    gt_dados_genericos-s_nome2 = lt_kna1_s-name2.
    gt_dados_genericos-s_stras = lt_kna1_s-stras.
    gt_dados_genericos-s_pstlz = lt_kna1_s-pstlz.
    gt_dados_genericos-s_ort01 = lt_kna1_s-ort01.
  ENDIF.

  SELECT SINGLE landx INTO gt_dados_genericos-s_landx
         FROM t005t
         WHERE spras EQ sy-langu
           AND land1 EQ lt_kna1_s-land1.

** Transporte
  IF lt_likp[] IS NOT INITIAL.
    SELECT * INTO TABLE lt_vttp FROM vttp
             FOR ALL ENTRIES IN lt_likp
             WHERE vbeln EQ lt_likp-vbeln.

    IF sy-subrc EQ 0.
      READ TABLE lt_vttp INDEX 1.
      CLEAR vttk.
      SELECT SINGLE * FROM vttk
             WHERE tknum EQ lt_vttp-tknum.
      IF sy-subrc EQ 0.
        MOVE vttk-exti1 TO gt_dados_genericos-container.
      ENDIF.
    ENDIF.
  ENDIF.

** Dados da Factura
  READ TABLE lt_vbrk INDEX 1.

  gt_dados_genericos-factura      = lt_vbrk-vbeln.
  gt_dados_genericos-data_factura = lt_vbrk-fkdat.
*  gt_dados_genericos-container = '.'.

  IF gt_dados_genericos-factura IS INITIAL.
    gt_dados_genericos-factura = '-----'.
  ENDIF.
  IF gt_dados_genericos-data_factura IS INITIAL.
    gt_dados_genericos-data_factura = '-----'.
  ENDIF.
  IF gt_dados_genericos-container IS INITIAL.
    gt_dados_genericos-container = '-----'.
  ENDIF.

** Obter dados dos materiais
**********************************************************************
  SORT lt_vbrp BY matnr ASCENDING werks ASCENDING.

  SELECT matnr werks stawn INTO TABLE lt_marc
         FROM marc
         FOR ALL ENTRIES IN lt_vbrp
         WHERE matnr EQ lt_vbrp-matnr
           AND werks EQ lt_vbrp-werks.

  SORT lt_vbrp BY matnr ASCENDING vrkme ASCENDING.

  SELECT * INTO TABLE lt_marm FROM marm
           FOR ALL ENTRIES IN lt_vbrp
           WHERE matnr EQ lt_vbrp-matnr
             AND meinh EQ lt_vbrp-vrkme.

  SELECT * INTO TABLE lt_mara FROM mara
           FOR ALL ENTRIES IN lt_vbrp
           WHERE matnr EQ lt_vbrp-matnr.

  SORT lt_mara BY matnr ASCENDING.

  DELETE lt_mara WHERE mtart EQ 'PALT'.

** Passar dados
**********************************************************************
  SORT lt_vbrp by posnr DESCENDING. " ordenar materias
  LOOP AT lt_vbrp.
    CLEAR: gt_dados, Unidades.

*########Adicionar total de unidades de consumo##################
    PERFORM get_total_unidades USING lt_vbrp-matnr Unidades.
    IF Unidades eq 0.
      Unidades = 1.
    ENDIF.
    gt_dados-TOTUNIDADES = lt_vbrp-fkimg * Unidades.
*###############################################################
    gt_dados-matnr = lt_vbrp-matnr.
    gt_dados-maktx = lt_vbrp-arktx.
    gt_dados-lfimg = lt_vbrp-fkimg.
    gt_dados-vrkme = lt_vbrp-vrkme.
    IF lt_vbrp-brgew IS INITIAL.
*      gt_dados-brgew = lt_lips-kcbrgew.
    ELSE.
      gt_dados-brgew = lt_vbrp-brgew.
    ENDIF.
    IF lt_vbrp-ntgew IS INITIAL.
*      gt_dados-ntgew = lt_lips-kcntgew.
    ELSE.
      gt_dados-ntgew = lt_vbrp-ntgew.
    ENDIF.
    IF lt_vbrp-volum IS INITIAL.
*      gt_dados-volum = lt_lips-kcvolum.
    ELSE.
      gt_dados-volum = lt_vbrp-volum.
    ENDIF.
    COLLECT gt_dados.
  ENDLOOP.


  CLEAR: wa_dados, temp_dados.
  REFRESH: temp_dados.

  LOOP AT gt_dados.
    lv_indice = sy-tabix.

    READ TABLE lt_mara WITH KEY matnr = gt_dados-matnr.
    IF sy-subrc NE 0.
      DELETE gt_dados INDEX lv_indice.
      CONTINUE.
    ENDIF.

    READ TABLE lt_marm WITH KEY matnr = gt_dados-matnr
                                meinh = gt_dados-vrkme.
    IF sy-subrc EQ 0.
      gt_dados-laeng = lt_marm-laeng.
      gt_dados-breit = lt_marm-breit.
      gt_dados-hoehe = lt_marm-hoehe.
    ENDIF.

    SELECT SINGLE * FROM mara
           WHERE matnr EQ gt_dados-matnr.
    IF sy-subrc EQ 0 AND
       mara-mtart EQ 'SRVÇ'.
      CLEAR: gt_dados-stawn, gt_dados-ntgew,
             gt_dados-brgew, gt_dados-volum, wa_dados.

*     Garantir o Shipping & Handling no fim
      wa_dados = gt_dados.
      APPEND wa_dados TO temp_dados.
      DELETE gt_dados INDEX lv_indice.
      CONTINUE.
    ELSE.
      READ TABLE lt_marc WITH KEY matnr = gt_dados-matnr.
      IF sy-subrc EQ 0.
        gt_dados-stawn = lt_marc-stawn.
      ENDIF.

      gt_dados_genericos-total_qtd =
       gt_dados_genericos-total_qtd + gt_dados-lfimg.

      "Total de unidade medida
      gt_dados_genericos-total_qtd_un = gt_dados_genericos-total_qtd_un + gt_dados-totunidades.

    ENDIF.



    gt_dados_genericos-total_peso_net =
           gt_dados_genericos-total_peso_net + gt_dados-ntgew.
    gt_dados_genericos-total_peso_gross =
           gt_dados_genericos-total_peso_gross + gt_dados-brgew.
    gt_dados_genericos-total_volume =
           gt_dados_genericos-total_volume + gt_dados-volum.

    MODIFY gt_dados INDEX lv_indice.
  ENDLOOP.

* Garantir o Shipping & Handling no fim
  APPEND LINES OF temp_dados TO gt_dados.

ENDFORM.                    " GET_DATA_FROM_INVOICE
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_LISTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_lista .
  DATA: f_funcao_sf      TYPE rs38l_fnam,
        fs_control       LIKE ssfctrlop,
        fs_options       TYPE ssfcompop,
        fs_print_options LIKE zwm017,
        lv_user_settings TYPE tdsfflag.

  DATA: fname TYPE fpname,
        zburks LIKE vbrk-BUKRS,
        zvkorg LIKE VBRK-VKORG.

** Opções de Impressão
***********************************************************************
  fs_options-tdimmed = 'X'.
  fs_options-tdnewid = ''.

  IF p_imp IS INITIAL.
    lv_user_settings = 'X'.
  ELSE.
    fs_options-tddest = p_imp.
    lv_user_settings = ''.
  ENDIF.

**  Pede opções de impressão
  CALL FUNCTION 'SSF_SHOW_DIALOG'
   EXPORTING
*       ARCHIVE_PARAMETERS         =
       user_settings              = lv_user_settings
      output_options             = fs_options
      control_parameters         = fs_control
*       OK_BUTTON                  = ' '
   IMPORTING
*       E_ARCHIVE_PARAMETERS       =
      e_output_options           = fs_options
      e_control_parameters       = fs_control
    EXCEPTIONS
      formatting_error           = 1
      internal_error             = 2
      send_error                 = 3
      user_canceled              = 4
      OTHERS                     = 5.

  IF sy-subrc <> 0.
    IF NOT sy-msgid IS INITIAL AND NOT sy-msgno IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
         DISPLAY LIKE sy-msgty
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    EXIT.
  ENDIF.


  fs_control-no_dialog = 'X'.

  CLEAR: fname, zvkorg, zburks.

  "Formulario Default
  fname = 'ZWM_PACKING_LIST_RENV'.

  "##################### GET FORMULARIO ##########################
  SELECT SINGLE VKORG BUKRS INTO (zvkorg,zburks ) FROM VBRK
  WHERE VBELN = gt_dados_genericos-FACTURA.

  SELECT SINGLE form_pack_list INTO fname FROM zpdf_empresas
  WHERE bukrs = zburks
  AND vkorg = zvkorg.
  IF sy-subrc <> 0.
**     default por empresa
    SELECT SINGLE form_pack_list INTO fname FROM zpdf_empresas
    WHERE bukrs = zburks.
  ENDIF.


  "#############################################################




** Obtem Função do Smartform
***********************************************************************
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname             = fname
*     VARIANT              = ' '
*     DIRECT_CALL          = ' '
   IMPORTING
     fm_name               = f_funcao_sf
   EXCEPTIONS
     no_form               = 1
     no_function_module    = 2
     OTHERS                = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

** Executa o Smartform
***********************************************************************
* SMARTFORM: /1BCDWB/SF00000004


  CALL FUNCTION f_funcao_sf
 EXPORTING
*   ARCHIVE_INDEX              =
*   ARCHIVE_INDEX_TAB          =
*   ARCHIVE_PARAMETERS         =
     control_parameters         = fs_control
*   MAIL_APPL_OBJ              =
*   MAIL_RECIPIENT             =
*   MAIL_SENDER                =
     output_options             = fs_options
     user_settings              = ' '
     dados                      = gt_dados_genericos
* IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    TABLES
      dados_item                 = gt_dados
   EXCEPTIONS
     formatting_error           = 1
     internal_error             = 2
     send_error                 = 3
     user_canceled              = 4
     OTHERS                     = 5
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " IMPRIME_LISTA
*&---------------------------------------------------------------------*
*&      Form  CHECK_BLOCO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_bloco .

  IF s_refnr[] IS INITIAL AND s_vbeln[] IS INITIAL AND s_vfeln[] IS INITIAL.
*   obrigatorio preencher o grupo ou a remessa ou a factura.
    MESSAGE e273.

  ELSEIF s_vfeln[] IS NOT INITIAL AND s_vbeln[] IS INITIAL.
*   obrigatorio preencher Remessa.
    MESSAGE e282.
  ENDIF.
ENDFORM.                    " CHECK_BLOCO
*&---------------------------------------------------------------------*
*&      Form  GET_TOTAL_UNIDADES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_VBRP_MATNR  text
*      -->P_UNIDADES  text
*----------------------------------------------------------------------*
FORM GET_TOTAL_UNIDADES  USING    P_LT_VBRP_MATNR
                                  P_UNIDADES.
  CALL FUNCTION 'Z_GET_UNCONS_POR_UNEXPED_2'
    EXPORTING
      MATNR             = P_LT_VBRP_MATNR
   IMPORTING
*   EAN_HE            =
*   EAN_IC            =
     UNIDADES          = P_UNIDADES
*   NUMERADOR         =
*   DENOMINADOR       =
*   EMBCONSUMO        =
            .


ENDFORM.                    " GET_TOTAL_UNIDADES
