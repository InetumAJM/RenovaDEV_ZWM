FUNCTION zwm_get_work_epe.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_EPE) TYPE  LGPLA
*"     REFERENCE(I_SIM) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(ES_WORK_EPE) TYPE  ZWM_013
*"     REFERENCE(ET_PAL_EPE) TYPE  ZTT_ZWM_012
*"----------------------------------------------------------------------
  DATA: lv_lgpla    TYPE lgpla.
  DATA: lv_lgtyp    TYPE lgtyp.
  DATA: lv_menge    TYPE menge_d.
  DATA: lv_menge2   TYPE menge_d.
  DATA: lv_menge3   TYPE menge_d.
  DATA: lv_lines    TYPE i.
  DATA: lv_tabix    TYPE sy-tabix.
  DATA: lv_valor    TYPE zwm_valor.
  DATA: lv_queue    TYPE lrf_queue.
  DATA: lv_kunnr    TYPE kunnr.
  DATA: lv_pos      TYPE lgpla.
  DATA: lv_num      TYPE numc1.
  DATA: lv_equip    TYPE char20.
  DATA: lv_pal      TYPE numc2.
  DATA: lv_lznum    TYPE lznum.
  DATA: lv_vbeln    TYPE vbeln.

  DATA: ls_kna1     TYPE kna1.
  DATA: ls_ltbk     TYPE ltbk.
  DATA: ls_ltbp     TYPE ltbp.
  DATA: ls_makt     TYPE makt.
  DATA: ls_t311     TYPE t311.
  DATA: ls_zwm031   TYPE zwm031.
  DATA: ls_zwm011   TYPE zwm011.
  DATA: ls_pal_epe  TYPE zwm_012.
  DATA: ls_pal_epe2 TYPE zwm_012.

  DATA: lt_makt     TYPE TABLE OF makt WITH HEADER LINE.
  DATA: lt_lqua     TYPE TABLE OF lqua WITH HEADER LINE.
  DATA: lt_marm     TYPE TABLE OF marm WITH HEADER LINE.
  DATA: lt_ltap     TYPE TABLE OF ltap WITH HEADER LINE.
  DATA: lt_ltak     TYPE TABLE OF ltak WITH HEADER LINE.

** Parâmetros
**********************************************************************
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'GESTAO_FILAS'
      i_parametro = 'FILA_ABAST_BPE'
    IMPORTING
      e_valor     = lv_valor
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc = 0.
    lv_queue = lv_valor.
  ENDIF.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'PALETIZACAO_ESPECIAL'
      i_parametro = 'TIPO_DEP_ESTACAO'
    IMPORTING
      e_valor     = lv_valor
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc = 0.
    lv_lgtyp = lv_valor.
  ENDIF.

** Validar se estação tem trabalho pendente
**********************************************************************
  lv_equip = i_epe.

  IF i_sim IS NOT INITIAL.

    SELECT SINGLE *
      FROM zwm011 INTO ls_zwm011
      WHERE armazem     = i_lgnum
      AND   equipamento = lv_equip.

  ELSE.
    CALL FUNCTION 'ZWM_CREATE_WORK_EPE'
      EXPORTING
        i_lgnum     = i_lgnum
        i_epe       = i_epe
      IMPORTING
        es_epe_work = ls_zwm011.
  ENDIF.

** Dados de Cabeçalho
**********************************************************************
  CHECK ls_zwm011 IS NOT INITIAL.

** NT
  SELECT SINGLE *
    FROM ltbk INTO ls_ltbk
    WHERE lgnum = ls_zwm011-armazem
    AND   tbnum = ls_zwm011-to_number.

  SELECT SINGLE *
    FROM ltbp INTO ls_ltbp
    WHERE lgnum = ls_zwm011-armazem
    AND   tbnum = ls_zwm011-to_number
    AND   tbpos = ls_zwm011-to_item.

** Grupo
  SELECT SINGLE *
    FROM t311 INTO ls_t311
    WHERE lgnum = ls_ltbk-lgnum
    AND   refnr = ls_ltbk-benum.

** Cliente e Remessa
  lv_kunnr = ls_ltbk-lznum.

  SELECT SINGLE *
    FROM kna1 INTO ls_kna1
    WHERE kunnr = lv_kunnr.

  IF sy-subrc <> 0.
    CLEAR lv_kunnr.

    lv_vbeln = ls_ltbk-lznum.

    SELECT SINGLE kunnr
      FROM likp INTO lv_kunnr
      WHERE vbeln = lv_vbeln.

    SELECT SINGLE *
      FROM kna1 INTO ls_kna1
      WHERE kunnr = lv_kunnr.
  ENDIF.

** Material
  SELECT SINGLE *
    FROM makt INTO ls_makt
    WHERE matnr = ls_ltbp-matnr
    AND   spras = sy-langu.

** Paletização Especial
  SELECT SINGLE *
    FROM zwm031 INTO ls_zwm031
    WHERE lgnum = ls_ltbk-lgnum
    AND   kunnr = ls_kna1-kunnr
    AND   matnr = ls_ltbp-matnr.

** Total de paletes
  IF sy-subrc = 0.
    ls_ltbp-menge = ls_ltbp-menge / ls_zwm031-unporpal.
    ls_ltbp-menge = ceil( ls_ltbp-menge ).
  ENDIF.

** Paletes finalizadas
  lv_lznum = ls_ltbk-tbnum.

  SELECT *
    FROM ltak INTO TABLE lt_ltak
    WHERE lgnum = ls_ltbk-lgnum
    AND   lznum = lv_lznum.

  IF lt_ltak[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      FOR ALL ENTRIES IN lt_ltak
      WHERE lgnum = lt_ltak-lgnum
      AND   tanum = lt_ltak-tanum.

    DELETE lt_ltap WHERE vltyp <> lv_lgtyp. "EPE
  ENDIF.

  LOOP AT lt_ltak.
    lv_tabix = sy-tabix.

    READ TABLE lt_ltap WITH KEY tanum = lt_ltak-tanum.
    CHECK sy-subrc <> 0.

    DELETE lt_ltak INDEX lv_tabix.
  ENDLOOP.

  DESCRIBE TABLE lt_ltak LINES lv_lines.

  CLEAR es_work_epe.
  es_work_epe-tbnum  = ls_ltbk-tbnum.
  es_work_epe-refnr  = ls_t311-refnr.
  es_work_epe-refnt  = ls_t311-refnt.
  es_work_epe-kunnr  = ls_kna1-kunnr.
  es_work_epe-name1  = ls_kna1-name1.
  es_work_epe-matnr  = ls_ltbp-matnr.
  es_work_epe-maktx  = ls_makt-maktx.
  es_work_epe-paltot = ls_ltbp-menge.
  es_work_epe-palfin = lv_lines.

** Dados de listagem das paletes
**********************************************************************
  lv_lgpla = i_epe.

** Validar stock na posição
  CONCATENATE lv_lgpla '%' INTO lv_lgpla.

  SELECT *
    FROM lqua INTO TABLE lt_lqua
    WHERE lgnum = i_lgnum
    AND   lgtyp = lv_lgtyp
    AND   lgpla LIKE lv_lgpla.

*  DELETE lt_lqua WHERE einme > 0. " Paletes Entrada
  DELETE lt_lqua WHERE verme <= 0.

  IF lt_lqua[] IS NOT INITIAL.
*    SELECT *
*      FROM ltap INTO TABLE lt_ltap
*      FOR ALL ENTRIES IN lt_lqua
*      WHERE lgnum = i_lgnum
*      AND   vlenr = lt_lqua-lenum.

    SELECT *
      FROM marm INTO TABLE lt_marm
      FOR ALL ENTRIES IN lt_lqua
      WHERE matnr = lt_lqua-matnr
      AND   meinh = 'PAL'.

    SELECT *
      FROM makt INTO TABLE lt_makt
      FOR ALL ENTRIES IN lt_lqua
      WHERE matnr = lt_lqua-matnr
      AND   spras = sy-langu.
  ENDIF.

  SORT lt_lqua BY edatu ezeit.

  LOOP AT lt_lqua.

    READ TABLE lt_makt WITH KEY matnr = lt_lqua-matnr.

    ls_pal_epe-lgpla  = lt_lqua-lgpla.
    ls_pal_epe-matnr  = lt_lqua-matnr.
    ls_pal_epe-maktx  = lt_makt-maktx.
    ls_pal_epe-qtdtot = lt_lqua-verme.
    ls_pal_epe-meins  = lt_lqua-meins.
    ls_pal_epe-lenum  = lt_lqua-lenum.

    COLLECT ls_pal_epe INTO et_pal_epe.
  ENDLOOP.

*  SORT et_pal_epe BY lgpla.

  lv_pal = es_work_epe-paltot - es_work_epe-palfin.

** Validar Status da palete de cada posição
**********************************************************************
  CLEAR lv_menge3.

*  SORT et_pal_epe BY qtdtot.

  LOOP AT et_pal_epe INTO ls_pal_epe.

    CLEAR: lv_menge, lv_menge2.

    lv_tabix = sy-tabix.

    SPLIT ls_pal_epe-lgpla AT '.' INTO lv_pos lv_num.

    CONCATENATE lv_pos '.2' INTO lv_pos.

    " Stock da Palete do meio
    READ TABLE et_pal_epe INTO ls_pal_epe2 WITH KEY lgpla = lv_pos.
    IF sy-subrc = 0.
      lv_menge2 = ls_pal_epe2-qtdtot.
    ENDIF.

    " Botão Transferência
    IF lv_pal > 1.

      IF lv_num = '1' OR lv_num = '3'.

        IF ls_pal_epe-qtdtot > ls_zwm031-unporpal.

          " Quantidade a transferir
          lv_menge = ls_pal_epe-qtdtot - ls_zwm031-unporpal.

          ls_pal_epe-qtdtrf = ls_zwm031-unporpal - lv_menge2 - lv_menge3.

          IF ls_pal_epe-qtdtrf <= 0.
            ls_pal_epe-qtdtrf = 0.
          ENDIF.

          IF ls_pal_epe-qtdtrf > lv_menge.
            ls_pal_epe-qtdtrf = lv_menge.
          ENDIF.

          IF lv_menge3 IS INITIAL.
            lv_menge3 = ls_pal_epe-qtdtrf.
          ENDIF.

          IF ls_pal_epe-qtdtrf > 0.
            ls_pal_epe-xtrf = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      " Botão Finalizar
      IF ls_pal_epe-qtdtot = ls_zwm031-unporpal.
        ls_pal_epe-xfin = 'X'.
      ENDIF.

    ELSE.
      " ultima palete da NT
      ls_pal_epe-xfin = 'X'.
    ENDIF.

    " Quantidade que fica na palete
    ls_pal_epe-qtdpal = ls_pal_epe-qtdtot - ls_pal_epe-qtdtrf.

    " Niveis
    IF ls_zwm031-lastro IS NOT INITIAL.
      ls_pal_epe-laytot = ls_pal_epe-qtdtot / ls_zwm031-lastro.
      ls_pal_epe-laytrf = ls_pal_epe-qtdtrf / ls_zwm031-lastro.
      ls_pal_epe-laypal = ls_pal_epe-qtdpal / ls_zwm031-lastro.
    ENDIF.

    MODIFY et_pal_epe FROM ls_pal_epe INDEX lv_tabix.
  ENDLOOP.

  SORT et_pal_epe BY lgpla.


ENDFUNCTION.
