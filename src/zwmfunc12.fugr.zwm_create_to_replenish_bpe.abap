FUNCTION zwm_create_to_replenish_bpe .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_PROCESS) TYPE  XFELD
*"  EXPORTING
*"     REFERENCE(E_NPAL_BPE) TYPE  I
*"     REFERENCE(E_TPAL_BPE) TYPE  I
*"     REFERENCE(E_PAL_TOT) TYPE  I
*"  TABLES
*"      T_NT STRUCTURE  ZWM_011 OPTIONAL
*"      T_NT_P STRUCTURE  ZWM_011 OPTIONAL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF st_t311.
           INCLUDE STRUCTURE t311.
           TYPES: tbpri TYPE tbpri.
  TYPES: END OF st_t311.

  DATA: lv_pal      TYPE i.
  DATA: lv_pal_max  TYPE i.
  DATA: lv_pal_nt   TYPE i.
  DATA: lv_cap_max  TYPE i.
  DATA: lv_valor    TYPE zwm_valor.
  DATA: lv_count    TYPE i.
  DATA: lv_pal_tot  TYPE i.
  DATA: lv_msg      TYPE char10.
  DATA: lv_tanum    TYPE tanum.
  DATA: lv_lines    TYPE i.
  DATA: lv_tabix    TYPE sy-tabix.
  DATA: lv_lgpla    TYPE lgpla.
  DATA: lv_lgtyp    TYPE lgtyp.
  DATA: lv_queue    TYPE lrf_queue.
  DATA: lv_kunnr    TYPE kunnr.
  DATA: lv_vbeln    TYPE vbeln.

  DATA: ls_lagp     TYPE lagp.
  DATA: ls_kna1     TYPE kna1.
  DATA: ls_trite    TYPE l03b_trite.

  DATA: lt_lqua     TYPE TABLE OF lqua    WITH HEADER LINE.
  DATA: lt_ltbk     TYPE TABLE OF ltbk    WITH HEADER LINE.
  DATA: lt_ltbk_all TYPE TABLE OF ltbk    WITH HEADER LINE.
  DATA: lt_ltbp     TYPE TABLE OF ltbp    WITH HEADER LINE.
  DATA: lt_ltak     TYPE TABLE OF ltak    WITH HEADER LINE.
  DATA: lt_ltak_trf TYPE TABLE OF ltak    WITH HEADER LINE.
  DATA: lt_ltap     TYPE TABLE OF ltap    WITH HEADER LINE.
  DATA: lt_ltap_bpe TYPE TABLE OF ltap    WITH HEADER LINE.
  DATA: lt_ltap_epe TYPE TABLE OF ltap    WITH HEADER LINE.
  DATA: lt_t311     TYPE TABLE OF st_t311 WITH HEADER LINE.
  DATA: lt_makt     TYPE TABLE OF makt    WITH HEADER LINE.
  DATA: lt_marm     TYPE TABLE OF marm    WITH HEADER LINE.
  DATA: lt_trite    TYPE l03b_trite_t.

** Parâmetros
**********************************************************************
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'PALETIZACAO_ESPECIAL'
      i_parametro = 'MAX_PAL_BPE_NT'
    IMPORTING
      e_valor     = lv_valor
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc = 0.
    lv_pal_nt = lv_valor.
  ENDIF.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'PALETIZACAO_ESPECIAL'
      i_parametro = 'CAP_MAX_PAL_BPE'
    IMPORTING
      e_valor     = lv_valor
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc = 0.
    lv_cap_max = lv_valor.
  ENDIF.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'PALETIZACAO_ESPECIAL'
      i_parametro = 'TIPO_DEPOSITO'
    IMPORTING
      e_valor     = lv_valor
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc = 0.
    lv_lgtyp = lv_valor.
  ENDIF.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'PALETIZACAO_ESPECIAL'
      i_parametro = 'POSICAO'
    IMPORTING
      e_valor     = lv_valor
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc = 0.
    lv_lgpla = lv_valor.
  ENDIF.

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

** Selecção de dados
**********************************************************************

** NTs
  SELECT *
    FROM ltbk INTO TABLE lt_ltbk
    WHERE lgnum = i_lgnum
    AND   statu <> 'E'
    AND   betyp = 'S'.

  IF lt_ltbk[] IS NOT INITIAL.
    SELECT *
      FROM ltbp INTO TABLE lt_ltbp
      FOR ALL ENTRIES IN lt_ltbk
      WHERE lgnum = lt_ltbk-lgnum
      AND   tbnum = lt_ltbk-tbnum.
  ENDIF.

** Grupos
  IF lt_ltbk[] IS NOT INITIAL.
    SELECT *
      FROM t311 INTO TABLE lt_t311
      FOR ALL ENTRIES IN lt_ltbk
      WHERE lgnum = lt_ltbk-lgnum
      AND   refnr = lt_ltbk-benum.
  ENDIF.

** Materiais
  IF lt_ltbp[] IS NOT INITIAL.
    SELECT *
      FROM makt INTO TABLE lt_makt
      FOR ALL ENTRIES IN lt_ltbp
      WHERE matnr = lt_ltbp-matnr
      AND   spras = sy-langu.
  ENDIF.

** Paletização
  IF lt_ltbp[] IS NOT INITIAL.
    SELECT *
      FROM marm INTO TABLE lt_marm
      FOR ALL ENTRIES IN lt_ltbp
      WHERE matnr = lt_ltbp-matnr
      AND   meinh = 'PAL'.
  ENDIF.

** OTs
  IF lt_t311[] IS NOT INITIAL.
    SELECT *
      FROM ltbk INTO TABLE lt_ltbk_all
      FOR ALL ENTRIES IN lt_t311
      WHERE lgnum = i_lgnum
      AND   betyp = 'S'
      AND   benum = lt_t311-refnr.
  ENDIF.

  IF lt_ltbk_all[] IS NOT INITIAL.
    SELECT *
      FROM ltak INTO TABLE lt_ltak
      FOR ALL ENTRIES IN lt_ltbk_all
      WHERE lgnum = lt_ltbk_all-lgnum
      AND   tbnum = lt_ltbk_all-tbnum.
  ENDIF.

  IF lt_ltak[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      FOR ALL ENTRIES IN lt_ltak
      WHERE lgnum = lt_ltak-lgnum
      AND   tanum = lt_ltap-tanum.

    DELETE lt_ltap WHERE vorga = 'ST' OR vorga = 'SL'. "Eliminar extornadas
  ENDIF.

  LOOP AT lt_ltak.
    lv_tabix = sy-tabix.

    READ TABLE lt_ltap WITH KEY tanum = lt_ltak-tanum.
    CHECK sy-subrc <> 0.

    DELETE lt_ltak INDEX lv_tabix.
  ENDLOOP.

  " Paletes no Buffer Paletização Especial
  lt_ltap_bpe[] = lt_ltap[].

  DELETE lt_ltap_bpe WHERE pquit IS INITIAL.
  DELETE lt_ltap_bpe WHERE vlenr IS INITIAL.

  IF lt_ltap_bpe[] IS NOT INITIAL.
    SELECT *
      FROM lqua INTO TABLE lt_lqua
      FOR ALL ENTRIES IN lt_ltap_bpe
       WHERE lgnum = lt_ltap_bpe-lgnum
       AND   lgtyp = lv_lgtyp
       AND   lgpla = lv_lgpla
       AND   lenum = lt_ltap_bpe-vlenr.
  ENDIF.

  " Paletes já enviadas para Estação Paletização Especial
  IF lt_ltap_bpe[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap_epe
      FOR ALL ENTRIES IN lt_ltap_bpe
      WHERE lgnum = lt_ltap_bpe-lgnum
      AND   vlenr = lt_ltap_bpe-vlenr.

    DELETE lt_ltap_epe WHERE vlpla <> 'BPE'.
    DELETE lt_ltap_epe WHERE nlpla <> 'EPE'.

    DELETE lt_ltap_epe WHERE vorga = 'ST' OR vorga = 'SL'.
  ENDIF.

** Definir Prioridade para NTs dos Grupos.
**********************************************************************

** Grupos com prioridade definida no cockpit
  LOOP AT lt_ltbk WHERE tbpri IS NOT INITIAL.

    lt_ltbk-tbpri = '7'.

    MODIFY lt_ltbk INDEX sy-tabix TRANSPORTING tbpri.

*    READ TABLE lt_t311 WITH KEY refnr = lt_ltbk-benum.
*    CHECK sy-subrc = 0.

*    lt_t311-tbpri = '7'.

*    MODIFY lt_t311 INDEX sy-tabix TRANSPORTING tbpri.
  ENDLOOP.

** Grupos com paletes no buffer de paletização especial
  LOOP AT lt_lqua.

    READ TABLE lt_ltap_epe WITH KEY vlenr = lt_lqua-lenum.
    CHECK sy-subrc <> 0.

    READ TABLE lt_ltap_bpe WITH KEY vlenr = lt_lqua-lenum.
    CHECK sy-subrc = 0.

    READ TABLE lt_ltak WITH KEY tanum = lt_ltap_bpe-tanum.
    CHECK sy-subrc = 0.

    READ TABLE lt_ltbk WITH KEY tbnum = lt_ltak-tbnum.
    IF sy-subrc = 0.
      lt_ltbk-tbpri = '8'.

      MODIFY lt_ltbk INDEX sy-tabix TRANSPORTING tbpri.
    ENDIF.

*    READ TABLE lt_t311 WITH KEY refnr = lt_ltbk_all-benum.
*    CHECK sy-subrc = 0.
*
*    CHECK lt_t311-tbpri IS INITIAL.
*
*    lt_t311-tbpri = '8'.
*
*    MODIFY lt_t311 INDEX sy-tabix TRANSPORTING tbpri.

  ENDLOOP.

** Grupos já a ser processados na estação
  LOOP AT lt_ltap_epe.

    READ TABLE lt_ltap_bpe WITH KEY vlenr = lt_ltap_epe-vlenr.
    CHECK sy-subrc = 0.

    READ TABLE lt_ltak WITH KEY tanum = lt_ltap_bpe-tanum.
    CHECK sy-subrc = 0.

    READ TABLE lt_ltbk WITH KEY tbnum = lt_ltak-tbnum.
    IF sy-subrc = 0.
      lt_ltbk-tbpri = '9'.

      MODIFY lt_ltbk INDEX sy-tabix TRANSPORTING tbpri.
    ENDIF.

*    CHECK sy-subrc = 0.
*
*    READ TABLE lt_t311 WITH KEY refnr = lt_ltbk_all-benum.
*    CHECK sy-subrc = 0.
*
*    lt_t311-tbpri = '9'.
*
*    MODIFY lt_t311 INDEX sy-tabix TRANSPORTING tbpri.
  ENDLOOP.

** Definir Prioridade.
*  LOOP AT lt_t311 WHERE tbpri IS NOT INITIAL.
*
*    LOOP AT lt_ltbk WHERE benum = lt_t311-refnr.
*
*      lt_ltbk-tbpri = lt_t311-tbpri.
*
*      MODIFY lt_ltbk INDEX sy-tabix TRANSPORTING tbpri.
*    ENDLOOP.
*  ENDLOOP.

** Validar stock no Buffer de Paletização Especial
**********************************************************************
  CLEAR lv_pal.

  SELECT SINGLE *
    FROM lagp INTO ls_lagp
    WHERE lgnum = i_lgnum
    AND   lgtyp = lv_lgtyp
    AND   lgpla = lv_lgpla.

  IF ls_lagp-anzle < lv_cap_max. "ls_lagp-maxle.

    lv_pal = lv_cap_max - ls_lagp-anzle.
*    lv_pal = ls_lagp-maxle - ls_lagp-anzle.

    " Descontar OT pendentes de paletes para BPE
    REFRESH: lt_ltak_trf.

    SELECT *
      FROM ltak INTO TABLE lt_ltak_trf
      WHERE lgnum = i_lgnum
      AND   kquit = ''
      AND   queue = 'QUEUEPE'.

*  DELETE lt_ltak_trf WHERE kquit = 'X'.

    DESCRIBE TABLE lt_ltak_trf LINES lv_lines.

    lv_pal = lv_pal - lv_lines.

** Validar numero máximo de paletes por NT no buffer
    SELECT *
      FROM lqua INTO TABLE lt_lqua
      WHERE lgnum = i_lgnum
      AND   lgtyp = lv_lgtyp
      AND   lgpla = lv_lgpla.

    SORT lt_lqua BY lenum.
  ENDIF.

  LOOP AT lt_ltbk.

    lv_tabix = sy-tabix.

    CLEAR lv_count.

    LOOP AT lt_ltak WHERE tbnum = lt_ltbk-tbnum.

      IF lt_ltak-kquit = 'X'.

        READ TABLE lt_ltap WITH KEY tanum = lt_ltak-tanum.
        CHECK sy-subrc = 0.

        READ TABLE lt_lqua WITH KEY lenum = lt_ltap-vlenr.
        CHECK sy-subrc = 0.
        lv_count = lv_count + 1.

      ELSE.
        lv_count = lv_count + 1.
      ENDIF.

    ENDLOOP.

    " só pode ocupar 70% do buffer
    IF lv_count = lv_pal_nt.
      DELETE lt_ltbk INDEX lv_tabix.
    ELSE.
      lt_ltbk-pkpos = lv_count.

      MODIFY lt_ltbk INDEX lv_tabix TRANSPORTING pkpos.
    ENDIF.

  ENDLOOP.

** Selecionar NTs para criação de OTs de reabastecimento
**********************************************************************
  e_npal_bpe = ls_lagp-anzle.
  e_tpal_bpe = lv_cap_max. "ls_lagp-maxle.

*  CHECK lv_pal > 0.

  SORT lt_ltbk BY tbpri DESCENDING statu DESCENDING bdatu bzeit.

  lv_pal_max = lv_pal.

  CLEAR lv_pal.

  SORT lt_marm BY matnr.
  SORT lt_makt BY matnr.

  LOOP AT lt_ltbk.

    READ TABLE lt_ltbp WITH KEY tbnum = lt_ltbk-tbnum.
    CHECK sy-subrc = 0.

    " Quantidade pendente da NT
    lt_ltbp-menge = lt_ltbp-menge - lt_ltbp-tamen.

    READ TABLE lt_marm WITH KEY matnr = lt_ltbp-matnr.

    READ TABLE lt_makt WITH KEY matnr = lt_ltbp-matnr.

    " Validar Cliente
    lv_kunnr = lt_ltbk-lznum.

    SELECT SINGLE *
      FROM kna1 INTO ls_kna1
      WHERE kunnr = lv_kunnr.

    IF sy-subrc <> 0.
      lv_vbeln = lt_ltbk-lznum.

      SELECT SINGLE kunnr
        FROM likp INTO lv_kunnr
        WHERE vbeln = lv_vbeln.
    ENDIF.

    CLEAR t_nt.
    t_nt-tbnum = lt_ltbk-tbnum.
    t_nt-tbpri = lt_ltbk-tbpri.
    t_nt-refnr = lt_ltbk-benum.
    t_nt-lznum = lt_ltbk-lznum.
    t_nt-kunnr = lv_kunnr.
    t_nt-matnr = lt_ltbp-matnr.
    t_nt-maktx = lt_makt-maktx.
    t_nt-menge = lt_ltbp-menge / lt_marm-umrez.
    t_nt-meins = 'PAL'.
    APPEND t_nt.

** Criar OTs de Reabastecimento
    WHILE lv_pal < lv_pal_max.

      CLEAR t_nt_p.
      t_nt_p-tbnum = lt_ltbk-tbnum.
      t_nt_p-tbpri = lt_ltbk-tbpri.
      t_nt_p-refnr = lt_ltbk-benum.
      t_nt_p-lznum = lt_ltbk-lznum.
      t_nt_p-kunnr = lv_kunnr.
      t_nt_p-matnr = lt_ltbp-matnr.
      t_nt_p-maktx = lt_makt-maktx.
      t_nt_p-menge = 1.
      t_nt_p-meins = 'PAL'.
      COLLECT t_nt_p.

      REFRESH: lt_trite.

      CLEAR ls_trite.
      ls_trite-tbpos = lt_ltbp-tbpos.
      ls_trite-anfme = lt_marm-umrez.
      ls_trite-altme = lt_ltbp-meins.
      APPEND ls_trite TO lt_trite.

      " Criar OT
      IF i_process = 'X'.
        CLEAR lv_tanum.

        CALL FUNCTION 'L_TO_CREATE_TR'
          EXPORTING
            i_lgnum       = i_lgnum
            i_tbnum       = lt_ltbp-tbnum
            i_teilk       = 'X'
            it_trite      = lt_trite
          IMPORTING
            e_tanum       = lv_tanum
          EXCEPTIONS
            error_message = 99.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO t_nt_p-error
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          RETURN.
        ENDIF.
      ENDIF.

      " Contador Paletes total
      lv_pal_tot = lv_pal_tot + 1.
      lv_pal     = lv_pal + 1.

      IF lv_pal = lv_pal_max.
        EXIT.
      ENDIF.

      " Contador paletes por NT
      lt_ltbk-pkpos = lt_ltbk-pkpos + 1.

      IF lt_ltbk-pkpos = lv_pal_nt.
        EXIT.
      ENDIF.

      " Desconta quantidade da OT criada de uma palete
      lt_ltbp-menge = lt_ltbp-menge - lt_marm-umrez.

      IF lt_ltbp-menge <= 0.
        EXIT.
      ENDIF.

    ENDWHILE.

*    IF lv_pal = lv_pal_max.
*      EXIT.
*    ENDIF.

  ENDLOOP.

  e_pal_tot = lv_pal_tot.

ENDFUNCTION.
