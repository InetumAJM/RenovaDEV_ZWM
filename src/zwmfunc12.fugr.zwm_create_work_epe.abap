FUNCTION zwm_create_work_epe.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_EPE) TYPE  LGPLA
*"  EXPORTING
*"     REFERENCE(ES_EPE_WORK) TYPE  ZWM011
*"----------------------------------------------------------------------
  TYPES: BEGIN OF st_ltbk_bpe,
           tbnum   TYPE tbnum,
           qtd_bpe TYPE i,
           qtd_nt  TYPE i,
         END OF st_ltbk_bpe.

  DATA: lv_equip     TYPE char20.
  DATA: lv_lines     TYPE i.
  DATA: lv_pal       TYPE i.
  DATA: lv_tabix     TYPE sy-tabix.
  DATA: lv_valor     TYPE zwm_valor.
  DATA: lv_lgpla     TYPE lgpla.
  DATA: lv_lgpla_2   TYPE lgpla.
  DATA: lv_queue_bpe TYPE lrf_queue.
  DATA: lv_lgpla_bpe TYPE lgpla.
  DATA: lv_lgtyp     TYPE lgtyp.
  DATA: lv_lgtyp_bpe TYPE lgtyp.
  DATA: lv_tanum     TYPE tanum.
  DATA: lv_bwlvs     TYPE bwlvs.
  DATA: lv_lock_key  TYPE keyword-keyword.
  DATA: lv_benum     TYPE lvs_benum.

  DATA: ls_zwm011    TYPE zwm011.

  DATA: lt_lqua      TYPE TABLE OF lqua        WITH HEADER LINE.
  DATA: lt_lagp      TYPE TABLE OF lagp        WITH HEADER LINE.
  DATA: lt_lqua_pos  TYPE TABLE OF lqua        WITH HEADER LINE.
  DATA: lt_lqua_bpe  TYPE TABLE OF lqua        WITH HEADER LINE.
  DATA: lt_lqua_nt   TYPE TABLE OF lqua        WITH HEADER LINE.
  DATA: lt_ltap      TYPE TABLE OF ltap        WITH HEADER LINE.
  DATA: lt_ltak      TYPE TABLE OF ltak        WITH HEADER LINE.
  DATA: lt_ltak_all  TYPE TABLE OF ltak        WITH HEADER LINE.
  DATA: lt_ltbk      TYPE TABLE OF ltbk        WITH HEADER LINE.
  DATA: lt_ltbp      TYPE TABLE OF ltbp        WITH HEADER LINE.
  DATA: lt_ltbk_bpe  TYPE TABLE OF st_ltbk_bpe WITH HEADER LINE.
  DATA: lt_zwm011    TYPE TABLE OF zwm011      WITH HEADER LINE.

** Obter parametros
**********************************************************************
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
    lv_lgtyp_bpe = lv_valor.
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
    lv_lgpla_bpe = lv_valor.
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

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'PALETIZACAO_ESPECIAL'
      i_parametro = 'MOV_WM_TRF_EPE'
    IMPORTING
      e_valor     = lv_valor
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc = 0.
    lv_bwlvs = lv_valor.
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
    lv_queue_bpe = lv_valor.
  ENDIF.

** Validar trabalho de NT na estação
**********************************************************************
  lv_lock_key = 'CREATE_WORK_EPE'.

  DO 10 TIMES.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = lv_lock_key
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  lv_equip = i_epe.

  SELECT SINGLE *
    FROM zwm011 INTO ls_zwm011
    WHERE equipamento = lv_equip.

  lv_lgpla = lv_equip.

** Validar stock nas posições da Estação
  CONCATENATE lv_lgpla '%' INTO lv_lgpla.

  SELECT *
    FROM lqua INTO TABLE lt_lqua
    WHERE lgnum = i_lgnum
    AND   lgtyp = lv_lgtyp
    AND   lgpla LIKE lv_lgpla.

  DELETE lt_lqua WHERE ausme > 0 AND einme = 0. " Paletes de saída

  lt_lqua_pos[] = lt_lqua[].

  SORT lt_lqua_pos BY lgpla.
  DELETE ADJACENT DUPLICATES FROM lt_lqua_pos COMPARING lgpla.

  CONCATENATE i_epe '.2' INTO lv_lgpla_2.

  DELETE lt_lqua_pos WHERE lgpla = lv_lgpla_2. " Descartar Posição do meio

  DESCRIBE TABLE lt_lqua_pos LINES lv_lines.

** Nº Paletes possíveis para envio
  lv_pal = 2.
  lv_pal = lv_pal - lv_lines.

  IF lv_pal < 0.
    lv_pal = 0.
  ENDIF.

** Posições disponiveis
  SELECT *
    FROM lagp INTO TABLE lt_lagp
    WHERE lgnum = i_lgnum
    AND   lgtyp = lv_lgtyp
    AND   lgpla LIKE lv_lgpla.

  DELETE lt_lagp WHERE lgpla = lv_lgpla_2.

  LOOP AT lt_lagp.
    lv_tabix = sy-tabix.

    READ TABLE lt_lqua_pos WITH KEY lgpla = lt_lagp-lgpla.
    CHECK sy-subrc = 0.

    DELETE lt_lagp INDEX lv_tabix.

  ENDLOOP.

  SORT lt_lagp BY lgpla.

** Validar se existe Paletes no BPE para enviar para estação
**********************************************************************
  IF lv_pal > 0.

** Stock
    SELECT *
      FROM lqua INTO TABLE lt_lqua_bpe
      WHERE lgnum = i_lgnum
      AND   lgtyp = lv_lgtyp_bpe
      AND   lgpla = lv_lgpla_bpe.

    DELETE lt_lqua_bpe WHERE verme <= 0.

** Validar OTs
    IF lt_lqua_bpe[] IS NOT INITIAL.
      SELECT *
        FROM ltap INTO TABLE lt_ltap
        FOR ALL ENTRIES IN lt_lqua_bpe
        WHERE lgnum = i_lgnum
        AND   nlenr = lt_lqua_bpe-lenum.
    ENDIF.

    IF lt_ltap[] IS NOT INITIAL.
      SELECT *
        FROM ltak INTO TABLE lt_ltak
        FOR ALL ENTRIES IN lt_ltap
        WHERE lgnum = lt_ltap-lgnum
        AND   tanum = lt_ltap-tanum.
    ENDIF.

    DELETE lt_ltak WHERE tbnum IS INITIAL.

** Validar NTs
    IF lt_ltak[] IS NOT INITIAL.
      SELECT *
        FROM ltbk INTO TABLE lt_ltbk
        FOR ALL ENTRIES IN lt_ltak
        WHERE lgnum = lt_ltak-lgnum
        AND   tbnum = lt_ltak-tbnum.
    ENDIF.

    IF lt_ltbk[] IS NOT INITIAL.
      SELECT *
        FROM ltbp INTO TABLE lt_ltbp
        FOR ALL ENTRIES IN lt_ltbk
        WHERE lgnum = lt_ltbk-lgnum
        AND   tbnum = lt_ltbk-tbnum.

      SELECT *
        FROM ltak INTO TABLE lt_ltak_all
        FOR ALL ENTRIES IN lt_ltbk
        WHERE lgnum = lt_ltbk-lgnum
        AND   tbnum = lt_ltbk-tbnum.
    ENDIF.

    LOOP AT lt_ltap.
      lv_tabix = sy-tabix.

      READ TABLE lt_ltak WITH KEY tanum = lt_ltap-tanum.
      CHECK sy-subrc <> 0.

      DELETE lt_ltap INDEX lv_tabix.
    ENDLOOP.

** NTs associadas às Paletes
    LOOP AT lt_lqua_bpe.

      lv_tabix = sy-tabix.

      CLEAR: lt_lqua_bpe-tbnum.

      READ TABLE lt_ltap WITH KEY vlenr = lt_lqua_bpe-lenum.
      IF sy-subrc = 0.

        READ TABLE lt_ltak WITH KEY tanum = lt_ltap-tanum.
        IF sy-subrc = 0.
          lt_lqua_bpe-tbnum = lt_ltak-tbnum.
        ENDIF.
      ENDIF.

      MODIFY lt_lqua_bpe INDEX lv_tabix TRANSPORTING tbnum.
    ENDLOOP.

** Trabalho pendente de NT
    IF ls_zwm011 IS NOT INITIAL.
      DELETE lt_lqua_bpe WHERE tbnum <> ls_zwm011-to_number.

      SORT lt_lqua_bpe BY bdatu bzeit.

** Nova NT
    ELSE.

      IF lt_ltbk[] IS NOT INITIAL.
        SELECT *
          FROM zwm011 INTO TABLE lt_zwm011
          FOR ALL ENTRIES IN lt_ltbk
          WHERE armazem   = lt_ltbk-lgnum
          AND   to_number = lt_ltbk-tbnum.
      ENDIF.

      " Quantidade de paletes no buffer BPE por NT
      LOOP AT lt_ltbk.

        CLEAR lt_ltbk_bpe.

        lt_ltbk_bpe-tbnum = lt_ltbk-tbnum.

        IF lt_ltbk-statu = 'E'.
          LOOP AT lt_ltak_all WHERE tbnum = lt_ltbk-tbnum.
            lt_ltbk_bpe-qtd_nt = lt_ltbk_bpe-qtd_nt + 1.
          ENDLOOP.
        ENDIF.

        LOOP AT lt_lqua_bpe WHERE tbnum = lt_ltbk-tbnum.
          lt_ltbk_bpe-qtd_bpe = lt_ltbk_bpe-qtd_bpe + 1.
        ENDLOOP.

        APPEND lt_ltbk_bpe.
      ENDLOOP.

      " Dar prioridade a NT com todas as paletes no Buffer BPE
      LOOP AT lt_lqua_bpe.

        lv_tabix = sy-tabix.

        READ TABLE lt_zwm011 WITH KEY to_number = lt_lqua_bpe-tbnum.
        IF sy-subrc = 0.
          DELETE lt_lqua_bpe INDEX lv_tabix.
          CONTINUE.
        ENDIF.

        CLEAR lt_lqua_bpe-ivpos.

        READ TABLE lt_ltbk_bpe WITH KEY tbnum = lt_lqua_bpe-tbnum.
        IF sy-subrc = 0.
          IF lt_ltbk_bpe-qtd_nt = lt_ltbk_bpe-qtd_bpe AND lt_ltbk_bpe-qtd_nt > 0.

            CLEAR lt_ltbk.
            READ TABLE lt_ltbk WITH KEY tbnum = lt_lqua_bpe-tbnum.

            IF lt_ltbk-tbpri IS NOT INITIAL.
              lt_lqua_bpe-ivpos = '99'.
            ELSE.
              lt_lqua_bpe-ivpos = '98'.
            ENDIF.

          ELSE.
            lt_lqua_bpe-ivpos = lt_ltbk_bpe-qtd_bpe. " Com mais Paletes no BPE
          ENDIF.
        ENDIF.

        MODIFY lt_lqua_bpe INDEX lv_tabix TRANSPORTING ivpos.
      ENDLOOP.

      SORT lt_lqua_bpe BY ivpos DESCENDING bdatu bzeit.

      " Filtrar por NT
      READ TABLE lt_lqua_bpe INDEX 1.
      IF sy-subrc = 0.
        DELETE lt_lqua_bpe WHERE tbnum <> lt_lqua_bpe-tbnum.
      ENDIF.

    ENDIF.
  ENDIF.

** Obter trabalho
**********************************************************************
  DO lv_pal TIMES.

    READ TABLE lt_lqua_bpe INDEX 1.
    CHECK sy-subrc = 0.

    READ TABLE lt_lagp INDEX 1.
    CHECK sy-subrc = 0.

** Criar OT
    lv_benum = lt_lqua_bpe-tbnum.

    CLEAR lv_tanum.
    CALL FUNCTION 'L_TO_CREATE_SINGLE'
      EXPORTING
        i_lgnum       = i_lgnum
        i_werks       = lt_lqua_bpe-werks
        i_lgort       = lt_lqua_bpe-lgort
        i_benum       = lv_benum
        i_betyp       = 'C'
        i_bwlvs       = lv_bwlvs
        i_letyp       = lt_lqua_bpe-letyp
        i_matnr       = lt_lqua_bpe-matnr
        i_charg       = lt_lqua_bpe-charg
        i_anfme       = lt_lqua_bpe-verme
        i_altme       = lt_lqua_bpe-meins
*       i_vlenr       = lt_lqua_bpe-lenum
        i_vltyp       = lv_lgtyp_bpe
        i_vlpla       = lv_lgpla_bpe
*       i_nlenr       = lt_lqua_bpe-lenum
        i_nltyp       = lv_lgtyp
        i_nlpla       = lt_lagp-lgpla
      IMPORTING
        e_tanum       = lv_tanum
      EXCEPTIONS
        error_message = 99.

    " Associar trabalho da NT à Estação
    IF sy-subrc = 0 AND lv_tanum IS NOT INITIAL.

      DELETE lt_lqua_bpe INDEX 1.
      DELETE lt_lagp     INDEX 1.

      IF ls_zwm011 IS INITIAL.
        CLEAR ls_zwm011.
        ls_zwm011-armazem   = lt_lqua_bpe-lgnum.
        ls_zwm011-to_number = lt_lqua_bpe-tbnum.

        READ TABLE lt_ltbp WITH KEY tbnum = lt_lqua_bpe-tbnum.
        IF sy-subrc = 0.
          ls_zwm011-to_item = lt_ltbp-tbpos.
        ENDIF.

        ls_zwm011-user_name   = sy-uname.
        ls_zwm011-equipamento = lv_equip.

        MODIFY zwm011 FROM ls_zwm011.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.

    ELSE.
      EXIT.
    ENDIF.

  ENDDO.

  es_epe_work = ls_zwm011.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = lv_lock_key
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

ENDFUNCTION.
