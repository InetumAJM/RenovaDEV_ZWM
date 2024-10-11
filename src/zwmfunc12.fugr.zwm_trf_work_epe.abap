FUNCTION zwm_trf_work_epe.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_POS) TYPE  LGPLA
*"     REFERENCE(I_LENUM) TYPE  LENUM
*"  EXPORTING
*"     REFERENCE(E_TANUM) TYPE  TANUM
*"     REFERENCE(E_SUCESS) TYPE  FLAG
*"     REFERENCE(E_MSG) TYPE  ZWM_MSG
*"----------------------------------------------------------------------
  DATA: lv_valor      TYPE zwm_valor.
  DATA: lv_lgtyp      TYPE lgtyp.
  DATA: lv_lgpla      TYPE lgpla.
  DATA: lv_num        TYPE char5.
  DATA: lv_tanum      TYPE tanum.
  DATA: lv_dummy      TYPE char10.
  DATA: lv_menge      TYPE menge_d.
  DATA: ls_pal_epe    TYPE zwm_012.
  DATA: lt_lqua       TYPE TABLE OF lqua       WITH HEADER LINE.
  DATA: lt_lqua_2     TYPE TABLE OF lqua       WITH HEADER LINE.
  DATA: lt_ltap_creat TYPE TABLE OF ltap_creat WITH HEADER LINE.
  DATA: lt_pal_epe    TYPE ztt_zwm_012.

** Validar SU
**********************************************************************
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

  SELECT *
    FROM lqua INTO TABLE lt_lqua
    WHERE lgnum = i_lgnum
    AND   lgtyp = lv_lgtyp
    AND   lgpla = i_pos
    AND   lenum = i_lenum.

  DELETE lt_lqua WHERE verme <= 0.

  IF lt_lqua[] IS INITIAL.
    " Palete com SSCC & sem stock na posição &!
    MESSAGE ID 'ZWM001' TYPE 'E' NUMBER '125' INTO e_msg WITH i_lenum i_pos.
    EXIT.
  ENDIF.

** Validar se Palete pode ser transferida
  SPLIT i_pos AT '.' INTO lv_lgpla lv_dummy.

  CALL FUNCTION 'ZWM_GET_WORK_EPE'
    EXPORTING
      i_lgnum    = i_lgnum
      i_epe      = lv_lgpla
      i_sim      = 'X'
    IMPORTING
*     es_work_epe = ls_work_epe
      et_pal_epe = lt_pal_epe.

  READ TABLE lt_pal_epe WITH KEY lgpla = i_pos INTO ls_pal_epe.
  IF sy-subrc = 0.
    IF ls_pal_epe-xtrf IS INITIAL.
      " Palete com SSCC & sem status de transferir!
      MESSAGE ID 'ZWM001' TYPE 'E' NUMBER '131' INTO e_msg WITH i_lenum.
      EXIT.
    ENDIF.

    lv_menge = ls_pal_epe-qtdtrf.
  ENDIF.

** Criar OT trf
**********************************************************************
  LOOP AT lt_lqua.
    CLEAR lt_ltap_creat.
    lt_ltap_creat-werks = lt_lqua-werks.
    lt_ltap_creat-lgort = lt_lqua-lgort.
    lt_ltap_creat-matnr = lt_lqua-matnr.
    lt_ltap_creat-charg = lt_lqua-charg.
    lt_ltap_creat-anfme = lv_menge.
    lt_ltap_creat-altme = lt_lqua-meins.
    lt_ltap_creat-vltyp = lt_lqua-lgtyp.
    lt_ltap_creat-vlpla = lt_lqua-lgpla.
    lt_ltap_creat-vlenr = lt_lqua-lenum.
    lt_ltap_creat-letyp = lt_lqua-letyp.
    lt_ltap_creat-nltyp = lv_lgtyp.

    " Estação - Posição do meio
    SPLIT lt_lqua-lgpla AT '.' INTO lv_lgpla lv_num.

    lv_num = '2'.

    CONCATENATE lv_lgpla lv_num INTO lv_lgpla SEPARATED BY '.'.

    lt_ltap_creat-nlpla = lv_lgpla.

    SELECT *
      FROM lqua INTO TABLE lt_lqua_2
      WHERE lgnum = i_lgnum
      AND   lgtyp = lv_lgtyp
      AND   lgpla = lv_lgpla.

    READ TABLE lt_lqua_2 INDEX 1.
    IF sy-subrc = 0.
      lt_ltap_creat-nlenr = lt_lqua_2-lenum.
    ENDIF.

*    lt_ltap_creat-nlenr = lv_hukey.

    APPEND lt_ltap_creat.
  ENDLOOP.

** Criar OT
  CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
    EXPORTING
      i_lgnum       = i_lgnum
      i_bwlvs       = '999'
    IMPORTING
      e_tanum       = lv_tanum
    TABLES
      t_ltap_creat  = lt_ltap_creat
    EXCEPTIONS
      error_message = 99.

  IF sy-subrc <> 0 OR lv_tanum IS INITIAL.
    " Erro na criação da OT de saída!
    MESSAGE ID 'ZWM001' TYPE 'E' NUMBER '127' INTO e_msg.
    EXIT.
  ENDIF.

  e_sucess = 'X'.
  e_tanum  = lv_tanum.

ENDFUNCTION.
