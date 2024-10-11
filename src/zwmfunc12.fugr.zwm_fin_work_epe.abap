FUNCTION zwm_fin_work_epe.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_POS) TYPE  LGPLA
*"     REFERENCE(I_LENUM) TYPE  LENUM
*"  EXPORTING
*"     REFERENCE(E_TANUM) TYPE  TANUM
*"     REFERENCE(E_WORK_FINISH) TYPE  FLAG
*"     REFERENCE(E_SUCESS) TYPE  FLAG
*"     REFERENCE(E_MSG) TYPE  ZWM_MSG
*"----------------------------------------------------------------------
  DATA: lv_lgtyp      TYPE lgtyp.
  DATA: lv_valor      TYPE zwm_valor.
  DATA: lv_vhilm      TYPE vhilm.
  DATA: lv_hukey      TYPE exidv.
  DATA: lv_bwlvs      TYPE bwlvs.
  DATA: lv_tanum      TYPE tanum.
  DATA: lv_refnr      TYPE ltak-refnr.
  DATA: lv_queue      TYPE ltak-queue.
  DATA: lv_lznum      TYPE lznum.
  DATA: lv_lines      TYPE i.
  DATA: lv_lgpla      TYPE lgpla.
  DATA: lv_dummy      TYPE char20.
  DATA: lv_tabix      TYPE sy-tabix.
  DATA: lv_pos        TYPE lgpla.
  DATA: lv_num        TYPE numc1.

  DATA: ls_ltbk       TYPE ltbk.
  DATA: ls_ltak       TYPE ltak.
  DATA: ls_work_epe   TYPE zwm_013.
  DATA: ls_pal_epe    TYPE zwm_012.
  DATA: ls_zwm011     TYPE zwm011.

  DATA: lt_lqua       TYPE TABLE OF lqua         WITH HEADER LINE.
  DATA: lt_return     TYPE TABLE OF bdcmsgcoll   WITH HEADER LINE.
  DATA: lt_items      TYPE TABLE OF zwm_items_hu WITH HEADER LINE.
  DATA: lt_ltap_creat TYPE TABLE OF ltap_creat   WITH HEADER LINE.
  DATA: lt_ltak       TYPE TABLE OF ltak         WITH HEADER LINE.
  DATA: lt_ltap       TYPE TABLE OF ltap         WITH HEADER LINE.
  DATA: lt_ltap_conf  TYPE TABLE OF ltap_conf    WITH HEADER LINE.

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
    AND   lgpla = i_pos.
*    AND   lenum = i_lenum.

  DELETE lt_lqua WHERE verme <= 0.

  READ TABLE lt_lqua WITH KEY lenum = i_lenum.
  IF sy-subrc <> 0.
    " Palete com SSCC & sem stock na posição &!
    MESSAGE ID 'ZWM001' TYPE 'E' NUMBER '125' INTO e_msg WITH i_lenum i_pos.
    EXIT.
  ENDIF.

** Validar se Palete pode ser finalizada
  SPLIT i_pos AT '.' INTO lv_lgpla lv_dummy.

  CALL FUNCTION 'ZWM_GET_WORK_EPE'
    EXPORTING
      i_lgnum    = i_lgnum
      i_epe      = lv_lgpla
      i_sim      = 'X'
    IMPORTING
      et_pal_epe = lt_pal_epe.

  READ TABLE lt_pal_epe INTO ls_pal_epe WITH KEY lgpla =  i_pos
                                                lenum = i_lenum.

  IF ls_pal_epe-xfin IS INITIAL.
    " Palete com SSCC & sem status de finalizada!
    MESSAGE ID 'ZWM001' TYPE 'E' NUMBER '130' INTO e_msg WITH i_lenum.
    EXIT.
  ENDIF.

** Validar dados da palete
  CLEAR lv_valor.

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

** Obter Grupo da palete
*  IF lt_lqua[] IS NOT INITIAL.
*    SELECT *
*      FROM ltap INTO TABLE lt_ltap
*      FOR ALL ENTRIES IN lt_lqua
*      WHERE lgnum = i_lgnum
*      AND   vlenr = lt_lqua-lenum.
*  ENDIF.
*
*  IF lt_ltap[] IS INITIAL.
*    SELECT *
*      FROM ltap INTO TABLE lt_ltap
*      FOR ALL ENTRIES IN lt_lqua
*      WHERE lgnum = i_lgnum
*      AND   nlenr = lt_lqua-lenum.
*  ENDIF.
*
*  " Palete Posição do Meio do EPE
*  CLEAR lt_ltap.
*  READ TABLE lt_ltap INDEX 1.
*
*  IF lt_ltap-vltyp = lv_lgtyp.
*    SELECT *
*      FROM ltap INTO TABLE lt_ltap
*       WHERE lgnum = i_lgnum
*       AND   vlenr = lt_ltap-vlenr.
*  ENDIF.
*
*  IF lt_ltap[] IS NOT INITIAL.
*    SELECT *
*      FROM ltak INTO TABLE lt_ltak
*      FOR ALL ENTRIES IN lt_ltap
*      WHERE lgnum = lt_ltap-lgnum
*      AND   tanum = lt_ltap-tanum.
*  ENDIF.
*
*  DELETE lt_ltak WHERE tbnum IS INITIAL.
** Validar NT
*  READ TABLE lt_ltak INDEX 1.
*  IF sy-subrc = 0.
*    SELECT SINGLE *
*      FROM ltbk INTO ls_ltbk
*      WHERE lgnum = lt_ltak-lgnum
*      AND   tbnum = lt_ltak-tbnum.
*  ENDIF.

** Validar NT
  SPLIT i_pos AT '.' INTO lv_lgpla lv_dummy.

  SELECT SINGLE *
    FROM zwm011 INTO ls_zwm011
    WHERE armazem     = i_lgnum
    AND   equipamento = lv_lgpla.

  IF sy-subrc = 0.
    SELECT SINGLE *
      FROM ltbk INTO ls_ltbk
      WHERE lgnum = i_lgnum
      AND   tbnum = ls_zwm011-to_number.
  ENDIF.

    lv_refnr = ls_ltbk-benum.

  IF ls_ltbk-tbnum IS INITIAL.
    " Palete & sem Necessidade de transferência definida!
    MESSAGE ID 'ZWM001' TYPE 'E' NUMBER '129' INTO e_msg WITH i_lenum.
    EXIT.
  ENDIF.

  IF lv_refnr IS INITIAL.
    " Palete & sem grupo definido para reserva!
    MESSAGE ID 'ZWM001' TYPE 'E' NUMBER '128' INTO e_msg WITH i_lenum.
    EXIT.
  ENDIF.

** Criar HU
**********************************************************************
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'PALETIZACAO_ESPECIAL'
      i_parametro = 'PALETE'
    IMPORTING
      e_valor     = lv_valor
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc = 0.
    lv_vhilm = lv_valor.
  ENDIF.

  READ TABLE lt_lqua INDEX 1.
  IF sy-subrc = 0.

    LOOP AT lt_lqua.
      CLEAR lt_items.
      lt_items-material = lt_lqua-matnr.
      lt_items-quantity = lt_lqua-verme.
      lt_items-unit     = lt_lqua-meins.
      lt_items-batch    = lt_lqua-charg.
      COLLECT lt_items.
    ENDLOOP.

    CALL FUNCTION 'ZWM_CREATE_HU'
      EXPORTING
        warehouse                  = lt_lqua-lgnum
        plant                      = lt_lqua-werks
        s_loc                      = lt_lqua-lgort
        packing_material           = lv_vhilm
      IMPORTING
        hukey                      = lv_hukey
      TABLES
        return_msg                 = lt_return
        items                      = lt_items
      EXCEPTIONS
        empty_table                = 1
        reference_document_differs = 2
        empty_delivery_item        = 3
        item_not_found             = 4
        OTHERS                     = 5.

    IF sy-subrc <> 0 OR lv_hukey IS INITIAL.
      " Erro na criação da HU!
      MESSAGE ID 'ZWM001' TYPE 'E' NUMBER '126' INTO e_msg.
      EXIT.
    ENDIF.

  ENDIF.

** Criar OT
**********************************************************************
  CLEAR lv_valor.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'PALETIZACAO_ESPECIAL'
      i_parametro = 'MOV_WM_OUT_EPE'
    IMPORTING
      e_valor     = lv_valor
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc = 0.
    lv_bwlvs = lv_valor.
  ENDIF.

  LOOP AT lt_lqua.
    CLEAR lt_ltap_creat.
    lt_ltap_creat-werks = lt_lqua-werks.
    lt_ltap_creat-lgort = lt_lqua-lgort.
    lt_ltap_creat-matnr = lt_lqua-matnr.
    lt_ltap_creat-charg = lt_lqua-charg.
    lt_ltap_creat-anfme = lt_lqua-verme.
    lt_ltap_creat-altme = lt_lqua-meins.
    lt_ltap_creat-vltyp = lt_lqua-lgtyp.
    lt_ltap_creat-vlpla = lt_lqua-lgpla.
    lt_ltap_creat-vlenr = lt_lqua-lenum.
    lt_ltap_creat-letyp = lt_lqua-letyp.
    lt_ltap_creat-nlenr = lv_hukey.
    lt_ltap_creat-ablad = i_pos.
    COLLECT lt_ltap_creat.
  ENDLOOP.

** Criar OT
  lv_lznum = ls_ltbk-tbnum.

  CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
    EXPORTING
      i_lgnum       = i_lgnum
      i_bwlvs       = lv_bwlvs
      i_refnr       = lv_refnr
      i_lznum       = lv_lznum
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

** Confirmar OT
**********************************************************************
  DO 10 TIMES.
    SELECT SINGLE *
      FROM ltak INTO ls_ltak
      WHERE lgnum = i_lgnum
      AND   tanum = lv_tanum.

    IF sy-subrc = 0.
      EXIT.
    ENDIF.

    WAIT UP TO 1 SECONDS.
  ENDDO.

  REFRESH: lt_ltap.

  SELECT *
    FROM ltap INTO TABLE lt_ltap
    WHERE lgnum = i_lgnum
    AND   tanum = lv_tanum.

  LOOP AT lt_ltap.
    CLEAR lt_ltap_conf.
    lt_ltap_conf-tanum = lt_ltap-tanum.
    lt_ltap_conf-tapos = lt_ltap-tapos.
    lt_ltap_conf-altme = lt_ltap-meins.
    lt_ltap_conf-nista = lt_ltap-vsolm.
    APPEND lt_ltap_conf.
  ENDLOOP.

  CALL FUNCTION 'L_TO_CONFIRM'
    EXPORTING
      i_lgnum       = i_lgnum
      i_tanum       = lv_tanum
      i_quknz       = '4' "lv_quknz
      i_commit_work = 'X'
    TABLES
      t_ltap_conf   = lt_ltap_conf
    EXCEPTIONS
      error_message = 99.

  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.

  e_sucess = 'X'.
  e_tanum  = lv_tanum.

** Validar se trabalho da NT foi finalizado
**********************************************************************
  SPLIT i_pos AT '.' INTO lv_lgpla lv_dummy.

  CALL FUNCTION 'ZWM_GET_WORK_EPE'
    EXPORTING
      i_lgnum     = i_lgnum
      i_epe       = lv_lgpla
      i_sim       = 'X'
    IMPORTING
      es_work_epe = ls_work_epe.

  IF ls_work_epe-paltot = ls_work_epe-palfin.

    DELETE FROM zwm011 WHERE armazem     = i_lgnum AND
                             equipamento = lv_lgpla.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.

    e_work_finish = 'X'.
  ENDIF.

ENDFUNCTION.
