FUNCTION zwm_count_pal_especial .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  TABLES
*"      T_PAL_ESP STRUCTURE  ZWM_019 OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  TYPES: BEGIN OF st_zwm_019.
           INCLUDE STRUCTURE zwm_019.
         TYPES END OF st_zwm_019.

  DATA: lv_flag_est TYPE flag.
  DATA: lv_tbnum    TYPE tbnum.
  DATA: lv_kunnr    TYPE kunnr.
  DATA: lv_vbeln    TYPE vbeln.
  DATA: lv_refnr    TYPE lvs_refnr.
  DATA: lv_tanum    TYPE tanum.
  DATA: lv_spgru    TYPE lvs_spgru.
  DATA: lv_menge    TYPE menge_d.
  DATA: lv_posnr    TYPE posnr.
  DATA: lv_dummy    TYPE char20.
  DATA: lv_2step    TYPE flag.
  DATA: lv_2spart   TYPE flag.
  DATA: lv_tabix    TYPE sy-tabix.

  DATA: ls_lein     TYPE lein.
  DATA: ls_ltbk     TYPE ltbk.
  DATA: ls_leinv    TYPE leinv.
  DATA: ls_kna1     TYPE kna1.
  DATA: ls_messages TYPE bdcmsgcoll.

  DATA: lt_likp     TYPE TABLE OF likp       WITH HEADER LINE.
  DATA: lt_lips     TYPE TABLE OF lips       WITH HEADER LINE.
  DATA: lt_lips_esp TYPE TABLE OF lips       WITH HEADER LINE.
  DATA: lt_t311     TYPE TABLE OF t311       WITH HEADER LINE.
  DATA: lt_t311a    TYPE TABLE OF t311a      WITH HEADER LINE.
  DATA: lt_zwm031   TYPE TABLE OF zwm031     WITH HEADER LINE.
  DATA: lt_pal_esp  TYPE TABLE OF zwm_019    WITH HEADER LINE.
  DATA: lt_ltak     TYPE TABLE OF ltak       WITH HEADER LINE.
  DATA: lt_ltap     TYPE TABLE OF ltap       WITH HEADER LINE.
  DATA: lt_makt     TYPE TABLE OF makt       WITH HEADER LINE.
  DATA: lt_marm     TYPE TABLE OF marm       WITH HEADER LINE.
  DATA: lt_ltap_aut TYPE TABLE OF ltap       WITH HEADER LINE.
  DATA: lt_lqua     TYPE TABLE OF lqua       WITH HEADER LINE.
  DATA: lt_lqua_aut TYPE TABLE OF lqua       WITH HEADER LINE.
  DATA: lt_ltap_e   TYPE TABLE OF ltap       WITH HEADER LINE.
  DATA: lt_ltbk     TYPE TABLE OF ltbk       WITH HEADER LINE.
  DATA: lt_zwm001   TYPE TABLE OF zwm001     WITH HEADER LINE.
  DATA: lt_ret_msg  TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.

** Validar Grupo
**********************************************************************
  SELECT *
    FROM t311a INTO TABLE lt_t311a
    WHERE lgnum = i_lgnum
    AND   refnr = i_refnr.

  IF lt_t311a[] IS NOT INITIAL.
    SELECT *
      FROM likp INTO TABLE lt_likp
      FOR ALL ENTRIES IN lt_t311a
      WHERE vbeln = lt_t311a-rbnum.

    SELECT *
      FROM lips INTO TABLE lt_lips
      FOR ALL ENTRIES IN lt_t311a
      WHERE vbeln = lt_t311a-rbnum.

    DELETE lt_lips WHERE lfimg <= 0.
  ENDIF.

** Validar materiais com Paletização especial
**********************************************************************
  IF lt_likp[] IS NOT INITIAL.
    SELECT *
      FROM zwm031 INTO TABLE lt_zwm031
      FOR ALL ENTRIES IN lt_likp
      WHERE lgnum = i_lgnum
      AND   kunnr = lt_likp-kunnr.
  ENDIF.

** Descrição Material
  IF lt_lips[] IS NOT INITIAL.
    SELECT *
      FROM makt INTO TABLE lt_makt
      FOR ALL ENTRIES IN lt_lips
      WHERE matnr = lt_lips-matnr
      AND   spras = sy-langu.
  ENDIF.

** Unidade PAL
  IF lt_makt[] IS NOT INITIAL.
    SELECT *
      FROM marm INTO TABLE lt_marm
      FOR ALL ENTRIES IN lt_makt
      WHERE matnr = lt_makt-matnr
      AND   meinh = 'PAL'.
  ENDIF.

*  LOOP AT lt_likp.
*
*    LOOP AT lt_lips WHERE vbeln = lt_likp-vbeln.
*
*      READ TABLE lt_zwm031 WITH KEY lgnum = i_lgnum
*                                    kunnr = lt_likp-kunnr
*                                    matnr = lt_lips-matnr.
*      CHECK sy-subrc = 0.
*      CLEAR lt_pal_esp.
*      lt_pal_esp-lgnum = i_lgnum.
*      lt_pal_esp-kunnr = lt_likp-kunnr.
*      lt_pal_esp-matnr = lt_lips-matnr.
*      lt_pal_esp-lfimg = lt_lips-lfimg.
*      lt_pal_esp-vrkme = lt_lips-vrkme.
*      COLLECT lt_pal_esp.
*    ENDLOOP.
*
*  ENDLOOP.

** Remessas
  CLEAR: lv_2step, lv_2spart.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = i_lgnum
      i_refnr  = i_refnr
    IMPORTING
      e_2step  = lv_2step
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  LOOP AT lt_t311a WHERE refnr = i_refnr.

    " Cliente
    READ TABLE lt_likp WITH KEY vbeln = lt_t311a-rbnum.
    CHECK sy-subrc = 0.

    LOOP AT lt_lips WHERE vbeln = lt_likp-vbeln.

      " Paletização Especial
      READ TABLE lt_zwm031 WITH KEY kunnr = lt_likp-kunnr
                                    matnr = lt_lips-matnr.

      CHECK sy-subrc = 0.

      " Validar se não é palete Remontada ????
      CHECK lt_zwm031-remontada IS INITIAL.

      CLEAR lt_pal_esp.
      lt_pal_esp-lgnum = i_lgnum.
      lt_pal_esp-refnr = i_refnr.
      lt_pal_esp-kunnr = lt_zwm031-kunnr.
      lt_pal_esp-matnr = lt_lips-matnr.
      lt_pal_esp-lfimg = lt_lips-lfimg.
      lt_pal_esp-vrkme = lt_lips-vrkme.

      " Picking à Remessa
      IF lv_2step IS INITIAL.

        CONCATENATE lt_lips-vbeln '#' lt_lips-posnr INTO lt_pal_esp-lznum.

        " Picking à Remessa agrupado
      ELSEIF lv_2spart = 'X'.
        lt_pal_esp-lznum = lt_likp-vbeln.

        " Picking ao Grupo
      ELSE.
        lt_pal_esp-lznum = lt_zwm031-kunnr.
      ENDIF.

      COLLECT lt_pal_esp.
    ENDLOOP.
  ENDLOOP.

  SORT lt_pal_esp BY kunnr lznum matnr.

** Validar quantidades só de paletes completas
  LOOP AT lt_pal_esp.

    lv_tabix = sy-tabix.

    READ TABLE lt_zwm031 WITH KEY kunnr = lt_pal_esp-kunnr
                                  matnr = lt_pal_esp-matnr.

    READ TABLE lt_marm WITH KEY matnr = lt_pal_esp-matnr.
    IF sy-subrc <> 0.
      " Material & sem unidade PAL definido
      CLEAR ls_messages.
      ls_messages-msgtyp = 'E'.
      ls_messages-msgid  = 'ZWMMSG001'.
      ls_messages-msgnr  = '000'.
      ls_messages-msgv1  = 'Material'.
      ls_messages-msgv2  = lt_pal_esp-matnr.
      ls_messages-msgv3  = 'sem unidade PAL definido'.
      APPEND ls_messages TO et_messages.
      EXIT.
    ENDIF.

    " Paletes
    lv_menge = lt_pal_esp-lfimg / lt_marm-umrez.
    lv_menge = floor( lv_menge ).

    IF lv_menge = 0.
      DELETE lt_pal_esp INDEX lv_tabix.
    ENDIF.

    lt_pal_esp-lfimg = lv_menge * lt_marm-umrez.
    lt_pal_esp-pal   = lv_menge.

    " Paletes - Paletização Especial
    lv_menge = lt_pal_esp-lfimg / lt_zwm031-unporpal.

    lv_menge = ceil( lv_menge ).

    lt_pal_esp-pal_esp = lv_menge.

    MODIFY lt_pal_esp INDEX lv_tabix.
  ENDLOOP.

  IF et_messages[] IS NOT INITIAL.
    RAISE error.
  ENDIF.

  t_pal_esp[] = lt_pal_esp[].

ENDFUNCTION.
