FUNCTION zwm_create_to_pal_esp .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TANUM) TYPE  TANUM
*"  EXPORTING
*"     REFERENCE(E_TANUM) TYPE  TANUM
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lv_tbnum    TYPE tbnum.
  DATA: lv_kunnr    TYPE kunnr.
  DATA: lv_vbeln    TYPE vbeln.
  DATA: lv_refnr    TYPE lvs_refnr.
  DATA: lv_tanum    TYPE tanum.
  DATA: lv_posnr    TYPE posnr.
  DATA: lv_dummy    TYPE char20.
  DATA: lv_2step    TYPE flag.
  DATA: lv_2spart   TYPE flag.
  DATA: lv_betyp    TYPE ltak-betyp.
  DATA: lv_benum    TYPE ltak-benum.

  DATA: ls_ltbk     TYPE ltbk.
  DATA: ls_kna1     TYPE kna1.
  DATA: ls_lqua     TYPE lqua.
  DATA: ls_ltak     TYPE ltak.
  DATA: ls_ltap     TYPE ltap.
  DATA: ls_delit    TYPE l03b_delit.
  DATA: ls_messages TYPE bdcmsgcoll.

  DATA: lt_ret_msg    TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.
  DATA: lt_ltap_creat	TYPE TABLE OF ltap_creat WITH HEADER LINE.
  DATA: lt_lqua       TYPE TABLE OF lqua       WITH HEADER LINE.
  DATA: lt_delit      TYPE l03b_delit_t.

** Criar OT de Expedição
**********************************************************************
  SELECT SINGLE *
    FROM ltak INTO ls_ltak
    WHERE lgnum = i_lgnum
    AND   tanum = i_tanum.

  CHECK sy-subrc = 0.

  SELECT SINGLE *
    FROM ltap INTO ls_ltap
    WHERE lgnum = i_lgnum
    AND   tanum = i_tanum.

  lv_tbnum = ls_ltak-lznum.

  SELECT SINGLE *
    FROM ltbk INTO ls_ltbk
    WHERE lgnum = ls_ltak-lgnum
    AND   tbnum = lv_tbnum.

  CHECK sy-subrc = 0.

  " Validar Cliente e Remessa
  CLEAR: lv_kunnr, lv_vbeln, lv_posnr.

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

    IF sy-subrc = 0.
      SPLIT ls_ltbk-lznum AT '#' INTO lv_dummy lv_posnr.
      IF sy-subrc <> 0.
        CLEAR lv_posnr.
      ENDIF.
    ENDIF.
  ENDIF.

  SELECT *
    FROM lqua INTO TABLE lt_lqua
    WHERE lgnum = i_lgnum
    AND   lenum = ls_ltap-nlenr.

  lv_refnr = ls_ltbk-benum.

** Validar Grupo
  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = i_lgnum
      i_refnr  = lv_refnr
      i_vbeln  = lv_vbeln
    IMPORTING
      e_2step  = lv_2step
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

** Cria OT Remessa
**********************************************************************
  IF lv_2step EQ abap_false.

    LOOP AT lt_lqua.
      CLEAR ls_delit.
      ls_delit-posnr = lv_posnr.
      ls_delit-anfme = lt_lqua-verme.
      ls_delit-altme = lt_lqua-meins.
      ls_delit-charg = lt_lqua-charg.
      ls_delit-vltyp = lt_lqua-lgtyp.
      ls_delit-vlpla = lt_lqua-lgpla.
      ls_delit-vlenr = lt_lqua-lenum.
      ls_delit-letyp = lt_lqua-letyp.
      COLLECT ls_delit INTO lt_delit.
    ENDLOOP.

    DO 2 TIMES.
      CALL FUNCTION 'L_TO_CREATE_DN'
        EXPORTING
          i_lgnum       = i_lgnum
          i_vbeln       = lv_vbeln
          i_refnr       = lv_refnr
          i_nospl       = 'X'
          i_squit       = ' '
          it_delit      = lt_delit
        IMPORTING
          e_tanum       = lv_tanum
        EXCEPTIONS
          error_message = 99.

      IF sy-subrc = 0 AND lv_tanum IS NOT INITIAL.
        e_tanum = lv_tanum.

        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.
    ENDDO.

** Criar OT ao Grupo
**********************************************************************
  ELSE.

    IF lv_2spart EQ abap_true.
      lv_betyp = 'L'.

      IF NOT lv_vbeln IS INITIAL.
        lv_benum = lv_vbeln.
      ENDIF.
    ENDIF.

    CLEAR lt_ltap_creat.
    REFRESH lt_ltap_creat.

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
      COLLECT lt_ltap_creat.
    ENDLOOP.

    DO 2 TIMES.
      CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
        EXPORTING
          i_lgnum       = i_lgnum
          i_bwlvs       = '850'
          i_betyp       = lv_betyp
          i_benum       = lv_benum
          i_refnr       = lv_refnr
          i_nospl       = 'X'
          i_l2ska       = '1'
        IMPORTING
          e_tanum       = lv_tanum
        TABLES
          t_ltap_creat  = lt_ltap_creat
        EXCEPTIONS
          error_message = 99.

      IF sy-subrc = 0 AND lv_tanum IS NOT INITIAL.
        e_tanum = lv_tanum.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.

    ENDDO.
  ENDIF.

  IF lv_tanum IS INITIAL.
    CLEAR lt_ret_msg.
    lt_ret_msg-msgtyp = 'E'.
    lt_ret_msg-msgid  = sy-msgid.
    lt_ret_msg-msgnr  = sy-msgno.
    lt_ret_msg-msgv1  = sy-msgv1.
    lt_ret_msg-msgv2  = sy-msgv2.
    lt_ret_msg-msgv3  = sy-msgv3.
    lt_ret_msg-msgv4  = sy-msgv4.
    APPEND lt_ret_msg.

    ROLLBACK WORK.

    et_messages[] = lt_ret_msg[].

    RAISE error.
  ENDIF.

ENDFUNCTION.
