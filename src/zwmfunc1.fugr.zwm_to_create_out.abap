FUNCTION zwm_to_create_out.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WAREHOUSE) TYPE  LGNUM
*"     REFERENCE(REFNR) TYPE  LVS_REFNR OPTIONAL
*"     REFERENCE(VBELN) TYPE  VBELN OPTIONAL
*"     REFERENCE(POSNR) TYPE  POSNR OPTIONAL
*"     REFERENCE(VSOLA) TYPE  LTAP-VSOLA OPTIONAL
*"     REFERENCE(MEINS) TYPE  MEINS OPTIONAL
*"     REFERENCE(SU) TYPE  LENUM OPTIONAL
*"     REFERENCE(SU2) TYPE  LENUM OPTIONAL
*"     REFERENCE(VLTYP) TYPE  LGTYP OPTIONAL
*"     REFERENCE(VLPLA) TYPE  LGPLA OPTIONAL
*"     REFERENCE(BACKGROUND) TYPE  LVS_DUNKL OPTIONAL
*"     REFERENCE(WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(LGORT) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(MATNR) TYPE  MATNR OPTIONAL
*"     REFERENCE(CHARG) TYPE  CHARG_D OPTIONAL
*"     REFERENCE(BENUM) TYPE  LVS_BENUM OPTIONAL
*"  EXPORTING
*"     REFERENCE(TO) TYPE  TANUM
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR_MESSAGE
*"----------------------------------------------------------------------
  DATA: lt_messages   TYPE tab_bdcmsgcoll,
        lt_ltap       TYPE pdt_t_ltap_vb,
        lt_ltap_creat TYPE TABLE OF ltap_creat.

  DATA: ls_ltap       TYPE ltap_vb,
        ls_ltap_creat TYPE ltap_creat,
        ls_message    TYPE bdcmsgcoll.

  DATA: lv_2step   TYPE flag,
        lv_vlenr   TYPE lenum,
        lv_betyp   TYPE lvs_betyp,
        lv_benum   TYPE lvs_benum,
        lv_2spart  TYPE flag.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = warehouse
      i_refnr  = refnr
      i_vbeln  = vbeln
    IMPORTING
      e_2step  = lv_2step
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

** Grupos em 1 Passo
***********************************************************************
  IF lv_2step EQ abap_false.
    CALL FUNCTION 'ZWM_CREATE_TO_DELIVERY'
      EXPORTING
        warehouse     = warehouse
        refnr         = refnr
        vbeln         = vbeln
        posnr         = posnr
        vsola         = vsola
        su            = su
        su2           = su2
        vltyp         = vltyp
        vlpla         = vlpla
        background    = background
        charg         = charg
      IMPORTING
        to            = to
      TABLES
        return_msg    = return_msg[]
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      RAISE error_message.
    ENDIF.
    EXIT.
  ENDIF.

** Grupos em 2 Passos
***********************************************************************
  CLEAR lt_ltap_creat.

  IF lv_2spart EQ abap_true.
    lv_betyp = 'L'.
    IF NOT vbeln IS INITIAL.
      lv_benum = vbeln.
    ELSE.
      lv_benum = benum.
    ENDIF.
  ENDIF.

  IF meins IS INITIAL.
** NAO TEM UNIDADE - VALIDAR ANTES DE DAR F8
***********************************************************************
    break roff-tp.
** NAO TEM UNIDADE - VALIDAR ANTES DE DAR F8
***********************************************************************
    break roff-om.
** NAO TEM UNIDADE - VALIDAR ANTES DE DAR F8
***********************************************************************
    break roff-rs.
** NAO TEM UNIDADE - VALIDAR ANTES DE DAR F8
***********************************************************************
  ENDIF.

  DO 2 TIMES.
    CLEAR lv_vlenr.

    IF sy-index EQ 1.
      lv_vlenr = su.
    ELSEIF NOT su2 IS INITIAL.
      lv_vlenr = su2.
    ELSE.
      EXIT.
    ENDIF.

    ls_ltap_creat-matnr = matnr.
    ls_ltap_creat-charg = charg.
    ls_ltap_creat-werks = werks.
    ls_ltap_creat-lgort = lgort.
    ls_ltap_creat-anfme = vsola.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        input          = meins
        language       = sy-langu
      IMPORTING
        output         = ls_ltap_creat-altme
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      ls_ltap_creat-altme = meins.
    ENDIF.

    ls_ltap_creat-vlenr = lv_vlenr.
    ls_ltap_creat-vltyp = vltyp.
    ls_ltap_creat-vlpla = vlpla.
    APPEND ls_ltap_creat TO lt_ltap_creat.
  ENDDO.

  CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
    EXPORTING
      i_lgnum                = warehouse
      i_bwlvs                = '850'
      i_betyp                = lv_betyp
      i_benum                = lv_benum
      i_refnr                = refnr
      i_l2ska                = '1'
    IMPORTING
      e_tanum                = to
    TABLES
      t_ltap_creat           = lt_ltap_creat
    EXCEPTIONS
      no_to_created          = 1
      bwlvs_wrong            = 2
      betyp_wrong            = 3
      benum_missing          = 4
      betyp_missing          = 5
      foreign_lock           = 6
      vltyp_wrong            = 7
      vlpla_wrong            = 8
      vltyp_missing          = 9
      nltyp_wrong            = 10
      nlpla_wrong            = 11
      nltyp_missing          = 12
      rltyp_wrong            = 13
      rlpla_wrong            = 14
      rltyp_missing          = 15
      squit_forbidden        = 16
      manual_to_forbidden    = 17
      letyp_wrong            = 18
      vlpla_missing          = 19
      nlpla_missing          = 20
      sobkz_wrong            = 21
      sobkz_missing          = 22
      sonum_missing          = 23
      bestq_wrong            = 24
      lgber_wrong            = 25
      xfeld_wrong            = 26
      date_wrong             = 27
      drukz_wrong            = 28
      ldest_wrong            = 29
      update_without_commit  = 30
      no_authority           = 31
      material_not_found     = 32
      lenum_wrong            = 33
      matnr_missing          = 34
      werks_missing          = 35
      anfme_missing          = 36
      altme_missing          = 37
      lgort_wrong_or_missing = 38
      OTHERS                 = 39.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING error_message.
  ENDIF.

ENDFUNCTION.
