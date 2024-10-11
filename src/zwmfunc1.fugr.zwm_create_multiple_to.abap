FUNCTION zwm_create_multiple_to.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WAREHOUSE) TYPE  LGNUM
*"     REFERENCE(MOV_TYPE) TYPE  BWLVS
*"     REFERENCE(ST_TYPE_O) TYPE  LGTYP OPTIONAL
*"     REFERENCE(BIN_ORIGEM) TYPE  LGPLA OPTIONAL
*"     REFERENCE(ST_TYPE_D) TYPE  LGTYP OPTIONAL
*"     REFERENCE(BIN_DESTINO) TYPE  LGPLA OPTIONAL
*"     REFERENCE(STOCK_CAT) TYPE  BESTQ OPTIONAL
*"     REFERENCE(PLANT) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(S_LOC) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(CERTIFICADO) TYPE  LTAP-ZEUGN OPTIONAL
*"     REFERENCE(ORIGEM) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(REQ_NUMBER) TYPE  LVS_BENUM OPTIONAL
*"     REFERENCE(REQ_TYPE) TYPE  LVS_BETYP OPTIONAL
*"     REFERENCE(SSCC_ADICIONAL) TYPE  LVS_LZNUM OPTIONAL
*"     REFERENCE(REFNR) TYPE  LVS_REFNR OPTIONAL
*"  EXPORTING
*"     REFERENCE(TO) TYPE  TANUM
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"      SSCC STRUCTURE  ZWM_SSCC
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

* Prepare create TO
  DATA: ltap_create LIKE ltap_creat OCCURS 0 WITH HEADER LINE.
  DATA: tab_ltak LIKE ltak_vb OCCURS 0 WITH HEADER LINE.
  DATA: tanum LIKE ltak-tanum.

** Para o movimento 101 é preciso passar este parametro
** MUDAR FIXO !!!!
*  DATA req_number TYPE lvs_benum.
*  IF mov_type = '101'.
*    MOVE bin_origem TO req_number.
*  ELSE.
*    CLEAR req_number.
*  ENDIF.

  REFRESH: return_msg.
  CLEAR:   return_msg.

  LOOP AT sscc.
    CLEAR ltap_create.
    MOVE sscc-material TO ltap_create-matnr.

    IF sscc-werks IS INITIAL.
      MOVE plant TO ltap_create-werks.
    ELSE.
      MOVE sscc-werks TO ltap_create-werks.
    ENDIF.

    IF sscc-lgort IS INITIAL.
      MOVE s_loc TO ltap_create-lgort.
    ELSE.
      MOVE sscc-lgort TO ltap_create-lgort.
    ENDIF.

    MOVE sscc-lote_producao TO ltap_create-charg.
    MOVE sscc-tipo_su TO ltap_create-letyp.
    MOVE sscc-quantidade TO ltap_create-anfme.

    MOVE sscc-uni TO ltap_create-altme.

** Se o SSCC tiver de ser colocado na posição de origem
    IF NOT origem IS INITIAL.
      IF NOT sscc-sscc IS INITIAL.
        MOVE sscc-sscc TO ltap_create-vlenr.
      ENDIF.
    ELSE.
      IF NOT sscc-sscc IS INITIAL.
        MOVE sscc-sscc TO ltap_create-nlenr.
      ENDIF.
    ENDIF.


    IF NOT stock_cat IS INITIAL.
      MOVE  stock_cat TO ltap_create-bestq.
    ENDIF.

    IF NOT st_type_o IS INITIAL.
      MOVE st_type_o TO ltap_create-vltyp.
    ENDIF.

    IF NOT bin_origem IS INITIAL.
      MOVE bin_origem TO ltap_create-vlpla.
    ENDIF.

    IF NOT st_type_d IS INITIAL.
      MOVE st_type_d TO ltap_create-nltyp.
    ENDIF.

    IF NOT bin_destino IS INITIAL.
      MOVE bin_destino TO ltap_create-nlpla.
    ENDIF.

    IF NOT certificado IS INITIAL.
      MOVE certificado TO ltap_create-zeugn.
    ENDIF.
    APPEND ltap_create.
  ENDLOOP.

  CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
    EXPORTING
      i_lgnum                = warehouse
      i_bwlvs                = mov_type
      i_betyp                = req_type
      i_benum                = req_number
      i_lznum                = sscc_adicional
      i_refnr                = refnr
*     i_nospl                = 'X'
*     i_kompl                = ' '
    IMPORTING
      e_tanum                = tanum
    TABLES
      t_ltap_creat           = ltap_create
      t_ltak                 = tab_ltak
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
      error_message          = 39
      OTHERS                 = 40.

  IF sy-subrc <> 0.
    return_msg-msgid  = sy-msgid.
    return_msg-msgtyp = sy-msgty.
    return_msg-msgnr  = sy-msgno.
    return_msg-msgv1  = sy-msgv1.
    return_msg-msgv2  = sy-msgv2.
    return_msg-msgv3  = sy-msgv3.
    return_msg-msgv4  = sy-msgv4.
    APPEND return_msg.
    RAISE error.
  ELSE.
    to = tanum.
  ENDIF.
ENDFUNCTION.
