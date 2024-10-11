FUNCTION zwm_to_validate_picking.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"  EXPORTING
*"     REFERENCE(E_SUCCESS) TYPE  FLAG
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: lt_lqua       TYPE TABLE OF lqua,
        lt_ltap_creat TYPE TABLE OF ltap_creat,
        lt_ltap_vb    TYPE TABLE OF ltap_vb,
        lt_ltap_vb_f  TYPE TABLE OF ltap_vb,
        lt_lagp       TYPE TABLE OF lagp.

  DATA: ls_mara       TYPE mara,
        ls_mlgn       TYPE mlgn,
        ls_lqua       TYPE lqua,
        ls_lagp       TYPE lagp,
        ls_ltap_vb    TYPE ltap_vb,
        ls_zwm026     TYPE zwm026,
        ls_ltap_creat TYPE ltap_creat,
        ls_message    TYPE bdcmsgcoll.

  DATA: lv_verme     TYPE lqua_verme,
        lv_anfme_c   TYPE rl03tanfme,
        lv_answer    TYPE c,
        lv_tanum     TYPE tanum,
        lv_item      TYPE sytabix,
        lv_lines_t   TYPE sytabix,
        lv_number    TYPE nrto,
        lv_2step     TYPE flag,
        lv_2spart    TYPE flag,
        lv_pck_break TYPE flag,
        lv_subrc     TYPE sysubrc.

  FIELD-SYMBOLS: <lv_matnr> TYPE matnr,
                 <lv_anfme> TYPE rl03tanfme.

  CLEAR: e_success, et_messages.

** Variaveis Retornadas
***********************************************************************
  ASSIGN ('(SAPLL03B)LTAP-MATNR') TO <lv_matnr>.
  CHECK <lv_matnr> IS ASSIGNED.

  ASSIGN ('(SAPLL03B)RL03A-ANFME') TO <lv_anfme>.
  CHECK <lv_anfme> IS ASSIGNED.

** Valida Grupos em 2 Passos
***********************************************************************
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


  CHECK lv_2step  EQ abap_true AND
        lv_2spart EQ abap_false.


** Retorna Quantidade de Pal e valida se quantidade é sup ou igual
***********************************************************************
  SELECT SINGLE * FROM mara
                  INTO ls_mara
                  WHERE matnr = <lv_matnr>.

  SELECT SINGLE * FROM mlgn
                  INTO ls_mlgn
                  WHERE matnr = <lv_matnr> AND
                        lgnum = i_lgnum.


  CHECK <lv_anfme> >= ls_mlgn-lhmg1.


** Valida quantidades de picking
***********************************************************************
  SELECT * FROM lqua
     INTO TABLE lt_lqua
     WHERE matnr = <lv_matnr> AND
           lgnum = i_lgnum AND
           lgtyp IN ('PCK','PKB').


  LOOP AT lt_lqua INTO ls_lqua.
    lv_verme = lv_verme + ls_lqua-verme.
  ENDLOOP.

  CHECK lv_verme >= <lv_anfme>.

** Só existe stock em Picking para Mat. &
  MESSAGE i007 DISPLAY LIKE 'W' WITH <lv_matnr>.

*  CALL FUNCTION 'POPUP_TO_CONFIRM'
*    EXPORTING
*      text_question         = 'Só existe stock em Picking, deseja criar OTs de Picking?'
*      text_button_1         = 'Sim'
*      icon_button_1         = '@01@'
*      text_button_2         = 'Não'
*      icon_button_2         = '@02@'
*      default_button        = '1'
*      display_cancel_button = ''
*    IMPORTING
*      answer                = lv_answer
*    EXCEPTIONS
*      text_not_found        = 1
*      OTHERS                = 2.
*
*  CHECK lv_answer EQ 1.

** Criação de OT
***********************************************************************
  lv_anfme_c = <lv_anfme>.

  READ TABLE lt_lqua
        INTO ls_lqua
        INDEX 1.

  DO.
    CLEAR: ls_ltap_creat, lt_ltap_creat, lt_ltap_vb_f.

    IF lv_anfme_c < ls_mlgn-lhmg1.
      EXIT.
    ENDIF.

    ls_ltap_creat-matnr = ls_mara-matnr.
    ls_ltap_creat-werks = ls_lqua-werks.
    ls_ltap_creat-lgort = ls_lqua-lgort.
    ls_ltap_creat-anfme = ls_mlgn-lhmg1.
    ls_ltap_creat-altme = ls_mara-meins.
    ls_ltap_creat-vltyp = ls_lqua-lgtyp.
    ls_ltap_creat-vlpla = ls_lqua-lgpla.

    lv_anfme_c = lv_anfme_c - ls_ltap_creat-anfme.

    DO.
      CLEAR lt_ltap_creat.

      APPEND ls_ltap_creat TO lt_ltap_creat.

      lv_pck_break = abap_true.
      EXPORT lv_pck_break TO MEMORY ID 'ZPCK_BREAK'.

      CLEAR: lt_ltap_vb, lv_tanum, lv_subrc.
      CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
        EXPORTING
          i_lgnum                = i_lgnum
          i_bwlvs                = '850'
          i_refnr                = i_refnr
          i_kompl                = abap_false"Cria para a quantidade que conseguir
          i_l2ska                = '1'
        IMPORTING
          e_tanum                = lv_tanum
        TABLES
          t_ltap_creat           = lt_ltap_creat
          t_ltap_vb              = lt_ltap_vb
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

      lv_subrc = sy-subrc.

      FREE MEMORY ID 'ZPCK_BREAK'.

      IF lv_subrc <> 0.
        ls_message-msgtyp = 'E'.
        ls_message-msgspra = sy-langu.
        ls_message-msgid = sy-msgid.
        ls_message-msgnr = sy-msgno.
        ls_message-msgv1 = sy-msgv1.
        ls_message-msgv2 = sy-msgv2.
        ls_message-msgv3 = sy-msgv3.
        ls_message-msgv4 = sy-msgv4.
        APPEND ls_message TO et_messages.
        ROLLBACK WORK.
        RAISE error.
      ENDIF.

      APPEND LINES OF lt_ltap_vb TO lt_ltap_vb_f.

      LOOP AT lt_ltap_vb INTO ls_ltap_vb.
        ls_ltap_creat-anfme = ls_ltap_creat-anfme - ls_ltap_vb-vsolm.
      ENDLOOP.

      IF ls_ltap_creat-anfme <= 0.
        EXIT.
      ENDIF.
    ENDDO.

*--> Retorna Proxima Numeração
    CLEAR: lv_number.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '03'
        object                  = 'LVS_LENUM'
        quantity                = '1'
      IMPORTING
        number                  = lv_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    CHECK NOT lt_ltap_vb IS INITIAL.

    SELECT * FROM lagp
       INTO TABLE lt_lagp
       FOR ALL ENTRIES IN lt_ltap_vb
       WHERE lgnum = lt_ltap_vb-lgnum AND
             lgtyp = lt_ltap_vb-vltyp AND
             lgpla = lt_ltap_vb-vlpla.

    SORT lt_lagp BY lgtyp lgpla.


    CLEAR: lv_item.

    DESCRIBE TABLE lt_ltap_vb_f LINES lv_lines_t.

    LOOP AT lt_ltap_vb_f INTO ls_ltap_vb.
      CLEAR: ls_zwm026.

      lv_item = lv_item + 1.

      CLEAR: ls_lagp.
      READ TABLE lt_lagp
            INTO ls_lagp
            WITH KEY lgtyp = ls_ltap_vb-vltyp
                     lgpla = ls_ltap_vb-vlpla.

      ls_zwm026-armazem       = i_lgnum.
      ls_zwm026-n_pal_picking = lv_number.
      ls_zwm026-i_pal_picking = lv_item.
      ls_zwm026-grupo         = i_refnr.
      ls_zwm026-remessa       = gc_vbeln_2step_dummy.
      ls_zwm026-posnr         = gc_posnr_2step_dummy.
      ls_zwm026-sub_item      = '1'.
      ls_zwm026-material      = ls_ltap_vb-matnr.
      ls_zwm026-quantidade    = ls_ltap_vb-vsolm.
      ls_zwm026-unidade       = ls_ltap_vb-meins.
      ls_zwm026-descricao     = ls_ltap_vb-maktx.
      ls_zwm026-sorlp         = ls_lagp-sorlp.
      ls_zwm026-to_number     = ls_ltap_vb-tanum.
      ls_zwm026-lote          = ls_ltap_vb-charg.

      IF lv_lines_t EQ lv_item.
        ls_zwm026-pal_picking   = '1'.
      ENDIF.

      INSERT zwm026 FROM ls_zwm026.
    ENDLOOP.

    COMMIT WORK.
  ENDDO.

  e_success = abap_true.

ENDFUNCTION.
