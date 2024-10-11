FUNCTION zwm_to_create_2_step_picking_m.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"     REFERENCE(I_MENGE) TYPE  MENGE_D
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lt_t311a       TYPE TABLE OF t311a,
        lt_likp        TYPE TABLE OF likp,
        lt_lips        TYPE TABLE OF lips,
        lt_ltap_vb     TYPE TABLE OF ltap_vb,
        lt_ltap_create TYPE TABLE OF ltap_creat,
        lt_ltap_vb_new TYPE TABLE OF ltap_vb.

  DATA: ls_t311a       TYPE t311a,
        ls_likp        TYPE likp,
        ls_lips        TYPE lips,
        ls_ltap_vb     TYPE ltap_vb,
        ls_ltap_create TYPE ltap_creat,
        ls_message     TYPE bdcmsgcoll.

  DATA: lv_2step TYPE flag.

  FIELD-SYMBOLS: <ls_ltap_vb> TYPE ltap_vb.

  SELECT * FROM t311a
           INTO TABLE lt_t311a
           WHERE lgnum = i_lgnum AND
                 refnr = i_refnr.
  CHECK sy-subrc EQ 0.

  SELECT * FROM likp
           INTO TABLE lt_likp
           FOR ALL ENTRIES IN lt_t311a
           WHERE vbeln = lt_t311a-rbnum.

  SELECT * FROM lips
           INTO TABLE lt_lips
           FOR ALL ENTRIES IN lt_likp
           WHERE vbeln = lt_likp-vbeln.

  CHECK sy-subrc EQ 0.

  LOOP AT lt_likp INTO ls_likp.
    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_vbeln = ls_likp-vbeln
      IMPORTING
        e_2step = lv_2step
      EXCEPTIONS
        error   = 1
        OTHERS  = 2.
    CHECK lv_2step EQ abap_true.

    CLEAR: lt_ltap_create.
    LOOP AT lt_lips INTO ls_lips WHERE vbeln = ls_likp-vbeln.
      CLEAR: ls_ltap_create.
      ls_ltap_create-matnr = ls_lips-matnr.
      ls_ltap_create-werks = ls_lips-werks.
      ls_ltap_create-lgort = ls_lips-lgort.
      ls_ltap_create-anfme = ls_lips-lfimg.
      ls_ltap_create-altme = ls_lips-vrkme.
      COLLECT ls_ltap_create INTO lt_ltap_create.
    ENDLOOP.

    CLEAR: lt_ltap_vb.
    CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
      EXPORTING
        i_lgnum       = i_lgnum
        i_bwlvs       = '850'
        i_commit_work = abap_false
        i_refnr       = i_refnr
        i_l2ska       = '2'
      TABLES
        t_ltap_creat  = lt_ltap_create
        t_ltap_vb     = lt_ltap_vb
      EXCEPTIONS
        OTHERS        = 1.

    IF sy-subrc <> 0.
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

    ROLLBACK WORK.


    LOOP AT lt_ltap_vb ASSIGNING <ls_ltap_vb>.
      CLEAR: <ls_ltap_vb>-tanum, <ls_ltap_vb>-tapos.

      CLEAR: lt_ltap_vb_new.
      APPEND <ls_ltap_vb> TO lt_ltap_vb_new.

      CALL FUNCTION 'ZWM_TO_CREATE_2_STEP_PICKING'
        EXPORTING
          i_lgnum      = i_lgnum
          i_refnr      = i_refnr
          i_betyp      = 'L'
          i_benum      = ls_likp-vbeln
          i_kzgsm      = ''
          i_kzanb      = 'X'
          i_up_grp_anl = ''
          it_ltap      = lt_ltap_vb_new
        IMPORTING
          et_messages  = et_messages
        EXCEPTIONS
          error        = 1
          OTHERS       = 2.

      IF sy-subrc <> 0.
        RAISE error.
      ENDIF.
    ENDLOOP.
  ENDLOOP.




ENDFUNCTION.
