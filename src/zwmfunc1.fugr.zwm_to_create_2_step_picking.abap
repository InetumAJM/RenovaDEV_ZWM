FUNCTION zwm_to_create_2_step_picking.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LTAK-LGNUM
*"     REFERENCE(I_REFNR) TYPE  LTAK-REFNR
*"     REFERENCE(I_BWLVS) TYPE  LTAK-BWLVS DEFAULT 0
*"     REFERENCE(I_BETYP) TYPE  LTAK-BETYP DEFAULT SPACE
*"     REFERENCE(I_BENUM) TYPE  LTAK-BENUM DEFAULT SPACE
*"     REFERENCE(I_WEMPF) TYPE  LTAP-WEMPF DEFAULT SPACE
*"     REFERENCE(I_ABLAD) TYPE  LTAP-ABLAD DEFAULT SPACE
*"     REFERENCE(I_KZGSM) TYPE  RL03T-KZGSM DEFAULT 'X'
*"     REFERENCE(I_KZVOL) TYPE  RL03T-KZVOL DEFAULT SPACE
*"     REFERENCE(I_KZANB) TYPE  RL03T-KZANB DEFAULT SPACE
*"     REFERENCE(I_SQUIT) TYPE  RL03T-SQUIT DEFAULT SPACE
*"     REFERENCE(I_NIDRU) TYPE  RL03A-NIDRU DEFAULT SPACE
*"     REFERENCE(I_DRUKZ) TYPE  T329F-DRUKZ DEFAULT SPACE
*"     REFERENCE(I_LDEST) TYPE  LTAP-LDEST DEFAULT SPACE
*"     REFERENCE(I_NOSPL) TYPE  RL03A-NOSPL DEFAULT SPACE
*"     REFERENCE(I_UPDATE_TASK) TYPE  RL03A-VERBU DEFAULT SPACE
*"     REFERENCE(I_COMMIT_WORK) TYPE  RL03B-COMIT DEFAULT 'X'
*"     REFERENCE(I_BNAME) TYPE  LTAK-BNAME DEFAULT SY-UNAME
*"     REFERENCE(I_SOLEX) TYPE  LTAK-SOLEX DEFAULT 0
*"     REFERENCE(I_PERNR) TYPE  LTAK-PERNR DEFAULT 0
*"     REFERENCE(I_UP_GRP_ANL) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(IT_LTAP) TYPE  PDT_T_LTAP_VB OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_TANUM) TYPE  TANUM
*"     REFERENCE(ET_LTAK) TYPE  PDT_T_LTAK_VB
*"     REFERENCE(ET_LTAP) TYPE  PDT_T_LTAP_VB
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"     REFERENCE(E_MATERIAL_ERROR) TYPE  FLAG
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: lt_rspar TYPE TABLE OF rsparams,
        lt_ltap	 TYPE	pdt_t_ltap_vb.

  DATA: ls_message TYPE bdcmsgcoll,
        ls_rspar   TYPE rsparams,
        ls_t311    TYPE t311.

  DATA: lv_subrc TYPE sysubrc.

  FIELD-SYMBOLS: <lv_refnr> TYPE lvs_refnr,
                 <lv_matnr> TYPE matnr.

  lt_ltap = it_ltap.

  CLEAR: e_tanum, et_ltak, et_ltap, et_messages, e_material_error.

  CALL FUNCTION 'L_TO_CREATE_2_STEP_PICKING'
    EXPORTING
      i_lgnum       = i_lgnum
      i_refnr       = i_refnr
      i_bwlvs       = i_bwlvs
      i_betyp       = i_betyp
      i_benum       = i_benum
      i_wempf       = i_wempf
      i_ablad       = i_ablad
      i_kzgsm       = i_kzgsm
      i_kzvol       = i_kzvol
      i_kzanb       = i_kzanb
      i_squit       = i_squit
      i_nidru       = i_nidru
      i_drukz       = i_drukz
      i_ldest       = i_ldest
      i_nospl       = i_nospl
      i_update_task = i_update_task
      i_commit_work = i_commit_work
      i_bname       = i_bname
      i_solex       = i_solex
      i_pernr       = i_pernr
    IMPORTING
      e_tanum       = e_tanum
    TABLES
      t_ltak        = et_ltak
      t_ltap_vb     = lt_ltap
    EXCEPTIONS
      error_message = 99.

*      refnr_lock                  = 1
*      refnr_does_not_exist        = 2
*      refnr_not_relevant          = 3
*      control_for_refnr_not_exist = 4
*      bwlvs_wrong                 = 5
*      betyp_wrong                 = 6
*      benum_missing               = 7
*      betyp_missing               = 8
*      xfeld_wrong                 = 9
*      ldest_wrong                 = 10
*      drukz_wrong                 = 11
*      squit_forbidden             = 12
*      manual_to_forbidden         = 13
*      no_to_created               = 14
*      update_without_commit       = 15
*      no_authority                = 16
*      only_one_quantity_selection = 17
*      no_active_item_found        = 18
*      OTHERS                      = 19.


  lv_subrc = sy-subrc.

** Limpa lixo em REFNR (Enancemnt)
***********************************************************************
  DO 1 TIMES.
    ASSIGN ('(SAPLL03B)I_REFNR') TO <lv_refnr>.
    CHECK <lv_refnr> IS ASSIGNED.

    CLEAR <lv_refnr>.
  ENDDO.

* Não foi possivel criar OT para o material &!
  IF sy-msgid = 'L3' AND sy-msgno = '008'.
    CLEAR et_messages.

    e_material_error = abap_true.

    ASSIGN ('(SAPLL03B)LTAP-MATNR') TO <lv_matnr>.
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = 'ZWMSG001'.
    ls_message-msgnr = '006'.
    ls_message-msgv1 = <lv_matnr>.
    APPEND ls_message TO et_messages.
    ROLLBACK WORK.
    RAISE error.
  ENDIF.

** Erros
***********************************************************************
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

  et_ltap = lt_ltap.

** Update de Status
***********************************************************************
  CHECK i_up_grp_anl EQ abap_true.

  CALL FUNCTION 'ZWM_UPDATE_ANALISE_GRUPOS'
    EXPORTING
      i_lgnum = i_lgnum
      i_refnr = i_refnr.
ENDFUNCTION.
