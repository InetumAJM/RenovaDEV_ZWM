FUNCTION zwm_to_create_single .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_BWLVS) TYPE  BWLVS
*"     REFERENCE(I_WERKS) TYPE  WERKS_D
*"     REFERENCE(I_LGORT) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(I_MATNR) TYPE  MATNR
*"     REFERENCE(I_CHARG) TYPE  CHARG_D OPTIONAL
*"     REFERENCE(I_ANFME) TYPE  RL03TANFME
*"     REFERENCE(I_ALTME) TYPE  LRMEI
*"     REFERENCE(I_BETYP) TYPE  LVS_BETYP OPTIONAL
*"     REFERENCE(I_BENUM) OPTIONAL
*"     REFERENCE(I_SOBKZ) TYPE  SOBKZ OPTIONAL
*"     REFERENCE(I_SONUM) OPTIONAL
*"     REFERENCE(I_LETYP) TYPE  LVS_LETYP OPTIONAL
*"     REFERENCE(I_WDATU) TYPE  LVS_WDATU OPTIONAL
*"     REFERENCE(I_SQUIT) TYPE  RL03TSQUIT OPTIONAL
*"     REFERENCE(I_NLENR) TYPE  LTAP_NLENR OPTIONAL
*"     REFERENCE(I_VLENR) TYPE  LTAP_VLENR OPTIONAL
*"     REFERENCE(I_BESTQ) TYPE  BESTQ OPTIONAL
*"     REFERENCE(I_VLTYP) TYPE  LTAP_VLTYP OPTIONAL
*"     REFERENCE(I_VLPLA) TYPE  LTAP_VLPLA OPTIONAL
*"     REFERENCE(I_NLTYP) TYPE  LTAP_NLTYP OPTIONAL
*"     REFERENCE(I_NLPLA) TYPE  LTAP_NLPLA OPTIONAL
*"     REFERENCE(I_ABLAD) OPTIONAL
*"     REFERENCE(I_KOMPL) TYPE  RL03B-KOMPL OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"     REFERENCE(I_L2SKA) TYPE  LTAK_L2SKA OPTIONAL
*"     REFERENCE(I_INVENT) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_TANUM) TYPE  TANUM
*"     REFERENCE(ES_LTAP) TYPE  LTAP
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: ls_message TYPE bdcmsgcoll,
         lv_benum   TYPE lvs_benum,
         lv_sonum   TYPE lvs_sonum,
         lv_ablad   TYPE ablad.

***********************************************************************
  CLEAR: e_tanum, es_ltap, et_messages.

** Conversões
***********************************************************************
  lv_benum = i_benum.
  lv_sonum = i_sonum.
  lv_ablad = i_ablad.


** Cria Tarefa
***********************************************************************
  CALL FUNCTION 'L_TO_CREATE_SINGLE'
    EXPORTING
      i_lgnum               = i_lgnum
      i_bwlvs               = i_bwlvs
      i_betyp               = i_betyp
      i_benum               = lv_benum
      i_matnr               = i_matnr
      i_werks               = i_werks
      i_lgort               = i_lgort
      i_charg               = i_charg
      i_bestq               = i_bestq
      i_sobkz               = i_sobkz
      i_sonum               = lv_sonum
      i_letyp               = i_letyp
      i_anfme               = i_anfme
      i_altme               = i_altme
      i_wdatu               = i_wdatu
*     I_VFDAT               = INIT_DATUM
*     I_ZEUGN               = ' '
*     I_LZNUM               = ' '
      i_squit               = i_squit
*     I_NIDRU               = ' '
*     I_DRUKZ               = ' '
*     I_LDEST               = ' '
*     I_WEMPF               = ' '
      i_ablad               = lv_ablad
      i_vltyp               = i_vltyp
*     I_VLBER               = ' '
      i_vlpla               = i_vlpla
*     I_VPPOS               = ' '
      i_vlenr               = i_vlenr
*     I_VLQNR               = ' '
      i_nltyp               = i_nltyp
*     I_NLBER               = ' '
      i_nlpla               = i_nlpla
*     I_NPPOS               = ' '
      i_nlenr               = i_nlenr
*     I_NLQNR               = ' '
*     I_RLTYP               = ' '
*     I_RLBER               = ' '
*     I_RLPLA               = ' '
*     I_RLQNR               = ' '
*     I_UPDATE_TASK         = ' '
      i_commit_work         = i_commit
*     I_BNAME               = SY-UNAME
      i_kompl               = i_kompl
*     I_SOLEX               = 0
*     I_PERNR               = 0
*     I_AUSFB               = ' '
      i_refnr               = i_refnr
      i_l2ska               = i_l2ska
      i_invent              = i_invent
    IMPORTING
      e_tanum               = e_tanum
      e_ltap                = es_ltap
    EXCEPTIONS
      no_to_created         = 1
      bwlvs_wrong           = 2
      betyp_wrong           = 3
      benum_missing         = 4
      betyp_missing         = 5
      foreign_lock          = 6
      vltyp_wrong           = 7
      vlpla_wrong           = 8
      vltyp_missing         = 9
      nltyp_wrong           = 10
      nlpla_wrong           = 11
      nltyp_missing         = 12
      rltyp_wrong           = 13
      rlpla_wrong           = 14
      rltyp_missing         = 15
      squit_forbidden       = 16
      manual_to_forbidden   = 17
      letyp_wrong           = 18
      vlpla_missing         = 19
      nlpla_missing         = 20
      sobkz_wrong           = 21
      sobkz_missing         = 22
      sonum_missing         = 23
      bestq_wrong           = 24
      lgber_wrong           = 25
      xfeld_wrong           = 26
      date_wrong            = 27
      drukz_wrong           = 28
      ldest_wrong           = 29
      update_without_commit = 30
      no_authority          = 31
      material_not_found    = 32
      lenum_wrong           = 33
      error_message         = 98
      OTHERS                = 99.

** Processamento de Erros
***********************************************************************
  IF sy-subrc <> 0.
    ls_message-msgid  = sy-msgid.
    ls_message-msgtyp = sy-msgty.
    ls_message-msgnr  = sy-msgno.
    ls_message-msgv1  = sy-msgv1.
    ls_message-msgv2  = sy-msgv2.
    ls_message-msgv3  = sy-msgv3.
    ls_message-msgv4  = sy-msgv4.
    APPEND ls_message TO et_messages.

**  Retira os Bloqueios da Posição
    ROLLBACK WORK.

    RAISE error.
  ENDIF.

  IF i_commit IS INITIAL.
***  Retira os Bloqueios da Posição
*    ROLLBACK WORK.
  ENDIF.


ENDFUNCTION.
