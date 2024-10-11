REPORT zrlmob003 MESSAGE-ID lf..
*---------------------------------------------------------------------*
*     This program builds the framework for the mobile handheld
*     supported transactions
*     **** COUNTING BY STORAGE UNIT NUMBER ****
*     It may be started either user or system guided
*     LM50 system guided    -   LM51 user guided
*---------------------------------------------------------------------*
INCLUDE rlmobinc.

START-OF-SELECTION.

*** MOD -> 22.03.2005
  GET PARAMETER ID 'LGT' FIELD linv-lgtyp.
  GET PARAMETER ID 'LGP' FIELD linv-lgpla.
*** MOD <- 22.03.2005

  message_lang = sy-langu.
* AUTHORITY CHECK for TCODE
  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = sy-tcode
    EXCEPTIONS
      ok     = 0
      not_ok = 2
      OTHERS = 3.
  IF sy-subrc <> 0.
    MESSAGE e172(00) WITH sy-tcode.
  ENDIF.

  sys_inv_i = 1.
  PERFORM user_own_data.
  MOVE whs_id TO linv-lgnum.
  CLEAR bin_type.

  error_code = 8. "call inventory selection
* outer while
  WHILE error_code NE 0.
*.....system guided ..............................................*
    "call just at first time and after finish counting a bin
    IF sy-tcode = con_lm50 AND error_code = 8. "8 = finish counting
      PERFORM select_inventory_document.
      PERFORM prepare_system_guided.
      "when finish counting a bin and no more bins are avilable
      IF error_code NE 0 AND error_code NE 8.
        LEAVE PROGRAM.
      ENDIF.
    ENDIF.
    PERFORM call_screen_0151.
    error_code = 0.
*.expect bin entry/verif. out:  ilagp, clgpla, ulgpla
    PERFORM barcode_check_bin.
    IF error_code NE 0.
      CLEAR sav_clgpla.
      CONTINUE.
    ENDIF.
* after bin verification, fill table INV.
*.Reads the inv.doc. into inv only when clgpla, ulgpla <> 0
    PERFORM invdoc_read_for_bin.
    IF error_code NE 0.
      CONTINUE.
    ENDIF.
*.checks entries ...............................................
    PERFORM check_screen_0151.
    IF error_code NE 0.
      CONTINUE.
    ENDIF.

*.......First screen by default always 151............................*
* inner while
    WHILE 1 = 1.
      CASE call_screen.
        WHEN '0151'.
          PERFORM prepare_system_guided.
          PERFORM call_screen_0151.
          PERFORM check_screen_0151.
          IF error_code NE 0.
            EXIT.
          ENDIF.
        WHEN '0152'.
          PERFORM call_screen_0152.
          PERFORM check_screen_0152.
          IF bin_type = su_managed.
            PERFORM decide_151_or_152.
          ELSE.
            PERFORM decide_153_or_152.
          ENDIF.
        WHEN OTHERS.
*
          LEAVE TO TRANSACTION sy-tcode.
      ENDCASE.
    ENDWHILE.  "while 1 = 1
  ENDWHILE.  "while error_code ne 0


*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_0151
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM call_screen_0151.

  DATA: hlp_plapos  LIKE lagp-lgpla.
  DATA: lv_error_code TYPE xfeld VALUE space.

  "error or a new bin selected
  IF error_code NE 0.
    lv_error_code = con_x.
  ENDIF.


  CALL FUNCTION 'CALL_SCREEN_151'
    EXPORTING
      i_lgnum                        = linv-lgnum
      i_lgtyp                        = linv-lgtyp
      i_lgpla                        = linv-lgpla  "HLP_PLAPOS
      i_tcode                        = sy-tcode
      i_clgpla                       = sav_clgpla
      i_kznul                        = rl04i-kznul
      i_error_code                   = lv_error_code
    IMPORTING
      e_ulgnum                       = ulgnum
      e_ulgtyp                       = ulgtyp
      e_ulgpla                       = ulgpla
      e_clgpla                       = clgpla
      e_clenum                       = clenum
      e_lenum                        = detail_linv-lenum
      e_screen_fcode                 = screen_fcode
      e_bin_type                     = bin_type
    TABLES
      x_sus                          = isus
    EXCEPTIONS
      fail_in_physical_screen_number = 01
      error_message                  = 99.

  IF sy-subrc <> 0.
    message_id = sy-msgid.
    message_number = sy-msgno.
    message_var1 = sy-msgv1.
    message_var2 = sy-msgv2.
    message_var3 = sy-msgv3.
    message_var4 = sy-msgv4.
    PERFORM error_message.
  ELSE.
    MOVE:   ulgnum  TO linv-lgnum,
            ulgtyp  TO linv-lgtyp,
            ulgpla  TO linv-lgpla.
  ENDIF.


ENDFORM.                               " CALL_SCREEN_0151
*&---------------------------------------------------------------------*
*&      Form  CHECK_SCREEN_0151
*&---------------------------------------------------------------------*
*       system reaction after calling screen 151
*       return: LM50 (sys)              LM51
*              - clgpla (verif. bin)   - E_ULGNUM/E_ULGTYP/E_ULGPLA
*              - clenum (entry SUs )   - clenum (entry of SUs )
*
*       1. expect verif entry
*       2. expect su entries
*       3. expect different pushbutton commands
*          o save -> update linvs and save
*          o Detail -> goto screen 152 for detailed entries
*          o New bin ? via Detail possible
*          o Clear  -> 1st clear clears bin loc and resets internal tabs
*          o page up, down -> show again with scrolled SUs in steploop
*          o Next -> continues: check if finished -> 151 not_ok, save ok
*          o Back -> steps out of transaction
*          o Exception ? could possibly create an IDoc for the SU
*----------------------------------------------------------------------*
FORM check_screen_0151.


*........2. expect SU entries -> brings back LINV-LENUM................
  PERFORM barcode_check_su.
*.........Check if su within the bin location and appen to isus/ilinv..
  PERFORM check_su_and_append USING linv-lenum.
  CLEAR linv-lenum.

*........3. Pushbutton actions
  CASE screen_fcode.
*........saves everything in table ilinv (e1linvx) ..................
    WHEN fcode_save.
      SET PARAMETER ID 'RLMOB_CLGPLA' FIELD 'Y'.
      PERFORM ilinv_update.
      IF error_code NE 0.
        EXIT.
      ENDIF.
      IF NOT sav_binempty IS INITIAL.
        PERFORM update_binempty.
        CLEAR sav_binempty.
      ENDIF.
*      perform exception_handling_for_su.
      PERFORM decide_151_or_152.

    WHEN fcode_back.
      LEAVE TO SCREEN 0.

    WHEN fcode_next.
      IF bin_type = non_su_managed.
        PERFORM invdoc_read_for_bin.
        PERFORM prepare_user_guided.
        PERFORM decide_153_or_152.
        IF call_screen = '0153'.
          call_screen = '0151'.
        ELSE.
          call_screen = '0152'.
        ENDIF.
      ELSE.
        call_screen = '0151'.
      ENDIF.


    WHEN fcode_empty.
      sav_binempty = con_x.

      IF bin_type = su_managed.
        PERFORM set_rest_to_empty.
        call_screen = '0151'.
      ELSE.
        call_screen = '0151'.
        IF rl04i-kznul = con_x.
          CLEAR rl04i-kznul.
        ELSE.
          MOVE con_x TO rl04i-kznul.
        ENDIF.
      ENDIF.

    WHEN fcode_detail.
      IF NOT detail_linv-lenum IS INITIAL.
*........reads from ilinv into detail_linv.............................
        PERFORM prepare_screen_152 USING detail_linv-lenum.
        PERFORM decide_151_or_152.
      ENDIF.

    WHEN fcode_delete.
      IF NOT detail_linv-lenum IS INITIAL.
        PERFORM delete_su_from_tables USING detail_linv-lenum.
        call_screen = '0151'.
      ENDIF.

    WHEN fcode_newitem.
      MOVE linv-lgnum TO e1linvx-lgnum.
      MOVE linv-lgtyp TO e1linvx-lgtyp.
      MOVE linv-lgpla TO e1linvx-lgpla.
      IF e1linvx-lgpla NA '/'.
*....check if the storage type is position managed
        PERFORM storage_type_pos_check USING e1linvx-lgnum
                                             e1linvx-lgtyp
                                             CHANGING gv_pos_managed.

      ENDIF.

      call_screen = '0152'.
      sav_newitem = con_newitem.
*     go on to screen 152 for detailed entry of quantities.
    WHEN OTHERS.
      call_screen = '0151'.
  ENDCASE.

ENDFORM.                    "CHECK_SCREEN_0151



*&---------------------------------------------------------------------*
*&      Form  CHECK_SU_AND_APPEND
*&---------------------------------------------------------------------*
*       scanned pallet number will be checked an accpeted for update
*----------------------------------------------------------------------*
*  -->  Linv-lenum, inv
*  <--  isus, and ilinv !   accepted SUs for display and saving
*  <--  new pallet:  prepare for screen 152, no isus, no ilinv.
*  <--  i_e1linvx:  for found pallets
*  <--  isus with text, when pallet was already counted, then no ilinv
*----------------------------------------------------------------------*
FORM check_su_and_append USING p_lenum.

  CHECK NOT p_lenum IS INITIAL.
*......make sure that lenum is not in isus yet...................
  LOOP AT isus WHERE lenum = p_lenum.
    EXIT.
  ENDLOOP.
*...if lenum is already in the list detail screen will be displayed

  IF sy-subrc = 0.
    screen_fcode = fcode_detail.
    detail_linv-lenum = p_lenum.
    EXIT.
  ENDIF.
  SELECT SINGLE * FROM lein WHERE lenum = p_lenum.

  IF sy-subrc = 0.

    IF lein-lgnum <> linv-lgnum OR
      lein-lgtyp <> linv-lgtyp OR
      lein-lgpla <> linv-lgpla.
*        OR LEIN-PLPOS <> LINV-PLPOS.

      message_lang   =  sy-langu.
      message_id   =  'L4'.
      message_number = 203.
      message_var1 = lein-lgtyp.
      message_var2 = lein-lgpla.
      PERFORM error_message.
      EXIT.
    ENDIF.
  ELSE.
* check if it is new HU creating by the system.
    LOOP AT inv WHERE lenum = p_lenum.
    ENDLOOP.
    IF sy-subrc <> 0.

      message_lang   =  sy-langu.
      message_id   =  'L3'.
      message_number = 209.
      message_var1 = linv-lenum.
      PERFORM error_message.
      EXIT.
    ENDIF.
  ENDIF.

*......only then take care of the lenum................................
*  SORT INV.
  LOOP AT inv WHERE lenum = p_lenum.   "AND ISTAT = CON_NOTCOUNTED.
    PERFORM fill_ilinv.
    APPEND ilinv.
  ENDLOOP.

  LOOP AT inv WHERE lenum = p_lenum AND istat = con_notcounted.
  ENDLOOP.

*........no notcounted inv = table of inv records per SU................
  IF sy-subrc <> 0.
    LOOP AT inv WHERE lenum = p_lenum AND istat = con_counted.
*check that there isnt recored with the same SU
      READ TABLE isus WITH KEY lenum = p_lenum.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
*........there are no notcounted but counted invs for the SU..........
      MOVE linv-lgnum TO isus-lgnum.
      MOVE p_lenum TO isus-lenum.
      IF inv-menga <> inv-gesme.
        MOVE text-002 TO isus-text.    "Qty changed
      ELSE.
        MOVE text-004 TO isus-text.    "already counted SU
      ENDIF.
      APPEND isus.
    ENDLOOP.
*.......exception handling, new pallet => goto screen 152..............
    IF sy-subrc <> 0.
      screen_fcode = fcode_detail.
      detail_linv-lenum = p_lenum.
    ENDIF.
  ELSE.
*........there were counted entries in inv for the SU..................
    MOVE linv-lgnum TO isus-lgnum.
    MOVE p_lenum TO isus-lenum.
    LOOP AT inv WHERE lenum = p_lenum AND istat = con_counted.
    ENDLOOP.
    IF sy-subrc = 0.
      MOVE text-007 TO isus-text.      "Partially counted
    ENDIF.
    APPEND isus.
  ENDIF.

ENDFORM.                               " CHECK_SU_AND_APPEND
*&---------------------------------------------------------------------*
*&      Form  FILL_ILINV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_ilinv.
  CLEAR ilinv.

  MOVE:            inv-lgnum  TO    ilinv-lgnum,
                   inv-ivnum  TO    ilinv-ivnum,
                   inv-ivpos  TO    ilinv-ivpos,
                   inv-lgtyp  TO    ilinv-lgtyp,
                   inv-lgpla  TO    ilinv-lgpla,
                   inv-plpos  TO    ilinv-plpos,
                   inv-matnr  TO    ilinv-matnr,
                   inv-werks  TO    ilinv-werks,
                   inv-charg  TO    ilinv-charg,
                   inv-sobkz  TO    ilinv-sobkz,
                   inv-bestq  TO    ilinv-bestq,
                   inv-wdatu  TO    ilinv-wdatu,
                   inv-lenum  TO    ilinv-lenum,
                   inv-altme  TO    ilinv-altme,
                   inv-lqnum  TO    ilinv-lqnum,
                   inv-nanum  TO    ilinv-nanum,
                   inv-nvers  TO    ilinv-nvers,
                   inv-istat  TO    ilinv-istat,
                   inv-idatu  TO    ilinv-idatu,
                   inv-kzinv  TO    ilinv-kzinv,
                   inv-irnum  TO    ilinv-irnum,
                   inv-iseit  TO    ilinv-iseit,
                   inv-letyp  TO    ilinv-letyp,
*                  inv-kznul  to    ilinv-kznul
                   inv-vfdat  TO    ilinv-vfdat,
                   inv-lgort  TO    ilinv-lgort,
                   sy-uname  TO     ilinv-uname.
  IF inv-istat = con_counted.
    MOVE inv-menga TO ilinv-menga.
    MOVE inv-altme TO ilinv-altme.
  ELSE.
*..................quantity taken over from total quantity from system
    MOVE inv-gesme TO ilinv-menga.  "use base qty
    MOVE inv-meins TO ilinv-altme.  "use base uom
  ENDIF.
  IF NOT inv-sobkz IS INITIAL.

    CALL FUNCTION 'L_SONUM_CONV_INT_EXT'
      EXPORTING
        sobkz = inv-sobkz
        sonum = inv-sonum
      IMPORTING
        lsonr = ilinv-lsonr.


  ENDIF.



ENDFORM.                               " FILL_ILINV
*&---------------------------------------------------------------------*
*&      Form  DECIDE_151_OR_152
*&---------------------------------------------------------------------*
*       as long as entries in detail_linv there will be calls on 152
*----------------------------------------------------------------------*
FORM decide_151_or_152.
  DATA: lin LIKE sy-tabix.
  CLEAR e1linvx.
  DESCRIBE TABLE detail_linv LINES lin.
  IF lin = 0.
    call_screen = '0151'.
  ENDIF.
  CHECK lin <> 0.
  PERFORM screen_152_read_entry.
  IF  call_screen <> '0152'.
    call_screen =  '0151'.
  ENDIF.

ENDFORM.                               " DECIDE_151_OR_152
*&---------------------------------------------------------------------*
*&      Form  CHECK_STORAGE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_storage_type_su USING p_lgnum p_lgtyp.
  CHECK NOT p_lgnum IS INITIAL.
  CHECK NOT p_lgtyp IS INITIAL.

  CALL FUNCTION 'L_T331_READ'
    EXPORTING
      i_lgnum        = p_lgnum
      i_lgtyp        = p_lgtyp
    IMPORTING
      e_t331         = t331
    EXCEPTIONS
      no_entry_found = 01.

  IF sy-subrc <> 0.
    message_id = 'L4'.
    message_number = '007'.
    message_var1 = p_lgtyp.
    PERFORM error_message.

    LEAVE TO TRANSACTION sy-tcode.
  ENDIF.

  IF t331-lenvw <> con_x.
    message_number = '018'.
    message_var1 = p_lgnum.
    message_var2 = p_lgtyp.
    PERFORM error_message.

    LEAVE TO TRANSACTION sy-tcode.
  ENDIF.

ENDFORM.                               " CHECK_STORAGE_TYPE
*&---------------------------------------------------------------------*
*&      Form  PREPARE_SYSTEM_GUIDED
*&---------------------------------------------------------------------*
*       decides which bin to count from selection list.
*----------------------------------------------------------------------*
FORM prepare_system_guided.
  CHECK sy-tcode = con_lm50.
  READ TABLE sys_inv INDEX 1.
  IF sy-subrc = 0.
*.........ok, still sth. found.........................................

*.........prepare screen 151 ..........................................
*    ERROR_CODE = 7. "sign for next bin
    MOVE:   sys_inv-lgnum TO linv-lgnum,
            sys_inv-lgtyp TO linv-lgtyp,
            sys_inv-lgpla TO linv-lgpla,
            sys_inv-plpos TO linv-plpos.
  ELSE.
*........well, you had the last already................................
*
    "LEAVE TO TRANSACTION SY-TCODE.
    error_code = 9.
    CLEAR linv.
  ENDIF.
ENDFORM.                               " PREPARE_SYSTEM_GUIDED
*&---------------------------------------------------------------------*
*&      Form  DELETE_SU_FROM_TABLES
*&---------------------------------------------------------------------*
*       After deletion of SU from list delete from internal tables.
*----------------------------------------------------------------------*
FORM delete_su_from_tables USING p_lenum.

*........ilinv: table of SUs that are stored for update................
  DELETE ilinv WHERE lgnum = linv-lgnum AND lenum = p_lenum.
*........isus:  table for the display of SUs in 151....................
  DELETE isus WHERE lgnum = linv-lgnum AND lenum = p_lenum.
*........i_e1linvx:  table of found pallets............................
  DELETE i_e1linvx WHERE lgnum = linv-lgnum AND lenum = p_lenum.

ENDFORM.                               " DELETE_SU_FROM_TABLES
*&---------------------------------------------------------------------*
*&      Form  SET_REST_TO_EMPTY
*&---------------------------------------------------------------------*
*       compare ilinv with inv and update ilinv and isus with text-005
*----------------------------------------------------------------------*
FORM set_rest_to_empty.

  LOOP AT inv WHERE lgnum = linv-lgnum AND
                    istat = con_notcounted.



    READ TABLE ilinv WITH KEY lgnum = inv-lgnum
                              lenum = inv-lenum
                              istat = inv-istat.
    IF sy-subrc <> 0.


*.......pallet not yet counted => add to isus and ilinv  ..............
      READ TABLE isus WITH KEY lgnum = inv-lgnum
                         lenum = inv-lenum.
      IF sy-subrc <> 0.
        MOVE:  inv-lgnum TO isus-lgnum,
               inv-lenum TO isus-lenum,
               text-005  TO isus-text.
        APPEND isus.
      ENDIF.
      PERFORM fill_ilinv.
      CLEAR:  ilinv-menga.
      MOVE con_x TO ilinv-kznul.
      MOVE con_counted TO ilinv-istat.
      APPEND ilinv.
      SORT ilinv.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " SET_REST_TO_EMPTY
*&---------------------------------------------------------------------*
*&      Form  PREPARE_SCREEN_152
*&---------------------------------------------------------------------*
*       everything displayable per SU will be displayed
*----------------------------------------------------------------------*
FORM prepare_screen_152 USING p_lenum.

  DATA: s_mlgn LIKE mlgn.
  REFRESH detail_linv.
*........all ilinv records for the SU number
  LOOP AT ilinv WHERE lenum = p_lenum.
    detail_linv = ilinv.
*  if su was not counted clear default quantity and UOM
    IF detail_linv-istat EQ con_notcounted.
      CLEAR detail_linv-menga.
    ENDIF.
    IF detail_linv-altme IS INITIAL.
      READ TABLE inv WITH KEY ivnum = ilinv-ivnum
        ivpos = ilinv-ivpos lqnum = ilinv-lqnum.
      IF NOT inv-altme IS INITIAL.
        detail_linv-altme = inv-altme.
      ELSE.
        "get WM UOM from material
        CALL FUNCTION 'MLGN_SINGLE_READ'
          EXPORTING
            matnr             = detail_linv-matnr
            lgnum             = detail_linv-lgnum
          IMPORTING
            wmlgn             = s_mlgn
          EXCEPTIONS
            lock_on_mlgn      = 1
            lock_system_error = 2
            wrong_call        = 3
            not_found         = 4
            OTHERS            = 5.

        IF sy-subrc = 0 AND NOT s_mlgn-lvsme IS INITIAL.
          detail_linv-altme = s_mlgn-lvsme.
        ELSE.
          detail_linv-altme = inv-meins.
        ENDIF.

      ENDIF.  "if not inv-altme is initial
    ENDIF.  "if detail_linv-altme is initial
    MOVE con_x TO detail_linv-processed.
    APPEND detail_linv.
  ENDLOOP.


*.......newly found items on a specific SU number.......................
  LOOP AT i_e1linvx WHERE lenum = p_lenum.
    detail_linv = i_e1linvx.
    MOVE con_x TO detail_linv-processed.
    APPEND detail_linv.
  ENDLOOP.

  detail_linv_i = 1.
ENDFORM.                               " PREPARE_SCREEN_152
*&---------------------------------------------------------------------*
*&      Form  STORAGE_TYPE_POS_CHECK
*&---------------------------------------------------------------------*
* check that the putaway strategy is postion strategy.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM storage_type_pos_check USING  p_lgnum p_lgtyp
                            CHANGING gv_pos_managed.

  DATA:tmp_t331 LIKE t331.
*....initial the variable
  CLEAR gv_pos_managed.
*....select from storage type table

  SELECT SINGLE * FROM t331 INTO tmp_t331 WHERE lgnum = p_lgnum
                                          AND lgtyp = p_lgtyp
                                          AND stein = 'P'.
  IF sy-subrc = 0.
    gv_pos_managed = con_x.
  ENDIF.


ENDFORM.                               " STORAGE_TYPE_POS_CHECK
