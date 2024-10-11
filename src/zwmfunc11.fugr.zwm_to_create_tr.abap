FUNCTION zwm_to_create_tr.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TBNUM) TYPE  TBNUM
*"     REFERENCE(I_SQUIT) TYPE  RL03TSQUIT OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(IT_TRITE) TYPE  L03B_TRITE_T OPTIONAL
*"     REFERENCE(I_WAIT) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(E_TANUM) TYPE  TANUM
*"     REFERENCE(ET_LTAK) TYPE  PDT_T_LTAK_VB
*"     REFERENCE(ET_LTAP) TYPE  PDT_T_LTAP_VB
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lt_ltak TYPE TABLE OF ltak,
         lt_ltap TYPE TABLE OF ltap,
         lt_ltbp TYPE TABLE OF ltbp.

  DATA: ls_message TYPE bdcmsgcoll,
        ls_ltak    TYPE ltak,
        ls_ltap    TYPE ltap,
        ls_ltbp    TYPE ltbp.

  DATA: lv_tabix TYPE sytabix,
        lv_tanum TYPE tanum,
        lv_lines TYPE sytabix.

***********************************************************************
  CLEAR: et_messages, e_tanum, et_ltak, et_ltap.


** Cria Tarefa
***********************************************************************
  CALL FUNCTION 'L_TO_CREATE_TR'
    EXPORTING
      i_lgnum                        = i_lgnum
      i_tbnum                        = i_tbnum
*     I_REFNR                        = ' '
      i_squit                        = i_squit
*     I_NIDRU                        = ' '
*     I_DRUKZ                        = ' '
*     I_LDEST                        = ' '
*     I_TBELI                        = ' '
*     I_NOSPL                        = ' '
*     I_UPDATE_TASK                  = ' '
      i_commit_work                  = i_commit
*     I_BNAME                        = SY-UNAME
*     I_TEILK                        = ' '
      i_teilk                        = 'X'
*     I_SOLEX                        = 0
*     I_PERNR                        = 0
*     I_RSNUM                        = ' '
*     I_LDEST_LANG                   = ' '
      it_trite                       = it_trite
    IMPORTING
      e_tanum                        = e_tanum
*     E_TEILK                        =
    TABLES
      t_ltak                         = et_ltak
      t_ltap_vb                      = et_ltap
*     T_WMGRP_MSG                    =
    EXCEPTIONS
      foreign_lock                   = 1
      qm_relevant                    = 2
      tr_completed                   = 3
      xfeld_wrong                    = 4
      ldest_wrong                    = 5
      drukz_wrong                    = 6
      tr_wrong                       = 7
      squit_forbidden                = 8
      no_to_created                  = 9
      update_without_commit          = 10
      no_authority                   = 11
      preallocated_stock             = 12
      partial_transfer_req_forbidden = 13
      input_error                    = 14
      error_message                  = 15
      OTHERS                         = 16.


** Processamento de Erros
***********************************************************************
  IF sy-subrc <> 0.

    IF sy-msgid EQ 'L3' AND
       sy-msgno EQ '332'.

      SELECT * FROM ltbp
         INTO TABLE lt_ltbp
         WHERE lgnum = i_lgnum AND
               tbnum = i_tbnum.

      DESCRIBE TABLE lt_ltbp LINES lv_lines.

      IF lv_lines EQ 1.
        READ TABLE lt_ltbp
              INTO ls_ltbp
              INDEX 1.
**      Não foi possivel criar TO para TR & Item & Material &
        ls_message-msgid  = 'ZWM001'.
        ls_message-msgtyp = 'E'.
        ls_message-msgnr  = '052'.
        ls_message-msgv1  = i_tbnum.
        ls_message-msgv2  = ls_ltbp-tbpos.
        ls_message-msgv3  = ls_ltbp-matnr.
        APPEND ls_message TO et_messages.
      ELSE.
**      Não foi possivel criar TO para TR &
        ls_message-msgid  = 'ZWM001'.
        ls_message-msgtyp = 'E'.
        ls_message-msgnr  = '053'.
        ls_message-msgv1  = i_tbnum.
        APPEND ls_message TO et_messages.
      ENDIF.
    ELSE.
      ls_message-msgid  = sy-msgid.
      ls_message-msgtyp = sy-msgty.
      ls_message-msgnr  = sy-msgno.
      ls_message-msgv1  = sy-msgv1.
      ls_message-msgv2  = sy-msgv2.
      ls_message-msgv3  = sy-msgv3.
      ls_message-msgv4  = sy-msgv4.
      APPEND ls_message TO et_messages.
    ENDIF.

**  Retira os Bloqueios da Posição
    IF i_commit EQ abap_true.
      ROLLBACK WORK.
    ENDIF.
    RAISE error.
  ENDIF.

*  IF i_commit IS INITIAL.
***  Retira os Bloqueios da Posição
*    ROLLBACK WORK.
*  ENDIF.

  CHECK i_wait EQ abap_true AND i_commit EQ abap_true.

  lt_ltak = et_ltak.
  lt_ltap = et_ltap.

  DO 20 TIMES.
    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    LOOP AT lt_ltak INTO ls_ltak.
      lv_tabix = sy-tabix.

      SELECT SINGLE tanum FROM ltak
                          INTO lv_tanum
                          BYPASSING BUFFER
                          WHERE lgnum = ls_ltak-lgnum AND
                                tanum = ls_ltak-tanum.
      CHECK sy-subrc EQ 0.

      DELETE lt_ltak INDEX lv_tabix.
    ENDLOOP.

    LOOP AT lt_ltap INTO ls_ltap.
      lv_tabix = sy-tabix.

      SELECT SINGLE tanum FROM ltap
                          INTO lv_tanum
                          BYPASSING BUFFER
                          WHERE lgnum = ls_ltap-lgnum AND
                                tanum = ls_ltap-tanum AND
                                tapos = ls_ltap-tapos.
      CHECK sy-subrc EQ 0.

      DELETE lt_ltap INDEX lv_tabix.
    ENDLOOP.

    IF lt_ltak IS INITIAL AND
       lt_ltap IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.

ENDFUNCTION.
