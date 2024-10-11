FUNCTION ZWM_TO_CONFIRM .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TANUM) TYPE  TANUM
*"     REFERENCE(I_TAPOS) TYPE  TAPOS OPTIONAL
*"     REFERENCE(I_LENUM) TYPE  LENUM OPTIONAL
*"     REFERENCE(I_QUKNZ) TYPE  RL03TQUKNZ OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_TAPOS) TYPE  TAPOS
*"     REFERENCE(E_LETYP) TYPE  LVS_LETYP
*"     REFERENCE(E_VSOLM) TYPE  LTAP_VSOLM
*"     REFERENCE(E_NSOLM) TYPE  LTAP_NSOLM
*"     REFERENCE(E_DELETED) TYPE  FLAG
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------


***********************************************************************
** I_QUKNZ Pode tomar os valores:
** ' ' -> Pick & Transfer se Movimento Parametrizado como 1 só Passo
** '1' -> Pick
** '2' -> Transfer
** '3' -> Apenas Informação (?)
** '4' -> Pick & Transfer num só Passo mesmo que o Movimento esteja
**        parametrizado como Pick & Transfer em separado. Não está
**        visivel esta opção mas o código contempla-a.
**        INCLUDE: LLHUMU07 (Nota: 1249275)
***********************************************************************

  TYPES: BEGIN OF lty_ltap,
           vlenr TYPE ltap-vlenr,
           nlenr TYPE ltap-nlenr,
           tanum TYPE ltap-tanum,
           tapos TYPE ltap-tapos,
           letyp TYPE ltap-letyp,
           vorga TYPE ltap-vorga,
           vsolm TYPE ltap-vsolm,
           nsolm TYPE ltap-nsolm,
         END OF lty_ltap.

  DATA: lt_ltap      TYPE TABLE OF ltap,
        ls_ltap      TYPE ltap,
        lt_ltap_aux  TYPE TABLE OF ltap,
        ls_ltap_aux  TYPE ltap,
        lt_ltap_canc TYPE TABLE OF ltap,
        ls_ltap_canc TYPE ltap,
        lt_lqua      TYPE TABLE OF lqua,
        ls_lqua      TYPE lqua,
        lt_ltap_conf TYPE TABLE OF ltap_conf,
        ls_ltap_conf TYPE ltap_conf,
        lt_ltap_find TYPE TABLE OF lty_ltap,
        ls_ltap_find TYPE lty_ltap,
        lv_to_qty    TYPE menge_d,
        lv_stein     TYPE t331-stein,
        lv_menge_to  TYPE menge_d,
        lv_menge_bin TYPE menge_d,
        lv_quknz     TYPE rl03tquknz,
        lv_ret_code  TYPE char01,
        lv_benum     TYPE ltak-benum,
        ls_message   TYPE bdcmsgcoll.

***********************************************************************
  CLEAR: et_messages, e_tapos, e_letyp, e_deleted, e_vsolm, e_nsolm.

  CHECK NOT i_lgnum IS INITIAL AND
        NOT i_tanum IS INITIAL.

  GET TIME.


** Dados Tarefas
***********************************************************************
  IF NOT i_tapos IS INITIAL.
    SELECT * FROM ltap INTO TABLE lt_ltap
     WHERE lgnum EQ i_lgnum
       AND tanum EQ i_tanum
       AND tapos EQ i_tapos.
  ELSE.
**  Obtem Todos os Items
    SELECT * FROM ltap INTO TABLE lt_ltap
     WHERE lgnum EQ i_lgnum
       AND tanum EQ i_tanum.
  ENDIF.

  DELETE lt_ltap WHERE pquit NE ''
                    OR vorga EQ 'ST'
                    OR vorga EQ 'SL'.

  CHECK NOT lt_ltap IS INITIAL.

  SORT lt_ltap BY werks lgort matnr charg.



** Items
***********************************************************************
  LOOP AT lt_ltap INTO ls_ltap.
    CLEAR: ls_lqua.

    ls_ltap_conf-tanum = ls_ltap-tanum.
    ls_ltap_conf-tapos = ls_ltap-tapos.

    lv_to_qty = ls_ltap-vsola - ls_ltap-vista.

    IF NOT i_lenum IS INITIAL.
      READ TABLE lt_lqua INTO ls_lqua WITH KEY matnr = ls_ltap-matnr
                                               charg = ls_ltap-charg
                                      BINARY SEARCH.

      ls_ltap_conf-lenum = i_lenum.
*      IF ls_lqua-gesme >= lv_to_qty.
      ls_ltap_conf-pickm = ls_lqua-gesme.
*      ELSE.
*        ls_ltap_conf-pickm = lv_to_qty.
*      ENDIF.
      ls_ltap_conf-altme = ls_ltap-altme.

    ELSE.
      ls_ltap_conf-squit = 'X'.
    ENDIF.

    APPEND ls_ltap_conf TO lt_ltap_conf.
    CLEAR  ls_ltap_conf.
  ENDLOOP.

  CHECK NOT lt_ltap_conf IS INITIAL.

** Verifica se já Pick
***********************************************************************
  lv_quknz = i_quknz.
  IF lv_quknz IS INITIAL OR lv_quknz EQ '4'.
    READ TABLE lt_ltap WITH KEY pvqui = 'X' TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      lv_quknz = '2'.
    ENDIF.
  ENDIF.


** Confirma Tarefas
***********************************************************************
  CALL FUNCTION 'L_TO_CONFIRM'
    EXPORTING
      i_lgnum                        = i_lgnum
      i_tanum                        = i_tanum
*     I_SQUIT                        = ' '
      i_quknz                        = lv_quknz
*     I_SUBST                        = ' '
*     I_QNAME                        = SY-UNAME
*     I_ENAME                        = SY-UNAME
*     I_SOLEX                        = 0
*     I_PERNR                        = 0
*     I_STDAT                        = INIT_DATUM
*     I_STUZT                        = 0
*     I_ENDAT                        = INIT_DATUM
*     I_ENUZT                        = 0
*     I_ISTWM                        = 0
*     I_KOMIM                        = ' '
*     I_EINLM                        = ' '
*     I_TBELI                        = ' '
*     I_UPDATE_TASK                  = ' '
*     I_COMMIT_WORK                  = 'X'
*     I_AUSFB                        = ' '
    TABLES
      t_ltap_conf                    = lt_ltap_conf
*     T_LTAP_CONF_HU                 =
*     T_LTAP_CONF_HU_SERIAL          =
    EXCEPTIONS
      to_confirmed                   = 1
      to_doesnt_exist                = 2
      item_confirmed                 = 3
      item_subsystem                 = 4
      item_doesnt_exist              = 5
      item_without_zero_stock_check  = 6
      item_with_zero_stock_check     = 7
      one_item_with_zero_stock_check = 8
      item_su_bulk_storage           = 9
      item_no_su_bulk_storage        = 10
      one_item_su_bulk_storage       = 11
      foreign_lock                   = 12
      squit_or_quantities            = 13
      vquit_or_quantities            = 14
      bquit_or_quantities            = 15
      quantity_wrong                 = 16
      double_lines                   = 17
      kzdif_wrong                    = 18
      no_difference                  = 19
      no_negative_quantities         = 20
      wrong_zero_stock_check         = 21
      su_not_found                   = 22
      no_stock_on_su                 = 23
      su_wrong                       = 24
      too_many_su                    = 25
      nothing_to_do                  = 26
      no_unit_of_measure             = 27
      xfeld_wrong                    = 28
      update_without_commit          = 29
      no_authority                   = 30
      lqnum_missing                  = 31
      charg_missing                  = 32
      no_sobkz                       = 33
      no_charg                       = 34
      nlpla_wrong                    = 35
      two_step_confirmation_required = 36
      two_step_conf_not_allowed      = 37
      pick_confirmation_missing      = 38
      quknz_wrong                    = 39
      hu_data_wrong                  = 40
      no_hu_data_required            = 41
      hu_data_missing                = 42
      hu_not_found                   = 43
      picking_of_hu_not_possible     = 44
      not_enough_stock_in_hu         = 45
      serial_number_data_wrong       = 46
      serial_numbers_not_required    = 47
      no_differences_allowed         = 48
      serial_number_not_available    = 49
      serial_number_data_missing     = 50
      to_item_split_not_allowed      = 51
      input_wrong                    = 52
      error_message                  = 53
      OTHERS                         = 54.

  IF sy-subrc <> 0.
    ls_message-msgid  = sy-msgid.
    ls_message-msgtyp = sy-msgty.
    ls_message-msgnr  = sy-msgno.
    ls_message-msgv1  = sy-msgv1.
    ls_message-msgv2  = sy-msgv2.
    ls_message-msgv3  = sy-msgv3.
    ls_message-msgv4  = sy-msgv4.
    APPEND ls_message TO et_messages.

    ROLLBACK WORK.
    RAISE error.

  ELSE.
**  Elimina Bloqueio da LTAK que fica ativo
    ROLLBACK WORK.

  ENDIF.


ENDFUNCTION.
