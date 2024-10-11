FUNCTION zwm_confirm_to.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) TYPE  LGNUM
*"     REFERENCE(CONFIRM_TYPE) TYPE  CHAR1
*"     REFERENCE(SU) TYPE  LENUM OPTIONAL
*"  TABLES
*"      RETURN_MSG STRUCTURE  BDCMSGCOLL
*"  CHANGING
*"     REFERENCE(TO) TYPE  TANUM
*"     REFERENCE(TO_ITEM) TYPE  TAPOS
*"  EXCEPTIONS
*"      CONFIRM_TYPE_ERROR
*"      TO_NOT_FOUND
*"      TO_ALREADY_CONFIRMED
*"      TO_ALREADY_PICKED
*"      TO_NOT_PICKED
*"      WRONG_SU
*"      MISSING_SU
*"      ERROR
*"----------------------------------------------------------------------
  DATA: toheaderdata LIKE  bapitohead OCCURS 0 WITH HEADER LINE,
        toitemdata   LIKE  bapitoitem OCCURS 0 WITH HEADER LINE,
        extensionout LIKE  bapiparex OCCURS 0 WITH HEADER LINE,
        return       LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  DATA: bdc_tab LIKE bdcdata OCCURS 0 WITH HEADER LINE.
  DATA messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: opt LIKE ctu_params.
  DATA: aux_su(20).

  REFRESH: return_msg.
  CLEAR:   return_msg.

* check CONFIRM_TYPE
  IF confirm_type <> 'P'
     AND confirm_type <> 'T'
     AND confirm_type <> 'B'.
    RAISE confirm_type_error.
    EXIT.
  ENDIF.

** Valida se bloqueio à remessa
  IF NOT to IS INITIAL AND confirm_type EQ 'T'.
    PERFORM valida_bloq USING armazem
                              to.

  ENDIF.

** Check TO exist + status
*  CALL FUNCTION 'BAPI_WHSE_TO_GET_DETAIL'
*    EXPORTING
*      whsenumber        = armazem
*      transferorderno   = to
*      transferorderitem = to_item
*    TABLES
*      toheaderdata      = toheaderdata
*      toitemdata        = toitemdata
*      extensionout      = extensionout
*      return            = return.
*  IF sy-subrc = 0.
*    IF NOT toitemdata[] IS INITIAL.
*      READ TABLE toitemdata INDEX 1.
*
*
** check SU
*      IF  NOT su IS INITIAL AND
*  ( ( NOT toitemdata-src_su IS INITIAL AND toitemdata-src_su <> su ) OR
*   ( NOT toitemdata-dest_su IS INITIAL AND toitemdata-dest_su <> su )
*).
*        RAISE wrong_su.
*      ENDIF.
**      IF toitemdata-src_su IS INITIAL AND toitemdata-dest_su IS
*INITIAL
**         AND su IS INITIAL.
**        RAISE missing_su.
**      ENDIF.
*      IF toitemdata-src_su IS INITIAL AND toitemdata-dest_su IS INITIAL
*         AND NOT su IS INITIAL.
*        SELECT SINGLE * FROM ltap
*               WHERE lgnum = armazem
*                 AND tanum = to
*                 AND ( vlenr = su OR nlenr = su ).
*        IF sy-subrc = 0.
*          to_item = ltap-tapos.
*          CALL FUNCTION 'ZWM_CONFIRM_TO'
*            EXPORTING
*              warehouse            = armazem
*              confirm_type         = confirm_type
*              su                   = su
*            TABLES
*              return_msg           = return_msg
*            CHANGING
*              to                   = to
*              to_item              = to_item
*            EXCEPTIONS
*              confirm_type_error   = 1
*              to_not_found         = 2
*              to_already_confirmed = 3
*              to_already_picked    = 4
*              to_not_picked        = 5
*              wrong_su             = 6
*              missing_su           = 7
*              error                = 8
*              OTHERS               = 9.
*          IF sy-subrc <> 0.
*            CASE sy-subrc.
*              WHEN 1.
*                RAISE confirm_type_error.
*              WHEN 2.
*                RAISE to_not_found.
*              WHEN 3.
*                RAISE to_already_confirmed.
*              WHEN 4.
*                RAISE to_already_picked.
*              WHEN 5.
*                RAISE to_not_picked.
*              WHEN 6.
*                RAISE wrong_su.
*              WHEN 7.
*                RAISE missing_su.
*              WHEN 8.
*                RAISE error.
*              WHEN OTHERS.
*                RAISE error.
*            ENDCASE.
*          ELSE.
*            EXIT.
*          ENDIF.
*
*        ENDIF.
*      ENDIF.
** check confirmed
*      IF NOT toitemdata-confirm_ind IS INITIAL.
*
*        return_msg-msgid   = 'ZWMMSG001'.
*        return_msg-msgtyp  = 'I'.
*        return_msg-msgnr   = '041'.
*        return_msg-msgspra = sy-langu.
*        return_msg-msgv1   = to.
*        return_msg-msgv2   = to_item.
*        APPEND return_msg.
*
*        RAISE to_already_confirmed.
*        EXIT.
*      ENDIF.
*    ELSE.
*
*      return_msg-msgid = 'ZWMMSG001'.
*      return_msg-msgtyp = 'I'.
*      return_msg-msgnr = '042'.
*      return_msg-msgspra = sy-langu.
*      return_msg-msgv1   = to.
*      return_msg-msgv2   = to_item.
*      APPEND return_msg.
*
*      RAISE to_not_found.
*      EXIT.
*    ENDIF.
*  ELSE.
*
*    return_msg-msgid = 'ZWMMSG001'.
*    return_msg-msgtyp = 'I'.
*    return_msg-msgnr = '042'.
*    return_msg-msgspra = sy-langu.
*    return_msg-msgv1   = to.
*    return_msg-msgv2   = to_item.
*    APPEND return_msg.
*    RAISE to_not_found.
*    EXIT.
*  ENDIF.

*  IF confirm_type = 'B' OR confirm_type = 'P'.
** não pode ter pick
*    IF NOT toitemdata-mat_pick IS INITIAL.
*      return_msg-msgid = 'ZWMMSG001'.
*      return_msg-msgtyp = 'I'.
*      return_msg-msgnr = '043'.
*      return_msg-msgspra = sy-langu.
*      return_msg-msgv1   = to.
*      return_msg-msgv2   = to_item.
*      APPEND return_msg.
*
*      RAISE to_already_picked.
*      EXIT.
*    ENDIF.
*  ELSEIF confirm_type = 'T'.
** Tem de ter pick
*    IF toitemdata-mat_pick IS INITIAL.
*      return_msg-msgid = 'ZWMMSG001'.
*      return_msg-msgtyp = 'I'.
*      return_msg-msgnr = '044'.
*      return_msg-msgspra = sy-langu.
*      return_msg-msgv1   = to.
*      return_msg-msgv2   = to_item.
*      APPEND return_msg.
*      RAISE to_not_picked.
*      EXIT.
*    ENDIF.
*  ENDIF.

* confirm TO with SU
    PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0121'.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-LGNUM' armazem.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'LTAK-TANUM' to.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-TAPOS' to_item.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-DUNKL' 'D'.
*PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-DUNKL' 'H'.

*  PERFORM DYNPRO TABLES BDC_TAB USING ' ' 'RL03T-DUNKL' ' '.

    IF confirm_type = 'P'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-QENTN' 'X'.
    ELSEIF confirm_type = 'T'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-QTRAN' 'X'.
    ELSEIF confirm_type = 'B'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-QENTR' 'X'.
    ENDIF.
    PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.


    IF su IS INITIAL OR
       NOT toitemdata-src_su IS INITIAL OR
       NOT toitemdata-dest_su IS INITIAL.

      PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0112'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'RL03T-NQUIT' 'X'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

    ELSE.
      WRITE su TO aux_su.

      PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0180'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'LQBL-LENUM(01)' aux_su.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '/00'.

      PERFORM dynpro TABLES bdc_tab USING 'X' 'SAPML03T' '0180'.
      PERFORM dynpro TABLES bdc_tab USING ' ' 'BDC_OKCODE' '=QUD'.

    ENDIF.

    opt-defsize = 'X'.
    opt-dismode = 'N'.   "No Display
    opt-updmode = 'S'.   "Update Sincrono

    IF confirm_type = 'P'.
      CALL TRANSACTION 'LT1B' USING bdc_tab
                                      OPTIONS FROM opt
                                      MESSAGES INTO messtab.
    ELSEIF confirm_type = 'T'.
      CALL TRANSACTION 'LT1C' USING bdc_tab
                                      OPTIONS FROM opt
                                      MESSAGES INTO messtab.
    ELSEIF confirm_type = 'B'.
      CALL TRANSACTION 'LT11' USING bdc_tab
                                      OPTIONS FROM opt
                                      MESSAGES INTO messtab.
    ENDIF.

    IF sy-subrc = 0.
      LOOP AT messtab.
        APPEND messtab TO return_msg.
      ENDLOOP.
    ELSE.
      LOOP AT messtab WHERE msgtyp = 'E' .
        APPEND messtab TO return_msg.
        RAISE error.
        EXIT.
      ENDLOOP.
    ENDIF.

    SELECT SINGLE * FROM ltap
           WHERE lgnum = armazem
             AND tanum = to
             AND ( vlenr = su OR nlenr = su ).
    IF sy-subrc = 0.
      IF NOT su IS INITIAL.
        MOVE ltap-tapos TO to_item.
      ENDIF.
*********    RAISE error.
    ELSE.

*    RAISE error.
    ENDIF.

ENDFUNCTION.
