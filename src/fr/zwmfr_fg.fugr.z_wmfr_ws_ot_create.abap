FUNCTION z_wmfr_ws_ot_create.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     VALUE(I_LENUM) TYPE  LENUM
*"     VALUE(I_BRGEW) TYPE  BRGEW OPTIONAL
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  SYSUBRC
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"     VALUE(ES_LTAK) TYPE  LTAK_VB
*"     VALUE(ET_LTAP) TYPE  TT_LTAP_VB
*"----------------------------------------------------------------------
  DATA lv_extno   TYPE balnrext.
  DATA lv_lgnum   TYPE lgnum.
  DATA lv_return  TYPE zwm_aux-retorno.
  DATA lv_message TYPE bapiret2-message.

  DATA ls_dblog   TYPE zwmfrt002.
  DATA ls_xuser   TYPE lrf_wkqu.
  DATA ls_ltap    TYPE ltap.
  DATA ls_return  TYPE bapiret2.
  DATA ls_zwm020  TYPE zwm020.

  DATA lo_ballog  TYPE REF TO zcl_ca_bal_log.

  FIELD-SYMBOLS <fs_return> LIKE LINE OF et_return[].
  FIELD-SYMBOLS <fs_balmsg> LIKE LINE OF lo_ballog->mt_logmsg[].
*"----------------------------------------------------------------------

  IF i_lgnum IS NOT SUPPLIED.
    PERFORM f_data_init_get_user_data CHANGING ls_xuser lv_return. " get user RF data for WM
    IF lv_return IS INITIAL.
      lv_lgnum  = ls_xuser-lgnum.
    ENDIF.
  ELSEIF i_lgnum IS INITIAL.
    PERFORM f_data_init_get_user_data CHANGING ls_xuser lv_return. " get user RF data for WM
    IF lv_return IS INITIAL.
      lv_lgnum  = ls_xuser-lgnum.
    ENDIF.
  ELSE.
    lv_lgnum  = i_lgnum.
  ENDIF.

  CONCATENATE lv_lgnum i_lenum INTO lv_extno.
  CONDENSE lv_extno.

  CREATE OBJECT lo_ballog
    EXPORTING
      i_log_obj  = c_balobj_zwm_fr
      i_log_sobj = c_balsubobj_interface
      i_extno    = lv_extno.

  GET TIME.
  GET TIME STAMP FIELD ls_dblog-tstam.
  ls_dblog-mandt  = sy-mandt.
  ls_dblog-lgnum  = lv_lgnum.
  ls_dblog-lenum  = i_lenum.
  ls_dblog-datum  = sy-datum.
  ls_dblog-uzeit  = sy-uzeit.
  ls_dblog-uname  = sy-uname.

  CALL FUNCTION 'ENQUEUE_EZ_ZWMFRT002'
    EXPORTING
      lgnum          = ls_dblog-lgnum
      lenum          = ls_dblog-lenum
      tstam          = ls_dblog-tstam
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc NE 0.
    lo_ballog->log_add_message( ).

    e_return = c_return_no_lock.
  ENDIF.

  PERFORM f_get_hardcodes USING lv_lgnum lo_ballog CHANGING e_return.
  PERFORM f_check_user_lgnum USING lv_lgnum lo_ballog CHANGING e_return.
  PERFORM f_check_pending_su USING lv_lgnum i_lenum lo_ballog CHANGING e_return es_ltak et_ltap[].



** Valida Palete remontada
***********************************************************************
  DO 1 TIMES.
    SELECT SINGLE * FROM zwm020
                    INTO ls_zwm020
                    WHERE armazem = lv_lgnum AND
                          ( p1 = i_lenum OR p2 = i_lenum ).

    CHECK sy-subrc EQ 0.

    IF i_lenum EQ ls_zwm020-p2.
      lo_ballog->log_add_message( ).
      e_return = c_return_no_lock.

**    Etiqueta de palete remontada & inválida, deve ler a etiqueta de baixo
      ls_return-id = 'ZWMFR001'.
      ls_return-number =  '074'.
      ls_return-type = 'E'.
      ls_return-message_v1 = i_lenum.


      MESSAGE ID  ls_return-id
            TYPE  ls_return-type
            NUMBER ls_return-number
            INTO ls_return-message
            WITH ls_return-message_v1 ls_return-message_v2
                 ls_return-message_v3 ls_return-message_v4.

      APPEND ls_return TO et_return.


      lo_ballog->log_add_message( i_msgty =  ls_return-type
                                  i_msgid = ls_return-id
                                  i_msgno = ls_return-number
                                  i_msgv1 = ls_return-message_v1
                                  i_msgv2 = ls_return-message_v2
                                  i_msgv3 = ls_return-message_v3
                                  i_msgv4 = ls_return-message_v4
                                 ).

    ENDIF.

  ENDDO.

**********************************************************************


  IF e_return EQ 0.
    PERFORM f_createto_move_su USING lo_ballog CHANGING ls_dblog es_ltak et_ltap[] e_return.
  ENDIF.

  IF e_return EQ c_return_pending_to.
    e_return  = 0.
  ENDIF.

  IF e_return EQ 0.
    LOOP AT et_ltap INTO ls_ltap.
      UPDATE ltap SET kzsub = abap_true
                  WHERE lgnum = ls_ltap-lgnum AND
                        tanum = ls_ltap-tanum AND
                        tapos = ls_ltap-tapos.
    ENDLOOP.

    COMMIT WORK AND WAIT.
  ENDIF.

  ls_dblog-logno  = lo_ballog->log_save( ).

  LOOP AT lo_ballog->mt_logmsg[] ASSIGNING <fs_balmsg>.
    CLEAR lv_message.

    MESSAGE ID <fs_balmsg>-msgid
      TYPE <fs_balmsg>-msgty
      NUMBER <fs_balmsg>-msgno
      WITH <fs_balmsg>-msgv1
           <fs_balmsg>-msgv2
           <fs_balmsg>-msgv3
           <fs_balmsg>-msgv4
           INTO lv_message.

    APPEND INITIAL LINE TO et_return[] ASSIGNING <fs_return>.
    <fs_return>-type        = <fs_balmsg>-msgty.
    <fs_return>-id          = <fs_balmsg>-msgid.
    <fs_return>-number      = <fs_balmsg>-msgno.
    <fs_return>-log_no      = ls_dblog-logno.
    <fs_return>-log_msg_no  = <fs_balmsg>-msg_count.
    <fs_return>-message_v1  = <fs_balmsg>-msgv1.
    <fs_return>-message_v2  = <fs_balmsg>-msgv2.
    <fs_return>-message_v3  = <fs_balmsg>-msgv3.
    <fs_return>-message_v4  = <fs_balmsg>-msgv4.
    <fs_return>-message     = lv_message.
  ENDLOOP.

  INSERT zwmfrt002 FROM ls_dblog.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.


  UPDATE zwm_log_efacec SET peso_pal = i_brgew WHERE sscc = i_lenum.
  COMMIT WORK.

  CALL FUNCTION 'DEQUEUE_EZ_ZWMFRT002'
    EXPORTING
      lgnum = ls_dblog-lgnum
      lenum = ls_dblog-lenum
      tstam = ls_dblog-tstam.
ENDFUNCTION.
