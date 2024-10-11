FUNCTION z_wm_shipment_set_status.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TKNUM) TYPE  TKNUM
*"     REFERENCE(I_STATUS) TYPE  INT1 OPTIONAL
*"     REFERENCE(I_STATUS_FORCE) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_WAIT) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  CONSTANTS: gc_mod_type TYPE bapishipmentchangeaction VALUE 'C',
              gc_del_type TYPE bapishipmentchangeaction VALUE 'D'.

  DATA: lt_return   TYPE TABLE OF bapiret2,
        lt_vttk     TYPE TABLE OF vttk,
        lt_vttp     TYPE TABLE OF vttp,
        lt_vttp_pai TYPE TABLE OF vttp.

  DATA: ls_vttk             TYPE vttk,
        ls_return           TYPE bapiret2,
        ls_message          TYPE bdcmsgcoll,
        ls_headerdata       TYPE bapishipmentheader,
        ls_headerdataaction TYPE bapishipmentheaderaction.

  DATA: lv_actual_status    TYPE sttrg,
        lv_status_reference TYPE sttrg,
        lv_staus            TYPE sttrg,
        lv_num_status       TYPE i.

  CLEAR: et_messages.

  CALL FUNCTION 'Z_WM_DEQUEUE_TKNUM_WAIT'
    EXPORTING
      i_tknum = i_tknum.


************************************************************************

  ls_headerdata-shipment_num = i_tknum.
  ls_headerdataaction-shipment_num = 'X'.

** Calcula satus a aplicar
**********************************************************************
  SELECT SINGLE sttrg FROM vttk
                      INTO lv_actual_status
                      WHERE tknum = i_tknum.


  IF i_status_force = 'X'.
    lv_num_status = i_status - lv_actual_status.
    lv_status_reference  = lv_actual_status + 1.
  ELSE.
    lv_num_status = 1.
    lv_status_reference  = i_status.
  ENDIF.

  IF lv_num_status < 0.
    lv_num_status = lv_num_status * -1.

    DO lv_num_status TIMES.

      lv_staus = lv_status_reference - ( sy-index ).

      CASE lv_staus.
        WHEN 1.
          ls_headerdata-status_plan = ''.
          ls_headerdataaction-status_plan = gc_del_type.
        WHEN 2.
          ls_headerdata-status_checkin = ''.
          ls_headerdataaction-status_checkin = gc_del_type.
        WHEN 3.
          ls_headerdata-status_load_start = ''.
          ls_headerdataaction-status_load_start = gc_del_type.
        WHEN 4.
          ls_headerdata-status_load_end = ''.
          ls_headerdataaction-status_load_end = gc_del_type.
        WHEN 5.
          ls_headerdata-status_compl = ''.
          ls_headerdataaction-status_compl = gc_del_type.
        WHEN 6.
          ls_headerdata-status_shpmnt_start = ''.
          ls_headerdataaction-status_shpmnt_start = gc_del_type.
        WHEN 7.
          ls_headerdata-status_shpmnt_end = ''.
          ls_headerdataaction-status_shpmnt_end = gc_del_type.
      ENDCASE.

    ENDDO.

  ELSE.

    DO lv_num_status TIMES.

      lv_staus = lv_status_reference + ( sy-index - 1 ).

      CASE lv_staus.
        WHEN 1.
          ls_headerdata-status_plan = 'X'.
          ls_headerdataaction-status_plan = gc_mod_type.
        WHEN 2.
          ls_headerdata-status_checkin = 'X'.
          ls_headerdataaction-status_checkin = gc_mod_type.
        WHEN 3.
          ls_headerdata-status_load_start = 'X'.
          ls_headerdataaction-status_load_start = gc_mod_type.
        WHEN 4.
          ls_headerdata-status_load_end = 'X'.
          ls_headerdataaction-status_load_end = gc_mod_type.
        WHEN 5.
          ls_headerdata-status_compl = 'X'.
          ls_headerdataaction-status_compl = gc_mod_type.
        WHEN 6.
          ls_headerdata-status_shpmnt_start = 'X'.
          ls_headerdataaction-status_shpmnt_start = gc_mod_type.
        WHEN 7.
          ls_headerdata-status_shpmnt_end = 'X'.
          ls_headerdataaction-status_shpmnt_end = gc_mod_type.
      ENDCASE.

    ENDDO.

  ENDIF.



  CALL FUNCTION 'BAPI_SHIPMENT_CHANGE'
    EXPORTING
      headerdata       = ls_headerdata
      headerdataaction = ls_headerdataaction
    TABLES
      return           = lt_return.

**  Remove Bloqueios
*  PERFORM dequeue_tknum USING i_tknum.

  LOOP AT lt_return INTO ls_return WHERE type EQ 'E'
                                      OR type EQ 'A'.
    ROLLBACK WORK.
    ls_message-msgid  = ls_return-id.
    ls_message-msgtyp = ls_return-type.
    ls_message-msgnr  = ls_return-number.
    ls_message-msgv1  = ls_return-message_v1.
    ls_message-msgv2  = ls_return-message_v2.
    ls_message-msgv3  = ls_return-message_v3.
    ls_message-msgv4  = ls_return-message_v4.
    APPEND ls_message TO et_messages.
    CLEAR  ls_message.
  ENDLOOP.
  IF sy-subrc = 0.
    RAISE error.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ROLLBACK WORK.

  CHECK i_wait = 'X'.

  DO 20 TIMES.
    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    SELECT SINGLE * FROM vttk
                    WHERE tknum = i_tknum AND
                          sttrg = i_status.
    IF sy-subrc = 0.
      EXIT.
    ENDIF.
  ENDDO.

  CALL FUNCTION 'Z_WM_DEQUEUE_TKNUM_WAIT'
    EXPORTING
      i_tknum = i_tknum.
ENDFUNCTION.
