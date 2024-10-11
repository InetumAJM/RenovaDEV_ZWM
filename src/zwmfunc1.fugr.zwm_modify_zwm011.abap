FUNCTION zwm_modify_zwm011.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) TYPE  LGNUM
*"     REFERENCE(TO_NUMBER) TYPE  TANUM
*"     REFERENCE(TO_ITEM) TYPE  TAPOS
*"     REFERENCE(STATUS) TYPE  CHAR1
*"     REFERENCE(SU) TYPE  LENUM OPTIONAL
*"  EXCEPTIONS
*"      ERROR_UPDATE_TABLE
*"----------------------------------------------------------------------
  DATA: izwm011 TYPE zwm011 OCCURS 0 WITH HEADER LINE.

* fazer lock antes de actualizar a tabela dos user´s
  DO.
    CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'ZWM011'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

  REFRESH: izwm011.
  CLEAR: izwm011.

  SELECT * FROM zwm011 INTO TABLE izwm011
      WHERE armazem   = armazem AND
            to_number = to_number AND
            to_item   = to_item AND
            user_name = sy-uname.

  IF sy-subrc <> 0.
    SELECT * FROM zwm011 INTO TABLE izwm011
        WHERE armazem   = armazem AND
              to_number = to_number AND
              user_name = sy-uname.

    IF sy-subrc = 0.
      READ TABLE izwm011 INDEX 1.

      DELETE zwm011 FROM TABLE izwm011.
      COMMIT WORK AND WAIT .

      izwm011-to_item = to_item.
      MODIFY izwm011 INDEX 1.

      INSERT zwm011 FROM TABLE izwm011.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDIF.

  REFRESH: izwm011.
  CLEAR: izwm011.

  SELECT * FROM zwm011 INTO TABLE izwm011
      WHERE armazem   = armazem AND
            to_number = to_number AND
            to_item   = to_item AND
            user_name = sy-uname.

  READ TABLE izwm011 INDEX 1.
  IF sy-subrc = 0.
    izwm011-status = status.
    izwm011-su     = su.
    MODIFY izwm011 INDEX 1.
  ELSE.
    CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'ZWM011'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    RAISE error_update_table.
  ENDIF.

  MODIFY zwm011 FROM TABLE izwm011.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
  ELSE.
    CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
      EXPORTING
        mode_keyword   = 'X'
        keyword_       = 'ZWM011'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    ROLLBACK WORK.
    RAISE error_update_table.
  ENDIF.

  DO 5 TIMES.
    SELECT SINGLE * FROM zwm011
    WHERE armazem   = armazem AND
              to_number = to_number AND
              to_item   = to_item AND
              user_name = sy-uname AND
              status = status.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

  CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
    EXPORTING
      mode_keyword   = 'X'
      keyword_       = 'ZWM011'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
ENDFUNCTION.
