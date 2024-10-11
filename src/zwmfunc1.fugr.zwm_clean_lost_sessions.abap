FUNCTION ZWM_CLEAN_LOST_SESSIONS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WAREHOUSE) TYPE  LGNUM
*"  EXCEPTIONS
*"      DONT_LOGON
*"----------------------------------------------------------------------
* determine current user session
  DATA: WHO_AM_I type USER_ID.

*  CALL 'ThUsrInfo' ID 'OPCODE' FIELD OPCODE_USR_ATTR
*    ID 'TID' FIELD WHO_AM_I-TERMINAL_ID
*    ID 'HOSTADDR' FIELD WHO_AM_I-HOST_ADDR.
*  WHO_AM_I-USER_NAME = SY-UNAME.

* DETERMINE ONLINE USERS ***********************************************
  DATA: X_USR41 LIKE USR41 OCCURS 0 WITH HEADER LINE.
  DATA: W_USR41 LIKE USR41 OCCURS 0 WITH HEADER LINE.
  DATA: IP(30), HOST(30), SESSION_HOST(30).
  PERFORM GET_PARAMETER
        USING WAREHOUSE
              'GERAL'
              'HOST'
              HOST.

  CHECK NOT HOST IS INITIAL.
  SELECT * FROM USR41 INTO TABLE X_USR41
           WHERE BNAME = SY-UNAME.
*
* delete this session
*  DELETE X_USR41 WHERE TERMID = WHO_AM_I-TERMINAL_ID.

* delete sessions not in host
  LOOP AT X_USR41.
    CLEAR: IP, SESSION_HOST.
    SPLIT X_USR41-TERMINAL AT '-' INTO IP SESSION_HOST.

    CHECK SESSION_HOST = HOST.
    APPEND X_USR41 TO W_USR41.
  ENDLOOP.

  FREE X_USR41.

* logoff other sessions for this user
  DATA: LOCK_KEY(50), WAIT_TIME(50).
  DATA: MSGV1 LIKE BDCMSGCOLL-MSGV1.
  DATA: RETCODE.

  PERFORM GET_PARAMETER
          USING WAREHOUSE
                'GERAL'
                'LOCK_WAREHOUSE'
                LOCK_KEY.
  PERFORM GET_PARAMETER
          USING WAREHOUSE
                'GERAL'
                'WAIT_TIME'
                WAIT_TIME.

  WRITE SY-UNAME TO MSGV1.
  LOOP AT W_USR41.
    AT FIRST.
*      CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
*           EXPORTING
*                MESSAGE_ID     = 'ZWMMSG001'
*                MESSAGE_LANG   = SY-LANGU
*                MESSAGE_TYPE   = 'W'
*                MESSAGE_NUMBER = '050'
*                MESSAGE_VAR1   = MSGV1
*           IMPORTING
*                RET_CODE       = RETCODE.
      IF RETCODE = 'C'.
* EXIT SAP CONSOLE
        SET SCREEN 0.
        LEAVE SCREEN.
        RAISE DONT_LOGON.
      ENDIF.

      DO.
        CALL FUNCTION 'ENQUEUE_ET_KEYWORD'
             EXPORTING
                  MODE_KEYWORD   = 'X'
                  KEYWORD_       = LOCK_KEY
             EXCEPTIONS
                  FOREIGN_LOCK   = 1
                  SYSTEM_FAILURE = 2
                  OTHERS         = 3.
        IF SY-SUBRC = 0.
          EXIT.
        ELSE.
          WAIT UP TO WAIT_TIME SECONDS.
        ENDIF.
      ENDDO.
    ENDAT.

*    CALL 'ThUsrInfo' ID 'OPCODE' FIELD OPCODE_DELETE_USR
*                   ID 'TID' FIELD W_USR41-TERMID.

    AT LAST.
* unlock warehouse
      CALL FUNCTION 'DEQUEUE_ET_KEYWORD'
           EXPORTING
                MODE_KEYWORD   = 'X'
                KEYWORD_       = LOCK_KEY
           EXCEPTIONS
                FOREIGN_LOCK   = 1
                SYSTEM_FAILURE = 2
                OTHERS         = 3.
    ENDAT.
  ENDLOOP.





ENDFUNCTION.
