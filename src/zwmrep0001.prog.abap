REPORT RLOGON MESSAGE-ID LF.

***********************************************************************
*
*  LOGON
*
***********************************************************************
*
INCLUDE: RLMOBINC, MAINMENU.

START-OF-SELECTION.

  MESSAGE_LANG = SY-LANGU.
  SET PARAMETER ID 'MLV' FIELD ''.
  PERFORM LOGON_VALIDATE2.
  WHILE 1 = 1.
    GET PARAMETER ID 'MLV' FIELD WHS_ID.
    IF WHS_ID IS INITIAL.
      PERFORM USER_OWN_DATA.
      SET PARAMETER ID 'MLV' FIELD WHS_ID.
    ENDIF.
    IF SY-SUBRC <> 0.
      MESSAGE_VAR1 = SY-UNAME.
*     No user parameter found for user &1
      MESSAGE_NUMBER = '016'.
      PERFORM ERROR_MESSAGE.
      EXIT.
    ENDIF.
*   IMPORT ITB_MENU_STACK  FROM DATABASE INDX(WM) ID 'AMENU_STACK'.
*   LOOP AT ITB_MENU_STACK WHERE USER = SY-UNAME.
*      DELETE ITB_MENU_STACK INDEX SY-TABIX.
*   ENDLOOP.
*   EXPORT ITB_MENU_STACK  TO DATABASE INDX(WM) ID 'AMENU_STACK'.
    SET PARAMETER ID 'PREV_MENU' FIELD ''.
*   set parameter ID 'FIRST_MM'  FIELD '1'.
    MOVE-CORRESPONDING LRF_WKQU TO LOGON_DATA.
    IF DOCNUM = 0 OR SCREEN_TYPE >< DESTINATION_SCREEN.
      PERFORM CALL_SCREEN_LOGON.
    ENDIF.
    PERFORM CHECK_SCREEN_LOGON.
  ENDWHILE.

*---------------------------------------------------------------------*
*       FORM LOGON_VALIDATE2                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM LOGON_VALIDATE2.
  CALL FUNCTION 'L_MOB_USR_CHECK'
       EXPORTING
            IV_BNAME    = SY-UNAME
       IMPORTING
            EV_SESSIONS = LOGON_NO.
  IF LOGON_NO > 1.
    PERFORM USER_OWN_DATA.
*    CALL FUNCTION 'YWM_RF_CLEAN_LOST_SESSIONS'
*         EXPORTING
*              WAREHOUSE  = WHS_ID
*         EXCEPTIONS
*              DONT_LOGON = 1
*              OTHERS     = 2.
*
* User &1 logged on to client &2
*      MESSAGE_NUMBER = '193'.
*      MESSAGE_VAR1   = SY-UNAME.
*      MESSAGE_VAR2   = SY-MANDT.
*      PERFORM ERROR_MESSAGE.
*      ERROR_CODE = 9.
*      EXIT.

  ENDIF.
ENDFORM.                               " LOGON_VALIDATE
