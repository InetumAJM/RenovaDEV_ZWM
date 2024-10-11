FUNCTION ZWM_CANCEL_TO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) TYPE  LGNUM
*"  TABLES
*"      T_LTAP_CANCL STRUCTURE  LTAP_CANCL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  READ TABLE T_LTAP_CANCL INDEX 1.

  CALL FUNCTION 'L_TO_CANCEL'
    EXPORTING
     I_LGNUM                            = ARMAZEM
     I_TANUM                            = T_LTAP_CANCL-TANUM
*    I_SOLEX                            = 0
*    I_CANCL                            = ' '
*    I_SUBST                            = ' '
     I_QNAME                            = SY-UNAME
     I_UPDATE_TASK                      = ' '
     I_COMMIT_WORK                      = 'X'
    TABLES
      T_LTAP_CANCL                       = T_LTAP_CANCL
   EXCEPTIONS
     TO_CONFIRMED                       = 1
     TO_DOESNT_EXIST                    = 2
     ITEM_CONFIRMED                     = 3
     ITEM_DOESNT_EXIST                  = 4
     FOREIGN_LOCK                       = 5
     DOUBLE_LINES                       = 6
     NOTHING_TO_DO                      = 7
     XFELD_WRONG                        = 8
     SU_MOVEMENT_PARTLY_CONFIRMED       = 9
     UPDATE_WITHOUT_COMMIT              = 10
     NO_AUTHORITY                       = 11
     OTHERS                             = 12
            .
  IF SY-SUBRC <> 0.
    RAISE ERROR.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFUNCTION.
