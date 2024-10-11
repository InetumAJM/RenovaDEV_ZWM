FUNCTION ZWM_PRINT_BOX_SSCC_LABEL.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(LABEL) TYPE  ZWM_PRINT_LABEL_DETAILS
*"  CHANGING
*"     REFERENCE(ITCPO) TYPE  ITCPO
*"----------------------------------------------------------------------


  CALL FUNCTION 'OPEN_FORM'
   EXPORTING
*   APPLICATION                       = 'X'
*   ARCHIVE_INDEX                     =
*   ARCHIVE_PARAMS                    =
     DEVICE                            = 'PRINTER'
*   DIALOG                            = 'X'
     FORM                              = 'ZWM_SSCC_CAIXA'
     LANGUAGE                          = 'P'
     OPTIONS                           = itcpo
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    exit.
  ENDIF.

  MOVE-CORRESPONDING ITCPP TO ITCPO.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
         ELEMENT                  = ' '
         FUNCTION                 = 'DELETE'
         TYPE                     = 'BODY'
         WINDOW                   = 'MAIN'
*    IMPORTING
*         PENDING_LINES            =
   EXCEPTIONS
        ELEMENT                  = 1
        FUNCTION                 = 2
        TYPE                     = 3
        UNOPENED                 = 4
        UNSTARTED                = 5
        WINDOW                   = 6
        BAD_PAGEFORMAT_FOR_PRINT = 7
        OTHERS                   = 8.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    EXIT.
  ENDIF.
  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
      OTHERS = 1.
  IF SY-SUBRC NE 0.
*    retcode = sy-subrc.
  ENDIF.


ENDFUNCTION.
