FUNCTION z_wmfr_deelivery_change.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_VBKOK_WA) TYPE  VBKOK
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"     REFERENCE(I_COMMIT) TYPE  FLAG
*"     REFERENCE(IT_VBPOK) TYPE  TT_VBPOK
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: lt_vbpok TYPE TABLE OF vbpok.

  lt_vbpok = it_vbpok.


  CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
    EXPORTING
      vbkok_wa                           = is_vbkok_wa
*   SYNCHRON                           = ' '
*   NO_MESSAGES_UPDATE_1               = ' '
      commit                             = i_commit
      delivery                           = i_vbeln
*   UPDATE_PICKING                     = ' '
*   NICHT_SPERREN_1                    = ' '
*   IF_CONFIRM_CENTRAL                 = ' '
*   IF_WMPP                            = ' '
*   IF_GET_DELIVERY_BUFFERED           = ' '
*   IF_NO_GENERIC_SYSTEM_SERVICE       = ' '
*   IF_DATABASE_UPDATE_1               = '1'
*   IF_NO_INIT_1                       = ' '
*   IF_NO_READ_1                       = ' '
*   IF_ERROR_MESSAGES_SEND             = 'X'
*   IF_NO_BUFFER_REFRESH               = ' '
*   IT_PARTNER_UPDATE                  =
*   IT_SERNR_UPDATE                    =
*   IF_NO_REMOTE_CHG_1                 = ' '
*   IF_NO_MES_UPD_PACK                 = ' '
*   IF_LATE_DELIVERY_UPD               = ' '
*   IF_TXT_REINITIALIZE                =
*   IF_BOR_INIT                        = ' '
*   SPE_MES_NO_SEND_NODIAL             =
* IMPORTING
*   EF_ERROR_ANY                       =
*   EF_ERROR_IN_ITEM_DELETION          =
*   EF_ERROR_IN_POD_UPDATE             =
*   EF_ERROR_IN_INTERFACE              =
*   EF_ERROR_IN_GOODS_ISSUE            =
*   EF_ERROR_IN_FINAL_CHECK            =
*   EF_ERROR_PARTNER_UPDATE            =
*   EF_ERROR_SERNR_UPDATE              =
   TABLES
     vbpok_tab                          = lt_vbpok
   EXCEPTIONS
     error_message = 1
     OTHERS        = 2.



ENDFUNCTION.
