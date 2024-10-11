FUNCTION ZWM_LM_GOODS_MOVEMENT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IF_BITYPE) TYPE  CHAR1 DEFAULT 'E'
*"     VALUE(IF_WADAT_IST) LIKE  LIKP-WADAT_IST OPTIONAL
*"     VALUE(IF_FLAG_INBOUND) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_USE_CALL_TRANSACTION) TYPE  CHAR1 DEFAULT SPACE
*"     VALUE(IF_PARTIAL_GOODS_RECEIPT) TYPE  CHAR1 DEFAULT SPACE
*"  EXPORTING
*"     VALUE(EF_SUCCESS_COUNT) TYPE  I
*"     VALUE(EF_ERROR_COUNT) TYPE  I
*"  TABLES
*"      CT_WORKTAB STRUCTURE  ZWMOV
*"      ET_SUCCESS STRUCTURE  ZWMOV
*"      ET_ERROR STRUCTURE  ZWMOV
*"      ET_TVST STRUCTURE  TVST OPTIONAL
*"      ET_VBFS STRUCTURE  VBFS OPTIONAL
*"  EXCEPTIONS
*"      NO_PERMISSION
*"----------------------------------------------------------------------
  data: begin of lt_select occurs 10,
          vbeln like zwmov-vbeln,
          wbstk like vbuk-wbstk,
          tabix like sy-tabix,
        end of lt_select,
        lt_select2 like lt_select occurs 10 with header line,
        lt_bdcdata like bdcdata occurs 0 with header line,
        lf_tvst_lines type i,
        lf_select_lines type i,
        lf_wadat_ist like likp-wadat_ist,
        lf_textline(40) type c,
        lf_titlebar(40) type c,
        lf_text_vbeln(12) type c,
        lf_answer type c,
        lf_document_counter type i,
        lf_counter type i,
        lf_progress type i,
        lf_progress_text(10) type c,
        lt_prot type standard table of prott,
        lf_error_any type c,
        ls_vbkok type vbkok,
        lt_messtab type table of bdcmsgcoll.


  refresh: et_success, et_error.
  clear: ef_success_count, ef_error_count.

  lf_wadat_ist = if_wadat_ist.                              "50A

* Extract shipping points of the selected deliveries
  loop at ct_worktab.
    et_tvst-vstel = ct_worktab-vstel.
    collect et_tvst.
  endloop.

* Check change authority for the given shipping points
  loop at et_tvst.
    authority-check object 'V_LIKP_VST'
             id 'VSTEL' field et_tvst-vstel
             id 'ACTVT' field '02'.
    if sy-subrc eq 0.
      delete et_tvst.
    endif.
  endloop.
  describe table et_tvst lines lf_tvst_lines.
  if lf_tvst_lines ne 0.
    raise no_permission.
  endif.

* Special logic for partial goods receipt
  if if_partial_goods_receipt ne space and
     if_flag_inbound ne space.

    perform partial_goods_receipt tables   ct_worktab
                                           et_success
                                           et_error
                                           et_vbfs
                                  using    lf_wadat_ist
                                  changing ef_success_count
                                           ef_error_count.
    exit.

  endif.


* Extract document numbers of the selected deliveries
  loop at ct_worktab.
    lt_select-vbeln = ct_worktab-vbeln.
    collect lt_select.
  endloop.

* Write line numbers to selection table in order to keep processing
* order
  loop at lt_select.
    lt_select-tabix = sy-tabix.
    modify lt_select transporting tabix.
  endloop.

  check not lt_select[] is initial.                         "50A

* Refresh status of goods movement
  select vbeln wbstk from vbuk
               appending corresponding fields of table lt_select2
               for all entries in lt_select
               where vbeln eq lt_select-vbeln.

* Sort LT_SELECT2 in the same sequence as LT_SELECT
  loop at lt_select2.
    read table lt_select with key vbeln = lt_select2-vbeln.
    if sy-subrc eq 0.
      lt_select2-tabix = lt_select-tabix.
      modify lt_select2 transporting tabix.
    endif.
  endloop.
  sort lt_select2 by tabix ascending.

  describe table lt_select2 lines lf_document_counter.
  clear: lt_select, lf_counter.
  refresh lt_select.
  loop at lt_select2.
    add 1 to lf_counter.
    if lt_select2-wbstk eq charc.
*     Goods issue/receive already posted
      loop at ct_worktab where vbeln eq lt_select2-vbeln.
        et_success = ct_worktab.
        et_success-color = color_success.
        append et_success.
      endloop.
      add 1 to ef_success_count.
      delete lt_select2.
    else.
*     Allow cancelling if last document has been processed with errors
      if sy-tabix gt 1 and
        ( if_bitype eq bitype_bright or
          if_bitype eq bitype_bright_on_error ) and
         lt_select-wbstk ca 'AB'.
        lf_textline = 'Nächsten Beleg &1 bearbeiten?'(006).
        if if_flag_inbound ne space.
          lf_titlebar = 'Buchen des Wareneingangs'(036).
        else.
          lf_titlebar = 'Buchen des Warenausgangs'(022).
        endif.
        write lt_select2-vbeln to lf_text_vbeln no-zero.
        replace '&1' with lf_text_vbeln into lf_textline.
        condense lf_textline.
        call function 'POPUP_TO_CONFIRM_STEP'
          exporting
            textline1 = lf_textline
            titel     = lf_titlebar
          importing
            answer    = lf_answer.
        case lf_answer.
          when 'N'.
            continue.
          when 'A'.
            exit.
        endcase.
      endif.
      if if_bitype eq bitype_dark and
         if_use_call_transaction eq space.
        clear ls_vbkok.
        refresh lt_prot.
        ls_vbkok-vbeln_vl  = lt_select2-vbeln.
        ls_vbkok-wadat_ist = lf_wadat_ist.
        ls_vbkok-wabuc     = 'X'.
        call function 'HU_PACKING_REFRESH'.
        call function 'SERIAL_INTTAB_REFRESH'
          exporting
            objects_status_refresh = 'C'.
        call function 'WS_DELIVERY_UPDATE'
          exporting
            vbkok_wa                 = ls_vbkok
            synchron                 = 'X'
            commit                   = 'X'
            delivery                 = lt_select2-vbeln
            if_error_messages_send_0 = ' '
          importing
            ef_error_any_0           = lf_error_any
          tables
            prot                     = lt_prot
          exceptions
            error_message            = 1
            others                   = 2.
        if sy-subrc eq 1.
          call function 'DEQUEUE_ALL'.
          clear et_vbfs.
          et_vbfs-vbeln = lt_select2-vbeln.
          et_vbfs-msgty = sy-msgty.
          et_vbfs-msgno = sy-msgno.
          et_vbfs-msgid = sy-msgid.
          et_vbfs-msgv1 = sy-msgv1.
          et_vbfs-msgv2 = sy-msgv2.
          et_vbfs-msgv3 = sy-msgv3.
          et_vbfs-msgv4 = sy-msgv4.
          append et_vbfs.
        elseif sy-subrc eq 2 or
               lf_error_any ne space.
          call function 'DEQUEUE_ALL'.
          perform move_prot_to_vbfs tables lt_prot
                                           et_vbfs.
        else.
          loop at lt_prot transporting no fields
                          where msgty ca 'AEX'.
            exit.
          endloop.
          if sy-subrc eq 0.
            call function 'DEQUEUE_ALL'.
            perform move_prot_to_vbfs tables lt_prot
                                             et_vbfs.
          endif.
        endif.
      else.
*       Export actual GI date to SAP memory
        export wadat_ist from lf_wadat_ist to memory id 'WS_LM'.
*       Fill parameter table for BI of transaction VL02/VL32
        refresh lt_bdcdata.
        if if_flag_inbound eq space.
          set_dynp_batchinput 'SAPMV50A' '4004'.
          set_var_batchinput 'LIKP-VBELN' lt_select2-vbeln.
          set_var_batchinput 'BDC_OKCODE' 'WABU_T'.
          call transaction 'VL02N' using lt_bdcdata
                                  mode if_bitype
                                  update 'S'
                                  messages into lt_messtab.
        else.
          set_dynp_batchinput 'SAPMV50A' '4104'.
          set_var_batchinput 'LIKP-VBELN' lt_select2-vbeln.
          set_var_batchinput 'BDC_OKCODE' 'WABU_T'.
          call transaction 'VL32N' using lt_bdcdata
                                  mode if_bitype
                                  update 'S'
                                  messages into lt_messtab.
        endif.
      endif.
*     Check status of goods movement
      select single vbeln wbstk from vbuk
                    into corresponding fields of lt_select
                    where vbeln eq lt_select2-vbeln.
      append lt_select.
    endif.
*   Display progress indicator in case of dark processing
    if   ( if_bitype eq bitype_dark and sy-batch eq space )
        or if_bitype eq bitype_bright_on_error.
      lf_progress = lf_counter * 100 / lf_document_counter.
      write lf_progress to lf_progress_text.
      lf_textline = 'Warenbewegungen buchen... &1 %'(063).
      replace '&1' with lf_progress_text into lf_textline.
      call function 'SAPGUI_PROGRESS_INDICATOR'
        exporting
          percentage = lf_progress
          text       = lf_textline.
    endif.
  endloop.

* Check successfull performance of goods issue
  describe table lt_select2 lines lf_select_lines.
  if lf_select_lines ne 0.
    loop at lt_select.
      if lt_select-wbstk eq charc.
        add 1 to ef_success_count.
        loop at ct_worktab where vbeln eq lt_select-vbeln.
          et_success = ct_worktab.
          et_success-color = color_success.
          append et_success.
        endloop.
      else.
        add 1 to ef_error_count.
        loop at ct_worktab where vbeln eq lt_select-vbeln.
          et_error = ct_worktab.
          et_error-color = color_error.
          append et_error.
        endloop.
      endif.
    endloop.
  endif.

* Clear ABAP-Memory to avoid undesired effects
  clear lf_wadat_ist.
  export wadat_ist from lf_wadat_ist to memory id 'WS_LM'.

endfunction.
