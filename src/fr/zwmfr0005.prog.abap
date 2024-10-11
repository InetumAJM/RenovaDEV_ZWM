*&---------------------------------------------------------------------*
*& Report  ZWMFR0005
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zwmfr0005.
DATA: lr_docn TYPE RANGE OF edi_docnum.

DATA: lt_edidd TYPE TABLE OF edidd.

DATA: ls_r_docn LIKE LINE OF lr_docn,
      ls_edids TYPE edi_ds.

DATA: lv_subrc TYPE sysubrc,
      lv_error TYPE c.

DATA: BEGIN OF gty_search_help,
        docnum TYPE edi_docnum,
       END OF gty_search_help.

PARAMETERS: p_lgnum   TYPE lgnum.
PARAMETERS: p_mestyp  TYPE edi4mestyp.
PARAMETERS: p_idocty  TYPE edi4idoctp.
SELECT-OPTIONS s_docn FOR gty_search_help-docnum NO INTERVALS OBLIGATORY.

LOOP AT s_docn INTO ls_r_docn.


  CALL FUNCTION 'EDI_DOCUMENT_OPEN_FOR_PROCESS'
    EXPORTING
      document_number          = ls_r_docn-low
    EXCEPTIONS
      document_foreign_lock    = 1
      document_not_exist       = 2
      document_number_invalid  = 3
      document_is_already_open = 4
      OTHERS                   = 5.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
      DISPLAY LIKE sy-msgty
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    CONTINUE.
  ENDIF.


  CALL FUNCTION 'EDI_SEGMENTS_GET_ALL'
    EXPORTING
      document_number         = ls_r_docn-low
    TABLES
      idoc_containers         = lt_edidd
    EXCEPTIONS
      document_number_invalid = 1
      end_of_document         = 2
      OTHERS                  = 3.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
    DISPLAY LIKE sy-msgty
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    CONTINUE.
  ENDIF.


  CALL FUNCTION 'Z_WMFR_WS_CO_ASYNCHRONOUS_LITE'
    EXPORTING
      i_lgnum  = p_lgnum
      i_mestyp = p_mestyp
      i_idocty = p_idocty
      it_edidd = lt_edidd
    IMPORTING
      e_subrc  = lv_subrc.

  IF lv_subrc NE 0.
    lv_error  = 'Y'.
  ENDIF.

  GET TIME.
  CLEAR: ls_edids.
  ls_edids-docnum = ls_r_docn-low.
  ls_edids-tabnam = 'EDI_DS'.
  ls_edids-logdat = sy-datum.
  ls_edids-logtim = sy-uzeit.
  ls_edids-repid  = 'Z_WMFR_EDI_OUTBOUND_FUNCTION'.
  ls_edids-stamqu = 'SAP'.

  IF lv_error EQ 'Y'. " error found
    ls_edids-status = 20.

    CALL FUNCTION 'EDI_DOCUMENT_STATUS_SET'
      EXPORTING
        document_number         = ls_r_docn-low
        idoc_status             = ls_edids
      EXCEPTIONS
        document_number_invalid = 1
        other_fields_invalid    = 2
        status_invalid          = 3
        OTHERS                  = 4.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
      DISPLAY LIKE sy-msgty
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      CONTINUE.
    ENDIF.

    CALL FUNCTION 'IDOC_ERROR_WORKFLOW_START'
      EXPORTING
        docnum                  = ls_r_docn-low
        eventcode               = 'EDIO'
      EXCEPTIONS
        no_entry_in_tede5       = 1
        error_in_start_workflow = 2
        OTHERS                  = 3.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
      DISPLAY LIKE sy-msgty
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      CONTINUE.
    ENDIF.
  ELSE. " no error
    ls_edids-status = 18.

    CALL FUNCTION 'EDI_DOCUMENT_STATUS_SET'
      EXPORTING
        document_number         = ls_r_docn-low
        idoc_status             = ls_edids
      EXCEPTIONS
        document_number_invalid = 1
        other_fields_invalid    = 2
        status_invalid          = 3
        OTHERS                  = 4.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
      DISPLAY LIKE sy-msgty
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      CONTINUE.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'EDI_DOCUMENT_CLOSE_PROCESS'
    EXPORTING
      document_number     = ls_r_docn-low
    EXCEPTIONS
      document_not_open   = 1
      failure_in_db_write = 2
      parameter_error     = 3
      status_set_missing  = 4
      OTHERS              = 5.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
    DISPLAY LIKE sy-msgty
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    CONTINUE.
  ENDIF.

ENDLOOP.
