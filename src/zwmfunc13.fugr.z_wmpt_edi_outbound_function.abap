FUNCTION z_wmpt_edi_outbound_function.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      I_EDIDC STRUCTURE  EDIDC
*"----------------------------------------------------------------------
  DATA lt_t300 TYPE SORTED TABLE OF t300 WITH UNIQUE KEY lgnum.

  DATA lv_send    TYPE abap_bool.
  DATA lv_error   TYPE char1.
  DATA lv_subrc   TYPE sysubrc.
  DATA lv_lgnum   TYPE lgnum.

  DATA ls_edids TYPE edi_ds.
  DATA ls_edidd TYPE edidd.
  DATA ls_t300  TYPE t300.

  DATA: lr_data        TYPE REF TO data.
  DATA: lr_type_struct TYPE REF TO cl_abap_structdescr,
        lr_descr_ref   TYPE REF TO cl_abap_typedescr.



  DATA lt_edidd TYPE STANDARD TABLE OF edidd.

  FIELD-SYMBOLS <fs_edidc> LIKE LINE OF i_edidc[].
  FIELD-SYMBOLS <ls_data> TYPE any.
  FIELD-SYMBOLS <lv_lgnum> TYPE lgnum.
*"--------------------------------------------------------------------

  LOOP AT i_edidc[] ASSIGNING <fs_edidc>.
    FREE lt_edidd[].

    CLEAR ls_edids.

    lv_send   = abap_false.
    lv_error  = 'N'.

    CALL FUNCTION 'ALE_MODEL_DETERMINE_IF_TO_SEND'
      EXPORTING
        message_type           = <fs_edidc>-mestyp
      IMPORTING
        idoc_must_be_sent      = lv_send
      EXCEPTIONS
        own_system_not_defined = 1
        OTHERS                 = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      CONTINUE.
    ENDIF.

    CHECK lv_send EQ abap_true.

    CALL FUNCTION 'EDI_DOCUMENT_OPEN_FOR_PROCESS'
      EXPORTING
        document_number          = <fs_edidc>-docnum
      EXCEPTIONS
        document_foreign_lock    = 1
        document_not_exist       = 2
        document_number_invalid  = 3
        document_is_already_open = 4
        OTHERS                   = 5.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      CONTINUE.
    ENDIF.

    CALL FUNCTION 'EDI_SEGMENTS_GET_ALL'
      EXPORTING
        document_number         = <fs_edidc>-docnum
      TABLES
        idoc_containers         = lt_edidd[]
      EXCEPTIONS
        document_number_invalid = 1
        end_of_document         = 2
        OTHERS                  = 3.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      CONTINUE.
    ENDIF.

** Determina LGNUM
***********************************************************************
    SELECT * FROM t300
             INTO TABLE lt_t300.

    LOOP AT lt_edidd INTO ls_edidd.
      CREATE DATA lr_data TYPE (ls_edidd-segnam).
      CHECK lr_data IS BOUND.

      ASSIGN lr_data->* TO <ls_data>.
      CHECK <ls_data> IS ASSIGNED.

      <ls_data> = ls_edidd-sdata.

      ASSIGN COMPONENT 'LGNUM' OF STRUCTURE <ls_data> TO <lv_lgnum>.
      CHECK <lv_lgnum> IS ASSIGNED.
      CHECK NOT <lv_lgnum> IS INITIAL.

      CLEAR: ls_t300.
      READ TABLE lt_t300
            INTO ls_t300
            WITH TABLE KEY lgnum = <lv_lgnum>.
      CHECK sy-subrc EQ 0.

      lv_lgnum = <lv_lgnum>.
      EXIT.
    ENDLOOP.

**********************************************************************


    CALL FUNCTION 'Z_WMPT_WS_CO_ASYNCHRONOUS_LITE'
      EXPORTING
        i_lgnum  = lv_lgnum
        i_mestyp = <fs_edidc>-mestyp
        i_idocty = <fs_edidc>-idoctp
        it_edidd = lt_edidd[]
      IMPORTING
        e_subrc  = lv_subrc.
    IF lv_subrc NE 0.
      lv_error  = 'Y'.
    ENDIF.

    GET TIME.
    ls_edids-docnum = <fs_edidc>-docnum.
    ls_edids-tabnam = 'EDI_DS'.
    ls_edids-logdat = sy-datum.
    ls_edids-logtim = sy-uzeit.
    ls_edids-repid  = 'Z_WMPT_EDI_OUTBOUND_FUNCTION'.
    ls_edids-stamqu = 'SAP'.

    IF lv_error EQ 'Y'. " error found
      ls_edids-status = 20.

      CALL FUNCTION 'EDI_DOCUMENT_STATUS_SET'
        EXPORTING
          document_number         = <fs_edidc>-docnum
          idoc_status             = ls_edids
        EXCEPTIONS
          document_number_invalid = 1
          other_fields_invalid    = 2
          status_invalid          = 3
          OTHERS                  = 4.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        CONTINUE.
      ENDIF.

      CALL FUNCTION 'IDOC_ERROR_WORKFLOW_START'
        EXPORTING
          docnum                  = <fs_edidc>-docnum
          eventcode               = 'EDIO'
        EXCEPTIONS
          no_entry_in_tede5       = 1
          error_in_start_workflow = 2
          OTHERS                  = 3.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        CONTINUE.
      ENDIF.
    ELSE. " no error
      ls_edids-status = 18.

      CALL FUNCTION 'EDI_DOCUMENT_STATUS_SET'
        EXPORTING
          document_number         = <fs_edidc>-docnum
          idoc_status             = ls_edids
        EXCEPTIONS
          document_number_invalid = 1
          other_fields_invalid    = 2
          status_invalid          = 3
          OTHERS                  = 4.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        CONTINUE.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'EDI_DOCUMENT_CLOSE_PROCESS'
      EXPORTING
        document_number     = <fs_edidc>-docnum
      EXCEPTIONS
        document_not_open   = 1
        failure_in_db_write = 2
        parameter_error     = 3
        status_set_missing  = 4
        OTHERS              = 5.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
