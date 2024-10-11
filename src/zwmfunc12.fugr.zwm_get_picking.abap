FUNCTION ZWM_GET_PICKING .
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"     REFERENCE(I_POSNR) TYPE  POSNR
*"     REFERENCE(I_MODE) TYPE  C DEFAULT ' '
*"  EXPORTING
*"     REFERENCE(E_PIKMG) TYPE  PIKMG
*"  EXCEPTIONS
*"      DOCUMENT_READ_ERROR
*"--------------------------------------------------------------------

* Local Data
  DATA: l_lips        LIKE lips,
        l_lipsvb      LIKE lipsvb,
*        lt_lipsvb     like lipsvb occurs 0 with header line,
        lt_vbfavb     LIKE vbfavb OCCURS 0 WITH HEADER LINE,
        lt_vbapf      LIKE vbapf  OCCURS 0 WITH HEADER LINE,
        l_item_ref    TYPE REF TO data,
        lf_qmenge_flo TYPE f.

  FIELD-SYMBOLS <item_data_lips> TYPE lipsvb.

* DB Case:
  IF i_mode IS INITIAL.
    CALL FUNCTION 'WB2_LIPS_READ'
      EXPORTING
        i_vbeln         = i_vbeln
        i_posnr         = i_posnr
      IMPORTING
        e_lips          = l_lips
      EXCEPTIONS
        not_found       = 1
        parameter_error = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING document_read_error.
    ENDIF.
    CALL FUNCTION 'SD_VBFA_READ_WITH_VBELV'
      EXPORTING
        i_vbelv            = i_vbeln
        i_bypassing_buffer = abap_true
        i_refresh_buffer   = abap_true
      TABLES
        et_vbfavb          = lt_vbfavb
      EXCEPTIONS
        record_not_found   = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING document_read_error.
    ENDIF.
    CALL FUNCTION 'RV_XVBAPF_CREATE'
      EXPORTING
        vbeln   = i_vbeln
      TABLES
        fxvbapf = lt_vbapf
        fxvbfa  = lt_vbfavb.
    READ TABLE lt_vbapf WITH KEY posnr = i_posnr.
    MOVE-CORRESPONDING l_lips TO l_lipsvb.
*  else.
** online case:
*    call function 'WB2_EXPENSE_SET_MASTER_DATA'
*      exporting
*        i_vbeln     = i_vbeln
*        i_doc_type  = 'D'
*        i_posnr     = i_posnr
*      importing
*        e_item_data = l_item_ref.
*    assign l_item_ref->* to <item_data_lips>.
*    if sy-subrc = 0 .
*      move-corresponding <item_data_lips> to l_lipsvb.
*    endif.
*    perform xvbapf_lesen_direkt(sapfv50p)
*                   using i_vbeln
*                         i_posnr
*                   changing
*                         lt_vbapf
*                         sy-subrc.
  ENDIF.

  IF l_lipsvb-kzfme CA 'AB' OR lt_vbapf-qmengev GE 0.
    l_lipsvb-pikmg = lt_vbapf-qmengev.
  ELSE.
    lf_qmenge_flo = lt_vbapf-qmenge.
    IF l_lipsvb-umrev > 0.
      l_lipsvb-pikmg = lf_qmenge_flo / l_lipsvb-umrev.
    ELSE.
      l_lipsvb-pikmg = lf_qmenge_flo / l_lipsvb-umvkz * l_lips-umvkn.
    ENDIF.
  ENDIF.

  e_pikmg =  l_lipsvb-pikmg.

ENDFUNCTION.
