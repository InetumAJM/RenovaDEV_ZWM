*&---------------------------------------------------------------------*
*& Report  ZWMREP0033                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zwmrep0033.

DATA : lvs_itcpo LIKE itcpo,
       ecra(5),
       maq(20).

PARAMETERS: p_maq(13).
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_ecra1 RADIOBUTTON GROUP r1,
            p_ecra2 RADIOBUTTON GROUP r1.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_200 RADIOBUTTON GROUP r2,
            p_300 RADIOBUTTON GROUP r2.

START-OF-SELECTION.

** Actualização dos parâmetros da impressão
  lvs_itcpo-tdimmed = 'X'.
  lvs_itcpo-tddelete = 'X'.

** Criação do formulário
  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device                      = 'PRINTER'
      dialog                      = 'X'
      form                        = 'ZWM_LABEL_MAQ'
      language                    = sy-langu
      options                     = lvs_itcpo
    EXCEPTIONS
      canceled                    = 1
      device                      = 2
      form                        = 3
      options                     = 4
      unclosed                    = 5
      mail_options                = 6
      archive_error               = 7
      invalid_fax_number          = 8
      more_params_needed_in_batch = 9
      spool_error                 = 10
      OTHERS                      = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  CALL FUNCTION 'START_FORM'
    EXPORTING
      form        = 'ZWM_LABEL_MAQ'
      language    = sy-langu
      program     = 'ZWMREP0033'
    EXCEPTIONS
      form        = 1
      format      = 2
      unended     = 3
      unopened    = 4
      unused      = 5
      spool_error = 6
      OTHERS      = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF NOT p_maq IS INITIAL.
    PERFORM imprime_maq.
    CLEAR p_maq.
  ENDIF.

  CALL FUNCTION 'END_FORM'
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      spool_error              = 3
      OTHERS                   = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      send_error               = 3
      spool_error              = 4
      OTHERS                   = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  imprime_maq
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_maq .

  MOVE p_maq TO maq.

  IF p_ecra1 = 'X'.
    MOVE '16X20' TO ecra.
  ELSE.
    MOVE '8X40' TO ecra.
  ENDIF.

  IF p_200 = 'X'.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element                  = '200'
        window                   = 'MAIN'
      EXCEPTIONS
        element                  = 1
        function                 = 2
        type                     = 3
        unopened                 = 4
        unstarted                = 5
        window                   = 6
        bad_pageformat_for_print = 7
        spool_error              = 8
        OTHERS                   = 9.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element                  = '300'
        window                   = 'MAIN'
      EXCEPTIONS
        element                  = 1
        function                 = 2
        type                     = 3
        unopened                 = 4
        unstarted                = 5
        window                   = 6
        bad_pageformat_for_print = 7
        spool_error              = 8
        OTHERS                   = 9.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.                    " imprime_maq
