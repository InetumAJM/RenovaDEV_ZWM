*&---------------------------------------------------------------------*
*& Report  ZWMREP0020                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zwmrep0020.

DATA : lvs_itcpo LIKE itcpo,
       sscc TYPE exidv,
       sscc1 TYPE exidv,
       sscc2 TYPE exidv,
       pick1(20),
       pick2(20),
       maq(20).

PARAMETERS: p_lgnum LIKE ltak-lgnum OBLIGATORY MEMORY ID lgn,
            p_werks LIKE ltap-werks OBLIGATORY MEMORY ID wrk,
            p_lgort LIKE ltap-lgort OBLIGATORY MEMORY ID lag,
            p_sscc(2) DEFAULT '0',
            p_pick1(2) DEFAULT '0',
            p_pick2(2) DEFAULT '0',
            p_pick3(2) DEFAULT '0',
            p_pick4(2) DEFAULT '0'.
*            p_maq(20).

SELECTION-SCREEN SKIP 1.
PARAMETERS: p_200 RADIOBUTTON GROUP r1,
            p_300 RADIOBUTTON GROUP r1.

START-OF-SELECTION.

*  IF NOT p_talao IS INITIAL AND NOT p_armaz IS INITIAL.
*    PERFORM carrega_info.
*    PERFORM imprime_talao.
*  ENDIF.

END-OF-SELECTION.

** Actualização dos parâmetros da impressão
  lvs_itcpo-tdimmed = 'X'.
  lvs_itcpo-tddelete = 'X'.


** Criação do formulário
  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device                      = 'PRINTER'
      dialog                      = 'X'
      form                        = 'ZWM_SSCC_PICK'
      language                    = sy-langu
      OPTIONS                     = lvs_itcpo
    EXCEPTIONS
      canceled                    = 1
      device                      = 2
      form                        = 3
      OPTIONS                     = 4
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
      form        = 'ZWM_SSCC_PICK'
      language    = sy-langu
      program     = 'ZWMREP0020'
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

  IF p_sscc <> '0'.
    PERFORM imprime_sscc.
    CLEAR p_sscc.
  ENDIF.
  IF p_pick1 <> '0'.
    PERFORM imprime_pick1.
    CLEAR p_pick1.
  ENDIF.
  IF p_pick2 <> '0'.
    PERFORM imprime_pick2.
    CLEAR p_pick2.
  ENDIF.
  IF p_pick3 <> '0'.
    PERFORM imprime_pick3.
    CLEAR p_pick3.
  ENDIF.
  IF p_pick4 <> '0'.
    PERFORM imprime_pick4.
    CLEAR p_pick4.
  ENDIF.

*  IF NOT p_maq IS INITIAL.
*    PERFORM imprime_maq.
*    CLEAR p_maq.
*  ENDIF.

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
*&      Form  IMPRIME_SSCC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_sscc.
  DATA lv_werks TYPE werks_d. " << INS ROFF(SDF):TMGP:29.12.2015 11:34:59

*& Begin of Modification by Tiago Pateiro - ROFF @ 29.12.2015 11:34:40
*/ Adaptar logica ao processamento França
**  SELECT werks UP TO 1 ROWS
**    FROM t320 INTO lv_werks
**    WHERE lgort EQ 'CD'
**      AND lgnum EQ p_lgnum.
**  ENDSELECT.
**  IF sy-subrc NE 0.
**    lv_werks = 'RENV'.
**  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 29.12.2015 11:34:50

*** Actualização dos parâmetros da impressão
*  lvs_itcpo-tdimmed = 'X'.
*  lvs_itcpo-tddelete = 'X'.
*
*
*** Criação do formulário
*  CALL FUNCTION 'OPEN_FORM'
*    EXPORTING
*      device                      = 'PRINTER'
*      dialog                      = 'X'
*      form                        = 'ZWM_SSCC_PICK'
*      language                    = sy-langu
*      options                     = lvs_itcpo
*    EXCEPTIONS
*      canceled                    = 1
*      device                      = 2
*      form                        = 3
*      options                     = 4
*      unclosed                    = 5
*      mail_options                = 6
*      archive_error               = 7
*      invalid_fax_number          = 8
*      more_params_needed_in_batch = 9
*      spool_error                 = 10
*      OTHERS                      = 11.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*
*  CALL FUNCTION 'START_FORM'
*    EXPORTING
*      form        = 'ZWM_SSCC_PICK'
*      language    = sy-langu
*      program     = 'ZWMREP0020'
*    EXCEPTIONS
*      form        = 1
*      format      = 2
*      unended     = 3
*      unopened    = 4
*      unused      = 5
*      spool_error = 6
*      OTHERS      = 7.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

  p_sscc = p_sscc DIV 2.
  DO p_sscc TIMES.

    CLEAR sscc.
    CALL FUNCTION 'ZWM_LE_SSCC_GENERATE'
      EXPORTING
*        if_werk               = 'RENV' " << DEL ROFF(SDF):TMGP:29.12.2015 11:35:12
        if_werk               = p_werks " << INS ROFF(SDF):TMGP:29.12.2015 11:35:14
        if_lgort              = p_lgort
        if_lgnum              = p_lgnum
        if_huart              = '3'
      IMPORTING
        ef_sscc               = sscc
      EXCEPTIONS
        iln_not_found         = 1
        invalid_call          = 2
        invalid_customizing   = 3
        internal_error        = 4
        error_on_number_range = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = sscc
      IMPORTING
        output = sscc1.

*    MOVE sscc TO sscc1.

    CLEAR sscc.
    CALL FUNCTION 'ZWM_LE_SSCC_GENERATE'
      EXPORTING
*        if_werk               = 'RENV' " << DEL ROFF(SDF):TMGP:29.12.2015 11:35:24
        if_werk               = p_werks " << INS ROFF(SDF):TMGP:29.12.2015 11:35:29
        if_lgort              = p_lgort
        if_lgnum              = p_lgnum
        if_huart              = '3'
      IMPORTING
        ef_sscc               = sscc
      EXCEPTIONS
        iln_not_found         = 1
        invalid_call          = 2
        invalid_customizing   = 3
        internal_error        = 4
        error_on_number_range = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = sscc
      IMPORTING
        output = sscc2.

*    MOVE sscc TO sscc2.

    IF p_200 = 'X'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element                  = 'SSCC'
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
          element                  = 'SSCC_300'
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

  ENDDO.

*  CALL FUNCTION 'END_FORM'
*    EXCEPTIONS
*      unopened                 = 1
*      bad_pageformat_for_print = 2
*      spool_error              = 3
*      OTHERS                   = 4.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  CALL FUNCTION 'CLOSE_FORM'
*    EXCEPTIONS
*      unopened                 = 1
*      bad_pageformat_for_print = 2
*      send_error               = 3
*      spool_error              = 4
*      OTHERS                   = 5.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

ENDFORM.                    " IMPRIME_SSCC
*&---------------------------------------------------------------------*
*&      Form  imprime_pick1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_pick1 .
  CLEAR pick1.
  SELECT SINGLE valor INTO pick1
      FROM zwm001
          WHERE armazem = p_lgnum AND
                processo = 'PICKING' AND
                parametro = 'PAL1'.

  pick2 = pick1.

  p_pick1 = p_pick1 DIV 2.
  DO p_pick1 TIMES.

    IF p_200 = 'X'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element                  = 'PICK1'
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
          element                  = 'PICK1_300'
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
  ENDDO.
ENDFORM.                    " imprime_pick1
*&---------------------------------------------------------------------*
*&      Form  imprime_pick2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_pick2 .

  CLEAR pick1.
  SELECT SINGLE valor INTO pick1
      FROM zwm001
          WHERE armazem = p_lgnum AND
                processo = 'PICKING' AND
                parametro = 'PAL2'.

  pick2 = pick1.

  p_pick2 = p_pick2 DIV 2.
  DO p_pick2 TIMES.

    IF p_200 = 'X'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element                  = 'PICK2'
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
          element                  = 'PICK2_300'
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
  ENDDO.

ENDFORM.                    " imprime_pick2
*&---------------------------------------------------------------------*
*&      Form  imprime_maq
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_maq .

*  MOVE p_maq TO maq.
*  CALL FUNCTION 'WRITE_FORM'
*    EXPORTING
*      element                  = 'MAQ'
*      window                   = 'MAIN'
*    EXCEPTIONS
*      element                  = 1
*      function                 = 2
*      type                     = 3
*      unopened                 = 4
*      unstarted                = 5
*      window                   = 6
*      bad_pageformat_for_print = 7
*      spool_error              = 8
*      OTHERS                   = 9.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

ENDFORM.                    " imprime_maq
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_PICK3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_pick3.
  CLEAR pick1.
  SELECT SINGLE valor INTO pick1
      FROM zwm001
          WHERE armazem = p_lgnum AND
                processo = 'PICKING' AND
                parametro = 'PAL3'.

  pick2 = pick1.

  p_pick3 = p_pick3 DIV 2.
  DO p_pick3 TIMES.

    IF p_200 = 'X'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element                  = 'PICK3'
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
          element                  = 'PICK3_300'
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
  ENDDO.
ENDFORM.                    " IMPRIME_PICK3
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_PICK4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_pick4.
  CLEAR pick1.
  SELECT SINGLE valor INTO pick1
      FROM zwm001
          WHERE armazem = p_lgnum AND
                processo = 'PICKING' AND
                parametro = 'PAL4'.

  pick2 = pick1.

  p_pick4 = p_pick4 DIV 2.
  DO p_pick4 TIMES.

    IF p_200 = 'X'.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element                  = 'PICK4'
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
          element                  = 'PICK4_300'
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
  ENDDO.
ENDFORM.                    " IMPRIME_PICK4
