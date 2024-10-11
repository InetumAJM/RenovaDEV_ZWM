*&---------------------------------------------------------------------*
*& Report  ZWMREP0004                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zwmrep0004.

TABLES : zwm003_aux, zwm005, zwm006_aux, vttk.

DATA: lvs_itcpo LIKE itcpo,

      BEGIN OF it_trans OCCURS 0,
        data          LIKE zwm005-data,
        num_entrada   LIKE zwm005-num_entrada,
        n_transporte  LIKE zwm006-n_transporte,
        ord_compra    LIKE zwm005-ord_compra,
        matricula     LIKE zwm005-matricula,
        tipo_camiao   LIKE zwm005-tipo_camiao,
        transportador LIKE zwm005-transportador,
        observacoes   LIKE zwm005-observacoes,
        observacoes2  LIKE zwm005-observacoes2,
        name1         LIKE lfa1-name1,
      END OF it_trans,

      g_dat      LIKE sy-datum,

      tpoper(10).

PARAMETERS: p_talao LIKE zwm003-num_entrada. "OBLIGATORY.
SELECT-OPTIONS: s_tknum FOR it_trans-n_transporte NO-EXTENSION NO INTERVALS.

START-OF-SELECTION.

  IF p_talao IS INITIAL AND
     s_tknum[] IS INITIAL.
    EXIT.
  ELSE.
*  IF NOT p_talao IS INITIAL.
    PERFORM imprime_talao.
  ENDIF.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  IMPRIME_TALAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_talao .

*** Actualização dos parâmetros da impressão
*  LVS_ITCPO-TDNOPREV = 'X'.
  lvs_itcpo-tdimmed = 'X'.
*  lvs_itcpo-tdprinter = 'CD42'.
*  LVS_ITCPO-TDDELETE = 'X'.

*--------------------------------------selecção dos dados de transporte
  PERFORM selecao_dados.
*---------------------------------------------------------------------
  IF NOT it_trans IS INITIAL.

** Criação do formulário
    CALL FUNCTION 'OPEN_FORM'
      EXPORTING
        device                      = 'PRINTER'
        dialog                      = 'X'
        form                        = 'ZWMFORM001'
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
        form        = 'ZWMFORM001'
        language    = sy-langu
        program     = 'ZWMREP0004'
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


    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element                  = 'TALAO'
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

** fecho do formulario
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

  ELSE.
    MESSAGE i056(zwmmsg001) WITH p_talao.
*   Erro! Talão & não existe ou ja foi finalizado!

  ENDIF.

ENDFORM.                    " IMPRIME_TALAO
*&---------------------------------------------------------------------*
*&      Form  selecao_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM selecao_dados .
*-------------------------o talão o representa carga ou descarga logo,
*-------------------------existe numa tabela ou na outra nunca nas duas
  CLEAR : it_trans, g_dat, tpoper.
  DATA lr_talao TYPE RANGE OF zwm006_aux-num_entrada.

  CONCATENATE sy-datum(4) '0101'INTO g_dat.

  IF p_talao IS NOT INITIAL.
    lr_talao = VALUE #( sign = 'I' option = 'EQ' ( low = p_talao ) ).
    SELECT *  INTO CORRESPONDING FIELDS OF it_trans FROM  zwm005
           WHERE  num_entrada  = p_talao
           AND    data         GE g_dat.
    ENDSELECT.
  ENDIF.

*  IF sy-subrc  NE 0.
  IF it_trans IS INITIAL.

    SELECT *  FROM zwm006_aux
*           WHERE  num_entrada  = p_talao
           WHERE  num_entrada IN lr_talao
             AND n_transporte IN s_tknum
           AND    data_reg    GE g_dat.

    ENDSELECT.

    MOVE-CORRESPONDING zwm006_aux TO it_trans.

    IF sy-subrc  NE 0.
      CLEAR : it_trans.
    ELSE.
      PERFORM get_info USING it_trans.
      tpoper = 'CARGAS'.
    ENDIF.

  ELSE.
    tpoper = 'DESCARGAS'.
  ENDIF.


  SELECT SINGLE name1 INTO it_trans-name1 FROM  lfa1
         WHERE  lifnr  = vttk-tdlnr.

ENDFORM.                    " selecao_dados
*&---------------------------------------------------------------------*
*&      Form  get_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TRANS  text
*----------------------------------------------------------------------*
FORM get_info  USING  it_trans STRUCTURE it_trans.

  CLEAR it_trans-data.

  it_trans-data = zwm006_aux-data_reg.

  CLEAR it_trans-tipo_camiao.

  SELECT SINGLE sdabw tdlnr INTO (it_trans-tipo_camiao, vttk-tdlnr)
                            FROM  vttk
         WHERE  tknum  = it_trans-n_transporte.


ENDFORM.                    " get_info
