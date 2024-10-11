REPORT zwmrep0008_1etiq MESSAGE-ID zwmmsg001.

TABLES: lagp, lrf_wkqu.

DATA: BEGIN OF tbins OCCURS 0,
      lgpla LIKE lagp-lgpla,
END OF tbins.

DATA: user_address LIKE addr3_val,
      user_usr03 LIKE usr03.
DATA: int_adr6 LIKE adsmtp OCCURS 0 WITH HEADER LINE.

DATA: nome(30).

DATA: linhas     TYPE i VALUE 0,
      aux_linhas TYPE i VALUE 0,
      indice     TYPE i VALUE 1.

DATA: bin1_aux LIKE lagp-lgpla.

DATA: BIN1(14).

DATA: lvs_itcpo LIKE itcpo.

* parameters
PARAMETERS: p_lgnum  LIKE ltap-lgnum OBLIGATORY.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_bin  RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 4(30) text-001.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_lgtyp  LIKE lagp-lgtyp.
SELECT-OPTIONS: p_lgpla FOR lagp-lgpla.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-003.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_imp200 RADIOBUTTON GROUP rad2.
SELECTION-SCREEN COMMENT (20) text-t01.
PARAMETERS: p_imp300 RADIOBUTTON GROUP rad2.
SELECTION-SCREEN COMMENT (20) text-t02.
SELECTION-SCREEN END OF LINE.
**
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_sem   RADIOBUTTON GROUP r3.
SELECTION-SCREEN COMMENT (20) text-t03.
PARAMETERS: p_cima  RADIOBUTTON GROUP r3.
SELECTION-SCREEN COMMENT (20) text-t04.
PARAMETERS: p_baixo RADIOBUTTON GROUP r3.
SELECTION-SCREEN COMMENT (20) text-t05.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_usr  RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 4(30) text-002.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_user  LIKE sy-uname,
            p_passwd LIKE rsyst-bcode,
            p2_passw LIKE rsyst-bcode.

SELECTION-SCREEN END OF BLOCK b3.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF NOT p_bin IS INITIAL AND
       ( screen-name = 'P_USER' OR screen-name = 'P_PASSWD' OR
         screen-name = 'P2_PASSW' ).
      screen-input = 0.
      CLEAR: p_user, p_passwd, p2_passw.
    ELSEIF NOT p_bin IS INITIAL AND
             ( screen-name = 'P_LGTYP' OR screen-name = 'P_LGPLA-LOW'
               OR screen-name = 'P_LGPLA-HIGH' ).
      screen-input = 1.
    ELSEIF NOT p_usr IS INITIAL AND
             ( screen-name = 'P_LGTYP' OR screen-name = 'P_LGPLA-LOW'
               OR screen-name = 'P_LGPLA-HIGH' ).
      screen-input = 0.
      CLEAR: p_lgtyp, p_lgpla. REFRESH p_lgpla.
    ELSEIF NOT p_usr IS INITIAL AND
             ( screen-name = 'P_USER' OR screen-name = 'P_PASSWD' OR
               screen-name = 'P2_PASSW' ).
      screen-input = 1.
    ENDIF.
    IF screen-name = 'P_PASSWD'.
      screen-invisible = 1.
    ENDIF.
    IF screen-name = 'P2_PASSW'.
      screen-invisible = 1.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.


AT SELECTION-SCREEN ON p_user.
  IF NOT p_user IS INITIAL.
    SELECT SINGLE * FROM lrf_wkqu
           WHERE lgnum = p_lgnum
             AND bname = p_user.
    IF sy-subrc <> 0.
      MESSAGE ID 'LF' TYPE 'E' NUMBER '192' WITH p_user.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p2_passw.
  IF p_passwd NE p2_passw.
    MESSAGE e067.
  ENDIF.


** Só imprime etiquetas por nivel
** Para portas - pulmões - devoluções não se faz esta validação
AT SELECTION-SCREEN ON p_lgpla.

  CHECK NOT p_lgtyp EQ 'DCK' AND
        NOT p_lgtyp EQ 'PUL' AND
        NOT p_lgtyp EQ 'DEV'.
  IF NOT p_lgpla-high IS INITIAL.
    IF p_lgpla-low+8(2) <> p_lgpla-high+8(2).
      MESSAGE e072.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_user.

  DATA: aux_key(7).

  IF NOT p_lgnum IS INITIAL.
    CONCATENATE sy-mandt p_lgnum '*' INTO aux_key.
    CALL FUNCTION 'HELP_VALUES_GET_WITH_DD_TABLE'
      EXPORTING
        selectfield         = 3
        shfields            = '23'
        tabkey              = aux_key
        tabname             = 'LRF_WKQU'
        title_in_list_popup = 'Valid Warehouse Users'
      IMPORTING
        selectvalue         = p_user.
  ENDIF.

AT SELECTION-SCREEN ON p_lgnum.

  SELECT SINGLE * FROM lrf_wkqu WHERE lgnum = p_lgnum
                                  AND bname = sy-uname.
  IF sy-subrc <> 0.
    MESSAGE e003 WITH sy-uname p_lgnum.
  ENDIF.

START-OF-SELECTION.

  IF p_bin = 'X'.
    IF p_lgpla IS INITIAL OR p_lgtyp IS INITIAL.
      MESSAGE e999 WITH 'É obrigatório preencher todos os campos.'.
    ENDIF.
  ELSE.
    IF p_user IS INITIAL OR p_passwd IS INITIAL.
      MESSAGE e999 WITH 'É obrigatório preencher todos os campos.'.
    ENDIF.
  ENDIF.

** Faz o processamento para os bins
  IF p_bin = 'X'.

** Para estes tipos de posições no depósito vai-se buscar todos os bins
** colocados no ecrã de selecção e não por nivel
    IF p_lgtyp EQ 'DCK' OR
       p_lgtyp EQ 'PUL' OR
       p_lgtyp EQ 'DEV'.

      SELECT lgpla INTO CORRESPONDING FIELDS OF TABLE tbins
                   FROM lagp
                   WHERE lgnum = p_lgnum
                   AND   lgtyp = p_lgtyp
                   AND   lgpla IN p_lgpla.
    ELSE.

      DATA : bin_aux(4).
      CLEAR bin_aux.
      CONCATENATE '%' p_lgpla-low+7(3) INTO bin_aux.

      SELECT lgpla INTO CORRESPONDING FIELDS OF TABLE tbins
                   FROM lagp
                   WHERE lgnum = p_lgnum
                   AND   lgtyp = p_lgtyp
                   AND   lgpla IN p_lgpla
                   AND   lgpla LIKE bin_aux.

    ENDIF.

    DESCRIBE TABLE tbins LINES linhas.
    MOVE linhas TO aux_linhas.


   lvs_itcpo-tddest = 'ZEB1'.
   lvs_itcpo-tdimmed = 'X'.


    IF NOT p_imp200 IS INITIAL.

      CALL FUNCTION 'OPEN_FORM'
        EXPORTING
          device                      = 'PRINTER'
          dialog                      = 'X'
          form                        = 'ZWM_BIN_1ETIQ'
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

    ELSEIF NOT p_imp300 IS INITIAL.

      CALL FUNCTION 'OPEN_FORM'
        EXPORTING
          device                      = 'PRINTER'
          dialog                      = 'X'
          form                        = 'ZWM_BIN300_1ETIQ'
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

    ENDIF.

    DO aux_linhas TIMES.

      IF NOT p_imp200 IS INITIAL.
        CALL FUNCTION 'START_FORM'
          EXPORTING
            form        = 'ZWM_BIN_1ETIQ'
            language    = sy-langu
            program     = 'ZWMREP0002'
          EXCEPTIONS
            form        = 1
            format      = 2
            unended     = 3
            unopened    = 4
            unused      = 5
            spool_error = 6
            OTHERS      = 7.

      ELSEIF NOT p_imp300 IS INITIAL.
        CALL FUNCTION 'START_FORM'
          EXPORTING
            form        = 'ZWM_BIN300_1ETIQ'
            language    = sy-langu
            program     = 'ZWMREP0002'
          EXCEPTIONS
            form        = 1
            format      = 2
            unended     = 3
            unopened    = 4
            unused      = 5
            spool_error = 6
            OTHERS      = 7.

      ENDIF.

      CLEAR: bin1, bin1_aux.

      READ TABLE tbins INDEX indice.
      MOVE tbins-lgpla TO bin1.
      MOVE tbins-lgpla TO bin1_aux.

** Concatenação do Storage type com a posição
      CALL FUNCTION 'ZWM_CONCATENATE_BIN'
        EXPORTING
          lgtyp = p_lgtyp
          lgpla = bin1_aux
        IMPORTING
          bin   = bin1.

      IF p_sem = 'X'. " Sem Seta

        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            window                   = 'MAIN'
            element                  = 'S_SETA'
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

      ELSEIF p_baixo = 'X'. "Seta para Baixo

        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            window                   = 'MAIN'
            element                  = 'SETA_BAIXO'
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

      ELSEIF p_cima = 'X'. "Seta para Cima

        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            window                   = 'MAIN'
            element                  = 'SETA_CIMA'
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

      CALL FUNCTION 'END_FORM'
        EXCEPTIONS
          unopened                 = 1
          bad_pageformat_for_print = 2
          spool_error              = 3
          OTHERS                   = 4.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      ADD 1 TO indice.

    ENDDO.

* faz a etiqueta de utilizador
  ELSE.

    CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
      EXPORTING
        user_name              = p_user
      IMPORTING
        user_address           = user_address
        user_usr03             = user_usr03
      EXCEPTIONS
        user_address_not_found = 1
        OTHERS                 = 2.

    CLEAR nome.
    CONCATENATE user_usr03-name1 user_usr03-name2
    INTO nome SEPARATED BY space.

    CALL FUNCTION 'OPEN_FORM'
      EXPORTING
        device                      = 'PRINTER'
        dialog                      = 'X'
        form                        = 'ZWM_LABEL_USER'
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

    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        window                   = 'MAIN'
        element                  = 'ETIQ_200'
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

* o close for dá para as duas opções
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

*END-OF-SELECTION.
