FUNCTION zwm_rfc_display_portaria.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(MODO) TYPE  ZWM_MODO_DISPLAY DEFAULT 'D'
*"     VALUE(CHAMADA_RFC) TYPE  CHAR1 DEFAULT 'X'
*"     VALUE(PORTA) TYPE  LGTOR OPTIONAL
*"     VALUE(I_LGNUM) TYPE  LGNUM DEFAULT '100'
*"  TABLES
*"      REGISTOS STRUCTURE  ZWM042 OPTIONAL
*"  EXCEPTIONS
*"      ERRO
*"      ERRO_MODO
*"----------------------------------------------------------------------

  DATA: msg_text(80),
        erro,
        n_linhas  TYPE i,
        msgv1     TYPE symsgv,
        msgv2     TYPE symsgv,
        wa_zwm042 LIKE zwm042.

  DATA: it_zwm042 LIKE zwm042      OCCURS 0 WITH HEADER LINE,
        dados     LIKE zwm_display OCCURS 0 WITH HEADER LINE.

*& Begin of Modification by Carlos Fernandes - ROFF @ 10.02.2016
  DATA ls_zwm063 TYPE zwm063.
*& End of Modification by Carlos Fernandes - ROFF @ 10.02.2016

*& Begin of Modification by Carlos Fernandes - ROFF @ 10.02.2016
  DATA lv_dest  TYPE rfcdest.
  DATA lv_func  TYPE rs38l_fnam.
  DATA lv_lgnum TYPE lgnum.
*& End of Modification by Carlos Fernandes - ROFF @ 10.02.2016

  REFRESH: it_zwm042, dados.
  CLEAR:   it_zwm042, dados, wa_zwm042, msg_text, msgv1, msgv2, erro.

** Inicializa Log
  PERFORM msg_log_display USING 'I' '' '' '' '' '' ''.
*& Begin of Modification by Carlos Fernandes - ROFF @ 10.02.2016
*** Obter os registos já existentes
*  SELECT * FROM zwm042
*           INTO TABLE it_zwm042
*           WHERE armazem EQ '100'.

  IF i_lgnum IS NOT INITIAL.
    lv_lgnum = i_lgnum.
  ELSE.
    lv_lgnum = '100'.
  ENDIF.

*** Obter os registos já existentes
  SELECT *
    FROM zwm042
    INTO TABLE it_zwm042
    WHERE armazem EQ lv_lgnum.
*& End of Modification by Carlos Fernandes - ROFF @ 10.02.2016

** Estrutura com os dados de saída para o Display
  LOOP AT it_zwm042.
    MOVE-CORRESPONDING it_zwm042 TO dados.
    APPEND dados.
    CLEAR  dados.
  ENDLOOP.


  CASE modo.

    WHEN 'I'.
** Inserir registos
      GET TIME.
      LOOP AT registos INTO wa_zwm042.
        wa_zwm042-data = sy-datum.
        wa_zwm042-hora = sy-uzeit.
        MODIFY zwm042 FROM wa_zwm042.

        IF sy-subrc EQ 0.
          COMMIT WORK.
          MOVE-CORRESPONDING wa_zwm042 TO dados.
          dados-novo = 'X'.
          APPEND dados.
          CLEAR  dados.
**        Log
          msgv1 = wa_zwm042-porta.
          msgv2 = wa_zwm042-num_entrada.
          PERFORM msg_log_display USING 'M'
                                        'S' '246' msgv1 msgv2 '' ''.

        ELSE.
          ROLLBACK WORK.
          erro = 'X'.
**        Log
          msgv1 = wa_zwm042-porta.
          msgv2 = wa_zwm042-num_entrada.
          PERFORM msg_log_display USING 'M'
                                        'E' '244' msgv1 msgv2 '' ''.

        ENDIF.

      ENDLOOP.

    WHEN 'A'.
** Apagar registos
*& Begin of Modification by Carlos Fernandes - ROFF @ 10.02.2016
*      DELETE FROM zwm042 WHERE armazem EQ '100'
*                           AND porta   EQ porta.
      DELETE FROM zwm042 WHERE armazem EQ lv_lgnum
                           AND porta   EQ porta.
*& End of Modification by Carlos Fernandes - ROFF @ 10.02.2016
      IF sy-subrc EQ 0.
        COMMIT WORK.
        DELETE dados WHERE porta EQ porta.
**      Log
        msgv1 = porta.
        PERFORM msg_log_display USING 'M'
                                      'S' '247' msgv1 '' '' ''.
      ELSE.
        ROLLBACK WORK.
        erro = 'X'.
**      Log
        msgv1 = porta.
        PERFORM msg_log_display USING 'M'
                                      'E' '245' msgv1 '' '' ''.
      ENDIF.

    WHEN 'D'.
** Apenas os registos actuais
**    Log
      IF chamada_rfc EQ 'X'.
        msgv1 = 'Chamada RFC efectuada pelo Display'.
      ELSE.
        msgv1 = 'Apenas actualização Display'.
      ENDIF.
      PERFORM msg_log_display USING 'M'
                                    'S' '000' msgv1 '' '' ''.

    WHEN OTHERS.
**    Log
      msgv1 = 'Modo Inválido: '.
      msgv2 = modo.
      PERFORM msg_log_display USING 'M'
                                    'E' '000' msgv1 msgv2 '' ''.
      PERFORM msg_log_display USING 'F' '' '' '' '' '' ''.
      RAISE erro_modo.

  ENDCASE.


  DELETE dados WHERE porta IS INITIAL.

  SORT dados BY porta ASCENDING novo DESCENDING.
  DELETE ADJACENT DUPLICATES FROM dados COMPARING porta.

*& Begin of Modification by Carlos Fernandes - ROFF @ 10.02.2016
*** Envia info para Display
*  CALL FUNCTION 'RFC_RNV_SAP_DISPLAY'
*    DESTINATION 'ZDISPLAY_PORTARIA'
*    TABLES
*      displaydata           = dados
*    EXCEPTIONS
*      communication_failure = 1  MESSAGE msg_text
*      system_failure        = 2  MESSAGE msg_text
*      OTHERS                = 3.

  SELECT SINGLE *
    FROM zwm063
    INTO ls_zwm063
    WHERE lgnum = lv_lgnum.
  IF sy-subrc EQ 0.
    IF ls_zwm063-funtion_name IS NOT INITIAL.
      IF ls_zwm063-destination IS INITIAL.
        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname           = ls_zwm063-funtion_name
          EXCEPTIONS
            function_not_exist = 1
            OTHERS             = 2.
        IF sy-subrc EQ 0.
          lv_func = ls_zwm063-funtion_name.
        ENDIF.
      ELSE.
        lv_func = ls_zwm063-funtion_name.
        lv_dest = ls_zwm063-destination.
      ENDIF.
    ENDIF.
  ELSE.
    lv_func = 'RFC_RNV_SAP_DISPLAY'.
    lv_dest = 'ZDISPLAY_PORTARIA'.
  ENDIF.

** Envia info para Display
  IF lv_func IS NOT INITIAL.
    IF lv_dest IS NOT INITIAL.
      CALL FUNCTION lv_func DESTINATION lv_dest
        TABLES
          displaydata           = dados
        EXCEPTIONS
          communication_failure = 1  MESSAGE msg_text
          system_failure        = 2  MESSAGE msg_text
          OTHERS                = 3.
    ELSE.
      CALL FUNCTION lv_func
        TABLES
          displaydata           = dados
        EXCEPTIONS
          communication_failure = 1
          system_failure        = 2
          OTHERS                = 3.
    ENDIF.
*& End of Modification by Carlos Fernandes - ROFF @ 10.02.2016

    IF sy-subrc <> 0.
**  Log
      IF msg_text IS INITIAL.
        PERFORM msg_log_display USING 'M'
                                      'E' '242' '' '' '' ''.
      ELSE.
        msgv1 = msg_text(50).
        msgv2 = msg_text+50(30).
        PERFORM msg_log_display USING 'M'
                                      'E' '000' msgv1 msgv2 '' ''.
      ENDIF.

      PERFORM msg_log_display USING 'F' '' '' '' '' '' ''.
      RAISE erro.

    ELSE.
**  Log
      CLEAR n_linhas.
      DESCRIBE TABLE dados LINES n_linhas.
      msgv1 = n_linhas.
      PERFORM msg_log_display USING 'M'
                                    'S' '248' msgv1 '' '' ''.
    ENDIF.

** Finaliza Log
    PERFORM msg_log_display USING 'F' '' '' '' '' '' ''.

** Erro na gestão da tabela ZWM042
    IF NOT erro IS INITIAL.
      RAISE erro.
    ENDIF.
*& End of Modification by Carlos Fernandes - ROFF @ 10.02.2016
  ENDIF.
*& Begin of Modification by Carlos Fernandes - ROFF @ 10.02.2016

ENDFUNCTION.
