FUNCTION ZWM_BIN_CHECK .
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_LGTYP) TYPE  LGTYP OPTIONAL
*"     REFERENCE(I_LGPLA_VAL) TYPE  LGPLA OPTIONAL
*"     REFERENCE(I_DIRECT) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_VOLUME) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_PRESENCE) TYPE  C DEFAULT '1'
*"     REFERENCE(I_TO) TYPE  C OPTIONAL
*"     REFERENCE(I_LOCK_IN) TYPE  C OPTIONAL
*"     REFERENCE(I_LOCK_OUT) TYPE  C OPTIONAL
*"     REFERENCE(I_DYNAMIC) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(E_EXIDV) TYPE  EXIDV
*"     REFERENCE(ES_LAGP) TYPE  LAGP
*"     REFERENCE(ET_LQUA) TYPE  HUSTOCK_LQUA_T
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  CHANGING
*"     REFERENCE(C_LGPLA) TYPE  ANY
*"  EXCEPTIONS
*"      ERROR
*"--------------------------------------------------------------------

  TYPES: BEGIN OF lty_ltak_mod,
          lgnum TYPE lgnum,
          tanum TYPE tanum,
         END OF  lty_ltak_mod.

  DATA: lt_messages     TYPE TABLE OF bdcmsgcoll,
        lt_lagp         TYPE TABLE OF lagp,
        lt_ltap         TYPE TABLE OF ltap,
        lt_ltak_mod     TYPE TABLE OF lty_ltak_mod.

  DATA: lv_lgtyp      TYPE lgtyp,
        lv_lgpla      TYPE barcode,
        lv_lines      TYPE i,
        lv_barcode    TYPE barcode,
        lv_lgtyp_c    TYPE lgtyp,
        lv_lgpla_c    TYPE lgpla,
        lr_uname      TYPE RANGE OF sy-uname,
        lr_lgtyp      TYPE RANGE OF lagp-lgtyp.

  DATA: ls_message    TYPE bdcmsgcoll,
        ls_r_lgtyp    LIKE LINE OF lr_lgtyp,
        ls_vekp       TYPE vekp.



***********************************************************************
  lv_lgpla = c_lgpla.
  lv_lgtyp = i_lgtyp.

  CLEAR: et_messages, e_exidv, es_lagp, c_lgpla, et_lqua.

  CHECK NOT lv_lgpla IS INITIAL.

  IF NOT lv_lgtyp IS INITIAL.
    ls_r_lgtyp-sign   = 'I'.
    ls_r_lgtyp-option = 'EQ'.
    ls_r_lgtyp-low    = lv_lgtyp.
    APPEND ls_r_lgtyp TO lr_lgtyp.
  ENDIF.

  DO 1 TIMES.
**  Valida se Posição Directa
***********************************************************************
    IF NOT i_direct IS INITIAL AND lv_lgpla(10) NE '0000000000'.
**    Valida Posição diretamente
      IF NOT i_dynamic IS INITIAL.
        SELECT * FROM lagp INTO es_lagp UP TO 1 ROWS
                WHERE lgnum EQ i_lgnum
                  AND lgtyp IN lr_lgtyp
                  AND lgpla EQ lv_lgpla(10).
        ENDSELECT.
      ELSE.
        SELECT * FROM lagp INTO es_lagp UP TO 1 ROWS
                WHERE lgnum EQ i_lgnum
                  AND lgtyp IN lr_lgtyp
                  AND lgpla EQ lv_lgpla(10)
                  AND kzdyn EQ ''. " Ignora Posições Dinamicas
        ENDSELECT.
      ENDIF.
    ENDIF.


**  Valida se Campo de Verificação
***********************************************************************
    IF lv_lgpla(10) NE '0000000000'.
      SELECT * FROM lagp INTO TABLE lt_lagp
       WHERE lgnum EQ i_lgnum
         AND verif EQ lv_lgpla(10).

      DESCRIBE TABLE lt_lagp LINES lv_lines.

      CASE lv_lines.
        WHEN 0.
**        Não Encontrado
        WHEN 1.
**        Encontrada
          READ TABLE lt_lagp INTO es_lagp INDEX 1.
          EXIT.
        WHEN OTHERS.
**        Confronta com Posição Esperada
          IF NOT i_lgpla_val IS INITIAL.
            READ TABLE lt_lagp INTO es_lagp
                           WITH KEY lgpla = i_lgpla_val.
            CHECK sy-subrc <> 0.
          ENDIF.
      ENDCASE.
    ENDIF.

    CHECK es_lagp IS INITIAL.

** Valida Por Posição Concatenada
***********************************************************************
    MOVE lv_lgpla(3)    TO lv_lgtyp_c.
    MOVE lv_lgpla+4(10) TO lv_lgpla_c.

    SELECT SINGLE * FROM lagp
                    INTO es_lagp
                    WHERE lgnum EQ i_lgnum AND
                          lgtyp EQ lv_lgtyp_c AND
                          lgpla EQ lv_lgpla_c.


**  Indicação Inválida
***********************************************************************
    IF es_lagp IS INITIAL.
      ls_message-msgid  = 'YWM001'.
      ls_message-msgtyp = 'E'.
      IF i_volume IS INITIAL.
**      Posição & inválida
        ls_message-msgnr = '018'.
      ELSE.
**      Posição/Volume & inválida(o)
        ls_message-msgnr = '019'.
      ENDIF.
      ls_message-msgv1 = lv_lgpla.
      APPEND ls_message TO et_messages.

      RAISE error.
    ENDIF.
  ENDDO.

** Validações Adicionais
***********************************************************************
  IF NOT i_lgpla_val IS INITIAL AND es_lagp-lgpla NE i_lgpla_val.
**  Posição & não é a esperada (&)
    ls_message-msgid  = 'YWM001'.
    ls_message-msgtyp = 'E'.
    ls_message-msgnr  = '020'.
    ls_message-msgv1  = es_lagp-lgpla.
    ls_message-msgv2  = i_lgpla_val.
    APPEND ls_message TO et_messages.
    CLEAR es_lagp.
    RAISE error.
  ENDIF.

  IF NOT lv_lgtyp IS INITIAL AND es_lagp-lgtyp NE lv_lgtyp.
**  Posição & não pertence ao Tipo Depósito &
    ls_message-msgid  = 'YWM001'.
    ls_message-msgtyp = 'E'.
    ls_message-msgnr  = '021'.
    ls_message-msgv1  = es_lagp-lgpla.
    ls_message-msgv2  = lv_lgtyp.
    APPEND ls_message TO et_messages.
    CLEAR es_lagp.
    RAISE error.
  ENDIF.

** Valida Bloqueios
***********************************************************************
  IF NOT i_lock_in IS INITIAL.
    CASE i_lock_in.
      WHEN '0'.
        IF es_lagp-skzue EQ abap_true.
**        Posição & bloqueada para Entradas
          CLEAR ls_message.
          ls_message-msgid  = 'YWM001'.
          ls_message-msgtyp = 'E'.
          ls_message-msgnr  = '036'.
          ls_message-msgv1  = es_lagp-lgpla.
          APPEND ls_message TO et_messages.
          CLEAR es_lagp.
          RAISE error.
        ELSEIF es_lagp-skzsi EQ abap_true.
**        Posição & bloqueada para Inventário
          CLEAR ls_message.
          ls_message-msgid  = 'YWM001'.
          ls_message-msgtyp = 'E'.
          ls_message-msgnr  = '040'.
          ls_message-msgv1  = es_lagp-lgpla.
          APPEND ls_message TO et_messages.
          CLEAR es_lagp.
          RAISE error.
        ENDIF.
      WHEN '1'.
        IF es_lagp-skzue EQ abap_false AND
           es_lagp-skzsi EQ abap_false.
**        Posição & não bloqueada para Entradas
          CLEAR ls_message.
          ls_message-msgid  = 'YWM001'.
          ls_message-msgtyp = 'E'.
          ls_message-msgnr  = '038'.
          ls_message-msgv1  = es_lagp-lgpla.
          APPEND ls_message TO et_messages.
          CLEAR es_lagp.
          RAISE error.
        ENDIF.
    ENDCASE.
  ENDIF.


  IF NOT i_lock_out IS INITIAL.
    CASE i_lock_out.
      WHEN '0'.
        IF es_lagp-skzua EQ abap_true.
**        Posição & bloqueada para Saídas
          CLEAR ls_message.
          ls_message-msgid  = 'YWM001'.
          ls_message-msgtyp = 'E'.
          ls_message-msgnr  = '037'.
          ls_message-msgv1  = es_lagp-lgpla.
          APPEND ls_message TO et_messages.
          CLEAR es_lagp.
          RAISE error.
        ELSEIF es_lagp-skzsi EQ abap_true.
**        Posição & bloqueada para Inventário
          CLEAR ls_message.
          ls_message-msgid  = 'YWM001'.
          ls_message-msgtyp = 'E'.
          ls_message-msgnr  = '040'.
          ls_message-msgv1  = es_lagp-lgpla.
          APPEND ls_message TO et_messages.
          CLEAR es_lagp.
          RAISE error.
        ENDIF.
      WHEN '1'.
        IF es_lagp-skzua EQ abap_false AND
           es_lagp-skzsi EQ abap_false.
**        Posição & não bloqueada para Saídas
          CLEAR ls_message.
          ls_message-msgid  = 'YWM001'.
          ls_message-msgtyp = 'E'.
          ls_message-msgnr  = '039'.
          ls_message-msgv1  = es_lagp-lgpla.
          APPEND ls_message TO et_messages.
          CLEAR es_lagp.
          RAISE error.
        ENDIF.
    ENDCASE.
  ENDIF.

** OT's
***********************************************************************
  IF NOT i_to IS INITIAL.
    DO 1 TIMES.
      SELECT lgnum tanum FROM ltak
                         INTO TABLE lt_ltak_mod
                         WHERE lgnum = i_lgnum AND
                               kquit = abap_false.

      CHECK sy-subrc EQ 0.

      SELECT * FROM ltap
         INTO TABLE lt_ltap
         FOR ALL ENTRIES IN lt_ltak_mod
         WHERE lgnum = lt_ltak_mod-lgnum AND
               tanum = lt_ltak_mod-tanum.

      DELETE lt_ltap WHERE ( nltyp <> es_lagp-lgtyp AND
                             vltyp <> es_lagp-lgtyp ) OR
                           ( nlpla <> es_lagp-lgpla AND
                             vlpla <> es_lagp-lgpla ).

    ENDDO.

    CASE i_to.
      WHEN '0'.
        IF NOT lt_ltap IS INITIAL.
**        Posição & com OT's pendentes
          CLEAR ls_message.
          ls_message-msgid  = 'YWM001'.
          ls_message-msgtyp = 'E'.
          ls_message-msgnr  = '043'.
          ls_message-msgv1  = es_lagp-lgpla.
          APPEND ls_message TO et_messages.
          CLEAR es_lagp.
          RAISE error.
        ENDIF.
      WHEN '1'.
        IF lt_ltap IS INITIAL.
**        Posição & sem OT's pendentes
          CLEAR ls_message.
          ls_message-msgid  = 'YWM001'.
          ls_message-msgtyp = 'E'.
          ls_message-msgnr  = '044'.
          ls_message-msgv1  = es_lagp-lgpla.
          APPEND ls_message TO et_messages.
          CLEAR es_lagp.
          RAISE error.
        ENDIF.
    ENDCASE.
  ENDIF.


** Retorno
***********************************************************************
  c_lgpla = es_lagp-lgpla.

** Quantos
  IF et_lqua IS INITIAL.
    SELECT * FROM lqua INTO TABLE et_lqua
     WHERE lgnum EQ es_lagp-lgnum
       AND lgtyp EQ es_lagp-lgtyp
       AND lgpla EQ es_lagp-lgpla.
  ENDIF.
ENDFUNCTION.
