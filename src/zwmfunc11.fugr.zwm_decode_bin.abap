FUNCTION zwm_decode_bin.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  LGNUM
*"     REFERENCE(IV_BIN_CODE) TYPE  CHAR14
*"  EXPORTING
*"     REFERENCE(EV_BIN) TYPE  CHAR14
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: lv_lgtyp_char     TYPE char3,
        lv_lgpla_char     TYPE char10,
        lv_valid_bin_code TYPE char14.

  DATA: ls_message TYPE bdcmsgcoll.

**********************************************************************

  " Verifica se o Código contem Espaço na 3 posição ( Lgtyp + Lgpla )
  IF iv_bin_code CA space AND sy-fdpos = 3.

    DATA(lv_pos_space) = sy-fdpos.

    SPLIT iv_bin_code AT space INTO lv_lgtyp_char lv_lgpla_char.

    SELECT lgnum, lgtyp, lgpla, verif
      FROM lagp
      INTO @DATA(ls_lagp)
      WHERE lgnum = @iv_lgnum
        AND lgtyp = @lv_lgtyp_char
        AND lgpla = @lv_lgpla_char.
    ENDSELECT.
    IF sy-subrc = 0.

      IF ls_lagp-verif IS INITIAL.
        ev_bin = iv_bin_code.
        RETURN.
      ENDIF.

    ENDIF.

    " Código da Posição(&) Inválido!
    ls_message-msgid  = 'ZWMMSG001'.
    ls_message-msgtyp = 'E'.
    ls_message-msgnr  = '348'.
    ls_message-msgv1  = iv_bin_code.
    APPEND ls_message TO et_messages.
    RAISE error.

  ELSE.

    " Valida que o bin code tem apenas 10 caracteres
    IF strlen( iv_bin_code ) = 10.

      SELECT lgnum, lgtyp, lgpla, verif
       FROM lagp
       INTO TABLE @DATA(lt_lagp)
       WHERE lgnum = @iv_lgnum
         AND verif = @iv_bin_code.
      IF sy-subrc = 0 AND lt_lagp IS NOT INITIAL.

        IF lines( lt_lagp ) = 1.

          READ TABLE lt_lagp INTO DATA(ls_lagp_aux) INDEX 1.
          IF sy-subrc = 0.
            CONCATENATE ls_lagp_aux-lgtyp ls_lagp_aux-lgpla INTO lv_valid_bin_code SEPARATED BY space.
            ev_bin = lv_valid_bin_code.
            RETURN.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

    " Código da Posição(&) Inválido!
    ls_message-msgid  = 'ZWMMSG001'.
    ls_message-msgtyp = 'E'.
    ls_message-msgnr  = '348'.
    ls_message-msgv1  = iv_bin_code.
    APPEND ls_message TO et_messages.
    RAISE error.

  ENDIF.

ENDFUNCTION.
