FUNCTION zwm_get_parameter.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_PROCESSO)
*"     REFERENCE(I_PARAMETRO)
*"     REFERENCE(I_ITEM) TYPE  INT2 OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_VALOR)
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  TABLES
*"      T_ZWM001 STRUCTURE  ZWM001 OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: ls_message TYPE bdcmsgcoll.

  CLEAR et_messages.

  IF ti_zwm001[] IS INITIAL.
    CALL FUNCTION 'ZWM_REFRESH_BUFFER'
      EXPORTING
        whs       = i_lgnum
      TABLES
        ti_zwm001 = ti_zwm001.
  ENDIF.

  SORT ti_zwm001 BY armazem processo parametro item.

  READ TABLE ti_zwm001 WITH KEY armazem   = i_lgnum
                                processo  = i_processo
                                parametro = i_parametro
                                BINARY SEARCH.

  IF sy-subrc <> 0.
**  Parametro inválido &/&/&
    ls_message-msgtyp = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid = 'ZWM001'.
    ls_message-msgnr = '001'.
    ls_message-msgv1 = i_lgnum.
    ls_message-msgv2 = i_processo.
    ls_message-msgv3 = i_parametro.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  READ TABLE ti_zwm001 WITH KEY armazem   = i_lgnum
                                processo  = i_processo
                                parametro = i_parametro
                                item      = i_item
                                BINARY SEARCH.
  IF sy-subrc = 0.
    MOVE ti_zwm001-valor TO e_valor.
  ENDIF.

  LOOP AT ti_zwm001 WHERE armazem   = i_lgnum     AND
                          processo  = i_processo  AND
                          parametro = i_parametro.
    CLEAR t_zwm001.
    t_zwm001 = ti_zwm001.
    APPEND t_zwm001.
  ENDLOOP.


ENDFUNCTION.
