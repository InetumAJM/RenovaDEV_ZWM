FUNCTION ZWM_BIN_LOCK_UNLOCK .
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_LGTYP) TYPE  LGTYP
*"     REFERENCE(I_LGPLA) TYPE  LGPLA
*"     REFERENCE(I_SKZUE) TYPE  LAGP_SKZUE OPTIONAL
*"     REFERENCE(I_SKZUA) TYPE  LAGP_SKZUA OPTIONAL
*"     REFERENCE(I_SPGRU) TYPE  LVS_SPGRU OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"--------------------------------------------------------------------
  DATA: ls_lagp    TYPE lagp,
        ls_lagpv   TYPE lagpv,
        ls_message TYPE bdcmsgcoll.

  CLEAR: et_messages.

  SELECT SINGLE * FROM lagp
                  INTO ls_lagp
                  WHERE lgnum = i_lgnum AND
                        lgtyp = i_lgtyp AND
                        lgpla = i_lgpla.

  IF sy-subrc <> 0.
**  Posição & inválida
    ls_message-msgtyp  = 'E'.
    ls_message-msgspra = sy-langu.
    ls_message-msgid   = 'YRBL_WM_001'.
    ls_message-msgnr   = '018'.
    ls_message-msgv1   = i_lgpla.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

  MOVE-CORRESPONDING ls_lagp TO ls_lagpv.

** Bloqueios
***********************************************************************
  ls_lagpv-skzue = i_skzue.
  ls_lagpv-skzua = i_skzua.
  ls_lagpv-spgru = i_spgru.

** Bloqueia
***********************************************************************
  CALL FUNCTION 'L_LAGP_VERAENDERN'
    EXPORTING
      xlagpv        = ls_lagpv
    EXCEPTIONS
      error_message = 1.

  IF sy-subrc <> 0 AND sy-msgty EQ 'E' OR sy-msgty EQ 'A'.
    ls_message-msgtyp  = sy-msgty.
    ls_message-msgspra = sy-langu.
    ls_message-msgid   = sy-msgid.
    ls_message-msgnr   = sy-msgno.
    ls_message-msgv1   = sy-msgv1.
    ls_message-msgv2   = sy-msgv2.
    ls_message-msgv3   = sy-msgv3.
    ls_message-msgv4   = sy-msgv4.
    APPEND ls_message TO et_messages.
    ROLLBACK WORK.
    RAISE error.
  ENDIF.

  COMMIT WORK.
ENDFUNCTION.
