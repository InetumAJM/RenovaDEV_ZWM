FUNCTION zwm_material_check_pal.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(I_MATNR) TYPE  MATNR
*"     REFERENCE(I_MENGE) OPTIONAL
*"     REFERENCE(I_MEINS) TYPE  MEINS OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_MENGE_PAL)
*"     REFERENCE(E_MEINS_PAL) TYPE  MEINS
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: lv_menge     TYPE menge_d,
          lv_menge_pal TYPE menge_d,
          lv_meins_pal TYPE meins,
          lv_lhmg1     TYPE lvs_lhmng1,
          lv_lhme1     TYPE lhmeh1.

  DATA: ls_message TYPE bdcmsgcoll.

  lv_menge     = i_menge.
  lv_meins_pal = i_meins.

  CLEAR: et_messages, e_menge_pal, e_meins_pal.

  IF NOT i_lgnum IS INITIAL.
    SELECT SINGLE lhmg1 FROM mlgn
                        INTO (lv_lhmg1)
                        WHERE matnr = i_matnr AND
                              lgnum = i_lgnum.
  ENDIF.

  IF lv_lhmg1 IS INITIAL.
    lv_lhmg1 = 1.
    lv_lhme1 = 'PAL'.
  ENDIF.

  CALL FUNCTION 'ZWM_MATERIAL_CONVERT_UNIT'
    EXPORTING
      i_matnr     = i_matnr
      i_menge     = lv_lhmg1
      i_meinh     = lv_lhme1
    IMPORTING
      e_menge     = lv_menge_pal
      et_messages = et_messages
    CHANGING
      c_meinh     = lv_meins_pal
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    RAISE error.
  ENDIF.

  IF NOT lv_menge IS INITIAL.
    IF lv_menge > lv_menge_pal.
**  Quantidade de Paletização ultrapassada para Material &
      ls_message-msgtyp = 'E'.
      ls_message-msgid  = 'ZRF001'.
      ls_message-msgnr  = '283'.
      ls_message-msgv1  = i_matnr.
      APPEND ls_message TO et_messages.
      RAISE error.
    ENDIF.
  ENDIF.

  e_meins_pal = lv_meins_pal.
  e_menge_pal = lv_menge_pal.



ENDFUNCTION.
