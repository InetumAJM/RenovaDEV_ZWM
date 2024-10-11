FUNCTION zwm_validate_delivery_delete.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lt_ltak TYPE TABLE OF ltak,
        lt_ltap TYPE TABLE OF ltap,
        wa_ltap TYPE ltap.

  DATA: ls_message TYPE bdcmsgcoll.

  DATA: lv_vbeln TYPE vbeln,
        lv_lgnum TYPE lgnum,
        lv_lvstk TYPE lvstk,
        lv_tanum TYPE tanum,
        lv_refnr TYPE lvs_refnr,
        lv_2step TYPE flag.

  CHECK sy-tcode EQ 'VL03N'  OR
        sy-tcode EQ 'VL03'   OR
        sy-tcode EQ 'VL02N'  OR
        sy-tcode EQ 'VL02'   OR
        sy-tcode EQ 'VL06P'  OR
        sy-tcode EQ 'ZWM127' OR
        sy-tcode EQ 'VT02N'  OR
        sy-tcode EQ 'VT03N'  OR
        sy-tcode EQ 'VG02'   OR
        sy-tcode EQ 'SE37'.

** Valida Remessa
***********************************************************************
  SELECT SINGLE vbeln lgnum FROM likp
                            INTO (lv_vbeln, lv_lgnum)
                            WHERE vbeln EQ i_vbeln.


** Valida Picking em 2 passos
***********************************************************************
  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum = lv_lgnum
      i_vbeln = lv_vbeln
    IMPORTING
      e_2step = lv_2step
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.

*  CHECK lv_2step EQ 'X'.

** Valida se Status de Picking Diferente de C
***********************************************************************
  SELECT SINGLE lvstk FROM vbuk
                      INTO lv_lvstk
                      WHERE vbeln = i_vbeln.

  CHECK sy-subrc EQ 0.


  CHECK lv_lvstk <> 'C'.

** Retorna Grupo de Picking
***********************************************************************
  SELECT SINGLE refnr FROM t311a
                      INTO lv_refnr
                      WHERE lgnum = lv_lgnum AND
                            rbnum = lv_vbeln.

  CHECK sy-subrc EQ 0.

** Valida Se tem OT's Criadas
***********************************************************************
  DO 1 TIMES.

    SELECT * FROM ltak
       INTO TABLE lt_ltak
       WHERE lgnum = lv_lgnum AND
             refnr = lv_refnr.

    IF lv_2step NE 'X'.
*      DELETE lt_ltak WHERE vbeln <> lv_vbeln.
      DELETE lt_ltak WHERE benum <> lv_vbeln.
    ELSE.
      DELETE lt_ltak WHERE benum <> lv_vbeln
                       AND benum IS NOT INITIAL.
    ENDIF.

    DELETE lt_ltak WHERE kquit IS NOT INITIAL.

*    CHECK sy-subrc EQ 0.
    CHECK lt_ltak[] IS NOT INITIAL.

    SELECT * FROM ltap
       INTO TABLE lt_ltap
       FOR ALL ENTRIES IN lt_ltak
       WHERE lgnum = lt_ltak-lgnum AND
             tanum = lt_ltak-tanum AND
             vorga NOT IN ('ST','SL').

** Loja Online
    READ TABLE lt_ltap INTO wa_ltap WITH KEY vltyp = 'PKL'
                                             vbeln = lv_vbeln.
    IF sy-subrc = 0.
      REFRESH lt_ltap.
    ENDIF.
  ENDDO.

  IF NOT lt_ltap IS INITIAL.
    AUTHORITY-CHECK OBJECT 'ZWM001'
                  FOR USER sy-uname
                  ID 'LGNUM' FIELD lv_lgnum.

    IF sy-subrc EQ 0.
      ls_message-msgtyp   = 'W'.
    ELSE.
      ls_message-msgtyp   = 'E'.
    ENDIF.

**  Remessa & do Grupo & já tem contem OT's Criadas
    ls_message-msgspra = sy-langu.
    ls_message-msgid   = 'ZWMSG001'.
    ls_message-msgnr   = '010'.
    ls_message-msgv1   = lv_vbeln.
    ls_message-msgv2   = lv_refnr.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.
ENDFUNCTION.
