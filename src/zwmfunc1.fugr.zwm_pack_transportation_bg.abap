FUNCTION zwm_pack_transportation_bg.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     VALUE(I_REFNR) TYPE  LVS_REFNR OPTIONAL
*"     VALUE(IT_PACK_IN_TRANSP) TYPE  ZWM_T_PACK_IN_TRANSP OPTIONAL
*"     VALUE(I_LOCK_KEY) TYPE  CHAR50 OPTIONAL
*"----------------------------------------------------------------------
  DATA: lt_messages       TYPE tab_bdcmsgcoll,
        lt_zwm051         TYPE TABLE OF zwm051,
        lt_t311a          TYPE TABLE OF t311a,
        lt_lips           TYPE TABLE OF lips,
        lt_pack_in_transp TYPE zwm01_t_pack_in_transp.

  DATA: ls_zwm051         TYPE zwm051,
        ls_pack_in_transp TYPE zwm01_pack_in_transp.

  DATA: lv_vemng       TYPE vemng,
        lv_message_var TYPE bdc_vtext1,
        lv_locked      TYPE flag.


  FIELD-SYMBOLS: <ls_lips>   TYPE lips,
                 <ls_zwm051> TYPE zwm051.

** Valida Grupo
***********************************************************************
  SELECT * FROM t311a
     INTO TABLE lt_t311a
     WHERE lgnum = i_lgnum AND
           refnr = i_refnr.

  IF sy-subrc <> 0.
** 	Grupo & inválido
    EXIT.
  ENDIF.

  lt_pack_in_transp = it_pack_in_transp.

  DO 1 TIMES.
    CHECK lt_pack_in_transp IS INITIAL.

** Retorna Backups
***********************************************************************
    SELECT * FROM zwm051
       INTO TABLE lt_zwm051
       WHERE lgnum = i_lgnum AND
             refnr = i_refnr.


    CHECK NOT lt_zwm051 IS INITIAL.

** Retorna Remessas
***********************************************************************
    SELECT * FROM lips
       INTO TABLE lt_lips
       FOR ALL ENTRIES IN lt_t311a
       WHERE vbeln = lt_t311a-rbnum.

    CHECK sy-subrc EQ 0.

    READ TABLE lt_zwm051 ASSIGNING <ls_zwm051> INDEX 1.
    IF <ls_zwm051>-vbeln = '9999999999' OR <ls_zwm051>-vbeln IS INITIAL.

    ELSE.
      LOOP AT lt_lips ASSIGNING <ls_lips>.

        LOOP AT lt_zwm051 ASSIGNING <ls_zwm051> WHERE vbeln = <ls_lips>-vbeln.
          EXIT.
        ENDLOOP.
        IF sy-subrc <> 0.
          DELETE lt_lips WHERE vbeln = <ls_lips>-vbeln.
        ENDIF.
      ENDLOOP.

    ENDIF.

** Separa Remessas
***********************************************************************
    LOOP AT lt_zwm051 ASSIGNING <ls_zwm051>.
      CLEAR ls_pack_in_transp.

      ls_pack_in_transp-exidv = <ls_zwm051>-exidv.

      LOOP AT lt_lips ASSIGNING <ls_lips> WHERE matnr = <ls_zwm051>-matnr AND
                                                charg = <ls_zwm051>-charg AND
                                                lfimg > 0.

        ls_pack_in_transp-vbeln = <ls_lips>-vbeln.
        ls_pack_in_transp-posnr = <ls_lips>-posnr.

        IF <ls_lips>-lfimg > <ls_zwm051>-vemng.
          lv_vemng = <ls_zwm051>-vemng.
        ELSE.
          lv_vemng = <ls_lips>-lfimg.
        ENDIF.

        <ls_lips>-lfimg = <ls_lips>-lfimg - lv_vemng.
        <ls_zwm051>-vemng = <ls_zwm051>-vemng - lv_vemng.

        ls_pack_in_transp-vemng = lv_vemng.
        ls_pack_in_transp-vrkme = <ls_zwm051>-vemeh.

        APPEND ls_pack_in_transp TO lt_pack_in_transp.

        IF <ls_zwm051>-vemng EQ 0.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDDO.

** Embala
***********************************************************************
  DO 2 TIMES.
    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    CALL FUNCTION 'ZWM_PACK_TRANSPORTATION'
      EXPORTING
        i_lgnum     = i_lgnum
        i_refnr     = i_refnr
        it_items    = lt_pack_in_transp
        i_pack      = abap_true
      IMPORTING
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc EQ 0.
      EXIT.
    ENDIF.
  ENDDO.

** Remove Bloqueio
***********************************************************************

  IMPORT lv_locked TO lv_locked FROM DATABASE indx(zp) ID i_lock_key.

  CLEAR lv_locked.
  EXPORT lv_locked TO DATABASE indx(zp) ID i_lock_key.
ENDFUNCTION.
