*&---------------------------------------------------------------------*
*&  Include           Z1OMMP05_V4B_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_screen_0100 .
  PERFORM reset_0100.

  CALL SCREEN 0100.
ENDFORM.                    " CALL_SCREEN_0100
*&---------------------------------------------------------------------*
*&      Form  RESET_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0100.

  CLEAR: scr0100.

ENDFORM.                    " RESET_0100
*&---------------------------------------------------------------------*
*&      Form  CHECK_0100_REFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_0100_refnr.
  DATA: lv_refnr TYPE lvs_refnr,
        lv_subrc TYPE sysubrc.

  CHECK NOT scr0100-refnr IS INITIAL.

  lv_refnr = scr0100-refnr.
  PERFORM reset_0100_refnr.
  scr0100-refnr = lv_refnr.

  PERFORM pack_in_transportation CHANGING lv_subrc.
  CHECK lv_subrc EQ 0.

** Fim
***********************************************************************

** Fim do Processo com sucesso!
  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWMSG001'
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = '009'.

  PERFORM reset_0100.
ENDFORM.                    " CHECK_0100_REFNR
*&---------------------------------------------------------------------*
*&      Form  RESET_0100_REFNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_0100_refnr.
  PERFORM reset_0100.
ENDFORM.                    " RESET_0100_REFNR
*&---------------------------------------------------------------------*
*&      Form  EXIT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_0100.
  LEAVE TO SCREEN 0.
ENDFORM.                                                    " EXIT_0100


*&---------------------------------------------------------------------*
*&      Form  pack_in_transportation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pack_in_transportation CHANGING cv_subrc TYPE sysubrc.
  DATA: lt_messages       TYPE tab_bdcmsgcoll,
        lt_zwm051         TYPE TABLE OF zwm051,
        lt_t311a          TYPE TABLE OF t311a,
        lt_lips           TYPE TABLE OF lips,
        lt_pack_in_transp TYPE zwm01_t_pack_in_transp.

  DATA: ls_zwm051         TYPE zwm051,
        ls_pack_in_transp TYPE zwm01_pack_in_transp.

  DATA: lv_vemng       TYPE vemng,
        lv_message_var TYPE bdc_vtext1.


  FIELD-SYMBOLS: <ls_lips> TYPE lips,
                 <ls_zwm051> TYPE zwm051.

  CLEAR: cv_subrc.

** Valida Grupo
***********************************************************************
  SELECT * FROM t311a
     INTO TABLE lt_t311a
     WHERE lgnum = gv_lgnum AND
           refnr = scr0100-refnr.

  IF sy-subrc <> 0.
** 	Grupo & inválido

    MOVE scr0100-refnr TO lv_message_var.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '008'
        message_var1   = lv_message_var.

    PERFORM reset_0100_refnr.
    cv_subrc = 4.
    EXIT.
  ENDIF.

** Retorna Backups
***********************************************************************
  SELECT * FROM zwm051
     INTO TABLE lt_zwm051
     WHERE lgnum = gv_lgnum AND
           refnr = scr0100-refnr.


  IF lt_zwm051 IS INITIAL.
    cv_subrc = 4.
**  Grupo não pode ser embalado, sem dados de Transporte
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWM001'
        message_lang   = 'P'
        message_type   = 'E'
        message_number = '072'.
    RETURN.
**  PERFORM get_data_transport CHANGING lt_zwm051.
  ENDIF.

  CHECK NOT lt_zwm051 IS INITIAL.

** Retorna Remessas
***********************************************************************
  SELECT * FROM lips
     INTO TABLE lt_lips
     FOR ALL ENTRIES IN lt_t311a
     WHERE vbeln = lt_t311a-rbnum.

  CHECK sy-subrc EQ 0.

  READ TABLE lt_zwm051  ASSIGNING <ls_zwm051> INDEX 1.
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

** Embala
***********************************************************************
  CALL FUNCTION 'ZWM_PACK_TRANSPORTATION'
    EXPORTING
      i_lgnum     = gv_lgnum
      i_refnr     = scr0100-refnr
      it_items    = lt_pack_in_transp
      i_pack      = abap_true
    IMPORTING
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    CALL FUNCTION 'YWM_RF_MESSAGE_SCREEN'
      EXPORTING
        it_messages = lt_messages.
    EXIT.
  ENDIF.
ENDFORM.                    " PACK_IN_TRANSPORTATION
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .
  gv_lgnum = '100'.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_TRANSPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_transport CHANGING ct_zwm051 TYPE gty_t_zwm051.
  DATA: lt_zwm026 TYPE TABLE OF zwm026,
        lt_vekp   TYPE TABLE OF vekp,
        lt_ltak   TYPE TABLE OF ltak,
        lt_ltap   TYPE TABLE OF ltap.

  DATA: ls_zwm051 TYPE zwm051,
        ls_zwm026 TYPE zwm026,
        ls_vekp   TYPE vekp,
        ls_ltak   TYPE ltak,
        ls_ltap   TYPE ltap.

** Paletes de Picking
***********************************************************************
  SELECT * FROM zwm026
           INTO TABLE lt_zwm026
           WHERE armazem = gv_lgnum AND
                 grupo = scr0100-refnr.

  LOOP AT lt_zwm026 INTO ls_zwm026.
    CLEAR: ls_zwm051.
    ls_zwm051-lgnum = ls_zwm026-armazem.
    ls_zwm051-refnr = ls_zwm026-grupo.
    ls_zwm051-vbeln = ls_zwm026-remessa.
    ls_zwm051-posnr = ls_zwm026-posnr.
    ls_zwm051-exidv = ls_zwm026-sscc.
    ls_zwm051-matnr = ls_zwm026-material.
    ls_zwm051-charg = ls_zwm026-lote.
    ls_zwm051-vemng = ls_zwm026-quantidade.
    ls_zwm051-vemeh = ls_zwm026-unidade.
    APPEND ls_zwm051 TO ct_zwm051.
  ENDLOOP.

** Paletes Completas
***********************************************************************
  DO 1 TIMES.
    SELECT * FROM ltak
             INTO TABLE lt_ltak
             WHERE lgnum = gv_lgnum AND
                   refnr = scr0100-refnr.
    CHECK sy-subrc EQ 0.

    SELECT * FROM ltap
             INTO TABLE lt_ltap
             FOR ALL ENTRIES IN lt_ltak
             WHERE lgnum = lt_ltak-lgnum AND
                   tanum = lt_ltak-tanum.
    CHECK sy-subrc EQ 0.

    DELETE lt_ltap WHERE vlenr IS INITIAL.
    CHECK NOT lt_ltap IS INITIAL.

    LOOP AT lt_ltap INTO ls_ltap.
      CHECK NOT ls_ltap-vbeln IS INITIAL.

      CLEAR: ls_zwm051.
      ls_zwm051-lgnum = ls_ltap-lgnum.
      ls_zwm051-refnr = scr0100-refnr.
      ls_zwm051-vbeln = ls_ltap-vbeln.
      ls_zwm051-posnr = ls_ltap-posnr.
      ls_zwm051-exidv = ls_ltap-vlenr.
      ls_zwm051-matnr = ls_ltap-matnr.
      ls_zwm051-charg = ls_ltap-charg.
      ls_zwm051-vemng = ls_ltap-vista.
      ls_zwm051-vemeh = ls_ltap-altme.
      APPEND ls_zwm051 TO ct_zwm051.
    ENDLOOP.
  ENDDO.

  CHECK NOT ct_zwm051 IS INITIAL.

  SELECT * FROM vekp
           INTO TABLE lt_vekp
           FOR ALL ENTRIES IN ct_zwm051
           WHERE exidv = ct_zwm051-exidv AND
                 status <> '0060'.

  DELETE lt_vekp WHERE vpobj EQ '01'.
  CHECK NOT lt_vekp IS INITIAL.
  SORT lt_vekp BY exidv.

  LOOP AT ct_zwm051 INTO ls_zwm051.
    READ TABLE lt_vekp
          INTO ls_vekp
          WITH KEY exidv = ls_zwm051-exidv
          BINARY SEARCH.
    CHECK sy-subrc <> 0.
    DELETE ct_zwm051 WHERE exidv EQ ls_zwm051-exidv.
  ENDLOOP.

ENDFORM.                    " GET_DATA_TRANSPORT
