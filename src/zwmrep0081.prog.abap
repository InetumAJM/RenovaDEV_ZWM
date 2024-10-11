*&---------------------------------------------------------------------*
*& Report  ZWMREP0081                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zwmrep0081 MESSAGE-ID zwmmsg001.

TABLES: lagp, lqua.

DATA: lt_lagp    LIKE lagp       OCCURS 0 WITH HEADER LINE,
      lt_lqua    LIKE lqua       OCCURS 0 WITH HEADER LINE,
      ltap_creat LIKE ltap_creat OCCURS 0 WITH HEADER LINE.

DATA: xlagp LIKE lagp,
      tanum LIKE ltak-tanum.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.

PARAMETERS lgnum  LIKE lqua-lgnum DEFAULT '100' OBLIGATORY.
PARAMETERS slgtyp LIKE lqua-lgtyp OBLIGATORY.
SELECT-OPTIONS slgpla FOR lqua-lgpla.
PARAMETERS dlgtyp LIKE lagp-lgtyp OBLIGATORY.
*SELECT-OPTIONS dlgpla FOR lagp-lgpla..

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON slgtyp.
  IF slgtyp <> 'DRI' AND slgtyp <> 'BLK' AND slgtyp <> 'Z01'.
    MESSAGE e261 WITH slgtyp.
  ENDIF.

AT SELECTION-SCREEN ON dlgtyp.
  IF dlgtyp <> 'DRI' AND slgtyp <> 'BLK' AND dlgtyp <> 'Z01'.
    MESSAGE e262 WITH dlgtyp.
  ENDIF.

  IF dlgtyp = slgtyp.
    MESSAGE e262 WITH dlgtyp.
  ENDIF.

INITIALIZATION.

START-OF-SELECTION.

  CLEAR: lt_lagp, lt_lqua.
  REFRESH: lt_lagp, lt_lqua.

  SELECT * INTO TABLE lt_lagp
      FROM lagp
          WHERE lgnum = lgnum
            AND lgtyp = slgtyp
            AND lgpla IN slgpla.

  CHECK NOT lt_lagp[] IS INITIAL.

  SELECT * INTO TABLE lt_lqua
      FROM lqua
          FOR ALL ENTRIES IN lt_lagp
              WHERE lgnum = lt_lagp-lgnum
                AND lgtyp = lt_lagp-lgtyp
                AND lgpla = lt_lagp-lgpla.

  CHECK NOT lt_lqua[] IS INITIAL.

END-OF-SELECTION.

  SORT lt_lagp.

**  Criar bins no storage type de destino iguais aos do
**  storage type de origem caso ainda não existão


  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 20
      text       = text-001.

  LOOP AT lt_lagp.
    CLEAR lagp.
    SELECT SINGLE *
        FROM lagp
            WHERE lgnum = lgnum
              AND lgtyp = dlgtyp
              AND lgpla = lt_lagp-lgpla.

    IF sy-subrc <> 0.

      CLEAR xlagp.
      MOVE lt_lagp-lgnum TO xlagp-lgnum.
      xlagp-lgtyp = dlgtyp.
      MOVE lt_lagp-lgpla TO xlagp-lgpla.
      MOVE lt_lagp-lgber TO xlagp-lgber.
      MOVE lt_lagp-lptyp TO xlagp-lptyp.
      MOVE lt_lagp-brand TO xlagp-brand.

      CALL FUNCTION 'L_LAGP_HINZUFUEGEN'
        EXPORTING
          xlagp = xlagp.
    ENDIF.
  ENDLOOP.


  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = text-002.
**  Transferir stock
  LOOP AT lt_lagp.

    CLEAR ltap_creat.
    REFRESH ltap_creat.
    LOOP AT lt_lqua WHERE lgnum = lt_lagp-lgnum
                      AND lgtyp = lt_lagp-lgtyp
                      AND lgpla = lt_lagp-lgpla.

      MOVE: lt_lqua-werks  TO ltap_creat-werks,
            lt_lqua-lgort  TO ltap_creat-lgort,
            lt_lqua-matnr  TO ltap_creat-matnr,
            lt_lqua-charg  TO ltap_creat-charg,
            lt_lqua-bestq  TO ltap_creat-bestq,
            lt_lqua-sobkz  TO ltap_creat-sobkz,
            lt_lqua-sonum  TO ltap_creat-sonum,
            lt_lqua-verme  TO ltap_creat-anfme,
            lt_lqua-meins  TO ltap_creat-altme,
            lt_lqua-lqnum  TO ltap_creat-vlqnr,
            lt_lqua-lenum  TO ltap_creat-vlenr,
            lt_lqua-lgtyp  TO ltap_creat-vltyp,
            lt_lqua-lgpla  TO ltap_creat-vlpla,
            lt_lqua-lgpla  TO ltap_creat-nlpla,
            dlgtyp         TO ltap_creat-nltyp,
            lt_lqua-lenum  TO ltap_creat-nlenr,
            lt_lqua-letyp  TO ltap_creat-letyp,
            'X'            TO ltap_creat-squit.
      APPEND ltap_creat.
      CLEAR ltap_creat.
    ENDLOOP.

    CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
      EXPORTING
        i_lgnum       = lgnum
        i_bwlvs       = '999'
        i_commit_work = 'X'
      TABLES
        t_ltap_creat  = ltap_creat[]
      EXCEPTIONS
        error_message = 99.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDLOOP.

  MESSAGE s263.
