*&---------------------------------------------------------------------*
*&  Include           ZWMREP0131_F01
*&---------------------------------------------------------------------*
" GET_WHS
*&---------------------------------------------------------------------*
*&      Form  INV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inv .

  CONSTANTS: lc_lgnum TYPE lgnum VALUE '100'.
  CONSTANTS: lc_rem   TYPE lgtyp VALUE '916'.
  CONSTANTS: lc_lgtyp TYPE lgtyp VALUE 'PKL'.
  CONSTANTS: lc_lgpla TYPE lgpla VALUE 'ACERTO PKL'.
  CONSTANTS: lc_dest  TYPE lgtyp VALUE '999'.
  CONSTANTS: lc_bwlvs TYPE bwlvs VALUE '999'.

  DATA: lv_2step      TYPE flag.
  DATA: lv_cond       TYPE string.
  DATA: lv_tanum      TYPE tanum.
  DATA: ls_return     TYPE bdcmsgcoll.
  DATA: lt_ltak       LIKE ltak       OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap       LIKE ltap       OCCURS 0 WITH HEADER LINE.
  DATA: lt_lqua       LIKE lqua       OCCURS 0 WITH HEADER LINE.
  DATA: lt_lqua_pkl   LIKE lqua       OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap_creat LIKE ltap_creat OCCURS 0 WITH HEADER LINE.

  SELECT *
    FROM lqua INTO TABLE lt_lqua
    WHERE lgnum = lc_lgnum  AND
          lgtyp = lc_rem.

  DELETE lt_lqua WHERE verme >= 0.

  DELETE lt_lqua WHERE charg is INITIAL.

  CHECK lt_lqua[] IS NOT INITIAL.

  REFRESH: lt_ltap_creat.

  LOOP AT lt_lqua.

    CLEAR lt_ltap_creat.
    lt_ltap_creat-werks = lt_lqua-werks.
    lt_ltap_creat-lgort = lt_lqua-lgort.
    lt_ltap_creat-matnr = lt_lqua-matnr.
    lt_ltap_creat-charg = lt_lqua-charg.
    lt_ltap_creat-anfme = abs( lt_lqua-verme ).
    lt_ltap_creat-altme = lt_lqua-meins.

    lt_ltap_creat-vltyp = lc_dest.
    lt_ltap_creat-vlpla = lc_lgpla.
    lt_ltap_creat-nltyp = lt_lqua-lgtyp.
    lt_ltap_creat-nlpla = lt_lqua-lgpla.
    lt_ltap_creat-squit = 'X'.
    APPEND lt_ltap_creat.
  ENDLOOP.

** Criar OT
  IF lt_ltap_creat[] IS NOT INITIAL.

    CLEAR lv_tanum.
    CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
      EXPORTING
        i_lgnum       = lc_lgnum
        i_bwlvs       = lc_bwlvs
      IMPORTING
        e_tanum       = lv_tanum
      TABLES
        t_ltap_creat  = lt_ltap_creat
      EXCEPTIONS
        error_message = 99.

    IF sy-subrc <> 0 OR lv_tanum IS INITIAL.
      CLEAR ls_return.
      ls_return-msgid = sy-msgid.
      ls_return-msgnr = sy-msgno.
      ls_return-msgv1 = sy-msgv1.
      ls_return-msgv2 = sy-msgv2.
      ls_return-msgv3 = sy-msgv3.
      ls_return-msgv4 = sy-msgv4.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = ls_return-msgid
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = ls_return-msgnr
          message_var1   = ls_return-msgv1
          message_var2   = ls_return-msgv2
          message_var3   = ls_return-msgv3
          message_var4   = ls_return-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.                    " INV
