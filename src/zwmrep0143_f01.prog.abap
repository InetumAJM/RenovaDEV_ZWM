*&---------------------------------------------------------------------*
*&  Include           ZWMREP0143_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_of_selection .
  DATA: lt_tanum TYPE TABLE OF tanum.

  DATA: lv_tanum    TYPE tanum,
        lv_canceled TYPE i.

  CHECK NOT s_refnr[] IS INITIAL OR
        NOT s_vbeln[] IS INITIAL.

  SELECT tanum FROM ltak
               INTO TABLE lt_tanum
               WHERE lgnum = p_lgnum AND
                     kquit = abap_false AND
                     refnr IN s_refnr AND
                     vbeln IN s_vbeln.

  SELECT tanum FROM ltak
               APPENDING TABLE lt_tanum
               WHERE lgnum = p_lgnum AND
                     benum IN s_refnr.

  SORT lt_tanum.
  DELETE ADJACENT DUPLICATES FROM lt_tanum.

  lv_canceled = 0.


  LOOP AT lt_tanum INTO lv_tanum.

    CALL FUNCTION 'ZWM_TO_CANCEL'
      EXPORTING
        i_lgnum  = p_lgnum
        i_tanum  = lv_tanum
        i_commit = 'X'
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    lv_canceled = lv_canceled + 1.

  ENDLOOP.


  MESSAGE s000 WITH 'Cancelled' lv_canceled.


ENDFORM.
