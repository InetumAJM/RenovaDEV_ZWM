*&---------------------------------------------------------------------*
*& Report  ZWMREP0063                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zwmrep0063 MESSAGE-ID zwmmsg001.

INCLUDE: rlmobinc.

TABLES: zwm013, zwm020.

DATA : BEGIN OF ti_zwm001 OCCURS 0.
         INCLUDE STRUCTURE zwm001.
       DATA: END OF ti_zwm001.

*dados gerais
DATA: ok_code_0001 LIKE sy-ucomm.

DATA  mesa LIKE zwm_aux-mesa.
DATA  sscc LIKE zwm_aux-sscc.

DATA: text TYPE  bdcmsgcoll-msgv1.
DATA: cursorfield(20),
      return_msg TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA: e_mesa    LIKE zwm_aux-mesa,
      e_sscc    LIKE zwm_aux-sscc,
      e_retorno LIKE zwm_aux-retorno,
      e_to      LIKE ltak-tanum.


START-OF-SELECTION.
  message_lang = sy-langu.
  PERFORM user_own_data.

  CALL SCREEN '0001'.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'ZRF_EP'.
  SET CURSOR FIELD cursorfield.

  IF xuser-lgnum IS INITIAL.
    PERFORM user_own_data.
  ENDIF.

  IF NOT mesa IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'MESA'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CLEAR: mesa, sscc, ok_code_0001.

  SET SCREEN '0000'.
  LEAVE SCREEN.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_mesa  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_mesa INPUT.

  DATA: ls_lagp TYPE lagp.

  CHECK NOT mesa IS INITIAL.

  SELECT SINGLE *
    FROM lagp INTO ls_lagp
    WHERE lgnum = xuser-lgnum
    AND   lgtyp = mesa(3)
    AND   lgpla = mesa+4(10).

  IF sy-subrc <> 0.
*  IF mesa <> 'PRO 000-000-01' AND
*     mesa <> 'PRO 000-000-02'.

    CLEAR text.

    WRITE mesa TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '214'
        message_var1   = text.

    CLEAR: ok_code_0001, mesa.
    MOVE 'MESA' TO cursorfield.
    EXIT.
  ELSE.
    CLEAR: ok_code_0001.
    MOVE 'SSCC' TO cursorfield.
    EXIT.
  ENDIF.
ENDMODULE.                 " check_mesa  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_sscc  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sscc INPUT.

  CHECK NOT sscc IS INITIAL.

  SELECT SINGLE *
      FROM zwm013
          WHERE armazem = xuser-lgnum AND
                   sscc = sscc.
  IF sy-subrc <> 0.
    CLEAR text.
    WRITE sscc TO text LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '215'
        message_var1   = text.

    CLEAR: ok_code_0001, sscc.
    MOVE 'SSCC' TO cursorfield.
    EXIT.
  ENDIF.

  IF z_wm_cl_management=>is_remontada( i_lgnum = xuser-lgnum i_letyp = zwm013-tipo_palete ) eq abap_true.

    SELECT SINGLE *
        FROM zwm020
            WHERE armazem = xuser-lgnum AND
                  ( p1 = sscc OR p2 = sscc ).
    IF sy-subrc <> 0.
      CLEAR text.
      WRITE sscc TO text LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '215'
          message_var1   = text.

      CLEAR: ok_code_0001, sscc.
      MOVE 'SSCC' TO cursorfield.
      EXIT.
    ENDIF.
  ENDIF.

ENDMODULE.                 " check_sscc  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CHECK NOT mesa IS INITIAL AND NOT sscc IS INITIAL.

  CASE ok_code_0001.

    WHEN 'NEXT'.

      CALL FUNCTION 'ZWM_RFC_GET_MESA'
        EXPORTING
          i_sscc  = sscc
          i_mesa  = mesa
        IMPORTING
          mesa    = e_mesa
          e_sscc  = e_sscc
          retorno = e_retorno.

      IF NOT e_retorno IS INITIAL.

        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '216'.

        CLEAR: mesa, sscc, ok_code_0001.
        MOVE 'MESA' TO cursorfield.
        EXIT.

      ELSE.

        CALL FUNCTION 'ZWM_RFC_GET_TO'
          EXPORTING
            i_sscc  = sscc
            mesa    = mesa
          IMPORTING
            e_sscc  = e_sscc
            e_to    = e_to
            retorno = e_retorno.

        IF NOT e_retorno IS INITIAL.

          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '216'.

          CLEAR: mesa, sscc, ok_code_0001.
          MOVE 'MESA' TO cursorfield.
          EXIT.
        ELSE.
          CLEAR: sscc, ok_code_0001.
          MOVE 'SSCC' TO cursorfield.
          EXIT.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001  INPUT
