*&---------------------------------------------------------------------*
*&  Include           ZWMREP0123I01                                    *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CLEAR: ean_in, matnr_in,menge_in, maktx_out1, maktx_out2,
         meins_out, charg_in, menge_in, vhilm_in,
         cursorfield, ok_code_0001.
  n_print = 1.
  SET SCREEN '0000'.
  LEAVE SCREEN.

ENDMODULE.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_ean_in  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ean_in INPUT.

  DATA: linhas TYPE i.

  CLEAR:  t_marm, linhas, matnr_in.
  REFRESH t_marm.

  CHECK NOT ean_in IS INITIAL.

  SELECT * INTO TABLE t_marm
      FROM marm
          WHERE ean11 = ean_in AND
                umrez = '1' AND
                umren = '1'.

  IF t_marm[] IS INITIAL.

    WRITE ean_in TO text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '135'
        message_var1   = text1.

    CLEAR ean_in.
    MOVE 'EAN_IN' TO cursorfield.
  ELSE.

    DESCRIBE TABLE t_marm LINES linhas.
    IF linhas = 1.
      READ TABLE t_marm INDEX 1.
      matnr_in = t_marm-matnr.
      SELECT SINGLE meins INTO meins_out
          FROM mara WHERE matnr = matnr_in.

      SELECT SINGLE * FROM makt
          WHERE matnr = matnr_in
              AND spras = sy-langu.

      maktx_out1 = makt-maktx(20).
      maktx_out2 = makt-maktx+20(20).
      MOVE 'MENGE_IN' TO cursorfield.
    ELSE.
      SORT t_marm BY matnr.
      MOVE 'MATNR_IN' TO cursorfield.
    ENDIF.
    n_print = 1.
  ENDIF.

ENDMODULE.                 " check_ean_in  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_matnr_in  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr_in INPUT.

  CHECK NOT matnr_in IS INITIAL AND
        NOT ean_in    IS INITIAL.

  READ TABLE t_marm WITH KEY matnr = matnr_in.
  IF sy-subrc <> 0.
    WRITE: matnr_in TO text1 LEFT-JUSTIFIED,
           ean_in    TO text2 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '250'
        message_var1   = text1
        message_var2   = text2.

    CLEAR : ean_in, matnr_in.
    MOVE 'EAN_IN' TO cursorfield.
    EXIT.
  ENDIF.

** Verificar se o material existe e não está marcado para eliminação
  SELECT SINGLE * FROM mara
                  WHERE matnr = matnr_in AND
                        lvorm = ' '.
  IF sy-subrc <> 0.

    WRITE matnr_in TO text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '070'
        message_var1   = text1.

    CLEAR : matnr_in.
    MOVE 'MATNR_IN' TO cursorfield.
  ELSE.
    SELECT SINGLE * FROM makt
    WHERE matnr = matnr_in
        AND spras = sy-langu.

    maktx_out1 = makt-maktx(20).
    maktx_out2 = makt-maktx+20(20).

    MOVE mara-meins TO meins_out.
    MOVE 'MENGE_IN' TO cursorfield.
  ENDIF.

ENDMODULE.                 " check_material  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_menge_in  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_menge_in INPUT.

  CHECK NOT matnr_in IS INITIAL AND
        NOT ean_in IS INITIAL AND
        NOT menge_in IS INITIAL.

  SELECT SINGLE *
    FROM mlgn
      WHERE matnr = matnr_in
        AND lgnum = whs.

  IF menge_in > mlgn-lhmg1.
    WRITE menge_in TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '143'
        message_var1   = text1.

    CLEAR : menge_in.
    MOVE 'MENGE_IN' TO cursorfield.
    EXIT.
  ENDIF.

  MOVE 'CHARG_IN' TO cursorfield.

ENDMODULE.                 " check_menge_in  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CHARG_IN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_charg_in INPUT.

  CHECK NOT charg_in IS INITIAL.

  SELECT SINGLE * FROM  mchb
         WHERE  matnr  = matnr_in
         AND    werks  = plant
*         AND    lgort  = lgort
         AND    charg  = charg_in.
  IF sy-subrc <> 0.
    WRITE charg_in TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '141'
        message_var1   = text1.

    CLEAR : charg_in.
    MOVE 'CHARG_IN' TO cursorfield.
    EXIT.
  ENDIF.

  MOVE 'VHILM_IN' TO cursorfield.
ENDMODULE.                 " CHECK_CHARG_IN  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_vhilm_in  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_vhilm_in INPUT.

  CHECK NOT vhilm_in IS INITIAL.

** verificar se o tipo do packing material é = 'PALT' e se existe no SAP
  SELECT SINGLE *
      FROM mara
          WHERE matnr = vhilm_in
            AND mtart = 'PALT'.
  IF sy-subrc <> 0.

    WRITE vhilm_in TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '070'
        message_var1   = text1.

    CLEAR : vhilm_in.
    MOVE 'VHILM_IN' TO cursorfield.
  ELSE.
*    MOVE 'VHILM_IN' TO cursorfield.
    MOVE 'N_PRINT' TO cursorfield.
  ENDIF.


ENDMODULE.                 " check_vhilm_in  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  DATA  hukey     LIKE bapihukey-hu_exid.
  DATA: ls_zwm013 TYPE zwm013.
  DATA: ls_mlgn   TYPE mlgn.
  DATA: lt_ud     TYPE TABLE OF lenum.
  DATA: ls_ud     TYPE lenum.
  DATA: lt_zwm020 TYPE TABLE OF zwm020.
  DATA: ls_zwm020 TYPE zwm020.

  CASE ok_code_0001.

    WHEN 'CLR'.

      CLEAR: ean_in, matnr_in,menge_in, maktx_out1, maktx_out2,
             meins_out, charg_in, menge_in, vhilm_in,
             cursorfield, ok_code_0001 , n_print, print_in.

    WHEN 'SAVE'.

      CHECK NOT ean_in   IS INITIAL OR
            NOT matnr_in IS INITIAL OR
            NOT menge_in IS INITIAL OR
            NOT charg_in IS INITIAL OR
            NOT vhilm_in IS INITIAL.

      CHECK n_print IS NOT INITIAL.
      REFRESH lt_ud.
      DO n_print TIMES.
        CLEAR:   return_msg, itab_sscc, items_hu, hukey,ls_ud .
        REFRESH: return_msg, itab_sscc, items_hu.

        items_hu-material = matnr_in.
        items_hu-quantity = menge_in.
        items_hu-unit = meins_out.
        items_hu-batch = charg_in.
        APPEND items_hu.
        CLEAR items_hu.

        CALL FUNCTION 'ZWM_CREATE_HU'
          EXPORTING
            warehouse                  = whs
            plant                      = plant
            s_loc                      = lgort
            packing_material           = vhilm_in
          IMPORTING
            hukey                      = hukey
          TABLES
            return_msg                 = return_msg
            items                      = items_hu
          EXCEPTIONS
            empty_table                = 1
            reference_document_differs = 2
            empty_delivery_item        = 3
            item_not_found             = 4
            OTHERS                     = 5.
        IF sy-subrc <> 0.

          READ TABLE return_msg INDEX 1.
          CALL FUNCTION 'YWM_MESSAGE_SCREEN'
            EXPORTING
              message_id     = return_msg-msgid
              message_lang   = sy-langu
              message_type   = return_msg-msgtyp
              message_number = return_msg-msgnr
              message_var1   = return_msg-msgv1.


          CLEAR : ok_code_0001.
          SET SCREEN '0001'.
          LEAVE SCREEN.

        ELSE.
          CLEAR: itab_sscc, gv_tud.
          itab_sscc-sscc = hukey.
          APPEND itab_sscc.
          IF gv_tcode = 'B'.

            ls_zwm013-armazem     = whs.
            ls_zwm013-sscc        = hukey.

            SELECT SINGLE * FROM mlgn INTO ls_mlgn
                    WHERE matnr = matnr_in AND
                          lgnum =  whs.

            ls_zwm013-tipo_palete = ls_mlgn-lety1.
            INSERT INTO zwm013 VALUES ls_zwm013.

            IF z_wm_cl_management=>is_remontada( is_data = ls_mlgn ) eq abap_true.
              CLEAR ls_ud.
              ls_ud = hukey.
              INSERT ls_ud INTO TABLE lt_ud.
            ENDIF.
          ENDIF.
        ENDIF.

        WAIT UP TO 3 SECONDS.

        CHECK NOT hukey IS INITIAL.

        CALL FUNCTION 'ZWM_IMPRIME_EAN128'
          EXPORTING
            printer                  = l_printer
            lbltype                  = 'L0'
            copies                   = n_copias
          TABLES
            sscc                     = itab_sscc
          EXCEPTIONS
            impressora_nao_existe    = 1
            sscc_nao_existe          = 2
            sscc_com_impressao_grupo = 3
            OTHERS                   = 4.
        IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ENDDO.

      LOOP AT lt_ud INTO ls_ud.
        IF ls_zwm020-p1 IS INITIAL.
          ls_zwm020-p1 = ls_ud.
        ELSE.
          ls_zwm020-p2 = ls_ud.
          ls_zwm020-armazem = whs.
          INSERT zwm020 FROM ls_zwm020.
          CLEAR ls_zwm020.
        ENDIF.
      ENDLOOP.

      CLEAR: ean_in, matnr_in,menge_in, maktx_out1, maktx_out2,
             meins_out, charg_in, menge_in, vhilm_in,
             cursorfield, ok_code_0001,n_print,ls_zwm020.
      EXIT.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_N_PRINT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_n_print INPUT.

  CHECK n_print IS INITIAL.
** Inserir numero de impressões
  CALL FUNCTION 'YWM_MESSAGE_SCREEN'
    EXPORTING
      message_id     = 'ZWMMSG001'
      message_lang   = sy-langu
      message_type   = 'E'
      message_number = '281'
      message_var1   = text1.

  MOVE 'N_PRINT' TO cursorfield.

ENDMODULE.                 " CHECK_N_PRINT  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PRINT_IN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_print_in INPUT.
  PERFORM check_print_in.
ENDMODULE.                 " CHECK_PRINT_IN  INPUT
