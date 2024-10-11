*----------------------------------------------------------------------*
***INCLUDE LZWMFUNC1I07 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_EAN11  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ean11 INPUT.

  DATA: linhas TYPE i.

  CLEAR:  t_marm, linhas, material.
  REFRESH t_marm.

  CHECK NOT ean11 IS INITIAL.

  SELECT * INTO TABLE t_marm
      FROM marm
          WHERE ean11 = ean11 AND
                umrez = '1' AND
                umren = '1'.

  IF t_marm[] IS INITIAL.

    WRITE ean11 TO text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '135'
        message_var1   = text1.

    CLEAR ean11.
    MOVE 'EAN11' TO cursorfield.
  ELSE.

    DESCRIBE TABLE t_marm LINES linhas.
    IF linhas = 1.
      READ TABLE t_marm INDEX 1.
      material = t_marm-matnr.
      SELECT SINGLE meins INTO unidade
          FROM mara WHERE matnr = material.
      MOVE 'QUANTIDADE' TO cursorfield.
    ELSE.
      SORT t_marm BY matnr.
      MOVE 'MATERIAL' TO cursorfield.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_EAN11  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_material INPUT.

  CLEAR : text1,
          text2.

  CHECK NOT material IS INITIAL AND
        NOT ean11    IS INITIAL.

  READ TABLE t_marm WITH KEY matnr = material.
  IF sy-subrc <> 0.
    WRITE: material TO text1 LEFT-JUSTIFIED,
           ean11    TO text2 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '250'
        message_var1   = text1
        message_var2   = text2.

    CLEAR : ean11, material.
    MOVE 'EAN11' TO cursorfield.
    EXIT.
  ENDIF.

** Verificar se o material existe e não está marcado para eliminação
  SELECT SINGLE * FROM mara
                  WHERE matnr = material AND
                        lvorm = ' '.
  IF sy-subrc <> 0.

    WRITE material TO text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '070'
        message_var1   = text1.

    CLEAR : material.
    MOVE 'MATERIAL' TO cursorfield.
  ELSE.

    CLEAR : plant,
            lgort,
            return_msg.

    REFRESH : return_msg.

** Verificar se o material está aberto no centro depósito
    CALL FUNCTION 'ZWM_GET_MATERIAL_PLANT_SLOC'
      EXPORTING
        warehouse    = xuser-lgnum
        material     = material
      IMPORTING
        plant        = plant
        s_loc        = lgort
      TABLES
        return_msg   = return_msg
      EXCEPTIONS
        not_found    = 1
        indetermined = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      READ TABLE return_msg INDEX 1.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = return_msg-msgid
          message_lang   = sy-langu
          message_type   = return_msg-msgtyp
          message_number = return_msg-msgnr
          message_var1   = return_msg-msgv1.

      CLEAR : material.

      MOVE 'MATERIAL' TO cursorfield.
    ELSE.
      MOVE mara-meins TO unidade.
      MOVE 'QUANTIDADE' TO cursorfield.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_QUANTIDADE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_quantidade INPUT.


  CHECK NOT material IS INITIAL AND
        NOT ean11 IS INITIAL AND
        NOT quantidade IS INITIAL.

  SELECT SINGLE *
    FROM mlgn
      WHERE matnr = material AND lgnum = xuser-lgnum.

  IF z_wm_cl_management=>is_remontada( is_data = mlgn ) eq abap_true.
    IF quantidade <> mlgn-lhmg1.
      WRITE quantidade TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '143'
          message_var1   = text1.

      CLEAR : quantidade.
      MOVE 'QUANTIDADE' TO cursorfield.
      EXIT.
    ENDIF.
  ENDIF.

  IF quantidade > mlgn-lhmg1.
    WRITE quantidade TO text1 LEFT-JUSTIFIED.
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '143'
        message_var1   = text1.

    CLEAR : quantidade.
    MOVE 'QUANTIDADE' TO cursorfield.
  ELSE.
    MOVE 'PACK_MATERIAL' TO cursorfield.
  ENDIF.
ENDMODULE.                 " CHECK_QUANTIDADE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_UNIDADE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE check_unidade INPUT.
*
*  CHECK NOT material IS INITIAL AND
*        NOT ean11 IS INITIAL AND
*        NOT quantidade IS INITIAL AND
*        NOT unidade IS INITIAL.
*
*  MOVE 'PACK_MATERIAL' TO cursorfield.
*
*
*ENDMODULE.                 " CHECK_UNIDADE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_lote INPUT.

** Verificar se o lote inserido - caso seja o operador a inserir -
** é válido, isto so acontece nas tranferencias!

  CHECK NOT lote IS INITIAL.
  IF ecra_chamador = '0017'.
    SELECT SINGLE *
        FROM mchb
            WHERE matnr = material AND
                  charg = lote AND
                  werks = 'RENV' AND
                  lgort = 'A'.
    IF sy-subrc <> 0.
      WRITE lote TO text1 LEFT-JUSTIFIED.
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '141'
          message_var1   = text1.

      CLEAR : lote.
      MOVE 'LOTE' TO cursorfield.
    ELSE.

      IF mchb-clabs < quantidade.

        WRITE lote TO text1 LEFT-JUSTIFIED.
        CALL FUNCTION 'YWM_MESSAGE_SCREEN'
          EXPORTING
            message_id     = 'ZWMMSG001'
            message_lang   = sy-langu
            message_type   = 'E'
            message_number = '142'
            message_var1   = text1.

        CLEAR : lote.
        MOVE 'LOTE' TO cursorfield.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_LOTE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PACK_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pack_material INPUT.

  CHECK NOT material IS INITIAL AND
        NOT ean11 IS INITIAL AND
        NOT quantidade IS INITIAL AND
        NOT unidade IS INITIAL AND
*        NOT lote IS INITIAL AND
        NOT pack_material IS INITIAL.

** verificar se o tipo do packing material é = 'PALT' e se existe no SAP
  SELECT SINGLE * FROM mara
                  WHERE matnr = pack_material.
  IF sy-subrc <> 0.

    WRITE pack_material TO text1 LEFT-JUSTIFIED.

    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '070'
        message_var1   = text1.

    CLEAR : pack_material.
    MOVE 'PACK_MATERIAL' TO cursorfield.

  ELSE.

    IF mara-mtart = 'PALT'.
      MOVE 'N_ETIQ' TO cursorfield.
    ELSE.

      WRITE pack_material TO text1 LEFT-JUSTIFIED.

      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMSG001'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '077'
          message_var1   = text1.

      CLEAR : pack_material.
      MOVE 'PACK_MATERIAL' TO cursorfield.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_PACK_MATERIAL  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_n_etiq  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_n_etiq INPUT.

** Só quando o campo lote não vier preenchido
  IF lote IS INITIAL.
    CHECK NOT material IS INITIAL AND
          NOT ean11 IS INITIAL AND
          NOT quantidade IS INITIAL AND
          NOT unidade IS INITIAL AND
          NOT pack_material IS INITIAL AND
          NOT n_etiq IS INITIAL.

    PERFORM valida_qtd_lote.
*    MOVE 'LOTE' TO cursorfield.
  ENDIF.

ENDMODULE.                 " check_n_etiq  INPUT
