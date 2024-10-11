*&---------------------------------------------------------------------*
*&  Include           ZWMREP0140_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FIND_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_whs.

  CLEAR gv_whs.
  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = gs_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
*   ERRO: Utilizador não tem armazem atribuído!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMPMSG'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '001'.

    LEAVE TO SCREEN 0.

  ELSE.
    READ TABLE gs_user WITH KEY statu = 'X'. "Util. Atrib. Arm.
    IF sy-subrc <> 0.
      WRITE gs_user-lgnum TO gv_text1 LEFT-JUSTIFIED.
*     ERRO: Utilizador não está atribuído ao armazém &!
      CALL FUNCTION 'YWM_MESSAGE_SCREEN'
        EXPORTING
          message_id     = 'ZWMMPMSG'
          message_lang   = sy-langu
          message_type   = 'E'
          message_number = '002'
          message_var1   = gv_text1.

      LEAVE TO SCREEN 0.

    ELSE.
      gv_whs = gs_user-lgnum.

      IF gs_user-devty(5) = '16X20'.
        gv_setscreen1 = '0001'.
      ELSE.
        gv_setscreen1 = '0011'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_CUSTOMIZING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_customizing.

  DATA: lt_zwm001 TYPE TABLE OF zwm001 WITH HEADER LINE.

  REFRESH: lt_zwm001.

  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = gv_whs
      i_processo  = 'PICKING'
      i_parametro = 'ST_TYPE'
    TABLES
      t_zwm001    = lt_zwm001
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  LOOP AT lt_zwm001.
    CLEAR gr_st_pick.
    gr_st_pick-sign   = 'I'.
    gr_st_pick-option = 'EQ'.
    gr_st_pick-low    = lt_zwm001-valor.
    APPEND gr_st_pick.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen.

  SET SCREEN gv_setscreen1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXIT_COMMAND_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exit_command_0001.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR scr0001.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR sy-ucomm.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHK_EAN11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_ean11.

  DATA: lv_lines   TYPE i.
  DATA: lv_field   TYPE char20.
  DATA: lv_wide    TYPE char1.
  DATA: lv_matnr   TYPE matnr.
  DATA: lv_tabix   TYPE sy-tabix.
  DATA: ls_makt    TYPE makt.
  DATA: lt_mean    TYPE TABLE OF mean      WITH HEADER LINE.
  DATA: lt_makt    TYPE TABLE OF makt      WITH HEADER LINE.
  DATA: lt_lqua    TYPE TABLE OF lqua      WITH HEADER LINE.
  DATA: lt_rflista TYPE TABLE OF zwmrfs003 WITH HEADER LINE.

** Validar EAN
**********************************************************************
  CHECK scr0001-ean11 IS NOT INITIAL.

  SELECT *
    FROM mean INTO TABLE lt_mean
    WHERE ean11 = scr0001-ean11.

  IF sy-subrc <> 0.
    MOVE scr0001-ean11 TO gv_text1.

    " Código EAN & inválido
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '284'
        message_var1   = gv_text1.

    CLEAR: scr0001-ean11.
    EXIT.
  ENDIF.

** Validar Stock
  IF lt_mean[] IS NOT INITIAL.
    SELECT *
       FROM lqua INTO TABLE lt_lqua
       FOR ALL ENTRIES IN lt_mean
       WHERE matnr = lt_mean-matnr
       AND   lgnum = gv_whs
       AND   lgtyp = scr0001-lgtyp.
  ENDIF.

  DELETE lt_lqua WHERE verme <= 0.

  IF lt_lqua[] IS INITIAL.

    MOVE scr0001-ean11 TO gv_text1.
    MOVE scr0001-lgtyp TO gv_text2.

    " Não existe stock para o EAN & no deposito &!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '339'
        message_var1   = gv_text1
        message_var2   = gv_text2.

    CLEAR: scr0001-ean11.
    EXIT.
  ENDIF.

  LOOP AT lt_mean.

    lv_tabix = sy-tabix.

    READ TABLE lt_lqua WITH KEY matnr = lt_mean-matnr.
    IF sy-subrc <> 0.
      DELETE lt_mean INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

** Selecionar Material
  DESCRIBE TABLE lt_mean LINES lv_lines.

  IF lv_lines = 1.
    READ TABLE lt_mean INDEX 1.
  ELSE.

    IF lt_mean[] IS NOT INITIAL.
      SELECT *
        FROM makt INTO TABLE lt_makt
        FOR ALL ENTRIES IN lt_mean
        WHERE matnr = lt_mean-matnr
        AND   spras = sy-langu.
    ENDIF.

    LOOP AT lt_mean.
      CLEAR lt_rflista.

      lv_matnr = lt_mean-matnr.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input  = lv_matnr
        IMPORTING
          output = lv_matnr.

      lt_rflista-rflabel = lv_matnr.

      READ TABLE lt_makt WITH KEY matnr = lt_mean-matnr.
      IF sy-subrc = 0.
        lt_rflista-rflabel2 = lt_makt-maktx(18).
        lt_rflista-rflabel3 = lt_makt-maktx+18(18).
      ENDIF.

      lt_rflista-rfvalor = lt_mean-matnr.
      APPEND lt_rflista.

    ENDLOOP.

    " Escolher Material
    SORT lt_rflista BY rfvalor DESCENDING.

    CLEAR lv_matnr.

    CALL FUNCTION 'ZWMMP_SCREEN_LIST'
      EXPORTING
        titulo              = 'Escolher Material'
        no_sel              = ''
        wide_screen         = lv_wide
        long_list           = 'X'
      IMPORTING
        valor_out           = lv_matnr
      TABLES
        it_rflista          = lt_rflista
      EXCEPTIONS
        sem_entradas        = 1
        index_errado        = 2
        demasiadas_entradas = 3
        OTHERS              = 4.

    READ TABLE lt_mean WITH KEY matnr = lv_matnr.
    IF sy-subrc <> 0.
      CLEAR: scr0001-ean11.
      EXIT.
    ENDIF.
  ENDIF.

** Stock no tipo depósito
  REFRESH lt_lqua.

  SELECT *
     FROM lqua INTO TABLE lt_lqua
     WHERE matnr = lt_mean-matnr
     AND   lgnum = gv_whs
     AND   lgtyp = scr0001-lgtyp.

  DELETE lt_lqua WHERE verme <= 0.

  SORT lt_lqua BY lgpla.
  DELETE ADJACENT DUPLICATES FROM lt_lqua COMPARING lgpla.
  DESCRIBE TABLE lt_lqua LINES lv_lines.

  IF lv_lines = 0.
    MOVE scr0001-matnr TO gv_text1.
    MOVE scr0001-lgtyp TO gv_text2.

    " Não existe stock para o material & no deposito &!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '293'
        message_var1   = gv_text1
        message_var2   = gv_text2.

    CLEAR: scr0001-ean11.
    EXIT.
  ENDIF.

** Obter dados
**********************************************************************
  scr0001-npos = lv_lines.

  scr0001-matnr = lt_mean-matnr.

  SELECT SINGLE *
    FROM makt INTO ls_makt
    WHERE matnr = scr0001-matnr
    AND   spras = sy-langu.

  IF sy-subrc = 0.
    scr0001-maktx_a = ls_makt-maktx(20).
    scr0001-maktx_b = ls_makt-maktx+20(20).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHK_LGTYP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_lgtyp.

  DATA: lv_lines TYPE i.
  DATA: ls_lagp  TYPE lagp.
  DATA: lt_lqua  TYPE TABLE OF lqua WITH HEADER LINE.

** Validar tipo depósito
**********************************************************************
  CHECK scr0001-lgtyp IS NOT INITIAL.

  SELECT SINGLE *
    FROM lagp INTO ls_lagp
    WHERE lgnum = gv_whs
    AND   lgtyp =  scr0001-lgtyp.

  IF sy-subrc <> 0.
    MOVE scr0001-lgtyp TO gv_text1.

    " Tipo de deposito de origem & incorrecto!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '261'
        message_var1   = gv_text1.

    CLEAR: scr0001-lgtyp.
    EXIT.
  ENDIF.

** Tipo depósito de Picking
  IF scr0001-lgtyp NOT IN gr_st_pick[].

    MOVE scr0001-lgtyp TO gv_text1.

    " Tipo depósito & não é válido para picking!
    CALL FUNCTION 'YWM_MESSAGE_SCREEN'
      EXPORTING
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '338'
        message_var1   = gv_text1.

    CLEAR: scr0001-lgtyp.
    EXIT.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_0001.

  CASE sy-ucomm.

    WHEN 'CLR'.
      PERFORM clear_fields.
    WHEN 'SAVE'.
      PERFORM list.
    WHEN OTHERS.

  ENDCASE.
  CLEAR sy-ucomm.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CLEAR_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_fields.

  IF scr0001-matnr IS NOT INITIAL.
    CLEAR:  scr0001-ean11,
            scr0001-matnr,
            scr0001-maktx_a,
            scr0001-maktx_b,
            scr0001-npos.
  ELSE.
    CLEAR scr0001.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list.

  DATA: lv_lines   TYPE i.
  DATA: lv_field   TYPE char20.
  DATA: lv_wide    TYPE char1.
  DATA: lv_matnr   TYPE matnr.
  DATA: lv_meins   TYPE meins.
  DATA: lv_lgpla   TYPE lgpla.
  DATA: lv_qtd     TYPE char15.
  DATA: lv_titulo  TYPE char20.
  DATA: lt_lqua    TYPE TABLE OF lqua      WITH HEADER LINE.
  DATA: lt_rflista TYPE TABLE OF zwmrfs003 WITH HEADER LINE.

** Listar stock material
**********************************************************************
  CHECK scr0001-matnr IS NOT INITIAL AND scr0001-lgtyp IS NOT INITIAL.

  IF scr0001-lgtyp = 'PKL'.
    SELECT SINGLE meinh
      FROM mean INTO lv_meins
      WHERE matnr = scr0001-matnr
      AND   ean11 = scr0001-ean11.

  ELSE.
    SELECT SINGLE meins
      FROM mara INTO lv_meins
      WHERE matnr = scr0001-matnr.
  ENDIF.

  SELECT *
    FROM lqua INTO TABLE lt_lqua
    WHERE matnr = scr0001-matnr
    AND   lgnum = gv_whs
    AND   lgtyp = scr0001-lgtyp.

  DELETE lt_lqua WHERE verme <= 0.

  SORT lt_lqua BY lgpla.

** Listar posições
  LOOP AT lt_lqua.

    IF lt_lqua-meins <> lv_meins.
      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = lt_lqua-matnr
          i_in_me              = lt_lqua-meins
          i_out_me             = lv_meins
          i_menge              = lt_lqua-verme
        IMPORTING
          e_menge              = lt_lqua-verme
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.
    ENDIF.

    WRITE lt_lqua-verme TO lv_qtd LEFT-JUSTIFIED.
    CONDENSE lv_qtd.

    CLEAR lt_rflista.
    lt_rflista-rflabel = lt_lqua-lgpla.
    lt_rflista-rfvalor = lt_lqua-lgpla.

    CONCATENATE lv_qtd lv_meins INTO lt_rflista-rflabel2 SEPARATED BY space.

    APPEND lt_rflista.
  ENDLOOP.

  " Listagem Stock
  lv_matnr = scr0001-matnr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    EXPORTING
      input  = lv_matnr
    IMPORTING
      output = lv_matnr.

  CONCATENATE 'Material:' lv_matnr INTO lv_titulo SEPARATED BY space.

  CALL FUNCTION 'ZWMMP_SCREEN_LIST'
    EXPORTING
      titulo              = lv_titulo
      no_sel              = 'X'
      wide_screen         = lv_wide
    IMPORTING
      valor_out           = lv_lgpla
    TABLES
      it_rflista          = lt_rflista
    EXCEPTIONS
      sem_entradas        = 1
      index_errado        = 2
      demasiadas_entradas = 3
      OTHERS              = 4.

ENDFORM.
