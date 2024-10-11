*&---------------------------------------------------------------------*
*&  Include           ZWMREP0130_F01
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
  DATA: lt_lqua       TYPE TABLE OF lqua,
        ls_lqua       TYPE lqua,
        lt_ltap       TYPE TABLE OF ltap,
        ls_ltap       TYPE ltap,
        lt_marm       TYPE TABLE OF marm,
        ls_marm       TYPE marm,
        lv_value      TYPE menge_d,
        lv_value_meio TYPE menge_d,
        lv_aux        TYPE menge_d,
        lv_valor_comp TYPE menge_d,
        lv_tanum      TYPE tanum.
  DATA: lt_messages   TYPE  tab_bdcmsgcoll,
        ls_message    TYPE bdcmsgcoll.
**********************************************************************
  SELECT * FROM lqua INTO TABLE lt_lqua
   WHERE lgnum = p_lgnum            AND
         lgtyp = gv_dep_loja_online AND
         gesme > 0                  and
         EINME = 0                  and
         AUSME = 0 .

  CHECK lt_lqua IS NOT INITIAL.

  SELECT * FROM ltap INTO TABLE lt_ltap
      FOR ALL ENTRIES IN lt_lqua
      WHERE lgnum = p_lgnum       AND
            pquit = 'X'           AND
            matnr = lt_lqua-matnr AND
            vltyp = lt_lqua-lgtyp AND
            vlpla = lt_lqua-lgpla.


CHECK lt_ltap IS NOT INITIAL.
LOOP AT lt_ltap INTO ls_ltap.
  CHECK ls_ltap-meins = ls_ltap-altme.
  DELETE TABLE lt_ltap FROM ls_ltap.
ENDLOOP.

  SORT lt_ltap BY matnr vlpla qdatu qzeit DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_ltap COMPARING matnr vltyp vlpla.


**  Obtem valores de conversão
  SELECT * FROM marm INTO TABLE lt_marm
    FOR ALL ENTRIES IN lt_lqua
    WHERE matnr = lt_lqua-matnr.

  CHECK lt_marm IS NOT INITIAL.


  LOOP AT lt_lqua INTO ls_lqua.
    READ TABLE lt_ltap INTO ls_ltap WITH KEY matnr = ls_lqua-matnr
                                             vlpla = ls_lqua-lgpla.
    CHECK sy-subrc = 0.
*    CHECK ls_lqua-EINME IS INITIAL and ls_lqua-AUSME IS INITIAL.
    READ TABLE lt_marm INTO ls_marm WITH KEY matnr = ls_lqua-matnr
                                             meinh = ls_ltap-altme.
    CHECK sy-subrc = 0.
**      calcula valor para 1 U.M.A. em U.M.B
    lv_value = ( ls_marm-umrez / ls_marm-umren ).
    lv_aux = lv_value * ls_marm-umren.
    IF lv_aux < 1 .
      lv_value = lv_value + '0.001'.
    ENDIF.
    lv_value_meio = lv_value / 2.
**      verifica se valor em estoque não consegue satisfazer 1 U.M.A.
    IF ls_lqua-gesme > lv_value_meio  AND ls_lqua-gesme < lv_value .
      lv_valor_comp = lv_value - ls_lqua-gesme.
      CALL FUNCTION 'ZWM_TO_CREATE_SINGLE'
        EXPORTING
          i_lgnum           = p_lgnum
          i_bwlvs           = gv_to_mov
          i_werks           = ls_lqua-werks
*         I_LGORT           =
          i_matnr           = ls_lqua-matnr
          i_charg           = ls_lqua-charg
          i_anfme           = lv_valor_comp
          i_altme           = ls_ltap-meins
          i_vltyp           = gv_dep_ori "999
          i_vlpla           = gv_pos_ori "
          i_nltyp           = ls_lqua-lgtyp
          i_nlpla           = ls_lqua-lgpla
          I_SQUIT           = 'X'
       IMPORTING
          E_TANUM           = lv_tanum
*         ES_LTAP           =
         et_messages       = lt_messages
       EXCEPTIONS
         error             = 1
         OTHERS            = 2
                .

      IF lv_tanum IS NOT INITIAL.
        WRITE lv_tanum.
      ENDIF.

      READ TABLE lt_messages INTO ls_message INDEX 1.
      IF sy-subrc = 0.
        MESSAGE ID ls_message-msgid TYPE 'I' NUMBER ls_message-msgnr
                DISPLAY LIKE ls_message-msgtyp
                WITH ls_message-msgv1 ls_message-msgv2
                     ls_message-msgv3 ls_message-msgv4.
      ENDIF.

    ENDIF.
  ENDLOOP.



ENDFORM.                    " INV
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init .

  DATA: lt_messages TYPE tab_bdcmsgcoll,
        ls_message  TYPE bdcmsgcoll.
  p_lgnum = '100'.
  DO 1 TIMES.

** Movimento WM
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = p_lgnum
        i_processo  = 'INVENTARIO'
        i_parametro = 'MOV_WM'
      IMPORTING
        e_valor     = gv_to_mov
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

** Parâmetro Tipo Depósito  PKL
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = p_lgnum
        i_processo  = 'INVENTARIO'
        i_parametro = 'DEPOSITO_DEST'
      IMPORTING
        e_valor     = gv_dep_loja_online
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

** Parâmetro Depósito origem
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = p_lgnum
        i_processo  = 'INVENTARIO'
        i_parametro = 'DEP_ORIGEM'
      IMPORTING
        e_valor     = gv_dep_ori
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

*    gv_lgort_dest_cons  = gv_lgort_dest.
*

** Parâmetro Posição Destino
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = p_lgnum
        i_processo  = 'INVENTARIO'
        i_parametro = 'POS_ORIGEM'
      IMPORTING
        e_valor     = gv_pos_ori
        et_messages = lt_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

  ENDDO.

  IF sy-subrc <> 0.
    READ TABLE lt_messages INTO ls_message INDEX 1.

    MESSAGE ID ls_message-msgid TYPE 'I' NUMBER ls_message-msgnr
            DISPLAY LIKE ls_message-msgtyp
            WITH ls_message-msgv1 ls_message-msgv2
                 ls_message-msgv3 ls_message-msgv4.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " INIT
