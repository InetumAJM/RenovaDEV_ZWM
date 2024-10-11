FUNCTION zwm_create_to_chd .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_LGPLA) TYPE  LGPLA
*"     REFERENCE(I_LZNUM) TYPE  LZNUM OPTIONAL
*"     REFERENCE(I_TYPE_MOV) TYPE  ZWM_TYPE_MOV OPTIONAL
*"     REFERENCE(I_LINHA) TYPE  FEVOR OPTIONAL
*"     REFERENCE(I_ZEUGN) TYPE  LVS_ZEUGN OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_TO_DUMMY) TYPE  TANUM
*"  TABLES
*"      T_RETURN STRUCTURE  BDCMSGCOLL OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: lv_bwlvs  LIKE ltak-bwlvs,
        lv_matnr  LIKE ltap-matnr,
        lv_werks  LIKE ltap-werks,
        lv_lgort  LIKE ltap-lgort,
        lv_lznum  TYPE lznum,
        ls_zwm020 TYPE zwm020,
        lv_linha  TYPE char3,
        lv_exidv  TYPE exidv.

  DATA: lt_ltap_creat TYPE TABLE OF ltap_creat WITH HEADER LINE.

  CONSTANTS: lc_anfme LIKE rl03t-anfme VALUE 1,
             lc_altme LIKE ltap-altme  VALUE 'UN'.

** Obter dados
**********************************************************************

** Movimento WM
  IF i_type_mov = 'E'.

    " Aviso Entrada
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'OT_DUMMY'
        i_parametro = 'MOV_WM_WCS_ENT'
      IMPORTING
        e_valor     = lv_bwlvs
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

  ELSEIF i_type_mov = 'G'.

    " Aviso confirmação Gravítica
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'OT_DUMMY'
        i_parametro = 'MOV_WM_WCS_GRA'
      IMPORTING
        e_valor     = lv_bwlvs
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

  ELSEIF i_type_mov = 'B'.

    " Aviso Bloqueio
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'OT_DUMMY'
        i_parametro = 'MOV_WM_WCS_BLQ'
      IMPORTING
        e_valor     = lv_bwlvs
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

  ELSEIF i_type_mov = 'R'.

    " Aviso Rejeição
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'OT_DUMMY'
        i_parametro = 'MOV_WM_WCS_REJ'
      IMPORTING
        e_valor     = lv_bwlvs
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

  ELSE.
    CALL FUNCTION 'ZWM_GET_PARAMETER'
      EXPORTING
        i_lgnum     = i_lgnum
        i_processo  = 'OT_DUMMY'
        i_parametro = 'MOV_WM'
      IMPORTING
        e_valor     = lv_bwlvs
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.
  ENDIF.

** Material
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'OT_DUMMY'
      i_parametro = 'MATERIAL'
    IMPORTING
      e_valor     = lv_matnr
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_matnr
    IMPORTING
      output = lv_matnr.

** Centro
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'OT_DUMMY'
      i_parametro = 'CENTRO'
    IMPORTING
      e_valor     = lv_werks
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

** Depósito
  CALL FUNCTION 'ZWM_GET_PARAMETER'
    EXPORTING
      i_lgnum     = i_lgnum
      i_processo  = 'OT_DUMMY'
      i_parametro = 'DEPOSITO'
    IMPORTING
      e_valor     = lv_lgort
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

** Validar tipo de palete
*  SPLIT i_lznum AT '#' INTO lv_linha lv_exidv.
*  IF sy-subrc <> 0.
  lv_exidv = i_lznum.
*  ENDIF.

*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = lv_exidv
*    IMPORTING
*      output = lv_exidv.

  " Palete Remontada - 1 chamada com 2 items
  SELECT SINGLE *
    FROM zwm020 INTO ls_zwm020
    WHERE armazem = i_lgnum
    AND   p1      = lv_exidv.

** Chamada
  CLEAR lt_ltap_creat.
  lt_ltap_creat-werks = lv_werks.
  lt_ltap_creat-lgort = lv_lgort.
  lt_ltap_creat-matnr = lv_matnr.
  lt_ltap_creat-anfme = lc_anfme.
  lt_ltap_creat-altme = lc_altme.
  lt_ltap_creat-vlpla = i_lgpla.
  lt_ltap_creat-nlpla = i_lgpla.
  lt_ltap_creat-ablad = lv_exidv.
  lt_ltap_creat-zeugn = i_zeugn.
  APPEND lt_ltap_creat.

  " Palete Remontada
  IF ls_zwm020 IS NOT INITIAL.
    CLEAR lt_ltap_creat.
    lt_ltap_creat-werks = lv_werks.
    lt_ltap_creat-lgort = lv_lgort.
    lt_ltap_creat-matnr = lv_matnr.
    lt_ltap_creat-anfme = lc_anfme.
    lt_ltap_creat-altme = lc_altme.
    lt_ltap_creat-vlpla = i_lgpla.
    lt_ltap_creat-nlpla = i_lgpla.
    lt_ltap_creat-ablad = ls_zwm020-p2.
    lt_ltap_creat-zeugn = i_zeugn.
    APPEND lt_ltap_creat.
  ENDIF.

  " WCS - Linha
  IF i_linha IS NOT INITIAL.
    lv_lznum = i_linha.
  ENDIF.

** Criar Chamada Dummy
**********************************************************************
  CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
    EXPORTING
      i_lgnum       = i_lgnum
      i_bwlvs       = lv_bwlvs
      i_lznum       = lv_lznum
    IMPORTING
      e_tanum       = e_to_dummy
    TABLES
      t_ltap_creat  = lt_ltap_creat
    EXCEPTIONS
      error_message = 99.

  IF sy-subrc <> 0.
    MOVE sy-msgty TO t_return-msgtyp.
    MOVE sy-msgid TO t_return-msgid.
    MOVE sy-msgno TO t_return-msgnr.
    MOVE sy-langu TO t_return-msgspra.
    MOVE sy-msgv1 TO t_return-msgv1.
    MOVE sy-msgv2 TO t_return-msgv2.
    MOVE sy-msgv3 TO t_return-msgv3.
    MOVE sy-msgv4 TO t_return-msgv4.
    APPEND t_return.

    RAISE error.
  ENDIF.

ENDFUNCTION.
