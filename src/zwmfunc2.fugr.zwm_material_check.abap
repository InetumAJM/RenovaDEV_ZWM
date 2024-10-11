FUNCTION zwm_material_check.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_VALUE) TYPE  ANY
*"     REFERENCE(I_MATNR_VAL) TYPE  MATNR OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_MATNR) TYPE  MATNR
*"     REFERENCE(E_MTART) TYPE  MTART
*"     REFERENCE(E_XCHPF) TYPE  XCHPF
*"     REFERENCE(E_MAKTX) TYPE  MAKTX
*"     REFERENCE(E_MAKTX_A) TYPE  CHAR20
*"     REFERENCE(E_MAKTX_B) TYPE  CHAR20
*"     REFERENCE(E_MHDHB) TYPE  MHDHB
*"     REFERENCE(E_MHDRZ) TYPE  MHDRZ
*"     REFERENCE(E_MEINS) TYPE  MEINS
*"     REFERENCE(E_INSMK) TYPE  INSMK_MAT
*"     REFERENCE(ES_EAN128) TYPE  EAN128
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: ls_message      TYPE bdcmsgcoll,
        ls_mara         TYPE mara.

  DATA: lv_matnr        TYPE matnr,
        lv_ean11        TYPE ean11,
        lv_size         TYPE i,
        lv_charg        TYPE charg_d,
        lv_barcode      TYPE barcode,
        lv_line_test    TYPE char20.


***********************************************************************
  CLEAR: et_messages, e_matnr, e_xchpf, e_maktx, e_maktx_a, e_maktx_b,
         e_mhdhb, es_ean128, e_meins, e_mtart, e_insmk.

  CHECK NOT i_value IS INITIAL.

** Valida se EAN128
***********************************************************************
  lv_barcode = i_value.

*  CALL FUNCTION 'Y_RBL_RF_EAN128_DECODE'
*    EXPORTING
*      i_barcode    = lv_barcode
*      i_validation = abap_true
*    IMPORTING
*      es_ean128    = es_ean128
*    EXCEPTIONS
*      error        = 1
*      OTHERS       = 2.
*
*  IF sy-subrc EQ 0.
*    lv_ean11 = es_ean128-matean.
*    lv_charg = es_ean128-charg.
*  ENDIF.

** Valida se Material Directo
***********************************************************************
  DO 1 TIMES.
    CHECK lv_ean11 IS INITIAL.

    lv_size = STRLEN( i_value ).
    CHECK lv_size <= 18.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = i_value
      IMPORTING
        output       = lv_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    CHECK sy-subrc EQ 0.


    SELECT SINGLE * FROM mara INTO ls_mara
            WHERE matnr EQ lv_matnr.

    CHECK sy-subrc EQ 0.

    e_matnr = lv_matnr.
  ENDDO.


** Resultados
***********************************************************************
  IF e_matnr IS INITIAL.
**  Material & inválido
    ls_message-msgid  = 'YMM001'.
    ls_message-msgtyp = 'E'.
    ls_message-msgnr  = '001'.
    ls_message-msgv1  = i_value.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.


  IF NOT i_matnr_val IS INITIAL AND e_matnr NE i_matnr_val.
**  O Material & não é o esperado (&)
    ls_message-msgid  = 'YMM001'.
    ls_message-msgtyp = 'E'.
    ls_message-msgnr  = '007'.
    ls_message-msgv1  = e_matnr.
    ls_message-msgv2  = i_matnr_val.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.


** Dados Adicionais do Material
***********************************************************************
  SELECT SINGLE maktx FROM makt INTO e_maktx
          WHERE matnr EQ e_matnr
            AND spras EQ sy-langu.


  CALL FUNCTION 'RKD_WORD_WRAP'
    EXPORTING
      textline            = e_maktx
      delimiter           = space
      outputlen           = 20
    IMPORTING
      out_line1           = e_maktx_a
      out_line2           = e_maktx_b
      out_line3           = lv_line_test
    EXCEPTIONS
      outputlen_too_large = 1
      OTHERS              = 2.

  IF NOT lv_line_test IS INITIAL.
    e_maktx_a = e_maktx(20).
    e_maktx_b = e_maktx+20(20).
  ENDIF.


** Gestão por Lote / Outros
***********************************************************************
  IF NOT ls_mara IS INITIAL.
    e_mtart = ls_mara-mtart.
    e_xchpf = ls_mara-xchpf.
    e_mhdhb = ls_mara-mhdhb.
    e_mhdrz = ls_mara-mhdrz.
    e_meins = ls_mara-meins.
  ELSE.
    SELECT SINGLE mtart xchpf mhdhb meins FROM mara
             INTO (e_mtart, e_xchpf, e_mhdhb, e_meins)
            WHERE matnr EQ e_matnr.
  ENDIF.

  IF ( e_xchpf IS INITIAL OR
     e_mhdhb IS INITIAL ) AND
     NOT i_werks IS INITIAL.
**  Gestão por Lote (A nível de Centro)
    SELECT SINGLE xchpf insmk FROM marc INTO (e_xchpf, e_insmk)
            WHERE matnr EQ e_matnr
              AND werks EQ i_werks.

    IF sy-subrc <> 0.
**    Material & não presente no Centro &
      ls_message-msgid  = 'YMM001'.
      ls_message-msgtyp = 'E'.
      ls_message-msgnr  = '010'.
      ls_message-msgv1  = e_matnr.
      ls_message-msgv2  = i_werks.
      APPEND ls_message TO et_messages.
      RAISE error.
    ENDIF.
  ENDIF.

ENDFUNCTION.
