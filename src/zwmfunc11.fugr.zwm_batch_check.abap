FUNCTION zwm_batch_check.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MATNR) TYPE  MATNR
*"     REFERENCE(I_WERKS) TYPE  WERKS_D
*"  EXPORTING
*"     REFERENCE(ES_EAN128) TYPE  EAN128
*"     REFERENCE(E_VFDAT) TYPE  VFDAT
*"     REFERENCE(E_HSDAT) TYPE  HSDAT
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  CHANGING
*"     REFERENCE(C_CHARG) TYPE  ANY
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA: lv_charg    TYPE charg_d,
        lv_xchpf    TYPE xchpf,
        ls_mcha     TYPE mcha,
        lv_barcode  TYPE barcode,
        lv_strlen   TYPE i,
        lv_vfdat    TYPE vfdat,
        lv_mhdhb    TYPE mhdhb,
        lv_aufnr    TYPE aufnr,
        ls_message  TYPE bdcmsgcoll.

***********************************************************************
  CLEAR: et_messages, es_ean128, e_hsdat.

  CHECK NOT c_charg IS INITIAL.

  GET TIME.

** Valida se Gestão por Lote
***********************************************************************
  SELECT SINGLE xchpf FROM marc INTO lv_xchpf
          WHERE matnr EQ i_matnr
            AND werks EQ i_werks.

  IF sy-subrc <> 0 OR lv_xchpf IS INITIAL.
**  Material & não gerido por Lote
    ls_message-msgid  = 'YMM001'.
    ls_message-msgtyp = 'E'.
    ls_message-msgnr  = '008'.
    ls_message-msgv1  = i_matnr.
    APPEND ls_message TO et_messages.
    CLEAR c_charg.
    RAISE error.
  ENDIF.


** Valida se EAN128
***********************************************************************
  lv_strlen = STRLEN( c_charg ).

*  IF c_charg(3) EQ ']C1'.
*    lv_barcode = c_charg.
*
*    CALL FUNCTION 'Y_RBL_RF_EAN128_DECODE'
*      EXPORTING
*        i_barcode = lv_barcode
*      IMPORTING
*        es_ean128 = es_ean128
*      EXCEPTIONS
*        error     = 1
*        OTHERS    = 2.
*  ENDIF.

  IF NOT es_ean128-charg IS INITIAL.
    lv_charg = es_ean128-charg.
  ELSE.
    SHIFT c_charg LEFT DELETING LEADING space.
    lv_charg = c_charg.
  ENDIF.

  c_charg = lv_charg.


** Valida se Lote do Material
***********************************************************************
  SELECT SINGLE * FROM mcha INTO ls_mcha
          WHERE matnr EQ i_matnr AND
                werks EQ i_werks AND
                charg EQ lv_charg.

  IF sy-subrc <> 0.
**  Lote & inválido para o Material & no Centro &
    ls_message-msgid  = 'YMM001'.
    ls_message-msgtyp = 'E'.
    ls_message-msgnr  = '009'.
    ls_message-msgv1  = lv_charg.
    ls_message-msgv2  = i_matnr.
    ls_message-msgv3  = i_werks.
    APPEND ls_message TO et_messages.
    CLEAR: es_ean128, c_charg.
    RAISE error.
  ENDIF.


** Retorno
***********************************************************************
  e_hsdat         = ls_mcha-hsdat.
  e_vfdat         = ls_mcha-vfdat.
  es_ean128-charg = lv_charg.
ENDFUNCTION.
