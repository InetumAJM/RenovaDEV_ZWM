FUNCTION zwm_material_convert_unit.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(I_MATNR) TYPE  MATNR
*"     REFERENCE(I_MENGE) TYPE  ANY
*"     REFERENCE(I_MEINH) TYPE  LRMEI OPTIONAL
*"     REFERENCE(I_UMB) TYPE  FLAG OPTIONAL
*"     REFERENCE(I_EAN11) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_MENGE) TYPE  ANY
*"     REFERENCE(E_EAN11) TYPE  EAN11
*"     REFERENCE(E_GTIN_VARIANT) TYPE  GTIN_VARIANT
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  CHANGING
*"     REFERENCE(C_MEINH) TYPE  LRMEI
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------



  DATA: lv_menge_i TYPE bstmg,
        lv_menge_o TYPE bstmg,
        lv_meinh_i TYPE meinh,
        lv_meinh_o TYPE meinh,
        lv_meins   TYPE meins,
        lv_itf14   TYPE char14,
        ls_message TYPE bdcmsgcoll.

***********************************************************************
  lv_menge_i = i_menge.

  CLEAR: et_messages, e_menge, e_ean11, e_gtin_variant.

** Dados Mestre Material
***********************************************************************
  SELECT SINGLE meins gtin_variant
           FROM mara INTO (lv_meins, e_gtin_variant)
          WHERE matnr EQ i_matnr.

  IF sy-subrc <> 0.
**  Material & inválido
    ls_message-msgid  = 'ZRF001'.
    ls_message-msgtyp = 'E'.
    ls_message-msgnr  = '033'.
    ls_message-msgv1  = i_matnr.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.


** Unidade de Entrada
***********************************************************************
  IF NOT i_meinh IS INITIAL.
    lv_meinh_i = i_meinh.
  ELSE.
**  Assume-se a UMB
    lv_meinh_i = lv_meins.
  ENDIF.



** Unidade de Saída
***********************************************************************
  IF NOT c_meinh IS INITIAL.
    lv_meinh_o = c_meinh.
  ENDIF.

  IF NOT i_umb IS INITIAL OR c_meinh IS INITIAL.
**  Unidade de Saída vai ser a UMB
    lv_meinh_o = lv_meins.
  ENDIF.


  c_meinh = lv_meinh_o.


** EAN11
***********************************************************************
  SELECT SINGLE ean11 FROM marm INTO e_ean11
          WHERE matnr EQ i_matnr
            AND meinh EQ lv_meinh_o.

  IF sy-subrc <> 0.
**  Unidade de Medida & não mantida para o Material &
    ls_message-msgid  = 'ZRF001'.
    ls_message-msgtyp = 'E'.
    ls_message-msgnr  = '034'.
    ls_message-msgv1  = lv_meinh_o.
    ls_message-msgv2  = i_matnr.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.

** Valida se sem Conversão
***********************************************************************
  IF lv_meinh_o EQ lv_meinh_i.
    e_menge = lv_menge_i.
    EXIT.
  ENDIF.

** Converte Unidades
***********************************************************************
  CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
    EXPORTING
      i_matnr              = i_matnr
      i_in_me              = lv_meinh_i
      i_out_me             = lv_meinh_o
      i_menge              = lv_menge_i
    IMPORTING
      e_menge              = lv_menge_o
    EXCEPTIONS
      error_in_application = 1
      error                = 2
      error_message        = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    ls_message-msgid  = sy-msgid.
    ls_message-msgtyp = sy-msgty.
    ls_message-msgnr  = sy-msgno.
    ls_message-msgv1  = sy-msgv1.
    ls_message-msgv2  = sy-msgv2.
    ls_message-msgv3  = sy-msgv3.
    ls_message-msgv4  = sy-msgv4.
    APPEND ls_message TO et_messages.
    RAISE error.
  ENDIF.


** Sucesso
***********************************************************************
  e_menge = lv_menge_o.



ENDFUNCTION.
