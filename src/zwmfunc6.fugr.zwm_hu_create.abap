FUNCTION zwm_hu_create.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     REFERENCE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_LGORT) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(I_PACKING_MAT) TYPE  MATNR OPTIONAL
*"     REFERENCE(I_VHILM_KU) TYPE  VHILM_KU OPTIONAL
*"     REFERENCE(I_INHALT) TYPE  INHALT OPTIONAL
*"     REFERENCE(I_EXIDV2) TYPE  EXIDV2 OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  FLAG OPTIONAL
*"     REFERENCE(IT_ITEMS) TYPE  ZRF01_T_BAPIHUITMPROPOSAL OPTIONAL
*"  EXPORTING
*"     REFERENCE(ES_VEKP) TYPE  VEKP
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  CHANGING
*"     REFERENCE(C_EXIDV) TYPE  EXIDV
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  DATA: ls_header   TYPE bapihuhdrproposal,
        ls_item     TYPE bapihuitmproposal,
        lt_items    TYPE TABLE OF bapihuitmproposal,
        lt_bapiret2 TYPE TABLE OF bapiret2,
        ls_bapiret2 TYPE bapiret2,
        ls_message  TYPE bdcmsgcoll.

***********************************************************************
  CLEAR: et_messages, es_vekp.

** Gera SSCC novo e valida choques de numeração
***********************************************************************
  IF c_exidv IS INITIAL.
    CALL FUNCTION 'ZWM_SSCC_GENERATE'
      EXPORTING
        i_lgnum     = i_lgnum
        i_werks     = i_werks
        i_lgort     = i_lgort
      IMPORTING
        e_exidv     = c_exidv
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      RAISE error.
    ENDIF.
  ENDIF.



** Dados de Cabeçalho
***********************************************************************
  ls_header-plant             = i_werks.
  ls_header-stge_loc          = i_lgort.
  ls_header-pack_mat_customer = i_vhilm_ku.
  ls_header-content           = i_inhalt.
  ls_header-ext_id_hu_2       = i_exidv2.
  ls_header-pack_mat          = i_packing_mat.
  ls_header-hu_exid           = c_exidv.

  IF NOT c_exidv IS INITIAL.
    ls_header-hu_exid_type = 'H'.
  ENDIF.


** Items
***********************************************************************
  LOOP AT it_items INTO ls_item.
    IF ls_item-stge_loc IS INITIAL.
*     Valor 'A' para o Deposito não ser obrigatório
      ls_header-hu_status_init = 'A'.
    ENDIF.

    IF ls_item-lower_level_exid IS INITIAL.
      ls_item-hu_item_type = '1'. " Material
    ELSE.
      ls_item-hu_item_type = '3'. " HU
    ENDIF.

    APPEND ls_item TO lt_items.
    CLEAR  ls_item.
  ENDLOOP.

  IF ls_header-stge_loc IS INITIAL.
*     Valor 'A' para o Deposito não ser obrigatório
    ls_header-hu_status_init = 'A'.
  ENDIF.

** Criação da HU
***********************************************************************
  CALL FUNCTION 'BAPI_HU_CREATE'
    EXPORTING
      headerproposal = ls_header
    IMPORTING
      hukey          = c_exidv
    TABLES
      itemsproposal  = lt_items
      return         = lt_bapiret2
    EXCEPTIONS
      error_message  = 98
      OTHERS         = 99.

  IF sy-subrc <> 0 AND ( sy-msgty EQ 'E' OR sy-msgty EQ 'A' ).
    READ TABLE lt_bapiret2 WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
**    Erro a processar HU &
      ls_bapiret2-type       = 'E'.
      ls_bapiret2-id         = 'ZRF001'.
      ls_bapiret2-number     = '215'.
      ls_bapiret2-message_v1 = c_exidv.
      APPEND ls_bapiret2 TO lt_bapiret2.
      CLEAR  ls_bapiret2.

      ls_bapiret2-type       = 'E'.
      ls_bapiret2-id         = sy-msgid.
      ls_bapiret2-number     = sy-msgno.
      ls_bapiret2-message_v1 = sy-msgv1.
      ls_bapiret2-message_v2 = sy-msgv2.
      ls_bapiret2-message_v3 = sy-msgv3.
      ls_bapiret2-message_v4 = sy-msgv4.
      APPEND ls_bapiret2 TO lt_bapiret2.
      CLEAR  ls_bapiret2.
    ENDIF.
  ENDIF.

** Controlo de Erros
***********************************************************************
  DELETE lt_bapiret2 WHERE NOT ( type EQ 'E' OR type EQ 'A' ).

  LOOP AT lt_bapiret2 INTO ls_bapiret2.
    ls_message-msgtyp = 'E'.
    ls_message-msgid  = ls_bapiret2-id.
    ls_message-msgnr  = ls_bapiret2-number.
    ls_message-msgv1  = ls_bapiret2-message_v1.
    ls_message-msgv2  = ls_bapiret2-message_v2.
    ls_message-msgv3  = ls_bapiret2-message_v3.
    ls_message-msgv4  = ls_bapiret2-message_v4.
    APPEND ls_message TO et_messages.
    CLEAR  ls_message.
  ENDLOOP.


  IF sy-subrc EQ 0.
    RAISE error.
  ENDIF.


** Commit
***********************************************************************
  CHECK NOT i_commit IS INITIAL.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

** Espera
**********************************************************************
  DO 20 TIMES.
    IF sy-index > 1.
      WAIT UP TO 1 SECONDS.
    ENDIF.

    SELECT SINGLE * FROM vekp
                    INTO es_vekp
                    WHERE exidv = c_exidv.
    CHECK sy-subrc EQ 0.

    EXIT.
  ENDDO.
ENDFUNCTION.
