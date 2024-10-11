FUNCTION zwm_entradas_paletes_producao .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_WERKS) TYPE  WERKS_D
*"     REFERENCE(I_LGORT) TYPE  LGORT_D
*"     REFERENCE(IT_ITEMS) TYPE  ZWM_TT018
*"----------------------------------------------------------------------

  DATA: lt_items        TYPE tab_bapi_goodsmvt_item,
        lt_mard         TYPE HASHED TABLE OF mard WITH UNIQUE KEY matnr,
        lt_items_create TYPE tab_bapi_goodsmvt_item,
        lt_messages     TYPE tab_bdcmsgcoll.

  DATA: ls_item_in     TYPE zwm018,
        ls_item        TYPE bapi2017_gm_item_create,
        ls_mard        TYPE mard,
        ls_return_msg  TYPE bdcmsgcoll.

  DATA: lv_mblnr     TYPE mblnr,
        lv_mjahr     TYPE mjahr,
        lv_menge     TYPE menge_d,
        lv_amount    TYPE bapi_exbwr,
        lv_code      TYPE gm_code,
        lv_mov_mm    TYPE bwart,
        lv_parameter TYPE char50,
        lv_target    TYPE so_recname.

** Parametros
***********************************************************************
  PERFORM get_zwm001 TABLES itab_zwm001
                     USING i_lgnum
                           'ENT_PROD_PAL_ADHOC'
                           'CODE'
                     CHANGING lv_code.

  PERFORM get_zwm001 TABLES itab_zwm001
                     USING i_lgnum
                           'ENT_PROD_PAL_ADHOC'
                           'TIP_MOV'
                     CHANGING lv_mov_mm.


  PERFORM get_zwm001 TABLES itab_zwm001
                     USING i_lgnum
                           'ENT_PROD_PAL_ADHOC'
                           'LISTA_DISTRIB'
                     CHANGING lv_target.


** Agrupa Items
***********************************************************************
  LOOP AT it_items INTO ls_item_in.
    ls_item-material   = ls_item_in-material.
    ls_item-plant      = i_werks.
    ls_item-stge_loc   = i_lgort.
    ls_item-entry_qnt  = ls_item_in-quantidade.
    ls_item-entry_uom  = ls_item_in-uni.
    COLLECT ls_item INTO lt_items.
  ENDLOOP.

** Valiad Stock
***********************************************************************
  CHECK NOT lt_items IS INITIAL.

  SELECT * FROM mard
     INTO TABLE lt_mard
     FOR ALL ENTRIES IN lt_items
     WHERE matnr = lt_items-material AND
           werks = i_werks AND
           lgort = i_lgort.

  SORT lt_mard BY matnr.

  LOOP AT lt_items INTO ls_item.
    READ TABLE lt_mard
          INTO ls_mard
          WITH TABLE KEY matnr = ls_item-material.
    CHECK sy-subrc EQ 0.

    CHECK ls_item-entry_qnt > ls_mard-labst.

    CONCATENATE 'QTD_'
                ls_item-material
           INTO lv_parameter.

    PERFORM get_zwm001 TABLES itab_zwm001
                       USING i_lgnum
                            'ENT_PROD_PAL_ADHOC'
                            lv_parameter
                       CHANGING lv_menge.
    CHECK lv_menge > 0.

    CONCATENATE 'PRECO_'
                ls_item-material
           INTO lv_parameter.

    PERFORM get_zwm001 TABLES itab_zwm001
                       USING i_lgnum
                            'ENT_PROD_PAL_ADHOC'
                            lv_parameter
                       CHANGING lv_amount.
*    CHECK lv_menge > 0.

    ls_item-entry_qnt = lv_menge.

*    ls_item-amount_lc  = lv_menge * lv_amount.

    APPEND ls_item TO lt_items_create.
  ENDLOOP.

** Cria Documentos
***********************************************************************
  CHECK NOT lt_items_create IS INITIAL.

  CALL FUNCTION 'ZWM_GOODSMVT_CREATE'
    EXPORTING
      i_code      = lv_code
      i_bwart     = lv_mov_mm
      it_items    = lt_items_create
    IMPORTING
      e_mblnr     = lv_mblnr
      e_mjahr     = lv_mjahr
      et_messages = lt_messages
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

** Envio de Emails
***********************************************************************
  CALL FUNCTION 'ZWM_EMAIL_ENT_PAL_PRODUCAO'
    EXPORTING
      i_werks     = i_werks
      i_lgort     = i_lgort
      i_mblnr     = lv_mblnr
      i_mjahr     = lv_mjahr
      it_messages = lt_messages
      it_items    = lt_items_create
      i_target    = lv_target
      i_commit    = abap_true.
ENDFUNCTION.
