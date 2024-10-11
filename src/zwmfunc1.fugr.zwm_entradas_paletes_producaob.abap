FUNCTION ZWM_ENTRADAS_PALETES_PRODUCAOB .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_WERKS) TYPE  WERKS_D
*"     REFERENCE(I_LGORT) TYPE  LGORT_D
*"     REFERENCE(IT_ITEMS) TYPE  ZWM_TT018
*"----------------------------------------------------------------------

*  DATA: lt_items TYPE zwm_tt018,
*        lt_mard         TYPE HASHED TABLE OF mard WITH UNIQUE KEY matnr,
*        lt_items_create TYPE zwm_tt018.
*
*  DATA: ls_item TYPE zwm018,
*        ls_mard TYPE mard.
*
*  DATA: lv_mblnr TYPE  mblnr,
*        lv_mjahr TYPE mjahr.
*
*** Parametros
************************************************************************
*  PERFORM get_zwm001 TABLES itab_zwm001
*                     USING f_lgnum
*                           'ENTRADA_PRODUCAO'
*                           'CODE'
*                     CHANGING valor.
*
*** Agrupa Items
************************************************************************
*  LOOP AT it_items INTO ls_item.
*    COLLECT ls_item INTO lt_items.
*  ENDLOOP.
*
*** Valiad Stock
************************************************************************
*  CHECK NOT lt_items IS INITIAL.
*
*  SELECT * FROM mard
*     INTO TABLE lt_mard
*     FOR ALL ENTRIES IN lt_items
*     WHERE matnr = lt_items-material AND
*           werks = i_werks AND
*           lgort = i_lgort.
*
*  SORT lt_mard BY matnr.
*
*  LOOP AT lt_items INTO ls_item.
*    READ TABLE lt_mard
*          INTO ls_mard
*          WITH TABLE KEY matnr = ls_item-material.
*    CHECK sy-subrc EQ 0.
*
*    CHECK ls_item-quantidade > ls_mard-labst.
*
*    APPEND ls_item TO lt_items_create.
*  ENDLOOP.
*
*** Cria Documentos
************************************************************************
*  CHECK NOT lt_items_create IS INITIAL.
*
*  CALL FUNCTION 'ZWM_ENTRADAS_MATERIAL'
*    EXPORTING
*      lgnum            = i_lgnum
*      code             = lv_code
*      mov_mm           = lv_mov_mm
*      testrun          = ' '
*      plant_o          = i_werks
*      sloc_o           = i_lgort
*    IMPORTING
*      materialdocument = lv_mblnr
*      matdocumentyear  = lv_mjahr
*    TABLES
*      return_msg       = lt_return_msg
*      items            = lt_items_create
*    EXCEPTIONS
*      error            = 1
*      OTHERS           = 2.




ENDFUNCTION.
