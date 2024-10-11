FUNCTION zwm_pal_picking.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARMAZEM) LIKE  LTAK-LGNUM
*"     REFERENCE(ACTUALIZA) LIKE  ZWM_AUX-ACTUALIZA
*"     REFERENCE(REMONTADA) TYPE  FLAG OPTIONAL
*"     REFERENCE(2STEP_RED) TYPE  FLAG DEFAULT 'X'
*"  TABLES
*"      ZPALETE_PICKING STRUCTURE  ZPALETE_PICKING
*"----------------------------------------------------------------------
  CONSTANTS: c_voleh LIKE mara-voleh VALUE 'M3'.

  DATA : l_total_qtd LIKE zpalete_picking-lfimg,
         save_index  LIKE sy-tabix,
         st_type_pck LIKE zwm001-valor,
         st_type_pkb LIKE zwm001-valor,
         resto       LIKE zpalete_picking-pal_completa,
         divisao     LIKE zpalete_picking-pal_completa.

  CLEAR : l_total_qtd,
          save_index,
          st_type_pck,
          resto,
          divisao.

  DATA: itab_mara LIKE zwm_mara OCCURS 0 WITH HEADER LINE.

  TYPES: BEGIN OF lty_grp,
           refnr TYPE lvs_refnr,
           vbeln TYPE vbeln,
           posnr TYPE posnr,
         END OF lty_grp.

  DATA: BEGIN OF itab_mlgn OCCURS 0,
          lgnum LIKE mlgn-lgnum,
          matnr LIKE mlgn-matnr,
          lhmg1 LIKE mlgn-lhmg1,
          lety1 LIKE mlgn-lety1,
          plkpt LIKE mlgn-plkpt,
        END OF itab_mlgn.

  DATA: BEGIN OF itab_mlgt OCCURS 0,
          lgnum LIKE mlgt-lgnum,
          matnr LIKE mlgt-matnr,
          lgtyp LIKE mlgt-lgtyp,
          lgpla LIKE mlgt-lgpla,
        END OF itab_mlgt.

  DATA: lt_pal_picking TYPE TABLE OF zpalete_picking,
        lt_new_pallet  TYPE TABLE OF zpalete_picking,
        lt_delitem     TYPE TABLE OF l2skdlitem,
        lt_total       TYPE TABLE OF l2sktotal,
        lt_grp_delete  TYPE TABLE OF lty_grp.

  DATA: ls_pal_picking TYPE zpalete_picking,
        ls_new_pallet  TYPE zpalete_picking,
        ls_delitem     TYPE l2skdlitem,
        ls_grp_delete  TYPE lty_grp.

  DATA: lv_2step      TYPE flag,
        lv_2spart     TYPE flag,
        lv_step       TYPE c,
        lv_last_step  TYPE c,
        lv_last_refnr TYPE lvs_refnr,
        lv_lfimg      TYPE menge_d,
        lv_tabix      TYPE sytabix.

  FIELD-SYMBOLS: <ls_new_pallet> TYPE zpalete_picking.

  CHECK NOT zpalete_picking[] IS INITIAL.

** Tipo de depósito picking fixo
  PERFORM get_parameter
          USING armazem
                'ENTRADA_ARMAZEM'
                'ST_PCK'
                st_type_pck.

** Tipo de depósito picking variavel
  PERFORM get_parameter
          USING armazem
                'ENTRADA_ARMAZEM'
                'ST_PKB'
                st_type_pkb.


*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 21.05.2012 09:50:22
*  Motivo: Picking Em 2 Passos
*--------------------------------------------------------------------*
  lt_pal_picking = zpalete_picking[].

  SORT lt_pal_picking BY refnr.

  LOOP AT lt_pal_picking INTO ls_pal_picking.
    IF ls_pal_picking-refnr <> lv_last_refnr.
      CLEAR: lv_step, lv_last_step.

      IF sy-tcode <> 'ZWM127' AND 2step_red = 'X'.
*         sy-tcode NE 'ZWM166'.
        CALL FUNCTION 'ZWM_2STEP_REDETERMINANTE'
          EXPORTING
            i_lgnum = armazem
            i_refnr = ls_pal_picking-refnr.
      ENDIF.


      CALL FUNCTION 'L_2_STEP_QUANTITY_REMOVAL'
        EXPORTING
          i_lgnum                       = armazem
          i_refnr                       = ls_pal_picking-refnr
        TABLES
          t_delitem                     = lt_delitem
          t_total                       = lt_total
        EXCEPTIONS
          refnr_no_found                = 1
          refnr_documents_no_found      = 2
          no_relevant_for_2step_picking = 3
          item_for_removal_not_found    = 4
          OTHERS                        = 5.


      lv_last_refnr = ls_pal_picking-refnr.
    ENDIF.

    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = armazem
        i_refnr  = ls_pal_picking-refnr
        i_vbeln  = ls_pal_picking-vbeln
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    CLEAR: ls_new_pallet, lv_lfimg.
    LOOP AT lt_delitem INTO ls_delitem WHERE matnr EQ ls_pal_picking-matnr.

      IF lv_2step EQ 'X'.
        lv_step = '2'.
      ELSE.
        lv_step = '1'.
      ENDIF.

      IF lv_2spart EQ abap_true.
        CHECK ls_delitem-vbeln EQ ls_pal_picking-vbeln.
      ENDIF.

      IF lv_last_step IS INITIAL.
        lv_last_step = lv_step.
      ENDIF.

      IF lv_2spart EQ abap_true.
        ls_new_pallet = ls_pal_picking.
        ls_new_pallet-vbeln = ls_pal_picking-vbeln.
        ls_new_pallet-posnr = gc_posnr_2step_dummy.
        lv_lfimg = lv_lfimg + ls_delitem-lfimg.
      ELSE. "IF lv_2step EQ abap_true. "< @Diogo - Trecho não enviado antes a PRD->removido
        ls_new_pallet = ls_pal_picking.
        ls_new_pallet-vbeln = gc_vbeln_2step_dummy.
        ls_new_pallet-posnr = gc_posnr_2step_dummy.
        lv_lfimg = lv_lfimg + ls_delitem-lfimg.
*      ELSE."< @Diogo - Trecho não enviado antes a PRD->removido
*        CONTINUE."< @Diogo - Trecho não enviado antes a PRD->removido
      ENDIF.

      CLEAR: ls_grp_delete.
      ls_grp_delete-refnr = ls_delitem-refnr.
      ls_grp_delete-vbeln = ls_delitem-vbeln.
      ls_grp_delete-posnr = ls_delitem-posnr.
      APPEND ls_grp_delete TO lt_grp_delete.
    ENDLOOP.

    ls_new_pallet-lfimg = lv_lfimg.

    IF sy-subrc <> 0 OR
      lv_step <> '2'.
**      DELETE lt_new_pallet  WHERE refnr = ls_pal_picking-refnr AND
**                                  vbeln = ls_pal_picking-vbeln.
      DELETE lt_pal_picking WHERE refnr = ls_pal_picking-refnr AND
                                  vbeln = ls_pal_picking-vbeln.
      CONTINUE.
    ENDIF.

**    COLLECT ls_new_pallet INTO lt_new_pallet.

    IF lv_2step EQ 'X'.
      READ TABLE lt_new_pallet
        ASSIGNING <ls_new_pallet>
         WITH KEY refnr     = ls_new_pallet-refnr
                  matnr     = ls_new_pallet-matnr
                  vbeln     = ls_new_pallet-vbeln
                  posnr     = ls_new_pallet-posnr
                  sub_item  = ls_new_pallet-sub_item.

      IF sy-subrc EQ 0.
        <ls_new_pallet>-volum = <ls_new_pallet>-volum + ls_new_pallet-volum.
        CONTINUE.
      ELSE.
        COLLECT ls_new_pallet INTO lt_new_pallet.
      ENDIF.
    ELSE.
      COLLECT ls_new_pallet INTO lt_new_pallet.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_grp_delete INTO ls_grp_delete.
    DELETE zpalete_picking WHERE refnr EQ ls_grp_delete-refnr AND
                                 vbeln EQ ls_grp_delete-vbeln AND
                                 posnr EQ ls_grp_delete-posnr AND
                                 nodel EQ abap_false.
  ENDLOOP.

  LOOP AT zpalete_picking WHERE nodel EQ abap_true.
    lv_tabix = sy-tabix.

    CLEAR: zpalete_picking-nodel.

    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        i_lgnum  = armazem
        i_refnr  = zpalete_picking-refnr
        i_vbeln  = zpalete_picking-vbeln
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF lv_2step EQ abap_true AND lv_2spart EQ abap_true.
      zpalete_picking-vbeln = gc_posnr_2step_dummy.
    ELSEIF lv_2step EQ abap_true.
      zpalete_picking-vbeln = gc_vbeln_2step_dummy.
      zpalete_picking-posnr = gc_posnr_2step_dummy.
    ELSE.
      CONTINUE.
    ENDIF.

    DELETE zpalete_picking INDEX lv_tabix.

    COLLECT zpalete_picking INTO lt_new_pallet.
  ENDLOOP.

  IF NOT lt_new_pallet IS INITIAL.
    APPEND LINES OF lt_new_pallet TO zpalete_picking[].
  ENDIF.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

** Cálculo das paletes completas e incompletas
**********************************************************************
  CHECK NOT zpalete_picking[] IS INITIAL.

  SORT zpalete_picking BY vbeln matnr posnr.

  CALL FUNCTION 'ZWM_CALCULA_PALETE'
    EXPORTING
      armazem         = armazem
      actualiza       = actualiza
      remontada       = remontada
    TABLES
      zpalete_picking = zpalete_picking.

ENDFUNCTION.
