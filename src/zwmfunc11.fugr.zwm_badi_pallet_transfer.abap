FUNCTION zwm_badi_pallet_transfer.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IT_XMKPF) TYPE  TY_T_MKPF
*"     VALUE(IT_XMSEG) TYPE  TY_T_MSEG
*"  EXPORTING
*"     VALUE(IT_DOCU) TYPE  ZWM_TT_DOCU_YEAR
*"----------------------------------------------------------------------
  TYPES: BEGIN OF lty_vbeln,
          vbeln TYPE vbeln_vl,
         END OF lty_vbeln.

  DATA: lt_xmseg    TYPE ty_t_mseg,
        lt_zwm001   TYPE TABLE OF zwm001,
        lt_mseg     TYPE TABLE OF mseg,
        lt_ekpo     TYPE TABLE OF ekpo,
        lt_likp     TYPE TABLE OF likp,
        lt_lips     TYPE TABLE OF lips,
        lt_vbeln    TYPE TABLE OF lty_vbeln,
        lt_items    TYPE tab_bapi_goodsmvt_item,
        lt_messages	TYPE tab_bdcmsgcoll.

  DATA: lt_werks    TYPE TABLE OF werks_d,
        lt_lfart    TYPE TABLE OF lfart,
        lt_or_bwart TYPE TABLE OF bwart,
        lt_bwart    TYPE TABLE OF bwart,
        lt_pstyv    TYPE TABLE OF pstyv_vl.

  DATA: lv_werks    TYPE werks_d,
        lv_lfart    TYPE lfart,
        lv_or_bwart TYPE bwart,
        lv_bwart    TYPE bwart,
        lv_pstyv    TYPE pstyv_vl,
        lv_vbeln    TYPE vbeln_vl,
        lv_xblnr    TYPE xblnr,
        lv_target   TYPE so_recname.

  DATA: ls_zwm001 TYPE zwm001,
        ls_mkpf   TYPE mkpf,
        ls_xmseg  TYPE mseg,
        ls_mseg   TYPE mseg,
        ls_ekpo   TYPE ekpo,
        ls_likp   TYPE likp,
        ls_lips   TYPE lips,
        ls_lips2  TYPE lips,
        ls_vbeln  TYPE lty_vbeln,
        ls_item   TYPE bapi2017_gm_item_create.

  DATA: lv_last_bwart TYPE bwart,
        lv_reverse    TYPE flag,
        lv_mblnr      TYPE mblnr,
        lv_mjahr      TYPE mjahr,
        lv_wait       TYPE i.

* ROFF SDF JA : Pedido RENSAF00006 09.05.2013
  DATA: ls_docu TYPE zwm_docu_year.

* ROFF SDF JA : Pedido RENSAF00006 09.05.2013
  CHECK NOT it_xmkpf IS INITIAL AND
        NOT it_xmseg IS INITIAL.

  lt_xmseg = it_xmseg.
  SORT lt_xmseg BY mblnr mjahr bwart.

*  DATA: lv_exit TYPE flag.
*
*  DO.
*
*    IF lv_exit EQ 'X' OR
*       sy-uname(4) <> 'ROFF'.
*      EXIT.
*    ENDIF.
*
*  ENDDO.


  LOOP AT it_xmkpf INTO ls_mkpf.

    lv_wait = 0.

    DO.

*     Verifica se o documento já foi criado
      CALL FUNCTION 'ENQUEUE_EMMKPF'
        EXPORTING
          mblnr          = ls_mkpf-mblnr
          mjahr          = ls_mkpf-mjahr
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.

      IF sy-subrc = 0.
        EXIT.
      ENDIF.

      lv_wait = lv_wait + 1.

      WAIT UP TO 1 SECONDS.

      IF lv_wait > 60.
        EXIT.
      ENDIF.

    ENDDO.

    CALL FUNCTION 'DEQUEUE_EMMKPF'.

  ENDLOOP.

** Parametros
***********************************************************************
  SELECT * FROM zwm001
     INTO TABLE lt_zwm001
     WHERE armazem  = '' AND
           processo = 'TRANSF_PALLETS'.


  LOOP AT lt_zwm001 INTO ls_zwm001.
    IF ls_zwm001-parametro(6) EQ 'CENTRO'.
      lv_werks = ls_zwm001-valor.
      APPEND lv_werks TO lt_werks.
    ELSEIF ls_zwm001-parametro(10) EQ 'TP_REMESSA'.
      lv_lfart = ls_zwm001-valor.
      APPEND lv_lfart TO lt_lfart.
    ELSEIF ls_zwm001-parametro(10) EQ 'MOV_ORIGEM'.
      lv_or_bwart = ls_zwm001-valor.
      APPEND lv_or_bwart TO lt_or_bwart.
    ELSEIF ls_zwm001-parametro(6) EQ 'MOV_MM'.
      lv_bwart = ls_zwm001-valor.
      APPEND lv_bwart TO lt_bwart.
    ELSEIF ls_zwm001-parametro(8) EQ 'CAT_ITEM'.
      lv_pstyv = ls_zwm001-valor.
      APPEND lv_pstyv TO lt_pstyv.
    ELSEIF ls_zwm001-parametro EQ 'LISTA_DIST_MAIL'.
      lv_target = ls_zwm001-valor.
    ENDIF.
  ENDLOOP.

** Retorna Remessa
***********************************************************************
*  LOOP AT it_mkpf INTO ls_mkpf.
*    CHECK NOT ls_mkpf-xblnr IS INITIAL.
*    ls_vbeln-vbeln = ls_mkpf-xblnr.
*    APPEND ls_vbeln TO lt_vbeln.
*  ENDLOOP.
*
*  CHECK NOT lt_vbeln IS INITIAL.
*
*  SORT lt_vbeln BY vbeln.
*  DELETE ADJACENT DUPLICATES FROM lt_vbeln COMPARING vbeln.

  SELECT * FROM likp
     INTO TABLE lt_likp
     FOR ALL ENTRIES IN it_xmkpf
     WHERE vbeln = it_xmkpf-le_vbeln.

  CHECK sy-subrc EQ 0.
  SORT lt_likp BY vbeln.


  SELECT * FROM lips
     INTO TABLE lt_lips
     FOR ALL ENTRIES IN lt_likp
     WHERE vbeln = lt_likp-vbeln.

  CHECK sy-subrc EQ 0.
  SORT lt_lips BY vbeln posnr.

  SELECT * FROM ekpo
     INTO TABLE lt_ekpo
     FOR ALL ENTRIES IN lt_lips
     WHERE ebeln = lt_lips-vgbel.

  DELETE lt_ekpo WHERE werks IS INITIAL OR
                       lgort IS INITIAL.

  CHECK NOT lt_ekpo IS INITIAL.
  SORT lt_ekpo BY ebeln ebelp.



  LOOP AT it_xmkpf INTO ls_mkpf.
    CLEAR: lv_last_bwart, lv_xblnr.
* ROFF SDF JA : Pedido RENSAF00006 09.05.2013
    ls_docu-mblnr = ls_mkpf-mblnr.
    ls_docu-mjahr = ls_mkpf-mjahr.
* ROFF SDF JA : Pedido RENSAF00006 09.05.2013
** Valida Tipo de Remessa
***********************************************************************
    CLEAR ls_likp.
    READ TABLE lt_likp
          INTO ls_likp
          WITH KEY vbeln = ls_mkpf-xblnr
          BINARY SEARCH.

    CHECK sy-subrc EQ 0.

    READ TABLE lt_lfart
      WITH KEY table_line = ls_likp-lfart
      TRANSPORTING NO FIELDS.

    CHECK sy-subrc EQ 0.

    LOOP AT lt_xmseg INTO ls_xmseg WHERE mblnr = ls_mkpf-mblnr AND
                                         mjahr = ls_mkpf-mjahr.
      ls_docu-bwart = ls_xmseg-bwart.
** Valida Movimento
***********************************************************************
      READ TABLE lt_or_bwart
        WITH KEY table_line = ls_xmseg-bwart
        TRANSPORTING NO FIELDS.

      CHECK sy-subrc EQ 0.

** Movimento de Destino
***********************************************************************
      CLEAR lv_bwart.
      READ TABLE lt_bwart
            INTO lv_bwart
            INDEX sy-tabix.

      CHECK sy-subrc EQ 0.

** Valida Centro
***********************************************************************
      READ TABLE lt_werks
        WITH KEY table_line = ls_xmseg-werks
        TRANSPORTING NO FIELDS.

      CHECK sy-subrc EQ 0.

      IF NOT ls_xmseg-sjahr IS INITIAL AND
         NOT ls_xmseg-smbln IS INITIAL AND
         NOT ls_xmseg-smblp IS INITIAL.
        lv_reverse = abap_true.
      ELSE.
        CLEAR lv_reverse.
      ENDIF.


      LOOP AT lt_lips INTO ls_lips WHERE vbeln = ls_likp-vbeln AND
                                         matnr = ls_xmseg-matnr.

** Valida Categoria de Item de Remessa
***********************************************************************
        READ TABLE lt_pstyv
         WITH KEY table_line = ls_lips-pstyv
         TRANSPORTING NO FIELDS.

        CHECK sy-subrc EQ 0.

        CLEAR ls_ekpo.
        LOOP AT lt_lips INTO ls_lips2 WHERE vbeln = ls_likp-vbeln AND
                                            NOT vgbel IS INITIAL AND
                                            NOT vgpos IS INITIAL.
          READ TABLE lt_ekpo
                INTO ls_ekpo
                WITH KEY ebeln = ls_lips2-vgbel
                         ebelp = ls_lips2-vgpos
                BINARY SEARCH.

          EXIT.
        ENDLOOP.

        CHECK NOT ls_ekpo IS INITIAL.

        IF lv_reverse EQ abap_true.
          ls_item-plant      = ls_lips-werks.
          ls_item-stge_loc   = ls_lips-lgort.
          ls_item-move_plant = ls_ekpo-werks.
          ls_item-move_stloc = ls_ekpo-lgort.
        ELSE.
          ls_item-plant      = ls_ekpo-werks.
          ls_item-stge_loc   = ls_ekpo-lgort.
          ls_item-move_plant = ls_lips-werks.
          ls_item-move_stloc = ls_lips-lgort.
        ENDIF.

        ls_item-material   = ls_lips-matnr.

        ls_item-entry_qnt  = ls_lips-lfimg.
        ls_item-entry_uom  = ls_lips-vrkme.

        lv_xblnr = ls_lips2-vbeln.
        lv_vbeln = ls_lips2-vbeln.
        ls_item-costcenter = ls_xmseg-kostl.
        ls_item-amount_lc  = ls_xmseg-dmbtr.
        APPEND ls_item TO lt_items.
      ENDLOOP.
    ENDLOOP.
    DATA: lv_datum TYPE sy-datum.
    lv_datum = ls_mkpf-budat.
    CALL FUNCTION 'ZWM_GOODSMVT_CREATE'
      EXPORTING
        i_code               = '06'
        i_bwart              = lv_bwart
*       I_BSSKZ              =
*       I_ABLAD              =
*       I_AUFNR              =
        i_xblnr              = lv_xblnr
*       I_XABLN              =
*       I_SGTXT              =
*       I_GRUND              =
*       I_WAIT               = '20'
*       I_FRBNR              =
*       I_LIFNR              =
       i_datum              = lv_datum
*       I_HEADER_TXT         =
*       I_TESTRUN            =
        it_items             = lt_items
*       IT_EXTENSIONIN       =
        i_commit             = abap_true
     IMPORTING
       e_mblnr              = lv_mblnr
       e_mjahr              = lv_mjahr
       et_mseg              = lt_mseg
       et_messages          = lt_messages
     EXCEPTIONS
       error                = 1
       OTHERS               = 2.

    IF sy-subrc <> 0.
      READ TABLE lt_mseg
            INTO ls_mseg
            INDEX 1.

      CALL FUNCTION 'ZWM_PALLET_TRANSFER_EMAIL'
        EXPORTING
          i_title      = 'Transferencia de Paletes'
*          is_mseg      = ls_mseg
          is_mkpf_orig = ls_mkpf
          is_likp      = ls_likp
          it_messages  = lt_messages
          i_target     = lv_target
          i_commit     = abap_true.

      CONTINUE.
* ROFF SDF JA : Pedido RENSAF00006 09.05.2013
*    ENDIF.
    ELSE.
      ls_docu-mblnrnew = lv_mblnr.
      ls_docu-mjahrnew = lv_mjahr.
      ls_docu-processado = 'X'.
      APPEND ls_docu TO it_docu.

    ENDIF.
* ROFF SDF JA : Pedido RENSAF00006 09.05.2013
  ENDLOOP.

* ROFF SDF JA : Pedido RENSAF00006 09.05.2013
  DELETE ADJACENT DUPLICATES FROM it_docu COMPARING ALL FIELDS.
* ROFF SDF JA : Pedido RENSAF00006 09.05.2013
ENDFUNCTION.
