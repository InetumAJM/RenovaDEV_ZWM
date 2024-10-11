FUNCTION ZWM_DELIVERY_PICKING_SSCC_DEL.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"     REFERENCE(I_SSCC) TYPE  EXIDV
*"  TABLES
*"      T_RETURN STRUCTURE  BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"--------------------------------------------------------------------
  DATA: lv_tabix       LIKE sy-tabix.
  DATA: lv_menge       TYPE menge_d.
  DATA: lv_menge_pk    TYPE menge_d.
  DATA: lv_pal         TYPE i.
  DATA: lv_refnr       TYPE lvs_refnr.

  DATA: ls_likp        TYPE likp.
  DATA: ls_return      TYPE bdcmsgcoll.
  DATA: ls_vbkok_wa    TYPE vbkok.
  DATA: ls_vekp        TYPE vekp.
  DATA: ls_vbuk        TYPE vbuk.

  DATA: lt_vbfa        TYPE vbfa          OCCURS 0 WITH HEADER LINE.
  DATA: lt_vbpok       TYPE vbpok         OCCURS 0 WITH HEADER LINE.
  DATA: lt_prot        TYPE prott         OCCURS 0 WITH HEADER LINE.
  DATA: lt_hu	         TYPE	hum_rehang_hu OCCURS 0 WITH HEADER LINE.
  DATA: lt_vepo        TYPE vepo          OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF lt_lips OCCURS 0.
          INCLUDE STRUCTURE lips.
  DATA: pikmg	TYPE rfmng.
  DATA: menge	TYPE rfmng.
  DATA END OF lt_lips.

** Validar Remessa
**********************************************************************
  CHECK i_sscc IS NOT INITIAL.

  SELECT SINGLE *
    FROM likp INTO ls_likp
    WHERE vbeln = i_vbeln.

  SELECT *
    FROM lips INTO TABLE lt_lips
    WHERE vbeln = i_vbeln.

  DELETE lt_lips WHERE lfimg IS INITIAL.

  SORT lt_lips BY matnr posnr DESCENDING.

** Validar Picking já efectuado
**********************************************************************
  IF lt_lips[] IS NOT INITIAL.

    SELECT *
      FROM vbfa INTO TABLE lt_vbfa
      FOR ALL ENTRIES IN lt_lips
      WHERE vbelv = lt_lips-vbeln AND
            posnv = lt_lips-posnr.

    DELETE lt_vbfa WHERE vbtyp_n <> 'Q'.
    DELETE lt_vbfa WHERE rfmng IS INITIAL.

    SORT lt_vbfa BY vbelv posnv.
  ENDIF.

** Validar quantidade picking
  LOOP AT lt_lips.

    lv_tabix = sy-tabix.

    lt_lips-menge = lt_lips-lfimg.

    LOOP AT lt_vbfa WHERE vbelv = lt_lips-vbeln AND
                          posnv = lt_lips-posnr.

      lv_menge = lt_vbfa-rfmng.

      IF lt_lips-vrkme <> lt_vbfa-meins.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = lt_lips-matnr
            i_in_me              = lt_vbfa-meins
            i_out_me             = lt_lips-vrkme
            i_menge              = lv_menge
          IMPORTING
            e_menge              = lv_menge
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
      ENDIF.

      lt_lips-pikmg = lt_lips-pikmg + lv_menge. "Qtd com picking efectuado
      lt_lips-lfimg = lt_lips-lfimg - lv_menge. "Qtd sem picking efectuado
    ENDLOOP.

*    IF lt_lips-lfimg IS INITIAL.
*      DELETE lt_lips INDEX lv_tabix.
*    ELSE.
    MODIFY lt_lips INDEX lv_tabix.
*    ENDIF.
  ENDLOOP.

** Efetuar Picking
**********************************************************************
  CLEAR ls_vbkok_wa.
  ls_vbkok_wa-vbeln_vl = ls_likp-vbeln.
  ls_vbkok_wa-vbtyp_vl = ls_likp-vbtyp.
  ls_vbkok_wa-vbeln    = ls_likp-vbeln.
*  ls_vbkok_wa-komue    = 'X'.

** Obter dados da palete
  SELECT SINGLE *
    FROM vekp INTO ls_vekp
    WHERE exidv = i_sscc.

  SELECT *
    FROM vepo INTO TABLE lt_vepo
    WHERE venum = ls_vekp-venum.

  READ TABLE lt_vepo INDEX 1.
  CHECK sy-subrc = 0.

  " Distribuir quantidade pelos items da remessa
  lv_menge = lt_vepo-vemng.

  LOOP AT lt_lips WHERE matnr = lt_vepo-matnr AND
                        charg = lt_vepo-charg.

    IF lt_vepo-vemeh <> lt_lips-vrkme.

      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = lt_vepo-matnr
          i_in_me              = lt_vepo-vemeh
          i_out_me             = lt_lips-vrkme
          i_menge              = lv_menge
        IMPORTING
          e_menge              = lv_menge
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.

      lt_vepo-vemeh = lt_lips-vrkme.
    ENDIF.

    lv_menge_pk = lv_menge - lt_lips-pikmg.

    IF lv_menge_pk < 0.
      lt_lips-lfimg = lt_lips-lfimg + lv_menge.
      lt_lips-pikmg = lt_lips-pikmg - lv_menge.
      lv_menge      = 0.

      MODIFY lt_lips INDEX sy-tabix TRANSPORTING lfimg pikmg.

    ELSE.
      lt_lips-pikmg = 0.
      lv_menge      = lv_menge - lt_lips-pikmg.

      DELETE lt_lips INDEX sy-tabix.
    ENDIF.

    CLEAR lt_vbpok.
    lt_vbpok-vbeln_vl = lt_lips-vbeln.
    lt_vbpok-posnr_vl = lt_lips-posnr.
    lt_vbpok-vbeln    = lt_lips-vbeln.
    lt_vbpok-posnn    = lt_lips-posnr.
    lt_vbpok-matnr    = lt_lips-matnr.
    lt_vbpok-charg    = lt_vepo-charg. "lt_lips-charg.
    lt_vbpok-vrkme    = lt_lips-vrkme.

    lt_vbpok-pikmg    = lt_lips-pikmg.

    APPEND lt_vbpok.

    IF lv_menge <= 0.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF lt_vbpok[] IS INITIAL.

    " Sem items da remessa para efetuar o picking.
    CLEAR ls_return.
    ls_return-msgid  = 'ZWMMSG001'.
    ls_return-msgtyp = 'E'.
    ls_return-msgnr  = '321'.
    APPEND ls_return TO t_return.

    RAISE error.
  ENDIF.

** Update
  CALL FUNCTION 'WS_DELIVERY_UPDATE'
    EXPORTING
      vbkok_wa                 = ls_vbkok_wa
      synchron                 = 'X'
      commit                   = 'X'
      delivery                 = i_vbeln
      update_picking           = 'X'
*      if_error_messages_send_0 = ''
    TABLES
      vbpok_tab                = lt_vbpok
      prot                     = lt_prot
      it_handling_units        = lt_hu
    EXCEPTIONS
        error_message          = 99.

  IF sy-subrc <> 0.
    CLEAR ls_return.
    ls_return-msgid = sy-msgid.
    ls_return-msgnr = sy-msgno.
    ls_return-msgv1 = sy-msgv1.
    ls_return-msgv2 = sy-msgv2.
    ls_return-msgv3 = sy-msgv3.
    ls_return-msgv4 = sy-msgv4.

    APPEND ls_return TO t_return.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    RAISE error.
  ENDIF.

  DELETE lt_prot WHERE msgty <> 'E'.

  READ TABLE lt_prot INDEX 1.
  IF sy-subrc = 0.
    CLEAR ls_return.
    ls_return-msgid = lt_prot-msgid.
    ls_return-msgnr = lt_prot-msgno.
    ls_return-msgv1 = lt_prot-msgv1.
    ls_return-msgv2 = lt_prot-msgv2.
    ls_return-msgv3 = lt_prot-msgv3.
    ls_return-msgv4 = lt_prot-msgv4.

    APPEND ls_return TO t_return.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    RAISE error.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFUNCTION.
