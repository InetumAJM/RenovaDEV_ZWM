FUNCTION zwm_aut_pal_especial .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_REFNR) TYPE  LVS_REFNR
*"     REFERENCE(I_SIMU) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_MESSAGES) TYPE  TAB_BDCMSGCOLL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  TYPES: BEGIN OF st_zwm031.
  TYPES: lgnum   TYPE lgnum.
  TYPES: refnr   TYPE lvs_refnr.
  TYPES: lznum   TYPE lznum.
  TYPES: kunnr   TYPE kunnr.
  TYPES: matnr   TYPE matnr.
  TYPES: lfimg   TYPE lfimg.
  TYPES: menge   TYPE menge_d.
  TYPES: vrkme   TYPE vrkme.
  TYPES: pal     TYPE int2.
  TYPES: pal_esp TYPE int2.
  TYPES END OF st_zwm031.

  DATA: lv_flag_est TYPE flag.
  DATA: lv_tbnum    TYPE tbnum.
  DATA: lv_kunnr    TYPE kunnr.
  DATA: lv_vbeln    TYPE vbeln.
  DATA: lv_refnr    TYPE lvs_refnr.
  DATA: lv_tanum    TYPE tanum.
  DATA: lv_spgru    TYPE lvs_spgru.
  DATA: lv_menge    TYPE menge_d.
  DATA: lv_posnr    TYPE posnr.
  DATA: lv_dummy    TYPE char20.
  DATA: lv_2step    TYPE flag.
  DATA: lv_2spart   TYPE flag.
  DATA: lv_tabix    TYPE sy-tabix.

  DATA: ls_lein     TYPE lein.
  DATA: ls_ltbk     TYPE ltbk.
  DATA: ls_leinv    TYPE leinv.
  DATA: ls_kna1     TYPE kna1.
  DATA: ls_messages TYPE bdcmsgcoll.

  DATA: lt_likp     TYPE TABLE OF likp       WITH HEADER LINE.
  DATA: lt_lips     TYPE TABLE OF lips       WITH HEADER LINE.
  DATA: lt_lips_esp TYPE TABLE OF lips       WITH HEADER LINE.
  DATA: lt_t311     TYPE TABLE OF t311       WITH HEADER LINE.
  DATA: lt_t311a    TYPE TABLE OF t311a      WITH HEADER LINE.
  DATA: lt_zwm031   TYPE TABLE OF zwm031     WITH HEADER LINE.
  DATA: lt_pal_esp  TYPE TABLE OF st_zwm031  WITH HEADER LINE.
  DATA: lt_ltak     TYPE TABLE OF ltak       WITH HEADER LINE.
  DATA: lt_ltak_est TYPE TABLE OF ltak       WITH HEADER LINE.
  DATA: lt_ltap     TYPE TABLE OF ltap       WITH HEADER LINE.
  DATA: lt_ltap_est TYPE TABLE OF ltap       WITH HEADER LINE.
  DATA: lt_makt     TYPE TABLE OF makt       WITH HEADER LINE.
  DATA: lt_marm     TYPE TABLE OF marm       WITH HEADER LINE.
  DATA: lt_ltap_aut TYPE TABLE OF ltap       WITH HEADER LINE.
  DATA: lt_lqua     TYPE TABLE OF lqua       WITH HEADER LINE.
  DATA: lt_lqua_aut TYPE TABLE OF lqua       WITH HEADER LINE.
  DATA: lt_ltap_e   TYPE TABLE OF ltap       WITH HEADER LINE.
  DATA: lt_ltbk     TYPE TABLE OF ltbk       WITH HEADER LINE.
  DATA: lt_zwm001   TYPE TABLE OF zwm001     WITH HEADER LINE.
  DATA: lt_ret_msg  TYPE TABLE OF bdcmsgcoll WITH HEADER LINE.

** Validar Grupo
**********************************************************************

** Validar se Grupo tem NT de Pal. Especial AUT
  SELECT *
    FROM ltbk INTO TABLE lt_ltbk
    WHERE lgnum = i_lgnum
    AND   betyp = 'S'
    AND   benum = i_refnr.

  CHECK sy-subrc = 0.

  SELECT *
    FROM t311a INTO TABLE lt_t311a
    WHERE lgnum = i_lgnum
    AND   refnr = i_refnr.

  IF lt_t311a[] IS NOT INITIAL.
    SELECT *
      FROM likp INTO TABLE lt_likp
      FOR ALL ENTRIES IN lt_t311a
      WHERE vbeln = lt_t311a-rbnum.

    SELECT *
      FROM lips INTO TABLE lt_lips
      FOR ALL ENTRIES IN lt_t311a
      WHERE vbeln = lt_t311a-rbnum.

    DELETE lt_lips WHERE lfimg <= 0.
  ENDIF.


** Validar materiais com Paletização especial
**********************************************************************
  IF lt_likp[] IS NOT INITIAL.
    SELECT *
      FROM zwm031 INTO TABLE lt_zwm031
      FOR ALL ENTRIES IN lt_likp
      WHERE lgnum = i_lgnum
      AND   kunnr = lt_likp-kunnr.
  ENDIF.

** Descrição Material
  IF lt_lips[] IS NOT INITIAL.
    SELECT *
      FROM makt INTO TABLE lt_makt
      FOR ALL ENTRIES IN lt_lips
      WHERE matnr = lt_lips-matnr
      AND   spras = sy-langu.
  ENDIF.

** Unidade PAL
  IF lt_makt[] IS NOT INITIAL.
    SELECT *
      FROM marm INTO TABLE lt_marm
      FOR ALL ENTRIES IN lt_makt
      WHERE matnr = lt_makt-matnr
      AND   meinh = 'PAL'.
  ENDIF.

*  LOOP AT lt_likp.
*
*    LOOP AT lt_lips WHERE vbeln = lt_likp-vbeln.
*
*      READ TABLE lt_zwm031 WITH KEY lgnum = i_lgnum
*                                    kunnr = lt_likp-kunnr
*                                    matnr = lt_lips-matnr.
*      CHECK sy-subrc = 0.
*      CLEAR lt_pal_esp.
*      lt_pal_esp-lgnum = i_lgnum.
*      lt_pal_esp-kunnr = lt_likp-kunnr.
*      lt_pal_esp-matnr = lt_lips-matnr.
*      lt_pal_esp-lfimg = lt_lips-lfimg.
*      lt_pal_esp-vrkme = lt_lips-vrkme.
*      COLLECT lt_pal_esp.
*    ENDLOOP.
*
*  ENDLOOP.

** Remessas
  CLEAR: lv_2step, lv_2spart.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = i_lgnum
      i_refnr  = i_refnr
    IMPORTING
      e_2step  = lv_2step
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  LOOP AT lt_t311a WHERE refnr = i_refnr.

    " Cliente
    READ TABLE lt_likp WITH KEY vbeln = lt_t311a-rbnum.
    CHECK sy-subrc = 0.

    LOOP AT lt_lips WHERE vbeln = lt_likp-vbeln.

      " Paletização Especial
      READ TABLE lt_zwm031 WITH KEY kunnr = lt_likp-kunnr
                                    matnr = lt_lips-matnr.

      CHECK sy-subrc = 0.

      " Validar se não é palete Remontada ????
      CHECK lt_zwm031-remontada IS INITIAL.

      CLEAR lt_pal_esp.
      lt_pal_esp-lgnum = i_lgnum.
      lt_pal_esp-refnr = i_refnr.
      lt_pal_esp-kunnr = lt_zwm031-kunnr.
      lt_pal_esp-matnr = lt_lips-matnr.
      lt_pal_esp-lfimg = lt_lips-lfimg.
      lt_pal_esp-vrkme = lt_lips-vrkme.

      " Picking à Remessa
      IF lv_2step IS INITIAL.

        CONCATENATE lt_lips-vbeln '#' lt_lips-posnr INTO lt_pal_esp-lznum.

        " Picking à Remessa agrupado
      ELSEIF lv_2spart = 'X'.
        lt_pal_esp-lznum = lt_likp-vbeln.

        " Picking ao Grupo
      ELSE.
        lt_pal_esp-lznum = lt_zwm031-kunnr.
      ENDIF.

      COLLECT lt_pal_esp.
    ENDLOOP.
  ENDLOOP.

  SORT lt_pal_esp BY kunnr lznum matnr.

** Validar quantidades só de paletes completas
  LOOP AT lt_pal_esp.

    lv_tabix = sy-tabix.

    READ TABLE lt_zwm031 WITH KEY kunnr = lt_pal_esp-kunnr
                                  matnr = lt_pal_esp-matnr.

    READ TABLE lt_marm WITH KEY matnr = lt_pal_esp-matnr.
    IF sy-subrc <> 0.
      " Material & sem unidade PAL definido
      CLEAR ls_messages.
      ls_messages-msgtyp = 'E'.
      ls_messages-msgid  = 'ZWMMSG001'.
      ls_messages-msgnr  = '000'.
      ls_messages-msgv1  = 'Material'.
      ls_messages-msgv2  = lt_pal_esp-matnr.
      ls_messages-msgv3  = 'sem unidade PAL definido'.
      APPEND ls_messages TO et_messages.
      EXIT.
    ENDIF.

    " Paletes
    lv_menge = lt_pal_esp-lfimg / lt_marm-umrez.
    lv_menge = floor( lv_menge ).

    IF lv_menge = 0.
      DELETE lt_pal_esp INDEX lv_tabix.
    ENDIF.

    lt_pal_esp-lfimg = lv_menge * lt_marm-umrez.

    MODIFY lt_pal_esp INDEX lv_tabix TRANSPORTING lfimg.
  ENDLOOP.

  IF et_messages[] IS NOT INITIAL.
    RAISE error.
  ENDIF.

** Validar Paletes Pal. Especial no Automático
**********************************************************************
  SELECT *
    FROM ltak INTO TABLE lt_ltak
    WHERE lgnum = i_lgnum
    AND   refnr = i_refnr.

  IF lt_ltak[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap
      FOR ALL ENTRIES IN lt_ltak
      WHERE lgnum = lt_ltak-lgnum
      AND   tanum = lt_ltak-tanum.
  ENDIF.

** Stock no automático
  lt_ltap_aut[] = lt_ltap[].

  DELETE lt_ltap_aut WHERE nltyp <> 'AUT'.

  IF lt_ltap_aut[] IS NOT INITIAL.
    SELECT *
      FROM lqua INTO TABLE lt_lqua
      FOR ALL ENTRIES IN lt_ltap_aut
      WHERE lgnum = lt_ltap_aut-lgnum
      AND   lenum = lt_ltap_aut-nlenr.

    DELETE lt_lqua WHERE lgtyp <> 'AUT'.
    DELETE lt_lqua WHERE lgpla <> 'AUT'.
  ENDIF.

  LOOP AT lt_lqua.

    " Validar paletes
    READ TABLE lt_ltap WITH KEY vltyp = 'EPE'
                                nlenr = lt_lqua-lenum.
    CHECK sy-subrc = 0.

    READ TABLE lt_ltak WITH KEY lgnum = lt_ltap-lgnum
                                tanum = lt_ltap-tanum.
    CHECK sy-subrc = 0.

    lv_tbnum = lt_ltak-lznum.

    SELECT SINGLE *
      FROM ltbk INTO ls_ltbk
      WHERE lgnum = lt_ltak-lgnum
      AND   tbnum = lv_tbnum.

    CHECK sy-subrc = 0.

    CHECK ls_ltbk-benum = i_refnr.

    " Validar Cliente e Remessa
    lv_kunnr = ls_ltbk-lznum.

    SELECT SINGLE *
      FROM kna1 INTO ls_kna1
      WHERE kunnr = lv_kunnr.

    IF sy-subrc <> 0.
      CLEAR lv_kunnr.

      lv_vbeln = ls_ltbk-lznum.

      SELECT SINGLE kunnr
        FROM likp INTO lv_kunnr
        WHERE vbeln = lv_vbeln.
    ENDIF.

*    SPLIT ls_ltbk-lznum AT '#' INTO lv_kunnr lv_vbeln.
*    IF sy-subrc <> 0.
*      lv_kunnr = ls_ltbk-lznum(10).
*    ENDIF.

    READ TABLE lt_pal_esp WITH KEY kunnr = lv_kunnr
                                   lznum = ls_ltbk-lznum
                                   matnr = lt_lqua-matnr.
    CHECK sy-subrc = 0.
    lv_tabix = sy-tabix.

    APPEND lt_lqua TO lt_lqua_aut.

    IF lt_pal_esp-vrkme <> lt_lqua-meins.
      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = lt_lqua-matnr
          i_in_me              = lt_lqua-meins
          i_out_me             = lt_pal_esp-vrkme
          i_menge              = lt_lqua-gesme
        IMPORTING
          e_menge              = lt_lqua-gesme
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.
    ENDIF.

    lt_pal_esp-menge = lt_pal_esp-menge + lt_lqua-gesme.

    MODIFY lt_pal_esp INDEX lv_tabix TRANSPORTING menge.
  ENDLOOP.

** Validar se Paletes estão todas no automático
  LOOP AT lt_pal_esp.

    lv_menge = lt_pal_esp-lfimg - lt_pal_esp-menge.

    IF lv_menge > 0.
      " Cli: & Mat: & - Falta & & Pal. Especial por concluir.
      CLEAR ls_messages.
      ls_messages-msgtyp = 'E'.
      ls_messages-msgid  = 'ZWMMSG001'.
      ls_messages-msgnr  = '365'.
      ls_messages-msgv1  = lt_pal_esp-kunnr.
      ls_messages-msgv2  = lt_pal_esp-matnr.
      ls_messages-msgv3  = lv_menge.
      ls_messages-msgv4  = lt_pal_esp-vrkme.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_messages-msgv2
        IMPORTING
          output = ls_messages-msgv2.

      CONDENSE ls_messages-msgv3.

      APPEND ls_messages TO et_messages.
    ENDIF.

  ENDLOOP.

  IF et_messages[] IS NOT INITIAL.
    RAISE error.
  ENDIF.

** Validar OTs
********************************************************************
  CHECK i_simu IS INITIAL.

  SELECT *
    FROM zwm001 INTO TABLE lt_zwm001
    WHERE armazem   = i_lgnum
    AND   processo  = 'PICKING'
    AND   parametro = 'ST_TYPE'.

  SELECT *
    FROM ltak INTO TABLE lt_ltak_est
    WHERE lgnum = i_lgnum
    AND   refnr = i_refnr.

  DELETE lt_ltak_est WHERE betyp <> 'L'. " Remessa
  DELETE lt_ltak_est WHERE kquit = 'X'.

  IF lt_ltak_est[] IS NOT INITIAL.
    SELECT *
      FROM ltap INTO TABLE lt_ltap_est
      FOR ALL ENTRIES IN lt_ltak_est
      WHERE lgnum = lt_ltak_est-lgnum
      AND   tanum = lt_ltak_est-tanum.
  ENDIF.

  LOOP AT lt_ltap_est.

    IF lt_ltap_est-vbeln IS INITIAL.
      READ TABLE lt_ltak_est WITH KEY tanum = lt_ltap_est-tanum.
      IF sy-subrc = 0.
        IF lt_ltak_est-betyp = 'L'.
          lt_ltap_est-vbeln = lt_ltak_est-benum.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE lt_likp WITH KEY vbeln = lt_ltap_est-vbeln.
    CHECK sy-subrc = 0.

    READ TABLE lt_pal_esp WITH KEY kunnr = lt_likp-kunnr
                                   matnr = lt_ltap_est-matnr.
    CHECK sy-subrc = 0.

    " OT estornada
    IF lt_ltap_est-vorga = 'ST' OR lt_ltap_est-vorga = 'SL'.
      CONTINUE.
    ENDIF.

    " OT de Tipo depósito picking
    READ TABLE lt_zwm001 WITH KEY valor(3) = lt_ltap_est-vltyp.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    " OT de Palete de Paletização Especial
    IF lt_ltap_est-vlenr IS NOT INITIAL AND ( lt_ltap_est-vltyp = 'AUT' OR lt_ltap_est-vltyp = 'EAU' ).
      CONTINUE.
    ENDIF.

    APPEND lt_ltap_est TO lt_ltap_e.
  ENDLOOP.

** Estornar - OTs Criadas - Só uma vez
**********************************************************************
  LOOP AT lt_ltap_e.

    CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
      EXPORTING
        warehouse     = lt_ltap_e-lgnum
        tanum         = lt_ltap_e-tanum
        tapos         = lt_ltap_e-tapos
      TABLES
        return_msg    = lt_ret_msg
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      et_messages[] = lt_ret_msg[].

      RAISE error.
    ENDIF.

  ENDLOOP.

** Criar OT de Expedição
**********************************************************************
  DELETE lt_lqua_aut WHERE verme <= 0.

  SORT lt_lqua_aut BY lenum.
  DELETE ADJACENT DUPLICATES FROM lt_lqua_aut COMPARING lenum.

  LOOP AT lt_lqua_aut.

    " Saída Automático
    CLEAR lt_ltap.
    READ TABLE lt_ltap WITH KEY vltyp = 'AUT'
                                vlenr = lt_lqua_aut-lenum
                                pquit = ''.
    CHECK sy-subrc <> 0.

    " Palete EPE
    READ TABLE lt_ltap WITH KEY vltyp = 'EPE'
                                nlenr = lt_lqua_aut-lenum.
    CHECK sy-subrc = 0.

    READ TABLE lt_ltak WITH KEY lgnum = lt_ltap-lgnum
                                tanum = lt_ltap-tanum.

    lv_tbnum = lt_ltak-lznum.

    SELECT SINGLE *
      FROM ltbk INTO ls_ltbk
      WHERE lgnum = lt_ltak-lgnum
      AND   tbnum = lv_tbnum.

    CHECK sy-subrc = 0.

    " Validar Cliente e Remessa
    CLEAR: lv_kunnr, lv_vbeln, lv_posnr.

    lv_kunnr = ls_ltbk-lznum.

    SELECT SINGLE *
      FROM kna1 INTO ls_kna1
      WHERE kunnr = lv_kunnr.

    IF sy-subrc <> 0.
      CLEAR lv_kunnr.

      lv_vbeln = ls_ltbk-lznum.

      SELECT SINGLE kunnr
        FROM likp INTO lv_kunnr
        WHERE vbeln = lv_vbeln.

      IF sy-subrc = 0.
        SPLIT ls_ltbk-lznum AT '#' INTO lv_dummy lv_posnr.
        IF sy-subrc <> 0.
          CLEAR lv_posnr.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE lt_pal_esp WITH KEY kunnr = lv_kunnr
                                   lznum = ls_ltbk-lznum
                                   matnr = lt_lqua_aut-matnr.
    CHECK sy-subrc = 0.

    lv_refnr = ls_ltbk-benum.

** Desbloquear SU
    SELECT SINGLE *
      FROM lein INTO ls_lein
      WHERE lenum = lt_lqua_aut-lenum.

    IF sy-subrc = 0.
      CLEAR ls_lein-skzua.
      CLEAR ls_lein-skzue.
      CLEAR ls_lein-spgru.

      MOVE-CORRESPONDING ls_lein TO ls_leinv.

      CALL FUNCTION 'L_LEIN_VERAENDERN'
        EXPORTING
          xleinv = ls_leinv.

      COMMIT WORK AND WAIT.
    ENDIF.

** Criar OT
    CLEAR lv_tanum.

    CALL FUNCTION 'ZWM_CREATE_TO_PAL_ESP'
      EXPORTING
        i_lgnum     = lt_ltak-lgnum
        i_tanum     = lt_ltak-tanum
      IMPORTING
        e_tanum     = lv_tanum
        et_messages = et_messages
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF lv_tanum IS INITIAL.

      " Bloquear UD novamente
      lv_spgru = '6'.

      ls_lein-skzua = 'X'.
      ls_lein-skzue = 'X'.
      ls_lein-spgru = lv_spgru.

      MOVE-CORRESPONDING ls_lein TO ls_leinv.

      CALL FUNCTION 'L_LEIN_VERAENDERN'
        EXPORTING
          xleinv = ls_leinv.

      COMMIT WORK AND WAIT.

      RAISE error.
    ENDIF.

  ENDLOOP.

ENDFUNCTION.
